import warnings
from pathlib import Path

import pandas as pd
import requests

from .auth import BASE_URL, get_credentials

ID_COLUMNS = ["annotation_label_id", "annotations_label_id", "image_label_id"]


def _ask(prompt):
    return input(prompt)


def delete_one_biigle_label(id, type="image", kind="annotation_label", base_url=BASE_URL,
                            login=None, token=None, verbose=True):
    """Delete one label assignment. type: "image" or "video"; kind: "annotation_label" or "image_label".

    Returns {"status": int, "ok": bool, "text": str}.
    """
    if id is None or str(id).strip() == "":
        raise ValueError("id is required.")
    if type not in ("image", "video"):
        raise ValueError('type must be "image" or "video".')
    if kind not in ("annotation_label", "image_label"):
        raise ValueError('kind must be "annotation_label" or "image_label".')
    try:
        id = int(float(id))
    except ValueError:
        raise ValueError("id must be numeric/integer-like.") from None

    endpoint = f"/{type}-annotation-labels/{id}" if kind == "annotation_label" else f"/{type}-labels/{id}"
    r = requests.delete(base_url + endpoint, auth=get_credentials(login, token),
                        headers={"Accept": "application/json"}, timeout=60)
    result = {"status": r.status_code, "ok": r.status_code in (200, 204), "text": r.text}
    if verbose:
        print(f"DELETE {endpoint} -> {r.status_code} ({'OK' if result['ok'] else 'FAILED'})")
    return result


def delete_biigle_label_list(input, id_column=None, type_column=None, kind_column=None, dry_run=False,
                             confirm_required=True, base_url=BASE_URL, login=None, token=None):
    """Delete many label assignments listed in a DataFrame or a CSV file.

    The ID column is auto-detected (annotation_label_id, annotations_label_id or image_label_id).
    Optional "type" (image/video) and "kind" (annotation_label/image_label) columns are used when present.
    Returns None if cancelled, a preview DataFrame with dry_run=True, otherwise one result row per deletion.
    """
    login, token = get_credentials(login, token)

    if isinstance(input, pd.DataFrame):
        df = input.copy()
    elif isinstance(input, (str, Path)) and Path(input).is_file():
        df = pd.read_csv(input)
    else:
        raise ValueError("input must be a DataFrame or a CSV file path.")

    if id_column is None:
        found = [c for c in ID_COLUMNS if c in df.columns]
        if not found:
            raise ValueError(f"No ID column found. Expected one of: {', '.join(ID_COLUMNS)}")
        id_column = found[0]
    if id_column not in df.columns:
        raise ValueError(f"ID column '{id_column}' not found.")
    type_column = type_column or ("type" if "type" in df.columns else None)
    kind_column = kind_column or ("kind" if "kind" in df.columns else None)
    default_kind = "annotation_label" if "annotation" in id_column else "image_label"

    df["id_value"] = df[id_column].astype(str).str.strip()
    df["item_type"] = df[type_column].astype(str).str.strip().str.lower() if type_column else "image"
    df["item_kind"] = df[kind_column].astype(str).str.strip().str.lower() if kind_column else default_kind
    df = df[df[id_column].notna() & (df["id_value"] != "")]
    if df.empty:
        raise ValueError("No valid IDs to delete.")
    df.loc[~df["item_type"].isin(["image", "video"]), "item_type"] = "image"
    df.loc[~df["item_kind"].isin(["annotation_label", "image_label"]), "item_kind"] = default_kind

    numeric = pd.to_numeric(df["id_value"], errors="coerce")
    if numeric.isna().any():
        warnings.warn(f"{int(numeric.isna().sum())} row(s) dropped because ID is not numeric/integer-like.")
        df, numeric = df[numeric.notna()], numeric[numeric.notna()]
    if df.empty:
        raise ValueError("No numeric/integer-like IDs to delete after filtering.")

    first = ", ".join(df["id_value"].head(10))
    print(f"\nYou are about to delete {len(df)} BIIGLE label assignment(s).")
    print(f"First IDs: {first}{' ...' if len(df) > 10 else ''}")
    print(f"Types: {', '.join(df['item_type'].unique())}")
    print(f"Kinds: {', '.join(df['item_kind'].unique())}\n")

    if confirm_required:
        try:
            if _ask("Type DELETE to confirm: ") != "DELETE":
                print("Cancelled. Nothing deleted.")
                return None
            if _ask("Final confirmation, type YES: ").upper() != "YES":
                print("Cancelled. Nothing deleted.")
                return None
        except EOFError:
            raise RuntimeError("confirm_required=True needs an interactive session. "
                               "Set confirm_required=False for non-interactive use.") from None

    if dry_run:
        print("Dry run enabled. No requests sent.")
        return df[[id_column, "item_type", "item_kind"]]

    results = []
    for i, (value, id_num, type_, kind) in enumerate(
            zip(df["id_value"], numeric, df["item_type"], df["item_kind"]), start=1):
        try:
            res = delete_one_biigle_label(id_num, type_, kind, base_url, login, token, verbose=False)
        except Exception as e:
            res = {"status": None, "ok": False, "text": str(e)}
        results.append({"id": value, "type": type_, "kind": kind, "status": res["status"],
                        "ok": res["ok"], "message": res["text"]})
        print(f"[{i}/{len(df)}] {value} ({type_}, {kind}) -> "
              f"{'DELETED' if res['ok'] else 'FAILED ' + str(res['status'])}")
    return pd.DataFrame(results)
