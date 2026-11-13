import pandas as pd
import requests

from .auth import BASE_URL, get_credentials


def _get(endpoint, base_url, auth):
    r = requests.get(base_url + endpoint, auth=auth, headers={"Accept": "application/json"}, timeout=60)
    r.raise_for_status()
    return r.json()


def label_in_project(project_id, label, base_url=BASE_URL, login=None, token=None):
    """Check whether a label (numeric ID or exact name, case-insensitive) exists in a project's label trees.

    Returns {"found": bool, "matches": DataFrame or None}.
    """
    if project_id is None or str(project_id).strip() == "":
        raise ValueError("project_id is required.")
    if label is None or str(label).strip() == "":
        raise ValueError("label is required (ID or name).")

    by_id = isinstance(label, int) or str(label).strip().isdigit()
    wanted = int(label) if by_id else str(label).strip().lower()
    auth = get_credentials(login, token)

    rows = []
    for tree in _get(f"/projects/{project_id}/label-trees", base_url, auth):
        for lab in _get(f"/label-trees/{tree['id']}", base_url, auth).get("labels", []):
            value = lab["id"] if by_id else str(lab["name"]).strip().lower()
            if value == wanted:
                rows.append({"project_id": project_id, "label_tree_id": tree["id"],
                             "label_tree_name": tree.get("name"), "label_id": lab["id"],
                             "label_name": lab["name"]})

    return {"found": bool(rows), "matches": pd.DataFrame(rows) if rows else None}
