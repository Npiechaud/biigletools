import io
import tempfile
import time
import zipfile
from pathlib import Path

import pandas as pd
import requests

from .auth import BASE_URL, get_credentials

REPORT_TYPES = {"csv": 3, "image_label": 7}


def get_biigle_report(volume_id, type, read=True, all_csv=False, out_dir=None, max_wait=300, interval=2,
                      base_url=BASE_URL, login=None, token=None, verbose=True):
    """Request a volume report, wait until it is ready and return it.

    read=True returns a DataFrame (or a dict of DataFrames with all_csv=True).
    read=False saves the CSV file(s) to out_dir (a temp folder by default) and returns the path(s).
    """
    if volume_id is None or str(volume_id).strip() == "":
        raise ValueError("volume_id is required.")
    if type is None or str(type).strip() == "":
        raise ValueError("type is required.")
    if isinstance(type, str):
        if type.strip() not in REPORT_TYPES:
            raise ValueError(f"Unknown report type '{type}'. Known: {', '.join(REPORT_TYPES)} "
                             "(or pass a numeric type ID).")
        type = REPORT_TYPES[type.strip()]
    if max_wait < 0:
        raise ValueError("max_wait must be a non-negative number.")
    if interval <= 0:
        raise ValueError("interval must be a positive number.")

    auth = get_credentials(login, token)
    r = requests.post(f"{base_url}/volumes/{volume_id}/reports", json={"type": type}, auth=auth,
                      headers={"Accept": "application/json"}, timeout=60)
    r.raise_for_status()
    report_id = r.json().get("id")
    if report_id is None:
        raise RuntimeError("API response did not contain a report ID.")
    if verbose:
        print(f"Report requested (id {report_id}). Polling until ready...")

    waited = 0
    while True:
        r = requests.get(f"{base_url}/reports/{report_id}", auth=auth, timeout=300)
        if r.status_code == 200:
            break
        if waited >= max_wait:
            raise TimeoutError(f"Report not ready after {max_wait} seconds (last HTTP status: {r.status_code}).")
        if verbose:
            print(f"Report not ready yet (status {r.status_code}). Retrying in {interval}s...")
        time.sleep(interval)
        waited += interval

    archive = zipfile.ZipFile(io.BytesIO(r.content))
    names = [n for n in archive.namelist() if n.lower().endswith(".csv")]
    if not names:
        raise RuntimeError("No CSV files found in the downloaded report archive.")
    if not all_csv:
        if len(names) > 1 and verbose:
            print(f"Multiple CSV files found; returning the first: {names[0]}")
        names = names[:1]

    if read:
        frames = {n: pd.read_csv(archive.open(n)) for n in names}
        return frames if all_csv else frames[names[0]]

    out_dir = Path(out_dir) if out_dir else Path(tempfile.mkdtemp(prefix="biigle_report_"))
    out_dir.mkdir(parents=True, exist_ok=True)
    paths = {}
    for n in names:
        paths[n] = out_dir / Path(n).name
        paths[n].write_bytes(archive.read(n))
    return paths if all_csv else paths[names[0]]
