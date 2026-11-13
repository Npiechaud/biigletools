# biigletools (Python)

Python version of the R helpers in this repo, with the same functions and arguments:

- `get_biigle_report()` - request a volume report and load it (or save the CSV)
- `label_in_project()` - check whether a label exists in a project's label trees
- `delete_one_biigle_label()` / `delete_biigle_label_list()` - delete label assignments (careful!)

## Installation

```bash
pip install "git+https://github.com/Npiechaud/biigletools.git#subdirectory=python"
```

From a local clone:

```bash
pip install -e python
```

## Credentials

Same as the R package: set `BIIGLE_LOGIN` (your BIIGLE email) and `BIIGLE_TOKEN` (your API token)
as environment variables, or pass `login=` and `token=` to any function.

```bash
# Linux / macOS
export BIIGLE_LOGIN=your.name@example.org
export BIIGLE_TOKEN=your_token_here
```

```powershell
# Windows (PowerShell, current session)
$env:BIIGLE_LOGIN = "your.name@example.org"
$env:BIIGLE_TOKEN = "your_token_here"
```

Never commit tokens. If a token is exposed, revoke it in BIIGLE.

## Quick start

```python
from biigletools import get_biigle_report, label_in_project, delete_biigle_label_list
import pandas as pd

df = get_biigle_report(volume_id=12345, type="image_label")   # or a numeric report type ID
path = get_biigle_report(volume_id=12345, type="csv", read=False, out_dir="reports/csv")

res = label_in_project(project_id=678, label="No observation")   # or a numeric label ID
res["found"], res["matches"]

to_delete = pd.DataFrame({"annotation_label_id": [111, 222, 333],
                          "type": ["image", "image", "video"]})
delete_biigle_label_list(to_delete, dry_run=True)   # preview first, asks DELETE / YES
```

## Tests

```bash
pip install -e "python[test]"
pytest python/tests
```

The tests run offline (no BIIGLE requests).
