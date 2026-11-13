import io
import zipfile

import pandas as pd
import pytest

import biigletools
from biigletools import (delete_biigle_label_list, delete_one_biigle_label, get_biigle_report,
                         get_credentials, label_in_project)


@pytest.fixture
def creds(monkeypatch):
    monkeypatch.setenv("BIIGLE_LOGIN", "a@b.c")
    monkeypatch.setenv("BIIGLE_TOKEN", "tok")


def test_credentials_from_arguments():
    assert get_credentials("a@b.c", "tok") == ("a@b.c", "tok")


def test_credentials_from_environment(monkeypatch):
    monkeypatch.setenv("BIIGLE_LOGIN", "env@b.c")
    monkeypatch.setenv("BIIGLE_TOKEN", "envtok")
    assert get_credentials() == ("env@b.c", "envtok")


def test_credentials_missing(monkeypatch):
    monkeypatch.delenv("BIIGLE_LOGIN", raising=False)
    monkeypatch.delenv("BIIGLE_TOKEN", raising=False)
    with pytest.raises(ValueError, match="Missing Biigle login"):
        get_credentials()
    with pytest.raises(ValueError, match="Missing Biigle token"):
        get_credentials(login="a@b.c")


def test_get_biigle_report_validates(creds):
    with pytest.raises(ValueError, match="volume_id"):
        get_biigle_report(None, "csv")
    with pytest.raises(ValueError, match="Unknown report type"):
        get_biigle_report(1, "nope")


def test_label_in_project_requires_arguments():
    with pytest.raises(ValueError, match="project_id"):
        label_in_project(None, "x")
    with pytest.raises(ValueError, match="label"):
        label_in_project(1, None)


def test_delete_one_validates_id():
    with pytest.raises(ValueError, match="id"):
        delete_one_biigle_label("")
    with pytest.raises(ValueError, match="numeric"):
        delete_one_biigle_label("abc")


def test_delete_list_rejects_bad_input(creds):
    with pytest.raises(ValueError, match="DataFrame"):
        delete_biigle_label_list(123)
    with pytest.raises(ValueError, match="No ID column"):
        delete_biigle_label_list(pd.DataFrame({"x": [1]}), confirm_required=False)


def test_delete_list_dry_run(creds):
    out = delete_biigle_label_list(pd.DataFrame({"annotation_label_id": [1, 2]}),
                                   dry_run=True, confirm_required=False)
    assert len(out) == 2


def test_delete_list_cancelled(creds, monkeypatch):
    monkeypatch.setattr(biigletools.delete, "_ask", lambda prompt: "no")
    assert delete_biigle_label_list(pd.DataFrame({"annotation_label_id": [1]})) is None


class FakeResponse:
    def __init__(self, status_code, payload=None, content=b""):
        self.status_code, self.payload, self.content = status_code, payload, content

    def raise_for_status(self):
        pass

    def json(self):
        return self.payload


def test_get_biigle_report_reads_zip(creds, monkeypatch, tmp_path):
    buffer = io.BytesIO()
    with zipfile.ZipFile(buffer, "w") as z:
        z.writestr("12-test-volume.csv", "label_name,image_id\nKolga,1\nThenea,2\n")
    responses = iter([FakeResponse(404), FakeResponse(200, content=buffer.getvalue())])
    monkeypatch.setattr(biigletools.reports.requests, "post", lambda *a, **k: FakeResponse(200, {"id": 5}))
    monkeypatch.setattr(biigletools.reports.requests, "get", lambda *a, **k: next(responses))
    monkeypatch.setattr(biigletools.reports.time, "sleep", lambda s: None)

    df = get_biigle_report(12, "csv", verbose=False)
    assert list(df["label_name"]) == ["Kolga", "Thenea"]

    responses = iter([FakeResponse(200, content=buffer.getvalue())])
    path = get_biigle_report(12, "csv", read=False, out_dir=tmp_path, verbose=False)
    assert path.name == "12-test-volume.csv" and path.exists()
