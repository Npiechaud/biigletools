import os

BASE_URL = "https://biigle.de/api/v1"


def get_credentials(login=None, token=None):
    """Return (login, token) from the arguments or the BIIGLE_LOGIN / BIIGLE_TOKEN environment variables."""
    login = login or os.environ.get("BIIGLE_LOGIN", "")
    token = token or os.environ.get("BIIGLE_TOKEN", "")
    if not login:
        raise ValueError("Missing Biigle login. Pass login= or set BIIGLE_LOGIN in your environment.")
    if not token:
        raise ValueError("Missing Biigle token. Pass token= or set BIIGLE_TOKEN in your environment.")
    return login, token
