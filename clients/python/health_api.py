"""Simple Python client for the Health Data Science API."""

import requests

class HealthAPI:
    def __init__(self, base_url, api_key=None, token=None):
        self.base_url = base_url.rstrip('/')
        self.session = requests.Session()
        if api_key:
            self.session.headers['X-API-Key'] = api_key
        if token:
            self.session.headers['Authorization'] = f'Bearer {token}'

    def health(self):
        r = self.session.get(f'{self.base_url}/v1/health/live')
        r.raise_for_status()
        return r.json()

    def submit_lca(self, path):
        with open(path, 'rb') as fh:
            files = {'file': fh}
            r = self.session.post(f'{self.base_url}/v1/lca', files=files)
        r.raise_for_status()
        return r.json()
