// Simple JS client for the Health Data Science API
export class HealthApi {
  constructor(baseUrl, apiKey = null, token = null) {
    this.baseUrl = baseUrl.replace(/\/$/, '');
    this.headers = {};
    if (apiKey) this.headers['X-API-Key'] = apiKey;
    if (token) this.headers['Authorization'] = `Bearer ${token}`;
  }

  async health() {
    const res = await fetch(`${this.baseUrl}/v1/health/live`, { headers: this.headers });
    if (!res.ok) throw new Error(await res.text());
    return res.json();
  }
}
