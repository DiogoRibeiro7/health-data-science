# Health Analytics Dashboard

The Shiny dashboard provides an interactive interface for healthcare analysts.

## Features
- Responsive design compatible with desktops, tablets, and phones
- Dark/light theme toggle with per-user preferences
- Drag-and-drop CSV upload with progress indicators
- Real-time parameter tuning and model execution
- Interactive Plotly visualisations for latent class profiles and streaming metrics
- Collaborative notes panel for team workflows
- Role-based access controls with activity logging
- Export capabilities for model objects and rendered reports

## Usage
Run the dashboard locally:

```bash
Rscript scripts/run_app.R
```

Authenticate with a username and password defined in `config/users.yaml`.
Administrators gain access to additional tools under the **Admin** tab.
