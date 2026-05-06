# Setup Guide — Shiny ML & Forecasting Lab

A premium, identical experience on **macOS · Windows · Linux**.
Pure R Shiny (with optional Python via reticulate). Author: **Malik Hebbat**.

---

## 1. Prerequisites

| Platform | Requirement | Install |
|----------|-------------|---------|
| **macOS** | R 4.2+ | `brew install --cask r` or [CRAN](https://cran.r-project.org/bin/macosx/) |
| **Windows** | R 4.2+ | [CRAN Windows installer](https://cran.r-project.org/bin/windows/base/) |
| **Linux** | R 4.2+ | `sudo apt install r-base` (Debian/Ubuntu) — or your distro equivalent |

(Optional) Python 3.10+ if you plan to use the LightGBM / CatBoost / Keras backends.

---

## 2. One-line install + launch

After cloning:

```bash
git clone https://github.com/MHebbat/shiny-ml-forecasting-lab.git
cd shiny-ml-forecasting-lab
```

### macOS / Linux
```bash
./run.sh
```

### Windows (Command Prompt or PowerShell)
```bat
run.bat
```

### Any platform with `make`
```bash
make install   # first time only
make run
```

The launcher will:
1. Check that `Rscript` is on your `PATH`.
2. Install missing R packages (idempotent — safe to re-run).
3. Open the app in your default browser at `http://127.0.0.1:4848`.

---

## 3. Custom port / host

```bash
./run.sh --port 5050
run.bat --port 5050
make run PORT=5050 HOST=0.0.0.0
```

---

## 4. AI verdicts (optional)

Set any of these environment variables before launching to enable
real LLM analyses (otherwise the app uses a deterministic heuristic):

```bash
export OPENAI_API_KEY="sk-..."
export ANTHROPIC_API_KEY="sk-ant-..."
export PPLX_API_KEY="pplx-..."
```

Windows (`cmd`):
```bat
setx OPENAI_API_KEY "sk-..."
```

---

## 5. Troubleshooting

| Symptom | Fix |
|---------|-----|
| `Rscript not found` | Ensure R is installed and `R/bin` is on `PATH`. Open a fresh terminal. |
| Package install fails on Linux | Install system deps: `sudo apt install build-essential libcurl4-openssl-dev libssl-dev libxml2-dev` |
| Port already in use | Re-run with `--port 5050` |
| Python backend missing | `pip install -r requirements.txt` (only needed for LightGBM/CatBoost/Keras) |

---

## 6. Project layout

```
shiny-ml-forecasting-lab/
├── app.R                  # entry point
├── run.sh / run.bat       # cross-platform launchers
├── Makefile               # make install / run / check / clean
├── install_R.R            # idempotent package installer
├── R/                     # 12 R modules (ingest, explore, model lab, dashboard, studio, runs, ...)
├── python/py_models.py    # optional Python backends
├── www/                   # custom.css + studio.css (editorial theme)
├── data/                  # sample datasets
└── db/                    # auto-created SQLite store
```
