#!/usr/bin/env bash
# =====================================================================
# Shiny ML & Forecasting Lab - launcher (macOS / Linux)
# =====================================================================
# Usage:
#   ./run.sh               # install (first time) + launch on port 4848
#   ./run.sh --port 5050   # custom port
#   ./run.sh --skip-install
# =====================================================================
set -euo pipefail

PORT=4848
SKIP_INSTALL=0
HOST="127.0.0.1"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --port)         PORT="$2"; shift 2 ;;
    --host)         HOST="$2"; shift 2 ;;
    --skip-install) SKIP_INSTALL=1; shift ;;
    -h|--help)
      grep '^# ' "$0" | sed 's/^# \{0,1\}//'
      exit 0 ;;
    *) echo "Unknown option: $1" >&2; exit 2 ;;
  esac
done

# Resolve script directory (works on macOS + Linux)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"
export SHINYML_HOME="$SCRIPT_DIR"

# ---- Locate R --------------------------------------------------------
if ! command -v Rscript >/dev/null 2>&1; then
  echo "ERROR: Rscript not found in PATH."
  echo "  macOS:  brew install --cask r"
  echo "  Linux:  sudo apt install r-base  (Debian/Ubuntu)"
  echo "  https://cran.r-project.org"
  exit 1
fi

R_VERSION="$(Rscript -e 'cat(paste(R.version$major, R.version$minor, sep="."))')"
echo "[run.sh] R ${R_VERSION} at $(command -v Rscript)"

# ---- Install dependencies (idempotent) -------------------------------
if [[ "$SKIP_INSTALL" -eq 0 ]]; then
  echo "[run.sh] Verifying R packages \u2026 (skip with --skip-install)"
  Rscript "$SCRIPT_DIR/install_R.R"
fi

# ---- Launch the Shiny app -------------------------------------------
echo "[run.sh] Launching Shiny on http://${HOST}:${PORT}"
Rscript -e "options(shiny.port=${PORT}, shiny.host='${HOST}'); \
            Sys.setenv(SHINYML_HOME='${SCRIPT_DIR}'); \
            shiny::runApp('${SCRIPT_DIR}', launch.browser = TRUE)"
