#!/usr/bin/env bash

# Installs the package into a virtual environment (.venv) at the repository root.
#
# Usage:
#   source ./install-venv.sh [--uv] [--python PYTHON]
#
#   --uv              Use `uv` to create and sync the virtual environment
#                     (alternative to venv + pip).
#   --python PYTHON   Path to the Python interpreter to use (default: python3.12).
#
# Requirements:
#   - Python 3.12 or higher
#   - pip (bundled with Python)
#   - uv (optional, only required when --uv flag is passed)
#

USE_UV=false
PYTHON=""  # Empty by default; resolved differently per install path

# Parse arguments
while [[ $# -gt 0 ]]; do
  case "$1" in
    --uv) USE_UV=true ;;
    --python)
      if [[ -z "${2:-}" ]]; then
        echo "Error: --python requires an argument." >&2
        return 1
      fi
      PYTHON="$2"
      shift
      ;;
    *) echo "Unknown argument: $1" >&2; return 1 ;;
  esac
  shift
done

if $USE_UV; then
  # Option 2: uv - alternative for users who prefer uv for environment management.
  # Syncs dependencies declared in pyproject.toml / uv.lock.
  # The Python version is resolved from pyproject.toml unless --python is specified.
  if ! command -v uv &>/dev/null; then
    echo "Error: 'uv' is not installed or not on PATH." >&2
    return 1
  fi
  uv sync --extra dev ${PYTHON:+--python "$PYTHON"}
else
  # Option 1: venv + pip - recommended for Met Office users.
  # Ensures packages are resolved via Met Office Artifactory rather than PyPI.
  # Falls back to python3.12 as the minimum version required by pyproject.toml.
  PYTHON="${PYTHON:-python3.12}"
  if ! command -v "$PYTHON" &>/dev/null; then
    echo "Error: '$PYTHON' is not installed or not on PATH." >&2
    return 1
  fi
  "$PYTHON" -m venv .venv
  .venv/bin/pip install -e ".[dev]"
fi
# Activate the environment after installation. This allows users to immediately
# start using the installed packages without needing to run the
# activation command separately.
echo "Installation complete. Activating the environment..."
#shellcheck disable=SC1091
source .venv/bin/activate
echo "Environment activated.
  To deactivate, run: deactivate.
  To remove the environment, delete the .venv directory."
