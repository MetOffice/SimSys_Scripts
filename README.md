# Simulation Systems Scripts

[![Checks](https://github.com/MetOffice/SimSys_Scripts/actions/workflows/lint.yml/badge.svg)](https://github.com/MetOffice/SimSys_Scripts/actions/workflows/lint.yml)
[![CodeQL](https://github.com/MetOffice/SimSys_Scripts/actions/workflows/github-code-scanning/codeql/badge.svg)](https://github.com/MetOffice/SimSys_Scripts/actions/workflows/github-code-scanning/codeql)
[![Python Unit Tests](https://github.com/MetOffice/SimSys_Scripts/actions/workflows/python_unit_tests.yaml/badge.svg)](https://github.com/MetOffice/SimSys_Scripts/actions/workflows/python_unit_tests.yaml)

This repository contains support scripts that are common across the many
simulation and modelling codes owned by the Met Office. Particularly those
owned and maintained by the Simulation Systems and Deployment (SSD) team.

Also contains a copy of `script_updater.sh` which is intended to live in the
fcm repositories to pull from this repository.

## Install venv with dev tools

- After cloning the repository, run

```sh
# venv + pip: recommended for Met Office use
python3.12 -m venv .venv
.venv/bin/pip install -e ".[dev]"

# Or, via uv (if available)
uv sync --extra dev --python 3.12
```

- Activate the virtual environment

```sh
source .venv/bin/activate
```

A helper script is also available to do this

```sh
# via venv + pip
source ./install-venv.sh
# via uv
source ./install-venv.sh [--uv] [--python PYTHON]
```

## Lint and Format Python

```sh
# Lint
ruff check . --config .github/linters/.ruff.toml
# Check Format
ruff format --check --preview --config .github/linters/.ruff.toml
```
