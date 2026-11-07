# Simulation Systems Scripts

[![Checks](https://github.com/MetOffice/SimSys_Scripts/actions/workflows/lint.yml/badge.svg)](https://github.com/MetOffice/SimSys_Scripts/actions/workflows/lint.yml)
[![CodeQL](https://github.com/MetOffice/SimSys_Scripts/actions/workflows/github-code-scanning/codeql/badge.svg)](https://github.com/MetOffice/SimSys_Scripts/actions/workflows/github-code-scanning/codeql)
[![Python Unit Tests](https://github.com/MetOffice/SimSys_Scripts/actions/workflows/python_unit_tests.yaml/badge.svg)](https://github.com/MetOffice/SimSys_Scripts/actions/workflows/python_unit_tests.yaml)

This repository contains support scripts that are common across the many
simulation and modelling codes owned by the Met Office. Particularly those
owned and maintained by the Simulation Systems and Deployment (SSD) team.

Also contains a copy of `script_updater.sh` which is intended to live in the
fcm repositories to pull from this repository.

## Local Development

Use Python 3.12 or newer. Run the following commands from the repository root.
Choose either `uv` (recommended) or `pip` to create your local environment.

### With uv

Install [uv](https://docs.astral.sh/uv/getting-started/installation/), then run:

```sh
uv sync --locked --extra dev --extra test
uv run --frozen --extra dev pre-commit install
uv run --frozen --extra test pytest -vv
```

`uv sync` creates `.venv`, installs the versions recorded in `uv.lock`, and
installs this project in editable mode. Code edits do not require reinstalling;
scripts continue to run from the checkout. Rerun `uv sync` after changing
dependencies.
Activate it before committing so the local hooks can find Ruff and rumdl:

```sh
source .venv/bin/activate
```

Alternatively, commit with `uv run --frozen --extra dev git commit`.

### With pip

Use a Python 3.12 or newer executable to create a virtual environment:

```sh
python3.12 -m venv .venv
source .venv/bin/activate
python -m pip install --upgrade pip
python -m pip install -e '.[dev,test]'
pre-commit install
pytest -vv
```

Keep the environment activated when committing or running quality tools.
The `-e` option installs the project in editable mode for continuous development.
The `pip` setup uses the same pinned Ruff, rumdl, and pre-commit versions as
`uv`, but resolves transitive dependencies independently of `uv.lock`.

### Quality Checks

Ruff handles Python linting, import sorting, and formatting. rumdl handles
Markdown linting and fixes. Their configuration is in `pyproject.toml`, and
the shared hooks are defined in `.pre-commit-config.yaml`.

With `uv`, run all hooks explicitly:

```sh
uv run --frozen --extra dev pre-commit run --hook-stage manual --all-files
```

With an activated `pip` environment, use:

```sh
pre-commit run --hook-stage manual --all-files
```

Hooks run automatically on staged files at commit time. If a hook changes a
file, review and stage its fixes, then commit again. The first run installs
the hook environments; subsequent runs reuse them. External hooks require
Git, and the actionlint and gitleaks hooks also require Go on `PATH`.

CI uses `uv.lock` and the same hooks to check changed files on pull requests
and pushes. Manually dispatching the Checks workflow audits all files;
existing files may still need import sorting or other cleanup.
Secret checks scan staged changes at commit time and Git history in CI and
manual audits.

The lightweight hooks also check shell scripts, GitHub Actions workflows,
JSON and YAML syntax, merge conflicts, whitespace, and hardcoded secrets.
Zizmor checks GitHub Actions security using `.github/zizmor.yml`. Changes to
workflows, action definitions, Dependabot, or pre-commit/zizmor configuration
trigger a repository-wide audit locally and through the same hook in the
Checks workflow. It runs offline without GitHub credentials; network-dependent
audits are not available in this mode.

Super-Linter is no longer used. Checkov, natural-language, and Perl checks
are not part of the replacement; YAML checking validates syntax rather than
the former style rules.

For dependency updates, edit `pyproject.toml` and regenerate the lockfile with
`uv lock --upgrade`. Commit both files together. The Python Unit Tests workflow
uses the same `test` extra and runs `pytest -vv`.

Remote pre-commit hooks are pinned to full Git commit SHAs. To update them
while retaining immutable pins, run:

```sh
uv run --frozen --extra dev pre-commit autoupdate --freeze
```

Review and commit the updated hook configuration. Python quality tools and
the build backend use exact package versions in `pyproject.toml`; `uv.lock`
records resolved dependencies and SHA-256 hashes for their distribution
artifacts. Keeping registry packages avoids Rust source builds for Ruff and
rumdl. Use the locked `uv` setup for reproducible, hash-verified dependencies;
the plain `pip` setup does not enforce the hashes in `uv.lock`.

### Dependency Security

Dependabot checks Python dependencies, pre-commit hooks, and GitHub Actions
for version updates every weekday. Routine updates are grouped by ecosystem;
security updates are left ungrouped for independent review. Hook updates retain
full commit SHA pins and their `# frozen:` version labels.

Routine version updates have a seven-day cooldown to reduce exposure to newly
published compromised releases. Security updates bypass this cooldown.
Setuptools remains at `80.9.0` for downstream compatibility, and future
setuptools updates are kept separate from grouped Python updates for review.

### Note for repository admins

A repository administrator must enable the dependency graph, Dependabot alerts,
and Dependabot security updates in GitHub's repository security settings after
this configuration reaches the default branch. The YAML configuration does not
enable alerts or email notifications by itself. Maintainers should also enable
Dependabot alert notifications in their personal GitHub notification settings.

Treat critical- and high-severity CVE alerts as emergency triage items. In the
repository's Security tab, filter Dependabot alerts using `severity:critical`
or `severity:high`, assess exposure, and prioritize the corresponding security
update pull request. Alerts are driven by GitHub's advisory database and are
not delayed until the scheduled version-update check. Do not wait for a grouped
routine update or auto-merge an unreviewed security fix.
