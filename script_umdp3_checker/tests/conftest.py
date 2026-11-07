# -----------------------------------------------------------------------------
# (C) Crown copyright Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
# -----------------------------------------------------------------------------
from pathlib import Path

import pytest


@pytest.fixture(scope="session")
def example_fortran_lines() -> list[str]:
    """Return the example Fortran source as a list of lines for tests."""
    test_dir = Path(__file__).resolve().parent
    return (test_dir / "example_fortran_code.F90").read_text().splitlines()
