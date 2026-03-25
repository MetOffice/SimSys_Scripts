from pathlib import Path
import pytest

@pytest.fixture(scope="session")
def example_fortran_lines() -> list[str]:
    """Return the example Fortran source as a list of lines for tests."""
    test_dir = Path(__file__).resolve().parent
    return (test_dir / "example_fortran_code.F90").read_text().splitlines()


@pytest.fixture
def modified_fortran_lines(request: pytest.FixtureRequest, example_fortran_lines: list[str]) -> list[str]:
    """Return a copy of example_fortran_lines with changes applied.

    ``request.param`` is a list of operation dicts, each with:
      - ``{"operation": "replace", "line": N, "text": "..."}`` : replace line N
      - ``{"operation": "delete",  "line": N}``                 : remove line N
      - ``{"operation": "add",     "line": N, "text": "..."}``  : insert before line N

    Operations are applied in descending line order so that earlier
    line numbers are not shifted by later mutations.
    """
    lines = example_fortran_lines.copy()
    for change in sorted(request.param, key=lambda o: o["line"], reverse=True):
        idx = change["line"] - 1
        if change["operation"] == "replace":
            lines[idx] = change["text"]
        elif change["operation"] == "delete":
            del lines[idx]
        elif change["operation"] == "add":
            lines.insert(idx, change["text"])
        else:
            raise ValueError(f"Unknown operation: {change['operation']}")
        # for line in lines:
        #     print(line)
    return lines
