# -----------------------------------------------------------------------------
# (C) Crown copyright Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
# -----------------------------------------------------------------------------
"""
Unit tests for copyrighter
"""

import os
import stat
from pathlib import Path

import pytest

from ..copyrighter import (
    BORDER,
    MARKER,
    Counts,
    atomic_write,
    collect_files,
    detect_type,
    generate_header,
    main,
    process_file,
    process_path,
    split_lines,
)

# ------------------------------------------------------------------------------
# generate_header
# ------------------------------------------------------------------------------


def test_generate_header_python_hash_style():
    lines = generate_header("python")
    assert lines[0] == f"# {BORDER}"
    assert lines[1] == "# (C) Crown copyright Met Office. All rights reserved."
    assert lines[4] == f"# {BORDER}"
    assert len(lines) == 5


def test_generate_header_fortran77_five_space_separator():
    lines = generate_header("fortran77")
    assert lines[0] == f"C     {BORDER}"
    assert lines[1] == "C     (C) Crown copyright Met Office. All rights reserved."
    # the marker must be in column 1, not indented
    assert lines[1].startswith("C")


def test_generate_header_fortran_free_form_uses_bang():
    lines = generate_header("fortran")
    assert lines[0] == f"! {BORDER}"


def test_generate_header_markdown_wraps_html_comment():
    lines = generate_header("markdown")
    assert lines[0] == "<!--"
    assert lines[-1] == "-->"
    assert lines[1] == BORDER  # no prefix, no separator


def test_generate_header_c_wraps_block_comment():
    lines = generate_header("c")
    assert lines[0] == "/*"
    assert lines[-1] == " */"
    assert lines[1] == f" * {BORDER}"


def test_generate_header_rst_directive_syntax():
    lines = generate_header("rst")
    assert lines[0] == f".. {BORDER}"
    assert lines[-1] == f"   {BORDER}"


def test_generate_header_json_uses_jsonc_style():
    lines = generate_header("json")
    assert lines[0] == f"// {BORDER}"


def test_generate_header_unknown_type_raises():
    with pytest.raises(KeyError):
        generate_header("not-a-real-type")


# ------------------------------------------------------------------------------
# detect_type
# ------------------------------------------------------------------------------


@pytest.mark.parametrize(
    "name,expected",
    [
        ("foo.py", "python"),
        ("FOO.PY", "python"),  # case-insensitive
        ("foo.sh", "shell"),
        ("foo.bash", "shell"),
        ("foo.ksh", "shell"),
        ("foo.cpp", "cpp"),
        ("foo.hpp", "cpp"),
        ("foo.c", "c"),
        ("foo.h", "c"),
        ("foo.f90", "fortran"),
        ("foo.f95", "fortran"),
        ("foo.f", "fortran77"),
        ("foo.f77", "fortran77"),
        ("foo.for", "fortran77"),
        ("foo.go", "go"),
        ("foo.java", "java"),
        ("foo.js", "js"),
        ("foo.mjs", "js"),
        ("foo.json", "json"),
        ("foo.md", "markdown"),
        ("foo.pl", "perl"),
        ("foo.pm", "perl"),
        ("foo.ps1", "powershell"),
        ("foo.ps", "postscript"),
        ("foo.eps", "postscript"),
        ("foo.r", "r"),
        ("foo.rb", "ruby"),
        ("foo.rs", "rust"),
        ("foo.rst", "rst"),
        ("foo.tex", "latex"),
        ("foo.toml", "toml"),
        ("foo.yaml", "yaml"),
        ("foo.yml", "yaml"),
        ("foo.awk", "awk"),
    ],
)
def test_detect_type_by_extension(name, expected):
    assert detect_type(name, "") == expected


@pytest.mark.parametrize(
    "first_line,expected",
    [
        ("#!/usr/bin/env python3", "python"),
        ("#!/usr/bin/perl", "perl"),
        ("#!/usr/bin/env ruby", "ruby"),
        ("#!/bin/bash", "shell"),
        ("#!/bin/sh", "shell"),
        ("#!/usr/bin/ksh", "shell"),
        ("not a shebang", ""),
        ("#!/bin/strange-interpreter", ""),
    ],
)
def test_detect_type_falls_back_to_shebang(first_line, expected):
    assert detect_type("noextension", first_line) == expected


def test_detect_type_extension_wins_over_shebang():
    assert detect_type("foo.py", "#!/bin/bash") == "python"


def test_detect_type_hidden_dotfile_has_no_extension_match():
    # ".bashrc" splits to ext "bashrc", which is not a known extension
    assert detect_type(".bashrc", "") == ""
    assert detect_type(".bashrc", "#!/bin/bash") == "shell"


# ------------------------------------------------------------------------------
# split_lines
# ------------------------------------------------------------------------------


@pytest.mark.parametrize(
    "text,expected",
    [
        ("", []),
        ("a\nb\n", ["a", "b"]),
        ("a\nb", ["a", "b"]),
        ("\n", [""]),
        ("a", ["a"]),
        ("a\n\nb\n", ["a", "", "b"]),
    ],
)
def test_split_lines(text, expected):
    assert split_lines(text) == expected


# ------------------------------------------------------------------------------
# collect_files
# ------------------------------------------------------------------------------


def test_collect_files_excludes_vcs_dirs_and_symlinks(tmp_path):
    (tmp_path / ".git").mkdir()
    (tmp_path / ".git" / "config").write_text("junk")
    (tmp_path / ".svn").mkdir()
    (tmp_path / ".svn" / "entries").write_text("junk")
    (tmp_path / "sub").mkdir()
    (tmp_path / "sub" / "a.py").write_text("print(1)\n")
    (tmp_path / "top.py").write_text("print(1)\n")
    real = tmp_path / "real.py"
    real.write_text("print(1)\n")
    (tmp_path / "link.py").symlink_to(real)

    non_recursive = collect_files(tmp_path, recursive=False)
    assert sorted(p.name for p in non_recursive) == ["real.py", "top.py"]

    recursive = collect_files(tmp_path, recursive=True)
    names = sorted(str(p.relative_to(tmp_path)) for p in recursive)
    assert names == ["real.py", "sub/a.py", "top.py"]


def test_collect_files_non_recursive_ignores_subdirs(tmp_path):
    (tmp_path / "sub").mkdir()
    (tmp_path / "sub" / "a.py").write_text("print(1)\n")
    (tmp_path / "top.py").write_text("print(1)\n")

    result = collect_files(tmp_path, recursive=False)
    assert [p.name for p in result] == ["top.py"]


# ------------------------------------------------------------------------------
# atomic_write
# ------------------------------------------------------------------------------


def test_atomic_write_preserves_permissions_and_content(tmp_path):
    target = tmp_path / "f.py"
    target.write_text("old content\n")
    target.chmod(0o640)

    atomic_write(target, b"new content\n")

    assert target.read_bytes() == b"new content\n"
    assert stat.S_IMODE(target.stat().st_mode) == 0o640


def test_atomic_write_no_stray_tempfile_left_behind(tmp_path):
    target = tmp_path / "f.py"
    target.write_text("old\n")
    atomic_write(target, b"new\n")
    assert list(tmp_path.iterdir()) == [target]


# ------------------------------------------------------------------------------
# process_file
# ------------------------------------------------------------------------------


def _run(path, forced_type=None, ignored_types=None, dry_run=False):
    counts = Counts()
    needs_header: list[str] = []
    process_file(
        path, forced_type, ignored_types or set(), counts, dry_run, needs_header
    )
    return counts, needs_header


def test_process_file_adds_header_to_new_file(tmp_path):
    f = tmp_path / "a.py"
    f.write_text("print(1)\n")

    counts, needs_header = _run(f)

    assert counts.added == 1
    assert needs_header == [f"{f} (Added)"]
    content = f.read_text()
    assert MARKER in content
    assert content.endswith("print(1)\n")


def test_process_file_preserves_shebang_before_header(tmp_path):
    f = tmp_path / "a.py"
    f.write_text("#!/usr/bin/env python3\nprint(1)\n")

    _run(f)

    lines = f.read_text().splitlines()
    assert lines[0] == "#!/usr/bin/env python3"
    assert MARKER in lines[2]


def test_process_file_preserves_postscript_magic_line(tmp_path):
    f = tmp_path / "a.ps"
    f.write_text("%!PS-Adobe-3.0\nshowpage\n")

    _run(f)

    lines = f.read_text().splitlines()
    assert lines[0] == "%!PS-Adobe-3.0"
    assert MARKER in lines[2]


def test_process_file_idempotent_second_run_is_unchanged(tmp_path):
    f = tmp_path / "a.py"
    f.write_text("print(1)\n")

    _run(f)
    before = f.read_bytes()
    counts, needs_header = _run(f)

    assert counts.unchanged == 1
    assert counts.added == 0
    assert needs_header == []
    assert f.read_bytes() == before


def test_process_file_updates_stale_header(tmp_path):
    f = tmp_path / "a.py"
    f.write_text(
        "# -----\n"
        "# (C) Crown copyright Met Office 2020. All rights reserved.\n"
        "# The file LICENCE, distributed with this code, contains details of the terms\n"
        "# under which the code may be used.\n"
        "# -----\n"
        "\n"
        "print(1)\n"
    )

    counts, needs_header = _run(f)

    assert counts.updated == 1
    assert needs_header == [f"{f} (Updated)"]
    content = f.read_text()
    assert content.count(MARKER) == 1
    assert f"# {BORDER}" in content


def test_process_file_collapses_duplicate_headers(tmp_path):
    header = "\n".join(generate_header("python")) + "\n"
    f = tmp_path / "a.py"
    f.write_text(header + header + "print(1)\n")

    counts, _ = _run(f)

    assert counts.updated == 1
    content = f.read_text()
    assert content.count(MARKER) == 1


def test_process_file_skips_ignored_type(tmp_path):
    f = tmp_path / "a.py"
    f.write_text("print(1)\n")

    counts, needs_header = _run(f, ignored_types={"python"})

    assert counts.skipped == 1
    assert needs_header == []
    assert f.read_text() == "print(1)\n"


def test_process_file_skips_binary_file(tmp_path):
    f = tmp_path / "a.py"
    f.write_bytes(b"\x00\x01binary-data")

    counts, _ = _run(f)

    assert counts.skipped == 1
    assert f.read_bytes() == b"\x00\x01binary-data"


def test_process_file_skips_undetectable_type(tmp_path):
    f = tmp_path / "noextension"
    f.write_text("just some text\n")

    counts, _ = _run(f)

    assert counts.skipped == 1


def test_process_file_skips_unreadable_file(tmp_path):
    f = tmp_path / "a.py"
    f.write_text("print(1)\n")
    f.chmod(0o000)
    try:
        counts, _ = _run(f)
        assert counts.skipped == 1
    finally:
        f.chmod(0o644)


def test_process_file_forced_type_overrides_detection(tmp_path):
    f = tmp_path / "a.weird"
    f.write_text("puts 1\n")

    _run(f, forced_type="ruby")

    assert f.read_text().splitlines()[0] == "# " + BORDER


def test_process_file_json_prints_jsonc_warning(tmp_path, capsys):
    f = tmp_path / "a.json"
    f.write_text("{}\n")

    _run(f)

    captured = capsys.readouterr()
    assert "JSONC-style" in captured.err
    assert f.read_text().splitlines()[0] == "// " + BORDER


def test_process_file_dry_run_does_not_modify_file(tmp_path):
    f = tmp_path / "a.py"
    original = "print(1)\n"
    f.write_text(original)

    counts, needs_header = _run(f, dry_run=True)

    assert counts.added == 1
    assert needs_header == [f"{f} (Added)"]
    assert f.read_text() == original


# ------------------------------------------------------------------------------
# process_path
# ------------------------------------------------------------------------------


def test_process_path_missing_target_returns_error(tmp_path, capsys):
    rc = process_path(
        str(tmp_path / "missing"), None, set(), False, Counts(), False, []
    )
    assert rc == 1
    assert "does not exist" in capsys.readouterr().err


def test_process_path_file_delegates_to_process_file(tmp_path):
    f = tmp_path / "a.py"
    f.write_text("print(1)\n")
    counts = Counts()

    rc = process_path(str(f), None, set(), False, counts, False, [])

    assert rc == 0
    assert counts.added == 1


def test_process_path_directory_recursive_vs_non_recursive(tmp_path):
    (tmp_path / "sub").mkdir()
    (tmp_path / "sub" / "b.py").write_text("print(2)\n")
    (tmp_path / "a.py").write_text("print(1)\n")

    counts = Counts()
    process_path(str(tmp_path), None, set(), False, counts, False, [])
    assert counts.added == 1  # only a.py

    counts = Counts()
    process_path(str(tmp_path), None, set(), True, counts, False, [])
    assert counts.added == 1  # only sub/b.py remains un-headered


# ------------------------------------------------------------------------------
# main (CLI)
# ------------------------------------------------------------------------------


def test_main_dry_run_exits_nonzero_when_changes_pending(tmp_path, capsys):
    f = tmp_path / "a.py"
    f.write_text("print(1)\n")

    rc = main(["--dry-run", str(f)])

    assert rc == 1
    assert f.read_text() == "print(1)\n"


def test_main_exits_zero_when_nothing_to_do(tmp_path):
    f = tmp_path / "a.py"
    f.write_text("print(1)\n")
    main([str(f)])

    rc = main(["--dry-run", str(f)])

    assert rc == 0


def test_main_nonexistent_path_exits_nonzero(tmp_path, capsys):
    rc = main([str(tmp_path / "missing.py")])
    assert rc == 1
    assert "does not exist" in capsys.readouterr().err


def test_main_ignore_option_comma_separated_and_repeatable(tmp_path):
    py = tmp_path / "a.py"
    rb = tmp_path / "b.rb"
    sh = tmp_path / "c.sh"
    py.write_text("print(1)\n")
    rb.write_text("puts 1\n")
    sh.write_text("echo hi\n")

    main(["--recursive", "--ignore", "python,ruby", str(tmp_path)])

    assert MARKER not in py.read_text()
    assert MARKER not in rb.read_text()
    assert MARKER in sh.read_text()


def test_main_ignore_option_rejects_unknown_type(tmp_path, capsys):
    f = tmp_path / "a.py"
    f.write_text("print(1)\n")

    with pytest.raises(SystemExit) as exc_info:
        main(["--ignore", "not-a-type", str(f)])

    assert exc_info.value.code == 2
    assert "unknown type" in capsys.readouterr().err


def test_main_forced_type_via_cli(tmp_path):
    f = tmp_path / "a.weird"
    f.write_text("puts 1\n")

    main(["--type", "ruby", str(f)])

    assert f.read_text().splitlines()[0] == "# " + BORDER


def test_main_recursive_flag_descends_into_subdirectories(tmp_path):
    (tmp_path / "sub").mkdir()
    nested = tmp_path / "sub" / "b.py"
    nested.write_text("print(2)\n")

    main(["--recursive", str(tmp_path)])

    assert MARKER in nested.read_text()


def test_script_is_executable_and_has_shebang():
    script = Path(__file__).resolve().parents[1] / "copyrighter.py"
    assert script.exists()
    assert os.access(script, os.X_OK)
    with open(script) as fh:
        assert fh.readline().startswith("#!")
