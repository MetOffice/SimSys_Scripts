#!/usr/bin/env python3
# -----------------------------------------------------------------------------
# (C) Crown copyright Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
# -----------------------------------------------------------------------------
"""copyrighter - insert or update the Met Office Crown copyright header in a
file, or in every file under a directory.

Standalone Python replica of sbin/copyrighter (bash). Can be run directly
(it is executable and carries its own shebang) or via `python3 copyrighter.py`.
"""

from __future__ import annotations

import argparse
import os
import re
import stat
import sys
import tempfile
from pathlib import Path

BORDER = "-" * 77
MARKER = "Crown copyright Met Office"

# version-control directories that are always pruned from directory walks
VCS_DIRS = frozenset({".git", ".svn"})

EXTENSION_TO_TYPE: dict[str, str] = {
    "awk": "awk",
    "bash": "shell", "ksh": "shell", "sh": "shell",
    "c++": "cpp", "cc": "cpp", "cpp": "cpp", "cxx": "cpp",
    "h++": "cpp", "hh": "cpp", "hpp": "cpp", "hxx": "cpp",
    "c": "c", "h": "c",
    "f90": "fortran", "f95": "fortran", "f03": "fortran", "f08": "fortran",
    "f": "fortran77", "f77": "fortran77", "for": "fortran77",
    "go": "go",
    "java": "java",
    "js": "js", "cjs": "js", "mjs": "js",
    "json": "json",
    "markdown": "markdown", "md": "markdown",
    "pl": "perl", "pm": "perl",
    "ps1": "powershell", "psm1": "powershell", "psd1": "powershell",
    "ps": "postscript", "eps": "postscript",
    "py": "python",
    "r": "r",
    "rb": "ruby",
    "rs": "rust",
    "rst": "rst",
    "tex": "latex",
    "toml": "toml",
    "yaml": "yaml", "yml": "yaml",
}

# comment-prefix per type; "c", "rst" and "markdown" get extra wrapping below
COMMENT_PREFIX: dict[str, str] = {
    "python": "#", "shell": "#", "yaml": "#", "perl": "#", "toml": "#",
    "ruby": "#", "awk": "#", "powershell": "#", "r": "#",
    "fortran": "!",
    "fortran77": "C",  # fixed-form F77: marker must be in column 1
    "cpp": "//", "json": "//", "java": "//", "rust": "//", "go": "//", "js": "//",
    "latex": "%", "postscript": "%",
    "c": " *",
    "rst": "  ",
    "markdown": "",
}

SUPPORTED_TYPES = tuple(sorted(set(COMMENT_PREFIX)))

SYM = {
    "added": "+", "updated": "~", "unchanged": "=",
    "warn": "!", "info": "i", "error": "\u2717", "summary": "\u00bb",
}


# per-type separator between the comment prefix and the header text; only
# fortran77 (column 1 marker + 5 spaces, matching the classic column-7
# statement field) and markdown (no prefix at all) differ from a single space
COMMENT_SEP: dict[str, str] = {"fortran77": "     ", "markdown": ""}


def generate_header(file_type: str) -> list[str]:
    """Return the comment-style-appropriate copyright block for file_type."""
    prefix = COMMENT_PREFIX[file_type]
    sep = COMMENT_SEP.get(file_type, " ")
    lines = [
        f"{prefix}{sep}{BORDER}",
        f"{prefix}{sep}(C) Crown copyright Met Office. All rights reserved.",
        (
            f"{prefix}{sep}The file LICENCE, distributed with this code, contains "
            "details of the terms"
        ),
        f"{prefix}{sep}under which the code may be used.",
        f"{prefix}{sep}{BORDER}",
    ]
    if file_type == "rst":
        lines[0] = f".. {BORDER}"
        lines[4] = f"   {BORDER}"
    elif file_type == "markdown":
        lines = ["<!--", *lines, "-->"]
    elif file_type == "c":
        lines = ["/*", *lines, " */"]
    return lines


def _extension(name: str) -> str:
    base = os.path.basename(name)
    if "." not in base:
        return ""
    ext = base.rsplit(".", 1)[1]
    return "" if ext == base else ext.lower()


def _shebang_type(first_line: str) -> str:
    if not first_line.startswith("#!"):
        return ""
    if "python" in first_line:
        return "python"
    if "perl" in first_line:
        return "perl"
    if "ruby" in first_line:
        return "ruby"
    if first_line.endswith("bash") or first_line.endswith("/sh") or first_line.endswith("ksh"):
        return "shell"
    return ""


def detect_type(name: str, first_line: str) -> str:
    """Detect a file's type from its extension, falling back to the shebang."""
    file_type = EXTENSION_TO_TYPE.get(_extension(name))
    return file_type if file_type else _shebang_type(first_line)


def split_lines(text: str) -> list[str]:
    """Split on '\\n' only (matching bash `mapfile`), dropping one trailing
    empty element caused by a final newline, never the file content itself."""
    if text == "":
        return []
    text = text.removesuffix("\n")
    return text.split("\n")


def collect_files(directory: Path, recursive: bool) -> list[Path]:
    """List regular files under directory, excluding VCS_DIRS and symlinks
    (matching the `find`/`fd -type f` semantics used by the bash original)."""
    results: list[Path] = []
    if recursive:
        for root, dirnames, filenames in os.walk(directory):
            dirnames[:] = [d for d in dirnames if d not in VCS_DIRS]
            for fname in filenames:
                full = Path(root) / fname
                if not full.is_symlink() and full.is_file():
                    results.append(full)
    else:
        for entry in directory.iterdir():
            if not entry.is_symlink() and entry.is_file():
                results.append(entry)
    results.sort(key=str)
    return results


class Counts:
    def __init__(self) -> None:
        self.added = 0
        self.updated = 0
        self.unchanged = 0
        self.skipped = 0


def atomic_write(path: Path, data: bytes) -> None:
    """Write data to path atomically, preserving the original file's exact
    permission bits, using a securely-created (O_EXCL) temp file to avoid a
    symlink race on the temporary path."""
    mode = stat.S_IMODE(path.stat().st_mode)
    fd, tmp_name = tempfile.mkstemp(dir=path.parent, prefix=f".{path.name}.copyrighter.")
    try:
        with os.fdopen(fd, "wb") as fh:
            fh.write(data)
        os.chmod(tmp_name, mode)
        os.replace(tmp_name, path)
    except BaseException:
        try:
            os.unlink(tmp_name)
        except OSError:
            pass
        raise


def process_file(
    path: Path,
    forced_type: str | None,
    ignored_types: set[str],
    counts: Counts,
    dry_run: bool,
    needs_header: list[str],
) -> None:
    try:
        raw = path.read_bytes()
    except OSError as exc:
        print(f"copyrighter: {SYM['warn']} skipping '{path}' (cannot read: {exc.strerror})", file=sys.stderr)
        counts.skipped += 1
        return

    if b"\x00" in raw:
        print(f"copyrighter: {SYM['warn']} skipping '{path}' (binary file)", file=sys.stderr)
        counts.skipped += 1
        return

    text = raw.decode("utf-8", errors="surrogateescape")
    lines = split_lines(text)
    first_line = lines[0] if lines else ""

    file_type = forced_type or detect_type(str(path), first_line)
    if not file_type:
        print(f"copyrighter: {SYM['warn']} skipping '{path}' (unable to determine file type, use --type)", file=sys.stderr)
        counts.skipped += 1
        return

    if file_type in ignored_types:
        print(f"copyrighter: {SYM['info']} skipping '{path}' (ignored type '{file_type}')", file=sys.stderr)
        counts.skipped += 1
        return

    if file_type == "json":
        print(f"copyrighter: {SYM['info']} JSON has no comment syntax; '{path}' will become JSONC-style", file=sys.stderr)

    header_lines = generate_header(file_type)
    header_count = len(header_lines)
    nlines = len(lines)

    offset = 0
    if first_line.startswith("#!"):
        offset = 1
    elif file_type == "postscript" and first_line.startswith("%!"):
        offset = 1  # preserve the required %!PS-Adobe magic line

    marker_rel = next((i for i, hl in enumerate(header_lines) if MARKER in hl), -1)
    marker_prefix = header_lines[marker_rel].split("(C) Crown copyright", 1)[0]
    marker_re = re.compile(
        re.escape(marker_prefix) + r"\(C\) Crown copyright Met Office(?: \d{4})?\. All rights reserved\."
    )

    search_limit = min(offset + 100, nlines)

    # scan for every header-like block near the top of the file (not just the
    # first) so stale duplicates are all collapsed into a single canonical header
    block_starts: list[int] = []
    block_ends: list[int] = []
    i = offset
    while i < search_limit:
        if marker_re.fullmatch(lines[i]):
            candidate_start = i - marker_rel
            candidate_end = candidate_start + header_count - 1
            if candidate_start >= offset and candidate_end < nlines:
                block_starts.append(candidate_start)
                block_ends.append(candidate_end)
                i = candidate_end + 1
                continue
        i += 1

    action = "Added"
    if len(block_starts) == 1:
        start = block_starts[0]
        action = "Unchanged" if lines[start:start + header_count] == header_lines else "Updated"
    elif len(block_starts) > 1:
        action = "Updated"

    if action == "Unchanged":
        print(f"copyrighter: {SYM['unchanged']} Unchanged {file_type} header in '{path}' (already up to date)")
        counts.unchanged += 1
        return

    # rebuild the file's content with every detected header block removed
    tail: list[str] = []
    idx = offset
    block_idx = 0
    while idx < nlines:
        if block_idx < len(block_starts) and idx == block_starts[block_idx]:
            idx = block_ends[block_idx] + 1
            block_idx += 1
            continue
        tail.append(lines[idx])
        idx += 1
    while tail and tail[0] == "":
        tail.pop(0)

    new_lines = lines[:offset] + header_lines + [""] + tail

    symbol = SYM["updated"] if action == "Updated" else SYM["added"]
    needs_header.append(f"{path} ({action})")
    if action == "Added":
        counts.added += 1
    else:
        counts.updated += 1

    if dry_run:
        print(f"copyrighter: {symbol} [dry-run] {action} {file_type} header in '{path}'")
        return

    new_text = "\n".join(new_lines) + "\n"
    atomic_write(path, new_text.encode("utf-8", errors="surrogateescape"))

    print(f"copyrighter: {symbol} {action} {file_type} header in '{path}'")


def process_path(
    target: str,
    forced_type: str | None,
    ignored_types: set[str],
    recursive: bool,
    counts: Counts,
    dry_run: bool,
    needs_header: list[str],
) -> int:
    path = Path(target)

    if path.is_file():
        process_file(path, forced_type, ignored_types, counts, dry_run, needs_header)
        return 0

    if path.is_dir():
        for f in collect_files(path, recursive):
            process_file(f, forced_type, ignored_types, counts, dry_run, needs_header)
        return 0

    print(f"copyrighter: {SYM['error']} '{target}' does not exist", file=sys.stderr)
    return 1


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="copyrighter.py",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        description=(
            "Insert a Met Office Crown copyright header at the top of a file "
            "(after any shebang line), or into every file under a directory. "
            "If a copyrighter header is already present it is updated in "
            "place rather than duplicated."
        ),
        epilog=(
            "File types are auto-detected from the file extension, falling back\n"
            "to the shebang line for extension-less scripts. JSON has no native\n"
            'comment syntax, so headers are inserted as "//" line comments\n'
            "(JSONC-style); a warning is printed whenever this happens.\n\n"
            "Exits non-zero if any file needed a header added or updated, or if\n"
            "a path could not be processed - useful for CI checks with --dry-run."
        ),
    )
    parser.add_argument("paths", nargs="+", metavar="file-or-directory")
    parser.add_argument(
        "-t", "--type", dest="forced_type", choices=SUPPORTED_TYPES, metavar="TYPE",
        help="Force the file type instead of auto-detecting it.",
    )
    parser.add_argument(
        "-i", "--ignore", dest="ignore", action="append", default=[], metavar="TYPE",
        help="Skip files whose (auto-detected or forced) type matches. Accepts "
        "a comma-separated list and may be given more than once.",
    )
    parser.add_argument(
        "-r", "--recursive", "--full", dest="recursive", action="store_true",
        help="When a directory is given, descend into every nested subdirectory.",
    )
    parser.add_argument(
        "-n", "--dry-run", "--check", dest="dry_run", action="store_true",
        help="Report what would change without modifying any file.",
    )
    return parser


def main(argv: list[str] | None = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)

    ignored_types: set[str] = set()
    for group in args.ignore:
        for item in group.split(","):
            item = item.strip()
            if not item:
                continue
            if item not in SUPPORTED_TYPES:
                parser.error(f"unknown type '{item}' (expected one of: {' '.join(SUPPORTED_TYPES)})")
            ignored_types.add(item)

    counts = Counts()
    needs_header: list[str] = []
    rc = 0

    for target in args.paths:
        if process_path(target, args.forced_type, ignored_types, args.recursive, counts, args.dry_run, needs_header) != 0:
            rc = 1

    if needs_header:
        print(f"copyrighter: {SYM['summary']} files needing a copyright header:")
        for entry in needs_header:
            print(f"  - {entry}")
        if args.dry_run:
            rc = 1

    print()
    print("  Run the script without -n|--dry-run|--check to actually apply changes.")
    print()
    print(
        f"copyrighter: {SYM['summary']} added {counts.added}, updated {counts.updated}, "
        f"unchanged {counts.unchanged}, skipped {counts.skipped}"
    )
    return rc


if __name__ == "__main__":
    sys.exit(main())
