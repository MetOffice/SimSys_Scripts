# -----------------------------------------------------------------------------
# (C) Crown copyright Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
# -----------------------------------------------------------------------------

import sys
import subprocess
from pathlib import Path
from types import SimpleNamespace
import pytest

# Add the current directory to Python path
sys.path.insert(0, str(Path(__file__).parent.parent))

from umdp3_conformance import (
    StyleChecker,
    detangle_file_types,
    ALLOWABLE_FILE_TYPES,
    GROUP_FILE_TYPES,
    Check_Runner,
    get_files_to_check,
    line_1,
    line_2,
    which_cms_is_it,
    ConformanceChecker,
    CheckResult,
    process_arguments,
)


def test_from_full_list_filters_by_extension():
    changed_files = [
        Path("src/code.F90"),
        Path("src/module.f"),
        Path("src/script.py"),
        Path("README.md"),
    ]
    check_functions = []

    checker = StyleChecker.from_full_list(
        name="Fortran Checker",
        file_extensions={".F90", ".f"},
        check_functions=check_functions,
        changed_files=changed_files,
    )

    assert checker.get_name() == "Fortran Checker"
    assert checker.check_functions == check_functions
    assert checker.files_to_check == [Path("src/code.F90"), Path("src/module.f")]


def test_from_full_list_returns_all_files_when_no_extensions():
    changed_files = [Path("a.F90"), Path("b.py"), Path("c.txt")]

    checker = StyleChecker.from_full_list(
        name="Any Checker",
        file_extensions=set(),
        check_functions=[],
        changed_files=changed_files,
    )

    assert checker.files_to_check == changed_files


def test_from_full_list_with_no_changed_files_returns_empty_list():
    checker = StyleChecker.from_full_list(
        name="Empty Checker",
        file_extensions={".py"},
        check_functions=[],
        changed_files=[],
    )

    assert checker.files_to_check == []


def test_filter_files_with_extensions_only_returns_matches():
    files = [Path("a.py"), Path("b.F90"), Path("c.txt"), Path("d.py")]

    result = StyleChecker.filter_files(files, {".py", ".F90"})

    assert result == [Path("a.py"), Path("b.F90"), Path("d.py")]


def test_filter_files_with_empty_extensions_returns_original_list():
    files = [Path("a.py"), Path("b.F90"), Path("c.txt")]

    result = StyleChecker.filter_files(files, set())

    assert result == files


def test_filter_files_with_no_matches_returns_empty_list():
    files = [Path("a.py"), Path("b.F90"), Path("c.txt")]

    result = StyleChecker.filter_files(files, {".md"})

    assert result == []


def test_create_free_runner_success(monkeypatch: pytest.MonkeyPatch):
    """Test that create_free_runner returns a runner that correctly captures the
    output of a successful command. We use monkeypatch to replace subprocess.run with
    a fake function that simulates a successful command execution."""

    def fake_run(cmd, capture_output, text, timeout):
        return SimpleNamespace(returncode=0, stdout="all good", stderr="")

    monkeypatch.setattr("umdp3_conformance.subprocess.run", fake_run)
    runner = StyleChecker.create_free_runner(["dummy_checker", "--flag"], "Dummy")

    result = runner(Path("sample.py"))

    assert result.checker_name == "Dummy"
    assert result.failure_count == 0
    assert result.passed is True
    assert result.output == "all good"
    assert result.errors == {}


def test_create_free_runner_checker_failure(monkeypatch: pytest.MonkeyPatch):
    """As above, but returning an error code"""

    def fake_run(cmd, capture_output, text, timeout):
        return SimpleNamespace(returncode=2, stdout="", stderr="some stderr")

    monkeypatch.setattr("umdp3_conformance.subprocess.run", fake_run)
    runner = StyleChecker.create_free_runner(["dummy_checker"], "Dummy")

    result = runner(Path("sample.py"))

    assert result.checker_name == "Dummy"
    assert result.failure_count == 1
    assert result.passed is False
    assert result.errors == {"Dummy": "some stderr"}


def test_create_free_runner_timeout(monkeypatch: pytest.MonkeyPatch):
    """As above, but simulating a timeout by having the fake subprocess.run raise a
    TimeoutExpired exception."""

    def fake_run(cmd, capture_output, text, timeout):
        raise subprocess.TimeoutExpired(cmd=cmd, timeout=timeout)

    monkeypatch.setattr("umdp3_conformance.subprocess.run", fake_run)
    runner = StyleChecker.create_free_runner(["slow_checker"], "SlowChecker")

    result = runner(Path("sample.py"))

    assert result.checker_name == "SlowChecker"
    assert result.failure_count == 1
    assert result.passed is False
    assert "timed out" in result.output
    assert result.errors == {"SlowChecker": "TimeoutExpired"}


def test_detangle_file_types_expands_ci_group():
    result = detangle_file_types({"CI"})

    assert result == {"Fortran", "Python"}
    assert result == GROUP_FILE_TYPES["CI"]


def test_detangle_file_types_expands_all_group():
    result = detangle_file_types({"ALL"})

    assert result == set(ALLOWABLE_FILE_TYPES)


def test_detangle_file_types_mixed_group_and_explicit_type():
    result = detangle_file_types({"CI", "AnyFile"})

    assert result == GROUP_FILE_TYPES["CI"].union({"AnyFile"})


def test_detangle_file_types_passthrough_without_groups():
    input_types = {"Fortran", "AnyFile"}

    result = detangle_file_types(set(input_types))

    assert result == input_types


def test_detangle_file_types_raises_for_invalid_type():
    with pytest.raises(ValueError, match="Invalid file types specified"):
        detangle_file_types({"Fortran", "Java"})


def test_stylechecker_check_aggregates_results(tmp_path: Path):
    """Test that StyleChecker.check() correctly aggregates results from multiple check functions.
    Here, seen is checking the lines passed to the checks are the ones written by this
    test."""
    file_path = tmp_path / "sample.txt"
    file_path.write_text("a\nb\n")

    seen = []

    def check_pass(lines):
        seen.append(lines)
        return SimpleNamespace(passed=True)

    def check_fail(lines):
        seen.append(lines)
        return SimpleNamespace(passed=False)

    checker = StyleChecker("StyleChecker test", [check_pass, check_fail], [])
    result = checker.check(file_path)

    assert seen == [["a", "b"], ["a", "b"]]
    assert result.file_path == str(file_path)
    assert result.tests_failed == 1
    assert result.all_passed is False
    assert len(result.test_results) == 2


def test_check_runner_check_passes_path_not_lines(tmp_path: Path):
    """Test that Check_Runner.check() passes the file path to the check function, not the file lines. This being it's sole difference from StyleChecker."""
    file_path = tmp_path / "sample.py"
    file_path.write_text("print('x')\n")

    seen = []

    def check_pass(path):
        seen.append(path)
        return SimpleNamespace(passed=True)

    def check_fail(path):
        seen.append(path)
        return SimpleNamespace(passed=False)

    checker = Check_Runner("CheckRunner test", [check_pass, check_fail], [])
    result = checker.check(file_path)

    assert seen == [file_path, file_path]
    assert result.tests_failed == 1
    assert result.all_passed is False


def test_create_external_runners_builds_expected_checkers(
    monkeypatch: pytest.MonkeyPatch,
):
    """Warning! when using monkeypatch, make sure you're testing your own code on not basic Python intrinsics.
    In this case, in order to test the class method, StyleChecker.create_external_runners, we're patching StyleChecker.create_free_runner, which is another static method. In reality StyleChecker.create_free_runner does most of the heavy lifting and StyleChecker.create_external_runners is just a thin wrapper to turn a list (of lists) which form commands into a dictionary of callables."""
    callables = []

    # (fake_)create_free_runner will be called once for each
    # command, and it will receive the command and the generated
    # external_opname. We want to capture those arguments to verify
    # them later, and then return a dummy runner that always
    # returns passed=True.
    def fake_create_free_runner(command, external_opname):
        callables.append((command, external_opname))
        return lambda _: SimpleNamespace(passed=True)

    monkeypatch.setattr(
        StyleChecker, "create_free_runner", staticmethod(fake_create_free_runner)
    )

    checker = StyleChecker.create_external_runners(
        "Python External Checkers",
        commands=[["ruff", "check"], ["black", "--check"]],
        all_files=[Path("a.py"), Path("b.txt"), Path("c.F90"), Path("d.py")],
        file_extensions={".py"},
    )

    # All_files got filtered to only those with 'py' extension.
    assert checker.files_to_check == [Path("a.py"), Path("d.py")]
    # The two commands get given generated 'names'.
    # Check the arguments passed to create_free_runner are as expected.
    assert callables == [
        (["ruff", "check"], "ruff"),
        (["black", "--check"], "black"),
    ]


def test_get_files_to_check_full_check_returns_all_files(tmp_path: Path):
    (tmp_path / "a.txt").write_text("a")
    (tmp_path / "sub").mkdir()
    (tmp_path / "sub" / "b.py").write_text("b")

    result = get_files_to_check(str(tmp_path), full_check=True)

    # Function contract is relative paths when full_check=True
    assert set(result) == {Path("a.txt"), Path("sub/b.py")}


def test_get_files_to_check_passes_path_and_volume_to_cms(
    monkeypatch: pytest.MonkeyPatch,
):
    calls = []

    class FakeCMS:
        def get_changed_files(self):
            return [Path("x.py")]

    def fake_which(path, volume):
        calls.append((path, volume))
        return FakeCMS()

    monkeypatch.setattr("umdp3_conformance.which_cms_is_it", fake_which)

    result = get_files_to_check("repo", full_check=False, print_volume=7)

    assert result == [Path("x.py")]
    assert calls == [("repo", 7)]


def test_get_files_to_check_uses_cms_when_not_fullcheck(
    monkeypatch: pytest.MonkeyPatch,
):
    class FakeCMS:
        def get_changed_files(self):
            return [Path("x.py"), Path("y.F90")]

    monkeypatch.setattr("umdp3_conformance.which_cms_is_it", lambda p, v: FakeCMS())

    result = get_files_to_check("repo", full_check=False, print_volume=0)

    assert result == [Path("x.py"), Path("y.F90")]


def test_which_cms_is_it_git_branch(tmp_path: Path, monkeypatch: pytest.MonkeyPatch):
    """CoPilot suggested this one. We're testing that the check for a ".git" directory
    returned true from a directory we created and put a ".git" directory in..."""
    (tmp_path / ".git").mkdir()

    class FakeGitWrapper:
        def __init__(self, repo_path):
            self.repo_path = repo_path

        def get_branch_name(self):
            return "feature/test"

        def is_branch(self):
            return True

        def get_changed_files(self):
            return [Path("a.py")]

    monkeypatch.setattr("umdp3_conformance.GitBdiffWrapper", FakeGitWrapper)

    cms = which_cms_is_it(str(tmp_path), print_volume=0)

    assert isinstance(cms, FakeGitWrapper)
    assert cms.repo_path == tmp_path


def test_which_cms_is_it_svn_branch(tmp_path: Path, monkeypatch: pytest.MonkeyPatch):
    (tmp_path / ".svn").mkdir()

    class FakeFCMWrapper:
        def __init__(self, repo_path):
            self.repo_path = repo_path

        def get_branch_name(self):
            return "fcm/branch"

        def is_branch(self):
            return True

        def get_changed_files(self):
            return [Path("x.F90")]

    monkeypatch.setattr("umdp3_conformance.FCMBdiffWrapper", FakeFCMWrapper)

    cms = which_cms_is_it(str(tmp_path), print_volume=0)

    assert isinstance(cms, FakeFCMWrapper)
    assert cms.repo_path == tmp_path


def test_which_cms_is_it_raises_for_unknown_cms(tmp_path: Path):
    with pytest.raises(RuntimeError, match="Unknown CMS type"):
        which_cms_is_it(str(tmp_path), print_volume=0)


def test_which_cms_is_it_exits_when_not_branch(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
):
    (tmp_path / ".git").mkdir()

    class FakeGitWrapper:
        def __init__(self, repo_path):
            self.repo_path = repo_path

        def get_branch_name(self):
            return "main"

        def is_branch(self):
            return False

        def get_changed_files(self):
            return []

    monkeypatch.setattr("umdp3_conformance.GitBdiffWrapper", FakeGitWrapper)

    with pytest.raises(SystemExit) as exc:
        which_cms_is_it(str(tmp_path), print_volume=0)
    assert exc.value.code == 0


"""
These last few are CoPilot Specials. I'm not sure how much some are actually testing our
code and how much is testing MonkeyPatch and pytest. Equally some of the latter ones I
might argue aren't really worth having... but we've got them now. Perhaps
not worth maintaining though.
"""


class _FakeFuture:
    def __init__(self, value=None, exc=None):
        self._value = value
        self._exc = exc

    def result(self):
        if self._exc is not None:
            raise self._exc
        return self._value


class _FakeProcessPoolExecutor:
    def __init__(self, max_workers=None):
        self.max_workers = max_workers

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc, tb):
        return False

    def submit(self, fn, *args, **kwargs):
        try:
            return _FakeFuture(value=fn(*args, **kwargs))
        except Exception as e:
            return _FakeFuture(exc=e)


def test_conformance_checker_check_files_collects_all_results(
    monkeypatch: pytest.MonkeyPatch,
):
    class DummyChecker:
        def __init__(self, files):
            self.files_to_check = files

        def check(self, file_path):
            return CheckResult(
                file_path=str(file_path),
                tests_failed=0,
                all_passed=True,
                test_results=[],
            )

    monkeypatch.setattr(
        "umdp3_conformance.concurrent.futures.ProcessPoolExecutor",
        _FakeProcessPoolExecutor,
    )
    monkeypatch.setattr(
        "umdp3_conformance.concurrent.futures.as_completed",
        lambda futures: list(futures),
    )

    checkers = [
        DummyChecker([Path("a.py"), Path("b.py")]),
        DummyChecker([Path("c.py")]),
    ]
    cc = ConformanceChecker(checkers, max_workers=2)  # type: ignore

    cc.check_files()
    # These asserts are basically just checking that the results from the two "dummy" checkers,one of which checked 2 files, are collected into a single list of three results objects.
    assert len(cc.results) == 3
    assert {r.file_path for r in cc.results} == {"a.py", "b.py", "c.py"}


def test_conformance_checker_check_files_propagates_worker_exception(
    monkeypatch: pytest.MonkeyPatch,
):
    class BadChecker:
        def __init__(self):
            self.files_to_check = [Path("bad.py")]

        def check(self, file_path):
            raise RuntimeError("boom")

    monkeypatch.setattr(
        "umdp3_conformance.concurrent.futures.ProcessPoolExecutor",
        _FakeProcessPoolExecutor,
    )
    monkeypatch.setattr(
        "umdp3_conformance.concurrent.futures.as_completed",
        lambda futures: list(futures),
    )

    cc = ConformanceChecker([BadChecker()], max_workers=1)  # type: ignore

    with pytest.raises(RuntimeError, match="boom"):
        cc.check_files()


def test_conformance_checker_print_results_quiet_pass_suppresses_passed(
    capsys: pytest.CaptureFixture[str],
):
    fail_tr = SimpleNamespace(
        checker_name="RuleX", failure_count=1, passed=False, errors={"RuleX": "bad"}
    )
    pass_tr = SimpleNamespace(
        checker_name="RuleY", failure_count=0, passed=True, errors={}
    )

    cc = ConformanceChecker([])
    cc.results = [
        CheckResult("ok.py", tests_failed=0, all_passed=True, test_results=[pass_tr]),  # type: ignore
        CheckResult("bad.py", tests_failed=1, all_passed=False, test_results=[fail_tr]),  # type: ignore
    ]

    all_passed = cc.print_results(print_volume=3, quiet_pass=True)
    out = capsys.readouterr().out
    """That seems to be a lot of effort to create fake results in a fake conformance
    checker just to check the text from the 'passed' test wasnt in the output written"""
    assert all_passed is False
    assert "bad.py" in out
    assert "ok.py" not in out


def test_conformance_checker_print_results_volume4_prints_error_details(
    capsys: pytest.CaptureFixture[str],
):
    fail_tr = SimpleNamespace(
        checker_name="RuleZ", failure_count=2, passed=False, errors={"RuleZ": "detail"}
    )

    cc = ConformanceChecker([])
    cc.results = [
        CheckResult("bad.py", tests_failed=1, all_passed=False, test_results=[fail_tr])  # type: ignore
    ]

    cc.print_results(print_volume=4, quiet_pass=True)
    out = capsys.readouterr().out
    """Again, that seems to be a lot of effort to create fake results in a fake
    conformance checker just to check the detail text from the test was in the output
    written"""
    assert "RuleZ" in out
    assert "detail" in out


def test_stylechecker_check_with_no_check_functions_passes(tmp_path: Path):
    # CoPilot described this as an 'edge case'. I'm not sure it shouldn't be
    # checked for in the code and a PEBCAK error raised.
    file_path = tmp_path / "sample.txt"
    file_path.write_text("content\n")

    checker = StyleChecker("EmptyChecks", [], [file_path])
    result = checker.check(file_path)

    assert result.file_path == str(file_path)
    assert result.tests_failed == 0
    assert result.all_passed is True
    assert result.test_results == []


def test_create_free_runner_generic_exception(monkeypatch: pytest.MonkeyPatch):
    def fake_run(cmd, capture_output, text, timeout):
        raise OSError("cannot execute")

    monkeypatch.setattr("umdp3_conformance.subprocess.run", fake_run)
    runner = StyleChecker.create_free_runner(["dummy_checker"], "Dummy")

    result = runner(Path("sample.py"))

    assert result.checker_name == "Dummy"
    assert result.failure_count == 1
    assert result.passed is False
    assert result.errors == {"Dummy": "cannot execute"}


def test_create_external_runners_with_empty_commands():
    # Again, is this something we should be ensuring works, or testing for in the code
    # to raise a PEBCAK error if this is ever the case? Although it's a 'coder error'
    # not a user error if it does.
    checker = StyleChecker.create_external_runners(
        "Noop External",
        commands=[],
        all_files=[Path("a.py"), Path("b.py")],
        file_extensions={".py"},
    )

    assert checker.files_to_check == [Path("a.py"), Path("b.py")]
    assert checker.check_functions == []


# Okay, c'mon CoPilot - where's the value in testing a tiny function to write a string
# of characters from a runtime supplied length is the length you asked for ?
def test_line_helpers_basic_shapes():
    assert line_1(0) == ""
    assert len(line_1(80)) == 80
    assert line_2(5) == "-----"


# hmmmm, isn't this really just testing that argparse is doing what we expect it to do
# with the arguments we give it. Again, is this something we should be testing for, or
# just ensuring that our code is using argparse correctly and trusting that argparse
# itself works as advertised ?
def test_process_arguments_defaults(monkeypatch: pytest.MonkeyPatch):
    monkeypatch.setattr(sys, "argv", ["umdp3_conformance.py"])

    args = process_arguments()
    # All the below are the defaults assigned if no args were given...
    assert args.path == "./"
    assert args.max_workers == 2
    assert args.fullcheck is False
    assert args.printpass is False
    assert args.volume == 3
    assert args.file_types == {"Fortran"}


def test_process_arguments_verbose_and_filetype_group_expansion(
    monkeypatch: pytest.MonkeyPatch,
):
    monkeypatch.setattr(
        sys,
        "argv",
        ["umdp3_conformance.py", "--file-types", "CI", "AnyFile", "-vv"],
    )

    args = process_arguments()

    assert args.volume == 5
    assert args.file_types == {"Fortran", "Python", "AnyFile"}


def test_process_arguments_rejects_verbose_and_quiet_together(
    monkeypatch: pytest.MonkeyPatch,
):
    monkeypatch.setattr(sys, "argv", ["umdp3_conformance.py", "-v", "-q"])
    with pytest.raises(SystemExit):
        process_arguments()
