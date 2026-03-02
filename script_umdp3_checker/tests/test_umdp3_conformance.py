import sys
import subprocess
from pathlib import Path
from types import SimpleNamespace
import pytest

# Add the current directory to Python path
sys.path.insert(0, str(Path(__file__).parent.parent))

from umdp3_conformance import StyleChecker, detangle_file_types, \
	ALLOWABLE_FILE_TYPES, GROUP_FILE_TYPES


def test_from_full_list_filters_by_extension():
	changed_files = [
		Path("src/code.F90"),
		Path("src/module.f"),
		Path("src/script.py"),
		Path("README.md"),
	]
	check_functions = {"dummy_check": lambda lines: None}

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
		check_functions={},
		changed_files=changed_files,
	)

	assert checker.files_to_check == changed_files


def test_from_full_list_with_no_changed_files_returns_empty_list():
	checker = StyleChecker.from_full_list(
		name="Empty Checker",
		file_extensions={".py"},
		check_functions={},
		changed_files=[],
	)

	assert checker.files_to_check == []


def test_from_full_list_verbose_output(capsys):
	changed_files = [Path("a.py"), Path("b.F90")]

	StyleChecker.from_full_list(
		name="Verbose Checker",
		file_extensions={".py"},
		check_functions={"dummy_check": lambda lines: None},
		changed_files=changed_files,
		print_volume=5,
	)

	output = capsys.readouterr().out
	assert "StyleChecker initialized using a filtered list:" in output
	assert "Name : Verbose Checker" in output
	assert "Has 1 check commands" in output
	assert "Using 1 file extensions" in output
	assert "Gives 1 files to check." in output


def test_filter_files_with_extensions_only_returns_matches():
	files = [Path("a.py"), Path("b.F90"), Path("c.txt"), Path("d.py")]

	result = StyleChecker.filter_files(files, {".py", ".F90"})

	assert result == [Path("a.py"), Path("b.F90"), Path("d.py")]


def test_filter_files_with_empty_extensions_returns_original_list():
	files = [Path("a.py"), Path("b.F90")]

	result = StyleChecker.filter_files(files, set())

	assert result == files


def test_filter_files_with_no_matches_returns_empty_list():
	files = [Path("a.py"), Path("b.F90")]

	result = StyleChecker.filter_files(files, {".md"})

	assert result == []


def test_create_free_runner_success(monkeypatch):
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


def test_create_free_runner_checker_failure(monkeypatch):
	def fake_run(cmd, capture_output, text, timeout):
		return SimpleNamespace(returncode=2, stdout="", stderr="some stderr")

	monkeypatch.setattr("umdp3_conformance.subprocess.run", fake_run)
	runner = StyleChecker.create_free_runner(["dummy_checker"], "Dummy")

	result = runner(Path("sample.py"))

	assert result.checker_name == "Dummy"
	assert result.failure_count == 1
	assert result.passed is False
	assert result.errors == {"Dummy": "some stderr"}


def test_create_free_runner_timeout(monkeypatch):
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

	assert result == {"Fortran", "Python", "Generic"}
	assert result == GROUP_FILE_TYPES["ALL"]
	assert result == set(ALLOWABLE_FILE_TYPES)



def test_detangle_file_types_mixed_group_and_explicit_type():
	result = detangle_file_types({"CI", "Generic"})

	assert result == {"Fortran", "Python", "Generic"}
	assert result == GROUP_FILE_TYPES["CI"].union({"Generic"})


def test_detangle_file_types_passthrough_without_groups():
	input_types = {"Fortran", "Generic"}

	result = detangle_file_types(set(input_types))

	assert result == input_types


def test_detangle_file_types_raises_for_invalid_type():
	with pytest.raises(ValueError, match="Invalid file types specified"):
		detangle_file_types({"Fortran", "Java"})
