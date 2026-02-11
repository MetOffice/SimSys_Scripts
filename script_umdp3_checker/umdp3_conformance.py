import subprocess
from abc import ABC, abstractmethod
from pathlib import Path
from typing import Callable, List, Dict, Set
from dataclasses import dataclass, field
import argparse
from checker_dispatch_tables import CheckerDispatchTables
from umdp3_checker_rules import TestResult
import concurrent.futures

# Add custom modules to Python path if needed
# Add the repository root to access fcm_bdiff and git_bdiff packages
import sys
sys.path.insert(0, str(Path(__file__).parent.parent))

"""
Framework and Classes to generate a list of files to check for style
conformance, and to run relevant style checkers on those files.
"""


@dataclass
class CheckResult:
    """
    Docstring for CheckResult
        A class to hold the results of running a style checker on a file.
        It contains the file path, the number of tests failed, whether all
        tests passed, and a list of TestResult objects for each test run on
        that file.
    """
    """TODO : Might be better to store number of tests run, and number passed,
    rather than just number failed and whether all passed."""
    file_path: str = "No file provided"
    tests_failed: int = 0
    all_passed: bool = False
    test_results: List[TestResult] = field(default_factory=list)


class CMSSystem(ABC):
    """Abstract base class for CMS systems like git or FCM."""

    @abstractmethod
    def get_changed_files(self) -> List[Path]:
        """Get list of files changed between base_branch and branch."""
        pass

    @abstractmethod
    def is_branch(self) -> bool:
        """Check if we're looking at a branch"""
        pass

    @abstractmethod
    def get_branch_name(self) -> str:
        """Get the current branch name."""
        pass


class GitBdiffWrapper(CMSSystem):
    """Wrapper around git_bdiff to get changed files."""

    def __init__(self, repo_path: Path = Path(".")):
        from github_scripts import git_bdiff

        self.repo_path = repo_path
        self.bdiff_obj = git_bdiff.GitBDiff(repo=self.repo_path)
        self.info_obj = git_bdiff.GitInfo(repo=self.repo_path)

    def get_changed_files(self) -> List[Path]:
        """Get list of files changed between base_branch and branch."""
        return [Path(f) for f in self.bdiff_obj.files()]

    def is_branch(self) -> bool:
        """Check if we're looking at a branch"""
        is_a_branch = not self.info_obj.is_main()
        return is_a_branch

    def get_branch_name(self) -> str:
        """Get the current branch name."""
        return self.info_obj.branch


class FCMBdiffWrapper(CMSSystem):
    """Wrapper around fcm_bdiff to get changed files."""

    def __init__(self, repo_path: Path = Path(".")):
        from fcm_bdiff import fcm_bdiff

        self.repo_path = repo_path
        self.bdiff_obj = fcm_bdiff.FCMBDiff(repo=self.repo_path)

    def get_changed_files(self) -> List[Path]:
        """Get list of files changed between base_branch and branch."""
        return [Path(f) for f in self.bdiff_obj.files()]

    def is_branch(self) -> bool:
        """Check if we're looking at a branch"""
        return self.bdiff_obj.is_branch

    def get_branch_name(self) -> str:
        """Get the current branch name."""
        return self.bdiff_obj.branch


class StyleChecker(ABC):
    """Abstract base class for style checkers."""

    """
    TODO: This is where it might be good to set up a threadsafe
        class instance to hold the 'expanded' check outputs.
        One for each file being checked in parallel.
        Curently the UMDP3 class holds "_extra_error_info" which
        was used to provide more detailed error logging.
        However, this is not threadsafe, so in a multithreaded
        environment, the extra error info could get mixed up between
        different files being checked in parallel.
        For now, I've modified the UMDP3 class methods to return
        a TestResult object directly, which includes the extra error
        info, so that each thread can work independently."""
    name: str
    file_extensions: Set[str]
    check_functions: Dict[str, Callable]
    files_to_check: List[Path]

    def __init__(
        self,
        name: str,
        file_extensions: Set[str],
        check_functions: Dict[str, Callable],
        changed_files: List[Path] = [],
    ):
        self.name = name
        self.file_extensions = file_extensions or set()
        self.check_functions = check_functions or {}
        self.files_to_check = (
            self.filter_files(changed_files, self.file_extensions)
            if changed_files
            else []
        )

    @abstractmethod
    def get_name(self) -> str:
        """Return the name of this checker."""
        pass

    @abstractmethod
    def check(self, file_path: Path) -> CheckResult:
        """Run the style checker on a file."""
        pass

    @classmethod
    def from_full_list(
        cls,
        name: str,
        file_extensions: Set[str],
        check_functions: Dict[str, Callable],
        all_files: List[Path],
    ) -> "StyleChecker":
        """Create a StyleChecker instance filtering files from a full list."""
        filtered_files = cls.filter_files(all_files, file_extensions)
        return cls(name, file_extensions, check_functions, filtered_files)

    @staticmethod
    def filter_files(
        files: List[Path], file_extensions: Set[str] = set()
    ) -> List[Path]:
        """Filter files based on the checker's file extensions."""
        if not file_extensions:
            return files
        return [f for f in files if f.suffix in file_extensions]


class UMDP3_checker(StyleChecker):
    """UMDP3 built-in style checker."""

    files_to_check: List[Path]

    def __init__(
        self,
        name: str,
        file_extensions: Set[str],
        check_functions: Dict[str, Callable],
        changed_files: List[Path] = [],
        print_volume: int = 3,
    ):
        self.name = name
        self.file_extensions = file_extensions or set()
        self.check_functions = check_functions or {}
        self.files_to_check = (
            super().filter_files(changed_files, self.file_extensions)
            if changed_files
            else []
        )
        if print_volume >= 5:
            print(f"UMDP3_checker initialized :\n"
                  f"    Name : {self.name}\n"
                  f"    Has {len(self.check_functions)} check functions\n"
                  f"    Using {len(self.file_extensions)} file extensions\n"
                  f"    Gives {len(self.files_to_check)} files to check.")

    def get_name(self) -> str:
        return self.name

    def check(self, file_path: Path) -> CheckResult:
        """Run UMDP3 check function on file."""
        lines = file_path.read_text().splitlines()
        file_results = []  # list of TestResult objects
        for check_name, check_function in self.check_functions.items():
            file_results.append(check_function(lines))

        tests_failed = sum([0 if result.passed else 1 for result in
                            file_results])
        return CheckResult(
            file_path=str(file_path),
            tests_failed=tests_failed,
            all_passed=tests_failed == 0,
            test_results=file_results,
        )


class ExternalChecker(StyleChecker):
    """Wrapper for external style checking tools."""

    """
    TODO : This is overriding the 'syle type hint from the base class.
    As we're currently passing in a list of strings to pass to 'subcommand'.
    Ideally we should be making callable functions for each check, but that
    would require more refactoring of the code.
    Is that a 'factory' method?"""
    check_commands: Dict[str, List[str]]

    def __init__(
        self,
        name: str,
        file_extensions: Set[str],
        check_functions: Dict[str, List[str]],
        changed_files: List[Path],
        print_volume: int = 3,
    ):
        self.name = name
        self.file_extensions = file_extensions or set()
        self.check_commands = check_functions or {}
        self.files_to_check = (
            super().filter_files(changed_files, self.file_extensions)
            if changed_files
            else []
        )
        if print_volume >= 5:
            print(f"ExternalChecker initialized :\n"
                  f"    Name : {self.name}\n"
                  f"    Has {len(self.check_commands)} check commands\n"
                  f"    Using {len(self.file_extensions)} file extensions\n"
                  f"    Gives {len(self.files_to_check)} files to check.")

    def get_name(self) -> str:
        return self.name

    def check(self, file_path: Path) -> CheckResult:
        """Run external checker commands on file."""
        file_results = []
        tests_failed = 0
        for test_name, command in self.check_commands.items():
            try:
                cmd = command + [str(file_path)]
                result = subprocess.run(cmd, capture_output=True,
                                        text=True, timeout=60)
            except subprocess.TimeoutExpired:
                file_results.append(
                    TestResult(
                        checker_name=test_name,
                        failure_count=1,
                        passed=False,
                        output=f"Checker {test_name} timed out",
                        errors={test_name: "TimeoutExpired"},
                    )
                )
                tests_failed += 1
            except Exception as e:
                file_results.append(
                    TestResult(
                        checker_name=test_name,
                        failure_count=1,
                        passed=False,
                        output=str(e),
                        errors={test_name: str(e)},
                    )
                )
                tests_failed += 1
            else:
                error_text = result.stderr if result.stderr else ""
                file_results.append(
                    TestResult(
                        checker_name=test_name,
                        failure_count=0 if result.returncode == 0 else 1,
                        passed=result.returncode == 0,
                        output=result.stdout,
                        errors={test_name: error_text} if error_text else {},
                    )
                )
                if result.returncode != 0:
                    tests_failed += 1
        return CheckResult(
            file_path=str(file_path),
            tests_failed=tests_failed,
            all_passed=tests_failed == 0,
            test_results=file_results,
        )


class ConformanceChecker:
    """Main framework for running style checks in parallel."""

    def __init__(
        self,
        checkers: List[StyleChecker],
        max_workers: int = 8,
    ):
        self.checkers = checkers
        self.max_workers = max_workers

    def check_files(self) -> None:
        """Run all checkers on given files in parallel.
        ========================================================
        Note :
        Each checker runs on its own set of files, and has a list of
        appropriate checkers for that file type.
        The loop to create the threads currently creates a thread for each
        (checker, file) pair, which may not be optimal.
        However, given that the number of files is likely to be small,
        and the number of checkers is also small, this should be acceptable
        for now.
        """
        """
        TODO : Might be good to have a threadsafe object for each file and
        allow multiple checks to be run at once on that file.
        """
        """
        TODO : Poor terminology makes discerning what is actually happening
        here hard work. A 'checker' is an instance of a StyleChecker
        (sub)class. Each of which has a list of checks to perform and a list of
        files to perform them on. e.g. A UMDP3_checker for Fortran files. or
        the ExternalChecker for Python files.
        However, when the 'results' are collected, each result is for a single
        file+check pair and holds no information about which 'checker' it was
        part of. Thus some files can be checked by multiple checkers, and the
        filename will appear multiple times in the output. Not Good!
        """
        results = []
        # print(f"About to use {len(self.checkers)} checkers")
        with concurrent.futures.ThreadPoolExecutor(
            max_workers=self.max_workers
        ) as executor:
            future_to_task = {
                executor.submit(checker.check, file_path): file_path
                for checker in self.checkers
                for file_path in checker.files_to_check
            }

            for future in concurrent.futures.as_completed(future_to_task):
                result = future.result()
                # print(f"Completed check for file: {result}")
                results.append(result)
        self.results = results
        return

    def print_results(self, print_volume: int = 3,
                      quiet_pass: bool = True) -> bool:
        """Print results and return True if all checks passed.
        ========================================================"""
        """
        TODO: If an object encapsulating the data for each file is created
        it should contain the "in depth" printing method for file data.
        With this method presenting the summary and then looping over
        each file object to print its details at the desired verbosity."""
        all_passed = True
        for result in self.results:
            file_status = "✓ PASS" if result.all_passed else "✗ FAIL"
            # Lousy variable names here: 'result' is the CheckResult for a file
            # which had multiple tests, so result.all_passed is for that file.
            all_passed = all_passed and result.all_passed
            # verbosity level 4 overides quiet_pass for file summary.
            if quiet_pass and result.all_passed and print_volume < 4:
                continue
            print(f"{file_status:7s} file : {result.file_path:50s}")
            if print_volume >= 3 and not result.all_passed:
                print(" " * 4 + line_2(86))
            for test_result in result.test_results:
                if print_volume < 5 and test_result.passed:
                    continue
                if print_volume >= 3 and not test_result.passed:
                    plural = "" if test_result.failure_count == 1 else "s"
                    print(f"     {test_result.checker_name:60s} : Found "
                          + f"{test_result.failure_count:3} failure{plural}.")
                    if test_result.errors and print_volume >= 4:
                        print(" " * 8 + line_2(82))
                        for count, (title, info) in enumerate(
                            test_result.errors.items()
                        ):
                            print(" " * 8 +
                                  f"{count + 1:2} : {title} : {info}")
                        print(" " * 8 + line_2(82))
                elif print_volume >= 3:
                    print(f"     {test_result.checker_name:60s} : ✓ PASS")
            if print_volume >= 3 and not result.all_passed:
                print(" " * 4 + line_2(86))
        return all_passed


def process_arguments():
    """Process command line arguments.
    Somewhat a work in progress, but it's going to be needed eventually."""
    parser = argparse.ArgumentParser(
        prog="umdp3_conformance.py",
        description="""UMDP3 Conformance Checker""",
        epilog="T-T-T-T-That's all folks !!",
    )
    parser.add_argument(
        "-f",
        "--file-types",
        type=str,
        nargs="+",
        choices=["Fortran", "Python", "Generic"],
        default=["Fortran"],
        help="File types to check, comma-separated",
    )
    """
    TODO : I /think/ the old version also checked '.h' files as Fortran.
        Not sure if that is still needed."""
    parser.add_argument(
        "-p", "--path", type=str, default="./", help="path to repository"
    )
    parser.add_argument(
        "--max-workers", type=int, default=8,
        help="Maximum number of parallel workers"
    )
    parser.add_argument(
        "--fullcheck", action="store_true",
        help="Instead of just checking changed files, check all files in "
             "the repository"
    )
    parser.add_argument(
        "--printpass", action="store_true",
        help="Print details of passed checks as well as failed ones.\n"
             "By default, only failed checks are printed in detail."
    )
    group = parser.add_mutually_exclusive_group()
    group.add_argument(
        "-v", "--verbose", action="count", default=0,
        help="Increase output verbosity"
    )
    group.add_argument(
        "-q", "--quiet", action="count", default=0,
        help="Decrease output verbosity"
    )
    # The following are not yet implemented, but may become useful
    # branch and base branch could be used to configure the CMS diff
    # if/when git_bdiff is changed to override those values.
    # parser.add_argument("--branch", type=str, default="HEAD",
    #                     help="Branch to check")
    # parser.add_argument("--base-branch", type=str, default="main",
    #                     help="Base branch for comparison")
    # parser.add_argument("--checker-configs", type=str, default=None,
    #                     help="Checker configuration file")
    args = parser.parse_args()
    # Determine output verbosity level
    args.volume = 3 + args.verbose - args.quiet
    return args


def line_1(length: int = 80) -> str:
    """Helper function to print a line for separating output sections."""
    repeats = length // 3
    pads = length % 3
    line = ""
    if pads > 1:
        line += "="
    line += "-=-" * repeats
    if pads > 0:
        line += "="
    return line


def line_2(length: int = 80) -> str:
    """Helper function to print a line for separating output sections."""
    return "-" * length


def which_cms_is_it(path: str, print_volume: int = 3) -> CMSSystem:
    """Determine which CMS is in use based on the presence of certain files."""
    repo_path = Path(path)
    if (repo_path / ".git").is_dir():
        cms = GitBdiffWrapper(repo_path)
    elif (repo_path / ".svn").is_dir():
        """
        TODO : If we still want this to work reliably with FCM, it will need
        to also accept URLs and not just local paths."""
        cms = FCMBdiffWrapper(repo_path)
    else:
        raise RuntimeError("Unknown CMS type at path: " + str(path))
    branch_name = cms.get_branch_name()
    if not cms.is_branch():
        # TODO : This /might/ be better as a raise ValueError to allow
        # printing the help message, but for now just print and exit.
        print(
            f"The path {path} is not a branch."
            f"\nReported branch name is : {branch_name}"
            "\nThe meaning of differences is unclear, and so"
            " checking is aborted.\n"
            f"Please try switching on the full check option"
        )
        # Soft exit mainly so nightly testing on main doesn't flag failure.
        exit(0)
    else:
        if print_volume >= 2:
            print(f"Found branch, {branch_name}, at path {path}.")
        if print_volume >= 4:
            print("The files changed on this branch are:")
            changed_files = cms.get_changed_files()
            no_of_changed_files = len(changed_files)
            extras = no_of_changed_files - 10
            if no_of_changed_files > 10:
                changed_files = changed_files[:10]
            for changed_file in changed_files:
                print(f"    {changed_file}")
            if no_of_changed_files > 10:
                print(f"    ... and {extras} more changed files.")
    return cms


def create_style_checkers(
    file_types: List[str], changed_files: List[Path],
    print_volume: int = 3
) -> List[StyleChecker]:
    """Create style checkers based on requested file types."""
    dispatch_tables = CheckerDispatchTables()
    checkers = []
    if "Fortran" in file_types:
        file_extensions = {".f", ".for", ".f90", ".f95",
                           ".f03", ".f08", ".F90"}
        fortran_diff_table = dispatch_tables.get_diff_dispatch_table_fortran()
        fortran_file_table = dispatch_tables.get_file_dispatch_table_fortran()
        generic_file_table = dispatch_tables.get_file_dispatch_table_all()
        if print_volume >= 3:
            print("Configuring Fortran checkers:")
        combined_checkers = fortran_diff_table | fortran_file_table | \
            generic_file_table
        fortran_file_checker = UMDP3_checker.from_full_list(
            "Fortran Checker", file_extensions,
            combined_checkers, changed_files
        )
        checkers.append(fortran_file_checker)
    if "Python" in file_types:
        if print_volume >= 3:
            print("Configuring External Python checkers:")
        file_extensions = {".py"}
        python_checkers = {
            # "flake 8":     ["flake8", "-q"],
            # "black":       ["black", "--check"],
            # "pylint":      ["pylint", "-E"],
            "ruff":        ["ruff", "check"],
        }
        python_file_checker = ExternalChecker(
            "External Python Checkers", file_extensions,
            python_checkers, changed_files
        )
        checkers.append(python_file_checker)
    if "Generic" in file_types or file_types == []:
        if print_volume >= 3:
            print("Configuring Generic File Checkers:")
        all_file_dispatch_table = dispatch_tables.get_file_dispatch_table_all()
        generic_checker = UMDP3_checker(
            "Generic File Checker", set(), all_file_dispatch_table,
            changed_files
        )
        checkers.append(generic_checker)

    return checkers


def get_files_to_check(path: str, full_check: bool,
                       print_volume: int = 3) -> List[Path]:
    """
    Docstring for get_files_to_check : A routine to get the list of files to
    check based on the CMS or the full check override.

    :param path: The top level path of the direcotry or clone of the
    repository to check.
    :type path: str
    :param full_check: Logical to focre checking of all files in the
    repository, rather than just the changed files.
    :type full_check: bool
    :param print_volume: Verbosity level for printing. Default is 3.
    :type print_volume: int
    :return: List of relative file paths to check.
    :rtype: List[Path]
    """
    if full_check:  # Override to check all files present.
        repo_path = Path(path)
        all_files = [f for f in repo_path.rglob("*") if f.is_file()]
        if print_volume >= 1:
            print("Full check override enabled.")
        if print_volume >= 3:
            print(f"    Found {len(all_files)} files to "
                  f"check in repository at path: {path}")
        return all_files
    else:  # Configure CMS, and check we've been passed a branch
        if print_volume >= 1:
            print("Using a CMS to determine changed files.")
        cms = which_cms_is_it(path, print_volume)
        changed_files = cms.get_changed_files()
        return changed_files


# Usage when run from command line.
if __name__ == "__main__":
    args = process_arguments()

    log_volume = args.volume
    quiet_pass = not args.printpass

    file_paths = get_files_to_check(args.path, args.fullcheck, log_volume)
    full_file_paths = [Path(f"{args.path}/{f}") for f in file_paths]

    # Configure checkers
    """
    TODO : Uncertain as to how flexible this needs to be.
        For now, just configure checkers based on file type requested.
        Later, could add configuration files to specify which
        checkers to use for each file type."""
    checkers = []

    active_checkers = create_style_checkers(args.file_types,
                                            full_file_paths)

    # TODO : Could create a conformance checker for each
    #  file type.
    #  Currently, just create a single conformance checker
    #  with all active checkers.
    checker = ConformanceChecker(
        active_checkers,
        max_workers=args.max_workers,
    )

    checker.check_files()

    if log_volume >= 3:
        print(line_1(81))
        print("## Results :" + " "*67 + "##")
        print(line_1(81) + "\n")
    else:
        print("Results  :")
    all_passed = checker.print_results(print_volume=log_volume,
                                       quiet_pass=quiet_pass)
    if log_volume >= 4:
        print("\n" + line_1(81))
        print("## Summary :" + " "*67 + "##")
        print(line_1(81))
    print(f"Total files checked: {len(checker.results)}")
    print(f"Total files failed: "
          f"{sum(1 for r in checker.results if not r.all_passed)}")

    exit(0 if all_passed else 1)
