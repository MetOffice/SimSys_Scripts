import subprocess
from abc import ABC, abstractmethod
from pathlib import Path
from typing import Callable, Iterable, List, Dict, Set
from dataclasses import dataclass, field
import argparse
# Add custom modules to Python path if needed
# Add the repository root to access fcm_bdiff and git_bdiff packages
import sys
from pathlib import Path
sys.path.insert(0, str(Path(__file__).parent.parent.parent))
from github_scripts import git_bdiff
import fcm_bdiff
from umdp3_dispatch_tables import UMDP3DispatchTables

"""
Framework and Classes to generate a list of files to check for style
conformance, and to run relevant style checkers on those files.
"""

import concurrent.futures

@dataclass
class TestResult:
    """Result from running a single style checker test on a file."""
    checker_name: str = "Unnamed Checker"
    failure_count: int = 0
    passed: bool = False
    output: str = ""

@dataclass
class CheckResult:
    """Result from running a style checker on a file."""
    file_path: str = "No file provided"
    tests_failed: int = 0
    all_passed: bool = False
    test_results: List[TestResult] = field(default_factory=list)

class CMSSystem(ABC):
    """Abstract base class for CMS systems like git or FCM."""
    
    @abstractmethod
    def get_changed_files(self, branch: str) -> Iterable[str]:
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
        self.repo_path = repo_path
        self.bdiff_obj = git_bdiff.GitBDiff(repo=self.repo_path)
        self.info_obj = git_bdiff.GitInfo(repo=self.repo_path)
    
    def get_changed_files(self, branch: str) -> Iterable[str]:
        """Get list of files changed between base_branch and branch."""    
        return self.bdiff_obj.files()
    
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
        self.repo_path = repo_path
        self.bdiff_obj = fcm_bdiff.FCMBDiff(repo=self.repo_path)

    def get_changed_files(self, branch: str) -> Iterable[str]:
        """Get list of files changed between base_branch and branch."""        
        return self.bdiff_obj.files()
    
    def is_branch(self) -> bool:
        """Check if we're looking at a branch"""
        return self.bdiff_obj.is_branch

    def get_branch_name(self) -> str:
        """Get the current branch name."""
        return self.bdiff_obj.branch

class StyleChecker(ABC):
    """Abstract base class for style checkers."""
    
    @abstractmethod
    def check(self, file_path: Path) -> CheckResult:
        """Run the style checker on a file."""
        pass
    
    @abstractmethod
    def get_name(self) -> str:
        """Return the name of this checker."""
        pass

class UMDP3_checker(StyleChecker):
    """UMDP3 built-in style checker."""
    
    def __init__(self, name: str, check_functions: Dict[str, Callable]):
        self.name = name
        self.check_functions = check_functions
    
    def check(self, file_path: Path) -> CheckResult:
        """Run UMDP3 check function on file."""
        lines = file_path.read_text().splitlines()
        file_results = []
        for check_name, check_function in self.check_functions.items():
            failures = check_function(lines)
            passed = failures == 0
            file_results.append(TestResult(
                                checker_name=check_name,
                                failure_count=failures,
                                passed=passed,
                                output="Not configured yet...",
                            ))
        tests_failed=sum([0 if result.passed else 1 for result in file_results])
        return CheckResult(
            file_path=str(file_path),
            tests_failed=tests_failed,
            all_passed= tests_failed == 0,
            test_results=file_results
        )
    
    def get_name(self) -> str:
        return self.name

# class ExternalChecker(StyleChecker):
#     """Wrapper for external style checking tools."""
    
#     def __init__(self, name: str, command: List[str], file_extensions: Set[str] = set()):
#         self.name = name
#         self.command = command
#         self.file_extensions = file_extensions or set()
    
#     def check(self, file_path: Path) -> CheckResult:
#         """Run external checker command on file."""
#         if self.file_extensions and file_path.suffix not in self.file_extensions:
#             return CheckResult(
#                 file_path=str(file_path),
#                 checker_name=self.name,
#                 passed=True,
#                 output=f"Skipped (extension {file_path.suffix} not in {self.file_extensions})",
#                 return_code=0
#             )
        
#         try:
#             cmd = self.command + [str(file_path)]
#             result = subprocess.run(
#                 cmd,
#                 capture_output=True,
#                 text=True,
#                 timeout=60
#             )
            
#             return CheckResult(
#                 file_path=str(file_path),
#                 checker_name=self.name,
#                 passed=result.returncode == 0,
#                 output=result.stdout + result.stderr,
#                 return_code=result.returncode
#             )
#         except subprocess.TimeoutExpired:
#             return CheckResult(
#                 file_path=str(file_path),
#                 checker_name=self.name,
#                 passed=False,
#                 output="Checker timed out",
#                 return_code=-1
#             )
#         except Exception as e:
#             return CheckResult(
#                 file_path=str(file_path),
#                 checker_name=self.name,
#                 passed=False,
#                 output=str(e),
#                 return_code=-1
#             )
    
#     def get_name(self) -> str:
#         return self.name


class ConformanceChecker:
    """Main framework for running style checks in parallel."""

    def __init__(self, cms: CMSSystem,
                 checker: StyleChecker,     
                 file_extensions: Set[str] = set(), 
                 max_workers: int = 4,
                 changed_files: List[Path] = [],
                 results: List[CheckResult] = []):
        self.cms = cms
        self.checker = checker
        self.file_extensions = file_extensions
        self.max_workers = max_workers
        self.changed_files = changed_files
        self.results = results

    def is_branch(self) -> bool:
        """Check if we're looking at a branch"""
        return self.cms.is_branch()
    
    def check_branch(self, branch: str, base_branch: str = "main"):
        """Check all changed files on a branch."""
        changed_files = self.cms.get_changed_files(branch)
        filtered_files = []
        count = 0
        for file in changed_files:
            count += 1
            path = Path(file)
            if not self.file_extensions or path.suffix in self.file_extensions:
                filtered_files.append(path)
        print(f"Started with {count} changed files.")
        print(f"Filtered down to {len(filtered_files)} files")
        self.changed_files = filtered_files
        return
        
    def check_files(self):
        """Run all checkers on given files in parallel.
        ToDo : This looks to create a task for each (file, checker) pair.
        Given each file would need to be opened multiple times, would it be
        more efficient to have each file opened once, and all checkers run on it
        before moving to the next file?"""
        results = []

        tasks = self.changed_files
        with concurrent.futures.ThreadPoolExecutor(max_workers=self.max_workers) as executor:
             future_to_task = {
                executor.submit(self.checker.check, file_path): file_path
                for file_path in tasks
            }
            
             for future in concurrent.futures.as_completed(future_to_task):
                result = future.result()
                results.append(result)
        self.results = results
        return
    
    def print_results(self, fail_only: bool = False) -> bool:
        """Print results and return True if all checks passed."""
        all_passed = True
        
        for result in self.results:
            file_status = "✓ PASS" if result.all_passed else "✗ FAIL"
            all_passed = all_passed and result.all_passed
            print(f"\nResults for file : {result.file_path} : {file_status}")
            if fail_only and result.all_passed:
                continue
            for test_result in result.test_results:
                if fail_only and test_result.passed:
                    continue
                print(f"  [{test_result.checker_name}] : ✗ FAIL")
                print(f"      {test_result.output}")
        return all_passed

def process_arguments():
    """Process command line arguments.
    Somewhat a work in progress, but it's going to be needed eventually."""
    parser = argparse.ArgumentParser(
        prog="umdp3_conformance.py",
        description="""UMDP3 Conformance Checker""",
        epilog="T-T-T-T-That's all folks !!")
    parser.add_argument("--path", type=str, default="./",
                        help="path to repository")
    parser.add_argument("--branch", type=str, default="HEAD",
                        help="Branch to check")
    parser.add_argument("--base-branch", type=str, default="main",
                        help="Base branch for comparison")
    parser.add_argument("--checker-configs", type=str, default=None,
                        help="Checker configuration file")
    parser.add_argument("--file-types", type=List[str], default=["Fortran"],
                        help="File types to check, comma-separated")
    return parser.parse_args()

def which_cms_is_it(path: str) -> CMSSystem:
    """Determine which CMS is in use based on the presence of certain files."""
    repo_path = Path(path)
    if (repo_path / ".git").is_dir():
        return GitBdiffWrapper(repo_path)
    elif (repo_path / ".svn").is_dir():
        # If we still want this to work reliably with FCM, it will need
        # to also accept URLs and not just local paths.
        return FCMBdiffWrapper(repo_path)
    else:
        raise RuntimeError("Unknown CMS type at path: " + str(path))
    
# Example usage
if __name__ == "__main__":
    args = process_arguments()

    # Configure CMS
    cms = which_cms_is_it(args.path)
    checkers = []
    file_extensions = set()
    fortran_file_checker = UMDP3_checker("Default Non Checker", 
                                                 {})

    # Configure checkers
    # ToDo : Uncertain as to how flexible this needs to be.
    #  For now, just configure UMDP3 Fortran checkers if
    #  the 'Fortran' file type is requested.
    if args.checker_configs:
        # Load checkers from configuration file (not implemented)
        print("Checker configuration from file not implemented. - Yet!")
        pass
    else:
        checkers = [
            # ExternalChecker("flake8", ["flake8"], {".py"}),
            # ExternalChecker("black", ["black", "--check"], {".py"}),
            # ExternalChecker("pylint", ["pylint"], {".py"}),
        ]
    
    if args.file_types:
        # Filter checkers based on file types.
        if args.file_types == ["Fortran"]:
            file_extensions = {".f", ".for", ".f90",
                               ".f95", ".f03", ".f08",
                               ".F90"}
            dispatch_tables = UMDP3DispatchTables()
            fortran_diff_table = dispatch_tables.get_diff_dispatch_table_fortran()
            fortran_file_table = dispatch_tables.get_file_dispatch_table_fortran()
            print("Configuring Fortran checkers:")
            checkers = fortran_diff_table | fortran_file_table
            fortran_file_checker = UMDP3_checker("Fortran Checker", 
                                                 checkers)

    # ToDo : Should probably create a list of checkers based on
    #  file types. Where each one filters out files it doesn't
    #  care about. Then create a conformance checker for each
    #  file type.
    #  Current assumption is that we're only dealing
    #  with Fortran files and the checkers are organised by file_types argument.
    # Create conformance checker
    checker = ConformanceChecker(cms,
                                 fortran_file_checker,
                                 file_extensions,
                                 max_workers=8)
    branch_name = cms.get_branch_name()
    if not checker.is_branch():
        print(f"The path {args.path} is not a branch."
              f"\nReported branch name is : {branch_name}"
              "\nThe meaning of differences is unclear, and so"
              " checking is aborted.")
        exit(1)
    else:
        print(f"The branch, {branch_name}, at path {args.path} is a branch.")
    # Check current branch

    checker.check_branch(args.branch, args.base_branch)
    # for file in checker.changed_files:
    #     print(file) 
    checker.check_files()
    for result in checker.results:
        if result.all_passed:
            print(f"Result for {result.file_path} is : [OK]")
        else:
            print(f"Result for {result.file_path} is : [FAIL]")

    print(f"Total files checked: {len(checker.results)}")
    print(f"Total files failed: {sum(1 for r in checker.results if not r.all_passed)}")

    for result in checker.results:
        if not result.all_passed:
            print(f"\nDetails for failed file: {result.file_path}")
            for test_result in result.test_results:
                if not test_result.passed:
                    print(f"  Test: {test_result.checker_name} - Failures: {test_result.failure_count}")
                    # print(f"    Output: {test_result.output}")
    
    all_passed = checker.print_results(fail_only=True)
    
    exit(0 if all_passed else 1)