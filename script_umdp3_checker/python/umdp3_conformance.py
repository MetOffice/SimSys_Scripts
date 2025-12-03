import subprocess
from abc import ABC, abstractmethod
from pathlib import Path
from typing import Callable, Iterable, List, Dict, Set
from dataclasses import dataclass
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
class CheckResult:
    """Result from running a style checker on a file."""
    file_path: str = "No file provided"
    checker_name: str = "Unnamed Checker"
    passed: bool = False
    output: str = ""
    return_code: int = 0

class GitBdiffWrapper:
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

class FCMBdiffWrapper:
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

class UMDP3_checkers(StyleChecker):
    """UMDP3 built-in style checker."""
    
    def __init__(self, name: str, check_function: Callable):
        self.name = name
        self.check_function = check_function
    
    def check(self, file_path: Path) -> CheckResult:
        """Run UMDP3 check function on file."""
        lines = file_path.read_text().splitlines()
        failures = self.check_function(lines)
        passed = failures == 0
        output = "Not configured yet..."
        return CheckResult(
            file_path=str(file_path),
            checker_name=self.name,
            passed=passed,
            output=output,
            return_code=0 if passed else 1
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

    def __init__(self, cms, checkers: List[StyleChecker],     
                 max_workers: int = 4):
        self.cms = cms
        self.checkers = checkers
        self.max_workers = max_workers
        self.changed_files = [] 

    def is_branch(self) -> bool:
        """Check if we're looking at a branch"""
        return self.cms.is_branch()
    
    def check_branch(self, branch: str, base_branch: str = "main"):
        """Check all changed files on a branch."""
        changed_files = self.cms.get_changed_files(branch)
        self.changed_files = [Path(file) for file in changed_files]
        return
        
    def check_files(self) -> Dict[str, List]:
        """Run all checkers on given files in parallel.
        ToDo : This looks to create a task for each (file, checker) pair.
        Given each file would need to be opened multiple times, would it be
        more efficient to have each file opened once, and all checkers run on it
        before moving to the next file?"""
        results = {}

        tasks = [
            (checker, file_path)
            for file_path in self.changed_files
            for checker in self.checkers
        ]
        print("Total number of files to check:", len(list(self.changed_files)))
        print("Total number of checkers:", len(self.checkers))
        print(f"Total tasks to run: {len(tasks)}")
        with concurrent.futures.ThreadPoolExecutor(max_workers=self.max_workers) as executor:
             future_to_task = {
                executor.submit(checker.check, file_path): (checker, file_path)
                for checker, file_path in tasks
            }
            
             for future in concurrent.futures.as_completed(future_to_task):
                result = future.result()
                if result.file_path not in results:
                    results[result.file_path] = []
                results[result.file_path].append(result)
        
        return results
    
    def print_results(self, results: Dict[str, List]) -> bool:
        """Print results and return True if all checks passed."""
        all_passed = True
        
        for file_path, check_results in sorted(results.items()):
            print(f"\n{file_path}:")
            for result in check_results:
                status = "✓ PASS" if result.passed else "✗ FAIL"
                print(f"  [{result.checker_name}] {status}")
                if not result.passed and result.output:
                    print(f"    {result.output}")
                all_passed = all_passed and result.passed
        
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

def which_cms_is_it(path):
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

    # Configure checkers
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
            dispatch_tables = UMDP3DispatchTables()
            fortran_diff_table = dispatch_tables.get_diff_dispatch_table_fortran()
            fortran_file_table = dispatch_tables.get_file_dispatch_table_fortran()
            print("Configuring Fortran checkers:")
            for checker_name, check in fortran_diff_table.items():
                print(f"Creating Diff Checker: {checker_name}")
                checkers.append(UMDP3_checkers(checker_name, check))

    # Create conformance checker
    checker = ConformanceChecker(cms, checkers, max_workers=8)
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
    results = checker.check_files()
    for result_name, result in results.items():
        if all([doodah.passed for doodah in result]):
            print(f"Result for {result_name} is : [OK]")
        else:
            print(f"Result for {result_name} is : [FAIL]")

    #all_passed = checker.print_results(results)
    
    #exit(0 if all_passed else 1)