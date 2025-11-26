import subprocess
from abc import ABC, abstractmethod
from pathlib import Path
from typing import List, Dict, Set
from dataclasses import dataclass
import argparse
# Add custom modules to Python path if needed
# Add the repository root to access fcm_bdiff and git_bdiff packages
import sys
from pathlib import Path
sys.path.insert(0, str(Path(__file__).parent.parent.parent))
from git_bdiff import git_bdiff
import fcm_bdiff

"""
Framework and Classes to generate a list of files to check for style
conformance, and to run relevant style checkers on those files.
"""

import concurrent.futures


@dataclass
class CheckResult:
    """Result from running a style checker on a file."""
    file_path: str
    checker_name: str
    passed: bool
    output: str
    return_code: int


class CMSInterface(ABC):
    """Abstract base class for CMS integration."""
    
    @abstractmethod
    def get_changed_files(self, branch: str, base_branch: str = "main") -> List[Path]:
        """Get list of files changed on a branch."""
        pass

class GitBdiffWrapper:
    """Wrapper around git_bdiff to get changed files."""
    
    def __init__(self, repo_path: Path = Path(".")):
        self.repo_path = repo_path
    
    def get_changed_files(self, branch: str, base_branch: str = "main") -> List[Path]:
        """Get list of files changed between base_branch and branch."""
        bdiff_obj = git_bdiff.GitBDiff(repo=self.repo_path)
        
        return bdiff_obj


class FCMBdiffWrapper:
    """Wrapper around fcm_bdiff to get changed files."""
    
    def __init__(self, repo_path: Path = Path(".")):
        self.repo_path = repo_path
    
    def get_changed_files(self, branch: str, base_branch: str = "main") -> List[Path]:
        """Get list of files changed between base_branch and branch."""
        bdiff_obj = fcm_bdiff.FCMBDiff(repo=self.repo_path)
        
        return bdiff_obj.files_changed_between(branch, base_branch)


class GitCMS(CMSInterface):
    """Git CMS implementation."""
    
    def __init__(self, repo_path: Path = Path(".")):
        self.repo_path = repo_path
    
    def get_changed_files(self, branch: str, base_branch: str = "main") -> List[Path]:
        """Get files changed between base_branch and branch."""
        try:
            result = subprocess.run(
                ["git", "diff", "--name-only", f"{base_branch}...{branch}"],
                cwd=self.repo_path,
                capture_output=True,
                text=True,
                check=True
            )
            files = [self.repo_path / f.strip() for f in result.stdout.split("\n") if f.strip()]
            return [f for f in files if f.exists()]
        except subprocess.CalledProcessError as e:
            raise RuntimeError(f"Git command failed: {e.stderr}")


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


class ExternalChecker(StyleChecker):
    """Wrapper for external style checking tools."""
    
    def __init__(self, name: str, command: List[str], file_extensions: Set[str] = None):
        self.name = name
        self.command = command
        self.file_extensions = file_extensions or set()
    
    def check(self, file_path: Path) -> CheckResult:
        """Run external checker command on file."""
        if self.file_extensions and file_path.suffix not in self.file_extensions:
            return CheckResult(
                file_path=str(file_path),
                checker_name=self.name,
                passed=True,
                output=f"Skipped (extension {file_path.suffix} not in {self.file_extensions})",
                return_code=0
            )
        
        try:
            cmd = self.command + [str(file_path)]
            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                timeout=60
            )
            
            return CheckResult(
                file_path=str(file_path),
                checker_name=self.name,
                passed=result.returncode == 0,
                output=result.stdout + result.stderr,
                return_code=result.returncode
            )
        except subprocess.TimeoutExpired:
            return CheckResult(
                file_path=str(file_path),
                checker_name=self.name,
                passed=False,
                output="Checker timed out",
                return_code=-1
            )
        except Exception as e:
            return CheckResult(
                file_path=str(file_path),
                checker_name=self.name,
                passed=False,
                output=str(e),
                return_code=-1
            )
    
    def get_name(self) -> str:
        return self.name


class DifferenceDiscoverer:
    """Main framework for establishing the differences to check."""
    
    def __init__(self, cms: CMSInterface):
        self.cms = cms
    
    def check_branch(self, branch: str, base_branch: str = "main") -> Dict[str, List[CheckResult]]:
        """Check all changed files on a branch."""
        changed_files = self.cms.get_changed_files(branch, base_branch)
        
        if not changed_files:
            return {}
        
        return self.check_files(changed_files)


class ConformanceChecker:
    """Main framework for running style checks in parallel."""

    def __init__(self, cms: CMSInterface, checkers: List[StyleChecker],     
                 checklist: List[str], max_workers: int = 4):
        self.cms = cms
        self.checkers = checkers
        self.max_workers = max_workers
        
    def check_files(self, files: List[Path]) -> Dict[str, List[CheckResult]]:
        """Run all checkers on given files in parallel."""
        results = {}
        
        tasks = [
            (checker, file_path)
            for file_path in files
            for checker in self.checkers
        ]
        
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
    
    def print_results(self, results: Dict[str, List[CheckResult]]) -> bool:
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
    parser.add_argument("path", type=str, default="./", help="path to repository")
    parser.add_argument("--branch", type=str, default="HEAD", help="Branch to check")
    parser.add_argument("--base-branch", type=str, default="main", help="Base branch for comparison")
    parser.add_argument("--checker-configs", type=str, default=None,
                        help="Checker configuration file")
    return parser.parse_args()


# Example usage
if __name__ == "__main__":
    args = process_arguments()

    # Configure CMS
    cms = GitCMS()
    
    # Configure checkers
    if args.checker_configs:
        # Load checkers from configuration file (not implemented)
        print("Checker configuration from file not implemented. - Yet!")
        pass
    else:
        checkers = [
            ExternalChecker("flake8", ["flake8"], {".py"}),
            ExternalChecker("black", ["black", "--check"], {".py"}),
            ExternalChecker("pylint", ["pylint"], {".py"}),
        ]
    
    # Create conformance checker
    checker = ConformanceChecker(cms, checkers, max_workers=8)
    
    # Check current branch

    files = checker.check_branch("HEAD", "main")
    all_passed = checker.print_results(results)
    
    exit(0 if all_passed else 1)