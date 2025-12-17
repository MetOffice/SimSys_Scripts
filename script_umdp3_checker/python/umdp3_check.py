#!/usr/bin/env python3
# *****************************COPYRIGHT*******************************
# (C) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file LICENSE
# which you should have received as part of this distribution.
# *****************************COPYRIGHT*******************************

"""
Script to check whether a code change complies with UMDP 003
Python translation of the original Perl script
"""

import sys
import os
import re
import threading
import time
import subprocess
import argparse
from pathlib import Path
from concurrent.futures import ThreadPoolExecutor, as_completed
from typing import Dict, List, Tuple, Optional, Set
import queue
#import magic
from dataclasses import dataclass

# Import our modules
from umdp3 import UMDP3
from umdp3_critic_policy import UMDP3CriticPolicy
from old_umdp3_checks import OldUMDP3Checks

# Declare version
VERSION = '13.5.0'

# Global variables
fcm_profile = '/etc/profile'
snooze = 120  # Time to wait before retrying
max_snooze = 10  # Maximum number of retries before aborting

@dataclass
class GlobalState:
    """Thread-safe global state container"""
    branch_mode: bool
    suite_mode: bool
    additions: Dict[str, List[str]]
    deletions: Dict[str, List[str]]
    output_threads: List[List[str]]
    exit_threads: List[int]
    fortran_includes: Set[str]
    
    def __init__(self, fortran_includes: Optional[Set[str]] = None, branch_mode=False, suite_mode=False):
        self.branch_mode = branch_mode
        self.suite_mode = suite_mode
        self.additions = {}
        self.deletions = {}
        self.output_threads = []
        self.exit_threads = []
        self.fortran_includes = fortran_includes or set()
        self._lock = threading.Lock()
    
    def add_file(self, filename: str, lines: List[str] = []):
        '''Dictionary where the keys are the names of files added or modified
        in the branch being examined'''
        with self._lock:
            self.additions[filename] = lines or []
    
    def add_deletion(self, filename: str):
        '''Dictionary where the keys are the names of files deleted in the
        branch being examined'''
        with self._lock:
            self.deletions[filename] = []
    
    def get_files(self):
        with self._lock:
            return list(self.additions.keys())

def main():
    """Main entry point"""
    # Argument parsing...
    parser = argparse.ArgumentParser(description='UMDP3 compliance checker')
    parser.add_argument('branch', nargs='?', default='.', 
                       help='Branch to check (default: current directory)')
    parser.add_argument('whitelist_file', 
                       help='Whitelist includes file')
    
    args = parser.parse_args()
    
    branch = args.branch
    whitelist_includes_file = args.whitelist_file
    
    # Cope with UTF-style working copy syntax
    # TODO: UTF-Style probably obsolete now.
    branch = re.sub(r'^wc:', '', branch)
    
    # Check whitelist file exists
    if not os.path.isfile(whitelist_includes_file):
        sys.exit("Whitelist filename not provided or doesn't exist.")
    
    # Read whitelist includes
    includes = read_file(whitelist_includes_file)
    
    # Check for suite mode
    suite_mode, branch = detect_suite_mode(branch)

    # Set up threading
    max_threads = set_max_threads()

    # Set up cylc logging
    ''' ToDo: cylc logging : Is this required ?
        What it appears to result in in the nightlies is too overwhelming to read.
        Should it be enabled by cmd line arg instead of 'on' for all suites ?'''
    log_cylc = os.environ.get('CYLC_TASK_LOG_ROOT', '')
    if log_cylc:
        print(f"Using cylc logging directory: {log_cylc}")
    
    # Initialize global state
    global_state = GlobalState(set(includes))
    #global_state.fortran_includes = set(includes)
    
    # Initialize dispatch tables
    dispatch_tables = OldUMDP3Checks()
    
    # Start branch checking
    is_trunk, error_trunk = check_branch_info(branch, suite_mode)
    print(f"DEBUG : Branch {branch} is {'trunk' if is_trunk else 'a branch'}")
    
    # Process files based on mode
    if is_trunk:
        file_list = process_trunk_mode(branch, suite_mode, global_state, max_threads)
        #file_list = [f"{branch}/{file}" for file in file_list] # This was needed for WC of trunk - but not in suite mode
        process_trunk_files_threaded(file_list, global_state, max_threads, suite_mode)
    else:
        file_list = process_branch_mode(branch, global_state)

    print(f"DEBUG : There are {len(file_list)} files in file list")
    print(f"DEBUG : There are {len(global_state.additions)} files in additions")
    # Run checks
    exit_code = run_all_checks(global_state, dispatch_tables, 
                              branch, is_trunk, max_threads, log_cylc)
    
    # Print results
    print_results(exit_code, global_state)
    
    # Exit with appropriate code
    if error_trunk == 1 or not is_trunk:
        sys.exit(exit_code > 0)
    else:
        sys.exit(0)

def detect_suite_mode(branch: str) -> Tuple[bool, str]:
    """
    Checks for the environment variable SOURCE_UM_MIRROR to determine if the script is running in suite mode.
    Returns a tuple (suite_mode: bool, branch: str), where suite_mode is True if suite mode is detected,
    and branch may be modified to reflect the suite source location.
    """
    ''' TODO: unsure if branch should be altered if in suite mode.
    Might also be better as 2 routines: one to establish suite mode, the other to set the branch.'''
    if os.environ.get('SOURCE_UM_MIRROR'):
        print("Detected SOURCE_UM_MIRROR environment variable.")
        branch = os.environ['SOURCE_UM_MIRROR']
        print(f"Redirecting branch to {branch}")
        return True, branch
    else:
        print("Not running in suite mode.")
        return False, branch

def set_max_threads():
    max_threads = int(os.environ.get('UMDP_CHECKER_THREADS', '1'))
    if max_threads < 1:
        print("UMDP_CHECKER_THREADS environment variable is invalid: overriding")
        max_threads = 1
    print(f"Using a maximum of {max_threads} threads at a time")
    return max_threads

def check_branch_info(branch: str, suite_mode: bool) -> Tuple[bool, int]:
    """Check branch information and determine mode"""
    is_trunk = False
    error_trunk = 0
    
    while True:
        # Get branch info
        binfo, binfocode = run_fcm_command(f'binfo {branch}')
        
        if binfocode != 0:
            if 'svn info --xml' in ' '.join(binfo) and suite_mode:
                for i in range(1, max_snooze + 1):
                    print(f"Revision probably doesn't exist yet - waiting {snooze} "
                          f"seconds for mirror to update (Snooze {i} of {max_snooze}).")
                    time.sleep(snooze)
                    binfo, binfocode = run_fcm_command(f'binfo {branch}')
                    if binfocode == 0:
                        break
            
            if binfocode != 0:
                print("Error running fcm binfo:")
                print('\n'.join(binfo))
                sys.exit("FCM error")
        
        # Check if this is trunk
        trunk_patterns = [
            r'URL:\s*svn://[^/]+/(\w|\.)+_svn/\w+/trunk',
            r'URL:\s*https://[^/]+/svn/[\w\.]+/\w+/trunk',
            r'URL:.*\/svn\/\w+\/main\/trunk',
            r'URL:.*_svn\/main\/trunk',
            r'URL:\s*file://.*\/trunk'
        ]
        
        is_trunk = any(re.search(pattern, line) for pattern in trunk_patterns 
                      for line in binfo)
        
        if is_trunk:
            print("Detected trunk: checking full source tree")
            branch = re.sub(r'@.*$', '', branch)
            is_trunk = True
            error_trunk = int(os.environ.get('UMDP_CHECKER_TRUNK_ERROR', '0'))
            
            if error_trunk == 1:
                print("UMDP_CHECKER_TRUNK_ERROR environment variable is set to 1: "
                      "failures will be fatal")
            elif error_trunk == -1:
                print("UMDP_CHECKER_TRUNK_ERROR environment variable is set to -1: "
                      "skipping UMPD3 checks for the trunk")
                sys.exit(0)
            else:
                print(f"UMDP_CHECKER_TRUNK_ERROR environment variable is set to "
                      f"{error_trunk}: failures will be ignored")
            break
        
        # Check for branch-of-branch
        branch_parent = None
        for line in binfo:
            if re.search(r'Branch\s+Parent:.*\/trunk@.*', line):
                break
            elif match := re.search(r'Branch\s+Parent:\s*(.*)', line):
                branch_parent = match.group(1)
                break
        
        if branch_parent:
            print(f"This branch is a branch-of-branch - testing parent ({branch_parent})")
            branch = branch_parent
            continue
        else:
            break
    
    return is_trunk, error_trunk

def process_branch_mode(branch: str, global_state: GlobalState):
    """Process files in branch mode"""
    """ # Get repository paths - this is commented out as 'info' is not used
    info, infocode = run_fcm_command(f'info {branch}')
    if infocode != 0:
        print("Error running fcm info:")
        print('\n'.join(info))
        sys.exit("FCM error") """
    file_list=[]
    
    # Get diff
    diff, diffcode = run_fcm_command(f'bdiff {branch}')
    if diffcode != 0:
        sys.exit(f"Error running 'fcm bdiff {branch}':\n" + '\n'.join(diff))
    
    # Get summary
    summary, summarycode = run_fcm_command(f'bdiff --summarise {branch}')
    if summarycode != 0:
        sys.exit(f"Error running 'fcm bdiff --summarise {branch}':\n" + 
                '\n'.join(summary))
    
    # Process summary for added/modified/deleted files
    for line in summary:
        # Added or modified files
        if match := re.search(r'^(A|M+)\s*(\S+)$', line):
            modified_file = normalize_path(match.group(2), branch)
            global_state.add_file(modified_file)
            file_list.append(modified_file)
            print(f"DEBUG : Added a modified file: {modified_file}")

        # Deleted files
        elif match := re.search(r'^D\s*(\S+)$', line):
            deleted_file = normalize_path(match.group(1), branch)
            global_state.add_deletion(deleted_file)
            print(f"DEBUG : Added a deleted file: {deleted_file}")

    # Process diff to get added lines
    store_line = False
    current_file = ""
    
    for line in diff:
        if line.startswith('+++'):
            if match := re.search(r'^\+\+\+\s+(\S+)', line):
                filename = match.group(1)
                if not (branch == "." or filename == branch):
                    filename = re.sub(f'.*{re.escape(branch)}/', '', filename)
                current_file = filename
                store_line = current_file in global_state.additions
        
        elif line.startswith('+') and store_line:
            line_content = line[1:]  # Remove the '+' prefix
            if current_file in global_state.additions:
                global_state.additions[current_file].append(line_content)
                #print(f"DEBUG : Added a line in modified file: {modified_file}")
    return file_list

def process_trunk_mode(branch: str, suite_mode: bool, global_state: GlobalState, 
                      max_threads: int):
    """Process files in trunk mode"""
    external_checks = ["shumlib", "meta", "ukca"]
    filepath_mapping = {'meta': 'um_meta'}
    extracts = []

    extracts = ["um"] + external_checks
    print(f"DEBUG : Extracts for trunk mode (before suite check): {extracts}")
    
    if suite_mode:
        # Handle suite mode logic for external repositories
        script_source = os.environ.get('SCRIPT_SOURCE', '')
        if script_source:
            suite_conf_path = os.path.join(script_source, "um/rose-stem/rose-suite.conf")
            if os.path.exists(suite_conf_path):
                suite_conf = read_file(suite_conf_path)
                host_sources = [line for line in suite_conf if line.startswith('HOST_SOURCE_')]
                print("Detected HOST_SOURCE variables:")
                print('\n'.join(host_sources))
                
                for repo in external_checks:
                    print(f"DEBUG :Looking at repo: {repo}")
                    o_repo = repo
                    if repo in filepath_mapping:
                        print(f"DEBUG : {repo} is in filepath_mapping, mapping to {filepath_mapping[repo]}")
                        repo = filepath_mapping[repo]
                        print(f"DEBUG : Mapped repo is now {repo}")
                    
                    host_var_name = f"HOST_SOURCE_{repo.upper()}"
                    env_var_res = os.environ.get(host_var_name, '')
                    
                    if not any(f'{host_var_name}=' in line for line in host_sources):
                        print(f"{host_var_name} modified in environment. "
                              f"Running full check on this repository")
                        extracts.append(o_repo)
                    else:
                        print(f"DEBUG : {host_var_name} not modified in environment. "
                              f"Skipping full check on this repository")
        
        # Check for rose-suite.conf modifications
        if "rose-stem/rose-suite.conf" in global_state.additions:
            print("rose-stem/rose-suite.conf modified: checking for external repository updates")
            added_lines = global_state.additions["rose-stem/rose-suite.conf"]
            
            for repo in external_checks:
                o_repo = repo
                if repo in filepath_mapping:
                    repo = filepath_mapping[repo]
                
                host_var_name = f"HOST_SOURCE_{repo.upper()}"
                if any(host_var_name in line for line in added_lines):
                    print(f"{host_var_name} modified in rose-suite.conf. "
                          f"Running full check on this repository")
                    extracts.append(o_repo)
        
        # Remove duplicates and set up extracts
        extracts = list(set(extracts))
        print(f"DEBUG : Extracts for trunk + suite mode: {extracts}")

    # Get file list
    if suite_mode:
        branchls = get_suite_file_list(extracts)
        print(f"DEBUG : Found {len(branchls)} files in suite mode using 'get_suite_file_list'")
    else:
        branchls, returncode = run_fcm_command(f'ls -R {branch}')
        if returncode != 0:
            sys.exit(f"Error running 'fcm ls -R {branch}':\n" + '\n'.join(branchls))
        print(f"DEBUG : Found {len(branchls)} files in trunk mode using 'fcm ls -R {branch}'")
    
    if not branchls:
        sys.exit(f"Error: no files in {branch}")
    
    # Process files with threading
    #process_trunk_files_threaded(branchls, global_state, max_threads, suite_mode)
    return branchls

def process_trunk_files_threaded(branchls: List[str], global_state: GlobalState, 
                                max_threads: int, suite_mode: bool):
    """Process trunk files using threads"""
    # Filter out directories
    files = [line.rstrip() for line in branchls if not line.endswith('/')]
    
    if len(files) < max_threads:
        max_threads = len(files)
    
    # Use ThreadPoolExecutor for better thread management
    with ThreadPoolExecutor(max_workers=max_threads) as executor:
        # Split work into chunks
        #chunk_size = max(1, len(files) // (3 * num_threads))
        #chunks = [files[i:i + chunk_size] for i in range(0, len(files), chunk_size)]
        
        # Submit tasks
        futures = []
        #for chunk in chunks:
        for file in files: # DEBUG :
            future = executor.submit(trunk_files_parse, [file], global_state, suite_mode)
            futures.append(future)
        
        # Wait for completion
        for future in as_completed(futures):
            try:
                future.result()
            except Exception as e:
                print(f"Thread terminated abnormally: {e}")

def trunk_files_parse(file_chunk: List[str], global_state: GlobalState, 
                     suite_mode: bool) -> int:
    """Parse trunk files in a thread"""
    for file_path in file_chunk:
        file_path = file_path.rstrip()
        
        if not file_path.endswith('/'):  # Skip directories
            modified_file = normalize_trunk_path(file_path, suite_mode)
            
            try:
                file_lines = cat_file(file_path if suite_mode else f"{file_path}")
                global_state.add_file(modified_file, file_lines)
            except Exception as e:
                print(f"Error reading file 1 {file_path}: {e}")
    
    return 0

def run_all_checks(global_state: GlobalState, dispatch_tables: OldUMDP3Checks,
                   branch: str, trunkmode: bool, max_threads: int, 
                   log_cylc: str) -> int:
    """Run all compliance checks"""
    add_keys = global_state.get_files()
    
    if not add_keys:
        return 0
    
    # Adjust thread count if needed
    if len(add_keys) < max_threads:
        max_threads = len(add_keys)
    #num_threads = len(add_keys) # DEBUG :
    
    # Initialize thread outputs
    global_state.output_threads = [[] for _ in range(len(add_keys))]
    global_state.exit_threads = [0] * len(add_keys)
    
    # Use ThreadPoolExecutor for checks
    with ThreadPoolExecutor(max_workers=max_threads) as executor:
        # DEBUG : I think dividing the list of files into "chunks" here is erroneous
        # I see no issues with running 1 thread per file
        #chunk_size = max(1, len(add_keys) // (3 * num_threads))
        #print(f"DEBUG : Running checks on {len(add_keys)} files using {num_threads} threads")
        #print(f"DEBUG : Chunk size for files is {chunk_size}")
        #chunks = [add_keys[i:i + chunk_size] for i in range(0, len(add_keys), chunk_size)]
        #print(f"DEBUG : chunks is {chunks}")

        
        futures = []
        for i, file in enumerate(add_keys):
            # DEBUG : I think this is unnecessary
            #if i >= num_threads:
            #    print(f"DEBUG : Reached maximum number of threads ({num_threads})")
            #    break
            future = executor.submit(run_checks, [file], global_state,
                                   dispatch_tables, branch, trunkmode, i, log_cylc)
            futures.append(future)
        
        # Wait for completion
        #print("DEBUG : Waiting for threads to complete")
        print(f"DEBUG : {len(futures)} threads submitted")
        for future in as_completed(futures):
            #print("DEBUG : Thread completed")
            # Handle exceptions in threads
            try:
                future.result()
            except Exception as e:
                print(f"Thread terminated abnormally: {e}")
    
    return sum(global_state.exit_threads)

def run_checks(file_chunk: List[str], global_state: GlobalState,
               dispatch_tables: OldUMDP3Checks, branch: str, 
               trunkmode: bool, thread_id: int, log_cylc: str) -> int:
    """Run checks for a chunk of files"""
    for modified_file in file_chunk:
        #print(f"DEBUG : Running checks for {modified_file} in thread {thread_id}")
        #print(f"DEBUG : file_chunk is {file_chunk}")
        failed = 0
        failed_tests = []
        is_c_file = False
        is_fortran_include_file = False
        
        # Check if it's an include file
        if modified_file.endswith('.h'):
            if modified_file in global_state.fortran_includes:
                components = modified_file.split('/')
                if (components[0] == 'src' and 
                    len(components) >= 3 and components[-2] == 'include' and 
                    components[1] != 'include'):
                    is_fortran_include_file = True
                elif (components[0] == 'src' and 
                      len(components) >= 2 and components[1] == 'include'):
                    is_c_file = True
                else:
                    failed_tests.append("Added an include file outside of a recognised 'include' directory")
            else:
                failed_tests.append("Modified or created non-whitelisted include file rather than using a module")
                failed += 1
        
        if modified_file.endswith('.c'):
            is_c_file = True
        
        # Apply tests based on file type
        if (modified_file.endswith(('.F90', '.f90')) or 
            is_c_file or is_fortran_include_file):
            
            # Get appropriate dispatch tables
            if is_c_file:
                dispatch_table_diff = dispatch_tables.get_diff_dispatch_table_c()
                dispatch_table_file = dispatch_tables.get_file_dispatch_table_c()
            else:
                dispatch_table_diff = dispatch_tables.get_diff_dispatch_table_fortran()
                dispatch_table_file = dispatch_tables.get_file_dispatch_table_fortran(modified_file)
            
            # Get added lines
            added_lines = global_state.additions.get(modified_file, [])
            
            # Run diff tests
            umdp3 = UMDP3()
            for testname, test_func in dispatch_table_diff.items():
                #print(f"DEBUG : Running diff test {testname} for {modified_file}")
                umdp3.reset_extra_error_information()
                answer = test_func(added_lines)
                extra_error = umdp3.get_extra_error_information()
                
                if extra_error:
                    extra_text = ", ".join(extra_error.keys())
                    testname += f": {extra_text}"
                
                if answer:
                    failed += 1
                    failed_tests.append(testname)
            
            # Get full file content for file tests
            if trunkmode:
                file_lines = added_lines
            else:
                file_lines = get_full_file_content(branch, modified_file)
            
            # Run file tests
            for testname, test_func in dispatch_table_file.items():
                umdp3.reset_extra_error_information()
                answer = test_func(file_lines)
                extra_error = umdp3.get_extra_error_information()
                
                if extra_error:
                    extra_text = ", ".join(extra_error.keys())
                    testname += f": {extra_text}"
                
                if answer:
                    failed += 1
                    failed_tests.append(testname)
        
        else:
            # Handle other file types
            if trunkmode:
                file_lines = global_state.additions.get(modified_file, [])
            else:
                file_lines = get_full_file_content(branch, modified_file)
            
            # Detect file type using python-magic
            try:
                file_content = '\n'.join(file_lines)
                #mimetype = magic.from_buffer(file_content.encode(), mime=True)
                mimetype = 'text/plain'
            except:
                mimetype = 'text/plain'
            
            # Skip binary files for universal tests
            binary_files = [
                'application/x-tar', 'application/octet-stream',
                'image/gif', 'image/png'
            ]
            
            if mimetype not in binary_files:
                # Run universal tests
                dispatch_table_all = dispatch_tables.get_file_dispatch_table_all()
                umdp3 = UMDP3()
                for testname, test_func in dispatch_table_all.items():
                    umdp3.reset_extra_error_information()
                    answer = test_func(file_lines)
                    extra_error = umdp3.get_extra_error_information()
                    
                    if extra_error:
                        extra_text = ", ".join(extra_error.keys())
                        testname += f": {extra_text}"
                    
                    if answer:
                        failed += 1
                        failed_tests.append(testname)
            
            # Check specific file types
            is_python = (mimetype == 'text/x-python' or modified_file.endswith('.py'))
            is_perl = (mimetype == 'application/x-perl' or 
                      modified_file.endswith(('.pl', '.pm')))
            is_shell = (mimetype == 'application/x-shellscript')
            
            # Run external tools
            if is_python:
                failed += run_pycodestyle(file_lines, failed_tests)
            
            if is_perl:
                failed += run_perl_critic(file_lines, failed_tests)
            
            if is_shell:
                failed += run_shellcheck(file_lines, failed_tests)
        
        # Universal tests for all files
        dispatch_table_all = dispatch_tables.get_file_dispatch_table_all()
        umdp3 = UMDP3()
        for testname, test_func in dispatch_table_all.items():
            umdp3.reset_extra_error_information()
            answer = test_func(file_lines if 'file_lines' in locals() else 
                             global_state.additions.get(modified_file, []))
            extra_error = umdp3.get_extra_error_information()
            
            if extra_error:
                extra_text = ", ".join(extra_error.keys())
                testname += f": {extra_text}"
            
            if answer:
                failed += 1
                failed_tests.append(testname)
        
        # Handle failures
        if failed > 0:
            failure_text = '\n  '.join(failed_tests)
            message = f"File {modified_file} :\n  {failure_text}\n"
            global_state.output_threads[thread_id].append(message)
            global_state.exit_threads[thread_id] += failed
            
            # Cylc logging
            if log_cylc:
                write_cylc_log(log_cylc, modified_file, failure_text)
    
    return 0

def run_pycodestyle(file_lines: List[str], failed_tests: List[str]) -> int:
    """Run pycodestyle check"""
    try:
        file_content = '\n'.join(file_lines)
        result = subprocess.run(['pycodestyle', '-'], 
                               input=file_content, 
                               capture_output=True, 
                               text=True)
        
        if result.returncode != 0:
            output = result.stdout + result.stderr
            output = re.sub(r'\n?\n', '\n  ', output)
            output = re.sub(r'stdin:', 'line ', output)
            failed_tests.append(output)
            return 1
    except Exception as e:
        failed_tests.append(f"Error running pycodestyle: {e}")
        return 1
    
    return 0

def run_perl_critic(file_lines: List[str], failed_tests: List[str]) -> int:
    """Run Perl::Critic equivalent check"""
    # This would need a Python equivalent of Perl::Critic
    # For now, we'll implement basic Perl checks
    file_content = '\n'.join(file_lines)
    
    # Basic Perl style checks
    violations = []
    
    # Check for use strict and warnings
    if 'use strict' not in file_content:
        violations.append("Code before strictures are enabled")
    
    if 'use warnings' not in file_content:
        violations.append("Code before warnings are enabled")
    
    if violations:
        failed_tests.extend(violations)
        return 1
    
    return 0

def run_shellcheck(file_lines: List[str], failed_tests: List[str]) -> int:
    """Run shellcheck"""
    try:
        file_content = '\n'.join(file_lines)
        result = subprocess.run(['shellcheck', '-'], 
                               input=file_content, 
                               capture_output=True, 
                               text=True)
        
        if result.returncode != 0:
            output = result.stdout + result.stderr
            output = re.sub(r'\n?\n', '\n  ', output)
            output = re.sub(r'\s\sIn\s-\s', '  ', output)
            failed_tests.append(output)
            return 1
    except Exception as e:
        failed_tests.append(f"Error running shellcheck: {e}")
        return 1
    
    return 0

def write_cylc_log(log_cylc: str, modified_file: str, failure_text: str):
    """Write Cylc log file"""
    filename = modified_file.replace('/', '+')
    if '.' in filename:
        filename += '_'
    else:
        filename += '.'
    
    log_filename = f"{log_cylc}.{filename}report"
    
    try:
        with open(log_filename, 'w') as f:
            f.write(failure_text)
    except Exception as e:
        print(f"ERR: {log_filename}: {e}")

def print_results(exit_code: int, global_state: GlobalState):
    """Print final results"""
    if exit_code > 0:
        print("The following files have failed the UMDP3 compliance tests:")
        for thread_output in global_state.output_threads:
            for message in thread_output:
                print(message, end='')
        print(f"\n[ERROR] There were a total of {exit_code} compliance tests failures")
    else:
        print("No modified files appear to have failed the compliance tests")

# Utility functions

def run_fcm_command(command: str) -> Tuple[List[str], int]:
    """Run an FCM command and return output and return code"""
    try:
        full_command = f'. {fcm_profile}; fcm {command}'
        result = subprocess.run(full_command, shell=True, 
                               capture_output=True, text=True)
        return result.stdout.splitlines() + result.stderr.splitlines(), result.returncode
    except Exception as e:
        return [str(e)], 1

def cat_file(url: str) -> List[str]:
    """Cat a file, either from FCM or from disk"""
    try:
        if ':' in url:
            # FCM URL
            lines, error = run_fcm_command(f'cat {url}')
            if error != 0:
                # Check if it's a directory
                info, info_error = run_fcm_command(f'info {url}')
                if info_error == 0:
                    info_text = '\n'.join(info)
                    if 'Node Kind: file' not in info_text:
                        return ['']
                raise Exception(f"Error cating file {url}")
            return lines
        else:
            # Regular file
            with open(url, 'r', encoding='utf-8', errors='ignore') as f:
                return f.read().splitlines()
    except Exception as e:
        raise Exception(f"Error reading file 2 {url}: {e}")

def read_file(filename: str) -> List[str]:
    """Read a file and return lines"""
    try:
        with open(filename, 'r', encoding='utf-8') as f:
            return [line.rstrip('\n') for line in f]
    except Exception as e:
        sys.exit(f"Cannot read {filename}: {e}")

def normalize_path(path: str, branch: str) -> str:
    """Normalize a file path"""
    # Remove repository prefixes and normalize
    path = re.sub(r'.*/trunk.*?/', '', path)
    return path

def normalize_trunk_path(path: str, suite_mode: bool) -> str:
    """Normalize trunk path"""
    if suite_mode:
        # Handle suite mode path normalization
        path = re.sub(r'.*/um/', '', path)
    else:
        # Handle regular FCM path normalization
        path = re.sub(r'.*/trunk/', '', path)
    return path

def get_suite_file_list(extracts: List[str]) -> List[str]:
    """Get file list for suite mode"""
    script_source = os.environ.get('SCRIPT_SOURCE', '')
    print(f"DEBUG : SCRIPT_SOURCE is {script_source}")
    if not script_source:
        return []
    
    file_list = []
    
    for extract in extracts:
        print(f"DEBUG : Looking for files in extract: {extract}, but probably never got here")
        if extract:
            extract_path = os.path.join(script_source, extract)
        else:
            extract_path = script_source
        
        try:
            result = subprocess.run(['find', extract_path, '-type', 'f', 
                                   '-exec', 'readlink', '-f', '{}', ';'],
                                  capture_output=True, text=True)
            if result.returncode == 0:
                file_list.extend(result.stdout.splitlines())
                print(f"DEBUG : Found {len(result.stdout.splitlines())} files in {extract_path}")
        except Exception:
            continue
    
    # Add imported scripts
    cylc_share = os.environ.get('CYLC_SUITE_SHARE_DIR', '')
    if cylc_share:
        try:
            scripts_path = os.path.join(cylc_share, 'imported_github_scripts')
            result = subprocess.run(['find', scripts_path, '-type', 'f',
                                   '-not', '-ipath', '*/.git/*',
                                   '-exec', 'readlink', '-f', '{}', ';'],
                                  capture_output=True, text=True)
            if result.returncode == 0:
                file_list.extend(result.stdout.splitlines())
        except Exception:
            pass
    
    return file_list

def get_full_file_content(branch: str, modified_file: str) -> List[str]:
    """Get full file content for non-trunk mode"""
    url_revision = ""
    short_branch = branch
    
    if '@' in short_branch:
        match = re.search(r'(@.*)', short_branch)
        if match:
            url_revision = match.group(1)
            short_branch = re.sub(r'@.*', '', short_branch)
    
    # Build file URL
    if url_revision:
        file_url = f"{short_branch}/{modified_file}{url_revision}"
    else:
        file_url = f"{short_branch}/{modified_file}"
    
    return cat_file(file_url)


if __name__ == "__main__":
    main()
