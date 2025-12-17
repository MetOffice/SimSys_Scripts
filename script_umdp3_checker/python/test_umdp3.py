#!/usr/bin/env python3
# *****************************COPYRIGHT*******************************
# (C) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file LICENSE
# which you should have received as part of this distribution.
# *****************************COPYRIGHT*******************************

"""
Test script for the Python UMDP3 checker
"""

import sys
import os
import tempfile
from pathlib import Path

# Add the current directory to Python path
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from umdp3 import UMDP3
from script_umdp3_checker.python.old_umdp3_checks import UMDP3DispatchTables

def test_basic_functionality():
    """Test basic UMDP3 functionality"""
    print("Testing basic UMDP3 functionality...")
    
    # Initialize UMDP3
    umdp3 = UMDP3()
    
    # Test line length check
    test_lines = [
        "This is a short line",
        "This is a very long line that exceeds eighty characters and should trigger a failure in the line length test"
    ]
    
    result = umdp3.line_over_80chars(test_lines)
    print(f"Line length test: {'PASS' if result > 0 else 'FAIL'} (expected failure)")
    
    # Test tab detection
    test_lines_tabs = [
        "Normal line",
        "Line with\ttab"
    ]
    
    result = umdp3.tab_detection(test_lines_tabs)
    print(f"Tab detection test: {'PASS' if result > 0 else 'FAIL'} (expected failure)")
    
    # Test trailing whitespace
    test_lines_whitespace = [
        "Normal line",
        "Line with trailing spaces   "
    ]
    
    result = umdp3.line_trail_whitespace(test_lines_whitespace)
    print(f"Trailing whitespace test: {'PASS' if result > 0 else 'FAIL'} (expected failure)")
    
    # Test IMPLICIT NONE check
    fortran_without_implicit = [
        "PROGRAM test",
        "INTEGER :: i",
        "END PROGRAM"
    ]
    
    result = umdp3.implicit_none(fortran_without_implicit)
    print(f"IMPLICIT NONE test: {'PASS' if result > 0 else 'FAIL'} (expected failure)")
    
    fortran_with_implicit = [
        "PROGRAM test",
        "IMPLICIT NONE",
        "INTEGER :: i",
        "END PROGRAM"
    ]
    
    result = umdp3.implicit_none(fortran_with_implicit)
    print(f"IMPLICIT NONE test (good): {'PASS' if result == 0 else 'FAIL'} (expected pass)")

def test_dispatch_tables():
    """Test dispatch tables"""
    print("\nTesting dispatch tables...")
    
    dispatch = UMDP3DispatchTables()
    
    # Test getting dispatch tables
    fortran_diff = dispatch.get_diff_dispatch_table_fortran()
    print(f"Fortran diff tests available: {len(fortran_diff)}")
    
    fortran_file = dispatch.get_file_dispatch_table_fortran()
    print(f"Fortran file tests available: {len(fortran_file)}")
    
    c_diff = dispatch.get_diff_dispatch_table_c()
    print(f"C diff tests available: {len(c_diff)}")
    
    c_file = dispatch.get_file_dispatch_table_c()
    print(f"C file tests available: {len(c_file)}")
    
    all_tests = dispatch.get_file_dispatch_table_all()
    print(f"Universal tests available: {len(all_tests)}")

def test_fortran_specific():
    """Test Fortran-specific checks"""
    print("\nTesting Fortran-specific checks...")
    
    umdp3 = UMDP3()
    
    # Test obsolescent intrinsics
    fortran_old_intrinsics = [
        "REAL :: x",
        "x = ALOG(2.0)",
        "y = DBLE(x)"
    ]
    
    result = umdp3.obsolescent_fortran_intrinsic(fortran_old_intrinsics)
    print(f"Obsolescent intrinsics test: {'PASS' if result > 0 else 'FAIL'} (expected failure)")
    
    # Test forbidden operators
    fortran_old_operators = [
        "IF (x .GT. y) THEN",
        "  PRINT *, 'x is greater'"
    ]
    
    result = umdp3.forbidden_operators(fortran_old_operators)
    print(f"Forbidden operators test: {'PASS' if result > 0 else 'FAIL'} (expected failure)")
    
    # Test PRINT statement
    fortran_print = [
        "PRINT *, 'Hello world'"
    ]
    
    result = umdp3.printstar(fortran_print)
    print(f"PRINT statement test: {'PASS' if result > 0 else 'FAIL'} (expected failure)")

def test_c_specific():
    """Test C-specific checks"""
    print("\nTesting C-specific checks...")
    
    umdp3 = UMDP3()
    
    # Test deprecated C identifiers
    c_deprecated = [
        '#include <stdio.h>',
        'char buffer[100];',
        'gets(buffer);'  # deprecated function
    ]
    
    result = umdp3.c_deprecated(c_deprecated)
    print(f"Deprecated C identifiers test: {'PASS' if result > 0 else 'FAIL'} (expected failure)")
    
    # Test format specifiers
    c_format = [
        'printf("%10d", value);'  # missing space
    ]
    
    result = umdp3.c_integral_format_specifiers(c_format)
    print(f"C format specifiers test: {'PASS' if result > 0 else 'FAIL'} (expected failure)")

def create_test_files():
    """Create test files for full integration test"""
    print("\nCreating test files...")
    
    # Create temporary directory
    test_dir = tempfile.mkdtemp(prefix="umdp3_test_")
    print(f"Test directory: {test_dir}")
    
    # Create a test Fortran file with issues
    fortran_file = Path(test_dir) / "test.F90"
    fortran_content = """! Test Fortran file with various issues
program test
    ! Missing IMPLICIT NONE
    integer :: i
    real :: x
    
    ! Line that is too long and exceeds the eighty character limit which should trigger a failure
    x = alog(2.0)  ! obsolescent intrinsic
    
    if (x .gt. 1.0) then  ! old operator
        print *, 'Value is greater than 1'  ! should use umPrint
    endif
    
end program test
"""
    
    with open(fortran_file, 'w') as f:
        f.write(fortran_content)
    
    # Create a test C file with issues
    c_file = Path(test_dir) / "test.c"
    c_content = """/* Test C file with various issues */
#include <stdio.h>

int main() {
    char buffer[100];
    
    // Line that is way too long and exceeds the eighty character limit set by UMDP3 standards
    gets(buffer);  /* deprecated function */
    printf("%10d", 42);  /* missing space in format specifier */
    
    return 0;
}
"""
    
    with open(c_file, 'w') as f:
        f.write(c_content)
    
    # Create a test Python file
    python_file = Path(test_dir) / "test.py"
    python_content = """#!/usr/bin/env python3
# Test Python file

def test_function():
    # Line that is way too long and exceeds the eighty character limit which should be caught
    x=1+2  # missing spaces around operators
    return x

if __name__ == "__main__":
    test_function()
"""
    
    with open(python_file, 'w') as f:
        f.write(python_content)
    
    return test_dir

def main():
    """Main test function"""
    print("UMDP3 Python Translation Test Suite")
    print("=" * 40)
    
    try:
        test_basic_functionality()
        test_dispatch_tables()
        test_fortran_specific()
        test_c_specific()
        
        # Create test files for demonstration
        test_dir = create_test_files()
        print(f"\nTest files created in: {test_dir}")
        print("You can now run the main checker on these files to see it in action.")
        
        print("\n" + "=" * 40)
        print("All tests completed successfully!")
        
    except Exception as e:
        print(f"Error during testing: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main()