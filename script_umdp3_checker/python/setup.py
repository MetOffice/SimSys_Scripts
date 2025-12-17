#!/usr/bin/env python3
# *****************************COPYRIGHT*******************************
# (C) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file LICENSE
# which you should have received as part of this distribution.
# *****************************COPYRIGHT*******************************

"""
Setup script for UMDP3 Python checker
"""

import os
import sys
import subprocess
from pathlib import Path

def check_python_version():
    """Check Python version compatibility"""
    if sys.version_info < (3, 8):
        print("ERROR: Python 3.8 or higher is required")
        print(f"Current version: {sys.version}")
        return False
    print(f"✓ Python version {sys.version_info.major}.{sys.version_info.minor} is compatible")
    return True

def install_python_dependencies():
    """Install Python dependencies"""
    print("Installing Python dependencies...")
    try:
        subprocess.run([sys.executable, '-m', 'pip', 'install', '-r', 'requirements.txt'],
                      check=True)
        print("✓ Python dependencies installed successfully")
        return True
    except subprocess.CalledProcessError as e:
        print(f"ERROR: Failed to install Python dependencies: {e}")
        return False

def check_external_tools():
    """Check for required external tools"""
    tools = {
        'pycodestyle': 'Python code style checker',
        'shellcheck': 'Shell script checker',
        'fcm': 'File and Configuration Management'
    }

    missing_tools = []

    for tool, description in tools.items():
        try:
            subprocess.run([tool, '--version'],
                          stdout=subprocess.DEVNULL,
                          stderr=subprocess.DEVNULL,
                          check=True)
            print(f"✓ {tool} is available ({description})")
        except (subprocess.CalledProcessError, FileNotFoundError):
            print(f"⚠ {tool} is not available ({description})")
            missing_tools.append(tool)

    if missing_tools:
        print("\nMissing tools installation suggestions:")
        if 'shellcheck' in missing_tools:
            print("  shellcheck: sudo apt-get install shellcheck  # Ubuntu/Debian")
            print("             brew install shellcheck          # macOS")
        if 'fcm' in missing_tools:
            print("  fcm: Please install FCM according to your system requirements")
        if 'pycodestyle' in missing_tools:
            print("  pycodestyle: pip install pycodestyle")

    return len(missing_tools) == 0

def make_executable():
    """Make main script executable"""
    script_path = Path('umdp3_check.py')
    if script_path.exists():
        os.chmod(script_path, 0o755)
        print("✓ Made umdp3_check.py executable")
    else:
        print("⚠ umdp3_check.py not found")

def run_tests():
    """Run basic tests"""
    print("\nRunning basic tests...")
    try:
        subprocess.run([sys.executable, 'test_umdp3.py'], check=True)
        print("✓ Basic tests passed")
        return True
    except subprocess.CalledProcessError:
        print("⚠ Some tests failed - check output above")
        return False

def create_sample_whitelist():
    """Create a sample whitelist file if one doesn't exist"""
    whitelist_path = Path('sample_whitelist.txt')
    if not whitelist_path.exists():
        sample_content = """# Sample whitelist includes file
# Add approved include files here, one per line
src/include/other/c_io.h
src/include/other/exceptions.h
"""
        with open(whitelist_path, 'w') as f:
            f.write(sample_content)
        print("✓ Created sample_whitelist.txt")

def main():
    """Main setup function"""
    print("UMDP3 Python Checker Setup")
    print("=" * 40)

    success = True

    # Check Python version
    if not check_python_version():
        success = False

    # Install Python dependencies
    if success and not install_python_dependencies():
        success = False

    # Check external tools
    tools_available = check_external_tools()
    if not tools_available:
        print("\n⚠ Warning: Some external tools are missing.")
        print("The checker will still work but some features may be limited.")

    # Make script executable
    make_executable()

    # Create sample files
    create_sample_whitelist()

    # Run tests
    if success:
        run_tests()

    print("\n" + "=" * 40)
    if success:
        print("✓ Setup completed successfully!")
        print("\nUsage:")
        print("  python umdp3_check.py <branch> <whitelist_file>")
        print("  python umdp3_check.py . sample_whitelist.txt")
    else:
        print("⚠ Setup completed with warnings/errors")
        print("Please address the issues above before using the checker")

if __name__ == "__main__":
    main()