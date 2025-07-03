# UMDP3 Checker - Python Version

This is a Python translation of the original Perl UMDP3 (Unified Model Development Process 3) compliance checker. The script validates code changes against coding standards and best practices.

## Overview

The UMDP3 checker examines code files for compliance with various coding standards including:

- Fortran coding standards (F90/f90 files)
- C coding standards (C files and headers)
- Python code style (using `pycodestyle`)
- Perl code style (basic checks)
- Shell script validation (using `shellcheck`)
- Universal checks (trailing whitespace, line length, etc.)

## Requirements

- Python 3.8 or higher
- External tools:
  - `pycodestyle` for Python code checking
  - `shellcheck` for shell script checking
  - `fcm` (File and Configuration Management) for version control operations

## Installation

1. Install Python dependencies:

```bash
pip install -r requirements.txt
```

2. Install external tools:

```bash
# On Ubuntu/Debian
sudo apt-get install shellcheck

# Install pycodestyle (included in requirements.txt)
pip install pycodestyle

# FCM needs to be installed separately based on your system
```

## Usage

### Basic Usage

```bash
python umdp3_check.py <branch> <whitelist_file>
```

Where:

- `<branch>`: Branch or directory to check (default: current directory)
- `<whitelist_file>`: Path to the whitelist includes file

### Examples

```bash
# Check current directory using whitelist
python umdp3_check.py . ../file/whitelist_includes.txt

# Check specific branch
python umdp3_check.py /path/to/branch ../file/whitelist_includes.txt
```

### Environment Variables

- `UMDP_CHECKER_THREADS`: Number of threads to use (default: 1)
- `SOURCE_UM_MIRROR`: Enable suite mode with specified source
- `UMDP_CHECKER_TRUNK_ERROR`: Control trunk mode error handling
  - `1`: Failures are fatal
  - `-1`: Skip UMDP3 checks for trunk
  - Other values: Failures are warnings only
- `CYLC_TASK_LOG_ROOT`: Enable Cylc logging to specified directory

## Features

### Multi-threading Support

The checker supports parallel processing using Python's ThreadPoolExecutor:
```bash
export UMDP_CHECKER_THREADS=4
python umdp3_check.py . whitelist.txt
```

### File Type Detection

Automatically detects and applies appropriate checks based on:

- File extensions (.F90, .f90, .c, .h, .py, .pl, .pm)
- MIME type detection using python-magic
- Content analysis

### Test Categories

#### Fortran Tests (Diff-based)

- Lowercase Fortran keywords
- OpenMP sentinel placement
- Keyword spacing
- GO TO usage restrictions
- WRITE statement formatting
- Variable naming conventions
- And many more...

#### Fortran Tests (File-based)

- IMPLICIT NONE presence
- Crown copyright statements
- Code owner comments
- Array initialization forms
- Forbidden STOP statements

#### C Tests

- Line length and formatting
- Deprecated identifier usage
- OpenMP pragma protection
- Format specifier spacing
- Header file compliance

#### Universal Tests

- Trailing whitespace detection
- Line length limits
- Tab character detection

## Architecture

### Core Components

1. **umdp3_check.py**: Main script handling:
   - Command line parsing
   - Branch/trunk detection
   - File processing coordination
   - Multi-threading management

2. **umdp3.py**: Core test implementations:
   - Individual compliance test functions
   - Fortran/C specific checks
   - Utility functions

3. **umdp3_dispatch_tables.py**: Test organization:
   - Maps test names to functions
   - Separates tests by file type and scope

4. **umdp3_critic_policy.py**: Perl code analysis:
   - Python implementation of Perl::Critic equivalent
   - Basic Perl best practices checking

### Operating Modes

#### Branch Mode (Default)

- Analyzes only changed files using FCM diff
- Checks added/modified lines against standards
- Suitable for development branches

#### Trunk Mode

- Analyzes entire source tree
- Triggered automatically for trunk URLs
- Can be enabled for specific repositories in suite mode

#### Suite Mode

- Activated by SOURCE_UM_MIRROR environment variable
- Handles extracted sources from multiple repositories
- Integrates with Rose/Cylc workflow systems

## Error Handling

The checker provides detailed error reporting:

- File-specific failure messages
- Test-specific error descriptions
- Thread-safe output collection
- Optional Cylc log file generation

Exit codes:

- `0`: All tests passed
- `>0`: Number of test failures (in non-trunk mode or when trunk errors are fatal)

## Testing

Run the test suite to verify functionality:

```bash
python test_umdp3.py
```

This will:

- Test basic functionality
- Verify dispatch table integrity
- Create sample files with common issues
- Demonstrate checker capabilities

## Differences from Perl Version

### Improvements

- Modern Python threading with ThreadPoolExecutor
- Better error handling and exception management
- Cleaner separation of concerns
- Type hints for better code maintainability

### Limitations

- Simplified Perl::Critic implementation (basic checks only)
- Some Perl-specific regex patterns adapted to Python
- Text::Balanced functionality replaced with simpler string processing

## Contributing

When adding new tests:

1. Add test function to `umdp3.py`
2. Update appropriate dispatch table in `umdp3_dispatch_tables.py`
3. Follow the standard interface: `test_function(lines: List[str]) -> int`
4. Return 0 for pass, >0 for number of failures
5. Use `add_extra_error()` for detailed error information

## Compatibility

This Python version maintains functional compatibility with the original Perl script:

- Same command line interface
- Same environment variable support
- Same whitelist file format
- Same FCM integration
- Same output format and exit codes

## License

Crown copyright Met Office. All rights reserved.
For further details please refer to the file LICENSE which you should have received as part of this distribution.
