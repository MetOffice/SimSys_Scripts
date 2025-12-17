# Migration Guide: Perl to Python UMDP3 Checker

This guide helps users migrate from the original Perl UMDP3 checker to the new Python version.

## Quick Start

If you're already using the Perl version, the Python version should be a drop-in replacement:

```bash
# Old Perl command
perl umdp3_check.pl . whitelist_includes.txt

# New Python command
python umdp3_check.py . whitelist_includes.txt
```

## Key Differences

### Command Line Interface

- **Perl**: `perl umdp3_check.pl <branch> <whitelist>`
- **Python**: `python umdp3_check.py <branch> <whitelist>`

The arguments and behavior are identical.

### Dependencies

#### Perl Version Required

- Perl 5.010+
- Perl modules: threads, Text::Balanced, File::MimeInfo::Magic, IPC::Run
- External tools: fcm, pycodestyle, shellcheck, perlcritic

#### Python Version Required

- Python 3.8+
- Python packages: python-magic, pycodestyle
- External tools: fcm, pycodestyle, shellcheck

### Environment Variables

All environment variables work identically:

| Variable | Purpose | Values |
|----------|---------|---------|
| `UMDP_CHECKER_THREADS` | Thread count | Integer (default: 1) |
| `SOURCE_UM_MIRROR` | Suite mode source | Path to mirror |
| `UMDP_CHECKER_TRUNK_ERROR` | Trunk error handling | 1, -1, or 0 |
| `CYLC_TASK_LOG_ROOT` | Cylc logging | Path to log directory |

### Threading Implementation

#### Perl Version

- Uses Perl threads with manual thread management
- Complex work distribution and recycling logic
- Shared memory variables with explicit locking

#### Python Version

- Uses ThreadPoolExecutor for cleaner thread management
- Automatic work distribution
- Thread-safe data structures and queue management

## Functional Compatibility

### Test Coverage

Both versions implement identical tests:

- **Fortran Tests**: 20+ diff-based tests, 7 file-based tests
- **C Tests**: 3 diff-based tests, 10 file-based tests
- **Universal Tests**: Trailing whitespace, line length, tabs
- **External Tools**: pycodestyle, shellcheck, perl critic

### Output Format

The Python version produces identical output:

- Same error message format
- Same file reporting structure
- Same exit codes (0 for success, >0 for failure count)
- Same Cylc logging format

### File Processing

- Identical FCM integration
- Same branch vs trunk detection logic
- Same file type detection and handling
- Same whitelist processing

## Migration Steps

### 1. Install Python Version

```bash
cd script_umdp3_checker/python_version
python setup.py
```

### 2. Test Compatibility

Run both versions on the same branch and compare outputs:

```bash
# Test with Perl version
perl ../bin/umdp3_check.pl . ../file/whitelist_includes.txt > perl_output.txt

# Test with Python version
python umdp3_check.py . ../file/whitelist_includes.txt > python_output.txt

# Compare outputs
diff perl_output.txt python_output.txt
```

### 3. Update Scripts/Workflows

Replace Perl calls with Python calls in:

- CI/CD pipelines
- Rose/Cylc workflows
- Build scripts
- Git hooks

### 4. Update Documentation

Update any documentation that references the Perl version.

## Known Differences

### Perl::Critic vs Python Implementation

The Python version includes a simplified Perl::Critic equivalent:

#### Perl Version

- Full Perl::Critic with all policies
- Configurable severity levels
- Advanced Perl-specific analysis

#### Python Version

- Basic Perl best practices checking
- Checks for strict/warnings usage
- Simple style and complexity analysis

### Text Processing

Minor differences in string/regex handling:

#### Perl Version

- Uses Text::Balanced for quoted string removal
- Perl-native regex features

#### Python Version

- Simplified quoted string removal
- Python regex patterns (functionally equivalent)

### Error Reporting

Both versions provide the same information but with slight formatting differences:

- Error messages are identical
- Extra error information uses same format
- Thread-specific output handling differs internally but produces same results

## Performance Comparison

### Threading Efficiency

- **Perl**: Manual thread management, can be memory intensive
- **Python**: ThreadPoolExecutor with better resource management

### Memory Usage

- **Perl**: Shared variables across threads
- **Python**: Process memory with thread-safe collections

### Startup Time

- **Perl**: Faster startup for small files
- **Python**: Better performance for large file sets

## Troubleshooting

### Common Migration Issues

#### 1. Missing Dependencies

```bash
# Error: No module named 'magic'
pip install python-magic

# Error: pycodestyle not found
pip install pycodestyle
```

#### 2. Permission Issues

```bash
# Make script executable
chmod +x umdp3_check.py
```

#### 3. Path Issues

```bash
# Add current directory to Python path
export PYTHONPATH=$PYTHONPATH:$(pwd)
```

#### 4. FCM Integration

Both versions require identical FCM setup and environment.

### Verification Tests

#### Test 1: Basic Functionality

```bash
python test_umdp3.py
```

#### Test 2: Compare Outputs

```bash
# Create test files
python test_umdp3.py

# Run both versions on test files
perl ../bin/umdp3_check.pl test_directory ../file/whitelist_includes.txt
python umdp3_check.py test_directory ../file/whitelist_includes.txt
```

#### Test 3: Threading Performance

```bash
export UMDP_CHECKER_THREADS=4
time python umdp3_check.py large_branch whitelist.txt
```

## Rollback Strategy

If issues occur during migration:

### 1. Keep Perl Version Available

Don't remove the original Perl version until Python version is fully validated.

### 2. Gradual Migration

- Test Python version on development branches first
- Run both versions in parallel during transition
- Switch production usage only after thorough testing

### 3. Fallback Commands

Keep wrapper scripts that can switch between versions:

```bash
#!/bin/bash
if [ "$USE_PYTHON_UMDP3" = "1" ]; then
    python umdp3_check.py "$@"
else
    perl umdp3_check.pl "$@"
fi
```

## Support and Issues

### Getting Help

1. Check this migration guide
2. Review README.md for Python version specifics
3. Compare outputs between versions for validation
4. Test with known good branches first

### Reporting Issues

When reporting migration issues, include:

- Python version (`python --version`)
- Error messages from both versions
- Sample files that demonstrate differences
- Environment variable settings
- FCM version and configuration

The Python version is designed to be functionally identical to the Perl version while providing better maintainability and performance characteristics.
