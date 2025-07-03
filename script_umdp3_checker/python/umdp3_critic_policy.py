# *****************************COPYRIGHT*******************************
# (C) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file LICENSE
# which you should have received as part of this distribution.
# *****************************COPYRIGHT*******************************

"""
UMDP3 Critic Policy module for Perl code checking
Python translation of the original Perl module
"""

import re
import subprocess
from typing import List, Dict

# Declare version
VERSION = '13.2.0'

class UMDP3CriticPolicy:
    """Python equivalent of Perl::Critic policy for UMDP3"""
    
    def __init__(self):
        self.allowed_spellings = ['CreateBC', 'FCM', 'UMUI', 'NEMO', 'CICE']
        
    def get_umdp3_critic_policy(self):
        """Get UMDP3 critic policy - returns self for Python implementation"""
        return self
        
    def critique(self, perl_code: str) -> List[str]:
        """
        Critique Perl code and return violations
        This is a simplified Python implementation of Perl::Critic functionality
        """
        violations = []
        
        if isinstance(perl_code, str):
            lines = perl_code.splitlines()
        else:
            # Handle reference case
            lines = str(perl_code).splitlines()
        
        # Basic Perl best practices checks
        violations.extend(self._check_strict_and_warnings(lines))
        violations.extend(self._check_pod_documentation(lines))
        violations.extend(self._check_subroutine_prototypes(lines))
        violations.extend(self._check_variable_declarations(lines))
        violations.extend(self._check_control_structures(lines))
        violations.extend(self._check_code_complexity(lines))
        violations.extend(self._check_naming_conventions(lines))
        violations.extend(self._check_error_handling(lines))
        
        return violations
    
    def _check_strict_and_warnings(self, lines: List[str]) -> List[str]:
        """Check for use strict and use warnings"""
        violations = []
        has_strict = False
        has_warnings = False
        
        for line in lines:
            if re.search(r'^\s*use\s+strict\s*;', line):
                has_strict = True
            elif re.search(r'^\s*use\s+warnings\s*;', line):
                has_warnings = True
        
        if not has_strict:
            violations.append("Code before strictures are enabled")
        
        if not has_warnings:
            violations.append("Code before warnings are enabled")
        
        return violations
    
    def _check_pod_documentation(self, lines: List[str]) -> List[str]:
        """Check for POD documentation"""
        violations = []
        has_pod = False
        
        for line in lines:
            if re.search(r'^\s*=\w+', line):
                has_pod = True
                break
        
        # Check for basic documentation requirements
        if not has_pod and len(lines) > 50:  # Only require POD for larger files
            violations.append("No POD documentation found")
        
        return violations
    
    def _check_subroutine_prototypes(self, lines: List[str]) -> List[str]:
        """Check subroutine prototypes"""
        violations = []
        
        for i, line in enumerate(lines):
            if re.search(r'^\s*sub\s+\w+\s*\(', line):
                violations.append(f"Subroutine prototypes used at line {i+1}")
        
        return violations
    
    def _check_variable_declarations(self, lines: List[str]) -> List[str]:
        """Check variable declarations"""
        violations = []
        
        for i, line in enumerate(lines):
            # Check for global variables
            if re.search(r'^\s*our\s+[\$@%]', line):
                violations.append(f"Global variable declared at line {i+1}")
            
            # Check for variables declared in wrong scope
            if re.search(r'^\s*my\s+[\$@%]\w+\s*=\s*shift', line):
                violations.append(f"Variable assignment from shift at line {i+1}")
        
        return violations
    
    def _check_control_structures(self, lines: List[str]) -> List[str]:
        """Check control structures"""
        violations = []
        
        for i, line in enumerate(lines):
            # Check for postfix control structures
            if re.search(r'\w+\s+(if|unless|while|until|for|foreach)\s+', line):
                if not re.search(r'^\s*(if|unless|while|until|for|foreach)', line):
                    violations.append(f"Postfix control structure at line {i+1}")
            
            # Check for complex control structures
            if re.search(r'(if|unless).*?(if|unless)', line):
                violations.append(f"Complex control structure at line {i+1}")
        
        return violations
    
    def _check_code_complexity(self, lines: List[str]) -> List[str]:
        """Check code complexity"""
        violations = []
        
        # Check for long subroutines
        in_sub = False
        sub_start = 0
        sub_name = ""
        
        for i, line in enumerate(lines):
            if match := re.search(r'^\s*sub\s+(\w+)', line):
                in_sub = True
                sub_start = i
                sub_name = match.group(1)
            elif re.search(r'^\s*}\s*$', line) and in_sub:
                sub_length = i - sub_start
                if sub_length > 50:  # Arbitrary threshold
                    violations.append(f"Subroutine '{sub_name}' too long ({sub_length} lines)")
                in_sub = False
        
        return violations
    
    def _check_naming_conventions(self, lines: List[str]) -> List[str]:
        """Check naming conventions"""
        violations = []
        
        for i, line in enumerate(lines):
            # Check for non-descriptive variable names
            if re.search(r'\b(my|our)\s+[\$@%]([a-z]|tmp|temp)\b', line):
                violations.append(f"Non-descriptive variable name at line {i+1}")
            
            # Check for camelCase in Perl (should be snake_case)
            if re.search(r'\b(my|our)\s+[\$@%][a-z]+[A-Z]', line):
                violations.append(f"CamelCase variable name at line {i+1}")
        
        return violations
    
    def _check_error_handling(self, lines: List[str]) -> List[str]:
        """Check error handling"""
        violations = []
        
        for i, line in enumerate(lines):
            # Check for die without error checking
            if re.search(r'\bdie\b', line) and not re.search(r'(or|unless|\|\|)', line):
                violations.append(f"Die statement without error checking at line {i+1}")
            
            # Check for open without error checking
            if re.search(r'\bopen\s*\(', line) and not re.search(r'(or|unless|\|\|)', line):
                violations.append(f"Open statement without error checking at line {i+1}")
        
        return violations