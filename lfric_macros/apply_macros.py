#!/usr/bin/env python3
##############################################################################
# (c) Crown copyright 2024 Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
##############################################################################
"""
Script for lfric_apps upgrade macros. Pre-processes the macros and then applies
them to rose-stem apps.
Warning: Should only be run on a Test branch or by CR on commit to trunk
"""

import argparse
import ast
import os
import re
import shutil
import subprocess
import tempfile
import networkx as nx
from collections import defaultdict

BLACK_COMMAND = "black --line-length=80"
CLASS_NAME_REGEX = r"vn\d+(_t\d+\w*)?"
TAG_REGEX = r"\s*=\s*[\"']\s*(\S+)\s*[\"']"


def run_command(command, shell=False):
    """
    Run a subprocess command and return the result object
    Inputs:
        - command, str with command to run
    Outputs:
        - result object from subprocess.run
    """
    if not shell:
        command = command.split()
    return subprocess.run(
        command,
        capture_output=True,
        text=True,
        timeout=120,
        shell=shell,
        check=False,
    )


def check_environment():
    """
    Check that required dependencies are loaded in the current environment
    Cylc >= 8.0
    Black
    Isort
    """

    result = run_command("cylc --version")
    major = result.stdout.split(".")[0]
    if int(major) < 8:
        raise Exception(
            "The current cylc environment must be at least Cylc 8. Currently loaded is "
            f"version {result.stdout}"
        )

    result = run_command("black --version")
    if result.returncode:
        raise Exception("'black' must be available to run this script")

    result = run_command("isort --version")
    if result.returncode:
        raise Exception("'isort' must be available to run this script")


def get_root_path(wc_path):
    """
    Given a path to a working copy, ensure the path and working copy are both
    valid and return the path to the working copy root directory
    Inputs:
        - wc_path, command line argument to a working copy
    Outputs:
        - str, path to the top level of the apps working copy
    """

    # Run fcm info on the given path to ensure it is
    command = f"fcm info {wc_path}"
    result = run_command(command)
    if result.returncode:
        print(
            "[WARN] - Could not find the fcm root path for the apps working copy. "
            "Defaulting to assuming the provided path in the root path."
        )
        return wc_path

    # If no error, then search through output for the working copy root path
    # return the found path
    for line in result.stdout.split("\n"):
        if line.startswith("Working Copy Root Path"):
            return line.split(":", 1)[1].strip()
    raise Exception(
        "Couldn't extract the Working Copy Root path from the output of the "
        f"command '{command}'"
    )


def apply_styling(filepath):
    """
    Run black on a given file
    Inputs:
        - filepath, the path to the file to run black on
    """
    result = run_command(f"isort --profile black {filepath}")
    if result.returncode:
        raise Exception(
            "Running 'isort' as a subprocess failed. This may indicate a "
            "syntax error with your macro.\nThe error message produced "
            f"was:\n\n{result.stderr}"
        )
    result = run_command(f"{BLACK_COMMAND} {filepath}")
    if result.returncode:
        raise Exception(
            "Running 'black' as a subprocess failed. This may indicate a "
            "syntax error with your macro.\nThe error message produced "
            f"was:\n\n{result.stderr}"
        )


def read_versions_file(meta_dir):
    """
    Read in a versions.py and parse out blank lines
    Inputs:
        - meta_dir, the path to a directory containing a versions.py file
    Returns:
        - a list of lines in the versions.py file with blank lines removed
    """

    version_file = os.path.join(meta_dir, "versions.py")

    # Read in versions file and then remove all blank lines
    with open(version_file) as f:
        file_raw = f.readlines()
    file_parsed = []
    for line in file_raw:
        if len(line.strip()) > 0:
            file_parsed.append(line)

    return file_parsed


def split_macros(parsed_versions):
    """
    Read through a versions.py file and split macros into individual strings
    Inputs:
        - parsed_versions, a list of lines from a versions.py file with blank
          lines removed
    Returns:
        - a list of strings of individual macros
    """

    # construct a string for each macro and save in macros list
    macros = []
    macro = ""
    in_macro = False
    for line in parsed_versions:
        if line.startswith("class vn") and not line.startswith("class vnXX_txxx"):
            # If the macro string is set, then append to the list. If it's
            # empty then this is the first macro we're looking at, so nothing to
            # append
            if macro:
                macros.append(macro)
            in_macro = True
            macro = ""
        if in_macro:
            macro += line
    # Make sure to record final macro
    if macro:
        macros.append(macro)

    return macros


def deduplicate_list(lst):
    """
    Remove duplicate items from a list, keeping the first
    Need to preserve order so not using a set
    Inputs:
        - lst, the list to deduplicate
    Returns:
        - the deduplicated list
    """
    deduplicated = []
    for item in lst:
        if item not in deduplicated:
            deduplicated.append(item)

    return deduplicated


def match_python_import(line):
    """
    Return true if string has form 'import *' or 'from * import *'
    Inputs:
        - line, str to match
    """
    if re.match(r"import \w+", line) or re.match(r"from [\.\w]+ import [\.\w]+", line):
        return True
    return False


def read_python_imports(path):
    """
    Given a path to a python file, return a set containing info of all module
    imports in the file
    Inputs:
        - path, path to a python file
    Returns:
        - set containing data of python imports in given file
    """

    with open(path) as fh:
        root = ast.parse(fh.read(), path)

    imports = set()

    for node in ast.walk(root):
        if isinstance(node, ast.Import):
            module = []
        elif isinstance(node, ast.ImportFrom):
            module = node.module.split(".")
        else:
            continue

        for n in node.names:
            imports.add((tuple(module), tuple(n.name.split(".")), n.asname))
    return imports


def banner_print(message):
    """Print a simple banner message"""
    print(f"\n{(len(message)+4)*'*'}\n* {message} *\n{(len(message)+4)*'*'}\n")


class ApplyMacros:
    """
    Object to hold data + methods to apply upgrade macros in lfric_apps
    """

    def __init__(self, tag, cname, version, apps, core, jules):
        self.tag = tag
        if cname:
            self.class_name = cname
        else:
            # The default class name is the After Tag with the '.'
            #  removed from the version
            self.class_name = tag.replace(".", "")
        self.temp_dirs = {}
        self.root_path = get_root_path(apps)
        self.core_source = self.get_dependency_paths(core, "lfric_core")
        # The Jules source is temporarily ignored as Jules Shared metadata has a
        # copy in LFRic, rather than using the Jules version. The LFRic build
        # system needs modifying to enable this
        # self.jules_source = self.get_dependency_paths(jules, "jules")
        self.central_rose_meta = False
        self.set_rose_meta_path()
        if version is None:
            self.version = re.search(r".*vn(\d+\.\d+)(_.*)?", tag).group(1)
        else:
            self.version = version
        self.author = None
        self.ticket_number = None
        # All parsed macros per metadata section
        self.parsed_macros = defaultdict(list)
        # Parsed macro with desired after tag, per metadata section
        self.target_macros = {}
        self.meta_dirs = set()
        self.sections_with_macro = []
        self.python_imports = set()
        self.upgraded_core = False

    def set_rose_meta_path(self):
        """
        Set up the ROSE_META_PATH environment variable in order to use the Jules
        and Core metadata. We also add the working copy root path as this should
        allow the script to be run from anywhere.
        When Jules Shared from Jules is enabled, self.jules_source will need
        adding here
        """
        if os.path.isdir(os.path.join(self.root_path, "rose-meta")):
            # For backwards compatibility with central rose-meta imports
            rose_meta_path = (
                f"{os.path.join(self.root_path, 'rose-meta')}:"
                f"{os.path.join(self.core_source, 'rose-meta')}"
            )
            self.central_rose_meta = True
        else:
            rose_meta_path = f"{self.root_path}:{self.core_source}"
        os.environ["ROSE_META_PATH"] = rose_meta_path

    def parse_application_section(self, meta_dir):
        """
        Given a path to a metadata directory, parse out the application/science
        section. Try to remove the apps, core and jules root paths. Then try to
        remove trailing /HEAD or /versions.py
        Inputs:
            - meta_dir, path to a metadata dir
        Returns:
            - path to the metadata directory with the root path removed
        """
        meta_dir = meta_dir.removeprefix(self.root_path)
        meta_dir = meta_dir.removeprefix(self.core_source)
        # Reinstate when using Jules Shared from Jules
        # meta_dir = meta_dir.removeprefix(self.jules_source)
        meta_dir = meta_dir.removeprefix("/")

        meta_dir = meta_dir.removesuffix("/HEAD")
        meta_dir = meta_dir.removesuffix("/versions.py")

        return meta_dir

    ############################################################################
    # Get Working Copy Functions
    ############################################################################

    def get_dependency_paths(self, source, repo):
        """
        Parse the core or jules command line arguments to get the path to a
        working copy.
        If the source isn't defined, first populate the source by reading the
        dependencies.sh file.
        If the source is an fcm url check it out to a temporary location
        Inputs:
            - source, str, The command line argument for the source. If not set
                           this will be None
            - repo, str, Either "lfric_core" or "jules" depending on which
                         source is being found
        Outputs:
            - str, The path to the source working copy to use
        """

        # If source is None then read the dependencies.sh file for the source
        if source is None:
            source = self.read_dependencies(repo)

        # If the source exists as a path then return as is
        if os.path.exists(os.path.expanduser(source)):
            return os.path.expanduser(source)
        if ":" in source:
            source_path = os.path.expanduser(source.split(":")[1])
            if os.path.exists(source_path):
                return source_path

        # Check that the source looks like an fcm keyword, raise an error if not
        if "fcm:" not in source:
            raise Exception(
                f"The {repo} source: {source}, was not found as a working copy "
                "and does not look like an fcm url. Please check the source."
                "If not set on the command then the dependencies.sh file is "
                "being used."
            )

        # Checkout the fcm source to a temporary location
        source = self.fcm_temp_copy(source, repo)
        return source

    def read_dependencies(self, repo):
        """
        Read through the dependencies.sh file for the source of the repo defined
        by repo. Uses self.root_path to locate the dependencies.sh file.
        Inputs:
            - repo, str, Either "lfric_core" or "jules" depending on which
                         source is being found. The function will work with
                         other repos, but not intended to within this script.
        Outputs:
            - str, The source as defined by the dependencies.sh file
        """
        dependencies_path = os.path.join(self.root_path, "dependencies.sh")
        source = ""
        rev = ""
        with open(dependencies_path, "r") as dependencies_file:
            # Loop over lines in dependencies.sh for lines relevant to repo
            for line in dependencies_file:
                line = line.strip()
                if line.startswith(f"export {repo}_rev"):
                    rev = line.split("=")[1]
                if line.startswith(f"export {repo}_sources"):
                    source = line.split("=")[1]
        # If source not set then default to trunk
        if source == "":
            # lfric_core doesn't match the url
            if repo == "lfric_core":
                source = "fcm:lfric.xm_tr"
            else:
                source = f"fcm:{repo}.xm_tr"
        # If a revision set then append to source
        # Defaults to the head of the source
        # Only do this if it's an fcm url
        if rev != "" and "fcm:" in source:
            source = f"{source}@{rev}"
        return source

    def fcm_temp_copy(self, url, repo):
        """
        Given an fcm url as a source, checkout a working copy to a temp location
        and return the path. Update self.temp_dirs with temporary directory path
        Inputs:
            - url, str, An fcm url of the source
            - repo, str, the name of the source being found
        Outputs:
            - str, The path to the temporary working copy
        """

        print(f"Extracting {url} to a temporary directory")
        tempdir = tempfile.mkdtemp()
        self.temp_dirs[repo] = tempdir
        command = f"fcm co {url} {tempdir}"
        result = run_command(command)
        if result.returncode:
            raise Exception(
                f"Failed to checkout from URL {url} into directory {tempdir} "
                f"with error message:\n\n{result.stderr}"
            )
        return tempdir

    ############################################################################
    # Preprocess Macros Functions
    ############################################################################

    def find_meta_dirs(self, path):
        """
        Searching from a working copy root path, return a list of paths to all
        the rose-meta directories using os.walk(). Search by looking
        for versions.py files
        Outputs:
            - str, stdout of find command looking for versions.py files
        """

        for dirpath, dirnames, filenames in os.walk(path, followlinks=True):
            dirnames[:] = [d for d in dirnames if d not in [".svn"]]
            if "versions.py" in filenames:
                self.meta_dirs.add(dirpath)

    def parse_macro(self, macro, meta_dir):
        """
        Given a macro string save the macro info in a dictionary
        Inputs:
            - macro, an upgrade macro that matches the class name we are
              looking for
            - meta_dir, the path to the rose metadata directory
        Returns:
            - dictionary of the parsed macro, containing before tag, after tag and
              commands
        """

        version_file = os.path.join(meta_dir, "versions.py")

        ticket_details = re.search(r"Upgrade .* (#\d+) by (\S+.*)", macro)
        try:
            ticket_number = ticket_details.group(1)
            author = ticket_details.group(2).rstrip('".')
            author = author.strip("<>")
        except AttributeError:
            ticket_number = "TTTT"
            author = "Unknown"
            pass

        class_name = re.search(r"class (vn\d+_t\d+)", macro).group(1)

        # Search for the before tag
        # Raise an exception if these are missing
        try:
            before_tag = re.search(rf"BEFORE_TAG{TAG_REGEX}", macro).group(1)
            after_tag = re.search(rf"AFTER_TAG{TAG_REGEX}", macro).group(1)
        except AttributeError as exc:
            raise Exception(
                "Couldn't find a Before/After tag for the requested "
                f"macro in the file {version_file}"
            ) from exc

        # Finally extract the lines which are defining the commands run by the
        # macro. Record these as a single block as each command may be over
        # multiple lines
        in_function = False
        commands = ""
        for line in macro.split("\n"):
            line_stripped = line.strip()
            if line_stripped.startswith("def upgrade(self"):
                in_function = True
                continue
            if (
                line_stripped.startswith("return config, self.reports")
                or line_stripped.startswith("# Input your macro commands here")
                or line_stripped.lower().startswith("# add settings")
                or not in_function
                or line_stripped.startswith("return config")
            ):
                continue
            commands += line + "\n"

        # Record macro details
        return {
            "before_tag": before_tag,
            "after_tag": after_tag,
            "commands": commands,
            "ticket_number": ticket_number,
            "author": author,
            "class_name": class_name,
        }

    def remove_macro(self, contents, meta_dir):
        """
        Rewrite the contents of a versions.py file without the newly added
        macro. Run black on the new file.
        Inputs:
            - contents, list of lines from the original file
            - meta_dir, the path to the versions.py file being rewritten
        """

        filepath = os.path.join(meta_dir, "versions.py")
        temppath = os.path.join(meta_dir, ".versions.py")

        with open(temppath, "w") as f:
            in_new_macro = False
            for line in contents:
                if re.match(r"class vn\d+_t\d+", line):
                    if in_new_macro:
                        in_new_macro = False
                    elif self.class_name in line:
                        in_new_macro = True
                if not in_new_macro:
                    f.write(line)

        apply_styling(temppath)

        if not os.path.getsize(temppath) > 0:
            raise Exception(
                f"The file modified at {filepath} has zero size, indicating "
                "something has gone wrong"
            )

        os.rename(temppath, filepath)

    def find_last_macro(self, macros, meta_dir):
        """
        Given a list of macros, determine the after tag of the final macro in
        the chain. Start from assuming the first before tag is the Version
        Number.
        Inputs:
            - macros, a list of macro strings
        Returns:
            - str, the after tag of the final macro in the chain
        """

        after_tag = f"vn{self.version}"
        while len(macros) > 0:
            for macro in macros:
                regexp = re.compile(rf"BEFORE_TAG\s*=\s*[\"']{after_tag}[\"']")
                if regexp.search(macro):
                    try:
                        after_tag = re.search(rf"AFTER_TAG{TAG_REGEX}", macro).group(1)
                    except AttributeError as exc:
                        raise Exception(
                            f"Couldn't find an after tag in the macro:\n{macro}"
                        ) from exc
                    found_macro = macro
                    macros.remove(found_macro)
                    break
            else:
                raise Exception(
                    f"Couldn't find the before tag '{after_tag}' in macros in "
                    f"the versions.py file at {meta_dir}"
                )
        return after_tag

    def find_macro(self, meta_dir, macros):
        """
        Read through a list of macros, trying to find the macro with a class
        name that matches the class name supplied (either from the tag or
        cname option). If this is present then return the macro.
        Inputs:
            - meta_dir, str, The path to the rose metadata directory containing
              these macros
            - macros, a list of macro strings
        Returns:
            - String containing the macro. Empty if the macro isn't found
        """

        version_file = os.path.join(meta_dir, "versions.py")

        # Find the macro we're interested in
        for macro in macros:
            try:
                macro_name = re.search(rf"class ({CLASS_NAME_REGEX})\(", macro).group(1)
            except AttributeError as exc:
                raise Exception(
                    "Unable to determine macro class name in "
                    f"{version_file} in macro:\n{macro}"
                ) from exc
            # Parse the macro if this is the one we're interested in
            if macro_name == self.class_name:
                return macro
        return ""

    def get_full_import_path(self, imp):
        """
        Search through the Core, Jules and Apps working copies to get the full
        path to a metadata directory
        Inputs:
            - imp, the import statement without the full path
        Returns:
            - the import statement containing the full path - raises an error if
              not found
        """

        # TODO: Reinstate Jules checks when using Jules Metadata from Jules

        # For backwards compatibility with central rose-meta imports
        if self.central_rose_meta:
            core_imp = os.path.join(self.core_source, "rose-meta", imp)
            apps_imp = os.path.join(self.root_path, "rose-meta", imp)
        else:
            core_imp = os.path.join(self.core_source, imp)
            apps_imp = os.path.join(self.root_path, imp)

        if os.path.exists(core_imp):
            return core_imp
        if os.path.exists(apps_imp):
            return apps_imp
        if os.path.exists(os.path.dirname(core_imp)):
            return core_imp
        if os.path.exists(os.path.dirname(apps_imp)):
            return apps_imp

        raise Exception(
            f"Couldn't find the import '{imp}' in any of the Apps, Core or "
            "Jules sources."
        )

    def read_meta_imports(self, meta_dir, flag="import"):
        """
        Read a rose-meta.conf and record which other metadata files are imported
        by this metadata.
        Inputs:
            - meta_dir, path to the rose metadata directory
            - flag, either 'import' or 'meta'. Searches for lines in the config
              file starting flag=. If 'meta', then will return the import
              statement on that line
        """

        if flag == "import":
            meta_file = os.path.join(meta_dir, "HEAD", "rose-meta.conf")
        else:
            meta_file = meta_dir

        imports = []
        with open(meta_file, "r") as f:
            in_import_list = False
            for line in f:
                line = line.strip()
                if not line:
                    # skip blank lines
                    continue
                if line.startswith(f"{flag}="):
                    in_import_list = True
                if in_import_list:
                    if line.startswith("=") or line.startswith(f"{flag}="):
                        # Split the import line by '=' then take the rhs
                        # Then remove the trailing '/HEAD'
                        # Then prepend the path to the working copy
                        imp = line.split("=", 1)[1].removesuffix("/HEAD")
                        imp = self.get_full_import_path(imp)
                        imports.append(imp)
                    else:
                        break
        return imports

    def write_python_imports(self, meta_dir):
        """
        Write out all required python module imports at the top of a versions.py
        file. New imports are written at the top of the current import
        statement section. Read imports from self.python_imports
        Inputs:
            - meta_dir, path to the metadata directory with a versions.py file
        """

        filepath = os.path.join(meta_dir, "versions.py")
        temppath = os.path.join(meta_dir, ".versions.py")

        # Work out where we need to insert the new imports
        # For simplicity, do this at the beginning of the existing imports
        # Should be safe as versions.py files always require importing code
        # from rose
        with open(filepath) as f:
            versions_file = f.readlines()
        for i, line in enumerate(versions_file):
            if match_python_import(line):
                insertion_index = i
                break

        found_imports = read_python_imports(filepath)
        missing_imports = self.python_imports.difference(found_imports)
        for mod, name, alias in missing_imports:
            imp_str = f"import {','.join(n for n in name)}"
            if mod:
                imp_str = f"from {'.'.join(m for m in mod)} {imp_str}"
            if alias:
                imp_str += f" as {alias}"
            # Don't add versionXX_YY import statements
            if re.match(r"from [\.]?version\d+_\d+ import \*", imp_str):
                continue
            versions_file.insert(insertion_index, imp_str)

        with open(temppath, "w") as f:
            for line in versions_file:
                f.write(line.strip("\n") + "\n")

        os.rename(temppath, filepath)

    def determine_import_order(self, app):
        """
        Work out what order metadata is imported. This recursively works through
        import statements recorded in self.target_macros["imports"]. Produces a
        list of the order in which macro commands should be applied (this should
        be the same order as the imports)
        Inputs:
            - app, the application or science section which we are trying to
              find the import order for
        Returns:
            - A list of meta imports in the correct order
        """

        # If using central metadata, use the basename, otherwise the full path
        if self.central_rose_meta:
            app_name = os.path.basename(app)
        else:
            app_name = app

        import_list = [app_name]

        try:
            imports = self.target_macros[app]["imports"]
        except KeyError:
            # Jules Shared directories will produce a key error - these are
            # guaranteed to not import anything
            return []
        for meta_import in imports:
            import_list = self.determine_import_order(meta_import) + import_list

        return deduplicate_list(import_list)

    def combine_macros(self, import_order):
        """
        Combine macro commands, adding commands in the order determined by
        import_order.
        Inputs:
            - import_order, the metadata import order to match the order of
              marcro commands.
        Returns:
            - string, combined macro commands
        """

        full_command = ""
        for meta_import in import_order:
            meta_import = self.get_full_import_path(meta_import)
            if (
                meta_import in self.target_macros
                and self.target_macros[meta_import]["commands"]
            ):
                # Add a comment labelling where these commands came from
                full_command += (
                    "        # Commands From: "
                    f"{self.parse_application_section(meta_import)}\n"
                )
                if self.target_macros[meta_import]["commands"].strip("\n"):
                    full_command += self.target_macros[meta_import]["commands"] + "\n"
                else:
                    full_command += "        # Blank Upgrade Macro\n"

                if self.target_macros[meta_import]["author"] != "Unknown":
                    self.author = self.target_macros[meta_import]["author"]
                if self.target_macros[meta_import]["ticket_number"] != "TTTT":
                    self.ticket_number = self.target_macros[meta_import][
                        "ticket_number"
                    ]
        return full_command

    def write_new_macro(self, meta_dir, full_command, macro):
        """
        Write out the new macro with all relevant commands to the versions.py
        file
        Inputs:
            - meta_dir, path to the metadata directory with a versions.py file
            - full_command, str of the combined macro commands
            - macro, the parsed macro being written
        """

        filepath = os.path.join(meta_dir, "versions.py")
        temppath = os.path.join(meta_dir, ".versions.py")
        shutil.copy(filepath, temppath)

        author = macro["author"]
        if not author:
            author = self.author
        ticket_number = macro["ticket_number"]
        if not ticket_number:
            ticket_number = self.ticket_number

        with open(temppath, "a") as f:
            f.write(
                f'class {macro["class_name"]}(MacroUpgrade):\n'
                f'    """Upgrade macro for ticket {ticket_number} '
                f'by {author}."""\n\n'
                f'    BEFORE_TAG = "{macro["before_tag"]}"\n'
                f'    AFTER_TAG = "{macro["after_tag"]}"\n\n'
                "    def upgrade(self, config, meta_config=None):\n"
                f"{full_command}"  # this variable contains required whitespace
                "        return config, self.reports\n"
            )

        apply_styling(temppath)

        os.rename(temppath, filepath)

    def check_missing_macros(self, meta_dir, meta_imports):
        """
        Check through macros of imported metadata sections, returning list of any that
        aren't in the current section (identified by the after tag)
        Inputs:
            - meta_dir, the current metadata section
            - meta_imports, a list of imported metadata sections
        Returns:
            - list of macros that need adding to the current section
        """

        after_tags = [m["after_tag"] for m in self.parsed_macros[meta_dir]]

        missing_macros = []
        for section in meta_imports:
            section = self.get_full_import_path(section)
            section_missing = []
            len_section_macros = len(self.parsed_macros[section])
            for macro in self.parsed_macros[section]:
                after_tag = macro["after_tag"]
                # Ignore the macro being upgraded - this is expected to be missing
                if after_tag == self.tag:
                    len_section_macros -= 1
                    continue
                if after_tag not in after_tags:
                    section_missing.append(after_tag)
            # Check that if macros are missing ALL of them are missing (this is the
            # case that a new metadata section has been added)
            # Otherwise raise an error as the macro chain is broken
            if section_missing and len_section_macros != len(section_missing):
                raise RuntimeError(
                    f"The versions.py file for section {meta_dir} is missing macros "
                    "from inherited metadata sections. This suggests something has "
                    "gone wrong in the macro chain and should be investigated."
                )
            for after_tag in section_missing:
                if after_tag not in missing_macros:
                    missing_macros.append(after_tag)

        return missing_macros

    def combine_missing_macros(self, meta_imports, missing_macros):
        """
        Combine missing macro commands
        Inputs:
            - meta_imports, a list of imported metadata sections
            - missing_macros, a list of after tags missing
        Returns:
            - dictionary of parsed macros with corrected before tag. Key is after tag
        """

        new_macros = {}

        # Loop over all metadata imports
        for meta_import in meta_imports:
            # For each missing after tag check whether it exists in this imported sect
            for after_tag in missing_macros:
                macro = None
                for m in self.parsed_macros[meta_import]:
                    if m["after_tag"] == after_tag:
                        macro = m
                        break
                # if the macro exists then save it
                if macro:
                    # if the macro not already saved, that's all that's required
                    if after_tag not in new_macros:
                        new_macros[after_tag] = macro
                        continue
                    # if the macro is already saved, then combine macros
                    existing = new_macros[after_tag]
                    existing["commands"] += macro["commands"]
                    # if the before tags are the same, we don't need to modify the chain
                    if existing["before_tag"] == macro["before_tag"]:
                        continue
                    # if the existing before tag is in the current metadata macro chain
                    # then we want to use the new before tag
                    for item in self.parsed_macros[meta_import]:
                        if item["before_tag"] == existing["before_tag"]:
                            existing["before_tag"] = macro["before_tag"]

        return new_macros

    def fix_missing_macros(self, meta_dir, meta_imports):
        """
        Function to handle checking and fixing of missing upgrade macros
        Inputs:
            - meta_dir, the current metadata section
            - meta_imports, a list of imported metadata sections
        Returns:
            - the final after tag in the newly written macro chain if macros are
              missing, otherwise None
        """

        missing_macros = self.check_missing_macros(meta_dir, meta_imports)

        if missing_macros:
            print(
                "[INFO] Writing missing macros to",
                self.parse_application_section(meta_dir),
            )
            macros = self.combine_missing_macros(meta_imports, missing_macros)
            # Record the identified missing macros for this metadata section
            self.parsed_macros[meta_dir] = [m for m in macros.values()]
            macro_strings = []
            for macro in macros.values():
                self.write_new_macro(meta_dir, macro["commands"], macro)
                macro_strings.append(
                    f"BEFORE_TAG = '{macro['before_tag']}'\n"
                    f"AFTER_TAG = '{macro['after_tag']}'"
                )
                self.parsed_macros[meta_dir].insert(0, macro)
            return self.find_last_macro(macro_strings, meta_dir)

        return None

    def order_meta_dirs(self):
        """
        Order the self.meta_dirs list by metadata import order, such that sections
        higher up the import tree come first
        Create a networkx ordered graph, with nodes as the import tree and edges as the
        import statements. Then recreate list from this
        """

        import_graph = nx.DiGraph()

        for meta_dir in self.meta_dirs:
            import_graph.add_node(meta_dir)
            for imp in self.target_macros[meta_dir]["imports"]:
                import_graph.add_edge(imp, meta_dir)

        # Return an ordered list of nodes. This requires non-circular edges, but this is
        # guaranteed for valid rose metadata
        return list(nx.topological_sort(import_graph))

    def preprocess_macros(self):
        """
        Overarching function to pre-process added macros
        Run before running any rose macro upgrade commands"
        Search through versions.py files for macros with the correct after-tag
        Save info and then delete the macro when found
        For each application combine required macros (those for the application
        plus any from imported metadata).
        Write this new macro to that applications versions.py file
        """

        # Get list of versions files to check - in both core and apps
        # Duplicated for backwards compatibility with central rose-meta imports
        if self.central_rose_meta:
            self.find_meta_dirs(os.path.join(self.root_path, "rose-meta"))
            self.find_meta_dirs(os.path.join(self.core_source, "rose-meta"))
        else:
            self.find_meta_dirs(self.root_path)
            self.find_meta_dirs(self.core_source)

        for meta_dir in self.meta_dirs:
            print(
                "[INFO] Pre-processing macros in",
                self.parse_application_section(meta_dir),
            )
            # Read an upgrade macro from a versions.py file, save the macro
            # info and delete the macro from the file
            parsed_versions = read_versions_file(meta_dir)
            macros = split_macros(parsed_versions)

            # Record all macros in this metadata section
            for macro in macros:
                self.parsed_macros[meta_dir].append(self.parse_macro(macro, meta_dir))

            # Check if target macro exists in this section
            found_macro = self.find_macro(meta_dir, macros)
            if not found_macro:
                # If we reach here then the new macro hasn't been added to
                # this versions file - in this case work out the final after
                # tag in the chain - if we import other commands for this
                # versions file, this final after tag will be the before tag of
                # that new macro.
                last_after_tag = self.find_last_macro(macros, meta_dir)
                self.target_macros[meta_dir] = {
                    "before_tag": last_after_tag,
                    "after_tag": self.tag,
                    "commands": "",
                    "imports": "",
                    "class_name": self.class_name,
                    "author": None,
                    "ticket_number": None,
                }
            else:
                self.target_macros[meta_dir] = self.parse_macro(found_macro, meta_dir)
                # Remove the macro from the file
                self.remove_macro(parsed_versions, meta_dir)

            # Read through rose-meta files for import statements of other metadata
            self.target_macros[meta_dir]["imports"] = self.read_meta_imports(meta_dir)

            # Read through the versions.py file for python import statements
            self.python_imports.update(
                read_python_imports(os.path.join(meta_dir, "versions.py"))
            )

        # Now reconstruct the macro for all applications which have the newly
        # added macro or import metadata with the new macro
        # The macro sections need to be processed in the order of import
        for meta_dir in self.order_meta_dirs():
            import_order = self.determine_import_order(meta_dir)
            full_command = self.combine_macros(import_order)

            # If there are commands to write out, do so and record this
            # application as having the macro
            if full_command:
                # Check if there are any macros in imported metadata versions.py files
                # that aren't in the current section.
                # If there are, then combine these and write them out first
                last_after_tag = None
                last_after_tag = self.fix_missing_macros(
                    meta_dir, self.target_macros[meta_dir]["imports"]
                )

                if last_after_tag:
                    self.target_macros[meta_dir]["before_tag"] = last_after_tag

                print(
                    "[INFO] Writing macros to",
                    self.parse_application_section(meta_dir),
                )
                self.write_python_imports(meta_dir)
                self.write_new_macro(
                    meta_dir, full_command, self.target_macros[meta_dir]
                )
                self.sections_with_macro.append(meta_dir)

    ############################################################################
    # Upgrade Apps Functions
    ############################################################################

    def metadata_check(self, meta_dir):
        """ "
        Note: Not currently run - see comment below
        Run rose metadata-check on rose metadata directories to check the
        validity of the metadata.
        Inputs:
            - meta_dir, path to a rose metadata directory
        """

        print(f"[INFO] Checking metadata in {meta_dir}")
        command = f"rose metadata-check -C {os.path.join(meta_dir, 'HEAD')}"
        result = run_command(command)
        if result.returncode:
            print(f"[FAIL] The metadata at {meta_dir} failed to validate.")
            raise RuntimeError(
                f"\nThe command run:\n{command}"
                f"\nThe error message produced:\n{result.stderr}"
            )
        print(f"[PASS] {meta_dir} validated")

    def apps_to_upgrade(self):
        """
        Loop over rose-stem apps, finding ones using metadata with an upgrade
        macro.
        Returns:
            - list of paths to apps requiring upgrading
        """
        upgradeable_apps = []
        app_dir_apps = os.path.join(self.root_path, "rose-stem", "app")
        app_dir_core = os.path.join(self.core_source, "rose-stem", "app")
        apps_list = [os.path.join(app_dir_apps, f) for f in os.listdir(app_dir_apps)]
        apps_list += [os.path.join(app_dir_core, f) for f in os.listdir(app_dir_core)]
        for app_path in apps_list:
            # Ignore lfric_coupled_rivers as this is based on Jules-standalone
            # metadata which is not currently available
            if "fcm_make" in app_path or "lfric_coupled_rivers" in app_path:
                continue
            if not os.path.isdir(app_path):
                continue
            meta_import = self.read_meta_imports(
                os.path.join(app_path, "rose-app.conf"), "meta"
            )
            # If there was a metadata import, it is the first value in the list
            # It includes the version directory, so remove this to compare with
            # self.sections_with_macro
            if meta_import:
                if os.path.dirname(meta_import[0]) in self.sections_with_macro:
                    upgradeable_apps.append(app_path)

        return upgradeable_apps

    def run_app_upgrade(self, app_path):
        """
        Run 'rose app-upgrade' on a particular rose-stem app
        Inputs:
            - app_path, the path to this app
        """
        app = os.path.basename(app_path)
        print(f"[INFO] Upgrading the rose-stem app {app}")
        command = f"rose app-upgrade -a -y -C {app_path} {self.tag}"
        result = run_command(command)
        if result.returncode:
            print(f"[FAIL] The rose-stem app {app} failed to upgrade")
            raise RuntimeError(
                f"\nThe command run:\n{command}"
                f"\nThe error message produced:\n{result.stderr}"
            )
        print(f"[PASS] Upgraded rose-stem app {app} successfully")

    def run_macro_fix(self, app_path):
        """
        Run 'rose macro --fix' on a particular rose-stem app to force metadata
        consistency
        Inputs:
            - app_path, the path to this app
        """
        app = os.path.basename(app_path)
        print(f"[INFO] Forcing metadata consistency in app {app}")
        command = f"rose macro --fix -y -C {app_path}"
        result = run_command(command)
        if result.returncode:
            print(f"[FAIL] Unable to force metadata consistency in {app}")
            raise RuntimeError(
                f"\nThe command run:\n{command}"
                f"\nThe error message produced:\n{result.stderr}"
            )
        print(f"[PASS] Successfully forced metadata consistency in {app}")

    def upgrade_apps(self):
        """
        Overarching function to run rose commands to apply upgrade macros to
        rose-stem apps.
        First run over all metadata directories and run rose metadata-check on
        each
        Then run over all rose apps. If there is an import statement for an
        application with the upgrade macro (stored in self.sections_with_macro)
        then run rose commands on it.
        - rose app-upgrade
        - rose macro --fix
        """

        # This step would be good to do (as we do in the UM). However, not all
        # metadata files validate currently as they use gungho namelists but are
        # imported by gungho (eg. um_physics_interface). The remainder are
        # tested regularly by the lfric_apps validate_rose_meta script, so happy
        # to not do this here. Leaving the code, so that we do validate these
        # once the metadata structure has been improved.
        # banner_print("[INFO] Validating Metadata Config Files")
        # for meta_dir in self.meta_dirs:
        #     self.metadata_check(meta_dir)

        banner_print("[INFO] Upgrading Apps")
        upgradeable_apps = self.apps_to_upgrade()
        for app_path in upgradeable_apps:
            self.run_app_upgrade(app_path)
            self.run_macro_fix(app_path)
            if app_path.startswith(self.core_source):
                self.upgraded_core = True


def check_tag(opt):
    """
    Check that a command line supplied tag is of a valid format
    """
    class_name = opt.replace(".", "")
    if not re.match(CLASS_NAME_REGEX, class_name):
        raise argparse.ArgumentTypeError(
            f"The tag '{opt}' does not conform to the "
            "'vnXX.Y_tTTTT' naming scheme. Please modify and rerun."
        )
    return opt


def version_number(opt):
    """
    Check that the command line supplied version number is of a suitable format
    """
    if opt is None:
        return opt
    if not re.match(r"\d+.\d+", opt):
        raise argparse.ArgumentTypeError(
            f"The version number '{opt}' does not conform to the 'X.Y' format."
            "Please modify the command line argument and rerun."
        )
    return opt


def parse_args():
    """
    Read command line args
    """

    parser = argparse.ArgumentParser("Pre-process and apply LFRic Apps upgrade macros.")
    parser.add_argument(
        "tag",
        type=check_tag,
        metavar="after-tag",
        help="The After Tag of the upgrade macro being upgraded to.",
    )
    parser.add_argument(
        "-n",
        "--cname",
        default=None,
        help="The class name of the upgrade macro. This should only be used at "
        "a new release when the tag and classname differ.",
    )
    parser.add_argument(
        "-v",
        "--version",
        default=None,
        type=version_number,
        help="The new version number we are updating to (format X.Y)",
    )
    parser.add_argument(
        "-a",
        "--apps",
        default=".",
        help="The path to the LFRic Apps working copy being used. Defaults to  "
        "the location the script is being run from - this assumes you are in a "
        "working copy.",
    )
    parser.add_argument(
        "-c",
        "--core",
        default=None,
        help="The LFRic Core source being used."
        "Either a path to a working copy or an FCM URL."
        "If not set, will be read from the dependencies.sh",
    )
    parser.add_argument(
        "-j",
        "--jules",
        default=None,
        help="The Jules source being used."
        "Either a path to a working copy or an FCM URL."
        "If not set, will be read from the dependencies.sh",
    )
    return parser.parse_args()


def apply_macros_main(tag, cname=None, version=None, apps=".", core=None, jules=None):
    """
    Main function for this program
    """

    check_environment()

    macro_object = ApplyMacros(tag, cname, version, apps, core, jules)

    # Pre-process macros
    banner_print("Pre-Processing Macros")
    macro_object.preprocess_macros()

    # Upgrade Rose Stem Apps
    macro_object.upgrade_apps()

    # Clean up temporary directories
    for repo, directory in macro_object.temp_dirs.items():
        if macro_object.upgraded_core and repo == "lfric_core":
            banner_print("WARNING")
            print(
                "Macros have been applied to apps in LFRic Core. A temporary "
                "copy of the LFRic Core source given by the `dependencies.sh` "
                f"file is located at:\n{macro_object.core_source}\nEnsure you "
                "have committed those changes back to the core branch."
            )
            continue
        shutil.rmtree(directory)

    # Run rose config-dump on rose-stem
    config_dump_apps = (
        f"rose config-dump {os.path.join(macro_object.root_path), 'rose-stem'}"
    )
    run_command(config_dump_apps)


if __name__ == "__main__":
    args = parse_args()
    apply_macros_main(
        args.tag, args.cname, args.version, args.apps, args.core, args.jules
    )
