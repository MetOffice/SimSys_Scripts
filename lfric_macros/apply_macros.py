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

import os
import re
import shutil
import argparse
import tempfile
import subprocess

BLACK_COMMAND = "black --line-length=80"
CLASS_NAME_REGEX = r"vn\d+_t\d+\w."
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
        timeout=60,
        shell=shell,
        check=False,
    )


def get_root_path(apps_path):
    """
    Given a path to a working copy, ensure the path and working copy are both
    valid and return the path to the working copy root directory
    Inputs:
        - apps_path, command line argument to the apps working copy
    Outputs:
        - str, path to the top level of the apps working copy
    """

    # Run fcm info on the given path to ensure it is
    command = f"fcm info {apps_path}"
    result = run_command(command)
    if result.returncode:
        raise FileNotFoundError(
            f"The provided LFRic Apps source, '{apps_path}', was not a valid "
            "working copy. Please either run this script from within an LFRic "
            "Apps working copy, or provide a path using the -a argument."
        )

    # If no error, then search through output for the working copy root path
    # return the found path
    for line in result.stdout.split("\n"):
        if line.startswith("Working Copy Root Path"):
            return line.split(":", 1)[1].strip()


def run_black(filepath):
    """
    Run black on a given file
    Inputs:
        - filepath, the path to the file to run black on
    """
    result = run_command(f"{BLACK_COMMAND} {filepath}")
    if result.returncode:
        raise RuntimeError(
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
        if len(line.strip().strip("\n")) > 0:
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
    in_comment = False
    for line in parsed_versions:
        if '"""' in line:
            for _ in range(line.count('"""')):
                in_comment = not in_comment
        if in_comment:
            continue
        if line.startswith("class vn"):
            if macro:
                macros.append(macro)
            in_macro = True
            macro = ""
        if in_macro:
            macro += line
    if macro:
        macros.append(macro)

    return macros


def match_python_import(line):
    """
    Return true if string has form 'import *' or 'from * import *'
    Inputs:
        - line, str to match
    """
    if re.match(r"import \w+", line) or re.match(
        r"from [\.\w]+ import [\.\w]+", line
    ):
        return True
    return False


def banner_print(message):
    """Print a simple banner message"""
    print(f"\n{(len(message)+4)*'*'}\n* {message} *\n{(len(message)+4)*'*'}\n")


class ApplyMacros:
    """
    Object to hold data + methods to apply upgrade macros in lfric_apps
    """

    def __init__(self, tag, apps, core, jules):
        self.tag = tag
        # The class name is the After Tag with the . removed from the version
        self.class_name = tag.replace(".", "")
        self.temp_dirs = {}
        self.root_path = get_root_path(apps)
        self.core_source = self.get_dependency_paths(core, "lfric_core")
        self.jules_source = self.get_dependency_paths(jules, "jules")
        self.set_rose_meta_path()
        self.version = re.search(r".*vn(\d+\.\d+)_.*", tag).group(1)
        self.ticket_number = None
        self.author = None
        self.parsed_macros = {}
        self.meta_dirs = []
        self.apps_with_macro = []
        self.python_imports = set()
        self.upgraded_core = False

    def set_rose_meta_path(self):
        """
        Set up the ROSE_META_PATH environment variable in order to use the Jules
        and Core metadata. We also add the working copy root path as this should
        allow the script to be run from anywhere
        """
        rose_meta_path = (
            f"{self.root_path}:{self.core_source}:{self.jules_source}"
        )
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
        meta_dir = meta_dir.removeprefix(self.root_path + "/")
        meta_dir = meta_dir.removeprefix(self.core_source + "/")
        meta_dir = meta_dir.removeprefix(self.jules_source + "/")

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
        if os.path.exists(source):
            return source
        if ":" in source:
            source_path = source.split(":")[1]
            if os.path.exists(source_path):
                return source_path

        # Check that the source looks like an fcm keyword, raise an error if not
        if "fcm:" not in source:
            raise RuntimeError(
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
        by repo. Assumes this file is in rose-stem/bin and gets the
        dependencies.sh path from that.
        Inputs:
            - repo, str, Either "lfric_core" or "jules" depending on which
                         source is being found
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
            raise RuntimeError(
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
        the rose-meta firectories using the "find" command. Search by looking
        for versions.py files
        Outputs:
            - str, stdout of find command looking for versions.py files
        """

        command = f"find {path} -name versions.py"
        result = run_command(command)
        if result.returncode:
            raise RuntimeError(
                "Error while finding versions.py files in the directory "
                f"{path}. Running command '{command}'\nError:\n\n"
                f"{result.stderr}\n"
            )
        meta_dirs = result.stdout.split("\n")
        for item in meta_dirs:
            item = item.removesuffix("/versions.py")
            if item:
                self.meta_dirs.append(item)

    def parse_macro(self, macro, meta_dir):
        """
        Given a macro string save the macro info in a dictionary
        Inputs:
            - macro, an upgrade macro that matches the class name we are
              looking for
            - meta_dir, the path to the rose metadata directory
        """

        version_file = os.path.join(meta_dir, "versions.py")

        # The ticket number and author will always be the same across all
        # macros for this ticket, so only grab these once
        # These are not vital so don't fail if not found
        if self.ticket_number is None or self.author is None:
            ticket_details = re.search(
                r"Upgrade .* (#\d+) by (\S+.*)", macro
            )
            try:
                self.ticket_number = ticket_details.group(1)
                self.author = ticket_details.group(2).rstrip('".')
                self.author = self.author.strip("<>")
            except AttributeError:
                pass

        # Search for the before tag
        # Raise an exception if these are missing
        try:
            before_tag = re.search(rf"BEFORE_TAG{TAG_REGEX}", macro).group(1)
        except AttributeError as exc:
            raise RuntimeError(
                "Couldn't find either a Before tag for the requested "
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
                line_stripped.startswith("return")
                or line_stripped.startswith("# Input your macro commands here")
                or line_stripped.lower().startswith("# add settings")
            ):
                continue
            if not in_function or line_stripped.startswith("return config"):
                continue
            commands += line + "\n"

        # Record macro details
        self.parsed_macros[meta_dir] = {
            "before_tag": before_tag,
            "commands": commands,
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

        with open(filepath, "w") as f:
            in_new_macro = False
            for line in contents:
                if re.match(r"class vn\d+_t\d+", line):
                    if in_new_macro:
                        in_new_macro = False
                    elif self.class_name in line:
                        in_new_macro = True
                if not in_new_macro:
                    f.write(line)

        run_black(filepath)

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
                        after_tag = re.search(
                            rf"AFTER_TAG{TAG_REGEX}", macro
                        ).group(1)
                    except AttributeError:
                        raise RuntimeError(
                            "Couldn't find an after tag in the macro:\n"
                            f"{macro}"
                        )
                    found_macro = macro
                    macros.remove(found_macro)
                    break
            else:
                raise RuntimeError(
                    f"Couldn't find the before tag '{after_tag}' in macros in "
                    f"the versions.py file at {meta_dir}"
                )
        return after_tag

    def find_macro(self, meta_dir, macros):
        """
        Read through a list of macros, trying to find desired one. If this is
        present then return true, otherwise false
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
                macro_name = re.search(
                    rf"class ({CLASS_NAME_REGEX})\(", macro
                ).group(1)
            except AttributeError as exc:
                raise RuntimeError(
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
            - the import statement containing the full path - default to using
              the apps working copy
        """

        core_imp = os.path.join(self.core_source, imp)
        if os.path.exists(core_imp):
            return core_imp

        jules_imp = os.path.join(self.jules_source, imp)
        if os.path.exists(jules_imp):
            return jules_imp

        apps_imp = os.path.join(self.root_path, imp)
        if os.path.exists(apps_imp):
            return apps_imp

        raise RuntimeError(
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

        # First grep for lines starting with import=
        # No imports if it doesn't return anything
        result = run_command(f"grep -E '^ *{flag}=' {meta_file}", shell=True)
        if not result.stdout:
            if flag == "meta":
                return ""
            self.parsed_macros[meta_dir]["imports"] = []

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
                        imp = line.split("=", 1)[1].strip("/HEAD")
                        imp = self.get_full_import_path(imp)
                        imports.append(imp)
                        if flag == "meta":
                            return imports[0]
                    else:
                        break
        self.parsed_macros[meta_dir]["imports"] = imports

    def read_python_imports(self, versions):
        """
        Read through a python versions.py file for any python module imports.
        Record this in self.python_imports
        Inputs:
            - versions, list of lines in a versions.py file
        """

        started_imports = False
        for line in versions:
            line = line.strip()
            if match_python_import(line):
                started_imports = True
                self.python_imports.add(line)
            elif started_imports:
                return

    def write_python_imports(self, meta_dir):
        """
        Write out all required python module imports at the top of a versions.py
        file. Read imports from self.python_imports
        Inputs:
            - meta_dir, path to the metadata directory with a versions.py file
        """

        fpath = os.path.join(meta_dir, "versions.py")

        with open(fpath) as f:
            versions_file = f.readlines()

        started_imports = False
        found_imports = set()
        for i, line in enumerate(versions_file):
            line = line.strip()
            if match_python_import(line):
                started_imports = True
                found_imports.add(line)
            elif started_imports:
                insertion_index = i
                break

        missing_imports = self.python_imports.difference(found_imports)
        for imp in missing_imports:
            versions_file.insert(insertion_index, imp)

        with open(fpath, "w") as f:
            for line in versions_file:
                f.write(line.strip("\n") + "\n")

    def deduplicate_list(self, lst):
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

    def determine_import_order(self, app):
        """
        Work out what order metadata is imported. This recursively works through
        import statements recorded in self.parsed_macros["imports"]. Produces a
        list of the order in which macro commands should be applied (this should
        be the same order as the imports)
        Inputs:
            - app, the application or science section which we are trying to
              find the import order for
        Returns:
            - A list of meta imports in the correct order
        """

        import_list = [app]
        try:
            imports = self.parsed_macros[app]["imports"]
        except KeyError:
            # Jules Shared directories will produce a key error - these are
            # guaranteed to not import anything
            imports = []
        for meta_import in imports:
            import_list = self.determine_import_order(meta_import) + import_list

        return self.deduplicate_list(import_list)

    def combine_macros(self, import_order):
        """
        For a given application, combine macro commands from imported metadata
        and itself to get full macro for a particular change.
        Inputs:
            - version_file, the path to an applications versions.py file
            - import_order, the metadata import order to match the order of
              marcro commands.
        Returns:
            - string, combined macro commands
        """

        full_command = ""
        for meta_import in import_order:
            if (
                meta_import in self.parsed_macros
                and self.parsed_macros[meta_import]["commands"]
            ):
                # Add a comment labelling where these commands came from
                full_command += (
                    f"        # {self.parse_application_section(meta_import)}\n"
                )
                if self.parsed_macros[meta_import]["commands"].strip("\n"):
                    full_command += (
                        self.parsed_macros[meta_import]["commands"] + "\n"
                    )
                else:
                    full_command += "        # Blank Upgrade Macro\n"
        return full_command

    def write_new_macro(self, meta_dir, full_command):
        """
        Write out the new macro with all relevant commands to the versions.py
        file
        Inputs:
            - meta_dir, path to the metadata directory with a versions.py file
            - full_command, str of the combined macro commands
        """

        parsed_macro = self.parsed_macros[meta_dir]
        version_file = os.path.join(meta_dir, "versions.py")

        with open(version_file, "a") as f:
            f.write(
                f"class {self.class_name}(MacroUpgrade):\n"
                f'    """Upgrade macro for ticket {self.ticket_number} '
                f'by {self.author}."""\n\n'
                f'    BEFORE_TAG = "{parsed_macro["before_tag"]}"\n'
                f'    AFTER_TAG = "{self.tag}"\n\n'
                "    def upgrade(self, config, meta_config=None):\n"
                f"{full_command}"  # this variable contains required whitespace
                "        return config, self.reports\n"
            )

        run_black(version_file)

    def preprocess_macros(self):
        """
        Overraching function to pre-process added macros
        Run before running any rose macro upgrade commands"
        Search through versions.py files for macros with the correct after-tag
        Save info and then delete the macro when found
        For each application combine required macros (those for the application
        plus any from imported metadata).
        Write this new macro to that applications versions.py file
        """

        # Get list of versions files to check - in both core and apps
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
            found_macro = self.find_macro(meta_dir, macros)
            if not found_macro:
                # If we reach here then the new macro hasn't been added to
                # this versions file - in this case work out the final after
                #  tag in the chain and record in parsed macros
                last_after_tag = self.find_last_macro(macros, meta_dir)
                self.parsed_macros[meta_dir] = {
                    "before_tag": last_after_tag,
                    "commands": "",
                    "imports": "",
                }
            else:
                self.parse_macro(found_macro, meta_dir)
                # Remove the macro from the file
                self.remove_macro(parsed_versions, meta_dir)

            # Read through rose-meta files for import statements
            # of other metadata
            self.read_meta_imports(meta_dir)

            # Read through the versions.py file for python import statements
            self.read_python_imports(parsed_versions)

        # Now reconstruct the macro for all applications which have the newly
        # added macro or import metadata with the new macro
        # Note, this is only done for applications, not for science sections
        for meta_dir in self.meta_dirs:
            if not (
                meta_dir.startswith(
                    os.path.join(self.root_path, "applications")
                )
                or meta_dir.startswith(
                    os.path.join(self.core_source, "applications")
                )
            ):
                continue
            import_order = self.determine_import_order(meta_dir)
            full_command = self.combine_macros(import_order)
            # If there are commands to write out, do so and record this
            # application as having the macro
            if full_command:
                print(
                    "[INFO] Writing macros to",
                    self.parse_application_section(meta_dir),
                )
                self.write_python_imports(meta_dir)
                self.write_new_macro(meta_dir, full_command)
                self.apps_with_macro.append(meta_dir)

    ############################################################################
    # Upgrade Apps Functions
    ############################################################################

    def metadata_check(self, meta_dir):
        """ "
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
        apps_lstdir = os.listdir(app_dir_apps)
        app_dir_core = os.path.join(self.core_source, "rose-stem", "app")
        core_lstdir = os.listdir(app_dir_core)
        for app in apps_lstdir + core_lstdir:
            if "fcm_make" in app or "lfric_coupled_rivers" in app:
                continue
            if app in apps_lstdir:
                app_path = os.path.join(app_dir_apps, app)
            else:
                app_path = os.path.join(app_dir_core, app)
            if not os.path.isdir(app_path):
                continue
            meta_import = self.read_meta_imports(
                os.path.join(app_path, "rose-app.conf"), "meta"
            )
            meta_import = os.path.dirname(meta_import)
            if meta_import in self.apps_with_macro:
                upgradeable_apps.append((app, app_path))

        return upgradeable_apps

    def run_app_upgrade(self, app, app_path):
        """
        Run 'rose app-upgrade' on a particular rose-stem app
        Inputs:
            - app, the name of the app being upgraded
            - app_path, the path to this app
        """

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

    def run_macro_fix(self, app, app_path):
        """
        Run 'rose macro --fix' on a particular rose-stem app to force metadata
        consistency
        Inputs:
            - app, the name of the app being upgraded
            - app_path, the path to this app
        """
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
        application with the upgrade macro (stored in self.apps_with_macro) then
        run rose commands on it.
        - rose app-upgrade
        - rose macro --fix
        """

        # banner_print("[INFO] Validating Metadata Config Files")
        # for meta_dir in self.meta_dirs:
        #     self.metadata_check(meta_dir)

        banner_print("[INFO] Upgrading Apps")
        upgradeable_apps = self.apps_to_upgrade()
        for app, app_path in upgradeable_apps:
            self.run_app_upgrade(app, app_path)
            self.run_macro_fix(app, app_path)
            if app_path.startswith(self.core_source):
                self.upgraded_core = True


def parse_args():
    """
    Read command line args
    """

    parser = argparse.ArgumentParser(
        "Pre-process and apply lfric_apps upgrade macros."
    )
    parser.add_argument(
        "tag",
        metavar="after-tag",
        help="The After Tag of the upgrade macro being upgraded to.",
    )
    parser.add_argument(
        "-a",
        "--apps",
        default=".",
        help="The path to the LFRic Apps working copy being used. Defaults to  "
        "the location the script is being run from - this assumes you are in a "
        "working copy."
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


def main():
    """
    Main function for this program
    """

    args = parse_args()

    # if the class name doesn't conform to the expected vnXX.Y_tTTTT naming
    # convention raise an error
    class_name = args.tag.replace(".", "")
    if not re.match(CLASS_NAME_REGEX, class_name):
        raise RuntimeError(
            f"The class name '{class_name}' does not conform to the "
            "'vnXX.Y_tTTTT' naming scheme. Please modify and rerun."
        )

    macro_object = ApplyMacros(args.tag, args.apps, args.core, args.jules)

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
    main()
