#!/usr/bin/env python3
# -----------------------------------------------------------------------------
# (C) Crown copyright Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
# -----------------------------------------------------------------------------

"""
Clone sources for a rose-stem run for use with git bdiff module in scripts
Only intended for use with rose-stem suites that have provided appropriate
environment variables
"""

import os
from pathlib import Path
from ast import literal_eval
from get_git_sources import get_source, merge_source, set_https


def main() -> None:
    """
    1. Read environment variables for:
        SOURCE_DIRECTORY - location to clone sources,
        DEPENDENCIES - dictionary of dependencies,
        USE_TOKENS - whether to use tokens for https URLs,
        USE_MIRRORS - whether to use local git mirrors,
        GIT_MIRROR_LOC - location of local git mirrors
    2. For each dependency in DEPENDENCIES, clone or sync the source
    3. If USE_TOKENS is True, modify the source URLs to use https
    4. If USE_MIRRORS is True, clone from local mirrors at GIT_MIRROR_LOC
    """

    clone_loc = Path(os.environ["SOURCE_DIRECTORY"])
    dependencies: dict = literal_eval(os.environ["DEPENDENCIES"])

    if os.environ.get("USE_TOKENS", "False") == "True":
        dependencies = set_https(dependencies)

    use_mirrors = os.environ.get("USE_MIRRORS", "False") == "True"

    for dependency, opts in dependencies.items():
        loc = clone_loc / dependency

        if not isinstance(opts, list):
            opts = [opts]

        for i, values in enumerate(opts):
            mirror_loc = Path(os.getenv("GIT_MIRROR_LOC", "")) / "MetOffice"

            if i == 0:
                get_source(
                    values["source"],
                    values["ref"],
                    loc,
                    dependency,
                    use_mirrors,
                    mirror_loc,
                )
                continue

            merge_source(
                values["source"],
                values["ref"],
                loc,
                dependency,
                use_mirrors,
                mirror_loc,
            )


if __name__ == "__main__":
    main()
