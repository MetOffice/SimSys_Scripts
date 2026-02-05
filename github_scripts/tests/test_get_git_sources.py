# -----------------------------------------------------------------------------
# (C) Crown copyright Met Office. All rights reserved.
# The file LICENCE, distributed with this code, contains details of the terms
# under which the code may be used.
# -----------------------------------------------------------------------------
"""
Unit tests for get_git_sources
"""

import os
import subprocess
import pytest

from ..get_git_sources import validate_dependencies, determine_mirror_fetch, set_https


def test_validate_dependencies():
    valid = {
        "repo1": {"source": "abc", "ref": "123"},
        "repo2": [{"source": "abc", "ref": "123"}, {"source": "abc", "ref": "123"}],
    }
    assert validate_dependencies(valid) is None

    invalid_dependencies = set()
    invalid_dependencies.add(1)
    with pytest.raises(TypeError):
        validate_dependencies(invalid_dependencies)

    invalid_repo_type = {"repo1": invalid_dependencies}
    with pytest.raises(TypeError):
        validate_dependencies(invalid_repo_type)

    invalid_list = {"repo1": [invalid_dependencies]}
    with pytest.raises(TypeError):
        validate_dependencies(invalid_list)

    missing_source = {"repo1": {"ref": "123"}}
    with pytest.raises(ValueError):
        validate_dependencies(missing_source)

    missing_ref = {"repo1": {"source": "abc"}}
    with pytest.raises(ValueError):
        validate_dependencies(missing_ref)


def test_determine_mirror_fetch():
    """
    Test determine_mirror_fetch
    """

    # Test MetOffice User
    assert (
        determine_mirror_fetch("git@github.com:MetOffice/SimSys_Scripts.git", "ref")
        == "ref"
    )
    assert (
        determine_mirror_fetch("https://github.com/MetOffice/SimSys_Scripts.git", "ref")
        == "ref"
    )

    # Test using hash
    commit_hash = "ba965768395de47de064a60ee769471e3868e02d"
    assert (
        determine_mirror_fetch(
            "git@github.com:user_name/SimSys_Scripts.git", commit_hash
        )
        == commit_hash
    )
    assert (
        determine_mirror_fetch(
            "https://github.com/user_name/SimSys_Scripts.git", commit_hash
        )
        == commit_hash
    )

    # Test using user and branch
    user_name = "user_name"
    branch = "branch"
    assert (
        determine_mirror_fetch(f"git@github.com:{user_name}/SimSys_Scripts.git", branch)
        == f"{user_name}/{branch}"
    )
    assert (
        determine_mirror_fetch(
            f"https://github.com/{user_name}/SimSys_Scripts.git", branch
        )
        == f"{user_name}/{branch}"
    )


def test_set_https():
    """
    Test set_https
    """

    input_dict = {
        "repo1": {
            "source": "git@github.com:MetOffice/SimSys_Scripts.git",
            "ref": "123",
        },
        "repo2": [
            {"source": "git@github.com:MetOffice/lfric_apps.git", "ref": "123"},
            {"source": "git@github.com:MetOffice/lfric_apps.git", "ref": "456"},
        ],
        "repo3": {
            "source": "https://github.com/MetOffice/lfric_core.git",
            "ref": "123",
        },
        "repo4": {"source": "hostname:/path/to/repository", "ref": "123"},
    }
    output_dict = {
        "repo1": [
            {"source": "https://github.com/MetOffice/SimSys_Scripts.git", "ref": "123"}
        ],
        "repo2": [
            {"source": "https://github.com/MetOffice/lfric_apps.git", "ref": "123"},
            {"source": "https://github.com/MetOffice/lfric_apps.git", "ref": "456"},
        ],
        "repo3": [
            {"source": "https://github.com/MetOffice/lfric_core.git", "ref": "123"}
        ],
        "repo4": [{"source": "hostname:/path/to/repository", "ref": "123"}],
    }

    assert set_https(input_dict) == output_dict
