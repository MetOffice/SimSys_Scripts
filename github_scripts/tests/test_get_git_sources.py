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
from shlex import split
from pathlib import Path
import pytest

from ..get_git_sources import (
    validate_dependencies,
    determine_mirror_fetch,
    set_https,
    clone_repo,
    clone_repo_mirror,
    sync_repo,
    check_existing,
    merge_source,
)

# Check if running in an action and setup git if so
if os.getenv("RUNNING_GH_ACTION", "False") == "True":
    subprocess.run(split("git config --global user.email 'Testing'"))
    subprocess.run(split("git config --global user.name 'Testing'"))


@pytest.fixture(scope="session")
def setup_sources(tmpdir_factory):
    """
    Setup a tempdir for cloning into, a mirror of SimSys_Scripts and a local clone of
    SimSys_Scripts
    Use SimSys_Scripts as a public repo
    """

    location = tmpdir_factory.mktemp("data")
    os.chdir(location)

    # Setup local mirror
    subprocess.run(
        split("git clone --mirror https://github.com/MetOffice/SimSys_Scripts.git"),
        check=True,
    )

    # Create local clone
    subprocess.run(
        split("git clone https://github.com/MetOffice/SimSys_Scripts.git"), check=True
    )
    subprocess.run(split("git -C SimSys_Scripts checkout 2025.12.1"))

    # Create a non-git repo to test check_existing
    existing = Path(location) / "empty_dir"
    existing.mkdir()

    # Create 2 clones with conflicting commits
    for i in range(2):
        subprocess.run(split(f"cp -r SimSys_Scripts merge{i}"), check=True)
        subprocess.run(split(f"git -C merge{i} switch -c merge{i}"), check=True)
        with open(f"merge{i}/merge.txt", "w") as f:
            f.write(f"merge{i}")
        subprocess.run(split(f"git -C merge{i} add merge.txt"), check=True)
        subprocess.run(
            split(f"git  -C merge{i} commit -a -m 'merge conflict'"), check=True,
        )

    return Path(location)


def test_clone_repo(setup_sources):
    """
    Test cloning from a github source
    """

    output_loc = setup_sources / "github_clone"
    assert (
        clone_repo(
            "https://github.com/MetOffice/SimSys_Scripts.git", "2025.12.1", output_loc
        )
        is None
    )
    assert Path(output_loc / ".git").is_dir() is True


def test_clone_repo_mirror(setup_sources):
    """
    Test rsyncing a local clone
    """

    output_loc = setup_sources / "mirror_clone"
    mirror_loc = setup_sources / "SimSys_Scripts.git"
    assert (
        clone_repo_mirror(
            "https://github.com/MetOffice/SimSys_Scripts.git",
            "2025.12.1",
            mirror_loc,
            output_loc,
        )
        is None
    )
    assert Path(output_loc / ".git").is_dir() is True


def test_sync_repo(setup_sources):
    """
    Test cloning from a github source
    """

    # Cant test syncing with a hostname in github actions
    source_loc = setup_sources / "SimSys_Scripts"
    output_loc = setup_sources / "sync_clone"
    assert sync_repo(source_loc, "2025.12.1", output_loc) is None
    assert Path(output_loc / ".git").is_dir() is True


def test_merge_sources(setup_sources):
    """
    Test merge_source
    """

    target_clone = setup_sources / "SimSys_Scripts"

    assert (
        merge_source(
            "https://github.com/MetOffice/SimSys_Scripts.git",
            "main",
            target_clone,
            "SimSys_Scripts",
        )
        is None
    )
    assert (
        merge_source(setup_sources / "merge0", "merge0", target_clone, "SimSys_Scripts")
        is None
    )
    with pytest.raises(RuntimeError):
        merge_source(setup_sources / "merge1", "merge1", target_clone, "SimSys_Scripts")


def test_check_exists(setup_sources):
    """
    Test check_existing
    """

    assert check_existing(setup_sources / "SimSys_Scripts") is None

    with pytest.raises(FileExistsError):
        check_existing(setup_sources / "empty_dir")


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
