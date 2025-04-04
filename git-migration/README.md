# Met Office Simulation Systems Git Migration Project

This project is aimed to migrate the Met Office simulation systems repositories
to Git ecosystem. See MetOffice/simulation-systems/discussions/337 for details.


> [!IMPORTANT] Pre-requisites to run the migration script
> - Read access to the Met Office Science Repository [MOSRS](https://code.metoffice.gov.uk/trac/home) or the Met Office internal mirror (`svn://fcm1`).
> - Optional Write access to the [Met Office GitHub organisation](https://github.com/MetOffice) for push to remote (usually, an admin).
> - Tools: [`fcm`](https://metomi.github.io/fcm/doc/user_guide/introduction.html), [`git`](https://git-scm.com), [`gitlify`](https://github.com/MetOffice/gitlify), [`jq`](https://jqlang.org), and [`gh`](https://cli.github.com) available on the system.

## Checklist

1. Test `gitlify` translation tool
   - [x] Able to convert only svn trunk to Git.
   - [x] Able to map svn revisions to Git tag.
   - [x] Synchronise trunk updates locally in Git repositories.
     - [x] Set-up a (`scron`) job to update code/tags routinely.

2. Create/update GitHub repositories under [MetOffice](https://github.com/MetOffice)
   - [ ] `svn:um/main` (@trunk) → [um](https://github.com/MetOffice/um) (@trunk)
   - [ ] `svn:um/aux` (@trunk) → [um_aux](https://github.com/MetOffice/um_aux) (@trunk)
   - [ ] `svn:um/meta` (@trunk) → [um_meta](https://github.com/MetOffice/um_meta) (@trunk)
   - [ ] `svn:um/doc` (@trunk) → [um_doc](https://github.com/MetOffice/um_doc) (@trunk)
     - [ ] Compress/convert large graphics in the repository consulting the authors, if possible.
     - [ ] Implement GitHub Action to build/deploy docs: test using [texlive docker image](https://hub.docker.com/r/texlive/texlive/tags?name=2018)
   - [ ] `svn:um/mule` (@trunk) → [mule](https://github.com/MetOffice/mule) (@trunk)
     - [ ] Contact [metomi](https://github.com/metomi/mule) owner to deprecate their repository.
   - [ ] `svn:gcom/main` (@trunk) → [gcom](https://github.com/MetOffice/gcom) (@trunk)
   - [ ] `svn:jules/main` (@trunk) → [jules](https://github.com/MetOffice/jules) (@trunk)
     - [ ] Check licence agreements, if going public.
     - [ ] Plan to migrate JULES documentation here.
   - [ ] `svn:socrates/main` (@trunk) → [socrates](https://github.com/MetOffice/socrates) (@trunk)
   - [ ] `svn:utils/shumlib` (@trunk) → [shumlib](https://github.com/MetOffice/shumlib) (@trunk)
     - [ ] - [ ] Contact [metomi](https://github.com/metomi/shumlib) owner to deprecate their repository.
   - [ ] `svn:ukca/main` (@trunk) → [ukca](https://github.com/MetOffice/ukca) (@trunk)
   - [ ] `svn:monc/casim` (@trunk) → [casim](https://github.com/MetOffice/casim) (@trunk)
   - [ ] `svn:moci/main` (@trunk) → [moci](https://github.com/MetOffice/moci) (@trunk)
   - [ ] `svn:lfric/LFRic` (@trunk) → [lfric_core](https://github.com/MetOffice/lfric_core) (@trunk)
     - [ ] Repository already exists: ask for admin access to the repository.
   - [ ] `svn:lfric_apps/main` (@trunk) → [lfric_apps](https://github.com/MetOffice/lfric_apps) (@trunk)
     - [ ] Propose `lfric_core` as a submodule in this repository?
   - [ ] Check `config.json` is correct and up-to-date with MOSRS revisions.

3. General updated for all repositories
   - [ ] All _private_ with specific team access only.
   - [ ] `README.md` and Repository Description.
   - [ ] `LICENCE`
   - [ ] `CONTRIBUTING.md` (CLA)
   - [ ] Correct Copyright statements, particularly in planned open-source repositories.

4. Testbed
   - [ ] Create additional `main` branches
   - [ ] Test Simulation System workflows with GitHub
   - [ ] Make sure tags/releases are still functional
   - [ ] Update Working practices
   - [ ] Allow limited number of developers to test their workflows
   - [ ] Add/update Templates

5. During SRS freeze
   - [ ] Merge `trunk` in _future_ default (`main`) and delete `trunk`

6. Update communication plan
   - [ ] Update [Simulation Systems Discussion](https://github.com/MetOffice/simulation-systems/discussions/337)
   - [ ] Engage in [Simulation Systems Q&A](https://github.com/MetOffice/simulation-systems/discussions/categories/questions-and-answers)


## FAQ

<details>
<summary><b>How long it takes to convert svn repo to Git?</b></summary>

The listing below shows the time taken to convert trunk-only branches and
attach svn tags to the Git repository locally using `ssd_svn2git,sh` script.

    $ tail -n1 *.log
    ==> 20250330T025505_casim.log <==
    2025-03-30 02:56:25 Done: casim in 00:01:20

    ==> 20250330T025625_moci.log <==
    2025-03-30 02:59:42 Done: moci in 00:03:17

    ==> 20250330T025942_jules.log <==
    2025-03-30 03:14:50 Done: jules in 00:15:08

    ==> 20250330T031450_socrates.log <==
    2025-03-30 03:17:04 Done: socrates in 00:02:14

    ==> 20250330T031704_ukca.log <==
    2025-03-30 03:18:06 Done: ukca in 00:01:02

    ==> 20250330T031806_shumlib.log <==
    2025-03-30 03:19:40 Done: shumlib in 00:01:34

    ==> 20250330T031940_mule.log <==
    2025-03-30 03:24:19 Done: mule in 00:04:39

    ==> 20250330T032419_um_aux.log <==
    2025-03-30 03:29:52 Done: um_aux in 00:05:33

    ==> 20250330T032952_um_doc.log <==
    2025-03-30 03:42:31 Done: um_doc in 00:12:39

    ==> 20250330T034231_um_meta.log <==
    2025-03-30 03:46:47 Done: um_meta in 00:04:16

    ==> 20250330T034647_um.log <==
    2025-03-30 05:04:04 Done: um in 01:17:17

    ==> 20250330T050404_gcom.log <==
    2025-03-30 05:06:17 Done: gcom in 00:02:13

    ==> 20250330T050617_lfric_apps.log <==
    2025-03-30 05:11:44 Done: lfric_apps in 00:05:27

    ==> 20250330T051145_lfric_core.log <==
    2025-03-30 05:48:32 Done: lfric_core in 00:36:48
</details>

<details>
<summary><b>What happens to a tag if the corresponding branch gets deleted?</b></summary>
A branch is simply a way to track a collection of commits. The tag and commit
would stull exist if the branch is deleted. Ref: https://github.com/orgs/community/discussions/23918.
</details>

<details>
<summary><b>When will the developers get access to the GitHub repositories</b></summary>
Please refer to the timeline in [Simulation Systems Discussion](https://github.com/MetOffice/simulation-systems/discussions/337).
We will announce the opportunity to participate during the later part of migration testbed.
</details>
