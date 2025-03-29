# Met Office Simulation Systems Git Migration Project

This project is aimed to migrate the Met Office simulation systems repositories
to Git ecosystem. See MetOffice/simulation-systems/discussions/337 for details.


### Pre-requisites to run the migration script

- Read access to the Met Office Science Repository [MOSRS](https://code.metoffice.gov.uk/trac/home).
- Write access to the [Met Office GitHub organisation](https://github.com/MetOffice).
- Tools: [`fcm`](https://metomi.github.io/fcm/doc/user_guide/introduction.html), [`git`](https://git-scm.com), [`gitlify`](https://github.com/MetOffice/gitlify), [`jq`](https://jqlang.org), and [`gh`](https://cli.github.com) available on the system.

## Checklist

1. Test `gitlify` translation tool
   - [x] Able to convert only svn trunk to git.
   - [x] Able to map svn revisions to git tag.
   - [x] Synchronise trunk updates locally in git repositories.
     - [ ] Set-up a (`scron`) job to update code/tags routinely.

2. Create/update GitHub repositories under [MetOffice](https://github.com/MetOffice) 
   - [ ] `svn:um/main` (@trunk) → [um](https://github.com/MetOffice/um) (@trunk)
   - [ ] `svn:um/aux` (@trunk) → [um_aux](https://github.com/MetOffice/um_aux) (@trunk)
   - [ ] `svn:um/meta` (@trunk) → [um_meta](https://github.com/MetOffice/um_meta) (@trunk)
   - [ ] `svn:um/doc` (@trunk) → [um_doc](https://github.com/MetOffice/um_doc) (@trunk)
     - [ ] Compress/convert large graphics in the repo consulting the authors, if possible. 
     - [ ] Implement GitHub action to build/deploy docs: test using [texlive docker image](https://hub.docker.com/r/texlive/texlive/tags?name=2018) 
   - [ ] `svn:um/mule` (@trunk) → [mule](https://github.com/MetOffice/mule) (@trunk)
     - [ ] Contact [metomi](https://github.com/metomi/mule) owner to deprecate their repository.
   - [ ] `svn:gcom/main` (@trunk) → [gcom](https://github.com/MetOffice/gcom) (@trunk)
   - [ ] `svn:jules/main` (@trunk) → [jules](https://github.com/MetOffice/jules) (@trunk)
     - [ ] Check licence agreements, if going public.
     - [ ] Plan to migrate JULES documentation here.
   - [ ] `svn:socrates/main` (@trunk) → [socrates](https://github.com/MetOffice/socrates) (@trunk)
   - [ ] `svn:utils/shumlib` (@trunk) → [shumlib](https://github.com/MetOffice/shumlib) (@trunk)
     - [ ] - [ ] Contact [metomi](https://github.com/metomi/shumlib) owner to deprecate their repository.
   - [ ] `svn:ukca/main` (@trunk) → [shumlib](https://github.com/MetOffice/ukca) (@trunk)
   - [ ] `svn:monc/casim` (@trunk) → [casim](https://github.com/MetOffice/casim) (@trunk)
   - [ ] `svn:moci/main` (@trunk) → [moci](https://github.com/MetOffice/moci) (@trunk)
   - [ ] `svn:lfric/LFRic` (@trunk) → [lfric_core](https://github.com/MetOffice/lfric_core) (@trunk)
     - [ ] Repo already exists: ask for admin access to the repository.
   - [ ] `svn:lfric_apps/main` (@trunk) → [lfric_core](https://github.com/MetOffice/lfric_apps) (@trunk)
     - [ ] Propose `lfric_core` as a submodule in this repository?
   - [ ] Check `config.json` is correct and up-to-date with MOSRS revisions.   

3. General updated for all repositories
   - [ ] All _private_ with specific team access only.
   - [ ] `README` and Repository Description.
   - [ ] `LICENCE`
   - [ ] `CLA`
   - [ ] Correct Copyright statements, particularly in planned open-source repositories.

4. Testbed
   - [ ] Create additional `main` or `develop` branches
   - [ ] Test Simulation System workflows with GitHub
   - [ ] Update Working practices
   - [ ] Allow limited number of developers to test their workflows
   - [ ] Add/update Templates

5. Update communication plan
   

