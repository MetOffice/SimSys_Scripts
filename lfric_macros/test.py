import tempfile
from apply_macros import create_temporary_config_file

nml_file = "/var/tmp/persistent/working_copies/example_dirs_macros/apps/applications/lfric_atm/example/lfric-lfric_atm/configuration.nml"
tag = "vn2.0"
meta_path="applications/lfric_atm/rose-meta/lfric-lfric_atm"

path = create_temporary_config_file(nml_file, meta_path, tag)
print(path)

