
 Decommutate an FC block data file (in IDL 4.0 or later) and save it as a charged current spectrum.

 At time of initial commit, local file system path names may be hard-coded and require manual correction
 for a user to install and run this software outside of the Harvard Center for Astrophysics network.


> idl

> resolve_routine,'make_spec_file',/compile_full_file
> make_spec_file, lzfile = '/{lz_file_path}/WI_LZ_SWE_{YYYYMMDD}_V{VV}.DAT' ; decommutate lz file. If no file provided, will launch dialogue

> resolve_routine,'restspec_mra',/compile_full_file
> restspec_mra, spec, file = '/{decommed_file_path}/ionspec{YYYYMMDD}.idl'; load decommed spectrum file, applies calibrations and converts to physics units. Data are loaded into the idl structure variable "spec"
