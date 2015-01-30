# TW-SIT

The newest version is v0.50

I have added a routine for reading WOA data to  SIT. A few experiments using climate SST and climate WOA (WOA05_pottemp_salt.nc) data are conducted now.

You can find the running script and codes in the following directoy in GAEA 
/autofs/na2_home2/Benjei.Tsuang/tikal/update/v0.5
/autofs/na2_home2/Benjei.Tsuang/tikal/update/v0.5/scripts

where
"run_v5.csh" shows how to submit jobs.
"compile_hiram_sit_201409.csh" shows how to compile code.

It will be nice if someone can try AMIP-type runs.

For running AMIP type runs, you might have to upload GODAS or Isshi data to GAEA, and modify "input.nml-woa" to point to the correct time series OCN profile data.

where input.nml-woa:
   use_woa_timeseries = .false.                                         => .true.
   data_names = "ptemp", "salt"                                         => variable names of potential temperatur and salinity for GODAS or Ishii netcdf files
   data_units = "C", "PSU"                                                 => their respective variable units of potential temperatur and salinity for GODAS or Ishii netcdf files
   filename = "WOA05_pottemp_salt.nc"                          => filename of GODAS or Ishii netcdf files

