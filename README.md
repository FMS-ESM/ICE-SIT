# TW-SIT

You can find my sit code in both gaea and GitHub. The newest version is v0.50

This version adds a routine for reading WOA (World Ocean Atlas) potential temperature and salinity to SIT. A few experiments using climate SST and climate WOA (WOA05_pottemp_salt.nc) data are conducted now. 

You can find the codes and running script in the following directoriess in gaea, respectively.
/autofs/na2_home2/Benjei.Tsuang/tikal/update/v0.5/*.F90
/autofs/na2_home2/Benjei.Tsuang/tikal/update/v0.5/scripts

where
"run_v5.csh" shows how to submit jobs.
"compile_hiram_sit_201409.csh" shows how to compile code.

A new namelist woa_nml is designed for describing the WOA data.

   use_woa_timeseries = .false.                                         => for climate run
   data_names = "ptemp", "salt"                                         => variable names of potential temperatur and salinity for GODAS or Ishii netcdf files
   data_units = "C", "PSU"                                                 => their respective variable units of potential temperatur and salinity for GODAS or Ishii netcdf files
   filename = "WOA05_pottemp_salt.nc"                          => filename of WOA, GODAS or Ishii netcdf files


In addition, I am experimenting GitHUB according to suggestion from OCN group. I have put the sit code in:

https://github.com/btsuang/TW-SIT


Note that,
1) The setup of ice component is to have only one output.
2) The grid unmatched problem for ice model output can be solved by fregrid using the command as 

gaea7:/<5>c192_dpc_sit_m2_5d5d5d_v0.50_test/history/19800104 > fregrid --input_mosaic ../../work/INPUT/ocean_mosaic.nc --nlon 720 --nlat 360 --input_file 19800104.ice_daily.nc --output_file 19800104.ice_daily.ll.nc --scalar_field wt,ws
which regrid data to regular lat and lon 
3) I have reduced the number of ice model level to 31. The output fields are less now. You can find some output files in
/lustre/f1/Benjei.Tsuang/tikal/runs/c192_rkmp_sit_m2_5d5d5d_v0.50/history/19800401
4) You can change the maximum depth (m) and model levels for handing by SIT by change the following  variables iin sit_nml:

ocn_tlz (maximum ocn depth)
ocn_kl (number of model levels in addition to 12  levels with  upper 10m  ddepth)
bathy_default (defaul depth in each ocn grid)

