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
   
   
Q/A:
-------------------
之前跑的AMIP runs差不多都已經完成，想做一個一樣設定的HiRAM/SIT AMIP integration，之前有跑過一個月，但是SiT會輸出很多檔案跟很多變數，用mppnccombine合併會有問題，想請問是否已經改成ice只輸出一個檔案？另外，如果要積分一個AMIP實驗，是否可以減少SiT變數輸出？目前ice輸出檔案太大了
------------------

1) Yes. I have changed the setup of ice component to have only one output.

2) In addition, the grid unmatched problem for ice model output can be solved by fregrid using the command as 

gaea7:/<5>c192_dpc_sit_m2_5d5d5d_v0.50_test/history/19800104 > fregrid --input_mosaic ../../work/INPUT/ocean_mosaic.nc --nlon 720 --nlat 360 --input_file 19800104.ice_daily.nc --output_file 19800104.ice_daily.ll.nc --scalar_field wt,ws

which regrid data to regular lat and lon 

3) I have reduced the number of ice model level to 31. The output fields are less now. You can find some output files in

/lustre/f1/Benjei.Tsuang/tikal/runs/c192_rkmp_sit_m2_5d5d5d_v0.50/history/19800401


