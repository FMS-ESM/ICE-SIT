#!/bin/csh -fv
####!/bin/csh

########################################################################
# generic run script for experiments                     
########################################################################
if ($#argv >= 7 ) then
  set batch=$1
  set qos=$2
  setenv ver $3
  setenv minor $4
  setenv res $5
  setenv model $6
  setenv do_test $7
  setenv use_climo $8
  setenv do_sit $9
  if ($#argv >= 11 ) then
    setenv maskid $10
    setenv sitArg $11
    setenv sit_suffix "_sit_m${maskid}_${sitArg}"
    if ($#argv >= 12 ) then
      setenv bathy_default ${12}    
      setenv sit_suffix "${sit_suffix}${bathy_default}"
    else
      setenv bathy_default -300.                  # -200.  
    endif
  else
    setenv maskid ""
    setenv sitArg ""
    setenv sit_suffix ""
  endif
else
  echo "Usage: $0 ver minor res model do_test do_sit maskid sitArg"
  echo "run script for experiments"
  echo "available options:"
  echo "batch  = msub                 # msub run"
  echo "       = interactive          # interactive run"
  echo "       = not_run              # creating script, but not run"
  echo "qos    = windfall             # creating script, but not run"
  echo "       = norm                 # creating script, but not run"
  echo "       = debug                # creating script, but not run"
  echo "ver    = org                  # orginal HiRAM"
  echo "       = v0.4                 # HiRAM/SIT"
  echo "minor  = "
  echo "       = 903"
  echo "       = 906"
  echo "res    = 96"
  echo "       = 96o1"
  echo "       = 192"
  echo "model  = hiram                 # Lucas works for c192 (HiRAM)"
  echo "       = dpc                   # Ming.Zhao for c180 (double plume)"
  echo "do_test = .true.               # test run"
  echo "       = .false.               # production run"
  echo "do_sit = .true."
  echo "       = .false."
  echo "maskid = 0, DIECAST grids only"
  echo "       = 1, Ocean and lakes (default)"
  echo "       = 2, Ocean"
  echo "       = 3, Ocean within 30N-30S"
  echo "       = 4, all the grid"
  echo "       = 5, NON SIT grids"
  echo "sitArg = nnnnnn"
  echo "       = nn1m1d"
  echo "       = 1d1d1d"
  echo "       = 0d0d0d"
  echo " eg,"
  echo "    $0 v0.4 minor res  model  do_test do_sit maskid sitArg"
  echo "    $0 org  906   192 b2_p7   .false. .false."
  echo "    $0 v0.4 ''    192 b2_p7   .false. .false."
  echo "    $0 v0.4 906   192 b2_p7   .false. .true. 2      nn1m1d"
  echo "    $0 v0.4 906   192 b2_p7   .false. .true. 2      nn1m1d"
  echo "    $0 v0.4 906   96  b2_p13b .false. .true. 2      nn1m1d"
  echo "    $0 v0.4 906   96  b2_p13b .false. .true. 3      nn1m1d"
  echo "    $0 v0.4 906   96  b2_p13b .true.  .true. 2      nn1m1d"
  echo ""
  goto done
endif

if ( ${ver} == "org" ) then
  set org=.true.
else
  set org=.false.
endif
#echo "${org}"
if ( ${do_test} == ".true." ) then
  if ( 1 == 0 ) then
    set test_aerosol=.true.
    set test_woa=.false.
    set program_coupler_main=.false.    
  else if ( 1 == 0 ) then
    set test_aerosol=.false.
    set test_woa=.true.
    set program_coupler_main=.false.    
  else if ( 1 == 1 ) then
    set test_aerosol=.false.
    set test_woa=.false.
    set program_coupler_main=.true.    
  endif
  set test="_test"
else
  set test=""
  set test_aerosol=.false.
  set test_woa=.false.
  set program_coupler_main=.true.    
endif



  if ((${res} == "96")||(${res} == "96o1")) then
    setenv npes_x  8
    setenv npes_y  8
    setenv dt_atmos 1200
    setenv npx  97
    setenv npy  97
  else if ( ${res} == "192" ) then
    setenv npes_x  12
    setenv npes_y  12  
    setenv dt_atmos 900
    setenv npx      193
    setenv npy      193
  else
    setenv npes_x  1
    setenv npes_y  1  
    setenv dt_atmos 900
    setenv npx      1
    setenv npy      1
  endif

  setenv mpes_x  0
  setenv mpes_y  0
  setenv layout_cube  $npes_x,$npes_y
  setenv layout_grid  $mpes_x,$mpes_y
  setenv spread 1

  if ( 1 == 1 )  then
    setenv npex_io 1
    setenv npey_io 1
  else
    setenv npex_io 2
    setenv npey_io 2
  endif
  setenv mpex_io 0
  setenv mpey_io 0

# echo program_coupler_main=${program_coupler_main}
  if ( ${program_coupler_main} == ".false." ) then
    if ( 1 == 0 ) then
      setenv npes_g1 2
      setenv npes 2    
    else
      setenv npes_g1 1
      setenv npes 1    
    endif
  else
    @ npes_g1=$npes_x * $npes_y * 6
    @ npes=$npes_x * $npes_y * 6
  endif
#  echo npes=${npes}
#exit
if ( ${use_climo} == ".true." ) then
  set use_climo_ice=.true.
  set use_climo_sst=.true.
  set data_set = 'reynolds_oi'
  set date_out_of_range = 'climo'
  set use_aerosol_timeseries=.false.
else
  set use_climo_ice=.false.
  set use_climo_sst=.false.
  set data_set = 'hurrell'
  set date_out_of_range = 'fail'
  set use_aerosol_timeseries=.true.
endif
#set use_woa_timeseries=.true.
set use_woa_timeseries=.false.
#echo use_woa_timeseries=${use_woa_timeseries}
#echo model=${model}

if ( ${model} == "hiram" ) then
  if ((${res} == "96")||(${res} == "96o1")) then
    set suffix  = b2_p13b               # Lucas works for c96 (HiRAM)"
#  else if ( ${res} == "192" ) then
  else
    set suffix = b2_p7
  endif
  set do_lin_cld_microphys=.true.
else if ( ${model} == "rkmp" ) then
# enabling the Rotsteyn-Klein Microphysics
  if ((${res} == "96")||(${res} == "96o1")) then
    set suffix  = b2_p13b               # Lucas works for c96 (HiRAM)"
#  else if ( ${res} == "192" ) then
  else
    set suffix = b2_p7
  endif
  set do_lin_cld_microphys=.false.
else if ( ${model} == "dpc" ) then
  set suffix   = dpc_H0                # Ming.Zhao for c180 (double plume)"
  set do_lin_cld_microphys=.false.
else
  set suffix   = dpc_H0                # Ming.Zhao for c180 (double plume)"
  set do_lin_cld_microphys=.false.
endif
echo do_lin_cld_microphys=${do_lin_cld_microphys}

if ( ${do_sit} == ".true." ) then
  set name=${res}_${model}${sit_suffix}
else
  set name=${res}_${model}
endif

if ( ${use_climo} == ".false." ) then
  set name=a${name}
else
  set name=c${name}
endif

set name=${name}_${ver}${minor}${test}

echo name=$name

set scriptName=runscript_${name}.csh
# experiment source directory name:
set base = tikal

set CWD=`pwd`
# home directory:
set sourceDir = $HOME/$base/update/${ver}/scripts/runs
echo $sourceDir
# dir name:
set dirName = runs/${name}
# fast scratch:
set fscratch = $CTMP/$USER/${base}/${dirName}
# long-term scratch:
set lscratch = $CPERM/$USER/${base}/${dirName}
# gfdl archive:
set gfdl_archive = /archive/$USER/${base}/${dirName}
set workDir        = $fscratch/work
if ( 1 == 0 ) then
  set scriptFullName = ${CWD}/runs/${scriptName}
  set stdoutDir      = ${CWD}/runs/stdout
else
  set scriptFullName = ${workDir}/${scriptName}
  set stdoutDir      = $scriptFullName:h/../stdout
endif
echo $stdoutDir
if ( ! -d $stdoutDir ) mkdir -p $stdoutDir
if ( ! -d $stdoutDir ) then
  echo "ERROR: $stdoutDir is not a directory."
  exit 1
endif

###exit
  
cat >! ${CWD}/runs/${scriptName} <<EOF 
#!/bin/csh -fv
#PBS -o ${stdoutDir}/
#PBS -N $name
###PBS -l walltime=4:00:00
#PBS -l walltime=16:00:00
#PBS -q batch
#PBS -A gfdl_w
#PBS -l partition=c1:c2
#PBS -l size=${npes}
###PBS -l size=$4
###PBS -l size=384
#PBS -m abe
#PBS -j oe
#PBS -l qos=${qos}
####PBS -l qos=windfall

setenv ver ${ver}
setenv minor ${minor}
setenv res ${res}
setenv model ${model}
setenv suffix ${suffix}
setenv do_test ${do_test}
setenv use_climo ${use_climo}
setenv do_sit ${do_sit}
setenv maskid ${maskid}
setenv sitArg ${sitArg}
setenv sit_suffix ${sit_suffix}
setenv scriptName ${scriptName}
setenv org .false.
setenv bathy_default ${bathy_default}
setenv use_climo_ice ${use_climo_ice}
setenv use_climo_sst ${use_climo_sst}
setenv data_set ${data_set}
setenv date_out_of_range ${date_out_of_range}
setenv use_aerosol_timeseries ${use_aerosol_timeseries}
setenv test_aerosol ${test_aerosol}
setenv use_woa_timeseries ${use_woa_timeseries}
setenv test_woa ${test_woa}
setenv do_lin_cld_microphys ${do_lin_cld_microphys}


if ( \${ver} == "org" ) then
  set org=.true.
else
  set org=.false.
endif

 set beginruntime = \`date "+%s"\`

########################################################################
# name of executable:

########################################################################
# set experiment-specific variables                       
########################################################################

if ( \${org} == ".true." ) then
  set ice_cold_start=F
  set executable = /lustre/f1/unswept/\$USER/tikal/exec-lin_201407a_lmh/fms_tikal.x
else if ( \${test_aerosol} == ".true." ) then
  set ice_cold_start=T
  set executable = /lustre/f1/unswept/\$USER/tikal/\${ver}/aerosol_sit_\${ver}\${minor}.x
else if ( \${test_woa} == ".true." ) then
  set ice_cold_start=T
  set executable = /lustre/f1/unswept/\$USER/tikal/\${ver}/woa_sit_\${ver}\${minor}.x
else
##else if ( \${do_test} == ".true." ) then
  set ice_cold_start=T
  set executable = /lustre/f1/unswept/\$USER/tikal/\${ver}/fms_tikal_sit_\${ver}\${minor}.x
endif

# experiment name:
set name = ${name}
#set name = runs/\${res}
# experiment source directory name:
set base = ${base}
# home directory:
set sourceDir = ${sourceDir}
#if ( \${model} == "hiram" ) then
#  if ( \${res} == "192" ) then
#    set suffix = b2_p7                 # Lucas works for c192 (HiRAM)"
#  else if ( \${res} == "96" ) then
#   set suffix  = b2_p13b               # Lucas works for c96 (HiRAM)"
#  endif
#else if ( \${model} == "dpc" ) then
#  set suffix   = dpc_H0                # Ming.Zhao for c180 (double plume)"
#else
#  set suffix   = dpc_H0                # Ming.Zhao for c180 (double plume)"
#endif


if (1 == 1) then
  set lwoa_gfdl = .true.
else if (1 == 1) then
  set lwoa_gfdl = .false.
endif

set input_nml    = \$sourceDir/../input.nml-\${suffix}
set woa_nml    = \$sourceDir/../input.nml-woa
set sit_nml    = \$sourceDir/../input.nml-sit

if ( \${sitArg} == "nnnnnn" ) then
  set ssit_restore_time = -9.000000000000000E+033
  set usit_restore_time = -9.000000000000000E+033
  set dsit_restore_time = -9.000000000000000E+033
else if ( \${sitArg} == "nn1m1d" ) then
  set ssit_restore_time = -9.000000000000000E+033
  set usit_restore_time = 2592000.
  set dsit_restore_time = 86400.
else if ( \${sitArg} == "nn1d1d" ) then
  set ssit_restore_time = -9.000000000000000E+033
  set usit_restore_time = 86400.
  set dsit_restore_time = 86400.
else if ( \${sitArg} == "10d10d1d" ) then
  set ssit_restore_time = 864000.
  set usit_restore_time = 864000.
  set dsit_restore_time = 86400.
else if ( \${sitArg} == "5d5d5d" ) then
  set ssit_restore_time = 432000.
  set usit_restore_time = 432000.
  set dsit_restore_time = 432000.
else if ( \${sitArg} == "5d1d1d" ) then
  set ssit_restore_time = 432000.
  set usit_restore_time = 86400.
  set dsit_restore_time = 86400.
else if ( \${sitArg} == "2d1d1d" ) then
  set ssit_restore_time = 172800.
  set usit_restore_time = 86400.
  set dsit_restore_time = 86400.
else if ( \${sitArg} == "1d1d1d" ) then
  set ssit_restore_time = 86400.
  set usit_restore_time = 86400.
  set dsit_restore_time = 86400.
else if ( \${sitArg} == "0d0d0d" ) then
  set ssit_restore_time = 0.
  set usit_restore_time = 0.
  set dsit_restore_time = 0.
endif


# dir name:

  set dirName = ${dirName}
  echo $dirName

# fast scratch:
  set fscratch = ${fscratch}

# long-term scratch:
  set lscratch = ${lscratch}

# gfdl archive:
  set gfdl_archive = ${gfdl_archive}

if ( ${use_climo} == ".true." ) then
  setenv begDate  '1980,1,1,0,0,0'
else
  setenv begDate  '2000,1,1,0,0,0'
endif
setenv baseDate '1980 1 1 0 0 0'

########################################################################
# set experiment initial conditions on short-term scratch                       
########################################################################

if ( \${res} == "96" ) then
  set initCond           = \$CDATA/gfdl_W/Lucas.Harris/IC/c96/
  set input_data         = \$CDATA/gfdl_W/Lucas.Harris/HiRAM/input_data-lmh-rev201403/
  set mosaic_grid        = \$CDATA/gfdl_W/Lucas.Harris/awg/input/grid/c96_GIS_025_grid_v20140327/
  set dst_static_veg     = \$CDATA/gfdl_W/Lucas.Harris/awg/input/lm3/19810101.static_veg.remapped_to_C96_GIS_025.v20140328/
  set input_data_land    = \$CDATA/gfdl_W/Lucas.Harris/awg/input/lm3/lm3_data_20130815/
  set input_hydrography  = \$CDATA/gfdl_W/Lucas.Harris/awg/input/lm3/c96_GIS_025_hydrography_v20140328/
else if ( \${res} == "96o1" ) then
  set initCond           = \$CDATA/gfdl_W/Lucas.Harris/IC/c96/
  set input_data         = \$CDATA/gfdl_W/Lucas.Harris/HiRAM/input_data-lmh-rev201403/
  set mosaic_grid        = \$CDATA/gfdl_W/Benjei.Tsuang/gridSpecs/C96O1/
  set dst_static_veg     = \$CDATA/gfdl_W/Lucas.Harris/awg/input/lm3/19810101.static_veg.remapped_to_C96_GIS_025.v20140328/
  set input_data_land    = \$CDATA/gfdl_W/Lucas.Harris/awg/input/lm3/lm3_data_20130815/
#  set input_data_land    = 'null'
#  set input_hydrography  = \$CDATA/gfdl_W/Lucas.Harris/awg/input/lm3/c96_GIS_025_hydrography_v20140328/
#  set input_hydrography  = \$CDATA/gfdl_W/Lucas.Harris/awg/input/lm3/c96_hydrography_20130805/
  set input_hydrography  = 'null'
else
### else if ( \${res} == "192" ) then
  if ( \${use_climo} == ".true." ) then
  # CLIM run
    set initCond           = \$CDATA/gfdl_W/Lucas.Harris/IC/c192L32-1g-awg/
    set input_data         = \$CDATA/gfdl_W/Lucas.Harris/HiRAM/input_data-lm3-mod/
    set mosaic_grid        = \$CDATA/gfdl_W/Lucas.Harris/awg/input/grid/C192_CM2.5FLOR_grid/
    # from   set gridSpec = /lustre/f1/unswept/Chris.Golaz/awg/input/grid/C192_CM2.5FLOR_grid.tar
    set dst_static_veg     = \$CDATA/gfdl_W/Lucas.Harris/awg/input/lm3/c192_19810101_static_veg_20130911/
    #from /lustre/f1/unswept/Chris.Golaz/awg/input/lm3/c192_19810101_static_veg_20130911.tar
    set input_data_land    = \$CDATA/gfdl_W/Lucas.Harris/awg/input/lm3/lm3_data_20130815/
    set input_hydrography  = \$CDATA/gfdl_W/Lucas.Harris/awg/input/lm3/c192_hydrography_20130813/
    #from /lustre/f1/unswept/Chris.Golaz/awg/input/lm3/c192_hydrography_20130813.tar and /lustre/ltfs/scratch/Chris.Golaz/awg/input/lm3/lm3_data_20130815.tar
  else
  # AMIP run
    set initCond           = \$CDATA/gfdl_W/Lucas.Harris/IC/c192L32-1g-awg/
    #set initCond           = /lustre/f1/Lucas.Harris/tikal/runs/c192-AMIP_b2_p7-gmax3-e2/restart/20140101/
    #/lustre/f1/pdata/gfdl_W/Lucas.Harris/IC/c192L32-1g-awg
    set input_data         = \$CDATA/gfdl_W/Lucas.Harris/HiRAM/input_data-lmh-rev201403/
    #  set input_data         = \$CDATA/gfdl_W/Lucas.Harris/HiRAM/input_data-lm3-mod
    set mosaic_grid        = \$CDATA/gfdl_W/Lucas.Harris/awg/input/grid/C192_CM2.5FLOR_grid/
    # from   set gridSpec = /lustre/f1/unswept/Chris.Golaz/awg/input/grid/C192_CM2.5FLOR_grid.tar
    set dst_static_veg     = \$CDATA/gfdl_W/Lucas.Harris/awg/input/lm3/c192_19810101_static_veg_20130911/
    #from /lustre/f1/unswept/Chris.Golaz/awg/input/lm3/c192_19810101_static_veg_20130911.tar
    set input_data_land    = \$CDATA/gfdl_W/Lucas.Harris/awg/input/lm3/lm3_data_20130815/
    set input_hydrography  = \$CDATA/gfdl_W/Lucas.Harris/awg/input/lm3/c192_hydrography_20130813/
    #from /lustre/f1/unswept/Chris.Golaz/awg/input/lm3/c192_hydrography_20130813.tar and /lustre/ltfs/scratch/Chris.Golaz/awg/input/lm3/lm3_data_20130815.tar
  endif
endif
set input_data_ocean    = \$CDATA/gfdl_O/datasets/obs/


  set input_data_grid    = 'null'
  set dst_restart_land   = 'null'

########################################################################
# set experiment initial conditions on long-term scratch                      
########################################################################

  set initCond_ltfs           = 'null'
  set input_data_ltfs         = 'null'
  set input_data_grid_ltfs    = 'null'
  set mosaic_grid_ltfs        = 'null'
  set dst_static_veg_ltfs     = 'null'
  set dst_restart_land_ltfs   = 'null'

########################################################################
# note: all string substitutions in these files must be done with 
# setting the variables as environmental variables
########################################################################

  if ( 1 == 1 ) then
    set diag_table  = \$sourceDir/../diag_table
  else if ( 1 == 1 ) then
    set diag_table  = \$sourceDir/../diag_table_v7
  else if ( 1 == 1 ) then
    set diag_table  = \$sourceDir/../diag_table_v6
  else if ( 1 == 1 ) then
    set diag_table  = \$sourceDir/../diag_table_v5
  else if ( 1 == 1 ) then
    set diag_table  = \$sourceDir/../diag_table_v4
  else if ( 1 == 0 ) then
    set diag_table  = \$sourceDir/../diag_table_v3
  else if ( 1 == 1 ) then
    set diag_table  = \$sourceDir/../diag_table_v2
  else if ( 1 == 1 ) then
    set diag_table  = \$sourceDir/../diag_table_sit
  else
    set diag_table   = \$sourceDir/../diag_table-AMIP
  endif

  set diag_table_lin_cld_microphys = \$sourceDir/../diag_table_lin_cld_microphys
  set field_table  = \$sourceDir/../field_table
  ###set field_table = \$sourceDir/../inputs/field_table

  echo \${res}
  if ( 1 == 0 ) then
    set monthslist     = ( 1 )
    #set dayslist       = ( 7 )
    set dayslist       = ( 0 )
    set hourslist      = ( 0 ) 
    set numSubmissions =  1
  else if ( 1 == 0 ) then
    set monthslist     = ( 0 0 0 0 )
    set dayslist       = ( 1 1 1 1 )
    set hourslist      = ( 0 0 0 0 ) 
    set numSubmissions =  1
  else if ( 1 == 0 ) then
    set monthslist     = ( 0 )
    set dayslist       = ( 0 )
    set hourslist      = ( 6 ) 
    set numSubmissions =  1
  #else if ( 1 == 0 ) then
  else if ( \${do_test} == ".true." ) then
    set monthslist     = ( 0 0 1 1 1 1 )
    set dayslist       = ( 3 7 0 0 0 0 )
    set hourslist      = ( 0 0 0 0 0 0 ) 
    set numSubmissions =  22
  else if ( \${do_test} == ".true." ) then
    set monthslist     = ( 0 0 0 0 0 0 0 0 0 0 0 0 0 0 )
    set dayslist       = ( 3 3 3 3 3 3 3 3 3 3 3 3 3 3 )
    set hourslist      = ( 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ) 
    set numSubmissions =  1
  else if ( \${res} == "96" ) then
    if ( \${do_sit} == ".true." ) then
      # sit run 1.1 h/1 mon
      set monthslist     = ( 3 3 3 3 )
      set dayslist       = ( 0 0 0 0 )
      set hourslist      = ( 0 0 0 0 ) 
      set numSubmissions =  11
    else 
      # non sit run 0.5 h/1 mon
      set monthslist     = ( 3 3 3 3 3 3 3 )
      set dayslist       = ( 0 0 0 0 0 0 0 )
      set hourslist      = ( 0 0 0 0 0 0 0 ) 
      set numSubmissions =  6
    endif
  else if ( \${res} == "96o1" ) then
    if ( \${do_sit} == ".true." ) then
      # sit run 1.1 h/1 mon
      set monthslist     = ( 1 1 1 1 1 1 1 1 1 1 1 1 )
      set dayslist       = ( 0 0 0 0 0 0 0 0 0 0 0 0 )
      set hourslist      = ( 0 0 0 0 0 0 0 0 0 0 0 0 ) 
      set numSubmissions =  12
    else 
      # sit run 1.1 h/1 mon
      set monthslist     = ( 1 1 1 1 1 1 1 1 1 1 1 1 )
      set dayslist       = ( 0 0 0 0 0 0 0 0 0 0 0 0 )
      set hourslist      = ( 0 0 0 0 0 0 0 0 0 0 0 0 ) 
      set numSubmissions =  12
    endif
  else if ( \${res} == "192" ) then
    if ( \${do_sit} == ".true." ) then
      # sit run 1.1 h/1 mon
      set monthslist     = ( 1 1 1 1 1 1 1 1 1 1 1 1 )
      set dayslist       = ( 0 0 0 0 0 0 0 0 0 0 0 0 )
      set hourslist      = ( 0 0 0 0 0 0 0 0 0 0 0 0 ) 
      set numSubmissions =  12
    else 
      # sit run 1.1 h/1 mon
      set monthslist     = ( 1 1 1 1 1 1 1 1 1 1 1 1 )
      set dayslist       = ( 0 0 0 0 0 0 0 0 0 0 0 0 )
      set hourslist      = ( 0 0 0 0 0 0 0 0 0 0 0 0 ) 
      set numSubmissions =  12
    endif
  else
    set monthslist     = ( 1 1 1 1 1 1 )
    set dayslist       = ( 0 0 0 0 0 0 )
    set hourslist      = ( 0 0 0 0 0 0 ) 
    set numSubmissions =  22
  endif

  echo \${monthslist}
  echo \${dayslist}
  echo \${hourslist}

  if ((\${res} == "96")||(\${res} == "96o1")) then
    setenv npes_x  8
    setenv npes_y  8
    setenv dt_atmos 1200
    setenv npx  97
    setenv npy  97
  else if ( \${res} == "192" ) then
    setenv npes_x  12
    setenv npes_y  12  
    setenv dt_atmos 900
    setenv npx      193
    setenv npy      193
  else
    setenv npes_x  1
    setenv npes_y  1  
    setenv dt_atmos 900
    setenv npx      1
    setenv npy      1    
  endif

  setenv mpes_x  0
  setenv mpes_y  0
  setenv layout_cube  \$npes_x,\$npes_y
  setenv layout_grid  \$mpes_x,\$mpes_y
  setenv spread 1

  if ( 1 == 1 )  then
    setenv npex_io 1
    setenv npey_io 1
  else
    setenv npex_io 2
    setenv npey_io 2
  endif
  setenv mpex_io 0
  setenv mpey_io 0

  setenv npes_g1 $npes_g1
  setenv npes $npes

#  @ npes_g1=\$npes_x * \$npes_y * 6
#  @ npes=\$npes_x * \$npes_y * 6

########################################################################

# experiment name:
#  if ( 1 == 1 ) then
#    set nmlFlag  = "-tune-p6"
#  else if ( 1 == 0 ) then
#    set nmlFlag  = "-tune-p6-sit"
#  else if ( 1 == 1 ) then
#    set nmlFlag  = "-sit-tw"
#  endif
##
#
#  if ( \${sitArg} == "nnnnnn" ) then
#    set ssit_restore_time = -9.000000000000000E+033
#    set usit_restore_time = -9.000000000000000E+033
#    set dsit_restore_time = -9.000000000000000E+033
#  else if ( \${sitArg} == "nn1m1d" ) then
#    set ssit_restore_time = -9.000000000000000E+033
#    set usit_restore_time = 2592000.
#    set dsit_restore_time = 86400.
#  else if ( \${sitArg} == "1d1d1d" ) then
#    set ssit_restore_time = 86400.
#    set usit_restore_time = 86400.
#    set dsit_restore_time = 86400.
#  else if ( \${sitArg} == "0d0d0d" ) then
#    set ssit_restore_time = 0.
#    set usit_restore_time = 0.
#    set dsit_restore_time = 0.
#  endif
#  
#
########################################################################
# set path names
########################################################################

  if ( 1 == 0 ) then
    # problem with ###PBS -q eslogin
    set send_file  = ~Lucas.Harris/coupled_siena/scripts/send_file.csh
  else if ( 1 == 0 ) then
    # problem with ###PBS -q eslogin
    set send_file  = ~Lucas.Harris/coupled_siena/scripts/send_file.csh
  else if ( 1 == 1 ) then
    # testing
    # partially OK for ascii_out.tgz, but error for nc.tar
    set send_file  = ~Benjei.Tsuang/tikal/update/v0.4/scripts/send_file_lch_v2.csh
  else if ( 1 == 0 ) then
    # Crash in send_file
    set send_file  = ~Benjei.Tsuang/tikal/update/v0.4/scripts/send_file_lch.csh
  else if ( 1 == 0 ) then
    set send_file  = ~Benjei.Tsuang/tikal/update/v0.4/scripts/send_file_org.csh
  else if ( 1 == 0 ) then
    set send_file  = /autofs/na2_home2/Daniel.Gall/Hiram_pub/scripts/pp.csh
  else if ( 1 == 0 ) then
    set send_file  = ~Benjei.Tsuang/tikal/update/v0.4/scripts/send_file_dg.csh
#sending /lustre/f1/Benjei.Tsuang/tikal/runs/c192_dpc_sit_m2_nn1m1d_v0.4926_test/history/19800104.tar
#Writing to /archive/Benjei.Tsuang/tikal/runs/c192_dpc_sit_m2_nn1m1d_v0.4926_test/history/
#
#Running '/ncrc/usw/gcp/local/opt/gcp/2.2/gcp -cd -v /lustre/f1/Benjei.Tsuang/tikal/runs/c192_dpc_sit_m2_nn1m1d_v0.4926_test/history/19800104.tar gfdl:/archive/Benjei.Tsuang/tikal/runs/c192_dpc_sit_m2_nn1m1d_v0.4926_test/history/'
#Can't locate Err.pm in @INC (@INC contains: /sw/rdtn/perl/5.10.1/centos5.7_gnu4.1.2/lib/5.10.1/x86_64-linux /sw/rdtn/perl/5.10.1/centos5.7_gnu4.1.2/lib/5.10.1 /sw/rdtn/perl/5.10.1/centos5.7_gnu4.1.2/lib/site_perl/5.10.1/x86_64-linux /sw/rdtn/perl/5.10.1/centos5.7_gnu4.1.2/lib/site_perl/5.10.1 .) at /ncrc/usw/gcp/local/opt/gcp/2.2/gcp line 42.
#BEGIN failed--compilation aborted at /ncrc/usw/gcp/local/opt/gcp/2.2/gcp line 42.
#ERROR: in gcp (2)
#
  else if ( 1 == 0 ) then
    set send_file  = ~Benjei.Tsuang/tikal/update/v0.4/scripts/send_file_jhc.csh    
#gcp -v -cd /lustre/f1/Benjei.Tsuang/tikal/runs/c192_hiram_sit_m2_nn1m1d_v0.4926_test/ascii/19800104.ascii_out.tgz gfdl:/archive/Benjei.Tsuang/tikal/runs/c192_hiram_sit_m2_nn1m1d_v0.4926_test/ascii/19800104.ascii_out.tgz
#gcp 2.2.1 on gaea2.ncrc.gov by Benjei.Tsuang at Fri Nov 14 18:32:59 2014
#Unique log session id is f89bf6b5-e679-47ca-ac29-1971e287462e at 2014-11-14Z23:32
#Error: No valid certificate found, cannot continue.
  else if ( 1 == 0 ) then
    set send_file  = ~Benjei.Tsuang/tikal/update/v0.4/scripts/send_file_dg_eslogin.csh
  else if ( 1 == 0 ) then
    set send_file  = ~Benjei.Tsuang/tikal/update/v0.4/scripts/send_file_jhc_eslogin.csh    
#gcp 2.2.1 on gaea6.ncrc.gov by Benjei.Tsuang at Fri Nov 14 20:18:15 2014
#Unique log session id is f1f9467b-f8fb-4e37-bab6-eff226ce9bbe at 2014-11-15Z01:18
#Error: No valid certificate found, cannot continue.
  endif


########################################################################
# default directories
########################################################################

  set workDir        = \$fscratch/work
# script name:
  set scriptFullName = ${scriptFullName}
  set stdoutDir      = ${stdoutDir}
  set outputDir      = \$fscratch
  set outputDir_ltfs = \$lscratch

#################################################################
# set environment
#################################################################

  setenv siteConfig ./env.cshrc.compile.csh
  setenv siteConfig \$HOME/\$base/scripts/env.cshrc 

  if ( -f \$siteConfig ) source \$siteConfig
  which time_stamp.csh

  setenv MAIN_PROGRAM coupler_main.o
  set lustre_options = '1048576 -1 12'

  limit stacksize    unlimited
  limit coredumpsize unlimited

  setenv MPICH_UNEX_BUFFER_SIZE 256m
  setenv MPICH_MAX_SHORT_MSG_SIZE 64000
  setenv MPICH_PTL_UNEX_EVENTS 160k
  setenv OMP_NUM_THREADS  \$spread
  setenv cores_per_node  32
  @ mpi_tasks_per_node=\$cores_per_node / \$OMP_NUM_THREADS
  echo ' number of MPI tasks per compute node  = ' \$mpi_tasks_per_node
########################################################################
# only highly premeditated user changes below here
########################################################################
  if ( \$#dayslist != \$#monthslist ) then
    unset echo
    echo "ERROR: dayslist and monthslist must be lists of the same length"
    exit 1
  endif
  set segmentsPerJob = \$#dayslist
  echo \${npes}pe_\${segmentsPerJob}run
########################################################################
# set up variables for looping and reloading
########################################################################
  set irun             = 1
  set ireload          = 1
  set reload_file      = \$outputDir/ascii/reload_commands
########################################################################
# source reload commands
########################################################################
  if ( -d \$outputDir )  then
    if ( -f \$reload_file ) then
      if ( -r \$reload_file ) then
        source \$reload_file
      else
        unset echo
        echo "ERROR: reload file is not readable: \$reload_file"
        exit 1
      endif
    endif
  else
    mkdir -p \$outputDir
    if ( \$ireload != 1 ) then
      set reload_file = \$outputDir_ltfs/ascii/reload_commands
      gcp -v -d \$reload_file reload_commands
      tar -xvf reload_commands
      source reload_commands
    endif
  endif
########################################################################
# set up working directory structure
########################################################################
  if ( -d \$workDir ) then
   rm -rf \$workDir
  endif
  mkdir -p \$workDir
  lfs setstripe \$workDir \$lustre_options
  if ( ! -d \$workDir ) then
    echo "ERROR: \$workDir is not a directory."
    exit 1
  endif
  cd \$workDir
  cp -p ${CWD}/runs/\${scriptName} \${scriptFullName}
#  if ( ! -f \$scriptFullName ) then
#    cp -p ${CWD}/runs/\${scriptName} \${scriptFullName}
#  else
#    echo WARNNING! \$scriptFullName exist!
#  endif
#
  if ( ! -d INPUT   ) mkdir INPUT 
  lfs setstripe INPUT \$lustre_options
  if ( ! -d INPUT ) then
    echo "ERROR: \$work/INPUT/ is not a directory."
    exit 1
  endif
 
  if ( ! -d RESTART ) mkdir RESTART
  lfs setstripe RESTART \$lustre_options
  if ( ! -d RESTART ) then
    echo "ERROR: \$work/RESTART/ is not a directory."
    exit 1
  endif

########################################################################
# check on existence of data in short-term scratch and then link data to 
# workdir
########################################################################

#Need to override hydrography and land inputs in input_data


  set data = \$input_data_land

  if (\$data != 'null') then
    set list = \`ls -C1 \$data\`
    cd \$workDir/INPUT
    foreach index (\$list)
      # echo \$index
      ln -s \$data/\$index .
    end
  endif

  set data = \$input_hydrography
  if (\$data != 'null') then
    set list = \`ls -C1 \$data\`
    cd \$workDir/INPUT
    foreach index (\$list)
      echo \$index
      ln -s \$data/\$index .
    end
  endif

  set data = \$input_data
  if (\$data != 'null') then
    set list = \`ls -C1 \$data\`
    cd \$workDir/INPUT
    foreach index (\$list)
      echo \$index
      ln -s \$data/\$index .
    end
  endif

  set data = \$mosaic_grid
  if (\$data != 'null') then
    set list = \`ls -C1 \$data\`
    cd \$workDir/INPUT
    foreach index (\$list)
      echo \$index
      ln -sf \$data/\$index .
    end
  endif

  set data = \$input_data_grid
  if (\$data != 'null') then
    set list = \`ls -C1 \$data\`
    cd \$workDir/INPUT
    foreach index (\$list)
      echo \$index
      ln -s \$data/\$index .
    end
  endif

  set data = \$initCond
  if (\$data != 'null') then
    set list = \`ls -C1 \$data\`
    cd \$workDir/INPUT
    foreach index (\$list)
      echo \$index
      ln -s \$data/\$index .
    end
  endif

  set data = \$dst_static_veg
  if (\$data != 'null') then
    set list = \`ls -C1 \$data\`
    cd \$workDir/INPUT
    foreach index (\$list)
      echo \$index
      ln -s \$data/\$index .
    end
  endif

  set data = \$input_data_ocean
  if (\$data != 'null') then
    set list = \`ls -C1 \$data\`
    cd \$workDir/INPUT
    foreach index (\$list)
      echo \$index
      ln -s \$data/\$index .
    end
  endif

  if ( \$ireload == 1 ) then
    set data = \$dst_restart_land
    if (\$data != 'null') then
      set list = \`ls -C1 \$data\`
      cd \$workDir/INPUT
      foreach index (\$list)
        echo \$index
        ln -sf \$data/\$index .
      end
    endif
  endif
########################################################################
# copy executable
########################################################################

  cd \$workDir
  if ( -e \$executable ) then
  cp \$executable \$executable:t
  else
    gcp -v \$executable \$executable:t  #:s+/++ .
    chmod +xr \$executable:t
  endif

########################################################################
# generate field_table
########################################################################

source \$field_table

########################################################################
# generate diag_table 
########################################################################

echo \$diag_table
source \$diag_table
if ( \${do_lin_cld_microphys} == ".true." ) then
  source \$diag_table_lin_cld_microphys
endif
###exit

########################################################################
# generate data_table 
########################################################################

touch data_table

########################################################################
# begin loop over \$segmentsPerJob
########################################################################
  
  while (\$irun <= \$segmentsPerJob)

    if ((\$ireload == 1 )&&(\$irun == 1 )) then
      setenv warmstart .false.
    else
      setenv warmstart .true.
    endif

########################################################################
    if ( \$warmstart == .false. ) then
      rm -f \$workDir/INPUT/ice_model.res.nc
      if (${res} == "96o1") then
        rm -f \$workDir/INPUT/river.res.*.nc
      endif
    endif

    cd \$workDir
    setenv months  \$monthslist[\$irun]
    setenv days    \$dayslist[\$irun]
    setenv hours   \$hourslist[\$irun]

########################################################################
# generate input.nml
########################################################################
    echo \$input_nml
    source \$input_nml
    source \$sit_nml
    source \$woa_nml

    echo loop_\$irun/\$segmentsPerJob
    echo run

########################################################################
# run the model
########################################################################

#   aprun -n \${npes} -N \$mpi_tasks_per_node -d \${OMP_NUM_THREADS} ./\$executable:t |& tee fms.out
echo    aprun -n \${npes} -d \$spread                                  ./\$executable:t |& tee fms.out
    aprun -n \${npes} -d \$spread                                  ./\$executable:t |& tee fms.out

    if ( \$status != 0 ) then
      echo "ERROR: in mpirun: run \$ireload, loop \$irun"
      exit 1
    endif

########################################################################
# generate date for file names
########################################################################
 
    set begindate = \`time_stamp.csh -bf digital\`
    if ( \$begindate == "" ) set begindate = tmp\`date '+%j%H%M%S'\`

    set enddate = \`time_stamp.csh -ef digital\`
    if ( \$enddate == "" ) set enddate = tmp\`date '+%j%H%M%S'\`
    set fyear = \`echo \$enddate | cut -c -4\`

    cd \$workDir
    cat time_stamp.out
#   if ( -f time_stamp.out ) rm -f time_stamp.out

########################################################################
# save ascii output files
########################################################################

    if ( ! -d \$outputDir/ascii ) mkdir \$outputDir/ascii
    if ( ! -d \$outputDir/ascii ) then
     echo "ERROR: \$outputDir/ascii is not a directory."
     exit 1
    endif

    foreach out (\`ls *.out *.results\`)
      mv \$out \$begindate.\$out
    end

    tar cvf - *\.out *\.results | gzip -c > \$outputDir/ascii/\$begindate.ascii_out.tgz

    cd \${stdoutDir}
    echo msub -v base=\$sourceDir,source=\$outputDir/ascii/\$begindate.ascii_out.tgz,destination=gfdl:\$gfdl_archive/ascii/\$begindate.ascii_out.tgz,extension=null,type=ascii,stdoutDir=\${stdoutDir} \$send_file
    msub -v base=\$sourceDir,source=\$outputDir/ascii/\$begindate.ascii_out.tgz,destination=gfdl:\$gfdl_archive/ascii/\$begindate.ascii_out.tgz,extension=null,type=ascii,stdoutDir=\${stdoutDir} \$send_file
    cd \$workDir

    rm -rf *\.out *\.results

########################################################################
# move restart files 
########################################################################

    cd \$workDir 

    if ( ! -d \$outputDir/restart ) mkdir -p \$outputDir/restart
    if ( ! -d \$outputDir/restart ) then
      echo "ERROR: \$outputDir/restart is not a directory."
      exit 1
    endif

    find \$workDir/RESTART -iname '*.res*' > file.restart.list.txt
    set resfiles     = \`wc -l file.restart.list.txt | awk '{print \$1}'\`
    set dateDir      = \$outputDir/restart/\$enddate
    set dateDir_ltfs = \$outputDir_ltfs/restart/\$enddate

    if ( \$irun < \$segmentsPerJob ) then
      set patternGrepRestarts = '\<res\>'
      set patternGrepNC = '\<nc\>'
      set patternGrepResNC = '(res|nc)'
      echo find \$workDir/INPUT   -maxdepth 1 -type f | egrep "\$patternGrepRestarts" | xargs -r rm -f
      find \$workDir/INPUT   -maxdepth 1 -type f | egrep "\$patternGrepRestarts" | xargs -r rm -f
      if ( 1 == 0 ) then
        echo find \$workDir/RESTART -maxdepth 1 -type f | egrep "\$patternGrepRestarts" | xargs -r cp -f -t \$workDir/INPUT
        find \$workDir/RESTART -maxdepth 1 -type f | egrep "\$patternGrepRestarts" | xargs -r cp -f -t \$workDir/INPUT
        echo find \$workDir/RESTART -maxdepth 1 -type f | egrep "\$patternGrepNC"       | xargs -r cp -f -t \$workDir/INPUT
        find \$workDir/RESTART -maxdepth 1 -type f | egrep "\$patternGrepNC"       | xargs -r cp -f -t \$workDir/INPUT
      else
        echo find \$workDir/RESTART -maxdepth 1 -type f | egrep -w "\$patternGrepResNC"       | xargs -r cp -f -t \$workDir/INPUT
        find \$workDir/RESTART -maxdepth 1 -type f | egrep -w "\$patternGrepResNC"       | xargs -r cp -f -t \$workDir/INPUT
      endif
    endif

    if ( \$resfiles > 0 ) then

      set dateDir      = \$outputDir/restart/\$enddate
      set dateDir_ltfs = \$outputDir_ltfs/restart/\$enddate
      if ( ! -d \$dateDir ) mkdir \$dateDir
      if ( ! -d \$dateDir ) then
        echo "ERROR: \$dateDir is not a directory."
        exit 1
      endif

      set restart_file = \$dateDir
      set list = \`ls -C1 \$workDir/RESTART\`
      foreach index (\$list)
        mv \$workDir/RESTART/\$index \$restart_file/\$index
      end

      cd \${stdoutDir}
      msub -v base=\$sourceDir,source=\$outputDir/restart/\$enddate,destination=gfdl:\$gfdl_archive/restart/\$enddate,extension=tar,type=restart,stdoutDir=\${stdoutDir} \$send_file
      cd \$workDir

    endif

########################################################################
# move history files
########################################################################

    cd \$workDir
    
    if ( ! -d \$outputDir/history ) mkdir -p \$outputDir/history
    if ( ! -d \$outputDir/history ) then
      echo "ERROR: \$outputDir/history is not a directory."
      exit 1
    endif

    set dateDir = \$outputDir/history/\$begindate 
    if ( ! -d  \$dateDir ) mkdir \$dateDir
    if ( ! -d  \$dateDir ) then
      echo "ERROR: \$dateDir is not a directory."
      exit 1
    endif

    find \$workDir -type f -maxdepth 1 -regex '.*.nc'      -exec mv {} \$dateDir \;
    find \$workDir -type f -maxdepth 1 -regex '.*.nc.....' -exec mv {} \$dateDir \;

    cd \${stdoutDir}
    msub -v base=\$sourceDir,source=\$outputDir/history/\$begindate,destination=gfdl:\$gfdl_archive/history/\$begindate,extension=tar,type=history,stdoutDir=\${stdoutDir} \$send_file
    cd \$workDir
 
########################################################################
# terminate script if mpirun crashed
########################################################################
    
    cd \$workDir

    if ( \$?MPI_FAIL ) then
      unset echo
      echo "ERROR: in mpirun, core dumped: run \$ireload, loop \$irun" 
      echo "ERROR: Any output that may have been generated " 
      echo "       is in \$outputDir" 
      exit 1
    endif

########################################################################
# terminate script if there are no restart files
########################################################################

    if ( \$resfiles == 0 ) then
      unset echo
      echo "ERROR: no restart files exist, run \$ireload, loop \$irun" 
      exit 1
    endif

########################################################################
# write new reload information
########################################################################
    
    @ irun++

    if ( -f \$reload_file ) then
      mv -f \$reload_file \${reload_file}_prev
    endif
    if ( \$irun <= \$segmentsPerJob ) then
      echo "set irun          =  \$irun"               >  \$reload_file
    else
      @ ireload++
      echo "set irun          =  1"                   >  \$reload_file
    endif
    echo   "set ireload       =  \$ireload"            >> \$reload_file
    echo   "set initCond      =  \$restart_file"       >> \$reload_file
    echo   "set fyear         =  \$fyear"              >> \$reload_file

  end

########################################################################
# loop over \$segmentsPerJob ended
########################################################################

########################################################################
# reload script if necessary
########################################################################

  if ( \$ireload <= \$numSubmissions ) then

    if ( -f \$scriptFullName ) then
      cd \$scriptFullName:h
      if ( 1 == 0 ) then
        msub \$scriptFullName
        #msub -q persistent \$scriptFullName
        # \$scriptFullName # only for dual-running for c2?      cd \$workDir
      else if ( 1 == 1 ) then
        msub ${CWD}/runs/\${scriptName}
      endif
    else
      unset echo
      echo "WARNING: reload script does not exist, run \$ireload, loop \$irun" 
      echo "         \$scriptFullName could not be resubmitted" 
    endif

  endif

unset echo
set beforewaittime = \`date "+%s"\`
set ttlnowait = \`echo "\$beforewaittime - \$beginruntime" | bc -l\`
echo "beginruntime \$beginruntime beforewaittime \$beforewaittime ttlnowait \$ttlnowait"
wait
set endruntime = \`date "+%s"\`
set ttlruntime = \`echo "\$endruntime - \$beginruntime" | bc -l\`
echo "beginruntime \$beginruntime endruntime \$endruntime ttlruntime \$ttlruntime"
echo end_of_run
echo "NOTE: Natural end-of-script for \$scriptFullName."
EOF

echo ${CWD}/runs/${scriptName}
if ( ${batch} == "msub" ) then
  msub ${CWD}/runs/${scriptName}
else if ( ${batch} == "interactive" ) then
  chmod 755  ${CWD}/runs/${scriptName}
  cd ${CWD}/runs/
  ./${scriptName}
else if ( ${batch} == "not_run" ) then
  chmod 755  ${CWD}/runs/${scriptName}
  echo creating ${CWD}/runs/${scriptName}, but not run.
else
  chmod 755  ${CWD}/runs/${scriptName}
  echo creating ${CWD}/runs/${scriptName}, but not run.
  echo invalid option for batch=${batch}.
endif

done:
  exit 0
err:
  exit 1
