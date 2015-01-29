#!/bin/tcsh -f
#FRE scheduler-options
#PBS -o /autofs/na2_home2/Benjei.Tsuang/hiram_201408/prod/exec-prodrun
#PBS -N compile_hiram.csh
#PBS -l walltime=7200
#PBS -r y
#PBS -q eslogin
#PBS -l partition=gaea:es
#PBS -l size=1
#PBS -m a
#PBS -j oe

# Compile Script for Experiment 'hiram'
# ------------------------------------------------------------------------------
# The script created at 2011-12-28T13:35:03 via:
# /ncrc/home2/fms/local/opt/fre-commands/arkansas-15/bin/fremake --ncores=8 --platform=ncrc.default --target=prod --walltime=120 --xmlfile=/autofs/na1_home1/Timothy.Merlis/idealized_siena_new.xml
# ------------------------------------------------------------------------------
if ( 1 == 1 ) then
  setenv MAIN_PROGRAM coupler_main.o
#  touch ../coupler_main.F90
#   touch ../interpolator.F90
# touch ../aerosol.F90
# touch ../woa.F90
  if ( 1 == 1 ) then
    set ver=v0.5
    set minor="0"
    set upDates    = ../update/${ver}
    set executable = fms_tikal_sit_${ver}${minor}.x
  else if ( 1 == 1 ) then
    set ver=""
    set minor=""
    set upDates    = 
    set executable = fms_tikal.x
  else
    set ver=""
    set minor=""
    set upDates    = 
    set executable = fms_tikal.x
  endif
  set cppDefs    = (-Duse_libMPI -Duse_netCDF  -DSPMD -DUSE_LOG_DIAG_FIELD_INFO  -Duse_LARGEFILE -DP_SPLIT -DHYDRO_DELZ_REMAP -Dprogram_coupler_main) 
else if ( 1 == 0 ) then
  touch ../coupler_main.F90
  touch ../aerosol.F90
  setenv MAIN_PROGRAM aerosol.o
  set ver=v0.5
  set minor="0"
  set upDates    = ../update/${ver}
  set executable = aerosol_sit_${ver}${minor}.x
  set cppDefs    = (-Duse_libMPI -Duse_netCDF  -DSPMD -DUSE_LOG_DIAG_FIELD_INFO  -Duse_LARGEFILE -DP_SPLIT -DHYDRO_DELZ_REMAP -Dtest_aerosol ) 
else if ( 1 == 1 ) then
# touch ../coupler_main.F90
# touch ../aerosol.F90
# touch ../woa.F90
# touch ../interpolator.F90
  setenv MAIN_PROGRAM woa.o
  set ver=v0.5
  set minor="0"
  set upDates    = ../update/${ver}
  set executable = woa_sit_${ver}${minor}.x
  set cppDefs    = (-Duse_libMPI -Duse_netCDF  -DSPMD -DUSE_LOG_DIAG_FIELD_INFO  -Duse_LARGEFILE -DP_SPLIT -DHYDRO_DELZ_REMAP -Dtest_woa ) 
else if ( 1 == 1 ) then
#  touch ../coupler_main.F90
# touch ../aerosol.F90
# touch ../woa.F90
  touch ../interpolator.F90
  setenv MAIN_PROGRAM interpolator.o
  set ver=v0.5
  set minor="0"
  set upDates    = ../update/${ver}
  set executable = interpolator_${ver}${minor}.x
  set cppDefs    = (-Duse_libMPI -Duse_netCDF  -DSPMD -DUSE_LOG_DIAG_FIELD_INFO  -Duse_LARGEFILE -DP_SPLIT -DHYDRO_DELZ_REMAP -Dtest_interp ) 
endif
  
echo Starting on `date`
echo $HOST $HOSTNAME
set echo
unalias *

#This was originally from /autofs/na2_home2/Chris.Kerr/preR_preS_c720_prod

#################################################################
# generic compile script for experiments                     
#################################################################

#################################################################
# set environment
#################################################################

  setenv siteConfig ./env.cshrc.compile.csh

  if ( -f $siteConfig ) source $siteConfig
#################################################################
# set paths
#################################################################

  set exp        = tikal
  set root       = $HOME/$exp
  set template   = $HOME/$exp/update/${ver}/scripts/intel.mk.compile.csh
  set srcname    = src-lin_201407a_lmh
##  set exename    = exec-lin_201407a_lmh
##  set executable = fms_tikal_sit_${ver}${minor}.x
#  set cppDefs    = (-Duse_libMPI -Duse_netCDF  -DSPMD -DUSE_LOG_DIAG_FIELD_INFO  -Duse_LARGEFILE -DP_SPLIT -DHYDRO_DELZ_REMAP ) 

#################################################################
# create Makefile
#################################################################

set buildDir = $CPERM/$USER/$exp/$ver
#set buildDir = $CPERM/$USER/$exp/$exename
set fsDir    = $buildDir
set srcDir   = $root/$srcname

mkdir -p $buildDir $fsDir

 cd $srcDir

  \rm pathnames
  list_paths -o pathnames 

  cd $buildDir

 mkmf -a $srcDir -m Makefile -t $template -p $executable -c "$cppDefs" $upDates \
              pathnames shared/mpp/include shared/include

#################################################################
# call the main Makefile
#################################################################

#  make DEBUG=ON -j 32 -f Makefile
  make OPENMP=ON -j 32 -f Makefile

   if ( $status ) then
     unset echo
     echo ERROR: make failed 
     exit 1
   else
     unset echo
     echo NOTE: make succeeded 
     
     echo "Executable at $fsDir/$executable"
     # tar -zcvf $srcDir/$upDates/hiram_sit_${ver}${minor}.tar.gz $srcDir/$upDates/*.F90
     tar -zcvf $srcDir/$upDates/${executable}.tar.gz $srcDir/$upDates/*.F90
   endif

  exit
