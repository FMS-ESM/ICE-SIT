#!/bin/tcsh

#This was originally from /autofs/na2_home2/Chris.Kerr/preR_preS_c720_prod

#################################################################
# generic compile script for experiments                     
#################################################################

#################################################################
# set environment
#################################################################

  setenv siteConfig ./env.cshrc.compile.csh

  if ( -f $siteConfig ) source $siteConfig
  setenv MAIN_PROGRAM coupler_main.o

#################################################################
# set paths
#################################################################

  set exp        = tikal
  set root       = $HOME/$exp
  set template   = $HOME/$exp/scripts/intel.mk.compile.csh
  set srcname    = src
  set exename    = exec
  set executable = fms_tikal.x
  set cppDefs    = (-Duse_libMPI -Duse_netCDF  -DSPMD -DUSE_LOG_DIAG_FIELD_INFO  -Duse_LARGEFILE -DP_SPLIT) 
  set upDates    = 

#################################################################
# create Makefile
#################################################################

set buildDir = $CPERM/$USER/$exp/$exename
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

  make OPENMP=ON -j 32 -f Makefile

   if ( $status ) then
     unset echo
     echo ERROR: make failed 
     exit 1
   else
     unset echo
     echo NOTE: make succeeded 
     
     echo "Executable at $fsDir/$executable"
   endif

  exit
