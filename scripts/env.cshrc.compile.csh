# Platform-Specific Environment Setting File
# ------------------------------------------------------------------------------
# The script created at 2012-03-06T16:21:31 via:
# /ncrc/home2/fms/local/opt/fre-commands/bronx-1/bin/fremake --link --ncores=8 --platform=ncrc2.intel --target=debug --walltime=120 --xmlfile=/autofs/na2_home2/Zhi.Liang/project/xgrid/xml/AM3.xml
# ------------------------------------------------------------------------------

#       module use -a /ncrc/home2/fms/local/modulefiles
#       module use -a /usw/gcp/local/modulefiles

source /opt/modules/3.2.6.6/init/tcsh
source $MODULESHOME/init/csh

module use /autofs/na1_home1/Tara.McQueen/local/modulefiles
module unload PrgEnv-pgi PrgEnv-pathscale PrgEnv-intel PrgEnv-gnu PrgEnv-cray
module unload netcdf fre fre-commands ncview
module load PrgEnv-intel
module load fre #debug
#module swap intel intel/12.0.5.220
#module load fre/bronx-1
module unload gcp
module load gcp/1.5.3
module list

setenv MAKEFLAGS --jobs=16
setenv F_UFMTENDIAN big
setenv KMP_STACKSIZE 2g
setenv MPICH_ENV_DISPLAY 
#setenv MPICH_CPUMASK_DISPLAY
setenv MPICH_GNI_LOCAL_CQ_SIZE  131072
      
module list
setenv PATH /ncrc/home2/fms/local/opt/fre-commands/bronx-1/bin:$PATH

if ( $?PBS_ENVIRONMENT ) then
  if ( $PBS_ENVIRONMENT == "PBS_BATCH" ) then
    set aliasMake = `alias make`
    if ( $aliasMake != "" ) then
      alias make $aliasMake VERBOSE=on
    else
      alias make make VERBOSE=on
    endif
    unset aliasMake
  endif
endif

