# Platform-Specific Environment Setting File
# ------------------------------------------------------------------------------
# The script created at 2013-12-08T14:09:41 via:
# /ncrc/home2/fms/local/opt/fre-commands/bronx-4/bin/fremake --link --ncores=8 --platform=ncrc2.intel --target=openmp --walltime=120 --xmlfile=/autofs/na1_home1/Lucas.Harris/hiram_tikal/xml/awg_rts.xml awgp2
# ------------------------------------------------------------------------------

setenv MAKEFLAGS --jobs=16

source /opt/modules/3.2.6.6/init/tcsh

        source $MODULESHOME/init/csh
 
        module use -a /ncrc/home2/fms/local/modulefiles
        module unload PrgEnv-pgi PrgEnv-pathscale PrgEnv-intel PrgEnv-gnu PrgEnv-cray
        module unload netcdf fre
        module load PrgEnv-intel/4.0.46
        module swap intel intel/12.0.5.220
        module load fre/bronx-6
 
        setenv KMP_STACKSIZE 512m
        setenv NC_BLKSZ 1M
        setenv F_UFMTENDIAN big
      
module list
setenv PATH /ncrc/home2/fms/local/opt/fre-commands/bronx-4/bin:$PATH

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

