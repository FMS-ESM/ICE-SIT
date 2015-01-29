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

#set ver=""
set ver=v0.4
set minor=""

echo Starting on `date`
echo $HOST $HOSTNAME
set echo
unalias *

# ---------------- set environment

### source /autofs/na2_home2/Benjei.Tsuang/hiram_201408/prod/exec-prodrun/env.cshrc
source /autofs/na2_home2/Benjei.Tsuang/hiram_201408/src-lin_201407a_lmh/env.cshrc

# ---------------- write main Makefile
mkdir -p /autofs/na2_home2/Benjei.Tsuang/hiram_201408/prod/exec-prodrun

cat > /autofs/na2_home2/Benjei.Tsuang/hiram_201408/prod/exec-prodrun/Makefile <<END
# Makefile for Experiment 'hiram'
include /autofs/na2_home2/Benjei.Tsuang/hiram_201408/src-lin_201407a_lmh/intel.mk.compile_script_c2.csh
#include /ncrc/home1/Lucas.Harris/Aquaplanet/src-lin_201407a_lmh/intel.mk.compile_script_c2.csh
#include /ncrc/home2/fms/local/opt/fre-commands/arkansas-15/site/ncrc/intel.mk

fms_hiram-prodrun_${ver}${minor}.x: libfms_hiram_201408.a
<TAB>\$(LD) \$^ \$(LDFLAGS) -o \$@ 

libfms_hiram_201408.a:  FORCE
<TAB>make  NETCDF=3  -f Makefile.fms_hiram_201408 \$@

FORCE:

clean:
<TAB>make  NETCDF=3  -f Makefile.fms_hiram_201408 clean

localize:
<TAB>make  NETCDF=3  -f Makefile.fms_hiram_201408 localize

END

# ---------------- create component Makefiles

cd /autofs/na2_home2/Benjei.Tsuang/hiram_201408/src-lin_201407a_lmh
rm -f pathnames_fms_hiram_201408
if ( 1 == 1 ) then
# use new coupler and ice_model
# update in the begining of list_paths
# As a resulti, *.F90 in  ice_amip and coupler are not used.
list_paths -o pathnames_fms_hiram_201408 \
    update/${ver}${minor}  \
    atmos_coupled atmos_cubed_sphere atmos_param atmos_shared ice_amip ice_param \
    land_lad2/ land_param ocean_amip shared coupler
else if ( 0 == 1 ) then
# OK
list_paths -o pathnames_fms_hiram_201408 \
    atmos_coupled atmos_cubed_sphere atmos_param atmos_shared ice_param \
    land_lad2/ land_param ocean_amip shared \
    update/${ver}${minor}  ice_amip
else
# ORIGINAL, OK
list_paths -o pathnames_fms_hiram_201408 \
    atmos_coupled atmos_cubed_sphere atmos_param atmos_shared ice_amip ice_param \
    land_lad2/ land_param ocean_amip shared coupler
endif
cd /autofs/na2_home2/Benjei.Tsuang/hiram_201408/prod/exec-prodrun
mkmf -m Makefile.fms_hiram_201408 -a /autofs/na2_home2/Benjei.Tsuang/hiram_201408/src-lin_201407a_lmh -p libfms_hiram_201408.a -t /autofs/na2_home2/Benjei.Tsuang/hiram_201408/src-lin_201407a_lmh/intel.mk.compile_script_c2.csh -c "-Duse_libMPI -Duse_netCDF -DINTERNAL_FILE_NML -DSPMD -DLAND_GRID_FROM_ATMOS -DLAND_BND_TRACERS" pathnames_fms_hiram_201408 shared/mpp/include shared/include

# ---------------- adjust the main Makefile

cat Makefile | sed -e 's/<TAB>/\t/' > Makefile.$$ && mv -f Makefile.$$ Makefile

# ---------------- call make on the main Makefile

make  NETCDF=3 fms_hiram-prodrun_${ver}${minor}.x

if ( $status ) then
  unset echo
  echo ERROR: make failed for hiram.
  exit 1
else
  unset echo
  echo NOTE: make succeeded for hiram.
endif
