#!/bin/csh
########################################################################
# generic run script for sending files /$PBS_JOBID_send_data.er
########################################################################
###PBS -o /autofs/na1_home1/Jan-Huey.Chen/Hiram_pub/scripts/stdout/
###PBS -d /autofs/na1_home1/Jan-Huey.Chen/Hiram_pub/scripts/stdout
#PBS -r y
#PBS -N combine
#PBS -l walltime=14:00:00
#PBS -q ldtn
####PBS -q eslogin
###PBS- A gfdl_s
#PBS- A gfdl_w
#PBS -l partition=es
#PBS -l size=1
#PBS -m a
#PBS -j oe
#################################################################
# set environment
#################################################################

set echo

#setenv siteConfig $base/env.cshrc
#if ( -f $siteConfig ) source $siteConfig

source $MODULESHOME/init/csh

module use -a /ncrc/home2/fms/local/modulefiles
module unload netcdf fre fre-commands
module load fre-nctools/bronx-6

# Check if ncdump is in PATH
#which ncdump >& /dev/null
#if ( $status != 0 ) then
#    module load netcdf
#    which ncdump >& /dev/null
#    if ( $status != 0 ) then
#        echo "Unable to find ncdump in PATH."
#	exit 1
#    endif
#endif

# Check if is-compressed is in PATH
which is-compressed >& /dev/null
if ( $status != 0 ) then
    module load fre-nctools/bronx-6
    which is-compressed >& /dev/null
    if ( $status != 0 ) then
        echo "Unable to find is-compressed in PATH"
        exit 1
    endif
endif

# Check if gcp is in PATH
which gcp >& /dev/null
if ( $status != 0 ) then
    module load gcp
    which gcp >& /dev/null
    if ( $status != 0 ) then
        echo "Unable to find gcp in PATH."
	exit 1
    endif
endif

hostname | grep -q batch

if ($status == 0) then
    echo "Running mppnccombine on the batch nodes is not allowed"
    exit 1
endif

 #set echo


limit stacksize unlimited
 #Csh NEEDS the quotes around the variable to do proper string comparison
    #(hence 'C shell considered harmful')

if ( 1 == 0 ) then
  set send_file  = $base/send_file.csh
else
  set send_file  = /ncrc/home2/fms/local/opt/fre-commands/bronx-7/site/ncrc/bin/send_file
endif

set found_res_switch = 0
set finer_steps = 0
set nointerp = 0
set gridspec_and_month_only = 0
set nlon = 360
set nlat = 180
while ($#argv > 0)
  echo $argv[1]
  switch ($argv[1])
    case "-l":
	echo "Original low resolution"
	set nlon = 240
	set nlat = 120
	set finer_steps = 1
	set found_res_switch = 1
	shift
	breaksw
    case "-h":
	echo "New higher resolution"
	set nlon = 480
	set nlat = 240
	set finer_steps = 1
        set found_res_switch = 1
	shift
	breaksw
    case "-p":
	echo "Production run high resolution"
	set nlon = 1440
	set nlat = 720
	set finer_steps = 0
        set found_res_switch = 1
	shift
	breaksw
    case "-low":
	echo "2 deg x 2 deg"
	set nlon = 180
	set nlat = 90
        set found_res_switch = 1
	shift
	breaksw
    case "-high":
	echo "0.5 deg x 0.5 deg"
	set nlon = 720
	set nlat = 360
        set found_res_switch = 1
	shift
	breaksw
    case "-ultra":
	echo "0.25 deg x 0.25 deg"
	set nlon = 1440
	set nlat = 720
        set found_res_switch = 1
	shift
	breaksw
    case "-n":
	echo "No interpolation to lat-lon grid"
	set nointerp=1
	shift
	breaksw
    case "-g":
	echo "Only processing grid_spec and atmos_month files"
	set gridspec_and_month_only = 1
	shift
	breaksw
    default:
	break #break while
  endsw
end
if ( $found_res_switch == 0 ) then
    echo "DEFAULT: 1 deg x 1 deg"
endif

 set root=''
 if ( $#argv > 0 ) then
    if ( -d $1 ) then
	cd $1
    else
	set root = $1
    endif
 endif
 if ( $?source ) then
   cd $source
 endif

 set files = (`ls -1 *.nc.???? *.nc.?????? | sed 's/\.nc\.[0-9]*/.nc/' | uniq | tr '\n' ' '`)
 foreach file ( $files )
    if ($gridspec_and_month_only > 0) then
	set filebase = $file:r:r:r
	if (! ( "$filebase" == "grid_spec" || "$filebase" == "atmos_month" || "$filebase" == "atmos_init" ) ) then
	    continue
	endif
    endif
    rm -f $file
    echo "CREATING $1/${file}"
    set first=`ls -1 $file.???? | head -n 1`
    is-compressed $first
    if ( $status == 0 ) then
	combine-ncc ${file}.???? $file 	 &
    else
	mppnccombine -r $file ${file}.???? &
    endif
 end
 wait

 if ($nointerp == 1) then
    exit
 endif

 foreach file ( *.tile1.nc  )
    set filebase = $file:r:r
    if ($filebase == "grid_spec") then
	continue
    endif
    if ($gridspec_and_month_only > 0) then
	if ( "$filebase" != "atmos_month" && "$filebase" != "atmos_init" ) then
	    continue
	endif
    endif
#Don't try to combine land restarts
    is-compressed $file
    if ( $status == 0 ) continue

#If a nested-grid file exists, rename it. Be sure not to overwrite an existing nested file
#    if ( -e ${filebase}.nc ) then
#	ncdump -c ${filebase}.nc | grep grid_xt > /dev/null
#	if ( $status == 0 ) then
#	    mv ${filebase}.nc ${filebase}_nest.nc
#	endif
#    endif

    cat >! c2l.nml <<EOF        
&c2l_nml
nlon  = $nlon
nlat  = $nlat
finer_steps = $finer_steps
write_res = .false.
read_res  = .false..
data_file = "$filebase"
/
EOF
    time ~Lucas.Harris/bin/c2l.x || exit

  #ncatted  -h -a "grid",global,c,c,"interp_latlon" ${base}.nc
  # rename from 'cubed' to 'interp_latlon'
  #mv ${base}.nc `echo ${base}.nc | sed 's/cubed/interp_latlon/'`
 end

###if ( 1 == 0 ) then
if ( $?PBS_JOBID ) then
###  gcp -cd -v $source/* $intermediary/
###  msub -N transfer_history_${name}_${begindate} -q rdtn -v base=$base,source=$intermediary,destination=$destination,extension=$extension,type=$type $send_file
  set files = (`ls *`)
  tar -cvf ${source}.tar ${files}
  ${send_file} ${source}.tar
#  msub -q rdtn -v source=${source}.tar $send_file
#  foreach file ( $files )
#   ${send_file} ${file}
#    msub -q rdtn $send_file ${file}
#  end
  #msub -N transfer_history_${name}_${begindate} -q rdtn -v base=$base,source=$source,destination=$destination,extension=$extension,type=$type $send_file
endif

