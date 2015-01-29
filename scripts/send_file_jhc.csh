#!/bin/csh -fv

########################################################################
# generic run script for sending files /$PBS_JOBID_send_data.er
########################################################################

####PBS -o /autofs/na1_home1/Jan-Huey.Chen/Hiram_C384L55/scripts/stdout/
####PBS -e /autofs/na1_home1/Jan-Huey.Chen/Hiram_C384L55/scripts/stdout/
#PBS -N Send_file
#PBS -l walltime=14:00:00
#PBS -r y
#PBS -q rdtn
#PBS -l partition=es 
#PBS -A gfdl_w
#PBS -l size=1
#PBS -m a
#PBS -j oe

#################################################################
# set environment
#################################################################

  set echo

  setenv siteConfig $base/env.cshrc
  if ( -f $siteConfig ) source $siteConfig

########################################################################
# determine extension of file to be sent 
########################################################################

  if ( $extension == tar ) then
    cd $source
    if ( $type == restart ) then
      find -iname '*res*' > file.list.txt
    else
      find -iname '*.nc*' > file.list.txt
    endif
    set files = `wc -l file.list.txt | awk '{print $1}'`

    if ( $files > 0 ) then
      tar -b 1000 -cf $source:t.tar --files-from file.list.txt
      if ( $type == history ) then
        mv $source:t.tar ../$source:t.nc.tar
      else
        mv $source:t.tar ../$source:t.tar
      endif
      rm file.list.txt
    else
       echo "NOTE: End-of-script for send_file for $source and $destination"
    endif

    if ( $type == history ) then
      set source_name      = $source.nc.tar
    else
      set source_name      = $source.tar
    endif
    set destination_name = $destination:h/
  else
    set source_name      = $source
    set destination_name = $destination
  endif

  set staged_name = `echo $source_name | sed "s/lustre\/fs\/scratch/lustre\/ltfs\/stage/g"`

########################################################################
# send the data from $source_name to $destination
########################################################################

  echo "source file      = " $source_name
  echo "staged file      = " $staged_name
  echo "destination file = " $destination_name

  if ( ! -d $staged_name:h ) mkdir -p $staged_name:h
  gcp -v -cd ${source_name} ${staged_name}
  cd $staged_name:h
#  gcp -v -cd ${staged_name} ${tag}${destination_name}
  gcp -v -cd ${staged_name} ${destination_name}

#  gcp -v -cd $source_name $destination_name

  if ( $status == 0 ) then
    rm $staged_name
    if ( $extension == tar ) then
      if ( $source:e != tar ) then
        rm $source_name
        echo "NOTE: Natural end-of-script for send_file for $source and $destination"
      endif
    endif
  endif

  exit


