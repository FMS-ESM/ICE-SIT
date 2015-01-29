#!/bin/csh

########################################################################
# generic run script for sending files /$PBS_JOBID_send_data.er
########################################################################

#PBS -N Send_file
####PBS -N ${stdoutDir}/Send_file
### bjt >>
#PBS -l walltime=16:00:00
####PBS -l walltime=06:00:00
### << bjt
#PBS -q ldtn
###PBS -q eslogin
#PBS- A gfdl_w
#PBS -l partition=es
#PBS -l size=1

#################################################################
# set environment
#################################################################

  set echo
  module load fre

  setenv siteConfig $base/env.cshrc
  if ( -f $siteConfig ) source $siteConfig

########################################################################
# determine extension of file to be sent 
########################################################################

  if ( $extension == tar ) then
    cd $source
    
    #~Lucas.Harris/bin/combine-land-restarts.csh .

    if ( $type == restart ) then
      #Do not combine restarts since if the combine
      #runs while a simulation is trying to read
      #the restart files it will crash
      #~Lucas.Harris/bin/filemaster2.csh -n .
      find -iname '*res*' >! file.list.txt
      find -iname '*BCfile*' >> file.list.txt
    else
      ~Lucas.Harris/bin/filemaster2.csh -n .

#If a nested-grid file exists, rename it. Be sure not to overwrite an existing nested file
      foreach f ( *.tile1.nc )
	set base=${f:r:r}
        if ( -e ${base}.nc ) then
	    ncdump -c ${base}.nc | grep grid_xt > /dev/null
	    if ( $status == 0 && ! -e ${base}_nest.nc ) then
		mv ${base}.nc ${base}_nest.nc
	    endif
	endif
      end

      set prefix=$source:t
      ~Lucas.Harris/bin/rename 's/^/'$prefix'./' [^0-9]*.nc*
      find -iname '*.nc*' >! file.list.txt
    endif
    set files = `wc -l file.list.txt | awk '{print $1}'`

    if ( $files > 0 ) then
      tar -b 1000 -cf $source:t.nc.tar --files-from file.list.txt
      mv $source:t.nc.tar ../$source:t.nc.tar
      rm file.list.txt
    else
       echo "NOTE: End-of-script for send_file for $source and $destination"
    endif

    set source_name      = $source.nc.tar
  else
    set source_name      = $source
  endif

########################################################################
# send the data from $source_name to $destination
########################################################################

  echo "source file      = " $source_name

  if ( 1 == 0 ) then
    send_file -iq $source_name
  else
    msub -v file=$source_name `which send_file`
  endif

  if ( $status == 0 ) then
    if ( $extension == tar ) then
      if ( $source:e != tar ) then
        rm $source_name
      endif
    endif
    echo "NOTE: Natural end-of-script for send_file for $source"
  endif

  exit


