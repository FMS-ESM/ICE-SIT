#!/bin/csh -f

set exp = aqua-planet-c90

set start_time = 1980
set end_time   = 1980 
#set end_time   = 1986 

set xml_file = aqua-planet-c90.xml

set chunck_end_time = $start_time

# set hisDir = $ARCHIVE/Aquaplanet/Kerr-aqua-planet-c90-tune-p6-sit2nnnnnn_v0.2_r12/history/
set hisDir = $ARCHIVE/Aquaplanet/Kerr-aqua-planet-c90-tune-p6-sit2nnnnnn_v0.202_r1/history/

while ( $chunck_end_time <= $end_time )

  echo "frepp -l -V -s -x $xml_file -P gfdl.ncrc2-intel -T prod-openmp -t $chunck_end_time -c atmos -d $hisDir $exp"
 
  if ( 1 == 1 ) then
    frepp -l -V -s -x $xml_file -P gfdl.ncrc2-intel -T prod-openmp -t $chunck_end_time -c atmos -d $hisDir/ $exp
  else if ( 1 == 1 ) then
    ## for ANALYSIS only
    frepp -A -l -V -s -x $xml_file -P gfdl.ncrc2-intel -T prod-openmp -t $chunck_end_time -c atmos -d $hisDir $exp
  endif

  set chunck_end_time = `expr $chunck_end_time + 1`

end
 

