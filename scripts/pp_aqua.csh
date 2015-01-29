#!/bin/csh -f

if ( 1 == 0 ) then
  set exp = aqua-planet-c90
  set hisDir = /archive/Lucas.Harris/Aquaplanet/Kerr-aqua-planet-c90/history/
  set start_time = 1984
  set end_time   = 1986 
  set xml_file = aqua-planet-c90.xml
else
  set exp = aqua-planet-c90-sit2nnnnnn_v0.202
  set hisDir = $ARCHIVE/Aquaplanet/Kerr-aqua-planet-c90-tune-p6-sit2nnnnnn_v0.202_r1/history/
  set start_time = 1980
  set end_time   = 1987 
  set xml_file = aqua-planet-c90-sit.xml
endif



set chunck_end_time = $start_time


while ( $chunck_end_time <= $end_time )

  echo "frepp -l -V -s -x $xml_file -P gfdl.ncrc2-intel -T prod-openmp -t $chunck_end_time -c atmos -d $hisDir $exp"
  if ( 1 == 0 ) then
    frepp -l -V -s -x $xml_file -P gfdl.ncrc2-intel -T prod-openmp -t $chunck_end_time -c atmos -d $hisDir/ $exp
  else
## for ANALYSIS only
    frepp -A -l -V -s -x $xml_file -P gfdl.ncrc2-intel -T prod-openmp -t $chunck_end_time -c atmos -d $hisDir/ $exp
  endif

  set chunck_end_time = `expr $chunck_end_time + 1`

end
 

