#!/bin/csh -fv 
set FILES=`ls -d -C1 *`
foreach file ( ${FILES} )
  if ( 1 == 1 ) then
    send_file -iq $file
  else
    msub -q rdtn -v file=$file `which send_file`
  endif
end

