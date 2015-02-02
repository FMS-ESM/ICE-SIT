VER=10.4
if [ 1 -eq 0 ]; then
###rsync -truv  --del --exclude=*.o* --exclude=*.bak --exclude=*log --exclude=*err --exclude=*out --exclude=*jpg* --exclude=*png* --exclude=tcrg* --exclude=*.nc . j40bjt00@140.110.122.63:/u1/u40bjt00/MPI/echam-5.4.00sit3/mine/v${VER}
  rsync --delete -truv  --exclude=*.o* --exclude=*.bak --exclude=*log --exclude=*err --exclude=*out --exclude=*jpg* --exclude=*png* --exclude=tcrg* --exclude=*.nc . j40bjt00@alps.nchc.org.tw:/home/u40bjt00/ehtw/mine/v${VER}
else
#  rsync -truv  --exclude=*.o* --exclude=*.bak --exclude=*log --exclude=*err --exclude=*out --exclude=*jpg* --exclude=*png* --exclude=tcrg* --exclude=10* --exclude=.git* Benjei.Tsuang@gaea.rdhpcs.noaa.gov:/autofs/na2_home2/Benjei.Tsuang/codes/Aquaplanet/dpmcube-merlis-sit/src-prodrun .
#  scp -v -r -p Benjei.Tsuang@gaea.rdhpcs.noaa.gov:/autofs/na2_home2/Benjei.Tsuang/codes/Aquaplanet/dpmcube-merlis-sit/src-prodrun ..
  scp -v -r -p Benjei.Tsuang@gaea.rdhpcs.noaa.gov:/autofs/na2_home2/Benjei.Tsuang/codes/Aquaplanet/dpmcube-merlis-sit/src-prodrun test
fi
