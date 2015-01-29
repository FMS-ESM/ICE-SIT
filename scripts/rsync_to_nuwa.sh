#ACCOUNT=Benjei.Tsuang@gaea
#ACCOUNT=btsuang@117.103.105.59
ACCOUNT=btsuang@117.103.105.58
VER=v0.5
DIR="gaea/tikal/update/${VER}"
if [ 1 -eq 1 ]; then
#  rsync -trv  --delete --exclude=*.o* --exclude=*.bak --exclude=*log --exclude=*err --exclude=*out --exclude=*jpg* --exclude=*png* --exclude=tcrg* --exclude=*.nc --exclude=.git* --exclude=exec* .. ${ACCOUNT}:~/${DIR}
 rsync -truv  --delete --exclude=*.o* --exclude=*.bak --exclude=*log --exclude=*err --exclude=*out --exclude=*jpg* --exclude=*png* --exclude=tcrg* --exclude=*.nc --exclude=.git* --exclude=exec* .. ${ACCOUNT}:~/${DIR}
else
  rsync -truv  --exclude=*.o* --exclude=*.bak --exclude=*log --exclude=*err --exclude=*out --exclude=*jpg* --exclude=*png* --exclude=tcrg* --exclude=*.nc --exclude=.git* --exclude=exec* --exclude=log* .. ${ACCOUNT}:~/${DIR}
fi


