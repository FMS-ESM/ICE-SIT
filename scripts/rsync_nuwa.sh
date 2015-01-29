#ACCOUNT=Benjei.Tsuang@gaea
#ACCOUNT=btsuang@117.103.105.58
ACCOUNT=btsuang@117.103.105.59
VER=v0.5
DIR="tikal/update/${VER}"
if [ 1 -eq  0 ]; then
  rsync -truv  --delete --exclude=*.o* --exclude=*.bak --exclude=*log --exclude=*err --exclude=*out --exclude=*jpg* --exclude=*png* --exclude=tcrg* --exclude=*.nc --exclude=.git* --exclude=exec* ${ACCOUNT}:~/gaea/${DIR} ../..
elif [ 1 -eq 1 ]; then
  rsync -truv  --exclude=*log* --exclude=*.o* --exclude=*.bak --exclude=*log --exclude=*err --exclude=*out --exclude=*jpg* --exclude=*png* --exclude=tcrg* --exclude=*.nc --exclude=.git* --exclude=exec* ${ACCOUNT}:~/gaea/${DIR} ../..
fi


