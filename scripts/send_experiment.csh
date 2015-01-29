#!/bin/csh -fv 
#PBS -o /lustre/f1/Benjei.Tsuang/tikal/runs/c192_hiram_sit_m2_nnnnnn_v0.4922/work/../stdout/
#PBS -N c192_hiram_sit_m2_nnnnnn_v0.4922
###PBS -l walltime=4:00:00
#PBS -l walltime=16:00:00
#PBS -q batch
#PBS -A gfdl_w
#PBS -l partition=c1:c2
#PBS -l size=864
###PBS -l size=922
###PBS -l size=384
#PBS -m abe
#PBS -j oe
#PBS -l qos=norm
####PBS -l qos=windfall

#################################################################
# set environment
#################################################################

set base = tikal
setenv siteConfig ./env.cshrc.compile.csh
setenv siteConfig $HOME/$base/scripts/env.cshrc 

if ( -f $siteConfig ) source $siteConfig
set lustre_options = '1048576 -1 12'

limit stacksize    unlimited
limit coredumpsize unlimited
 
#set EXPS="c192_dpc_sit_m2_nnnnnn_v0.4922 c192_dpc_sit_m2_0d0d0d_v0.4926 c192_hiram_v0.4922 c192_hiram_sit_m2_0d0d0d_v0.4926 c192_dpc_sit_m2_nn1m1d_v0.4926 c192_dpc_v0.4922 c192_hiram_sit_m2_nn1m1d_v0.4926 c192_hiram_sit_m2_nnnnnn_v0.4922"
#set EXPS="c192_dpc_sit_m2_nnnnnn_v0.4922 c192_dpc_sit_m2_0d0d0d_v0.4926 c192_hiram_v0.4922 c192_hiram_sit_m2_0d0d0d_v0.4926 c192_dpc_sit_m2_nn1m1d_v0.4926 c192_dpc_v0.4922 c192_hiram_sit_m2_nn1m1d_v0.4926"
#set EXPS="Kerr-aqua-planet-c90-tune-p6-sit2nnnnnn_v0.202_r1"
#set EXPS="c192_dpc_sit_m2_5d1d1d_amip_v0.4927"
set EXPS="c192_hiram_sit_m2_5d1d1d_amip_v0.4927"
foreach expname ( ${EXPS} )
  # experiment name:
  # set expname = c192_hiram_sit_m2_nnnnnn_v0.4922
  #set expname = runs/${res}
  # experiment source directory name:
  # home directory:
  set sourceDir = /ncrc/home2/Benjei.Tsuang/tikal/update/v0.4/scripts/runs
  # dir name:
  set dirName = runs/${expname}
  echo runs/${expname}
  # fast scratch:
  set fscratch = /lustre/f1/Benjei.Tsuang/tikal/runs/${expname}
  # long-term scratch:
  set lscratch = /lustre/f1/unswept/Benjei.Tsuang/tikal/runs/${expname}
  # gfdl archive:
  set gfdl_archive = /archive/Benjei.Tsuang/tikal/runs/${expname}
########################################################################
# set experiment initial conditions on short-term scratch                       
########################################################################
  if ( 1 == 0 ) then
    # problem with ###PBS -q eslogin
    set send_file  = ~Lucas.Harris/coupled_siena/scripts/send_file.csh
  else if ( 1 == 0 ) then
    # problem with ###PBS -q eslogin
    set send_file  = ~Lucas.Harris/coupled_siena/scripts/send_file.csh
  else if ( 1 == 1 ) then
    # testing
    # partially OK for ascii_out.tgz, but error for nc.tar
    set send_file  = ~Benjei.Tsuang/tikal/update/v0.4/scripts/send_file_lch_v2.csh
  else if ( 1 == 0 ) then
    # Crash in send_file
    set send_file  = ~Benjei.Tsuang/tikal/update/v0.4/scripts/send_file_lch.csh
  else if ( 1 == 0 ) then
    set send_file  = ~Benjei.Tsuang/tikal/update/v0.4/scripts/send_file_org.csh
  else if ( 1 == 0 ) then
    set send_file  = /autofs/na2_home2/Daniel.Gall/Hiram_pub/scripts/pp.csh
  else if ( 1 == 0 ) then
    set send_file  = ~Benjei.Tsuang/tikal/update/v0.4/scripts/send_file_dg.csh
#sending /lustre/f1/Benjei.Tsuang/tikal/runs/c192_dpc_sit_m2_nn1m1d_v0.4926_test/history/19800104.tar
#Writing to /archive/Benjei.Tsuang/tikal/runs/c192_dpc_sit_m2_nn1m1d_v0.4926_test/history/
#
#Running '/ncrc/usw/gcp/local/opt/gcp/2.2/gcp -cd -v /lustre/f1/Benjei.Tsuang/tikal/runs/c192_dpc_sit_m2_nn1m1d_v0.4926_test/history/19800104.tar gfdl:/archive/Benjei.Tsuang/tikal/runs/c192_dpc_sit_m2_nn1m1d_v0.4926_test/history/'
#Can't locate Err.pm in @INC (@INC contains: /sw/rdtn/perl/5.10.1/centos5.7_gnu4.1.2/lib/5.10.1/x86_64-linux /sw/rdtn/perl/5.10.1/centos5.7_gnu4.1.2/lib/5.10.1 /sw/rdtn/perl/5.10.1/centos5.7_gnu4.1.2/lib/site_perl/5.10.1/x86_64-linux /sw/rdtn/perl/5.10.1/centos5.7_gnu4.1.2/lib/site_perl/5.10.1 .) at /ncrc/usw/gcp/local/opt/gcp/2.2/gcp line 42.
#BEGIN failed--compilation aborted at /ncrc/usw/gcp/local/opt/gcp/2.2/gcp line 42.
#ERROR: in gcp (2)
#
  else if ( 1 == 0 ) then
    set send_file  = ~Benjei.Tsuang/tikal/update/v0.4/scripts/send_file_jhc.csh    
#gcp -v -cd /lustre/f1/Benjei.Tsuang/tikal/runs/c192_hiram_sit_m2_nn1m1d_v0.4926_test/ascii/19800104.ascii_out.tgz gfdl:/archive/Benjei.Tsuang/tikal/runs/c192_hiram_sit_m2_nn1m1d_v0.4926_test/ascii/19800104.ascii_out.tgz
#gcp 2.2.1 on gaea2.ncrc.gov by Benjei.Tsuang at Fri Nov 14 18:32:59 2014
#Unique log session id is f89bf6b5-e679-47ca-ac29-1971e287462e at 2014-11-14Z23:32
#Error: No valid certificate found, cannot continue.
  else if ( 1 == 0 ) then
    set send_file  = ~Benjei.Tsuang/tikal/update/v0.4/scripts/send_file_dg_eslogin.csh
  else if ( 1 == 0 ) then
    set send_file  = ~Benjei.Tsuang/tikal/update/v0.4/scripts/send_file_jhc_eslogin.csh    
#gcp 2.2.1 on gaea6.ncrc.gov by Benjei.Tsuang at Fri Nov 14 20:18:15 2014
#Unique log session id is f1f9467b-f8fb-4e37-bab6-eff226ce9bbe at 2014-11-15Z01:18
#Error: No valid certificate found, cannot continue.
  endif

########################################################################
# default directories
########################################################################

  set workDir        = $fscratch/work
# script name:
  set scriptFullName = /lustre/f1/Benjei.Tsuang/tikal/runs/${expname}/work/runscript_${expname}.csh
  set stdoutDir      = /lustre/f1/Benjei.Tsuang/tikal/runs/${expname}/work/../stdout
  set outputDir      = $fscratch
  set outputDir_ltfs = $lscratch
  set TYPES="ascii restart history"
  foreach type ( ${TYPES} )
    cd ${stdoutDir}/../${type}
    if ( ${type} == ascii ) then
      if ( 1 == 0 ) then
        set FILES=`ls -d -C1 2000*.tgz`
      else
        set FILES=`ls -d -C1 *.tgz`
      endif
      cd ${stdoutDir}
      foreach file ( ${FILES} )    
        echo msub -v base=$sourceDir,source=$outputDir/ascii/$file,destination=gfdl:$gfdl_archive/ascii/$file,extension=null,type=ascii,stdoutDir=${stdoutDir} $send_file
        msub -v base=$sourceDir,source=$outputDir/ascii/$file,destination=gfdl:$gfdl_archive/ascii/$file,extension=null,type=ascii,stdoutDir=${stdoutDir} $send_file
      end
    else
      if ( 1 == 0 ) then
        set YYYYMMDDs=`ls -d -C1 2000*01`
      else
        set YYYYMMDDs=`ls -d -C1 [0-9]*01`
      endif
      cd ${stdoutDir}
      foreach date ( $YYYYMMDDs )
        echo msub -v base=$sourceDir,source=$outputDir/${type}/$date,destination=gfdl:$gfdl_archive/${type}/$date,extension=tar,type=${type},stdoutDir=${stdoutDir} $send_file
        msub -v base=$sourceDir,source=$outputDir/${type}/$date,destination=gfdl:$gfdl_archive/${type}/$date,extension=tar,type=${type},stdoutDir=${stdoutDir} $send_file
      end
    endif
  end
end

########################################################################
# loop over $segmentsPerJob ended
########################################################################

unset echo
set beforewaittime = `date "+%s"`
echo end_of_run
echo "NOTE: Natural end-of-script for $scriptFullName."
