#!/bin/bash
#
# syntax:
#
#         run_template.sh <machine> <queue>    <C/P/F> <EXP>   <SFC_NUDG> <UPP_NUDG> <DEEP_NUDG> <LSICE_NUDG> <RES> <LEV> <ORES> <PUTDATA> <FYEAR> <FMM> <FDD> <FHH> <LYEAR> <LMM> <LDD> <LHH> <ACCOUNT> <LEVENT> <PUTRERUN> <LNUDGE> <NOTRUN> <LCOLD_START>  <LOCN_WARMSTART>  <LFORECAST> <Forecast_Days> <GID>       <EMAIL>                <LARCHIVE> <LMINSIZE>  <LMINOR>  <LNWP>   <LP>    <PW> <EXP_P>  <RFILE_P> <DPATH_P>
#                            ${1}     ${2}     ${3}    ${4}    ${5}       ${6}       ${7}        ${8}         ${9}  ${10} ${11}  ${12}     ${13}  ${14} ${15} ${16}  ${17}  ${18} ${19} ${20}  ${21}     ${22}    ${23}      ${24}    ${25}    ${26}          ${27}             ${28}       ${29}           ${30}       ${31}                  ${32}      ${33}       ${34}     ${35}    ${36}   ${37} ${38}    ${39}     ${40}
#
# LNUDGE=T: nudging a run.
# NOTRUN=T: generaltes *.sh in the working directories without submitting a run.
# LCOLD_START=T: cold start (delete OCN_SV, rerun files)
# LOCN_WARMSTART=T: ocn warm start (using 300-y ocean spinup file as the initial ocean u, v, T and S profiles) 
###############################################
#
run_template() {

declare -i FIMM
declare -i FIDD
declare -i LIMM
declare -i LIDD
### local RFILE_P
### local DPATH_P

CURDIR=`pwd`

VER=10.4
MACHINE="IRISH"
QUEQE_OPTIONS=""
#default
LDEBUG=F
PUTDATA="6, 'hours'"
NMONTH=12
LFORECAST=F
LASIA=T
ACCOUNT=u40bjt00

LEVENT=F
RERUN_INC=1
RERUN_UNIT=months
LRERUN_1m=T
LRERE=F
LNUDGE=F
NDGDATA=1
NOTRUN=F
###LOCN_PREP=F
LCOLD_START=F
LOCN_WARMSTART=F
OCN_WARMSTART_DATA=1
EMAIL=btsuang@gmail.com
LARCHIVE=F
ARCHIVE=ST
LMINSIZE=F
GID=""
LPOST=F
PNETCDF=F
PMEM=""
TRASH=""
LPtile24=F
LPtile48=F
TEMPLATE=v0
LMINOR=F
MINOR=""
#
### ocn_domain_w="-90.,90.,0.,360."
if [ 1 == 1 ]; then
  ocn_domain_w=0.
  ocn_domain_e=360.
  ocn_domain_s=-80.
  ocn_domain_n=80.
elif [ 1 == 0 ]; then
  ocn_domain_w=0.
  ocn_domain_e=360.
  ocn_domain_s=-70.
  ocn_domain_n=70.  
else
  ocn_domain_w=0.
  ocn_domain_e=360.
  ocn_domain_s=-90.
  ocn_domain_n=90.
fi
#
nobox_nudg=0
obox_ndg_restore_time=-9.e33
# number of nudg squares in ocean grids (default = 0, maximun=6)
obox_nudg_flag=0
# 0: no nudging, 1: nudging inside square boxes, 2: nudging outside the square boxes
obox_nudg_w="-999.,-999.,-999.,-999.,-999.,-999."
# west coords (lon) of nudging boxes [-180., 360.](deg). There are 6 boxes. 
obox_nudg_e="-999.,-999.,-999.,-999.,-999.,-999."
# east coords (lon) of nudging boxes [-180., 360.](deg). There are 6 boxes.
obox_nudg_s="-999.,-999.,-999.,-999.,-999.,-999."
# south coords (lat) of nudging boxes [-180., 360.](deg). There are 6 boxes.
obox_nudg_n="-999.,-999.,-999.,-999.,-999.,-999."
# north coords (lat) of nudging boxes [-180., 360.](deg). There are 6 boxes.

if [ -n "${1}" ]; then
  echo "MACHINE" =${1}
  MACHINE=${1}
fi

if [ -n "${2}" ]; then
  echo "QUEUE" =${2}
  if [ ${MACHINE} == "IRISH" ]; then
    HUGE_MEM=F
    WALLTIME_HH=96
    if [ "${2}" == "4cpu" ]; then
      QUEUE=${2}
      NODE=1
      TASKS_PER_NODE=4
      NPROCA=2
      NPROCB=2
      LINFINIBAND=T
      WALLTIME_HH=96
    elif [ "${2}" == "e8cpu" ]; then
      QUEUE=${2}
      NODE=1
      TASKS_PER_NODE=8
      NPROCA=2
      NPROCB=4
      LINFINIBAND=F
      WALLTIME_HH=96
  ###    LDEBUG=T
    elif [ "${2}" == "e8cpu2" ]; then
      QUEUE="e8cpu"
      NODE=1
      TASKS_PER_NODE=8
      NPROCA=8
      NPROCB=1
      LINFINIBAND=F
      WALLTIME_HH=96
  ###    LDEBUG=T
    elif [ "${2}" == "8cpu" ]; then
      QUEUE=${2}
      NODE=2
      TASKS_PER_NODE=4
      NPROCA=2
      NPROCB=4
      LINFINIBAND=T
      WALLTIME_HH=96
    elif [ "${2}" == "16cpu" ]; then
      QUEUE=${2}
      NODE=4
      TASKS_PER_NODE=4
      NPROCA=4
      NPROCB=4
      LINFINIBAND=T
      WALLTIME_HH=96 
    elif [ "${2}" == "32cpu" ]; then
      QUEUE=${2}
      NODE=8
      TASKS_PER_NODE=4
      NPROCA=4
      NPROCB=8
      LINFINIBAND=T
      WALLTIME_HH=96 
    elif [ "${2}" == "e64cpu" ]; then
      QUEUE=${2}
      NODE=8
      TASKS_PER_NODE=8
      NPROCA=8
      NPROCB=8
      LINFINIBAND=F
      WALLTIME_HH=96 
    elif [ "${2}" == "e64cpu16" ]; then
      QUEUE="e64cpu"
      NODE=2
      TASKS_PER_NODE=8
      NPROCA=2
      NPROCB=8
      LINFINIBAND=F
      WALLTIME_HH=96 
    elif [ "${2}" == "e64cpu24" ]; then
      QUEUE="e64cpu"
      NODE=3
      TASKS_PER_NODE=8
      NPROCA=3
      NPROCB=8
      LINFINIBAND=F
      WALLTIME_HH=96 
    elif [ "${2}" == "e64cpu32" ]; then
      QUEUE="e64cpu"
      NODE=4
      TASKS_PER_NODE=8
      NPROCA=4
      NPROCB=8
      LINFINIBAND=F
      WALLTIME_HH=96 
    elif [ "${2}" == "e64cpu40" ]; then
      QUEUE="e64cpu"
      NODE=5
      TASKS_PER_NODE=8
      NPROCA=5
      NPROCB=8
      LINFINIBAND=F
      WALLTIME_HH=96 
    elif [ "${2}" == "e64cpu48" ]; then
      QUEUE="e64cpu"
      NODE=6
      TASKS_PER_NODE=8
      NPROCA=6
      NPROCB=8
      LINFINIBAND=F
      WALLTIME_HH=96 
    elif [ "${2}" == "e64cpu56" ]; then
      QUEUE="e64cpu"
      NODE=7
      TASKS_PER_NODE=8
      NPROCA=7
      NPROCB=8
      LINFINIBAND=F
      WALLTIME_HH=96 
    elif [ "${2}" == "64cpu" ]; then
      QUEUE=${2}
      NODE=16
      TASKS_PER_NODE=4
      NPROCA=8
      NPROCB=8
      LINFINIBAND=T
      WALLTIME_HH=96 
    elif [ "${2}" == "n4cpu" ]; then
      QUEUE=nctfr
      NODE=1
      TASKS_PER_NODE=4
      NPROCA=1
      NPROCB=4
      LINFINIBAND=T
      WALLTIME_HH=96 
    elif [ "${2}" == "n8cpu" ]; then
      QUEUE=nctfr
      NODE=2
      TASKS_PER_NODE=4
      NPROCA=2
      NPROCB=4
      LINFINIBAND=T
      WALLTIME_HH=96 
    elif [ "${2}" == "n12cpu" ]; then
      QUEUE=nctfr
      NODE=3
      TASKS_PER_NODE=4
      NPROCA=3
      NPROCB=4
      LINFINIBAND=T
      WALLTIME_HH=96 
    elif [ "${2}" == "n16cpu" ]; then
      QUEUE=nctfr
      NODE=4
      TASKS_PER_NODE=4
      NPROCA=4
      NPROCB=4
      LINFINIBAND=T
      WALLTIME_HH=96 
    elif [ "${2}" == "n32cpu" ]; then
      QUEUE=nctfr
      NODE=8
      TASKS_PER_NODE=4
      NPROCA=4
      NPROCB=8
      LINFINIBAND=T
      WALLTIME_HH=96 
    elif [ "${2}" == "n64cpu" ]; then
      QUEUE=nctfr
      NODE=16
      TASKS_PER_NODE=4
      NPROCA=8
      NPROCB=8
      LINFINIBAND=T
      WALLTIME_HH=96 
    elif [ "${2}" == "test4" ]; then
      QUEUE=test
      NODE=1
      TASKS_PER_NODE=4
      NPROCA=1
      NPROCB=4
      LINFINIBAND=T
      WALLTIME_HH=1 
      PUTDATA="1, 'steps'"
      LDEBUG=T
    elif [ "${2}" == "test8" ]; then
      QUEUE=test
      NODE=2
      TASKS_PER_NODE=4
      NPROCA=2
      NPROCB=4
      LINFINIBAND=T  
      WALLTIME_HH=1
      PUTDATA="1, 'steps'"
      LDEBUG=T
    elif [ "${2}" == "test12" ]; then
      QUEUE=test
      NODE=3
      TASKS_PER_NODE=4
      NPROCA=3
      NPROCB=4
      LINFINIBAND=T
      WALLTIME_HH=1 
      PUTDATA="1, 'steps'"
      LDEBUG=T
    elif [ "${2}" == "test16" ]; then
      QUEUE=test
      NODE=4
      TASKS_PER_NODE=4
      NPROCA=4
      NPROCB=4
      LINFINIBAND=T
      WALLTIME_HH=1  
      PUTDATA="1, 'steps'"
      LDEBUG=T
    elif [ "${2}" == "test" ]; then
      QUEUE=${2}
      NODE=8
      TASKS_PER_NODE=4
      NPROCA=4
      NPROCB=8
      LINFINIBAND=T
      WALLTIME_HH=1 
      PUTDATA="1, 'steps'"
      LDEBUG=T
    else
      QUEUE=test
      NODE=2
      TASKS_PER_NODE=4
      NPROCA=2
      NPROCB=4
      LINFINIBAND=T
      WALLTIME_HH=1
      PUTDATA="1, 'steps'"
      LDEBUG=T
    fi
  elif [ ${MACHINE} == "CWB" ]; then
    HUGE_MEM=F
    WALLTIME_HH=96
    if [ "${2}" == "16cpu" ]; then
      QUEUE="research"
      NODE=1
      TASKS_PER_NODE=16
      NPROCA=4
      NPROCB=4
      LINFINIBAND=T
      WALLTIME_HH=8
    elif [ ""research"" == "32cpu" ]; then
     QUEUE="research"
      NODE=2
      TASKS_PER_NODE=16
      NPROCA=4
      NPROCB=8
      LINFINIBAND=T
      WALLTIME_HH=8
    elif [ "${2}" == "64cpu" ]; then
     QUEUE="research"
      NODE=4
      TASKS_PER_NODE=16
      NPROCA=8
      NPROCB=8
      LINFINIBAND=F
      WALLTIME_HH=8
    elif [ "${2}" == "128cpu" ]; then
     QUEUE="research"
      NODE=8
      TASKS_PER_NODE=16
      NPROCA=8
      NPROCB=16
      LINFINIBAND=F
      WALLTIME_HH=8
    elif [ "${2}" == "256cpu" ]; then
     QUEUE="research"
      NODE=16
      TASKS_PER_NODE=16
      NPROCA=8
      NPROCB=32
      LINFINIBAND=F
      WALLTIME_HH=8
    elif [ "${2}" == "test16" ]; then
      QUEUE=test
      NODE=1
      TASKS_PER_NODE=16
      NPROCA=4
      NPROCB=4
      LINFINIBAND=T
      WALLTIME_HH=1  
      PUTDATA="1, 'steps'"
      LDEBUG=T
    else
      QUEUE=test
      NODE=1
      TASKS_PER_NODE=16
      NPROCA=4
      NPROCB=4
      LINFINIBAND=T
      WALLTIME_HH=1
      PUTDATA="1, 'steps'"
      LDEBUG=T
    fi
  elif [ ${MACHINE} == "NUWA" ]; then
    LINFINIBAND=T
    WALLTIME_HH=120
    if [ "${2}" == "8cpu" ]; then
      QUEUE="pl-16"
      NODE=2
      TASKS_PER_NODE=4
      NPROCA=2
      NPROCB=4
      WALLTIME_HH=120
    elif [ "${2}" == "16cpu" ]; then
      QUEUE="pl-32"
      NODE=4
      TASKS_PER_NODE=4
      NPROCA=4
      NPROCB=4
      WALLTIME_HH=72
    elif [ "${2}" == "32cpu" ]; then
      QUEUE="pl-64"
      NODE=8
      TASKS_PER_NODE=4
      NPROCA=4
      NPROCB=8
      WALLTIME_HH=48
    elif [ "${2}" == "64cpu" ]; then
      QUEUE="pl-64"
      NODE=8
      TASKS_PER_NODE=8
      NPROCA=8
      NPROCB=8
      WALLTIME_HH=48
    elif [ "${2}" == "64cpu_m" ]; then
#     2 times faster than
      QUEUE="pl-128"
      NODE=16
      TASKS_PER_NODE=4
      NPROCA=8
      NPROCB=8
      WALLTIME_HH=24
      PMEM=",pmem=4110564k"
    elif [ "${2}" == "128cpu" ]; then
      QUEUE="pl-128"
      NODE=16
      TASKS_PER_NODE=8
      NPROCA=8
      NPROCB=16
      WALLTIME_HH=24
    elif [ "${2}" == "128cpu_m" ]; then
#     2 times faster than
      QUEUE="pl-256"
      NODE=32
      TASKS_PER_NODE=4
      NPROCA=8
      NPROCB=16
      WALLTIME_HH=18
      PMEM=",pmem=4110564k"
    elif [ "${2}" == "256cpu" ]; then
      QUEUE="pl-512"
      NODE=64
      TASKS_PER_NODE=4
      NPROCA=8
      NPROCB=32
      WALLTIME_HH=12
    elif [ "${2}" == "512cpu" ]; then
      QUEUE="pl-1024"
      NODE=128
      TASKS_PER_NODE=4
      NPROCA=8
      NPROCB=64
      WALLTIME_HH=12
    elif [ "${2}" == "test64" ]; then
      QUEUE="pl-128"
      NODE=16
      TASKS_PER_NODE=4
      NPROCA=8
      NPROCB=8
      PUTDATA="1, 'steps'"
      LDEBUG=T
      WALLTIME_HH=24
    elif [ "${2}" == "test32" ]; then
      QUEUE="pl-64"
      NODE=8
      TASKS_PER_NODE=4
      NPROCA=4
      NPROCB=8
      PUTDATA="1, 'steps'"
      LDEBUG=T
      WALLTIME_HH=24
    elif [ "${2}" == "test8" ]; then
      QUEUE="pl-16"
      NODE=2
      TASKS_PER_NODE=4
      NPROCA=2
      NPROCB=4
      PUTDATA="1, 'steps'"
      LDEBUG=T
      WALLTIME_HH=1
##  #    WALLTIME=24:00:00
    else
      QUEUE="pl-64"
      NODE=8
      TASKS_PER_NODE=8
      NPROCA=8
      NPROCB=8
      PUTDATA="1, 'steps'"
      LDEBUG=T
      WALLTIME_HH=24
    fi
  elif [ ${MACHINE} == "ALPS" ]; then
    HUGE_MEM=F
    WALLTIME_HH=96
    if [ "${2}" == "768cpu" ]; then
      QUEUE=${2}
      NODE=768
      NPROCA=32
      NPROCB=24
      LINFINIBAND=T
      WALLTIME_HH=48      
    elif [ "${2}" == "384cpu" ]; then
      QUEUE=${2}
      NODE=384
      NPROCA=24
      NPROCB=16
      LINFINIBAND=T
      WALLTIME_HH=72
    elif [ "${2}" == "248cpu" ]; then
      QUEUE=${2}
      NODE=248
      NPROCA=8
      NPROCB=31
      LINFINIBAND=T
      WALLTIME_HH=72
#    elif [ "${2}" == "192cpu" ]; then
#      QUEUE=${2}
#      NODE=192
#      NPROCA=16
#      NPROCB=12
#      LINFINIBAND=T
    elif [ "${2}" == "mono192_p48" ]; then
      QUEUE=monos10
      NODE=192
      NPROCA=8
      NPROCB=24
      LINFINIBAND=T      
      WALLTIME_HH=120
      LPtile48=T
    elif [ "${2}" == "mono192_p" ]; then
      QUEUE=monos10
      NODE=192
      NPROCA=8
      NPROCB=24
      LINFINIBAND=T      
      WALLTIME_HH=120
      LPtile24=T
    elif [ "${2}" == "192cpu" ]; then
      QUEUE=${2}
      NODE=192
      NPROCA=8
      NPROCB=24
      LINFINIBAND=T      
      WALLTIME_HH=120
    elif [ "${2}" == "192cpu_p" ]; then
      QUEUE=384cpu
      NODE=192
      NPROCA=8
      NPROCB=24
      LINFINIBAND=T      
      WALLTIME_HH=120
      LPtile24=T
    elif [ "${2}" == "mono144_p48" ]; then
      QUEUE=monos10
      NODE=144
      NPROCA=8
      NPROCB=18
      LINFINIBAND=T
      WALLTIME_HH=120
      LPtile48=T
    elif [ "${2}" == "mono144_p" ]; then
      QUEUE=monos10
      NODE=144
      NPROCA=8
      NPROCB=18
      LINFINIBAND=T
      WALLTIME_HH=120
      LPtile24=T
    elif [ "${2}" == "mono144" ]; then
      QUEUE=monos10
      NODE=144
      NPROCA=8
      NPROCB=18
      LINFINIBAND=T
      WALLTIME_HH=120
    elif [ "${2}" == "144cpu" ]; then
      QUEUE=${2}
      NODE=144
      NPROCA=8
      NPROCB=18
      LINFINIBAND=T
      WALLTIME_HH=120
    elif [ "${2}" == "mono128" ]; then
      QUEUE=monos10
      NODE=128
      NPROCA=8
      NPROCB=16
      LINFINIBAND=T      
      WALLTIME_HH=120
    elif [ "${2}" == "mono128_p" ]; then
      QUEUE=monos10
      NODE=128
      NPROCA=8
      NPROCB=16
      LINFINIBAND=T      
      WALLTIME_HH=120
      LPtile24=T
    elif [ "${2}" == "128cpu" ]; then
      QUEUE=${2}
      NODE=128
      NPROCA=8
      NPROCB=16
      LINFINIBAND=T      
      WALLTIME_HH=120
    elif [ "${2}" == "128cpu_p" ]; then
      QUEUE=384cpu
      NODE=128
      NPROCA=8
      NPROCB=16
      LINFINIBAND=T
      WALLTIME_HH=72
      LPtile24=T
    elif [ "${2}" == "mono96_p48" ]; then
      QUEUE=monos10
      NODE=96
      NPROCA=8
      NPROCB=12
      LINFINIBAND=T      
      WALLTIME_HH=120
      LPtile48=T
    elif [ "${2}" == "mono96_p" ]; then
      QUEUE=monos10
      NODE=96
      NPROCA=8
      NPROCB=12
      LINFINIBAND=T      
      WALLTIME_HH=120
      LPtile24=T
    elif [ "${2}" == "mono96" ]; then
      QUEUE=monos10
      NODE=96
      NPROCA=8
      NPROCB=12
      LINFINIBAND=T      
      WALLTIME_HH=120
    elif [ "${2}" == "96cpu_p" ]; then
      QUEUE=192cpu
      NODE=96
      NPROCA=8
      NPROCB=12
      LINFINIBAND=T      
      WALLTIME_HH=120
      LPtile24=T
    elif [ "${2}" == "96cpu" ]; then
      QUEUE=192cpu
      NODE=96
      NPROCA=8
      NPROCB=12
      LINFINIBAND=T
      WALLTIME_HH=120
    elif [ "${2}" == "mono72_p" ]; then
      QUEUE=monos10
      NODE=72
      NPROCA=8
      NPROCB=9
      LINFINIBAND=T      
      WALLTIME_HH=120
      LPtile24=T
    elif [ "${2}" == "mono72" ]; then
      QUEUE=monos10
      NODE=72
      NPROCA=8
      NPROCB=9
      LINFINIBAND=T      
      WALLTIME_HH=120
    elif [ "${2}" == "mono64" ]; then
      QUEUE=monos10
      NODE=64
      NPROCA=8
      NPROCB=8
      LINFINIBAND=T      
      WALLTIME_HH=120
    elif [ "${2}" == "64cpu" ]; then
      QUEUE=${2}
      NODE=64
      NPROCA=8
      NPROCB=8
      LINFINIBAND=T      
      WALLTIME_HH=120
    elif [ "${2}" == "mono48_p" ]; then
      QUEUE=monos10
      NODE=48
      NPROCA=8
      NPROCB=6
      LINFINIBAND=T
      WALLTIME_HH=144
      LPtile24=T
    elif [ "${2}" == "mono48" ]; then
      QUEUE=monos10
      NODE=48
      NPROCA=8
      NPROCB=6
      LINFINIBAND=T
      WALLTIME_HH=144
    elif [ "${2}" == "48cpu_p" ]; then
      QUEUE=${2}
      NODE=48
      NPROCA=8
      NPROCB=6
      LINFINIBAND=T
      WALLTIME_HH=144
      LPtile24=T
    elif [ "${2}" == "48cpu" ]; then
      QUEUE=${2}
      NODE=48
      NPROCA=8
      NPROCB=6
      LINFINIBAND=T
      WALLTIME_HH=144
    elif [ "${2}" == "short48" ]; then
      QUEUE=short
      NODE=48
      NPROCA=8
      NPROCB=6
      LINFINIBAND=T
      WALLTIME_HH=48
    elif [ "${2}" == "medium48" ]; then
      QUEUE=medium
      NODE=48
      NPROCA=8
      NPROCB=6
      LINFINIBAND=T
      WALLTIME_HH=144
    elif [ "${2}" == "long48" ]; then
      QUEUE=long
      NODE=48
      NPROCA=8
      NPROCB=6
      LINFINIBAND=T
      WALLTIME_HH=240
    elif [ "${2}" == "mono24_p" ]; then
      QUEUE=monos10
      NODE=24
      NPROCA=8
      NPROCB=3
      LINFINIBAND=T
      WALLTIME_HH=144
      LPtile24=T
    elif [ "${2}" == "mono24" ]; then
      QUEUE=monos10
      NODE=24
      NPROCA=8
      NPROCB=3
      LINFINIBAND=T
      WALLTIME_HH=144
    elif [ "${2}" == "mono12_p" ]; then
      QUEUE=monos10
      NODE=12
      NPROCA=4
      NPROCB=3
      LINFINIBAND=T
      WALLTIME_HH=192
      LPtile24=T
    elif [ "${2}" == "12cpu" ]; then
      QUEUE=${2}
      NODE=12
      NPROCA=4
      NPROCB=3
      LINFINIBAND=T
      WALLTIME_HH=192
    elif [ "${2}" == "mono6_p" ]; then
      QUEUE=monos10
      NODE=6
      NPROCA=2
      NPROCB=3
      LINFINIBAND=T
      WALLTIME_HH=192
      LPtile24=T
    elif [ "${2}" == "mono3_p" ]; then
      QUEUE=monos10
      NODE=3
      NPROCA=3
      NPROCB=1
      LINFINIBAND=T
      WALLTIME_HH=192
    elif [ "${2}" == "mono2_p" ]; then
      QUEUE=monos10
      NODE=2
      NPROCA=2
      NPROCB=1
      LINFINIBAND=T
      WALLTIME_HH=192
    elif [ "${2}" == "mono1_p" ]; then
      QUEUE=monos10
      NODE=1
      NPROCA=1
      NPROCB=1
      LINFINIBAND=T
      WALLTIME_HH=192
    elif [ "${2}" == "short" ]; then
      QUEUE=${2}
      NODE=128
      NPROCA=8
      NPROCB=16
      LINFINIBAND=T      
      WALLTIME_HH=48
    elif [ "${2}" == "medium" ]; then
      QUEUE=${2}
      NODE=128
      NPROCA=8
      NPROCB=16
      LINFINIBAND=T      
      WALLTIME_HH=144
    elif [ "${2}" == "long" ]; then
      QUEUE=${2}
      NODE=128
      NPROCA=8
      NPROCB=16
      LINFINIBAND=T      
      WALLTIME_HH=240
    elif [ "${2}" == "test" ]; then
      QUEUE="test"
      NODE=128
      NPROCA=8
      NPROCB=16
      LINFINIBAND=T
      WALLTIME_HH=1
    elif [ "${2}" == "test_p" ]; then
      QUEUE="test"
      NODE=128
      NPROCA=8
      NPROCB=16
      LINFINIBAND=T
      WALLTIME_HH=1
      LPtile24=T
    elif [ "${2}" == "test_debug" ]; then
      QUEUE="test"
      NODE=128
      NPROCA=8
      NPROCB=16
      LDEBUG=T
      LINFINIBAND=T
      WALLTIME_HH=1
    elif [ "${2}" == "test96_p" ]; then
      QUEUE="test"
      NODE=96
      NPROCA=8
      NPROCB=12
      LINFINIBAND=T
      WALLTIME_HH=1
      LPtile24=T
    elif [ "${2}" == "test96" ]; then
      QUEUE="test"
      NODE=96
      NPROCA=8
      NPROCB=12
      LINFINIBAND=T
      WALLTIME_HH=1
    elif [ "${2}" == "test64" ]; then
      QUEUE="test"
      NODE=64
      NPROCA=8
      NPROCB=8
      LINFINIBAND=T
      WALLTIME_HH=1
    elif [ "${2}" == "test64_debug" ]; then
      QUEUE="test"
      NODE=64
      NPROCA=8
      NPROCB=8
      LDEBUG=T      
      LINFINIBAND=T
      WALLTIME_HH=1
    elif [ "${2}" == "test48" ]; then
      QUEUE=test
      NODE=48
      NPROCA=8
      NPROCB=6
      LINFINIBAND=T
      WALLTIME_HH=1
    elif [ "${2}" == "test48_debug" ]; then
      QUEUE=test
      NODE=48
      NPROCA=8
      NPROCB=6
      LDEBUG=T
      LINFINIBAND=T
      WALLTIME_HH=1
    elif [ "${2}" == "test32" ]; then
      QUEUE="test"
      NODE=32
      NPROCA=8
      NPROCB=4
      LINFINIBAND=T
      WALLTIME_HH=1
    elif [ "${2}" == "test32_debug" ]; then
      QUEUE="test"
      NODE=32
      NPROCA=8
      NPROCB=4
      LDEBUG=T
      LINFINIBAND=T
      WALLTIME_HH=1
    elif [ "${2}" == "test16" ]; then
      QUEUE="test"
      NODE=16
      NPROCA=8
      NPROCB=2
      LINFINIBAND=T
      WALLTIME_HH=1
    elif [ "${2}" == "test8" ]; then
      QUEUE="test"
      NODE=8
      NPROCA=8
      NPROCB=1
      LINFINIBAND=T
      WALLTIME_HH=1
    fi
  fi  #${MACHINE}
fi

WALLTIME=${WALLTIME_HH}:00:00
LRCP26=F
LRCP45=F
LRCP85=F
LPREINDUSTRY=F
LHISTORY=F
LFUTURE=F
LPRESENT=F
if [ -n "${3}" ]; then
# Period for forecast
# specify greenhouse gases
#
  PF=${3}
  echo ${3}
  if [ "${3}" == "r26" ]; then
  # future R26 scenario
    LRCP26=T
    LFUTURE=T
  elif [ "${3}" == "r45" ]; then
  # future R45 scenario
    LRCP45=T
    LFUTURE=T
  elif [ "${3}" == "r85" ]; then
  # future R85 scenario
    LRCP85=T
    LFUTURE=T
  elif [ "${3}" == "c" ]; then
  # climate run
    LRCP26=F
    LRCP45=F
    LRCP85=F
    LFUTURE=F
    LPRESENT=F
    TEMPLATE=v0
  elif [ "${3}" == "c1" ]; then
  # climate run
    LRCP26=F
    LRCP45=F
    LRCP85=F
    LFUTURE=F
    LPRESENT=F
    TEMPLATE=v1
  elif [ "${3}" == "h" ]; then
  # preindustry/historical run
  # climate run
    LHISTORY=T
    LFUTURE=F
    LPRESENT=F
  elif [ "${3}" == "p" ]; then
  # present run
    LPRESENT=T
  elif [ "${3}" == "pi" ]; then
  # preindustry run
  # climate run
    LPREINDUSTRY=T
    LFUTURE=F
    LPRESENT=F
  else
    echo "PF="${PF}
    echo "ERROR: undefined arg3="${3}
    echo "ERROR: job not submitted"
    exit
  fi
else
  # present run
  LPRESENT=T
fi

if [ "${LPRESENT}" == "T" ]; then
  LAMIP=T
  IGHG=1
  IPCC=T
  SO4=T
elif [ ${LRCP26} == "T" ]; then  
  LAMIP=T
  IGHG=1
  IPCC=T
  SO4=T
elif [ ${LRCP45} == "T" ]; then  
  LAMIP=T
  IGHG=2
  IPCC=T
  SO4=T
elif [ ${LRCP85} == "T" ]; then  
  LAMIP=T
  IGHG=3
  IPCC=T
  SO4=T
elif [ "${LHISTORY}" == "T" ]; then
  LAMIP=T
  IGHG=4
  IPCC=T
  SO4=F
else
# fixed CO2 run (constant/climate rum)
  LAMIP=F
  IGHG=0
  IPCC=F
  SO4=F
fi

LPERS_SST=F
CLIMATE_SST="AMIP2"
if [ 1 -eq 1 ]; then
# default for forecasting
  SSTDATA=0   # using 1870~1979: Hadley SST monthly data, 1979~ 2003: ncepr R2 daily SST, 2004~ present: ncepr R2 daily sic + TMI sst (default)
  WOADATA=0   ## <1946: Ishii_1945, 1946~1980: Ishii data, 1981~now: GODAS data (default)    
else
  SSTDATA=2   # Hadley SST (default)
  WOADATA=1   # Ishii ocean profile data    
fi 
LHD=F
LSIT=T
TRIGSIT="1, 'steps'"
TRIGOCN="1, 'steps'"
LSSST=T
LSIT_ICE=T
LGODAS=T
LWOA0=T
ICE_OPTION=0
LOCN=F
OCN_COUPLE_OPTION=11
# TIMCOM decide
#MASKID_OPTION=0   (crash for AMIP run)
# ECHAM decide
MASKID_OPTION=1
LOCN_MSG=F
LOCAF=F
LSICE_NUDG=F
LSIT_LW=F
CSL=-27.

if [ -n "${4}" ]; then
  echo "TYPE" =${3}${4}
  TYPE=${4}
  if [ "${4}" == "o" ] || [ "${4}" == "ob" ]; then
    # default (ocean run)
    LOCN=T
    LHD=T
    OCN_COUPLE_OPTION=11
    MASKID_OPTION=1
  elif [ "${4}" == "o0" ] || [ "${4}" == "o00" ]; then
    LOCN=T
    LHD=T
    OCN_COUPLE_OPTION=0
    MASKID_OPTION=0
  elif [ "${4}" == "o01" ]; then
    LOCN=T
    LHD=T
    OCN_COUPLE_OPTION=0
    MASKID_OPTION=1
  elif [ "${4}" == "o1" ]; then
    LOCN=T
    LHD=T
    OCN_COUPLE_OPTION=1
  elif [ "${4}" == "o2" ]; then
    LOCN=T
    LHD=T
    OCN_COUPLE_OPTION=2
  elif [ "${4}" == "o3" ]; then
    LOCN=T
    LHD=T
    OCN_COUPLE_OPTION=3
  elif [ "${4}" == "o4" ]; then
    LOCN=T
    LHD=T
    OCN_COUPLE_OPTION=4
  elif [ "${4}" == "o8" ]; then
    LOCN=T
    LHD=T
    OCN_COUPLE_OPTION=8
  elif [ "${4}" == "o12" ]; then
    LOCN=T
    LHD=T
    OCN_COUPLE_OPTION=12
  elif [ "${4}" == "oa" ]; then
    LOCN=T
    LHD=T
    OCN_COUPLE_OPTION=a
  elif [ "${4}" == "ob" ]; then
    LOCN=T
    LHD=T
    OCN_COUPLE_OPTION=11
  elif [ "${4}" == "ob0" ]; then
    LOCN=T
    LHD=T
    OCN_COUPLE_OPTION=11
    MASKID_OPTION=0
  elif [ "${4}" == "ob1" ]; then
    LOCN=T
    LHD=T
    OCN_COUPLE_OPTION=11
    MASKID_OPTION=1
  elif [ "${4}" == "obf" -o  "${4}" == "ob0f" ]; then
    LOCN=T
    LHD=T
    OCN_COUPLE_OPTION=11
    MASKID_OPTION=1
    LSIT_ICE=F
  elif [ "${4}" == "obg" ]; then
    LOCN=T
    LHD=T
    OCN_COUPLE_OPTION=11
    LAMIP=T
    SSTDATA=2   # Hadley SST (default)
    WOADATA=2                      # GODAS ocean profile data
  elif [ "${4}" == "obi" ]; then
    LOCN=T
    LHD=T
    OCN_COUPLE_OPTION=11
    LAMIP=T
    SSTDATA=2   # Hadley SST (default)
    WOADATA=1                      # Ishii ocean profile data
   elif [ "${4}" == "obiq" ]; then
    LOCN=T
    LHD=T
    OCN_COUPLE_OPTION=11
    LAMIP=T
    SSTDATA=2   # Hadley SST (default)
    WOADATA=1                      # Ishii ocean profile data
    LOCAF=T
  elif [ "${4}" == "obni" ]; then
    LOCN=T
    LHD=T
    OCN_COUPLE_OPTION=11
    LAMIP=T
    SSTDATA=3   # NCEP daily SST
    WOADATA=1                      # Ishii ocean profile data
  elif [ "${4}" == "obt" ] || [ "${4}" == "obti" ]; then
    LOCN=T
    LHD=T
    OCN_COUPLE_OPTION=11
    LAMIP=T
    SSTDATA=4    # TMI daily SST and ncep_r2 seaice
    WOADATA=1                      # Ishii ocean profile data
  elif [ "${4}" == "obtg" ]; then
    LOCN=T
    LHD=T
    OCN_COUPLE_OPTION=11
    LAMIP=T
    SSTDATA=4    # TMI daily SST and ncep_r2 seaice
    WOADATA=2                      # GODAS ocean profile data
  elif [ "${4}" == "obng" ]; then
    LOCN=T
    LHD=T
    OCN_COUPLE_OPTION=11
    LAMIP=T
    SSTDATA=3   # NCEP daily SST
    WOADATA=2   # GODAS ocean profile data
  elif [ "${4}" == "obngq" ]; then
    LOCN=T
    LHD=T
    OCN_COUPLE_OPTION=11
    LAMIP=T
    SSTDATA=3   # NCEP daily SST
    WOADATA=2   # GODAS ocean profile data
    MASKID_OPTION=1
    LOCAF=T
  elif [ "${4}" == "od" ]; then
    # default (ocean run)
    LOCN=T
    LHD=T
    OCN_COUPLE_OPTION=11
    MASKID_OPTION=1
    TRIGOCN="1, 'days'"
  elif [ "${4}" == "odx" ]; then
    # default (ocean run)
    LOCN=T
    LHD=T
    OCN_COUPLE_OPTION=11
    MASKID_OPTION=1
    TRIGOCN="1, 'days'"
    WOADATA=99   # no GODAS and WOA0 ocean profile data                
  elif [ "${4}" == "odq" ]; then
    LOCN=T
    LHD=T
    OCN_COUPLE_OPTION=11
    MASKID_OPTION=1
    TRIGOCN="1, 'days'"    
    LOCAF=T
  elif [ "${4}" == "ocs19" ]; then
    # Caspian Sea only run. Setting depth at -27 m ASL (ocean run)
    LAMIP=F
    CLIMATE_SST="MEDIVA2"
    LSIT=T    
    LOCN=T
    LHD=T
    OCN_COUPLE_OPTION=11
    MASKID_OPTION=1
    TRIGOCN="1, 'days'"
    # Caspian Sea mask
    nobox_nudg=1
    # number of nudg squares in ocean grids (default = 0, maximun=6)    
    obox_ndg_restore_time=0.        # 0s
    #obox_ndg_restore_time=86400.    # 1d
    #obox_ndg_restore_time=864000.    # 10d
    #obox_ndg_restore_time=2592000.  # 1m
    #obox_ndg_restore_time=31536000. # 1y
    #obox_ndg_restore_time=-9.e33    # nn
    obox_nudg_flag=112
    # 0: no nudging, 1: nudging inside square boxes, 2: nudging outside the square boxes
    obox_nudg_w="42.,-999.,-999.,-999.,-999.,-999.",
    # west coords (lon) of nudging boxes [-180., 360.](deg). There are 6 values for 6 squares, 
    # must be filled with values. Set them to be -999. for missing squares. 
    obox_nudg_e="56.,-999.,-999.,-999.,-999.,-999.",
    # east coords (lon) of nudging boxes [-180., 360.](deg). There are 6 boxes.
    obox_nudg_s="36.,-999.,-999.,-999.,-999.,-999.",
    # south coords (lat) of nudging boxes [-180., 360.](deg). There are 6 boxes.
    obox_nudg_n="51.,-999.,-999.,-999.,-999.,-999.",
    # north coords (lat) of nudging boxes [-180., 360.](deg). There are 6 boxes.
    CSL=-19.
  elif [ "${4}" == "ocs27" ]; then
    # Caspian Sea only run. Setting depth at -27 m ASL (ocean run)
    LAMIP=F
    CLIMATE_SST="MEDIVA2"
    LSIT=T    
    LOCN=T
    LHD=T
    OCN_COUPLE_OPTION=11
    MASKID_OPTION=1
    TRIGOCN="1, 'days'"
    # Caspian Sea mask
    nobox_nudg=1
    # number of nudg squares in ocean grids (default = 0, maximun=6)    
    obox_ndg_restore_time=0.        # 0s
    #obox_ndg_restore_time=86400.    # 1d
    #obox_ndg_restore_time=864000.    # 10d
    #obox_ndg_restore_time=2592000.  # 1m
    #obox_ndg_restore_time=31536000. # 1y
    #obox_ndg_restore_time=-9.e33    # nn
    obox_nudg_flag=112
    # 0: no nudging, 1: nudging inside square boxes, 2: nudging outside the square boxes
    obox_nudg_w="42.,-999.,-999.,-999.,-999.,-999.",
    # west coords (lon) of nudging boxes [-180., 360.](deg). There are 6 values for 6 squares, 
    # must be filled with values. Set them to be -999. for missing squares. 
    obox_nudg_e="56.,-999.,-999.,-999.,-999.,-999.",
    # east coords (lon) of nudging boxes [-180., 360.](deg). There are 6 boxes.
    obox_nudg_s="36.,-999.,-999.,-999.,-999.,-999.",
    # south coords (lat) of nudging boxes [-180., 360.](deg). There are 6 boxes.
    obox_nudg_n="51.,-999.,-999.,-999.,-999.,-999.",
    # north coords (lat) of nudging boxes [-180., 360.](deg). There are 6 boxes.
    CSL=-27.
  elif [ "${4}" == "ocs30" ]; then
    # Caspian Sea only run. Setting depth at -30 m ASL (ocean run)
    LAMIP=F
    CLIMATE_SST="MEDIVA2"
    LSIT=T    
    LOCN=T
    LHD=T
    OCN_COUPLE_OPTION=11
    MASKID_OPTION=1
    TRIGOCN="1, 'days'"
    # Caspian Sea mask
    nobox_nudg=1
    # number of nudg squares in ocean grids (default = 0, maximun=6)    
    obox_ndg_restore_time=0.        # 0s
    #obox_ndg_restore_time=86400.    # 1d
    #obox_ndg_restore_time=864000.    # 10d
    #obox_ndg_restore_time=2592000.  # 1m
    #obox_ndg_restore_time=31536000. # 1y
    #obox_ndg_restore_time=-9.e33    # nn
    obox_nudg_flag=112
    # 0: no nudging, 1: nudging inside square boxes, 2: nudging outside the square boxes
    obox_nudg_w="42.,-999.,-999.,-999.,-999.,-999.",
    # west coords (lon) of nudging boxes [-180., 360.](deg). There are 6 values for 6 squares, 
    # must be filled with values. Set them to be -999. for missing squares. 
    obox_nudg_e="56.,-999.,-999.,-999.,-999.,-999.",
    # east coords (lon) of nudging boxes [-180., 360.](deg). There are 6 boxes.
    obox_nudg_s="36.,-999.,-999.,-999.,-999.,-999.",
    # south coords (lat) of nudging boxes [-180., 360.](deg). There are 6 boxes.
    obox_nudg_n="51.,-999.,-999.,-999.,-999.,-999.",
    # north coords (lat) of nudging boxes [-180., 360.](deg). There are 6 boxes.
    CSL=-30.
  elif [ "${4}" == "ocs33" ]; then
    # Caspian Sea only run. Setting depth at -33 m ASL (ocean run)
    LAMIP=F
    CLIMATE_SST="MEDIVA2"
    LSIT=T    
    LOCN=T
    LHD=T
    OCN_COUPLE_OPTION=11
    MASKID_OPTION=1
    TRIGOCN="1, 'days'"
    # Caspian Sea mask
    nobox_nudg=1
    # number of nudg squares in ocean grids (default = 0, maximun=6)    
    obox_ndg_restore_time=0.        # 0s
    #obox_ndg_restore_time=86400.    # 1d
    #obox_ndg_restore_time=864000.    # 10d
    #obox_ndg_restore_time=2592000.  # 1m
    #obox_ndg_restore_time=31536000. # 1y
    #obox_ndg_restore_time=-9.e33    # nn
    obox_nudg_flag=112
    # 0: no nudging, 1: nudging inside square boxes, 2: nudging outside the square boxes
    obox_nudg_w="42.,-999.,-999.,-999.,-999.,-999.",
    # west coords (lon) of nudging boxes [-180., 360.](deg). There are 6 values for 6 squares, 
    # must be filled with values. Set them to be -999. for missing squares. 
    obox_nudg_e="56.,-999.,-999.,-999.,-999.,-999.",
    # east coords (lon) of nudging boxes [-180., 360.](deg). There are 6 boxes.
    obox_nudg_s="36.,-999.,-999.,-999.,-999.,-999.",
    # south coords (lat) of nudging boxes [-180., 360.](deg). There are 6 boxes.
    obox_nudg_n="51.,-999.,-999.,-999.,-999.,-999.",
    # north coords (lat) of nudging boxes [-180., 360.](deg). There are 6 boxes.
    CSL=-33.
  elif [ "${4}" == "ods" ]; then
    # Sulu Sea Mask run. The model in T63 has huge vertical transport in Sulu Sea due to numerical error.
    # Nudging Sulu Sea to prevent this effect (ocean run)
    LOCN=T
    LHD=T
    OCN_COUPLE_OPTION=11
    MASKID_OPTION=1
    TRIGOCN="1, 'days'"
    # Sulu Sea mask
    nobox_nudg=1
    # number of nudg squares in ocean grids (default = 0, maximun=6)    
    #obox_ndg_restore_time=86400.    # 1d
    obox_ndg_restore_time=864000.    # 10d
    #obox_ndg_restore_time=2592000.  # 1m
    #obox_ndg_restore_time=31536000. # 1y
    #obox_ndg_restore_time=-9.e33    # nn
    obox_nudg_flag=111
    # 0: no nudging, 1: nudging inside square boxes, 2: nudging outside the square boxes
    obox_nudg_w="117.,-999.,-999.,-999.,-999.,-999.",
    # west coords (lon) of nudging boxes [-180., 360.](deg). There are 6 values for 6 squares, 
    # must be filled with values. Set them to be -999. for missing squares. 
    obox_nudg_e="127.,-999.,-999.,-999.,-999.,-999.",
    # east coords (lon) of nudging boxes [-180., 360.](deg). There are 6 boxes.
    obox_nudg_s="2.,-999.,-999.,-999.,-999.,-999.",
    # south coords (lat) of nudging boxes [-180., 360.](deg). There are 6 boxes.
    obox_nudg_n="11.,-999.,-999.,-999.,-999.,-999.",
    # north coords (lat) of nudging boxes [-180., 360.](deg). There are 6 boxes.
  elif [ "${4}" == "ods10" ]; then
    # Sulu Sea Mask run. The model in T63 has huge vertical transport in Sulu Sea due to numerical error.
    # Nudging Sulu Sea to prevent this effect (ocean run)
    LOCN=T
    LHD=T
    OCN_COUPLE_OPTION=11
    MASKID_OPTION=1
    TRIGOCN="1, 'days'"
    # Sulu Sea mask
    nobox_nudg=1
    # number of nudg squares in ocean grids (default = 0, maximun=6)    
    #obox_ndg_restore_time=86400.    # 1d
    obox_ndg_restore_time=864000.    # 10d
    #obox_ndg_restore_time=2592000.  # 1m
    #obox_ndg_restore_time=31536000. # 1y
    #obox_ndg_restore_time=-9.e33    # nn
    obox_nudg_flag=721
    # .?: (first digit): T,S nudging in Sulu Sea, no nudging, 1: nudging inside square boxes, 2: nudging outside the square boxes
    # ?.: (decimal digit): high_damping coefficient for u,v currents in Sulu Sea, no nudging, 1: nudging inside square boxes, 2: nudging outside the square boxes
    # obox_nudg_flag
    # 0             |    0         |    0: no nudging
    # 1:  0- 10 m   |    1: T,S    |    1: nudging inside
    # 2: 10-100 m   |    2: U,V    |    2: nudging outside
    # 4    >100 m   |              |    
    # For example, sit_ocean.f90
    # IF (obox_mask(jl).GT.0._dp).AND.(MOD(INT(INT(INT(obox_nudg_flag/100)/2)/2),2).EQ.1)) THEN      
    obox_nudg_w="117.,-999.,-999.,-999.,-999.,-999.",
    # west coords (lon) of nudging boxes [-180., 360.](deg). There are 6 values for 6 squares, 
    # must be filled with values. Set them to be -999. for missing squares. 
    obox_nudg_e="127.,-999.,-999.,-999.,-999.,-999.",
    # east coords (lon) of nudging boxes [-180., 360.](deg). There are 6 boxes.
    obox_nudg_s="2.,-999.,-999.,-999.,-999.,-999.",
    # south coords (lat) of nudging boxes [-180., 360.](deg). There are 6 boxes.
    obox_nudg_n="11.,-999.,-999.,-999.,-999.,-999.",
    # north coords (lat) of nudging boxes [-180., 360.](deg). There are 6 boxes.
  elif [ "${4}" == "ol" ]; then
    # ocean run with for turning LW code for water in SIT.
    LOCN=T
    LHD=T
    OCN_COUPLE_OPTION=11
    MASKID_OPTION=1
    LSIT_LW=T
  elif [ "${4}" == "indo" ]; then
  # Indian Ocean mask
    LOCN=T
    LHD=T
    nobox_nudg=1
    #obox_ndg_restore_time=86400.    # 1d
    obox_ndg_restore_time=864000.    # 10d
    #obox_ndg_restore_time=2592000.  # 1m
    #obox_ndg_restore_time=31536000. # 1y
    #obox_ndg_restore_time=-9.e33    # nn
# number of nudg squares in ocean grids (default = 0, maximun=6)
    obox_nudg_flag=112
# 0: no nudging, 1: nudging inside square boxes, 2: nudging outside the square boxes
    obox_nudg_w="40.,-999.,-999.,-999.,-999.,-999.",
# west coords (lon) of nudging boxes [-180., 360.](deg). There are 6 values for 6 squares, 
# must be filled with values. Set them to be -999. for missing squares. 
    obox_nudg_e="120.,-999.,-999.,-999.,-999.,-999.",
# east coords (lon) of nudging boxes [-180., 360.](deg). There are 6 boxes.
    obox_nudg_s="-30.,-999.,-999.,-999.,-999.,-999.",
# south coords (lat) of nudging boxes [-180., 360.](deg). There are 6 boxes.
    obox_nudg_n="30.,-999.,-999.,-999.,-999.,-999.",
# north coords (lat) of nudging boxes [-180., 360.](deg). There are 6 boxes.
  elif [ "${4}" == "paco" ]; then
  # Pacific Ocean mask
    LOCN=T
    LHD=T
    nobox_nudg=2
    #obox_ndg_restore_time=86400.    # 1d
    obox_ndg_restore_time=864000.    # 10d
    #obox_ndg_restore_time=2592000.  # 1m
    #obox_ndg_restore_time=31536000. # 1y
    #obox_ndg_restore_time=-9.e33    # nn
# number of nudg squares in ocean grids (default = 0, maximun=6)
    obox_nudg_flag=111
# 0: no nudging, 1: nudging inside square boxes, 2: nudging outside the square boxes
    obox_nudg_w="-170.,-140.,-999.,-999.,-999.,-999.",
# west coords (lon) of nudging boxes [-180., 360.](deg). There are 6 values for 6 squares, 
# must be filled with values. Set them to be -999. for missing squares. 
    obox_nudg_e="-120.,-80.,-999.,-999.,-999.,-999.",
# east coords (lon) of nudging boxes [-180., 360.](deg). There are 6 boxes.
    obox_nudg_s="25.,-30.,-999.,-999.,-999.,-999.",
# south coords (lat) of nudging boxes [-180., 360.](deg). There are 6 boxes.
    obox_nudg_n="30.,-25.,-999.,-999.,-999.,-999.",
# north coords (lat) of nudging boxes [-180., 360.](deg). There are 6 boxes.
  elif [ "${4}" == "obnitir" ]; then
  # Tropical Ocean mask
    LOCN=T
    LHD=T
    SSTDATA=3   # NCEP daily SST
    WOADATA=1                      # Ishii ocean profile data
    nobox_nudg=1
    obox_ndg_restore_time=0.
# number of nudg squares in ocean grids (default = 0, maximun=6)
    obox_nudg_flag=112
# 0: no nudging, 1: nudging inside square boxes, 2: nudging outside the square boxes
    obox_nudg_w="-180.,-999.,-999.,-999.,-999.,-999.",
# west coords (lon) of nudging boxes [-180., 360.](deg). There are 6 values for 6 squares, 
# must be filled with values. Set them to be -999. for missing squares. 
    obox_nudg_e="180.,-999.,-999.,-999.,-999.,-999.",
# east coords (lon) of nudging boxes [-180., 360.](deg). There are 6 boxes.
    obox_nudg_s="-20.,-999.,-999.,-999.,-999.,-999.",
# south coords (lat) of nudging boxes [-180., 360.](deg). There are 6 boxes.
    obox_nudg_n="20.,-999.,-999.,-999.,-999.,-999.",
# north coords (lat) of nudging boxes [-180., 360.](deg). There are 6 boxes.
  elif [ "${4}" == "s" ]; then
    LSIT=T
  elif [ "${4}" == "sl" ]; then
# for turning LW code for water in SIT.  
    LSIT=T
    LSIT_LW=T
  elif [ "${4}" == "s12" ]; then
    LSIT=T
    TRIGSIT="12, 'hours'"
  elif [ "${4}" == "s24" ]; then
    LSIT=T
    TRIGSIT="24, 'hours'"
  elif [ "${4}" == "sc" ]; then
#   coarse resolution run (lssst=F)
    LSIT=T
    LSSST=F
  elif [ "${4}" == "sc12" ]; then
#   coarse resolution run (lssst=F)
    LSIT=T
    LSSST=F
    TRIGSIT="12, 'hours'"
  elif [ "${4}" == "si" ] || [ "${4}" == "shi" ]; then    
    LSIT=T
    LAMIP=T
    SSTDATA=2   # Hadley SST (default)
    WOADATA=1        
  elif [ "${4}" == "si1" ]; then
    LSIT=T
    ICE_OPTION=1
  elif [ "${4}" == "si2" ]; then
    LSIT=T
    ICE_OPTION=2
  elif [ "${4}" == "si3" ]; then
    LSIT=T
    ICE_OPTION=3
  elif [ "${4}" == "si4" ]; then
    LSIT=T
    ICE_OPTION=4    
  elif [ "${4}" == "sif" ]; then
    LSIT=T
    LSIT_ICE=F
  elif [ "${4}" == "sq" ]; then
    LSIT=T
    LOCAF=T
  elif [ "${4}" == "sni" ]; then
    LSIT=T
    LAMIP=T
    SSTDATA=3   # NCEP daily SST
    WOADATA=1
  elif [ "${4}" == "sti" ]; then
    LSIT=T
    LAMIP=T
    SSTDATA=4    # TMI daily SST and ncep_r2 seaice
    WOADATA=1
  elif [ "${4}" == "stg" ]; then
    LSIT=T
    LAMIP=T
    SSTDATA=4    # TMI daily SST and ncep_r2 seaice
    WOADATA=2                      # GODAS ocean profile data
  elif [ "${4}" == "s3" ]; then
    LSIT=T
    MASKID_OPTION=3
  elif [ "${4}" == "skp" ]; then
    LAMIP=F
    CLIMATE_SST="AMIP2"
    LSIT=T
    MASKID_OPTION=5
    CSL=-27.
  elif [ "${4}" == "skm" ]; then
# the present CSL is about -27m and during the medieval time it was about -30m  
    LAMIP=F
    CLIMATE_SST="MEDIVA"
    LSIT=T
    MASKID_OPTION=5
    CSL=-30.
  elif [ "${4}" == "skm2" ]; then
# Same as skm but using the medieval time Land Use  
    LAMIP=F
    CLIMATE_SST="MEDIVA2"
    LSIT=T
    MASKID_OPTION=5
    CSL=-30.
  elif [ "${4}" == "skm3" ]; then
# Same as skm2, but et CSL=-27
    LAMIP=F
    CLIMATE_SST="MEDIVA2"
    LSIT=T
    MASKID_OPTION=5
    CSL=-27.
  elif [ "${4}" == "skm4" ]; then
# Same as skm2, but et CSL=-33
    LAMIP=F
    CLIMATE_SST="MEDIVA2"
    LSIT=T
    MASKID_OPTION=5
    CSL=-33.
  elif [ "${4}" == "skm27" ]; then
# Using the medieval time Land Use    
# but set CSL=-27
    LAMIP=F
    CLIMATE_SST="MEDIVA2"
    LSIT=T
    MASKID_OPTION=5
    CSL=-27.
  elif [ "${4}" == "skm30" ]; then
# Using the medieval time Land Use    
# but set CSL=-30
    LAMIP=F
    CLIMATE_SST="MEDIVA2"
    LSIT=T
    MASKID_OPTION=5
    CSL=-30.
  elif [ "${4}" == "skm33" ]; then
# Using the medieval time Land Use    
# but set CSL=-33
    LAMIP=F
    CLIMATE_SST="MEDIVA2"
    LSIT=T
    MASKID_OPTION=5
    CSL=-33.
  elif [ "${4}" == "sx" ]; then
    LSIT=T
    WOADATA=99   # no GODAS and WOA0 ocean profile data
  elif [ "${4}" == "l" ]; then
    LAMIP=F
    CLIMATE_SST="AMIP2"
    LSIT=F
    CSL=-27.
  elif [ "${4}" == "lge2" ]; then
  # climate run
    LAMIP=F
    CLIMATE_SST="E2"
    LSIT=F
  elif [ "${4}" == "lge1" ]; then
  # climate run
    LAMIP=F
    CLIMATE_SST="E1"    
    LSIT=F
  elif [ "${4}" == "lm" ]; then
  # climate run
    LAMIP=F
    CLIMATE_SST="MEDIVA"
    LSIT=F
    CSL=-30.
  elif [ "${4}" == "lm2" ]; then
  # climate run
    LAMIP=F
    CLIMATE_SST="MEDIVA2"
    LSIT=F
    CSL=-30.
  elif [ "${4}" == "lm3" ]; then
  # climate run
    LAMIP=F
    CLIMATE_SST="MEDIVA2"
    LSIT=F
    CSL=-27.
  elif [ "${4}" == "m" ]; then
    echo "AMIP type run"
    LAMIP=T
    LSIT=F
  elif [ "${4}" == "ma" ]; then
  # Monthly AMIP SST
    LAMIP=T
    LSIT=F
    SSTDATA=1
  elif [ "${4}" == "mh" ]; then
  # Monthly Hadily SST
    LAMIP=T
    LSIT=F
    SSTDATA=2
  elif [ "${4}" == "mn" ]; then
  # NCEP daily SST
    LAMIP=T
    LSIT=F
    SSTDATA=3
  elif [ "${4}" == "mt" ]; then
  # TMI daily SST
    LAMIP=T
    LSIT=F
    SSTDATA=4
  elif [ "${4}" == "pn" ]; then
  # PERSITENT SST/SIC RUN
  # NCEP daily SST
    LPERS_SST=T
    LSIT=F
    LAMIP=T
    SSTDATA=3
  elif [ "${4}" == "pt" ]; then
  # PERSITENT SST/SIC RUN
  # TMI daily SST
    LPERS_SST=T
    LSIT=F
    LAMIP=T
    SSTDATA=4    
  elif [ "${4}" == "sitir" ]; then
  # Tropical Ocean mask
    LSIT=T
    LAMIP=T
    SSTDATA=2   # Hadley SST (default)
    WOADATA=1    
    nobox_nudg=1
    obox_ndg_restore_time=0.
# number of nudg squares in ocean grids (default = 0, maximun=6)
    obox_nudg_flag=112
# 0: no nudging, 1: nudging inside square boxes, 2: nudging outside the square boxes
    obox_nudg_w="-180.,-999.,-999.,-999.,-999.,-999.",
# west coords (lon) of nudging boxes [-180., 360.](deg). There are 6 values for 6 squares, 
# must be filled with values. Set them to be -999. for missing squares. 
    obox_nudg_e="180.,-999.,-999.,-999.,-999.,-999.",
# east coords (lon) of nudging boxes [-180., 360.](deg). There are 6 boxes.
    obox_nudg_s="-20.,-999.,-999.,-999.,-999.,-999.",
# south coords (lat) of nudging boxes [-180., 360.](deg). There are 6 boxes.
    obox_nudg_n="20.,-999.,-999.,-999.,-999.,-999.",
# north coords (lat) of nudging boxes [-180., 360.](deg). There are 6 boxes.
  else
    echo "ERROR: undefined arg4 = "${4}
    echo "ERROR: job not submitted"
    exit
  fi
else
  TYPE=a
fi

if [ "${WOADATA}" == "99" ]; then
  # no GODAS and WOA0 ocean profile data
    LGODAS=F
    LWOA0=F
elif [ "${LSIT}" == "T" ]; then
    LGODAS=T
    LWOA0=T
else
    LGODAS=F
    LWOA0=F
fi

if [ -n "${5}" ]; then
  echo "SFC_NUDG" =${5}
  SFC_NUDG=${5}
  if [ "${5}" == "0d" ]; then
#   0 d = 0. s    
    SOCN_RESTORE_TIME=0.
  elif [ "${5}" == "1d" ]; then
#   1 d = 86400. s    
    SOCN_RESTORE_TIME=86400.
  elif [ "${5}" == "10d" ]; then
#   10 d = 864000. s    
    SOCN_RESTORE_TIME=864000.
  elif [ "${5}" == "1m" ]; then
#   30 d = 2592000. s    
    SOCN_RESTORE_TIME=2592000.
  elif [ "${5}" == "1y" ]; then
#   1 y = 365 d = 31536000. s    
    SOCN_RESTORE_TIME=31536000.
  elif [ "${5}" == "nn" ]; then
    SOCN_RESTORE_TIME=-9.e33
  else
    echo "ERROR: undefined SFC_NUDG =" $SFC_NUDG
    echo "ERROR: job not submitted"
    exit
  fi
else
  SFC_NUDG="nn"
  SOCN_RESTORE_TIME=-9.e33
fi

if [ -n "${6}" ]; then
  echo "UPP_NUDG" =${6}
  UPP_NUDG=${6}
  if [ "${6}" == "0d" ]; then
#   0 d = 0. s    
    UOCN_RESTORE_TIME=0.
  elif [ "${6}" == "1d" ]; then
#   1 d = 86400. s    
    UOCN_RESTORE_TIME=86400.
  elif [ "${6}" == "10d" ]; then
#   1 d = 864000. s    
    UOCN_RESTORE_TIME=864000.
  elif [ "${6}" == "1m" ]; then
#   30 d = 2592000. s    
    UOCN_RESTORE_TIME=2592000.
  elif [ "${6}" == "1y" ]; then
#   1 y = 365 d = 31536000. s    
    UOCN_RESTORE_TIME=31536000.
  elif [ "${6}" == "nn" ]; then
    UOCN_RESTORE_TIME=-9.e33
  else
    echo "ERROR: undefined UPP_NUDG =" $UPP_NUDG
    echo "ERROR: job not submitted"
    exit
  fi
else
  UPP_NUDG="nn"
  UOCN_RESTORE_TIME=-9.e33
fi

if [ -n "${7}" ]; then
  echo "DEEP_NUDG" =${7}
  DEEP_NUDG=${7}
  if [ "${7}" == "0d" ]; then
#   0 d = 0. s    
    DOCN_RESTORE_TIME=0.
  elif [ "${7}" == "1d" ]; then
#   1 d = 86400. s    
    DOCN_RESTORE_TIME=86400.
  elif [ "${7}" == "10d" ]; then
#   10 d = 864000. s    
    DOCN_RESTORE_TIME=864000.
  elif [ "${7}" == "1m" ]; then
#   30 d = 2592000. s    
    DOCN_RESTORE_TIME=2592000.
  elif [ "${7}" == "1y" ]; then
#   1 y = 365 d = 31536000. s    
    DOCN_RESTORE_TIME=31536000.
  elif [ "${7}" == "nn" ]; then
    DOCN_RESTORE_TIME=-9.e33
  else
    echo "ERROR: undefined DEEP_NUDG =" $DEEP_NUDG
    echo "ERROR: job not submitted"
    exit
  fi
else
  DEEP_NUDG="nn"
  DOCN_RESTORE_TIME=-9.e33
fi

if [ "${LOCN}" == "T" ]; then
# nn1m1d for SIT only grid
  SSIT_RESTORE_TIME=-9.e33            #nn
  USIT_RESTORE_TIME=2592000.          #1m
  DSIT_RESTORE_TIME=86400.            #1d
else
  SSIT_RESTORE_TIME=${SOCN_RESTORE_TIME}
  USIT_RESTORE_TIME=${UOCN_RESTORE_TIME}
  DSIT_RESTORE_TIME=${DOCN_RESTORE_TIME}
fi

if [ -n "${8}" ]; then
  echo "LSICE_NUDG" =${8}
  LSICE_NUDG=${8}
  if [ "${8}" == "T" ]; then
    LSICE_NUDG=T
  elif [ "${8}" == "F" ]; then
    LSICE_NUDG=F
  else
    echo "ERROR: undefined LSICE_NUDG =" ${8}
    echo "ERROR: job not submitted"
    exit
  fi
fi


if [ "${9}" == "T31" ]; then
  echo "RES" =${9}
  RES=${9}
  RERUN_INC=1
  RERUN_UNIT=months  
  NMONTH=12  
elif [ "${9}" == "T42" ]; then
  echo "RES" =${9}
  RES=${9}
  RERUN_INC=1
  RERUN_UNIT=months  
  NMONTH=12  
elif [ "${9}" == "T63" ]; then
  echo "RES" =${9}
  RES=${9}
  RERUN_INC=1
  RERUN_UNIT=months  
  NMONTH=12  
elif [ "${9}" == "T106" ]; then
  echo "RES" =${9}
  RES=${9}
  RERUN_INC=10
  RERUN_UNIT=days  
  NMONTH=37
  SO4=F  
elif [ "${9}" == "T213" ]; then
  echo "RES" =${9}
  RES=${9}
  RERUN_INC=10
  RERUN_UNIT=days  
  NMONTH=37  
  SO4=F  
elif [ "${9}" == "T511" ]; then
  echo "RES" =${9}
  RES=${9}
  RERUN_INC=10
  RERUN_UNIT=days
  NMONTH=37
  SO4=F
else
  RES=T31
  RERUN_INC=1
  RERUN_UNIT=months  
  NMONTH=12
fi
NRES=`echo "$RES" | cut -c2-${#RES}`

if [ -n "${10}" ]; then
  echo "LEV" =${10}
  LEV=${10}
else
  LEV=L19
fi
NLEV=`echo "$LEV" | cut -c2-${#LEV}`


ORES=O1
OCN_XNDIM_FACTOR=1
OCN_YNDIM_FACTOR=1
OCN_ZNDIM_FACTOR=40
if [ -n "${11}" ]; then
  echo "ORES" =${11}
  ORES=${11}
  if [ "${11}" == "O1" ]; then
    OCN_XNDIM_FACTOR=1
  elif [ "${11}" == "O1Z30" ]; then
    OCN_YNDIM_FACTOR=1
    OCN_ZNDIM_FACTOR=30
  elif [ "${11}" == "O1Z40" ]; then
    OCN_YNDIM_FACTOR=1
    OCN_ZNDIM_FACTOR=40
  elif [ "${11}" == "O1Z50" ]; then
    OCN_YNDIM_FACTOR=1
    OCN_ZNDIM_FACTOR=50
  elif [ "${11}" == "O1Z60" ]; then
    OCN_YNDIM_FACTOR=1
    OCN_ZNDIM_FACTOR=60
  elif [ "${11}" == "O2" ]; then
    OCN_XNDIM_FACTOR=2  
    OCN_YNDIM_FACTOR=2  
  elif [ "${11}" == "O3" ]; then
    OCN_XNDIM_FACTOR=3  
    OCN_YNDIM_FACTOR=3  
  elif [ "${11}" == "O4" ]; then
    OCN_XNDIM_FACTOR=4  
    OCN_YNDIM_FACTOR=4  
  elif [ "${11}" == "O5" ]; then
    OCN_XNDIM_FACTOR=5  
    OCN_YNDIM_FACTOR=5  
  elif [ "${11}" == "O6" ]; then
    OCN_XNDIM_FACTOR=6  
    OCN_YNDIM_FACTOR=6  
  elif [ "${11}" == "O8" ]; then
    OCN_XNDIM_FACTOR=8  
    OCN_YNDIM_FACTOR=8  
  elif [ "${11}" == "O10" ]; then
    OCN_XNDIM_FACTOR=10  
    OCN_YNDIM_FACTOR=10  
  elif [ "${11}" == "OY2" ]; then
    OCN_XNDIM_FACTOR=1  
    OCN_YNDIM_FACTOR=2  
  else
    echo "not a valid ORES option="${11}
  fi  
else
  ORES=O1
  OCN_XNDIM_FACTOR=1
  OCN_YNDIM_FACTOR=1
  OCN_ZNDIM_FACTOR=40
fi

if [ -n "${12}" ]; then
  echo "PUTDATA" =${12}
  PUTDATA=${12}
  N12m1=$(( ${#12} - 1 ))
  PUTDATA_INC=`echo "${12}" | cut -c1-${N12m1}`
  UNIT_ABB=`echo "${12}" | cut -c${#12}-${#12}`
  echo N12m1=${N12m1}, PUTDATA_INC=${PUTDATA_INC}, UNIT_ABB=${UNIT_ABB}
  if [ ${UNIT_ABB} == "m" ]; then
    PUTDATA_UNIT=months
  elif [ ${UNIT_ABB} == "d" ]; then
    PUTDATA_UNIT=days
  elif [ ${UNIT_ABB} == "h" ]; then
    PUTDATA_UNIT=hours
  elif [ ${UNIT_ABB} == "s" ]; then
    PUTDATA_UNIT=steps
  else
    echo "ERROR: undefined PUTDATA time interval =" ${12}
    echo "ERROR: job not submitted"
    exit
  fi
  PUTDATA="${PUTDATA_INC}, '${PUTDATA_UNIT}'"
  echo PUTDATA=${PUTDATA}  
###  if [ "${12}" == "1d" ]; then
###    PUTDATA="1, 'days'"
###  elif [ "${12}" == "2h" ]; then
###    PUTDATA="2, 'hours'"
###  elif [ "${12}" == "3h" ]; then
###    PUTDATA="3, 'hours'"
###  elif [ "${12}" == "6h" ]; then
###    PUTDATA="6, 'hours'"
###  elif [ "${12}" == "40mn" ]; then
###    PUTDATA="40, 'minutes'"
###  elif [ "${12}" == "2s" ]; then
###    PUTDATA="2, 'steps'"
###  elif [ "${12}" == "1s" ]; then
###    PUTDATA="1, 'steps'"
###  else
###    echo "ERROR: undefined PUTDATA time interval =" ${12}
###    echo "ERROR: job not submitted"
###    exit
###  fi
fi

if [ -n "${13}" ]; then
  echo "FYEAR" =${13}
  FYEAR=${13}
fi
if [ ${#FYEAR} -eq 1 ]; then
  FYYYY=000${FYEAR}
elif [ ${#FYEAR} -eq 2 ]; then 
  FYYYY=00${FYEAR}
elif [ ${#FYEAR} -eq 3 ]; then 
  FYYYY=0${FYEAR}
else  
  FYYYY=${FYEAR}
fi

if [ -n "${14}" ]; then
  echo "FIMM" =${14}
  FIMM=${14}
  if [ ${FIMM} -lt 10 ]
  then
    FMM=0${FIMM}
  else
    FMM=${FIMM}
  fi
fi

if [ -n "${15}" ]; then
  echo "FIDD" =${15}
  FIDD=${15}
  if [ ${FIDD} -lt 10 ]
  then
    FDD=0${FIDD}
  else
    FDD=${FIDD}
  fi
fi

if [ -n "${16}" ]; then
  echo "FIHH" =${16}
  FIHH=${16}
  if [ ${FIHH} -lt 10 ]
  then
    FHH=0${FIHH}
  else
    FHH=${FIHH}
  fi
fi

if [ -n "${17}" ]; then
  echo "LYEAR" =${17}
  LYEAR=${17}
fi

if [ -n "${18}" ]; then
  echo "LIMM" =${18}
  LIMM=${18}
  if [ ${LIMM} -lt 10 ]
  then
    LMM=0${LIMM}
  else
    LMM=${LIMM}
  fi
fi

if [ -n "${19}" ]; then
  echo "LIDD" =${19}
  LIDD=${19}
  if [ ${LIDD} -lt 10 ]
  then
    LDD=0${LIDD}
  else
    LDD=${LIDD}
  fi
fi

if [ -n "${20}" ]; then
  echo "LIHH" =${20}
  LIHH=${20}
  if [ ${LIHH} -lt 10 ]
  then
    LHH=0${LIHH}
  else
    LHH=${LIHH}
  fi
fi

if [ -n "${21}" ]; then
  echo "ACCOUNT" =${21}
  ACCOUNT=${21}
fi

if [ -n "${22}" ]; then
  echo "LEVENT" =${22}
  LEVENT=${22}
fi

if [ -n "${23}" ]; then
  echo "PUTRERUN" =${23}
  PRERUN=${23}
  N23m1=$(( ${#23} - 1 ))
  RERUN_INC=`echo "${23}" | cut -c1-${N23m1}`
  UNIT_ABB=`echo "${23}" | cut -c${#23}-${#23}`
  echo N23m1=${N23m1}, RERUN_INC=${RERUN_INC}, UNIT_ABB=${UNIT_ABB}
  if [ ${UNIT_ABB} == "m" ]; then
    RERUN_UNIT=months
  elif [ ${UNIT_ABB} == "d" ]; then
    RERUN_UNIT=days
  elif [ ${UNIT_ABB} == "h" ]; then
    RERUN_UNIT=hours
  elif [ ${UNIT_ABB} == "s" ]; then
    RERUN_UNIT=steps
  else
    echo "ERROR: undefined PUTRERUN time interval =" ${23}
    echo "ERROR: job not submitted"
    exit
  fi
  if [ "${23}" == "1m" ]; then
    LRERUN_1m=T
  else
    LRERUN_1m=F
  fi


###  if [ "${23}" == "1m" ]; then
###    RERUN_INC=1
###    RERUN_UNIT=months
###    LRERUN_1m=T
###  elif [ "${23}" == "45d" ]; then
###    RERUN_INC=45
###    RERUN_UNIT=days    
###    LRERUN_1m=F
###  elif [ "${23}" == "10d" ]; then
###    RERUN_INC=10
###    RERUN_UNIT=days    
###    LRERUN_1m=F
###  elif [ "${23}" == "7d" ]; then
###    RERUN_INC=7
###    RERUN_UNIT=days    
###    LRERUN_1m=F
###  elif [ "${23}" == "5d" ]; then
###    RERUN_INC=5
###    RERUN_UNIT=days    
###    LRERUN_1m=F
###  elif [ "${23}" == "1d" ]; then
###    RERUN_INC=1
###    RERUN_UNIT=days
###    LRERUN_1m=F
###  elif [ "${23}" == "6h" ]; then
###    RERUN_INC=6
###    RERUN_UNIT=hours
###    LRERUN_1m=F
###  elif [ "${23}" == "1000s" ]; then
###    RERUN_INC=1000
###    RERUN_UNIT=steps
###    LRERUN_1m=F
###  elif [ "${23}" == "300s" ]; then
###    RERUN_INC=300
###    RERUN_UNIT=steps
###    LRERUN_1m=F
###  elif [ "${23}" == "100s" ]; then
###    RERUN_INC=100
###    RERUN_UNIT=steps
###    LRERUN_1m=F
###  elif [ "${23}" == "10s" ]; then
###    RERUN_INC=10
###    RERUN_UNIT=steps
###    LRERUN_1m=F
###  elif [ "${23}" == "1s" ]; then
###    RERUN_INC=1
###    RERUN_UNIT=steps
###    LRERUN_1m=F
###  else
###    echo "ERROR: undefined PUTRERUN time interval =" ${23}
###    echo "ERROR: job not submitted"
###    exit
###  fi
fi

PUTRERUN="${RERUN_INC}, '${RERUN_UNIT}'"
echo PUTRERUN=${PUTRERUN}

if [ -n "${24}" ]; then
  echo "LNUDGE" =${24}
  LNUDGE=${24}
fi

if [ -n "${25}" ]; then
  echo "NOTRUN" =${25}
  NOTRUN=${25}
fi

if [ -n "${26}" ]; then
  echo "LCOLD_START" =${26}
  LCOLD_START=${26}
fi

if [ -n "${27}" ]; then
  echo "LOCN_WARMSTART" =${27}
  LOCN_WARMSTART=`echo "${27}" | cut -c1-1`
  if [ ${LOCN_WARMSTART} == "T" ]; then
    OCN_WARMSTART_DATA=`echo "${27}" | cut -c2-${#27}`
  else
    LOCN_WARMSTART=F
  fi  
fi

if [ -n "${28}" ]; then
  echo "LFORECAST" =${28}
  LFORECAST=${28}
fi

if [ -n "${29}" ]; then
  echo "Forecast_Days" =${29}
  ForecastDays=${29}
fi

if [ -n "${30}" ]; then
  echo "GID" =${30}
  GID=${30}
fi

if [ -n "${31}" ]; then
  echo "email" =${31}
  EMAIL=${31}
fi

if [ -n "${32}" ]; then
  echo "LARCHIVE" =${32}
  LARCHIVE=${32}
fi

if [ -n "${33}" ]; then
  echo "LMINSIZE" =${33}
  LMINSIZE=${33}
fi

if [ -n "${34}" ]; then
  if [ 1 -eq 1 ]; then
    echo "LMINOR" =${34}
    LMINOR=${34}
    LMINOR1=`echo "${34}" | cut -c1-1`
    echo "LMINOR1" =${LMINOR1}
    if [ "${LMINOR1}" == "T" ]; then
      MINOR=`echo "${34}" | cut -c2-${#34}`
    else
      LMINOR=F
    fi
    #echo "LMINOR" =${LMINOR}
    #echo "MINOR" =${MINOR}
    #exit
    if [ "${LMINOR1}" == "T" ]; then
      TEMPLATE=v1
    fi
  elif [ 1 -eq 1 ]; then
    echo "LPOST" =${34}
    LPOST=${34}
  elif [ ${MACHINE} == "IRISH" ]; then
    echo "HUGE_MEM" =${34}
    HUGE_MEM=${34}
  elif [ ${MACHINE} == "NUWA" ]; then
    echo "PNETCDF" =${34}
    PNETCDF=${34}    
  elif [ ${MACHINE} == "ALPS" ]; then
    echo "PNETCDF" =${34}
    PNETCDF=${34}
  fi
fi

LNWP=F
if [ -n "${35}" ]; then
  echo "LNWP" =${35}
  LNWP=${35}
fi

LP="NULL"
HIGH_CURRENT_KILLER=4
KOCN_DM0Z=1.
NCARPET=1
KCSMAG=1.2
LASIA=T
if [ 1 -eq 0 ]; then
#run at work
LDPATHWORK=T
else
LDPATHWORK=F
fi
ETOPO_NRES=1
LOPEN_BOUND=F
LALL_STRAITS=T
LSTRICT_CHANNEL=T
##declare POR_MIN=0.001
declare POR_MIN=0.01
O2A=1.
Prw=1.
KALBW=1.
CSICED=0.
if [ -n "${36}" ]; then
  echo "LP" =${36}
  LP=${36}  
  LP0=`echo "${36}" | cut -c1-1`
  REMAINS=`echo "${36}" | cut -c2-${#36}`
  while [ "${LP0}" != "NULL" ]; do
    if [ "${LP0}" == "A" ]; then
      echo "LP0" = "${LP0}"
      KALBW=`echo "${REMAINS}" | cut -c1-2`
      KALBW=`echo "scale=1;${KALBW}/10." | bc`
      REMAINS=`echo "${REMAINS}" | cut -c3-`      
      echo KALBW=${KALBW}
    elif [ "${LP0}" == "C" ]; then
      echo "LP0" = "${LP0}"
      LP1=`echo "${REMAINS}" | cut -c1-1`
      REMAINS=`echo "${REMAINS}" | cut -c2-`
      if [ "${LP1}" == "T" ]; then
        LSTRICT_CHANNEL=T
      elif [ "${LP1}" == "F" ]; then
        LSTRICT_CHANNEL=F
      else
        echo "NOT DEFINED,LP=", ${LP0}${LP1}  
        exit
      fi  
      echo LSTRICT_CHANNEL=${LSTRICT_CHANNEL}
    elif [ "${LP0}" == "E" ]; then
      echo "LP0" = "${LP0}"
      LP1=`echo "${REMAINS}" | cut -c1-1`
      REMAINS=`echo "${REMAINS}" | cut -c2-`
      if [ "${LP1}" == "1" ]; then
        ETOPO_NRES=1
      elif [ "${LP1}" == "2" ]; then
        ETOPO_NRES=2
      elif [ "${LP1}" == "5" ]; then
        ETOPO_NRES=5
      else
        echo "NOT DEFINED,LP=", ${LP0}${LP1}
        exit
      fi
      echo ETOPO_NRES=${ETOPO_NRES}
    elif [ "${LP0}" == "I" ]; then
      LP1=`echo "${REMAINS}" | cut -c1-2`
      REMAINS=`echo "${REMAINS}" | cut -c3-`
      echo "LP0" = "${LP0}""${LP1}"
      CSICED=$(echo "scale=5;${LP1}/10." |bc)
      echo CSICED=${CSICED}
    elif [ "${LP0}" == "K" ]; then
      echo "LP0" = "${LP0}"
      HIGH_CURRENT_KILLER=`echo "${REMAINS}" | cut -c1-3`
      REMAINS=`echo "${REMAINS}" | cut -c4-`      
      HIGH_CURRENT_KILLER=`echo "scale=0;${HIGH_CURRENT_KILLER}" | bc`
      echo HIGH_CURRENT_KILLER=${HIGH_CURRENT_KILLER}
    elif [ "${LP0}" == "N" ]; then
      echo "LP0" = "${LP0}"
      NCARPET=`echo "${REMAINS}" | cut -c1-1`
      REMAINS=`echo "${REMAINS}" | cut -c2-`
      if [ ${NCARPET} -ge 1 ]; then
        KOCN_DM0Z=10.
      else
        KOCN_DM0Z=0.
      fi
      echo NCARPET=${NCARPET}
    elif [ "${LP0}" == "O" ]; then
      echo "LP0" = "${LP0}"
      LP1=`echo "${REMAINS}" | cut -c1-1`
      REMAINS=`echo "${REMAINS}" | cut -c2-`
      if [ "${LP1}" == "T" ]; then
        LOPEN_BOUND=T
      elif [ "${LP1}" == "F" ]; then
        LOPEN_BOUND=F
      else
        echo "NOT DEFINED,LP=", ${LP0}${LP1}
        exit
      fi
      echo LOPEN_BOUND=${LOPEN_BOUND}
    elif [ "${LP0}" == "P" ]; then
      LP1=`echo "${REMAINS}" | cut -c1-3`
      REMAINS=`echo "${REMAINS}" | cut -c4-`
      echo "LP0" = "${LP0}""${LP1}"
      POR_MIN=$(echo "scale=5;${LP1}/1000." |bc)
      echo POR_MIN=${POR_MIN}
    elif [ "${LP0}" == "R" ]; then
      echo "LP0" = "${LP0}"
      Prw=`echo "${REMAINS}" | cut -c1-3`
      REMAINS=`echo "${REMAINS}" | cut -c4-`      
      Prw=$(echo "scale=2;${Prw}/10." |bc)
      echo Prw=${Prw}
    elif [ "${LP0}" == "S" ]; then
      echo "LP0" = "${LP0}"
      KCSMAG=`echo "${REMAINS}" | cut -c1-2`
      REMAINS=`echo "${REMAINS}" | cut -c3-`
      KCSMAG=$(echo "scale=2;${KCSMAG}/10." |bc)
      echo KCSMAG=${KCSMAG}
    elif [ "${LP0}" == "t" ]; then
      echo "LP0" = "${LP0}"
      O2A=`echo "${REMAINS}" | cut -c1-2`
      REMAINS=`echo "${REMAINS}" | cut -c3-`
      O2A=$(echo "scale=5;${O2A}/10." |bc)
      echo O2A=${O2A}
    else
      echo "NOT DEFINED,LP0=", ${LP0}${REMAINS}
      exit
    fi
    if [ ${#REMAINS} -ge 1 ]; then
      LP0=`echo "${REMAINS}" | cut -c1-1`
      REMAINS=`echo "${REMAINS}" | cut -c2-`
    else
      LP0=NULL
    fi
  done
fi

if [ 1 -eq 0 ]; then
  #run at work
  LDPATHWORK=T
else
  LDPATHWORK=F
fi

if [ -n "${37}" ]; then
  if [ "${37}" == "PW" ]; then
    LDPATHWORK=T
  fi
fi

if [ -n "${38}" ]; then
  echo "EXP_P" =${38}
  EXP_P=${38}
fi

if [ -n "${39}" ]; then
  echo "RFILE_P" =${39}
  RFILE_P=${39}
fi

if [ -n "${40}" ]; then
  echo "DPATH_P" =${40}
  DPATH_P=${40}
fi

######################
#  3.0
echo 3.0
######################


if [ ${MACHINE} == "IRISH" ]; then
  NCDUMP2="\/package\/atmo\/netcdf\/bin\/ncdump"
  SERV2="\/package\/atmo\/bin\/after"
  #COMPRESS="\/package\/atmo\/bin\/grib"
  CDO2="\/package\/atmo\/bin\/cdo"
  GRADS2="\/package\/atmo\/opengrads\/grads"  
  NCL2="\/package\/atmo\/bin\/ncl"
  SED="/bin/sed"
  QSUB="/opt/ibmll/LoadL/full/bin/llsubmit"
  QSUB2="\/opt\/ibmll\/LoadL\/full\/bin\/llsubmit"
  EHTWDIR="/u1/${ACCOUNT}/MPI/echam-5.4.00sit3"
  EHTWDIR2="\/u1\/${ACCOUNT}\/MPI\/echam-5.4.00sit3"  
  DATADIR="/tcrg/u40bjt00/data"
  DATADIR2="\/tcrg\/u40bjt00\/data"
elif [ ${MACHINE} == "NUWA" ]; then
  NCDUMP2="\/nuwa_cluster\/opt\/netcdf-4.0.1_intel-11.1\/bin\/ncdump"
  SERV2="\/usr\/local\/bin\/after"
  #COMPRESS="\/usr\/local\/bin\/grib"
  CDO2="\/usr\/local\/bin\/cdo"
  ### GRADS2="\/nuwa_cluster\/opt\/grads\/bin\/grads"
  GRADS2="\/nuwa_cluster\/opt\/grads-2.0.a7.oga.2\/Contents\/grads"
  NCL2="\/nuwa_cluster\/home\/btsuang\/software\/atmo\/bin\/ncl"
  SED="/bin/sed"
  QSUB="/opt/torque/bin/qsub"
  QSUB2="\/opt\/torque\/bin\/qsub"
  EHTWDIR="/nuwa_cluster/home/${ACCOUNT}/models/echam-5.4.00sit3"
  EHTWDIR2="\/nuwa_cluster\/home\/${ACCOUNT}\/models\/echam-5.4.00sit3"  
  DATADIR="/nuwa_cluster/home/btsuang/data"  
  DATADIR2="\/nuwa_cluster\/home\/btsuang\/data"  
elif [ ${MACHINE} == "BICEGATE" ]; then
# need to be checked  
  CDO2="\/usr\/local\/bin\/cdo"
  GRADS2="\/nuwa_cluster\/opt\/grads-2.0.a7.oga.2\/Contents\/grads"
  SED2="/bin/sed"
  QSUB="/opt/torque/bin/qsub"
  QSUB2="\/opt\/torque\/bin\/qsub"
elif [ ${MACHINE} == "ALPS" ]; then
# need to be checked  
  NCDUMP2="\/work\/u11tyh01\/package\/atmo\/netcdf_intel\/bin\/ncdump"
  SERV2="\/home\/u40bjt00\/software\/atmo\/bin\/after"
  #COMPRESS="\/pkg\/atmo\/bin\/grib"
  CDO2="\/pkg\/atmo\/bin\/cdo"
  GRADS2="\/pkg\/atmo\/grads\/opengrads\/grads"
  #NCL2="\/pkg\/atmo\/ncarg\/bin\/ncl"
  NCL2="\/home\/j07hsu00\/opt\/ncl_ncarg-6.1.0\/bin\/ncl"
  SED="/bin/sed"
  QSUB="/pkg/lsf/cluster5/7.0/linux2.6-glibc2.3-x86_64/bin/bsub"
  QSUB2="\/pkg\/lsf\/cluster5\/7.0\/linux2.6-glibc2.3-x86_64\/bin\/bsub"
  EHTWDIR="/home/${ACCOUNT}/ehtw"
  EHTWDIR2="\/home\/${ACCOUNT}\/ehtw"
  DATADIR="/home/u40bjt00/data"
  DATADIR2="\/home\/u40bjt00\/data"
elif [ ${MACHINE} == "CWB" ]; then
# need to be checked
  NCDUMP2="\/package\/netcdf-3.6.0-p1_64\/bin\/ncdump"
  CDO2="\/nwpr\/cfsres\/b258\/btsuang\/cdo-1.5.3\/bin\/cdo"
  SERV2="\/nwpr\/cfsres\/b258\/btsuang\/afterburner-4.7.0\/bin\/after"
  GRADS2="\/package\/grads-2.0.1\/bin\/grads"
  #GRADS="\/package\/grads-1.8sl11\/bin\/grads"
  NCL2="\/package\/ncl_ncarg-6.0.0\/bin\/ncl"
  SED="/nwpr/cfsres/b258/sed-4.2.1/bin/sed"
  QSUB="/usr/lpp/LoadL/full/bin/llsubmit"
  QSUB2="\/usr\/lpp\/LoadL\/full\/bin\/llsubmit"
  EHTWDIR="/nwpr/cfsres/${ACCOUNT}/ehtw-1.0.00"
  EHTWDIR2="\/nwpr\/cfsres\/${ACCOUNT}\/ehtw-1.0.00"  
  DATADIR="/nwpr/cfsres/b258/data"
  DATADIR2="\/nwpr\/cfsres\/b258\/data"
fi

################################################################################
# load functions
######################
source ${EHTWDIR}/mine/v${VER}/julianday_functions.sh
source ${EHTWDIR}/mine/v${VER}/remote_file_functions.sh
######################

EXPID0=${TYPE}

if [ "${LRERE}" == "T" ]; then
  EXPID0=r${PRERUN}${EXPID0}
fi

if [ "${LNUDGE}" == "T" ]; then                # nudging run
  EXPID0=n${EXPID0}
  NDGDATA=1
  LNUDGE=T              
elif [ "${LNUDGE}" == "T1" ]; then                # nudging run
  EXPID0=n1${EXPID0}
  NDGDATA=1                 
  LNUDGE=T              
elif [ "${LNUDGE}" == "T2" ]; then                # nudging run
  EXPID0=n2${EXPID0}
  NDGDATA=2                 
  LNUDGE=T              
elif [ "${LNUDGE}" == "T3" ]; then                # nudging run
  EXPID0=n3${EXPID0}
  NDGDATA=3                 
  LNUDGE=T              
elif [ "${LNUDGE}" == "T4" ]; then                # nudging run
  EXPID0=n4${EXPID0}
  NDGDATA=4
  LNUDGE=T
elif [ "${LNUDGE}" == "T5" ]; then                # nudging run
  EXPID0=n5${EXPID0}
  NDGDATA=5
  LNUDGE=T
else
  NDGDATA=0                 
  LNUDGE=F              
fi

if [ "${LSIT}" == "T" ]; then
  if [ ${SFC_NUDG} != "nn" ] || [ ${UPP_NUDG} != "nn" ] || [ ${DEEP_NUDG} != "nn" ] || [ "${LSICE_NUDG}" != "F" ]; then
    EXPID0="${EXPID0}${SFC_NUDG}${UPP_NUDG}${DEEP_NUDG}${LSICE_NUDG}"
  fi
fi

if [ "${LSIT}" == "T" ] && [ "${LOCN_WARMSTART}" == "T" ]; then
  EXPID0=w${OCN_WARMSTART_DATA}${EXPID0}                                          # ocn_warmstart run
fi

if [ "${LP}" != "P0" ] && [ "${LP}" != "NULL" ]; then
  EXPID0=${EXPID0}${LP}
fi

if [ "${LOCN}" == "T" ]; then
  EXPNAME0=${EXPID0}.${RES}${ORES}.${VER}${MINOR}                # OCN run
elif [ "${LSIT}" == "T" ]; then  
  EXPNAME0=${EXPID0}.${RES}.${VER}${MINOR}                    # SIT run
else
  EXPNAME0=${EXPID0}.${RES}${LEV}.${VER}${MINOR}                 # AMIP/CLIM run
fi

#if [ ${LFUTURE} == T ]; then
if [ "${GID}" != "" ]; then
  EXPNAME0=${EXPNAME0}.${GID}                                   # OCN run
fi

EXPID=${PF}${EXPID0}
EXPNAME=${PF}${EXPNAME0}                                     
###EXP_P=p${EXPID0}.${RES}${ORES}.${VER}                         # original present name


if [ ${LFUTURE} == T ]; then
  # last nudging day= fday+1 
  cal_julianday ${FYEAR} ${FIMM} ${FIDD}
  julianday_add_day ${outjd} 1
  NYEAR=${outiy}
  NIMM=${outim}
  NIDD=${outid} 
else
  NYEAR=${LYEAR}
  NIMM=${LIMM}
  NIDD=${LIDD}
fi


#EXP=`echo $EXPNAME | cut -c1-19`
EXP=`echo $EXPNAME | cut -c1-50`
echo "EXP" =${EXP}
#RE=`echo $RENAME | cut -c1-19`
###RE=`echo ${EXP_P} | cut -c1-25`
###echo "RE" =${RE}
DDIR0="${EXPNAME}.${FYYYY}${FMM}${FDD}${FHH}"

if [ "${GID}" != "" ]; then
###if [ ${GID} -ne "" ]; then
  DDIR="${GID}/${DDIR0}"    
  DDIR2="${GID}\/${DDIR0}"
  DDIR_P="${GID}\/${DPATH_P}"
else
  DDIR="${RES}/${DDIR0}"
  DDIR2="${RES}\/${DDIR0}"
  DDIR_P="${RES}\/${DPATH_P}"
fi
DDIR="exp/${DDIR}" 
DDIR2="exp\/${DDIR2}"
DDIR_P="exp\/${DDIR_P}"

if [ ${MACHINE} == "IRISH" ]; then
  DPATHDIR="/tcrg/work/${ACCOUNT}/${DDIR}"
  DPATHDIR2="\/tcrg\/work\/${ACCOUNT}\/${DDIR2}"
elif [ ${MACHINE} == "NUWA" ]; then
  DPATHDIR="/nuwa_work/scratch/${ACCOUNT}/${DDIR}"
  DPATHDIR2="\/nuwa_work\/scratch\/${ACCOUNT}\/${DDIR2}"
elif [ ${MACHINE} == "ALPS" ]; then
  if [ "${LDPATHWORK}" == "T" ]; then
    DPATHDIR="/work/${ACCOUNT}/${DDIR}"
    DPATHDIR2="\/work\/${ACCOUNT}\/${DDIR2}"
    HOMEPATHDIR="/home/${ACCOUNT}/${DDIR}"
    HOMEPATHDIR2="\/home\/${ACCOUNT}\/${DDIR2}"
  elif [ 1 -eq 1 ]; then
    DPATHDIR="/home/${ACCOUNT}/${DDIR}"
    DPATHDIR2="\/home\/${ACCOUNT}\/${DDIR2}"
    HOMEPATHDIR="/home/${ACCOUNT}/${DDIR}"
    HOMEPATHDIR2="\/home\/${ACCOUNT}\/${DDIR2}"
  else
  # write permission error
    DPATHDIR="/work3/${ACCOUNT}/${DDIR}"
    DPATHDIR2="\/work3\/${ACCOUNT}\/${DDIR2}"
  fi
elif [ ${MACHINE} == "CWB" ]; then
  DPATHDIR="/cfs/b258/${DDIR}"
  DPATHDIR2="\/cfs\/b258\/${DDIR2}"
else
  DPATHDIR="${HOME}/${DDIR}"
  DPATHDIR2="${HOME}\/${DDIR2}"
fi

echo DPATHDIR=${DPATHDIR}
echo HOMEPATHDIR=${HOMEPATHDIR}

if [ ${ARCHIVE} == "TCRG" ]; then
# TCRG ARCHIVE
  REMOTE="u40bjt00@140.110.122.63"
  PERMDIR2="\/tcrg\/archive\/u40bjt00\/${DDIR2}"
  PERMDIR3="/tcrg/archive/u40bjt00/${DDIR}"
  PERMDIR=${REMOTE}:${PERMDIR2}
  PERM_DPATH_P="\/tcrg\/archive\/u40bjt00\/${DDIR_P}"
  ARCHIVE_METHOD="RSYNC"
elif [ ${ARCHIVE} == "ALPS" ]; then
# ALPS WORK
  REMOTE="u40bjt00@alps.nchc.org.tw"
  PERMDIR2="\/work\/u40bjt00\/${DDIR2}"
  PERMDIR3="/work/u40bjt00/${DDIR}"
  PERMDIR=${REMOTE}:${PERMDIR2}
  PERM_DPATH_P="\/work\/u40bjt00\/${DDIR_P}"
  PERMDATA2="\/work\/u40bjt00\/data"
  PERMDATA3="/work/u40bjt00/data"
  PERMDATA=${REMOTE}:${PERMDATA2}
  ARCHIVE_METHOD="RSYNC"
elif [ ${ARCHIVE} == "ST" ]; then
# CLOUD
  REMOTE="st.nchc.tw:2122"
  PERMDIR2="\/home\/u40bjt00\/${DDIR2}"
  PERMDIR3="/home/u40bjt00/${DDIR}"
  PERMDIR=${REMOTE}${PERMDIR2}
  PERM_DPATH_P="\/home\/u40bjt00\/${DDIR_P}"
  PERMDATA2="\/home\/u40bjt00\/data"
  PERMDATA3="/home/u40bjt00/data"
  PERMDATA=${REMOTE}:${PERMDATA2}
  ARCHIVE_METHOD="LFTP"
else
# CLOUD
  REMOTE="st.nchc.tw:2122"
  PERMDIR2="\/home\/u40bjt00\/${DDIR2}"
  PERMDIR3="/home/u40bjt00/${DDIR}"
  PERMDIR=${REMOTE}${PERMDIR2}
  ARCHIVE_METHOD="LFTP"  
fi

echo "DPATHDIR" =${DPATHDIR}
#-----------------------------------------------------------------------------
# the directory for the experiment will be created, if not already there
DIRS="run rerun hour daily mon yhour ydaily ymon year wrf"
for DIR in $DIRS; do
  if [ ! -d ${DPATHDIR}/${DIR} ]; then
    mkdir -p ${DPATHDIR}/${DIR}
  fi
done


GIDmkdir=`echo ${HOMEPATHDIR} | cut -d '/' -f 1-5`
if [ ! -d ${GIDmkdir} ]; then
  mkdir -p ${GIDmkdir}
fi

if [ ${MACHINE} == "ALPS" ]; then
  mkdir -p /work/${ACCOUNT}/${DDIR}/trash
  if [ ! -d ${HOMEPATHDIR} ]; then
    ln -s ${DPATHDIR} ${HOMEPATHDIR}
  else
    ln -s /work/${ACCOUNT}/${DDIR}/trash ${HOMEPATHDIR}/
  fi
else
  echo "FATAL"
  echo "mkdir -p /work/${ACCOUNT}/${DDIR}/trash" should be implemented
  exit
fi

#-----------------------------------------------------------------------------
# generate QUEUE for rsync_archive.template
#
###if [ ${MACHINE} == "IRISH" ]; then
###  QUEQE_JOB="#@job_name         = j2_pobnnnn.T31O1.9.5"
###  QUEQE_STEP="#@step_name        = $(job_name)_01"
###  QUEQE_OUT="#@output           = $(step_name).$(jobid).log"
###  QUEQE_ERR="#@error            = $(output)"
###  QUEUE="#@class = serial\n#@ tasks_per_node = 1"
###  QUEQE_NODE="#@node = 1"
###  QUEQE_OTHERS="#@ notification     = error\n#@ node_usage       = shared\#@ job_type         = parallel\#@ wall_clock_limit = 96:0:0,95:50:0"
###  QUEQE_END="#@queue"
###elif [ ${MACHINE} == "NUWA" ]; then
###  echo "QUEUE: under construction (on demand)"
###elif [ ${MACHINE} == "ALPS" ]; then
###  QUEQE_JOB="#BSUB -J rsync_archive"
###  QUEQE_STEP=""
###  QUEQE_OUT="#BSUB -o %J.out"
###  QUEQE_ERR="#BSUB -e %J.err"
###  QUEUE="#BSUB -q serial"
###  QUEQE_NODE="#BSUB -n 1"
###  QUEQE_OTHERS="#BSUB -R 'span[ptile=12]'"
###  QUEQE_END=""
###elif [ ${MACHINE} == "CWB" ]; then
###  echo "QUEUE: under construction (on demand)"
###fi

if [ ${MACHINE} == "IRISH" ]; then
  JOB2_QUEQE="#@job_name=j2\n#@step_name=$(job_name)_01\n#@output=$(step_name).$(jobid).log\n#@error=$(output)\n#@class = serial\n#@ tasks_per_node = 1\n#@node = 1\n#@notification=error\n#@ node_usage= shared\#@ job_type= parallel\#@ wall_clock_limit = 96:0:0,95:50:0\n#@queue"
  RUN_JOB2_QUEUE="#@job_name=j2batch\n#@step_name=$(job_name)_01\n#@output=$(step_name).$(jobid).log\n#@error=$(output)\n#@class = serial\n#@ tasks_per_node = 1\n#@node = 1\n#@notification=error\n#@ node_usage= shared\#@ job_type= parallel\#@ wall_clock_limit = 96:0:0,95:50:0\n#@queue"
  RSYNC_ARCHIVE_QUEQE="#@job_name=rsync_archive\n#@step_name=$(job_name)_01\n#@output=$(step_name).$(jobid).log\n#@error=$(output)\n#@class = serial\n#@ tasks_per_node = 1\n#@node = 1\n#@notification=error\n#@ node_usage= shared\#@ job_type= parallel\#@ wall_clock_limit = 96:0:0,95:50:0\n#@queue"
elif [ ${MACHINE} == "NUWA" ]; then
  echo "QUEUE: NUWA"
  JOB2_QUEQE="#PBS -N j2_${EXP}\n#PBS -q serial\n#PBS -l nodes=1:ppn=1\n#PBS -l walltime=48:00:00\n#PBS -o nuwahn:\$PBS_O_WORKDIR\/\$PBS_JOBNAME.\$PBS_JOBID.out\n#PBS -e nuwahn:\$PBS_O_WORKDIR\/\$PBS_JOBNAME.\$PBS_JOBID.err\n#PBS -m abe\n#PBS -M btsuang@gmail.com\n## Export all my environment variables to the job\n#PBS -V\nexport PBS_SERVER=nuwahn\nmodule() { eval \`\/opt\/modules\/Modules\/\$MODULE_VERSION\/bin\/modulecmd sh \$*\`; }\nmodule purge\nmodule load intel\/icc-10.1 intel\/ifort-11.1 intel\/mkl-11.1\nmodule load openmpi-1.3.3\/intel-11.1\nmodule load netcdf-4.0.1\/intel-11.1" 
  RUN_JOB2_QUEUE="#PBS -N runj2_${EXP}\n#PBS -q serial\n#PBS -l nodes=1:ppn=1\n#PBS -l walltime=48:00:00\n#PBS -o nuwahn:\$PBS_O_WORKDIR\/\$PBS_JOBNAME.\$PBS_JOBID.out\n#PBS -e nuwahn:\$PBS_O_WORKDIR\/\$PBS_JOBNAME.\$PBS_JOBID.err\n#PBS -m abe\n#PBS -M btsuang@gmail.com\n## Export all my environment variables to the job\n#PBS -V\nexport PBS_SERVER=nuwahn\nmodule() { eval \`\/opt\/modules\/Modules\/\$MODULE_VERSION\/bin\/modulecmd sh \$*\`; }\nmodule purge\nmodule load intel\/icc-10.1 intel\/ifort-11.1 intel\/mkl-11.1\nmodule load openmpi-1.3.3\/intel-11.1\nmodule load netcdf-4.0.1\/intel-11.1" 
  RSYNC_ARCHIVE_QUEQE="#PBS -N rsync_archive_${EXP}\n#PBS -q serial\n#PBS -l nodes=1:ppn=1\n#PBS -l walltime=48:00:00\n#PBS -o nuwahn:\$PBS_O_WORKDIR\/\$PBS_JOBNAME.\$PBS_JOBID.out\n#PBS -e nuwahn:\$PBS_O_WORKDIR\/\$PBS_JOBNAME.\$PBS_JOBID.err\n#PBS -m abe\n#PBS -M btsuang@gmail.com\n## Export all my environment variables to the job\n#PBS -V\nexport PBS_SERVER=nuwahn\nmodule() { eval \`\/opt\/modules\/Modules\/\$MODULE_VERSION\/bin\/modulecmd sh \$*\`; }\nmodule purge\nmodule load intel\/icc-10.1 intel\/ifort-11.1 intel\/mkl-11.1\nmodule load openmpi-1.3.3\/intel-11.1\nmodule load netcdf-4.0.1\/intel-11.1" 
  YDAILY_QUEQE="#PBS -N ydaily_${EXP}\n#PBS -q serial\n#PBS -l nodes=1:ppn=1\n#PBS -l walltime=48:00:00\n#PBS -o nuwahn:\$PBS_O_WORKDIR\/\$PBS_JOBNAME.\$PBS_JOBID.out\n#PBS -e nuwahn:\$PBS_O_WORKDIR\/\$PBS_JOBNAME.\$PBS_JOBID.err\n#PBS -m abe\n#PBS -M btsuang@gmail.com\n## Export all my environment variables to the job\n#PBS -V\nexport PBS_SERVER=nuwahn\nmodule() { eval \`\/opt\/modules\/Modules\/\$MODULE_VERSION\/bin\/modulecmd sh \$*\`; }\nmodule purge\nmodule load intel\/icc-10.1 intel\/ifort-11.1 intel\/mkl-11.1\nmodule load openmpi-1.3.3\/intel-11.1\nmodule load netcdf-4.0.1\/intel-11.1" 
elif [ ${MACHINE} == "ALPS" ]; then
  if [ ${QUEUE} == "monos10" ]; then
    ###SQUEUE="monos10"
    SQUEUE="serial"
  else
    ###SQUEUE="monos10"
    SQUEUE="serial"
  fi
  if [ "${GID}" == "" ]; then
    GRP="${DDIR0}"
  else
    GRP="${GID}"
  fi
#  JOB2_QUEQE="#BSUB -q ${SQUEUE}\n#BSUB -n 1\n#BSUB -x\n#BSUB -J j2\n#BSUB -o j2.%J.out\n#BSUB -e j2.%J.out\n#BSUB -R 'span[ptile=12]'"
#  RUN_JOB2_QUEUE="#BSUB -q ${SQUEUE}\n#BSUB -n 1\n#BSUB -x\n#BSUB -J j2batch\n#BSUB -o j2batch.%J.out\n#BSUB -e j2batch.%J.out\n#BSUB -R 'span[ptile=12]'"
#  RSYNC_ARCHIVE_QUEQE="#BSUB -q ${SQUEUE}\n#BSUB -n 1\n#BSUB -x\n#BSUB -J rsync_archive\n#BSUB -o rsync_archive.%J.out\n#BSUB -e rsync_archive.%J.out\n#BSUB -R 'span[ptile=12]'"
#  JOB2_QUEQE="#BSUB -q ${SQUEUE}\n#BSUB -n 1\n#BSUB -J j2_${EXP}\n#BSUB -o j2.%J.out\n#BSUB -e j2.%J.out\n#BSUB -R 'span[ptile=12]'"
  JOB2_QUEQE="#BSUB -q ${SQUEUE}\n#BSUB -n 1\n#BSUB -J j2_${EXP}\n#BSUB -g \/${GRP}\n#BSUB -o j2.%J.out\n#BSUB -e j2.%J.out"
  RUN_JOB2_QUEUE="#BSUB -q ${SQUEUE}\n#BSUB -n 1\n#BSUB -J runj2_${EXP}\n#BSUB -g \/${GRP}\n#BSUB -o run_job2.%J.out\n#BSUB -e run_job2.%J.out"
  YMON_BATCH_QUEQE="#BSUB -q ${SQUEUE}\n#BSUB -n 1\n#BSUB -J ymon_${EXP}\n#BSUB -g \/${GRP}\n#BSUB -o ymon.%J.out\n#BSUB -e ymon.%J.out"
  RSYNC_ARCHIVE_QUEQE="#BSUB -q ${SQUEUE}\n#BSUB -n 1\n#BSUB -J rsync_archive_${EXP}\n#BSUB -g \/${GRP}\n#BSUB -o rsync_archive.%J.out\n#BSUB -e rsync_archive.%J.out"
  if [ ${LPtile24} == T ]; then
    QUEQE_OPTIONS="#BSUB -R 'span[ptile=24]'\n#BSUB -R 'ipathavail==0'\n#BSUB -R 'rusage[ipathres=1]'"
  elif [ ${LPtile48} == T ]; then
    QUEQE_OPTIONS="#BSUB -R 'span[ptile=48]'\n#BSUB -R 'ipathavail==0'\n#BSUB -R 'rusage[ipathres=1]'"
  elif [ 1 -eq 1 ]; then
  ### if [ ${NODE} -gt 48 ]; then
  # In ALPS, only 192cpu queue and above allow #BSUB -x. "#BSUB -x" close connection from other users.
  # It can prevent jobs from crash, while other jobs want to use the infinity band during pre/post processing of the submitted job. 
  #  QUEQE_OPTIONS="#BSUB -x\n#BSUB -R 'span[ptile=24]'\n#BSUB -R 'ipathavail==0'" 
  #  QUEQE_OPTIONS="#BSUB -x\n#BSUB -R 'span[ptile=24]'\n#BSUB -R 'ipathavail==0'\n#BSUB -R 'rusage[ipathres=1]'"
    QUEQE_OPTIONS="#BSUB -R 'ipathavail==0'\n#BSUB -R 'rusage[ipathres=1]'"     
  else
    QUEQE_OPTIONS="#BSUB -R 'span[ptile=${NODE}]'"
  fi  
  YDAILY_QUEQE="#BSUB -q ${SQUEUE}\n#BSUB -n 1\n#BSUB -J ydaily_${EXP}\n#BSUB -o ydaily.%J.out\n#BSUB -e ydaily.%J.out"
elif [ ${MACHINE} == "CWB" ]; then
  echo "QUEUE: under construction (on demand)"
fi
######################
#  4.0
echo 4.0
######################
#-----------------------------------------------------------------------------

cat > sed.script << EOF
# sed comment - This script changes lower case vowels to upper case
s/(SQUEUE_template)/${SQUEUE}/g
s/(QUEUE_template)/${QUEUE}/g
s/(NODE_template)/${NODE}/g
s/(QUEQE_OPTIONS_template)/${QUEQE_OPTIONS}/g
s/(YDAILY_QUEQE_template)/${YDAILY_QUEQE}/g
s/(TASKS_PER_NODE_template)/${TASKS_PER_NODE}/g
s/(NPROCA_template)/${NPROCA}/g
s/(NPROCB_template)/${NPROCB}/g
s/(EXPID_template)/${EXPID}/g
s/(LPRESENT_template)/${LPRESENT}/g
s/(LFUTURE_template)/${LFUTURE}/g
s/(LPERS_SST_template)/${LPERS_SST}/g
s/(LAMIP_template)/${LAMIP}/g
s/(LSIT_template)/${LSIT}/g
s/(LOCN_template)/${LOCN}/g
s/(LWOA0_template)/${LWOA0}/g
s/(LGODAS_template)/${LGODAS}/g
s/(SOCN_RESTORE_TIME_template)/${SOCN_RESTORE_TIME}/g
s/(UOCN_RESTORE_TIME_template)/${UOCN_RESTORE_TIME}/g
s/(DOCN_RESTORE_TIME_template)/${DOCN_RESTORE_TIME}/g
s/(SSIT_RESTORE_TIME_template)/${SSIT_RESTORE_TIME}/g
s/(USIT_RESTORE_TIME_template)/${USIT_RESTORE_TIME}/g
s/(DSIT_RESTORE_TIME_template)/${DSIT_RESTORE_TIME}/g
s/(LDEBUG_template)/${LDEBUG}/g
s/(LOCAF_template)/${LOCAF}/g
s/(RES_template)/${RES}/g
s/(LEV_template)/${LEV}/g
s/(EXP_template)/${EXP}/g
s/(LINFINIBAND_template)/${LINFINIBAND}/g
s/(VER_template)/${VER}/g
s/(OCN_COUPLE_OPTION_template)/${OCN_COUPLE_OPTION}/g
s/(HIGH_CURRENT_KILLER_template)/${HIGH_CURRENT_KILLER}/g
s/(IPCC_template)/${IPCC}/g
s/(SO4_template)/${SO4}/g
s/(ICE_OPTION_template)/${ICE_OPTION}/g
s/(EXPNAME_template)/${EXPNAME}/g
s/(DDIR0_template)/${DDIR0}/g
s/(LOCN_MSG_template)/${LOCN_MSG}/g
s/(LOPEN_BOUND_template)/${LOPEN_BOUND}/g
s/(LALL_STRAITS_template)/${LALL_STRAITS}/g
s/(LSTRICT_CHANNEL_template)/${LSTRICT_CHANNEL}/g
s/(LSIT_ICE_template)/${LSIT_ICE}/g
s/(LSICE_NUDG_template)/${LSICE_NUDG}/g
s/(LSIT_LW_template)/${LSIT_LW}/g
s/(ORES_template)/${ORES}/g
s/(ACCOUNT_template)/${ACCOUNT}/g
s/(IGHG_template)/${IGHG}/g
s/(PUTRERUN_template)/${PUTRERUN}/g
s/(RERUN_INC_template)/${RERUN_INC}/g
s/(RERUN_UNIT_template)/${RERUN_UNIT}/g
s/(LRERUN_1m_template)/${LRERUN_1m}/g
s/(NMONTH_template)/${NMONTH}/g
s/(FYEAR_template)/${FYEAR}/g
s/(FIMM_template)/${FIMM}/g
s/(FIDD_template)/${FIDD}/g
s/(FIHH_template)/${FIHH}/g
s/(LYEAR_template)/${LYEAR}/g
s/(LIMM_template)/${LIMM}/g
s/(LIDD_template)/${LIDD}/g
s/(LIHH_template)/${LIHH}/g
s/(LEVENT_template)/${LEVENT}/g
s/(LMINOR_template)/${LMINOR}/g
s/(LMINOR1_template)/${LMINOR1}/g
s/(MINOR_template)/${MINOR}/g
s/(LNUDGE_template)/${LNUDGE}/g
s/(OCN_XNDIM_FACTOR_template)/${OCN_XNDIM_FACTOR}/g
s/(OCN_YNDIM_FACTOR_template)/${OCN_YNDIM_FACTOR}/g
s/(OCN_ZNDIM_FACTOR_template)/${OCN_ZNDIM_FACTOR}/g
s/(WALLTIME_template)/${WALLTIME}/g
s/(DPATHDIR_template)/${DPATHDIR2}/g
s/(EHTWDIR_template)/${EHTWDIR2}/g
s/(DATADIR_template)/${DATADIR2}/g
s/(MASKID_template)/${MASKID_OPTION}/g
s/(NYEAR_template)/${NYEAR}/g
s/(NIMM_template)/${NIMM}/g
s/(NIDD_template)/${NIDD}/g
s/(LSSST_template)/${LSSST}/g
s/(EMAIL_template)/${EMAIL}/g
s/(TRIGSIT_template)/${TRIGSIT}/g
s/(TRIGOCN_template)/${TRIGOCN}/g
s/(NOBOX_NUDG_template)/${nobox_nudg}/g
s/(OBOX_RESTORE_TIME_template)/${obox_ndg_restore_time}/g
s/(OBOX_NUDG_FLAG_template)/${obox_nudg_flag}/g
s/(OBOX_NUDG_W_template)/${obox_nudg_w}/g
s/(OBOX_NUDG_E_template)/${obox_nudg_e}/g
s/(OBOX_NUDG_N_template)/${obox_nudg_n}/g
s/(OBOX_NUDG_S_template)/${obox_nudg_s}/g
s/(LHD_template)/${LHD}/g
s/(LOCN_WARMSTART_template)/${LOCN_WARMSTART}/g
s/(OCN_WARMSTART_DATA_template)/${OCN_WARMSTART_DATA}/g
s/(WALLTIME_HH_template)/${WALLTIME_HH}/g
s/(PMEM_template)/${PMEM}/g
s/(TRASH_template)/${TRASH}/g
s/(REMOTE_template)/${REMOTE}/g
s/(PERMDIR_template)/${PERMDIR}/g
s/(PERMDIR2_template)/${PERMDIR2}/g
s/(PERMDATA_template)/${PERMDATA}/g
s/(PERMDATA2_template)/${PERMDATA2}/g
s/(PNETCDF_template)/${PNETCDF}/g
s/(OCN_DOMAIN_W_template)/${ocn_domain_w}/g
s/(OCN_DOMAIN_E_template)/${ocn_domain_e}/g
s/(OCN_DOMAIN_N_template)/${ocn_domain_n}/g
s/(OCN_DOMAIN_S_template)/${ocn_domain_s}/g
s/(LARCHIVE_template)/${LARCHIVE}/g
s/(LMINSIZE_template)/${LMINSIZE}/g
s/(SSTDATA_template)/${SSTDATA}/g
s/(WOADATA_template)/${WOADATA}/g
s/(NCDUMP_template)/${NCDUMP2}/g
s/(CDO_template)/${CDO2}/g
s/(SERV_template)/${SERV2}/g
s/(GRADS_template)/${GRADS2}/g
s/(NCL_template)/${NCL2}/g
s/(QSUB_template)/${QSUB2}/g
s/(NDGDATA_template)/${NDGDATA}/g
s/(LFORECAST_template)/${LFORECAST}/g
s/(ForecastDays_template)/${ForecastDays}/g
s/(GID_template)/${GID}/g
s/(PUTDATA_template)/${PUTDATA}/g
s/(PUTDATA_INC_template)/${PUTDATA_INC}/g
s/(PUTDATA_UNIT_template)/${PUTDATA_UNIT}/g
s/(LASIA_template)/${LASIA}/g
s/(KOCN_DM0Z_template)/${KOCN_DM0Z}/g
s/(NCARPET_template)/${NCARPET}/g
s/(KCSMAG_template)/${KCSMAG}/g
s/(Prw_template)/${Prw}/g
s/(KALBW_template)/${KALBW}/g
s/(1_template)/${1}/g
s/(2_template)/${2}/g
s/(3_template)/${3}/g
s/(4_template)/${4}/g
s/(5_template)/${5}/g
s/(6_template)/${6}/g
s/(7_template)/${7}/g
s/(8_template)/${8}/g
s/(9_template)/${9}/g
s/(10_template)/${10}/g
s/(11_template)/${11}/g
s/(12_template)/${12}/g
s/(13_template)/${13}/g
s/(14_template)/${14}/g
s/(15_template)/${15}/g
s/(16_template)/${16}/g
s/(17_template)/${17}/g
s/(18_template)/${18}/g
s/(19_template)/${19}/g
s/(20_template)/${20}/g
s/(21_template)/${21}/g
s/(22_template)/${22}/g
s/(23_template)/${23}/g
s/(24_template)/${24}/g
s/(25_template)/${25}/g
s/(26_template)/${26}/g
s/(27_template)/${27}/g
s/(28_template)/${28}/g
s/(29_template)/${29}/g
s/(30_template)/${30}/g
s/(31_template)/${31}/g
s/(32_template)/${32}/g
s/(33_template)/${33}/g
s/(34_template)/${34}/g
s/(35_template)/${35}/g
s/(36_template)/${36}/g
s/(37_template)/${37}/g
###s/(38_template)/${38}/g
###s/(39_template)/${39}/g
###s/(40_template)/${40}/g
s/(LARCHIVE_template)/${LARCHIVE}/g
s/(LNWP_template)/${LNWP}/g
s/(LP_template)/${LP}/g
s/(MACHINE_template)/${MACHINE}/g
s/(NRES_template)/${NRES}/g
s/(NLEV_template)/${NLEV}/g

s/(JOB2_QUEQE_template)/${JOB2_QUEQE}/g
s/(RUN_JOB2_QUEUE_QUEQE_template)/${RUN_JOB2_QUEUE}/g
s/(RSYNC_ARCHIVE_QUEQE_template)/${RSYNC_ARCHIVE_QUEQE}/g
s/(YMON_BATCH_QUEQE_template)/${YMON_BATCH_QUEQE}/g
s/(ARCHIVE_template)/${ARCHIVE_METHOD}/g
s/(LRCP85_template)/${LRCP85}/g
s/(CLIMATE_SST_template)/${CLIMATE_SST}/g
# the present Caspian Sea Level (CSL) is about -27m and during the medieval time it was about -30m
s/(CSL_template)/${CSL}/g
s/(ETOPO_NRES_template)/${ETOPO_NRES}/g
s/(POR_MIN_template)/${POR_MIN}/g
s/(CSICED_template)/${CSICED}/g
s/(O2A_template)/${O2A}/g
EOF
######################
#  5.0 Generate ${EXPNAME}.sh
echo 5.0 Generate ${EXPNAME}.sh
######################
if [ ${MACHINE} == "IRISH" ]; then
  ${SED} -f sed.script template_irish.sh > xx.1
elif [ ${MACHINE} == "NUWA" ]; then
  ${SED} -f sed.script template_nuwa.sh > xx.1
elif [ ${MACHINE} == "ALPS" ]; then
  if [ ${TEMPLATE} == "v0" ]; then
    ${SED} -f sed.script template_alps.sh > xx.1
  else
    ${SED} -f sed.script template_alps.sh > xx.1
  fi
elif [ ${MACHINE} == "CWB" ]; then
  ${SED} -f sed.script template_cwb.sh > xx.1
fi
if [ ${IGHG} -eq 0 ]; then
  ${SED} -e "s/(RAD_template)/\&radctl\n\//" xx.1 > xx.2
else
  if [ ${RES} == "T31" ]; then
    ${SED} -e "s/(RAD_template)/\&radctl\n\tIGHG=${IGHG},\n\tICO2=4,\n\tICH4=4,\n\tIN2O=4,\n\tICFC=4,\n\tIAERO=4,\n\tLGADSRH=T,\n\tNDFAER=11,\n\tIO3=4,\n\//" xx.1 > xx.2
  else
    ${SED} -e "s/(RAD_template)/\&radctl\n\tIGHG=${IGHG},\n\tICO2=4,\n\tICH4=4,\n\tIN2O=4,\n\tICFC=4,\n\tIAERO=4,\n\tLGADSRH=T,\n\tNDFAER=11,\n\//" xx.1 > xx.2
  fi
fi
if [ "${HUGE_MEM}" == "T" ]; then
  ${SED} -e "s/(MEMORY_template)/#@ requirements = (Memory >=32000)/" xx.2 > xx.3
else
  ${SED} -e "s/(MEMORY_template)/###@ requirements = (Memory >=32000)/" xx.2 > xx.3
fi
echo in: DPATH_P=${DPATH_P}
echo in: DPATH_P_2=${DPATH_P_2}
#DPATH_P_2=`echo ${DPATH_P} | ${SED} -e "s/\//\\\\\//g" -`
DPATH_P_2=`echo ${DPATH_P} | ${SED} -e "s/\//\\\\\\\\\//g" -`
echo o1: DPATH_P=${DPATH_P}
echo o1: DPATH_P_2=${DPATH_P_2}
${SED} -e "s/(DPATH_P_template)/'${DPATH_P_2}'/" xx.3 > ${DPATHDIR}/run/${EXPNAME}.sh
echo o2: DPATH_P=${DPATH_P}
echo o2: DPATH_P_2=${DPATH_P_2}

######################
#  6.0 Generate job2, job2_batch.sh, ymon.sh, rerun_${EXP}_echam.ctl, hour.sh, plot.template.sh
echo 6.0 Generate job2
######################
${SED} -f sed.script job1.template > ${DPATHDIR}/run/job1

if [ ${MACHINE} == "CWB" ]; then
  ${SED} -f sed.script job2.cwb > ${DPATHDIR}/run/job2
else
  ${SED} -f sed.script job2.template > ${DPATHDIR}/run/job2
fi

if [ ${MACHINE} == "CWB" ]; then
  ${SED} -f sed.script job2_cwb_batch.template > ${DPATHDIR}/run/job2_batch.sh
else
  ${SED} -f sed.script job2_batch.template > ${DPATHDIR}/run/job2_batch.sh
fi

${SED} -f sed.script runncl.sh.template > ${DPATHDIR}/ymon/runncl.sh
#${SED} -f sed.script clivar_meanstate.ncl.template > ${DPATHDIR}/ymon/clivar_meanstate.ncl
#${SED} -f sed.script ymon.template > ${DPATHDIR}/ymon/ymon.sh
#${SED} -f sed.script ymon_function.template > ${DPATHDIR}/ymon/ymon_function.sh
${SED} -f sed.script rerun_echam.ctl.template > ${DPATHDIR}/run/rerun_${EXP}_echam.ctl
${SED} -f sed.script hour.template > ${DPATHDIR}/hour/hour.sh
${SED} -f sed.script ydaily.template > ${DPATHDIR}/ydaily/ydaily.sh
${SED} -f sed.script yhour.template > ${DPATHDIR}/yhour/yhour.sh
${SED} -f sed.script runncl.sh.template > ${DPATHDIR}/ydaily/runncl.sh
${SED} -f sed.script ydaily.mjo.template > ${DPATHDIR}/ydaily/ydaily.mjo.sh
${SED} -f sed.script rsync_to_nuwa.template > ${DPATHDIR}/ydaily/rsync_to_nuwa.sh
${SED} -f sed.script mjo.template > ${DPATHDIR}/daily/mjo.sh
${SED} -f sed.script plot.template > ${DPATHDIR}/run/plot.template.sh
${SED} -f sed.script rsync_archive.template > ${DPATHDIR}/run/rsync_archive.sh
${SED} -f sed.script run_job2.template > ${DPATHDIR}/run/run_job2.sh
${SED} -f sed.script run_job2.1.template > ${DPATHDIR}/run/run_job2.1.sh
${SED} -f sed.script change_LYEAR.template > ${DPATHDIR}/run/change_LYEAR.sh

###rm -f xx.* yy.* zz.* sed.script
rm -f xx.* yy.* zz.*

######################
#  7.0 copy
######################

### cp -p ymon.sh ${DPATHDIR}/run/
cp -p readme.txt mo_ocean.f90 sit_ocean.f90 HISTORY.txt ehtw.${LEV}.codes ehtw.inv.codes ehtw_wrf.${LEV}.codes julianday_functions.sh remote_file_functions.sh ${DPATHDIR}/run/
cp -p xanim.gs lineplot.gs plot.gs ${DPATHDIR}/run/
cp -p *.gs ${DPATHDIR}/ymon/
cp -p *.gs ${DPATHDIR}/hour/
cp -p mjo.gs ${DPATHDIR}/daily/

cd ${DPATHDIR}/run

######################
#  8.0 submit the run
######################
if [ ${LCOLD_START} == T ]; then
  echo "LCOLD_START= "T
  echo rm -f rerun_${EXP}_echam OCN_SV_* hdrestart.nc
  rm -f rerun_${EXP}_echam rerun_${EXP}_nudg OCN_SV_* hdrestart.nc
else
  echo "COLD_START= "F
fi

if [ ${LFUTURE} == T ]; then
  echo "DPATH_P=${DPATH_P}"

  if [ ! -e ${DPATHDIR}/run/rerun_${EXP}_echam ]; then
    echo "rerun file ${DPATHDIR}/run/rerun_${EXP}_echam does not exist!"
    # if [ -e ../../${DPATH_P}/rerun/${RFILE_P} ]; then
    #  cp -p ../../${DPATH_P}/rerun/${RFILE_P} ${DPATHDIR}/run/
    if [ -e "${DPATH_P}/${RFILE_P}" ]; then
      cp -p ${DPATH_P}/${RFILE_P} ${DPATHDIR}/run/
    else
      echo "${DPATH_P}/${RFILE_P} does not exit."
      if [ ${ARCHIVE} == "LFTP" ]; then
        # cal_retrive_lftp ${DPATHDIR}/run/ ${RFILE_P} ${REMOTE} "${PERM_DPATH_P}/rerun"
        cal_retrive_lftp ${DPATHDIR}/run/ ${RFILE_P} ${REMOTE} "${DPATH_P}/"
      else
        # cal_retrive_rsync ${DPATHDIR}/run/ ${RFILE_P} ${REMOTE} "${PERM_DPATH_P}/rerun"
        cal_retrive_rsync ${DPATHDIR}/run/ ${RFILE_P} ${REMOTE} "${DPATH_P}/"
      fi      
    fi
    # make a copy in rerun directory
    cp -p ${RFILE_P} ${DPATHDIR}/rerun/
    # untar and change the name for the rerun file 
    tar -zxvf ${RFILE_P}
    if [ -e rerun_${EXP_P}_echam ]; then
      mv rerun_${EXP_P}_echam rerun_${EXP}_echam
    else
      echo rerun_${EXP_P}_echam': fail, rerun file does not exist'
      exit 1    
    fi
    if [ -e rerun_${EXP_P}_nudg ]; then
      mv rerun_${EXP_P}_nudg rerun_${EXP}_nudg
    fi
    echo rerun_${EXP_P}_echam': rerun files restored (for both ECHAM and ocean)'
  else
    echo 'NOT restore rerun files since there are rerun files in ${DPATHDIR}/run/'
  fi
fi

if [ ${NOTRUN} == F ]; then
  if [ ${MACHINE} == "IRISH" ]; then
    echo "${QSUB} ${EXPNAME}.sh"
    ${QSUB} ${EXPNAME}.sh
  elif [ ${MACHINE} == "NUWA" ]; then
    echo "${QSUB} ${EXPNAME}.sh"
    ${QSUB} ${EXPNAME}.sh
  elif [ ${MACHINE} == "ALPS" ]; then
    echo "${QSUB} < ${EXPNAME}.sh"
    ${QSUB} < ${EXPNAME}.sh
  elif [ ${MACHINE} == "CWB" ]; then
    echo "${QSUB} ${EXPNAME}.sh"
    ${QSUB} ${EXPNAME}.sh
  fi
fi

######################
#  9.0 create remote permanent dir
######################
if [ 1 -eq 1 ]; then
# It is done in job2 now.
# for irish
# the directory for the experiment will be created, if not already there
if [ ${LARCHIVE} == T ]; then
  echo I am using ${MACHINE}: create remote permanent dir
  for DIR in $DIRS; do
    echo ssh ${REMOTE} mkdir -p ${PERMDIR3}/${DIR}
    if [ ${ARCHIVE_METHOD} == "LFTP" ]; then
      cal_mkdir_lftp ${REMOTE} ${PERMDIR3}/${DIR}
    else
      cal_mkdir_ssh ${REMOTE} ${PERMDIR3}/${DIR}
    fi
    #STAT=$?
    #if [ $STAT -eq 0 ] ; then
    #  echo successful!
    #else
    #  echo fail!
    #  exit
    #fi
  done
fi
fi

######################
#  10.0 postprocessing
######################

if [ ${LPOST} == "T" ]; then
  if [ ${MACHINE} == "IRISH" ]; then
    ${QSUB} ${DPATHDIR}/run/run_job2.sh
  elif [ ${MACHINE} == "NUWA" ]; then
    ${QSUB} ${DPATHDIR}/run/run_job2.sh
  elif [ ${MACHINE} == "ALPS" ]; then
    ${QSUB} < ${DPATHDIR}/run/run_job2.sh
  elif [ ${MACHINE} == "CWB" ]; then
    ${QSUB} ${DPATHDIR}/run/run_job2.sh
  fi
fi

cd ${CURDIR}


}

