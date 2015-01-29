#!/bin/sh
#CWD=`pwd`
#EXP=c96_dpc_H0_v0.4909
#EXPDIR="tikal/runs/${EXP}/history"
#GFDLDIR="/archive/bjt"
FILES=`ls *.nc.tar`
send_file=/ncrc/home2/fms/local/opt/fre-commands/bronx-7/site/ncrc/bin/send_file
for file in ${FILES}; do
  echo send_file ${file}
  ${send_file} ${file}
  echo ${file} finished
#  echo gcp -v ${file} gfdl:${GFDLDIR}/${EXPDIR}/
#  gcp -v ${file} gfdl:${GFDLDIR}/${EXPDIR}/
# gcp -v --synrc ${file} gfdl:${GFDLDIR}/${EXPDIR}/
#  echo gcp ${file} finished
done
###gcp -v -r --sync *.nc.tar gfdl:${GFDLDIR}/${EXPDIR}/
