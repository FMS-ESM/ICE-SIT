#!/bin/bash

base=~Lucas.Harris/coupled_siena/

set -e
set -x

for file in $*
do
    file=${file%/} #need to remove trailing slash, or send_file.csh will not work
    #Also need FULL path name; test whether this is a relative path
    if [[ ${file:0:1} != / ]]
    then
	file=$PWD/$file
    fi
    dest=gfdl:/archive/${file#/lustre/fs/scratch/}
    if [[ $file =~ restart ]]
    then
	msub -v base=${base},source=${file},destination=${dest},extension=tar,type=restart ~Lucas.Harris/coupled_siena/scripts/send_file.frepp.csh
    else
	msub -v base=${base},source=${file},destination=${dest},extension=tar,type=history ~Lucas.Harris/coupled_siena/scripts/send_file.frepp.csh
    fi

done