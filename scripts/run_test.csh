# source   ./run_template.csh lbatch. v0.4 minor res  suffix  do_test do_sit maskid sitArg
# source   ./run_template.csh .true.  org  907   c192 b2_p7   .false. .false.
# source   ./run_template.csh .true.  v0.4 ''    c192 b2_p7   .false. .false.

if ( 1 == 1 ) then
 source   ./run_template.csh .false. v0.4 912  c96  dpc_H0  .true. .true.  2      nn1m1d
#source   ./run_template.csh .false. v0.4 910  c96  dpc_H0  .true. .true.  3      nn1m1d
# source   ./run_template.csh .false. v0.4 908  c96  dpc_H0  .true.  .true.  3      nn1m1d
else if ( 1 == 1 ) then
  source   ./run_template.csh .true. v0.4 907   c96  b2_p13b .false. .true.  3      0d0d0d
  source   ./run_template.csh .true. v0.4 907   c96  dpc_H0  .false. .true.  3      nn1d1d
  source   ./run_template.csh .true. v0.4 907   c96  dpc_H0  .false. .false. 3      nn1d1d
  source   ./run_template.csh .true. v0.4 907   c96  b2_p13b .false. .true.  3      nn1d1d
  source   ./run_template.csh .true. v0.4 907   c96  b2_p13b .false. .false. 3      nn1d1d
else
# source   ./run_template.csh .false. v0.4 907   c192 dpc_H0   .true. .true.  3      nn1d1d
# source   ./run_template.csh .true. v0.4 907   c192 b2_p7   .false. .true.  3      nn1d1d
#REDUNCT
# source   ./run_template.csh .true. v0.4 907   c192 b2_p7   .false. .false. 3      nn1d1d
# CARSH
# source   ./run_template.csh .true. v0.4 907   c192 b2_p7   .false. .true.  2      nn1d1d
# CARSH in month 3
# source   ./run_template.csh .true. v0.4 903   c192 b2_p7   .false. .true.  2      nn1d1d
  source   ./run_template.csh .true. v0.4 907   c192 dpc_H0  .false. .true.  3      nn1d1d
  source   ./run_template.csh .true. v0.4 907   c192 dpc_H0  .false. .false. 3      nn1d1d
  source   ./run_template.csh .true. v0.4 907   c192 b2_p7   .false. .true.  3      nn1d1d
  source   ./run_template.csh .true. v0.4 907   c192 b2_p7   .false. .false. 2      nn1d1d
endif
# source    ./run_template.csh .true. v0.4 907   c96  b2_p13b .true.  .true. 2      nn1d1d

