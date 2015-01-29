# source   ./run_template_v2.csh lbatch.      windfall v0.5 minor res  suffix  do_test use_clim do_sit  maskid sitArg bathy
# source   ./run_template_v2.csh .true.       windfall org  915   192 hiram   .false. .false. 
# source   ./run_template_v2.csh .true.       windfall v0.5 ''    192 hiram   .false. .false. 
                                                                                               
if ( 1 == 0 ) then                                                                             
 source   ./run_template_v5.csh interactive  windfall v0.5 0    192  dpc    .true.   .true.  .true.   2      5d5d5d
# source   ./run_template_v5.csh msub         norm     v0.5 0     192   dpc    .true.   .true.  .true.   2      5d5d5d
else if ( 1 == 0 ) then          
 source   ./run_template_v5.csh interactive  windfall v0.5 019   192   dpc    .true.   .true.  .true.   2      nn1m1d
else if ( 1 == 1 ) then          
 source   ./run_template_v5.csh msub         norm     v0.5 0     192   hiram   .false.   .true.  .true.   2      0d0d0d
else if ( 1 == 0 ) then          
 source   ./run_template_v5.csh interactive  windfall v0.5 019   192   dpc    .true.   .true.  .true.   2      nnnnnn
else if ( 1 == 1 ) then          
 source   ./run_template_v5.csh msub         norm     v0.5 0     192   dpc    .false.   .true.  .true.   2      5d5d5d
 source   ./run_template_v5.csh msub         norm     v0.5 0     192   dpc    .false.   .true.  .true.   2      0d0d0d
 source   ./run_template_v5.csh msub         windfall v0.5 0     192   rkmp    .false.   .true.  .true.   2      5d5d5d
 source   ./run_template_v5.csh msub         norm     v0.5 0     192   rkmp    .false.   .true.  .true.   2      0d0d0d
 source   ./run_template_v5.csh msub         windfall v0.5 0     192   hiram   .false.   .true.  .true.   2      5d5d5d
 source   ./run_template_v5.csh msub         norm     v0.5 0     192   hiram   .false.   .true.  .true.   2      0d0d0d
else if ( 1 == 1 ) then          
# source   ./run_template_v5.csh msub         windfall v0.5 0     192   hiram  .true.   .true.  .true.   2      5d5d5d
# source   ./run_template_v5.csh msub         windfall v0.5 0     192   hiram  .true.   .true.  .true.   2      0d0d0d
else if ( 1 == 0 ) then          
 source   ./run_template_v5.csh interactive  norm     v0.5 019   1     dpc    .true.   .true.  .true.   2      0d0d0d
# source   ./run_template_v5.csh interactive  norm     v0.5 001   1     dpc    .true.   .false. .true.   2      0d0d0d
else if ( 1 == 0 ) then          
  source   ./run_template_v5.csh interactive  windfall v0.5 926   192  hiram  .true.   .true.  .true.   2      nn1m1d
else if ( 1 == 0 ) then 
  source   ./run_template_v5.csh interactive  windfall v0.5 922   96   dpc    .true.   .true.  .false. 2      nn1m1d
else if ( 1 == 0 ) then
  source   ./run_template_v5.csh interactive  windfall v0.5 922   96   dpc    .true.   .true.  .true.  2      nn1m1d
else if ( 1 == 0 ) then
  source   ./run_template_v5.csh interactive  windfall v0.5 922   96o1 dpc    .true.   .true.  .true.  2      nn1m1d
else if ( 1 == 0 ) then  
# PRODUCTION RUN
  source   ./run_template_v5.csh not_run      norm     v0.5 926   192 hiram   .false.  .true.  .true.  2      nn1m1d
else if ( 1 == 1 ) then
# PRODUCTION RUN       
  source   ./run_template_v5.csh msub         windfall v0.5 001   192 dpc     .false.  .false. .false. 2      5d1d1d
  source   ./run_template_v5.csh msub         windfall v0.5 001   192 hiram   .false.  .false. .false. 2      5d1d1d
else if ( 1 == 1 ) then
# PRODUCTION RUN       
  source   ./run_template_v5.csh msub         windfall v0.5 001   192 dpc     .false.  .false. .true.  2      5d1d1d
  source   ./run_template_v5.csh msub         windfall v0.5 001   192 hiram   .false.  .false. .true.  2      5d1d1d
else if ( 1 == 1 ) then 
# PRODUCTION RUN        
  source   ./run_template_v5.csh msub         norm     v0.5 001   192 dpc     .false.  .true.  .true.  2      10d10d1d
  source   ./run_template_v5.csh msub         windfall v0.5 001   192 hiram   .false.  .true.  .true.  2      10d10d1d
else if ( 1 == 1 ) then          
# source   ./run_template_v5.csh msub         windfall v0.5 0     192   hiram  .true.   .true.  .true.   2      5d5d5d
# source   ./run_template_v5.csh msub         windfall v0.5 0     192   hiram  .true.   .true.  .true.   2      0d0d0d
else if ( 1 == 0 ) then          
 source   ./run_template_v5.csh interactive  norm     v0.5 019   1     dpc    .true.   .true.  .true.   2      0d0d0d
# source   ./run_template_v5.csh interactive  norm     v0.5 001   1     dpc    .true.   .false. .true.   2      0d0d0d
else if ( 1 == 0 ) then          
  source   ./run_template_v5.csh interactive  windfall v0.5 926   192  hiram  .true.   .true.  .true.   2      nn1m1d
else if ( 1 == 0 ) then 
  source   ./run_template_v5.csh interactive  windfall v0.5 922   96   dpc    .true.   .true.  .false. 2      nn1m1d
else if ( 1 == 0 ) then
  source   ./run_template_v5.csh interactive  windfall v0.5 922   96   dpc    .true.   .true.  .true.  2      nn1m1d
else if ( 1 == 0 ) then
  source   ./run_template_v5.csh interactive  windfall v0.5 922   96o1 dpc    .true.   .true.  .true.  2      nn1m1d
else if ( 1 == 0 ) then  
# PRODUCTION RUN
  source   ./run_template_v5.csh not_run      norm     v0.5 926   192 hiram   .false.  .true.  .true.  2      nn1m1d
else if ( 1 == 1 ) then
# PRODUCTION RUN       
  source   ./run_template_v5.csh msub         windfall v0.5 001   192 dpc     .false.  .false. .false. 2      5d1d1d
  source   ./run_template_v5.csh msub         windfall v0.5 001   192 hiram   .false.  .false. .false. 2      5d1d1d
else if ( 1 == 1 ) then
# PRODUCTION RUN       
  source   ./run_template_v5.csh msub         windfall v0.5 001   192 dpc     .false.  .false. .true.  2      5d1d1d
  source   ./run_template_v5.csh msub         windfall v0.5 001   192 hiram   .false.  .false. .true.  2      5d1d1d
else if ( 1 == 1 ) then 
# PRODUCTION RUN        
  source   ./run_template_v5.csh msub         norm     v0.5 001   192 dpc     .false.  .true.  .true.  2      10d10d1d
  source   ./run_template_v5.csh msub         windfall v0.5 001   192 hiram   .false.  .true.  .true.  2      10d10d1d
else if ( 1 == 1 ) then
# PRODUCTION RUN       
  source   ./run_template_v5.csh not_run      windfall v0.5 926   192 dpc     .false.  .true.  .true.  2      nn1m1d
  source   ./run_template_v5.csh msub         norm     v0.5 926   192 hiram   .false.  .true.  .true.  2      nn1m1d
  source   ./run_template_v5.csh not_run      windfall v0.5 926   192 dpc     .false.  .true.  .true.  2      0d0d0d
  source   ./run_template_v5.csh not_run      windfall v0.5 926   192 hiram   .false.  .true.  .true.  2      0d0d0d
###else if ( 1 == 1 ) then 
# PRODUCTION RUN           
  source   ./run_template_v5.csh not_run      windfall v0.5 922   192 dpc     .false.  .true.  .false. 2      nn1m1d
  source   ./run_template_v5.csh not_run      windfall v0.5 922   192 hiram   .false.  .true.  .false. 2      nn1m1d
  source   ./run_template_v5.csh not_run      windfall v0.5 922   192 dpc     .false.  .true.  .true.  2      nnnnnn
  source   ./run_template_v5.csh not_run      norm     v0.5 922   192 hiram   .false.  .true.  .true.  2      nnnnnn
else if ( 1 == 1 ) then 
  source   ./run_template_v5.csh not_run      windfall v0.5 922   96  dpc     .false.  .true.  .true.  2      nn1m1d
  source   ./run_template_v5.csh not_run      windfall v0.5 922   96  hiram   .false.  .true.  .true.  2      nn1m1d
  source   ./run_template_v5.csh not_run      windfall v0.5 922   96  dpc     .false.  .true.  .true.  2      0d0d0d
  source   ./run_template_v5.csh not_run      windfall v0.5 922   96  hiram   .false.  .true.  .true.  2      0d0d0d
  source   ./run_template_v5.csh not_run      windfall v0.5 922   96  dpc     .false.  .true.  .false. 3      nn1m1d
  source   ./run_template_v5.csh not_run      windfall v0.5 922   96  hiram   .false.  .true.  .false. 3      nn1m1d
# source   ./run_template_v5.csh not_run      windfall v0.5 922   96  dpc     .false.  .true.  .true.  2      nnnnnn
#  source   ./run_template_v5.csh not_run      windfall v0.5 922   96  hiram   .false. .true.   .true.  2      nnnnnn
endif

