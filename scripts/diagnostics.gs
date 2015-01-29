In-line diagnostic test:



sdfopen /lustre/f1/Benjei.Tsuang/tikal/runs/c96_v0.4906_b2_p13b_sit_m3_nn1m1d/history/19821001/19821001.ice_8xdaily.nc


d tsw
draw title 01OCT1982 starting from 01Jan1980
printim wt.jpg
c

set lat 0
lineplot --v wt(z=1) wt(z=19) wt(z=23) --l "0m" "78m" "142m"
draw ylab WT (K)
draw title 0N, 01OCT1982 starting from 01Jan1980
printim wt0N.jpg
c


cd /lustre/f1/Benjei.Tsuang/tikal/runs/c96_dpc_H0_sit_m2_nn1m1d_v0.4912/history/19800901
 cdo hourmean 19800901.ice_8xdaily.nc 19800901.ice_diurnal_v2.nc


