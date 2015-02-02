cp -p /autofs/na2_home2/Benjei.Tsuang/tikal/update/v0.5/scripts/.. .
cp -p ~ChiaYing.Tu/hiram_t2som/src/ocean_amip/ocean_model.F90 ocean_model.F90.ChiaYing.Tu

fregrid from ocn tripolar grid to lat/lon grid:
gaea7:/<5>c192_dpc_sit_m2_5d5d5d_v0.50_test/history/19800104 > fregrid --input_mosaic ../../work/INPUT/ocean_mosaic.nc --nlon 720 --nlat 360 --input_file 19800104.ice_daily.nc --output_file 19800104.ice_daily.ll.nc --scalar_field wt,ws



./atmos_param/sea_esf_rad/aerosol.F90              ! # modify: test_aerosol
./atmos_coupled/atmos_model.F90                    ! # define DEBUG only
./shared/constants/constants.F90                   ! # add missing value
./coupler/coupler_main.F90                         ! # add DEBUG
         ! # add the follwoing in the fast time loop 
         !      call generate_sfc_xgrid( Land, Ice )
         ! # add flux_exchange_end
./shared/data_override/data_override.F90           ! # no change (deleted from v0.50)
./coupler/flux_exchange.F90                        ! many changes (wind10, tprec, ....), flux_exchange_end
./shared/horiz_interp/horiz_interp_conserve.F90    ! horiz_interp_conserve: version1 and version2: NOT SURE TO BE CORRECT

      select case ( Interp%version)
      case (1)
         call horiz_interp_conserve_version1(Interp, data_in, data_out, verbose, mask_in, mask_out)
      case (2)
         !!! if(present(mask_in) .OR. present(mask_out) ) call mpp_error(FATAL,  &
         !!!    'horiz_interp_conserve: for version 2, mask_in and mask_out must be passed in horiz_interp_new, not in horiz_interp')
         call horiz_interp_conserve_version2(Interp, data_in, data_out, verbose)     
      end select
      
ice_model                                           ! New Ice Model
./atmos_shared/interpolator/interpolator.F90        ! MISSING value treatment
ocean_model.F90                                     ! New NULL OCN model
./atmos_param/radiation_driver/radiation_driver.F90 ! # no change (deleted from v0.50)
./shared/sat_vapor_pres/sat_vapor_pres.F90          ! # no change (deleted from v0.50)
sit_vdiff_mod.F90                                   ! New





Add default missing values for missing values handling in various code 


I am write code for reading World Ocean Data. The data set have missing values. I found in various codes. The missing values can be varies from code to code. Therefore, default missing values for real (real_missing) and for integer (int_missing) are suggested, here, in the FMS. Then, programmers can be shared with the same missing value. 


FMS:
0[598]0 gaea8:/<3>Benjei.Tsuang/GitHub/NOAA-GFDL-FMS > git status
# On branch master
# Changes not staged for commit:
#   (use "git add <file>..." to update what will be committed)
#   (use "git checkout -- <file>..." to discard changes in working directory)
#
#       modified:   data_override/data_override.F90
#       modified:   horiz_interp/horiz_interp_conserve.F90
#       modified:   mpp/include/mpp_io_read.inc
#       modified:   sat_vapor_pres/sat_vapor_pres.F90
#
no changes added to commit (use "git add" and/or "git commit -a")



