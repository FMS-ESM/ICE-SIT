#undef DEBUG
!!!#define DEBUG
#undef ONEWAY
#undef MIMIC_SIT 
!!! #undef DEBUG
!!!#define GDCHK1 .FALSE.
!!!#define GDCHK1 mpp_pe().EQ.449
!!!#define GDCHK1 mpp_pe().EQ.0
!!!#define GDCHK1 (mpp_pe().EQ.216)
!!!#define GDCHK1 (mpp_pe().EQ.399)
!!!#define GDCHK1 (mpp_pe().EQ.817)
!!!#define GDCHK1 (mpp_pe().EQ.495)
!!!#define GDCHK1 (mpp_pe().EQ.530)
!!!#define GDCHK1 (mpp_pe().EQ.740)
!!!#define GDCHK1 (mpp_pe().EQ.0)
!!!#define GDCHK1 (mpp_pe().EQ.734)
!!!#define GDCHK1 (mpp_pe().EQ.334)
!!!#define GDCHK0 ( (mpp_pe().EQ.334) )
!!!#define GDCHK1 ( (mpp_pe().EQ.334).AND.(j.EQ.78) )
!!!#define GDCHK0 (mpp_pe().EQ.198)
!!!#define GDCHK1 (mpp_pe().EQ.198).AND.(j.EQ.52)

!!!#define GDCHK0 (mpp_pe().EQ.480)
!!!#define GDCHK1 (mpp_pe().EQ.480).AND.(j.EQ.112)

!!!#define GDCHK0 (mpp_pe().EQ.480)
!!!#define GDCHK1 (mpp_pe().EQ.480).AND.(j.EQ.113)

!!!#define GDCHK0 (mpp_pe().EQ.171)
!!!#define GDCHK1 (mpp_pe().EQ.171).AND.(j.EQ.37)

!!!#define GDCHK0 (mpp_pe().EQ.369)
!!!#define GDCHK1 (mpp_pe().EQ.369).AND.(j.EQ.85)

!!!#define GDCHK0 (mpp_pe().EQ.741)
!!!#define GDCHK1 (mpp_pe().EQ.741).AND.(j.EQ.170)

!!!#define GDCHK0 (mpp_pe().EQ.325)
!!!#define GDCHK1 (mpp_pe().EQ.325)
!!!#define GDCHK2 (mpp_pe().EQ.325)
!!!#define GDCHK0 (mpp_pe().EQ.819)
!!!#define GDCHK1 (mpp_pe().EQ.819)
!!!#define GDCHK2 (mpp_pe().EQ.819)
!!!#define GDCHK0 (mpp_pe().EQ.833)
!!!#define GDCHK1 (mpp_pe().EQ.833)
!!!#define GDCHK2 (mpp_pe().EQ.833)
!!!#define GDCHK0 (mpp_pe().EQ.98)
!!!#define GDCHK1 (mpp_pe().EQ.98)
!!!#define GDCHK2 ((mpp_pe().EQ.98).AND.(i.eq.269).AND.(j.eq.20))
#define II :
#define JJ :
#define GDCHK0 (.false.)
#define GDCHK1 (.false.)
#define GDCHK2 (.false.)

!!!#define GDCHK0 (mpp_pe().EQ.324)
!!!#define GDCHK1 (mpp_pe().EQ.324)

module ice_model_mod

use   ice_albedo_mod, only:  ice_albedo_init, ice_albedo

use ocean_albedo_mod, only:  compute_ocean_albedo_new

use  ocean_rough_mod, only:  compute_ocean_roughness, fixed_ocean_roughness

use          fms_mod, only: file_exist, open_restart_file, close_file, &
                            mpp_pe, mpp_root_pe, mpp_npes, write_version_number, stdlog,   &
                            error_mesg, FATAL, check_nml_error, read_data, write_data,     &
                            NOTE, WARNING, field_exist, field_size, get_mosaic_tile_grid, stdout

use fms_io_mod,       only: save_restart, register_restart_field, restart_file_type, &
                            restore_state, set_domain, nullify_domain, query_initialized, &
                            get_restart_io_mode

use mpp_mod,          only: mpp_chksum, mpp_error, mpp_sync ! bjt

#ifdef INTERNAL_FILE_NML
use          mpp_mod, only: input_nml_file
#else
use          fms_mod, only: open_namelist_file
#endif

use    constants_mod, only: hlv, hlf, tfreeze, pi, radius

use  mpp_domains_mod, only: domain1d, domain2d, mpp_define_domains, mpp_get_compute_domain, &
                            mpp_get_compute_domains, mpp_get_domain_components, mpp_get_pelist, &
                            mpp_define_layout, mpp_define_io_domain, mpp_global_field, &
                            XUPDATE, YUPDATE, CORNER, mpp_set_domain_symmetry

use diag_manager_mod, only: diag_axis_init, register_diag_field, send_data

use time_manager_mod, only: time_type, get_time, operator(+), print_date

use mosaic_mod,       only: get_mosaic_ntiles, get_mosaic_grid_sizes
use grid_mod,         only: get_grid_comp_area, get_grid_size, get_grid_cell_vertices, &
                            get_grid_cell_centers, get_grid_cell_area

use  amip_interp_mod, only: amip_interp_type, amip_interp_new, amip_interp_del, get_amip_ice, get_amip_sst    ! bjt

use coupler_types_mod,only: coupler_2d_bc_type, coupler_3d_bc_type

!bjt
use constants_mod, only: xmissing, real_missing, int_missing
use sit_vdiff_mod,    only: sit_vdiff_init_1d,sit_vdiff,SICEDFN,lkvl,set_ocndepth,sit_nml, &
                          maskid,sit_zdepth,sit_fluxdepth,lwoa_gfdl, &
                          lprint0,lprint1,lprint2,bathy_default,    &
                          j3m,j10m,j100m
use eos_ocean_mod,    only: tmelts
use woa_mod,          only: woa_type, woa_init, woa_time_vary, woa_driver, woa_end, woa_dealloc, nfields, &
                            Model_init_time, woa_time, &
                            do_column_woa, do_predicted_woa,do_specified_woa, &
                            column_woa_driver, predicted_woa_driver,specified_woa_driver
use mpp_mod,          only: mpp_clock_id, mpp_clock_begin, mpp_clock_end, CLOCK_COMPONENT
use fms_mod,          only: clock_flag_default

!bjt

implicit none
private

public :: ice_data_type, ocean_ice_boundary_type,               &
          atmos_ice_boundary_type, land_ice_boundary_type,      &
          ice_model_init, ice_model_end, update_ice_model_fast, &
          update_ice_model_slow_up, update_ice_model_slow_dn,   &
          ice_stock_pe, cell_area, ice_model_restart,           &
          ocn_ice_bnd_type_chksum, atm_ice_bnd_type_chksum, &
          lnd_ice_bnd_type_chksum, ice_data_type_chksum, do_sit
          

type ice_data_type
  type(domain2d)                        :: Domain

   logical                              :: pe

   integer, pointer, dimension(:)       :: pelist =>NULL()

   real,    pointer, dimension(:)       :: glon_bnd =>NULL(), &
                                           glat_bnd =>NULL(), &
                                           lon_bnd =>NULL() , &
                                           lat_bnd =>NULL()

   real,    pointer, dimension(:,:)     :: lon =>NULL(), &
                                           lat =>NULL(), &
                                           lonv =>NULL(), &
                                           latv =>NULL()

   logical, pointer, dimension(:,:)     :: mask =>NULL()

   logical, pointer, dimension(:,:,:)   :: ice_mask =>NULL()

   real,    pointer, dimension(:,:,:,:) :: temp =>NULL()

   real,    pointer, dimension(:,:,:)   :: part_size =>NULL(), &
                                           t_surf =>NULL(), &
                                           albedo =>NULL(), &
                                           albedo_vis_dir =>NULL(), &
                                           albedo_nir_dir =>NULL(), &
                                           albedo_vis_dif =>NULL(), &
                                           albedo_nir_dif =>NULL(), &
                                           rough_mom =>NULL(),&
                                           rough_heat =>NULL(), &
                                           rough_moist =>NULL(),  &
                                           frazil =>NULL(),  &
                                           u_surf =>NULL(),  &
                                           v_surf =>NULL()

   real,    pointer, dimension(:,:,:)   :: flux_u_bot =>NULL(), &
                                           flux_v_bot =>NULL(), &
                                           flux_t_bot =>NULL(),   &
                                           flux_q_bot =>NULL(), &
                                           flux_lh_bot =>NULL(), &
                                           flux_sw_bot =>NULL(), &
                                           flux_sw_vis_bot =>NULL(), &
                                           flux_sw_dir_bot =>NULL(), &
                                           flux_sw_dif_bot =>NULL(), &
                                           flux_sw_vis_dir_bot =>NULL(), &
                                           flux_sw_vis_dif_bot =>NULL(), &
                                           flux_sw_nir_dir_bot =>NULL(), &
                                           flux_sw_nir_dif_bot =>NULL(), &
                                           flux_lw_bot =>NULL(), &
                                           lprec_bot =>NULL(), &
                                           fprec_bot =>NULL(), &
                                           runoff_bot =>NULL()

   real,    pointer, dimension(:,:  )   :: flux_u =>NULL(), &
                                           flux_v =>NULL(), &
                                           flux_t =>NULL(), &
                                           flux_q =>NULL(), &
                                           flux_lh =>NULL(), &
                                           flux_sw =>NULL(), &
                                           flux_sw_vis =>NULL(), &
                                           flux_sw_dir =>NULL(), &
                                           flux_sw_dif =>NULL(), &
                                           flux_sw_vis_dir =>NULL(), &
                                           flux_sw_vis_dif =>NULL(), &
                                           flux_sw_nir_dir =>NULL(), &
                                           flux_sw_nir_dif =>NULL(), &
                                           flux_lw =>NULL(), &
                                           lprec =>NULL(), &
                                           fprec =>NULL(), &
                                           p_surf =>NULL(), &
                                           runoff =>NULL(), &
                                           calving =>NULL(), &
                                           runoff_hflx =>NULL(), &
                                           calving_hflx =>NULL(), &
                                           area =>NULL(), &
                                           flux_salt =>NULL()
                                           
  logical, pointer, dimension(:,:) :: maskmap =>NULL()   ! A pointer to an array indicating which
                                                         ! logical processors are actually used for
                                                         ! the ocean code. The other logical
                                                         ! processors would be all land points and
                                                         ! are not assigned to actual processors.
                                                         ! This need not be assigned if all logical
                                                         ! processors are used. This variable is dummy and need 
                                                         ! not to be set, but it is needed to pass compilation.

   integer                              :: avg_kount

   real,    pointer, dimension(:,:,:)   :: thickness =>NULL()

   type (time_type)                     :: Time_Init, Time,  &
                                           Time_step_fast,   &
                                           Time_step_slow
   integer, dimension(3)              :: axes
   type(coupler_3d_bc_type)           :: ocean_fields       ! array of fields used for additional tracers
   type(coupler_2d_bc_type)           :: ocean_fluxes       ! array of fluxes used for additional tracers
   type(coupler_3d_bc_type)           :: ocean_fluxes_top   ! array of fluxes for averaging
   real,    pointer, dimension(:,:)   :: mi                  =>NULL() ! This is needed for the wave model. It is introduced here,

   ! sit, bjt
   real,    pointer, dimension(:,:)   :: sitmask=>NULL(),&       !  psitmask : mask for sit(1=.TRUE., 0=.FALSE.)                                               I
                                         obsseaice=>NULL(),&     !  pobsseaice  : observed sea ice fraction (fraction)                                         I
                                         obswtb=>NULL(),&        !  pobswtb     : observed bulk sea surface temperature (K)                                    I
                                         obswsb=>NULL()          !  obswsb    : observed salinity (PSU, 0/00)                                                  I
                                         
   real,    pointer, dimension(:,:)   :: cor =>NULL(), &         !  corlios factor 2*omega*sin(lat) (1/s)
                                         slm=>NULL(),&           !  pslf: sea land fraction (1. = land, 0. = sea/lakes), land fraction [0-1]
                                         lclass=>NULL(), &       !  land cover class, 1: land, 2: ocean, 3: lake, 4: glacier
                                         bathy=>NULL(),&         !  pbathy  : bathymeter (topography or orography) of ocean (m)                                I  
                                         wlvl=>NULL(),&          !  pwlvl  : current water level (ice/water interface) a water body grid                       I/O
                                         ocnmask=>NULL(),&       !  pocnmask : fractional mask for 3-D ocean grid (0-1)                                        I
                                         obox_mask=>NULL(),&     !  obox_mask : 3-D ocean nudging mask, =0: nudging, = 1 (>0): nudging                         I

                                         sni=>NULL(),&           !  snow thickness over ice (m in water equivalent)
                                         siced=>NULL(),&         !  ice thickness (m in water equivalent)
                                         tsi=>NULL(),&           !  ptsi: surface temperature of ice (K) 
                                         tsw=>NULL(),&           !  skin temperatrue (K) over water
                                         tsl=>NULL(),&           !  ptsl: calcuated Earth's skin temperature from vdiff/tsurf at t+dt (K)
                                         tslm=>NULL(),&          !  ptslm: calcuated Earth's skin temperature from vdiff/tsurf at t (K)
                                         tslm1=>NULL(),&         !  ptslm1: calcuated Earth's skin temperature from vdiff/tsurf at t-dt (K)
                                         ocu=>NULL(),&           !  pocu: ocean eastw. velocity (m/s)
                                         ocv=>NULL(),&           !  pocv: ocean northw. velocity (m/s) 
                                         ctfreez2=>NULL(),&      !  pctfreez2 : ref water freezing temperature (K)
   ! 2-d SIT vars                                          I
                                         wtb=>NULL(),&           !  pwtb: 10-m mean water temperature (K)                                                           O
                                         wub=>NULL(),&           !  pwub: 10-m mean water u current (m/s)                                                           O
                                         wvb=>NULL(),&           !  pwvb: 10-m mean water v current (m/s)                                                           O
                                         wsb=>NULL(),&           !  pwsb: 10-m mean water salinity (PSU)                                                            O
                                         fluxiw=>NULL(),&        !  pfluxiw: over-water net surface heat flux (W/m2, + upward, i.e., from ocean))              O
                                         pme2=>NULL(),&          !  ppme2: net fresh water into ocean (P-E+ice_corr) (m/s, + downward)                         O
                                         subfluxw=>NULL(),&      !  psubfluxw: subsurface ocean heat flux (W/m2, + upward)                                     O
                                         wsubsal=>NULL(),&       !  pwsubsal: subsurface ocean salinity flux (m*PSU/s, + upward)                               O
                                         cc=>NULL(),&            !  pcc: cold content per water fraction (ice sheet+openwater) (J/m2)
                                                                 !    (energy need to melt snow and ice, i.e., energy below liquid water at tmelt)             I/O
                                         hc=>NULL(),&            !  phc: heat content per water fraction (ice sheet+openwater) (J/m2)
                                                                 !    (energy of a water column above tmelt)                                                   I/O
                                         engwac=>NULL(),&        !  pengwac: accumulated energy per water fraction (ice sheet+openwater) (+ downward, J/m2)
                                                                 !    (pfluxw+pfluxi+rain/snow advected energy in respect to liquid water at
                                                                 !    tmelt)*dt                                                                                I/O
                                         sc=>NULL(),&            !  psc: salinity content per water fraction (ice sheet+openwater) (PSU*m)                     I/O
                                         saltwac=>NULL(),&       !  psaltwac: accumulated salt into water fraction (+ downward, PSU*m)                         I/O
                                         wtfns=>NULL(),&         !  pwtfns: nudging flux into sit-ocean for an entire column (W/m**2), i.e.
                                                                 !    pwtfns=SUM(pwtfn(icol,:))                                                                I/O
                                         wsfns=>NULL()           !  pwsfn: nudging salinity flux into sit-ocean for an entire column (PSU*m/s),                  
                                                                 !    i.e., pwsfns=SUM(pwsfn(icol,:)                                                           I/O
  ! 3-d SIT vars: snow/ice
   real,    pointer, dimension(:,:,:) :: zsi=>NULL(),&           !  pzsi:                                                                        
                                                                 !    pzsi(icol,0): dry snow water equivalent (m)                                              I/O
                                                                 !    pzsi(icol,1): dry ice water equivalent (m)                                               I/O
                                         silw=>NULL(),&          !  psilw:                                                                        
                                                                 !    psilw(icol,0): snow liquid water storage (m)                                             I/O
                                                                 !    psilw(icol,1): ice liquid water storage (m)                                              I/O
                                         tsnic=>NULL()           !  ptsnic:
                                                                 !    ptsnic(icol,0): snow skin temperatrue (jk)                                               I/O
                                                                 !    ptsnic(icol,1): mean snow temperatrue (jk)                                               I/O
                                                                 !    ptsnic(icol,2): ice skin temperatrue (jk)                                                I/O
                                                                 !    ptsnic(icol,3): mean ice temperatrue (jk)                                                I/O
  ! 3-d SIT vars: water column
   real,    pointer, dimension(:,:,:) :: obswt=>NULL(),&         !  pobswt: observed potentail water temperature (K)                                           I/O
                                         obsws=>NULL(),&         !  pobsws: observed salinity (PSU, 0/00)                                                      I/O
                                         obswu=>NULL(),&         !  pobswu: observed u-current (m/s)                                                           I/O
                                         obswv=>NULL(),&         !  pobswv: observed v-current (m/s)                                                           I/O           
                                         wt=>NULL(),&            !  pwt: potential water temperature (K)                                                       I/O   
                                         wu=>NULL(),&            !  pwu: u current (m/s)                                                                       I/O
                                         wv=>NULL(),&            !  pwv: v current (m/s)                                                                       I/O
                                         ww=>NULL(),&            !  pww: w current (m/s)                                                                       I/O
                                         ws=>NULL(),&            !  pws: practical salinity (0/00)                                                             I/O
                                         wtke=>NULL(),&          !  pwtke: turbulent kinetic energy (M2/S2)                                                    I/O
                                         wlmx=>NULL(),&          !  pwlmx: mixing length (m)                                                                   O
                                         wldisp=>NULL(),&        !  pwldisp: dissipation length (m)                                                            O
                                         wkm=>NULL(),&           !  pwkm: eddy diffusivity for momentum (m2/s)                                                 O
                                         wkh=>NULL(),&           !  pwkh: eddy diffusivity for heat (m2/s)                                                     O
                                         wrho1000=>NULL(),&      !  pwrho1000: 1000-m potential water density (kg/m3)                                          O
                                         wtfn=>NULL(),&          !  pwtfn: mean temperature flux nudging at each level (positive into ocean) (K/s)             I/O
                                         wsfn=>NULL(),&          !  pwsfn: mean salinity flux nudging at each level (positive into ocean) (PSU/s)              I/O
                                         awufl=>NULL(),&         !  pawufl: advected u flux at each level (positive into ocean) (m/s**2)                       I
                                         awvfl=>NULL(),&         !  pawvfl: advected v flux at each level (positive into ocean) (m/s**2)                       I
                                         awtfl=>NULL(),&         !  pawtfl: advected temperature flux at each level (positive into ocean) (K/s)                I
                                         awsfl=>NULL(),&         !  pawsfl: advected salinity flux at each level (positive into ocean) (PSU/s)                 I
                                         awtkefl=>NULL(),&       !  pawtkefl: advected tke at each level (positive into ocean) (m2/s3)                         I
                                         p_flux=>NULL()          !  30-D flux depth (m)                         I
                                         
   ! final output only
   real,    pointer, dimension(:,:) :: seaice=>NULL()            !  ice cover: fraction of 1-SLM (0-1)

   ! implicit with vdiff
   real,    pointer, dimension(:,:) :: grndcapc=>NULL(),&        !  pgrndcapc: areal heat capacity of the uppermost sit layer (snow/ice/water) (J/m**2/K)
                                       grndhflx=>NULL(),&        !  pgrndhflx: ground heat flux below the surface (W/m**2) (+ upward, into the skin layer)
                                       grndflux=>NULL()          !  pgrndflx:  acc. ground heat flux below the surface (W/m**2*s) (+ upward, into the skin layer)

end type ice_data_type

type :: ocean_ice_boundary_type
  real, dimension(:,:),   pointer :: u =>NULL(), &
                                     v =>NULL(), &
                                     t =>NULL(), &
                                     s =>NULL(), &
                                     frazil =>NULL(), &
                                     sea_level =>NULL()
  real, dimension(:,:,:), pointer :: data =>NULL()
  integer                         :: xtype
  type(coupler_2d_bc_type)        :: fields     ! array of fields used for additional tracers
end type ocean_ice_boundary_type

type :: atmos_ice_boundary_type
  real, dimension(:,:,:), pointer :: u_flux =>NULL(), &
                                     v_flux =>NULL(), &
                                     u_star =>NULL(), &
                                     t_flux =>NULL(), &
                                     q_flux =>NULL(), &
                                     lw_flux =>NULL(), &
                                     sw_flux =>NULL(), &
                                     sw_flux_vis_dir =>NULL(), &
                                     sw_flux_vis_dif =>NULL(), &
                                     sw_flux_nir_dir =>NULL(), &
                                     sw_flux_nir_dif =>NULL(), &
                                     fluxn =>NULL(), &
                                     dfluxn =>NULL(), &                                     
                                     lprec =>NULL(), &
                                     fprec =>NULL(), &
                                     tprec =>NULL(), &        !sit
                                     gust =>NULL()             !sit
                        
  real, dimension(:,:,:), pointer :: dhdt =>NULL(), &
                                     dedt =>NULL(), &
                                     drdt =>NULL(), &
                                     coszen =>NULL(), &
                                     p =>NULL(), &
                                     data =>NULL()
  integer                         :: xtype
  type(coupler_3d_bc_type)        :: fluxes     ! array of fluxes used for additional tracers
end type atmos_ice_boundary_type

type :: land_ice_boundary_type
  real, dimension(:,:),   pointer :: runoff =>NULL(), &
                                     calving =>NULL(), &
                                     runoff_hflx =>NULL(), &
                                     calving_hflx =>NULL()
  real, dimension(:,:,:), pointer :: data =>NULL()
  integer :: xtype
end type land_ice_boundary_type

character(len=128) :: version = '$Id: ice_model.F90,v 20.0 2013/12/13 23:28:06 fms Exp $'
character(len=128) :: tagname = '$Name: tikal_201403_sit $'

character(len=80) :: restart_format = 'amip ice model restart format 02'
logical :: module_is_initialized = .false.
logical :: stock_warning_issued  = .false.

! id's for diagnostics
integer :: id_sh, id_lh, id_sw, id_lw, id_snofl, id_rain, &
           id_runoff, id_calving, id_evap, id_fax, id_fay, &
           id_sw_vis, id_sw_dir, id_sw_dif, id_sw_vis_dir, id_sw_vis_dif, &
           id_sw_nir_dir, id_sw_nir_dif

! sit
integer :: dt,isecs,istep=1,idays
!!!integer :: id_siced, id_obsseaice, id_obswtb, id_obswsb
integer :: id_obsseaice, id_obswtb, id_obswsb
integer :: id_cor, id_slm,id_lclass,                                &
                  id_sitmask,id_bathy, id_wlvl,                     &
                  id_ocnmask, id_obox_mask,                         &
                  id_sni, id_siced,                                 &
                  id_tsi,                                           &
                  id_tsw, id_tsl,                                   &
                  id_tslm, id_tslm1,                                &
                  id_ocu, id_ocv,                                   &
                  id_ctfreez2,                                      &

             ! 2-d SIT vars
                  id_wtb, id_wub, id_wvb,                           &
                  id_wsb,                                           &
                  id_fluxiw, id_pme2,                               &
                  id_subfluxw, id_wsubsal,                          &
                  id_cc, id_hc, id_engwac,                          &
                  id_sc, id_saltwac,                                &
                  id_wtfns, id_wsfns,                               &

             ! 3-d SIT vars: snow/ice
                  id_zsi, id_silw, id_tsnic,                        &             

             ! 3-d SIT vars: snow/ice (seperate to levels)
                  id_zsi0, id_zsi1,                                 &             
                  id_silw0, id_silw1,                               &             
                  id_tsnic0, id_tsnic1, id_tsnic2, id_tsnic3,       &             

             ! 3-d SIT vars: water column
                  id_obswt, id_obsws, id_obswu, id_obswv,           &
                  id_wt, id_wu, id_wv, id_ww,                       &
                  id_ws, id_wtke, id_wlmx,                          &
                  id_wldisp, id_wkm, id_wkh,                        &
                  id_wrho1000,                                      &
                  id_wtfn, id_wsfn,                                 &
                  id_awufl, id_awvfl, id_awtfl,                     &
                  id_awsfl, id_awtkefl,                             &
             ! 4- OUTPUT only, original ATM variabels
                  id_seaice,                                        &
                  id_grndcapc, id_grndhflx, id_grndflux,            &

             ! Additional OUTPUT     
                  id_wt0m, id_wt3m, id_wt10m,  id_wt100m                 

!!!#ifdef DEBUG

integer :: id_fluxi, &
           id_fluxw, & 
           id_dfluxi, &
           id_dfluxw, &
           id_soflw, &
           id_sofli, & 
           id_uflux, &
           id_vflux, &
           id_lprec, &
           id_fprec, &
           id_qflux, &
           id_tprec, &
           id_gust
!!!#endif

integer:: iceClock, sitClock, sit_vdiff_ini_Clock, sit_vdiff_end_Clock, sit_vdiff_Clock, sit_vdiff_write_Clock
logical :: sent

!-----------------------------------------------------------------------
!
!  use_climo_ice           = use monthly climatological amip ice mask
!  use_annual_ice          = use annual climatology amip ice mask
!  temp_ice_freeze         = temperature at which sea ice melts
!  no_ice                  = run with no ice (only open water)
!  use_leads               = use fraction ice coverage (i.e., leads) if it exists
!  roughness_ice           = constant roughness for all ice
!  specified_ice_thickness = constant thickness for specified ice

integer, parameter :: num_lev  = 1
integer, parameter :: num_part = 2

real    :: diff                     = 2.092  
real    :: thickness_min            = 0.10 
real    :: specified_ice_thickness  = 2.0
real    :: temp_ice_freeze          = -1.66    ! was 271.5
real    :: roughness_ice            = 1.e-4
logical :: no_ice                   = .false.
logical :: use_leads                = .false.   ! bjt
!!!logical :: use_leads                = .true.        ! bjt
logical :: use_climo_ice            = .false.
logical :: use_annual_ice           = .false.
logical :: lobswt           = .false.
logical :: lobsws           = .false.
logical :: lobswu           = .false.
logical :: lobswv           = .false.

!------- namelist ---------
!!!   logical :: do_netcdf_restart = .true.
logical :: use_climo_sst  = .false.
logical :: use_annual_sst = .false.
!!!   integer, dimension(2) :: layout = (/ 0, 0 /)
!!!   character(len=64) :: interp_method  = "conservative" ! default, conservative scheme
!  layout =  domain decomposition (# X-PEs by # Y-PEs)
!             layout = (/0,0/) will use default rules
!
!!!   namelist /ocean_model_nml/ do_netcdf_restart, use_climo_sst, use_annual_sst, layout, interp_method


!bjt	  
logical :: do_sit                   = .false.
!bjt 

integer, dimension(2) :: layout     = (/ 0, 0 /)
!!!integer, dimension(2) :: io_layout  = (/ 0, 0 /)       ! one output each cpu
integer, dimension(2) :: io_layout  = (/ 1, 1 /)          ! only one output (bjt)
character(len=64) :: interp_method  = "conservative" ! default conservative scheme
logical :: do_netcdf_restart        = .true.
character(len=128) :: axisname_x    = 'xt'  ! x-axis name of temperature grid
character(len=128) :: axisname_y    = 'yt'  ! y-axis name of temperature grid
character(len=128) :: axisname_xb   = 'xb'  ! x-axis bounds name of temperature grid
character(len=128) :: axisname_yb   = 'yb'  ! y-axis bounds name of temperature grid

namelist /ice_model_nml/ do_netcdf_restart, diff, thickness_min, &
                         specified_ice_thickness,                &
                         temp_ice_freeze, roughness_ice,         &
                         use_climo_ice, use_annual_ice,          &
                         no_ice, use_leads, layout, interp_method,  &
                         axisname_x, axisname_y, axisname_xb, axisname_yb, &
                         io_layout,                              &     ! bjt
                         do_sit, use_climo_sst, use_annual_sst         ! bjt


real, parameter :: latent = hlv + hlf
type(amip_interp_type), save :: Amip_ice, Amip_sst
real, allocatable, dimension(:,:) ::  cell_area  ! grid cell area; sphere frac.

integer :: id_restart_albedo
integer :: mlon, mlat, npart ! global grid size
type(restart_file_type), save :: Ice_restart

integer                             :: is, ie, js, je                  ! bjt

!--- CYTu --------------------------------------------------------------
  real, allocatable, dimension (:,:) :: geo_lonv, geo_latv, geo_lon, geo_lat
  real, allocatable, dimension(:) :: xb1d, yb1d ! 1d global grid for diag_manager

  integer :: im, jm
!!!  integer :: id_sst, id_hflx, id_swflx_nir_dir, id_swflx_nir_dif, id_swflx_vis_dir, id_swflx_vis_dif, &
!!!             id_lwflx, id_shflx, id_lhflx, id_snwflx, id_frazil
!!!  integer :: id_qflx_adj, id_qflux_restore_sst, id_net_hflx
!!!  integer :: id_sst_obs
!!!  integer :: id_pme, id_river

!!!  logical :: sent
  real    :: mlcp
!
! mosaic
!
integer             :: grid_version
integer, parameter  :: VERSION_0 = 0  ! grid file with field geolon_c
integer, parameter  :: VERSION_1 = 1  ! grid file with field x_C
integer, parameter  :: VERSION_2 = 2  ! mosaic file

! nml variables
logical :: debug_this_module = .false.      ! CYTu
logical :: verbose_init = .false.

! Grid variables
character(len=256) :: name
integer :: grid_ni, grid_nj
real, dimension(:,:), pointer :: x_c, y_c, grid_wet
real, dimension(:), pointer :: grid_x_c, grid_y_c
logical :: tripolar, mosaic

! grid file name
character(len=256) :: ocean_hgrid        ! will be set in set_ocean_grid_size
character(len=256) :: ocean_vgrid = 'INPUT/ocean_vgrid.nc'
!--- CYTu --------------------------------------------------------------

! bjt >>
    type(woa_type)  :: woa
! << bjt        
contains

!=============================================================================================
  subroutine ice_model_init( Ice, Time_Init, Time, Time_step_fast, Time_step_slow )
    type(ice_data_type), intent(inout) :: Ice
    type(time_type)    , intent(in)    :: Time_Init, Time, Time_step_fast, Time_step_slow

    !!! real, allocatable, dimension(:,:)   :: lonv, latv, rmask
    real, allocatable, dimension(:,:)   :: rmask
    real, allocatable, dimension(:)     :: glon, glat
    real, allocatable, dimension(:)     :: xb, yb ! 1d global grid for diag_mgr
    real, allocatable, dimension(:,:)   :: tmpx, tmpy, tmp_2d
    integer                             :: io, ierr, unit, siz(4), logunit
    !!! integer                             :: nlon, nlat, is, ie, js, je, i, j, k
    integer                             :: nlon, nlat, i, j, k, im, jm    ! bjt
!!!    real, allocatable,   dimension(:,:) :: geo_latv                       ! bjt
!!!    real, allocatable,   dimension(:,:) :: geo_lonv                       ! bjt
!!!    real, allocatable, dimension(:,:)   ::  geo_lat            ! true latitude (rotated grid)  ! bjt
!!!    real, allocatable, dimension(:,:)   ::  geo_lon            ! true longitude                ! bjt
    character(len=80)                   :: control
    character(len=80)                   :: domainname
    character(len=256)                  :: err_mesg
    character(len=64)                   :: fname = 'INPUT/ice_model.res.nc'
    character(len=64)                   :: lvltag
    character(len=256)                  :: grid_file='INPUT/grid_spec.nc'
    character(len=256)                  :: ocean_mosaic, tile_file
    character(len=256)                  :: axo_file      ! atmosXocean exchange grid file
    integer                             :: nx(1), ny(1)
    integer                             :: ntiles, n, m
    integer                             :: grid_version
    integer, parameter                  :: VERSION_0 = 0  ! grid file with field geolon_t
    integer, parameter                  :: VERSION_1 = 1  ! grid file with field x_T
    integer, parameter                  :: VERSION_2 = 2  ! mosaic file

    ! sit >>
    !!! integer, intent(in) :: nlon, nlat
    !!! logical, dimension(nlon,nlat), intent(in) :: land_mask 
    real, parameter :: omega = .7292e-4      ! solid rotation velocity of the earth
    character(len=64), dimension(:), pointer :: woa_names => NULL()
    character(len=64), dimension(:), pointer :: woa_family_names => NULL()
    ! character(len=64), dimension(MAX_woa_NAMES) :: names
!!!     type(woa_type)  :: woa
    ! << sit                                 ! in 1/s (2*pi/86164 s)

  if (GDCHK0) print *, "I am in ice_model_init 1.0."

    iceClock = mpp_clock_id( 'ICE', flags=clock_flag_default, grain=CLOCK_COMPONENT )
    sitClock = mpp_clock_id( ' SIT: total', flags=clock_flag_default, grain=CLOCK_COMPONENT )
    sit_vdiff_ini_Clock = mpp_clock_id( ' SIT: sit_vdiff_ini', flags=clock_flag_default, grain=CLOCK_COMPONENT )
    sit_vdiff_end_Clock = mpp_clock_id( ' SIT: sit_vdiff_end', flags=clock_flag_default, grain=CLOCK_COMPONENT )
    sit_vdiff_Clock = mpp_clock_id( ' SIT: sit_vdiff', flags=clock_flag_default, grain=CLOCK_COMPONENT )
    sit_vdiff_write_Clock = mpp_clock_id( ' SIT: sit_vdiff_write', flags=clock_flag_default, grain=CLOCK_COMPONENT )

    call mpp_clock_begin(iceClock)
            
    if(module_is_initialized) return

#ifdef INTERNAL_FILE_NML
    read (input_nml_file, nml=ice_model_nml, iostat=io)
    ierr = check_nml_error(io, 'ice_model_nml')
#else
    if ( file_exist( 'input.nml' ) ) then
       unit = open_namelist_file ( )
       ierr = 1
       do while ( ierr /= 0 )
          read ( unit,  nml = ice_model_nml, iostat = io, end = 10 )
          ierr = check_nml_error ( io, 'ice_model_nml' )
       enddo
10     continue
       call close_file (unit)
    endif
#endif

  if (GDCHK0) print *, "I am in ice_model_init 2.0."


    call get_restart_io_mode(do_netcdf_restart)

    call write_version_number (version, tagname)
    if ( mpp_pe() == mpp_root_pe() ) then
       logunit = stdlog()
       write (logunit, nml=ice_model_nml)
    endif

   !if (num_part /= 2) call error_mesg ('ice_model_init','this version must have num_part = 2', FATAL)
   !if (num_lev  /= 1) call error_mesg ('ice_model_init','this version must have num_lev = 1', FATAL)

    !--- get the grid size 
    call get_grid_size('OCN',1,nlon,nlat)

    !-------------------- domain decomposition -----------------------------
    if( layout(1).EQ.0 .AND. layout(2).EQ.0 ) &
         call mpp_define_layout( (/1,nlon,1,nlat/), mpp_npes(), layout )
    if( layout(1).NE.0 .AND. layout(2).EQ.0 )layout(2) = mpp_npes()/layout(1)
    if( layout(1).EQ.0 .AND. layout(2).NE.0 )layout(1) = mpp_npes()/layout(2)
    domainname = 'AMIP Ice'
    call mpp_define_domains( (/1,nlon,1,nlat/), layout, Ice%Domain, name=domainname )
    call mpp_define_io_domain (Ice%Domain, io_layout)
    call set_domain (Ice%Domain)
    call mpp_get_compute_domain( Ice%Domain, is, ie, js, je )
    allocate ( glon     (nlon  ), glat     (nlat  )  )
    allocate(  rmask(is:ie,js:je) )
    allocate(  Ice%glon_bnd(nlon+1),    Ice%glat_bnd(nlat+1)    )
    allocate ( Ice%lon_bnd (is:ie+1),      &
               Ice%lat_bnd (js:je+1),      &
               Ice%lon (is:ie, js:je),   &
               Ice%lat (is:ie, js:je),  &
               Ice%lonv (is:ie+1, js:je+1), &
               Ice%latv (is:ie+1, js:je+1) )


!-----------------------------------------------------------------------

!-----------------------------------------------------------------------

!  ---- set up local grid -----
   call get_grid_cell_vertices('OCN', 1, Ice%glon_bnd, Ice%glat_bnd)
   call get_grid_cell_centers ('OCN', 1, glon, glat)
   call get_grid_cell_vertices('OCN', 1, Ice%lonv, Ice%latv, Ice%domain)
   call get_grid_cell_centers ('OCN', 1, Ice%lon, Ice%lat, Ice%domain)

   !--- for conservation interpolation, the grid should be rectangular ----
   if(trim(interp_method) == "conservative" ) then
      err_mesg = 'Bilinear interpolation must be used for a tripolar grid'
      do i=is, ie
         if(any(glon(i) /= Ice%lon(i,:)))  &
              call error_mesg ('ice_model_init',err_mesg,FATAL)
      enddo
      do j=js,je
         if(any(glat(j) /= Ice%lat(:,j)))  &
              call error_mesg ('ice_model_init',err_mesg,FATAL)
      enddo
   endif

    !---------------- read ice cell areas from grid_spec.nc or ----------------
    !---------------- calculate the area for mosaic grid file  ----------------
    allocate (cell_area(is:ie, js:je))
    cell_area = 0.0
    call get_grid_cell_area('OCN', 1, cell_area, Ice%domain)
        
   !   ------ define Data masks ------
   if(field_exist(grid_file, 'wet')) then
      call read_data(grid_file, "wet",      rmask,     Ice%Domain)
   else if(field_exist(grid_file, 'ocn_mosaic_file') ) then ! read from mosaic file
      call read_data(grid_file, "ocn_mosaic_file", ocean_mosaic)
      ocean_mosaic = "INPUT/"//trim(ocean_mosaic)
      ntiles = get_mosaic_ntiles(ocean_mosaic)
      if(ntiles .NE. 1) call error_mesg('ice_model_init', ' ntiles should be 1 for ocean mosaic, contact developer', FATAL)
      rmask = 0.0
      call get_grid_comp_area('OCN', 1, rmask, domain=Ice%Domain)
      rmask = rmask/cell_area
      do j = js, je
         do i = is, ie
            if(rmask(i,j) == 0.0) cell_area(i,j) = 0.0
         end do
      end do
   else
      call error_mesg('ice_model_init','wet and ocn_mosaic_file does not exist in file '//trim(grid_file), FATAL )
   end if

    !--- xb and yb is for diagnostics --------------------------------------
   allocate(xb(nlon+1), yb(nlat+1) )
 if (.false.) then
   allocate ( tmpx(is:ie+1, nlat+1) )
   call mpp_set_domain_symmetry(Ice%Domain, .TRUE.)
   call mpp_global_field(Ice%Domain, Ice%lonv, tmpx, flags=YUPDATE, position=CORNER)
   allocate ( tmp_2d(is:ie+1, js:je+1) )
   tmp_2d = 0
   tmp_2d(is:ie+1,js) = sum(tmpx,2)/(nlat+1);
   deallocate(tmpx)
   allocate ( tmpx(nlon+1, js:je+1) )

   call mpp_global_field(Ice%Domain, tmp_2d, tmpx, flags=XUPDATE, position=CORNER)
   xb = tmpx(:,js)
   deallocate(tmpx, tmp_2d)

   allocate ( tmpy(nlon+1, js:je+1) )
   call mpp_global_field(Ice%Domain, Ice%latv, tmpy, flags=XUPDATE, position=CORNER)
   allocate ( tmp_2d(is:ie+1, js:je+1) )
   tmp_2d = 0
   tmp_2d(is,js:je+1) = sum(tmpy,1)/(nlon+1);
   deallocate(tmpy)
   allocate ( tmpy(is:ie+1, nlat+1) )
   call mpp_global_field(Ice%Domain, tmp_2d, tmpy, flags=YUPDATE, position=CORNER)
   yb = tmpy(is,:)
   deallocate(tmpy, tmp_2d)
   call mpp_set_domain_symmetry(Ice%Domain, .FALSE.)   
 else if (.false.) then
   xb=Ice%glon_bnd
   yb=Ice%glat_bnd

 else if (.true.) then

!--- CYTu --------------------------------------------------------------
!
    im=nlon;
    jm=nlat;
    
    call set_ocean_grid_size(im, jm, grid_file)

    !!! allocate ( Ocean_state%mask (is:ie, js:je) )    ! CYTu
    allocate ( geo_lon (is:ie , js:je ), geo_lat (is:ie , js:je  ) )
    allocate ( geo_lonv (1:im+1,1:jm+1), geo_latv (1:im+1,1:jm+1) )

    call set_ocean_hgrid_arrays(geo_lonv, geo_latv, rmask, Ice%Domain)
    !!! call set_ocean_hgrid_arrays(geo_lonv, geo_latv, rmask, Ocean%Domain)

    geo_lon = (geo_lonv(is:ie,js:je)    +geo_lonv(is+1:ie+1,js:je) &
         +geo_lonv(is:ie,js+1:je+1)+geo_lonv(is+1:ie+1,js+1:je+1))/4
    geo_lat = (geo_latv(is:ie,js:je)    +geo_latv(is+1:ie+1,js:je) &
         +geo_latv(is:ie,js+1:je+1)+geo_latv(is+1:ie+1,js+1:je+1))/4

    !!! allocate ( xb (im+1) )
    !!! allocate ( yb (jm+1) )
    xb = sum(geo_lonv,2)/(jm+1);
    yb = sum(geo_latv,1)/(im+1);

   !!! if (mpp_pe().EQ.0) then
   !!!   print *, "I am in ice_model_init 3.0."
   !!!   print *, "geo_lonv=", geo_lonv
   !!!   print *, "geo_latv=", geo_latv
   !!! endif
   
    deallocate (geo_lonv)
    deallocate (geo_latv)
!
!--- CYTu --------------------------------------------------------------

 endif  
   !!! if ( (mpp_pe().EQ.160).OR.(mpp_pe().EQ.99) ) then
   !!! if (mpp_pe().EQ.0) then
   !!!   print *, "I am in ice_model_init 3.1."
   !!!   print *, "mpp_pe()=", mpp_pe()
   !!!   print *, "Ice%glon_bnd=", Ice%glon_bnd
   !!!   print *, "Ice%glat_bnd=", Ice%glat_bnd
   !!!   print *, "xb=", xb
   !!!   print *, "yb=", yb
   !!! endif
    !--- define the ice data -----------------------------------------------
   Ice%lon = Ice%lon*pi/180.
   Ice%lat = Ice%lat*pi/180.
   Ice%lonv = Ice%lonv*pi/180.
   Ice%latv = Ice%latv*pi/180.
   Ice%glon_bnd = Ice%glon_bnd*pi/180.
   Ice%glat_bnd = Ice%glat_bnd*pi/180.
   Ice%lon_bnd    = Ice%glon_bnd(is:ie+1)
   Ice%lat_bnd    = Ice%glat_bnd(js:je+1)
    !-----------------------------------------------------------------------
    allocate ( Ice%ice_mask    (is:ie, js:je, num_part)         , &
               Ice%temp        (is:ie, js:je, num_part, num_lev), &
               Ice%part_size   (is:ie, js:je, num_part)         , &
               Ice%albedo      (is:ie, js:je, num_part)         , &
            Ice%albedo_vis_dir (is:ie, js:je, num_part)         , &
            Ice%albedo_nir_dir (is:ie, js:je, num_part)         , &
            Ice%albedo_vis_dif (is:ie, js:je, num_part)         , &
            Ice%albedo_nir_dif (is:ie, js:je, num_part)         , &
               Ice%rough_mom   (is:ie, js:je, num_part)         , &
               Ice%rough_heat  (is:ie, js:je, num_part)         , &
               Ice%rough_moist (is:ie, js:je, num_part)         , &
               Ice%u_surf      (is:ie, js:je, num_part)         , &
               Ice%v_surf      (is:ie, js:je, num_part)         , &
!!!               Ice%fluxn      (is:ie, js:je, num_part)         , &      ! bjt
!!!               Atmos_boundary%dfluxn      (is:ie, js:je, num_part)        , &      ! bjt
!!!               Ice%sw_flux      (is:ie, js:je, num_part)         , &      ! bjt
               Ice%thickness   (is:ie, js:je, num_part)         , &
               Ice%mask        (is:ie, js:je) )
    
    Ice%t_surf(is:, js:, 1:) => Ice%temp (is:, js:, 1:, 1)
    
    Ice%ice_mask=.false.    ! bjt for sit
    
    Ice%Time           = Time
    Ice%Time_init      = Time_init
    Ice%Time_step_fast = Time_step_fast
    Ice%Time_step_slow = Time_step_slow
    Ice%avg_kount = 0
    Ice%mask = .false.
    where ( rmask > 0 ) Ice%mask = .true.


    allocate ( Ice%flux_u_bot  (is:ie, js:je, num_part) , &
               Ice%flux_v_bot  (is:ie, js:je, num_part) , &
               Ice%flux_t_bot  (is:ie, js:je, num_part) , &
               Ice%flux_q_bot  (is:ie, js:je, num_part) , &
               Ice%flux_lh_bot (is:ie, js:je, num_part) , &
               Ice%flux_sw_bot (is:ie, js:je, num_part) , &
        Ice%flux_sw_vis_bot    (is:ie, js:je, num_part) , &
        Ice%flux_sw_dir_bot    (is:ie, js:je, num_part) , &
        Ice%flux_sw_dif_bot    (is:ie, js:je, num_part) , &
        Ice%flux_sw_vis_dir_bot(is:ie, js:je, num_part) , &
        Ice%flux_sw_vis_dif_bot(is:ie, js:je, num_part) , &
        Ice%flux_sw_nir_dir_bot(is:ie, js:je, num_part) , &
        Ice%flux_sw_nir_dif_bot(is:ie, js:je, num_part) , &
               Ice%flux_lw_bot (is:ie, js:je, num_part) , &
               Ice%lprec_bot   (is:ie, js:je, num_part) , &
               Ice%fprec_bot   (is:ie, js:je, num_part) , &
               Ice%runoff_bot  (is:ie, js:je, num_part) , &
               Ice%frazil      (is:ie, js:je, num_part)   )

    allocate ( Ice%flux_u    (is:ie, js:je) , &
               Ice%flux_v    (is:ie, js:je) , &
               Ice%flux_t    (is:ie, js:je) , &
               Ice%flux_q    (is:ie, js:je) , &
               Ice%flux_lh   (is:ie, js:je) , &
               Ice%flux_sw   (is:ie, js:je) , &
         Ice%flux_sw_vis     (is:ie, js:je) , &
         Ice%flux_sw_dir     (is:ie, js:je) , &
         Ice%flux_sw_dif     (is:ie, js:je) , &
         Ice%flux_sw_vis_dir (is:ie, js:je) , &
         Ice%flux_sw_vis_dif (is:ie, js:je) , &
         Ice%flux_sw_nir_dir (is:ie, js:je) , &
         Ice%flux_sw_nir_dif (is:ie, js:je) , &
               Ice%flux_lw   (is:ie, js:je) , &
               Ice%lprec     (is:ie, js:je) , &
               Ice%fprec     (is:ie, js:je) , &
               Ice%p_surf    (is:ie, js:je) , &
               Ice%runoff    (is:ie, js:je) , &
               Ice%calving   (is:ie, js:je) , &
             Ice%runoff_hflx (is:ie, js:je) , &
             Ice%calving_hflx(is:ie, js:je) , &
             Ice%area        (is:ie, js:je) , &
             Ice%mi          (is:ie, js:je) , &
               Ice%flux_salt (is:ie, js:je)   )
    Ice%flux_u = 0.0
    Ice%flux_v = 0.0
    Ice%flux_t = 0.0
    Ice%flux_q = 0.0
    Ice%flux_lh = 0.0
    Ice%flux_sw = 0.0
    Ice%flux_sw_vis = 0.0
    Ice%flux_sw_dir = 0.0
    Ice%flux_sw_dif = 0.0
    Ice%flux_sw_vis_dir = 0.0
    Ice%flux_sw_vis_dif = 0.0
    Ice%flux_sw_nir_dir = 0.0
    Ice%flux_sw_nir_dif = 0.0
    Ice%flux_lw = 0.0
    Ice%lprec = 0.0
    Ice%fprec = 0.0
    Ice%p_surf = 0.0
    Ice%runoff = 0.0
    Ice%calving = 0.0
    Ice%runoff_hflx = 0.0
    Ice%calving_hflx = 0.0
    Ice%area = 0.0
    Ice%mi = 0.0
    Ice%flux_salt = 0.0
    
    Ice%area = cell_area  * 4*PI*RADIUS*RADIUS
    Ice%mi   = 0.0

  if (GDCHK0) print *, "I am in ice_model_init 4.0."

    ! sit  >>

    call mpp_clock_begin(sitClock)
    call mpp_clock_begin(sit_vdiff_ini_Clock)
    
    allocate ( Ice%sitmask(is:ie, js:je),         &
               Ice%obsseaice      (is:ie, js:je), &
               Ice%obswtb         (is:ie, js:je), &
               Ice%obswsb         (is:ie, js:je) )

    Ice%sitmask=0.                   ! non SIT grids first
    Ice%obsseaice=xmissing
    Ice%obswtb=xmissing
    Ice%obswsb=xmissing

    if (.TRUE.) then
      Ice%sni(is:,js:)=>Ice%thickness(is:,js:,1)
      Ice%siced(is:,js:)=>Ice%thickness(is:,js:,2)

      Ice%tsi(is:,js:)=>Ice%t_surf(is:,js:,2)
      Ice%tsw(is:,js:)=>Ice%t_surf(is:,js:,1)

      Ice%ocu(is:,js:)=>Ice%u_surf(is:,js:,1)
      Ice%ocv(is:,js:)=>Ice%v_surf(is:,js:,1)

      Ice%seaice(is:,js:)=>Ice%part_size(is:,js:,2)                                                             
    endif
  call mpp_clock_end(sit_vdiff_ini_Clock)
  call mpp_clock_end(sitClock)


  if(trim(interp_method) == "conservative") then
     Amip_ice = amip_interp_new ( Ice%lon_bnd,     Ice%lat_bnd,  &
          Ice%mask(:,:),                 &
          interp_method = interp_method, &
          use_climo=use_climo_ice,       &
          use_annual=use_annual_ice     )
     Amip_sst = amip_interp_new ( Ice%lon_bnd,     Ice%lat_bnd,  &
          Ice%mask(:,:),                 &
          interp_method = interp_method, &
          use_climo=use_climo_sst,       &
          use_annual=use_annual_sst     )          
  else if(trim(interp_method) == "bilinear") then
     Amip_ice = amip_interp_new ( Ice%lon,     Ice%lat,          &
          Ice%mask(:,:),                 &
          interp_method = interp_method, &
          use_climo=use_climo_ice,       &
          use_annual=use_annual_ice     )
     Amip_sst = amip_interp_new ( Ice%lon,     Ice%lat,          &
          Ice%mask(:,:),                 &
          interp_method = interp_method, &
          use_climo=use_climo_sst,       &
          use_annual=use_annual_sst     )
  else
     call error_mesg('ice_model_init', 'interp_method should be conservative or bilinear', &
          FATAL)
  endif
  
!  ---- initialize other modules ----
  if (GDCHK0) print *, "I am in ice_model_init 5.0."

  call mpp_clock_begin(sitClock)
  call mpp_clock_begin(sit_vdiff_ini_Clock)  

#ifdef MIMIC_SIT
  if (.true.) then
#else
  if (do_sit) then              ! ok, won't crash
#endif
  !------------------- read namelist input -------------------------------
      
#ifdef INTERNAL_FILE_NML
    read (input_nml_file, nml=sit_nml, iostat=io)
    ierr = check_nml_error(io, 'sit_nml')
#else
    if ( file_exist( 'input.nml' ) ) then
       unit = open_namelist_file ( )
       ierr = 1
       do while ( ierr /= 0 )
          read ( unit,  nml = sit_nml, iostat = io, end = 11 )
          ierr = check_nml_error ( io, 'sit_nml' )
       enddo
11     continue
       call close_file (unit)
    endif
#endif
  !------- write version number and namelist ---------
  
    if ( mpp_pe() == mpp_root_pe() ) then
         call write_version_number(version, tagname)
         unit = stdlog()
         write (unit, nml=sit_nml)
    endif
  ! ----------------------------------------------------------------------
    if (GDCHK0) print *, "I am in ice_model_init 6.0."
  
    CALL set_ocndepth()

    allocate (    Ice%cor(is:ie, js:je), Ice%slm(is:ie, js:je), Ice%lclass(is:ie, js:je),                             & ! NOT in sit_vdiff_init_1d
                  Ice%bathy(is:ie, js:je), Ice%wlvl(is:ie, js:je),                          & 
                  Ice%ocnmask(is:ie, js:je), Ice%obox_mask(is:ie, js:je),                                             &

                  !!! Ice%sni(is:ie, js:je),                                                         & ! pointer to thickness(:,:,1)
                  !!! Ice%siced(is:ie, js:je),                                                       & ! pointer to thickness(:,:,2)
                  !!! Ice%tsi(is:ie, js:je),                                                         &
                  !!! Ice%tsw(is:ie, js:je),                                                         & ! NOT in sit_vdiff_init_1d
                  
                  Ice%tsl(is:ie, js:je),                                                                              & ! NOT in sit_vdiff_init_1d
                  Ice%tslm(is:ie, js:je), Ice%tslm1(is:ie, js:je),                                                    & ! NOT in sit_vdiff_init_1d
                  !!! Ice%ocu(is:ie, js:je), Ice%ocv(is:ie, js:je),                                                       &  ! NOT in sit_vdiff_init_1d
                  !!! Ice%obsseaice(is:ie, js:je), Ice%obswtb(is:ie, js:je), Ice%obswsb(is:ie, js:je),                    &  ! located earlier for non sit version as well
                  Ice%ctfreez2(is:ie, js:je),                                                                         &

             ! 2-d SIT vars
                  Ice%wtb(is:ie, js:je), Ice%wub(is:ie, js:je), Ice%wvb(is:ie, js:je),                                &
                  Ice%wsb(is:ie, js:je),                                                                              &
                  Ice%fluxiw(is:ie, js:je), Ice%pme2(is:ie, js:je),                                                   &   ! NOT in sit_vdiff_init_1d
                  Ice%subfluxw(is:ie, js:je), Ice%wsubsal(is:ie, js:je),                                              &
                  Ice%cc(is:ie, js:je), Ice%hc(is:ie, js:je), Ice%engwac(is:ie, js:je),                               &
                  Ice%sc(is:ie, js:je), Ice%saltwac(is:ie, js:je),                                                    &
                  Ice%wtfns(is:ie, js:je), Ice%wsfns(is:ie, js:je),                                                   &

             ! 3-d SIT vars: snow/ice
                  Ice%zsi(is:ie, js:je, 0:1), Ice%silw(is:ie, js:je, 0:1), Ice%tsnic(is:ie, js:je, 0:3),              &
                  !!! Ice%silw(is:ie, js:je, 0:1), Ice%tsnic(is:ie, js:je, 0:3),                                                       &

             ! 3-d SIT vars: water column
                  !!! Ice%obswt(is:ie, js:je, 0:lkvl+1), Ice%obsws(is:ie, js:je, 0:lkvl+1), Ice%obswu(is:ie, js:je, 0:lkvl+1), Ice%obswv(is:ie, js:je, 0:lkvl+1),  &
                  Ice%wt(is:ie, js:je, 0:lkvl+1), Ice%wu(is:ie, js:je, 0:lkvl+1), Ice%wv(is:ie, js:je, 0:lkvl+1), Ice%ww(is:ie, js:je, 0:lkvl+1),              &
                  Ice%ws(is:ie, js:je, 0:lkvl+1), Ice%wtke(is:ie, js:je, 0:lkvl+1), Ice%wlmx(is:ie, js:je, 0:lkvl+1),                                          &
                  Ice%wldisp(is:ie, js:je, 0:lkvl+1), Ice%wkm(is:ie, js:je, 0:lkvl+1), Ice%wkh(is:ie, js:je, 0:lkvl+1),                                        &
                  Ice%wrho1000(is:ie, js:je, 0:lkvl+1),                                                                                                        &
                  Ice%wtfn(is:ie, js:je, 0:lkvl+1), Ice%wsfn(is:ie, js:je, 0:lkvl+1),                                                                          &
                  Ice%awufl(is:ie, js:je, 0:lkvl+1), Ice%awvfl(is:ie, js:je, 0:lkvl+1), Ice%awtfl(is:ie, js:je, 0:lkvl+1),                                     &
                  Ice%awsfl(is:ie, js:je, 0:lkvl+1), Ice%awtkefl(is:ie, js:je, 0:lkvl+1),                                                                      &

             ! 4- OUTPUT only, original ATM variabels
                  !!! Ice%seaice(is:ie, js:je),                                                                                                                    &
                  !!! Ice%seaice(is:ie, js:je),                                                                                                                    &
                  Ice%grndcapc(is:ie, js:je), Ice%grndhflx(is:ie, js:je), Ice%grndflux(is:ie, js:je)    )

    if (GDCHK0) print *, "I am in ice_model_init 7.0."

    ! 1.0 set geological data
    Ice%cor=2*omega*sin(Ice%lat)            ! corilos factor

    if (GDCHK0) print *, "I am in ice_model_init 7.1."

    ! 2.0 set to be missing values
    Ice%slm=xmissing
    Ice%lclass=xmissing
    Ice%bathy=xmissing
    Ice%wlvl=xmissing
    Ice%ocnmask=xmissing
    Ice%obox_mask=xmissing
    !!! Ice%sni(is:,js:)=>Ice%thickness(:,:,1)
    !!! Ice%siced(is:,js:)=>Ice%thickness(:,:,2)
    
    !!! Ice%tsi(is:,js:)=>Ice%t_surf(:,:,2)
    !!! Ice%tsw(is:,js:)=>Ice%t_surf(:,:,1)
    
    Ice%tsl=xmissing
    Ice%tslm=xmissing
    Ice%tslm1=xmissing
 
    !!! Ice%ocu(is:,js:)=>Ice%u_surf(:,:,1)
    !!! Ice%ocv(is:,js:)=>Ice%v_surf(:,:,1)
    Ice%ctfreez2=xmissing

    if (.FALSE.) then
      Ice%sni=xmissing
      Ice%siced=xmissing

      Ice%tsi=xmissing
      Ice%tsw=xmissing

      Ice%ocu=xmissing
      Ice%ocv=xmissing

      Ice%seaice=xmissing
    endif

  ! 2-d SIT vars
    Ice%wtb=xmissing
    Ice%wub=xmissing
    Ice%wvb=xmissing
    Ice%wsb=xmissing
    Ice%fluxiw=xmissing
    Ice%pme2=xmissing
    Ice%subfluxw=xmissing
    Ice%wsubsal=xmissing
    Ice%cc=0.
    Ice%hc=0.
    Ice%engwac=0.
    Ice%sc=0.
    Ice%saltwac=0.
    Ice%wtfns=0.
    Ice%wsfns=0.

  ! 3-d SIT vars: snow/ice

    Ice%zsi=xmissing

    Ice%silw=xmissing
    Ice%tsnic=xmissing

    if (GDCHK0) print *, "I am in ice_model_init 7.2."
                                                                 
  ! 3-d SIT vars: water column
    !!! Ice%obswt=xmissing
    !!! Ice%obsws=xmissing
    !!! Ice%obswu=xmissing
    !!! Ice%obswv=xmissing
    Ice%wt=xmissing
    Ice%wu=xmissing
    Ice%wv=xmissing
    Ice%ww=xmissing
    Ice%ws=xmissing
    Ice%wtke=xmissing
    Ice%wlmx=xmissing
    Ice%wldisp=xmissing
    Ice%wkm=xmissing
    Ice%wkh=xmissing
    Ice%wrho1000=xmissing
    Ice%wtfn=0.
    Ice%wsfn=0.                   
    Ice%awufl=0.
    Ice%awvfl=0.
    Ice%awtfl=0.
    Ice%awsfl=0.
    Ice%awtkefl=0.
                   
  ! 4- OUTPUT only, original ATM variabels
    !!! Ice%seaice=xmissing                                                             
    !!! Ice%seaice(is:,js:)=>Ice%part_size(:,:,2)                                                             
    Ice%grndcapc=xmissing
    Ice%grndhflx=xmissing
    Ice%grndflux=xmissing

    if (GDCHK0) print *, "I am in ice_model_init 7.3."

    if (lwoa_gfdl) then
      if (GDCHK0) print *, "I am in ice_model_init 7.3.1"
      allocate (Ice%p_flux(is:ie, js:je, size(sit_fluxdepth)+1))
      do i = 1,size(sit_fluxdepth)
        Ice%p_flux(:,:,i) = sit_fluxdepth(i-1)
      enddo
      Ice%p_flux(:,:,size(sit_fluxdepth)+1) = sit_zdepth(lkvl+1)
      if (GDCHK0) print *, "I am in ice_model_init 7.3.2"                                                                               
      call woa_init (Ice%lonv(is:ie+1, js:je+1), Ice%latv(is:ie+1, js:je+1), woa_names, woa_family_names)                                                                                 
      if (GDCHK0) print *, "I am in ice_model_init 7.3.3"
      !!! model_time = increment_date(Model_init_time,0,0,0,0,0,0,0)                                                                                            
      call print_date (Ice%Time,  str= 'woa_time_vary: ')                                                                                                 
      call flush()
      !!! call woa_time_vary (Ice%Time)                                                                                                                       
      !!! call print_date (woa_time(1),  str= 'woa_driver: ')                                                                                                   
      !!! call woa_driver (is,js,Ice%Time, tracer, phalf, sit_zdepth, woa,.true.)
      !!! if (.false.) then
      !!! !! origianl version
      !!!   call woa_time_vary (model_time)
      !!!   call print_date (woa_time(1),  str= 'woa_driver: ')
      !!!   call woa_driver (1,1,model_time, tracer, phalf, p_flux, woa,.true.)
      if (do_column_woa) then     
      !! seperated new version
        if (GDCHK0) print *, "I am in ice_model_init 7.3.4"
      !!  call woa_time_vary (Ice%Time)
      !!  call print_date (woa_time(1), str= 'woa_driver: ')
      !!  call flush()
        call column_woa_driver (is, js, Ice%Time, Ice%p_flux, woa)
      else if (do_specified_woa) then
      !! read woa data
        if (GDCHK0) print *, "I am in ice_model_init 7.3.5"      
      !!  call woa_time_vary (Ice%Time)
      !!  call print_date (woa_time(1), str= 'woa_driver: ')
        call specified_woa_driver (is, js, Ice%Time, Ice%p_flux, woa)
      !! else if (do_predicted_woa) then  
      !!   call predicted_woa_driver (is, js, Ice%Time, tracer, Ice%p_flux, woa,.true.)
      !!  if (mpp_pe() == mpp_root_pe() ) then
        if (GDCHK0) then
          print *, "pe=",mpp_pe(),"size(woa%data,4)=",size(woa%data,4)
          do i=1,size(woa%data,4)
            print *, "pe=",mpp_pe(),"woa_names=",woa_names(i)
            print *, "woa=",woa%data(:,:,:,i)
            call flush()
          enddo
        endif
        if (GDCHK0) print *, "I am in ice_model_init 7.3.6"

      endif
      if (size(woa%data,4).ge.1) then
        lobswt=.true.
        Ice%obswt(is:, js:, 0:) => woa%data(:,:,:,1)                                                                                                          
        if (size(woa%data,4).ge.2) then
          lobsws=.true.
          Ice%obsws(is:, js:, 0:) => woa%data(:,:,:,2)                                                                                                          
          if (size(woa%data,4).ge.3) then
            lobswu=.true.
            Ice%obswu(is:, js:, 0:) => woa%data(:,:,:,3)                                                                                                          
            if (size(woa%data,4).ge.4) then
              lobswv=.true.
              Ice%obswv(is:, js:, 0:) => woa%data(:,:,:,4)
            endif
          endif
        endif
      endif                                                                                                          
      if (GDCHK0) then
        print *, "I am in ice_model_init 7.3.7, pe=",mpp_pe()
        if (lobswt) print *, "pe=",mpp_pe(),"1. Ice%obswt=",Ice%obswt(is:ie, js:je, 0:lkvl)
        if (lobsws) print *, "pe=",mpp_pe(),"1. Ice%obsws=",Ice%obsws(is:ie, js:je, 0:lkvl)
        if (lobswu) print *, "pe=",mpp_pe(),"1. Ice%obswu=",Ice%obswu(is:ie, js:je, 0:lkvl)
        if (lobswv) print *, "pe=",mpp_pe(),"1. Ice%obswv=",Ice%obswv(is:ie, js:je, 0:lkvl)
        call flush()
      endif
      call mpp_sync
!!!      call error_mesg('ice_model_init','TERMINATED', FATAL )
!!!  endif                                                                                                      
    endif
    if (GDCHK0) print *, "I am in ice_model_init 7.4"
    if (.not.lobswt) then
      allocate (Ice%obswt(is:ie, js:je, 0:lkvl+1))
      Ice%obswt=xmissing
    endif
    if (.not.lobsws) then
      allocate (Ice%obsws(is:ie, js:je, 0:lkvl+1))
      Ice%obsws=xmissing
    endif
    if (.not.lobswu) then
      allocate (Ice%obswu(is:ie, js:je, 0:lkvl+1))
      Ice%obswu=xmissing
    endif
    if (.not.lobswv) then
      allocate (Ice%obswv(is:ie, js:je, 0:lkvl+1))
      Ice%obswv=xmissing
    endif
    if (GDCHK0) print *, "I am in ice_model_init 7.5"
  endif
  
  call mpp_clock_end(sit_vdiff_ini_Clock)  
  call mpp_clock_end(sitClock)

  if (GDCHK0) print *, "I am in ice_model_init 8.0."
    
  ! << sit
  !  -------- read restart --------
  if (do_netcdf_restart) call ice_register_restart(Ice, 'ice_model.res.nc')

  if (file_exist('INPUT/ice_model.res.nc') ) then
  
    if (GDCHK0) print *, "I am in ice_model_init 8.1: ice_model.res.nc"
  
  
  !if (mpp_pe() == mpp_root_pe()) call error_mesg ('ice_model_mod', &
  !         'Reading NetCDF formatted restart file: INPUT/ice_model.res.nc', NOTE)
    call restore_state(Ice_restart)

    if (.not. query_initialized(Ice_restart, id_restart_albedo)) then
      if (mpp_pe() == mpp_root_pe()) call error_mesg ('ice_model_mod', &
                'Initializing diffuse and direct streams to albedo', NOTE)
    ! initialize ocean values - ice values initialized below
      Ice%albedo_vis_dir(:,:,1) = Ice%albedo(:,:,1)
      Ice%albedo_nir_dir(:,:,1) = Ice%albedo(:,:,1)
      Ice%albedo_vis_dif(:,:,1) = Ice%albedo(:,:,1)
      Ice%albedo_nir_dif(:,:,1) = Ice%albedo(:,:,1)
    endif

  else if (file_exist('INPUT/ice_model.res')) then

    if (GDCHK0) print *, "I am in ice_model_init 8.2: ice_model.res"

    if (mpp_pe() == mpp_root_pe()) call error_mesg ('ice_model_mod', &
         'Reading native formatted restart file.', NOTE)
    
    unit = open_restart_file ('INPUT/ice_model.res', 'read')
    
    read  (unit) control
    
    ! must use correct restart version with native format
    if (trim(control) /= trim(restart_format)) call error_mesg &
         ('ice_model_init', 'invalid restart format', FATAL)
    
    read  (unit) mlon, mlat, npart
    
    !     ---- restart resolution must be consistent with input args ---
    if (mlon /= nlon .or. mlat /= nlat .or. npart /= 2)  &
         call error_mesg ('ice_model_init',           &
         'incorrect resolution on restart', FATAL)
    
    call read_data ( unit, Ice%part_size  )
    call read_data ( unit, Ice%temp       )
    call read_data ( unit, Ice%thickness  )
    call read_data ( unit, Ice%albedo     )
    
    call read_data ( unit, Ice%albedo_vis_dir )
    call read_data ( unit, Ice%albedo_nir_dir )
    call read_data ( unit, Ice%albedo_vis_dif )
    call read_data ( unit, Ice%albedo_nir_dif )
    
    call read_data ( unit, Ice%rough_mom  )
    call read_data ( unit, Ice%rough_heat )
    call read_data ( unit, Ice%rough_moist)
    call read_data ( unit, Ice%u_surf     )
    call read_data ( unit, Ice%v_surf     )
!!!    call read_data ( unit, Ice%fluxn     )              ! bjt
!!!    call read_data ( unit, Atmos_boundary%dfluxn    )              ! bjt
!!!    call read_data ( unit, Ice%sw_flux     )              ! bjt
    call read_data ( unit, Ice%frazil     )
    call read_data ( unit, Ice%flux_u_bot )
    call read_data ( unit, Ice%flux_v_bot )

    call mpp_clock_begin(sitClock)
    call mpp_clock_begin(sit_vdiff_ini_Clock)     
    
    call read_data ( fname, 'sitmask',       Ice%sitmask,        Ice%Domain)
    call read_data ( fname, 'obsseaice',     Ice%obsseaice,      Ice%Domain)
    call read_data ( fname, 'obswtb',        Ice%obswtb,         Ice%Domain)
    call read_data ( fname, 'obswsb',        Ice%obswsb,         Ice%Domain)

#ifdef MIMIC_SIT
    if (.true.) then
#else
    if (do_sit) then
#endif    
      !!! call read_data ( fname, 'cor',           Ice%cor,            Ice%Domain)
      call read_data ( fname, 'slm',           Ice%slm,            Ice%Domain)
      call read_data ( fname, 'lclass',        Ice%lclass,         Ice%Domain)
      call read_data ( fname, 'bathy',         Ice%bathy,          Ice%Domain)
      call read_data ( fname, 'wlvl',          Ice%wlvl,           Ice%Domain)
      call read_data ( fname, 'ocnmask',       Ice%ocnmask,        Ice%Domain)
      call read_data ( fname, 'obox_mask',     Ice%obox_mask,      Ice%Domain)
      !!! call read_data ( fname, 'sni',           Ice%sni,            Ice%Domain)           ! pointer to thickness(:,:,1)
      !!! call read_data ( fname, 'siced',           Ice%siced,            Ice%Domain)       ! pointer to thickness(:,:,1)
      !!! call read_data ( fname, 'tsi',           Ice%tsi,            Ice%Domain)
      !!! call read_data ( fname, 'tsw',           Ice%tsw,            Ice%Domain)
      call read_data ( fname, 'tsl',           Ice%tsl,            Ice%Domain)
      call read_data ( fname, 'tslm',          Ice%tslm,            Ice%Domain)
      call read_data ( fname, 'tslm1',         Ice%tslm1,            Ice%Domain)
      !!! call read_data ( fname, 'ocu',           Ice%ocu,            Ice%Domain)
      !!! call read_data ( fname, 'ocv',           Ice%ocv,            Ice%Domain)
      call read_data ( fname, 'ctfreez2',      Ice%ctfreez2,       Ice%Domain)
    
      ! 2-d SIT vars
      !!!bcall read_data ( fname, 'wtb',           Ice%wtb,           Ice%Domain)
      !!!bcall read_data ( fname, 'wub',           Ice%wub,           Ice%Domain)
      !!!bcall read_data ( fname, 'wvb',           Ice%wvb,           Ice%Domain)
      !!!bcall read_data ( fname, 'wsb',           Ice%wsb,           Ice%Domain)
      !!!bcall read_data ( fname, 'fluxiw',        Ice%fluxiw,        Ice%Domain)
      !!!bcall read_data ( fname, 'pme2',          Ice%pme2,          Ice%Domain)
      !!!bcall read_data ( fname, 'subfluxw',      Ice%subfluxw,      Ice%Domain)
      !!!bcall read_data ( fname, 'wsubsal',       Ice%wsubsal,       Ice%Domain)
      call read_data ( fname, 'cc',            Ice%cc,            Ice%Domain)
      call read_data ( fname, 'hc',            Ice%hc,            Ice%Domain)
      call read_data ( fname, 'engwac',        Ice%engwac,        Ice%Domain)
      call read_data ( fname, 'sc',            Ice%sc,            Ice%Domain)
      call read_data ( fname, 'saltwac',       Ice%saltwac,       Ice%Domain)
      !!! call read_data ( fname, 'wtfns',         Ice%wtfns,         Ice%Domain)
      !!! call read_data ( fname, 'wsfns',         Ice%wsfns,         Ice%Domain)  
    
      ! 3-d SIT vars: snow/ice
      call read_data ( fname, 'zsi',           Ice%zsi,           Ice%Domain)
      call read_data ( fname, 'silw',          Ice%silw,          Ice%Domain)
      call read_data ( fname, 'tsnic',         Ice%tsnic,         Ice%Domain)
    
      ! 3-d SIT vars: water column
      !!! if (.NOT.lwoa_gfdl) then
      !!!   call read_data ( fname, 'obswt',         Ice%obswt,         Ice%Domain)
      !!!   call read_data ( fname, 'obsws',         Ice%obsws,         Ice%Domain)
      !!!   call read_data ( fname, 'obswu',         Ice%obswu,         Ice%Domain)
      !!!   call read_data ( fname, 'obswv',         Ice%obswv,         Ice%Domain)
      !!! endif
      call read_data ( fname, 'wt',            Ice%wt,            Ice%Domain)
      call read_data ( fname, 'ws',            Ice%ws,            Ice%Domain)
      call read_data ( fname, 'wu',            Ice%wu,            Ice%Domain)
      call read_data ( fname, 'wv',            Ice%wv,            Ice%Domain)
      call read_data ( fname, 'ww',            Ice%ww,            Ice%Domain)
      call read_data ( fname, 'wtke',          Ice%wtke,          Ice%Domain)
      call read_data ( fname, 'wlmx',          Ice%wlmx,          Ice%Domain)
      call read_data ( fname, 'wldisp',        Ice%wldisp,        Ice%Domain)
      call read_data ( fname, 'wkm',           Ice%wkm,           Ice%Domain)
      call read_data ( fname, 'wkh',           Ice%wkh,           Ice%Domain)
      !!! call read_data ( fname, 'wrho1000',      Ice%wrho1000,      Ice%Domain)
      !!! call read_data ( fname, 'wtfn',          Ice%wtfn,          Ice%Domain)
      !!! call read_data ( fname, 'wsfn',          Ice%wsfn,          Ice%Domain)
      !!! call read_data ( fname, 'awufl',         Ice%awufl,         Ice%Domain)
      !!! call read_data ( fname, 'awvfl',         Ice%awvfl,         Ice%Domain)
      !!! call read_data ( fname, 'awtfl',         Ice%awtfl,         Ice%Domain)
      !!! call read_data ( fname, 'awsfl',         Ice%awsfl,         Ice%Domain)
      !!! call read_data ( fname, 'awtkefl',       Ice%awtkefl,       Ice%Domain)
    
      ! 4- OUTPUT only, original ATM variabels
      !!! call read_data ( fname, 'seaice',        Ice%seaice,        Ice%Domain)
      !!! call read_data ( fname, 'grndcapc',      Ice%grndcapc,      Ice%Domain)
      !!! call read_data ( fname, 'grndhflx',      Ice%grndhflx,      Ice%Domain)
      !!! call read_data ( fname, 'grndflux',      Ice%grndflux,      Ice%Domain)
    endif
    call mpp_clock_end(sit_vdiff_ini_Clock)
    call mpp_clock_end(sitClock)
    
    call close_file (unit)

  else

    !     ----- no restart - no ice -----
    if (GDCHK0) print *, "I am in ice_model_init 8.3: no restart"
  
    Ice%temp      = tfreeze + temp_ice_freeze
    Ice%thickness = 0.0
!!!    Ice%part_size         = 0.0
    Ice%part_size         = xmissing                     ! bjt, set to missing value first
    Ice%part_size (:,:,1) = 1.0
    Ice%albedo     = 0.14
   !initialize ocean values - ice values initialized below
    Ice%albedo_vis_dir(:,:,1) = Ice%albedo(:,:,1)
    Ice%albedo_nir_dir(:,:,1) = Ice%albedo(:,:,1)
    Ice%albedo_vis_dif(:,:,1) = Ice%albedo(:,:,1)
    Ice%albedo_nir_dif(:,:,1) = Ice%albedo(:,:,1)
    Ice%rough_mom  = 0.0004
    Ice%rough_heat = 0.0004
    Ice%rough_moist= 0.0004
    Ice%u_surf     = 0.0
    Ice%v_surf     = 0.0

!!!    Ice%fluxn     = 0.0     ! bjt
!!!    Atmos_boundary%dfluxn    = 0.0     ! bjt
!!!    Ice%sw_flux     = 0.0     ! bjt

    Ice%frazil     = 0.0

    call mpp_clock_begin(sitClock)
    call mpp_clock_begin(sit_vdiff_ini_Clock)

   !bjt
    if (lobsws) then
      where (Ice%mask)
        Ice%obswsb  = MERGE(Ice%obsws(:,:,1),36.3, Ice%obsws(:,:,1).ne.xmissing)        ! set observed SSS fom WOA dataset
      elsewhere
      ! over land/glacier
        Ice%obswsb  = MERGE(Ice%obsws(:,:,1),0., Ice%obsws(:,:,1).ne.xmissing)         ! set observed SSS at 0 PSU over land water
                                  ! Other variables remains to be missing values
      endwhere
    else
      Ice%obswsb  = 36.3                                                              ! set observed SSS at 36.3 PSU (We need to seperate for lake)  ?????????????
    endif

    call mpp_clock_end(sit_vdiff_ini_Clock)
    call mpp_clock_end(sitClock)
  
    !     --- open water roughness (initially fixed) ---
  
    call fixed_ocean_roughness ( Ice%mask, Ice%rough_mom  (:,:,1), &
         Ice%rough_heat (:,:,1), &
         Ice%rough_moist(:,:,1)  )

    if (GDCHK0) print *, "I am in ice_model_init 8.3.1."
      
    !----------------------------------------------------------
  
   
      ! 1.0 set obsseaice and obswtb
  
      ! if (sst_method == "aqua_planet_sit") then
      !  Ice%obsseaice=0.
      !   ice_method = 'none'
      !   Ice%ice_mask = .false.
      !  ! constants
      !  lon0 = 0.
      !   latd = 30.*pi/180.
      !  amp = 3.
      !   where (Ice%mask)
      !       Ice%obswtb = 27.*(1.-sin(max(min(1.5*Ice%lat,pi*0.5),-pi*0.5))**2) + TFREEZE + &
      !                    amp * cos(Ice%lon-lon0) * cos(0.5*pi*min(max(Ice%lat/latd,-1.),1.))**2
      !  endwhere
      ! else
      if (GDCHK0) print *, "I am in ice_model_init 8.3.2."

        !---- get the specified sea-ice fraction -----
        call get_amip_ice (Ice%Time, Amip_ice, Ice%obsseaice )
        !---- get the specified ocean temperature -----
        call get_amip_sst (Ice%Time, Amip_sst, Ice%obswtb, Ice%lon, Ice%lat )
      ! endif
!!!      if ( GDCHK0 ) then
!!!        print *, "I am in ice_model_init 8.3.3."
!!!        print *, "Ice%obsseaice=",Ice%obsseaice
!!!        print *, "Ice%obswtb=",Ice%obswtb
!!!      endif
 
    !!! else
      
      ! 2.0 set sst, sss and sic
      ! 2.1 lets work on "seaice"
      where (Ice%mask)
        where (Ice%obsseaice.NE.xmissing)
          Ice%seaice=Ice%obsseaice
        elsewhere
          where (Ice%obswtb.NE.xmissing)
            where (Ice%obswtb.LE.TFREEZE)
              Ice%seaice=1.
            elsewhere
              Ice%seaice=0.
            endwhere
          elsewhere
!!!            where (Atmos_boundary%t_bot.LE.TFREEZE)
!!!              Ice%seaice(:,:)=1.
!!!            elsewhere
            ! assume no ice for missing data
              Ice%seaice(:,:)=0.
!!!            endwhere
          endwhere    
        endwhere
        Ice%part_size(:,:,1) = 1.0 - Ice%part_size(:,:,2)
      endwhere
      if ( GDCHK0 ) print *, "I am in ice_model_init 8.3.4."

      ! 2.2 lets work on sea ice skin temp "tsi or Ice%t_surf(:,:,2)"

      where (Ice%seaice.GT.0.)
      ! assume to be tfreeze + temp_ice_freeze
        Ice%tsi=tfreeze + temp_ice_freeze
        !!! Ice%tsi=TFREEZE
        !!! Ice%tsi=MIN(Atmos_boundary%t_bot(:,:), TFREEZE)
      elsewhere
        Ice%tsi=TFREEZE
      endwhere
      if ( GDCHK0 ) print *, "I am in ice_model_init 8.3.5."

      ! 2.3 lets work on SST "tsw or Ice%t_surf(:,:,1)"

      where (Ice%seaice.GT.0.)
        Ice%tsw=TFREEZE
      elsewhere
        where (Ice%obswtb.NE.xmissing)
          Ice%tsw=MAX(Ice%obswtb, TFREEZE)
        elsewhere
        ! assume 288 K for missing value
          Ice%tsw=288.
!!!          Ice%tsw=MAX(Atmos_boundary%t_bot(:,:), TFREEZE)
        endwhere
      endwhere

      if ( GDCHK0 ) print *, "I am in ice_model_init 8.3.6."

      ! 2.4 final setup for Ice%seaice > 0.5 logial

      where ( Ice%mask(:,:) )
        where ( Ice%mask(:,:) .and. Ice%seaice > 0.5 )
        ! iced grid
          Ice%siced(:,:) = specified_ice_thickness
          Ice%ice_mask(:,:,2) = .true.
          Ice%tsw=TFREEZE
          Ice%tsi=MIN( Ice%tsi, TFREEZE )
        elsewhere
        ! open water
          Ice%siced(:,:) = 0.0
          Ice%ice_mask(:,:,2) = .false.
          Ice%tsw=MAX( Ice%tsw, TFREEZE )
          Ice%tsi=TFREEZE
        endwhere
!      elsewhere
!      ! land grids
!!!!        IF (.FALSE.) THEN
!          Ice%siced(:,:)=xmissing
!          Ice%ice_mask(:,:,2)=.false.
!          Ice%tsw=xmissing
!          Ice%tsi=xmissing
!!!!        ELSE
!!!!          Ice%siced(:,:)=0.
!!!!          Ice%ice_mask(:,:,2)=.false.
!!!!          Ice%tsw=TFREEZE
!!!!          Ice%tsi=TFREEZE
!!!!        ENDIF
      endwhere
      if ( GDCHK0 ) print *, "I am in ice_model_init 8.3.7."

      
      ! 4. set additional SIT input variables

    call mpp_clock_begin(sitClock)
    call mpp_clock_begin(sit_vdiff_ini_Clock)

#ifdef MIMIC_SIT
    if (.true.) then
#else
    if (do_sit) then
#endif
  
      where (Ice%mask)
        Ice%lclass  = 2.           ! water landclass
        Ice%bathy   = bathy_default  ! 200 m depth for the first guess, need to read terrain data later)
        Ice%wlvl    = 0.           ! set water level at 0 m
        Ice%ocnmask=xmissing       ! not coupled to 3-D ocn model
        Ice%obox_mask=xmissing     ! not coupled to 3-D ocn model
        Ice%sni     = 0.           ! assuming no snow over seaice (m swe). It can be read from NCEP data.
        !!! Ice%tsi     = MIN( Ice%t_surf, TFREEZE )
        Ice%slm  = 0.              ! land fraction
      elsewhere
      ! over land/glacier
        Ice%lclass  = 1.           ! land landclass
        Ice%bathy   = 0.           ! 0 m for the first guess, need to read terrain data later
        Ice%wlvl    = Ice%bathy-1. ! set water level at 1 m below the bathy for land grids
        Ice%ocnmask=xmissing       ! not coupled to 3-D ocn model
        Ice%obox_mask=xmissing     ! not coupled to 3-D ocn model
        Ice%sni     = 0.           ! assuming initially no snow over seaice (m swe)
        !!! Ice%tsi     = MIN( Ice%t_surf, TFREEZE )
                                   ! Other variables remains to be missing values
        Ice%slm  = 1.              ! land fraction
      endwhere
      if ( GDCHK0 ) print *, "I am in ice_model_init 8.3.8. maskid=",maskid
      
      if (maskid==0) then
      !   0: OCN model grids only
      !   Should be coded afterwards.
        where (Ice%mask)
           Ice%sitmask = 1.           ! all the water grids
        elsewhere
           Ice%sitmask = 0.           ! rest of the grids (such as land, glacier)
        endwhere
      elseif (maskid==1) then
      !   1: Ocean and lakes (default)
      ! no lake mask in this version. It should be changed later
        where (Ice%mask)
           Ice%sitmask = 1.           ! all the water grids
        elsewhere
           Ice%sitmask = 0.           ! rest of the grids (such as land, glacier)
        endwhere
      elseif (maskid==2) then
      !   2: Ocean      
        where (Ice%mask)
           Ice%sitmask = 1.           ! all the water grids
        elsewhere
           Ice%sitmask = 0.           ! rest of the grids (such as land, glacier)
        endwhere
      elseif (maskid==3) then
      !   3: Ocean within 30N-30S
        if ( GDCHK0 ) print *, "I am in ice_model_init 8.3.8.1: maskid=",maskid,"Ocean within 30N-30S"
        where (Ice%mask .and.         &
          -30. < Ice%lat*180/pi .and. Ice%lat*180/pi < 30.)
         ! only within 30S to 30 N
           Ice%sitmask = 1.           ! all the water grids
        elsewhere
           Ice%sitmask = 0.           ! rest of the grids (such as land, glacier)
        endwhere
      elseif (maskid==4) then
      !   4: all the grid
        Ice%sitmask = 1.           ! all the water/land grids      
      else
        Ice%sitmask = 0.           ! NON sit grids      
      endif
!----------------------------------------------------------
      if ( GDCHK0 ) print *, "I am in ice_model_init 8.3.9: call sit_vdiff_init (Ice)"
      call sit_vdiff_init (Ice)
!----------------------------------------------------------
      if ( GDCHK0 ) print *, "I am in ice_model_init 8.3.10."      
      Ice%tsl     = (Ice%seaice*Ice%tsi+(1-Ice%seaice)*Ice%tsw)
      Ice%tslm    = Ice%tsl
      Ice%tslm1   = Ice%tsl
      if ( GDCHK0 ) print *, "I am in ice_model_init 8.3.11."     
    endif
    call mpp_clock_end(sit_vdiff_ini_Clock)
    call mpp_clock_end(sitClock)
  endif


!----------------------------------------------------------                      
    if ( GDCHK0 ) print *, "I am in ice_model_init 8.4.1"                        
                                                                                 
!! set ice partiton values to that of Ice%albedo.                                
   Ice%albedo_vis_dir(:,:,2) = Ice%albedo(:,:,2)                                 
   Ice%albedo_nir_dir(:,:,2) = Ice%albedo(:,:,2)                                 
   Ice%albedo_vis_dif(:,:,2) = Ice%albedo(:,:,2)                                 
   Ice%albedo_nir_dif(:,:,2) = Ice%albedo(:,:,2)                                 
                                                                                 
    !---- initialization of ice mask (actually where ice exists) -----           
    Ice%ice_mask = .false.                                                       
    where (Ice%mask     (:,:)           .and.     &                              
         Ice%part_size(:,:,2) > 0.0   .and.     &                                
         Ice%thickness(:,:,2) >= thickness_min) &                                
         Ice%ice_mask(:,:,2) = .true.                                            
                                                                                 
                                                                                 
    if ( GDCHK0 ) print *, "I am in ice_model_init 8.4.2"                        

                                                                                                                                                          
  call ice_albedo_init (tfreeze)

  

  ! --- diagnostics ---
  if ( GDCHK0 ) print *, "I am in ice_model_init 8.5."

  call ice_diag_init (Ice, xb, yb)

  !--- release the memory ------------------------------------------------
  !!! deallocate(lonv, latv, glon, glat, rmask, xb, yb )
  deallocate(glon, glat, rmask, xb, yb )

  call nullify_domain()

  module_is_initialized = .true.

  call mpp_clock_end(iceClock)
  if ( GDCHK0 ) print *, "I am in ice_model_init 8.6."
!----------------------------------------------------------
CONTAINS
!----------------------------------------------------------------------------------------------------

  subroutine sit_vdiff_init ( Ice )
  IMPLICIT NONE
  type (ice_data_type), intent (inout):: Ice

  ! local dimensions

  integer  unit, io, ierr
  !!! integer i,j,is,ie,js,je
  !is=Ice%is
  !ie=Ice%ie
  !js=Ice%js
  !je=Ice%je

  ! 5.0 set additional SIT ocn profle t,s,u,v and tke

!!!  where (Ice%obswsb.GE.0.)
!!!    Ice%ctfreez2=tmelts(Ice%obswsb)
!!!  elsewhere
!!!    Ice%ctfreez2=tfreeze + temp_ice_freeze
!!!  endwhere

#ifdef DEBUG
      lprint0=GDCHK0
      if (lprint0) print *, "I am in sit_vdiff_init 1.0."
      if (lprint0) print *, "Ice%lat",Ice%lat*180./pi
      if (lprint0) print *, "Ice%lon",Ice%lon*180./pi
      if (lprint0) print *, "Ice%tsw=",Ice%tsw
      if (lprint0) print *, "Ice%tsi=",Ice%tsi
      if (lprint0) print *, "Ice%t_surf=",Ice%t_surf
      if (lprint0) print *, "Ice%thickness=",Ice%thickness
      if (lprint0) print *, "Ice%seaice=",Ice%seaice
      if (lprint0) print *, "Ice%obswsb=",Ice%obswsb
#endif



  
  do j = js, je
    do i = is, ie
#ifdef DEBUG
      lprint2=GDCHK2
#endif
      if (.true.) then
        Ice%ctfreez2(i,j)=tfreeze + temp_ice_freeze
      else if (Ice%obswsb(i,j).GE.0.) then
        Ice%ctfreez2(i,j)=tmelts(Ice%obswsb(i,j))
      else
        Ice%ctfreez2(i,j)=tfreeze + temp_ice_freeze
      endif
!!!#ifdef DEBUG
!!!      lprint2=GDCHK2
!!!      if (lprint2) print *, "I am in sit_vdiff_init 1.0."
!!!      if (lprint2) print *, "Ice%lat",Ice%lat(i,j)*180./pi
!!!      if (lprint2) print *, "Ice%lon",Ice%lon(i,j)*180./pi
!!!      if (lprint2) print *, "Ice%tsw=",Ice%tsw(i,j)
!!!      if (lprint2) print *, "Ice%tsi=",Ice%tsi(i,j)
!!!      if (lprint2) print *, "Ice%t_surf=",Ice%t_surf
!!!      if (lprint2) print *, "Ice%thickness=",Ice%thickness
!!!      if (lprint2) print *, "Ice%seaice=",Ice%seaice(i,j)
!!!      if (lprint2) print *, "Ice%obswsb=",Ice%obswsb(i,j)  
!!!      if (lprint2) print *, "Ice%ctfreez2=",Ice%ctfreez2(i,j)
!!!#endif
      call sit_vdiff_init_1d ( istep,                               &
         ! 0-INPUT only, original ATM/SIT variabels
              Ice%lat(i,j)*180./pi, Ice%lon(i,j)*180./pi,                              &
              Ice%sitmask(i,j),Ice%bathy(i,j), Ice%wlvl(i,j),                          &
              Ice%ocnmask(i,j), Ice%obox_mask(i,j),                                    &
              Ice%sni(i,j), Ice%siced(i,j), Ice%tsi(i,j),                          &
              Ice%obsseaice(i,j), Ice%obswtb(i,j), Ice%obswsb(i,j),                    &
              Ice%ctfreez2(i,j),                                                       &
         ! 2-d SIT vars
              Ice%wtb(i,j), Ice%wub(i,j), Ice%wvb(i,j),                                &
              Ice%wsb(i,j),                                                            &
              Ice%subfluxw(i,j), Ice%wsubsal(i,j),                                     &
              Ice%cc(i,j), Ice%hc(i,j), Ice%engwac(i,j),                               &
              Ice%sc(i,j), Ice%saltwac(i,j),                                           &
              Ice%wtfns(i,j), Ice%wsfns(i,j),                                          &
         ! 3-d SIT vars: snow/ice
              Ice%zsi(i,j,:), Ice%silw(i,j,:), Ice%tsnic(i,j,:),                       &
         ! 3-d SIT vars: water column
              Ice%obswt(i,j,:), Ice%obsws(i,j,:), Ice%obswu(i,j,:), Ice%obswv(i,j,:),  &
              Ice%wt(i,j,:), Ice%wu(i,j,:), Ice%wv(i,j,:), Ice%ww(i,j,:),              &
              Ice%ws(i,j,:), Ice%wtke(i,j,:), Ice%wlmx(i,j,:),                         &
              Ice%wldisp(i,j,:), Ice%wkm(i,j,:), Ice%wkh(i,j,:),                       &
              Ice%wrho1000(i,j,:),                                                     &
              Ice%wtfn(i,j,:), Ice%wsfn(i,j,:),                                        &
              Ice%awufl(i,j,:), Ice%awvfl(i,j,:), Ice%awtfl(i,j,:),                    &
              Ice%awsfl(i,j,:), Ice%awtkefl(i,j,:),                                    &
         ! 4- OUTPUT only, original ATM variabels
              Ice%seaice(i,j),                                                         &
              Ice%grndcapc(i,j), Ice%grndhflx(i,j), Ice%grndflux(i,j)    )
    enddo
  enddo
  Ice%part_size(:,:,1) = 1.0 - Ice%part_size(:,:,2)

#ifdef DEBUG
      if (lprint0) print *, "I am in sit_vdiff_init 2.0."
      if (lprint0) print *, "Ice%lat",Ice%lat*180./pi
      if (lprint0) print *, "Ice%lon",Ice%lon*180./pi
      if (lprint0) print *, "Ice%tsw",Ice%tsw
      if (lprint0) print *, "Ice%tsi=",Ice%tsi
      if (lprint0) print *, "Ice%t_surf=",Ice%t_surf
      if (lprint0) print *, "Ice%thickness=",Ice%thickness
      if (lprint0) print *, "Ice%seaice=",Ice%seaice          
#endif
   !--------------------------
   END SUBROUTINE sit_vdiff_init

  end subroutine ice_model_init
!=============================================================================================
subroutine print_layout (npes, layout, Domain)
integer, intent(in) :: npes, layout(2)
type(domain2d), intent(in) :: Domain
integer, dimension(0:npes-1) :: xsize, ysize
integer :: i, j, xlist(layout(1)), ylist(layout(2))
type (domain1D) :: Xdom, Ydom

call mpp_get_compute_domains   ( Domain, xsize=xsize, ysize=ysize )
call mpp_get_domain_components ( Domain, Xdom, Ydom )
call mpp_get_pelist ( Xdom, xlist )
call mpp_get_pelist ( Ydom, ylist ) 

write (*,100)             
write (*,110) (xsize(xlist(i)),i=1,layout(1))
write (*,120) (ysize(ylist(j)),j=1,layout(2))
                               
100 format ('ICE MODEL DOMAIN DECOMPOSITION')
110 format ('  X-AXIS = ',24i4,/,(11x,24i4))
120 format ('  Y-AXIS = ',24i4,/,(11x,24i4))

end subroutine print_layout
!=============================================================================================
subroutine update_ice_model_fast( Atmos_boundary, Ice )
type(atmos_ice_boundary_type), intent(in) :: Atmos_boundary
type (ice_data_type), intent(inout) :: Ice

!!! v0.473
!!!#ifdef DEBUG
real, dimension(is:ie, js:je, num_part) ::  &
!!!      ts_new, gamma, fluxn, t_dt_surf, flux_t_new, flux_q_new, flux_lw_new, sw_flux, &
      ts_new, gamma, flux_i_new, t_dt_surf, flux_t_new, flux_q_new, flux_lw_new, flux_sw_new, &
      flux_sw_vis_new, flux_sw_dir_new, flux_sw_dif_new, &
      flux_sw_vis_dir_new, flux_sw_vis_dif_new, &
      flux_u_new, flux_v_new, lprec_new, fprec_new, flux_lh_new,   &
      dflux_i_new
!!!#else
!!!real, dimension(size(Ice%flux_u_bot,1),size(Ice%flux_u_bot,2),size(Ice%flux_u_bot,3)) ::  &
!!!!!!      ts_new, gamma, fluxn, t_dt_surf, flux_t_new, flux_q_new, flux_lw_new, sw_flux, &
!!!      ts_new, gamma, fluxn, t_dt_surf, flux_t_new, flux_q_new, flux_lw_new, &
!!!      flux_sw_vis_new, flux_sw_dir_new, flux_sw_dif_new, &
!!!      flux_sw_vis_dir_new, flux_sw_vis_dif_new, &
!!!      flux_u_new, flux_v_new, lprec_new, fprec_new, flux_lh_new,   &
!!!      dfluxn
!!!#endif
!!! v0.46
#ifdef ONEWAY
real, dimension(size(Ice%flux_u_bot,1),size(Ice%flux_u_bot,2)) ::  &
             seaice,                                                        &
             sni, siced, tsi, tsw,               &
             tsl, tslm, tslm1,                            &
             ocu, ocv,  ctfreez2,                         &
             grndcapc, grndhflx, grndflux
#endif
integer:: i,j                                                ! bjt

!-----------------------------------------------------------------------
!
!   updates ice model on the atmospheric (fast) time step 
!   averages input quantities to be seen by the ocean
!
!    flux_u  = zonal wind stress
!    flux_v  = meridional wind stress
!    flux_sw = net shortwave radiation (down-up) 
!    flux_sw_vis = net visible shortwave radiation (down-up)
!    flux_sw_dir = net direct shortwave radiation (down-up)
!    flux_sw_dif = net diffuse shortwave radiation (down-up)
!    flux_sw_vis_dir = net visible direct shortwave radiation (down-up)
!    flux_sw_vis_dif = net visible diffuse shortwave radiation (down-up)
!    flux_sw_nir_dir = net near IR direct shortwave radiation (down-up)
!    flux_sw_nir_dif = net near IR diffuse shortwave radiation (down-up)
!    flux_lw = net longwave radiation (down-up) 
!    flux_t  = sensible heat flux
!    flux_q  = specific humidity flux
!    flux_lh = latent heat flux
!    lprec   = liquid precipitiation rate (kg/m2/s)
!    fprec   = frozen precipitiation rate (kg/m2/s)
!    coszen  = cosine of the zenith angle
!
!-----------------------------------------------------------------------
!----- set up local copies of fluxes for modification -----
  call mpp_clock_begin(iceClock)

flux_u_new  = Atmos_boundary%u_flux
flux_v_new  = Atmos_boundary%v_flux
flux_t_new  = Atmos_boundary%t_flux
flux_q_new  = Atmos_boundary%q_flux
flux_lh_new = Atmos_boundary%q_flux*hlv
flux_lw_new = Atmos_boundary%lw_flux
Atmos_boundary%sw_flux=Atmos_boundary%sw_flux_vis_dir + &
                 Atmos_boundary%sw_flux_vis_dif + &
                 Atmos_boundary%sw_flux_nir_dir + &
                 Atmos_boundary%sw_flux_nir_dif
flux_sw_new = Atmos_boundary%sw_flux
flux_sw_vis_new = Atmos_boundary%sw_flux_vis_dir + Atmos_boundary%sw_flux_vis_dif
flux_sw_dir_new = Atmos_boundary%sw_flux_vis_dir + Atmos_boundary%sw_flux_nir_dir
flux_sw_dif_new = Atmos_boundary%sw_flux_vis_dif + Atmos_boundary%sw_flux_nir_dif
flux_sw_vis_dir_new = Atmos_boundary%sw_flux_vis_dir
flux_sw_vis_dif_new = Atmos_boundary%sw_flux_vis_dif
lprec_new   = Atmos_boundary%lprec
fprec_new   = Atmos_boundary%fprec

  if (GDCHK0) print *, "I am in update_ice_model_fast 1.0: by original scheme first."

!!!  if (GDCHK0) print *, "Atmos_boundary%sw_flux_vis_dir=",Atmos_boundary%sw_flux_vis_dir(II,JJ,:)
!!!  if (GDCHK0) print *, "Atmos_boundary%sw_flux_vis_dif=", Atmos_boundary%sw_flux_vis_dif(II,JJ,:)
!!!  if (GDCHK0) print *, "Atmos_boundary%sw_flux_nir_dir=",Atmos_boundary%sw_flux_nir_dir(II,JJ,:)
!!!  if (GDCHK0) print *, "Atmos_boundary%sw_flux_nir_dif=",Atmos_boundary%sw_flux_nir_dif(II,JJ,:)
!!!  if (GDCHK0) print *, "flux_sw_new=",flux_sw_new

  !----- implicit update of ice surface temperature -----
  ts_new = 0.0
  
  where (Ice%ice_mask)
    gamma = diff / max(Ice%thickness,thickness_min)
    Atmos_boundary%fluxn = gamma * (tfreeze + temp_ice_freeze - Ice%t_surf)
  
    t_dt_surf = (Atmos_boundary%fluxn + Atmos_boundary%lw_flux + &
                 Atmos_boundary%sw_flux_vis_dir + &
                 Atmos_boundary%sw_flux_vis_dif + &
                 Atmos_boundary%sw_flux_nir_dir + &
                 Atmos_boundary%sw_flux_nir_dif - &
                 Atmos_boundary%t_flux - Atmos_boundary%q_flux*latent)           &
             / (Atmos_boundary%dhdt + Atmos_boundary%dedt*latent + Atmos_boundary%drdt + gamma)
  
    ts_new = Ice%t_surf + t_dt_surf
    flux_lh_new = flux_lh_new + Atmos_boundary%q_flux*hlf
  endwhere
  
  !   ----- compute new fluxes (adjusted for temp change) -----
  !              (longwave up has negative sign)
  
  where (Ice%ice_mask .and. ts_new > tfreeze )
    t_dt_surf   = tfreeze - Ice%t_surf
    Ice%t_surf  = tfreeze
    flux_t_new  = flux_t_new  + t_dt_surf * Atmos_boundary%dhdt
    flux_q_new  = flux_q_new  + t_dt_surf * Atmos_boundary%dedt
    flux_lh_new = flux_lh_new + t_dt_surf * Atmos_boundary%dedt*latent
    flux_lw_new = flux_lw_new - t_dt_surf * Atmos_boundary%drdt
  endwhere
  
  where (Ice%ice_mask .and. ts_new <= tfreeze)
    Ice%t_surf  = Ice%t_surf  + t_dt_surf
    flux_t_new  = flux_t_new  + t_dt_surf * Atmos_boundary%dhdt
    flux_q_new  = flux_q_new  + t_dt_surf * Atmos_boundary%dedt
    flux_lh_new = flux_lh_new + t_dt_surf * Atmos_boundary%dedt*latent
    flux_lw_new = flux_lw_new - t_dt_surf * Atmos_boundary%drdt
  endwhere

call mpp_clock_begin(sitClock)
call mpp_clock_begin(sit_vdiff_Clock)

!!! v0.45
#ifdef MIMIC_SIT
if (.true.) then
#else
if (do_sit) then
#endif

  if (GDCHK0) print *, "I am in update_ice_model_fast 2.0: SIT model."


  call get_time ( Ice%Time_step_fast, dt )
#ifdef DEBUG
  if (GDCHK0) print *, "dt=",dt
#endif    
  
  ! net heat flux over ocean grids
  where (Ice%mask)                     ! ocean grid
    ! positive downward (Rln+Rsn-H-LE)
    !!! sw_flux=Atmos_boundary%sw_flux_vis_dir + &
    !!!                Atmos_boundary%sw_flux_vis_dif + &
    !!!                Atmos_boundary%sw_flux_nir_dir + &
    !!!                Atmos_boundary%sw_flux_nir_dif
    Atmos_boundary%fluxn(:,:,2) = ( Atmos_boundary%lw_flux(:,:,2) + Atmos_boundary%sw_flux(:,:,2) -               &
                 Atmos_boundary%t_flux(:,:,2) - Atmos_boundary%q_flux(:,:,2)*LATENT )
    Atmos_boundary%fluxn(:,:,1) = ( Atmos_boundary%lw_flux(:,:,1) + Atmos_boundary%sw_flux(:,:,1) -               &
                 Atmos_boundary%t_flux(:,:,1) - Atmos_boundary%q_flux(:,:,1)*HLV -   &
                 Atmos_boundary%fprec(:,:,1)*HLF )
    Atmos_boundary%dfluxn(:,:,2) = ( Atmos_boundary%dhdt(:,:,2) + Atmos_boundary%dedt(:,:,2)*LATENT +    &
                 Atmos_boundary%drdt(:,:,2))
    Atmos_boundary%dfluxn(:,:,1) = ( Atmos_boundary%dhdt(:,:,1) + Atmos_boundary%dedt(:,:,1)*HLV +       &
                 Atmos_boundary%drdt(:,:,1))
  elsewhere
    Atmos_boundary%fluxn(:,:,2) = xmissing
    Atmos_boundary%fluxn(:,:,1) = xmissing
    Atmos_boundary%dfluxn(:,:,2) = xmissing
    Atmos_boundary%dfluxn(:,:,1) = xmissing
  endwhere
!!!#ifdef DEBUG
!!!  if (GDCHK0) print *, "I am in update_ice_model_fast 3.0."
!!!  if (GDCHK0) print *, "is=",is
!!!  if (GDCHK0) print *, "ie=",ie
!!!  if (GDCHK0) print *, "js=",js
!!!  if (GDCHK0) print *, "je=",je
!!!  if (GDCHK0) print *, "cor=",Ice%cor(II,JJ)
!!!  if (GDCHK0) print *, "Ice%lat=",Ice%lat(II,JJ)*180./pi
!!!  if (GDCHK0) print *, "Ice%lon=",Ice%lon(II,JJ)*180./pi
!!!  if (GDCHK0) print *, "Ice%bathy=",Ice%bathy(II,JJ)
!!!  if (GDCHK0) print *, "Ice%wlvl=",Ice%wlvl(II,JJ)
!!!  if (GDCHK0) print *, "Ice%sitmask=",Ice%sitmask(II,JJ)
!!!  if (GDCHK0) print *, "Atmos_boundary%lw_flux=",Atmos_boundary%lw_flux(II,JJ,:)  
!!!  if (GDCHK0) print *, "Atmos_boundary%sw_flux=",Atmos_boundary%sw_flux(II,JJ,:)
!!!  if (GDCHK0) print *, "Atmos_boundary%fluxn=",Atmos_boundary%fluxn(II,JJ,:)
!!!  if (GDCHK0) print *, "Atmos_boundary%dfluxn=",Atmos_boundary%dfluxn(II,JJ,:)
!!!  if (GDCHK0) print *, "Atmos_boundary%t_flux=",Atmos_boundary%t_flux(II,JJ,:)  
!!!  if (GDCHK0) print *, "Atmos_boundary%le_flux=",Atmos_boundary%q_flux(II,JJ,:)*hlv 
!!!  if (GDCHK0) print *, "Atmos_boundary%fluxn=",Atmos_boundary%fluxn(II,JJ,:)
!!!  if (GDCHK0) print *, "Atmos_boundary%dfluxn=",Atmos_boundary%dfluxn(II,JJ,:)
!!!  if (GDCHK0) print *, "Atmos_boundary%u_flux=",Atmos_boundary%u_flux(II,JJ,:)
!!!  if (GDCHK0) print *, "Atmos_boundary%v_flux=",Atmos_boundary%v_flux(II,JJ,:)
!!!  if (GDCHK0) print *, "Atmos_boundary%lprec=",Atmos_boundary%lprec(II,JJ,:)
!!!  if (GDCHK0) print *, "Atmos_boundary%fprec=",Atmos_boundary%fprec(II,JJ,:)
!!!  if (GDCHK0) print *, "Atmos_boundary%t_bot=",Atmos_boundary%tprec(II,JJ,:)
!!!  if (GDCHK0) print *, "Atmos_boundary%gust=",Atmos_boundary%gust(II,JJ,:)
!!!  if (GDCHK0) print *, "Ice%runoff=",Ice%runoff(II,JJ)
!!!  if (GDCHK0) print *, "Ice%calving=",Ice%calving(II,JJ)
!!!#endif

  ! update surface (snow/ice/sea surface) temperaure of ocean grid
  ! note: temperatures allowed below freezing for conservation


  call get_time(Ice%Time,isecs,idays)
  !!! istep=(real(isecs)+idays*86400.)/dt
#ifdef DEBUG
    if (GDCHK0) print *, "istep=",istep
    if (GDCHK0) print *, "idays=",idays
    if (GDCHK0) print *, "isecs=",isecs
    lprint0=GDCHK0
    if (lprint0) print *, "I am in update_ice_model_fast 4.0:"
    if (lprint0) print *, "cor=",Ice%cor
    if (lprint0) print *, "Ice%lat=",Ice%lat*180./pi
    if (lprint0) print *, "Ice%lon=",Ice%lon*180./pi
    if (lprint0) print *, "Ice%bathy=",Ice%bathy
    if (lprint0) print *, "Ice%wlvl=",Ice%wlvl
    if (lprint0) print *, "Ice%sitmask=",Ice%sitmask
!!!    if (lprint0) print *, "Atmos_boundary%lw_flux=",Atmos_boundary%lw_flux(i,j,:)  
!!!    if (lprint0) print *, "Atmos_boundary%t_flux=",Atmos_boundary%t_flux(i,j,:)  
!!!    if (lprint0) print *, "Atmos_boundary%sw_flux=",Atmos_boundary%sw_flux(i,j,:)  
!!!    if (lprint0) print *, "Atmos_boundary%sw_flux_vis_dir=",Atmos_boundary%sw_flux_vis_dir(i,j,:)
!!!    if (lprint0) print *, "Atmos_boundary%sw_flux_vis_dif=", Atmos_boundary%sw_flux_vis_dif(i,j,:)
!!!    if (lprint0) print *, "Atmos_boundary%sw_flux_nir_dir=",Atmos_boundary%sw_flux_nir_dir(i,j,:)
!!!    if (lprint0) print *, "Atmos_boundary%sw_flux_nir_dif=",Atmos_boundary%sw_flux_nir_dif(i,j,:)
!!!    if (lprint0) print *, "Atmos_boundary%le_flux=",Atmos_boundary%q_flux(i,j,:)*hlv 
!!!    if (lprint0) print *, "Atmos_boundary%fluxn=",Atmos_boundary%fluxn(i,j,:)
!!!    if (lprint0) print *, "Atmos_boundary%dfluxn=",Atmos_boundary%dfluxn(i,j,:)
!!!    if (lprint0) print *, "Atmos_boundary%u_flux=",Atmos_boundary%u_flux(i,j,:)
!!!    if (lprint0) print *, "Atmos_boundary%v_flux=",Atmos_boundary%v_flux(i,j,:)
!!!    if (lprint0) print *, "Atmos_boundary%lprec=",Atmos_boundary%lprec(i,j,:)
!!!    if (lprint0) print *, "Atmos_boundary%fprec=",Atmos_boundary%fprec(i,j,:)
!!!    if (lprint0) print *, "Atmos_boundary%t_bot=",Atmos_boundary%tprec(i,j,:)
!!!    if (lprint0) print *, "Atmos_boundary%gust=",Atmos_boundary%gust(i,j,:)
!!!    if (lprint0) print *, "Ice%runoff=",Ice%runoff
!!!    if (lprint0) print *, "Ice%calving=",Ice%calving
    if (lprint0) print *, "Ice%tsw=",Ice%tsw
    if (lprint0) print *, "Ice%tsi=",Ice%tsi          
    if (lprint0) print *, "Ice%t_surf=",Ice%t_surf(i,j,:)
    if (lprint0) print *, "Ice%thickness=",Ice%thickness(i,j,:)
    if (lprint0) print *, "Ice%seaice=",Ice%seaice
    if (lprint0) print *, "Ice%ice_mask=",Ice%ice_mask(i,j,:)            
#endif  

  do j = js, je
#ifdef DEBUG
    lprint1=GDCHK1
#endif
    do i = is, ie
#ifdef DEBUG
    lprint2=GDCHK2
#endif

      if (Ice%sitmask(i,j) > 0.5) then


!! v0.46  
#ifdef ONEWAY
!!!    if (.false.) call sit_vdiff ( istep, real(dt),                            &
        call sit_vdiff (i,j,istep, real(dt),                            &
               Ice%lat(i,j)*180./pi, Ice%lon(i,j)*180./pi,                             &
               Ice%cor(i,j), Ice%slm(i,j), Ice%lclass(i,j),                            & 
               Ice%sitmask(i,j), Ice%bathy(i,j), Ice%wlvl(i,j),                        &
               Ice%ocnmask(i,j), Ice%obox_mask(i,j),                                   &
             ! - same as lake and ml_ocean
             ! combine flux over water and ice together, for handling open water- iced transition periods                                             
               Ice%lat(i,j)*0., SUM(Atmos_boundary%dfluxn(i,j,:)), Ice%lat(i,j)*0.,                      &
               -SUM(Atmos_boundary%fluxn(i,j,:)),           &
               SUM(Atmos_boundary%dfluxn(i,j,:)),          &
               SUM(Atmos_boundary%sw_flux(i,j,:)),          &
!!!               -(1.-Ice%seaice(i,j))*Atmos_boundary%fluxn(i,j,1), (1.-Ice%seaice(i,j))*Atmos_boundary%dfluxn(i,j,1), (1.-Ice%seaice(i,j))*Atmos_boundary%sw_flux(i,j,1),    &
!!!               -Ice%seaice(i,j)*Atmos_boundary%fluxn(i,j,2), Ice%seaice(i,j)*Atmos_boundary%dfluxn(i,j,2), Ice%seaice(i,j)*Atmos_boundary%sw_flux(i,j,2),                   &
             !
             ! - 1D from mo_memory_g3b (wind stress)                                   
               SUM(Atmos_boundary%u_flux(i,j,:)),SUM(Atmos_boundary%v_flux(i,j,:)),             &
             ! -    water mass variables(rain, snow, evap and runoff):                 
               SUM(Atmos_boundary%lprec(i,j,:)),SUM(Atmos_boundary%fprec(i,j,:)),               &
               -SUM(Atmos_boundary%q_flux(i,j,:)),                                          &
               Ice%runoff(i,j)+Ice%calving(i,j),                                       &
             ! - 1D from mo_memory_g3b                                                  
               SUM(Atmos_boundary%tprec(i,j,:)),                                            &
               SUM(Atmos_boundary%gust(i,j,:)),                                             &
             ! - 1D from mo_memory_g3b (sit variables)
               Ice%obsseaice(i,j), Ice%obswtb(i,j), Ice%obswsb(i,j),                   &
               Ice%wtb(i,j), Ice%wub(i,j), Ice%wvb(i,j),                               &
               Ice%wsb(i,j),                                                           &
               Ice%fluxiw(i,j),    Ice%pme2(i,j),                                      &
               Ice%subfluxw(i,j), Ice%wsubsal(i,j),                                    &
               Ice%cc(i,j), Ice%hc(i,j), Ice%engwac(i,j),                              &
               Ice%sc(i,j), Ice%saltwac(i,j),                                          &
               Ice%wtfns(i,j), Ice%wsfns(i,j),                                         &
             ! 3-d SIT vars: snow/ice
               Ice%zsi(i,j,:), Ice%silw(i,j,:), Ice%tsnic(i,j,:),                      &
             ! 3-d SIT vars: water column
               Ice%obswt(i,j,:), Ice%obsws(i,j,:), Ice%obswu(i,j,:), Ice%obswv(i,j,:), &
               Ice%wt(i,j,:), Ice%wu(i,j,:), Ice%wv(i,j,:), Ice%ww(i,j,:),             &
               Ice%ws(i,j,:), Ice%wtke(i,j,:), Ice%wlmx(i,j,:),                        &
               Ice%wldisp(i,j,:), Ice%wkm(i,j,:), Ice%wkh(i,j,:),                      &
               Ice%wrho1000(i,j,:),                                                    &
               Ice%wtfn(i,j,:), Ice%wsfn(i,j,:),                                       &
               Ice%awufl(i,j,:), Ice%awvfl(i,j,:), Ice%awtfl(i,j,:),                   &
               Ice%awsfl(i,j,:), Ice%awtkefl(i,j,:),                                   &
            ! final output only
               seaice(i,j),                                                        &
               sni(i,j), siced(i,j), tsi(i,j), tsw(i,j),               &
               tsl(i,j), tslm(i,j), tslm1(i,j),                            &
               ocu(i,j), ocv(i,j),  Ice%ctfreez2(i,j),                         &
            ! implicit with vdiff
               grndcapc(i,j), grndhflx(i,j), grndflux(i,j)    )


#else

        call sit_vdiff (i,j,istep, real(dt),                            &
               Ice%lat(i,j)*180./pi, Ice%lon(i,j)*180./pi,                             &
               Ice%cor(i,j), Ice%slm(i,j), Ice%lclass(i,j),                            & 
               Ice%sitmask(i,j), Ice%bathy(i,j), Ice%wlvl(i,j),                        &
               Ice%ocnmask(i,j), Ice%obox_mask(i,j),                                   &
             ! - same as lake and ml_ocean                                             
             ! combine flux over water and ice together, for handling open water- iced transition periods                                             
               Ice%lat(i,j)*0., Ice%lat(i,j)*0., Ice%lat(i,j)*0.,                      &
               -SUM(Atmos_boundary%fluxn(i,j,:)),           &
               SUM(Atmos_boundary%dfluxn(i,j,:)),          &
               SUM(Atmos_boundary%sw_flux(i,j,:)),          &
!!!             !           
!!!               -Ice%seaice(i,j)*Atmos_boundary%fluxn(i,j,2), Ice%seaice(i,j)*Atmos_boundary%sw_flux(i,j,2),                               &
!!!               -(1.-Ice%seaice(i,j))*Atmos_boundary%fluxn(i,j,1), (1.-Ice%seaice(i,j))*Atmos_boundary%dfluxn(i,j,1), (1.-Ice%seaice(i,j))*Atmos_boundary%sw_flux(i,j,1),    &
!!!               -Ice%seaice(i,j)*Atmos_boundary%fluxn(i,j,2), Ice%seaice(i,j)*Atmos_boundary%dfluxn(i,j,2), Ice%seaice(i,j)*Atmos_boundary%sw_flux(i,j,2),                   &
             ! - 1D from mo_memory_g3b (wind stress)                                   
               SUM(Atmos_boundary%u_flux(i,j,:)),SUM( Atmos_boundary%v_flux(i,j,:)),             &
             ! -    water mass variables(rain, snow, evap and runoff):                 
               SUM(Atmos_boundary%lprec(i,j,:)), SUM(Atmos_boundary%fprec(i,j,:)),               &
               -SUM(Atmos_boundary%q_flux(i,j,:)),                                          &
               Ice%runoff(i,j)+Ice%calving(i,j),                                       &
             ! - 1D from mo_memory_g3b                                                  
               SUM(Atmos_boundary%tprec(i,j,:)),                                            &
               SUM(Atmos_boundary%gust(i,j,:)),                                             &
             ! - 1D from mo_memory_g3b (sit variables)
               Ice%obsseaice(i,j), Ice%obswtb(i,j), Ice%obswsb(i,j),                   &
               Ice%wtb(i,j), Ice%wub(i,j), Ice%wvb(i,j),                               &
               Ice%wsb(i,j),                                                           &
               Ice%fluxiw(i,j),    Ice%pme2(i,j),                                      &
               Ice%subfluxw(i,j), Ice%wsubsal(i,j),                                    &
               Ice%cc(i,j), Ice%hc(i,j), Ice%engwac(i,j),                              &
               Ice%sc(i,j), Ice%saltwac(i,j),                                          &
               Ice%wtfns(i,j), Ice%wsfns(i,j),                                         &
             ! 3-d SIT vars: snow/ice
               Ice%zsi(i,j,:), Ice%silw(i,j,:), Ice%tsnic(i,j,:),                      &
             ! 3-d SIT vars: water column
               Ice%obswt(i,j,:), Ice%obsws(i,j,:), Ice%obswu(i,j,:), Ice%obswv(i,j,:), &
               Ice%wt(i,j,:), Ice%wu(i,j,:), Ice%wv(i,j,:), Ice%ww(i,j,:),             &
               Ice%ws(i,j,:), Ice%wtke(i,j,:), Ice%wlmx(i,j,:),                        &
               Ice%wldisp(i,j,:), Ice%wkm(i,j,:), Ice%wkh(i,j,:),                      &
               Ice%wrho1000(i,j,:),                                                    &
               Ice%wtfn(i,j,:), Ice%wsfn(i,j,:),                                       &
               Ice%awufl(i,j,:), Ice%awvfl(i,j,:), Ice%awtfl(i,j,:),                   &
               Ice%awsfl(i,j,:), Ice%awtkefl(i,j,:),                                   &
            ! final output only
               Ice%seaice(i,j),                                                        &
               Ice%sni(i,j), Ice%siced(i,j), Ice%tsi(i,j), Ice%tsw(i,j),               &
               Ice%tsl(i,j), Ice%tslm(i,j), Ice%tslm1(i,j),                            &
               Ice%ocu(i,j), Ice%ocv(i,j),  Ice%ctfreez2(i,j),                         &
            ! implicit with vdiff
               Ice%grndcapc(i,j), Ice%grndhflx(i,j), Ice%grndflux(i,j)    )

#endif
         Ice%part_size(i,j,1) = 1.0 - Ice%part_size(i,j,2)
         Ice%ice_mask(i,j,2)  = Ice%seaice(i,j) > 0.5
      endif
!!!#ifdef DEBUG
!!!      if (lprint2) print *, "I am in update_ice_model_fast 5.0."
!!!      if (lprint2) print *, "Ice%sitmask=",Ice%sitmask(i,j)
!!!      if (lprint2) print *, "Ice%tsw=",Ice%tsw(i,j)
!!!      if (lprint2) print *, "Ice%tsi=",Ice%tsi(i,j)          
!!!      if (lprint2) print *, "Ice%t_surf=",Ice%t_surf(i,j,:)
!!!      if (lprint2) print *, "Ice%thickness=",Ice%thickness(i,j,:)
!!!      if (lprint2) print *, "Ice%seaice=",Ice%seaice(i,j)
!!!      if (lprint2) print *, "Ice%ice_mask=",Ice%ice_mask(i,j,:)
!!!      if (lprint2) print *, "Ice%albedo=",Ice%albedo(i,j,:)              
!!!#endif
    end do
  end do
endif

#ifdef DEBUG
      if (lprint0) print *, "I am in update_ice_model_fast 5.0."
      if (lprint0) print *, "Ice%sitmask=",Ice%sitmask
      if (lprint0) print *, "Ice%tsw=",Ice%tsw
      if (lprint0) print *, "Ice%tsi=",Ice%tsi
      if (lprint0) print *, "Ice%t_surf=",Ice%t_surf
      if (lprint0) print *, "Ice%thickness=",Ice%thickness
      if (lprint0) print *, "Ice%seaice=",Ice%seaice
      if (lprint0) print *, "Ice%ice_mask=",Ice%ice_mask
      if (lprint0) print *, "Ice%albedo=",Ice%albedo              
#endif

call mpp_clock_end(sit_vdiff_Clock)
call mpp_clock_end(sitClock)

!-----------------------------------------------------------------------
!------ update ocean/ice surface parameters --------

!  ---- over ice -----

where (Ice%ice_mask)
  Ice%rough_mom   = roughness_ice
  Ice%rough_heat  = roughness_ice
  Ice%rough_moist = roughness_ice
endwhere

call ice_albedo (Ice%ice_mask, Ice%thickness, Ice%t_surf, Ice%albedo)

!!! EIther define some or all of the additional albedoes in ice_albedo, 
!!! or define them  upon return, based on the values returned.
!call ice_albedo (Ice%ice_mask, Ice%thickness, Ice%t_surf, Ice%albedo, &
!                 Ice%albedo_vis_dir, Ice%albedo_nir_dir,   &
!               Ice%albedo_vis_dif, Ice%albedo_nir_dif)

!! FOR now, simply set all to be the same:
     Ice%albedo_vis_dir = Ice%albedo
     Ice%albedo_nir_dir = Ice%albedo
     Ice%albedo_vis_dif = Ice%albedo
     Ice%albedo_nir_dif = Ice%albedo
#ifdef DEBUG
  if (GDCHK0) print *, "I am in update_ice_model_fast 5.1: after call ice_albedo."
  if (GDCHK0) print *, "Ice%albedo=",Ice%albedo(II,JJ,:)              
#endif     

!  ---- over ocean -----
!  store values into ice-free partition (n=1)

call compute_ocean_roughness ( Ice%mask,   &       
            Atmos_boundary%u_star(:,:,1),  &
            Ice%rough_mom(:,:,1), Ice%rough_heat(:,:,1), Ice%rough_moist(:,:,1) )

!!! EIther define some or all of the additional albedoes in 
!!   compute_ocean_albedo, or define them  upon return, based on the 
!!   values returned.

!call compute_ocean_albedo ( Ice%mask, Atmos_boundary%coszen(:,:,1), Ice%albedo(:,:,1) )
call compute_ocean_albedo_new ( Ice%mask, Atmos_boundary%coszen(:,:,1), &
                                Ice%albedo_vis_dir(:,:,1), Ice%albedo_vis_dif(:,:,1),      &
                                Ice%albedo_nir_dir(:,:,1), Ice%albedo_nir_dif(:,:,1), Ice%lat )

!-----------------------------------------------------------------------
!----- average fluxes to be used by the ocean model -----
!-----------------------------------------------------------------------

    call sum_bottom_quantities ( Ice, flux_u_new,  flux_v_new,  &
                                      flux_t_new,  flux_q_new,  flux_lh_new,  &
                                      flux_sw_new, flux_lw_new, &
                                      lprec_new,   fprec_new,   &
                                      flux_sw_vis_new, flux_sw_dir_new,&
                                      flux_sw_dif_new,   &
                                      flux_sw_vis_dir_new,&
                                      flux_sw_vis_dif_new )

!!!    if (GDCHK0) print *, "Atmos_boundary%sw_flux_vis_dir=",Atmos_boundary%sw_flux_vis_dir(II,JJ,:)
!!!    if (GDCHK0) print *, "Atmos_boundary%sw_flux_vis_dif=", Atmos_boundary%sw_flux_vis_dif(II,JJ,:)
!!!    if (GDCHK0) print *, "Atmos_boundary%sw_flux_nir_dir=",Atmos_boundary%sw_flux_nir_dir(II,JJ,:)
!!!    if (GDCHK0) print *, "Atmos_boundary%sw_flux_nir_dif=",Atmos_boundary%sw_flux_nir_dif(II,JJ,:)
!-----------------------------------------------------------------------
!--------- advance time -----------------

Ice%Time = Ice%Time + Ice%Time_step_fast
istep=istep+1

!--------- do diagnostics here ----------

  call mpp_clock_begin(sitClock)
  call mpp_clock_begin(sit_vdiff_write_Clock)
  call write_ice_model( Atmos_boundary, Ice )
  call mpp_clock_end(sit_vdiff_write_Clock)
!!!  call write_ice_model( Atmos_boundary, Land_boundary, Ice )
  call mpp_clock_end(sitClock)
  call mpp_clock_end(iceClock)
end subroutine update_ice_model_fast
!=============================================================================================

subroutine write_ice_model( Atmos_boundary, Ice )
!!!subroutine write_ice_model( Atmos_boundary, Land_boundary, Ice )
type(atmos_ice_boundary_type), intent(in   ) :: Atmos_boundary
!!!type(land_ice_boundary_type),  intent(in   ) :: Land_boundary
type(ice_data_type),           intent(inout) :: Ice

!!! v0.47
!!!#ifdef DEBUG
  if (id_fluxw > 0)    sent = send_data(id_fluxw, Atmos_boundary%fluxn(:,:,1), Ice%Time, mask=Ice%mask)              ! bjt
  if (id_fluxi > 0)    sent = send_data(id_fluxi, Atmos_boundary%fluxn(:,:,2), Ice%Time, mask=Ice%mask)              ! bjt
  if (id_dfluxw > 0)    sent = send_data(id_dfluxw, Atmos_boundary%dfluxn(:,:,1), Ice%Time, mask=Ice%mask)              ! bjt
  if (id_dfluxi > 0)    sent = send_data(id_dfluxi, Atmos_boundary%dfluxn(:,:,2), Ice%Time, mask=Ice%mask)              ! bjt
  if (id_soflw > 0)    sent = send_data(id_soflw, Atmos_boundary%sw_flux(:,:,1), Ice%Time, mask=Ice%mask)              ! bjt
  if (id_sofli > 0)    sent = send_data(id_sofli, Atmos_boundary%sw_flux(:,:,2), Ice%Time, mask=Ice%mask)              ! bjt
  if (id_uflux > 0)    sent = send_data(id_uflux, SUM(Atmos_boundary%u_flux(:,:,:),DIM=3), Ice%Time, mask=Ice%mask)              ! bjt
  if (id_vflux > 0)    sent = send_data(id_vflux, SUM(Atmos_boundary%v_flux(:,:,:),DIM=3), Ice%Time, mask=Ice%mask)              ! bjt
  if (id_lprec > 0)    sent = send_data(id_lprec, SUM(Atmos_boundary%lprec(:,:,:),DIM=3), Ice%Time, mask=Ice%mask)              ! bjt
  if (id_fprec > 0)    sent = send_data(id_fprec, SUM(Atmos_boundary%fprec(:,:,:),DIM=3), Ice%Time, mask=Ice%mask)              ! bjt
  if (id_qflux > 0)    sent = send_data(id_qflux, SUM(Atmos_boundary%q_flux(:,:,:),DIM=3), Ice%Time, mask=Ice%mask)              ! bjt
  if (id_tprec > 0)    sent = send_data(id_tprec, SUM(Atmos_boundary%tprec(:,:,:),DIM=3), Ice%Time, mask=Ice%mask)              ! bjt
  if (id_gust > 0)    sent = send_data(id_gust, SUM(Atmos_boundary%gust(:,:,:),DIM=3), Ice%Time, mask=Ice%mask)              ! bjt
!!!#endif

!!!!!-----------------------------------------------------------------------
!!!!
!!!!!----- compute average fluxes -----
!!!!
!!!!!!!   call avg_bottom_quantities ( Ice )
!!!!
!!!!!
!!!!! Flux diagnostics
!!!!!
!!!!  if (id_sh   >0) sent = send_data(id_sh,    Ice%flux_t,  Ice%Time, mask=Ice%mask)
!!!!  if (id_lh   >0) sent = send_data(id_lh,    Ice%flux_lh, Ice%Time, mask=Ice%mask)
!!!!  if (id_evap >0) sent = send_data(id_evap,  Ice%flux_q,  Ice%Time, mask=Ice%mask)
!!!!  if (id_sw   >0) sent = send_data(id_sw,    Ice%flux_sw, Ice%Time, mask=Ice%mask)
!!!!  if (id_sw_vis   >0) sent = send_data(id_sw_vis,    Ice%flux_sw_vis, Ice%Time, mask=Ice%mask)
!!!!  if (id_sw_dir   >0) sent = send_data(id_sw_dir,    Ice%flux_sw_dir, Ice%Time, mask=Ice%mask)
!!!!  if (id_sw_dif   >0) sent = send_data(id_sw_dif,    Ice%flux_sw_dif, Ice%Time, mask=Ice%mask)
!!!!  if (id_sw_vis_dir   >0) sent = send_data(id_sw_vis_dir,    Ice%flux_sw_vis_dir, Ice%Time, mask=Ice%mask)
!!!!  if (id_sw_vis_dif   >0) sent = send_data(id_sw_vis_dif,    Ice%flux_sw_vis_dif, Ice%Time, mask=Ice%mask)
!!!!  if (id_sw_vis_dir   >0) sent = send_data(id_sw_nir_dir,    Ice%flux_sw_nir_dir, Ice%Time, mask=Ice%mask)
!!!!  if (id_sw_vis_dif   >0) sent = send_data(id_sw_nir_dif,    Ice%flux_sw_nir_dif, Ice%Time, mask=Ice%mask)
!!!!  if (id_lw   >0) sent = send_data(id_lw,    Ice%flux_lw, Ice%Time, mask=Ice%mask)
!!!!  if (id_snofl>0) sent = send_data(id_snofl, Ice%fprec,   Ice%Time, mask=Ice%mask)
!!!!  if (id_rain >0) sent = send_data(id_rain,  Ice%lprec,   Ice%Time, mask=Ice%mask)
!!!!
!!!!  if (id_fax  >0) sent = send_data(id_fax,   Ice%flux_u,  Ice%Time, mask=Ice%mask)
!!!!  if (id_fay  >0) sent = send_data(id_fay,   Ice%flux_v,  Ice%Time, mask=Ice%mask)
!!!!
!!!!!----- quantities from land model ----
!!!!
!!!!!!!  Ice%runoff  = Land_boundary%runoff
!!!!!!!  Ice%calving = Land_boundary%calving
!!!!
!!!!  if (id_runoff >0) sent = send_data(id_runoff,  Ice%runoff,  Ice%Time, mask=Ice%mask)
!!!!  if (id_calving>0) sent = send_data(id_calving, Ice%calving, Ice%Time, mask=Ice%mask)
!!!!
!!!!-----------------------------------------------------------------------
!!!!----- modify fluxes to be used by the ocean model -----
!!!!-----------------------------------------------------------------------
!!!
!!!!----- where there is ice, ocean will not feel fluxes ? -----
!!!
!!!where (Ice%ice_mask(:,:,2))
!!!  Ice%flux_u = 0.0
!!!  Ice%flux_v = 0.0
!!!endwhere
!!!
!!!!-----------------------------------------------------------------------
!!!!---- get the specified ice field -----
!!!
!!!      call get_amip_ice (Ice%Time, Amip_ice, frac)
!!!      call get_amip_sst (Ice%Time, Amip_sst, Ice%obswtb, Ice%lon, Ice%lat )
!!!
!!!!  --- turn off sea-ice ??? ---
!!!   if (no_ice) frac = 0.0
!!!
!!!!  --- set constants for determining ice fraction
!!!   if (use_leads) then
!!!       frac_cutoff = 1.e-6 ! machine dependent value
!!!       frac_floor = 0.0
!!!   else
!!!      !--- discretize (0. or 1.) ----
!!!       frac_cutoff = 0.5
!!!       frac_floor = 1.0
!!!   endif
!!!
!!!! sit >>
!!!
!!!   Ice%obsseaice=min(max(frac_floor,frac),1.0)  
!!!
!!!   if (.NOT.do_sit) then
!!!!    --- determine which grid boxes have ice coverage ---
!!!     where ( Ice%mask(:,:) .and. frac > frac_cutoff )
!!!!       --- ice ---
!!!        Ice%part_size(:,:,2) = Ice%obsseaice
!!!        Ice%part_size(:,:,1) = 1.0 - Ice%part_size(:,:,2)
!!!        Ice%siced(:,:) = specified_ice_thickness
!!!        Ice%ice_mask(:,:,2) = .true.
!!!        Ice%t_surf(:,:,1)=TFREEZE
!!!     elsewhere
!!!!       --- no ice ---
!!!        Ice%part_size(:,:,1) = 1.0
!!!        Ice%part_size(:,:,2) = 0.0
!!!        Ice%siced(:,:) = 0.0
!!!        Ice%ice_mask(:,:,2) = .false.
!!!        Ice%t_surf(:,:,1)=MAX(Ice%obswtb, TFREEZE)
!!!     endwhere
!!!   endif
!!!             ! 2-d new common vars
!!!   !!! if (id_siced > 0) sent = send_data(id_siced, Ice%siced(:,:), Ice%Time, mask=Ice%mask)      ! bjt, seaice thickness in water equivalent
   if (id_sitmask > 0)    sent = send_data(id_sitmask, Ice%sitmask, Ice%Time, mask=Ice%mask)        ! bjt
   if (id_obsseaice > 0) sent = send_data(id_obsseaice, Ice%obsseaice, Ice%Time, mask=Ice%mask)     ! bjt
   if (id_obswtb > 0) sent = send_data(id_obswtb, Ice%obswtb, Ice%Time, mask=Ice%mask)              ! bjt
   if (id_obswsb > 0) sent = send_data(id_obswsb, Ice%obswsb, Ice%Time, mask=Ice%mask)              ! bjt
             ! 2-d SIT/ECHAM vars
   if (id_cor > 0)    sent = send_data(id_cor, Ice%cor, Ice%Time, mask=Ice%mask)                    ! bjt
   if (id_slm > 0)    sent = send_data(id_slm, Ice%slm, Ice%Time, mask=Ice%mask)                    ! bjt
   if (id_lclass > 0)    sent = send_data(id_lclass, Ice%lclass, Ice%Time, mask=Ice%mask)           ! bjt
   if (id_bathy > 0)    sent = send_data(id_bathy, Ice%bathy, Ice%Time, mask=Ice%mask)              ! bjt
   if (id_wlvl > 0)    sent = send_data(id_wlvl, Ice%wlvl, Ice%Time, mask=Ice%mask)                 ! bjt
   if (id_ocnmask > 0)    sent = send_data(id_ocnmask, Ice%ocnmask, Ice%Time, mask=Ice%mask)        ! bjt
   if (id_obox_mask > 0)    sent = send_data(id_obox_mask, Ice%obox_mask, Ice%Time, mask=Ice%mask)  ! bjt
   if (id_sni > 0)    sent = send_data(id_sni, Ice%sni, Ice%Time, mask=Ice%mask)                    ! bjt
   if (id_siced > 0)    sent = send_data(id_siced, Ice%siced, Ice%Time, mask=Ice%mask)                    ! bjt
   if (id_tsi > 0)    sent = send_data(id_tsi, Ice%tsi, Ice%Time, mask=Ice%mask)                    ! bjt
   if (id_tsw > 0)    sent = send_data(id_tsw, Ice%tsw, Ice%Time, mask=Ice%mask)                    ! bjt
   if (id_tsl > 0)    sent = send_data(id_tsl, Ice%tsl, Ice%Time, mask=Ice%mask)                    ! bjt
   if (id_tslm > 0)    sent = send_data(id_tslm, Ice%tslm, Ice%Time, mask=Ice%mask)                 ! bjt
   if (id_tslm1 > 0)    sent = send_data(id_tslm1, Ice%tslm1, Ice%Time, mask=Ice%mask)              ! bjt
   if (id_ocu > 0)    sent = send_data(id_ocu, Ice%ocu, Ice%Time, mask=Ice%mask)                    ! bjt
   if (id_ocv > 0)    sent = send_data(id_ocv, Ice%ocv, Ice%Time, mask=Ice%mask)                    ! bjt
   if (id_ctfreez2 > 0)    sent = send_data(id_ctfreez2, Ice%ctfreez2, Ice%Time, mask=Ice%mask)     ! bjt
             ! 2-d SIT vars
   if (id_wtb > 0)    sent = send_data(id_wtb, Ice%wtb, Ice%Time, mask=Ice%mask)                    ! bjt
   if (id_wub > 0)    sent = send_data(id_wub, Ice%wub, Ice%Time, mask=Ice%mask)                    ! bjt
   if (id_wvb > 0)    sent = send_data(id_wvb, Ice%wvb, Ice%Time, mask=Ice%mask)                    ! bjt
   if (id_wsb > 0)    sent = send_data(id_wsb, Ice%wsb, Ice%Time, mask=Ice%mask)                    ! bjt
   if (id_fluxiw > 0)    sent = send_data(id_fluxiw, Ice%fluxiw, Ice%Time, mask=Ice%mask)           ! bjt
   if (id_pme2 > 0)    sent = send_data(id_pme2, Ice%pme2, Ice%Time, mask=Ice%mask)                 ! bjt
   if (id_subfluxw > 0)    sent = send_data(id_subfluxw, Ice%subfluxw, Ice%Time, mask=Ice%mask)     ! bjt
   if (id_wsubsal > 0)    sent = send_data(id_wsubsal, Ice%wsubsal, Ice%Time, mask=Ice%mask)        ! bjt
   if (id_cc > 0)    sent = send_data(id_cc, Ice%cc, Ice%Time, mask=Ice%mask)                       ! bjt
   if (id_hc > 0)    sent = send_data(id_hc, Ice%hc, Ice%Time, mask=Ice%mask)                       ! bjt
   if (id_engwac > 0)    sent = send_data(id_engwac, Ice%engwac, Ice%Time, mask=Ice%mask)           ! bjt
   if (id_sc > 0)    sent = send_data(id_sc, Ice%sc, Ice%Time, mask=Ice%mask)                       ! bjt
   if (id_saltwac > 0)    sent = send_data(id_saltwac, Ice%saltwac, Ice%Time, mask=Ice%mask)        ! bjt
   if (id_wtfns > 0)    sent = send_data(id_wtfns, Ice%wtfns, Ice%Time, mask=Ice%mask)              ! bjt
   if (id_wsfns > 0)    sent = send_data(id_wsfns, Ice%wsfns, Ice%Time, mask=Ice%mask)              ! bjt
             ! 3-d SIT vars: snow/ice
   if (id_zsi > 0)    sent = send_data(id_zsi, Ice%zsi, Ice%Time)                    ! bjt
   if (id_silw > 0)    sent = send_data(id_silw, Ice%silw, Ice%Time)                 ! bjt
   if (id_tsnic > 0)    sent = send_data(id_tsnic, Ice%tsnic, Ice%Time)              ! bjt
             ! 3-d SIT vars: snow/ice (by level)
   if (id_zsi0 > 0)    sent = send_data(id_zsi0, Ice%zsi(:,:,0), Ice%Time)                    ! bjt
   if (id_zsi1 > 0)    sent = send_data(id_zsi1, Ice%zsi(:,:,1), Ice%Time)                    ! bjt
   if (id_silw0 > 0)    sent = send_data(id_silw0, Ice%silw(:,:,0), Ice%Time)                 ! bjt
   if (id_silw1 > 0)    sent = send_data(id_silw1, Ice%silw(:,:,1), Ice%Time)                 ! bjt
   if (id_tsnic0 > 0)    sent = send_data(id_tsnic0, Ice%tsnic(:,:,0), Ice%Time)              ! bjt
   if (id_tsnic1 > 0)    sent = send_data(id_tsnic1, Ice%tsnic(:,:,1), Ice%Time)              ! bjt
   if (id_tsnic2 > 0)    sent = send_data(id_tsnic2, Ice%tsnic(:,:,2), Ice%Time)              ! bjt
   if (id_tsnic3 > 0)    sent = send_data(id_tsnic3, Ice%tsnic(:,:,3), Ice%Time)              ! bjt
             ! 3-d SIT vars: water column
   if (id_obswt > 0)    sent = send_data(id_obswt, Ice%obswt, Ice%Time)              ! bjt
   if (id_obsws > 0)    sent = send_data(id_obsws, Ice%obsws, Ice%Time)              ! bjt
   if (id_obswu > 0)    sent = send_data(id_obswu, Ice%obswu, Ice%Time)              ! bjt
   if (id_obswv > 0)    sent = send_data(id_obswv, Ice%obswv, Ice%Time)              ! bjt
   if (id_wt > 0)    sent = send_data(id_wt, Ice%wt, Ice%Time)                       ! bjt
   if (id_wu > 0)    sent = send_data(id_wu, Ice%wu, Ice%Time)                       ! bjt
   if (id_wv > 0)    sent = send_data(id_wv, Ice%wv, Ice%Time)                       ! bjt
   if (id_ww > 0)    sent = send_data(id_ww, Ice%ww, Ice%Time)                       ! bjt
   if (id_ws > 0)    sent = send_data(id_ws, Ice%ws, Ice%Time)                       ! bjt
   if (id_wtke > 0)    sent = send_data(id_wtke, Ice%wtke, Ice%Time)                 ! bjt
   if (id_wlmx > 0)    sent = send_data(id_wlmx, Ice%wlmx, Ice%Time)                 ! bjt
   if (id_wldisp > 0)    sent = send_data(id_wldisp, Ice%wldisp, Ice%Time)           ! bjt
   if (id_wkm > 0)    sent = send_data(id_wkm, Ice%wkm, Ice%Time)                    ! bjt
   if (id_wkh > 0)    sent = send_data(id_wkh, Ice%wkh, Ice%Time)                    ! bjt
   if (id_wrho1000 > 0)    sent = send_data(id_wrho1000, Ice%wrho1000, Ice%Time)     ! bjt
   if (id_wtfn > 0)    sent = send_data(id_wtfn, Ice%wtfn, Ice%Time)                 ! bjt
   if (id_wsfn > 0)    sent = send_data(id_wsfn, Ice%wsfn, Ice%Time)                 ! bjt
   if (id_awufl > 0)    sent = send_data(id_awufl, Ice%awufl, Ice%Time)              ! bjt
   if (id_awvfl > 0)    sent = send_data(id_awvfl, Ice%awvfl, Ice%Time)              ! bjt
   if (id_awtfl > 0)    sent = send_data(id_awtfl, Ice%awtfl, Ice%Time)              ! bjt
   if (id_awsfl > 0)    sent = send_data(id_awsfl, Ice%awsfl, Ice%Time)              ! bjt
   if (id_awtkefl > 0)    sent = send_data(id_awtkefl, Ice%awtkefl, Ice%Time)        ! bjt
   if (id_awufl > 0)    sent = send_data(id_awufl, Ice%awufl, Ice%Time)              ! bjt
   if (id_awufl > 0)    sent = send_data(id_awufl, Ice%awufl, Ice%Time)              ! bjt
   if (id_awufl > 0)    sent = send_data(id_awufl, Ice%awufl, Ice%Time)              ! bjt
   if (id_awufl > 0)    sent = send_data(id_awufl, Ice%awufl, Ice%Time)              ! bjt

    ! 4- OUTPUT only, original ATM variabels
   if (id_seaice > 0)    sent = send_data(id_seaice, Ice%seaice, Ice%Time)              ! bjt
   if (id_grndcapc > 0)    sent = send_data(id_grndcapc, Ice%grndcapc, Ice%Time)              ! bjt
   if (id_grndhflx > 0)    sent = send_data(id_grndhflx, Ice%grndhflx, Ice%Time)              ! bjt
   if (id_grndflux > 0)    sent = send_data(id_grndflux, Ice%grndflux, Ice%Time)              ! bjt


   if (id_wt0m > 0)    sent = send_data(id_wt0m, Ice%wt(:,:,0), Ice%Time)                       ! bjt
   if (id_wt3m > 0)    sent = send_data(id_wt3m, Ice%wt(:,:,j3m), Ice%Time)                     ! bjt
   if (id_wt10m > 0)    sent = send_data(id_wt10m, Ice%wt(:,:,j10m), Ice%Time)                  ! bjt
   if (id_wt100m > 0)    sent = send_data(id_wt100m, Ice%wt(:,:,j100m), Ice%Time)               ! bjt
   
end subroutine write_ice_model
!=============================================================================================

subroutine sum_bottom_quantities ( Ice, flux_u,  flux_v,  &
                                        flux_t,  flux_q,  flux_lh, &
                                        flux_sw, flux_lw, &
                                        lprec,   fprec,   &
                                        flux_sw_vis, flux_sw_dir,&
                                        flux_sw_dif,   &
                                        flux_sw_vis_dir,&
                                        flux_sw_vis_dif )

type (ice_data_type), intent(inout)  :: Ice
real, intent(in), dimension(:,:,:)   :: flux_u,  flux_v,  &
                                        flux_t,  flux_q,  flux_lh, &
                                        flux_sw, flux_lw, &
                                        lprec,   fprec,   &
                                        flux_sw_vis, flux_sw_dir,&
                                        flux_sw_dif,   &
                                        flux_sw_vis_dir,&
                                        flux_sw_vis_dif

if (Ice%avg_kount == 0) call zero_bottom_quantities (Ice)

Ice%flux_u_bot  = Ice%flux_u_bot  + flux_u
Ice%flux_v_bot  = Ice%flux_v_bot  + flux_v
Ice%flux_t_bot  = Ice%flux_t_bot  + flux_t
Ice%flux_q_bot  = Ice%flux_q_bot  + flux_q
Ice%flux_lh_bot = Ice%flux_lh_bot + flux_lh
Ice%flux_sw_bot = Ice%flux_sw_bot + flux_sw
Ice%flux_sw_vis_bot = Ice%flux_sw_vis_bot + flux_sw_vis
Ice%flux_sw_dir_bot = Ice%flux_sw_dir_bot + flux_sw_dir
Ice%flux_sw_dif_bot = Ice%flux_sw_dif_bot + flux_sw_dif
Ice%flux_sw_vis_dir_bot = Ice%flux_sw_vis_dir_bot + flux_sw_vis_dir
Ice%flux_sw_vis_dif_bot = Ice%flux_sw_vis_dif_bot + flux_sw_vis_dif
Ice%flux_sw_nir_dir_bot = Ice%flux_sw_nir_dir_bot + flux_sw_vis_dir
Ice%flux_sw_nir_dif_bot = Ice%flux_sw_nir_dif_bot + flux_sw_vis_dif
Ice%flux_lw_bot = Ice%flux_lw_bot + flux_lw
Ice%lprec_bot   = Ice%lprec_bot   + lprec
Ice%fprec_bot   = Ice%fprec_bot   + fprec

Ice%avg_kount = Ice%avg_kount + 1

end subroutine sum_bottom_quantities
!=============================================================================================
subroutine zero_bottom_quantities ( Ice )
type (ice_data_type), intent(inout) :: Ice

Ice%avg_kount = 0
Ice%flux_u_bot  = 0.0
Ice%flux_v_bot  = 0.0
Ice%flux_t_bot  = 0.0
Ice%flux_q_bot  = 0.0
Ice%flux_lh_bot = 0.0
Ice%flux_sw_bot = 0.0
Ice%flux_sw_vis_bot = 0.0
Ice%flux_sw_dir_bot = 0.0
Ice%flux_sw_dif_bot = 0.0
Ice%flux_sw_vis_dir_bot = 0.0
Ice%flux_sw_vis_dif_bot = 0.0
Ice%flux_sw_nir_dir_bot = 0.0
Ice%flux_sw_nir_dif_bot = 0.0
Ice%flux_lw_bot = 0.0
Ice%lprec_bot   = 0.0
Ice%fprec_bot   = 0.0

end subroutine zero_bottom_quantities
!=============================================================================================
subroutine update_ice_model_slow_up( Ocean_boundary, Ice )
type(ocean_ice_boundary_type), intent(in) :: Ocean_boundary
type(ice_data_type),           intent(inout) :: Ice

#ifndef MIMIC_SIT
  if (do_sit) return
#endif 

call ice_bottom_to_ice_top ( Ice, &
                             Ocean_boundary%t,      &
                             Ocean_boundary%frazil, &
                             Ocean_boundary%u,      &
                             Ocean_boundary%v )

end subroutine update_ice_model_slow_up
!=============================================================================================
subroutine ice_bottom_to_ice_top ( Ice, t_surf_ice_bot, frazil_ice_bot, &
                                   u_surf_ice_bot, v_surf_ice_bot  )
  type (ice_data_type), intent(inout) :: Ice
  real, dimension(:,:), intent(in)    :: t_surf_ice_bot,  frazil_ice_bot, &
                                         u_surf_ice_bot,  v_surf_ice_bot
  !-----------------------------------------------------------------------
  !                 pass ocean state through ice
  !            store values into ice-free partition (n=1)
#ifndef MIMIC_SIT
  if (do_sit) return
#endif  
  where (Ice%part_size(:,:,1) > .00001)
    Ice%temp  (:,:,1,1) = t_surf_ice_bot
    Ice%frazil(:,:,1)   = frazil_ice_bot
    Ice%u_surf(:,:,1)   = u_surf_ice_bot
    Ice%v_surf(:,:,1)   = v_surf_ice_bot
  endwhere
end subroutine ice_bottom_to_ice_top
!=============================================================================================
subroutine update_ice_model_slow_dn( Atmos_boundary, Land_boundary, Ice )
type(atmos_ice_boundary_type), intent(in   ) :: Atmos_boundary
type(land_ice_boundary_type),  intent(in   ) :: Land_boundary
type(ice_data_type),           intent(inout) :: Ice

!!! real, dimension(size(Ice%mask,1),size(Ice%mask,2)) :: frac
real :: frac_cutoff, frac_floor

!-----------------------------------------------------------------------

!----- compute average fluxes -----

   call avg_bottom_quantities ( Ice )

!
! Flux diagnostics
!
  if (id_sh   >0) sent = send_data(id_sh,    Ice%flux_t,  Ice%Time, mask=Ice%mask)
  if (id_lh   >0) sent = send_data(id_lh,    Ice%flux_lh, Ice%Time, mask=Ice%mask)
  if (id_evap >0) sent = send_data(id_evap,  Ice%flux_q,  Ice%Time, mask=Ice%mask)
  if (id_sw   >0) sent = send_data(id_sw,    Ice%flux_sw, Ice%Time, mask=Ice%mask)
  if (id_sw_vis   >0) sent = send_data(id_sw_vis,    Ice%flux_sw_vis, Ice%Time, mask=Ice%mask)
  if (id_sw_dir   >0) sent = send_data(id_sw_dir,    Ice%flux_sw_dir, Ice%Time, mask=Ice%mask)
  if (id_sw_dif   >0) sent = send_data(id_sw_dif,    Ice%flux_sw_dif, Ice%Time, mask=Ice%mask)
  if (id_sw_vis_dir   >0) sent = send_data(id_sw_vis_dir,    Ice%flux_sw_vis_dir, Ice%Time, mask=Ice%mask)
  if (id_sw_vis_dif   >0) sent = send_data(id_sw_vis_dif,    Ice%flux_sw_vis_dif, Ice%Time, mask=Ice%mask)
  if (id_sw_vis_dir   >0) sent = send_data(id_sw_nir_dir,    Ice%flux_sw_nir_dir, Ice%Time, mask=Ice%mask)
  if (id_sw_vis_dif   >0) sent = send_data(id_sw_nir_dif,    Ice%flux_sw_nir_dif, Ice%Time, mask=Ice%mask)
  if (id_lw   >0) sent = send_data(id_lw,    Ice%flux_lw, Ice%Time, mask=Ice%mask)
  if (id_snofl>0) sent = send_data(id_snofl, Ice%fprec,   Ice%Time, mask=Ice%mask)
  if (id_rain >0) sent = send_data(id_rain,  Ice%lprec,   Ice%Time, mask=Ice%mask)

  if (id_fax  >0) sent = send_data(id_fax,   Ice%flux_u,  Ice%Time, mask=Ice%mask)
  if (id_fay  >0) sent = send_data(id_fay,   Ice%flux_v,  Ice%Time, mask=Ice%mask)

!----- quantities from land model ----

  Ice%runoff  = Land_boundary%runoff
  Ice%calving = Land_boundary%calving

  if (id_runoff >0) sent = send_data(id_runoff,  Ice%runoff,  Ice%Time, mask=Ice%mask)
  if (id_calving>0) sent = send_data(id_calving, Ice%calving, Ice%Time, mask=Ice%mask)

!-----------------------------------------------------------------------
!----- modify fluxes to be used by the ocean model -----
!-----------------------------------------------------------------------

!----- where there is ice, ocean will not feel fluxes ? -----

where (Ice%ice_mask(:,:,2))
  Ice%flux_u = 0.0
  Ice%flux_v = 0.0
endwhere

!-----------------------------------------------------------------------
!---- get the specified ice field -----

      call get_amip_ice (Ice%Time, Amip_ice, Ice%obsseaice)
      call get_amip_sst (Ice%Time, Amip_sst, Ice%obswtb, Ice%lon, Ice%lat )
      if (lwoa_gfdl) then
        call specified_woa_driver (is, js, Ice%Time, Ice%p_flux, woa)        
      endif

!  --- turn off sea-ice ??? ---
   if (no_ice) Ice%obsseaice = 0.0

!  --- set constants for determining ice fraction
   if (use_leads) then
       frac_cutoff = 1.e-6 ! machine dependent value
       frac_floor = 0.0
   else
      !--- discretize (0. or 1.) ----
       frac_cutoff = 0.5
       frac_floor = 1.0
   endif

! sit >>

!!!   Ice%obsseaice=min(max(frac_floor,frac),1.0)  
!  --- determine which grid boxes have ice coverage ---
   where (Ice%sitmask > 0.5)
    !!!  ts_new        = Ice%tsw
    !!  ts_new        = (Ice%seaice*Ice%tsi+(1-Ice%seaice)*Ice%tsw)
    !!  Ice%t_surf    = ts_new
     Ice%ice_mask(:,:,2)  = Ice%seaice > frac_cutoff
   elsewhere
     where ( Ice%mask(:,:) )
       where ( Ice%obsseaice > frac_cutoff )
!         --- ice ---
          Ice%part_size(:,:,2) = 1.
          Ice%siced(:,:) = specified_ice_thickness
          Ice%ice_mask(:,:,2) = .true.
          Ice%t_surf(:,:,1)=TFREEZE
       elsewhere
!         --- no ice ---
          Ice%part_size(:,:,2) = 0.0
          Ice%siced(:,:) = 0.0
          Ice%ice_mask(:,:,2) = .false.
          Ice%t_surf(:,:,1)=MAX(Ice%obswtb, TFREEZE)
       endwhere
       Ice%part_size(:,:,1) = 1.0 - Ice%part_size(:,:,2)
     endwhere
   endwhere   

end subroutine update_ice_model_slow_dn
!=============================================================================================
subroutine avg_bottom_quantities ( Ice )
type(ice_data_type), intent(inout) :: Ice
real :: divid

!----- compute average fluxes -----

if (Ice%avg_kount == 0) call error_mesg ('avg_bottom_quantities', &
                      'no ocean model fluxes have been averaged', FATAL)

divid = 1./float(Ice%avg_kount)

Ice%flux_u_bot  = Ice%flux_u_bot  * divid
Ice%flux_v_bot  = Ice%flux_v_bot  * divid
Ice%flux_t_bot  = Ice%flux_t_bot  * divid
Ice%flux_q_bot  = Ice%flux_q_bot  * divid
Ice%flux_lh_bot = Ice%flux_lh_bot * divid
Ice%flux_sw_bot = Ice%flux_sw_bot * divid
Ice%flux_sw_vis_bot = Ice%flux_sw_vis_bot * divid
Ice%flux_sw_dir_bot = Ice%flux_sw_dir_bot * divid
Ice%flux_sw_dif_bot = Ice%flux_sw_dif_bot * divid
Ice%flux_sw_vis_dir_bot = Ice%flux_sw_vis_dir_bot * divid
Ice%flux_sw_vis_dif_bot = Ice%flux_sw_vis_dif_bot * divid
Ice%flux_sw_nir_dir_bot = Ice%flux_sw_nir_dir_bot * divid
Ice%flux_sw_nir_dif_bot = Ice%flux_sw_nir_dif_bot * divid
Ice%flux_lw_bot = Ice%flux_lw_bot * divid
Ice%lprec_bot   = Ice%lprec_bot   * divid
Ice%fprec_bot   = Ice%fprec_bot   * divid

Ice%flux_t  = all_avg( Ice%flux_t_bot , Ice%part_size )
Ice%flux_q  = all_avg( Ice%flux_q_bot , Ice%part_size )
Ice%flux_lh = all_avg( Ice%flux_lh_bot, Ice%part_size )
Ice%flux_sw = all_avg( Ice%flux_sw_bot, Ice%part_size )
Ice%flux_sw_vis = all_avg( Ice%flux_sw_vis_bot, Ice%part_size )
Ice%flux_sw_dir = all_avg( Ice%flux_sw_dir_bot, Ice%part_size )
Ice%flux_sw_dif = all_avg( Ice%flux_sw_dif_bot, Ice%part_size )
Ice%flux_sw_vis_dir = all_avg( Ice%flux_sw_vis_dir_bot, Ice%part_size )
Ice%flux_sw_vis_dif = all_avg( Ice%flux_sw_vis_dif_bot, Ice%part_size )
Ice%flux_sw_nir_dir = all_avg( Ice%flux_sw_nir_dir_bot, Ice%part_size )
Ice%flux_sw_nir_dif = all_avg( Ice%flux_sw_nir_dif_bot, Ice%part_size )
Ice%flux_lw = all_avg( Ice%flux_lw_bot, Ice%part_size )
Ice%fprec   = all_avg( Ice%fprec_bot  , Ice%part_size )
Ice%lprec   = all_avg( Ice%lprec_bot  , Ice%part_size )

Ice%flux_u  = all_avg( Ice%flux_u_bot , Ice%part_size )
Ice%flux_v  = all_avg( Ice%flux_v_bot , Ice%part_size )

!--- set count to zero and fluxes will be zeroed before the next sum

Ice%avg_kount = 0

end subroutine avg_bottom_quantities
!=============================================================================================
function all_avg(x,part)
real, dimension(:,:,:) :: x, part
real, dimension(size(x,1), size(x,2)) :: all_avg
integer :: k

if(any(shape(x) /= shape(part))) then
  call error_mesg('all_avg','input arguments "x" and "part" are not dimensioned the same',FATAL)
endif

all_avg = 0.
do k=1,size(x,3)
  all_avg = all_avg + part(:,:,k)*x(:,:,k)
enddo

return
end function all_avg
!=============================================================================================
subroutine ice_model_end(Ice)
type(ice_data_type), intent(inout) :: Ice
character(len=64) :: fname='RESTART/ice_model.res.nc'
integer :: unit, k
character(len=64) :: lvltag

if(.not.module_is_initialized) return
if( do_netcdf_restart) then
   if(mpp_pe() == mpp_root_pe() ) then
      call error_mesg ('ice_model_mod', 'Writing NetCDF formatted restart file: RESTART/ice_model.res.nc', NOTE)
   endif
   call ice_model_restart()
else

   if(mpp_pe() == mpp_root_pe() ) then
      call error_mesg ('ice_model_mod', 'Writing native formatted restart file.', NOTE)
   endif
  unit = open_restart_file ('RESTART/ice_model.res', 'write')
  if ( mpp_pe() == mpp_root_pe() ) then
    write (unit) restart_format
    write (unit) size(Ice%glon_bnd(:))-1, size(Ice%glat_bnd(:))-1, num_part
  endif

  call set_domain (Ice%Domain)
  call write_data ( unit, Ice%part_size  )
  call write_data ( unit, Ice%temp       )
  call write_data ( unit, Ice%thickness  )
  call write_data ( unit, Ice%albedo     )
! code to output the new albedos
  call write_data ( unit, Ice%albedo_vis_dir )
  call write_data ( unit, Ice%albedo_nir_dir )
  call write_data ( unit, Ice%albedo_vis_dif )
  call write_data ( unit, Ice%albedo_nir_dif )

  call write_data ( unit, Ice%rough_mom  )
  call write_data ( unit, Ice%rough_heat )
  call write_data ( unit, Ice%rough_moist)
  call write_data ( unit, Ice%u_surf     )
  call write_data ( unit, Ice%v_surf     )
!!!  call write_data ( unit, Ice%fluxn     )                       ! bjt
!!!  call write_data ( unit, Atmos_boundary%dfluxn     )                      ! bjt
!!!  call write_data ( unit, Ice%sw_flux     )                       ! bjt
  call write_data ( unit, Ice%frazil     )
  call write_data ( unit, Ice%flux_u_bot )
  call write_data ( unit, Ice%flux_v_bot )


  ! bjt
  call write_data ( unit, Ice%sitmask)
  call write_data ( unit, Ice%obsseaice)
  call write_data ( unit, Ice%obswtb)
  call write_data ( unit, Ice%obswsb)

#ifdef MIMIC_SIT
  if (.true.) then
#else
  if (do_sit) then
#endif  

    !!! call write_data ( unit, Ice%cor)

    call write_data ( unit, Ice%slm)
    call write_data ( unit, Ice%lclass)
    call write_data ( unit, Ice%bathy)
    call write_data ( unit, Ice%wlvl)
    call write_data ( unit, Ice%ocnmask)
    call write_data ( unit, Ice%obox_mask)
    !!! call write_data ( unit, Ice%sni)                  ! =>Ice%thickness(:,:,1)
    !!! call write_data ( unit, Ice%siced)                ! =>Ice%thickness(:,:,1)
    !!! call write_data ( unit, Ice%tsi)
    !!! call write_data ( unit, Ice%tsw)
    call write_data ( unit, Ice%tsl)
    call write_data ( unit, Ice%tslm)
    call write_data ( unit, Ice%tslm1)
    !!! call write_data ( unit, Ice%ocu)
    !!! call write_data ( unit, Ice%ocv)    
    call write_data ( unit, Ice%ctfreez2)
    ! 2-d SIT vars
    call write_data ( unit, Ice%wtb)
    call write_data ( unit, Ice%wub)
    call write_data ( unit, Ice%wvb)
    call write_data ( unit, Ice%wsb)
    call write_data ( unit, Ice%fluxiw)
    call write_data ( unit, Ice%pme2)
    call write_data ( unit, Ice%subfluxw)
    call write_data ( unit, Ice%wsubsal)
    call write_data ( unit, Ice%cc)
    call write_data ( unit, Ice%hc)
    call write_data ( unit, Ice%engwac)
    call write_data ( unit, Ice%sc)
    call write_data ( unit, Ice%saltwac)
    call write_data ( unit, Ice%wtfns)
    call write_data ( unit, Ice%wsfns)  

    ! 3-d SIT vars: snow/ice
    call write_data ( unit, Ice%zsi)
    call write_data ( unit, Ice%silw)
    call write_data ( unit, Ice%tsnic)
    ! 3-d SIT vars: water column
    !!! if (.NOT.lwoa_gfdl) then
    !!!   call write_data ( unit, Ice%obswt)
    !!!   call write_data ( unit, Ice%obsws)
    !!!   call write_data ( unit, Ice%obswu)
    !!!   call write_data ( unit, Ice%obswv)
    !!! endif
    call write_data ( unit, Ice%wt)
    call write_data ( unit, Ice%ws)
    call write_data ( unit, Ice%wu)
    call write_data ( unit, Ice%wv)
    call write_data ( unit, Ice%ww)
    call write_data ( unit, Ice%wtke)
    call write_data ( unit, Ice%wlmx)
    call write_data ( unit, Ice%wldisp)
    call write_data ( unit, Ice%wkm)
    call write_data ( unit, Ice%wkh)
    !!! call write_data ( unit, Ice%wrho1000)
    !!! call write_data ( unit, Ice%wtfn)
    !!! call write_data ( unit, Ice%wsfn)
    !!! call write_data ( unit, Ice%awufl)
    !!! call write_data ( unit, Ice%awvfl)
    !!! call write_data ( unit, Ice%awtfl)
    !!! call write_data ( unit, Ice%awsfl)
    !!! call write_data ( unit, Ice%awtkefl)
    ! 4- OUTPUT only, original ATM variabels
    !!! call write_data ( unit, Ice%seaice)
    !!! call write_data ( unit, Ice%grndcapc)
    !!! call write_data ( unit, Ice%grndhflx)
    !!! call write_data ( unit, Ice%grndflux)
  endif
  call close_file ( unit )
endif

deallocate(cell_area)
call amip_interp_del(Amip_sst)
call amip_interp_del(Amip_ice)

!!! >>> bjt

deallocate (  Ice%glon_bnd, Ice%glat_bnd, &
              Ice%lon_bnd, Ice%lat_bnd,   &
              Ice%lon, Ice%lat,           &
              Ice%lonv, Ice%latv )


    deallocate ( Ice%ice_mask         , &
               Ice%temp, &
               Ice%part_size, &
               Ice%albedo, &
               Ice%albedo_vis_dir, &
               Ice%albedo_nir_dir, &
               Ice%albedo_vis_dif, &
               Ice%albedo_nir_dif, &
               Ice%rough_mom, &
               Ice%rough_heat, &
               Ice%rough_moist, &
               Ice%u_surf, &
               Ice%v_surf, &
!!!               Ice%fluxn, &                     ! bjt
!!!               Atmos_boundary%dfluxn, &                    ! bjt
!!!               Ice%sw_flux, &                     ! bjt
               Ice%thickness, &
               Ice%mask )
    
    deallocate ( Ice%flux_u_bot, &
               Ice%flux_v_bot  , &
               Ice%flux_t_bot  , &
               Ice%flux_q_bot  , &
               Ice%flux_lh_bot , &
               Ice%flux_sw_bot , &
               Ice%flux_sw_vis_bot    , &
               Ice%flux_sw_dir_bot    , &
               Ice%flux_sw_dif_bot    , &
               Ice%flux_sw_vis_dir_bot, &
               Ice%flux_sw_vis_dif_bot, &
               Ice%flux_sw_nir_dir_bot, &
               Ice%flux_sw_nir_dif_bot, &
               Ice%flux_lw_bot , &
               Ice%lprec_bot   , &
               Ice%fprec_bot   , &
               Ice%runoff_bot  , &
               Ice%frazil        )

    deallocate ( Ice%flux_u    , &
               Ice%flux_v    , &
               Ice%flux_t    , &
               Ice%flux_q    , &
               Ice%flux_lh   , &
               Ice%flux_sw   , &
               Ice%flux_sw_vis     , &
               Ice%flux_sw_dir     , &
               Ice%flux_sw_dif     , &
               Ice%flux_sw_vis_dir , &
               Ice%flux_sw_vis_dif , &
               Ice%flux_sw_nir_dir , &
               Ice%flux_sw_nir_dif , &
               Ice%flux_lw   , &
               Ice%lprec     , &
               Ice%fprec     , &
               Ice%p_surf    , &
               Ice%runoff    , &
               Ice%calving   , &
               Ice%runoff_hflx , &
               Ice%calving_hflx, &
               Ice%area        , &
               Ice%mi          , &
               Ice%flux_salt   )
!!! <<< bjt

deallocate ( Ice%sitmask, Ice%obsseaice, Ice%obswtb, Ice%obswsb )

#ifdef MIMIC_SIT
if (.true.) then
#else
if (do_sit) then
#endif  
  deallocate (  Ice%cor, Ice%lclass,                                           &
                Ice%bathy, Ice%wlvl,                               &
                Ice%ocnmask, Ice%obox_mask,                                    &
                !!! Ice%sni,                                                      &
                !!! Ice%siced,                                                    &
                !!! Ice%tsi,                                                      &
                !!! Ice%tsw,                                                      &
                Ice%tsl, Ice%tslm, Ice%tslm1,                                  &
                !!! Ice%ocu, Ice%ocv,                                     &
                Ice%slm,                                     &
                Ice%ctfreez2,                                                  &
           ! 2-d SIT vars
                Ice%wtb, Ice%wub, Ice%wvb,                                     &
                Ice%wsb,                                                       &
                Ice%fluxiw, Ice%pme2,                                          &
                Ice%subfluxw, Ice%wsubsal,                                     &
                Ice%cc, Ice%hc, Ice%engwac,                                    &
                Ice%sc, Ice%saltwac,                                           &
                Ice%wtfns, Ice%wsfns,                                          &
           ! 3-d SIT vars: snow/ice
                Ice%zsi, Ice%silw, Ice%tsnic,                                  &
           ! 3-d SIT vars: water column
                !!! Ice%obswt, Ice%obsws, Ice%obswu, Ice%obswv,                    &
                Ice%wt, Ice%wu, Ice%wv, Ice%ww,                                &
                Ice%ws, Ice%wtke, Ice%wlmx,                                    &
                Ice%wldisp, Ice%wkm, Ice%wkh,                                  &
                Ice%wrho1000,                                                  &
                Ice%wtfn, Ice%wsfn,                                            &
                Ice%awufl, Ice%awvfl, Ice%awtfl,                               &
                Ice%awsfl, Ice%awtkefl,                                        &

           ! 4- OUTPUT only, original ATM variabels
                !!! Ice%seaice,                                                    &
                !!! Ice%seaice,                                                    &
                Ice%grndcapc, Ice%grndhflx, Ice%grndflux    )

  if (.NOT.lobswt) deallocate ( Ice%obswt )
  if (.NOT.lobsws) deallocate ( Ice%obsws )
  if (.NOT.lobswu) deallocate ( Ice%obswu )
  if (.NOT.lobswv) deallocate ( Ice%obswv )
  if (lwoa_gfdl) then
    deallocate (Ice%p_flux)
    call woa_dealloc (woa)
    call woa_end
  endif
!!! << sit
endif

module_is_initialized = .false.

end subroutine ice_model_end

 !#######################################################################
  ! <SUBROUTINE NAME="ice_model_restart">
  ! <DESCRIPTION>
  !  dummy routine
  ! </DESCRIPTION>
  subroutine ice_model_restart(Ice, time_stamp)
    type (ice_data_type),     intent(inout), optional :: Ice
    character(len=*),         intent(in), optional :: time_stamp

  if( .not. do_netcdf_restart ) then
     call error_mesg ('ice_model_mod', 'ice_model_restart should not be called when do_netcdf_restart = false', FATAL)
  endif

   call save_restart(Ice_restart, time_stamp)

  end subroutine ice_model_restart
  ! </SUBROUTINE>

!=============================================================================================

subroutine ice_diag_init (Ice, xb, yb)
type(ice_data_type), intent(in) :: Ice
real   , intent(in) :: xb(:), yb(:)

integer :: nlon, nlat
integer :: id_xv, id_yv, id_xb, id_xt, id_yb, id_yt, axv(2), axt(2)
integer :: id_sit_zdepth, id_sit_fluxdepth, id_snowice2, id_snowice4, &
  axt_water(3), axt_waterf(3), axt_snowice2(3), axt_snowice4(3)

real, parameter :: missing = -1e34

  nlon = size(xb(:))-1
  nlat = size(yb(:))-1

! define axes

 !id_xv = diag_axis_init('xv', xb(2:nlon+1), 'degrees_E', 'X','longitude', &
 !                                 set_name='ice', Domain2=Ice%Domain )
 !id_yv = diag_axis_init('yv', yb(2:nlat+1), 'degrees_N', 'Y','latitude',  &
 !                                 set_name='ice', Domain2=Ice%Domain )
 !axv = (/ id_xv, id_yv /)

  id_xb = diag_axis_init(trim(axisname_xb), xb, 'degrees_E', 'X', 'longitude', &
                                    set_name='ice', Domain2=Ice%Domain )
  id_xt = diag_axis_init(trim(axisname_x), (xb(1:nlon)+xb(2:nlon+1))/2, 'degrees_E', 'X', &
                     'longitude',set_name='ice',edges=id_xb,Domain2=Ice%Domain)
  id_yb = diag_axis_init(trim(axisname_yb), yb, 'degrees_N', 'Y', 'latitude', &
                                   set_name='ice', Domain2=Ice%Domain )
  id_yt = diag_axis_init(trim(axisname_y), (yb(1:nlat)+yb(2:nlat+1))/2, 'degrees_N', 'Y', &
                     'latitude',set_name='ice', edges=id_yb,Domain2=Ice%Domain)
  axt  = (/ id_xt, id_yt /)

  !!! ALLOCATE (sit_zdepth(0:lkvl+1))
  !!! ALLOCATE (sit_fluxdepth(0:lkvl+1))

#ifdef MIMIC_SIT
  if (.true.) then
#else
  if (do_sit) then
#endif  
    id_snowice2 = diag_axis_init('snow_ice2', (/1.,2./), 'level', 'z', &
              'snow depth + ice depth', direction=+1, set_name="ice")
    id_snowice4 = diag_axis_init('snow_ice4', (/1.,2.,3.,4./), 'level', 'z', &
              'snow sfc + snow mean + ice sfc + ice mean', direction=+1, set_name="ice")
    if ( GDCHK0 ) then
      print *, "sit_fluxdepth=",sit_fluxdepth
      print *, "sit_zdepth=",sit_zdepth
    endif
    
    id_sit_fluxdepth = diag_axis_init('sit_fluxdepth', sit_fluxdepth, 'm', 'z', &
              'SIT flux (surface + water) depth at boundary (+, upward)', direction=+1, set_name="ice")
    !!! id_sit_zdepth = diag_axis_init('sit_zdepth', sit_zdepth, 'm', 'z', &
    !!!           'SIT T,S,U depths at center (+, upward)', direction=+1, set_name="ice", edges=id_sit_fluxdepth)
    id_sit_zdepth = diag_axis_init('sit_zdepth', sit_zdepth, 'm', 'z', &
              'SIT T,S,U depths at center (+, upward)', direction=+1, set_name="ice")
    
    axt_water  = (/ id_xt, id_yt, id_sit_zdepth /)
    axt_waterf  = (/ id_xt, id_yt, id_sit_fluxdepth /)
    axt_snowice2  = (/ id_xt, id_yt, id_snowice2 /)
    axt_snowice4  = (/ id_xt, id_yt, id_snowice4 /)
  endif
  
! register fields

  id_sh = register_diag_field('ice_model','SH' ,axt, Ice%Time, &
                              'sensible heat flux', 'W/m^2', &
                              missing_value=missing)
  id_lh = register_diag_field('ice_model','LH' ,axt, Ice%Time, &
                             'latent heat flux', 'W/m^2', missing_value=missing)
  id_sw = register_diag_field('ice_model','SW' ,axt, Ice%Time, &
                              'short wave heat flux', 'W/m^2',  &
                              missing_value=missing)
  id_sw_vis = register_diag_field('ice_model','SWvis' ,axt, Ice%Time, &
                              'visible short wave heat flux', 'W/m^2', &
                              missing_value=missing)
  id_sw_dir = register_diag_field('ice_model','SWdir' ,axt, Ice%Time, &
                              'direct short wave heat flux', 'W/m^2',  &
                              missing_value=missing)
  id_sw_dif = register_diag_field('ice_model','SWdif' ,axt, Ice%Time, &
                              'diffuse short wave heat flux', 'W/m^2', &
                              missing_value=missing)
  id_sw_vis_dir = register_diag_field('ice_model','SWvisdir' ,axt, Ice%Time, &
                              'vis dir short wave heat flux', 'W/m^2', &
                              missing_value=missing)
  id_sw_vis_dif = register_diag_field('ice_model','SWvisdif' ,axt, Ice%Time, &
                              'vis diff short wave heat flux', 'W/m^2',  &
                              missing_value=missing)
  id_sw_nir_dir = register_diag_field('ice_model','SWnirdir' ,axt, Ice%Time, &
                              'NIR dir short wave heat flux', 'W/m^2', &
                              missing_value=missing)
  id_sw_nir_dif = register_diag_field('ice_model','SWnirdif' ,axt, Ice%Time, &
                              'NIR diff short wave heat flux', 'W/m^2',  &
                              missing_value=missing)
  id_lw = register_diag_field('ice_model','LW' ,axt, Ice%Time, &
                              'long wave heat flux over ice', 'W/m^2', &
                              missing_value=missing)
  id_snofl = register_diag_field('ice_model','SNOWFL' ,axt, Ice%Time, &
                                 'rate of snow fall', 'kg/(m^2*s)', &
                                 missing_value=missing)
  id_rain  = register_diag_field('ice_model','RAIN' ,axt, Ice%Time, &
                                 'rate of rain fall', 'kg/(m^2*s)', &
                                 missing_value=missing)
  id_runoff= register_diag_field('ice_model','RUNOFF' ,axt, Ice%Time, &
                                 'liquid runoff', 'kg/(m^2*s)', &
                                 missing_value=missing)
  id_calving = register_diag_field('ice_model','CALVING',axt, Ice%Time, &
                                 'frozen runoff', 'kg/(m^2*s)', &
                                 missing_value=missing)
  id_evap = register_diag_field('ice_model','EVAP',axt, Ice%Time, &
                                 'evaporation', 'kg/(m^2*s)', &
                                 missing_value=missing)
! wind stress at t points
  id_fax = register_diag_field('ice_model', 'FA_X', axt, Ice%Time, &
                               'air stress on ice - x component', 'Pa', &
                               missing_value=missing)
  id_fay = register_diag_field('ice_model', 'FA_Y', axt, Ice%Time, &
                               'air stress on ice - y component', 'Pa', &
                               missing_value=missing)
  ! sit
  id_sitmask      = &
  register_diag_field ( 'ice_model', 'sitmask',      axt, Ice%Time, &
                       'mask for sit(1=.TRUE., 0=.FALSE.)',  '0/1', &
                       missing_value=xmissing )

  !!! id_siced      = &
  !!! register_diag_field ( 'ice_model', 'siced',      axt, Ice%Time, &
  !!!                      'ice thickness (swe)',  'm', &
  !!!                      missing_value=xmissing )
  id_obsseaice  = &
  register_diag_field ( 'ice_model', 'obsseaice',      axt, Ice%Time, &
                       'observed sea ice fraction',  '0-1', &
                       missing_value=xmissing )
  id_obswtb  = &
  register_diag_field ( 'ice_model', 'obswtb',      axt, Ice%Time, &
                       'observed sea surface temperature',  'K', &
                       missing_value=xmissing )
  id_obswsb  = &
  register_diag_field ( 'ice_model', 'obswsb',      axt, Ice%Time, &
                       'observed sea surface salinity',  'PSU', &
                       missing_value=xmissing )
    id_sni      = &
    register_diag_field ( 'ice_model', 'sni',      axt, Ice%Time, &
                         'snow thickness over ice', 'm in water equivalent', &
                         missing_value=xmissing )

    id_siced      = &
    register_diag_field ( 'ice_model', 'siced',      axt, Ice%Time, &
                         'ice thickness', 'm in water equivalent', &
                         missing_value=xmissing )
                         
    id_tsi      = &
    register_diag_field ( 'ice_model', 'tsi',      axt, Ice%Time, &
                         'surface temperature of ice',  'K', &
                         missing_value=xmissing )
    id_tsw      = &
    register_diag_field ( 'ice_model', 'tsw',      axt, Ice%Time, &
                         'skin temperatrue over water ',  'K', &
                         missing_value=xmissing )
    id_seaice      = &
    register_diag_field ( 'ice_model', 'seaice',      axt, Ice%Time, &
                         'ice cover: fraction of 1-SLM',  &
                         ' (0-1)', &
                         missing_value=xmissing )
    id_ocu      = &
    register_diag_field ( 'ice_model', 'ocu',      axt, Ice%Time, &
                         'ocean eastw. velocity',  'm/s', &
                         missing_value=xmissing )
    id_ocv      = &
    register_diag_field ( 'ice_model', 'ocv',      axt, Ice%Time, &
                         'ocean northw. velocity',  'm/s', &
                         missing_value=xmissing )


!!! v0.47
!!!#ifdef DEBUG
    id_fluxi      = &
    register_diag_field ( 'ice_model', 'fluxi',      axt, Ice%Time, &
                         'Rln+Rsn-H-LE over water',  'W/m2', &
                         missing_value=xmissing )
    id_fluxw      = &
    register_diag_field ( 'ice_model', 'fluxw',      axt, Ice%Time, &
                         'Rln+Rsn-H-LE over ice',  'W/m2', &
                         missing_value=xmissing )
    id_dfluxi      = &
    register_diag_field ( 'ice_model', 'dfluxi',      axt, Ice%Time, &
                         'd(fluxi)/dT',  'W/m2/K', &
                         missing_value=xmissing )
    id_dfluxw      = &
    register_diag_field ( 'ice_model', 'dfluxw',      axt, Ice%Time, &
                         'd(fluxw)/dT',  'W/m2/K', &
                         missing_value=xmissing )
    id_soflw      = &
    register_diag_field ( 'ice_model', 'soflw',      axt, Ice%Time, &
                         'Rsn over water',  'W/m2', &
                         missing_value=xmissing )
    id_sofli      = &
    register_diag_field ( 'ice_model', 'sofli',      axt, Ice%Time, &
                         'Rsn over ice',  'W/m2', &
                         missing_value=xmissing )
    id_uflux      = &
    register_diag_field ( 'ice_model', 'uflux',      axt, Ice%Time, &
                         'u-wind stress over water',  'Pa', &
                         missing_value=xmissing )
    id_vflux      = &
    register_diag_field ( 'ice_model', 'vflux',      axt, Ice%Time, &
                         'v-wind stress over water',  'Pa', &
                         missing_value=xmissing )
    id_lprec      = &
    register_diag_field ( 'ice_model', 'lprec',      axt, Ice%Time, &
                         'rainfall rate',  'mm/s', &
                         missing_value=xmissing )
    id_fprec      = &
    register_diag_field ( 'ice_model', 'fprec',      axt, Ice%Time, &
                         'snowfall rate',  'mm/s', &
                         missing_value=xmissing )
    id_qflux      = &
    register_diag_field ( 'ice_model', 'qflux',      axt, Ice%Time, &
                         'evaportion',  'mm/s', &
                         missing_value=xmissing )
    id_tprec      = &
    register_diag_field ( 'ice_model', 'tprec',      axt, Ice%Time, &
                         'precipitation temperature',  'K', &
                         missing_value=xmissing )
    id_gust      = &
    register_diag_field ( 'ice_model', 'gust',      axt, Ice%Time, &
                         'gust wind speed',  'm/s', &
                         missing_value=xmissing )

!!!#endif
                         

#ifdef MIMIC_SIT
  if (.true.) then
#else
  if (do_sit) then
#endif 
    id_cor      = &
    register_diag_field ( 'ice_model', 'cor',      axt, Ice%Time, &
                         'corlios factor 2*omega*sin(lat)',  '1/s', &
                         missing_value=xmissing )
    id_slm      = &
    register_diag_field ( 'ice_model', 'slm',      axt, Ice%Time, &
                         'pslm: land fraction',  '0-1', &
                         missing_value=xmissing )
    id_lclass      = &
    register_diag_field ( 'ice_model', 'lclass',      axt, Ice%Time, &
                         'land cover class, 1: land, 2: ocean, 3: lake, 4: glacier',  '1-4', &
                         missing_value=xmissing )
    
    id_bathy      = &
    register_diag_field ( 'ice_model', 'bathy',      axt, Ice%Time, &
                         'bathymeter (topography or orography) of ocean',  'm', &
                         missing_value=xmissing )
                         
    id_wlvl      = &
    register_diag_field ( 'ice_model', 'wlvl',      axt, Ice%Time, &
                         'current water level (ice/water interface) a water body grid',  'm', &
                         missing_value=xmissing )
                         
    id_ocnmask      = &
    register_diag_field ( 'ice_model', 'ocnmask',      axt, Ice%Time, &
                         'mask for 3-D ocean grid',  '1: ocn', &
                         missing_value=xmissing )
                         
    id_obox_mask      = &
    register_diag_field ( 'ice_model', 'obox_mask',      axt, Ice%Time, &
                         '3-D ocean nudging mask, =0: nudging, = 1 (>0): nudging ',  '0/1', &
                         missing_value=xmissing )
                         
!    id_sni      = &
!    register_diag_field ( 'ice_model', 'sni',      axt, Ice%Time, &
!                         'snow thickness over ice', 'm in water equivalent', &
!                         missing_value=xmissing )
!
!    id_siced      = &
!    register_diag_field ( 'ice_model', 'siced',      axt, Ice%Time, &
!                         'ice thickness', 'm in water equivalent', &
!                         missing_value=xmissing )
!                         
!    id_tsi      = &
!    register_diag_field ( 'ice_model', 'tsi',      axt, Ice%Time, &
!                         'surface temperature of ice',  'K', &
!                         missing_value=xmissing )
!    id_tsw      = &
!    register_diag_field ( 'ice_model', 'tsw',      axt, Ice%Time, &
!                         'skin temperatrue over water ',  'K', &
!                         missing_value=xmissing )
!                         
    id_tsl      = &
    register_diag_field ( 'ice_model', 'tsl',      axt, Ice%Time, &
                         'calcuated Earth skin temperature at t+dt',  'K', &
                         missing_value=xmissing )
    id_tslm      = &
    register_diag_field ( 'ice_model', 'tslm',      axt, Ice%Time, &
                         'calcuated Earth skin temperature at t',  'K', &
                         missing_value=xmissing )
    id_tslm1      = &
    register_diag_field ( 'ice_model', 'tslm1',      axt, Ice%Time, &
                         'zcalcuated Earth skin temperature at t-dt',  'K', &
                         missing_value=xmissing )
!    id_ocu      = &
!    register_diag_field ( 'ice_model', 'ocu',      axt, Ice%Time, &
!                         'ocean eastw. velocity',  'm/s', &
!                         missing_value=xmissing )
!    id_ocv      = &
!    register_diag_field ( 'ice_model', 'ocv',      axt, Ice%Time, &
!                         'ocean northw. velocity',  'm/s', &
!                         missing_value=xmissing )
    id_ctfreez2      = &
    register_diag_field ( 'ice_model', 'ctfreez2',      axt, Ice%Time, &
                         'ref water freezing temperature',  'K', &
                         missing_value=xmissing )
    ! 2-d SIT vars                                          I
    id_wtb      = &
    register_diag_field ( 'ice_model', 'wtb',      axt, Ice%Time, &
                         '10-m mean water temperature',  'K', &
                         missing_value=xmissing )
    id_wub      = &
    register_diag_field ( 'ice_model', 'wub',      axt, Ice%Time, &
                         '10-m mean water u current',  'm/s', &
                         missing_value=xmissing )
    id_wvb      = &
    register_diag_field ( 'ice_model', 'wvb',      axt, Ice%Time, &
                         '10-m mean water v current',  'm/s', &
                         missing_value=xmissing )
    id_wsb      = &
    register_diag_field ( 'ice_model', 'wsb',      axt, Ice%Time, &
                         '10-m mean water salinity',  'PSU', &
                         missing_value=xmissing )
    id_fluxiw      = &
    register_diag_field ( 'ice_model', 'fluxiw',      axt, Ice%Time, &
                         'over-water net surface heat flux',  'W/m2, + upward, i.e., from ocean', &
                         missing_value=xmissing )
    
    id_pme2      = &
    register_diag_field ( 'ice_model', 'pme2',      axt, Ice%Time, &
                         'net fresh water into ocean (P-E+ice_corr)',  'm/s, + downward', &
                         missing_value=xmissing )
    id_subfluxw      = &
    register_diag_field ( 'ice_model', 'subfluxw',      axt, Ice%Time, &
                         'subsurface ocean heat flux',  'W/m2, + upward', &
                         missing_value=xmissing )
    id_wsubsal      = &
    register_diag_field ( 'ice_model', 'wsubsal',      axt, Ice%Time, &
                         'subsurface ocean salinity flux',  'm*PSU/s, + upward', &
                         missing_value=xmissing )
    id_cc      = &
    register_diag_field ( 'ice_model', 'cc',      axt, Ice%Time, &
                         'cold content per water fraction (ice sheet+openwater), energy need to melt snow and ice, i.e., energy below liquid water at tmelt',  &
                         'J/m2', &
                         missing_value=xmissing )
    id_hc      = &
    register_diag_field ( 'ice_model', 'hc',      axt, Ice%Time, &
                         'heat content per water fraction (ice sheet+openwater), energy of a water column above tmelt', &
                         'J/m2', &
                         missing_value=xmissing )
    id_engwac      = &
    register_diag_field ( 'ice_model', 'engwac',      axt, Ice%Time, &
                         'accumulated energy per water fraction (ice sheet+openwater), (pfluxw+pfluxi+rain/snow advected energy in respect to liquid water at tmelt)*dt',  &
                         'J/m2, + downward, i.e., from ocean', &
                         missing_value=xmissing )
    
    id_sc      = &
    register_diag_field ( 'ice_model', 'sc',      axt, Ice%Time, &
                         'salinity content per water fraction (ice sheet+openwater)', &
                         'PSU*m', &
                         missing_value=xmissing )
    id_saltwac      = &
    register_diag_field ( 'ice_model', 'saltwac',      axt, Ice%Time, &
                         'accumulated salt into water fraction',  &
                         'PSU*m, + downward', &
                         missing_value=xmissing )
    
    id_wtfns      = &
    register_diag_field ( 'ice_model', 'wtfns',      axt, Ice%Time, &
                         'nudging flux into sit-ocean for an entire column, i.e., pwtfns=SUM(pwtfn(icol,:))',  &
                         'W/m**2', &
                         missing_value=xmissing )
    id_wsfns      = &
    register_diag_field ( 'ice_model', 'wsfns',      axt, Ice%Time, &
                         'nudging salinity flux into sit-ocean for an entire column, i.e., pwsfns=SUM(pwsfn(icol,:))',  &
                         'PSU*m/s', &
                         missing_value=xmissing )
                                                                  !    i.e., pwsfns=SUM(pwsfn(icol,:)                                                           I/O
  !  3-d SIT vars: snow/ice
    
    id_zsi      = &
    register_diag_field ( 'ice_model', 'zsi',      axt_snowice2, Ice%Time, &
                         'dry snow (level=0)/ice (level=0) water equivalent,',  &
                         'm', &
                         missing_value=xmissing )
    id_silw      = &
    register_diag_field ( 'ice_model', 'silw',      axt_snowice2, Ice%Time, &
                         'snow (level=0)/ice (level=0) liquid water storage',  &
                         'm', &
                         missing_value=xmissing )
    id_tsnic      = &
    register_diag_field ( 'ice_model', 'tsnic',      axt_snowice4, Ice%Time, &
                         'snow skin (level=0), mean snow (level=1), ice skin (level=2), mean ice (level=3) temperatrue',  &
                         'K', &
                         missing_value=xmissing )

  !  3-d SIT vars: snow/ice
    
    id_zsi0      = &
    register_diag_field ( 'ice_model', 'zsi0',      axt, Ice%Time, &
                         'dry snow water equivalent (level=0),',  &
                         'm', &
                         missing_value=xmissing )

    id_zsi1      = &
    register_diag_field ( 'ice_model', 'zsi1',      axt, Ice%Time, &
                         'dry ice water equivalent (level=1),',  &
                         'm', &
                         missing_value=xmissing )

    id_silw0     = &
    register_diag_field ( 'ice_model', 'silw0',      axt, Ice%Time, &
                         'snow (level=0) liquid water storage',  &
                         'm', &
                         missing_value=xmissing )

    id_silw1     = &
    register_diag_field ( 'ice_model', 'silw1',      axt, Ice%Time, &
                         'ice (level=1) liquid water storage',  &
                         'm', &
                         missing_value=xmissing )

    id_tsnic0      = &
    register_diag_field ( 'ice_model', 'tsnic0',      axt, Ice%Time, &
                         'snow skin (level=0) temperatrue',  &
                         'K', &
                         missing_value=xmissing )

    id_tsnic1      = &
    register_diag_field ( 'ice_model', 'tsnic1',      axt, Ice%Time, &
                         'mean snow (level=1) temperatrue',  &
                         'K', &
                         missing_value=xmissing )
    id_tsnic2      = &
    register_diag_field ( 'ice_model', 'tsnic2',      axt, Ice%Time, &
                         'ice skin (level=2) temperatrue',  &
                         'K', &
                         missing_value=xmissing )
    id_tsnic3      = &
    register_diag_field ( 'ice_model', 'tsnic3',      axt, Ice%Time, &
                         'mean ice (level=3) temperatrue',  &
                         'K', &
                         missing_value=xmissing )

    
  !  3-d SIT vars: water column
    
    id_obswt      = &
    register_diag_field ( 'ice_model', 'obswt',      axt_water, Ice%Time, &
                         'observed potentail water temperature',  &
                         'K', &
                         missing_value=xmissing )
    id_obsws      = &
    register_diag_field ( 'ice_model', 'obsws',      axt_water, Ice%Time, &
                         'observed salinity',  &
                         'PSU, 0/00', &
                         missing_value=xmissing )
    id_obswu      = &
    register_diag_field ( 'ice_model', 'obswu',      axt_water, Ice%Time, &
                         'observed u-current',  &
                         'm/s', &
                         missing_value=xmissing )
    id_obswv      = &
    register_diag_field ( 'ice_model', 'obswv',      axt_water, Ice%Time, &
                         'observed v-current',  &
                         'm/s', &
                         missing_value=xmissing )
    id_wt      = &
    register_diag_field ( 'ice_model', 'wt',      axt_water, Ice%Time, &
                         'potential water temperature',  &
                         'K', &
                         missing_value=xmissing )
    id_wu      = &
    register_diag_field ( 'ice_model', 'wu',      axt_water, Ice%Time, &
                         'u current',  &
                         'm/s', &
                         missing_value=xmissing )
    id_wv      = &
    register_diag_field ( 'ice_model', 'wv',      axt_water, Ice%Time, &
                         'v current',  &
                         'm/s', &
                         missing_value=xmissing )
    id_ww      = &
    register_diag_field ( 'ice_model', 'ww',      axt_waterf, Ice%Time, &
                         'w current',  &
                         'm/s', &
                         missing_value=xmissing )
    id_ws      = &
    register_diag_field ( 'ice_model', 'ws',      axt_water, Ice%Time, &
                         'practical salinity',  &
                         '0/00', &
                         missing_value=xmissing )
    id_wtke      = &
    register_diag_field ( 'ice_model', 'wtke',      axt_waterf, Ice%Time, &
                         'turbulent kinetic energy',  &
                         'm2/s2', &
                         missing_value=xmissing )
    id_wlmx      = &
    register_diag_field ( 'ice_model', 'wlmx',      axt_waterf, Ice%Time, &
                         'mixing length',  &
                         'm', &
                         missing_value=xmissing )
    id_wldisp      = &
    register_diag_field ( 'ice_model', 'wldisp',      axt_waterf, Ice%Time, &
                         'dissipation length',  &
                         'm', &
                         missing_value=xmissing )
    id_wkm      = &
    register_diag_field ( 'ice_model', 'wkm',      axt_waterf, Ice%Time, &
                         'eddy diffusivity for momentum',  &
                         'm2/s', &
                         missing_value=xmissing )
    id_wkh      = &
    register_diag_field ( 'ice_model', 'wkh',      axt_waterf, Ice%Time, &
                         'eddy diffusivity for heat',  &
                         'm2/s', &
                         missing_value=xmissing )
    id_wrho1000      = &
    register_diag_field ( 'ice_model', 'wrho1000',      axt_water, Ice%Time, &
                         '1000-m potential water density',  &
                         'kg/m3', &
                         missing_value=xmissing )
    id_wtfn      = &
    register_diag_field ( 'ice_model', 'wtfn',      axt_water, Ice%Time, &
                         'mean temperature flux nudging at each level (positive into ocean)',  &
                         'K/s', &
                         missing_value=xmissing )
    id_wsfn      = &
    register_diag_field ( 'ice_model', 'wsfn',      axt_water, Ice%Time, &
                         'mean salinity flux nudging at each level (positive into ocean)',  &
                         'PSU/s', &
                         missing_value=xmissing )
    id_awufl      = &
    register_diag_field ( 'ice_model', 'awufl',      axt_water, Ice%Time, &
                         'advected u flux at each level (positive into ocean)',  &
                         'm/s2', &
                         missing_value=xmissing )
    id_awvfl      = &
    register_diag_field ( 'ice_model', 'awvfl',      axt_water, Ice%Time, &
                         'advected v flux at each level (positive into ocean)',  &
                         'm/s2', &
                         missing_value=xmissing )
    id_awtfl      = &
    register_diag_field ( 'ice_model', 'awtfl',      axt_water, Ice%Time, &
                         'advected temperature flux at each level (positive into ocean)',  &
                         'K/s', &
                         missing_value=xmissing )
    id_awsfl      = &
    register_diag_field ( 'ice_model', 'awsfl',      axt_water, Ice%Time, &
                         'advected salinity flux at each level (positive into ocean)',  &
                         'PSU/s', &
                         missing_value=xmissing )
    id_awtkefl      = &
    register_diag_field ( 'ice_model', 'awtkefl',      axt_waterf, Ice%Time, &
                         'advected tke at each level (positive into ocean)',  &
                         'm2/s3', &
                         missing_value=xmissing )
!  !  final output only
!    id_seaice      = &
!    register_diag_field ( 'ice_model', 'seaice',      axt, Ice%Time, &
!                         'ice cover: fraction of 1-SLM',  &
!                         ' (0-1)', &
!                         missing_value=xmissing )
  !  implicit with vdiff
    id_grndcapc      = &
    register_diag_field ( 'ice_model', 'grndcapc',      axt, Ice%Time, &
                         'areal heat capacity of the uppermost sit layer (snow/ice/water)',  &
                         'J/m**2/K', &
                         missing_value=xmissing )
    id_grndhflx      = &
    register_diag_field ( 'ice_model', 'grndhflx',      axt, Ice%Time, &
                         'ground heat flux below the surface',  &
                         'W/m**2, + upward, into the skin layer', &
                         missing_value=xmissing )
    id_grndflux      = &
    register_diag_field ( 'ice_model', 'grndflux',      axt, Ice%Time, &
                         'acc. ground heat flux below the surface',  &
                         'W/m**2*s, + upward, into the skin layer', &
                         missing_value=xmissing )

    id_wt0m      = &
    register_diag_field ( 'ice_model', 'wt0',      axt, Ice%Time, &
                         'potential water temperature at 0m depth',  &
                         'K', &
                         missing_value=xmissing )                         

    id_wt3m      = &
    register_diag_field ( 'ice_model', 'wt3',      axt, Ice%Time, &
                         'potential water temperature at 3m depth',  &
                         'K', &
                         missing_value=xmissing )                         

    id_wt10m      = &
    register_diag_field ( 'ice_model', 'wt10',      axt, Ice%Time, &
                         'potential water temperature at 10m depth',  &
                         'K', &
                         missing_value=xmissing )                         

    id_wt100m      = &
    register_diag_field ( 'ice_model', 'wt100',      axt, Ice%Time, &
                         'potential water temperature at 100m depth',  &
                         'K', &
                         missing_value=xmissing )               

  endif

end subroutine ice_diag_init

!=============================================================================================

 subroutine ice_register_restart (Ice, restart_file)
 type(ice_data_type), intent(inout) :: Ice
 character(len=*), intent(in) :: restart_file
 integer                      :: id_restart

  !id_restart = register_restart_field (Ice_restart, restart_file, 'mlon',      mlon)
  !id_restart = register_restart_field (Ice_restart, restart_file, 'mlon',      mlat)
  !id_restart = register_restart_field (Ice_restart, restart_file, 'num_part',  num_part)
   id_restart = register_restart_field (Ice_restart, restart_file, 'part_size', Ice%part_size, domain=Ice%domain)
   id_restart = register_restart_field (Ice_restart, restart_file, 'thickness', Ice%thickness, domain=Ice%domain)
   id_restart = register_restart_field (Ice_restart, restart_file, 'albedo',    Ice%albedo,    domain=Ice%domain)
   id_restart = register_restart_field (Ice_restart, restart_file, 'temp_1',    Ice%temp(:,:,:,1), domain=Ice%domain)

   ! albedo streams
   id_restart_albedo = register_restart_field (Ice_restart, restart_file, 'albedo_vis_dir', Ice%albedo_vis_dir, &
                                                 domain=Ice%domain, mandatory=.false.)
   id_restart        = register_restart_field (Ice_restart, restart_file, 'albedo_nir_dir', Ice%albedo_nir_dir, &
                                                 domain=Ice%domain, mandatory=.false.)
   id_restart        = register_restart_field (Ice_restart, restart_file, 'albedo_vis_dif', Ice%albedo_vis_dif, &
                                                 domain=Ice%domain, mandatory=.false.)
   id_restart        = register_restart_field (Ice_restart, restart_file, 'albedo_nir_dif', Ice%albedo_nir_dif, &
                                                 domain=Ice%domain, mandatory=.false.)

   id_restart = register_restart_field (Ice_restart, restart_file, 'rough_mom',   Ice%rough_mom,   domain=Ice%domain)
   id_restart = register_restart_field (Ice_restart, restart_file, 'rough_heat',  Ice%rough_heat,  domain=Ice%domain)
   id_restart = register_restart_field (Ice_restart, restart_file, 'rough_moist', Ice%rough_moist, domain=Ice%domain)
   id_restart = register_restart_field (Ice_restart, restart_file, 'u_surf',      Ice%u_surf,      domain=Ice%domain)
   id_restart = register_restart_field (Ice_restart, restart_file, 'v_surf',      Ice%v_surf,      domain=Ice%domain)
!!!   id_restart = register_restart_field (Ice_restart, restart_file, 'fluxn',      Ice%fluxn,      domain=Ice%domain)          ! bjt
!!!   id_restart = register_restart_field (Ice_restart, restart_file, 'dfluxn',     Atmos_boundary%dfluxn,     domain=Ice%domain)          ! bjt
!!!   id_restart = register_restart_field (Ice_restart, restart_file, 'sw_flux',      Ice%sw_flux,      domain=Ice%domain)          ! bjt
   id_restart = register_restart_field (Ice_restart, restart_file, 'frazil',      Ice%frazil,      domain=Ice%domain)
   id_restart = register_restart_field (Ice_restart, restart_file, 'flux_u_bot',  Ice%flux_u_bot,  domain=Ice%domain)
   id_restart = register_restart_field (Ice_restart, restart_file, 'flux_v_bot',  Ice%flux_v_bot,  domain=Ice%domain)

   ! sit
   !!! id_restart = register_restart_field (Ice_restart, restart_file, 'siced',   Ice%siced,   domain=Ice%domain)
   id_restart = register_restart_field (Ice_restart, restart_file, 'sitmask',   Ice%sitmask,   domain=Ice%domain)
   id_restart = register_restart_field (Ice_restart, restart_file, 'obsseaice',   Ice%obsseaice,   domain=Ice%domain)
   id_restart = register_restart_field (Ice_restart, restart_file, 'obswtb',   Ice%obswtb,   domain=Ice%domain)
   id_restart = register_restart_field (Ice_restart, restart_file, 'obswsb',   Ice%obswsb,   domain=Ice%domain)

#ifdef MIMIC_SIT
  if (.true.) then
#else
  if (do_sit) then
#endif

    !!! id_restart = register_restart_field (Ice_restart, restart_file, 'cor',   Ice%cor,   domain=Ice%domain)
    id_restart = register_restart_field (Ice_restart, restart_file, 'slm',   Ice%slm,   domain=Ice%domain)
    id_restart = register_restart_field (Ice_restart, restart_file, 'lclass',   Ice%lclass,   domain=Ice%domain)
    id_restart = register_restart_field (Ice_restart, restart_file, 'bathy',   Ice%bathy,   domain=Ice%domain)
    id_restart = register_restart_field (Ice_restart, restart_file, 'wlvl',   Ice%wlvl,   domain=Ice%domain)
    id_restart = register_restart_field (Ice_restart, restart_file, 'ocnmask',   Ice%ocnmask,   domain=Ice%domain)
    id_restart = register_restart_field (Ice_restart, restart_file, 'obox_mask',   Ice%obox_mask,   domain=Ice%domain)

    !!! id_restart = register_restart_field (Ice_restart, restart_file, 'sni',   Ice%sni,   domain=Ice%domain)
    !!! id_restart = register_restart_field (Ice_restart, restart_file, 'siced',   Ice%siced,   domain=Ice%domain)
    !!! id_restart = register_restart_field (Ice_restart, restart_file, 'tsi',   Ice%tsi,   domain=Ice%domain)
    !!! id_restart = register_restart_field (Ice_restart, restart_file, 'tsw',   Ice%tsw,   domain=Ice%domain)
    id_restart = register_restart_field (Ice_restart, restart_file, 'tsl',   Ice%tsl,   domain=Ice%domain)
    id_restart = register_restart_field (Ice_restart, restart_file, 'tslm',   Ice%tslm,   domain=Ice%domain)
    id_restart = register_restart_field (Ice_restart, restart_file, 'tslm1',   Ice%tslm1,   domain=Ice%domain)
    !!! id_restart = register_restart_field (Ice_restart, restart_file, 'ocu',   Ice%ocu,   domain=Ice%domain)
    !!! id_restart = register_restart_field (Ice_restart, restart_file, 'ocv',   Ice%ocv,   domain=Ice%domain)

    id_restart = register_restart_field (Ice_restart, restart_file, 'ctfreez2',   Ice%ctfreez2,   domain=Ice%domain)

   ! 2-d SIT vars
    !!! id_restart = register_restart_field (Ice_restart, restart_file, 'wtb',   Ice%wtb,   domain=Ice%domain)
    !!! id_restart = register_restart_field (Ice_restart, restart_file, 'wub',   Ice%wub,   domain=Ice%domain)
    !!! id_restart = register_restart_field (Ice_restart, restart_file, 'wvb',   Ice%wvb,   domain=Ice%domain)
    !!! id_restart = register_restart_field (Ice_restart, restart_file, 'wsb',   Ice%wsb,   domain=Ice%domain)
    !!! id_restart = register_restart_field (Ice_restart, restart_file, 'fluxiw',   Ice%fluxiw,   domain=Ice%domain)
    !!! id_restart = register_restart_field (Ice_restart, restart_file, 'pme2',   Ice%pme2,   domain=Ice%domain)
    !!! id_restart = register_restart_field (Ice_restart, restart_file, 'subfluxw',   Ice%subfluxw,   domain=Ice%domain)
    !!! id_restart = register_restart_field (Ice_restart, restart_file, 'wsubsal',   Ice%wsubsal,   domain=Ice%domain)
    id_restart = register_restart_field (Ice_restart, restart_file, 'cc',   Ice%cc,   domain=Ice%domain)
    id_restart = register_restart_field (Ice_restart, restart_file, 'hc',   Ice%hc,   domain=Ice%domain)
    id_restart = register_restart_field (Ice_restart, restart_file, 'engwac',   Ice%engwac,   domain=Ice%domain)
    id_restart = register_restart_field (Ice_restart, restart_file, 'sc',   Ice%sc,   domain=Ice%domain)
    id_restart = register_restart_field (Ice_restart, restart_file, 'saltwac',   Ice%saltwac,   domain=Ice%domain)
    !!! id_restart = register_restart_field (Ice_restart, restart_file, 'wtfns',   Ice%wtfns,   domain=Ice%domain)
    !!! id_restart = register_restart_field (Ice_restart, restart_file, 'wsfns',   Ice%wsfns,   domain=Ice%domain)

  ! 3-d SIT vars: snow/ice
    id_restart = register_restart_field (Ice_restart, restart_file, 'zsi',   Ice%zsi,   domain=Ice%domain)
    id_restart = register_restart_field (Ice_restart, restart_file, 'silw',   Ice%silw,   domain=Ice%domain)
    id_restart = register_restart_field (Ice_restart, restart_file, 'tsnic',   Ice%tsnic,   domain=Ice%domain)

  ! 3-d SIT vars: water column
    !!! if (lwoa_gfdl) then
    !!!   id_restart = register_restart_field (Ice_restart, restart_file, 'obswt',   Ice%obswt,   domain=Ice%domain)
    !!!   id_restart = register_restart_field (Ice_restart, restart_file, 'obsws',   Ice%obsws,   domain=Ice%domain)
    !!!   id_restart = register_restart_field (Ice_restart, restart_file, 'obswu',   Ice%obswu,   domain=Ice%domain)
    !!!   id_restart = register_restart_field (Ice_restart, restart_file, 'obswv',   Ice%obswv,   domain=Ice%domain)  
    !!! endif
    id_restart = register_restart_field (Ice_restart, restart_file, 'wt',   Ice%wt,   domain=Ice%domain)
    id_restart = register_restart_field (Ice_restart, restart_file, 'wu',   Ice%wu,   domain=Ice%domain)
    id_restart = register_restart_field (Ice_restart, restart_file, 'wv',   Ice%wv,   domain=Ice%domain)
    id_restart = register_restart_field (Ice_restart, restart_file, 'ww',   Ice%ww,   domain=Ice%domain)
    id_restart = register_restart_field (Ice_restart, restart_file, 'ws',   Ice%ws,   domain=Ice%domain)
    id_restart = register_restart_field (Ice_restart, restart_file, 'wtke',   Ice%wtke,   domain=Ice%domain)
    id_restart = register_restart_field (Ice_restart, restart_file, 'wlmx',   Ice%wlmx,   domain=Ice%domain)
    id_restart = register_restart_field (Ice_restart, restart_file, 'wldisp',   Ice%wldisp,   domain=Ice%domain)
    id_restart = register_restart_field (Ice_restart, restart_file, 'wkm',   Ice%wkm,   domain=Ice%domain)
    id_restart = register_restart_field (Ice_restart, restart_file, 'wkh',   Ice%wkh,   domain=Ice%domain)
    !!! id_restart = register_restart_field (Ice_restart, restart_file, 'wrho1000',   Ice%wrho1000,   domain=Ice%domain)
    !!! id_restart = register_restart_field (Ice_restart, restart_file, 'wtfn',   Ice%wtfn,   domain=Ice%domain)
    !!! id_restart = register_restart_field (Ice_restart, restart_file, 'wsfn',   Ice%wsfn,   domain=Ice%domain)
    !!! id_restart = register_restart_field (Ice_restart, restart_file, 'awufl',   Ice%awufl,   domain=Ice%domain)
    !!! id_restart = register_restart_field (Ice_restart, restart_file, 'awvfl',   Ice%awvfl,   domain=Ice%domain)
    !!! id_restart = register_restart_field (Ice_restart, restart_file, 'awtfl',   Ice%awtfl,   domain=Ice%domain)
    !!! id_restart = register_restart_field (Ice_restart, restart_file, 'awsfl',   Ice%awsfl,   domain=Ice%domain)
    !!! id_restart = register_restart_field (Ice_restart, restart_file, 'awtkefl',   Ice%awtkefl,   domain=Ice%domain)

  ! Final output only
    !!! id_restart = register_restart_field (Ice_restart, restart_file, 'seaice',   Ice%seaice,   domain=Ice%domain)
    !!! id_restart = register_restart_field (Ice_restart, restart_file, 'grndcapc',   Ice%grndcapc,   domain=Ice%domain)
    !!! id_restart = register_restart_field (Ice_restart, restart_file, 'grndhflx',   Ice%grndhflx,   domain=Ice%domain)
    !!! id_restart = register_restart_field (Ice_restart, restart_file, 'grndflux',   Ice%grndflux,   domain=Ice%domain)
  endif

 end subroutine ice_register_restart

!=============================================================================================

! dummy routine
 subroutine ice_stock_pe(Ice, index, value)
 type(ice_data_type), intent(in) :: Ice
 integer, intent(in) :: index
 real, intent(out)   :: value

 value = 0.0
 if(.not.Ice%pe) return

 if(.not.stock_warning_issued) then
   call error_mesg('ice_stock_pe','Stocks not yet implemented. Returning zero.',NOTE)
   stock_warning_issued = .true.
 endif

 end subroutine ice_stock_pe
!=============================================================================================

subroutine ice_data_type_chksum(id, timestep, data_type)
  use fms_mod,                 only: stdout
  use mpp_mod,                 only: mpp_chksum

    character(len=*), intent(in) :: id
    integer         , intent(in) :: timestep
    type(ice_data_type), intent(in) :: data_type
    integer ::   n, m, outunit
    
    outunit = stdout()

100 FORMAT("CHECKSUM::",A32," = ",Z20)
    write(outunit,*) 'BEGIN CHECKSUM(ice_data_type):: ', id, timestep
    write(outunit,100) 'ice_data_type%part_size          ',mpp_chksum(data_type%part_size          )
    write(outunit,100) 'ice_data_type%t_surf             ',mpp_chksum(data_type%t_surf             )
    write(outunit,100) 'ice_data_type%albedo             ',mpp_chksum(data_type%albedo             )
    write(outunit,100) 'ice_data_type%albedo_vis_dir     ',mpp_chksum(data_type%albedo_vis_dir     )
    write(outunit,100) 'ice_data_type%albedo_nir_dir     ',mpp_chksum(data_type%albedo_nir_dir     )
    write(outunit,100) 'ice_data_type%albedo_vis_dif     ',mpp_chksum(data_type%albedo_vis_dif     )
    write(outunit,100) 'ice_data_type%albedo_nir_dif     ',mpp_chksum(data_type%albedo_nir_dif     )
    write(outunit,100) 'ice_data_type%rough_mom          ',mpp_chksum(data_type%rough_mom          )
    write(outunit,100) 'ice_data_type%rough_heat         ',mpp_chksum(data_type%rough_heat         )
    write(outunit,100) 'ice_data_type%rough_moist        ',mpp_chksum(data_type%rough_moist        )
    write(outunit,100) 'ice_data_type%frazil             ',mpp_chksum(data_type%frazil             )
    write(outunit,100) 'ice_data_type%u_surf             ',mpp_chksum(data_type%u_surf             )
    write(outunit,100) 'ice_data_type%v_surf             ',mpp_chksum(data_type%v_surf             )
!!!    write(outunit,100) 'ice_data_type%fluxn             ',mpp_chksum(data_type%fluxn             )                  ! bjt
!!!    write(outunit,100) 'ice_data_type%dfluxn            ',mpp_chksum(data_type%dfluxn            )                  ! bjt
!!!    write(outunit,100) 'ice_data_type%sw_flux             ',mpp_chksum(data_type%sw_flux             )                  ! bjt
    write(outunit,100) 'ice_data_type%flux_u_bot         ',mpp_chksum(data_type%flux_u_bot         )
    write(outunit,100) 'ice_data_type%flux_v_bot         ',mpp_chksum(data_type%flux_v_bot         )
    write(outunit,100) 'ice_data_type%flux_t_bot         ',mpp_chksum(data_type%flux_t_bot         )
    write(outunit,100) 'ice_data_type%flux_q_bot         ',mpp_chksum(data_type%flux_q_bot         )
    write(outunit,100) 'ice_data_type%flux_lh_bot        ',mpp_chksum(data_type%flux_lh_bot        )
    write(outunit,100) 'ice_data_type%flux_sw_bot        ',mpp_chksum(data_type%flux_sw_bot        )
    write(outunit,100) 'ice_data_type%flux_sw_vis_bot    ',mpp_chksum(data_type%flux_sw_vis_bot    )
    write(outunit,100) 'ice_data_type%flux_sw_dir_bot    ',mpp_chksum(data_type%flux_sw_dir_bot    )
    write(outunit,100) 'ice_data_type%flux_sw_dif_bot    ',mpp_chksum(data_type%flux_sw_dif_bot    )
    write(outunit,100) 'ice_data_type%flux_sw_vis_dir_bot',mpp_chksum(data_type%flux_sw_vis_dir_bot)
    write(outunit,100) 'ice_data_type%flux_sw_vis_dif_bot',mpp_chksum(data_type%flux_sw_vis_dif_bot)
    write(outunit,100) 'ice_data_type%flux_sw_nir_dir_bot',mpp_chksum(data_type%flux_sw_nir_dir_bot)
    write(outunit,100) 'ice_data_type%flux_sw_nir_dif_bot',mpp_chksum(data_type%flux_sw_nir_dif_bot)
    write(outunit,100) 'ice_data_type%flux_lw_bot        ',mpp_chksum(data_type%flux_lw_bot        )
    write(outunit,100) 'ice_data_type%lprec_bot          ',mpp_chksum(data_type%lprec_bot          )
    write(outunit,100) 'ice_data_type%fprec_bot          ',mpp_chksum(data_type%fprec_bot          )
    write(outunit,100) 'ice_data_type%runoff_bot         ',mpp_chksum(data_type%runoff_bot         )

    write(outunit,100) 'ice_data_type%flux_u             ',mpp_chksum(data_type%flux_u             )
    write(outunit,100) 'ice_data_type%flux_v             ',mpp_chksum(data_type%flux_v             )
    write(outunit,100) 'ice_data_type%flux_t             ',mpp_chksum(data_type%flux_t             )
    write(outunit,100) 'ice_data_type%flux_q             ',mpp_chksum(data_type%flux_q             )
    write(outunit,100) 'ice_data_type%flux_lh            ',mpp_chksum(data_type%flux_lh            )
    write(outunit,100) 'ice_data_type%flux_sw            ',mpp_chksum(data_type%flux_sw            )
    write(outunit,100) 'ice_data_type%flux_sw_vis        ',mpp_chksum(data_type%flux_sw_vis        )
    write(outunit,100) 'ice_data_type%flux_sw_dir        ',mpp_chksum(data_type%flux_sw_dir        )
    write(outunit,100) 'ice_data_type%flux_sw_dif        ',mpp_chksum(data_type%flux_sw_dif        )
    write(outunit,100) 'ice_data_type%flux_sw_vis_dir    ',mpp_chksum(data_type%flux_sw_vis_dir    )
    write(outunit,100) 'ice_data_type%flux_sw_vis_dif    ',mpp_chksum(data_type%flux_sw_vis_dif    )
    write(outunit,100) 'ice_data_type%flux_sw_nir_dir    ',mpp_chksum(data_type%flux_sw_nir_dir    )
    write(outunit,100) 'ice_data_type%flux_sw_nir_dif    ',mpp_chksum(data_type%flux_sw_nir_dif    )
    write(outunit,100) 'ice_data_type%flux_lw            ',mpp_chksum(data_type%flux_lw            )
    write(outunit,100) 'ice_data_type%lprec              ',mpp_chksum(data_type%lprec              )
    write(outunit,100) 'ice_data_type%fprec              ',mpp_chksum(data_type%fprec              )
    write(outunit,100) 'ice_data_type%p_surf             ',mpp_chksum(data_type%p_surf             )
    write(outunit,100) 'ice_data_type%runoff             ',mpp_chksum(data_type%runoff             )
    write(outunit,100) 'ice_data_type%calving            ',mpp_chksum(data_type%calving            )
    write(outunit,100) 'ice_data_type%flux_salt          ',mpp_chksum(data_type%flux_salt          )

    do n = 1, data_type%ocean_fields%num_bcs  !{
       do m = 1, data_type%ocean_fields%bc(n)%num_fields  !{
          !write(outunit,101) 'ice%', m, n, mpp_chksum(Ice%ocean_fields%bc(n)%field(m)%values)
          write(outunit,101) 'ice%',trim(data_type%ocean_fields%bc(n)%name), &
               trim(data_type%ocean_fields%bc(n)%field(m)%name), &
               mpp_chksum(data_type%ocean_fields%bc(n)%field(m)%values)
       enddo  !} m
    enddo  !} n
101 FORMAT("CHECKSUM::",A16,a,'%',a," = ",Z20)

end subroutine ice_data_type_chksum


subroutine ocn_ice_bnd_type_chksum(id, timestep, bnd_type)

    character(len=*), intent(in) :: id
    integer         , intent(in) :: timestep
    type(ocean_ice_boundary_type), intent(in) :: bnd_type
    integer ::   n, m, outunit
    
    outunit = stdout()

    write(outunit,*) 'BEGIN CHECKSUM(ocean_ice_boundary_type):: ', id, timestep
    write(outunit,100) 'ocn_ice_bnd_type%u        ',mpp_chksum(bnd_type%u        )
    write(outunit,100) 'ocn_ice_bnd_type%v        ',mpp_chksum(bnd_type%v        )
    write(outunit,100) 'ocn_ice_bnd_type%t        ',mpp_chksum(bnd_type%t        )
    write(outunit,100) 'ocn_ice_bnd_type%s        ',mpp_chksum(bnd_type%s        )
    write(outunit,100) 'ocn_ice_bnd_type%frazil   ',mpp_chksum(bnd_type%frazil   )
    write(outunit,100) 'ocn_ice_bnd_type%sea_level',mpp_chksum(bnd_type%sea_level)
!    write(outunit,100) 'ocn_ice_bnd_type%data     ',mpp_chksum(bnd_type%data     )
100 FORMAT("CHECKSUM::",A32," = ",Z20)

    do n = 1, bnd_type%fields%num_bcs  !{
       do m = 1, bnd_type%fields%bc(n)%num_fields  !{
          write(outunit,101) 'oibt%',trim(bnd_type%fields%bc(n)%name), &
               trim(bnd_type%fields%bc(n)%field(m)%name), &
               mpp_chksum(bnd_type%fields%bc(n)%field(m)%values)
       enddo  !} m
    enddo  !} n
101 FORMAT("CHECKSUM::",A16,a,'%',a," = ",Z20)

end subroutine ocn_ice_bnd_type_chksum

subroutine atm_ice_bnd_type_chksum(id, timestep, bnd_type)

    character(len=*), intent(in) :: id
    integer         , intent(in) :: timestep
    type(atmos_ice_boundary_type), intent(in) :: bnd_type
    integer ::   n, outunit
    
    outunit = stdout()

    write(outunit,*) 'BEGIN CHECKSUM(atmos_ice_boundary_type):: ', id, timestep
    write(outunit,100) 'atm_ice_bnd_type%u_flux          ',mpp_chksum(bnd_type%u_flux)          
    write(outunit,100) 'atm_ice_bnd_type%v_flux          ',mpp_chksum(bnd_type%v_flux)
    write(outunit,100) 'atm_ice_bnd_type%u_star          ',mpp_chksum(bnd_type%u_star)
    write(outunit,100) 'atm_ice_bnd_type%t_flux          ',mpp_chksum(bnd_type%t_flux)
    write(outunit,100) 'atm_ice_bnd_type%q_flux          ',mpp_chksum(bnd_type%q_flux)
    write(outunit,100) 'atm_ice_bnd_type%lw_flux         ',mpp_chksum(bnd_type%lw_flux)
    write(outunit,100) 'atm_ice_bnd_type%sw_flux         ',mpp_chksum(bnd_type%sw_flux)
    write(outunit,100) 'atm_ice_bnd_type%sw_flux_vis_dir ',mpp_chksum(bnd_type%sw_flux_vis_dir)
    write(outunit,100) 'atm_ice_bnd_type%sw_flux_vis_dif ',mpp_chksum(bnd_type%sw_flux_vis_dif)
    write(outunit,100) 'atm_ice_bnd_type%sw_flux_nir_dir ',mpp_chksum(bnd_type%sw_flux_nir_dir)
    write(outunit,100) 'atm_ice_bnd_type%sw_flux_nir_dif ',mpp_chksum(bnd_type%sw_flux_nir_dif)
    write(outunit,100) 'atm_ice_bnd_type%fluxn          ',mpp_chksum(bnd_type%fluxn)
    write(outunit,100) 'atm_ice_bnd_type%dfluxn         ',mpp_chksum(bnd_type%dfluxn)
    write(outunit,100) 'atm_ice_bnd_type%lprec           ',mpp_chksum(bnd_type%lprec)
    write(outunit,100) 'atm_ice_bnd_type%fprec           ',mpp_chksum(bnd_type%fprec)
    write(outunit,100) 'atm_ice_bnd_type%dhdt            ',mpp_chksum(bnd_type%dhdt)
    write(outunit,100) 'atm_ice_bnd_type%dedt            ',mpp_chksum(bnd_type%dedt)
    write(outunit,100) 'atm_ice_bnd_type%drdt            ',mpp_chksum(bnd_type%drdt)
    write(outunit,100) 'atm_ice_bnd_type%coszen          ',mpp_chksum(bnd_type%coszen)
    write(outunit,100) 'atm_ice_bnd_type%p               ',mpp_chksum(bnd_type%p)
!    write(outunit,100) 'atm_ice_bnd_type%data            ',mpp_chksum(bnd_type%data)
100 FORMAT("CHECKSUM::",A32," = ",Z20)
end subroutine atm_ice_bnd_type_chksum

subroutine lnd_ice_bnd_type_chksum(id, timestep, bnd_type)

    character(len=*), intent(in) :: id
    integer         , intent(in) :: timestep
    type(land_ice_boundary_type), intent(in) :: bnd_type
    integer ::   n, outunit
    
    outunit = stdout()

    write(outunit,*) 'BEGIN CHECKSUM(land_ice_boundary_type):: ', id, timestep
    write(outunit,100) 'lnd_ice_bnd_type%runoff  ',mpp_chksum(bnd_type%runoff)
    write(outunit,100) 'lnd_ice_bnd_type%calving ',mpp_chksum(bnd_type%calving)
!    write(outunit,100) 'lnd_ice_bnd_type%data    ',mpp_chksum(bnd_type%data)
100 FORMAT("CHECKSUM::",A32," = ",Z20)
end subroutine lnd_ice_bnd_type_chksum

!--------------------------------------------------------
subroutine set_ocean_grid_size(ni,nj,grid_file, grid_name)

  use mosaic_mod,      only: get_mosaic_ntiles, get_mosaic_ncontacts
  use fms_mod,         only: get_global_att_value, stderr
  use diag_output_mod,   only: get_diag_global_att, set_diag_global_att
  use mosaic_mod,      only: calc_mosaic_grid_area, get_mosaic_contact
      
  character(len=*),      intent(in), optional :: grid_file
  character(len=*),      intent(in), optional :: grid_name
  integer, intent(inout)                      :: ni, nj

  integer                                   :: siz(4)
  integer                                   :: m
  integer                                   :: nx(1), ny(1)
  integer                                   :: ntiles, ncontacts
  integer, dimension(2)                     :: tile1, tile2
  integer, dimension(2)                     :: istart1, iend1, jstart1, jend1
  integer, dimension(2)                     :: istart2, iend2, jstart2, jend2
  character(len=256)                        :: grd_file, ocean_mosaic, attvalue


  grid_ni=0 ; grid_nj=0
  mosaic = .false.
  grd_file = "INPUT/grid_spec.nc"
  if(present(grid_file)) grd_file = grid_file

  !
  !  Determine if the grid is mosaic file
  !
  if(field_exist(grd_file, 'ocn_mosaic_file') .or. field_exist(grd_file, 'gridfiles') ) then ! read from mosaic file
     if ( mpp_pe().EQ.mpp_root_pe() ) write(stdout(),*) '==>Note from ocean_grids_mod(set_ocean_grid_size): read grid from mosaic version grid'
     grid_version = VERSION_2
     mosaic = .true.
     if( field_exist(grd_file, 'ocn_mosaic_file') ) then ! coupler mosaic
        call read_data(grd_file, "ocn_mosaic_file", ocean_mosaic)
        ocean_mosaic = "INPUT/"//trim(ocean_mosaic)
     else
        ocean_mosaic = trim(grd_file)
     end if
     ntiles = get_mosaic_ntiles(ocean_mosaic)
     if(ntiles .NE. 1) call mpp_error(FATAL, '==>Error from ocean_grids_mod(set_ocean_grid_size): '//&
                   'ntiles should be 1 for ocean mosaic, contact developer')
! Notify diag_manager of mosaic grid
     call set_diag_global_att ('ocean','mosaic','1')
     call read_data(ocean_mosaic, "gridfiles", ocean_hgrid)
     ocean_hgrid = 'INPUT/'//trim(ocean_hgrid)
     call field_size(ocean_hgrid, 'x', siz)
     grid_ni = nx(1)
     grid_nj = ny(1)
     if(mod(siz(1),2) .NE. 1) call mpp_error(FATAL, '==>Error from ocean_grids_mod(set_ocean_grid_size): '//&
          'x-size of x in file '//trim(ocean_hgrid)//' should be 2*ni+1')
     if(mod(siz(2),2) .NE. 1) call mpp_error(FATAL, '==>Error from ocean_grids_mod(set_ocean_grid_size): '//&
          'y-size of x in file '//trim(ocean_hgrid)//' should be 2*nj+1')
     grid_ni = siz(1)/2
     grid_nj = siz(2)/2
  else  if(field_exist(grd_file, 'x_C')) then
     ocean_hgrid = grd_file
     if ( mpp_pe().EQ.mpp_root_pe()) write(stdout(),*) '==>Note from ocean_grids_mod(set_ocean_grid_size): read grid from new version grid'
     grid_version = VERSION_1
     call field_size( ocean_hgrid, 'x_C', siz)
     grid_ni = siz(1)
     grid_nj = siz(2)
  else if(field_exist(grd_file, 'geolon_c')) then
     ocean_hgrid = grd_file
     if ( mpp_pe().EQ.mpp_root_pe() ) write(stdout(),*) '==>Note from ocean_grids_mod(set_ocean_grid_size): read grid from old version grid'
     grid_version = VERSION_0
     call field_size( ocean_hgrid, 'geolon_c', siz)
     grid_ni = siz(1)
     grid_nj = siz(2)
  else
     call mpp_error(FATAL, '==>Error from ocean_grids_mod(set_ocean_grid_size): '//&
                     'x_C, geolon_c, ocn_mosaic_file, gridfiles does not exist in file ' //trim(grd_file))
  endif

  if (grid_ni == 0 .or. grid_nj == 0 ) then
     if ( mpp_pe().EQ.mpp_root_pe() ) write(stdout(),*) '==>Error reading grid information from ',trim(grd_file),'. Make sure file exists'
     call mpp_error(FATAL,'==>Error reading grid information from grid file.  Are you sure file exists?')
  endif

  tripolar=.false.

  if(grid_version == VERSION_2) then
     !z1l: f_plane, beta_plane area not supported in mosaic grid. Need to think about to implement this.
     if(field_exist(ocean_mosaic, "contacts") ) then
        ncontacts = get_mosaic_ncontacts(ocean_mosaic)
        if(ncontacts < 1) call mpp_error(FATAL,'==>Error from ocean_grids_mod(set_ocean_grid_size): '//&
                       'number of contacts should be larger than 0 when field contacts exist in file '//trim(ocean_mosaic) )
        if(ncontacts > 2) call mpp_error(FATAL,'==>Error from ocean_grids_mod(set_ocean_grid_size): '//&
                       'number of contacts should be no larger than 2')
        call get_mosaic_contact( ocean_mosaic, tile1(1:ncontacts), tile2(1:ncontacts),           &
             istart1(1:ncontacts), iend1(1:ncontacts), jstart1(1:ncontacts), jend1(1:ncontacts), &
             istart2(1:ncontacts), iend2(1:ncontacts), jstart2(1:ncontacts), jend2(1:ncontacts)  )
        do m = 1, ncontacts
           if(istart1(m) == iend1(m) ) then  ! x-direction contact, only cyclic condition
              if(istart2(m) .NE. iend2(m) ) call mpp_error(FATAL,  &
                   "==>Error from ocean_grids_mod(set_ocean_grid_size): only cyclic condition is allowed for x-boundary")
           else if( jstart1(m) == jend1(m) ) then  ! y-direction contact, cyclic or folded-north
              if(jstart2(m) .NE. jend2(m) ) call mpp_error(FATAL,  &
                   "==>Error from ocean_grids_mod(set_ocean_grid_size): "//&
                   "only cyclic/folded-north condition is allowed for y-boundary")
              if( jstart1(m) == jstart2(m) ) then ! folded north
             tripolar = .true.
         endif
           else
              call mpp_error(FATAL,  &
                   "==>Error from ocean_grids_mod(set_ocean_grid_size): invalid boundary contact")
         end if
  end do
     end if
  else
     if( get_global_att_value(ocean_hgrid, "y_boundary_type", attvalue) ) then
        if(attvalue == 'fold_north_edge') then
           tripolar = .true.
        end if
     end if
  end if

  if(tripolar) then
     call mpp_error(NOTE,'==>Note from ocean_grids_mod(set_ocean_grid_size): y_boundary_type is fold_north_edge')
  endif

  if (PRESENT(grid_name)) then
     name = grid_name
  else
     name = 'ocean'
  endif

  ni = grid_ni
  nj = grid_nj

end subroutine set_ocean_grid_size
!#######################################################################
! </SUBROUTINE> NAME="set_ocean_hgrid_arrays"
!
!Following are the available fields in mosaic files
!--------------------------------------------------------
!Mosaic file     fields
!--------------------------------------------------------
!ocean_hgrid.nc  x, y, dx, dy, angle_dx, area
!ocean_vgrid.nc  zeta
!topog.nc        depth
!
! </DESCRIPTION>
subroutine set_ocean_hgrid_arrays(geolon,geolat,wet,domain)

  real, dimension(:,:), intent(inout) :: geolon, geolat,wet
  type(domain2D),       intent(in)    :: domain

  real, dimension(:),   allocatable          :: data
  real, dimension(:,:), allocatable          :: tmp
  real, dimension(:,:), allocatable          :: tmp_local
  real, dimension(:,:), allocatable          :: tmp1_local
  real, dimension(:,:), allocatable          :: tmp2_local
  real, dimension(:,:), allocatable          :: tmpx
  real, dimension(:,:), allocatable          :: tmpy

  integer :: i, j, k, lon_Tripol, ni, nj
  integer :: ioff, joff, iend, jend
  character(len=128) :: grd_file ! contains wet field
  character(len=128) :: topog_file

  ! set the grid points coordinates (degrees) and grid spacing (degrees)

  grd_file='INPUT/grid_spec.nc'
  topog_file='INPUT/topog.nc'

  ni = grid_ni;nj = grid_nj

  if (size(geolon,1).ne.ni+1) call mpp_error(FATAL,'array size mismatch in call to set_ocean_hgrid_arrays')
  if (size(geolon,2).ne.nj+1) call mpp_error(FATAL,'array size mismatch in call to set_ocean_hgrid_arrays')
  if (size(geolat,1).ne.ni+1) call mpp_error(FATAL,'array size mismatch in call to set_ocean_hgrid_arrays')
  if (size(geolat,2).ne.nj+1) call mpp_error(FATAL,'array size mismatch in call to set_ocean_hgrid_arrays')

  allocate (x_c(ni+1,nj+1))
  allocate (y_c(ni+1,nj+1))
  allocate (grid_x_c(ni))
  allocate (grid_y_c(nj))
  allocate (grid_wet(size(wet,1),size(wet,2)))

  !--- initialize grid data

  x_c=0.0;    y_c=0.0;     grid_x_c=0.0; grid_y_c=0.0 ; grid_wet=0.0

  select case( grid_version )
  case( VERSION_0 )
     call read_data(ocean_hgrid, "gridlon_c",      grid_x_c, no_domain = .true.)
     call read_data(ocean_hgrid, "gridlat_c",      grid_y_c, no_domain = .true.)
  case( VERSION_1 )
     call read_data(ocean_hgrid, "grid_x_C",      grid_x_c, no_domain = .true.)
     call read_data(ocean_hgrid, "grid_y_C",      grid_y_c, no_domain = .true.)
  case( VERSION_2 )
     allocate(tmpx(2*ni+1, 2*nj+1), tmpy(2*ni+1, 2*nj+1) )
     call read_data(ocean_hgrid, "x", tmpx, no_domain=.TRUE.)
     call read_data(ocean_hgrid, "y", tmpy, no_domain=.TRUE.)
     do i = 1, ni
        grid_x_c(i) = tmpx(2*i,  2)
     enddo

     lon_Tripol = ni/4 ! 90 for 1 degree grid
     do j = 1, nj
        grid_y_c(j) = tmpy(2*lon_Tripol+1 , 2*j)
     enddo
  end select

  allocate(tmp(ni+1,nj+1))
  !--- xc
  select case(grid_version)
  case(VERSION_0)
     call read_data(ocean_hgrid, 'geolon_c', tmp(1:ni,1:nj), no_domain = .true.)
  case(VERSION_1)
     call read_data(ocean_hgrid, 'x_C', tmp, no_domain = .true.)
  case(VERSION_2)
     tmp(1:ni+1,1:nj+1) = tmpx(1:2*ni+1:2,1:2*nj+1:2)
  end select
  x_c = tmp

  !--- yc
  select case(grid_version)
  case(VERSION_0)
     call read_data(ocean_hgrid, 'geolat_c', tmp(1:ni,1:nj), no_domain = .true.)
  case(VERSION_1)
     call read_data(ocean_hgrid, 'y_C', tmp, no_domain = .true.)
  case(VERSION_2)
     tmp(1:ni+1,1:nj+1) = tmpy(1:2*ni+1:2,1:2*nj+1:2)
  end select
  y_c = tmp

!--- wet / depth
   select case(grid_version)
     case(VERSION_0)
        call read_data(ocean_hgrid, 'wet', grid_wet, domain)
     case (VERSION_1)
        call read_data(ocean_hgrid, 'wet', grid_wet, domain)
     case (VERSION_2)
        if (field_exist(topog_file,'depth')) then
           call read_data(topog_file,'depth',grid_wet, domain)
           where ( grid_wet > 0.0 )
              grid_wet = 1.0
           elsewhere
              grid_wet = 0.0
           end where
        else
           call mpp_error(FATAL,'depth field does not exist in topog_file')
        endif
   end select

   geolon=x_c; geolat=y_c; wet=grid_wet

end subroutine set_ocean_hgrid_arrays
!
!--- CYTu --------------------------------------------------------------
!-----------------------------------------------------------------------
!
!#######################################################################
end module ice_model_mod
