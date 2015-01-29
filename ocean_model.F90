#undef DEBUG
!!!#define GDCHK .FALSE.
#define GDCHK mpp_pe().EQ.449

module ocean_model_mod

!-----------------------------------------------------------------------

use time_manager_mod, only: time_type, operator(+), operator(>), &
                            get_date, set_time

use          fms_mod, only: file_exist, open_restart_file, &
                            close_file, mpp_pe, mpp_root_pe, mpp_npes,         &
                            write_version_number, stdlog, error_mesg, WARNING, FATAL, &
                            check_nml_error, write_data, set_domain, NOTE, &
                            field_exist, get_mosaic_tile_grid, read_data,  &
                            field_size

#ifdef INTERNAL_FILE_NML
use          mpp_mod, only: input_nml_file
#else
use          fms_mod, only: open_namelist_file
#endif

use  fms_io_mod,      only: get_restart_io_mode, register_restart_field, restart_file_type, save_restart

use  amip_interp_mod, only: amip_interp_type, amip_interp_new,  &
                            amip_interp_del, get_amip_sst

use  mpp_domains_mod, only: domain1d, domain2d, mpp_define_domains,  &
                            mpp_get_compute_domain, mpp_get_compute_domains,  &
                            mpp_get_domain_components, mpp_get_pelist,  &
                            CYCLIC_GLOBAL_DOMAIN, mpp_define_layout
use          mpp_mod, only: mpp_npes, mpp_pe, mpp_root_pe, stdout,mpp_error, mpp_chksum

use    constants_mod, only: PI, RADIUS

use mosaic_mod,       only: get_mosaic_ntiles, get_mosaic_grid_sizes
use mosaic_mod,       only: calc_mosaic_grid_area
use grid_mod,         only: get_grid_comp_area, get_grid_size, get_grid_cell_vertices
use grid_mod,         only: get_grid_cell_centers

use coupler_types_mod,only: coupler_2d_bc_type

#ifdef SCM
use     scm_forc_mod, only: do_specified_tskin, TSKIN
#endif

implicit none
private

!-----------------------------------------------------------------------
!----------------- public interfaces -----------------------------------

public :: ocean_model_init, ocean_model_end, update_ocean_model, ocean_public_type, &
          ice_ocean_boundary_type, &
          ocean_model_flux_init, ocean_model_init_sfc, ocean_stock_pe, ocean_model_restart
public :: ice_ocn_bnd_type_chksum, ocean_public_type_chksum

public    ocean_model_data_get
interface ocean_model_data_get
   module procedure ocean_model_data1D_get 
   module procedure ocean_model_data2D_get 
end interface

!
! the following type is for data exchange with the new coupler
! it is defined here but declared in coupler_main and allocated in flux_init
!
type ice_ocean_boundary_type
  real, dimension(:,:), pointer :: u_flux =>NULL(), &
                                   v_flux =>NULL(), &
                                   t_flux =>NULL(), &
                                   q_flux =>NULL(), &
                                   salt_flux =>NULL(), &
                                   lw_flux =>NULL(), &
                                   sw_flux_vis_dir =>NULL(), &
                                   sw_flux_vis_dif =>NULL(), &
                                   sw_flux_nir_dir =>NULL(), &
                                   sw_flux_nir_dif =>NULL(), &
                                   lprec =>NULL(), &
                                   fprec  =>NULL()
  real, dimension(:,:), pointer :: runoff =>NULL(), &
                                   calving  =>NULL(), &
                                   runoff_hflx  =>NULL(), &
                                   calving_hflx  =>NULL()
  real, dimension(:,:), pointer :: p  =>NULL()
  ! "data" is collective field for "named" fields above
  real, dimension(:,:,:), pointer :: data  =>NULL()
  integer :: xtype             !REGRID, REDIST or DIRECT used by coupler
  type(coupler_2d_bc_type)      :: fluxes  ! array of fields used for additional tracers
  real, pointer, dimension(:,:) :: mi              =>NULL() ! mass of overlying sea ice 
end type ice_ocean_boundary_type

!-----------------------------------------------------------------------


!   lon_bnd    = longitude boundaries for grid boxes
!   lat_bnd    = latitude  boundaries for grid boxes
!   mask       = land-sea mask for grid boxes
!
!    note: longitude/latitude is in radians
!          mask is true for ocean points
!
!-----------------------------------------------------------------------

type ocean_public_type
   type (domain2d)               :: Domain
   real, pointer, dimension(:,:) :: lon =>NULL(), lat =>NULL()
   real, pointer, dimension(:)   :: lon_bnd =>NULL(), lat_bnd =>NULL()
   logical, pointer, dimension(:,:) :: mask  =>NULL()
   real, pointer, dimension(:,:) :: t_surf =>NULL() , &
                                    frazil =>NULL() ,  &
                                    u_surf =>NULL() , &
                                    v_surf =>NULL() , &
                                    s_surf =>NULL() , &
                                    area   =>NULL() , &
                                    sea_lev =>NULL()
   logical, pointer, dimension(:,:) :: maskmap =>NULL()! A pointer to an array indicating which
                                                       ! logical processors are actually used for
                                                       ! the ocean code. The other logical
                                                       ! processors would be all land points and
                                                       ! are not assigned to actual processors.
                                                       ! This need not be assigned if all logical
                                                       ! processors are used. This variable is dummy and need 
                                                       ! not to be set, but it is needed to pass compilation.
   type (time_type)              :: Time, &
                                    Time_step
   logical :: is_ocean_pe
   integer, pointer :: pelist(:) =>NULL()
   integer, dimension(3)            :: axes    
   type(coupler_2d_bc_type)         :: fields  ! array of fields used for additional tracers
end type ocean_public_type

!  t_surf      = surface temperature on the local ocean model grid
!  frazil      = frazil on the local ocean model grid
!  u_surf      = zonal ocean current on the local ocean model grid
!  v_surf      = meridional ocean current on the local ocean model grid
!
!  Time      = current ocean model time
!  Time_step = ocean model time step
!
!-----------------------------------------------------------------------

  type, public ::  ocean_state_type; private
     ! This type is private, and can therefore vary between different ocean models.
     ! All information entire ocean state may be contained here, although it is not
     ! necessary that this is implemented with all models.
     logical       :: is_ocean_pe = .false.       ! .true. on processors that run the ocean model.
  end type ocean_state_type

!------- namelist ---------
   logical :: do_netcdf_restart = .true.
   logical :: use_climo_sst  = .false.
   logical :: use_annual_sst = .false.
   integer, dimension(2) :: layout = (/ 0, 0 /)
   character(len=64) :: interp_method  = "conservative" ! default, conservative scheme

!  layout =  domain decomposition (# X-PEs by # Y-PEs)
!             layout = (/0,0/) will use default rules
!
   namelist /ocean_model_nml/ do_netcdf_restart, use_climo_sst, use_annual_sst, layout, interp_method

!-----------------------------------------------------------------------
!--------------------- private below here ------------------------------

!  ---- version number -----

   character(len=128) :: version = '$Id: ocean_model.F90,v 20.0.2.1 2014/05/21 21:17:32 Lucas.Harris Exp $'
   character(len=128) :: tagname = '$Name: lin_201408_lmh $'

!-----------------------------------------------------------------------
!------ model resolution parameters ------

   type (amip_interp_type), save :: Amip

   logical :: module_is_initialized=.false.
   logical :: stock_warning_issued =.false.

contains

!#######################################################################

 !#######################################################################
! <SUBROUTINE NAME="update_ocean_model">
!
! <DESCRIPTION>
! Update in time the ocean model fields. 
!   This subroutine uses the forcing in Ice_ocean_boundary to advance the
! ocean model's state from the input value of Ocean_state (which must be for
! time time_start_update) for a time interval of Ocean_coupling_time_step,
! returning the publicly visible ocean surface properties in Ocean_sfc and
! storing the new ocean properties in Ocean_state.
!
! Arguments: 
!  Ice_ocean_boundary - A structure containing the various forcing
!                                 fields coming from the ice. It is intent in.
!  Ocean_state - A structure containing the internal ocean state.
!  Ocean_sfc - A structure containing all the publicly visible ocean
!                        surface fields after a coupling time step.
!  time_start_update - The time at the beginning of the update step.
!  Ocean_coupling_time_step - The amount of time over which to advance
!                                       the ocean.

! Note: although several types are declared intent(inout), this is to allow for
!   the possibility of halo updates and to keep previously allocated memory.
!   In practice, Ice_ocean_boundary is intent in, Ocean_state is private to
!   this module and intent inout, and Ocean_sfc is intent out.
! </DESCRIPTION>
!
  subroutine update_ocean_model(Ice_ocean_boundary, Ocean_state, Ocean_sfc, &
       time_start_update, Ocean_coupling_time_step)
    type(ice_ocean_boundary_type), intent(inout) :: Ice_ocean_boundary
    type(ocean_state_type),        pointer       :: Ocean_state
    type(ocean_public_type),       intent(inout) :: Ocean_sfc
    type(time_type), intent(in)                  :: time_start_update
    type(time_type), intent(in)                  :: Ocean_coupling_time_step
    
    integer :: num_ocean_calls, no
    
    return    ! sit

 !check if required boundary fields have been initialized
      if ( .NOT.ASSOCIATED(Ice_ocean_boundary%sw_flux_vis_dir) .OR. &
        .NOT.ASSOCIATED(Ice_ocean_boundary%sw_flux_vis_dif) .OR. &
        .NOT.ASSOCIATED(Ice_ocean_boundary%sw_flux_nir_dir) .OR. &
        .NOT.ASSOCIATED(Ice_ocean_boundary%sw_flux_nir_dif) .OR. &
        .NOT.ASSOCIATED(Ice_ocean_boundary%lw_flux) .OR. &
        .NOT.ASSOCIATED(Ice_ocean_boundary%fprec)   .OR. &
        .NOT.ASSOCIATED(Ice_ocean_boundary%calving) .OR. &
        .NOT.ASSOCIATED(Ice_ocean_boundary%t_flux)  .OR. &
        .NOT.ASSOCIATED(Ice_ocean_boundary%q_flux) ) &
          call error_mesg( 'Update_ocean_model', &
           'Ice_ocean_boundary not correctly initialized.', FATAL )

!-----------------------------------------------------------------------
! ---- update time ----

      Ocean_sfc%Time = Ocean_sfc%Time + Ocean_coupling_time_step
!-----------------------------------------------------------------------
!----- update sst, set currents -----

      call set_ocean_model_state (Ocean_sfc)

!-----------------------------------------------------------------------

 end subroutine update_ocean_model

!#######################################################################

 subroutine set_ocean_model_state (Ocean_sfc)

   type (ocean_public_type), intent(inout) :: Ocean_sfc

!-----------------------------------------------------------------------
!----- get new sea surface temperature ------

! modified by JHC
        call get_amip_sst ( Ocean_sfc%Time, Amip, Ocean_sfc%t_surf, Ocean_sfc%lon, Ocean_sfc%lat )
!       call get_amip_sst ( Ocean_sfc%Time, Amip, Ocean_sfc%t_surf )

#ifdef SCM
!--- for single column model -------------------------------------!
!--- initialize surface temperature to observed value ------------!  
       if (do_specified_tskin) then     
          Ocean_sfc%t_surf = TSKIN
       end if
!-----------------------------------------------------------------!
#endif

!-----------------------------------------------------------------------
!----- currents -----

   Ocean_sfc%u_surf = 0.0
   Ocean_sfc%v_surf = 0.0

!----- dummy out frazil ??? -----

   Ocean_sfc%frazil = 0.0
   Ocean_sfc%area   = 1.0

!-----------------------------------------------------------------------

 end subroutine set_ocean_model_state

!#######################################################################

subroutine ocean_model_init (Ocean, Ocean_state, Time_init, Time)
!
! <DESCRIPTION>
! Initialize the ocean model. 
! Arguments: 
!  Ocean (inout)  - A structure containing various publicly visible ocean
!                    surface properties after initialization.
!  Ocean_state (pointer)- A structure whose internal contents are private
!                    to ocean_model_mod that may be used to contain all
!                    information about the ocean's interior state.
!  Time_init (in) - The start time for the coupled model's calendar.
!  Time_in   (in) - The time at which to initialize the ocean model.
! </DESCRIPTION>

  type (ocean_public_type), intent(inout) :: Ocean
  type (ocean_state_type),  pointer       :: Ocean_state
  type (time_type),         intent(in)    :: Time_init, Time

  integer                              :: siz(4)
  integer                              :: unit, ierr, io, nlon, nlat, logunit
  integer                              :: isd, ied, jsd, jed
  integer                              :: i, j, npes
  integer, allocatable, dimension(:)   :: xextent, yextent
  real,    allocatable, dimension(:,:) :: rmask
  real,    allocatable, dimension(:,:) :: area, lonv, latv
  real,    allocatable, dimension(:)   :: glon, glat, glon_bnd, glat_bnd
  character(len=256)                   :: err_mesg
  character(len=80)                    :: domainname
  character(len=256)                   :: grid_file = "INPUT/grid_spec.nc"
  character(len=256)                   :: ocean_mosaic, tile_file
  integer                              :: n, m, ntiles, outunit
  integer                              :: grid_version
  integer, parameter                   :: VERSION_0 = 0  ! grid file with field geolon_t
  integer, parameter                   :: VERSION_1 = 1  ! grid file with field x_T
  integer, parameter                   :: VERSION_2 = 2  ! mosaic file
  integer                              :: id_restart
  
  !!! return                     !!! sit. It can be turnoff in later times, when SIT is well tested.

  if (GDCHK) print *, "I am in ocean_model_init 1.0." 

   if(module_is_initialized) return

  if (GDCHK) print *, "I am in ocean_model_init 2.0." 

     Ocean%Time      = Time

!   ----- read namelist -----

     if ( file_exist('input.nml')) then
#ifdef INTERNAL_FILE_NML
        read (input_nml_file, nml=ocean_model_nml, iostat=io)
        ierr = check_nml_error(io, 'ocean_model_nml')
#else
        unit = open_namelist_file ( )
        ierr=1
         do while (ierr /= 0)
           read  (unit, nml=ocean_model_nml, iostat=io, end=10)
           ierr = check_nml_error(io,'ocean_model_nml')
        enddo
 10     call close_file (unit)
#endif
     endif
     call get_restart_io_mode(do_netcdf_restart)

!   ----- write version number and namelist -----
     call write_version_number(version, tagname)

     if ( mpp_pe() == mpp_root_pe() ) then
          logunit = stdlog()
          write (logunit,nml=ocean_model_nml)
     endif

    !--- get the grid size 
    call get_grid_size('OCN',1,nlon,nlat)

!--- domain decompsition -----------------------------------------------

!---- domain decomposition ----

    if( layout(1).EQ.0 .AND. layout(2).EQ.0 ) &
         call mpp_define_layout( (/1,nlon,1,nlat/), mpp_npes(), layout )
    if( layout(1).NE.0 .AND. layout(2).EQ.0 )layout(2) = mpp_npes()/layout(1)
    if( layout(1).EQ.0 .AND. layout(2).NE.0 )layout(1) = mpp_npes()/layout(2)

    domainname = 'AMIP Ocean'
    call mpp_define_domains ( (/1,nlon,1,nlat/), layout, Ocean%Domain, name=domainname )
    call mpp_get_compute_domain ( Ocean%Domain, isd, ied, jsd, jed )

    call set_domain(Ocean%domain)

    !----- grid information -----
    allocate ( glon_bnd (nlon+1), glat_bnd (nlat+1)  )
    allocate ( glon     (nlon  ), glat     (nlat  )  )
    allocate ( lonv (isd:ied+1, jsd:jed+1) )
    allocate ( latv (isd:ied+1, jsd:jed+1) )
    allocate(rmask(isd:ied,jsd:jed))
    allocate ( Ocean%lon_bnd (isd:ied+1),      &
               Ocean%lat_bnd (jsd:jed+1),      &
               Ocean%lon (isd:ied, jsd:jed),   &
               Ocean%lat (isd:ied, jsd:jed),   &
               Ocean%mask    (isd:ied,jsd:jed) )

!  ---- set up local grid -----
   call get_grid_cell_vertices('OCN', 1, glon_bnd, glat_bnd)
   call get_grid_cell_centers ('OCN', 1, glon, glat)
   call get_grid_cell_vertices('OCN', 1, lonv, latv, Ocean%domain)
   call get_grid_cell_centers ('OCN', 1, Ocean%lon, Ocean%lat, Ocean%domain)
   !--- for conservation interpolation, the grid should be rectangular ----
   if(trim(interp_method) == "conservative" ) then
      err_mesg = 'Bilinear interpolation must be used for a tripolar grid'
      do i=isd, ied
         if(any(glon(i) /= Ocean%lon(i,:)))  &
              call error_mesg ('ocean_model_init',err_mesg,FATAL)
      enddo
      do j=jsd,jed
         if(any(glat(j) /= Ocean%lat(:,j)))  &
              call error_mesg ('ocean_model_init',err_mesg,FATAL)
      enddo
   endif
   Ocean%lon = Ocean%lon*pi/180.
   Ocean%lat = Ocean%lat*pi/180.
   Ocean%lon_bnd = glon_bnd(isd:ied+1)*pi/180.
   Ocean%lat_bnd = glat_bnd(jsd:jed+1)*pi/180.

   !   ------ define Data masks ------
   if(field_exist(grid_file, 'wet')) then
      call read_data(grid_file, "wet",      rmask,     Ocean%Domain)
   else if(field_exist(grid_file, 'ocn_mosaic_file') ) then ! read from mosaic file
      call read_data(grid_file, "ocn_mosaic_file", ocean_mosaic)
      ocean_mosaic = "INPUT/"//trim(ocean_mosaic)
      ntiles = get_mosaic_ntiles(ocean_mosaic)
      if(ntiles .NE. 1) call error_mesg('ocean_model_init', ' ntiles should be 1 for ocean mosaic, contact developer', FATAL)
      allocate(area(isd:ied,jsd:jed))
      call calc_mosaic_grid_area(lonv*pi/180., latv*pi/180., area )
      rmask = 0.0
      call get_grid_comp_area('OCN', 1, rmask, domain=Ocean%Domain)
      rmask = rmask/area
      deallocate(area)
   else
      call error_mesg('ocean_model_init','wet and ocn_mosaic_file does not exist in file '//trim(grid_file), FATAL )
   end if

   Ocean%mask = .false.
   where (rmask > 0) Ocean%mask = .true.

   deallocate(rmask, lonv, latv, glon, glat, glon_bnd, glat_bnd )

  if (GDCHK) print *, "I am in ocean_model_init 3.0." 

!----- write (to standard output?) domain decomposition -----

     if (allocated(xextent))  deallocate ( xextent, yextent )

     if ( mpp_pe() == mpp_root_pe() ) then
          allocate ( xextent(layout(1)), yextent(layout(2)) )
          call compute_extent ( Ocean%Domain, layout, xextent, yextent )
          outunit = stdout()
          write (outunit,100)
          write (outunit,110) xextent
          write (outunit,120) yextent
      100 format ('OCEAN DATA DOMAIN DECOMPOSITION')
      110 format ('  X-AXIS = ',24i4,/,(11x,24i4))
      120 format ('  Y-AXIS = ',24i4,/,(11x,24i4))
          deallocate ( xextent, yextent )
     endif

!------------ done domain decomposition --------------------------------
!=======================================================================
!-----------------------------------------------------------------------

     allocate ( Ocean%t_surf (isd:ied,jsd:jed), &
          Ocean%u_surf (isd:ied,jsd:jed), &
          Ocean%v_surf (isd:ied,jsd:jed), &
          Ocean%frazil (isd:ied,jsd:jed), &
          Ocean%area   (isd:ied,jsd:jed), &
          Ocean%s_surf (isd:ied,jsd:jed), &
          Ocean%sea_lev(isd:ied,jsd:jed))

     Ocean%s_surf  = 0.0
     Ocean%sea_lev = 0.0
     !-----------------------------------------------------------------------

     !---- maybe this should be on restart? -----
     Ocean%frazil = 0.0
     Ocean%area   = 1.0

  if (GDCHK) print *, "I am in ocean_model_init 4.0: set_ocean_model_state" 

!  ---- initialize other modules ----

   if(trim(interp_method) == "conservative") then
      Amip = amip_interp_new ( Ocean%lon_bnd,            &
                               Ocean%lat_bnd,            &
                               Ocean%mask,               &
                               interp_method = interp_method, &
                               use_climo=use_climo_sst,       &
                               use_annual=use_annual_sst )
   else if(trim(interp_method) == "bilinear") then
      Amip = amip_interp_new ( Ocean%lon,                &
                               Ocean%lat,                &
                               Ocean%mask,               &
                               interp_method = interp_method, &
                               use_climo=use_climo_sst,       &
                               use_annual=use_annual_sst )
   else
      call error_mesg('ice_model_init', 'interp_method should be conservative or bilinear', &
                      FATAL)
   endif
!!! The calculations are in set_ocean_model_state   
!!! bjt >>> 
!!!! modified by JHC
!!!   call get_amip_sst ( Ocean%Time, Amip, Ocean%t_surf, Ocean%lon, Ocean%lat )
!!!!   call get_amip_sst ( Ocean%Time, Amip, Ocean%t_surf )
!!!
!!!#ifdef SCM
!!!!--- for single column model -------------------------------------!
!!!!--- initialize surface temperature to observed value ------------!  
!!!       if (do_specified_tskin) then     
!!!          Ocean%t_surf = TSKIN
!!!       end if
!!!!-----------------------------------------------------------------!
!!!#endif
!!! <<< bjt
!-----------------------------------------------------------------------
!----- set the initial state -------------

   call set_ocean_model_state (Ocean)

  if (GDCHK) print *, "I am in ocean_model_init 5.0." 

!-----------------------------------------------------------------------
   allocate(Ocean_state)
!------------------------------------------

   module_is_initialized = .true.

!-----------------------------------------------------------------------

 end subroutine ocean_model_init

!#######################################################################

 subroutine ocean_model_end(Ocean_sfc, Ocean_state, Time)
  type(ocean_public_type),           intent(inout) :: Ocean_sfc
  type(ocean_state_type),            pointer       :: Ocean_state
  type(time_type),                   intent(in)    :: Time
!   This subroutine terminates the model run, saving the ocean state in a
! restart file and deallocating any data associated with the ocean.

! Arguments: Ocean_sfc - An ocean_public_type structure that is to be
!                        deallocated upon termination.
!  (inout)   Ocean_state - A pointer to the structure containing the internal
!                          ocean state to be deallocated upon termination.
!  (in)      Time - The model time, used for writing restarts.

!!! return   !!! sit. It can be turnoff in later times, when SIT is well tested.

   if(.not.module_is_initialized) return

  call amip_interp_del(Amip)
  module_is_initialized = .false.
     
 end subroutine ocean_model_end

!#######################################################################
! <SUBROUTINE NAME="ocean_model_restart">
!
! <DESCRIPTION>
! dummy interface.
! Arguments: 
!   timestamp (optional, intent(in)) : A character string that represents the model time, 
!                                      used for writing restart. timestamp will append to
!                                      the any restart file name as a prefix. 
! </DESCRIPTION>
!
  subroutine ocean_model_restart(Ocean_state, timestamp)
     type(ocean_state_type),    pointer     :: Ocean_state
     character(len=*), intent(in), optional :: timestamp

     !--- do thing, there is no need for restart file.


  end subroutine ocean_model_restart
! </SUBROUTINE> NAME="ocean_model_restart"

!#######################################################################
! dummy interface for ESM coupler
subroutine ocean_model_init_sfc(Ocean_state, Ocean)

type(ocean_state_type), pointer          :: Ocean_state
type(ocean_public_type), intent(in)      :: Ocean

return
end subroutine ocean_model_init_sfc

!#######################################################################
subroutine ocean_model_flux_init(Ocean_state)
type(ocean_state_type), pointer       :: Ocean_state

return
end subroutine ocean_model_flux_init
!#######################################################################

 subroutine compute_extent (Domain, layout, xsizelist, ysizelist) 
 type (domain2D), intent(in) :: Domain
 integer, intent(in) :: layout(2)
 integer, intent(out), optional :: xsizelist(:), ysizelist(:)
 integer, dimension(0:layout(1)*layout(2)-1) :: xsize, ysize
 integer :: i, j, xlist(layout(1)), ylist(layout(2))
 type (domain1D) :: Xdom, Ydom

   call mpp_get_compute_domains   ( Domain, xsize=xsize, ysize=ysize )
   call mpp_get_domain_components ( Domain, Xdom, Ydom )
   call mpp_get_pelist ( Xdom, xlist ) 
   call mpp_get_pelist ( Ydom, ylist ) 

     do i = 1, layout(1)
       xsizelist(i) = xsize(xlist(i)-mpp_root_pe())
     enddo

     do j = 1, layout(2)
       ysizelist(j) = ysize(ylist(j)-mpp_root_pe())
     enddo

 end subroutine compute_extent

!#######################################################################
! dummy routine

!
subroutine ocean_stock_pe(Ocean_state, index, value, time_index)
  type(ocean_state_type),pointer     :: Ocean_state
  integer,               intent(in)  :: index
  real,                  intent(out) :: value
  integer, optional,     intent(in)  :: time_index

  value = 0.0
  if (.not.associated(Ocean_state)) return
  if (.not.Ocean_state%is_ocean_pe) return
  if(.not.stock_warning_issued) then
     call error_mesg('ocean_stock_pe','Stocks not yet implemented. Returning zero.',NOTE)
     stock_warning_issued = .true.
  endif

  end subroutine ocean_stock_pe


subroutine ocean_model_data2D_get(OS,Ocean, name, array2D,isc,jsc)
  type(ocean_state_type),     pointer    :: OS
  type(ocean_public_type),    intent(in) :: Ocean
  character(len=*)          , intent(in) :: name
  real, dimension(isc:,jsc:), intent(out):: array2D
  integer                   , intent(in) :: isc,jsc
  
  array2D(isc:,jsc:) = 0.0
  
end subroutine ocean_model_data2D_get

subroutine ocean_model_data1D_get(OS,Ocean, name, value)
  type(ocean_state_type),     pointer    :: OS
  type(ocean_public_type),    intent(in) :: Ocean
  character(len=*)          , intent(in) :: name
  real                      , intent(out):: value

  value = 0.0

end subroutine ocean_model_data1D_get


!#######################################################################

subroutine ice_ocn_bnd_type_chksum(id, timestep, iobt)

    character(len=*), intent(in) :: id
    integer         , intent(in) :: timestep
    type(ice_ocean_boundary_type), intent(in) :: iobt
 integer ::   n,m, outunit
    
    outunit = stdout()

    write(outunit,*) 'BEGIN CHECKSUM(atmos_land_boundary_type):: ', id, timestep
    write(outunit,100) 'iobt%u_flux         ', mpp_chksum( iobt%u_flux         )
    write(outunit,100) 'iobt%v_flux         ', mpp_chksum( iobt%v_flux         )
    write(outunit,100) 'iobt%t_flux         ', mpp_chksum( iobt%t_flux         )
    write(outunit,100) 'iobt%q_flux         ', mpp_chksum( iobt%q_flux         )
    write(outunit,100) 'iobt%salt_flux      ', mpp_chksum( iobt%salt_flux      )
    write(outunit,100) 'iobt%lw_flux        ', mpp_chksum( iobt%lw_flux        )
    write(outunit,100) 'iobt%sw_flux_vis_dir', mpp_chksum( iobt%sw_flux_vis_dir)
    write(outunit,100) 'iobt%sw_flux_vis_dif', mpp_chksum( iobt%sw_flux_vis_dif)
    write(outunit,100) 'iobt%sw_flux_nir_dir', mpp_chksum( iobt%sw_flux_nir_dir)
    write(outunit,100) 'iobt%sw_flux_nir_dif', mpp_chksum( iobt%sw_flux_nir_dif)
    write(outunit,100) 'iobt%lprec          ', mpp_chksum( iobt%lprec          )
    write(outunit,100) 'iobt%fprec          ', mpp_chksum( iobt%fprec          )
    write(outunit,100) 'iobt%runoff         ', mpp_chksum( iobt%runoff         )
    write(outunit,100) 'iobt%calving        ', mpp_chksum( iobt%calving        )
    write(outunit,100) 'iobt%p              ', mpp_chksum( iobt%p              )

100 FORMAT("CHECKSUM::",A32," = ",Z20)
    do n = 1, iobt%fluxes%num_bcs  !{
       do m = 1, iobt%fluxes%bc(n)%num_fields  !{
          write(outunit,101) 'iobt%',trim(iobt%fluxes%bc(n)%name), &
               trim(iobt%fluxes%bc(n)%field(m)%name), &
               mpp_chksum(iobt%fluxes%bc(n)%field(m)%values)
       enddo  !} m
    enddo  !} n
101 FORMAT("CHECKSUM::",A16,a,'%',a," = ",Z20)

end subroutine ice_ocn_bnd_type_chksum

subroutine ocean_public_type_chksum(id, timestep, ocn)

    character(len=*), intent(in) :: id
    integer         , intent(in) :: timestep
    type(ocean_public_type), intent(in) :: ocn
 integer ::   n,m, outunit
    
    outunit = stdout()

    write(outunit,*) 'BEGIN CHECKSUM(ocean_type):: ', id, timestep
    write(outunit,100) 'ocean%t_surf   ',mpp_chksum(ocn%t_surf )
    write(outunit,100) 'ocean%s_surf   ',mpp_chksum(ocn%s_surf )
    write(outunit,100) 'ocean%u_surf   ',mpp_chksum(ocn%u_surf )
    write(outunit,100) 'ocean%v_surf   ',mpp_chksum(ocn%v_surf )
    write(outunit,100) 'ocean%sea_lev  ',mpp_chksum(ocn%sea_lev)
    write(outunit,100) 'ocean%frazil   ',mpp_chksum(ocn%frazil )

    do n = 1, ocn%fields%num_bcs  !{
       do m = 1, ocn%fields%bc(n)%num_fields  !{
          write(outunit,101) 'ocean%',trim(ocn%fields%bc(n)%name), &
               trim(ocn%fields%bc(n)%field(m)%name), &
               mpp_chksum(ocn%fields%bc(n)%field(m)%values)
       enddo  !} m
    enddo  !} n
101 FORMAT("CHECKSUM::",A16,a,'%',a," = ",Z20)


100 FORMAT("CHECKSUM::",A32," = ",Z20)
end subroutine ocean_public_type_chksum

end module ocean_model_mod
