#define GDCHK0 (.false.)
!!#define GDCHK1 (mpp_pe().EQ.819)
!!#define GDCHK1 (mpp_pe().EQ.98)
#define GDCHK1 (.false.)

                    module woa_mod
! <CONTACT EMAIL="btsuang@gmail.com">
!  fil
! </CONTACT>
! <REVIEWER EMAIL="Stuart.Freidenreich@noaa.gov">
!  smf
! </REVIEWER>
! <HISTORY SRC="http://www.gfdl.noaa.gov/fms-cgi-bin/cvsweb.cgi/FMS/"/>
! <OVERVIEW>
!  Code to initialize/allocate woa climatology
! </OVERVIEW>
! <DESCRIPTION>
!  This code initializes prescribed woa climatology from input file,
!  allocates necessary memory space and interpolate the woa climatology
!  to the model specification. Afterwards the memory space is deallocated,
!  the woa climatology information is freed.
!  This code supplies T, S, u and v data of world ocean altas (WOA) dataset to the 
!  sit_vdiff ice/ocean_package. This code is modifed from ozone.F90. The original author is "Fei.Liu@noaa.gov" and
!  the reviewer is "Dan.Schwarzkopf@noaa.gov". Recent changes allow provision for predicted woa to be considered. 

! </DESCRIPTION>
!  shared modules:

use time_manager_mod,  only: time_type, time_manager_init, operator(+),&
                             set_date, operator(-), print_date, &
                             assignment(=), &
                             set_time, days_in_month, get_date, &
                             operator(>), operator(/=)
use diag_manager_mod,  only: diag_manager_init, get_base_time, &
                             send_data, register_diag_field,  &
                             register_static_field
use field_manager_mod, only: MODEL_ATMOS
use tracer_manager_mod,only: get_tracer_index,   &
                             get_tracer_names,   &
                             get_tracer_indices, &
                             get_number_tracers, &
                             MAX_TRACER_FIELDS,  &
                             query_method
use mpp_mod,           only: input_nml_file
use fms_mod,           only: open_namelist_file, fms_init, &
                             mpp_pe, mpp_root_pe, stdlog, &
                             file_exist, write_version_number, &
                             check_nml_error, error_mesg, &
                             FATAL, NOTE, WARNING, close_file, &
                             lowercase
use interpolator_mod,  only: interpolate_type, interpolator_init, &
                             interpolator, interpolator_end, &
                             obtain_interpolator_time_slices, &    
                             unset_interpolator_time_flag, &
                             CONSTANT, INTERP_WEIGHTED_P
use mpp_io_mod,        only: mpp_open, mpp_close, MPP_RDONLY,   &
                             MPP_ASCII, MPP_SEQUENTIAL, MPP_MULTI,  &
                             MPP_SINGLE, mpp_io_init
use constants_mod,     only: constants_init, RADIAN, GRAV, TFREEZE, xmissing
use data_override_mod, only: data_override

!  shared radiation package modules:

!!! bjt use   rad_utilities_mod, only  : woa_type, rad_utilities_init, &
!!! bjt                                  get_radiative_param,              &
!!! bjt                                  atmos_input_type     

!---------------------------------------------------------------------

implicit none
private

!---------------------------------------------------------------------
!    woa_mod provides woa information that is needed by a
!    model physics package. the initial use of woa_mod was to
!    provide climatological woa fields to the model radiation 
!    package for use in calculating radiative fluxes and heating rates; 
!    with the introduction of predicted woas as members of the 
!    model's tracer array, woa_mod became the mechanism to collect
!    and bundle those tracers which were to be seen as woa by the
!    radiation code. the introduction of the treatment of woa 
!    impacts on cloud properties (woa indirect effect) required that
!    woa_mod be modified to provide needed woa information to 
!    routines involved with cloud calculation.
!---------------------------------------------------------------------


!---------------------------------------------------------------------
!----------- version number for this module -------------------

character(len=128) :: version = '$Id: woa.F90,v 20.0 2013/12/13 23:18:53 fms Exp $'
character(len=128) :: tagname = '$Name: tikal_201403 $'


!-----------------------------------------------------------------------
!------  interfaces -------

public          &
        woa_init, woa_driver, woa_end, &
        woa_time_vary, woa_endts, woa_dealloc, &
        nfields, woa_time, Model_init_time, woa_column_time, &
        woa_type, do_column_woa, do_predicted_woa, do_specified_woa, &
        column_woa_driver, predicted_woa_driver, specified_woa_driver

         
!private         &


!---------------------------------------------------------------------
!------  namelist ------

 character(len=32)      ::      &
         woa_data_source = 'climatology'
                                   ! source of woa data, either
                                   ! 'climatology' file (default) or
                                   ! single column 'input' file or
                                   ! calculate a column for location and
                                   ! time specified ('calculate_column')
                                   ! or 'predicted' (calculated online)
integer, parameter     ::      &
        MAX_DATA_FIELDS = 100     ! maximum number of woa variables
integer, parameter     ::      &
        MAX_WOA_FAMILIES = 12 ! maximum number of woa families
character(len=64) :: interp_method  = 'bilinear' ! default bilinear scheme
character(len=64)      ::      &
        data_names(MAX_DATA_FIELDS) = '  ' 
                                  ! names of active woa variables 
character(len=64)      ::      &
        data_units(MAX_DATA_FIELDS) = '  ' 
                                  ! units of active woa variables from nml
character(len=64)      ::      &
        clim_units(MAX_DATA_FIELDS) = '  ' 
                                  ! units of active woa variables from netcdf
character(len=64)      ::      &
        filename = '  '           ! name of netcdf file containing 

                                  ! woa variables to be activated
character(len=64)      ::      &
      family_names(MAX_WOA_FAMILIES) = '  ' 
                                  ! names of active woa families 
logical, dimension(MAX_DATA_FIELDS) :: in_family1 = .false.
                                  ! woa n is in family 1 ?
logical, dimension(MAX_DATA_FIELDS) :: in_family2 = .false.
                                  ! woa n is in family 2 ?
logical, dimension(MAX_DATA_FIELDS) :: in_family3 = .false.
                                  ! woa n is in family 3 ?
logical,dimension(MAX_DATA_FIELDS) :: in_family4 = .false.
                                  ! woa n is in family 4 ?
logical, dimension(MAX_DATA_FIELDS) :: in_family5 = .false.
                                  ! woa n is in family 5 ?
logical, dimension(MAX_DATA_FIELDS) :: in_family6 = .false.
                                  ! woa n is in family 6 ?
logical, dimension(MAX_DATA_FIELDS) :: in_family7 = .false.
                                  ! woa n is in family 7 ?
logical, dimension(MAX_DATA_FIELDS) :: in_family8 = .false.
                                  ! woa n is in family 8 ?
logical, dimension(MAX_DATA_FIELDS) :: in_family9 = .false.
                                  ! woa n is in family 9 ?
logical, dimension(MAX_DATA_FIELDS) :: in_family10 = .false.
                                  ! woa n is in family 10 ?
logical, dimension(MAX_DATA_FIELDS) :: in_family11 = .false.
                                  ! woa n is in family 11 ?
logical, dimension(MAX_DATA_FIELDS) :: in_family12 = .false.
                                  ! woa n is in family 12 ?
logical         :: use_woa_timeseries = .false.
                                  ! use a timeseries providing inter-
                                  ! annual woa variation ?
logical, dimension(MAX_DATA_FIELDS) :: time_varying_species = .true.
                                  ! this woa variables is 
                                  ! time-varying  ?
integer, dimension(6,MAX_DATA_FIELDS) :: woa_dataset_entry  =  1
                      ! time in woa data set corresponding to model
                      ! initial time  (yr, mo, dy, hr, mn, sc)
logical, dimension(MAX_WOA_FAMILIES) ::   &
                                    volc_in_fam_col_opt_depth = .false.
                      ! is the volcanic contribution to column optical
                      ! depth to be included for this family in the
                      ! netcdf output fields ?
real,dimension(2)   :: lonb_col = (/-999., -999./)
                      ! longitudes defining the region to use for column
                      ! data calculation
real,dimension(2)   :: latb_col = (/-999., -999./)
                      ! latitudes defining the region to use for column
                      ! data calculation
integer, dimension(6)  :: time_col = (/0,0,0,0,0,0/)
                      ! time to use for column data calculation


type woa_type
     !!! real, dimension(:,:,:,:),        allocatable :: data
     !!! real, dimension(:,:,:,:),        allocatable, target :: data
     !!! logical, dimension(:,:),        allocatable, target :: family_members
     !!! character(len=64), dimension(:), allocatable, target :: woa_names
     real, dimension(:,:,:,:),        pointer :: data
     logical, dimension(:,:),        pointer :: family_members
     character(len=64), dimension(:), pointer :: woa_names     
end type woa_type

namelist /woa_nml/         &
                           woa_data_source,   &
                           lonb_col, latb_col, time_col, &
                           interp_method, &
                           data_names, data_units, filename,  &
                           family_names,   &
                           use_woa_timeseries, &
                           time_varying_species,  &
                           woa_dataset_entry,  &               
                           in_family1, in_family2, in_family3, &
                           in_family4, in_family5, in_family6, &
                           in_family7, in_family8, in_family9, &
                           in_family10, in_family11, in_family12, &
                           volc_in_fam_col_opt_depth
                           

!---------------------------------------------------------------------
!---- public data ----


!---------------------------------------------------------------------
!---- private data ----


!-------------------------------------------------------------------
!    specified_woa contains the column input woa concentration
!    ratio (kg/m**2).  used when woa_data_source = 'input'.
!-------------------------------------------------------------------
real, dimension (:), allocatable     ::  specified_woa
 
!---------------------------------------------------------------------
!   the following is an interpolate_type variable containing the
!   information about the woa variables.
!---------------------------------------------------------------------
type(interpolate_type), dimension(:), allocatable  :: woa_interp

!--------------------------------------------------------------------
!    miscellaneous variables
!--------------------------------------------------------------------
integer  :: id                               ! number of grid points in 
                                             ! x direction (on processor)
integer  :: jd                               ! number of grid points in 
                                             ! y direction (on processor)
logical  :: make_separate_calls=.false.      ! woa interpolation
                                             ! to be done one at a 
                                             ! time
type(time_type), dimension(:), allocatable   ::    &
                   woa_time              ! time for which data is
                                             ! obtained from woa
                                             ! timeseries
logical  :: do_column_woa = .false.      ! using single column aero-
                                             ! sol data ?
logical  :: do_predicted_woa = .false.   ! using predicted woa fields?
logical  :: do_specified_woa = .false.   ! using specified woa fields 
                                             ! from a timeseries file?
integer  :: nfields=0                        ! number of active woa 
                                             ! species
integer  :: nfamilies=0                      ! number of active woa 
                                             ! families
logical  :: module_is_initialized = .false.  ! module has been 
                                             ! initialized  ?

type(time_type) :: Model_init_time  ! initial calendar time for model  
                                    ! [ time_type ]
type(time_type), dimension(:), allocatable ::   &
                   woa_offset   ! difference between model initial
                                    ! time and woa timeseries app-
                                    ! lied at model initial time
                                    ! [ time_type ]
type(time_type), dimension(:), allocatable ::   &
                   woa_entry    ! time in woa timeseries which
                                    ! is mapped to model initial time
                                    ! [ time_type ]
type(time_type)  ::   &
              woa_column_time   ! time for which woa data is
                                    ! extracted from woa timeseries
                                    ! in 'calculate_columns' case
                                    ! [ time_type ]
logical, dimension(:), allocatable    ::     &
                   negative_offset 
                                    ! the model initial time is later 
                                    ! than the woa_dataset_entry 
                                    ! time  ?
integer , dimension(:), allocatable :: data_out_of_bounds, vert_interp
logical, dimension(:), allocatable :: using_fixed_year_data 
                                    ! are we using a fixed year
                                    !  of data from a timeseries file ?
integer, dimension (MAX_DATA_FIELDS) :: woa_tracer_index
                                    ! tracer index for each of the 
                                    ! prognostic tracer to be seen as 
                                    ! woas by the radiation package
real, dimension (MAX_DATA_FIELDS)    :: woa_tracer_scale_factor
                                    ! scaling factor for each of the 
                                    ! prognostic tracer to be seen as 
                                    ! woas by the radiation package
character(len=32), dimension (:),  &
                     allocatable     ::  tracer_names
logical, dimension(:), allocatable   :: being_overridden
                                    ! is a given woa field to be over-
                                    ! ridden based on model data_table?
logical                              :: output_override_info = .true.
                                    ! should override info about each 
                                    ! woa field be output (will be set 
                                    ! to .false. after first time step)
integer                              :: override_counter = 0
                                    ! used to count calls to woa_endts
                                    ! so that output_override_info may be
                                    ! set .false. after physics_up.

#include <netcdf.inc>
 
!---------------------------------------------------------------------
!---------------------------------------------------------------------


                           contains


 
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
!                     PUBLIC SUBROUTINES
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

!---------------------------------------------------------------------
! <SUBROUTINE NAME="woa_init">
!  <OVERVIEW>
!   Subroutine to initialize/interpolate prescribed woa climatology
!  </OVERVIEW>
!  <DESCRIPTION>
!   Subroutine to initialize/interpolate prescribed woa climatology
!  </DESCRIPTION>
!  <TEMPLATE>
!   call woa_init(lonb, latb, woa_names)
!  </TEMPLATE>
!  <IN NAME="lonb" TYPE="real">
!   2d array of model longitudes on cell corners in [radians]
!  </IN>
!  <IN NAME="latb" TYPE="real">
!   2d array of model latitudes on cell corners in [radians]
!  </IN>
!  <IN NAME="woa_names" TYPE="character">
!   names of the activated woa variables
!  </IN>
! </SUBROUTINE>
!
subroutine woa_init (lonb, latb, woa_names,   &
                         woa_family_names)

!-----------------------------------------------------------------------
!    woa_init is the constructor for woa_mod.
!-----------------------------------------------------------------------

real, dimension(:,:),            intent(in)  :: lonb,latb
character(len=64), dimension(:), pointer     :: woa_names
character(len=64), dimension(:), pointer     :: woa_family_names

!----------------------------------------------------------------------
!  intent(in) variables:
!
!       lonb           2d array of model longitudes on cell corners 
!                      [ radians ]
!       latb           2d array of model latitudes at cell corners 
!                      [ radians ]
!
!   pointer variables:
!
!       woa_names  names of the activated woa variables
!       woa_family_names  names of the activated woa families
!
!---------------------------------------------------------------------

!----------------------------------------------------------------------
!  local variables:
      
      character(len=64)  :: data_names_predicted (MAX_DATA_FIELDS) = '  '
                              ! predicted woa names to be 
                              ! seen by radiation code
      logical :: flag,rad_forc_online, single_year_file
      character(len=80)       ::tr_rad_name, tr_clim_name
      character(len=80)       :: name,control
      real                    ::tr_rad_scale_factor
      integer   ::   unit, ierr, io, logunit
      integer   ::   ntrace
      integer   ::   n

!---------------------------------------------------------------------
!    local variables:
!
!         unit       io unit number used to read namelist file
!         ierr       error code
!         io         error status returned from io operation
!         n          do-loop index
!
!---------------------------------------------------------------------

  if (GDCHK0) print *, "I am in woa_init 1.0"

!---------------------------------------------------------------------
!    if routine has already been executed, exit.
!---------------------------------------------------------------------
      if (module_is_initialized) return
 
!---------------------------------------------------------------------
!    verify that modules used by this module that are not called later
!    have already been initialized.
!---------------------------------------------------------------------
      call mpp_io_init
      call fms_init
      call diag_manager_init
!!!bjt      call rad_utilities_init
      call time_manager_init
      call constants_init

 if (GDCHK0) print *, "I am in woa_init 2.0"

!-----------------------------------------------------------------------
!    read namelist.
!-----------------------------------------------------------------------
 if (.true.) then
#ifdef INTERNAL_FILE_NML
 if (GDCHK0) print *, "I am in woa_init 2.1"
      read (input_nml_file, nml=woa_nml, iostat=io)
      ierr = check_nml_error(io,'woa_nml')
#else
 if (GDCHK0) print *, "I am in woa_init 2.2"
      if ( file_exist('input.nml')) then
 if (GDCHK0) print *, "I am in woa_init 2.3"
        unit =  open_namelist_file ( )
        ierr=1; do while (ierr /= 0)
        read  (unit, nml=woa_nml, iostat=io,  &
               end=10)
        ierr = check_nml_error(io,'woa_nml')
        end do
10      call close_file (unit)
 if (GDCHK0) print *, "I am in woa_init 2.4"
      endif                      
#endif

 else
   filename = "WOA05_pottemp_salt.nc" ! name of netcdf file containing
   data_names(1) = "ptemp"
   data_names(2) = "salt"
 endif
 if (GDCHK0) print *, "I am in woa_init 3.0"
 
!---------------------------------------------------------------------
!    write version number and namelist to logfile.
!---------------------------------------------------------------------
      call write_version_number (version, tagname)
      logunit = stdlog()
      if (mpp_pe() == mpp_root_pe() ) &
                        write (logunit, nml=woa_nml)

!---------------------------------------------------------------------
!    define the dimensions of the local processors portion of the grid.
!---------------------------------------------------------------------
      id    = size(lonb,1) - 1
      jd    = size(latb,2) - 1

!---------------------------------------------------------------------
!    case of single input woa field. when running standalone code
!    on other than FMS level structure, woa_data_source must be 
!    'input'.
!---------------------------------------------------------------------
 if (GDCHK0) print *, "I am in woa_init 4.0, woa_data_source=",trim(woa_data_source)
      if (trim(woa_data_source)== 'input') then
 if (GDCHK0) print *, "I am in woa_init 4.1, woa_data_source=",trim(woa_data_source)
        do_column_woa = .true.
        call obtain_input_file_data
        nfields = 1
        allocate (woa_names(nfields))
        woa_names (1) = 'total_woa'

!---------------------------------------------------------------------
!    case of predicted woas.
!---------------------------------------------------------------------
      else if (trim(woa_data_source) == 'predicted') then
 if (GDCHK0) print *, "I am in woa_init 4.2, woa_data_source=",trim(woa_data_source)
      
        do_predicted_woa = .true.

!-----------------------------------------------------------------------
!    count number of activated woa variables, which will be carried
!    in the model as tracers with an attribute of 'radiative_param'. 
!    define the names associated with these woas.
!-----------------------------------------------------------------------
        call get_number_tracers(MODEL_ATMOS, num_tracers= ntrace)
        allocate (tracer_names(ntrace))
        do n = 1, ntrace
          call get_tracer_names(MODEL_ATMOS,n,tracer_names(n))
          flag = query_method ('radiative_param', MODEL_ATMOS, &
                               n, name, control)
          if (flag) then
!!!bjt            call get_radiative_param(name,control,rad_forc_online, &
!!!bjt                                     tr_rad_name, tr_clim_name, &
!!!bjt                                     tr_rad_scale_factor)
            if (rad_forc_online) then
              nfields = nfields +1
              woa_tracer_index(nfields) = n
              data_names_predicted(nfields) = trim(tr_rad_name)
!             data_names(nfields)           = trim(tr_clim_name)
              data_names(nfields)           = trim(tr_rad_name)
              woa_tracer_scale_factor(nfields) = tr_rad_scale_factor
            endif
          endif
        end do

!----------------------------------------------------------------------
!    allocate and fill pointer arrays to return the names of the activ-
!    ated species and any activated families to the calling routine.
!---------------------------------------------------------------------
        allocate (woa_names(nfields))
        woa_names(:)        = data_names_predicted(1:nfields)

!----------------------------------------------------------------------
!    allocate and fill an array to indicate whether or not each woa 
!    field is  to be overridden.
!---------------------------------------------------------------------
        allocate (being_overridden(nfields))
        being_overridden(:) = .false.

!---------------------------------------------------------------------
!    case of 'climatology' and 'calculate_column' woa data source.
!---------------------------------------------------------------------
      else   ! (trim(woa_data_source) == 'input')     
 if (GDCHK0) print *, "I am in woa_init 4.3, woa_data_source=",trim(woa_data_source)
        do_specified_woa = .true.

!---------------------------------------------------------------------
!    determine how many woas in the file are activated.
!--------------------------------------------------------------
        do n=1,MAX_DATA_FIELDS
          if (data_names(n) /= ' '  ) then
            nfields = n
          else
            exit
          endif
        end do
  
 if (GDCHK0) then
   print *, "I am in woa_init 4.3.1, nfields=",nfields
   call flush()
 endif
!---------------------------------------------------------------------
!    check for case of inconsistent nml specification -- case of re-
!    questing time-varying woa for a given woa, but indicating
!    that the time series is not to be used. in this case, a note will
!    be written to stdout indicating that the woa variables will not
!    be time-varying. this is needed because the default nml settings  
!    are defined so as to allow backward compatibility with existing 
!    code and script settings, and lead to this conflict.
!---------------------------------------------------------------------
        do n=1, nfields
          if (.not. use_woa_timeseries) then
            if (time_varying_species(n)) then
              call error_mesg ('woa_mod', &
                  'inconsistent nml settings -- not using woa  &
                  &timeseries but requesting interannual variation of  &
                  & woa amount for '  // trim (data_names(n))  // &
                 ' -- this woa will NOT exhibit interannual &
                  &variation', NOTE)
              time_varying_species(n) = .false.
            endif
          endif
        end do
 if (GDCHK0) then
   print *, "I am in woa_init 4.3.2, time_varying_species=",time_varying_species(1:nfields)
   call flush()
 endif

!---------------------------------------------------------------------
!    allocate and fill pointer arrays to return the names of the activ-
!    ated species and any activated families to the calling routine.
!--------------------------------------------------------------------
        allocate (woa_names(nfields))
        woa_names (:) = data_names(1:nfields)

 if (GDCHK0) then
        print *, "I am in woa_init 4.3.3"
        print *, "woa_names=",woa_names
        call flush()
 endif

!----------------------------------------------------------------------
!    allocate and initialize module variables.
!----------------------------------------------------------------------
        allocate (woa_offset(nfields), woa_entry(nfields), &
              negative_offset(nfields), using_fixed_year_data(nfields)) 
        woa_offset = set_time (0,0)
        woa_entry = set_time (0,0)
        negative_offset = .false.
        using_fixed_year_data = .false.

!----------------------------------------------------------------------
!    define the model base time  (defined in diag_table) 
!----------------------------------------------------------------------
        Model_init_time = get_base_time()

!----------------------------------------------------------------------
!    define the array using_fixed_year_data. it will be .true. for a 
!    given woa variables if the nml variable use_woa_timeseries 
!    is .true., and the nml variable time_varying_species for that
!    woa is .false., or if use_woa_timeseries is .false but a 
!    non-default woa_dataset_entry has been specified; otherwise it 
!    will be .false..
!----------------------------------------------------------------------
        do n=1,nfields           
          if (use_woa_timeseries) then
            if (time_varying_species(n)) then
              using_fixed_year_data(n) = .false.
            else
              using_fixed_year_data(n) = .true.
            endif
  
!---------------------------------------------------------------------
!    if no dataset entry point is supplied when an woa timeseries
!    file is being used, define the entry point as the model base time.
!---------------------------------------------------------------------
            if (woa_dataset_entry(1,n) == 1 .and. &
                woa_dataset_entry(2,n) == 1 .and. &
                woa_dataset_entry(3,n) == 1 .and. &
                woa_dataset_entry(4,n) == 1 .and. &
                woa_dataset_entry(5,n) == 1 .and. &
                woa_dataset_entry(6,n) == 1 ) then
              woa_entry(n) = Model_init_time

!----------------------------------------------------------------------
!    if a dataset entry time is defined, compute the offset from model 
!    base time to woa_dataset_entry as a time_type variable.
!----------------------------------------------------------------------
            else
              woa_entry(n) = set_date (woa_dataset_entry(1,n), &
                                           woa_dataset_entry(2,n), &
                                           woa_dataset_entry(3,n), &
                                           woa_dataset_entry(4,n), &
                                           woa_dataset_entry(5,n), &
                                           woa_dataset_entry(6,n))
            endif

!----------------------------------------------------------------------
!    indicate that woa variables n will be defined from the timeseries
!    file, and the relationship of the timeseries to the model calendar.

!--------------------------------------------------------------------
 
            call error_mesg ( 'woa_mod', &
               'PROCESSING WOA TIMESERIES FOR ' // &
               trim(woa_names(n)), NOTE)
            call print_date (woa_entry(n) ,   &
                str= ' Data from woa timeseries at time: ')
            call print_date (Model_init_time , str=' This data is &
                               &mapped to model time:')
 if (GDCHK0) then 
   print *, "I am in woa_init 4.3.4"
   call flush()
 endif

!---------------------------------------------------------------------
!    indicate whether a single year of the woa climatology will be
!    repeated throughout the model run, or if the woa time behavior
!    will show interannual changes.
!---------------------------------------------------------------------
            if (using_fixed_year_data(n)) then
              call error_mesg ('woa_mod', &
                 'This annual cycle will be used every model year &
               & -- no interannual variation for '  &
                                     // trim(woa_names(n)), NOTE)
            else
              call error_mesg ('woa_mod', &
                      trim(woa_names(n)) //   &
                     ' will exhibit interannual variation as defined &
                     & in the climatology file ', NOTE)
            endif
!---------------------------------------------------------------------
!    define the offset between the woa timeseries and the model
!    calendar, and whether this is a positive or negative offset.
!--------------------------------------------------------------------
            woa_offset(n) = woa_entry(n) - Model_init_time
            if (Model_init_time > woa_entry(n)) then
              negative_offset(n) = .true.
            else
              negative_offset(n) = .false.
            endif

 if (GDCHK0) then 
   print *, "I am in woa_init 4.3.5"
   call flush()
 endif
!---------------------------------------------------------------------
!    if use_woa_timeseries is .false., then either data from a 
!    single year defined in a timeseries file is to be used throughout 
!    the model integration, or a non-specific single-year woa 
!    climatology file is to be used.
!---------------------------------------------------------------------
          else 

 if (GDCHK0) print *, "I am in woa_init 4.3.6"

!---------------------------------------------------------------------
!    if no dataset entry has been specified, then a non-specific single
!    year climatology file is being used. set the variable 
!    using_fixed_year_data to be .false.. output a descriptive message
!    to stdout.
!---------------------------------------------------------------------
            if (woa_dataset_entry(1,n) == 1 .and. &
                woa_dataset_entry(2,n) == 1 .and. &
                woa_dataset_entry(3,n) == 1 .and. &
                woa_dataset_entry(4,n) == 1 .and. &
                woa_dataset_entry(5,n) == 1 .and. &
                woa_dataset_entry(6,n) == 1 ) then
              using_fixed_year_data(n) = .false.
              if (mpp_pe() == mpp_root_pe() ) then
                print *, 'woa data for ', trim(woa_names(n)),   &
                   ' obtained from single year climatology file '
              endif

!---------------------------------------------------------------------
!    if a year has been specified for the dataset entry, then the data
!    will be coming from an woa timeseries file, but the same annual
!    woa variation will be used for each model year. set the var-
!    iable using_fixed_year_data to be .true..  define woa_entry as
!    feb 01 of the year given by the first element of nml variable 
!    woa_dataset_entry. output a descriptive message to stdout.
!--------------------------------------------------------------------
            else
              using_fixed_year_data(n) = .true.
              woa_entry(n) = set_date (woa_dataset_entry(1,n), &
                                           2, 1, 0, 0, 0)
              call error_mesg ('woa_mod', &
                  'woa data is defined from a single annual cycle &
                  &for ' // trim(woa_names(n)) //   &
                  &' - no interannual variation', NOTE)
              if (mpp_pe() == mpp_root_pe() ) then
                print *, 'woa data for ', trim(woa_names(n)),  &
                    ' obtained from woa timeseries &
                    &for year:', woa_dataset_entry(1,n)
              endif
            endif
          endif
        end do

!-----------------------------------------------------------------------
!    count number of activated woa families. allocate a pointer 
!    array to return the names of the activated species to the calling 
!    routine.
!-----------------------------------------------------------------------
!       do n=1,MAX_WOA_FAMILIES
!         if (family_names(n) /= ' '  ) then
!           nfamilies = n
!         else
!           exit
!         endif
!       end do

!-----------------------------------------------------------------------
!    allocate and initialize variables needed for interpolator_mod if 
!    any woa variables have been activated.
!-----------------------------------------------------------------------
        allocate (data_out_of_bounds(nfields))
        allocate (vert_interp       (nfields))
        data_out_of_bounds = CONSTANT
        vert_interp = INTERP_WEIGHTED_P

!----------------------------------------------------------------------
!    determine if separate calls to interpolator  must be made for 
!    each woa variables, or if all variables in the file may be
!    interpolated together.  reasons for separate calls include differ-
!    ent data times desired for different woas, different vertical
!    interpolation procedures and different treatment of undefined
!    data.
!----------------------------------------------------------------------
          do n=2,nfields
            if (time_varying_species(n) .and.   &
               (.not. time_varying_species(n-1) ) ) then
              make_separate_calls = .true.
              exit
            endif
            if (using_fixed_year_data(n) .and.   &
                (.not. using_fixed_year_data(n-1) ) ) then
              make_separate_calls = .true.
              exit
            endif
            if (woa_entry(n) /= woa_entry(n-1)) then
              make_separate_calls = .true.
              exit
            endif
            if (data_out_of_bounds(n) /= data_out_of_bounds(n-1)) then
              make_separate_calls = .true.
              exit
            endif
            if (vert_interp       (n) /= vert_interp       (n-1)) then
              make_separate_calls = .true.
              exit
            endif
          end do
          if (make_separate_calls) then
            allocate (woa_interp(nfields))  
            allocate (woa_time  (nfields))  
          else
            allocate (woa_interp(1))  
            allocate (woa_time  (1))  
          endif

!----------------------------------------------------------------------
!    determine if the woa_data_source is specified as 
!    'calculate_column'. 
!--------------------------------------------------------------------
        if (trim(woa_data_source) == 'calculate_column') then

 if (GDCHK0) print *, "I am in woa_init 4.3.4, woa_data_source=",trim(woa_data_source)

!----------------------------------------------------------------------
!    if the woa_data_source is specified as 'calculate_column', then
!    the woa fields will be obtained by averaging the woa fields
!    in the climatology over a specified latitude-longitude section at
!    a specified calendar time, and this profile will be used in all 
!    model columns. make sure the specified lats / lons / time are 
!    valid.
!-------------------------------------------------------------------
          do n=1,2
            if (lonb_col(n) < 0. .or. lonb_col(n) > 360.) then
              call error_mesg ('woa_mod', &
                  ' invalid value for lonb_col', FATAL)
            endif
            if (latb_col(n) < -90. .or. latb_col(n) > 90.) then
              call error_mesg ('woa_mod', &
                  ' invalid value for latb_col', FATAL)
            endif
          end do
          if (time_col(1) == 0) then
            call error_mesg ('woa_mod', &
                'invalid time specified for time_col', FATAL)
          endif

          if (.not. use_woa_timeseries) then
              call error_mesg ('woa_mod', &
         'must use_woa_timeseries when calculate_column is .true.', FATAL)
          endif 

          if (any(time_varying_species(1:nfields))) then
              call error_mesg ('woa_mod', &
                   'woa values must be fixed in time when &
                                   &calculate_column is .true.', FATAL)
          endif 


!----------------------------------------------------------------------
!    call interpolator_init to begin processing the woa climat-
!    ology file. define the valid time as a time_type, and output
!    informative messages.
!---------------------------------------------------------------------
    if (make_separate_calls) then
              call error_mesg ('woa_mod', &
         'make_separate_calls not allowed  for calculate_column', FATAL)
     else       
          call interpolator_init (woa_interp(1)    , filename,  &
                                    spread(lonb_col/RADIAN,2,2),  &
                                    spread(latb_col/RADIAN,1,2),&
                                    data_names(:nfields),   &
                                    data_out_of_bounds=   &
                                                  data_out_of_bounds, &
                                    vert_interp=vert_interp,  &
                                    interp_method=interp_method, &
                                    single_year_file = single_year_file, clim_units=clim_units(:nfields))
     endif
          woa_column_time = set_date (time_col(1), time_col(2), &
                                          time_col(3), time_col(4), &
                                          time_col(5), time_col(6))
          call print_date (woa_column_time,  str= &
              ' woa data used is from woa timeseries at time: ')
          if (mpp_pe() == mpp_root_pe() ) then
            print *, 'woa data is averaged over latitudes',  &
                latb_col(1), ' to', latb_col(2), ' and longitudes',&
                lonb_col(1), ' to', lonb_col(2)
          endif

!----------------------------------------------------------------------
!    if 'calculate_column' is .false., then the woa fields will have
!    the appropriate horizontal variation. call interpolator_init to
!    begin processing the woa climatology file.    
!-------------------------------------------------------------------
        else  ! (calculate_column)
    if (make_separate_calls) then
       do n=1,nfields
          call interpolator_init (woa_interp(n), filename, lonb, &
                                  latb, data_names(n:n     ),   &
                                  data_out_of_bounds=    &
                                                  data_out_of_bounds(n:n), &
                                  vert_interp=vert_interp(n:n), &
                                  interp_method=interp_method, &
                                  single_year_file=single_year_file, clim_units=clim_units(n:n))
       end do
     else
          call interpolator_init (woa_interp(1), filename, lonb, &
                                  latb, data_names(:nfields),   &
                                  data_out_of_bounds=    &
                                                  data_out_of_bounds, &
                                  vert_interp=vert_interp, &
                                  interp_method=interp_method, &
                                  single_year_file=single_year_file, clim_units=clim_units(:nfields))
     endif
        endif ! (calculate_column)

!--------------------------------------------------------------------
!    check for compatibility of nml options requested and the woa
!    data file which was read.
!--------------------------------------------------------------------
 if (GDCHK0) then
        print *, "I am in woa_init 4.3.5"
        print *, "use_woa_timeseries=",use_woa_timeseries
        print *, "using_fixed_year_data=",using_fixed_year_data
        print *, "single_year_file=",single_year_file
 endif
        if (single_year_file .and.  use_woa_timeseries) then
          call error_mesg ('woa_mod', &
               'woa input file is single-year, yet interannual &
                &variation of woa is requested', FATAL  )
        endif
        do n=1, nfields
          if (.not. use_woa_timeseries .and.   &
              .not. using_fixed_year_data(n) .and. &
              .not. single_year_file)  then
            call error_mesg ('woa_mod', &
              'woa input file contains a time-series, yet nml  &
               &settings indicate that a non-specific single-year &
               &climatology is to be used', FATAL)
          endif
          if (.not. use_woa_timeseries .and.   &
              using_fixed_year_data(n) .and. &
              single_year_file)  then
            call error_mesg ('woa_mod', &
             'woa input file is non-specific single-year file,  &
               &yet nml settings specify that a particular single-year &
               &climatology is to be used', FATAL)
          endif
        end do
      endif  ! ('woa_data_source == 'input')

 if (GDCHK0) print *, "I am in woa_init 5.0"


!-----------------------------------------------------------------------
!    count number of activated woa families. allocate a pointer 
!    array to return the names of the activated species to the calling 
!    routine.
!-----------------------------------------------------------------------
      do n=1,MAX_WOA_FAMILIES
        if (family_names(n) /= ' '  ) then
          nfamilies = n
        else
          exit
        endif
      end do

!---------------------------------------------------------------------
!    allocate and fill pointer arrays to return the names of any activ-
!    ated families to the calling routine.
!-------------------------------------------------------------------
      allocate (woa_family_names(nfamilies))
      woa_family_names (:) = family_names(1:nfamilies)

!---------------------------------------------------------------------
!    mark the module as initialized.
!---------------------------------------------------------------------
      module_is_initialized = .true.

!---------------------------------------------------------------------



end subroutine woa_init

!############################################################################

subroutine woa_time_vary (model_time)

!--------------------------------------------------------------------------- 
!   subroutine woa_time_vary makes sure the woa interpolate_type 
!   variable has access to the proper time levels of data in the woa
!---------------------------------------------------------------------------

type(time_type), intent(in) :: model_time


      integer :: n
#ifdef test_woa
         print *, "woa_time_vary: do_specified_woa=",do_specified_woa
#endif
!----------------------------------------------------------------------
!    be sure the proper time levels are in memory for the woa timeseries.
!----------------------------------------------------------------------
      if ( do_specified_woa) then
#ifdef test_woa
         print *, "woa_time_vary: make_separate_calls=",make_separate_calls
         print *, "woa_time_vary: use_woa_timeseries=",use_woa_timeseries
         print *, "woa_time_vary: time_varying_species=",time_varying_species
#endif
        if (make_separate_calls) then
!--------------------------------------------------------------------
!    if separate calls are required for each woa variables, loop over
!    the individual species.
!--------------------------------------------------------------------
          do n=1,nfields

!--------------------------------------------------------------------
!    if the data timeseries is to be used for species n, define the
!    time for which data is desired, and then call interpolator to
!    verify the time levels bracketing the desired time are available.
!--------------------------------------------------------------------
            if (use_woa_timeseries) then
              if (time_varying_species(n)) then

!----------------------------------------------------------------------
!    define the woa_time for woa n and check for the  
!    appropriate time slices.
!----------------------------------------------------------------------
                if (negative_offset(n)) then
                  woa_time(n) = model_time - woa_offset(n)
                else
                  woa_time(n) = model_time + woa_offset(n)
                endif
#ifdef test_woa
                print *, "woa_time_vary: 1.0 obtain_interpolator_time_slices"
#endif
                call obtain_interpolator_time_slices (woa_interp(n), &
                                                          woa_time(n))     
              else
                call set_woa_time (model_time, woa_entry(n), &
                                       woa_time(n))
#ifdef test_woa
                print *, "woa_time_vary: 1.1 obtain_interpolator_time_slices"
#endif
                call obtain_interpolator_time_slices (woa_interp(n), &
                                                       woa_time(n))     
              endif

!--------------------------------------------------------------------
!    if the data timeseries is not to be used for species n, define the
!    time for which data is desired, and then call interpolator to
!    obtain the data.
!--------------------------------------------------------------------
            else  ! (use_woa_timeseries)

!---------------------------------------------------------------------
!    if a fixed year has not been specified, obtain data relevant for
!    the current model year.
!---------------------------------------------------------------------
              if ( .not. using_fixed_year_data(n)) then
#ifdef test_woa
                print *, "woa_time_vary: 1.2 obtain_interpolator_time_slices"
#endif
                call obtain_interpolator_time_slices (woa_interp(n), &
                                                              model_time)     
                woa_time(n) = model_time

!----------------------------------------------------------------------
!    if a fixed year has been specified, call set_woa_time to define
!    the woa_time to be used for woa n. call interpolator to 
!    obtain the woa values and store the woa amounts in 
!    woa%data.
!----------------------------------------------------------------------
              else 
                call set_woa_time (model_time, woa_entry(n), &
                                         woa_time(n))
#ifdef test_woa
                print *, "woa_time_vary: 1.3 obtain_interpolator_time_slices"
#endif
                call obtain_interpolator_time_slices (woa_interp(n), &
                                                          woa_time(n))     
              endif  
            endif ! (use_woa_timeseries)
          end do  !(nfields)

        else  ! (make_separate_calls)
           
!--------------------------------------------------------------------
!    if the data timeseries is to be used for species n, define the
!    time for which data is desired.
!--------------------------------------------------------------------
          if (use_woa_timeseries) then
            if (negative_offset(1)) then
              woa_time(1) = model_time - woa_offset(1)
            else
              woa_time(1) = model_time + woa_offset(1)
            endif

!--------------------------------------------------------------------
!    if 'calculate_column' is being used,  be sure the needed time slices
!    are in memory.
!--------------------------------------------------------------------
            if (trim(woa_data_source) == 'calculate_column') then
#ifdef test_woa
                print *, "woa_time_vary: 1.4 obtain_interpolator_time_slices."
                call print_date (woa_column_time,  str= ' woa_column_time: ')
#endif
              call obtain_interpolator_time_slices (woa_interp(1), &
                                woa_column_time)
            else

!-------------------------------------------------------------------
!    since separate calls are not required, all woa variables are 
!    either time_varying  or not.  be sure the needed time slices are available.
!--------------------------------------------------------------------
              if (time_varying_species(1)) then
#ifdef test_woa
                print *, "woa_time_vary: 1.5 obtain_interpolator_time_slices."
                call print_date (woa_time(1),  str= ' woa_time(1): ')
#endif
                call obtain_interpolator_time_slices (woa_interp(1), &
                                woa_time(1))
              else
                call set_woa_time (model_time, woa_entry(1), &
                                       woa_time(1))
#ifdef test_woa
                print *, "woa_time_vary: 1.6 obtain_interpolator_time_slices."
                call print_date (woa_time(1),  str= ' woa_time(1): ')
#endif
                call obtain_interpolator_time_slices (woa_interp(1), &
                                woa_time(1))
              endif    
            endif  ! (calculate_column)

!--------------------------------------------------------------------
!    if the data timeseries is not to be used for species n, define the
!    time for which data is desired, and verify the presence of the needed
!    bracketing time slices.
!--------------------------------------------------------------------
          else ! (use_woa_timeseries)

!---------------------------------------------------------------------
!    if a fixed year has not been specified, obtain data relevant for
!    the current model year. this data comes from a non-specific single-yr
!    climatology file.
!---------------------------------------------------------------------
            if (.not. using_fixed_year_data(1)) then
#ifdef test_woa
                print *, "woa_time_vary: 1.7 obtain_interpolator_time_slices."
                call print_date (model_time,  str= 'model_time: ')
#endif            
              call obtain_interpolator_time_slices (woa_interp(1), &
                                  model_time)
              woa_time(1) = model_time

!----------------------------------------------------------------------
!    if a fixed year has been specified, call set_woa_time to define
!    the woa_time, then verify the needed time slices are available. 
!----------------------------------------------------------------------
            else
              call set_woa_time (model_time, woa_entry(1), &
                                                             woa_time(1))
#ifdef test_woa
                print *, "woa_time_vary: 1.8 obtain_interpolator_time_slices."
                call print_date (woa_time(1),  str= 'woa_time(1): ')
#endif 
              call obtain_interpolator_time_slices (woa_interp(1), &
                                                            woa_time(1))
            endif ! (using_fixed_year)
          endif ! (use_woa_timeseries)
        endif  ! (make_separate_calls   )
      endif ! (do_column_woa)
               
!-------------------------------------------------------------------- 

#ifdef test_woa
                print *, "woa_time_vary: 2.0 after obtain_interpolator_time_slices."
#endif


end subroutine woa_time_vary 



!####################################################################

subroutine woa_endts

     integer :: n

     if (allocated(woa_interp)) then
       do n=1, size(woa_interp,1)
         call unset_interpolator_time_flag (woa_interp(n))
       end do
     endif
  
     override_counter = override_counter + 1
     if (override_counter == 2) then
       output_override_info = .false.
     endif

end subroutine woa_endts



!######################################################################
! <SUBROUTINE NAME="woa_driver">
!  <OVERVIEW>
!   Interpolate woa verical profile based on prescribed woa
!   climatology input and model set up.
!  </OVERVIEW>
!  <TEMPLATE>
!   call woa_driver (is, js, model_time, p_half, woa)
!  </TEMPLATE>
!  <INOUT NAME="woa" TYPE="woa_type">
!   woa climatology input
!  </INOUT>
!  <IN NAME="model_time" TYPE="time_type">
!   The internal model simulation time, i.e. Jan. 1 1982
!  </IN>
!  <IN NAME="tracer" TYPE="real">
!   4 dimensional array of tracers, last index is the number of all tracers
!  </IN>
!  <IN NAME="p_half" TYPE="real">
!   The array of model layer pressure values
!  </IN>
!  <IN NAME="is" TYPE="integer">
!   The longitude index of model physics window domain
!  </IN>
!  <IN NAME="js" TYPE="integer">
!   The latitude index of model physics window domain
!  </IN>
!  <IN NAME="override_woas" TYPE="logical, optional">
!   use offline woas via data_override?
!  </IN>
! </SUBROUTINE>
!
subroutine woa_driver (is, js, model_time, tracer, &
                           p_half, p_flux, woa, override_woas)

!-----------------------------------------------------------------------
!    woa_driver returns the names and concentrations of activated 
!    woa variables at model grid points at the model_time to the 
!    calling routine in woa_type variable woa. 
!-----------------------------------------------------------------------

integer,                  intent(in)     :: is,js
type(time_type),          intent(in)     :: model_time
real, dimension(:,:,:,:), intent(in)     :: tracer
real, dimension(:,:,:),   intent(in)  :: p_half, p_flux
type(woa_type),       intent(inout)  :: woa
logical, optional,        intent(in)     :: override_woas

!--------------------------------------------------------------------
!   intent(in) variables:
!
!       is, js           starting subdomain i,j indices of data in 
!                        the physics_window being integrated
!       model_time       time for which woa data is desired
!                        [ time_type ]
!       p_half           model pressure at interface levels 
!                        [ Pa ]
!      
!   intent(inout) variables:
!
!       woa    woa_type variable. the following components will
!                  be returned from this routine:
!                   data      concentration of each active woa 
!                                species at each model grid point
!                                [ kg / m**2 ]
!                   woa_names 
!                                names assigned to each active species
!
!----------------------------------------------------------------------

!---------------------------------------------------------------------
!  local variables:

      real, dimension(1,1, size(p_half,3)-1,    &
                                               nfields) :: woa_data
      real, dimension(1,1, size(p_half,3))   :: p_half_col
      real, dimension(id,jd,size(p_half,3)-1) :: woa_proc
      integer         :: n, k, j, i, na            ! do-loop index
      integer         :: nn
      logical         :: do_override, used
      integer         :: ie, je

!---------------------------------------------------------------------
!    be sure module has been initialized.
!---------------------------------------------------------------------
      if (.not. module_is_initialized ) then
        call error_mesg ('woa_mod',   &
                         'module has not been initialized',FATAL )
      endif

!---------------------------------------------------------------------
!    allocate an array to hold the activated woa names. allocate an
!    array which defines the members of the requested woa families.
!    allocate an array to hold the woa amounts for each species at
!    each grid point. 
!---------------------------------------------------------------------
      allocate (woa%woa_names (nfields))
      allocate (woa%family_members(nfields+1, nfamilies))
      allocate (woa%data(size(p_half,1),  &
                                size(p_half,2), &
                                size(p_half,3) - 1, nfields)) 
      ie = is + size(p_half,1) - 1
      je = js + size(p_half,2) - 1

      if (do_column_woa) then
 
!---------------------------------------------------------------------
!    here all woa is consolidated into a single variable.
!----------------------------------------------------------------------
        woa%woa_names(1) = 'total_woa'
        do k=1, size(woa%data,3)
          woa%data(:,:,k,1) = specified_woa(k)
        end do
      else 
      
!--------------------------------------------------------------------
!    define an array to hold the activated woa names.
!---------------------------------------------------------------------
        woa%woa_names = data_names(:nfields) 

!--------------------------------------------------------------------
!    define an array which defines the members of the requested woa
!    families.
!---------------------------------------------------------------------
        if (nfamilies > 0) then
          do n=1,nfamilies
            do na = 1, nfields
              select case(n)
                case (1)
                  woa%family_members(na,1) = in_family1(na)
                case (2)
                  woa%family_members(na,2) = in_family2(na)
                case (3)
                  woa%family_members(na,3) = in_family3(na)
                case (4)
                  woa%family_members(na,4) = in_family4(na)
                case (5)
                  woa%family_members(na,5) = in_family5(na)
                case (6)
                  woa%family_members(na,6) = in_family6(na)
                case (7)
                  woa%family_members(na,7) = in_family7(na)
                case (8)
                  woa%family_members(na,8) = in_family8(na)
                case (9)
                  woa%family_members(na,9) = in_family9(na)
                case (10)
                  woa%family_members(na,10) = in_family10(na)
                case (11)
                  woa%family_members(na,11) = in_family11(na)
                case (12)
                  woa%family_members(na,12) = in_family12(na)
                case DEFAULT
              end select
            end do
            if (volc_in_fam_col_opt_depth(n)) then
              woa%family_members(nfields+1,n) = .true.
            else
              woa%family_members(nfields+1,n) = .false.
            endif
          end do
        endif

!----------------------------------------------------------------------
#ifdef test_woa
         print *, "do_specified_woa=",do_specified_woa
#endif
        if ( do_specified_woa) then

!--------------------------------------------------------------------
!    if 'calculate_column' is being used, obtain the woa values for
!    each column, one at a time, using the pressure profile for that
!    column. this allows each column to see the same woa fields,
!    but distributed appropriately for its pressure structure.
!--------------------------------------------------------------------
#ifdef test_woa
          print *, "woa_data_source=",trim(woa_data_source)
#endif
          if (trim(woa_data_source) == 'calculate_column') then
            do j=1, size(p_half,2)
              do i=1, size(p_half,1)
                p_half_col(1,1,:) = p_flux(i,j,:)
                call interpolator (woa_interp(1),&
                                   woa_column_time,  &
                                   p_half_col, woa_data, &
                                   woa%woa_names(1), 1, 1  )
                woa%data(i,j,:,:) = woa_data(1,1,:,:)
              end do
            end do
          else

!--------------------------------------------------------------------
!    if separate calls are required for each woa variables, loop over
!    the individual species.
!--------------------------------------------------------------------
#ifdef test_woa
            print *, "make_separate_calls=",make_separate_calls
#endif
            if (make_separate_calls) then
              do n=1,nfields                
#ifdef test_woa
                print *, "woa_time(",n,"):"
                call print_date(woa_time(n))
#endif     
                call interpolator (woa_interp(n), woa_time(n),  &
                                  p_flux, &
                                  woa%data(:,:,:,n),    &
                                  woa%woa_names(n), is, js)
              end do  !(nfields)

!----------------------------------------------------------------------
!    if separate calls are not required, use the first woa char-
!    acteristics to define woa_time and make a single call to 
!    interpolator. store the woa amounts in woa%data.
!----------------------------------------------------------------------
            else      
#ifdef test_woa
              print *, "woa_time(",1,"):"
              call print_date(woa_time(1))
#endif

              call interpolator (woa_interp(1), woa_time(1),  &
                                p_flux, &
                                woa%data,    &
                                woa%woa_names(1), is, js)
            endif      
          endif ! (calculate_column)
#ifdef test_woa
          do n=1,nfields                
            print *, "woa_name:",woa%woa_names(n)
            print *, "woa=",woa%data(:,:,:,n)
          enddo
#endif

!-------------------------------------------------------------------- 
!    for predicted woas (obtained from tracer array), assign the 
!    tracer to "woa" if that TRACER has "radiative_param" and 
!    "online", both defined in the field_table. 
! ***********************WARNINGS**************************************
!    the tracers are assumed to be expressed in Mass Mixing Ratio (MMR), 
!    and are converted into mass column for the radiative code.
!    Conversions (e.g. OC -> OM, or SO4 -> (NH4)2SO4 ) can be done
!    via radiative_param attribute scale_factor (in field_table).
!------------------------------------------------------------------ 
        else                  ! (do_specified_woa')
          if (present(override_woas)) then
            do_override = override_woas
          else
            do_override = .false.
          end if
#ifdef test_woa
          print *, "override_woas=",override_woas
#endif
          do nn=1,nfields
            n = woa_tracer_index(nn)

            woa%data(:,:,:,nn) = tracer(:,:,:,n)
            if (do_override) then
              call data_override('ATM', TRIM(tracer_names(n))//'_woa',&
                                 woa_proc, model_time, override=used)
              if (used) then
                if (output_override_info) then
                  call error_mesg ('woa_mod', &
                       TRIM(tracer_names(n))//'_woa => '// &
                     TRIM(tracer_names(n)) // ' is being overridden', NOTE)
                  being_overridden(nn) = .true.
                endif
                woa%data(:,:,:,nn) = woa_proc(is:ie,js:je,:)
              else
                if (output_override_info) then
                  call error_mesg ('woa_mod', &
                    TRIM(tracer_names(n))//'_woa => '//  &
                       TRIM(tracer_names(n)) // ' not overridden', NOTE)
                else
                  if (being_overridden(nn)) then
                    call error_mesg ('woa_mod', &
                       TRIM(tracer_names(n))//'_woa => '//  &
                         TRIM(tracer_names(n)) // ' not overridden &
                                      &when override was requested', FATAL)
                  endif
                endif
              endif
            endif
#ifdef test_woa
            print *, "override_woas=",override_woas
#endif

            do k=1,size(woa%data,3)
              do j=1,size(woa%data,2)
                do i=1,size(woa%data,1)
                  woa%data(i,j,k,nn) =    &
                          MAX (0.0, woa%data(i,j,k,nn)) * &
                          woa_tracer_scale_factor(nn) * &
                          ( p_half(i,j,k+1)-p_half(i,j,k) )/GRAV
                end do
              end do
            end do
          end do
        endif   ! (do_specified_woa')
      endif  ! (do_column_woa)

!-------------------------------------------------------------------- 

end subroutine woa_driver

subroutine predicted_woa_driver (is, js, model_time, tracer, &
                           p_half, woa, override_woas)

!-----------------------------------------------------------------------
!    woa_driver returns the names and concentrations of activated 
!    woa variables at model grid points at the model_time to the 
!    calling routine in woa_type variable woa. 
!-----------------------------------------------------------------------

integer,                  intent(in)     :: is,js
type(time_type),          intent(in)     :: model_time
real, dimension(:,:,:,:), intent(in)     :: tracer
real, dimension(:,:,:),   intent(in)  :: p_half
type(woa_type),       intent(inout)  :: woa
logical, optional,        intent(in)     :: override_woas

!--------------------------------------------------------------------
!   intent(in) variables:
!
!       is, js           starting subdomain i,j indices of data in 
!                        the physics_window being integrated
!       model_time       time for which woa data is desired
!                        [ time_type ]
!       p_half           model pressure at interface levels 
!                        [ Pa ]
!      
!   intent(inout) variables:
!
!       woa    woa_type variable. the following components will
!                  be returned from this routine:
!                   data      concentration of each active woa 
!                                species at each model grid point
!                                [ kg / m**2 ]
!                   woa_names 
!                                names assigned to each active species
!
!----------------------------------------------------------------------

!---------------------------------------------------------------------
!  local variables:

      real, dimension(1,1, size(p_half,3)-1,    &
                                               nfields) :: woa_data
      real, dimension(1,1, size(p_half,3))   :: p_half_col
      real, dimension(id,jd,size(p_half,3)-1) :: woa_proc
      integer         :: n, k, j, i, na            ! do-loop index
      integer         :: nn
      logical         :: do_override, used
      integer         :: ie, je

!---------------------------------------------------------------------
!    be sure module has been initialized.
!---------------------------------------------------------------------
      if (.not. module_is_initialized ) then
        call error_mesg ('woa_mod',   &
                         'module has not been initialized',FATAL )
      endif

     call woa_time_vary (model_time)
!---------------------------------------------------------------------
!    allocate an array to hold the activated woa names. allocate an
!    array which defines the members of the requested woa families.
!    allocate an array to hold the woa amounts for each species at
!    each grid point. 
!---------------------------------------------------------------------
      allocate (woa%woa_names (nfields))
      allocate (woa%family_members(nfields+1, nfamilies))
      allocate (woa%data(size(p_half,1),  &
                                size(p_half,2), &
                                size(p_half,3) - 1, nfields)) 
      ie = is + size(p_half,1) - 1
      je = js + size(p_half,2) - 1
     
!--------------------------------------------------------------------
!    define an array to hold the activated woa names.
!---------------------------------------------------------------------
        woa%woa_names = data_names(:nfields) 

!--------------------------------------------------------------------
!    define an array which defines the members of the requested woa
!    families.
!---------------------------------------------------------------------
        if (nfamilies > 0) then
          do n=1,nfamilies
            do na = 1, nfields
              select case(n)
                case (1)
                  woa%family_members(na,1) = in_family1(na)
                case (2)
                  woa%family_members(na,2) = in_family2(na)
                case (3)
                  woa%family_members(na,3) = in_family3(na)
                case (4)
                  woa%family_members(na,4) = in_family4(na)
                case (5)
                  woa%family_members(na,5) = in_family5(na)
                case (6)
                  woa%family_members(na,6) = in_family6(na)
                case (7)
                  woa%family_members(na,7) = in_family7(na)
                case (8)
                  woa%family_members(na,8) = in_family8(na)
                case (9)
                  woa%family_members(na,9) = in_family9(na)
                case (10)
                  woa%family_members(na,10) = in_family10(na)
                case (11)
                  woa%family_members(na,11) = in_family11(na)
                case (12)
                  woa%family_members(na,12) = in_family12(na)
                case DEFAULT
              end select
            end do
            if (volc_in_fam_col_opt_depth(n)) then
              woa%family_members(nfields+1,n) = .true.
            else
              woa%family_members(nfields+1,n) = .false.
            endif
          end do
        endif

!----------------------------------------------------------------------
#ifdef test_woa
         print *, "do_specified_woa=",do_specified_woa
#endif
!-------------------------------------------------------------------- 
!    for predicted woas (obtained from tracer array), assign the 
!    tracer to "woa" if that TRACER has "radiative_param" and 
!    "online", both defined in the field_table. 
! ***********************WARNINGS**************************************
!    the tracers are assumed to be expressed in Mass Mixing Ratio (MMR), 
!    and are converted into mass column for the radiative code.
!    Conversions (e.g. OC -> OM, or SO4 -> (NH4)2SO4 ) can be done
!    via radiative_param attribute scale_factor (in field_table).
!------------------------------------------------------------------ 
          if (present(override_woas)) then
            do_override = override_woas
          else
            do_override = .false.
          end if
#ifdef test_woa
          print *, "override_woas=",override_woas
#endif
          do nn=1,nfields
            n = woa_tracer_index(nn)

            woa%data(:,:,:,nn) = tracer(:,:,:,n)
            if (do_override) then
              call data_override('ATM', TRIM(tracer_names(n))//'_woa',&
                                 woa_proc, model_time, override=used)
              if (used) then
                if (output_override_info) then
                  call error_mesg ('woa_mod', &
                       TRIM(tracer_names(n))//'_woa => '// &
                     TRIM(tracer_names(n)) // ' is being overridden', NOTE)
                  being_overridden(nn) = .true.
                endif
                woa%data(:,:,:,nn) = woa_proc(is:ie,js:je,:)
              else
                if (output_override_info) then
                  call error_mesg ('woa_mod', &
                    TRIM(tracer_names(n))//'_woa => '//  &
                       TRIM(tracer_names(n)) // ' not overridden', NOTE)
                else
                  if (being_overridden(nn)) then
                    call error_mesg ('woa_mod', &
                       TRIM(tracer_names(n))//'_woa => '//  &
                         TRIM(tracer_names(n)) // ' not overridden &
                                      &when override was requested', FATAL)
                  endif
                endif
              endif
            endif
#ifdef test_woa
            print *, "override_woas=",override_woas
#endif

            do k=1,size(woa%data,3)
              do j=1,size(woa%data,2)
                do i=1,size(woa%data,1)
                  woa%data(i,j,k,nn) =    &
                          MAX (0.0, woa%data(i,j,k,nn)) * &
                          woa_tracer_scale_factor(nn) * &
                          ( p_half(i,j,k+1)-p_half(i,j,k) )/GRAV
                end do
              end do
            end do
          end do
!-------------------------------------------------------------------- 

end subroutine predicted_woa_driver


!-------------------------------------------------------------------- 

subroutine specified_woa_driver (is, js, model_time, p_flux, woa)

!-----------------------------------------------------------------------
!    woa_driver returns the names and concentrations of activated 
!    woa variables at model grid points at the model_time to the 
!    calling routine in woa_type variable woa. 
!-----------------------------------------------------------------------

integer,                  intent(in)     :: is,js
type(time_type), intent(in) :: model_time
real, dimension(:,:,:),   intent(in)  :: p_flux
type(woa_type),       intent(inout)  :: woa

!--------------------------------------------------------------------
!   intent(in) variables:
!
!       is, js           starting subdomain i,j indices of data in 
!                        the physics_window being integrated
!       p_half           model pressure at interface levels 
!                        [ Pa ]
!      
!   intent(inout) variables:
!
!       woa    woa_type variable. the following components will
!                  be returned from this routine:
!                   data      concentration of each active woa 
!                                species at each model grid point
!                                [ kg / m**2 ]
!                   woa_names 
!                                names assigned to each active species
!
!----------------------------------------------------------------------

!---------------------------------------------------------------------
!  local variables:

      real, dimension(1,1, size(p_flux,3)-1,    &
                                               nfields) :: woa_data
      real, dimension(1,1, size(p_flux,3))   :: p_half_col
      integer         :: n, k, j, i, na            ! do-loop index
      integer         :: nn
      logical         :: do_override, used
      integer         :: ie, je

!---------------------------------------------------------------------
!    be sure module has been initialized.
!---------------------------------------------------------------------
      if (.not. module_is_initialized ) then
        call error_mesg ('woa_mod',   &
                         'module has not been initialized',FATAL )
      endif
      call woa_time_vary (model_time)
!---------------------------------------------------------------------
!    allocate an array to hold the activated woa names. allocate an
!    array which defines the members of the requested woa families.
!    allocate an array to hold the woa amounts for each species at
!    each grid point. 
!---------------------------------------------------------------------
      allocate (woa%woa_names (nfields))
      allocate (woa%family_members(nfields+1, nfamilies))
      allocate (woa%data(size(p_flux,1),  &
                                size(p_flux,2), &
                                size(p_flux,3) - 1, nfields)) 

      !!! if (.not.allocated(woa%woa_names)) allocate (woa%woa_names (nfields))
      !!! if (.not.allocated(woa%family_members)) allocate (woa%family_members(nfields+1, nfamilies))
      !!! if (.not.allocated(woa%data)) allocate (woa%data(size(p_flux,1),  &
      !!!                           size(p_flux,2), &
      !!!                           size(p_flux,3) - 1, nfields)) 
      ie = is + size(p_flux,1) - 1
      je = js + size(p_flux,2) - 1
     
!--------------------------------------------------------------------
!    define an array to hold the activated woa names.
!---------------------------------------------------------------------
      woa%woa_names = data_names(:nfields) 

!--------------------------------------------------------------------
!    define an array which defines the members of the requested woa
!    families.
!---------------------------------------------------------------------
      if (nfamilies > 0) then
        do n=1,nfamilies
          do na = 1, nfields
            select case(n)
              case (1)
                woa%family_members(na,1) = in_family1(na)
              case (2)
                woa%family_members(na,2) = in_family2(na)
              case (3)
                woa%family_members(na,3) = in_family3(na)
              case (4)
                woa%family_members(na,4) = in_family4(na)
              case (5)
                woa%family_members(na,5) = in_family5(na)
              case (6)
                woa%family_members(na,6) = in_family6(na)
              case (7)
                woa%family_members(na,7) = in_family7(na)
              case (8)
                woa%family_members(na,8) = in_family8(na)
              case (9)
                woa%family_members(na,9) = in_family9(na)
              case (10)
                woa%family_members(na,10) = in_family10(na)
              case (11)
                woa%family_members(na,11) = in_family11(na)
              case (12)
                woa%family_members(na,12) = in_family12(na)
              case DEFAULT
            end select
          end do
          if (volc_in_fam_col_opt_depth(n)) then
            woa%family_members(nfields+1,n) = .true.
          else
            woa%family_members(nfields+1,n) = .false.
          endif
        end do
      endif

!----------------------------------------------------------------------
#ifdef test_woa
      print *, "do_specified_woa=",do_specified_woa
#endif
      if ( do_specified_woa) then

!--------------------------------------------------------------------
!    if 'calculate_column' is being used, obtain the woa values for
!    each column, one at a time, using the pressure profile for that
!    column. this allows each column to see the same woa fields,
!    but distributed appropriately for its pressure structure.
!--------------------------------------------------------------------
#ifdef test_woa
        print *, "woa_data_source=",trim(woa_data_source)
#endif
        if (trim(woa_data_source) == 'calculate_column') then
          do j=1, size(p_flux,2)
            do i=1, size(p_flux,1)
              p_half_col(1,1,:) = p_flux(i,j,:)
              call interpolator (woa_interp(1),&
                                 woa_column_time,  &
                                 p_half_col, woa_data, &
                                 woa%woa_names(1), 1, 1  )
              woa%data(i,j,:,:) = woa_data(1,1,:,:)
            end do
          end do
        else

!--------------------------------------------------------------------
!    if separate calls are required for each woa variables, loop over
!    the individual species.
!--------------------------------------------------------------------
#ifdef test_woa
          print *, "make_separate_calls=",make_separate_calls
#endif    
          if (make_separate_calls) then
            do n=1,nfields                
#ifdef test_woa
              print *, "woa_time(",n,"):"
              call print_date(woa_time(n))
#endif      
              call interpolator (woa_interp(n), woa_time(n),  &
                                p_flux, &
                                woa%data(:,:,:,n),    &
                                woa%woa_names(n), is, js)
            end do  !(nfields)

!----------------------------------------------------------------------
!    if separate calls are not required, use the first woa char-
!    acteristics to define woa_time and make a single call to 
!    interpolator. store the woa amounts in woa%data.
!----------------------------------------------------------------------
          else      
#ifdef test_woa
            print *, "woa_time(",1,"):"
            call print_date(woa_time(1))
#endif      
            
            call interpolator (woa_interp(1), woa_time(1),  &
                            p_flux, &
                            woa%data,    &
                            woa%woa_names(1), is, js)
          endif      
        endif ! (calculate_column)
  if (GDCHK1) then
!!! #ifdef test_woa
      do n=1,nfields                
        print *, "woa_name:",woa%woa_names(n)
        print *, "woa=",woa%data(:,:,:,n)
      enddo
!!! #endif
   endif
      endif   ! (do_specified_woa')
      if ((lowercase(trim(clim_units(1))) == "c" ).or.(lowercase(trim(data_units(1))) == "c" )) then
      ! covert to Kelvin
        woa%data(:,:,:,1)=MERGE(woa%data(:,:,:,1)+TFREEZE,xmissing,woa%data(:,:,:,1).ne.xmissing)
        ! note that woa%data(:,:,:,1) is for obswt
        ! note that woa%data(:,:,:,2) is for obsws
        ! note that woa%data(:,:,:,3) is for obswu
        ! note that woa%data(:,:,:,4) is for obswv
      endif
  if (GDCHK1) then
!!! #ifdef test_woa
      do n=1,1                
        print *, "after unit check, woa_name:",woa%woa_names(n)
        print *, "woa=",woa%data(:,:,:,n)
      enddo
!!! #endif
   endif
!-------------------------------------------------------------------- 

end subroutine specified_woa_driver

!-------------------------------------------------------------------- 

subroutine column_woa_driver (is, js, model_time, p_half, woa)

!-----------------------------------------------------------------------
!    woa_driver returns the names and concentrations of activated 
!    woa variables at model grid points at the model_time to the 
!    calling routine in woa_type variable woa. 
!-----------------------------------------------------------------------

integer,                  intent(in)     :: is,js
type(time_type), intent(in) :: model_time
real, dimension(:,:,:),   intent(in)  :: p_half
type(woa_type),       intent(inout)  :: woa

!--------------------------------------------------------------------
!   intent(in) variables:
!
!       is, js           starting subdomain i,j indices of data in 
!                        the physics_window being integrated
!       p_half           model pressure at interface levels 
!                        [ Pa ]
!      
!   intent(inout) variables:
!
!       woa    woa_type variable. the following components will
!                  be returned from this routine:
!                   data      concentration of each active woa 
!                                species at each model grid point
!                                [ kg / m**2 ]
!                   woa_names 
!                                names assigned to each active species
!
!----------------------------------------------------------------------

!---------------------------------------------------------------------
!  local variables:

      integer         :: n, k, j, i, na            ! do-loop index
      integer         :: nn
      integer         :: ie, je

!---------------------------------------------------------------------
!    be sure module has been initialized.
!---------------------------------------------------------------------
      if (.not. module_is_initialized ) then
        call error_mesg ('woa_mod',   &
                         'module has not been initialized',FATAL )
      endif
      call woa_time_vary (model_time)
!---------------------------------------------------------------------
!    allocate an array to hold the activated woa names. allocate an
!    array which defines the members of the requested woa families.
!    allocate an array to hold the woa amounts for each species at
!    each grid point. 
!---------------------------------------------------------------------
      allocate (woa%woa_names (nfields))
      allocate (woa%family_members(nfields+1, nfamilies))
      allocate (woa%data(size(p_half,1),  &
                                size(p_half,2), &
                                size(p_half,3) - 1, nfields))
                                
 
      if (do_column_woa) then
 
!---------------------------------------------------------------------
!    here all woa is consolidated into a single variable.
!----------------------------------------------------------------------
        woa%woa_names(1) = 'total_woa'
        do k=1, size(woa%data,3)
          woa%data(:,:,k,1) = specified_woa(k)
        end do
      endif  ! (do_column_woa)

!-------------------------------------------------------------------- 

end subroutine column_woa_driver


!#####################################################################
! <SUBROUTINE NAME="woa_end">
!  <OVERVIEW>
!   woa_end is the destructor for woa_mod.
!  </OVERVIEW>
!  <DESCRIPTION>
!   woa_end is the destructor for woa_mod.
!  </DESCRIPTION>
!  <TEMPLATE>
!   call woa_end
!  </TEMPLATE>
! </SUBROUTINE>
!
subroutine woa_end

!----------------------------------------------------------------------
!    woa_end is the destructor for woa_mod.
!----------------------------------------------------------------------

      integer  :: n


!---------------------------------------------------------------------
!    be sure module has been initialized.
!---------------------------------------------------------------------
      if (.not. module_is_initialized ) then
        call error_mesg ('woa_mod',   &
                         'module has not been initialized',FATAL )
      endif

!---------------------------------------------------------------------
!    call interpolator_end to release the interpolate_type variable 
!    used in this module.
!---------------------------------------------------------------------
      if (do_specified_woa) then
          if (nfields > 0) then
            do n=1, size(woa_interp,1)
              call interpolator_end (woa_interp(n))
            end do        
          endif
          deallocate (woa_time)
      endif

      if (allocated (specified_woa)) deallocate (specified_woa)
      if (allocated (woa_offset   )) deallocate (woa_offset   )
      if (allocated (woa_entry    )) deallocate (woa_entry    )
      if (allocated (negative_offset  )) deallocate (negative_offset  )
      if (allocated (data_out_of_bounds))   &
                                      deallocate (data_out_of_bounds  )
      if (allocated (vert_interp      )) deallocate (vert_interp      ) 
      if (allocated (using_fixed_year_data))  &
                                     deallocate (using_fixed_year_data)

!--------------------------------------------------------------------
!    mark the module as uninitialized.
!--------------------------------------------------------------------
      module_is_initialized = .false.

!---------------------------------------------------------------------



end subroutine woa_end


!#####################################################################

subroutine set_woa_time (Model_time, Entry, woa_time)

type(time_type), intent(in)   :: Model_time, Entry
type(time_type), intent(out)  :: woa_time

      integer :: mo_yr, yr, mo, dy, hr, mn, sc, dum, dayspmn

      call get_date (Model_time, mo_yr, mo, dy, hr, mn, sc)
      call get_date (Entry, yr, dum,dum,dum,dum,dum)
      if (mo ==2 .and. dy == 29) then
        dayspmn = days_in_month(Entry)
        if (dayspmn /= 29) then
          woa_time = set_date (yr, mo, dy-1, hr, mn, sc)
        else
          woa_time = set_date (yr, mo, dy, hr, mn, sc)
        endif
      else
        woa_time = set_date (yr, mo, dy, hr, mn, sc)
      endif

!--------------------------------------------------------------------


end subroutine set_woa_time


!#####################################################################
! <SUBROUTINE NAME="woa_dealloc">
!  <OVERVIEW>
!    woa_dealloc deallocates the array components of an 
!    aersol_type derived type variable.
!  </OVERVIEW>
!  <DESCRIPTION>
!    woa_dealloc deallocates the array components of an 
!    aersol_type derived type variable.
!  </DESCRIPTION>
!  <TEMPLATE>
!   call woa_dealloc
!  </TEMPLATE>
! </SUBROUTINE>
!
subroutine woa_dealloc (woa)

!---------------------------------------------------------------------
!    woa_dealloc deallocates the array components of an 
!    aersol_type derived type variable.
!---------------------------------------------------------------------

type(woa_type), intent(inout) :: woa

!---------------------------------------------------------------------
!  intent(inout) variables:
! 
!      woa       woa_type variable containing information on
!                    the activated woa variables
!
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!    be sure module has been initialized.
!---------------------------------------------------------------------
      if (.not. module_is_initialized ) then
        call error_mesg ('woa_mod',   &
                         'module has not been initialized',FATAL )
      endif

!---------------------------------------------------------------------
!    deallocate the components of the woa_type variable.
!---------------------------------------------------------------------
      !!! if (allocated(woa%data)) deallocate (woa%data)
      !!! if (allocated(woa%woa_names)) deallocate (woa%woa_names)
      !!! if (allocated(woa%family_members)) deallocate (woa%family_members)
 
      deallocate (woa%data)
      deallocate (woa%woa_names)
      deallocate (woa%family_members)
!----------------------------------------------------------------------


end subroutine woa_dealloc


!#####################################################################

! <SUBROUTINE NAME="obtain_input_file_data">
!  <OVERVIEW>
!   obtain_input_file_data reads an input file containing a single
!    column woa profile. 
!  </OVERVIEW>
!  <DESCRIPTION>
!   obtain_input_file_data reads an input file containing a single
!    column woa profile. 
!  </DESCRIPTION>
!  <TEMPLATE>
!   call  obtain_input_file_data
!  </TEMPLATE>
! </SUBROUTINE>
!
subroutine obtain_input_file_data 

!---------------------------------------------------------------------
!    obtain_input_file_data reads an input file containing a single
!    column woa profile.
!---------------------------------------------------------------------

!--------------------------------------------------------------------
!  local variables:

      integer   :: iounit    ! unit to read file on
      integer   :: kmax_file ! number of levels of data in file
      integer   :: k         ! do-loop index
      character(len=31), dimension(200) :: dimnam
      integer(kind=4), dimension(200) :: dimsiz
      integer(kind=4)                 :: ncid, rcode, nvars, ndims, &
                                         ngatts, recdim
      integer    :: i, j
      integer, PARAMETER :: MAXDIMS = 10
      integer(kind=4), dimension(MAXDIMS) :: start, count, vdims
      integer(kind=4)                     :: ivarid, ntp, nvdim, nvs, &
                                             ndsize
      character(len=31)   dummy
      



!-------------------------------------------------------------------
!    determine if a netcdf input data file exists. if so, read the 
!    number of data records in the file.
!---------------------------------------------------------------------
      if (file_exist ( 'INPUT/id1aero.nc') ) then
        ncid = ncopn ('INPUT/id1aero.nc', 0, rcode)
        call ncinq (ncid, ndims, nvars, ngatts, recdim, rcode)
        do i=1,ndims
          call ncdinq (ncid, i, dimnam(i), dimsiz(i), rcode)
          if (dimnam(i) == 'lev') then
            kmax_file = dimsiz(i)
          endif
        end do
             
!-------------------------------------------------------------------
!    allocate space for the input data. read the data set. close the 
!    file upon completion.
!---------------------------------------------------------------------
        allocate (specified_woa(kmax_file) )
        ivarid = ncvid(ncid, 'woa', rcode)
        call ncvinq (ncid, ivarid, dummy, ntp, nvdim, vdims, nvs, rcode)
        do j=1,nvdim
          call ncdinq (ncid, vdims(j), dummy, ndsize, rcode)
          start(j) = 1
          count(j) = ndsize
        end do
       call ncvgt (ncid, ivarid, start, count, specified_woa, rcode)

         call ncclos (ncid, rcode)

!-------------------------------------------------------------------
!    determine if the input data input file exists in ascii format. if 
!    so, read the number of data records in the file.
!---------------------------------------------------------------------
      else if (file_exist ( 'INPUT/id1aero') ) then
        iounit = open_namelist_file ('INPUT/id1aero')
        read (iounit,FMT = '(i4)') kmax_file

!-------------------------------------------------------------------
!    allocate space for the input data. read the data set. close the 
!    file upon completion.
!---------------------------------------------------------------------
         allocate (specified_woa(kmax_file) )
         read (iounit,FMT = '(5e18.10)')   &
                          (specified_woa(k),k=1,kmax_file)
         call close_file (iounit)

!---------------------------------------------------------------------
!    if file is not present, write an error message.
!---------------------------------------------------------------------
       else
         call error_mesg ( 'woa_mod', &
              'desired woa input file is not present',FATAL)
       endif

!----------------------------------------------------------------------


end subroutine obtain_input_file_data 


!###################################################################### 



                  end module woa_mod 



!=======================================================================



#ifdef test_woa

program main

use woa_mod,          only: woa_type, woa_init, woa_time_vary, woa_driver, woa_end, woa_dealloc, &
                            Model_init_time, woa_time, &
                            do_column_woa, do_predicted_woa,do_specified_woa, &
                            column_woa_driver, predicted_woa_driver,specified_woa_driver
!!! use woa_mod
use sit_vdiff_mod,    only: sit_vdiff_init_1d,sit_vdiff,SICEDFN,lkvl,set_ocndepth,sit_nml, &
                          maskid,sit_zdepth,sit_fluxdepth,lwoa_gfdl, &
                          lprint0,lprint1,lprint2,bathy_default,    &
                          j3m,j10m,j100m
use mpp_mod
use mpp_io_mod
use mpp_domains_mod
use time_manager_mod
use diag_manager_mod
use rad_utilities_mod




implicit none

!-----------------------------------------------------------------------
! ... Local variables
!-----------------------------------------------------------------------
integer, parameter :: NLON=5, NLAT=5,NLEV=5
!integer, parameter :: NLON=20, NLAT=10,NLEV=1
integer, parameter :: MAX_AERSOL_NAMES = 100
real :: latb(NLON+1,NLAT+1),lonb(NLON+1,NLAT+1),pi
real, dimension(:,:,:), pointer:: phalf, p_flux

integer :: i,n
type(time_type) :: model_time
character(len=64), dimension(:), pointer :: woa_names => NULL()
character(len=64), dimension(:), pointer :: woa_family_names => NULL()
! character(len=64), dimension(MAX_woa_NAMES) :: names
type(woa_type)  :: woa
!!! character(len=64), dimension(:), pointer     :: woa_family_names
!!! real, dimension(:,:,:,:)  :: tracer
real, dimension(NLON,NLAT,NLEV+1,12)  :: tracer
!!! real,dimension(:,:,:,:),intent(inout)          :: r,rm       ! cjg: inout

pi = 4.*atan(1.)

call mpp_init
call mpp_io_init
call mpp_domains_init
call diag_manager_init
call set_calendar_type(JULIAN)

if (mpp_pe() == mpp_root_pe() ) then
  do i = 1,NLAT+1
     latb(:,i) = -90. + 180.*REAL(i-1)/REAL(NLAT)
  end do
  do i = 1,NLON+1
     lonb(i,:) = -180. + 360.*REAL(i-1)/REAL(NLON)
  end do
else
  do i = 1,NLAT+1
     latb(:,i) = -30. + 60.*REAL(i-1)/REAL(NLAT)
  end do
  do i = 1,NLON+1
     lonb(i,:) = -60. + 180.*REAL(i-1)/REAL(NLON)
  end do
endif


latb(:,:) = latb(:,:) * pi/180.
lonb(:,:) = lonb(:,:) * pi/180.

CALL set_ocndepth()
allocate (phalf(NLON,NLAT, size(sit_fluxdepth)))
allocate (p_flux(NLON,NLAT, size(sit_fluxdepth)))

do i = 1,size(sit_fluxdepth)
  phalf(:,:,i) = sit_fluxdepth(i-1)
  p_flux(:,:,i) = sit_fluxdepth(i-1)
enddo
      
!!! do i = 1,NLEV+1
!!!    phalf(:,:,i) = 101325. * REAL(i-1) / REAL(NLEV)
!!!    p_flux(:,:,i) = REAL(i-1)/REAL(NLEV)*4000.   
!!! end do

print *,"p_flux=",p_flux(1,1,:)

call woa_init (lonb, latb, woa_names, woa_family_names)
!call woa_init (lonb, latb, names, woa_family_names)

!model_time = set_date(1,1,1,0,0,0)
!model_time = Model_init_time
!model_time = set_date(9,1,1,0,0,0)
!!! model_time = set_date(1855,2,1,0,0,0)

do i =5, 5
  model_time = increment_date(Model_init_time,0,i,0,0,0,0,0)
  call print_date (model_time,  str= 'woa_time_vary: ')
  if (.false.) then
  !! origianl version
    call woa_time_vary (model_time)
    call print_date (woa_time(1),  str= 'woa_driver: ')
    call woa_driver (1,1,model_time, tracer, phalf, p_flux, woa,.true.)
  else if (do_column_woa) then
  !! seperated new version
  !!!  call woa_time_vary (model_time)
  !!!  call print_date (woa_time(1),  str= 'woa_driver: ')
    call column_woa_driver (1,1, model_time, phalf, woa)
  else if (do_specified_woa) then
  !! read woa data
  !!!  call woa_time_vary (model_time)
  !!!  call print_date (woa_time(1),  str= 'woa_driver: ')
    call specified_woa_driver (1,1, model_time, p_flux, woa)
  else if (do_predicted_woa) then  
    call predicted_woa_driver (1,1, model_time, tracer, phalf, woa,.true.)
  endif
enddo

do n=1,size(woa_names)
  print *, "pe=",mpp_pe(),"woa_names=",woa_names(n)    
  print *, "pe=",mpp_pe(),"woa=",woa%data(:,:,:,n)
enddo  

!!! do i =5,6
!!! !do i =-5,15
!!!   model_time = increment_date(Model_init_time,i,0,0,0,0,0,0)
!!!   call print_date (model_time,  str= 'woa_time_vary: ')
!!!   call woa_time_vary (model_time)
!!!   call print_date (woa_time(1),  str= 'woa_driver: ')
!!!   call woa_driver (1,1,model_time, tracer, phalf, p_flux, woa,.true.)
!!! enddo



!!! if (mpp_pe() == mpp_root_pe() ) then
!!!   print *, "woa_names=",woa_names
!!!   print *, "woa=",woa%data(:,:,:,:)  
!!! endif

!!! model_time = set_date(1855,3,1,0,0,0)
!!! model_time = set_date(10,1,1,0,0,0)
!!! call woa_time_vary (model_time)
!!! call woa_driver (1,1,model_time, tracer, phalf, p_flux, woa,.true.)
!!! call print_date (model_time,  str= 'date: ')
!!! if (mpp_pe() == mpp_root_pe() ) then
!!!   print *, "woa_names=",woa_names
!!!   print *, "woa=",woa%data(:,:,:,:)
!!! 
!!! !!!       allocate (woa%data(size(p_half,1),  &
!!! !!!                                 size(p_half,2), &
!!! !!!                                 size(p_half,3) - 1, nfields)) 
!!!                                   
!!! endif

call woa_dealloc (woa)

call woa_end

call mpp_exit

end program main

#endif
