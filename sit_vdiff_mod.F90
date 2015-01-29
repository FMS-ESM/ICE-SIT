#undef DEBUG
!!!#define DEBUG
#undef CTFREEZE
!!!#undef Prw001 T
!!!#undef Prw01
!!!#define Prw1
#define TMELTS0
#define SALTI0
!
#define GDCHK1 (.false.)
#define GDCHK2 (.false.)

#define DBLE REAL   ! EVERYTHING IN REAL

#define tmelt TFREEZE
MODULE sit_vdiff_mod

! 1. History
!
!     v0.1: July, 1998,  Ben-Jei Tsuang
!     v0.76g: 18 July 2001, Ben-Jei Tsuang
!     1) correct the bug of molecular heat diffusivity (1.34E-7 m2/s)
!     2) new lwaterlevel logic for fixed water level run
!     3) set heat diffusivity within the surface viscous sublayer to be
!        the molecular heat diffusivity
!     v0.76i: 18 July 2001, Ben-Jei Tsuang
!     1) modify diffusivity of skin layer to be the geometric mean between molecular diff and XK(0)
!     2) modify radiation parameterization of skin layer of thickness "d"
!     v0.76j: 18 July 2001, Ben-Jei Tsuang
!     1) reset the diffusivity of skin layer to be XK(0)
!     v0.76k: 18 July 2001, Ben-Jei Tsuang
!     1) set radiation param. to be dFFN
!     v0.76l: 18 July 2001, Ben-Jei Tsuang
!     1) reset radiation param. to be the thickness of effective thickness
!     v0.76i: 18 July 2001, Ben-Jei Tsuang
!     1) reset radiation param and skin layer diff to be that in v.76i
!     v0.76m: 24 July 2001, Ben-Jei Tsuang
!     1) modify DTDZ to DRHODZ for stabiliy criterion
!     v0.76n: 25 July 2001, Chia-Ying Tu
!     1) ambient diffusion coefficeint is added
!     v0.76o: 25 July 2001, Chia-Ying Tu
!     1) minimum 0.3 m of mixing length is chosen
!     v0.77: 1 Aug 2001,
!     1) calibrate Prandtl number
!     v0.78: emin, EResRatio
!     v0.90: 9 Jan 2003, Chia-Ying Tu
!     1) add mo_thmcln
!     2) xkmmin xkhmin hcoolskin hcoolskin emin are set from arguments
!     3) use Inverse Problem to determine above values
!     v1.00: 29 July 2007, Ben-Jei Tsuang
!     1) port to ECHMA-5.4.00
!     2) modify heice and hesn
!     v1.30: 1 September 2008, Ben-Jei Tsuang
!     1) Modify Water Level Logical for lsoil
!     v3.2: 26 August 2008, Ben-Jei Tsuang
!     1) modify coordinate system according to Tsuang et al. (2009)
!        "A more accurate scheme for calcualting Earth's skin temperature",
!        Climate Dynamics
!     v5.0: June 2009, Ben-Jei Tsuang
!     1) Add below ice surface heat/fresh water fluxes for coupling with embedded ocean model
!     2) Add sufsurface (at about 10-20 m depth) heat/salinity fluxes for coupling with embedded ocean model
!     3) Turn on the ice module with security number for coupling with embedded ocean model
!     v5.1: June 2009, Ben-Jei Tsuang
!     1) A semi-implicity scheme for snow/ice temperature (TSI) coupling with vdiff (atmosphere)
!     v5.3: July 2009, Ben-Jei Tsuang
!     1) A new density function of pressure, temperature and salinity is needed for
!        vertical diffusion calculation
!     2) Bug fixed for density function
!     v7.7: snow/ice temperate set to be the underneach temp if these layers are missing. (bjt,2010/5)
!     v7.9 (bjt, 2010/5/29)
!     1) sit_vdiff.f90: seaice mask: Winner wins!
!     v8.7 (bjt, 2011/8/29)
!      Assunimg no skin layer for momentm since wind shear can enforce on the side. Note that the surface heat flux is from the top. (2011/8/29 bjt)
!     v9.8b          zcor=MAX(ABS(pcoriol),zepcor)  ! v9.8b: 20130822  (restore the corlios security number)
!     v9.86: bug correction: initialize pwtke
!     v9.84 (Same as v9.83 + xlkmin=0.) (bjt, 2013/9/10)
!     v9.85: salti  (seaice is salty at salti PSU) (bjt, 2013/9/11)
!     v9.86: emin  (truncate tke to 0, for tke < emin, 10-6 ms/s2) (bjt, 2013/9/13)
!     v9.862: rhom(jk)=rho_from_theta(wsm(jk),wtm(jk)-tmelt,0.) (bjt, 2013/9/15), change to potential water density
!     v9.864: CALL nudging_sit_viff_gd(six_hour,six_hour,six_hour,six_hour,six_hour,.TRUE.) (bjt, 2013/9/16), nudging within 10 m depth as well    
!     v9.865: v9.864+no truncate low tke+no contrain in huge tke, emin=1.0E-6 (bjt, 2013/9/18)
!     v9.866: emin=1.0E-4 !limit min pwtke to 1.0E-4 (v9.866)
!     v9.867: emin=1.0E-5 !limit min pwtke to 1.0E-5 (v9.867)
!     v9.868: emin=1.0E-4 !limit min pwtke to 1.0E-4 (v9.868)
!     v9.869: CALL nudging_sit_viff_gd(six_hour,six_hour,one_month,six_hour,six_hour,.TRUE.) (bjt, 2013/9/16), nudging > 100 m depth at 1-month time scale
!     v9.871: Add d0 for reaching the bottom d0=0.03  ! zero-displacement (m) (0.02,0.05)
!     v9.873: emin=1.0E-5 !limit min pwtke to 1.0E-5 (v9.873)
!     v9.874: change drho/dz from potentail density at surface to the potential density diff at the level (v9.874)
!             real,PARAMETER::emin=1.0E-4 !limit min pwtke to 1.0E-4 (v9.866),(v9.874)
!     v9.880: 1) bug found for ! bug, v0.9879
! &             -0.5_dp*ce*zdtime*wtkem(level)**(1.5_dp)/pwldisp(level)                   &   ! bug, v0.9879
!             2) emin=1.0E-5 !limit min pwtke to 1.0E-5 (v9.873)
!     v9.884: bug correction for RHOE
!     v9.886: 1)bug correction for pwkm=ck*lk*e^0.5, then pwkh=pwkm/Pr
!             2) introducing steady TKE
!     v9.892: 1) with prho1000 output
!     v9.893: 1) nudging for deep salinity for coupled model during spinup period
!     v9.8999:
!     v10.4:  1) partial open water and partial ice sheet are revised by changing  
!                0.5*hsn to 0.5*hsn/seaice 
!                0.5*hice to 0.5*hice/seaice 
!
! 11. REFERENCE
!     Gaspar, P., Y. Gregoris, and J.-M. Lefevre,1990: A simple eddy
!         kinetic energy model for simulation of the oceanic vertical
!         mixing: test at station Papa and long-term upper ocean study
!         site. JGR, 95, 16179-16193.
!     Tu, C.-Y.; Tsuang, B.-J., 2005/11: Cool-skin simulation by a one-column
!         ocean model, Geophys. Res. Lett., 32, L22602, doi:10.1029/2005GL024252.
!     Tsuang, B.-J., C.-Y. Tu, J.-L. Tsai, J.A. Dracup, K. Arpe and T. Meyers, 2009:
!         A more accurate scheme for calculating Earth's skin temperature.
!         Climate Dynamics, DOI 10.1007/s00382-008-0479-2, on-line version available.
!
! 12. Bug known
!     Coriols force needed to be corrected for considering the curvature of the Earth
!  ---------------------------------------------------------------------
!
  use mpp_mod, only: mpp_init, mpp_pe, mpp_root_pe, &
                   stdlog, mpp_error, NOTE, FATAL, WARNING
  USE eos_ocean_mod,      ONLY: tmaxden,tmelts,rho_from_theta,theta_from_t
  USE eos_ocean_mod,      ONLY: api,argas,avo,ak,stbo,amco2,amch4,amo3,amn2o,amc11,amc12,amw,amd,cpd,cpv,rd,rv,rcpd,vtmpc1,vtmpc2
  USE eos_ocean_mod,      ONLY: rhoh2o,alv,als,alf,clw
  USE eos_ocean_mod,      ONLY: a,omega,g,IDAYLEN
  USE eos_ocean_mod,      ONLY: c1es,c2es,c3les,c3ies,c4les,c4ies,c5les,c5ies,c5alvcp,c5alscp,alvdcp,alsdcp
  ! Missing Value
  use constants_mod, only: xmissing, real_missing, int_missing, RADIAN, GRAV, TFREEZE  
                                   
  IMPLICIT NONE

!--------------------------------------------------------------------------
! from mo_kinds
!----------------------

  ! Number model from which the SELECTED_*_KIND are requested:
  !
  !                   4 byte REAL      8 byte REAL
  !          CRAY:        -            precision =   13
  !                                    exponent  = 2465
  !          IEEE:    precision =  6   precision =   15  
  !                   exponent  = 37   exponent  =  307 
  !
  ! Most likely this are the only possible models.

  ! Floating point section 

  INTEGER, PARAMETER :: sp = SELECTED_REAL_KIND(6,37)  
  INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(12,307)
  !INTEGER, PARAMETER :: dp = 4                            ! single precision
  INTEGER, PARAMETER :: wp = dp   ! working precision

  ! Integer section

  INTEGER, PARAMETER :: i4 = SELECTED_INT_KIND(9)
  INTEGER, PARAMETER :: i8 = SELECTED_INT_KIND(14)

  ! Missing Value
  
  !!! real, PARAMETER:: xmissing= -9e+33_dp     ! default missing value
  !!! REAL,     PARAMETER:: real_missing= -9e+33    ! default missing value for REAL
  !!! INTEGER,  PARAMETER:: int_missing= -99999     ! default missing value for INTEGER  

  !--------------------------------------------------------------------------
  ! 
  ! USE mo_netcdf,         ONLY: lkvl,                &  ! lkvl=39 : number of water layers
  !                              nfnlvl,              &  ! nfnlvl=12, number of fine layers in sit for the uppermost ocean layer
  !                              sit_zdepth,sit_fluxdepth  
  !----------------------

  INTEGER :: nfnlvl        ! =12, number of fine layers in sit for the uppermost ocean layer
  INTEGER :: lkvl          ! =49, =ocn_k1+nfnlvl-3, i.e., lkvl=49, ocn_k1-2=lkvl+1-nfnlvl
                           ! number of vertical levels of sit ocean model 
!!!  INTEGER :: ocn_k1        != 40, number of ocean ocean levels

  real, POINTER  :: ocn_z(:)
  real, POINTER  :: diecast_zdepth(:),diecast_fluxdepth(:)     ! unit: m
  real, POINTER  :: sit_zdepth(:),sit_fluxdepth(:)             ! unit: m
  real, POINTER  :: sit_gribzdepth(:),sit_gribfluxdepth(:)     ! sit_gribzdepth (for grib format)! bjt
  INTEGER:: j3m                                                ! depth j in (0:lkvl+1) for 3 m depth
  INTEGER:: j10m                                               ! depth j in (0:lkvl+1) for 10 m depth
  INTEGER:: j100m                                              ! depth j in (0:lkvl+1) for 100 m depth

  !--------------------------------------------------------------------------
  ! USE mo_sst,            ONLY: csn,rhosn,xksn,cice,rhoice,xkice,xkw,              &
  !                             omegas,wcri,wlvlref,dpthmx

  real :: albice = 0.2,             & ! albedo of glacier ice (0.2 - 0.4) (Pielke, 1984). Lake ice is the smallest.
          albsn  = 0.7,             & ! albedo of snow decreases with age (0.4 - 0.95) (Pielke, 1984)
          albw   = 0.06,            & !	  (Brutsaert, 1982; Tsuang, 1990; Gaspar et al., 1990)
          csn    = 2116.,           &
          cice   = 2116.,           & ! Cw = 4217.7-2.55*(Tw-tmelt) (4178.4 is used)
                                      ! csn = cice=104.369+7.369*TSN (Marks, 1988)
                                      ! csn = 2116 j kg k-1 at 0 ! is used for simplicity.
          rhosn  = 300.,            & !	rhosn = dry snow density. Although snow density changes with age,
                                      !	it is not sensitive to snowmelt runoff. A constant value is assumed.
          rhoice = 917.,            & !	rhoice = density of ice (917 kg/m3)
          omegas = 2.*API/IDAYLEN,  & ! ANGULAR VELOCITY OF EARTH in respect to sun
          xkice = 1.2E-6,           & ! Dickinson et al. (1986)
          xksn = 2.0E-7,            & !	  Effect heat diffusivity of snow
                                      !       (conduction heat diffusivity + vapor transfer)
                                      ! xksn=kcon+De*Lv/cp*dqsat/dT
                                      !	kcon (conduction) = (3.2238E-8)*rhosn/csn (Yeh, 1965)
                                      ! This value increases with age of snow, and is an
                                      ! important tunning parameter for snow melting rate.
                                      !   =(1E-7 ~ 4E-7) (Dickinson et al., 1986)
          xkw = 1.50E-4,            & !  (Kondo, 1979, Tsuang, 1990)
          wcri=0.1                    !     minimum thickness of a water layer. Thickness less than
                                      !     this thickness is treated as thin layer. That is T,s,U,V,
                                      !     TKE is the same as the layer underneath.


  !! memory pointer for GODAS+Ishii WORLD OCEAN data (lwoa_echam) (GODAS+Ishii)
  INTEGER               :: nodepth        ! number of depths of the godas data (=24)
  real, ALLOCATABLE :: odepths(:)     ! depths of the godas data (m)  
  real, ALLOCATABLE :: ot12(:,:) ! (nlon,nodepth,ngl,0:13) in global coordinates,
                                          ! observed water tempeature profile (K): "ot"       
  real, ALLOCATABLE :: os12(:,:) ! (nlon,nodepth,ngl,0:13) in global coordinates,
                                          ! observed salinity (0/00): "os"
  real, ALLOCATABLE :: ou12(:,:) ! (nlon,nodepth,ngl,0:13) in global coordinates,
                                          ! observed u current (m/s): "ou"
  real, ALLOCATABLE :: ov12(:,:) ! (nlon,nodepth,ngl,0:13) in global coordinates,
                                          ! observed v current (m/s): "ou"
                                          
  !! memory pointer for Initial WORLD OCEAN ATLAS 2005 data (lwoa0_echam)(http://www.nodc.noaa.gov/OC5/WOA05/pr_woa05.html)
  INTEGER           :: nodepth0      ! number of depths of the woa data (=24)
  real, ALLOCATABLE :: odepth0(:)    ! depths of the woa data (m)  
  real, ALLOCATABLE :: ot0(:)    ! (nlon,nodepth0,ngl,0:13) in global coordinates,
                                     ! observed water tempeature profile (K): "ot"       
  real, ALLOCATABLE :: os0(:)    ! (nlon,nodepth0,ngl,0:13) in global coordinates,
                                     ! observed salinity (0/00): "os"
  real, ALLOCATABLE :: ou0(:)    ! (nlon,nodepth0,ngl,0:13) in global coordinates,
                                     ! observed u-componet current (m/s): "ou"
  real, ALLOCATABLE :: ov0(:)    ! (nlon,nodepth0,ngl,0:13) in global coordinates,
                                     ! observed v-componet current (m/s): "ov"                                        
  LOGICAL :: lou=.FALSE.             ! u- current available ?
  LOGICAL :: lov=.FALSE.             ! v- current available ?
  
  !! memory pointer for sit flux correction term
  INTEGER           :: nwdepth         ! number of depths of the woa data
  real, ALLOCATABLE :: wdepths(:)      ! depths of the woa data (m)  
  real, ALLOCATABLE :: wtfn12(:,:) ! (nlon,nwdepth,ngl,0:13) in global coordinates,
                                          ! observed water tempeature profile (K): "ot"       
  real, ALLOCATABLE :: wsfn12(:,:) ! (nlon,nwdepth,ngl,0:13) in global coordinates,
 
                               
  !--------------------------------------------------------------------------
  !   USE mo_time_control,   ONLY: delta_time, lstart, get_time_step, current_date,   &
  !                             write_date, lobs_ocn_rerun, ltrigsit  
  !-----------------------
  !!! INTEGER :: nn             !   max meridional wave number for m=0.
  !!! LOGICAL, SAVE :: lrere = .FALSE. !   .true. for IC (Initial Condition) run (forcasting mode),
  !!! LOGICAL :: lhd      = .FALSE.  !   .true. for hydrologic discharge model
  !!! LOGICAL :: ltrigsit = .false.     ! need check GFS


  !---------------------------------------------------------------------
  !   USE mo_control,        ONLY: nn,lsit,lssst,lsit_ice,lsit_salt,lhd,                      &
  !                                sit_ice_option,locaf,lwoa_echam,lrere,locn,                    &
  !                                lsice_nudg,lsit_lw,                                        &
  !                                ssit_restore_time,usit_restore_time,dsit_restore_time,     &
  !                                lwarning_msg,ocn_couple_option,                            &
  !                                socn_restore_time,uocn_restore_time,docn_restore_time,     &
  !                                obox_restore_time,obox_nudg_flag,Prw,csiced
  !   

  ! 3.0 SIT variables
  !   3.1 I/O
  INTEGER :: nwoa0            = 97   !  *nwoa0*     logical unit for world ocean atlas profile file (bjt), initial ocean field
  INTEGER :: ngodas           = 98   !  *ngodas*    logical unit for GODAS dataset (bjt), time series ocean field
  INTEGER :: nocaf            = 99   !  *nocaf*     logical unit for read_ocaf (bjt), flux correction terms
  INTEGER :: nrere            = 93  !  *nrere*      logical unit for ReReAnalysis run (xl and xi) (bjt)
  !   3.2 logical variables
  LOGICAL, SAVE :: lasia      = .FALSE. ! .true. for using etopo, new land surface data over rice paddy and Tibet
  !!! LOGICAL, SAVE :: lsit       = .FALSE. ! .true. for calculation of upper ocean temperature profile using sit model
  LOGICAL, SAVE :: lsice_nudg = .FALSE. ! .true. for nudging siced and seaice in SIT  
  LOGICAL, SAVE :: lsit_lw    = .FALSE.  ! .true. for turning LW code for water in SIT  
  LOGICAL, SAVE :: lsit_ice   = .TRUE.  ! .true. (default) for turn on the ice module of sit model
  LOGICAL, SAVE :: lsit_salt  = .TRUE.  ! .true. (default) for turn on salinity module of sit model
  LOGICAL, SAVE :: lzgodas    = .FALSE. ! .true.= godas z cord, fine in thermocline, 10 m apart within 100-225 m, but coarse (~200 m) in deep-water formation depths (>500 m).  
                                        ! (default: .false.)
  real, SAVE :: ocn_tlz=5800.    ! depth of bottom z-level (m) of ocean model (v9.8992, 2013/10/3) (=5000. prior to v9.8992)
  INTEGER, SAVE :: ocn_k1=40            ! vertical dimension parameter (number of layer interfaces, equal
                                        ! the number of layers or pressure levels, does not include ghost zone)
  LOGICAL, SAVE :: lssst          = .TRUE.  ! .true. for turnon thermocline skin layer, .false. for turnoff thermocline skin layer
  LOGICAL, SAVE :: lwoa_gfdl      = .TRUE. ! .true. for reading world ocean atlas (woa) data for sit model (for GFDL HiRAM coupler only) 
  LOGICAL, SAVE :: lwoa_echam     = .FALSE. ! .true. for reading world ocean atlas (woa) data for sit model  (for ECHAM only) 
  LOGICAL, SAVE :: lwoa0_echam    = .FALSE. ! .true. for reading initial ocean profile (unit: 97)          (for ECHAM only)
  INTEGER, SAVE :: lwarning_msg = 1     !  or printing warsning message
                                        !   =0, no message
                                        !   =1, basic message
                                        !   =2, medium message
                                        !   =3, many message
  LOGICAL, SAVE :: locaf = .FALSE.             ! .true. for q flux adjustment
  LOGICAL, SAVE :: lcook_skin = .TRUE.        ! .true. for cool-skin parameterization
! LOGICAL, SAVE :: lcook_skin = .FALSE.        ! .true. for cool-skin parameterization
  LOGICAL, SAVE :: lsteady_TKE=.TRUE.          ! .true. = using calc_steady_TKE for TKE 
  LOGICAL, SAVE :: lwave_breaking=.FALSE.      !
  
  !   3.3 other variables  
  INTEGER, SAVE :: sit_ice_option  = 2  !   ice option in sit (i.e., calc. of snow/ice) (=0, off; >=1, on) 
                                        !   0: for coupling with vdiff semi-implicitly for tsi calculation (default)
                                        !   1: explcity coupling with strong security number
                                        !   2: original SIT output, no security. It can be crashed in few time steps
  INTEGER, SAVE :: maskid  = 1          !   0: DIECAST grids only
                                        !   1: Ocean and lakes (default)
                                        !   2: Ocean
                                        !   3: Ocean within 30N-30S
                                        !   4: all the grid                                       
  real:: ssit_restore_time =xmissing ! surface (0 <= ~ <10 m) sit grids restore time scale (s) (default: no nudging)
  real:: usit_restore_time =86400. ! upper ocean (10 <= ~ <100 m) sit grids restore time scale (s) (default: no nudging)
  real:: dsit_restore_time =86400. ! deep (>=100 m) restore time scale (s)  (default: no nudging)

  ! 4. 3-D Diecast Ocean Model
  !   4.1 I/O
  INTEGER :: nocn_sv          = 51              ! *nocn_sv*         logical unit for ocn_sv file  
  INTEGER, SAVE :: etopo_nres=1                 ! 1 for etopo1 (1min) (default), 2 for etopo2 (2min), and 5 for etop5 (5min)
  !   4.2 logical variables
  LOGICAL, SAVE :: locn  = .FALSE.              ! .true. for embedded 3-D ocean
  LOGICAL, SAVE :: locn_msg  = .FALSE.          ! .true. for writing embedded 3-D ocean fields
  LOGICAL, SAVE :: lopen_bound=.FALSE.          ! .TRUE. = open boundary condition (set depth at j=jos0,jon0 according to its physical depth
                                                ! .FALSE.= closed boundary condition (set depth to be 0 for J=jos0,jon0
  LOGICAL, SAVE :: lall_straits=.TRUE.          ! .FALSE.= only open Gibraltar Strait
  LOGICAL, SAVE :: lstrict_channel=.TRUE.       ! .TRUE.  too strick, less vent in Gibraltar
                                                ! .FALSE. too loose, open Central Ameican, Phillipine  
  !   4.3 other variables
  real:: ratio_dt_o2a=1.                 ! ratio_delta_time_and_ocn_dt (fractional)
  real:: ocn_domain_w  = 0.              ! west coords (lon) of embedded ocean [-180., 360.](deg).
  real:: ocn_domain_e  = 360.            ! east coords (lon) of embedded ocean [-180., 360.](deg).
  real:: ocn_domain_s  = -80.            ! south coords (lat) of embedded ocean [-180., 360.](deg).
  real:: ocn_domain_n  = 80.             ! north coords (lat) of embedded ocean [-180., 360.](deg).
  INTEGER, SAVE :: ocn_lon_factor=1             ! number of grid per dx in ECHAM resolution. 
  INTEGER, SAVE :: ocn_lat_factor=1             ! number of grid per dy in ECHAM resolution. 
  INTEGER, SAVE :: ocn_couple_option = 0        ! 0: put awust2,awvst2 and full-level T,s coupling with 3-D ocean, and T,u,v,s back to SIT.
!                                               ! 1: put afluxs(G0),awust2(ustr),awvst2(vstr),awfre(P-E) of echam to ocean, and all the T,u,v,s back to ECHAMS,
!                                               !    with sea ice correction (fluxiw rather than fluxs)
!                                               ! 2: Same as option 1, but also with sitwkh to ocean
!                                               ! 3: Same as option 2, but also with sitwkm to ocean
!                                               ! 4: Same as option 3, but without secruity number for diffusivity
!                                               ! 5: Same as option 1, but don't feedback anything back to sit.
!                                               ! 6: Same as option 5, but don't put awust2(ustr),awvst2(vstr) of echam to ocean.
!                                               ! 7: put wind stress, 1st layer sst and salinity of echam to ocean,
!                                               !    but don't feedback anything back to sit.
!                                               ! 8: put awust2,awvst2 of echam to ocean, but nothing back to sit.
!                                               ! 9: put wtb,wsb,awust2,awvst2,subfluxw,wsubsal of echam to ocean, and T,u,v,s back to SIT.
!                                               !10: Same as option 0, but skip TX,TY,TZ,SX,SY,SZ in ocn_stepon
!                                               !11: full-level (T,u,v,s) 2-way coupling with 3-D ocean, and T,u,v,s back to SIT.
!                                               !12: put nothing of echam to ocean, and nothing back to sit.
!                                               !13: Same as option 1, but T,S initialized by DIECAST
!                                               !14: Same as option 1, but without sea ice correction (fluxs rather than fluxiw)
!                                               !15: Same as option 10, + TX,SX 
!                                               !16: Same as option 10, + TY,SY
!                                               !17: Same as option 10, + TZ,SZ
  INTEGER :: high_current_killer = 4        ! 1: based on bottom vorticity: damp_high_vel(1:lnlon,1:lnlat,1:nh)=DAMP_high_current*MERGE( 1.,0.,ABS(vor_bottom(1:lnlon,1:lnlat,1:nh)).GT.VOR_cri )
                                            ! 2: based on bottom vertical velocity: damp_high_vel(i,j,ih)=DAMP_high_current*MERGE( 1.,0.,ABS(W(i,j,KB(i,j,ih),ih)).GT.W_cri ) 
                                            ! 3: based on bottom vorticity: damp_high_vel(1:lnlon,1:lnlat,1:nh)=DAMP_high_current*( ABS(vor_bottom(1:lnlon,1:lnlat,1:nh))/VOR_cri )
                                            ! 4: based on bottom vertical velocity: damp_high_vel(i,j,ih)=DAMP_high_current*( ABS(W(i,j,KB(i,j,ih),ih))/W_cri )
  real:: socn_restore_time =xmissing        ! surface (0 <= ~ <10 m) ocn grids restore time scale (s) (default: no nudging)
  real:: uocn_restore_time =xmissing        ! upper ocean (10 <= ~ <100 m) ocn grids restore time scale (s) (default: no nudging)
  real:: docn_restore_time =xmissing        ! deep (>=100 m) restore time scale (s)  (default: no nudging)
!  
  INTEGER :: nobox_nudg= 0                  ! number of nudg squares in ocean grids (default = 0, maximun=6)
  real:: obox_restore_time=xmissing         ! restore time scale (s) in all depths for iop_ocnmask>0 grids (default: no nudging)
  INTEGER :: obox_nudg_flag= 0              ! 0: no nudging, 1: nudging inside square boxes, 2: nudging outside the square boxes
  real:: obox_nudg_w(6)= -999.              ! west coords (lon) of nudging boxes [-180., 360.](deg). There are 6 boxes.
  real:: obox_nudg_e(6)= -999.              ! east coords (lon) of nudging boxes [-180., 360.](deg). There are 6 boxes.
  real:: obox_nudg_s(6)= -999.              ! south coords (lat) of nudging boxes [-180., 360.](deg). There are 6 boxes.
  real:: obox_nudg_n(6)= -999.              ! north coords (lat) of nudging boxes [-180., 360.](deg). There are 6 boxes.
  real:: kocn_dm0z=1.                       ! =1 (default), ratio to near shore momentum diffusivity (m2/s) for preventing generating near-shore vortex
  INTEGER::  ncarpet=1                      ! number of coastal grid for carpet filter. =0, no carpet filter; =1 for N2 (default); =2 for N4; =3 for N6
  real:: kcsmag=1.                          ! =1, ratio to Smagorinsky horizontal diff coeff.
                                            ! Smagorinsky, J. General circulation experiments with the primitive equations, 
                                            !   I. The basic experiment. Monthly Weather Rev. 1963, 91, 99-164.
  real:: kalbw=1.                           ! =1, ratio to solar albedo over water by 
                                            ! Li J. J. Scinocca M. Lazare N. McFarlane K. von Salzen L. Solheim 2006
                                            !   Ocean Surface Albedo and Its Impact on Radiation Balance in Climate Models.
                                            !   J. Climate 19 6314¡V6333.
                                            ! Taylor, J. P., J. M. Edwards, M. D. Glew, P. Hignett, and A. Slingo, 1996:
                                            !   Studies with a flexible new radiation code. II: Comparisons with aircraft short-wave
                                            !   observations. Quart. J. Roy. Meteor. Soc., 122, 839¡V861.
  real:: Prw=1.                             ! =1, Prandtl number in water (sit_ocean.f90)                    
  real:: csl=-27.                           ! =-27 m (default), the present Caspian Sea Level (CSL) is about -27m and during the medieval time it was about -30m
  real:: por_min=0.1_dp                     ! = minimum porisity for setting as ocean grid
  real:: csiced=0.                          ! =0., threshold sea ice depth, seaice[csiced]=0.5, seaice[2*csiced]=0.84
  real:: bathy_default=-200.                ! =-200., default bathy (m).
  
! from mo_time_control

!!!  real           :: delta_time    = 0.0_dp ! distance of adjacent times
  real           :: time_step_len = 0.0_dp ! forecast time step,
                                               ! at beginning equal delta_time
                                               ! else 2*delta_time

  ! dt_initial  corresponds to the initial condition date
  ! dt_start    defines the start of an experiment
  !
!!!  TYPE(time_days),SAVE :: initial_date       ! should set at initial time from file
  !
  INTEGER, PARAMETER   :: INIT_STEP   = 0    ! initial time step
  INTEGER              :: dt_start(6) = 0    ! (runctl) start date of experiment
                                             ! meaning (yr, mo, dy, hr, mi, se)
!!!  TYPE(time_days),SAVE :: start_date         ! transformed start date
!!!  LOGICAL              :: lstart    = .TRUE. ! .TRUE. for the first time step

  LOGICAL            :: lfirst_day = .TRUE.  ! .TRUE. during the first day
  LOGICAL            :: l2nd_day   = .TRUE.  ! .TRUE. during the first+second day

  INTEGER              :: dt_resume(6) = 0   ! user defined restart time

  INTEGER              :: dt_stop(6) = 0     ! (runctl) stop experiment here
                                             ! meaning (yr, mo, dy, hr, mi, se)
!!!  TYPE(time_days),SAVE :: stop_date          ! transformed stop date
  LOGICAL              :: lbreak   = .FALSE. ! .TRUE. at end of one time segment
  LOGICAL              :: lstop    = .FALSE. ! .TRUE. during the last time step
  LOGICAL              :: labort   = .TRUE.  ! .TRUE. return error signal at end
!!!  LOGICAL              :: lwarmstart   = .FALSE.  ! .TRUE. read rerun file but modified with obs. ocean/atm/land data
  LOGICAL              :: lobs_ocn_rerun   = .FALSE.  ! .TRUE. read rerun file but modified with obs. ocean/atm/land data
  LOGICAL              :: lresume = .FALSE.  ! .TRUE. during rerun step
!!!  TYPE(time_days),SAVE ::  resume_date       ! transformed rerun date

!!!  TYPE(time_days),SAVE ::  previous_date     ! date at (time - delta_time)
!!!  TYPE(time_days),SAVE ::   current_date     ! date at (time)
!!!  TYPE(time_days),SAVE ::      next_date     ! date at (time + delta_time)

!!!  TYPE (time_days),SAVE :: radiation_date    ! date corresponding to rad_calc
  LOGICAL           :: l_orbvsop87 = .TRUE.  ! .TRUE. : orbit routine from vsop87
  LOGICAL           :: lprint0 = .FALSE.  ! .TRUE. : for printing debugging inforation (GDCHK0) 
  LOGICAL           :: lprint1 = .FALSE.  ! .TRUE. : for printing debugging inforation (GDCHK1) 
  LOGICAL           :: lprint2 = .FALSE.  ! .TRUE. : for printing debugging inforation (GDCHK2)

!--------------------------------------------------------------------------
! from mo_time_event
!-----------------------
  INTEGER, PARAMETER :: STR_LEN_A = 20
  !+
  ! **************** parameters ------------------------------------------------
  !
  ! TIME_INC_*     predefined counter units
  !
  CHARACTER(len=*), PUBLIC, PARAMETER :: &
       TIME_INC_SECONDS = 'seconds'  ,&!
       TIME_INC_MINUTES = 'minutes'  ,&!
       TIME_INC_HOURS   = 'hours'    ,&!
       TIME_INC_DAYS    = 'days'     ,&!
       TIME_INC_MONTHS  = 'months'   ,&!
       TIME_INC_YEARS   = 'years'      !

  ! TRIG_*  type of trigger adjustment of an event
  !
  CHARACTER(len=*), PUBLIC, PARAMETER :: &
       TRIG_FIRST  = 'first'  ,&! trigger in first step of counter unit
       TRIG_LAST   = 'last'   ,&! trigger in last step of counter unit
       TRIG_EXACT  = 'exact'  ,&! trigger without adjustment in side the unit
       TRIG_NONE   = 'off'     ! event trigger non active

  ! **************** structures ------------------------------------------------
  !
  TYPE, PUBLIC :: io_time_event            ! external given event properties
    INTEGER                  :: counter    = 0                ! No. of steps in given unit
    CHARACTER(len=STR_LEN_A) :: unit       = TIME_INC_SECONDS ! counter unit type
    CHARACTER(len=STR_LEN_A) :: adjustment = TRIG_EXACT       ! adjustment in side the unit
    INTEGER                  :: offset     = 0            ! offset to initial date in seconds
  END TYPE io_time_event


!--------------------------------------------------------------------------
! from mo_time_control
!-----------------------

  ! the adjustment of events for RERUN is dependent on the trigger step
  ! the trigger step can be the present date or the next date
  !
  CHARACTER(len=*), PARAMETER, PRIVATE :: &
       EV_TLEV_PRES    = 'present'        ,&! check event with present date
       EV_TLEV_NEXT    = 'next'           ,&! check event with next date
       TIME_INC_STEPS  = 'steps'          ,&! special event interval unit
       TIME_INC_ALWAYS = 'always'           ! special event always used  


  TYPE(io_time_event),SAVE ::    trigsit       &! time interval for triggerring sit_ocean
       = &                                      ! model
       io_time_event(1,TIME_INC_STEPS,TRIG_EXACT,0) ! every one time step
  TYPE(io_time_event),SAVE ::    trigocn       &! time interval for triggerring embedded ocean
       = &                                      ! model (every 1 time step)
       io_time_event(1,TIME_INC_STEPS,TRIG_EXACT,0)
 
!--------------------------------------------------------------------------
!  USE convect_tables_mod
!-----------------------

  ! Lookup tables for convective adjustment code
  !
  ! D. Salmond, CRAY (UK), August 1991, original code
  ! L. Kornblueh, MPI, April 2003, cleanup and move of table setup code
  !                                from setphys.f90 in module  
  !

  !!! USE mo_kind,   ONLY: dp

  !!! IMPLICIT NONE
  !!! ! Standard i-o units
  !!! 
  !!! INTEGER, PARAMETER :: nout = 0     ! standard output stream
  !!! INTEGER, PARAMETER :: nerr = 0     ! error output stream
  !!! INTEGER, PARAMETER :: nin  = 5     ! standard input stream
  !!! 
  !!! INTEGER, PARAMETER:: dp=4
  !!! 
  !!! SAVE
  !!! 
  !!! PRIVATE

  ! variables public

  PUBLIC :: jptlucu1          ! lookup table lower bound
  PUBLIC :: jptlucu2          ! lookup table upper bound
  PUBLIC :: tlucua            ! table -- e_s*Rd/Rv
  PUBLIC :: tlucub            ! table -- for derivative calculation: d es/ d t
  PUBLIC :: tlucuc            ! table -- l/cp
  PUBLIC :: tlucuaw           ! table
  PUBLIC :: lookupoverflow    ! lookup table overflow flag

  ! subroutines public

  PUBLIC :: init_convect_tables ! initialize LUTs 
  PUBLIC :: lookuperror         ! error handling routine 

  INTEGER, PARAMETER :: jptlucu1 =  50000  ! lookup table lower bound
  INTEGER, PARAMETER :: jptlucu2 = 400000  ! lookup table upper bound

  LOGICAL :: lookupoverflow = .FALSE.          ! preset with false
  
  real :: tlucua(jptlucu1:jptlucu2)        ! table - e_s*Rd/Rv
  real :: tlucub(jptlucu1:jptlucu2)        ! table - for derivative calculation
  real :: tlucuc(jptlucu1:jptlucu2)        ! table - l/cp
  real :: tlucuaw(jptlucu1:jptlucu2)       ! table

!--------------------------------------------------------------------------
!   USE mo_doctor,         ONLY: nout, nin, nerr
!-----------------------

  ! Standard i-o units

!!!  INTEGER, PARAMETER :: nout = 0     ! standard output stream 
!!  INTEGER, PARAMETER :: nerr = 0     ! error output stream
  INTEGER, PARAMETER :: nin  = 5     ! standard input stream
  INTEGER, PARAMETER :: nout = 6     ! standard output stream 
  INTEGER, PARAMETER :: nerr = 6     ! error output stream


!--------------------------------------------------------------------------
! from mo_exception
!-----------------------

  !!! PUBLIC :: message_text
  !!! PUBLIC :: message, finish
  !!! PUBLIC :: em_none, em_info, em_warn

  INTEGER, PARAMETER :: em_none = 0 
  INTEGER, PARAMETER :: em_info = 1
  INTEGER, PARAMETER :: em_warn = 2

  CHARACTER(512) :: message_text = ''


!--------------------------------------------------------------------------
! from mo_interpro
!-----------------------

!
!    WEIGHTNG FACTORS AND MONTH INDICES FOR
!    INTERPOLATION IN TIME IN *CLSST* AND *RADINT*
!

     real:: WGT1=0.5 
     real:: WGT2=0.5
     INTEGER :: NMW1=1
     INTEGER :: NMW2=1
     INTEGER :: NMW1CL=1
     INTEGER :: NMW2CL=1
     real:: WGTD1=0.5
     real:: WGTD2=0.5
     INTEGER :: NDW1=1
     INTEGER :: NDW2=1
!--------------------------------------------------------------------------
! from mo_mpi
!-----------------------
  ! PE identifier

  !!! USE mo_mpi,            ONLY: p_io, p_pe

  !!! PUBLIC :: p_pe, p_io, p_nprocs, p_ocean
  
  ! logical switches

  !!! PUBLIC :: p_parallel
  !!! 
  !!! INTEGER :: p_pe=0                ! this is the PE number of this task
  !!! INTEGER :: p_io=0                ! PE number of PE handling IO
  !!! INTEGER :: p_ocean               ! PE number of PE handling ocean model
  !!! INTEGER :: p_nprocs              ! number of available PEs (processors)
  
  ! public parallel run information

  LOGICAL :: p_parallel = .TRUE.
  
!--------------------------------------------------------------------------
! from mo_parameters
!-----------------------

!----------------------------------------------------------------
!   USE mo_physc2,         ONLY: wicemx, csncri, xicri  
!-----------------------

  real:: wicemx = 0.010_dp               ! maximum water (assume 0.01 m) on the top of a ice layer
  real:: csncri = 5.85036E-3_dp          !  CRITICAL SNOW DEPTH FOR SOIL COMPUTATIONS
  real:: xicri  = 0.005_dp               ! critial depth of ice, causing lost of ice heat flux into water)
  real:: ctfreez= 271.38_dp                        !   temperature at which sea
                                                       !   starts freezing/melting
!----------------------------------------------------------------
! from mo_semi_impl
!-----------------------
  real:: eps  = 0.1_dp !   time filtering coefficient.

!  USE mo_gaussgrid, ONLY: gl_coriol

!!!  USE mo_geoloc,         ONLY: coriol_2d, philat_2d, philon_2d

!!!  USE ice_model_mod,     ONLY: ice_data_type


  !!! PUBLIC :: sit_vdiff_init,sit_vdiff_init_1d,sit_vdiff,sit_vdiff_end,SICEDFN
  !!! PUBLIC :: sit_vdiff_init_1d,sit_vdiff,sit_vdiff_end,SICEDFN,lkvl,set_ocndepth,xmissing,sit_nml,  &
  !!!           maskid,sit_zdepth,sit_fluxdepth,lwoa_echam,                                                  &
  !!!           lprint0,lprint1,lprint2,j3m,j10m,j100m

  PUBLIC :: sit_vdiff_init_1d,sit_vdiff,sit_vdiff_end,SICEDFN,lkvl,set_ocndepth,sit_nml,  &
            maskid,sit_zdepth,sit_fluxdepth,lwoa_echam,                                                  &
            lprint0,lprint1,lprint2,j3m,j10m,j100m

  
 
  NAMELIST /sit_nml/          &
    !!! lrere,                     &! true for IC (Initial Condition) run, false for BC (Boundary Condition) run.
    lobs_ocn_rerun,            &! .TRUE. read rerun file but modified with obs. ocean/atm/land data
    !!! lsit,                      &! switch sit (i.e., calc. of vertical ocean temp. profile) on/off
    trigsit,                   &! coupling interval - trigger embedded ocean-model (default: 1 time setp)
    lsit_ice,                  &! switch on for turnning ice the ice_module of sit for tsi calculation (default: .true.)
    lsit_salt,                 &! switch salt routine in sit (i.e., calc. of salinity ) on/off
    lzgodas,                   &! godas z cord, fine in thermocline, 10 m apart within 100-225 m, but coarse (~200 m)
                                ! in deep-water formation depths (>500 m). (default: .false.)
    ocn_tlz,                   &! depth of bottom z-level (default =5800 m)
    ocn_k1,                    &! vertical dimension parameter (number of layer interfaces, equal
                                ! the number of layers or pressure levels, does not include ghost zone) (=40, default)
    lssst,                     &! .true. for turnon Skin SST, .false. for turnoff Skin SST.
    sit_ice_option,            &! ice option in sit (=0 (default), for coupling with vdiff semi-implicitlyoff; >=1, explicitly)
                                !   2: original SIT output, no security. It can be crashed in few time steps
    maskid,                    &! maskid (=0 (DIECAST only)) 
    lwoa_gfdl,                 &! switch for using world ocean monthly atlas data (salinity and temp. profile) on/off in GFDL model
    lwoa_echam,                    &! switch for using world ocean monthly atlas data (salinity and temp. profile) (n=12) on/off in ECHAM model
    lwoa0_echam,                     &! switch for reading initial ocean salinity and temp. profile (n=1) on/off  in ECHAM model
    lwarning_msg,              &! switch lwarning_msg (i.e., warning message) on/off
    lsice_nudg,                &! spinup time (s) for nudging SIT/DIECAST (default: 1 yr=365.*86400.)
    lsit_lw,                   &! .true. for turning LW code for water in SIT.
    lcook_skin,                &! .true. for cool-skin parameterization (default=.FALSE.)        
    lsteady_TKE,               &! .true. = using calc_steady_TKE for TKE (default=.TRUE.)     
    lwave_breaking,            &! .true. for turning on wave_breaking TKE from ocean surface. (default=.FALSE.)
    ssit_restore_time,         &!  surface [0m,10m) other SIT grids restore time scale (s) (default: no nudging)
    usit_restore_time,         &!  upper [10m,100m) other SIT grids restore time scale (s) (default: no nudging)
    dsit_restore_time,         &!  deep (>=100 m) other SIT grids restore time scale (s)  (default: no nudging)
                                ! 86400 s : 1 d, -9.e33_dp : no nudging (default)
    locaf,                     &! switch using qflux adjustment on/off
!                              
    locn,                      &! switch embedded 3-D ocean on/off
    lopen_bound,               &! .TRUE. = open boundary condition (set depth at j=jos0,jon0 according to its physical depth
                                ! .FALSE.= closed boundary condition (set depth to be 0 for J=jos0,jon0
    lall_straits,              &! .FALSE.= only open Gibraltar Strait
    lstrict_channel,           &! .TRUE.  too strick, less vent in Gibraltar
                                ! .FALSE. too loose, open Central Ameican, Phillipine    
    etopo_nres,                &! 1 for etopo1 (1min), 2 for etopo2 (2min), and 5 for etop5 (5min)
    ocn_domain_w,              &! west coords (lon) of the ocean domain [-180., 360.](deg).
    ocn_domain_e,              &! east coords (lon) of the ocean domain [-180., 360.](deg).
    ocn_domain_s,              &! south coords (lat) of the ocean domain [-90., 90.](deg).
    ocn_domain_n,              &! north coords (lat) of the ocean domain [-90., 90.](deg).  
    ratio_dt_o2a,              &! ratio of the time step between ocn_dt and delta_time (ratio_dt_o2a=ocn_dt/delta_time) (default 1.) 
    ocn_couple_option,         &! switch full-level 2-way coupling with embedded 3-D ocean on/off
    high_current_killer,       &! 1: based on bottom vorticity
                                ! 2: based on bottom vertical velocity
    locn_msg,                  &! .true. for writing embedded 3-D ocean fields
    ocn_lon_factor,            &! number of grid per dx in ECHAM resolution
    ocn_lat_factor,            &! number of grid per dy in ECHAM resolution
    trigocn,                   &! coupling interval - trigger embedded ocean-model (default: 1 time setp)
    socn_restore_time,         &!  surface [0m,10m) ocn grids restore time scale (s) (default: no nudging)
    uocn_restore_time,         &!  upper [10m,100m) ocn grids restore time scale (s) (default: no nudging)
    docn_restore_time,         &!  deep (>=100 m) ocn grids restore time scale (s) (default: no nudging)
    nobox_nudg,                &! number of nudg squares in ocean grids (default = 0, maximun=6)
    obox_restore_time,         &!  restore time scale (s) in all depths for obox_mask>0 grids (default: -9.e33_dp) (no nudging)  
    obox_nudg_flag,            &! 0: no nudging, 1: nudging inside square boxes, 2: nudging outside the square boxes
    obox_nudg_w,               &! west coords (lon) of nudging boxes [-180., 360.](deg). There are 6 boxes.
    obox_nudg_e,               &! east coords (lon) of nudging boxes [-180., 360.](deg). There are 6 boxes.
    obox_nudg_s,               &! south coords (lat) of nudging boxes [-90., 90.](deg). There are 6 boxes.
    obox_nudg_n,               &! north coords (lat) of nudging boxes [-90., 90.](deg). There are 6 boxes.
    kocn_dm0z,                 &! =1, rato to near shore momentum diffusivity (m2/s) for preventing generating near-shore vortex
    ncarpet,                   &! number of coastal grid for carpet filter =0, no carpet filter; =1 for N2 (default); =2 for N4; =3 for N6  
    kcsmag,                    &! =1, ratio to Smagorinsky horizontal diff coeff.
                                ! Smagorinsky, J. General circulation experiments with the primitive equations, I. The basic experiment. Monthly Weather Rev. 1963, 91, 99-164.
    kalbw,                     &! =1, ratio to solar albedo over water by 
                                ! Li J. J. Scinocca M. Lazare N. McFarlane K. von Salzen L. Solheim 2006 Ocean Surface Albedo and Its Impact on Radiation Balance in Climate Models. J. Climate 19 6314¡V6333.
                                ! Taylor, J. P., J. M. Edwards, M. D. Glew, P. Hignett, and A. Slingo, 1996: Studies with a flexible new radiation code. II: Comparisons with aircraft short-wave observations. Quart. J.
                                !   Roy. Meteor. Soc., 122, 839¡V861.
    Prw,                       &! Prandtl number in water (sit_vdiff.f90)                    
    csl,                       &! =-27, the present Caspian Sea Level (CSL) is about -27m and during the medieval time it was about -30m
    por_min,                   &! =0.1, minimum porisity for setting as ocean grid
    csiced,                    &! =0., threshold sea ice depth, seaice[csiced]=0.5, seaice[2*csiced]=0.84
    bathy_default,             &! =-200., default bathy (m).    
    lasia                       ! switch for etopo, new land surface data over rice paddy and Tibet on/off
                                ! http://www.ngdc.noaa.gov/mgg/global/global.html                  


  !!! real, PARAMETER::tol=1.E-14_dp  ! tol: very small numerical value to prevent numerical error
  real, PARAMETER:: tol=1.E-12_dp  ! tol: very small numerical value to prevent numerical error
                                      ! =1.E-6 (ok), tol=1.E-12 (ok), =1.E-33 (crash), , =1.E-20 (crash)
                                      ! =1.E-16_dp (crash)
                                      ! =1.E-14_dp (ok)
                                      ! =0. (ok)

!!!  real,PARAMETER::emin=0. !limit min pwtke to 0. to avoid too warm below mixing depth (20090710) (v5.4)(v9.913)
  real,PARAMETER:: emin=1.0E-6 !limit min pwtke to 1.0E-6 (v9.865, v9.885) (1.0E-6 in GASPAR ET AL., 1990)
!  real,PARAMETER:: emin=1.0E-4 !limit min pwtke to 1.0E-4 (v9.866),(v9.868)
!  real,PARAMETER:: emin=1.0E-5 !limit min pwtke to 1.0E-5 (v9.867) (v9.873)
  real,PARAMETER:: xkmmin=1.2E-6     ! molecular momentum diffusivity (Paulson and Simpson, 1981; Chia and pwu, 1998; Mellor and Durbin, 1975)
  real,PARAMETER:: xkhmin=1.34E-7    ! molecular heat diffusivity (Paulson and Simpson, 1981; Chia and pwu, 1998; Mellor and Durbin, 1975)
  real,PARAMETER:: e_mixing=1.0E-1      ! default ini TKE within mixing depth (1.0E-3=still some spikes)  
  real,PARAMETER:: wlmx_mixing=50.      ! default ini wlmx within mixing depth (50 m)
  real,PARAMETER:: pwldisp_mixing=50.   ! default ini wldisp within mixing depth (50 m)
  real,PARAMETER:: xkm_mixing=1.0E-1    ! default ini km within mixing depth (1E-3 m2/s=still some spikes)
  real,PARAMETER:: xkh_mixing=1.0E-1    ! default ini kh within mixing depth  (1E-3 m2/s=still some spikes)


! --------------------------------
!
!     2.1 OCEAN PARAMETERS
!
  real,PARAMETER:: rhowcw = rhoh2o*clw
  INTEGER :: wtype = 0
! wtype: water type of the ocean
! wtype = 0, PAULSON AND SIMPSON (1981),  Fairall et al., (1996)
!
 ! WTYPE = 22, two-components, JERLOV'S (1976)
  ! WTYPE = 0, PAULSON AND SIMPSON (1981),  Fairall et al., (1996)
  ! WTYPE = 10, SOLOVIEV AND SCHLUESSEL (1996,type I)
  ! WTYPE = 11, SOLOVIEV AND SCHLUESSEL (1996,type IA)
  ! WTYPE = 12, SOLOVIEV AND SCHLUESSEL (1996,type IB)
  ! WTYPE = 20, SOLOVIEV AND SCHLUESSEL (1996,type II)
  ! WTYPE = 30, SOLOVIEV AND SCHLUESSEL (1996,type III)
  ! WTYPE = 1, SOLOVIEV AND SCHLUESSEL (1996,type 1)
  ! WTYPE = 3, SOLOVIEV AND SCHLUESSEL (1996,type 3)
  ! WTYPE = 5, SOLOVIEV AND SCHLUESSEL (1996,type 5)
  ! WTYPE = 7, SOLOVIEV AND SCHLUESSEL (1996,type 7)
  ! WTYPE = 9, SOLOVIEV AND SCHLUESSEL (1996,type 9)

  real:: mixing_depth=100.          ! mixing depth of ocean, set at 100 m, should be changed as a function of time and location  
!**************************************************	
!    2) SOIL PARAMETERS
!
  real,PARAMETER::xkg = 0.4835E-6
!     heat capacity of soil
!       =(0.5675E-6-0.175E-6*porosity)*soil type factor (de Vries, 1975)
!       =0.4835E-6 ,
!     sandy clay loam at water content , porosity(48%), factor =1
!     CGSOIL=248672.
  real,PARAMETER::rhogcg=1.04E6+0.48*4.19E6
!     area heat capacity of the above soil (Tsuang and Wang, 1994)
!       =rhog*cg*Sqrt(Kg/omega)
!       =(1.04E6+0.48*4.19E6)*0.0815
!     ALBG = 0.3
!     ALBG: ALBEDO OF UNDERNEATH SOIL
!
  real,PARAMETER::hspg=0         ! HOT SPRING FROM THE BOTTOM (W/M2)                                I
!
!!!  real, PARAMETER:: nudge_depth_10=0.   ! (v8.3 - v9.1)
  real, PARAMETER:: nudge_depth_10=10.     ! (- v8.3 and v9.2 -)
  real, PARAMETER:: nudge_depth_100=100.

CONTAINS
!----------------------------------------------------------------------------------------------------
  SUBROUTINE set_ocndepth()        
    IMPLICIT NONE
    INTEGER :: ocn_k0                    ! vertical dimension parameter (number of layer interfaces, equal
                                         ! the number of layers or pressure levels plus 1, does not include
                                         ! ghost zone)  
    real:: tmp,dzz,b,c,d,aa,zz
    INTEGER:: jk,k
    IF (lzgodas) THEN
    ! RESET ocn_k1
      ocn_k1=40  
      WRITE (nerr,*) 'lzgodas=',lzgodas
      WRITE (nerr,*) 'RESET ocn_k1=',ocn_k1
    ENDIF
    ocn_k0=ocn_k1+1   ! = 41  number of ocean ocean levels plus 1
    ALLOCATE (ocn_z(ocn_k0+ocn_k1))
    ALLOCATE (diecast_zdepth(1:ocn_k1))
    ALLOCATE (diecast_fluxdepth(1:ocn_k1))
    IF (lzgodas) THEN
    ! fine in thermocline, 10 m apart within 100-225 m, but coarse (~200 m) in deep-water formation depths (>500 m).
      diecast_zdepth=(/                                                                              &
          5.,  15.,  25.,  35.,  45.,  55.,  65.,  75.,  85.,  95.,    &
        105., 115., 125., 135., 145., 155., 165., 175., 185., 195.,    &
        205., 215., 225., 238., 262., 303., 366., 459., 584., 747.,    &
        949.,1193.,1479.,1807.,2174.,2579.,3016.,3483.,3972.,4478./)
      diecast_fluxdepth(1)=(0.+diecast_zdepth(1))/2.  
      DO jk = 2, ocn_k1
        diecast_fluxdepth(jk)=(diecast_zdepth(jk-1)+diecast_zdepth(jk))/2.
      ENDDO
      DO jk = 1, ocn_k1
        ocn_z(2*jk-1)=diecast_fluxdepth(jk)    
        ocn_z(2*jk)=diecast_zdepth(jk)
      ENDDO
      ocn_z(ocn_k0+ocn_k1)=ocn_tlz
    ELSE 
  !   generate combined linear-exponential expanding coordinate Z(zz)
  !   Z=aa+b*exp(c*zz)+d*zz, 0 < zz < tlz
  !   where zz is the linear (unstretched) space coordinate
  !   NOTE: for expanding coordinate, 0 < d < 1
  !   at zz=0, Z=aa+b*exp(0)=0, so aa+b=0.
  !   at zz=tlz: Z=aa+b*exp(c*tlz)+d*tlz=tlz
  !   so aa*(1-exp(c*tlz))=tlz-d*tlz, aa=tlz*(1-d)/(1-exp(c*tlz))
  !  convert tlz to meters
      tmp=ocn_tlz
      dzz=tmp/(2.*ocn_k1)
  !   exponential part
      c=.001
  !   linear part
      d=0.03
      aa=tmp*(1.-d)/(1.-exp(c*tmp))
      b=-aa
      zz=0.
      DO k=1,ocn_k0+ocn_k1
  !   z(k) in m
        ocn_z(k)=(aa+b*exp(c*zz)+d*zz)
        zz=zz+dzz
      ENDDO
      DO jk = 1, ocn_k1
        diecast_zdepth(jk)=ocn_z(2*jk)
        diecast_fluxdepth(jk)=ocn_z(2*jk-1)
      ENDDO
    ENDIF
    IF (lssst) THEN
  !   nfnlvl=12     ! number of fine layers in sit for the uppermost ocean layer
      nfnlvl=MIN(INT(diecast_zdepth(2)-1.),10)+2     ! number of fine layers in sit for the uppermost ocean layer
                                                        ! 0,0.0005,1,2,...,MIN(INT(diecast_zdepth(2)),10.)
    ELSE
      nfnlvl=2      ! number of fine layers in sit for the uppermost ocean layer (0m, 0.0005m)
    ENDIF
    lkvl=ocn_k1+nfnlvl-3    ! number of vertical levels of sit ocean model, =49, lssst=T; =39 , lssst=F 
    ALLOCATE (sit_zdepth(0:lkvl+1))
    ALLOCATE (sit_fluxdepth(0:lkvl+1))
    ALLOCATE (sit_gribzdepth(1:lkvl+2))
    ALLOCATE (sit_gribfluxdepth(1:lkvl+2))
  ! 
  ! for capture the diurnal cycle of SST, we need the following z-cord for upper ocean
  !
    IF (lssst) THEN
      sit_zdepth(0:1)=(/0.,0.0005_dp/)         !=zdepth(0)*10=-0 m in dm
      DO k=1,nfnlvl-2
        !sit_zdepth(k+1)=REAL(k,dp)
        sit_zdepth(k+1)=REAL(k)
      ENDDO
    ELSE
      sit_zdepth(0:nfnlvl-1)=(/0.,diecast_zdepth(1)/)
    ENDIF
  !
  ! for combining with 3-D ocean model (DIECAST), we need the following z-cord for ocean
  !
    sit_zdepth(nfnlvl:lkvl+1)=diecast_zdepth(2:ocn_k1)                 ! convert unit from m to m
                                                                       ! for the index, it can be seen that lkvl+1-nfnlvl=ocn_k1-2
                                                                       ! i.e., lkvl=ocn_k1+nfnlvl-3
    IF (.TRUE.) THEN
      sit_fluxdepth(0)=sit_zdepth(0)
      DO jk = 1,nfnlvl !i.e., jk = 1,12
        sit_fluxdepth(jk)=(sit_zdepth(jk-1)+sit_zdepth(jk))/2.
      ENDDO
      sit_fluxdepth(nfnlvl+1:lkvl+1)=diecast_fluxdepth(3:ocn_k1)           ! convert unit from m to m
      ! modify diecast_fluxdepth(2) and ocn_z(3) based on lssst option 
      diecast_fluxdepth(2)=sit_fluxdepth(nfnlvl)
      ocn_z(3)=diecast_fluxdepth(2)
    ELSE
      sit_fluxdepth(0)=sit_zdepth(0)
      DO jk = 1,nfnlvl-1 !i.e., jk = 1,11
        sit_fluxdepth(jk)=(sit_zdepth(jk-1)+sit_zdepth(jk))/2.
      ENDDO
      sit_fluxdepth(nfnlvl:lkvl+1)=diecast_fluxdepth(2:ocn_k1)           ! convert unit from m to m  
    ENDIF
    IF (mpp_pe() == mpp_root_pe()) THEN
    !!! IF (p_pe==p_io) THEN
    !!! IF (p_parallel_ocean) THEN
      OPEN (UNIT=nocn_sv,FILE="diecast_zdepth.txt",ACTION="WRITE",STATUS="REPLACE")
      WRITE (nocn_sv,*) 'zaxistype = depth_below_sea'
      WRITE (nocn_sv,*) 'size =',  size(diecast_zdepth)
      WRITE (nocn_sv,*) 'levels =',diecast_zdepth
      CLOSE (nocn_sv)
      OPEN (UNIT=nocn_sv,FILE="diecast_fluxdepth.txt",ACTION="WRITE",STATUS="REPLACE")
      WRITE (nocn_sv,*) 'zaxistype = depth_below_sea'
      WRITE (nocn_sv,*) 'size =',  size(diecast_fluxdepth)
      WRITE (nocn_sv,*) 'levels =',diecast_fluxdepth
      CLOSE (nocn_sv)
      OPEN (UNIT=nocn_sv,FILE="zdepth.txt",ACTION="WRITE",STATUS="REPLACE")
      WRITE (nocn_sv,*) 'zaxistype = depth_below_sea'
      WRITE (nocn_sv,*) 'size =',  size(sit_zdepth)
      WRITE (nocn_sv,*) 'levels =',sit_zdepth
      CLOSE (nocn_sv)
      OPEN (UNIT=nocn_sv,FILE="fluxdepth.txt",ACTION="WRITE",STATUS="REPLACE")
      WRITE (nocn_sv,*) 'zaxistype = depth_below_sea'
      WRITE (nocn_sv,*) 'size =',  size(sit_fluxdepth)
      WRITE (nocn_sv,*) 'levels =',sit_fluxdepth
      CLOSE (nocn_sv)
      OPEN (UNIT=nocn_sv,FILE="ocnz_depth.txt",ACTION="WRITE",STATUS="REPLACE")
      WRITE (nocn_sv,*) 'zaxistype = depth_below_sea'
      WRITE (nocn_sv,*) 'size =',  size(ocn_z)
      WRITE (nocn_sv,*) 'levels =',ocn_z
      CLOSE (nocn_sv)
    ENDIF
     IF (lssst) THEN    
      sit_gribzdepth(1:2)=(/0.,                                   &    !=zdepth(0)*10=-0 m in dm
        1./)                                                          !-0.0005 msit_zdepth(0)
      sit_gribzdepth(3:lkvl+2)=sit_zdepth(2:lkvl+1)*10.                ! convert unit from m to dm (i.e., 0.1 m)
!     
      sit_gribfluxdepth(1:2)=(/0.,                               &    !=-0 m                               
        1./)                                                          !=(zdepth(0)+zdepth(1))/2 =-0.00025 m
      sit_gribfluxdepth(3:lkvl+2)=sit_fluxdepth(2:lkvl+1)*10.         !=(zdepth(1)+zdepth(2))/2 =-0.50025 m to dm (i.e., 0.1 m)
    ELSE
      sit_gribzdepth(1:lkvl+2)=sit_zdepth(0:lkvl+1)*10.                ! convert unit from m to dm (i.e., 0.1 m)
      sit_gribfluxdepth(1:lkvl+2)=sit_fluxdepth(0:lkvl+1)*10.         !=(zdepth(1)+zdepth(2))/2 =-0.50025 m to dm (i.e., 0.1 m)
    ENDIF
    IF ( (lwarning_msg.GE.2).AND.(mpp_pe() == mpp_root_pe()) ) THEN
    !!! IF ( (lwarning_msg.GE.2).AND.p_pe==p_io ) THEN       
      WRITE(nerr,*) "pe=",mpp_pe(),"I am in mo_netcdf 0.0: IO_init_dims"    
      WRITE (nerr,*) 'diecast_zdepth(m)= ',diecast_zdepth
      WRITE (nerr,*) 'sit_zdepth(m)= ',sit_zdepth
!!!      WRITE (nerr,*) 'sit_gribzdepth(dm)= ',sit_gribzdepth(lkvl-1:lkvl+2)
      WRITE (nerr,*) 'sit_gribzdepth(dm)= ',sit_gribzdepth
      WRITE (nerr,*) 'sit_fluxdepth(m)= ',sit_fluxdepth
      WRITE (nerr,*) 'sit_gribfluxdepth(dm)= ',sit_gribfluxdepth
    ENDIF
    
!
!   1.0 Find jk at 3, 10 and 100 m depth
    jk=1
    DO WHILE ( (sit_zdepth(jk).LT.3.).AND.(jk.LT.lkvl+1) ) 
!      nudging at depth just >= 3 m or at level lkvl+1 (last water level)
       jk=jk+1
    END DO
    j3m=jk
    DO WHILE ( (sit_zdepth(jk).LT.10.).AND.(jk.LT.lkvl+1) ) 
!      nudging at depth just >= 10 m or at level lkvl+1 (last water level)
       jk=jk+1
    END DO
    j10m=jk
    DO WHILE ( (sit_zdepth(jk).LT.100.).AND.(jk.LT.lkvl+1) ) 
!      nudging at depth >= nudge_depth (100 m)
       jk=jk+1
    END DO
    j100m=jk
           
  END SUBROUTINE set_ocndepth
!
!***************************************************
!
SUBROUTINE sit_vdiff_init_1d ( istep,               &
       !  
       ! 1-input only, original ATM/SIT variabels
       !
                  plat,       plon,                                   &
                  psitmask,   pbathy,     pwlvl,                      &
                  pocnmask,   obox_mask,                              &
                  psni,       psiced,     ptsi,                       &
                  pobsseaice, pobswtb,    pobswsb,                    &
                  pctfreez2,                                          &
       ! 2-d SIT vars
                  pwtb,       pwub,       pwvb,                       &
                  pwsb,                                               &
                  psubfluxw,  pwsubsal,                               &
                  pcc,        phc,        pengwac,                    &
                  psc,        psaltwac,                               &
                  pwtfns,     pwsfns,                                 &
       ! 3-d SIT vars: snow/ice
                  pzsi,       psilw,      ptsnic,                     &
       ! 3-d SIT vars: water column
                  pobswt,     pobsws,     pobswu,    pobswv,          &
                  pwt,        pwu,        pwv,       pww,             &
                  pws,        pwtke,      pwlmx,                      & 
                  pwldisp,    pwkm,       pwkh,                       &
                  pwrho1000,                                          &
                  pwtfn,      pwsfn,                                  &
                  pawufl,     pawvfl,     pawtfl,                     &
                  pawsfl,     pawtkefl,                               &
       ! 4-output only, original ATM variabels
                  pseaice,                                            &
                  pgrndcapc,  pgrndhflx,  pgrndflux                   )
!  ---------------------------------------------------------------------
!
!                           SUBROUTINE DESCRIPTION
!
! 1. FUNCTION DESCRIPTION
!
!     THIS SUBROUTINE initialize the sit_vdiff variables including T,S,u,v profiles
!
! 2. CALLING MODULES
!
!          *sit_vdiff_init_1d* is called from *initemp.
!
! 3. PARAMETER SPECIFICATION
!
!      Arguments.
!      ----------
! local dimensions
!  istep    : # of time steps
!  pcoriol : coriol factor = 2*omega*sin(latitudes)  (1/s)                         I
!  plat    : latitide (geg)                                                        I
!  plon    : longitude (deg)                                                       I
! 2-d SIT vars
!  pobsseaice  : observed sea ice fraction (fraction)                              I
!  pobswtb     : observed bulk sea surface temperature (K)                         I
!  pobswsb    : observed salinity (PSU, 0/00)                                      I
!  psitmask : mask for sit(1=.TRUE., 0=.FALSE.)                                    I
!  pbathy  : bathymeter (topography or orography) of ocean (m)                     I
!  pctfreez2 : ref water freezing temperature (K)                                  I
!  pwlvl  : current water level (ice/water interface) a water body grid            I/O
!  pocnmask : fractional mask for 3-D ocean grid (0-1)                             I
!  obox_mask : 3-D ocean nudging mask, =0: nudging, = 1 (>0): nudging              I
!  pwtb: bulk water temperature (K)                                                O
!  pwub: bulk water u current (m/s)                                                O
!  pwvb: bulk water v current (m/s)                                                O
!  pwsb: bulk water salinity (PSU)                                                 O
!  psubfluxw: subsurface ocean heat flux (W/m2, + upward)                          O
!  pwsubsal: subsurface ocean salinity flux (m*PSU/s, + upward)                    O
!  pcc: cold content per water fraction (ice sheet+openwater) (J/m2)
!    (energy need to melt snow and ice, i.e., energy below liquid water at tmelt)  I/O
!  phc: heat content per water fraction (ice sheet+openwater) (J/m2)
!    (energy of a water column above tmelt)                                        I/O
!  pengwac: accumulated energy per water fraction (ice sheet+openwater) (+ downward, J/m2)
!    (pfluxw+pfluxi+rain/snow advected energy in respect to liquid water at
!     tmelt)*dt                                                                    I/O
!  psaltwac: accumulated salt into water fraction (+ downward, PSU*m)              I/O
!
!  pgrndcapc: areal heat capacity of the uppermost sit layer (snow/ice/water)
!    (J/m**2/K)                                                                    I/O
!  pgrndhflx: ground heat flux below the surface (W/m**2)
!    (+ upward, into the skin layer)                                               I/O
!  pgrndflx:  acc. ground heat flux below the surface (W/m**2*s)
!    (+ upward, into the skin layer)                                               I/O
! 3-d SIT vars: snow/ice
!  pzsi   :
!        pzsi(0): dry snow water equivalent (m)                                 I/O
!        pzsi(1): dry ice water equivalent (m)                                  I/O
!  psilw  :
!        psilw(0): snow liquid water storage (m)                                I/O
!        psilw(1): ice liquid water storage (m)                                 I/O
!  ptsnic   :
!        ptsnic(0): snow skin temperatrue (jk)                                  I/O
!        ptsnic(1): mean snow temperatrue (jk)                                  I/O
!        ptsnic(2): ice skin temperatrue (jk)                                   I/O
!        ptsnic(3): mean ice temperatrue (jk)                                   I/O
! 3-d SIT vars: water column
!  pobswt: observed potentail water temperature (K)                                I/O
!  pobsws: observed salinity (PSU, 0/00)                                           I/O
!  pobswu: observed u-current (m/s)                                                I/O
!  pobswv: observed v-current (m/s)                                                I/O
!  pwt      : potential water temperature (K)                                      I/O
!  pwu      : u current (m/s)                                                      I/O
!  pwv      : v current (m/s)                                                      I/O
!  pww      : w current (m/s)                                                      I/O
!  pws      : practical salinity (0/00)                                            I/O
!  pwtke    : turbulent kinetic energy (M2/S2)                                     I/O
!  pwlmx    : mixing length (m)                                                    O
!  pwldisp  : dissipation length (m)                                               O
!  pwkm     : eddy diffusivity for momentum (m2/s)                                 O
!  pwkh     : eddy diffusivity for heat (m2/s)                                     O
!  pwrho1000: potential temperature at 1000 m (PSU)                                O
!  pwtfn  : nudging flux into sit-ocean at each level (W/m**2)                     I/O
!  pwsfn  : nudging salinity flux into sit-ocean at each level (PSU*m/s)           I/O
!  pwtfns : nudging flux into sit-ocean for an entire column (W/m**2), i.e.
!     pwtfns=SUM(pwtfn(:))                                                      I/O
!  pwsfn  : nudging salinity flux into sit-ocean for an entire column (PSU*m/s),
!     i.e., pwsfns=SUM(pwsfn(:)                                                 I/O
!  pawufl : advected u flux at each level (positive into ocean) (m/s/s)            I
!  pawvfl : advected v flux at each level (positive into ocean) (m/s/s)            I
!  pawtfl : advected temperature flux at each level (positive into ocean) (K/s)    I
!  pawsfl : advected salinity flux at each level (positive into ocean) (PSU/s)     I
!  pawtkefl : advected tke at each level (positive into ocean) (m3/s3)             I
! - variables internal to physics
! - water body or ml_ocean variables
!  pseaice   : ice cover (fraction of 1-SLM) (0-1)                                 I/O
!  psni      : snow thickness over ice (m in water equivalent)                     I/O
!  psiced    : ice thickness (m in water equivalent)                               I/O
!  ptsl      : calcuated Earth's skin temperature from vdiff/tsurf at t+dt (K)     I/O
!  ptslm     : calcuated Earth's skin temperature from vdiff/tsurf at t (K)        I/O
!  ptslm1    : calcuated Earth's skin temperature from vdiff/tsurf at t-dt (K)     I/O
! 2-d SIT vars

  IMPLICIT NONE

  INTEGER, INTENT(in)                      :: istep  ! # of time steps

  real, INTENT(in)::                                                      &
       plat, plon   
       ! pcoriol,   plat, plon   

  real, INTENT(in out):: pseaice, psni, psiced,     &
     ptsi
! 2-d SIT vars                                                
  real, INTENT(in out) ::                                                &
       pwtfn(0:lkvl+1),  pwsfn(0:lkvl+1)
  real, INTENT(in out) ::                                                &
       pwtfns,  pwsfns
  real, INTENT(in):: pobsseaice
  real, INTENT(in out):: pobswtb,      pobswsb
! 2-d SIT vars                                                
  real, INTENT(in)::   psitmask         ! grid mask for lsit (1 or 0)      
  real, INTENT(in out) ::   pbathy
  real, INTENT(in out) :: pctfreez2
  real, INTENT(in out) :: pwlvl
  real, INTENT(in)::   pocnmask         ! (fractional) grid mask for 3-D ocean (DIECAST)
  real, INTENT(in)::   obox_mask        ! (fractional) ocean iop nudging mask for 3-D ocean (DIECAST)
  real, INTENT(out) :: pwtb, pwub, pwvb, pwsb, &
    psubfluxw, pwsubsal
  real, INTENT(in out) :: pgrndcapc, pgrndhflx, pgrndflux
  real, INTENT(in out) :: pcc, phc, pengwac
  real, INTENT(in out) :: psc, psaltwac
! 3-d SIT vars: snow/ice
  real, INTENT(in out) ::                                                &
       pzsi(0:1),   psilw(0:1), ptsnic(0:3)
! 3-d SIT vars: water column
  real, INTENT(in out) ::                                                &
       pobswt(0:lkvl+1), pobsws(0:lkvl+1),                       &
       pobswu(0:lkvl+1), pobswv(0:lkvl+1),                       &
       pwt(0:lkvl+1), pwu(0:lkvl+1), pwv(0:lkvl+1),        &
       pww(0:lkvl+1),                                                  &
       pws(0:lkvl+1), pwtke(0:lkvl+1), pwlmx(0:lkvl+1),    & 
       pwrho1000(0:lkvl+1),                                                & 
       pwldisp(0:lkvl+1), pwkm(0:lkvl+1), pwkh(0:lkvl+1)  

  real, INTENT(in out) :: pawufl(0:lkvl+1), pawvfl(0:lkvl+1), &
       pawtfl(0:lkvl+1), pawsfl(0:lkvl+1), pawtkefl(0:lkvl+1)

! - variables internal to physics

! - local variables

  !*    1.0 Depth of the coordinates of the water body point. (zdepth = 0 at surface )
!!!  real, PARAMETER:: zdepth(0:lkvl)=(/0.,0.0005_dp,1.,2.,3.,4.,5.,6.,7.,8.,9.,10.,&
!!!              20.,30.,50.,75.,100.,125.,150.,200.,250.,300.,400.,500.,&
!!!              600.,700.,800.,900.,1000.,1100.,1200.,1300.,1400.,1500./) 
  !
  !********************
  ! Note that WOA 2005 data are at depths:
  !  depth = 0, 10, 20, 30, 50, 75, 100, 125, 150, 200, 250, 300, 400, 500, 600,
  !    700, 800, 900, 1000, 1100, 1200, 1300, 1400, 1500 ;
  !********************

  !*    1.1  INFORMATION OF OCEAN GRID (COMLKE)
  !     WATER BODY POINT VERTICAL LEVELS
  INTEGER nls ! first water level -1,
              ! starting water level of the water body point.
              ! z(k)=sitwlvl, IF k is <= nls. 
  INTEGER nle ! nle: last water level (excluding skin level)
              ! nle: maximum water levels of the water body point.
              ! Note nle=-1 IF water level is below point water body bed.
              ! Soil layer always is sitwt(nle+1). If there is no water,
              ! sitwt(nle+1)=sitwt(0). In addition, z(k)=bathy IF k >= nle.
  !!! INTEGER nlv ! number of water levels (excluding water surface &
  !!!             ! soil layer). Note nlv=-1 IF water level is below
  !!!             ! water bed.  
  real, DIMENSION(0:lkvl+1):: z   ! coordinates of the water body point.
  real, DIMENSION(0:lkvl+1):: zlk ! standard z coordinates of a water body 
  real, DIMENSION(0:lkvl):: hw    ! thickness of each layer
  LOGICAL lshlw  ! .true. = shallow water mode (due to evaportion or freezing)
              ! (one layer water body)
  LOGICAL lsoil  ! .true. = soil grid

  INTEGER:: lstfn     ! index of last fine level
  real:: heice  ! effective skin thickness of ice (m)
!!!  real:: hice   ! ice thickness (m)
  real:: hesn   ! effective skin thickness of snow (m)
!!!  real:: hsn    ! snow thickness (m)
  real:: hew    ! effective skin thickness of water (m)
  real:: xkhskin   ! skin layer heat diffusivity (m2/s)
!***************
! Local variables for backgroud initial ocean profiles
  real:: bg_wt0(0:lkvl+1)
  real:: bg_ws0(0:lkvl+1)
  real:: bg_wu0(0:lkvl+1)
  real:: bg_wv0(0:lkvl+1)
!!!!***************
  LOGICAL :: lsitmask   ! sit mask
!!!  INTEGER :: istep=1                           ! istep=time step

!!!  RETURN

    lsitmask=psitmask.EQ.1.                          ! convert from REAL to Integer and to Logical
!!!    RETURN
!!!    IF (lprint1) THEN
!!!      WRITE(nerr,*) "I am in sit_vdiff_init_1d 1.0:"   
!!!!!!      WRITE(nerr,*) "lstart=.TRUE."
!!!!!!      WRITE(nerr,*) "delta_time=",delta_time,"zdtime=",zdtime,"n_sit_step=",n_sit_step
!!!    ENDIF
#ifdef DEBUG
  if (lprint1) print *, "I am in sit_vdiff_init_1d 0.0."
  if (lprint1) print *, "lat=",plat
  if (lprint1) print *, "lon=",plon
!!!  if (lprint1) print *, "tsw=",ptsw
  if (lprint1) print *, "sni=",psni          
  if (lprint1) print *, "siced=",psiced          
  if (lprint1) print *, "tsi=",ptsi          
  if (lprint1) print *, "obsseaice=",pobsseaice          
  if (lprint1) print *, "obswtb=",pobswtb          
  if (lprint1) print *, "obswsb=",pobswsb
  if (lprint1) print *, "ctfreez2=",pctfreez2  
  if (lprint1) print *, "seaice=",pseaice
#endif
  IF (lprint2) then
    WRITE(nerr,*) ", I am in sit_vdiff_init_1d 1.0: prior to init_sit_viff_gd"
    IF (lsitmask ) CALL output2
  ENDIF
  CALL init_sit_viff_gd
  IF (lprint2) then
    WRITE(nerr,*) ", I am in sit_vdiff_init_1d 2.0: after init_sit_viff_gd"
    IF (lsitmask ) CALL output2
  ENDIF
!!!    RETURN
!!!    masking RETURN will crash in one day
!!!    run for one time step for self-adjusting before entering to ocean
!!!    RETURN
  CONTAINS

  SUBROUTINE init_sit_viff_gd
!
! Initialise sit T,U,V,S on ocean levels (lake, ocean and soil)
!
  !!! USE mo_mpi,           ONLY: p_pe 
  !!! USE sit_constants_mod,     ONLY: tmelt
  !!! USE mo_control,       ONLY: lwoa_echam,lwoa0_echam
!!!  USE mo_physc2,        ONLY: ctfreez
                          
  IMPLICIT NONE

  INTEGER  :: jk,kkk
  real:: depth  
  real:: sumxxz,sumxxt,sumxxu,sumxxv,sumxxs
  
  IF (lprint2) then
    WRITE(nerr,*) ", I am in init_sit_viff_gd 1.0"
  ENDIF

! initialize accumulated variables  
  pwtfns=0.
  pwsfns=0.
  pwtfn(:)=0.
  pwsfn(:)=0.
  pawufl(:)=0.
  pawvfl(:)=0.
  pawtfl(:)=0.
  pawsfl(:)=0.
  pawtkefl(:)=0.

!  
  IF (lprint2) WRITE(nerr,*) ", I am in init_sit_viff_gd 1.1, lsitmask=",lsitmask
  IF (.NOT.lsitmask) THEN
    IF (.FALSE.) THEN    
      ptsnic(:)=tmelt
      pzsi(:)=0.
      
      pobswt(:)=tmelt
      pobsws(:)=0.
      pobswu(:)=0.
      pobswv(:)=0.
      
      pwt(:)=tmelt
      pws(:)=0.
      pwu(:)=0.
      pwv(:)=0.
      pww(:)=0.
          
      pwtke(:)=0.
      pwlmx(:)=0.
      pwldisp(:)=0.
      pwkm(:)=0.
      pwkh(:)=0.
      
      pgrndcapc=0.
      pgrndhflx=0.
      pgrndflux=0.
      pengwac=0.
      psaltwac=0.
      pwtb=tmelt
      pwub=0.
      pwvb=0.
      pwsb=0.
      psubfluxw=0.
      pwsubsal=0.
    ELSE
      ptsnic(:)=xmissing
      pzsi(:)=xmissing
      
      pobswt(:)=xmissing
      pobsws(:)=xmissing
      pobswu(:)=xmissing
      pobswv(:)=xmissing
      
      pwt(:)=xmissing
      pws(:)=xmissing
      pwu(:)=xmissing
      pwv(:)=xmissing    
      pww(:)=xmissing
          
      pwtke(:)=xmissing
      pwlmx(:)=xmissing
      pwldisp(:)=xmissing
      pwkm(:)=xmissing
      pwkh(:)=xmissing
      
      pgrndcapc=xmissing
      pgrndhflx=xmissing
      pgrndflux=xmissing
      pengwac=xmissing
      psaltwac=xmissing
      pwtb=xmissing
      pwub=xmissing
      pwvb=xmissing
      pwsb=xmissing
      psubfluxw=xmissing
      pwsubsal=xmissing
    ENDIF          
    RETURN
  ENDIF 
!!!  bjt >> 20170703
!     Make all water least 2 layers deep
  pbathy=MIN(pbathy,pwlvl-sit_zdepth(2))
  CALL pzcord(pwlvl,pbathy,nls,nle,z,zlk,hw,lsoil,lshlw)
!!!  IF (lsoil) THEN
!!!!!!    pwlvl=pbathy+wcri+10.
!!!    pbathy=pwlvl-wcri-10.
!!!    !! add 10-m depth water to soil grid.
!!!    !! This 10-m water line should be deleted for more physcial sound,
!!!    CALL pzcord(pwlvl,pbathy,nls,nle,z,zlk,hw,lsoil,lshlw)
!!!  ENDIF
!!
!! Note that the vertical index of g3b starts from 1 although the index in the sit_vdiff 
!!   routine starts from 1,
  IF (ptsi.NE.xmissing) THEN
    ptsnic(:)=ptsi    ! assume snow temperature = ice temperature
  ELSE
    ptsnic(:)=tmelt       ! assume snow temperature = tmelt
  ENDIF
  IF (lprint2) then
      WRITE(nerr,*) ", I am in init_sit_viff_gd 1.8: after setting ptsnic"
      !!! CALL output2
  ENDIF
  
  IF (psni.NE.xmissing) THEN
    pzsi(0)=psni            ! snow depth
  ELSE
    pzsi(0)=0.
  ENDIF
  IF (psiced.NE.xmissing) THEN
    pzsi(1)=psiced         ! ice depth
  ELSE
    pzsi(1)=0.
  ENDIF
  IF (lprint2) then
    WRITE(nerr,*) "istep=",istep,"pe=",mpp_pe(),", I am in init_sit_viff_gd 1.9: after setting zsi"
    !!! CALL output2
  ENDIF
!    sitsilw improper chosen might cause larger KM
!  runtoc=0.
!  hspg=0.
! 
  IF (lobs_ocn_rerun) THEN
    ! modify prog. vars (wt, ws, wu, wv) from rerun file according to obs
    CALL compose_obs_ocean(.TRUE.,.TRUE.,.TRUE.,.TRUE.)
  ELSE
    psilw(:)=0.                 ! assume no liquid water in snow and ice
    CALL compose_obs_ocean(.FALSE.,.FALSE.,.FALSE.,.FALSE.)
    IF (lprint2) then
      WRITE(nerr,*) ", I am in init_sit_viff_gd 2.1: lobs_ocn_rerun=F"
      CALL output2
    ENDIF
    DO jk=0,nle+1
      depth=(pwlvl-z(jk))
      IF ( depth.LE.mixing_depth) then
        pwtke(jk)=e_mixing
        pwlmx(jk)=wlmx_mixing
        pwldisp(jk)=pwldisp_mixing
        pwkm(jk)=xkm_mixing
        pwkh(jk)=xkh_mixing
      ELSE
        pwtke(jk)=emin
        pwlmx(jk)=0.
        pwldisp(jk)=0.
        pwkm(jk)=xkmmin
        pwkh(jk)=xkhmin
      ENDIF
    ENDDO
    IF (lprint2) then
      WRITE(nerr,*) ", I am in init_sit_viff_gd 2.3: after setting pwkh"
      WRITE(nerr,*) "nle=",nle
!        CALL output2
    ENDIF
  ENDIF  
  !
!
!*   14.2 pgrndcapc, pgrndhflx, current, tsw
!
  pseaice=SEAICEFN(pzsi(1))
!!!  RETURN
  !!! CALL update_snow_ice_property(pzsi(:),hsn,hesn,hice,heice,pseaice)
  !!! hsn=HSNFN(pzsi(0))
  !!! hesn=HEFN(hsn/4.,xksn,omegas)
  !!! hice=HICEFN(pzsi(1))
  !!! heice=HEFN(hice/4.,xkice,omegas)
  xkhskin=SQRT(pwkh(0)*pwkh(nls+1))         ! calc the heat diffusivity for skin layer
  hew=HEFN(hw(0),xkhskin,omegas)

  IF (hesn.GT.csncri) THEN
! snow on top
    pgrndcapc=rhosn*csn*hesn
  ELSEIF (heice.GT.xicri) THEN
! ice on top
    pgrndcapc=rhoice*cice*heice
  ELSEIF(.NOT.lsoil) THEN
! water on top
    pgrndcapc=rhowcw*hew
  ELSE
! soil on top
    pgrndcapc=rhogcg*SQRT(xkg/omegas)
  ENDIF  
  pgrndhflx=0.
  pgrndflux=0.
  pengwac=0.
  psaltwac=0.
  !!! !!! IF (locn) THEN
  !!! IF (.true.) THEN  
  !!!   sumxxz=0.
  !!!   sumxxt=0.
  !!!   sumxxu=0.
  !!!   sumxxv=0.
  !!!   sumxxs=0.
  !!!   IF (locn) THEN
  !!!     lstfn=MIN(nls+nfnlvl-1,nle)  ! index of last fine level
  !!!   ELSE
  !!!     lstfn=j10m  ! 10 m depth index
  !!!   ENDIF
  !!!   DO jk=nls+1,lstfn
  !!!     ! excluidng skin layer
  !!!     sumxxz=sumxxz+hw(jk)
  !!!     sumxxt=sumxxt+pwt(jk)*hw(jk)
  !!!     sumxxu=sumxxu+pwu(jk)*hw(jk)
  !!!     sumxxv=sumxxv+pwv(jk)*hw(jk)
  !!!     sumxxs=sumxxs+pws(jk)*hw(jk)
  !!!     IF ( lwarning_msg.GE.3 ) THEN
  !!!       WRITE(nerr,*) 'jk=',jk,'hw=',hw(jk)
  !!!     ENDIF
  !!!   ENDDO
  !!!   pwtb=sumxxt/sumxxz
  !!!   pwub=sumxxu/sumxxz
  !!!   pwvb=sumxxv/sumxxz
  !!!   pwsb=sumxxs/sumxxz
  !!!   psubfluxw=0.
  !!!   pwsubsal=0.
  !!!   IF ( (sumxxz.LE.0.).OR.((pwtb+pwub+pwvb+pwsb+psubfluxw+pwsubsal).LT.-9.E20) ) THEN
  !!!     WRITE(nerr,*) ", I am in init_sit_viff_gd 2.6: sumxxz = (<=0)", sumxxz
  !!!     WRITE(nerr,*) 'nls+1=',nls+1,'lstfn=',lstfn,'sumxxz=',sumxxz,'sumxxt=',sumxxt,'sumxxu=',sumxxu,'sumxxv=',sumxxv,'sumxxs=',sumxxs
  !!!     WRITE(nerr,*) 'pwtb=',pwtb,'pwub=',pwub,'pwvb=',pwvb,'pwsb=',pwsb,'psubfluxw=',psubfluxw,'pwsubsal=',pwsubsal
  !!!     CALL output2      
  !!!   ENDIF
  !!! ENDIF
  IF (lprint2) then
    WRITE(nerr,*) ", I am in init_sit_viff_gd 2.7: leaving "
    CALL output2
  ENDIF        
  END SUBROUTINE init_sit_viff_gd
! ----------------------------------------------------------------------  
  SUBROUTINE compose_obs_ocean(l_no_expolation_wt,l_no_expolation_ws,l_no_expolation_wu,l_no_expolation_wv)
  !
  ! Initialise sit T,U,V,S on ocean levels (lake, ocean and soil)
  ! Change in-situ water temperature to potential water temperature
  !
    !!! USE mo_mpi,           ONLY: mpp_pe() 
    !!! USE sit_constants_mod,     ONLY: tmelt
    !!! USE mo_control,       ONLY: lwoa_echam,lwoa0_echam
                            
    IMPLICIT NONE
  
    LOGICAL, INTENT(IN):: l_no_expolation_wt   ! logical for missing data
    LOGICAL, INTENT(IN):: l_no_expolation_ws   ! logical for missing data
    LOGICAL, INTENT(IN):: l_no_expolation_wu   ! logical for missing data
    LOGICAL, INTENT(IN):: l_no_expolation_wv   ! logical for missing data  
    INTEGER  :: jk,kkk
    real:: depth  
    
  
    IF (lprint2) then
      WRITE(nerr,*) ", I am in compose_obs_ocean 1.0: start"
      WRITE(nerr,*) "l_no_expolation_wt=",l_no_expolation_wt
      WRITE(nerr,*) "l_no_expolation_ws=",l_no_expolation_ws
      WRITE(nerr,*) "l_no_expolation_wu=",l_no_expolation_wu
      WRITE(nerr,*) "l_no_expolation_wv=",l_no_expolation_wv
      WRITE(nerr,*) "pobswtb=",pobswtb
      WRITE(nerr,*) "pobswsb=",pobswsb
      WRITE(nerr,*) "tmaxden=",tmaxden(pobswsb)
      !!! WRITE(nerr,2300) "pe,","lat,","lon,","step,","k,","z,", "obswt,", "obsws", "obswu,", "obswv"
      !!! DO jk = 0, nle+1
      !!!  !!!      WRITE(nerr,2301) mpp_pe(),plat,plon,istep,jk,z(jk),bg_wt0(jk),bg_ws0(jk),bg_wu0(jk),bg_wv0(jk)
      !!!   WRITE(nerr,2301) mpp_pe(),plat,plon,istep,jk,z(jk),pobswt(jk),pobsws(jk),pobswu(jk),pobswv(jk)
      !!! END DO
    ENDIF

    !
    ! 1.0 using observed SST and SSS for the first guess
    !
    IF (.NOT.l_no_expolation_wt) THEN
      CALL extrapolation_sst(pwlvl,z,nle,pobswtb,pobswsb,mixing_depth,bg_wt0)
      pobswt(0:nle+1)=MERGE(bg_wt0(0:nle+1),pobswt(0:nle+1),pobswt(0:nle+1).eq.xmissing)
    ENDIF
    IF (.NOT.l_no_expolation_ws) pobsws(0:nle+1)=MERGE(pobswsb,pobsws(0:nle+1),pobsws(0:nle+1).eq.xmissing)
    IF (.NOT.l_no_expolation_wu) pobswu(0:nle+1)=MERGE(0.,pobswu(0:nle+1),pobswu(0:nle+1).eq.xmissing)
    IF (.NOT.l_no_expolation_wv) pobswv(0:nle+1)=MERGE(0.,pobswv(0:nle+1),pobswv(0:nle+1).eq.xmissing)
    IF (lprint2) then
      WRITE(nerr,*) ", I am in compose_obs_ocean 2.0: after initial guess"
      WRITE(nerr,*) "pobswtb=",pobswtb
      WRITE(nerr,*) "pobswsb=",pobswsb
      WRITE(nerr,*) "tmaxden=",tmaxden(pobswsb)
      WRITE(nerr,2300) "pe,","lat,","lon,","step,","k,","z,", "obswt,", "obsws", "obswu,", "obswv"
      DO jk = 0, nle+1
        !!!      WRITE(nerr,2301) mpp_pe(),plat,plon,istep,jk,z(jk),bg_wt0(jk),bg_ws0(jk),bg_wu0(jk),bg_wv0(jk)
        WRITE(nerr,2301) mpp_pe(),plat,plon,istep,jk,z(jk),pobswt(jk),pobsws(jk),pobswu(jk),pobswv(jk)
      END DO
    ENDIF
    !
    ! 2.0 modified again according to monthly OCN profile data such as GODAS
    !    
    IF (lwoa_echam) THEN
      CALL interpolation_godas(pwlvl, z, nle, pobswtb, pobswsb, mixing_depth, &
                               pobsseaice,                                    &                               
                               l_no_expolation_wt, l_no_expolation_ws,        &
                               l_no_expolation_wu, l_no_expolation_wv,        &
                               pobswt, pobsws, pobswu, pobswv, pctfreez2)
    
      ! extrapolation for missing values
      IF (lprint2) then
        WRITE(nerr,*) ", I am in compose_obs_ocean 2.0: after monthly OCN profile"
        WRITE(nerr,*) "tmaxden=",tmaxden(pobswsb)
        WRITE(nerr,2300) "pe,","lat,","lon,","step,","k,","z,", "obswt,", "obsws", "obswu,", "obswv"
        DO jk = 0, nle+1
          !!!      WRITE(nerr,2301) mpp_pe(),plat,plon,istep,jk,z(jk),bg_wt0(jk),bg_ws0(jk),bg_wu0(jk),bg_wv0(jk)
          WRITE(nerr,2301) mpp_pe(),plat,plon,istep,jk,z(jk),pobswt(jk),pobsws(jk),pobswu(jk),pobswv(jk)
        END DO
      ENDIF
    ENDIF
    !
    ! 3.0 modified again according to initial OCN profile data such as WOA0
    !
    IF (lwoa0_echam) THEN  
      bg_wt0(0:lkvl+1)=xmissing
      bg_ws0(0:lkvl+1)=xmissing
      bg_wu0(0:lkvl+1)=xmissing
      bg_wv0(0:lkvl+1)=xmissing
      ! CALL interpolation_woa0(.TRUE.,.TRUE.)
      ! using original field in DIECAST for missing values
      ! The above command will crash the model at non-DIECAST grids such as lakes
      CALL interpolation_woa0(l_no_expolation_wt,l_no_expolation_ws,l_no_expolation_wu,l_no_expolation_wv)
      pobswt(0:nle+1)=MERGE(bg_wt0(0:nle+1),pobswt(0:nle+1),bg_wt0(0:nle+1).NE.xmissing)
      pobsws(0:nle+1)=MERGE(bg_ws0(0:nle+1),pobsws(0:nle+1),bg_ws0(0:nle+1).NE.xmissing)
      pobswu(0:nle+1)=MERGE(bg_wu0(0:nle+1),pobswu(0:nle+1),bg_wu0(0:nle+1).NE.xmissing)
      pobswv(0:nle+1)=MERGE(bg_wv0(0:nle+1),pobswv(0:nle+1),bg_wv0(0:nle+1).NE.xmissing)
      IF (lprint2) then
        WRITE(nerr,*) ", I am in compose_obs_ocean 3.0: after initial OCN profile"
        WRITE(nerr,*) "tmaxden=",tmaxden(pobswsb)
        WRITE(nerr,2300) "pe,","lat,","lon,","step,","k,","z,", "obswt,", "obsws", "obswu,", "obswv"
        DO jk = 0, nle+1
          !!!      WRITE(nerr,2301) mpp_pe(),plat,plon,istep,jk,z(jk),bg_wt0(jk),bg_ws0(jk),bg_wu0(jk),bg_wv0(jk)
          WRITE(nerr,2301) mpp_pe(),plat,plon,istep,jk,z(jk),pobswt(jk),pobsws(jk),pobswu(jk),pobswv(jk)
        END DO
      ENDIF
    ENDIF
    !
    ! 4.0 Change from in-situ temperature to potential temperature
    !
    IF (.NOT.lwoa_gfdl) THEN   ! lwoa_gfdl is ptemp (potential temperature already)
      DO jk=0,nle+1
        pobswt(jk)=theta_from_t(pobsws(jk),pobswt(jk)-tmelt,pwlvl-z(jk),0.)+tmelt
      ENDDO
    ENDIF
    !
    IF (lprint2) then
      WRITE(nerr,*) ", I am in compose_obs_ocean 4.0: after setting obswt, obsws"
      WRITE(nerr,*) "tmaxden=",tmaxden(pobswsb)
      WRITE(nerr,2300) "pe,","lat,","lon,","step,","k,","z,", "obswt,", "obsws", "obswu,", "obswv"
      DO jk = 0, nle+1
        WRITE(nerr,2301) mpp_pe(),plat,plon,istep,jk,z(jk),pobswt(jk),pobsws(jk),pobswu(jk),pobswv(jk)
      END DO
    ENDIF
    !
    ! 5.0 Set progonastic variabls pws, pwt, pwu and pwv
    !
    IF (lobs_ocn_rerun) THEN
        IF (lprint2) then
          WRITE(nerr,*) ", I am in compose_obs_ocean 5.0: lobs_ocn_rerun=T"
          WRITE(nerr,*) "nle=",nle
        ENDIF
    !! unchanged
    ELSE
    !! coldstart
      !!! 2.0 Set initial pws and pwt according to pobswsb and pobswtb first 
      !!! pws(0:nle+1)=pobswsb
      pws(0:nle+1)=pobsws(0:nle+1)
      !!! Set some initial values for pwt    
      pwt(0:nle+1)=pobswt(0:nle+1)
      !!! 3.0 Adjust pwt for accounting the ice grid for depth <= 10 m and the limit of pctfreez2
      IF (.NOT.lsoil) pwt(0:nle+1)=MAX(pctfreez2,pwt(0:nle+1))    
      DO jk=0,nle+1
        IF (pzsi(1).GT.0.) THEN
          depth=(pwlvl-z(jk))
          IF (depth.LE.10.) pwt(jk)=pctfreez2
        ENDIF
      END DO
      !!! 4.0 Modification according to observations
      IF (lwoa0_echam.OR.lwoa_echam) THEN
        ! reset initial pws and pwt if observation is available  
        pwt(0:nle+1)=MERGE(pobswt(0:nle+1),pwt(0:nle+1),pobswt(0:nle+1).NE.xmissing)
        pws(0:nle+1)=MERGE(pobsws(0:nle+1),pws(0:nle+1),pobsws(0:nle+1).NE.xmissing)
      ENDIF
    ENDIF  
    IF (lobs_ocn_rerun) THEN
      pwu(0:nle+1)=MERGE(pobswu(0:nle+1),pwu(0:nle+1),pobswu(0:nle+1).NE.xmissing)
      pwv(0:nle+1)=MERGE(pobswv(0:nle+1),pwv(0:nle+1),pobswv(0:nle+1).NE.xmissing)
    ELSE
    ! coldstart
      pwu(0:nle+1)=MERGE(pobswu(0:nle+1),0.,pobswu(0:nle+1).NE.xmissing)
      pwv(0:nle+1)=MERGE(pobswv(0:nle+1),0.,pobswv(0:nle+1).NE.xmissing)
      pww(0:nle+1)=0.
    ENDIF
    IF (lprint2) then
      WRITE(nerr,*) ", I am in compose_obs_ocean 5.0: leaving "
      CALL output2
    ENDIF
    2300 FORMAT(1X,1A4,2A9,2A4,1A13,4A9,2A10,10A9)
    2301 FORMAT(1X,1(I3,","),2(F8.2,","),2(I3,","),F10.0,9(F10.1,","),2(E10.2,","),&
          1(F8.3,","),2(E9.2,","),10(F8.3,","))  
  END SUBROUTINE compose_obs_ocean
  ! ----------------------------------------------------------------------
  SUBROUTINE interpolation_woa0(l_no_expolation_wt,l_no_expolation_ws,l_no_expolation_wu,l_no_expolation_wv)
  !
  !     input
  !      real:: obstsw, ot0, os0, ou0, ov0
  !     output
  !      real:: bg_wt0(0:lkvl+1),bg_ws0(0:lkvl+1)bg_wu0(0:lkvl+1),bg_wv0(0:lkvl+1)
  
    !!! USE mo_interpo,       ONLY: nmw1, nmw2, wgt1, wgt2, ndw1, ndw2, wgtd1, wgtd2                           
  !!!  USE sit_constants_mod,     ONLY: tmelt
  !!!  USE mo_physc2,        ONLY: ctfreez
    !!! USE mo_mpi,           ONLY: p_parallel_io, p_bcast, p_io, mpp_pe() 
    !!! USE mo_sst,           ONLY: nodepth0, odepth0, ot0, os0, ou0, ov0  
    USE eos_ocean_mod,     ONLY: tmaxden  
    !!! USE mo_control,       ONLY: lwoa0_echam
    IMPLICIT NONE
    LOGICAL, INTENT(IN):: l_no_expolation_wt   ! logical for missing data
    LOGICAL, INTENT(IN):: l_no_expolation_ws   ! logical for missing data
    LOGICAL, INTENT(IN):: l_no_expolation_wu   ! logical for missing data
    LOGICAL, INTENT(IN):: l_no_expolation_wv   ! logical for missing data
      ! .TRUE. no expolation, missing value returned
      ! .FALSE. expolation enforced, non-missing value returned
  
    LOGICAL  :: l_upperdata            ! =TRUE, if data of an upper level is available
    INTEGER  :: jk,kkk
    real:: ttt,ttt1,sss,sss1,uuu,uuu1,vvv,vvv1,depth
    IF (lprint2) then
      WRITE(nerr,*) ", I am in interpolation_woa0"
      WRITE(nerr,*) "lwarning_msg=",lwarning_msg
    ENDIF
    IF ((.NOT.lwoa0_echam).AND.((pobswtb.EQ.xmissing).OR.(pobswsb.EQ.xmissing))) THEN
      WRITE(nerr,*) "I am in interpolation_woa0 1.0"
      WRITE(nerr,*) ", FATAL: I am not able to interpolation the grid"
      WRITE(nerr,2300) "pe,","lat,","lon,","step,","k,","z,", "bg_wt0,", "bg_ws0"
      WRITE(nerr,2301) mpp_pe(),plat,plon,istep,jk,z(0),pobswtb,pobswsb
      WRITE(nerr,*) ", pobswtb.or. pobswsb .EQ.xmissing"
      RETURN
    ENDIF
  !!!  CALL pzcord  ! bjt 2010/2/21
  !!
  !! Note that the vertical index of g3b starts from 1 although the index in the sit_vdiff 
  !!   routine starts from 1,
    IF (lprint2) then
      WRITE(nerr,*) "I am in interpolation_woa0 1.0"
      WRITE(nerr,*) "tmaxden=",tmaxden(pobswsb)
      WRITE(nerr,2300) "pe,","lat,","lon,","step,","k,","z,", "bg_wt0,", "bg_ws0", "bg_wu0,", "bg_wv0"
      DO jk = 0, nle+1
        WRITE(nerr,2301) mpp_pe(),plat,plon,istep,jk,z(jk),bg_wt0(jk),bg_ws0(jk),bg_wu0(jk),bg_wv0(jk)
      END DO
    ENDIF
  
  !!!  IF (lprint2) then
  !!!    WRITE(nerr,2300) "pe,","lat,","lon,","step,","k,","z,", "ot0,", "os0"
  !!!  ENDIF   
    DO jk=0,nle+1
      depth=(pwlvl-z(jk))
      IF (lwoa0_echam) THEN
        !! initialized the water profile according to world ocean altas data (woa) data
        kkk=1
        DO WHILE ( (odepth0(kkk).LE.depth) .AND. (kkk.LT.nodepth0) ) 
           kkk=kkk+1
        END DO
        IF (kkk.GT.1) kkk=kkk-1              ! restore back one layer
      ENDIF
  !
  ! 1.0 water salinity
  !
      l_upperdata=.FALSE.
      sss=pobswsb
      sss1=pobswsb
      IF (( lwoa0_echam .AND.                               &
           (os0(kkk).NE.xmissing)   .AND.     &
           (os0(kkk+1).NE.xmissing) .AND.     &
           (depth.LE.odepth0(nodepth0)) )) THEN
        ! depth < max. obs. depth
        ! linearly interpolation (no extrapolation)
        l_upperdata=.TRUE.
        sss=os0(kkk)
        sss1=os0(kkk+1)
        bg_ws0(jk)=sss+ &
          (depth-odepth0(kkk))/(odepth0(kkk+1)-odepth0(kkk))* (sss1-sss)
      ELSE
        ! depth > max obs. depth or missing observed data
        ! water salinity
        IF (l_no_expolation_ws) THEN
        ! no extrapolation
          bg_ws0(jk)=xmissing
        ELSE 
        ! assume to be sss1
          bg_ws0(jk)=sss1
  !!!      ! extrapolation
  !!!        bg_ws0(jk)=sss+ &
  !!!           (depth-odepth0(kkk))/(odepth0(kkk+1)-odepth0(kkk))* (sss1-sss)
        ENDIF
      ENDIF      
  !
  ! 1.1 modify pobswsb/freezing temp again according to initial ocean T/S profile.
  !     Note that pobswsb/freezing temp were firstly setup in ioinitial.f90
  !    
      IF (jk.EQ.0) THEN
        pobswsb=MERGE(bg_ws0(jk),pobswsb,bg_ws0(jk).NE.xmissing)
#ifdef CTFREEZE
#if defined (V9897)
        pctfreez2=tmelt+3.   
#elif defined (TMELTS0)
        pctfreez2=MERGE(tmelts(pobswsb),tmelt,pobswsb.NE.xmissing)
#elif defined (TMELTS1)
        pctfreez2=MERGE(tmelts(pobswsb)+1.,tmelt+1.,pobswsb.NE.xmissing)
#elif defined (TMELTS15)
        pctfreez2=MERGE(tmelts(pobswsb)+1.5_dp,tmelt+1.5_dp,pobswsb.NE.xmissing)
#elif defined (TMELTS2)
        pctfreez2=MERGE(tmelts(pobswsb)+2.,tmelt+2.,pobswsb.NE.xmissing)  ! v9.9003, there is 7600 ice grids, while the observation is 760 ice grid.    
#elif defined (TMELTS25)
        pctfreez2=MERGE(tmelts(pobswsb)+2.,tmelt+2.,pobswsb.NE.xmissing)  ! v9.9007.    
#elif defined (TMELTS3)
        pctfreez2=MERGE(tmelts(pobswsb)+3.,tmelt+3.,pobswsb.NE.xmissing)  ! v9.898 (too hot and too salty in S.H.)
#else
        pctfreez2=MERGE(tmelts(pobswsb),tmelt,pobswsb.NE.xmissing)
#endif
#endif
      ENDIF
  !
  ! 2.0 water temperature
  !
      l_upperdata=.FALSE.
      ttt=pobswtb
      ttt1=pobswtb
      IF (( lwoa0_echam .AND.                               &
           (ot0(kkk).NE.xmissing)   .AND.     &
           (ot0(kkk+1).NE.xmissing) .AND.     &
           (depth.LE.odepth0(nodepth0)) )) THEN
        ! depth < max. obs. depth
        ! linearly interpolation (no extrapolation)
        l_upperdata=.TRUE.
        ttt=ot0(kkk)
        ttt1=ot0(kkk+1)
        bg_wt0(jk)=MAX(pctfreez2,ttt+ &
          (depth-odepth0(kkk))/(odepth0(kkk+1)-odepth0(kkk))* (ttt1-ttt) )
      ELSE
        ! depth > max obs. depth or missing observed data
        ! water temperature
        IF (l_no_expolation_wt) THEN
        ! no extrapolation
          bg_wt0(jk)=xmissing
        ELSE 
        ! extrapolation
          IF (l_upperdata) THEN
            bg_wt0(jk)=MAX(pctfreez2,ttt+ &
               (depth-odepth0(kkk))/(odepth0(kkk+1)-odepth0(kkk))* (ttt1-ttt) )
            IF ((tmaxden(pobswsb).GT.ttt1).AND.(tmaxden(pobswsb).GT.ttt)) THEN
              bg_wt0(jk)=MIN(tmaxden(bg_ws0(jk)),bg_wt0(jk))
            ELSEIF ((tmaxden(pobswsb).LT.ttt1).AND.(tmaxden(pobswsb).LT.ttt)) THEN
              bg_wt0(jk)=MAX(tmaxden(bg_ws0(jk)),bg_wt0(jk))
            ELSE                                      
              bg_wt0(jk)=tmaxden(bg_ws0(jk))
            ENDIF
          ELSE
            ! set initial profile to be expontential decay to tmaxden
            bg_wt0(jk)=wt_maxden(depth,pobswtb,pobswsb,mixing_depth)
          ENDIF
          ! range check
          bg_wt0(jk)=MAX(pctfreez2,bg_wt0(jk))                  
        ENDIF
      ENDIF
  !
  ! 3.0 water u current
  !
      l_upperdata=.FALSE.
      uuu=0.
      uuu1=0.
      IF (( lwoa0_echam .AND.                               &
           (ou0(kkk).NE.xmissing)   .AND.     &
           (ou0(kkk+1).NE.xmissing) .AND.     &
           (depth.LE.odepth0(nodepth0)) )) THEN
        ! depth < max. obs. depth
        ! linearly interpolation (no extrapolation)
        l_upperdata=.TRUE.
        uuu=ou0(kkk)
        uuu1=ou0(kkk+1)
        bg_wu0(jk)=uuu+ &
          (depth-odepth0(kkk))/(odepth0(kkk+1)-odepth0(kkk))* (uuu1-uuu)
      ELSE
        ! depth > max obs. depth or missing observed data
        IF (l_no_expolation_wu) THEN
        ! no extrapolation
          bg_wu0(jk)=xmissing
        ELSE 
        ! extrapolation
          bg_wu0(jk)=0.
  !!!      ! extrapolation
  !!!        bg_wu0(jk)=uuu+ &
  !!!           (depth-odepth0(kkk))/(odepth0(kkk+1)-odepth0(kkk))* (uuu1-uuu)
        ENDIF
      ENDIF
  !
  ! 4.0 water v current
  !
      l_upperdata=.FALSE.
      vvv=0.
      vvv1=0.
      IF (( lwoa0_echam .AND.                               &
           (ov0(kkk).NE.xmissing)   .AND.     &
           (ov0(kkk+1).NE.xmissing) .AND.     &
           (depth.LE.odepth0(nodepth0)) )) THEN
        ! depth < max. obs. depth
        ! linearly interpolation (no extrapolation)
        l_upperdata=.TRUE.
        vvv=ov0(kkk)
        vvv1=ov0(kkk+1)
        bg_wv0(jk)=vvv+ &
          (depth-odepth0(kkk))/(odepth0(kkk+1)-odepth0(kkk))* (vvv1-vvv)
      ELSE
        ! depth > max obs. depth or missing observed data
        IF (l_no_expolation_wv) THEN
        ! no extrapolation
          bg_wv0(jk)=xmissing
        ELSE 
        ! extrapolation
          bg_wv0(jk)=0.
  !!!      ! extrapolation
  !!!        bg_wv0(jk)=vvv+ &
  !!!           (depth-odepth0(kkk))/(odepth0(kkk+1)-odepth0(kkk))* (vvv1-vvv)
        ENDIF
      ENDIF
    ENDDO
  
    IF (lprint2) then
      WRITE(nerr,*) "I am in interpolation_woa0 2.0"
      WRITE(nerr,*) "tmaxden=",tmaxden(pobswsb)
      WRITE(nerr,2300) "pe,","lat,","lon,","step,","k,","z,", "bg_wt0,", "bg_ws0", "bg_wu0,", "bg_wv0"
      DO jk = 0, nle+1
        WRITE(nerr,2301) mpp_pe(),plat,plon,istep,jk,z(jk),bg_wt0(jk),bg_ws0(jk),bg_wu0(jk),bg_wv0(jk)
      END DO
    ENDIF
  
  ! Adjust pwt for accounting the ice grid for depth <= 10 m and the limit of pctfreez2
  !!!  IF (.NOT.lsoil) bg_wt0(0:nle+1)=MERGE(MAX(pctfreez2,bg_wt0(0:nle+1)),xmissing,bg_wt0(0:nle+1).NE.xmissing)
    DO jk=0,nle+1
      IF (pobsseaice.GT.0.) THEN
        depth=(pwlvl-z(jk))
        IF ((depth.LE.10.).AND.(bg_wt0(jk).EQ.xmissing)) bg_wt0(jk)=pctfreez2
      ENDIF
    END DO
       
    IF (lprint2) then
      WRITE(nerr,*) "I am leaving interpolation_woa0"
      WRITE(nerr,*) "tmaxden=",tmaxden(pobswsb)
      WRITE(nerr,2300) "pe,","lat,","lon,","step,","k,","z,", "bg_wt0,", "bg_ws0", "bg_wu0,", "bg_wv0"
      DO jk = 0, nle+1
        WRITE(nerr,2301) mpp_pe(),plat,plon,istep,jk,z(jk),bg_wt0(jk),bg_ws0(jk),bg_wu0(jk),bg_wv0(jk)
      END DO
    ENDIF
  
    2300 FORMAT(1X,1A4,2A9,2A4,1A13,4A9,2A10,10A9)
    !ps 2301 FORMAT(1X,1(I3,","),2(F8.2,","),2(I3,","),1(F10.4,","),10(F8.3,","),2(E10.2,","),&
    2301 FORMAT(1X,1(I3,","),2(F8.2,","),2(I3,","),F10.0,9(F10.1,","),2(E10.2,","),&
    &       1(F8.3,","),2(E9.2,","),10(F8.3,","))
  END SUBROUTINE interpolation_woa0
  ! ----------------------------------------------------------------------
  SUBROUTINE output2
  !!!! A DUPLICATE VERSION of sit_vdiff_init_1d 
  !
  !-----------------------------------------------------------------
  !
  !*    OUTPUT: WRITE DIAGNOSTIC VARIABLES
  !
  !-----------------------------------------------------------------
    !!! USE mo_mpi,           ONLY: mpp_pe()   
    IMPLICIT NONE
  
    INTEGER jk
    real:: tmp
    
  !
  !!!  CALL pzcord   ! bjt 2010/02/21
    IF (nle.GE.1) THEN
  !     water exists
      WRITE(nerr,*) "Water exist, nle=", nle
    ELSE
  !     soil only
      WRITE(nerr,*) "Soil only, nle=", nle
    ENDIF
    WRITE(nerr,2300) "pe,","lat,","lon,","step,","","","we,","lw,","tsi0,","tsi1"
    WRITE(nerr,2301) mpp_pe(),plat,plon,istep,0,0.,pzsi(0),psilw(0),&
  &  ptsnic(0),ptsnic(1)
    WRITE(nerr,2301) mpp_pe(),plat,plon,istep,1,1.,pzsi(1),psilw(1),&
  &  ptsnic(2),ptsnic(3)
  !  
    WRITE(nerr,2300) "pe,","lat,","lon,","step,","k,","z,", "wt,", "ws,", "wu,", "wv,",&
  &  "wtke,", "wkh,","wldisp,","wlmx,","awtfl,","awufl,","awvfl,","awsfl,","awtkefl"
    DO jk = 0, nle+1
      WRITE(nerr,2301) mpp_pe(),plat,plon,istep,jk,z(jk),&
  &     pwt(jk),pws(jk),pwu(jk),pwv(jk),&
  &     pwtke(jk),pwkh(jk),pwldisp(jk),pwlmx(jk), &
  &     pawtfl(jk),pawufl(jk),pawvfl(jk),pawsfl(jk),pawtkefl(jk)
    END DO
      
  !
    RETURN
  !
   2200 FORMAT(1X,3(A4),2(A9),1(A4),4(A10),2(A9),1(A9), 1(A10))
  !ps 2201 FORMAT(1X,4(I3,","),1(I9,","),2(2(E9.2,","),2(F8.3,",")),&
   2201 FORMAT(1X,3(I3,","),2(F8.2,","),1(I3,","),4(F10.3,","),1(I9,","),2(2(E9.2,","),2(F8.3,",")),&
  &       1(F8.3,","), 1(E10.3,","))
  !
   2300 FORMAT(1X,1A4,2A9,2A4,1A11,4A9,2A10,10A9)
   2301 FORMAT(1X,1(I3,","),2(F8.2,","),2(I3,","),1(F10.0,","),1(F8.3,","),1(F8.3,","),&
  &       2(F8.2,","),2(E9.2,","),10(F8.3,","))
  !
  END SUBROUTINE output2
END SUBROUTINE sit_vdiff_init_1d
  
! ****************************************************************************
SUBROUTINE sit_vdiff ( ilon,jlat,istep,  delta_time,    &
                  plat,       plon,                                   &
                  pcoriol,    pslm,       plclass,                    &
                  psitmask,   pbathy,     pwlvl,                      &
                  pocnmask,   obox_mask,                              &
! - same as lake and ml_ocean
                  pfluxw,     pdfluxw,    psoflw,                     &
                  pfluxi,     pdfluxi,    psofli,                                 &
! 2-d SIT vars (wind stress)
                  taucx,      taucy,                                  &
! -    water mass variables(rain, snow, evap and runoff):
                  prsf,      pssf,                                    &
                  pevapw,                                             &
                  pdisch,                                             &
! 2-d SIT vars
                  ptemp2,                                             &
                  pwind10w,                                           &
! 2-d SIT vars (sit variables)
                  pobsseaice, pobswtb,    pobswsb,    &
                  pwtb,       pwub,       pwvb,                       &
                  pwsb,                                               &
                  pfluxiw,    ppme2,                                  &
                  psubfluxw,  pwsubsal,                               &
                  pcc,        phc,        pengwac,                    &
                  psc,        psaltwac,                               &
                  pwtfns,     pwsfns,                                 &
! 3-d SIT vars: snow/ice
                  pzsi,       psilw,      ptsnic,                     &
! 3-d SIT vars: water column
                  pobswt,     pobsws,     pobswu,    pobswv,          &
                  pwt,        pwu,        pwv,       pww,             &
                  pws,        pwtke,      pwlmx,                      & 
                  pwldisp,    pwkm,       pwkh,                       &
                  pwrho1000,                                          &
                  pwtfn,      pwsfn,                                  &
                  pawufl,     pawvfl,     pawtfl,                     &
                  pawsfl,     pawtkefl,                               &
! final output only
                  pseaice,                                            &
                  psni,       psiced,     ptsi,     ptsw,             &    !!
                  ptsl,       ptslm,      ptslm1,                     &
                  pocu,       pocv,       pctfreez2,                  &
! implicit with vdiff
                  pgrndcapc,  pgrndhflx,  pgrndflux                   )

!  ---------------------------------------------------------------------
!
!                           SUBROUTINE DESCRIPTION
!
! 1. FUNCTION DESCRIPTION
!
!     THIS SUBROUTINE CALCULATES WATER BODY THERMOCLINE STRUCTURE
!     AS WELL AS OVER-WATER BODY SNOW/ICE STRUCTURE. THERMOCLINE
!     STRUCTURE IS DETERMINED BY THE TKE METHOD
!     BY COMPUTING pwtke, pwlmx AND pwldisp, AND UPDATE
!     U, T, s DUE TO TURBULENCE MIXING EFFECTS. The design of shallow
!     water mode is to preserve salinity conservation at the conditions
!     water completely evaporated or frozen. The water does not have heat
!     capacity. In addition, it set taucx,taucy to 0 if snow/ice on top
!
! 2. CALLING MODULES
!
!          *sit_vdiff* is called from *physc*.
!
! 3. PARAMETER SPECIFICATION
!     zdtime: time step of the simulation (s), time step of ECHAM     I
!         is used.
!
!      Arguments.
!      ----------
! local dimensions
!  istep    : # of time steps
!  pcoriol : coriol factor = 2*omega*sin(latitudes)  (1/s)                         I
!  plat    : latitide (geg)                                                        I
!  plon    : longitude (deg)                                                       I
! - water body or ml_ocean variables
!  pfluxw    : net surface energy flux over open water per water fraction          I
!              (icesheet+openwater)*(open water fraction) (w/m2) (positive upward)
!              = -(net solar + net longwave - sensible heat - latent heat)*(1-seaice)
!    i.e., fluxw=-(1.-seaice)*(zhflw+zhfsw+ztrflw+zsoflw)
!  pdfluxw   : dG/dT (W/m2/K) over water (positive upward)                                    I
!  pfluxi    : net surface energy flux over icesheet per water fraction            I
!              (icesheet+openwater)*(seaice fraction) (w/m2) (positive upward)
!              = -(net solar + net longwave - sensible heat - latent heat)*(seaice)
!    i.e., fluxi=-seaice*(zhfli+zhfsi+ztrfli+zsofli)
!  pdfluxi   : dG/dT (W/m2/K) over ice (positive upward)                                    I
!  psoflw    : net SW flux over open water per water fraction (w/m2) (positive downward)          I
!    i.e., soflw=(1.-seaice)*zsoflw
!  psofli    : net SW flux over over icesheet per water fraction (w/m2) (positive downward)       I
!    i.e., sofli=seaice*zsofli
!  taucx    : u-stress (Pa) over ice/water at current time step                        I/O
!              (set to tauwx at zero if snow/ice on top)
!  taucy    : v-stress (Pa) over ice/water at current time step                        I
!              (set to tauwy at zero if snow/ice on top)

!  prsf    : large scale + convective rain flux at the surface (kg/m2/s) (positive downward)   I
!  pssf    : large scale + convective snow flux at the surface (kg/m2/s) (positive downward)   I
!  pevapw   : evaporation from water surface (kg/m2/s) (positive downward)         I
!!!!  pruntoc   : surface runoff into ocean (kg/m**2s)                                I
!  pdisch   : surface runoff into ocean (m/s)                                I
!  ptemp2    : 2 m temperature (K) at current time step                            I
!  pwind10w     : 10m windspeed over water (m/s)                                   I
! 2-d SIT vars
!  pobsseaice  : observed sea ice fraction (fraction)                              I
!  pobswtb     : observed bulk sea surface temperature (K)                         I
!  pobswsb    : observed salinity (PSU, 0/00)                                      I
!  psitmask : mask for sit(1=.TRUE., 0=.FALSE.)                                    I
!  pbathy  : bathymeter (topography or orography) of ocean (m)                     I
!  pctfreez2 : ref water freezing temperature (K)                                  I
!  pwlvl  : current water level (ice/water interface) a water body grid            I/O
!  pocnmask : fractional mask for 3-D ocean grid (0-1)                             I
!  obox_mask : 3-D ocean nudging mask, =0: nudging, = 1 (>0): nudging              I
!  pwtb: bulk water temperature (K)                                                O
!  pwub: bulk water u current (m/s)                                                O
!  pwvb: bulk water v current (m/s)                                                O
!  pwsb: bulk water salinity (PSU)                                                 O
!  pfluxiw: over-water net surface heat flux (W/m2, + upward, i.e., from ocean))   O
!  ppme2: net fresh water into ocean (P-E+ice_corr) (m/s, + downward)            O
!  psubfluxw: subsurface ocean heat flux (W/m2, + upward)                          O
!  pwsubsal: subsurface ocean salinity flux (m*PSU/s, + upward)                    O
!  pcc: cold content per water fraction (ice sheet+openwater) (J/m2)
!    (energy need to melt snow and ice, i.e., energy below liquid water at tmelt)  I/O
!  phc: heat content per water fraction (ice sheet+openwater) (J/m2)
!    (energy of a water column above tmelt)                                        I/O
!  pengwac: accumulated energy per water fraction (ice sheet+openwater) (+ downward, J/m2)
!    (pfluxw+pfluxi+rain/snow advected energy in respect to liquid water at
!     tmelt)*dt                                                                    I/O
!!!!  pengw: mean net surface heat flux per water fraction (ice sheet+openwater)
!!!!    (+ downward, W/m2)                                                            I/O
!!!!  pengw2: mean snow corrected net surface heat flux per water fraction
!!!!    (ice sheet+openwater) (+ downward, W/m2)
!!!!    (pfluxw+pfluxi+rain/snow advected heat flux in respect to liquid water at
!!!!     tmelt)                                                                       I/O
!  psc: salinity content per water fraction (ice sheet+openwater) (PSU*m)          I/O
!  psaltwac: accumulated salt into water fraction (+ downward, PSU*m)              I/O
!
! 3-d SIT vars: snow/ice
!  pzsi   :
!        pzsi(0): dry snow water equivalent (m)                                 I/O
!        pzsi(1): dry ice water equivalent (m)                                  I/O
!  psilw  :
!        psilw(0): snow liquid water storage (m)                                I/O
!        psilw(1): ice liquid water storage (m)                                 I/O
!  ptsnic   :
!        ptsnic(0): snow skin temperatrue (jk)                                  I/O
!        ptsnic(1): mean snow temperatrue (jk)                                  I/O
!        ptsnic(2): ice skin temperatrue (jk)                                   I/O
!        ptsnic(3): mean ice temperatrue (jk)                                   I/O
! 3-d SIT vars: water column
!  pobswt: observed potentail water temperature (K)                                I/O
!  pobsws: observed salinity (PSU, 0/00)                                           I/O
!  pobswu: observed u-current (m/s)                                                I/O
!  pobswv: observed v-current (m/s)                                                I/O
!  pwt      : potential water temperature (K)                                      I/O
!  pwu      : u current (m/s)                                                      I/O
!  pwv      : v current (m/s)                                                      I/O
!  pww      : w current (m/s)                                                      I/O
!  pws      : practical salinity (0/00)                                            I/O
!  pwtke    : turbulent kinetic energy (M2/S2)                                     I/O
!  pwlmx    : mixing length (m)                                                    O
!  pwldisp  : dissipation length (m)                                               O
!  pwkm     : eddy diffusivity for momentum (m2/s)                                 O
!  pwkh     : eddy diffusivity for heat (m2/s)                                     O
!  pwrho1000: potential temperature at 1000 m (PSU)                                O
!  pwtfn  : nudging flux into sit-ocean at each level (W/m**2)                     I/O
!  pwsfn  : nudging salinity flux into sit-ocean at each level (PSU*m/s)           I/O
!  pwtfns : nudging flux into sit-ocean for an entire column (W/m**2), i.e.
!     pwtfns=SUM(pwtfn(:))                                                      I/O
!  pwsfn  : nudging salinity flux into sit-ocean for an entire column (PSU*m/s),
!     i.e., pwsfns=SUM(pwsfn(:)                                                 I/O
!  pawufl : advected u flux at each level (positive into ocean) (m/s/s)            I
!  pawvfl : advected v flux at each level (positive into ocean) (m/s/s)            I
!  pawtfl : advected temperature flux at each level (positive into ocean) (K/s)    I
!  pawsfl : advected salinity flux at each level (positive into ocean) (PSU/s)     I
!  pawtkefl : advected tke at each level (positive into ocean) (m3/s3)             I
! - variables internal to physics
!  pslm: land fraction [0-1]                                                       I
!  pseaice   : ice cover (fraction of 1-SLM) (0-1)                                 I/O
!  plclass    :
!  psni      : snow thickness over ice (m in water equivalent)                     I/O
!  psiced    : ice thickness (m in water equivalent)                               I/O
!  ptsw      : skin temperatrue (K) over water                                     O
!  ptsl      : calcuated Earth's skin temperature from vdiff/tsurf at t+dt (K)     I/O
!  ptslm     : calcuated Earth's skin temperature from vdiff/tsurf at t (K)        I/O
!  ptslm1    : calcuated Earth's skin temperature from vdiff/tsurf at t-dt (K)     I/O
! 2-d SIT vars
!  pocu      : ocean eastw. velocity (m/s)                                         O
!  pocv      : ocean northw. velocity (m/s)                                        O
!
!  pgrndcapc: areal heat capacity of the uppermost sit layer (snow/ice/water)
!    (J/m**2/K)                                                                    I/O
!  pgrndhflx: ground heat flux below the surface (W/m**2)
!    (+ upward, into the skin layer)                                               I/O
!  pgrndflx:  acc. ground heat flux below the surface (W/m**2*s)
!    (+ upward, into the skin layer)                                               I/O

!
! 4. LOCAL VARIABLE  (STAGGERED GRID)
!
!     tsim: ptsnic temperatures at pervious time step, respectively (jk)
!     wum: VELOCITY (kk) AT PAST T LVL (M/s)
!     wtm: TEMPERATURE (kk) AT PAST T LVL (jk)
!     wsm: salinity (kk) at past T LVL (KG/KG)
!     wtkem: TURBULENCE KINETIC ENERGY (kk) AT PAST LVL (M2/S2)
!     nle: VERTICAL DIMENSION
!     AA: INPUT TRI-DIAGONAL MATRIX COEF.        (MN,kk,4)
!     RHS: RIGHT HAND SIDE OF THE MATRIX (FORCING)
!     SOL: SOLUTION OF THE MATRIX
!     z: ELEVATION AT THE CENTER OF A GIRD                  (MN,kk+1)
!     z(0): ELEVATION OF THE SURFACE OF THE WATER BODY, ZERO IS DEFAULT (M)
!     z(nle+1): ELEVATION OF THE BOTTOM OF THE WATER BODY (M)
!     beta : PARAMETER TO CONTROL TIME SCHEME ;
!         1.0 -> BACKWARD, 1/2. -> CRANK-NICHOLSON, 0. -> FORWARD.
!     beta2: PARAMETER TO CONTROL TIME SCHEME OF TKE;
!         1.0 -> BACKWARD, 1/2. -> CRANK-NICHOLSON, 0. -> FORWARD.
!
!
!
! 6. USAGE
!
!      CALL thermocline(LKID,jl,JL,istep,zdtime,SRFL,pfluxw,pdfluxw,
!     &                prsf,pssf,
!     &                TSN,SN,istep,lsit_debug,
!     &                pwkm,pwlmx,pwldisp)
!
! 7. FUNCTIONS CALLED
!
!     FFN
!     rhofn
!     LU,LU2
!     pzcord(LKID,jl,nls,nle,z,zlk,hw)
!     LKDIFC(mas,nle,zdtime,z,pwkm,hw,X,Y)
!     lkerr
!
! 8. LIMITATION
!
!     1) Sometimes, the model is unable to judge the ice is completely
!        melted or still some ice remains (recursive more than 4 times).
!        It is because the solar radiation might penetrate into certain
!        depth. That causes the surface energy budget of ice on top
!        slightly differ from water on top. While ice on top, all the
!        solar radiation will be used to melt ice. If water on top, there is
!        some solar radiation penetrate into deeper layer causing the skin
!        colder
!        than tmelt, and water starts to refreeze.
!     2) If the depths of snow, ice & water below their critial depths
!        (csncri,xicri, wcri), their temperature are assumed to be their
!        underneath temperatures in order to prevent "divide by zero error".
!        However, this introduces artifical energies. More rigous tests are
!        needed to decide their existence.
!
!


  IMPLICIT NONE
  !
  ! Arguments
  !

  INTEGER, INTENT(in)                      :: ilon  ! # of lon index
  INTEGER, INTENT(in)                      :: jlat  ! # of lat index
  INTEGER, INTENT(in)                      :: istep  ! # of time steps
  real, INTENT(in):: delta_time                       ! time step of the parent routine (s)

  real, INTENT(in):: plat, plon   
  real, INTENT(in):: pcoriol
  real, INTENT(in):: pslm, plclass
  real, INTENT(in):: psitmask         ! grid mask for lsit (1 or 0)      
  real, INTENT(in out):: pbathy
  real, INTENT(in out):: pwlvl
  real, INTENT(in):: pocnmask         ! (fractional) grid mask for 3-D ocean (DIECAST)
  real, INTENT(in):: obox_mask        ! (fractional) ocean iop nudging mask for 3-D ocean (DIECAST)

! - same as lake and ml_ocean
  real, INTENT(in):: pfluxw, pdfluxw, psoflw
  real, INTENT(in):: pfluxi, pdfluxi, psofli

! 2-d SIT vars (wind stress)
  real, INTENT(in):: taucx,    taucy

! -    water mass variables(rain, snow, evap and runoff):
  real, INTENT(in):: prsf, pssf
  real, INTENT(in):: pevapw
  real, INTENT(in):: pdisch

! 2-d SIT vars
  real, INTENT(in):: ptemp2, pwind10w

! 2-d SIT vars (sit variables)
  real, INTENT(in):: pobsseaice
  real, INTENT(in out):: pobswtb,      pobswsb
  real, INTENT(out):: pwtb, pwub, pwvb, pwsb
  real, INTENT(out):: pfluxiw,ppme2
  real, INTENT(out):: psubfluxw, pwsubsal

  real, INTENT(in out) :: pcc, phc, pengwac
  real, INTENT(in out) :: psc, psaltwac
  real, INTENT(in out) :: pwtfns,  pwsfns
! 3-d SIT vars: snow/ice
  real, INTENT(in out)::                                                &
       pzsi(0:1),   psilw(0:1), ptsnic(0:3)
! 3-d SIT vars: water column
  real, INTENT(in out)::                                                &
       pobswt(0:lkvl+1), pobsws(0:lkvl+1),                       &
       pobswu(0:lkvl+1), pobswv(0:lkvl+1),                       &
       pwt(0:lkvl+1), pwu(0:lkvl+1), pwv(0:lkvl+1),        &
       pww(0:lkvl+1),                                                  &
       pws(0:lkvl+1), pwtke(0:lkvl+1), pwlmx(0:lkvl+1),    & 
       pwldisp(0:lkvl+1), pwkm(0:lkvl+1), pwkh(0:lkvl+1),  &
       pwrho1000(0:lkvl+1)
  real, INTENT(in out):: pwtfn(0:lkvl+1),  pwsfn(0:lkvl+1)
  real, INTENT(in out):: pawufl(0:lkvl+1), pawvfl(0:lkvl+1), pawtfl(0:lkvl+1)
  real, INTENT(in out):: pawsfl(0:lkvl+1), pawtkefl(0:lkvl+1)


! final output only
  real, INTENT(in out):: pseaice
  real, INTENT(in out):: psni, psiced, ptsi, ptsw
  real, INTENT(in out):: ptsl, ptslm, ptslm1
  real, INTENT(in out):: pocu, pocv, pctfreez2

! implicit with vdiff
  real, INTENT(in out) :: pgrndcapc, pgrndhflx, pgrndflux


! - local variables


  !*    1.0 Depth of the coordinates of the water body point. (zdepth = 0 at surface )
!!!  real, PARAMETER:: zdepth(0:lkvl)=(/0.,0.0005_dp,1.,2.,3.,4.,5.,6.,7.,8.,9.,10.,&
!!!              20.,30.,50.,75.,100.,125.,150.,200.,250.,300.,400.,500.,&
!!!              600.,700.,800.,900.,1000.,1100.,1200.,1300.,1400.,1500./) 
  !
  !********************
  ! Note that WOA 2005 data are at depths:
  !  depth = 0, 10, 20, 30, 50, 75, 100, 125, 150, 200, 250, 300, 400, 500, 600,
  !    700, 800, 900, 1000, 1100, 1200, 1300, 1400, 1500 ;
  !********************

  !*    1.1  INFORMATION OF OCEAN GRID (COMLKE)
  !     WATER BODY POINT VERTICAL LEVELS
  INTEGER nls ! first water level -1,
              ! starting water level of the water body point.
              ! z(k)=sitwlvl, IF k is <= nls. 
  INTEGER nle ! nle: last water level (excluding skin level)
              ! nle: maximum water levels of the water body point.
              ! Note nle=-1 IF water level is below point water body bed.
              ! Soil layer always is sitwt(nle+1). If there is no water,
              ! sitwt(nle+1)=sitwt(0). In addition, z(k)=bathy IF k >= nle.
  !!! INTEGER nlv ! number of water levels (excluding water surface &
  !!!             ! soil layer). Note nlv=-1 IF water level is below
  !!!             ! water bed.  
  real, DIMENSION(0:lkvl+1):: z   ! coordinates of the water body point.
  real, DIMENSION(0:lkvl+1):: zlk ! standard z coordinates of a water body 
  real, DIMENSION(0:lkvl):: hw    ! thickness of each layer
  LOGICAL lshlw  ! .true. = shallow water mode (due to evaportion or freezing)
              ! (one layer water body)
  LOGICAL lsoil  ! .true. = soil grid
!
!*      1.1 CONSTANTS (LOCAL PARAMETERS)
!    1) eddy DIFFUSION PARAMETERS (GASPAR ET AL., 1990)
  real,PARAMETER::ck=0.1    ! (0.1 in GASPAR ET AL., 1990) (v9.887)
!!!#if defined (Prw001)
!!!  real,PARAMETER::ck=0.1    ! (0.1 in GASPAR ET AL., 1990) (v9.887)
!!!  real,PARAMETER::Prw=0.01_dp ! 1., Prt_molecular=8.96, (40.,50.)  (v8.6)  0.0006 m2/s ocnkvm 0.016
!!!#elif defined (Prw01)
!!!  real,PARAMETER::ck=0.1    ! (0.1 in GASPAR ET AL., 1990) (v9.887)
!!!  real,PARAMETER::Prw=0.1_dp ! 1., Prt_molecular=8.96, (40.,50.)  (v8.6)  0.0006 m2/s ocnkvm 0.016
!!!#elif defined (Prw1)
!!!  real,PARAMETER::ck=0.1    ! (0.1 in GASPAR ET AL., 1990) (v9.887)
!!!  real,PARAMETER::Prw=1. ! 1., Prt_molecular=8.96, (40.,50.)  (v8.6)  0.0006 m2/s ocnkvm 0.016
!!!#else
!!!  real,PARAMETER::ck=1.0        ! (v9.885 - v9.886 for sensitivity test)  (OK)
!!!  real,PARAMETER::Prw=1. ! 1., Prt_molecular=8.96, (40.,50.) ! (Cold tongue is vanished) ! v9.4
!!!#endif
  real,PARAMETER::ce=0.7    ! (0.7 in Bougeault and Lacarrere, 1989)
!  real,PARAMETER::Prw=10. ! 1., Prt_molecular=8.96, (40.,50.) ! V9.2 v9.3
!  real,PARAMETER::Prw=1. ! 1., Prt_molecular=8.96, (40.,50.) ! (Cold tongue is vanished) ! v9.4
!  real,PARAMETER::Prw=0.04_dp ! 1., Prt_molecular=8.96, (40.,50.)  (v8.6)  0.0006 m2/s ocnkvm 0.016
!  real,PARAMETER::Prw=0.01_dp ! 1., Prt_molecular=8.96, (40.,50.)  (v8.6)  0.0006 m2/s ocnkvm 0.016
!!!  real,PARAMETER::emin=0. !limit min pwtke to 0. to avoid too warm below mixing depth (20090710) (v5.4)(v9.913)
  real,PARAMETER::emin=1.0E-6 !limit min pwtke to 1.0E-6 (v9.865, v9.885) (1.0E-6 in GASPAR ET AL., 1990)
!  real,PARAMETER::emin=1.0E-4 !limit min pwtke to 1.0E-4 (v9.866),(v9.868)
!  real,PARAMETER::emin=1.0E-5 !limit min pwtke to 1.0E-5 (v9.867) (v9.873)
  real,PARAMETER::xkmmin=1.2E-6     ! molecular momentum diffusivity (Paulson and Simpson, 1981; Chia and pwu, 1998; Mellor and Durbin, 1975)
  real,PARAMETER::xkhmin=1.34E-7    ! molecular heat diffusivity (Paulson and Simpson, 1981; Chia and pwu, 1998; Mellor and Durbin, 1975)
!  real,PARAMETER::hcoolskin=4.E-4 ! thickness of conductive sublayer (m), where only molecular transfer exists (m) (Khundzuha et al., 1977; Paulson and Simpson, 1981)
  real,PARAMETER::d0=0.03  ! zero-displacement (m) (0.02,0.05) (v0.9871)
!  real,PARAMETER::d0=0.    ! The wt(nle+1) at 30-50 m is too high, 
                                   ! due to the very small vertical diffusivity but still strong solar radiaion  (v0.9871)
!  real,PARAMETER::xlkmin=0.3        ! minimum pwlmx (0.5,0.8) produced unreasoable high SST near Equator bjt 2007/8, v09.83
  real,PARAMETER::xlkmin=0.        ! minimum pwlmx (0.5,0.8) produced unreasoable high SST near Equator bjt 2013/9, v0.984
  real,PARAMETER::xldispmin=0.3 ! minimum pwldisp
!  real,PARAMETER::xldispmin=0. ! 1., minimum pwldisp, (0.5,0.6)
! optimal (Lotus: Case 2: WT10, emin=3.E-5, xldispmin=2., xlkmin=0., stderr=0.282), no warm-layer
! optimal (Lotus: Case 3: WT10, emin=4.E-5, xldispmin=0.4, xlkmin=0., stderr=0.397)
! optimal (Lotus: Case 4: WT0, emin=3.E-5, xldispmin=0.6, xlkmin=0., stderr=0.260)
! optimal (Lotus: Case 5: WT0, emin=2.E-5, xldispmin=0.03, xlkmin=0.05, stderr=0.529)
! optimal (Lotus: Case 6: WT0, emin=3.E-5, xldispmin=2., xlkmin=0.,d0=0.001, stderr=0.307), no warm-layer
! optimal (Lotus: Case 7: WT0, emin=1.E-5, xldispmin=0.03, xlkmin=0.,d0=0.001, stderr=1.45), has warm-layer
! optimal (TOGA: Case 1: WT0, emin=1.E-6, xldispmin=0., xlkmin=0.)
! optimal (Lotus: WT0, emin=3.E-5, Prw=40., stderr=0.368)
!       v.78
!v90  real,PARAMETER::EResRatio=0.01 ! emin=EResRatio*E(0), 1% of TKE(0) (0.5%,1%) 
!v90  real,PARAMETER::EMPOW=.8               !limit min pwtke to 1.0E-6 (0.5,1)
!v90  real,PARAMETER::EMINMAX=3.E-5 ! 1., minimum pwldisp, (1.E-5,3.E-5)
!v90  real,PARAMETER::EMINMIN=3.E-5 ! 1., minimum pwldisp, (0.5,0.6)
! optimal (Lotus: Case 8: WT0, EResRatio=3%,EMINMAX=1.E-4,EMINMIN=1.E-7,EMPOW=1, xldispmin=1., xlkmin=0.,d0=0.02, stderr=0.54), has warm-layer
! optimal (Lotus: Case 9: WT0, EResRatio=3%,EMINMAX=1.E-4,EMINMIN=2.E-5,EMPOW=1., xldispmin=1., xlkmin=0.,d0=0.02,stderr=0.461), no warm-layer
! optimal (Lotus: Case 10: WT0, EResRatio=2%,EMINMAX=1.E-4,EMINMIN=1.E-6,EMPOW=0.8, xldispmin=1., xlkmin=0.,d0=0.02,stderr=0.537), no warm-layer

! optimal (Lotus: Case 1: WT0, EResRatio=1%, d0=0.03, stderr=1.90)
! optimal (TOGA: Case 1: WT0, EResRatio=1%, d0<0.05)
!  real, PARAMETER:: zepcor=5.e-05           ! minimum corilol force, 5.e-05 at 20 deg. There are still huge current within 20S-20N
!  real, PARAMETER:: zepcor=2*omegas*sin(30./180.*api) ! minimum corilol force  30 deg: 7.29212E-05
!  real, PARAMETER:: zepcor=7.3e-05           ! minimum corilol force, 7.3 e-05 at 30 deg
  real, PARAMETER:: zepcor=0.                 ! minimum corilol force, 7.3 e-05 at 30 deg
  real:: fr=7.E-5                             ! = friction factor for current (1/s). It is due to that one-column model neglect horizontal diffusion.
                                              ! It should be 0. for 3-D ocean. Otherwise oscilliation and current > 3 m/s may occurs.
  
!pwldisp
! Security number
  real, PARAMETER:: SALT_MAX=41. ! maximun salinity
#if defined(SALTI0)
  real, PARAMETER:: salti=0. ! salinity of ice (PSU)  
!!!#else  
!!!  real, PARAMETER:: salti=20. ! salinity of ice (PSU)  
#endif
!
!*    4) PARAMETER TO CONTROL TIME SCHEME; 
!
  real, PARAMETER:: beta=1.
  real, PARAMETER:: beta2=0.5_dp
  real, PARAMETER:: zero_hour=0.
  real, PARAMETER:: one_hour=3600.
  real, PARAMETER:: six_hour=6.*3600.
  real, PARAMETER:: one_day=86400.  
  real, PARAMETER:: one_month=30.*86400.  
!
!     beta: 1.-> BACKWARD,  1/2.-> CRANK-NICOLSON, 0.-> FORWARD.
!
!*    5) Logics to control the run:
!
  LOGICAL, PARAMETER::  lv81=.TRUE.
  LOGICAL, PARAMETER::  lwaterlevel=.FALSE.
! .TRUE. for handeling water level change
! .FALSE. for not fixed water level
  LOGICAL:: lpenetrative_convection=.FALSE.  ! .true. = set penetrative convection
  INTEGER, PARAMETER:: debug_level=1
!!#ifdef ARGCHECK
!!  LOGICAL, PARAMETER:: lssst=.False.
!!#else
!!  LOGICAL, PARAMETER:: lssst=.TRUE.
!!#endif
! .TRUE. for with thermocline skin layer
! .FALSE. for without thermocline skin layer
!!  LOGICAL, PARAMETER:: lsit_ice=.FALSE.
!!  LOGICAL, PARAMETER:: lsit_salt=.FALSE.
  !! .FALSE.=turn off the salinity module
  LOGICAL, PARAMETER:: ldiag=.FALSE.
! LOGICAL, PARAMETER:: lwarning_msg=.TRUE.
!!!  INTEGER, PARAMETER:: sit_ice_option=1
  LOGICAL, PARAMETER:: ldeep_water_nudg=.FALSE.
!! deep water column nudging where obsevation data are not available
  real, PARAMETER:: QFLTI=0.2_dp
  real, PARAMETER:: QFLTA=0.2_dp
    ! QFLTI+QFLTA should be a value within 0-1., a security number
    ! for preventing ocillation. It should be modified with
    ! implicit coupling with the atmospehre for getting 1st order accuracy.
    ! QFLTI: weighting of current time step
    ! QFLTA: weighting of previous time step
    ! (1-QFLTI-QFLTA): weighting of previous time step   
!
!     6) LOCAL VARIABLES, ARRAY
!
  INTEGER:: lstfn     ! index of last fine level
  real, DIMENSION(0:3)::      tsim
  real, DIMENSION(0:lkvl+1):: wtm,wum,wvm,wsm
  real, DIMENSION(0:lkvl+1):: wtkem
  ! potential water density at the surface for T and S at old time step
  real, DIMENSION(0:lkvl+1):: rhom                           
  ! potential water density at one level higher (denoted as "h")
  real, DIMENSION(0:lkvl+1):: pwrhoh,rhomh,pwrho
  real:: pgrndhflx_int
  real:: pfluxiw_int,ppme2_int,pwsubflux_int, pwsubsal_int
  real:: zsf     ! salinity flux (PSU*m) (positive upward)  
  real:: wlvlm  ! old water level (m in elevation)
  real:: heice  ! effective skin thickness of ice (m)
  real:: hice   ! ice thickness (m)
  real:: hesn   ! effective skin thickness of snow (m)
  real:: hsn    ! snow thickness (m)
  real:: hew    ! effective skin thickness of water (m)
  real:: xkhskin   ! skin layer heat diffusivity (m2/s)
  real:: pfluxwm  ! original surface energy flux over open water per water fraction (icesheet+openwater) (W/m2)
  real:: pfluxim  ! original surface energy flux over icesheet per water fraction (icesheet+openwater)(W/m2)
  real:: pfluxw2  ! same as pfluxw but (adv energy included) (w/m2) (positive upward) 
  real:: pfluxi2  ! same as pfluxi but (adv energy included) (w/m2) (positive upward)
  real:: ccm      ! cold content at previous time step (J/m2) 
  real:: utauw         ! water-side friction velocity (m/s)     

  real:: tauwx,    tauwy   ! wind stress over water, They will 0 while iced.

!!!!***************
!!!! Local variables for backgroud initial ocean profiles
!!!  real:: bg_wt0(0:lkvl+1)
!!!  real:: bg_ws0(0:lkvl+1)
!!!  real:: bg_wu0(0:lkvl+1)
!!!  real:: bg_wv0(0:lkvl+1)
!***************
  INTEGER, DIMENSION(1) :: imax, imin
  LOGICAL :: lsitmask   ! sit mask
!!!  INTEGER :: istep                          ! istep=time step
  INTEGER :: i_sit_step, n_sit_step         ! istep=time step
! bjt
  real:: zdtime                         ! sit time step (s)
  real:: acc_time                       ! accum. time (s)
  real:: tmaxb,tminb,tmax,tmin  
! ----------------------------------------------------------------------

! -----------------------------------------------
!!!  istep=999
!  istep=get_time_step()
  lsitmask=psitmask.EQ.1.                          ! convert from REAL to Integer and to Logical
!
! 1.0 Determine time step
!     
  n_sit_step = CEILING(delta_time/900.)
  zdtime=delta_time/DBLE( n_sit_step)

!
! 2.0 Initialization
!     
  IF (lprint1) then
     WRITE(nerr,*) ", I am in sit_vdiff" 
  ENDIF

  IF (lprint1) THEN
!!!    WRITE(nerr,*) "lstart=.FALSE."
    WRITE(nerr,*) "sit_vdiff:"   
    WRITE(nerr,*) "delta_time=",delta_time,"zdtime=",zdtime,"n_sit_step=",n_sit_step
    WRITE(nerr,*) "lsitmask=",lsitmask    
!!!     IF ((ptsw.GE.tmelt+60.).or.(ptsw.LE.tmelt-100.)) THEN
        !!! WRITE(nerr,*) "istep=",istep,"istep=",istep
        WRITE(nerr,*) "ptsw=",ptsw,                       &
           "lat=",plat,"lon=",plon,    &
           "slm=",pslm,"lclass=",plclass,    &
           "sit=",lsitmask
        WRITE(nerr,*) "pobswtb=",pobswtb,                          &
           "lat=",plat,"lon=",plon,    &
           "slm=",pslm,"lclass=",plclass,    &
           "sit=",lsitmask
        WRITE(nerr,*) "sitmask=",lsitmask
        WRITE(nerr,*) "lclass=",plclass
        WRITE(nerr,*) "slm=",pslm
        WRITE(nerr,*) "seaice=",pseaice
        WRITE(nerr,*) "tsw=",ptsw
        WRITE(nerr,*) "obstsw=",pobswtb
        WRITE(nerr,*) "obswsb=",pobswsb
        WRITE(nerr,*) "fluxi=",pfluxi
        WRITE(nerr,*) "dfluxi=",pdfluxi
        WRITE(nerr,*) "sofli=",psofli
        WRITE(nerr,*) "fluxw=",pfluxw
        WRITE(nerr,*) "dfluxw=",pdfluxw
        WRITE(nerr,*) "soflw=",psoflw
        WRITE(nerr,*) "wtfn(10)=",pwtfn(10)
        WRITE(nerr,*) "wsfn(10)=",pwsfn(10)
        WRITE(nerr,*) "disch=",pdisch
        WRITE(nerr,*) "temp2=",ptemp2
        WRITE(nerr,*) "wind10w=",pwind10w
        WRITE(nerr,*) "wlvl=",pwlvl
        WRITE(nerr,*) "evapw=",pevapw
        WRITE(nerr,*) "rsf=",prsf
        WRITE(nerr,*) "ssf=",pssf
!!!        CALL mp_barrier()
!!!        call mpp_error ('sit_vdiff', 'unreasonable ptsw', FATAL)
! 
!!!     ENDIF
  ENDIF


!
! 3.0 Start to work
!     

  IF (lsitmask) THEN     ! sitmask true
    CALL pzcord(pwlvl,pbathy,nls,nle,z,zlk,hw,lsoil,lshlw)  ! bjt 2010/2/21
    IF (lprint2) THEN
      WRITE(nerr,*) ", I am in sit_vdiff 3.1: entering"
      CALL output2
    ENDIF
    pgrndhflx=0.
    pfluxiw=0.
    ppme2=0.
    IF (locn) THEN
      psubfluxw=0.
      pwsubsal=0.
    ENDIF
    IF (locaf) CALL interpolation_ocaf(.TRUE.,.TRUE.)
    IF (lprint2) THEN
      WRITE(nerr,*) ", I am in sit_vdiff 3.2: after interpolation_ocaf"
      CALL output2
    ENDIF
    acc_time=0.
    DO i_sit_step=1, n_sit_step
      acc_time=acc_time+zdtime
      CALL thermocline
      CALL acc_flux
      IF (lprint2) THEN
        WRITE(nerr,*) ", I am in sit_vdiff 3.3: after thermocline"
        CALL output2
      ENDIF
      ! nudging the sit_vdiff grid accoridng to water temperature
      CALL nudging_sit_viff_gd(obox_restore_time,       &
        socn_restore_time,uocn_restore_time,docn_restore_time,   &
        ssit_restore_time,usit_restore_time,dsit_restore_time)
      IF (lprint2) THEN
        WRITE(nerr,*) ", I am in sit_vdiff 3.4: after nudging_sit_viff_gd"
        WRITE(nerr,*) "lsice_nudg=",lsice_nudg      
        CALL output2
      ENDIF
    END DO
    CALL final
  ENDIF
!
! 4.0 Warning message
!         
!    IF (lprint2) then
!       WRITE(nerr,*) ", I am leaving sit_vdiff"
!!!       CALL output2
!    ENDIF

  IF (lprint2) THEN
     WRITE(nerr,*) ", I am leaving sit_vdiff II:"
     IF ((ptsw.GE.tmelt+60.).or.(ptsw.LE.tmelt-100.).or.  &
         (ptsi.GE.tmelt+60.).or.(ptsi.LE.tmelt-100.)) THEN
        WRITE(nerr,*) "istep=",istep,"ilon=",ilon,"jlat=",jlat     
        !!! WRITE(nerr,*) "istep=",istep,"istep=",istep
        WRITE(nerr,*) "ptsw=",ptsw,                       &
           "lat=",plat,"lon=",plon,    &
           "slm=",pslm,"lclass=",plclass,    &
           "sit=",lsitmask
        WRITE(nerr,*) "pobswtb=",pobswtb,                          &
           "lat=",plat,"lon=",plon,    &
           "slm=",pslm,"lclass=",plclass,    &
           "sit=",lsitmask
        WRITE(nerr,*) "sitmask=",lsitmask
        WRITE(nerr,*) "lclass=",plclass
        WRITE(nerr,*) "slm=",pslm
        WRITE(nerr,*) "seaice=",pseaice
        WRITE(nerr,*) "tsi=",ptsi
        WRITE(nerr,*) "tsw=",ptsw
        WRITE(nerr,*) "obstsw=",pobswtb
        WRITE(nerr,*) "obswsb=",pobswsb
        WRITE(nerr,*) "obswt=",pobswt
        WRITE(nerr,*) "obsws=",pobsws
        WRITE(nerr,*) "fluxi=",pfluxi
        WRITE(nerr,*) "dfluxi=",pdfluxi
        WRITE(nerr,*) "sofli=",psofli
        WRITE(nerr,*) "fluxw=",pfluxw
        WRITE(nerr,*) "dfluxw=",pdfluxw
        WRITE(nerr,*) "soflw=",psoflw
        WRITE(nerr,*) "wtfn(10)=",pwtfn(10)
        WRITE(nerr,*) "wsfn(10)=",pwsfn(10)
        WRITE(nerr,*) "disch=",pdisch
        WRITE(nerr,*) "temp2=",ptemp2
        WRITE(nerr,*) "wind10w=",pwind10w
        WRITE(nerr,*) "wlvl=",pwlvl
        WRITE(nerr,*) "evapw=",pevapw
        WRITE(nerr,*) "rsf=",prsf
        WRITE(nerr,*) "ssf=",pssf
!!!        CALL mp_barrier()
        call flush()
        call mpp_error ('sit_vdiff', 'unreasonable ptsw', FATAL)
! 
     ENDIF
  ENDIF
  RETURN
! **********************************************************************
CONTAINS
  !----------------------------------------------------------  
  !*    5.0  SUBROUTINES
  ! **********************************************************************
  SUBROUTINE final
  !
    !!! USE mo_semi_impl,        ONLY: eps
  !!!   USE convect_tables_mod,   ONLY: jptlucu1, jptlucu2
  !   INTEGER, PARAMETER:: jptlucu1 =  50000  ! lookup table lower bound (50K)
  !   INTEGER, PARAMETER:: jptlucu2 = 400000  ! lookup table upper bound (400K) 
    IMPLICIT NONE  
    real:: sumxxz,sumxxt,sumxxu,sumxxv,sumxxs
    real, PARAMETER:: tmin_table=jptlucu1/1000.
    real, PARAMETER:: tmax_table=jptlucu2/1000.
  !!!  real, PARAMETER:: eps=0.001_dp
    INTEGER:: jk
    !!! IF (ltrigsit) THEN
  !!! couple with SIT: return SIT sst and ice to atmosphere model
  !   
  !*     5.1 pgrndcapc, pgrndhflx, current, tsw
  !   
      IF (hesn.GT.csncri) THEN
  !   snow on top
        pgrndcapc=rhosn*csn*hesn
        ptsl=MAX(MIN(ptsl,tmelt),tmin_table)
        ptslm=MAX(MIN(ptslm,tmelt),tmin_table)
        ptslm1=MAX(MIN(ptslm1,tmelt),tmin_table)
      ELSEIF (heice.GT.xicri) THEN
  !   ice on top
        pgrndcapc=rhoice*cice*heice
        ptsl=MAX(MIN(ptsl,tmelt),tmin_table)
        ptslm=MAX(MIN(ptslm,tmelt),tmin_table)
        ptslm1=MAX(MIN(ptslm1,tmelt),tmin_table)
      ELSEIF(.NOT.lsoil) THEN
  !   water on top
        pgrndcapc=rhowcw*hew
        ptsl=MIN(MAX(ptsl,pctfreez2),tmelt+100.)
        ptslm=MIN(MAX(ptslm,pctfreez2),tmelt+100.)
        ptslm1=MIN(MAX(ptslm1,pctfreez2),tmelt+100.)
      ELSE
  !   soil on top
        pgrndcapc=rhogcg*SQRT(xkg/omegas)
      ENDIF
      pocu=pwu(0)
      pocv=pwv(0)
  !*       5.3     Time filter for surface temperature
!!!      IF (.NOT.lstart) THEN
      ptslm1=ptslm+eps*(ptslm1-2.*ptslm+ptsl)
      ptslm=ptsl        
!!!      ELSE
!!!        ptslm1=ptslm
!!!      ENDIF
  !   
  !*    5.2 snow/ice properties
  !   
      IF(lsit_ice) THEN
        psni=pzsi(0)
        !! an extra varible for snow is needed for partial water/partial land,
        !! and modification is need for subroutine albedo for distinquish snow on ice
        !! or snow on land
        psiced=pzsi(1)
  !!  !
  !!  
  !!  ! change to lognormal distribution:
  !!  !
        pseaice=SEAICEFN(psiced)    
        !
        IF (hesn.GT.csncri) THEN
  !     snow on top
          IF(sit_ice_option.EQ.0) THEN
            ptsnic(0)=ptslm1
          ELSE IF (sit_ice_option.EQ.1) THEN
            ptsnic(0)=MAX(ptsnic(0),tmelt-10.)
            ! tmelt-10.: security number
            ! for preventing ocillation
            ! It should be modified with implicit coupling
            ! with the atmospehre.
          ELSE IF (sit_ice_option.EQ.2) THEN
            ptsnic(0)=ptsnic(0)
            ! This also crash after few time steps.
          ELSE IF (sit_ice_option.EQ.3) THEN
            ptsnic(0)=ptsnic(1)
            ! This also crash after few time steps.
          ELSE IF (sit_ice_option.EQ.4) THEN
            ptsnic(0)=MAX(QFLTI*ptsnic(0)+QFLTA*ptemp2+(1.-QFLTI-QFLTA)*tsim(0),tmelt-50.)
            ! This also crash after few time steps.
            ! QFLTI should be a value within 0-1., a security number
            ! for preventing ocillation. It should be modified with
            ! implicit coupling with the atmospehre for getting 1st order accuracy.
          ENDIF
          ptsi=ptsnic(0)
          ptsw=pwt(0)
        ELSEIF (heice.GT.xicri) THEN
  !     ice on top
          IF(sit_ice_option.EQ.0) THEN
            ptsnic(2)=ptslm1
          ELSE IF (sit_ice_option.EQ.1) THEN
            ptsnic(2)=MAX(ptsnic(2),tmelt-10.)
             ! tmelt-10.: security number
             ! for preventing ocillation
             ! It should be modified with implicit coupling
             ! with the atmospehre.
          ELSE IF (sit_ice_option.EQ.2) THEN
            ptsnic(2)=ptsnic(2)
              ! This will creash in few time steps
          ELSE IF (sit_ice_option.EQ.3) THEN
            ptsnic(2)=ptsnic(3)
              ! This will creash in few time steps
          ELSE IF (sit_ice_option.EQ.4) THEN
            ptsnic(2)=MAX(QFLTI*ptsnic(2)+QFLTA*ptemp2+(1.-QFLTI-QFLTA)*tsim(2),tmelt-50.)
              ! QFLTI should be a value within 0-1., a security number
              ! for preventing ocillation. It should be modified with
              ! implicit coupling with the atmospehre for getting 1st order accuracy.
          ENDIF
          ptsi=ptsnic(2)
          ptsw=pwt(0)
        ELSEIF(.NOT.lsoil) THEN
  !     water on top
          IF(sit_ice_option.EQ.0) THEN
            !!! ptsw=ptslm1
            !!! rather using pwt than ptslm1 for ptsw
            ptsw=pwt(0)
          ELSE
            ptsw=pwt(0)
          ENDIF
          ptsi=tmelt
        ELSE
  !     soil on top
          ptsi=tmelt
          ptsw=pwt(nle+1)
        ENDIF
      ELSE
        IF(.NOT.lsoil) THEN
          ptsw=pwt(0) 
        ELSE
  !     soil on top
          ptsw=pwt(nle+1)
        ENDIF
      ENDIF
  !!!????    pobswsb=pws(0)
  !   
  !!!   IF ( (ptsw.GE.400_dp).OR.(ptsw.LT.50.) ) THEN
  !!!     CALL output(hesn,hew,heice,fcew,pfluxwm,wtm,wum,wvm,wsm,wtkem,tsim,mas,mae)
  !!!   ENDIF
  
    !!! ENDIF
  !
  !*   14.4 Calc the uppermost bulk layer properties for coupling with 3-D ocean
  !
  !
  !!!   IF (locn) THEN
      ! Not valid for lwaterlevel=.TRUE.
      sumxxz=0.
      sumxxt=0.
      sumxxu=0.
      sumxxv=0.
      sumxxs=0.
      IF (locn) THEN
        lstfn=MIN(nls+nfnlvl-1,nle)  ! index of last fine level
      ELSE
        lstfn=j10m  ! 10 m depth index
      ENDIF      
      DO jk=nls+1,lstfn
        ! excluidng skin layer
        sumxxz=sumxxz+hw(jk)
        sumxxt=sumxxt+pwt(jk)*hw(jk)
        sumxxu=sumxxu+pwu(jk)*hw(jk)
        sumxxv=sumxxv+pwv(jk)*hw(jk)
        sumxxs=sumxxs+pws(jk)*hw(jk)
        IF ( lwarning_msg.GE.3 ) THEN
          WRITE(nerr,*) 'jk=',jk,'hw=',hw(jk)
        ENDIF
      ENDDO
      pwtb=sumxxt/sumxxz
      pwub=sumxxu/sumxxz
      pwvb=sumxxv/sumxxz
      pwsb=sumxxs/sumxxz

#ifdef DEBUG  
      IF ( locn.AND. ((sumxxz.LE.0.).OR.((pwtb+pwub+pwvb+pwsb+psubfluxw+pwsubsal).LT.-9.E20)) ) THEN
        WRITE(nerr,*) ", I am in thermocline: sumxxz = (<=0)", sumxxz
        WRITE(nerr,*) 'nls+1=',nls+1,'lstfn=',lstfn,'sumxxz=',sumxxz,'sumxxt=',sumxxt,'sumxxu=',sumxxu,'sumxxv=',sumxxv,'sumxxs=',sumxxs
        WRITE(nerr,*) 'pwtb=',pwtb,'pwub=',pwub,'pwvb=',pwvb,'pwsb=',pwsb,'psubfluxw=',psubfluxw,'pwsubsal=',pwsubsal
        WRITE(nerr,*) 'pobswtb=',pobswtb
        CALL output2
      ENDIF
#endif
  !!!  ENDIF
  !
  !*  14.5 Calc ground heat flux for coupling with vdiff/surftemp,
  !        net surface heat/fresh water flux into ocean, and 
  !        subsurface heat/salinity fluxes
  !  
    pgrndhflx=pgrndhflx/acc_time
    pfluxiw=pfluxiw/acc_time
    ppme2=ppme2/acc_time
    IF (locn) THEN
      psubfluxw=psubfluxw/acc_time
      pwsubsal=pwsubsal/acc_time
    ENDIF
  !
  !*  14.6 Calc heat content of a water column above tmelt
  ! 
    IF (nle.GE.1)THEN
  !     water exists
      phc=rhowcw*DOT_PRODUCT((pwt(nls+1:nle)-tmelt),MAX(hw(nls+1:nle),0.))
      psc=DOT_PRODUCT(pws(nls+1:nle),MAX(hw(nls+1:nle),0.))
    ELSE
  !     soil only
      phc=0.
      psc=0.
    ENDIF
    IF (lwaterlevel) THEN
      pengwac=pengwac-delta_time*(  pfluxw+pfluxi        &
        +prsf*clw*(tmelt-ptemp2)                    &
        +pssf*(alf+csn*(tmelt-ptemp2))  )
  !!!  ! Assuming that rain temp to be wtm(nls+1), that of snowfall to be tsim(1) 
  !!!    pengwac=pengwac-delta_time*(  pfluxw+pfluxi        &
  !!!      +prsf*clw*(tmelt-wtm(nls+1))                    &
  !!!      +pssf*(alf+csn*(tmelt-tsim(1)))  )
    ELSE
    ! Neglected adveced heat flux 
      pengwac=pengwac-delta_time*(  pfluxw+pfluxi        &
        +pssf*(alf)  )
    ENDIF
  !!!  pengw=pengw-delta_time*(  pfluxw+pfluxi )
  !!!  pengw2=pengw2-delta_time*(  pfluxw+pfluxi+pssf*(alf)  )
      
    IF (nle.GE.1)THEN
  !     water exists
      pwtfns=rhowcw*DOT_PRODUCT(pwtfn(nls+1:nle),hw(nls+1:nle))
      pwsfns=DOT_PRODUCT(pwsfn(nls+1:nle),hw(nls+1:nle))
    ELSE
  !     soil only
      pwtfns=0.
      pwsfns=DOT_PRODUCT(pwsfn(nls+1:nle),hw(nls+1:nle))
    ENDIF
  END SUBROUTINE final
!----------------------------------------------------------  
  SUBROUTINE acc_flux
! ----------------------------------------------------------------------
!
!*   accumlate fluxes
!
! ----------------------------------------------------------------------
!     pfluxiw: net surface heat flux into ocean (W/m2, + upward)
!     ppme2: net fresh water into ocean (m/s, + downward)
!
!
!*   14.2 pgrndcapc, pgrndhflx, current, tsw
!
   pgrndhflx=pgrndhflx+pgrndhflx_int*zdtime
   pgrndflux=pgrndflux+pgrndhflx_int*(1.-pslm)*zdtime
   pfluxiw=pfluxiw+pfluxiw_int*zdtime
   ppme2=ppme2+ppme2_int*zdtime
   IF (locn) THEN
     psubfluxw=psubfluxw+pwsubflux_int*zdtime
     pwsubsal=pwsubsal+pwsubsal*zdtime
   ENDIF
  END SUBROUTINE acc_flux
!----------------------------------------------------------  
SUBROUTINE thermocline()
!!    input from SURF or calling routine
!v77  INTEGER LKID ! Ocean ID (Caspian Sea, Great Lake have their own ID)
!v77  INTEGER istep ! the corresponding latitude ID for jl
!v77  real:: zdtime   ! time step in sec
!v77  INTEGER istep ! (YYMMDDHH) or (MMDDHHMM) an I8 integer to be used in 
!                Subroutine OUTPUT to add the time stamp of the resut.
!!
!! INPUT & OUTPUT VARIABLE
!!
!! Note that, there are two extra variables from SURF
!! pfluxw & pdfluxs. It is located in module mo_pgrads in this
!! routine. While coupling with ECHAM4, pfluxw and pdfluxs should
!! be included in the Calling variables.
!
!-----------------------------------------------------------------
! 
!!!  USE sit_constants_mod,      ONLY: tmelt, rhoh2o, alf, clw, g, alv, cpd, stbo
  IMPLICIT NONE
!
! 0.0 Calling Variables
!-----------------------------------------------------------------
! 1.1 Local Integer
      
  INTEGER :: jk
  INTEGER mas ! mas: AA matrix staring level
  INTEGER mae ! mae: AA/AAC matrix last level
  INTEGER iderr ! iderr: error id. See Subroutine lkerr for details.
  INTEGER irsv  ! irsv: recursive number 
  INTEGER :: levelm,level,levelp
!
  LOGICAL lwf ! lwf: water freeze/ice melt logic at the interface  
  LOGICAL lim ! lim: ice melt logic at the surface
  LOGICAL lsm ! lsm: snow melt logic at the surface

!
  real, DIMENSION(0:lkvl+5):: X,Y,RHS,SOL
  real, DIMENSION(0:lkvl+5,3):: AA
  COMPLEX(dp), DIMENSION(0:lkvl+1)::wumc,pawuflc
  COMPLEX(dp), DIMENSION(0:lkvl+5)::RHSC,SOLCMPLX
  COMPLEX(dp), DIMENSION(0:lkvl+5,3)::AAC
!
! 1.3 LOCAL VARIABLES
!
  real:: zsoflw ! net solar radiation flux over openwater per water fraction (w/m2) (positive downward)
  real:: zsofli ! net solar radiation flux over ice sheet per water fraction (w/m2) (positive downward)
!v90  real:: emin  !limit min pwtke to 3.0E-5              v78
  real:: fcew   ! calculate FCE of ice/water interface (ice melt positive)
  real:: fcei,fces ! potential phase change energy of ice and snow
  real:: xife   ! ice freezing energy due to liquid water on ice freezed (j/m2)
  real:: sfe    ! snow freezing energy due to liquid water in snow freezed (j/m2)
  real:: fcewm,tmp ! temporary working variables. ??M means previous variable.
  real:: fcewf  ! final phase change energy for water.
  
  real:: sfm    ! temporary zsf
!  real:: depth  
  REAL::hw1m
  
  COMPLEX(dp) :: tauc ! tauc: shear stress at the surface (N/m2)
  COMPLEX(dp) :: fr_corc  ! friction and corilois factor  (1/s)
!
  real:: totice ! total ice above water (snow+snowfall+ice-sublimation) (m)
!
  !
  real:: pzsim       !
  real:: s      ! effective water content
  real:: hair   ! porosity of snow
  real:: hi     ! hight of irreducible water content
!
  real:: gzero  ! test of non-solar energy in Equation (12)
!!!  real:: g0          ! test of non-solar energy in Equation (12)
!
!  real:: alphaffn  !Ratio of soalr absorbtion within skin layer, v92
!
  real:: hcoolskin
! thickness of conductive sublayer (m), where only molecular transfer exists (m) 
!   (Khundzuha et al., 1977; Paulson and Simpson, 1981)
!
  real:: zcor          ! coriolis force   
  real:: dz            ! distance between two density levels/thickness for freeze (m)


!*    2. Initialization
!
!
!
!*    2.2 VERTICAL LEVELS (lkvl=11)
!
!v77      CALL pzcord(LKID,jl,nls,nle,z,zlk,hw,lsoil,lshlw)
!!!  CALL pzcord()  ! bjt 2010/2/21
!
!     2.3 Initialization
!

!      pfluxw=-(pahflw+pahfsw+ptrflw+psoflw)   ! zfluw: positive upward, which is oppsitive to lake subroutine
      zsf=0.
!     
      lwf=.FALSE.
      lim=.FALSE.
      lsm=.FALSE.
!     default all phase change logics are false
      irsv = 0   ! recursive number, this is beginning
!
!     2.4 Store Old Value (Note Soil always at nle+1 level)
!
      pfluxwm=pfluxw
      pfluxw2=pfluxw
      pfluxim=pfluxi
      pfluxi2=pfluxi
      wtm(0:lkvl+1)=pwt(0:lkvl+1)
      wum(0:lkvl+1)=pwu(0:lkvl+1)
      wvm(0:lkvl+1)=pwv(0:lkvl+1)
      wsm(0:lkvl+1)=pws(0:lkvl+1)
      wtkem(0:lkvl+1)=pwtke(0:lkvl+1)
      IF(lsice_nudg.AND.(i_sit_step.EQ.1)) THEN
        IF(ptsi.NE.xmissing) ptsnic(0:3)=ptsi
        IF(psni.NE.xmissing) pzsi(0)=psni
        IF(psiced.NE.xmissing) pzsi(1)=psiced
      ENDIF
!!!      IF(lsit_ice.AND.(i_sit_step.EQ.1)) THEN
!!!        IF(ptsi.NE.xmissing) ptsnic(0:3)=ptsi
!!!        IF(psni.NE.xmissing) pzsi(0)=psni
!!!        IF(psiced.NE.xmissing) pzsi(1)=psiced
!!!      ENDIF
      tsim(0:3)=ptsnic(0:3)
 
      wlvlm=pwlvl
      ppme2_int=0.
      IF (.NOT.lsoil) THEN
        hw1m=hw(nls+1)
        !!! IF (lhd) THEN
!!!          hw(nls+1)=hw(nls+1)+pruntoc*zdtime/rhoh2o     
          hw(nls+1)=hw(nls+1)+pdisch*zdtime
        !!! ENDIF
      ENDIF
!
!     calc cold content ccm (J/m2): the energy to melt snow + ice
!
      IF (lwaterlevel) THEN
      !! take advected heat flux from snow into account
        ccm=pcc+ zdtime*(                                           &
&         ( prsf )*clw*                                  &
&         ( tmelt-ptemp2 ) +                                        &
&         ( pssf )*                                      &
          ( alf+csn*(tsim(1)-ptemp2) )                              &
         )
!       Since the advection energy of snowfall has been included in
!       pfluxi2, the calculated Tsn will equal to temp2 (temp of snowfall).
      ELSE
      !! assuming no advected heat flux from snowfall and rainfall
        ccm=( pssf )*zdtime*( alf ) &
          +pcc
      ENDIF
!-----------------------------------------------------------------
!
!*    3. Precipitation & Evaporation (Sublimation) Events
!
!-----------------------------------------------------------------
  300 CONTINUE
!      PRINT *, ", I am in Precipitation & Evaporation Events."
!     3.1 Calc total ice above water (soil)
      totice=pzsi(0)+pzsi(1)&
&       +( pssf+pevapw)*zdtime/rhoh2o
!
      IF ((totice-pzsi(1).GT.0.).AND.&
&             ( (pzsi(1).GT.0.).OR.lsoil)&
&                )  THEN
!
!*    3.2 snow over ice/ snow over land 
!
        mas=0
        IF (lwaterlevel) THEN
        !! take advected heat flux from snow into account
          pfluxi2=pfluxi2+&
&           ( prsf )*clw*&
&           (tmelt-ptemp2 ) +&
&           ( pssf )*csn*&
&           (tsim(1)-ptemp2)
!         Since the advection energy of snowfall has been included in
!         pfluxi2, the calculated Tsn will equal to temp2 (temp of snowfall).
        ENDIF
        pzsi(0)=totice-pzsi(1)
        psilw(0)=psilw(0)+prsf*zdtime/rhoh2o
      ELSEIF ((totice .GT. 0.).AND.(pzsi(1).GT.0.)) THEN
!
!*    3.3 Ice on top or snow sublimates in this time step completely
!
        mas=2
        IF (lwaterlevel) THEN
        !! take advected heat flux from snow into account
          pfluxi2=pfluxi2                                         &
&          +prsf*clw*(tmelt-ptemp2)          &
&          +pssf*                                &
&           ( csn*(tmelt-ptemp2)                              &
&               +cice*(tsim(3)-tmelt) )                           &
&          +pzsi(0)*rhoh2o/zdtime*                             &
&           ( csn*(tmelt-tsim(1))+cice*(tsim(3)-tmelt) )
        ELSE
          pfluxi2=pfluxi2                                         &
&          +pzsi(0)*rhoh2o/zdtime*                             &
&           ( csn*(tmelt-tsim(1))+cice*(tsim(3)-tmelt) )
        ENDIF
        pzsi(1)=totice
!       merge snow, snowfall into ice & assume sublimation only
        psilw(1)=psilw(1)+psilw(0)&
&         +prsf*zdtime/rhoh2o
!       If there was snow, reset snow progonastic variables & ptsw.
        IF (pzsi(0).GT.0.) THEN
          pzsi(0)=0.
          psilw(0)=0.
!!!          ptsnic(0)=tmelt
!!!          ptsnic(1)=tmelt
!!! set to be undeneath temp instead (v7.7)
        ENDIF
      ELSE
!
!*    3.4 Water/Soil on top, snow & ice sublimate completely in this
!         time step, or snow on water  
!         starting level
        mas=4
        IF (.NOT.lsoil) THEN
!       water on top (Normal & Shallow Water)
!         ref temp of water (1st layer water temp)
          tmp=wtm(nls+1)
        ELSE
!       soil on top
!         ref temp of soil (soil skin temp)
          tmp=wtm(nle+1)
        ENDIF 
        IF (lwaterlevel) THEN
        !! take advected heat flux from snow into account
          pfluxw2=pfluxi2+pfluxw2                                      &
&           +( prsf )*clw*                              &
&              (tmp-ptemp2)                                        &
&           +( pssf )*                                  &
&                ( alf+csn*(tmelt-ptemp2)                          &
&                     +clw*(tmp-tmelt) )                               &
&           +pzsi(0)*rhoh2o/zdtime*                                 &
&                ( alf+csn*(tmelt-tsim(1))+clw*(tmp-tmelt) )           &
&           +psilw(0)*rhoh2o/zdtime* clw*(tmp-tmelt)                &
&           +pzsi(1)*rhoh2o/zdtime*                                 &
&                ( alf+cice*(tmelt-tsim(3))+clw*(tmp-tmelt) )          &
&           +psilw(1)*rhoh2o/zdtime*clw*(tmp-tmelt)
        ELSE
          pfluxw2=pfluxi2+pfluxw2                                      &
&           +( pssf )* alf                              &
&           +pzsi(0)*rhoh2o/zdtime*                                 &
&                ( alf+csn*(tmelt-tsim(1))+clw*(tmp-tmelt) )           &
&           +psilw(0)*rhoh2o/zdtime* clw*(tmp-tmelt)                &
&           +pzsi(1)*rhoh2o/zdtime*                                 &
&                ( alf+cice*(tmelt-tsim(3))+clw*(tmp-tmelt) )          &
&           +psilw(1)*rhoh2o/zdtime*clw*(tmp-tmelt)
        ENDIF
        pfluxi2=0.
!         includes advection flux (positive upward) 
!         assume precipitation temperature is temp2 &
!         snowfall melted released latent heat
!         assume wtm(nls+1) is the temperature of outflow
!         merge snow/ice layers into water
!
        IF (.NOT.lsoil) THEN
          hw(nls+1)=hw(nls+1)+&
&           (prsf+pssf+pevapw)*zdtime/rhoh2o&
&           +pzsi(0)+psilw(0)+pzsi(1)+psilw(1)
          IF (.FALSE.) THEN
            zsf=zsf+(MAX(hw(nls+1),0.)-hw1m)*wsm(nls+1)
          ELSE
            zsf=zsf+&
&             ((prsf+pssf+pevapw)*zdtime/rhoh2o+pzsi(0)+psilw(0))*wsm(nls+1)&
&             +(pzsi(1)+psilw(1))*(wsm(nls+1)-salti)
          ENDIF
!
          DO WHILE (hw(nls+1).LE.0.AND.nle.GE.nls+2)
            hw1m=hw(nls+2)
            IF ((nle-nls).EQ.2) THEN
              hw(nls+2)=MAX(hw1m+hw(nls+1),wcri)
!             preserve wcri for shallow water mode
!             Although it is improper for water conservation, 
!             it's important for salinity conservation.
            ELSE
              hw(nls+2)=hw1m+hw(nls+1)
            ENDIF
            zsf=zsf+(MAX(hw(nls+2),0.)-hw1m)*wsm(nls+2)
            hw(nls+1)=0.
            nls=nls+1
          ENDDO
          IF ((nle-nls).EQ.1) THEN
            lshlw = .TRUE.
!           shallow water body
          ENDIF
!       the minmum thickness of wcri is reserved to perserve 
!       salinity conservation.
        ELSE
          IF (lwaterlevel) THEN
            pwlvl=pwlvl+&
&             (prsf+pssf+pevapw)*zdtime/rhoh2o&
&             +pzsi(0)+psilw(0)+pzsi(1)+psilw(1)
          ENDIF
        ENDIF
!
!       If there was snow or ice, reset snow/ice progonastic
!       variables & ptsw.
        IF (SUM(pzsi(0:1))+SUM(psilw(0:1)).GT.0.) THEN
          pzsi(:)=0.
          psilw(:)=0.
!!!          ptsnic(:)=tmelt
!!!          ptsnic(:)=tmelt
!!! set to be undeneath temp instead (v7.7)
        ENDIF
      ENDIF

!*    3.42 Calculate Effective thickness of snow and ice. Note the the effective thickness can 
!         be zero although its physcial thickness is not due to numerical error.

      CALL update_snow_ice_property(pzsi(0),pzsi(1),hsn,hesn,hice,heice,pseaice)


      !!! hsn=HSNFN(pzsi(0))
      !!! hesn=HEFN(hsn/4.,xksn,omegas)
      !!! hice=HICEFN(pzsi(1))
      !!! heice=HEFN(hice/4.,xkice,omegas)
      !!! pseaice=SEAICEFN(pzsi(1))          
!
!*    3.5 Determine Solar radaition on Water. This value is fixed at
!         current stage to prevent recursive. The model might not be
!         able to determine icemelt or water freeze IF it change
!         accordingly. 
!
!     surface net solar radiation flux over water
!
      
      IF ( (heice.LE.xicri).AND.(hesn.LE.csncri) ) THEN
!     no/or only thin snow and ice on top
        zsoflw=psofli+psoflw
        zsofli=0.
      ELSE
        zsoflw=psoflw
        zsofli=psofli
      ENDIF
 
      IF (mas.EQ.0) THEN
! ----------------------------------------------------------------------
!
!*    4. Snow & Ice Melt Runoff
!        Calc Liquid Water Balance in Snow
!        One-time step is assumed to allow liquid water becoming runoff
!        before it refreezes.
!
! ----------------------------------------------------------------------
  400 CONTINUE
!      PRINT *, ", I am in PAHSE Change Energy."
!
!*    4.1 Chk consistency
!
        IF (hesn.LE.csncri) THEN
!       Small value of snow thickness. Assume all liquid water becoming 
!       runoff to prevent numerical error.
          psilw(1)=psilw(1)+MAX(psilw(0),0.)
!         reset snow progonastic variables
          psilw(0)=0.
        ELSE
!
!     4.2 Calc porosity (hair), irreducible water content (hi),
!         effective water content (s)
!
!         Calc porosity
          hair=pzsi(0)*(rhoh2o/rhosn-rhoh2o/rhoice)
!         irreducible water content
!         csi=0.03-0.07. A value of 0.05 is used
!         hi = hight of irreducible water content
          hi = 0.05_dp*hair
!     4.3 Chk water content > irreducible water content?
          IF (psilw(0).GE.hi) THEN
            IF( psilw(0).LT.hair) THEN
              s=(psilw(0)-hi)/(hair-hi)
            ELSE
!             more liquid water than porosity
!             assume it becomes runoff & store in ice layer
              psilw(1)=psilw(1)+(psilw(0)-hair)
              psilw(0)=hair
              s=1.
            ENDIF
!!
!!    4.4 Calc Runoff (tmp) according to Darcy's Law (Shimizu's Formula)
!!
            tmp=MIN(5.47E6*(0.077_dp*0.9E-3**2)*&
&               Exp(-7.8_dp*rhosn/rhoh2o)*s**3*rhoh2o*zdtime,&
&               psilw(0)-hi)
!           where grain size of 0.9E-3 M is assumed.
!           DTY*G/MU=ALPHA=5.47E6
            psilw(0)=psilw(0)-tmp
            psilw(1)=psilw(1)+tmp
          ELSE
!           water content less than irreducible water content. No runoff.
          ENDIF
        ENDIF
      ENDIF
!
      IF(mas.LE.2) THEN
! ----------------------------------------------------------------------
!
!*    5. Calc Liquid Water Balance in Ice
!
! ----------------------------------------------------------------------
!     5.1 Chk LAGER THAN MAX WATER ON THE TOP OF ICE LAYER
!
        IF (heice.GT.xicri) THEN
          IF (psilw(1).GT.wicemx) THEN
!           ice melt runoff occurs
            IF(.NOT.lsoil) THEN
              hw(nls+1)=hw(nls+1)+psilw(1)-wicemx
              zsf=zsf+(psilw(1)-wicemx)*(wsm(nls+1)-salti)
            ELSE
              IF(lwaterlevel) THEN
                pwlvl=pwlvl+psilw(1)-wicemx
              ENDIF
            ENDIF
            psilw(1)=wicemx
          ENDIF
        ENDIF
      ENDIF
!
! ----------------------------------------------------------------------
!
!*    6. Determine phase change energy of snow and ice & modify snow & ice
!        thickness due to refreezing of liquid water. Assume the refreezing
!        occurs at the center (not surface) of snow, and at the surface of
!        ice. The refreezing will change temperature profiles, which will
!        be evaluated in section 8. Note that, putting this routine after
!        section 6 implies allowing one time step for snow and ice melt
!        becoming runoff before it might refreeze in this section.
!
! ----------------------------------------------------------------------
!
  600 CONTINUE
!      PRINT *, "I am update SN & ICE due to refreeze."
!     6.1 Snow

      IF ( (tsim(1).LT.tmelt).AND.(psilw(0).GT.0.) ) THEN
        sfe=MIN( (tmelt-tsim(1))*rhoh2o*pzsi(0)*csn,&
&         psilw(0)*rhoh2o*alf)
!       refreezing amount
        pzsi(0)=pzsi(0)+sfe/rhoh2o/alf
        !!! hsn=HSNFN(pzsi(0))
        !!! hesn=HEFN(hsn/4.,xksn,omegas)
        psilw(0)=MAX(psilw(0)-sfe/rhoh2o/alf,0.)
      ELSE
        sfe=0.
      ENDIF
!
!     6.2 Ice
!
 620  CONTINUE
      IF ((psilw(1).GT.0.).AND.(heice.GT.xicri)) THEN
        IF (pzsi(0).GT. csncri) THEN
          xife=&
&           +zdtime*rhosn*csn*xksn*(-(tsim(1)-tmelt) )&
&             /(0.5*hsn/pseaice)&
&           +zdtime*rhoh2o*cice*xkice*(tmelt-tsim(3))&
&             /(pzsi(1)*rhoh2o/rhoice/2.)
        ELSE
          xife=&
&           +zdtime*rhoh2o*cice*xkice*(tmelt-tsim(3))&
&             /(pzsi(1)*rhoh2o/rhoice/2.)
        ENDIF
        xife=MIN( xife, psilw(1)*rhoh2o*alf)
!       refreezing amount
        pzsi(1)=pzsi(1)+xife/rhoh2o/alf
        !!! hice=HICEFN(pzsi(1))
        !!! heice=HEFN(hice/4.,xkice,omegas)     
        psilw(1)=MAX(psilw(1)-xife/rhoh2o/alf,0.)
      ELSE
        xife=0.
      ENDIF
      CALL update_snow_ice_property(pzsi(0),pzsi(1),hsn,hesn,hice,heice,pseaice)

      
!
!-----------------------------------------------------------------
!
!*    7.  COMPUTE eddy mixing coeff. pwkm, pwlmx, pwldisp
!
!-----------------------------------------------------------------
  700 CONTINUE
!      PRINT *, ", I am in computing eddy diffusivity."
!
      CALL eddy(hcoolskin,utauw,wtkem,wtm,wsm)
!
!-----------------------------------------------------------------
!
!*    8. PREPARE T MATRIX
!
!-----------------------------------------------------------------
  800 CONTINUE
!      PRINT *, ", I am in Prepare T matrix."
!
!       first level matrix updated by this routine
!
!*    8.1 SNOW LAYER
!
!!! (v7.7)
!!!
  810 CONTINUE
!!!
!!!  810 IF ( mas.EQ.0) THEN
!     snow exists
      IF (hesn.GT.csncri) THEN
!     thick snow
        Y(0) = zdtime/hesn*xksn/(0.5*hsn/pseaice)
        RHS(0)= tsim(0)-(1.-beta)*Y(0)*(tsim(0)-tsim(1))&
&         +zdtime*( -pfluxi2+pdfluxi*tsim(0) )/rhosn/csn/hesn
        AA(0,1)=0.
        AA(0,3)=-beta*Y(0)
        AA(0,2)= 1.+zdtime*pdfluxi/rhosn/csn/hesn                        &
&          - AA(0,1) - AA(0,3)
!       First Layer
        Y(1) = zdtime/hsn*xksn/(0.5*hsn/pseaice)
        X(1)= Y(1)
        RHS(1)= tsim(1)-hesn/hsn*tsim(0) &
&         +sfe/rhosn/csn/hsn+(1.-beta)*&
&         ( X(1)*(tsim(0)-tsim(1))-Y(1)*(tsim(1)-tsim(2)) )
        AA(1,1)=-beta*X(1)-hesn/hsn
        AA(1,3)=-beta*Y(1)
        AA(1,2)= 1. +beta*X(1)+beta*Y(1)
!
      ELSE
!     thin snow
!     assume snow temperature equals to the temperature underneath
        RHS(0)=0.
        AA(0,1)=0.
        AA(0,3)=-1.
        AA(0,2)=1.
!
        RHS(1)=0.
        AA(1,1)=0.
        AA(1,3)=-1.
        AA(1,2)=1.
      ENDIF
!!! (v7.7)
!!!      ENDIF
!
!*    8.2 ICE LAYER
!
  820 CONTINUE
!!!  
!!!  820 IF (mas.LE.2) THEN
      IF (heice.GT.xicri) THEN
!     thick ice
        IF (hesn.GT.csncri) THEN
!       thick snow on top
!          hsn=HSNFN(pzsi(0))
          X(2) = zdtime/(rhoice*cice*heice)*&
&           (rhosn*csn*xksn)/(0.5*hsn/pseaice)
          Y(2) = zdtime/heice*xkice/(0.5*hice/pseaice)
          RHS(2)= tsim(2)+(1.-beta)*&
&           ( X(2)*(tsim(1)-tsim(2))-Y(2)*(tsim(2)-tsim(3)) )&
&           +xife/rhoice/cice/heice
          AA(2,1)=-beta*X(2)
          AA(2,3)=-beta*Y(2)
          AA(2,2)= 1. - AA(2,1) - AA(2,3)
        ELSE
!       thin snow or no snow on top
          Y(2) = zdtime/heice*xkice/(0.5*hice/pseaice)
          RHS(2)= tsim(2)-(1.-beta)*Y(2)*(tsim(2)-tsim(3))               &
            +(  rhosn*csn*hsn*tsim(1)+                                      &
                +zdtime*(-pfluxi2+pdfluxi*tsim(2))+xife  )              &
             /rhoice/cice/heice                                             
          AA(2,1)=0.                                                     
          AA(2,3)=-beta*Y(2)                                                
          AA(2,2)=1.-AA(2,1)-AA(2,3)                                     &
            +(rhosn*csn*hsn+zdtime*pdfluxi)/rhoice/cice/heice
!
        ENDIF
!       First Layer
        Y(3) = zdtime/hice*xkice/(0.5*hice/pseaice)
        X(3)= Y(3)
        RHS(3)= tsim(3)-heice/hice*tsim(2)+(1.-beta)*&
&         ( X(3)*(tsim(2)-tsim(3))-Y(3)*(tsim(3)-wtm(0)) )
        AA(3,1)=-beta*X(3)-heice/hice
        AA(3,3)=-beta*Y(3)
        AA(3,2)= 1. +beta*X(3)+beta*Y(3)
      ELSE
!     thin ice
!     assume ice temperature equals to the temperature underneath
        RHS(2)=0.
        AA(2,1)=0.
        AA(2,3)=-1.
        AA(2,2)=1.
!
        RHS(3)=0.
        AA(3,1)=0.
        AA(3,3)=-1.
        AA(3,2)=1.
      ENDIF
!!!      ENDIF
!
!*    8.3 WATER & SOIL LAYER 
!
  830 IF (.NOT.lsoil) THEN
  !
  !*   Total Fresh Water Inflow = (pwlvl-wlvlm)/zdtime
  !
        ppme2_int=ppme2_int+(pbathy+SUM(hw(nls+1:nle))-wlvlm)/zdtime
        IF (lwaterlevel) THEN
          pwlvl=pbathy+SUM(hw(nls+1:nle))
        ELSE
        ! restore back to original water level, and associtaed hw
          pwlvl=wlvlm
        ENDIF
        CALL pzcord(pwlvl,pbathy,nls,nle,z,zlk,hw,lsoil,lshlw)  ! bjt 2010/2/21
!
        CALL LKDIFKH(hew,X,Y)
!       Skin layer
        IF (lssst) THEN
          IF ( (hesn.LE.csncri).AND.(heice.LE.xicri)) THEN
!           water on top or only thin snow/ice layer exists
            RHS(4)= wtm(0)-(1.-beta)*Y(4)*(wtm(0)-wtm(nls+1))              &
                    +(  rhosn*csn*hsn*tsim(1)+rhoice*cice*hice*tsim(3)        &
                    +zdtime*( zsoflw*(FFN(0.)-FFN(zlk(nls+1)-pwlvl))   &
                      -(pfluxw2+zsoflw)+pdfluxw*wtm(0) )                  &
                     )/rhoh2o/clw/hew
            AA(4,1)=0.
            AA(4,3)=-beta*Y(4)
            AA(4,2)= 1.-AA(4,1)-AA(4,3)                                    &
                    +(  rhosn*csn*hsn+rhoice*cice*hice+zdtime*pdfluxw  )  &
                    /rhoh2o/clw/hew
!!!           IF (lsit_lw) THEN
!!!             AA(4,3)=AA(4,3)-4.*stbo*zdtime*wtm(nls+1)**3/rhoh2o/clw/hew
!!!             AA(4,2)=AA(4,2)+4.*stbo*zdtime*wtm(0)**3/rhoh2o/clw/hew
!!!           ENDIF
         ELSE
!         thick ice or thick snow on top
!           set pwt(0) =pctfreez2
            RHS(4)= pctfreez2
            AA(4,1)=0.
            AA(4,3)=0.
            AA(4,2)=1. - AA(4,1) - AA(4,3)
          ENDIF
        ELSE
!         Without skin layer, skin temperature = first layer temperature
!         so AA(4,3)=-1 & AA(4,2)= 1.  
          RHS(4)=0.
          AA(4,1)=0.
          AA(4,3)=-1.
          AA(4,2)= 1.
        ENDIF
!       First Layer
        jk = 1
        IF (lssst) THEN
          RHS(4+jk)= wtm(nls+jk)-hew/hw(nls+jk)*wtm(0)+(1.-beta)*                &
              ( X(4+jk)*(wtm(0)-wtm(nls+jk))-Y(4+jk)*(wtm(nls+jk)-wtm(nls+jk+1)) )  &
            +zdtime*(                                                               &
              zsoflw*( FFN(zlk(nls+jk)-pwlvl)-FFN(zlk(nls+jk+1)-pwlvl) )    &
                /rhoh2o/clw/hw(nls+jk)                                              &
              +pawtfl(nls+jk)                                                    &
              )
          AA(4+jk,1)=-beta*X(4+jk)-hew/hw(nls+jk)
          AA(4+jk,3)=-beta*Y(4+jk)
          AA(4+jk,2)= 1. +beta*X(4+jk)+beta*Y(4+jk)
          IF (lsit_lw) THEN
            AA(4+jk,3)=AA(4+jk,3)-4.*stbo*zdtime*wtm(nls+jk+1)**3/rhoh2o/clw/hw(nls+jk)
            AA(4+jk,2)=AA(4+jk,2)+4.*stbo*zdtime*wtm(nls+jk)**3/rhoh2o/clw/hw(nls+jk)
          ENDIF
        ELSE
          IF ( (hesn.LE.csncri).AND.(heice.LE.xicri)) THEN
            RHS(4+jk)= wtm(nls+jk)-(1.-beta)*Y(4+jk)*(wtm(nls+jk)-wtm(nls+jk+1))  &
                    +(  rhosn*csn*hsn*tsim(1)+rhoice*cice*hice*tsim(3)               &
                       +zdtime*( zsoflw*(FFN(0.)-FFN(zlk(nls+jk+1)-pwlvl))    &
                         -(pfluxw2+zsoflw)+pdfluxw*wtm(0)                        &
                        )                                                            &
                      )/rhoh2o/clw/hw(nls+jk)                                        &
                       +zdtime*pawtfl(nls+jk)
            AA(4+jk,1)=0.
            AA(4+jk,3)=-beta*Y(4+jk)
            AA(4+jk,2)= 1. - AA(4+jk,1) - AA(4+jk,3)                              &
                    +(  rhosn*csn*hsn+rhoice*cice*hice+zdtime*pdfluxw  )         &
                     /rhoh2o/clw/hw(nls+jk)
            IF (lsit_lw) THEN
              AA(4+jk,3)=AA(4+jk,3)-4.*stbo*zdtime*wtm(nls+jk+1)**3/rhoh2o/clw/hw(nls+jk)
              AA(4+jk,2)=AA(4+jk,2)+4.*stbo*zdtime*wtm(nls+jk)**3/rhoh2o/clw/hw(nls+jk)
            ENDIF
          ELSE
!         thick ice or thick snow on top
!           set pwt(jk) =pctfreez2
            RHS(4+jk)= pctfreez2
            AA(4+jk,1)=0.
            AA(4+jk,3)=0.
            AA(4+jk,2)= 1. - AA(4+jk,1) - AA(4+jk,3)
          ENDIF
        ENDIF
!       Middle & Bottom Layers
        DO jk =2,nle-nls
          RHS(4+jk)= wtm(nls+jk)+(1.-beta)*&
&             ( X(4+jk)*(wtm(nls+jk-1)-wtm(nls+jk))-&
&               Y(4+jk)*(wtm(nls+jk)-wtm(nls+jk+1)) )+&
&           zdtime*( zsoflw* &
&             ( FFN(zlk(nls+jk)-pwlvl)-FFN(zlk(nls+jk+1)-pwlvl) )&
&               /rhoh2o/clw/hw(nls+jk)                                   &
&             +pawtfl(nls+jk) )
          AA(4+jk,1)=-beta*X(4+jk)
          AA(4+jk,3)=-beta*Y(4+jk)
          AA(4+jk,2)= 1. - AA(4+jk,1) - AA(4+jk,3)
          IF (lsit_lw) THEN
            AA(4+jk,1)=AA(4+jk,1)-4.*stbo*zdtime*wtm(nls+jk-1)**3/rhoh2o/clw/hw(nls+jk)
            AA(4+jk,3)=AA(4+jk,3)-4.*stbo*zdtime*wtm(nls+jk+1)**3/rhoh2o/clw/hw(nls+jk)
            AA(4+jk,2)=AA(4+jk,2)+8.*stbo*zdtime*wtm(nls+jk)**3/rhoh2o/clw/hw(nls+jk)
          ENDIF
        ENDDO
!       soil layer
        jk=nle+1-nls
!       
        RHS(4+jk)= wtm(nls+jk)&
&         +(1.-beta)*X(4+jk)*(wtm(nls+jk-1)-wtm(nls+jk))&
&         +zdtime*(zsoflw*FFN(zlk(nls+jk)-pwlvl)+hspg)&
&          /(rhogcg*SQRT(xkg/omegas))  
        AA(4+jk,1)=-beta*X(4+jk)
        AA(4+jk,3)=0.
        AA(4+jk,2)=1. - AA(4+jk,1) - AA(4+jk,3)
        IF (lsit_lw) THEN
          AA(4+jk,1)=AA(4+jk,1)-4.*stbo*zdtime*wtm(nls+jk-1)**3/(rhogcg*SQRT(xkg/omegas))
          AA(4+jk,2)=AA(4+jk,2)+4.*stbo*zdtime*wtm(nls+jk)**3/(rhogcg*SQRT(xkg/omegas))
        ENDIF
!       last level matrix updated by this routine
        mae=5+nle-nls
      ELSE
!     Soil Mode
!
!       water completely frozen or evaporated
!       soil layer (note nle=-1)
        IF  ((hesn.LE.csncri).AND.(heice.LE.xicri)) THEN
!       soil on top
          RHS(4)= wtm(nle+1)&
&           +zdtime*(-pfluxw2+hspg+pdfluxw*wtm(nle+1))&
&            /(rhogcg*SQRT(xkg/omegas))  
          AA(4,1)=0.
          AA(4,3)=0.
          AA(4,2)=1.+zdtime*pdfluxw/(rhogcg*SQRT(xkg/omegas))&
&           -AA(4,1)-AA(4,3)
        ELSEIF (heice.GT.xicri) THEN
!       water completely frozen, with ice above
          X(4) = zdtime/(rhogcg*SQRT(xkg/omegas))&
&           *rhoice*cice*xkice/(pzsi(1)*rhoh2o/rhoice/2.)
          RHS(4)= wtm(nle+1)&
&           +(1.-beta)*X(4)*(tsim(3)-wtm(nle+1))&
&           +zdtime*hspg/(rhogcg*SQRT(xkg/omegas))  
          AA(4,1)=-beta*X(4)
          AA(4,3)=0.
          AA(4,2)=1. - AA(4,1) - AA(4,3)
        ELSEIF (pzsi(0).GT.csncri) THEN
!       snow on top
          X(4) = zdtime/(rhogcg*SQRT(xkg/omegas))&
&           *rhosn*csn*xksn/(0.5*hsn/pseaice)
          RHS(4)= wtm(nle+1)&
&           +(1.-beta)*X(4)*(tsim(1)-wtm(nle+1))&
&           +zdtime*hspg/(rhogcg*SQRT(xkg/omegas))  
          AA(4,1)=-beta*X(4)
          AA(4,3)=0.
          AA(4,2)=1. - AA(4,1) - AA(4,3)
        ENDIF
        mae=4
      ENDIF
!
!-----------------------------------------------------------------
!
!*    9. Update T by Solving a Tri-Diagonal Matrix.
!         Determine Melting Rate in Snow & Ice, Freezing Rate in Water
!         and adjust their water storage (not water equivalent)
!
!-----------------------------------------------------------------
  900 CONTINUE
!      PRINT *, "I am update Temperature."
!
!*    9.1  SOLVE THE TRIDIAGONAL MXTRIX
!
      CALL LU(0,mae,AA,RHS,SOL)
!!!      CALL LU(mas,mae,AA,RHS,SOL)
!
      IF (mas.GE.4.AND..NOT.lsoil) THEN
!     9.2 Water on top: CHK Water Skin Temperature pwt(0) = SOL(4)
        IF ( SOL(4).LT.pctfreez2-tol) THEN
!       freeze
          lwf=.TRUE.
!         set pwt(0) =tmelt
          RHS(4)= pctfreez2
          AA(4,1)=0.
          AA(4,3)=0.
          AA(4,2)=1. - AA(4,1) - AA(4,3)
          CALL LU(0,mae,AA,RHS,SOL)
!!!          CALL LU(mas,mae,AA,RHS,SOL)
!         calculate FCE of ice/water interface (ice melt positive)
!v.76i
          fcew=rhowcw*hew*( -pctfreez2+wtm(0)&
&             -beta*Y(4)*(pctfreez2-SOL(5+nls))&
&             -(1.-beta)*Y(4)*(wtm(0)-wtm(nls+1))  )&
&           +zdtime*(  zsoflw* ( FFN(0.)-FFN(-hw(0)) )&
&             -(pfluxw2+zsoflw) )
!
          iderr=11
          CALL lkerr("Water starts to freeze.",hesn,hew,heice,fcew,pfluxwm,wtm,wsm,tsim,pctfreez2,mas,mae,SOL)
        ELSE
          lwf=.FALSE.
          fcew=0.
        ENDIF
!
      ELSEIF ( ( (mas.GE.1) .OR.&
&        ((mas.EQ.0).AND.(hesn.LE.csncri))  )&
&      .AND.(heice.GT.xicri) )THEN
!
!     9.3 Ice on top: CHK Thick Ice Skin Temperature store in SOL(1)
!
        IF (SOL(2) .GT. (tmelt+tol)) THEN
          lim=.TRUE.
!         set ptsw =tmelt
          RHS(2)= tmelt
          AA(2,1)=0.
          AA(2,3)=0.
          AA(2,2)=1.
          CALL LU(0,mae,AA,RHS,SOL)
!!!          CALL LU(mas,mae,AA,RHS,SOL)
!         calculate FCE of ice surface (ice melt positive)
          fcei=rhoice*cice*heice*&
&           ( -tmelt+tsim(2)-beta*Y(2)*(tmelt-SOL(3))&
&                       -(1.-beta)*Y(2)*(tsim(2)-tsim(3)) )&
&           +zdtime*( -pfluxi2 )+xife
        ELSE
          lim=.FALSE.
          fcei=0.
        ENDIF
      ELSEIF (pzsi(0).GT.csncri) THEN
!
!     9.4 Snow on top: CHK Thick Snow Skin Temperature store in SOL(0)
!
        IF (SOL(0) .GT. (tmelt+tol) ) THEN
          lsm=.TRUE.
!         set ptsw =tmelt
          RHS(0)= tmelt
          AA(0,1)=0.
          AA(0,3)=0.
          AA(0,2)= 1.
!         recalculate temp profile
          CALL LU(0,mae,AA,RHS,SOL)
!!!          CALL LU(mas,mae,AA,RHS,SOL)
!         calculate FCE of snow surface (snow melt positive)
          fces=rhosn*csn*hesn*&
&           ( -tmelt+tsim(0)-beta*Y(0)*(tmelt-SOL(1))&
&                       -(1.-beta)*Y(0)*(tsim(0)-tsim(1)) )&
&           +zdtime*( -pfluxi2 )
        ELSE
          lsm=.FALSE.
          fces=0.
        ENDIF
      ENDIF
!
!     9.5 Determine Phase Change Energy in the ice/water interface
!
      IF ((mas.LT.4).AND.(.NOT.lsoil)) THEN
        lwf=.TRUE.
        IF ( ((mas.EQ.2).AND.(heice.LE.xicri)).OR.&
&            ((mas.EQ.0).AND.(heice.LE.xicri).AND.&
&             (hesn.LE.csncri)) ) THEN
!       thin ice layer only or thin ice & thin snow layer
          fcew=rhowcw*hew*( -SOL(4)+wtm(0)&
&             -beta*Y(4)*(SOL(4)-SOL(5+nls))&
&             -(1.-beta)*Y(4)*(wtm(0)-wtm(nls+1))  )&
&           +zdtime*(  zsoflw* ( FFN(0.)-FFN(-hw(0)) )&
&             -(pfluxi2+pfluxw2+zsoflw) )

!!!          fcew=rhowcw*hew*( -tmelt+wtm(0)&
!!!&             -beta*Y(4)*(tmelt-SOL(5+nls))&
!!!&             -(1.-beta)*Y(4)*(wtm(0)-wtm(nls+1))  )&
!!!&           +zdtime*(  zsoflw* ( FFN(0.)-FFN(-hw(0)) )&
!!!&             -(pfluxw2+zsoflw) )
        ELSEIF (heice.GT.xicri) THEN
!       thick ice layer
          fcew=+(                                                            &
&           (1.-beta)*( rhoice*cice*xkice*(tsim(3)-wtm(0))/(0.5*hice/pseaice)     &
&            -rhowcw*pwkh(nls+1)*(wtm(0)-wtm(nls+1))/(z(0)-z(nls+1))      &
&                     )                                                      &
&           +beta*( rhoice*cice*xkice*(SOL(3)-SOL(4))/(0.5*hice/pseaice)             &
&             -rhowcw*pwkh(nls+1)*(SOL(4)-SOL(5+nls))/(z(0)-z(nls+1))     &
&                 )                                                          &
&           )*zdtime                                                         &
&           +zdtime*(  zsoflw* ( FFN(0.)-FFN(-hw(0)) )                    &
&             -(pfluxw2+zsoflw) )
!         note pwt(0)=wtm(0)=pctfreez2.
        ELSE
!       thin ice layer & thick snow layer
          fcew=+(                                                            &
&           (1.-beta)*( rhosn*csn*xksn*(tsim(1)-tsim(2))/(0.5*hsn/pseaice)        &
&            -rhowcw*pwkh(nls+1)*(wtm(0)-wtm(nls+1))/(z(0)-z(nls+1)) )    &
&           +beta*( rhosn*csn*xksn*(SOL(1)-SOL(2))/(0.5*hsn/pseaice)                 &
&            -rhowcw*pwkh(nls+1)*(SOL(4)-SOL(5+nls))/(z(0)-z(nls+1)) )    &
&           )*zdtime                                                         &
&           +zdtime*(  zsoflw* ( FFN(0.)-FFN(-hw(0)) )                    &
&             -(pfluxw2+zsoflw) )
        ENDIF
        IF (lsit_lw) THEN
          fcew=fcew+zdtime*4.*stbo*(-wtm(nls+1)**3*SOL(4)+wtm(nls+2)**3*SOL(5+nls))
        ENDIF
      ENDIF
!
!*    9.6 Restore Temperatures
!
!*    9.6.1 WATER & SOIL LAYER 
!
      IF (.NOT.lsoil) THEN
        pwt(0) = SOL(4)
        pwt(nls+1:nle+1) = SOL(5:nle+5-nls)
!       Assume the missing layer temperatures to be the first
!         available water temperature, i.e., =pwt(nls+1).
        DO jk = 1, nls, 1
          pwt(jk) = pwt(nls+1)
        END DO
      ELSE
        pwt(nle+1)=SOL(4)
!       SOL(4) is for soil while bare soil or water body completely frozen/
!       evaporated
        DO jk = 0, nle,1
          pwt(jk) = pwt(nle+1)
        END DO
!       Set temp of the missing water to be the temp underneath, soil
!       temp at this case.
      ENDIF
!
!     9.6.2 Ice, Snow & Skin Temp
!!!      IF (mas.LE.2) THEN
!!!        ptsnic(2) = SOL(2)
!!!        ptsnic(3) = SOL(3)
!!!        IF (mas.EQ.0) THEN
!!!          ptsnic(0) = SOL(0)
!!!          ptsnic(1) = SOL(1)
!!!        ENDIF
!!!      ENDIF
      ptsnic(0:3) = SOL(0:3)
!
!v.76i
       gzero=rhoh2o*clw*hew*(pwt(0)-wtm(0))/zdtime-&
&                      zsoflw*( FFN(0.)-FFN(-hw(0)) )+&
&                      rhoh2o*clw*pwkh(nls+1)*(wtm(0)-wtm(1))/(z(0)-z(nls+1))
!v1
!       gzero=-zsoflw*( FFN(0.)-FFN(-hw(0)) )+&
!&                      rhoh2o*clw*pwkh(nls+1)*(wtm(0)-wtm(1))/(z(0)-z(nls+1))
!!!        g0=-(pfluxw2+zsoflw)
!
!-----------------------------------------------------------------
!
!*    10. Determine Melting Rate in Snow & Ice, Freezing Rate in Water
!         and adjust their water storage (not water equivalent)
!         (Note melted water won't refreeze untill next time step)
!
!-----------------------------------------------------------------
 1000 CONTINUE
!      PRINT *, ", I am in 10). PAHSE Change Energy ."
!
!*    10.1 Calc Net Freezing Rate of Water
!
      IF (lwf)THEN
!     water freeze/ice melt in the water-ice interface
        lwf=.FALSE.
!
        IF (fcew.LT.0.) THEN
!       freeze
!-----------------------------------------------------------------------
          fcewf=fcew
          DO WHILE ((fcewf.LE.0.).AND.(nle.GE.nls+1))
            IF (nle.EQ.nls+1) THEN
              dz=hw(nle)-wcri
!             remain minimum water layer of thickness wcri
!             to perserve salinity properity
            ELSE
              dz=hw(nls+1)
            ENDIF
            hw1m=hw(nls+1)
            fcewm=fcewf
            sfm=zsf
            pzsim=pzsi(1)
            tmp=ptsnic(3)
!
            hw(nls+1)=hw1m-dz
            fcewf=fcewf+dz*rhoh2o*&
&             (alf+clw*(pwt(nls+1)-pctfreez2))
            zsf=zsf-dz*(wsm(nls+1)-salti)
            pzsi(1)=pzsi(1)+dz
!           modify ice mean temp due to adding in new freezed
!           ice with temp = pctfreez2
            ptsnic(3)=ptsnic(3)+dz*(pctfreez2-ptsnic(3))/pzsi(1)
            nls=nls+1
          ENDDO
!         roll back one layer to point correct index
          nls=nls-1
          
          IF(fcewf.GT.0.) THEN
            dz=-fcewm/rhoh2o/(alf+clw*(pwt(nls+1)-pctfreez2))
            hw(nls+1)=hw1m-dz
            zsf=sfm-dz*(wsm(nls+1)-salti)
            pzsi(1)=pzsim+dz
            ptsnic(3)=tmp+dz*(pctfreez2-tmp)/pzsi(1)
          ENDIF
          hice=HICEFN(pzsi(1))
          heice=HEFN(hice/4.,xkice,omegas)     
          IF(fcewf.LT.0.) THEN
!         water body freezes completely
            lshlw=.TRUE.
            iderr=4
            CALL lkerr("Water freezes completely.",hesn,hew,heice,fcew,pfluxwm,wtm,wsm,tsim,pctfreez2,mas,mae,SOL)
            pfluxi2=pfluxi2-(fcewf-fcew)/zdtime
            pfluxw2=0.
            lim=.FALSE.
            lsm=.FALSE.
            irsv=irsv+1
!!!            CONTINUE
            IF (irsv.LT.4) THEN
!           recursive less than 4 times
              GOTO 800
!             prepare lvl3 matrix & recalculate temperature profile
            ELSE
              iderr=5
              CALL lkerr("Recursive more than 4 times.",hesn,hew,heice,fcew,pfluxwm,wtm,wsm,tsim,pctfreez2,mas,mae,SOL)
!             The model is unable to judge melt or freeze, just arbitraily
!             pick one.
              CONTINUE
            ENDIF
          ENDIF
        ELSE
!       ice melt
          tmp=fcew/rhoh2o/(alf+cice*(tmelt-ptsnic(3)))
!         tmp is the melting amount of pzsi(1)
          IF (tmp.GE.pzsi(1)) THEN
!         ice melts completely
!         causing snow falling into the water
            pfluxw2=pfluxw2+pfluxi2+(  pzsi(0)*&
&                ( alf+csn*(tmelt-tsim(1))+clw*(wtm(nls+1)-tmelt) )&
&             +pzsi(1)*&
&                ( alf+cice*(tmelt-tsim(3))+clw*(wtm(nls+1)-tmelt) )&
&             +psilw(0)*clw*(wtm(nls+1)-tmelt)&
&             +psilw(1)*clw*(wtm(nls+1)-tmelt)&
&             )*rhoh2o/zdtime
            pfluxi2=0.
            zsoflw=zsoflw+zsofli
            zsofli=0.
            hw(nls+1)=hw(nls+1)&
&             +pzsi(0)+pzsi(1)+psilw(0)+psilw(1)
            zsf=zsf+(pzsi(0)+psilw(0))*wsm(nls+1)&
&             +(pzsi(1)+psilw(1))*(wsm(nls+1)-salti)
!           reset snow/ice progonastic variables
!!!            DO jk=0,1
!!!              pzsi(jk)=0.
!!!              psilw(jk)=0.
!!!              ptsnic(2*jk)=tmelt
!!!              ptsnic(2*jk+1)=tmelt
!!!            ENDDO
            pzsi(:)=0.
            psilw(:)=0.
!!! set to be underneath temp instead
            ptsnic(:)=pwt(0)
!!!
            hice=HICEFN(pzsi(1))
            heice=HEFN(hice/4.,xkice,omegas)
            iderr=7
            CALL lkerr("Ice melts completely due to level 4 forcing.",hesn,hew,heice,fcew,pfluxwm,wtm,wsm,tsim,pctfreez2,mas,mae,SOL)

            lim=.FALSE.
            lsm=.FALSE.
            irsv=irsv+1
!!!            CONTINUE
            IF (irsv.LT.4) THEN
!           recursive less than 4 times
              mas=4
              GOTO 830
!             prepare lvl3 matrix & recalculate temperature profile
            ELSE
              iderr=5
              CALL lkerr("Recursive more than 4 times.",hesn,hew,heice,fcew,pfluxwm,wtm,wsm,tsim,pctfreez2,mas,mae,SOL)
!             The model is unable to judge melt or freeze, just arbitraily
!             pick one.
              CONTINUE
            ENDIF
          ELSE
            pzsi(1)=pzsi(1)-tmp
            hw(nls+1)=hw(nls+1)+tmp
            zsf=zsf+tmp*(wsm(nls+1)-salti)
            hice=HICEFN(pzsi(1))
            heice=HEFN(hice/4.,xkice,omegas)     
          ENDIF
        ENDIF
      ENDIF
!
!*    10.2 Calc Melting Rate of ice
!
      IF (lim) THEN
!     ice melt on the surface
        lim=.FALSE.
!       melts only
        IF (fcei.LT.0.) THEN
!         numerical error
          iderr=2
          CALL lkerr("Ice melts but phase change energy < 0.",hesn,hew,heice,fcew,pfluxwm,wtm,wsm,tsim,pctfreez2,mas,mae,SOL)
          fcei=0.
        ENDIF
        tmp=fcei/rhoh2o/(alf+clw*(tmelt-ptsnic(3)))
!       surface ice melting amount
        IF (tmp.GE.pzsi(1)) THEN
!         ice melts completely
!         causing snow falling into the water
          iderr=8
          CALL lkerr("Ice melts completely due to level 3 forcing.",hesn,hew,heice,fcew,pfluxwm,wtm,wsm,tsim,pctfreez2,mas,mae,SOL)
          pfluxw2=pfluxw2+pfluxi2+(  pzsi(0)*&
&              ( alf+csn*(tmelt-tsim(1))+clw*(wtm(nls+1)-tmelt) )&
&           +pzsi(1)*&
&              ( alf+cice*(tmelt-tsim(3))+clw*(wtm(nls+1)-tmelt) )&
&           +psilw(0)*clw*(wtm(nls+1)-tmelt)&
&           +psilw(1)*clw*(wtm(nls+1)-tmelt)&
&           )*rhoh2o/zdtime
          pfluxi2=0.
          zsoflw=zsoflw+zsofli
          zsofli=0.
          IF (.NOT.lsoil) THEN
            hw(nls+1)=hw(nls+1)+pzsi(0)+pzsi(1)+&
&             psilw(0)+psilw(1)
            zsf=zsf+(pzsi(0)+psilw(0))*wsm(nls+1)&
&             +(pzsi(1)+psilw(1))*(wsm(nls+1)-salti)
          ELSE
            IF (lwaterlevel) THEN
              pwlvl=pwlvl+pzsi(0)+pzsi(1)+&
&               psilw(0)+psilw(1)
            ENDIF
          ENDIF
!         reset snow/ice progonastic variables
!!!          DO jk=0,1
!!!            pzsi(jk)=0.
!!!            psilw(jk)=0.
!!!            ptsnic(2*jk)=tmelt
!!!            ptsnic(2*jk+1)=tmelt
!!!          ENDDO
          pzsi(:)=0.
          psilw(:)=0.
!!! set to be underneath temp instead
          ptsnic(:)=pwt(0)
!!!
          CALL update_snow_ice_property(pzsi(0),pzsi(1),hsn,hesn,hice,heice,pseaice)


          !!! hsn=HSNFN(pzsi(0))
          !!! hesn=HEFN(hsn/4.,xksn,omegas)
          !!! hice=HICEFN(pzsi(1))
          !!! heice=HEFN(hice/4.,xkice,omegas)             
!
          lsm=.FALSE.
          irsv=irsv+1
!!!          CONTINUE
          IF (irsv.LT.4) THEN
!         recursive less than 4 times
            mas=4
            GOTO 830
!           prepare lvl3 matrix & recalculate temperature profile
          ELSE
            iderr=5
            CALL lkerr("Recursive more than 4 times.",hesn,hew,heice,fcew,pfluxwm,wtm,wsm,tsim,pctfreez2,mas,mae,SOL)
!           The model is unable to judge melt or freeze, just arbitraily
!           pick one.
            CONTINUE
          ENDIF
        ELSE
          pzsi(1)=pzsi(1)-tmp
          psilw(1)=psilw(1)+tmp
          CALL update_snow_ice_property(pzsi(0),pzsi(1),hsn,hesn,hice,heice,pseaice)

          !!! hice=HICEFN(pzsi(1))
          !!! heice=HEFN(hice/4.,xkice,omegas)
        ENDIF
      ENDIF
!
!*    10.3 Calc Melting Rate of snow
!
      IF (lsm) THEN
!     snow melt on the surface
        lsm=.FALSE.
!       melts only
        IF (fces.LT.0.) THEN
!         numerical error
          iderr=3
          CALL lkerr("Snow melts but phase change energy <0.",hesn,hew,heice,fcew,pfluxwm,wtm,wsm,tsim,pctfreez2,mas,mae,SOL)
          fces=0.
        ENDIF
        tmp= fces/rhoh2o/(alf+clw*(tmelt-ptsnic(1)))
!       tmp is melted amount of SIWE
!       similar error chk routine as 11.1
        IF (tmp.GE.pzsi(0)) THEN
!         snow melts completely
          iderr=12
!          CALL lkerr("Snow melts completely.",hesn,hew,heice,fcew,pfluxwm,wtm,wsm,tsim,pctfreez2,mas,mae,SOL)
          pfluxi2=pfluxi2+(  pzsi(0)*( alf+csn*(tmelt-tsim(1)) )&
&             )*rhoh2o/zdtime
          psilw(1)=psilw(1)+pzsi(0)+psilw(0)
!         reset snow progonastic variables
          DO jk=0,0
            pzsi(jk)=0.
            psilw(jk)=0.
!!            ptsnic(2*jk)=tmelt
!!            ptsnic(2*jk+1)=tmelt
          ENDDO
!!! set to be underneath temp instead
          ptsnic(0:1)=ptsnic(2)
!
          CALL update_snow_ice_property(pzsi(0),pzsi(1),hsn,hesn,hice,heice,pseaice)

          !!! hsn=HSNFN(pzsi(0))
          !!! hesn=HEFN(hsn/4.,xksn,omegas)
!
          irsv=irsv+1
          IF (irsv.LT.4) THEN
!           recursive less than 4 times
!!!            CONTINUE
            IF (pzsi(1) .GT.xicri) THEN
              mas=2
              GOTO 820
            ELSE
              mas=4
              GOTO 830
            ENDIF
!           prepare lvl2 matrix & recalculate temperature profile
          ELSE
            iderr=5
            CALL lkerr("Recursive more than 4 times.",hesn,hew,heice,fcew,pfluxwm,wtm,wsm,tsim,pctfreez2,mas,mae,SOL)
!           The model is unable to judge melt or freeze, just arbitraily
!           pick one.
            CONTINUE
          ENDIF
        ELSE
          pzsi(0)=pzsi(0)-tmp
          psilw(0)=psilw(0)+tmp
          CALL update_snow_ice_property(pzsi(0),pzsi(1),hsn,hesn,hice,heice,pseaice)

          !!! hsn=HSNFN(pzsi(0))
          !!! hesn=HEFN(hsn/4.,xksn,omegas)
        ENDIF
      ENDIF
      
! ----------------------------------------------------------------------
      IF ((.NOT.lsoil).AND.(lsit_salt)) THEN
! ----------------------------------------------------------------------
!
!*   11. UPDATE SALINITY DUE TO VERTICAL MIXING
!        BY SOLVING A TRI-DIAGONAL MATRIX:
!
! ----------------------------------------------------------------------
 1100 CONTINUE
!      PRINT *, ", I am in 11) UPDATE Salinity."
        CALL LKDIFKH(hew,X,Y)  
!
!
!* 11.0 Calc. the salinity flux
!
!      
!*   pwlvl=pbathy+SUM(hw(nls+1:nle))
!*   Total Fresh Water Inflow = (pwlvl-wlvlm)/zdtime
!
!!!     ppme2_int=(pbathy+SUM(hw(nls+1:nle))-wlvlm)/zdtime
!!! bjt 2010/2/22
!!!     IF(.NOT.lwaterlevel) THEN
!!!       zsf=-ppme2_int*wsm(nls+1)
!!!     ENDIF
!
     psaltwac=psaltwac-zsf
!
!* 11.1 SKIN LAYER
!
        jk = 0
        IF (.FALSE.) THEN
        !!! v7.5 for including runoff salt flux. (2010/5/8) (bjt)
        !!! Since runoff from river usually enters a water column in the top few meters,
        !!! not only in the skin layer
!!!        IF (lssst) THEN
          RHS(4)= wsm(0)-(1.-beta)*Y(4)*(wsm(0)-wsm(nls+1))-zsf/hew
!         Total Fresh Water Inflow = (pwlvl-wlvlm)/zdtime
          AA(4,1)=0.
          AA(4,3)=-beta*Y(4)
          AA(4,2)= 1. - AA(4,1) - AA(4,3)
        ELSE
          RHS(4)=0.
          AA(4,1)=0.
          AA(4,3)=-1.
          AA(4,2)= 1.
        ENDIF
!
!* 11.2 FIRST LAYER
!
        jk=1
        IF (.FALSE.) THEN
        !!! v7.5 for including runoff salt flux. (2010/5/8) (bjt)
        !!! Since runoff from river usually enters a water column in the top few meters,
        !!! not only in the skin layer
!!!        IF (lssst) THEN
          RHS(4+jk)= wsm(nls+jk)-hew/hw(nls+jk)*wsm(0)+ &
&          (1.-beta)*&
&          ( X(4+jk)*(wsm(0)-wsm(nls+jk))-&
&            Y(4+jk)*(wsm(nls+jk)-wsm(nls+jk+1)) )+ &
&          zdtime*pawsfl(nls+jk)
          AA(4+jk,1)=-beta*X(4+jk)-hew/hw(nls+jk)
          AA(4+jk,3)=-beta*Y(4+jk)
          AA(4+jk,2)= 1. +beta*X(4+jk)+beta*Y(4+jk)
        ELSE
          RHS(4+jk)= wsm(nls+jk)-(1.-beta)*Y(4+jk)*(wsm(0)-wsm(nls+jk)) &
&          -zsf/hw(nls+jk)+zdtime*pawsfl(nls+jk)
          AA(4+jk,1)=0.
          AA(4+jk,3)=-beta*Y(4+jk)
          AA(4+jk,2)= 1. - AA(4+jk,1) - AA(4+jk,3)
        ENDIF
!
!       Note AA Matrix from jk=5 to nle is the same as pwt matrix
!
!* 11.3 MIDDLE LAYERS (jk=2,nle)
!
        DO jk=2,nle-nls
!         If nle less than nls+2, do-loop won't do anything.
          RHS(jk+4)= wsm(nls+jk)+(1.-beta)*(X(jk+4)*(wsm(nls+jk-1)-wsm(nls+jk))-&
&           Y(jk+4)*(wsm(nls+jk)-wsm(nls+jk+1)))+ &
&           zdtime*pawsfl(nls+jk)
          AA(4+jk,1)=-beta*X(4+jk)
          AA(4+jk,3)=-beta*Y(4+jk)
          AA(4+jk,2)= 1. - AA(4+jk,1) - AA(4+jk,3)
        END DO
!
!* 11.4 SOIL LAYER (jk=nle+1=G)
!
!       assume equal to the salinity of previous layer, LVL nle
        RHS(5+nle-nls)=0.
        AA(5+nle-nls,1)=-1.
        AA(5+nle-nls,3)=0.
        AA(5+nle-nls,2)=1.
!
!* 11.5 SOLVE THE TRIDIAGONAL MXTRIX
!
        CALL LU(4,5+nle-nls,AA,RHS,SOL)
!
!* 11.6 Restore salinity profile
!
        pws(0) = MIN(MAX(SOL(4),0.),SALT_MAX)
        DO jk = 1, nle+1-nls
          pws(nls+jk) = MIN(MAX(SOL(jk+4),0.),SALT_MAX)
        END DO
!       Assume the missing layer salinity to be the first
!       available salinity, i.e., =pws(nls+1).
        DO jk = 1, nls, 1
          pws(jk) = pws(nls+1)
        END DO
!
      ENDIF
!
!* 11.7 Update seawater density for new salinity and temperature
!
     DO jk=0,nle+1
       !! pwrho(jk)=rho_from_theta(pws(jk),pwt(jk)-tmelt,0.)     ! density at surface (0 m depth)
       pwrho(jk)=rho_from_theta(pws(jk),pwt(jk)-tmelt,pwlvl-z(jk))     ! in situ density
       pwrho1000(jk)=rho_from_theta(pws(jk),pwt(jk)-tmelt,1000.)     ! potential temperature at 1000 m depth
       IF (jk.GE.1) pwrhoh(jk)=rho_from_theta(pws(jk-1),pwt(jk-1)-tmelt,pwlvl-z(jk))     ! in situ density
     ENDDO

!
!*   12. UPDATE U, V DUE TO VERTICAL MIXING
! ----------------------------------------------------------------------
!      IF (.NOT.(lsoil.OR.(locn.AND.(pocnmask.GT.0.).AND.(ocn_couple_option.NE.11)))) THEN
      IF ( .NOT.lsoil.AND.(.NOT.locn.OR.(pocnmask.LE.0.).OR.(ocn_couple_option.EQ.11)) ) THEN
! ----------------------------------------------------------------------
!
!*   12. UPDATE U, V DUE TO VERTICAL MIXING
!          BY SOLVING A COMPLEX(dp) :: TRI-DIAGONAL MATRIX:
!
! ----------------------------------------------------------------------
!      PRINT *, ", I am in 12) UPDATE U,V."
!
!*   12.1 Update Outflow Velocity & Water Level of Ocean
!
!      OUTFLW=OUTFLM+MAX(pwlvl-z(0),0.)
!      pwlvl=MIN(pwlvl,z(0))
!
        CALL LKDIFKM(hew,X,Y)
!
!*    this line is moved to Ocean Sub
!
!!!        IF (lsit_ice.AND.mas.LT.4) THEN
        IF (mas.LT.4) THEN
!         set tauwx and tauwy to zero if snow/ice on top
          tauwx=0.
          tauwy=0.
        ELSE
          tauwx=taucx
          tauwy=taucy
        ENDIF
        tauc=tauwx*(1.,0.)+tauwy*(0.,1.)
!!
!
!*   12.2 PREPARE CURRENT VECTOR
!
 1220   CONTINUE 
        IF(locn.AND.(ocn_couple_option.EQ.11).AND.(pocnmask.GT.0.)) THEN
        ! ocean grids: taking care in mo_ocean
          fr=0.
          zcor=0.
        ELSE
          zcor=SIGN(MAX(ABS(pcoriol),zepcor),pcoriol)  ! v9.8b: 20130822
          fr=7.E-5                                     ! = friction factor for current (1/s).
                                                       ! It is due to that one-column model neglect horizontal diffusion.
        ENDIF
        fr_corc=fr*(1.,0.)+zcor*(0.,1.)

!        PRINT *, "istep=",istep,"pe=",mpp_pe(),", I am in UPDATE U,V., 12.2."
        DO jk=0,nle+1
          wumc(jk)=wum(jk)*(1.,0.)+wvm(jk)*(0.,1.)
          pawuflc(jk)=pawufl(jk)*(1.,0.)+pawvfl(jk)*(0.,1.)
        ENDDO
!
!*   12.3 SKIN LAYER
!
!      PRINT *, "istep=",istep,"pe=",mpp_pe(),", I am in UPDATE U,V., 12.3."
        jk=0
        IF (lssst) THEN
          IF (lv81) THEN
            RHSC(4)= wumc(0)&
&                   -(1.-beta2)*zdtime*fr_corc*wumc(0)&
&                   +zdtime*tauc/rhoh2o/hew&
&                   -(1.-beta)*Y(4)*(wumc(0)-wumc(nls+1))
            AAC(4,1)=(0.,0.)
            AAC(4,3)=-beta*Y(4)*(1.,0.)
            AAC(4,2)= (1.,0.) - AAC(4,1) - AAC(4,3)&
&                   +beta2*zdtime*fr_corc
          ELSE
            RHSC(4)= wumc(0)&
&                   +zdtime*tauc/rhoh2o/hew&
&                   -(1.-beta)*Y(4)*(wumc(0)-wumc(nls+1))
            AAC(4,1)=(0.,0.)
            AAC(4,3)=-beta*Y(4)*(1.,0.)
            AAC(4,2)= (1.,0.) - AAC(4,1) - AAC(4,3)
          ENDIF
        ELSE
          RHSC(4)=(0.,0.)
          AAC(4,1)=(0.,0.)
          AAC(4,3)=(-1.,0.)
          AAC(4,2)= (1.,0.)
        ENDIF
!
!     Note: In the equator, gl_coriol is too small to balance the wind
!     shear. Extra friction might be needed.
!
!*   12.5 FIRST LAYER
!
!      PRINT *, "istep=",istep,"pe=",mpp_pe(),", I am in UPDATE U,V., 12.4."
        jk=1
        IF (lssst) THEN
          IF(lv81) THEN
            RHSC(4+jk)= wumc(nls+jk)-hew/hw(nls+jk)*wumc(0)&
&             -(1.-beta2)*zdtime*fr_corc*wumc(nls+jk)*0.75&
&             +(1.-beta)*(X(4+jk)*(wumc(0)-wumc(nls+jk))&
&             -Y(4+jk)*(wumc(nls+jk)-wumc(nls+jk+1)))&
&             +zdtime*pawuflc(nls+jk)
            AAC(4+jk,1)=(-beta*X(4+jk)-hew/hw(nls+jk))*(1.,0.)
            AAC(4+jk,3)=(-beta*Y(4+jk))*(1.,0.)
            AAC(4+jk,2)= (1.,0.) + (beta*X(4+jk)+beta*Y(4+jk))*(1.,0.)&
&             +beta2*zdtime*fr_corc*0.75_dp
          ELSE
            RHSC(4+jk)= wumc(nls+jk)-hew/hw(nls+jk)*wumc(0)&
&             -(1.-beta2)*zdtime*fr_corc*wumc(nls+jk) &
&             +(1.-beta)*(X(4+jk)*(wumc(0)-wumc(nls+jk))&
&             -Y(4+jk)*(wumc(nls+jk)-wumc(nls+jk+1)))&
&             +zdtime*pawuflc(nls+jk)
            AAC(4+jk,1)=(-beta*X(4+jk)-hew/hw(nls+jk))*(1.,0.)
            AAC(4+jk,3)=(-beta*Y(4+jk))*(1.,0.)
            AAC(4+jk,2)= (1.,0.) + (beta*X(4+jk)+beta*Y(4+jk))*(1.,0.)&
&             +beta2*zdtime*fr_corc
          ENDIF
        ELSE
          IF(lv81) THEN
            RHSC(4+jk)= wumc(nls+jk)-(1.-beta2)*zdtime*fr_corc*wumc(nls+jk)&
&             -(1.-beta)*Y(4+jk)*(wumc(0)-wumc(nls+jk))&
&             +zdtime*pawuflc(nls+jk)
            AAC(4+jk,1)=(0.,0.)
            AAC(4+jk,3)=(-beta*Y(4+jk))*(1.,0.)
            AAC(4+jk,2)= (1.,0.) - AAC(4+jk,1) - AAC(4+jk,3)&
&             +beta2*zdtime*fr_corc
          ELSE
            RHSC(4+jk)= wumc(nls+jk)-(1.-beta2)*zdtime*fr_corc*wumc(nls+jk)&
&             +zdtime*tauc/rhoh2o/hw(nls+jk)&
&             -(1.-beta)*Y(4+jk)*(wumc(0)-wumc(nls+jk))&
&             +zdtime*pawuflc(nls+jk)
            AAC(4+jk,1)=(0.,0.)
            AAC(4+jk,3)=(-beta*Y(4+jk))*(1.,0.)
            AAC(4+jk,2)= (1.,0.) - AAC(4+jk,1) - AAC(4+jk,3)&
&             +beta2*zdtime*fr_corc
          ENDIF
        ENDIF
!
!       or
! ----------------------------------------------------------------------
!         RHSC(5)= wumc(1)
!     1    -(1.-beta2)*zdtime*fr_corc*wumc(1)
!     2    +zdtime*tauc/rhoh2o/hw(1)
!     3    -(1.-beta)*Y(5)*(wumc(1)-wumc(2))
!
!        AAC(5,1)=(0.,0.)
!        AAC(5,3)=-beta*Y(5)*(1.,0.)
!        AAC(5,2)= (1.,0.) - AAC(5,1) - AAC(5,3)
!     1    +beta2*zdtime*fr_corc
! ----------------------------------------------------------------------
!
!       Note both of the above two equations are identical. The one we
!       choose is still a tridigonal matrix while coupled with ATM PBL
!       directly. i.e. tauc can be implicitly determined by a coupled ATM
!       and ocean momentum matrix by solving a tri-diagonal matrix only. A
!       value of 0.75 to adjust coriolis force due to the physical thickness
!       of skin layer is 0.25 hw(1). Therefore only 75% remained to be
!       counted here.
!
!*   12.6 MIDDLE LAYERS (jk=2,nle)
!
!      PRINT *, "istep=",istep,"pe=",mpp_pe(),", I am in UPDATE U,V., 12.5."
        DO jk=2,nle-nls
          RHSC(4+jk)= wumc(nls+jk)&
&           -(1.-beta2)*zdtime*fr_corc*wumc(nls+jk)&
&           +(1.-beta)*(X(4+jk)*(wumc(nls+jk-1)-wumc(nls+jk))&
&           -Y(4+jk)*(wumc(nls+jk)-wumc(nls+jk+1)))&
&           +zdtime*pawuflc(nls+jk)
          AAC(4+jk,1)=-beta*X(4+jk)*(1.,0.)
          AAC(4+jk,3)=-beta*Y(4+jk)*(1.,0.)
          AAC(4+jk,2)= (1.,0.) -AAC(4+jk,1) - AAC(4+jk,3)&
&           +beta2*zdtime*fr_corc
!         X,Y is the same as temperature's
        END DO
!
!*   12.7 SOIL LAYER (jk=nle+1=G)
!     
!     zero velocity is assumed.
        jk=nle+1-nls
        RHSC(4+jk)= (0.,0.)
        AAC(4+jk,1)=(0.,0.)
        AAC(4+jk,3)=(0.,0.)
        AAC(4+jk,2)= (1.,0.) - AAC(4+jk,1) - AAC(4+jk,3)
!
!*   12.8 SOLVE THE TRIDIAGONAL MATRIX
!
!      PRINT *, "I am going to LU2."
        CALL LU2(4,5+nle-nls,AAC,RHSC,SOLCMPLX)
!      PRINT *, "I left LU2."
!
!*   12.9 Restore velocity profile
!
        pwu(0) = REAL(SOLCMPLX(4))
        pwv(0) = AIMAG(SOLCMPLX(4))
        DO jk = 1, nle+1-nls
          pwu(nls+jk) = REAL(SOLCMPLX(jk+4))
          pwv(nls+jk) = AIMAG(SOLCMPLX(jk+4))
        END DO
!         Assume the missing layer current to be the first
!         available current, i.e., =pwu(nls+1), pwv(nls+1).
        DO jk = 1, nls, 1
          pwu(jk) = pwu(nls+1)
          pwv(jk) = pwv(nls+1)
        END DO
! ----------------------------------------------------------------------
      ENDIF
! ----------------------------------------------------------------------
!
!*   13. UPDATE TKE DUE TO VERTICAL MIXING
!            BY SOLVING A TRI-DIAGONAL MATRIX:
!
! ----------------------------------------------------------------------
      IF (.NOT.lsoil) THEN
! ----------------------------------------------------------------------
 1300   CONTINUE
!      PRINT *, "istep=",istep,"pe=",mpp_pe(),", I am in UPDATE TKE"
        IF (lsteady_TKE) THEN
          CALL calc_steady_tke
        ELSE
          CALL calc_unsteady_tke
        ENDIF
! ----------------------------------------------------------------------
      ENDIF
! ----------------------------------------------------------------------
!*   14. This is needed unless resolution is better than about 10 km; otherwise
!        rotation does not allow proper and full convective adjustment. Should
!        be applied stochastically, as the strong events leading to
! ----------------------------------------------------------------------
      IF (lpenetrative_convection) THEN !!v9.83 (MINOR=3), bjt, 20130824
        CALL penetrative_convection
      ENDIF
! ----------------------------------------------------------------------
!
!*   15. FINAL: PRINT DIAGNOSTIC VARIABLES
!
! ----------------------------------------------------------------------
!      
!*   15.1 Fluxes
!
      CALL calc_flux
!      
!*   15.2 Water Level
!
!!!      IF(.NOT.lsoil) THEN
!!!        IF (lwaterlevel) THEN      
!!!          pwlvl=pbathy+SUM(hw(nls+1:nle))
!!!        ENDIF
!!!      ENDIF
!!!
!!!  do not change the progonastic variable hw, lsoil during the iteration 
!!!      IF (lwaterlevel) THEN
!!!      ! for both water and soil grid      
!!!          CALL pzcord()                    ! bjt 2010/2/21
!!!      ENDIF
  RETURN
 2100 FORMAT(1X,2(I4,1X),1(I8,1X),(I4,1X),5F8.3,2E9.2)
!
END SUBROUTINE thermocline
! **********************************************************************
! **********************************************************************  
  real FUNCTION calc_tkewave(utauw,depth)
  !  utauw   : friction velocity of the water side (m/s)              I
  !  depth: depth (m), 0 at the surface and plus downward (+, always)
  !  calc_tkewave: tke due to wave (m2/s2)
  !  Mellor and Blumberg (2004)
  !
    IMPLICIT NONE
    real, INTENT(in):: utauw
    real, INTENT(in):: depth
    real,PARAMETER:: ccb=100.      ! constant for wave breaking proposed by Craig and Banner (1994) in Mellor and Blumberg (2004)
                                          ! Mellor and Blumberg (2004), 150 in Stacey (1999)
    real:: lambda
    IF (.NOT.lwave_breaking) THEN
      calc_tkewave=0.
    ELSE
      lambda=1.15925e-5*G/utauw**2.
      calc_tkewave=0.5_dp*(15.8_dp*ccb)**(2./3.)*utauw**2.*EXP(-lambda*depth)
    ENDIF
  END FUNCTION calc_tkewave
! **********************************************************************
  SUBROUTINE calc_steady_tke
  ! ----------------------------------------------------------------------
  !*   calc TKE at steady state
  ! ----------------------------------------------------------------------          
    IMPLICIT NONE
    INTEGER:: jk,levelm,level
    DO jk=1,nle+1-nls
      levelm=nls+jk-1
      IF (levelm.EQ.nls) levelm=0
      level=nls+jk
      pwtke(level) = ck*pwlmx(level)*pwldisp(level)/ce*                            &
     &        (  ( (pwu(levelm)-pwu(level))/(z(levelm)-z(level)) )**2.+               &
     &           ( (pwv(levelm)-pwv(level))/(z(levelm)-z(level)) )**2.+               &
     &           G/rhoh2o/Prw*( pwrhoh(level)-pwrho(level) )/(z(levelm)-z(level))  )
#ifdef DEBUG     
      IF (lprint2) THEN
        print *, "pwtke(",level,")=", pwtke(level)
        print *, "tkewave=",calc_tkewave(utauw,pwlvl-zlk(level))
        print *, "utauw=",utauw
        print *, "depth=",pwlvl-zlk(level)
      ENDIF
#endif
      pwtke(level) = MAX(  pwtke(level), emin )+ calc_tkewave(utauw,pwlvl-zlk(level))
    END DO
    !     Assume the skin layer and the missing layer TKE to be the first
    !     available TKE, i.e., =pwtke(nls+1).
    DO jk = 0, nls, 1
      pwtke(jk) = pwtke(nls+1)
    END DO
  END SUBROUTINE calc_steady_tke
!***********************************************************************
  SUBROUTINE calc_unsteady_tke
  ! ----------------------------------------------------------------------
  !*   calc TKE at unsteady state
  ! ----------------------------------------------------------------------          
    IMPLICIT NONE
    INTEGER:: jk,levelm,level,levelp
    real, DIMENSION(0:lkvl+5):: X,Y,RHS,SOL
    real, DIMENSION(0:lkvl+5,3):: AA
!
!*   13.1 Skin layer (jk = 0)
!
        jk = 0
        IF (lwave_breaking) THEN
        ! set pwtke(0) to be those in Mellor and Blumberg (2004)
          RHS(4)= calc_tkewave(utauw,0.)
          AA(4,1)=0.
          AA(4,3)=0.
          AA(4,2)=1.              
        ELSE
        ! assume to equal to the value of the first layer
          RHS(4)=0.
          AA(4,1)=0.
          AA(4,3)=-1.
          AA(4,2)= 1.
        ENDIF
#ifdef DEBUG
          IF (lprint2) print *, "calc_tkewave=",RHS(4)
#endif        
!
!*   13.2 For first to bottom layers (jk = 1 - nle+1)  
!

        DO jk=1,nle+1-nls
          levelm=nls+jk-1
          IF (levelm.EQ.nls) levelm=0
          level=nls+jk
          levelp=nls+jk+1
!!!          IF (levelm.EQ.nls) THEN
!!!          !! first layer
!!!            X(4+jk)= 0.
!!!            Y(4+jk)= zdtime*pwkm(level)/&
!!!&             ( (z(levelm)-z(level))*(zlk(level)-zlk(levelp)) )
!!!            RHS(4+jk)= wtkem(level)&
!!!&             +(1.-beta)*(                                       &
!!!&                         -Y(4+jk)*(wtkem(level)-wtkem(levelp)) )&
!!!&             +beta2* (&
!!!&             +pwkm(level)*zdtime*((wvm(levelm)-wvm(level))/(z(levelm)-z(level)))**2.   &
!!!&             +pwkh(level)*zdtime*G/rhoh2o*( rhomh(level)-            &
!!!&               rhom(level) )/(z(levelm)-z(level)) )                      &
!!!&             +(1-beta2)*(                                                                    &
!!!&             +pwkm(level)*zdtime*((pwu(levelm)-pwu(level))                          &
!!!&                            /(z(levelm)-z(level)))**2.                                    &
!!!&             +pwkm(level)*zdtime*((pwv(levelm)-pwv(level))                          &
!!!&                            /(z(levelm)-z(level)))**2.                                    &
!!!&             +pwkh(level)*zdtime*G/rhoh2o*( pwrhoh(level)-      &
!!!&               pwrho(level) )/(z(levelm)-z(level))    )             &
!!!&             -0.5_dp*ce*zdtime*wtkem(level)**(1.5_dp)/pwldisp(level)                   &   ! bug, v0.9879
!!!&             +zdtime*pawtkefl(level)
!!!
!!!          ELSE IF (levelp.EQ.(nle+2)) THEN
          IF (levelp.EQ.(nle+2)) THEN
          !! bottom layer
            X(4+jk)= zdtime*pwkm(level)/&
&             ( (z(levelm)-z(level))*(zlk(levelm)-zlk(level)) )
            Y(4+jk)= 0.
            RHS(4+jk)= wtkem(level)&
&             +(1.-beta)*( X(4+jk)*(wtkem(levelm)-wtkem(level))&
&                                                               )&
&             +beta2* (&
&             +pwkm(level)*zdtime*((wvm(levelm)-wvm(level))/(z(levelm)-z(level)))**2.   &
&             +pwkh(level)*zdtime*G/rhoh2o*( rhomh(level)-            &
&               rhom(level) )/(z(levelm)-z(level)) )                      &
&             +(1-beta2)*(                                                                    &
&             +pwkm(level)*zdtime*((pwu(levelm)-pwu(level))                          &
&                            /(z(levelm)-z(level)))**2.                                    &
&             +pwkm(level)*zdtime*((pwv(levelm)-pwv(level))                          &
&                            /(z(levelm)-z(level)))**2.                                    &
&             +pwkh(level)*zdtime*G/rhoh2o*( pwrhoh(level)-      &
&               pwrho(level) )/(z(levelm)-z(level))    )             &
&             -0.5_dp*ce*zdtime*wtkem(level)**(1.5_dp)/pwldisp(level)                   &             ! bug, v0.9879
&             +zdtime*pawtkefl(level)
          ELSE
          !! middle layers
            X(4+jk)= zdtime*pwkm(level)/                                                   &
&             ( (z(levelm)-z(level))*(zlk(levelm)-zlk(level)) )
            Y(4+jk)= zdtime*pwkm(level)/&
&             ( (z(levelm)-z(level))*(zlk(level)-zlk(levelp)) )
            RHS(4+jk)= wtkem(level)&
&             +(1.-beta)*( X(4+jk)*(wtkem(levelm)-wtkem(level))                            &
&                         -Y(4+jk)*(wtkem(level)-wtkem(levelp)) )                             &
&             +beta2* (&
&             +pwkm(level)*zdtime*((wvm(levelm)-wvm(level))/(z(levelm)-z(level)))**2.   &
&             +pwkh(level)*zdtime*G/rhoh2o*( rhomh(level)-            &
&               rhom(level) )/(z(levelm)-z(level)) )                      &
&             +(1-beta2)*(                                                                    &
&             +pwkm(level)*zdtime*((pwu(levelm)-pwu(level))                          &
&                            /(z(levelm)-z(level)))**2.                                    &
&             +pwkm(level)*zdtime*((pwv(levelm)-pwv(level))                          &
&                            /(z(levelm)-z(level)))**2.                                    &
&             +pwkh(level)*zdtime*G/rhoh2o*( pwrhoh(level)-      &
&               pwrho(level) )/(z(levelm)-z(level))    )             &
&             -0.5_dp*ce*zdtime*wtkem(level)**(1.5_dp)/pwldisp(level)                   &               ! bug, v0.9879
&             +zdtime*pawtkefl(level)
          ENDIF
!
!
!     -- DO NOT ALLOW   zdtime*(TOTAL DAMPING) > pwtke   --
!
          RHS(4+jk)=MAX(RHS(4+jk),0.)
!
          AA(4+jk,1)=-beta*X(4+jk)
          AA(4+jk,3)=-beta*Y(4+jk)
          AA(4+jk,2)= 1.+1.5*ce*zdtime*SQRT(wtkem(level))/pwldisp(level)&
&          -AA(4+jk,1)- AA(4+jk,3)
        ENDDO
!
!*   13.4 SOLVE THE TRIDIAGONAL MATRIX
!
        CALL LU(4,5+nle-nls,AA,RHS,SOL)
!
!*   13.5 Restore TKE profile
!     limit tom emin (=1.0E-6) (GASPAR ET AL., 1990)
!
        pwtke(0) = MAX(SOL(4)+emin, emin)
        DO jk = 1, nle+1-nls
          pwtke(nls+jk) = SOL(jk+4)
#ifdef DEBUG
          IF (lprint2) print *, "pwtke(",nls+jk,")=", pwtke(nls+jk)
#endif
          pwtke(nls+jk) = MAX(pwtke(nls+jk),emin)
        END DO
!         Assume the skin layer and the missing layer TKE to be the first
!         available TKE, i.e., =pwtke(nls+1).
        DO jk = 0, nls, 1
          pwtke(jk) = pwtke(nls+1)
        END DO
  END SUBROUTINE calc_unsteady_tke        
!***********************************************************************
  SUBROUTINE calc_flux
  ! ----------------------------------------------------------------------
  !*   calc fluxes
  ! ----------------------------------------------------------------------
!*   14.2 pgrndcapc, pgrndhflx_int, current, tsw
!
   IF (hesn.GT.csncri) THEN
!  snow on top
     pgrndhflx_int=-rhosn*csn*xksn*                               &  
       (  beta*(ptsnic(0)-ptsnic(1))                            &
         +(1.-beta)*(tsim(0)-tsim(1))                              &
         )/(0.5*hsn/pseaice)
   ELSEIF (heice.GT.xicri) THEN
!  ice on top
     pgrndhflx_int=-rhoice*cice*xkice*                            &  
       (  beta*(ptsnic(2)-ptsnic(3))                            &
         +(1.-beta)*(tsim(2)-tsim(3))                              &
         )/(0.5*hice/pseaice)
   ELSEIF(.NOT.lsoil) THEN
!  water on top
     pgrndhflx_int=-rhowcw*pwkh(nls+1)*                        &  
       (  beta*(pwt(0)-pwt(nls+1))                              &
         +(1.-beta)*(wtm(0)-wtm(nls+1))                            &
         )/(z(0)-z(nls+1))-psoflw*FFN(-hw(0))       
   ELSE
!  soil on top
     pgrndhflx_int=0.
     ! the flux below a soil layer of the infinity thickness is zero
   ENDIF
!
!    14.4 Calc cold content ccm (J/m2): the energy to melt snow + ice
!         energy below liquid water at tmelt
!
   pcc=pzsi(0)*rhoh2o*( alf+csn*(tmelt-tsim(1)) )          &
     +pzsi(1)*rhoh2o*( alf+cice*(tmelt-tsim(3)) )
!
!*   14.5 Calc net surface heat/fresh water flux into ocean
!
!!!     pfluxiw_int: net surface heat flux into ocean (W/m2, + downward)
!!!     pfluxiw_int=-pfluxi2-pfluxw2+(cc-ccm)/zdtime
!
!    pfluxiw_int: net surface heat flux from ocean (W/m2, + upward)     
!    ppme2_int: net fresh water into ocean (m/s, + downward)
     pfluxiw_int=pfluxi2+pfluxw2-(pcc-ccm)/zdtime

!!!     IF (pzsi(1) .GT. 0.) THEN
!!!       pfluxiw_int=-pfluxi2-pfluxw2+(cc-ccm)/zdtime
!!!!!!       pfluxiw_int= rhowcw*hw(nls+1)*(pwt(nls+1)-wtm(nls+1))/zdtime           &
!!!!!!         +rhowcw*pwkh(nls+2)*( beta*(pwt(nls+1)-pwt(nls+2))                  &
!!!!!!            +(1.-beta)*(wtm(nls+1)-wtm(nls+2)) )/(z(nls+1)-z(nls+2))
!!!!!!
!!!!!!    Although the below Eq. is correcnt, it is numerically unstable. The above Eq is also correct.
!!!!!!
!!!!!!    pfluxiw_int= rhowcw*hew*(pwt(0)-wtm(0))/zdtime          &
!!!!!!      rhowcw*pwkh(nls+1)*( beta*(pctfreez2-pwt(nls+1))    &
!!!!!!         +(1.-beta)*(wtm(0)-wtm(nls+1)) )/(z(0)-z(nls+1))
!!!!!!
!!!!!!    After second thought, the above Eq. is correct
!!!!!!
!!!!!!    the above Eq. is used for freeze/melt seaice
!!!!!!     Since wt(0) is fixed at pctfreez2, pfluxiw_int=0.
!!!!!!
!!!!!!       pfluxiw_int=0.
!!!!!!
!!!     ELSE
!!!       pfluxiw_int=-pfluxi2-pfluxw2
!!!     ENDIF
!!!     IF (abs(pfluxiw_int).GT.2000.)  CALL output("fluxw > 2000 W/m2. ",hesn,hew,heice,pfluxwm,wtm,wsm,tsim)            
!!!     ppme2_int=-zsf/wsm(nls+1)/zdtime
!      
!*   pwlvl=pbathy+SUM(hw(nls+1:nle))
!*   Total Fresh Water Inflow = (pwlvl-wlvlm)/zdtime
!
!!!     ppme2_int=(pbathy+SUM(hw(nls+1:nle))-wlvlm)/zdtime

!
!*   14.6 Calc the uppermost bulk layer properties for coupling with 3-D ocean
!
!
   IF (locn) THEN
     ! Not valid for lwaterlevel=.TRUE.
     lstfn=MIN(nls+nfnlvl-1,nle)  ! index of last fine level
!
!
!    14.7 Calc net subsurface flux
!
!     pwsubflux_int: subsurface heat flux (W/m2, + upward)
!     pwsubsal_int: subsurface salinity flux (m*PSU/s, + upward)
!
     pwsubflux_int=-rhowcw*pwkh(lstfn+1)*           &  
         (  beta*(pwt(lstfn)-pwt(lstfn+1))      &
           +(1.-beta)*(wtm(lstfn)-wtm(lstfn+1))    &
           )/(z(lstfn)-z(lstfn+1))                    &
         -psoflw*FFN(zlk(nls+lstfn+1)-pwlvl)
     pwsubsal_int=-pwkh(lstfn+1)*                  &    ! unit: 
         ( beta*(pws(lstfn)-pws(lstfn+1))       &
           +(1.-beta)*(wsm(lstfn)-wsm(lstfn+1))    &
           )/(z(lstfn)-z(lstfn+1)) 
   ENDIF
  END SUBROUTINE calc_flux
  !----------------------------------------------------------  
  SUBROUTINE eddy(hcoolskin,utauw,wtkem,wtm,wsm)
  ! ----------------------------------------------------------------------
   real, DIMENSION(0:lkvl+1), INTENT(in):: wtm,wsm
   real, DIMENSION(0:lkvl+1), INTENT(in):: wtkem
   real, INTENT(out):: utauw,hcoolskin
   real:: XLD    ! downward mixing length   (m)
   real:: XLU    ! upward mixing length (m)
   real:: TKESTR ! =TKE*rho/G in (rho*z) (m2/s2*kg/m3/(m/s2))=(kg/m3*m)
   real:: POT    ! rho*z (potential energy difference) (kg/m3 *m)
   real:: wse,wte,RHOE   ! salinity, potential temp, density at TKE level (kg/m3)
   real:: dz     ! distance between two density levels/thickness for freeze (m)
   INTEGER  :: kk, jk  

   IF (.NOT.lsoil) THEN
   !
   !   1.0 calc friction velocity of water side and thickness of cool skin (hcoolskin)
   !           = lamda * nu / uf
     utauw=frictionvelocity(tauwx,tauwy)
     IF (lcook_skin) THEN
       hcoolskin=cool_skin(pwind10w,utauw)
     ELSE
       hcoolskin=0. 
     ENDIF
   !
   !   2.0 For the skin layer and the missing layers
   !
     pwldisp(0:nls)=xldispmin
     pwlmx(0:nls)=xlkmin
     pwkm(0:nls)=xkmmin
     pwkh(0:nls)=xkhmin

     DO jk=0,nle+1
!!!      rhom(jk)=rhofn2(wtm(jk),wsm(jk),pwlvl-z(jk))
!!!       rhom(jk)=rho_from_theta(wsm(jk),wtm(jk)-tmelt,0.)     
       rhom(jk)=rho_from_theta(wsm(jk),wtm(jk)-tmelt,pwlvl-z(jk))     
       ! potential water density of one level higher (denoted as "h") at level jk
       IF (jk.GE.1) rhomh(jk)=rho_from_theta(wsm(jk-1),wtm(jk-1)-tmelt,pwlvl-z(jk))     
     ENDDO
     
   !
   !   3.0 For layere nls+1 to nle+1
   !
     DO jk=nls+1,nle+1,1
       TKESTR         = wtkem(jk)*rhoh2o/G
!      density at TKE level
       IF (jk.EQ.(nls+1)) THEN
         wse     = (wsm(0)+wsm(jk))*.5
         wte     = (wtm(0)+wtm(jk))*.5
       ELSE
         wse     = (wsm(jk-1)+wsm(jk))*.5
         wte     = (wtm(jk-1)+wtm(jk))*.5
       ENDIF
!       Calc LD
       XLD        = 0.
       POT        = 0.
       kk         = jk
       DO WHILE ((POT.LT.TKESTR).AND.(kk.LE.nle+1))
        IF ((kk.LE.nls).AND.(kk.NE.0)) THEN
!         Do nothing & skip the layer!
          kk=kk+1
        ELSE
          IF (kk.EQ.nle+1) THEN
            dz=zlk(kk)-z(nle+1)     ! +, always
          ELSE
            dz=zlk(kk)-zlk(kk+1)    ! +, always
          ENDIF
          RHOE=rho_from_theta(wse,wte-tmelt,pwlvl-z(kk))     
          XLD=XLD+dz
          POT=POT+dz*(rhom(kk)-RHOE)  ! increase along the depth
          kk=kk+1
        ENDIF
       ENDDO 
       IF (POT.GT.TKESTR) THEN
       !! restore back slightlty
         IF(kk.EQ.0) THEN
           XLD  = MAX(XLD-(POT-TKESTR)/&
&            (rhom(0)-RHOE),tol)
         ELSE
           XLD  = MAX(XLD-(POT-TKESTR)/&
&            (rhom(kk-1)-RHOE),tol)
         ENDIF
       ELSE
         XLD  = MAX(XLD,tol)
       ENDIF
       ! add zero-dosplacement for reaching the bottom         
       XLD  = MAX(XLD,d0)
!      
!      Calc LU
       XLU        = 0.
       POT        = 0.
       kk         = jk
       DO WHILE ((POT.LT.TKESTR).AND.(kk.GE.nls+1))
         IF ((kk.LE.nls).AND.(kk.NE.0)) THEN
!          Do nothing & skip the layer!
           kk=kk-1
         ELSE
           RHOE=rho_from_theta(wse,wte-tmelt,pwlvl-z(kk))     
           dz=zlk(kk-1)-zlk(kk)
           XLU=XLU+dz
           POT=POT+dz*(RHOE-rhom(kk))
           kk=kk-1
         ENDIF
       ENDDO
       IF (POT.GT.TKESTR) THEN
       !! restore back slightlty
         XLU  = MAX(XLU-(POT-TKESTR)/&
&            (RHOE-rhom(kk+1)),tol)
       ELSE
         XLU  = MAX(XLU,tol)
       ENDIF
       ! add zero-dosplacement for reaching the top         
       XLU  = MAX(XLU,d0)    ! add zero-dosplacement
       pwldisp(jk)  = MAX(SQRT(XLD*XLU),xldispmin)        ! v.76, v76p
       pwlmx(jk)    = MIN(XLD,XLU)
       pwlmx(jk)    = MAX(pwlmx(jk),xlkmin)            !v.76p
!      
!      4.0 Calc Momentum Diffusivity
!
!      Assunimg no skin layer for momentm since wind shear can enforce on the side. Note that the surface heat flux is from the top. (2011/8/29 bjt)
!!!       pwkm(jk) = MIN(ck*pwlmx(jk)*SQRT(wtkem(jk)),0.1_dp)*Prw+xkmmin                 !v9.7 or v.77
!!!       pwkm(jk) = ck*pwlmx(jk)*SQRT(wtkem(jk))*Prw+xkmmin                             !v9.8: bjt, 2013/5/2, crash
       pwkm(jk) = MIN(ck*pwlmx(jk)*SQRT(wtkem(jk)),100.)             !v9.8: Note that Lee JCL, 2001 set maximum pwkh to be 100 m^2/s        
!!!       IF ( (z(0)-zlk(jk)).LE. (2.*hcoolskin) ) THEN                       ! v.91, 2004.10.18, (pwu, 1971)
!!!         ! momentum within viscous layer(1mm)
!!!         pwkm(jk)  = xkmmin
!!!       ELSE
!!!         pwkm(jk) = MIN(ck*pwlmx(jk)*SQRT(wtkem(jk)),0.1_dp)*Prw+xkmmin                 ! v.77
!!!       ENDIF
!!       PRINT *,"pwlmx=",pwlmx(jk),"TKE=",wtkem(jk),"pwkm=",pwkm(jk)
!      A maximum value of 10 m2/s is imposed on KM to prevent
!      numerical error.
!      1.4e-6 is the molecular momentum diffusivity of water
!       (Pond & Pickard, 1983).
!      1.34e-6 is the molecular momentum diffusivity of water (Chia and pwu, 1998; Mellor and Durbin, 1975)
!      1.34e-7 is the molecular heat diffusivity of water (Chia and pwu, 1998; Mellor and Durbin, 1975)
!
!      5.0 Calc Heat Diffusivity
!
       IF ( (z(0)-zlk(jk)).LT.hcoolskin) THEN                              !v.77
         ! heat within conduction sublayer (0.4mm)
         pwkh(jk)   = xkhmin
       ELSE
         IF (.TRUE.) THEN
         !! KE due to molecular diffusion has been considered in TKESTR
           pwkh(jk)   = MIN(pwkm(jk)/Prw,100.)                             !v9.8: Note that Lee JCL, 2001 set maximum pwkh to be 100 m^2/s
         ELSEIF ( (z(0)-zlk(jk)).LE.10.) THEN                              !v.77
         ! heat within conduction sublayer (0.4mm)
           pwkh(jk)   = MIN(pwkm(jk)/Prw,100.)+xkhmin                      ! otherwise it will crash
         ELSEIF (.TRUE.) THEN
         !! KE due to molecular diffusion has been considered in TKESTR
           pwkh(jk)   = MIN(pwkm(jk)/Prw,100.)                             !v9.8: Note that Lee JCL, 2001 set maximum pwkh to be 100 m^2/s
         ELSEIF (.TRUE.) THEN
           IF (jk.GE.nle) THEN
             pwkh(jk)   = pwkm(jk)/Prw                                        !v9.8994: last 2 levels remain the same
           ELSE
             pwkh(jk)   = SQRT(pwkm(jk)*pwkm(jk+1))/Prw                    !v9.8994: Artifically using diffusivity one-level below (usually very small)
                                                                                        !         (or geometric mean) to preventing heat cross thermocline
           ENDIF
         ELSEIF (.TRUE.) THEN                                                           !v9.83 (MINOR=3), bjt, 20130824
           IF (jk.EQ.nle+1) THEN
             pwkh(jk)   = pwkm(jk)/Prw+xkhmin                                 !v9.8994: last level remain the same
           ELSE 
             pwkh(jk)   = SQRT(pwkm(jk)*pwkm(jk+1))/Prw+xkhmin             !v9.8994: Artifically using diffusivity one-level below (usually very small)
                                                                                        !         (or geometric mean) to preventing heat cross thermocline
           ENDIF
         ELSEIF (.TRUE.) THEN                                                               !v9.83, v9.98998 (MINOR=3), bjt, 20130824
           pwkh(jk)   = pwkm(jk)/Prw+xkhmin                                   !v9.8: bjt, 2013/5/2, crash
         ELSEIF (.TRUE.) THEN                                                           !v9.83 (MINOR=3), bjt, 20130824
           pwkh(jk)   = MIN(pwkm(jk)/Prw,100.)+xkhmin                       !v9.8: Note that Lee JCL, 2001 set maximum pwkh to be 100 m^2/s
         ELSEIF (lv81) THEN
           pwkh(jk)   = MIN(pwkm(jk)/Prw,0.1_dp)+xkhmin                       !v9.7 or v.77
         ELSE
           pwkh(jk)   = MIN(pwkm(jk)/Prw,100.)+xkhmin                       !v9.8: Note that Lee JCL, 2001 set maximum pwkh to be 100 m^2/s
         ENDIF
       ENDIF
     END DO
   ENDIF
  END SUBROUTINE eddy
  ! **********************************************************************
  SUBROUTINE REGRID(  pbathy,     pctfreez2,  pwlvl,                      &
                      pzsi,       psilw,      ptsnic,                     &
                      pwt,        pwu,        pwv,                        &
                      pws,     pwtke,      pwlmx,                      & 
                      pwldisp,    pwkm,       pwkh)
  ! ----------------------------------------------------------------------
  !!!  USE mo_sst,            ONLY: csn,rhosn,xksn,cice,rhoice,xkice,xkw,                   &
  !!!                               omegas,wcri,tol,wlvlref,dpthmx
    IMPLICIT NONE
    real, INTENT(in):: pbathy,   pctfreez2
  ! 3-d SIT vars: water column
    real, INTENT(in out) :: pwlvl,                                   &
         pzsi(0:1),   psilw(0:1), ptsnic(0:3),                &
         pwt(0:lkvl+1), pwu(0:lkvl+1), pwv(0:lkvl+1),         &
         pws(0:lkvl+1), pwtke(0:lkvl+1), pwlmx(0:lkvl+1),  & 
         pwldisp(0:lkvl+1), pwkm(0:lkvl+1), pwkh(0:lkvl+1)  
  
  END SUBROUTINE REGRID
  ! ----------------------------------------------------------------------
  real FUNCTION frictionvelocity(taucx,taucy)
    !  taucx    : u-stress (Pa) over water                              I  
    !  taucy    : v-stress (Pa) over water                              I
    !  frictionvelocity: friction velocity of water side (m/s)          O 
    !   Tau/rho=N/m^2/(kg/m3)=(kg*m/s^2/m^2)/(kg/m3)=m^2/s^2
    !   frictionvelocity=sqrt(abs(taucx/rhoh2o)+abs(taucy/rhoh2o))
    !   or, frictionvelocity=sqrt((ustrw**2 + vstrw**2))
    !
    IMPLICIT NONE
    real, INTENT(in):: taucx,taucy
    frictionvelocity=sqrt(abs(taucx/rhoh2o)+abs(taucy/rhoh2o))
  END FUNCTION frictionvelocity
  ! ---------------------------------------------------------------------- 
  real FUNCTION cool_skin(wind10w,utauw)
    ! Description:
    !
    ! Calculates the thickness of cool skin by Saunders (1967) and Artale (2002)
    !
    !  wind10w  : 10m wind speed (m/s) over water                I
    !  utauw    : friction velocity of water side (m/s)          I    
    !  cool_skin: thickness of cool skin (m)                     O
    !
    ! Method:
    !
    !   A. Gamma
    !      = 0.2u + 0.5  ; u <= 7.5m/s
    !      = 1.6u - 10   ; 7.5m/s < u < 10m/s
    !      = 6           ; 10 <= u
    !   Where u is the 10m wind speed
    !
    !   B. lamda
    !       = ( uf * k * C )/(gamma * rhoh2o * cw * h * nu )
    !
    !  Where ( for sea water of 20 degree Celsius at salinity 35 [g/kg] )
    !     uf = frictionl velocity of water (or stress)
    !     k = thermal conductivity = 0.596 [W/m K]
    !     C = 86400 s in a day
    !     rhoh2o = density of water = 1024.75 [kg/m**3]
    !     cw = specific heat of water = 3993  [kj/kg] 
    !     h = reference depth = 10m
    !     nu = kinematic viscosity = 1.05*10**-6 [m**2/s]
    !
    !   C. hcoolskin  (Cool skin thickness)
    !       = lamda * nu / uf
    !
    !   D. Temperature difference across cool skin
    !       = Qn * hcoolskin / k
    !   Where 
    !       Qn = net surface heat flux [W/m**2]
    !
    ! *skinsst* is called from *physc*.
    !
    ! Authors:
    !
    ! N. Keenlyside & Chia-Ying Tu, IFM-GEOMAR, June 2006, original source
  
    IMPLICIT NONE
    real, INTENT(in):: wind10w,utauw
    
    ! Local Physical Parameters
    real, PARAMETER:: k = 0.596      ! thermal conductivity  [W/m K]
    INTEGER, PARAMETER  :: C = 86400      ! s in a day
  !!!  real, PARAMETER:: rhoh2o = 1024.75 ! density of water [kg/m**3]
  !!!  real, PARAMETER:: cw = 3993      ! specific heat of water [kj/kg] 
    real, PARAMETER:: refh = 10       
    ! reference depth [m] in Artale's formula of lamda_d (Artale, 2002)
    real, PARAMETER:: nu = 1.05E-6   ! kinematic viscosity of water [m**2/s]
  !  real:: Qb                       !CYTu, v91
  !  real:: gamma            !CYTu, v91
  !  real:: lamda_d          ! nodimensional constant in determing cool skin depth (Sauners, 1967)
  !                                                       ! lamda_d = 5.8 (Jin sitwu, 1971)
  !  real:: lamda_h          
  !  real:: nuw !/1.14E-6/           ! kinematic viscosity of seawater (m2 s-1) (Pauson and Simpson, 1981)
  !
    INTEGER, PARAMETER::  lsaunders=1     ! Artale et al.,2002 scheme
  
    ! Local variables
    real:: gamma, lamda, frictionvelocity, Qnet, Tdiff
    !
    !   A. Gamma
    !      = 0.2u + 0.5  ; u <= 7.5m/s
    !      = 1.6u - 10   ; 7.5m/s < u < 10m/s
    !      = 6           ; 10 <= u
    !   Where u is the 10m wind speed
    IF (lsaunders .EQ. 1) THEN !   Saunders Constant (Artale et al.,2002)
      IF (.FALSE.) THEN
        IF ( wind10w <= 7.5 ) THEN
          gamma = 0.2 * wind10w  + 0.5
        ELSEIF (( 7.5 < wind10w).and.(wind10w < 10 )) THEN
          gamma = 1.6 * wind10w - 10
        ELSE
          gamma=6
        ENDIF
      ELSE
        gamma=6
      ENDIF
  !   B. lamda
  !           =( uf * k * C )/(gamma * rhoh2o * cw * h * nu )
  !                tauc/rhoh2o
       lamda=(utauw*0.596*86400)/(gamma*rhoh2o*clw*refh*nu)
  !
  !!!!      wsstrw=wsstra*SQRT(1.26/1020.)        ! density ratio of air/seawater (Paulson and Simpson, 1981)
    ELSE IF (lsaunders .EQ. 2)THEN !      Saunders Constant (pwu,1971)
       lamda=5.8
    ELSE IF (lsaunders .EQ. 3)THEN !      Saunders Constant (Paulson and Simpson,1981)
       lamda=6.5
    ELSE IF (lsaunders .EQ. 4)THEN !      Saunders Constant (pwu,1985)
       IF (wind10w.LE.7.)THEN
          lamda=2.+5.*wind10w/7.
       ELSE
          lamda=7.
       ENDIF
  !  ELSE IF (lsaunders .EQ. 5)THEN !     Saunders Constant (Fairall,1996)
  !     Qb=(-Rld+LE+H+Rlu)+((0.026*4.19E-3)/(alv*3.E-4))*H 
  !     lamda=6.*(1.+((Qb*16.*g*3.E-4*rhowcw*(nuw**3.))/((wsstrw**4.)*(0.59**2.)))**(3./4.))**(-1./3.)
    ENDIF
  !
  ! Thickness of Thermal Sublayer (Saunders, 1967)
  !
  !   C. hcoolskin  (Cool skin thickness)
  !           = lamda * nu / uf
    cool_skin=lamda*nu/utauw
  !  !   D. Temperature difference across cool skin
  !  !           = Qn * hcoolskin / k
  !  Tdiff=-pfluxw2*hcoolskin/0.596
  !  !
  !  ptsw=pobswtb+Tdiff
    RETURN
  END FUNCTION cool_skin
  ! **********************************************************************
  SUBROUTINE LKDIFKH(hew,X,Y)
  !
  ! Calculate X, Y coefficient matrix for tmeperature and salinity with kh
  !
  ! Output: X,Y,hew
  !
  !*    0. Locate Space
  !
    IMPLICIT NONE
  !
    real, INTENT(out):: hew    ! effective skin thickness of water (m)
    real, DIMENSION(0:lkvl+5), INTENT(out):: X,Y
    
    INTEGER :: jk,levelm,level,levelp
    
  ! For the skin layer 
  
    xkhskin=SQRT(pwkh(0)*pwkh(nls+1))         ! calc the heat diffusivity for skin layer
    hew=HEFN(hw(0),xkhskin,omegas)
  !
  ! For skin layer, middle layers and bottom soil layers
  !
    DO jk=0,nle+1-nls
      levelm=nls+jk-1
      IF (levelm.EQ.nls) levelm=0
      level=nls+jk
      IF (level.EQ.nls) level=0
      levelp=nls+jk+1
      
      IF (level.EQ.0) THEN
      !! Skin layer
        X(4+jk)=0.
        Y(4) = zdtime/hew*pwkh(levelp)/(z(level)-z(levelp))  
      ELSE IF (level.EQ.nle+1) THEN
      !! Soil layer
        X(4+jk)= zdtime/(rhogcg*SQRT(xkg/omegas))*  &
          rhoh2o*clw*pwkh(level)/(z(levelm)-z(level))
        Y(4+jk)=0.
      ELSE
      !! Middle water layers
        X(4+jk)= zdtime/hw(level)*pwkh(level)/(z(levelm)-z(level))
        Y(4+jk)= zdtime/hw(level)*pwkh(levelp)/(z(level)-z(levelp))
      ENDIF
    ENDDO
  END SUBROUTINE LKDIFKH
  ! ----------------------------------------------------------------------
  SUBROUTINE LKDIFKM(hew,X,Y)
  !
  ! Calculate X, Y coefficient (dimensionless) matrix for TKE with Km
  !
  ! Output: X,Y,hew
  !
  !*    0. Locate Space
  !
    IMPLICIT NONE
    real, INTENT(out):: hew    ! effective skin thickness of water (m)
    real, DIMENSION(0:lkvl+5), INTENT(out):: X,Y
  !
    INTEGER :: jk,levelm,level,levelp
    real:: xkmskin   ! skin layer momentum diffusivity (m2/s)
    
  ! For the skin layer 
  
    xkmskin=SQRT(xkmmin*pwkm(0))         ! calc the momentum diffusivity for skin layer
    hew=HEFN(hw(0),xkmskin,omegas)
  !
  ! For skin layer, middle layers and bottom soil layers
  !
    DO jk=0,nle-nls
      levelm=nls+jk-1
      IF (levelm.EQ.nls) levelm=0
      level=nls+jk
      IF (level.EQ.nls) level=0
      levelp=nls+jk+1
      
      IF (level.EQ.0) THEN
      !! Skin layer
        X(4+jk)=0.
        Y(4) = zdtime/hew*pwkm(levelp)/(z(level)-z(levelp))  
      ELSE
      !! Middle water layers
        X(4+jk)= zdtime/hw(level)*pwkm(level)/(z(levelm)-z(level))
        Y(4+jk)= zdtime/hw(level)*pwkm(levelp)/(z(level)-z(levelp))
      ENDIF
    ENDDO
  END SUBROUTINE LKDIFKM
  ! ----------------------------------------------------------------------
  SUBROUTINE LU(mas,mae,AA,RHS,SOL)
  ! ----------------------------------------------------------------------
  !  AA : MATRIX OF COEFFICIENTS A  (MN,kk,3)           I
  !       1ST COLUMN = ELEMENTS LEFT TO THE DIAGONAL,
  !       2ND COLUMN = DIAGONAL ELEMENTS,
  !       3RD COLUMN = ELEMENTS RIGHT TO THE DIAGONAL,
  !  RHS: RIGHT HAND SIDE OF THE MATRIX (FORCING)
  !  SOL: SOLUTION OF THE MATRIX
  !  WKK : WORKING ARRAY               (MN,kk,4)
  ! ----------------------------------------------------------------------
  !
!!!    USE mo_kind
    IMPLICIT NONE
    real:: RHS(0:lkvl+5),AA(0:lkvl+5,3),SOL(0:lkvl+5)
    real:: WKK(0:lkvl+5,4)
    INTEGER jk,mas,mae
    DO jk=mas,mae
      WKK(jk,1)=DBLE(AA(jk,1))
      WKK(jk,2)=DBLE(AA(jk,2))
      WKK(jk,3)=DBLE(AA(jk,3))
      WKK(jk,4)=DBLE(RHS(jk))
    ENDDO
  !
  ! ----------------------------------------------------------------------
  !
  !*   SOLVE THE TRIDIAGONAL MXTRIX
  !       
  !   [ 1  ]       [DR   ] [23  ]
  !   [ L1 ] * [ DR  ]=[123 ]
  !   [ L1 ]       [      DR ] [ 123]
  !-----------------------------------------------------------------
  !
    DO jk = mas+1, mae
      WKK(jk,1) = WKK(jk,1) / WKK(jk-1,2)
      WKK(jk,2) = WKK(jk,2) - WKK(jk,1)*WKK(jk-1,3)
      WKK(jk,4) = WKK(jk,4) - WKK(jk,1)*WKK(jk-1,4)
    ENDDO
  !
  !     BACK SUBSTITUTION
  !
    WKK(mae,4) = WKK(mae,4) /WKK(mae,2)
  !
    DO jk = mae-1, mas,-1
      WKK(jk,4) = (WKK(jk,4)-WKK(jk,3)*WKK(jk+1,4)) /WKK(jk,2)
    ENDDO
    DO jk=mas,mae
      SOL(jk)=REAL(WKK(jk,4))
    ENDDO
  END     SUBROUTINE LU
  ! ----------------------------------------------------------------------
  ! ----------------------------------------------------------------------
  SUBROUTINE LU2(mas,mae,AA,RHS,SOL)
  ! ----------------------------------------------------------------------
  !     same as LU, except for complex matrix
  ! ----------------------------------------------------------------------
  !
    IMPLICIT NONE
    INTEGER mas,mae
    COMPLEX(dp) :: RHS(0:lkvl+5),AA(0:lkvl+5,3),SOL(0:lkvl+5),WKK(0:lkvl+5,4)
    INTEGER jk
    DO jk=mas,mae
          WKK(jk,1)=AA(jk,1)
      WKK(jk,2)=AA(jk,2)
      WKK(jk,3)=AA(jk,3)
      WKK(jk,4)=RHS(jk)
    ENDDO
  !
  !-----------------------------------------------------------------
  !*   SOLVE THE TRIDIAGONAL MXTRIX
  !       
  !       [ 1      ]       [DR   ] [23  ]
  !    [ L1        ] * [ DR  ]=[123 ]
  !    [  L1 ]     [      DR ] [ 123]
  !-----------------------------------------------------------------
  !
    DO jk = mas+1, mae
      WKK(jk,1) = WKK(jk,1) / WKK(jk-1,2)
      WKK(jk,2) = WKK(jk,2) - WKK(jk,1)*WKK(jk-1,3)
      WKK(jk,4) = WKK(jk,4) - WKK(jk,1)*WKK(jk-1,4)
    ENDDO
  !
  !     BACK SUBSTITUTION
  !
      WKK(mae,4) = WKK(mae,4) /WKK(mae,2)
  !
    DO jk = mae-1, mas,-1
      WKK(jk,4) = (WKK(jk,4)-WKK(jk,3)*WKK(jk+1,4)) /WKK(jk,2)
    ENDDO
    DO jk=mas,mae
      SOL(jk)=WKK(jk,4)
    ENDDO
  END SUBROUTINE LU2
  ! ----------------------------------------------------------------------
  real FUNCTION levitus_t(tb,z)
  !  z=0. m at the surface (+ upward)
  !  tb: bulk sea temperature (K)
  !-----------------------------------------------------------------------
    IMPLICIT NONE
    real, INTENT(in)::  z, tb
    levitus_t   = tmelt+4+(tb-tmelt-4)*EXP(z/100.)  ! set initial profile to be expontential decay to tmelt+4 K
  END FUNCTION levitus_t
  ! ----------------------------------------------------------------------
  real FUNCTION levitus_s(obswsb,z)
  !  z=0. m at the surface (+ upward)
  !  obswsb: bulk sea salinity (PSU)
  !-----------------------------------------------------------------------
    IMPLICIT NONE
    real, INTENT(in)::  z, obswsb
    levitus_s   = obswsb
  END FUNCTION levitus_s
  ! ----------------------------------------------------------------------
  real FUNCTION FFN(z)
  !*** 2 components ***
  !*****************************************************************************
  ! CALC PENETRATION COEFFICIENT OF SOLAR RADIATION PAULSON AND SIMPSON 1977
  ! J. OF PHYSICAL OCEANOGRAPHY, VOL. 7, PP. 952- 956 
  !*****************************************************************************
  ! CONSTANT (JERLOV's (1976) OPTICAL WATER TYPE I)
  !  z=0. M at the surface (+ upward)
  !-----------------------------------------------------------------------
  !  IMPLICIT NONE
  !  real:: z,R,D1,D2
  !  R    = 0.58
  !  D1   = 0.35
  !  D2   = 23.
  !  FFN   = R*EXP(+z/D1)+(1-R)*EXP(+z/D2)
  !END FUNCTION FFN
  !
  !*** 9 components ***
  !*****************************************************************************
  ! THE TEMPERATURE DIFFERENCE ACROSS COOL SKIN : PAULSON AND SIMPSON 1981
  ! J. OF GEOPHYSICAL RESEARCH, VOL. 86, PP. 11,044- 11,054 
  !*****************************************************************************
  ! EVOLUTION OF COOL SKIN AND DIRECT SEA-AIR GAS TRANSFER COEFFICEINT DURING 
  ! DAYTIME : SOLOVIEV AND SCHLUESSEL 1996
  ! BOUNDARY LAYER METEOROLOGY, VOL. 77, PP. 45- 68
  !*****************************************************************************
  ! SOLOVIEV AND SCHLUESSEL (1996) MODIFIED THE PAULSON AND SIMPSON (1981) EQUATION
  ! OF CLEAR WATER FRO VARIOUS TYPES OF WATER DEFINED BY JERLOV (1976)
  !*****************************************************************************
    IMPLICIT NONE
    real:: z ! depth (m), plus upward
    real:: F(9)      ! spectral distribution (ratio)
    real:: D(9)      ! absorption length (m)
    D(1:9)=(/34.795,2.27,3.15E-2,5.48E-3,8.32E-4,1.26E-4,3.13E-4,7.82E-5,1.44E-5/)
    F(1:9)=(/0.237,0.36,0.179,0.087,0.08,0.0246,0.025,0.007,0.0004/)
  
    IF(wtype .EQ. 10)THEN
          D(1)    = 15.152                ! SOLOVIEV AND SCHLUESSEL (1996,type I)
    ELSE IF(wtype .EQ. 11)THEN
          D(1)    = 13.158                ! SOLOVIEV AND SCHLUESSEL (1996,type IA)
    ELSE IF(wtype .EQ. 12)THEN
          D(1)    = 11.364                ! SOLOVIEV AND SCHLUESSEL (1996,type IB)
    ELSE IF(wtype .EQ. 20)THEN
          D(1)    = 7.576                 ! SOLOVIEV AND SCHLUESSEL (1996,type II)
    ELSE IF(wtype .EQ. 30)THEN
          D(1)    = 2.618                 ! SOLOVIEV AND SCHLUESSEL (1996,type III)
    ELSE IF(wtype .EQ. 1)THEN
          D(1)    = 2.041                 ! SOLOVIEV AND SCHLUESSEL (1996,type 1)
    ELSE IF(wtype .EQ. 3)THEN
          D(1)    = 1.429                 ! SOLOVIEV AND SCHLUESSEL (1996,type 3)
    ELSE IF(wtype .EQ. 5)THEN
          D(1)    = 1.                   ! SOLOVIEV AND SCHLUESSEL (1996,type 5)
    ELSE IF(wtype .EQ. 7)THEN
          D(1)    = 0.917                 ! SOLOVIEV AND SCHLUESSEL (1996,type 7)
    ELSE IF(wtype .EQ. 9)THEN
          D(1)    = 0.625                 ! SOLOVIEV AND SCHLUESSEL (1996,type 9)
    ELSE
          D(1)    = 34.795                ! PAULSON AND SIMPSON (1981)
    ENDIF
    FFN   = SUM(F*EXP(z/D)) 
  !  dFFN   = (F1/D1)+(F2/D2)+(F3/D3)+(F4/D4)+(F5/D5)+(F6/D6) &
  !                       +(F7/D7)+(F8/D8)+(F9/D9)
  
  END FUNCTION FFN
  ! ----------------------------------------------------------------------
  ! ----------------------------------------------------------------------
  real FUNCTION DFFN(z)
  !*** 2 components ***
  !*****************************************************************************
  ! CALC Derivative of PENETRATION COEFFICIENT OF SOLAR RADIATION PAULSON AND SIMPSON 1977
  ! J. OF PHYSICAL OCEANOGRAPHY, VOL. 7, PP. 952- 956 
  !*****************************************************************************
  ! CONSTANT (JERLOV's (1976) OPTICAL WATER TYPE I)
  !  z=0. M at the surface (+ upward)
  !-----------------------------------------------------------------------
  !  IMPLICIT NONE
  !  real:: z,R,D1,D2
  !  R    = 0.58
  !  D1   = 0.35
  !  D2   = 23.
  !  FFN   = R*EXP(+z/D1)+(1-R)*EXP(+z/D2)
  !END FUNCTION FFN
  !
  !*** 9 components ***
  !*****************************************************************************
  ! THE TEMPERATURE DIFFERENCE ACROSS COOL SKIN : PAULSON AND SIMPSON 1981
  ! J. OF GEOPHYSICAL RESEARCH, VOL. 86, PP. 11,044- 11,054 
  !*****************************************************************************
  ! EVOLUTION OF COOL SKIN AND DIRECT SEA-AIR GAS TRANSFER COEFFICEINT DURING 
  ! DAYTIME : SOLOVIEV AND SCHLUESSEL 1996
  ! BOUNDARY LAYER METEOROLOGY, VOL. 77, PP. 45- 68
  !*****************************************************************************
  ! SOLOVIEV AND SCHLUESSEL (1996) MODIFIED THE PAULSON AND SIMPSON (1981) EQUATION
  ! OF CLEAR WATER FRO VARIOUS TYPES OF WATER DEFINED BY JERLOV (1976)
  !*****************************************************************************
    IMPLICIT NONE
    real:: z ! depth (m), plus upward
    real:: F(9)      ! spectral distribution (ratio)
    real:: D(9)      ! absorption length (m)
    D(1:9)=(/34.795,2.27,3.15E-2,5.48E-3,8.32E-4,1.26E-4,3.13E-4,7.82E-5,1.44E-5/)
    F(1:9)=(/0.237,0.36,0.179,0.087,0.08,0.0246,0.025,0.007,0.0004/)
  
    IF(wtype .EQ. 10)THEN
          D(1)    = 15.152_dp                ! SOLOVIEV AND SCHLUESSEL (1996,type I)
    ELSE IF(wtype .EQ. 11)THEN
          D(1)    = 13.158_dp                ! SOLOVIEV AND SCHLUESSEL (1996,type IA)
    ELSE IF(wtype .EQ. 12)THEN
          D(1)    = 11.364_dp                ! SOLOVIEV AND SCHLUESSEL (1996,type IB)
    ELSE IF(wtype .EQ. 20)THEN
          D(1)    = 7.576_dp                 ! SOLOVIEV AND SCHLUESSEL (1996,type II)
    ELSE IF(wtype .EQ. 30)THEN
          D(1)    = 2.618_dp                 ! SOLOVIEV AND SCHLUESSEL (1996,type III)
    ELSE IF(wtype .EQ. 1)THEN
          D(1)    = 2.041_dp                 ! SOLOVIEV AND SCHLUESSEL (1996,type 1)
    ELSE IF(wtype .EQ. 3)THEN
          D(1)    = 1.429_dp                 ! SOLOVIEV AND SCHLUESSEL (1996,type 3)
    ELSE IF(wtype .EQ. 5)THEN
          D(1)    = 1.                   ! SOLOVIEV AND SCHLUESSEL (1996,type 5)
    ELSE IF(wtype .EQ. 7)THEN
          D(1)    = 0.917_dp                 ! SOLOVIEV AND SCHLUESSEL (1996,type 7)
    ELSE IF(wtype .EQ. 9)THEN
          D(1)    = 0.625_dp                 ! SOLOVIEV AND SCHLUESSEL (1996,type 9)
    ELSE
          D(1)    = 34.795_dp                ! PAULSON AND SIMPSON (1981)
    ENDIF
    DFFN=SUM(F/D*EXP(z/D))
  !  DFFN   = (F1/D1)+(F2/D2)+(F3/D3)+(F4/D4)+(F5/D5)+(F6/D6) &
  !                       +(F7/D7)+(F8/D8)+(F9/D9)
  
  END FUNCTION DFFN
  !!! bjt >> too cold
  !!!  real, PARAMETER:: csiced=0.8_dp     ! threshold sea ice depth, (= 0.8 m)
  !!!                                           ! seaice[siced]=1-Exp[-siced/csiced]
  !!!                                           ! seaice[2.]=0.91795
  !!! 1)
  !!!    ! Assuming sea ice fraction increases with siced
  !!!    ! sea ice fraction = 100% if siced > csiced (=1 m)
  !!!    ! Otherwise increase linearly with siced/csiced
  !!!    IF (pzsi(1) .GT. csiced) THEN
  !!!      pseaice=1.
  !!!    ELSE
  !!!      pseaice=psiced/csiced
  !!!    ENDIF
  !!! 2)
  !!!    pseaice=1.-EXP(-psiced/csiced)
  !!!    seaice2[siced_, m_, c_] := N[Erf[c*(siced - m)]/2 - Erf[-c*m]/2]
  !!!    c=2
  !!!    m=1
  !!! cold bias is found for the above Eq.
  ! ----------------------------------------------------------------------
  SUBROUTINE lkerr(err_message,hesn,hew,heice,fcew,pfluxwm,wtm,wsm,tsim,ctfreeze,mas,mae,SOL)
  !   USE mo_memory_g3b,    ONLY: obswt,obsws
    IMPLICIT NONE
    real, INTENT(in):: hesn,heice,hew,fcew,pfluxwm,ctfreeze
    real, DIMENSION(0:3), INTENT(in):: tsim
    real, DIMENSION(0:lkvl+1), INTENT(in):: wtm,wsm
    real, DIMENSION(0:lkvl+5), INTENT(in):: SOL
    INTEGER, INTENT(in)  :: mas,mae
    CHARACTER(len = *) :: err_message
  !  CHARACTER(len = 80) :: err_message(12)={"Water freezes but phase change energy > 0.",         &  !  1
  !                                       "Ice melts but phase change energy < 0.",             &  !  2
  !                                       "Snow melts but phase change energy <0.",             &  !  3
  !                                       "Water freezes completely.",                           &  !  4
  !                                       "Recursive more than 4 times.",                       &  !  5
  !                                       "Depth is less than 0.2 M. Soil only is assumed.",    &  !  6
  !                                       "Ice melts completely due to level 4 forcing.",       &  !  7
  !                                       "Ice melts completely due to level 3 forcing.",       &  !  8
  !                                       "Recursive goto 830 skin water layer setup.",         &  !  9
  !                                       "Recursive goto 840 ice layer setup.",                &  ! 10 
  !                                       "Water starts to freeze.",                             &  ! 11
  !                                       "Snow melts completely." }                               ! 12
#ifdef DEBUG
    IF (lprint2) THEN
#else
    IF (lwarning_msg.GE.3) THEN
#endif
       WRITE(nerr,*) "sit_vdiff: ","istep=",istep,                        &
           "sit_step=",i_sit_step,                                                                &
           "lat=",plat,"lon=",plon, err_message,                      &
           "hew=",hew,"hesn=",hesn,                          &
           "rhoh2o=",rhoh2o,"rhosn=",rhosn,"xksn=",xksn,"omegas=",omegas,                         &
           "heice=",heice,"fcew=",fcew,"ctfreeze=",ctfreeze,                     &
           "pfluxw=",pfluxw2,"sn=",pzsi(0),"ice=",pzsi(1),"tsw=",pwt(0),              &
           "pfluxim=",pfluxim,"pdfluxi=",pdfluxi,"sofli=",psofli,                         &
           "pfluxwm=",pfluxwm,"pdfluxw=",pdfluxw,"soflw=",psoflw,                         &
           "evapw=",pevapw,"rsf=",prsf,"ssf=",pssf,             &
           "silw(0)=",psilw(0),"silw(1)=",psilw(1),                       &
           "rainfall heat flux=",( prsf )*clw*(wtm(nls+1)-ptemp2),             &
           "snowfall heat flux=",( pssf )*( alf+csn*(tmelt-ptemp2)+clw*(wtm(nls+1)-tmelt) ), &
           "snow heat flux=",pzsi(0)*rhoh2o/zdtime*( alf+csn*(tmelt-tsim(1))+clw*(wtm(nls+1)-tmelt) ) &
             +psilw(0)*rhoh2o/zdtime*clw*(wtm(nls+1)-tmelt),                                   &
           "ice flux=", pzsi(1)*rhoh2o/zdtime*( alf+cice*(tmelt-tsim(3))+clw*(wtm(nls+1)-tmelt) )     &
             +psilw(1)*rhoh2o/zdtime*clw*(wtm(nls+1)-tmelt),                                   & 
           "temp2=",ptemp2,"nls=",nls,"nle=",nle,                                             &
           "tsim(:)=",tsim(:),"tsi=",ptsi,                                                    &
           "sittsi(:)=",ptsnic(:),                                                             &
           "wtm(:)=",wtm(:),"sitwt(:)=",pwt(:),                                                &
           "mas=",mas,"mae=",mae,"SOL=",SOL
  !!!         "mas=",mas,"mae=",mae,"AA(0)=",AA(0,:),"RHS=",RHS,"SQL=",SOL
  
    ELSE
       RETURN
    ENDIF
        
  !  CALL write_date(current_date,' sit_vdiff Current date: ')
  !  WRITE(nerr,*) "sit_vdiff Pt. (istep)=(",jl, istep,")"
  !  IF (iderr.EQ.1) THEN
  !    WRITE(nerr,*) "Water freezes but phase change energy > 0."
  !  ELSEIF (iderr.EQ.2) THEN
  !    WRITE(nerr,*) "Ice melts but phase change energy < 0."
  !  ELSEIF (iderr.EQ.3) THEN
  !    WRITE(nerr,*) "Snow melts but phase change energy <0."
  !  ELSEIF (iderr.EQ.4) THEN
  !    WRITE(nerr,*) "Water freezes completely."
  !  ELSEIF (iderr.EQ.5) THEN
  !    WRITE(nerr,*) "Recursive more than 4 times."
  !  ELSEIF (iderr.EQ.6) THEN
  !    WRITE(nerr,*) "Depth is less than 0.2 M. Soil only is assumed."
  !  ELSEIF (iderr.EQ.7) THEN
  !    WRITE(nerr,*) "Ice melts completely due to level 4 forcing."
  !  ELSEIF (iderr.EQ.8) THEN
  !    WRITE(nerr,*) "Ice melts completely due to level 3 forcing."
  !  ELSEIF (iderr.EQ.9) THEN
  !    WRITE(nerr,*) "Recursive goto 830 skin water layer setup."
  !  ELSEIF (iderr.EQ.10) THEN
  !    WRITE(nerr,*) "Recursive goto 840 ice layer setup."
  !  ELSEIF (iderr.EQ.11) THEN
  !    WRITE(nerr,*) "Water starts to freeze."
  !  ELSEIF (iderr.EQ.12) THEN
  !    WRITE(nerr,*) "Snow melts completely."
  !  ENDIF
  END SUBROUTINE lkerr
  ! ----------------------------------------------------------------------
  !!!SUBROUTINE OUTPUT(hesn,hew,heice,fcew,pfluxwm,wtm,wum,wvm,wsm,wtkem,tsim,mas,mae)
  SUBROUTINE output(err_message,hesn,hew,heice,pfluxwm,wtm,wsm,tsim)
  !
  !-----------------------------------------------------------------
  !
  !*    OUTPUT: WRITE DIAGNOSTIC VARIABLES
  !
  !-----------------------------------------------------------------
    !!! USE mo_mpi,           ONLY: mpp_pe()   
    IMPLICIT NONE
    real, INTENT(in):: hesn,heice,hew,pfluxwm
    real, DIMENSION(0:3), INTENT(in):: tsim
    real, DIMENSION(0:lkvl+1), INTENT(in):: wtm,wsm
    CHARACTER(len = *) :: err_message  
  !!!  real, INTENT(in):: hesn,heice,hew,fcew,pfluxwm
  !!!  real, DIMENSION(0:3), INTENT(in):: tsim
  !!!  real, DIMENSION(0:lkvl+1), INTENT(in):: wtm,wum,wvm,wsm,wtkem
  !!!  INTEGER, INTENT(in)  :: mas,mae
    INTEGER jk
    real:: tmp
    
  !!!  WRITE(nerr,*) "sit_vdiff: ","pe=",mpp_pe(),"istep=",istep,   &
  !!!     "lat=",plat,"lon=",plon, "SST overflow"
!!!    CALL write_date(current_date,' sit_vdiff Current date: ')
  !  WRITE(nerr,*) "sit_vdiff Current date: ",current_date
    WRITE(nerr,*) "sit_vdiff: ","it=",istep,"pe=",mpp_pe(),                  &
        "sit_step=",i_sit_step,                                                                &
        "lat=",plat,"lon=",plon, err_message,                      &
        "fluxiw=",pfluxiw_int,                                                              &
        "pfluxi=",pfluxi2,"pfluxim=",pfluxim,"pdfluxi=",pdfluxi,"sofli=",psofli,       &
        "pfluxw=",pfluxw2,"pfluxwm=",pfluxwm,"pdfluxw=",pdfluxw,"soflw=",psoflw,       &
        "temp2=",ptemp2,"tsw=",pwt(0),"tsi=",ptsi,                                  &
        "sn=",pzsi(0),"ice=",pzsi(1),"hew=",hew,"hesn=",hesn,                            &
        "rhoh2o=",rhoh2o,"rhosn=",rhosn,"xksn=",xksn,"omegas=",omegas,                         &
        "heice=",heice,                                                                        &
        "evapw=",pevapw,"rsf=",prsf,"ssf=",pssf,                                   &
        "silw(0)=",psilw(0),"silw(1)=",psilw(1),                                         &
        "rainfall heat flux=",( prsf )*clw*(wtm(nls+1)-ptemp2),             &
        "snowfall heat flux=",( pssf )*( alf+csn*(tmelt-ptemp2)+clw*(wtm(nls+1)-tmelt) ), &
        "snow heat flux=",pzsi(0)*rhoh2o/zdtime*( alf+csn*(tmelt-tsim(1))+clw*(wtm(nls+1)-tmelt) ) &
          +psilw(0)*rhoh2o/zdtime*clw*(wtm(nls+1)-tmelt),                                   &
        "ice flux=", pzsi(1)*rhoh2o/zdtime*( alf+cice*(tmelt-tsim(3))+clw*(wtm(nls+1)-tmelt) )     &
          +psilw(1)*rhoh2o/zdtime*clw*(wtm(nls+1)-tmelt)
  
    WRITE(nerr,*) "sitmask=",psitmask.EQ.1.
    WRITE(nerr,*) "lclass=",plclass
    WRITE(nerr,*) "slm=",pslm
    WRITE(nerr,*) "seaice=",pseaice
    WRITE(nerr,*) "siced=",psiced
    WRITE(nerr,*) "lsoil=",lsoil
    WRITE(nerr,*) "temp2=",ptemp2
    WRITE(nerr,*) "tsi=",ptsi
    WRITE(nerr,*) "tsw=",ptsw
    WRITE(nerr,*) "obstsw=",pobswtb
    WRITE(nerr,*) "tmelt=",pctfreez2
    WRITE(nerr,*) "soflw=",psoflw
    WRITE(nerr,*) "fluxi=",pfluxi
    WRITE(nerr,*) "dfluxi=",pdfluxi
    WRITE(nerr,*) "sofli=",psofli
    WRITE(nerr,*) "fluxw=",pfluxw
    WRITE(nerr,*) "dfluxw=",pdfluxw
    WRITE(nerr,*) "soflw=",psoflw
    WRITE(nerr,*) "fluxw2=",pfluxw2
    WRITE(nerr,*) "wtfn(10)=",pwtfn(10)
    WRITE(nerr,*) "wsfn(10)=",pwsfn(10)
    WRITE(nerr,*) "runtoc=",pdisch
    WRITE(nerr,*) "wind10w=",pwind10w
    WRITE(nerr,*) "wlvl=",pwlvl
    WRITE(nerr,*) "evapw=",pevapw
    WRITE(nerr,*) "rsf=",prsf
    WRITE(nerr,*) "ssf=",pssf
    WRITE(nerr,*) "nls=",nls,"nle=",nle,                                                       &
        "tsim(:)=",tsim(:),                                                                    &
        "sittsi(:)=",ptsnic(:),                                                             &
        "wtm(:)=",wtm(:),"sitwt(:)=",pwt(:)
  !
    CALL output2
  !    
    WRITE(nerr,*) "Data in previous time step"
    DO jk = 0, nle+1
      WRITE(nerr,2301) mpp_pe(),plat,plon,istep,jk,z(jk),&
  &     wtm(jk),wum(jk),wvm(jk),wsm(jk),&
  &     wtkem(jk)
    END DO
  !
  !     calc. column mean temp 
    IF (nle.GE.1)THEN
  !     water exists
      tmp=DOT_PRODUCT(pwt(nls+1:nle),MAX(hw(nls+1:nle),0.))/SUM(MAX(hw(nls+1:nle),0.))
    ELSE
  !     soil only
      tmp=pwt(0)
    ENDIF
    WRITE(nerr,*) "depth=",(z(0)-z(nle+1))
    WRITE(nerr,*) "mean column temp=",tmp
  !
  !ps 2301 FORMAT(1X,4(I3,","),1(I9,","),1(I3,","),1(F10.4,","),1(F8.3,","),2(F8.4,","),&
   2301 FORMAT(1X,1(I3,","),2(F8.2,","),2(I3,","),1(F10.0,","),1(F8.3,","),2(F8.4,","),&
  &       1(F8.3,","),2(E9.2,","),10(F8.3,","))
  
  !
    RETURN
  !
  !
  END SUBROUTINE output
  !
  ! ----------------------------------------------------------------------
  !
  SUBROUTINE penetrative_convection
  !
  !
  !-----------------------------------------------------------------
  ! Parameterize penetrative convection by mixing unstable surface
  ! layer regions directly with deep water layer where density starts
  ! increasing with depth.
  ! This is needed unless resolution is better than about 10 km; otherwise
  ! rotation does not allow proper and full convective adjustment. Should
  ! be applied stochastically, as the strong events leading to
  ! vigorous convection are short lived and infrequent.
  ! This is especially designed for 18 deg water formation in Sargasso Sea.
  !
  !-----------------------------------------------------------------
  ! Corresponding variables:
  !  TIMCOM: SIT
  !  KB(I,J): nle
  !  K=1: K=nls+1
  ! original code:
  !  u40bjt00@alps6:/work/j07tyh00/PRODUCTION_RUNS/GLOBAL_1DEG_Z61_NOCN_HNUDGa/global.F
  !  line 759-774
  !
    IMPLICIT NONE
  ! 0.0 Calling Variables
  ! 0.1 Local Variables
    real:: eps               ! exchange ratio for each zdtime time_step
    real:: sdif,tdif
    INTEGER  :: jk
  !!!      hw(nls+1)=Z(3)-Z(1)    ! calculated somewhere else
  ! one-day by half exchange time scale may be too fast for NUMERICAL stability
  ! Set one-day to desired vertical exchange time scale (such as six_hour, one_hour, ...)
    eps=(1.-0.5_dp**(zdtime/one_day))
  ! find unstable top layer points
    IF (pwrho(nls+1).GT.pwrho(nls+2)) THEN
      jk=nls+1
      jk=jk+1
  ! find first layer below top layer having density more than top layer
      DO WHILE (pwrho(nls+1).GT.pwrho(jk).AND.jk.LT.nle) 
        jk=jk+1
      END DO
  ! Vertically exchange heat and salt.
  ! This will usually destabilize next layer down, but that is more easily
  ! convectively adjusted by model due to deep layers being thicker than
  ! those near surface.
      sdif=pws(jk)-pws(nls+1)
      pws(nls+1)=pws(nls+1)+eps*sdif
      pws(jk)=pws(jk)-eps*sdif*hw(nls+1)/hw(nls+jk)
      tdif=pwt(jk)-pwt(nls+1)
      pwt(nls+1)=pwt(nls+1)+eps*tdif
      pwt(jk)=pwt(jk)-eps*tdif*hw(nls+1)/hw(nls+jk)
    ENDIF
  END SUBROUTINE penetrative_convection
  ! ----------------------------------------------------------------------
  SUBROUTINE interpolation_ocaf(l_no_expolation_wt,l_no_expolation_ws)
!
!     input
!      real:: wtfn12, wsfn12
!     output
!      real:: awtfl(0:lkvl+1),awsfl(0:lkvl+1)

  !!! USE mo_interpo,       ONLY: nmw1, nmw2, wgt1, wgt2, ndw1, ndw2, wgtd1, wgtd2                           
!!!  USE mo_control,       ONLY: locaf
  !!! USE mo_mpi,           ONLY: p_parallel_io, p_bcast, p_io, mpp_pe() 
  !!! USE mo_sst,           ONLY: nwdepth, wdepths, wtfn12, wsfn12
  
  IMPLICIT NONE
  LOGICAL, INTENT(IN):: l_no_expolation_wt   ! logical for missing data
  LOGICAL, INTENT(IN):: l_no_expolation_ws   ! logical for missing data
    ! .TRUE. no expolation, missing value returned
    ! .FALSE. expolation enforced, non-missing value returned
  INTEGER  :: jk,kkk
  real:: ttt,ttt1,sss,sss1,depth
   
  IF(lwarning_msg.GE.4) then
    WRITE(nerr,*) ", I am in interpolation_ocaf"
  ENDIF
  IF (.NOT.locaf) THEN
    pawtfl=0.
    pawsfl=0.   
    RETURN
  ENDIF
!!!  CALL pzcord()  ! bjt 2010/2/21
!!
!! Note that the vertical index of g3b starts from 1 although the index in the sit_vdiff 
!!   routine starts from 1,
  DO jk=0,nle+1
    depth=(pwlvl-z(jk))
    !! initialized the water profile according to world ocean altas data (woa) data
    kkk=1
    DO WHILE ( (wdepths(kkk).LE.depth) .AND. (kkk.LT.nwdepth) ) 
       kkk=kkk+1
    END DO
    IF (kkk.GT.1) kkk=kkk-1  ! restore back one layer
    IF (  &
         (wtfn12(kkk,nmw1).NE.xmissing) .AND. &
         (wtfn12(kkk,nmw2).NE.xmissing) .AND. &
         (wtfn12(kkk+1,nmw1).NE.xmissing) .AND. &
         (wtfn12(kkk+1,nmw2).NE.xmissing) .AND. &
         (wsfn12(kkk,nmw1).NE.xmissing) .AND. &
         (wsfn12(kkk,nmw2).NE.xmissing) .AND. &
         (wsfn12(kkk+1,nmw1).NE.xmissing) .AND. &
         (wsfn12(kkk+1,nmw2).NE.xmissing) ) THEN
      ttt=wgt1*wtfn12(kkk,nmw1)+wgt2*wtfn12(kkk,nmw2)
      ttt1=wgt1*wtfn12(kkk+1,nmw1)+wgt2*wtfn12(kkk+1,nmw2)
      sss=wgt1*wsfn12(kkk,nmw1)+wgt2*wsfn12(kkk,nmw2)
      sss1=wgt1*wsfn12(kkk+1,nmw1)+wgt2*wsfn12(kkk+1,nmw2)
      IF ( (depth.LE.wdepths(nwdepth)) ) THEN
      ! depth < max. obs. depth
      ! linearly interpolation (no extrapolation)
        pawtfl(jk)=ttt+ &
          (depth-wdepths(kkk))/(wdepths(kkk+1)-wdepths(kkk))* (ttt1-ttt)
        pawsfl(jk)=sss+ &
          (depth-wdepths(kkk))/(wdepths(kkk+1)-wdepths(kkk))* (sss1-sss)
      ELSE
      ! depth > max obs. depth
      ! water temperature flux
        IF (l_no_expolation_wt) THEN
        ! no extrapolation
          pawtfl(jk)=0.
        ELSE 
        ! extrapolation
          pawtfl(jk)=ttt+ &
             (depth-wdepths(kkk))/(wdepths(kkk+1)-wdepths(kkk))* (ttt1-ttt)
        ENDIF
      ! water salinity flux
        IF (l_no_expolation_ws) THEN
        ! no extrapolation
          pawsfl(jk)=0.
        ELSE 
        ! extrapolation
          pawsfl(jk)=sss+ &
             (depth-wdepths(kkk))/(wdepths(kkk+1)-wdepths(kkk))* (sss1-sss)
        ENDIF
      ENDIF
    ELSE
      IF (l_no_expolation_wt) THEN
        ! no extrapolation
        pawtfl(jk)=0.
      ELSE
        pawtfl(jk)=0.
      ENDIF
      IF (l_no_expolation_ws) THEN
        ! no extrapolation
        pawsfl(jk)=0. 
      ELSE
        pawsfl(jk)=0.
      ENDIF
    ENDIF
  ENDDO
  IF (lprint2) then
    DO jk=1,nle
      WRITE(nerr,*) 'pe=',mpp_pe(),'z=',z(jk),pawtfl(jk),pawsfl(jk)
    ENDDO
    WRITE(nerr,*) "I am leaving interpolation_ocaf"
  ENDIF
  END SUBROUTINE interpolation_ocaf  
! **********************************************************************                                                                        
  SUBROUTINE nudging_sit_viff_gd_sfc(jk,st_restore_time,ss_restore_time,restore_temp,restore_salt)
    IMPLICIT NONE
    INTEGER, INTENT(IN):: jk    ! lonitude index, and level
    real, INTENT(IN):: st_restore_time, ss_restore_time          
    real, INTENT(OUT) :: restore_temp 
    real, INTENT(OUT) :: restore_salt    
    IF ((pobswtb.NE.xmissing).AND.(st_restore_time.GT.0.)) THEN
       restore_temp=(pobswtb-pwt(jk))*(1.-0.5_dp**(zdtime/st_restore_time))
    ELSEIF ((pobswtb.NE.xmissing).AND.(st_restore_time.EQ.0.)) THEN
       restore_temp=(pobswtb-pwt(jk))
    ELSEIF ((st_restore_time.LT.0.)) THEN
       restore_temp=0.
    ELSE
    ! no pobswtb data
      IF (.NOT.ldeep_water_nudg) THEN
      ! no nudging
       restore_temp=0.
      ELSE
      ! assuming restore_temp to that of previous level
      ENDIF
    ENDIF
    IF ((pobswsb.NE.xmissing).AND.(ss_restore_time.GT.0.)) THEN
      restore_salt=(pobswsb-pws(jk))*(1.-0.5_dp**(zdtime/ss_restore_time))
    ELSEIF ((pobswsb.NE.xmissing).AND.(ss_restore_time.EQ.0.)) THEN
      restore_salt=(pobswsb-pws(jk))
    ELSEIF ((ss_restore_time.LT.0.)) THEN
      restore_salt=0.
    ELSE
    ! no pobswsb data
      IF (.NOT.ldeep_water_nudg) THEN
      ! no nudging
        restore_salt=0.
      ELSE
      ! assuming restore_salt to that of previous level
      ENDIF
    ENDIF
  END SUBROUTINE nudging_sit_viff_gd_sfc
! **********************************************************************                                                                        
  SUBROUTINE nudging_sit_viff_gd(obox_restore_time, &
    socn_restore_time,uocn_restore_time,docn_restore_time,   &       
    ssit_restore_time,usit_restore_time,dsit_restore_time)
    IMPLICIT NONE
    real, INTENT(IN):: obox_restore_time  ! nudging restore time for obox_mask.GT.0. grids
    real, INTENT(IN):: socn_restore_time ! surface [0m,10m) ocean restore time for pocnmask.GT.0. grids
    real, INTENT(IN):: uocn_restore_time ! upper [10m,100m) ocean restore time for pocnmask.GT.0. grids 
    real, INTENT(IN):: docn_restore_time ! deep (>100 m) ocean restore time for pocnmask.GT.0. grids
    real, INTENT(IN):: ssit_restore_time   ! surface [0m,10m) ocean temp/salt restore time for the rest SIT grids
    real, INTENT(IN):: usit_restore_time   ! upper [10m,100m) ocean temp/salt restore time for the rest SIT grids
    real, INTENT(IN):: dsit_restore_time   ! deep (>100 m) ocean temp/salt restore time for the rest SIT grids
    INTEGER :: jk,jk10,jk100
    real:: restore_temp 
    real:: restore_salt
    real:: st_restore_time, ss_restore_time
!   nudging from surface to nudge_depth=100 m depth (default)
    IF (lprint2) then
       WRITE(nerr,*) ", I am in nudging_sit_viff_gd"

       WRITE(nerr,*) "ssit_restore_time=",ssit_restore_time
       WRITE(nerr,*) "usit_restore_time=",usit_restore_time
       WRITE(nerr,*) "dsit_restore_time=",dsit_restore_time
       
       WRITE(nerr,*) "socn_restore_time=",socn_restore_time
       WRITE(nerr,*) "uocn_restore_time=",uocn_restore_time
       WRITE(nerr,*) "docn_restore_time=",docn_restore_time
       WRITE(nerr,*) "pocnmask=",pocnmask
    ENDIF
!!!  CALL pzcord()  ! bjt 2010/2/21
!!!    IF (pobswtb.NE.xmissing) pobswtb=MAX(pobswtb,pctfreez2)
    IF (lsoil) RETURN
    IF (lwoa_echam) THEN
!!!      CALL interpolation_godas(.TRUE.,.TRUE.,.TRUE.,.TRUE.)
      CALL interpolation_godas(pwlvl, z, nle, pobswtb, pobswsb, mixing_depth, &
                               pobsseaice,                                    &                               
                               .TRUE.,.TRUE.,.TRUE.,.TRUE.,                   &
                               pobswt, pobsws, pobswu, pobswv, pctfreez2)      
      IF (pobsws(1).NE.xmissing) pobswsb=pobsws(1)
      ! extrapolation for missing values
      IF (lprint2) then
        WRITE(nerr,*) ", I am in compose_obs_ocean 2.0: after monthly OCN profile"
        WRITE(nerr,*) "tmaxden=",tmaxden(pobswsb)
        WRITE(nerr,2300) "pe,","lat,","lon,","step,","k,","z,", "obswt,", "obsws", "obswu,", "obswv"
        DO jk = 0, nle+1
          !!!      WRITE(nerr,2301) mpp_pe(),plat,plon,istep,jk,z(jk),bg_wt0(jk),bg_ws0(jk),bg_wu0(jk),bg_wv0(jk)
          WRITE(nerr,2301) mpp_pe(),plat,plon,istep,jk,z(jk),pobswt(jk),pobsws(jk),pobswu(jk),pobswv(jk)
        END DO
      ENDIF      
    ELSEIF (.NOT.lwoa_gfdl) THEN
      CALL extrapolation_sst(pwlvl,z,nle,pobswtb,pobswsb,mixing_depth,pobswt)         
    ENDIF
    IF ((obox_restore_time.LT.0.).AND.(socn_restore_time.LT.0.).AND.    &
        ( uocn_restore_time.LT.0.).AND.( docn_restore_time.LT.0.).AND. &
        ( ssit_restore_time.LT.0.).AND.( usit_restore_time.LT.0.).AND. &
        ( dsit_restore_time.LT.0.) ) RETURN                               ! no nudging    
!
!   1.0 Find jk at 10 m depth and nudge_depth (=100 m)
    jk=nls+1
    DO WHILE ( ((pwlvl-z(jk)).LT.nudge_depth_10).AND.(jk.LT.nle) ) 
!      nudging at depth just >= 10 m or at level nle (last water level)
       jk=jk+1
    END DO
    jk10=jk
    DO WHILE ( ((pwlvl-z(jk)).LT.nudge_depth_100).AND.(jk.LT.nle) ) 
!      nudging at depth >= nudge_depth (100 m)
       jk=jk+1
    END DO
    jk100=jk
        
!   3.0 Start nudging from 10 m (at nle, 10 m and >=100 m) 
    restore_temp=0.
    restore_salt=0.    
    jk=nls+1
    DO WHILE ( jk.LE.nle )
      IF ( jk.LT.jk10) THEN    
      ! [0m, 10m (or waterbed) )
        IF ((obox_mask.GT.0.).AND.(MOD(INT(obox_nudg_flag/100),2).EQ.1)) THEN
        ! obox grids
        ! ie., obox_mask.GT.0. .AND. obox_nudg_flag=1xx
        !        
          st_restore_time=obox_restore_time
          ss_restore_time=obox_restore_time
        ELSEIF (pocnmask.GT.0.) THEN
        ! ocn grids
          st_restore_time=socn_restore_time
          ss_restore_time=socn_restore_time
        ELSE
        ! other SIT grids
          st_restore_time=ssit_restore_time
          ss_restore_time=ssit_restore_time
        ENDIF
      ELSEIF ( jk.LT.jk100) THEN
      ! [10m, 100m(waterbed) )
        IF ((obox_mask.GT.0.).AND.(MOD(INT(INT(obox_nudg_flag/100)/2),2).EQ.1)) THEN
        ! ie., obox_mask.GT.0. .AND. obox_nudg_flag=2xx
        !
          st_restore_time=obox_restore_time
          ss_restore_time=obox_restore_time
        ELSEIF (pocnmask.GT.0.) THEN
        ! no nudging for depth < 100 m (default)
          st_restore_time=uocn_restore_time
          ss_restore_time=uocn_restore_time
        ELSE  
          st_restore_time=usit_restore_time
          ss_restore_time=usit_restore_time
        ENDIF
      ELSE
      ! >= 100 m or at nle
        IF ((obox_mask.GT.0.).AND.(MOD(INT(INT(INT(obox_nudg_flag/100)/2)/2),2).EQ.1)) THEN
        ! ie., obox_mask.GT.0. .AND. obox_nudg_flag=4xx
        !
          st_restore_time=obox_restore_time
          ss_restore_time=obox_restore_time
        ELSEIF (pocnmask.GT.0.) THEN
        ! no nudging
          st_restore_time=docn_restore_time
          ss_restore_time=docn_restore_time
        ELSE
          st_restore_time=dsit_restore_time
          ss_restore_time=dsit_restore_time
        ENDIF
      ENDIF
!      
!     3.1 Determining restore temperature at each level
      IF ( jk.LE.jk10) THEN
      ! nudging to obstsw at [0m, 10m] or at waterbed(=nle)
        CALL nudging_sit_viff_gd_sfc(jk,st_restore_time,ss_restore_time,restore_temp,restore_salt)
      ELSE
      ! for depth > 10 m
        !!! IF (lwoa_echam) THEN
        IF ((pobswt(jk).NE.xmissing).AND.(st_restore_time.GT.0.)) THEN
          restore_temp=(pobswt(jk)-pwt(jk))*(1.-0.5_dp**(zdtime/st_restore_time))
          ! obsws  vertical index start at 1
        ELSEIF ((pobswt(jk).NE.xmissing).AND.(st_restore_time.EQ.0.)) THEN
          restore_temp=(pobswt(jk)-pwt(jk))
        ELSEIF (st_restore_time.LT.0.) THEN
        !! no nudging
          restore_temp=0.
        ELSE
          IF (.NOT.ldeep_water_nudg) THEN
            restore_temp=0.
          ELSE
          !! no pobswt data, assume restore_temp to be that of the previous level
          ENDIF
        ENDIF
        IF ((pobsws(jk).NE.xmissing).AND.(ss_restore_time.GT.0.)) THEN
          restore_salt=(pobsws(jk)-pws(jk))*(1.-0.5_dp**(zdtime/ss_restore_time))
          ! obsws  vertical index start at 1
        ELSEIF ((pobsws(jk).NE.xmissing).AND.(ss_restore_time.EQ.0.)) THEN
          restore_salt=(pobsws(jk)-pws(jk))
        ELSEIF (ss_restore_time.LT.0.) THEN
          restore_salt=0.
        ELSE
          IF (.NOT.ldeep_water_nudg) THEN
            restore_salt=0.
          ELSE
          !! no pobsws data, assume restore_temp to be that of the previous level
          ENDIF
        ENDIF
        !!! ELSE
        !!! ! no woa data
        !!!   IF (st_restore_time.LT.0.) THEN
        !!!   !! no nudging
        !!!     restore_temp=0.
        !!!   ELSE
        !!!     IF (.NOT.ldeep_water_nudg) THEN
        !!!     !! no nudging
        !!!       restore_temp=0.
        !!!     ELSE
        !!!     !! no pobswt data, assume restore_temp to be that of the previous level
        !!!     ENDIF
        !!!   ENDIF
        !!!   IF (ss_restore_time.LT.0.) THEN
        !!!     restore_salt=0.
        !!!   ELSE
        !!!     IF (.NOT.ldeep_water_nudg) THEN
        !!!     !! no nudging
        !!!       restore_salt=0.
        !!!     ELSE
        !!!     !! no pobsws data, assume restore_temp to be that of the previous level
        !!!     ENDIF
        !!!   ENDIF
        !!! ENDIF
      ENDIF
    
!     3.2 Nudging at each level     

      pwt(jk)= pwt(jk)+restore_temp
      pws(jk)= pws(jk)+restore_salt
!     calc acc. nudging flux (positive into the water column)
!     Note that energy in skin layer (jk=0) has been accounted in the first water layer (jk=1)
      IF ((jk.NE.0).AND.(hw(jk).NE.xmissing)) THEN
        pwtfn(jk)=pwtfn(jk)+restore_temp+pawtfl(jk)*zdtime           ! v9.8 including added pawtfl
        pwsfn(jk)=pwsfn(jk)+restore_salt+pawsfl(jk)*zdtime           ! v9.8 including added pawsfl
      ENDIF
      jk=jk+1
    END DO

    IF (lprint2) then    
       WRITE(nerr,*) "I am leaving nudging_sit_viff_gd"
    ENDIF

    2300 FORMAT(1X,1A4,2A9,2A4,1A11,4A9,2A10,10A9)
    2301 FORMAT(1X,1(I3,","),2(F8.2,","),2(I3,","),F10.0,9(F10.1,","),2(E10.2,","),&
           &       1(F8.3,","),2(E9.2,","),10(F8.3,","))
  END SUBROUTINE nudging_sit_viff_gd
  ! ----------------------------------------------------------------------
  SUBROUTINE output2
  !!!! A DUPLICATE VERSION of sit_vdiff_init_1d 
  !
  !-----------------------------------------------------------------
  !
  !*    OUTPUT: WRITE DIAGNOSTIC VARIABLES
  !
  !-----------------------------------------------------------------
    !!! USE mo_mpi,           ONLY: mpp_pe()   
    IMPLICIT NONE
  
    INTEGER jk
    real:: tmp
    
  !
  !!!  CALL pzcord()   ! bjt 2010/02/21
    IF (nle.GE.1) THEN
  !     water exists
      WRITE(nerr,*) "Water exist, nle=", nle
    ELSE
  !     soil only
      WRITE(nerr,*) "Soil only, nle=", nle
    ENDIF
    WRITE(nerr,2300) "pe,","lat,","lon,","step,","","","we,","lw,","tsi0,","tsi1"
    WRITE(nerr,2301) mpp_pe(),plat,plon,istep,0,0.,pzsi(0),psilw(0),&

  &  ptsnic(0),ptsnic(1)
    WRITE(nerr,2301) mpp_pe(),plat,plon,istep,1,1.,pzsi(1),psilw(1),&
  &  ptsnic(2),ptsnic(3)
  !  
    WRITE(nerr,2300) "pe,","lat,","lon,","step,","k,","z,", "wt,", "ws,", "wu,", "wv,",&
  &  "wtke,", "wkh,","wldisp,","wlmx,","awtfl,","awufl,","awvfl,","awsfl,","awtkefl"
    DO jk = 0, nle+1
      WRITE(nerr,2301) mpp_pe(),plat,plon,istep,jk,z(jk),&
  &     pwt(jk),pws(jk),pwu(jk),pwv(jk),&
  &     pwtke(jk),pwkh(jk),pwldisp(jk),pwlmx(jk), &
  &     pawtfl(jk),pawufl(jk),pawvfl(jk),pawsfl(jk),pawtkefl(jk)
    END DO
      
  !
    RETURN
  !
   2200 FORMAT(1X,3(A4),2(A9),1(A4),2(2(A10),2(A9)),1(A9), 1(A10))
  !ps 2201 FORMAT(1X,4(I3,","),1(I9,","),2(2(E9.2,","),2(F8.3,",")),&
   2201 FORMAT(1X,3(I3,","),2(F8.2,","),1(I3,","),4(F10.3,","),1(I9,","),2(2(E9.2,","),2(F8.3,",")),&
  &       1(F8.3,","), 1(E10.3,","))
  !
   2300 FORMAT(1X,4A4,1A10,1A4,1A11,4A9,2A10,10A9)
  !ps 2301 FORMAT(1X,4(I3,","),1(I9,","),1(I3,","),1(F10.4,","),1(F8.3,","),2(F8.4,","),&
  !!! 2301 FORMAT(1X,1(I3,","),2(F8.2,","),2(I3,","),1(F10.0,","),1(F8.3,","),1(F8.4,","),&
  !!!&       2(F8.3,","),2(E9.2,","),10(F8.3,","))
   2301 FORMAT(1X,1(I3,","),2(F8.2,","),2(I3,","),1(F10.0,","),1(F8.3,","),1(F8.3,","),&
  &       2(F8.2,","),2(E9.2,","),10(F8.3,","))  
  !
  END SUBROUTINE output2
END SUBROUTINE sit_vdiff
! **********************************************************************
SUBROUTINE sit_vdiff_end ()
  IMPLICIT NONE  
  !!! type (sit_vdiff_data_type), intent(inout) :: Sit
  integer           :: k
  integer           :: unit
  character(len=22) :: restart='RESTART/sit_vdiff.res'

  !!!if (conservation_check) then
  !!!   do k=1,4
  !!!      CALL mpp_sum(h2o(k))
  !!!      CALL mpp_sum(heat(k))
  !!!   end do
  !!!   if (mpp_pe()==mpp_root_pe()) then
  !!!      print *
  !!!      print '(a10,5a13)',   'ICE MODEL ','   AT START  ', &
  !!!           ' TOP FLUX DN.', &
  !!!           ' BOT FLUX DN.', &
  !!!           '   AT END    ', &
  !!!           '   ERROR     '
  !!!      print '(a10,5es13.5)','WATER     ', h2o , h2o (4)-(h2o (1)+h2o (2)-h2o (3))
  !!!      print '(a10,5es13.5)','HEAT      ', heat, heat(4)-(heat(1)+heat(2)-heat(3))
  !!!      print *
  !!!   end if
  !!!end if

  CALL sit_vdiff_restart()

  !--- release memory ------------------------------------------------  
  !
  ! Deallocate module variables
  !
  DEALLOCATE (ocn_z)
  DEALLOCATE (diecast_zdepth)
  DEALLOCATE (diecast_fluxdepth)    
  DEALLOCATE (sit_zdepth)
  DEALLOCATE (sit_fluxdepth)
  DEALLOCATE (sit_gribzdepth)
  DEALLOCATE (sit_gribfluxdepth)
  
  !!!! Interpolation array     
  !!!!nxj,                    !  nxj      : number of local longitudes (echam, =nproma=nbdim)
  !!!!!!  gauss grid description
  !!!!!! - water body or ml_ocean variables
  !!!!echam, check GFS    DEALLOCATE (Sit%slm)             !  pslm      : sea land fraction (1. = land, 0. = sea/lakes)
  !!!!echam, check GFS    DEALLOCATE (Sit%seaice)          !  pseaice   : ice cover (fraction of 1-SLM) (0-1)                                                I/O
  !!!!echam, check GFS    DEALLOCATE (Sit%alake)           !  palake    : lake fraction of grid box                                                          
  !!!!echam, check GFS    DEALLOCATE (Sit%sni)             !  psni      : snow thickness over ice (m in water equivalent)                                    I/O
  !!!!echam, check GFS    DEALLOCATE (Sit%siced)           !  psiced    : ice thickness (m in water equivalent)                                              I/O
  !!!!echam, check GFS    DEALLOCATE (Sit%tsi)             !  ptsi      : surface temperature of ice (K)                                                     
  !!!!echam, check GFS    DEALLOCATE (Sit%tsw)             !  ptsw      : skin temperatrue (K) over water                                                    O     
  !!!!echam, check GFS    DEALLOCATE (Sit%ssw)             !  pssw      : skin salinity (PSU) over water                                                    O     
  !!!!echam, check GFS    DEALLOCATE (Sit%tsl)             !  ptsl      : calcuated Earth's skin temperature from vdiff/tsurf at t+dt (K)                    I/O
  !!!!echam, check GFS    DEALLOCATE (Sit%tslm)            !  ptslm     : calcuated Earth's skin temperature from vdiff/tsurf at t (K)                       I/O
  !!!!echam, check GFS    DEALLOCATE (Sit%tslm1)           !  ptslm1    : calcuated Earth's skin temperature from vdiff/tsurf at t-dt (K)                    I/O
  !!!!echam, check GFS    DEALLOCATE (Sit%fluxw)           !  pfluxw    : net surface energy flux over open water per water fraction                         I
  !!!!echam, check GFS                                 !              (icesheet+openwater) (w/m2) (positive upward)                                      
  !!!!echam, check GFS                                 !              = -(net solar + net longwave - sensible heat - latent heat)                        
  !!!!echam, check GFS    DEALLOCATE (Sit%dfluxs)          !  pdfluxs   : dG/dT (W/m2/K) (positive upward)                                                   I
  !!!!echam, check GFS    DEALLOCATE (Sit%soflw)           !  psoflw    : net SW flux over open water per water fraction (w/m2) (positive downward)          I         
  !!!!echam, check GFS    DEALLOCATE (Sit%fluxi)           !  pfluxi    : net surface energy flux over icesheet per water fraction                           I
  !!!!echam, check GFS                                 !              (icesheet+openwater) (w/m2) (positive upward)
  !!!!echam, check GFS                                 !              = -(net solar + net longwave - sensible heat - latent heat)
  !!!!echam, check GFS    DEALLOCATE (Sit%sofli)           !  psofli    : net SW flux over over icesheet per water fraction (w/m2) (positive downward)       I
  !!!!echam, check GFS      
  !!!!echam, check GFS!!! 2-d SIT vars 
  !!!!echam, check GFS    DEALLOCATE (Sit%disch)           !  pdisch   : surface runoff into ocean (m/s)                                                     I
  !!!!echam, check GFS    DEALLOCATE (Sit%ustrw)           !  taucx    : u-stress (Pa) over water at current time step                                       I/O
  !!!!echam, check GFS                                 !              (set to zero if snow/ice on top)
  !!!!echam, check GFS    DEALLOCATE (Sit%vstrw)           !  taucy    : v-stress (Pa) over water at current time step                                       I
  !!!!echam, check GFS                                 !              (set to zero if snow/ice on top)
  !!!!echam, check GFS    DEALLOCATE (Sit%temp2)           !  ptemp2    : 2 m temperature (K) at current time step                                           I
  !!!!echam, check GFS    DEALLOCATE (Sit%wind10w)         !  pwind10w     : 10m windspeed over water (m/s)                                                  I
  !!!!echam, check GFS    DEALLOCATE (Sit%ocu)             !  pocu      : ocean eastw. velocity (m/s)                                                        O
  !!!!echam, check GFS    DEALLOCATE (Sit%ocv)             !  pocv      : ocean northw. velocity (m/s)                                                       O
  !!!                         
  !!!!!! 2-d SIT vars (sit variable, allocate as 2D) 
  !!!DEALLOCATE (Sit%obsseaice)       !  pobsseaice  : observed sea ice fraction (fraction)                                             I
  !!!DEALLOCATE (Sit%obstsw)          !  pobswtb     : observed bulk sea surface temperature (K)                                        I
  !!!DEALLOCATE (Sit%obswsb)          !  pobswsb    : observed salinity (PSU, 0/00)                                                     I
  !!!DEALLOCATE (Sit%sitmask)         !  psitmask : mask for sit(1=.TRUE., 0=.FALSE.                                                    I
  !!!DEALLOCATE (Sit%bathy)           !  pbathy  : bathymeter (topography or orography) of ocean (m)                                    I  
  !!!DEALLOCATE (Sit%ctfreez2)        !  pctfreez2 : ref water freezing temperature (K)                                                 I
  !!!DEALLOCATE (Sit%wlvl)         !  pwlvl  : current water level (ice/water interface) a water body grid                           I/O
  !!!DEALLOCATE (Sit%ocnmask)         !  pocnmask : fractional mask for 3-D ocean grid (0-1)                                            I
  !!!DEALLOCATE (Sit%obox_mask)       !  obox_mask : 3-D ocean nudging mask, =0: nudging, = 1 (>0): nudging                             I
  !!!DEALLOCATE (Sit%wtb)          !  pwtb: bulk water temperature (K)                                                               O
  !!!DEALLOCATE (Sit%wub)          !  pwub: bulk water u current (m/s)                                                               O
  !!!DEALLOCATE (Sit%wvb)          !  pwvb: bulk water v current (m/s)                                                               O
  !!!DEALLOCATE (Sit%wsb)          !  pwsb: bulk water salinity (PSU)                                                                O
  !!!DEALLOCATE (Sit%fluxiw)          !  pfluxiw: over-water net surface heat flux (W/m2, + upward, i.e., from ocean))                  O
  !!!DEALLOCATE (Sit%pme2)            !  ppme2: net fresh water into ocean (P-E+ice_corr) (m/s, + downward)                             O
  !!!DEALLOCATE (Sit%subfluxw)        !  psubfluxw: subsurface ocean heat flux (W/m2, + upward)                                         O
  !!!DEALLOCATE (Sit%wsubsal)         !  pwsubsal: subsurface ocean salinity flux (m*PSU/s, + upward)                                   O
  !!!DEALLOCATE (Sit%cc)              !  pcc: cold content per water fraction (ice sheet+openwater) (J/m2)                              
  !!!                                !    (energy need to melt snow and ice, i.e., energy below liquid water at tmelt)                 I/O
  !!!DEALLOCATE (Sit%hc)              !  phc: heat content per water fraction (ice sheet+openwater) (J/m2)                              
  !!!                                !    (energy of a water column above tmelt)                                                       I/O
  !!!DEALLOCATE (Sit%engwac)          !  pengwac: accumulated energy per water fraction (ice sheet+openwater) (+ downward, J/m2)
  !!!                             !           (pfluxw+pfluxi+rain/snow advected energy in respect to liquid water at
  !!!                             !           tmelt)*dt                                                                             I/O
  !!!                             !!!  pengw: mean net surface heat flux per water fraction (ice sheet+openwater)
  !!!                             !!!    (+ downward, W/m2)                                                                         I/O
  !!!                             !!!  pengw2: mean snow corrected net surface heat flux per water fraction
  !!!                             !!!    (ice sheet+openwater) (+ downward, W/m2)
  !!!                             !!!    (pfluxw+pfluxi+rain/snow advected heat flux in respect to liquid water at
  !!!                             !!!     tmelt)                                                                                    I/O
  !!!!!!              engw(:,:)engw2(:,:)                              &
  !!!DEALLOCATE (Sit%sc)              !  psc: salinity content per water fraction (ice sheet+openwater) (PSU*m)                         I/O
  !!!DEALLOCATE (Sit%saltwac)         !  psaltwac: accumulated salt into water fraction (+ downward, PSU*m)                             I/O
  !!!
  !!!! implicit with vdiff
  !!!!echam, check GFS    DEALLOCATE (Sit%slm)             !  pslm: land fraction [0-1]                                                                      I
  !!!!echam, check GFS    DEALLOCATE (Sit%grndcapc)        !  pgrndcapc: areal heat capacity of the uppermost sit layer (snow/ice/water)
  !!!!echam, check GFS                                    !       (J/m**2/K)                                                                                I/O
  !!!!echam, check GFS    DEALLOCATE (Sit%grndhflx)        !  pgrndhflx: ground heat flux below the surface (W/m**2)
  !!!!echam, check GFS                                    !          (+ upward, into the skin layer)                                                        I/O
  !!!!echam, check GFS    DEALLOCATE (Sit%grndflux)        !  pgrndflx:  acc. ground heat flux below the surface (W/m**2*s)                                  
  !!!                                    !          (+ upward, into the skin layer)                                                        I/O
  !!!               
  !!!! - 2D from mo_memory_g3b (sit variables, allocate as 3D)
  !!!DEALLOCATE (Sit%obswt)         !  pobswt: observed potentail water temperature (K)                                          I/O
  !!!DEALLOCATE (Sit%obsws)         !  pobsws: observed salinity (PSU, 0/00)                                                     I/O
  !!!DEALLOCATE (Sit%obswu)         !  pobswu: observed u-current (m/s)                                                          I/O
  !!!DEALLOCATE (Sit%obswv)         !  pobswv: observed v-current (m/s)                                                          I/O           
  !!!DEALLOCATE (Sit%zsi)        !  pzsi  :                                                                                   
  !!!                           !        pzsi(0): dry snow water equivalent (m)                                           I/O
  !!!                           !        pzsi(1): dry ice water equivalent (m)                                            I/O
  !!!DEALLOCATE (Sit%silw)       !  psilw  :                                                                                  
  !!!                           !        psilw(0): snow liquid water storage (m)                                          I/O
  !!!                           !        psilw(1): ice liquid water storage (m)                                           I/O
  !!!DEALLOCATE (Sit%tsi)        !  ptsnic   :                                                                      
  !!!                           !        ptsnic(0): snow skin temperatrue (jk)                                            I/O
  !!!                           !        ptsnic(1): mean snow temperatrue (jk)                                            I/O
  !!!                           !        ptsnic(2): ice skin temperatrue (jk)                                             I/O
  !!!                           !        ptsnic(3): mean ice temperatrue (jk)                                             I/O
  !!!                                                                                                
  !!!DEALLOCATE (Sit%wt)         !  pwt      : potential water temperature (K)                                                I/O   
  !!!DEALLOCATE (Sit%wu)         !  pwu      : u current (m/s)                                                                I/O
  !!!DEALLOCATE (Sit%wv)         !  pwv      : v current (m/s)                                                                I/O
  !!!DEALLOCATE (Sit%ww)         !  pww      : w current (m/s)                                                                I/O
  !!!DEALLOCATE (Sit%ws)         !  pws      : practical salinity (0/00)                                                      I/O
  !!!DEALLOCATE (Sit%wtke)       !  pwtke    : turbulent kinetic energy (M2/S2)                                               I/O
  !!!DEALLOCATE (Sit%wlmx)       !  pwlmx    : mixing length (m)                                                              O
  !!!DEALLOCATE (Sit%wldisp)     !  pwldisp  : dissipation length (m)                                                         O
  !!!DEALLOCATE (Sit%wkm)        !  pwkm     : eddy diffusivity for momentum (m2/s)                                           O
  !!!DEALLOCATE (Sit%wkh)        !  pwkh     : eddy diffusivity for heat (m2/s)                                               O
  !!!DEALLOCATE (Sit%wrho)          !  pwrho1000: potential temperature at 1000 m (PSU)                                          O
  !!!DEALLOCATE (Sit%wtfn)          !  pwtfn  : nudging flux into sit-ocean at each level (W/m**2)                               I/O
  !!!DEALLOCATE (Sit%wsfn)          !  pwsfn  : nudging salinity flux into sit-ocean at each level (PSU*m/s)                     I/O
  !!!DEALLOCATE (Sit%wtfns)         !  pwtfns : nudging flux into sit-ocean for an entire column (W/m**2), i.e.
  !!!                           !        pwtfns=SUM(pwtfn(:))                                                             I/O
  !!!DEALLOCATE (Sit%wsfns)         !  pwsfn  : nudging salinity flux into sit-ocean for an entire column (PSU*m/s),             
  !!!                           !        i.e., pwsfns=SUM(pwsfn(:)                                                        I/O
  !!!DEALLOCATE (Sit%awufl)         !  pawufl : advected u flux at each level (positive into ocean) (m/s*m/s)                    I
  !!!DEALLOCATE (Sit%awvfl)         !  pawvfl : advected v flux at each level (positive into ocean) (m/s*m/s)                    I
  !!!DEALLOCATE (Sit%awtfl)         !  pawtfl : advected temperature flux at each level (positive into ocean) (W/m2)             I
  !!!DEALLOCATE (Sit%awsfl)         !  pawsfl : advected salinity flux at each level (positive into ocean) (PSU*m/s)             I
  !!!DEALLOCATE (Sit%awtkefl)       !  pawtkefl : advected tke at each level (positive into ocean) (m3/s3)                       I
  !!!                                                                                                                                  
  !!!!!! - variables internal to physics                                                                                                     
  !!!!!!echam, check GFS    DEALLOCATE (Sit%zevapw)        !  pevapw   : evaporation from water surface (kg/m2/s) (positive downward)                   I
  !!!!!!echam, check GFS    DEALLOCATE (Sit%rsfl)          !  prsfl    : large scale rain flux at the surface (kg/m2/s) (positive downward)             I
  !!!!!!echam, check GFS    DEALLOCATE (Sit%rsfc)          !  prsfc    : convective rain flux at the surface (kg/m2/s) (positive downward)              I
  !!!!!!echam, check GFS    DEALLOCATE (Sit%ssfl)          !  pssfl    : large scale snow flux at the surface (kg/m2/s) (positive downward)             I
  !!!!!!echam, check GFS    DEALLOCATE (Sit%ssfc)          !  pssfc    : convective snow flux at the surface (kg/m2/s) (positive downward)              I
  !!!!end sit_vdiff variable                                             
  !#######################################################################
  CONTAINS   
  !#######################################################################
  ! <SUBROUTINE NAME="sit_vdiff_restart">
  ! <DESCRIPTION>
  !  Write out restart files registered through register_restart_file
  ! </DESCRIPTION>
  !SUBROUTINE sit_vdiff_restart(Sit, time_stamp)
  SUBROUTINE sit_vdiff_restart( time_stamp)
    IMPLICIT NONE  
    !!! type (sit_vdiff_data_type), intent(inout), optional :: Sit
    character(len=*),         intent(in), optional :: time_stamp
  
    !!! CALL save_restart(Sit_restart, time_stamp)
   !CALL icebergs_save_restart(Ice%icebergs)
   ! This should go here but since "Ice" is not available we have to
   ! rely on the restart written via sit_vdiff_end() -AJA
  
  END SUBROUTINE sit_vdiff_restart
  ! </SUBROUTINE>
  !#######################################################################
END SUBROUTINE sit_vdiff_end
  !#######################################################################

  SUBROUTINE init_convect_tables

    !!! USE sit_constants_mod, ONLY: alv, als, cpd, rd, rv, tmelt, &
    !!!                         c3les, c3ies, c4les, c4ies, c5les, c5ies

    real, PARAMETER :: zavl1 = -6096.9385_dp
    real, PARAMETER :: zavl2 =    21.2409642_dp
    real, PARAMETER :: zavl3 =    -2.711193_dp
    real, PARAMETER :: zavl4 =     1.673952_dp
    real, PARAMETER :: zavl5 =     2.433502_dp 

    real, PARAMETER :: zavi1 = -6024.5282_dp
    real, PARAMETER :: zavi2 =    29.32707_dp
    real, PARAMETER :: zavi3 =     1.0613868_dp
    real, PARAMETER :: zavi4 =    -1.3198825_dp
    real, PARAMETER :: zavi5 =    -0.49382577_dp        

    real :: z5alvcp, z5alscp, zalvdcp, zalsdcp
    real :: ztt, zldcp
    real :: zcvm3, zcvm4, zcvm5
    real :: zavm1, zavm2, zavm3, zavm4, zavm5

    INTEGER :: it

    z5alvcp = c5les*alv/cpd
    z5alscp = c5ies*als/cpd

    zalvdcp = alv/cpd
    zalsdcp = als/cpd

    DO it = jptlucu1, jptlucu2
      ztt = 0.001_dp*it
      IF ((ztt-tmelt) > 0.0_dp) THEN
        zcvm3 = c3les
        zcvm4 = c4les
        zcvm5 = z5alvcp
        zldcp = zalvdcp
        zavm1 = zavl1
        zavm2 = zavl2
        zavm3 = zavl3
        zavm4 = zavl4
        zavm5 = zavl5
      ELSE
        zcvm3 = c3ies
        zcvm4 = c4ies
        zcvm5 = z5alscp
        zldcp = zalsdcp
        zavm1 = zavi1
        zavm2 = zavi2
        zavm3 = zavi3
        zavm4 = zavi4
        zavm5 = zavi5
      END IF
      tlucuc(it)  = zldcp
      tlucua(it)  = EXP((zavm1/ztt+zavm2+zavm3*0.01_dp*ztt+zavm4*ztt*ztt*1.e-5_dp+zavm5*LOG(ztt)))*rd/rv
      tlucub(it)  = zcvm5*(1.0_dp/(ztt-zcvm4))**2
      tlucuaw(it) = EXP((zavl1/ztt+zavl2+zavl3*0.01_dp*ztt+zavl4*ztt*ztt*1.e-5_dp+zavl5*LOG(ztt)))*rd/rv
    END DO
    
  END SUBROUTINE init_convect_tables

  SUBROUTINE lookuperror (name)

    !!! USE mo_exception,  ONLY: message, finish

    CHARACTER(len=*), INTENT(in) :: name

    ! normal run informs only
    ! CALL message (name, ' lookup table overflow')
    ! debug run, so stop at problem
    !!! CALL finish (name, ' lookup table overflow')
    WRITE (nerr,*) name, ' lookup table overflow'
    ! reset lookupoverflow for next test  

    lookupoverflow = .FALSE.

  END SUBROUTINE lookuperror

  !#######################################################################

  
SUBROUTINE update_snow_ice_property(swe,iwe,hsn,hesn,hice,heice,seaice)
  IMPLICIT NONE
  real, INTENT(IN):: swe    ! snow water equivalent of the grid (m)
  real, INTENT(IN):: iwe    ! iwe water equivalent of the grid (m)
  real, INTENT(OUT):: hsn,    & ! physical snow thickness of the grid (m)
                          hesn,   & ! effictive snow thickness of the grid (m)
                          hice,   & ! physical ice thickness of the grid (m)
                          heice,  & ! effective ice thickness of the grid (m)
                          seaice    ! sea ice fraction of the grid [0-1]
  hsn=HSNFN(swe)
  hesn=HEFN(hsn/4.,xksn,omegas)
  hice=HICEFN(iwe)
  heice=HEFN(hice/4.,xkice,omegas)
  seaice=SEAICEFN(iwe)
END SUBROUTINE update_snow_ice_property

! ----------------------------------------------------------------------
real FUNCTION HASTRFN(hstr)
!*********************
! CALCUL Effective thickness
! h0: physical thickness of the skin layer (m)
! kh: heat diffusivity (m2/s)
! omegas: Earth's angular velocity respect to Sun (2*pi/86400 s-1)
! HEFN: effective thickness of the skin layer (m)
!*********************
  IMPLICIT NONE
  real:: hstr
  hastrfn=SQRT( 1.-2.*COS(hstr)*EXP(-hstr)+EXP(-hstr)**2. )/SQRT(2.)
END FUNCTION HASTRFN
! ----------------------------------------------------------------------
real FUNCTION HEFN(h0,kh,omegas)
!*********************
! CALCUL Effective thickness
! h0: physical thickness of the skin layer (m)
! kh: heat diffusivity (m2/s)
! omegas: Earth's angular velocity respect to Sun (2*pi/86400 s-1)
! HEFN: effective thickness of the skin layer (m)
!*********************
  IMPLICIT NONE
  real:: kh,omegas,h0
  real:: h_ref,hstr,hastr
  h_ref=SQRT(2.*kh/omegas)
  hstr=h0/h_ref
  hastr=HASTRFN(hstr)
  HEFN=h_ref*hastr
END FUNCTION HEFN
! ----------------------------------------------------------------------
real FUNCTION TASTRFN(hstr)
!*********************
! CALCUL Effective thickness
! h0: physical thickness of the skin layer (m)
! kh: heat diffusivity (m2/s)
! omegas: Earth's angular velocity respect to Sun (2*pi/86400 s-1)
! HEFN: effective thickness of the skin layer (m)
!*********************
!!  USE sit_constants_mod,    ONLY: api
  IMPLICIT NONE
  real:: hstr
!!!  TASTRFN=api/4.-ARCTAN( SIN(hstr)*EXP(-hstr)/(1.-Cos(hstr)*EXP(-hstr)) )
  TASTRFN=api/4.-ATAN( SIN(hstr)/(EXP(hstr)-Cos(hstr)) )
END FUNCTION TASTRFN
! ----------------------------------------------------------------------
real FUNCTION SSTRFN(zkm1str,zkstr,zkp1str)
!*********************
! CALCUL dimensionless elasticity of numerical layer k
! zkm1,zk,zkp1: vertical coordinates of k-1, k, k+1 (m) (positive upward)
! s: elasticity (dGk/dTk-dGk+1/dTk)
! sstr: dimensionless elasticity
! sstr=s/rhogcg/SQRT(kh*omegas)
! kh: heat diffusivity (m2/s)
! omegas: Earth's angular velocity respect to Sun (2*pi/86400 s-1)
!*********************
  IMPLICIT NONE
  real, INTENT(in):: zkm1str,zkstr,zkp1str
  SSTRFN=(1./(zkm1str-zkstr)+1./(zkstr-zkp1str))/SQRT(2.)
END FUNCTION SSTRFN
! ----------------------------------------------------------------------
real FUNCTION dGdTFN(em,ra,rc,T0,ps,rhoa)
!*********************
! CALCUL dG/dT of land surface (W/m2/K)
! em: emissivity of land surface
! ra: aerodynamic resistsnce (s/m)
! rc: canopy resistance (s/m)
! T0: land skin temperature (K)
! ps: surface pressure (Pa)
! rhoa: air density (kg/m3)
! dGdT: 
!*********************
!!!  USE sit_constants_mod,      ONLY: stbo,cpd,alv
!!!  USE convect_tables_mod,   ONLY: jptlucu1,jptlucu2,tlucub  
!!!  USE convect_tables_mod, ONLY : lookuperror, lookupoverflow, jptlucu1    &
!!!                              , jptlucu2, tlucua, tlucub, tlucuaw
  IMPLICIT NONE
  real, INTENT(in):: em,ra,rc,T0,ps,rhoa
  INTEGER  :: it
  real:: dqsatdT
  it = MAX(MIN(NINT(T0*1000.),jptlucu2),jptlucu1)
  dqsatdT=0.622*tlucub(it)/ps
  dGdTFN=4.*em*stbo*T0**3+rhoa*cpd/ra+rhoa*alv/(ra+rc)*dqsatdT
END FUNCTION dGdTFN
! ----------------------------------------------------------------------
real FUNCTION SSTR0FN(rhogcg,kh,omegas,dGdT,z0str,z1str)
!*********************
! CALCUL dimensionless elasticity of numerical layer k
! rhogcg: volume heat capacity (kg/m3*J/kg/K)=(J/m3/K)
! kh: heat diffusivity (m2/s)
! omegas: Earth's angular velocity respect to Sun (2*pi/86400 s-1)
! dGdT: 
! z0str,z1str: dimensionless vertical coordinates of k=0, k=1 (positive upward)
! s: elasticity (dGk/dTk-dGk+1/dTk)
! sstr: dimensionless elasticity
! sstr=s/rhogcg/SQRT(kh*omegas)
!*********************
  IMPLICIT NONE
  real, INTENT(in):: rhogcg,kh,omegas,dGdT,z0str,z1str
  SSTR0FN=dGdT/rhogcg/SQRT(kh*omegas)+1./(z0str-z1str)/SQRT(2.)
END FUNCTION SSTR0FN
! ----------------------------------------------------------------------
real FUNCTION HEPSTRFN(ha,ht,s,ta)
  IMPLICIT NONE
  real:: ha,ht,s,ta
  HEPSTRFN=( 2.*ha**2-EXP(-ht)**2*s**2                                                  &
      +SQRT(4.*ha**4+4.*Cos(2.*(ta-ht))*EXP(-ht)**2*s**2+EXP(-ht)**4*s**4)        &
    )/(4.*Cos(ta-ht)*EXP(-ht)*ha) 
END FUNCTION HEPSTRFN
! ----------------------------------------------------------------------
real FUNCTION HEPFN(h,ht,s,kh,omegas,rhogcg)
!*********************
! CALCUL Effective thickness
! h: physical thickness of the numerial layer (m)
! ht: center of temperature below upper interface (m)
! s: elasticity (dGk/dTk-dGk+1/dTk)
! kh: heat diffusivity (m2/s)
! omegas: Earth's angular velocity respect to Sun (2*pi/86400 s-1)
! rhogcg: volume heat capacity (kg/m3*J/kg/K)=(J/m3/K)
! HEPFN: effective thickness of the numerical layer (m)
!*********************
  IMPLICIT NONE
  real, INTENT(in):: h,ht,s,kh,omegas,rhogcg
  real:: h_ref,hstr,htstr,hastr,tastr,sstr,hepstr
  h_ref=SQRT(2.*kh/omegas)
  hstr=h/h_ref
  htstr=ht/h_ref
  hastr=HASTRFN(hstr)
  tastr=TASTRFN(hstr)
  sstr=s/rhogcg/SQRT(kh*omegas)
  hepstr=HEPSTRFN(hastr,htstr,sstr,tastr)
  HEPFN=h_ref*hepstr
END FUNCTION HEPFN
! ----------------------------------------------------------------------
real FUNCTION HE0PFN(h,s,kh,omegas,rhogcg)
!*********************
! CALCUL Effective thickness of the surface numerical layer
! h: physical thickness of the numerial layer (m)
! s: elasticity (dGk/dTk-dGk+1/dTk)
! kh: heat diffusivity (m2/s)
! omegas: Earth's angular velocity respect to Sun (2*pi/86400 s-1)
! rhogcg: volume heat capacity (kg/m3*J/kg/K)=(J/m3/K)
! HE0PFN: optimal effective thickness of the skin layer (m)
!*********************
  IMPLICIT NONE
  real, INTENT(in):: h,s,kh,omegas,rhogcg
  HE0PFN=HEPFN(h,0.,s,kh,omegas,rhogcg)
END FUNCTION HE0PFN
! ----------------------------------------------------------------------
real FUNCTION SEAICEFN(siced)
!*********************
! CALCUL sea ice cover fraction of a grid basen on mean sea ice depth
! siced: mean sea ice depth of a grid (m, swe)
! SEAICEFN: sea ice cover fraction [0-1] (dimensionless)
! assume to be lognormal distribution
! seaice[0.]=0.
! seaice[csiced]=0.5
! seaice[2*csiced]=0.84
!*********************
  IMPLICIT NONE
! threshold sea ice depth, (= 2 m)
! this value should be resolution dependent
! 0 to be infinity fine resolution
! seaice[csiced]=50%
!  real:: csiced      ! seaice[csiced]=0.5

  real, INTENT(in):: siced
  IF (csiced.GT.TOL) THEN
#ifdef __ibm__
    SEAICEFN=0.5_dp*(ERF(LOG(siced/csiced))+1.)
#else            
    SEAICEFN=0.5_dp*(ERF(LOG(siced/csiced))+1.)
    !!! SEAICEFN=0.5_dp*(DERF(LOG(siced/csiced))+1.)
#endif
  ELSEIF (.FALSE.) THEN
    ! csiced=3.      !    
    ! csiced=2.      ! seaice fraction is estimated to be 2.8%, while the observation 3.7% (T31)
    ! csiced=0.5_dp     ! seaice fraction is estimated to be 6.8%, while the observation 3.7% m (v9.9003, T31)                      
    ! csiced=1.0_dp     ! seaice fraction is estimated to be still as high as 6.8%, while the observation 3.7% m (v9.9004, T31, T63)
    csiced=2.0_dp
    !!!   IF (lprint2) then
    !!!     WRITE(nerr,*) "csiced: Truncation is not tested in T',nn,' runs."
    !!!   ENDIF
#ifdef __ibm__
    SEAICEFN=0.5_dp*(ERF(LOG(siced/csiced))+1.)
#else            
    !!! SEAICEFN=0.5_dp*(DERF(LOG(siced/csiced))+1.)
    SEAICEFN=0.5_dp*(ERF(LOG(siced/csiced))+1.)
#endif
  ELSE
    IF (siced.GT.0.) THEN  
    ! seaice mask: Winner wins!
    ! seaice fraction is estimated to be 5.6%, while the observation 3.7% (T31)(cob10dnnnn, cob10d10dnn, cob10d10d10d)
    ! there is 660 ice grids, while the observation is 760 ice grid.    
      SEAICEFN=1.
    ELSE
      SEAICEFN=0.
    ENDIF
  ENDIF
END FUNCTION SEAICEFN
! ----------------------------------------------------------------------
real FUNCTION SICEDFN(seaice)
!*********************
! CALCUL sea ice cover fraction of a grid basen on mean sea ice depth
! siced: mean sea ice depth of a grid (m, swe)
! SEAICEFN: sea ice cover fraction [0-1] (dimensionless)
! assume to be lognormal distribution
! seaice[0.]=0.
! seaice[csiced]=0.5
! seaice[2*csiced]=0.84
!*********************
  IMPLICIT NONE
! threshold sea ice depth, (= 2 m)
! this value should be resolution dependent
! 0 to be infinity fine resolution
! seaice[csiced]=50%
!!!  real:: csiced      ! seaice[csiced]=0.5

  real, INTENT(in):: seaice
!!!  IF (csiced.GT.TOL) THEN
!!!#ifdef __ibm__
!!!    !!! SICEDFN=csiced*EXP(ERFINV(2.*seaice-1.))
!!!    SICEDFN=csiced*EXP(ERFINV(2.*seaice-1.))
!!!#else            
!!!    !!! SICEDFN=csiced*EXP(DERFINV(2.*seaice-1.))
!!!    SICEDFN=csiced*EXP(ERFINV(2.*seaice-1.))
!!!#endif
!!!  ELSE
    IF (seaice.GT.0.5_dp) THEN  
    !!! IF (seaice.GT.TOL) THEN  
    ! seaice mask: Winner wins!
    ! seaice fraction is estimated to be 5.6%, while the observation 3.7% (T31)(cob10dnnnn, cob10d10dnn, cob10d10d10d)
    ! there is 660 ice grids, while the observation is 760 ice grid.    
      SICEDFN=1.
    ELSE
      SICEDFN=0.
    ENDIF
!!!  ENDIF
END FUNCTION SICEDFN
! ----------------------------------------------------------------------
  real FUNCTION HSNFN(sni)
  !*********************
  ! CALCU snow thickness of the grid (m), based on grid mean snow depth in swe (sni)
  !*********************
    IMPLICIT NONE
    real, INTENT(in):: sni  
    HSNFN=sni*rhoh2o/rhosn
  END FUNCTION HSNFN
  !---------------------
  real FUNCTION HICEFN(siced)
  !*********************
  ! CALCUL mean ice thickness of the grid (m), based on grid mean sea ice depth in swe (siced)
  !*********************
    IMPLICIT NONE
    real, INTENT(in):: siced  
    HICEFN=siced*rhoh2o/rhoice
  END FUNCTION HICEFN
! ----------------------------------------------------------------------
  real FUNCTION wt_maxden(depth,obswtb,obswsb,mixing_depth)
  !*********************
  ! CALCUL water temperature based on max density water temperature approach
  ! set initial profile to be expontential decay to tmaxden
  !   =  3.73 C for fresh water s=0
  !   = -4.35 C for ocena water s=36.3 0/00.
  !*********************
    IMPLICIT NONE
    real, INTENT(in):: depth           ! depth (m, positive downward)
    real, INTENT(in):: obswtb          ! observed bulk water temperature (K)
    real, INTENT(in):: obswsb          ! observed bulk salinity (PSU)
    real, INTENT(in):: mixing_depth    ! mixing depth
    IF ( depth.LE.mixing_depth) then
      wt_maxden=obswtb
    ELSE
      wt_maxden=tmaxden(obswsb)+(obswtb-tmaxden(obswsb))*EXP(-(depth-mixing_depth)/100.)
    ENDIF  
  END FUNCTION wt_maxden
! ----------------------------------------------------------------------        
  SUBROUTINE pzcord(zwlvl,bathy,nls,nle,z,zlk,hw,lsoil,lshlw)
!
!     WATER BODY POINT VERTICAL LEVELS
!     zlk: coordinates of a water body flux (m ASL).
!     z: z coordinates of the water body temperature (m ASL).
!     nls: starting water level of the water body point.
!       z(k)=zwlvl, IF k is <= nls. 
!     nle: maximum water levels of the water body point.
!        Note nle=-1 IF water level is below point water body bed.
!        Soil layer always is sitwt(nle+1). If there is no water,
!        sitwt(nle+1)=sitwt(0). In addition, z(k)=bathy IF k >= nle.
!     hw: thickness of each layer
!     lshlw = shallow water mode (due to evaportion or freezing)
!       (one layer water body)
!     lsoil = soil grid
!     zwlvl  : current water level (ice/water interface) a water body grid            I
!----------------------------------------------------------
!*    0 Locate Space
      IMPLICIT NONE
!     0.1 Calling Variables
      real, INTENT(IN):: zwlvl,bathy
      
!     input
!     output
      INTEGER, INTENT(OUT):: nls,nle
      real, INTENT(OUT):: z(0:lkvl+1),zlk(0:lkvl+1),hw(0:lkvl)
      LOGICAL, INTENT(OUT):: lsoil, lshlw
!     0.2 local variables
      INTEGER k
      REAL (dp) :: tmp
      real:: zdepth(0:lkvl)
      zdepth(0:lkvl)=sit_zdepth(0:lkvl)
!     0.3 Initial	(default)
      nls=-999
      nle=-999
      z=xmissing
      zlk=xmissing
      hw=xmissing
      lshlw = .FALSE.
      lsoil = .FALSE.
!
!*    1.0 Determine Ocean Coordinate
!
!
      IF ( (zwlvl.GE.bathy+wcri) )  THEN
!
!     2. Water 
!
        nls=0
        nle=lkvl
        z(lkvl+1)=bathy       
        DO k = 0,lkvl
          tmp=zwlvl-zdepth(k)
          IF ( tmp.GT.(bathy+wcri) ) THEN
            z(k)=tmp
            nle=k
          ELSE
            z(k)=bathy       
          ENDIF
        ENDDO
!
!*    3. Determine Thickness (hw) and zlk of Each Layer
!
        zlk(0)=zwlvl
        DO k = 1,nls
          !! vanisih top layers when waterlevel is lower than the reference level
          zlk(k)=zwlvl
        ENDDO
        DO k = nls+1,lkvl+1
!!!          zlk(k)=(z(k-1)+z(k))/2.
          zlk(k)=zwlvl-sit_fluxdepth(k)
        ENDDO
                
        hw(0)=zlk(0)-zlk(nls+1)
        DO k = 1,nls
          hw(k)=0.
        ENDDO
        hw(nls+1)=zlk(0)-zlk(nls+2)
        DO k = nls+2,nle-1
          hw(k)=zlk(k)-zlk(k+1)
        ENDDO
        hw(nle)=zlk(nle)-z(nle+1)

!
!*    4. Determine Current Existence of Water
!
        IF ((nle-nls).EQ.1) THEN
          lshlw = .TRUE.
!         shallow water body
        ENDIF
!
      ELSE
!
!     5.0 Not a Ocean Grid (Soil)
!
        nle=-1
        z(nle+1)=bathy 
        lsoil = .TRUE.
      ENDIF
   IF(lwarning_msg.GE.4) then
      WRITE(nerr,*) "lshlw=",lshlw,"lsoil=",lsoil,"nls=",nls,"nle=",nle
   ENDIF
      
  END SUBROUTINE pzcord
! **********************************************************************
SUBROUTINE extrapolation_sst(pwlvl,z,nle,pobswtb,pobswsb,mixing_depth,pobswt)
  !
  ! Using observed SST and SSS for the first guess of ocean profie
  !
  IMPLICIT NONE
  real, INTENT(in):: pwlvl
  real, INTENT(in):: z(0:lkvl+1)
  INTEGER, INTENT(in):: nle
  real, INTENT(in):: pobswtb
  real, INTENT(in):: pobswsb
  real, INTENT(in):: mixing_depth
  real, INTENT(out):: pobswt(0:lkvl+1)    
  INTEGER:: jk
  real:: depth
  DO jk=0,nle+1
    depth=(pwlvl-z(jk))
    pobswt(jk)=wt_maxden(depth,pobswtb,pobswsb,mixing_depth)
  END DO
END SUBROUTINE extrapolation_sst
!
!***************************************************
!
SUBROUTINE interpolation_godas(pwlvl, z, nle, pobswtb, pobswsb, mixing_depth, &
    pobsseaice,                                                               &                               
    l_no_expolation_wt, l_no_expolation_ws, l_no_expolation_wu, l_no_expolation_wv, &
    pobswt, pobsws, pobswu, pobswv, pctfreez2)
  !!!!!!DUPLICATE in sit_vdiff  !!!!!!!!!!!
  !
  !     input
  !      real:: obstsw, ot12, os12, ou12, ov12
  !     output
  !      real:: obswt(0:lkvl+1),obsws(0:lkvl+1),obswu(0:lkvl+1),obswv(0:lkvl+1)
  ! .TRUE. no expolation, missing value returned
  ! .FALSE. expolation enforced, non-missing value returned

  !!! USE mo_interpo,       ONLY: nmw1, nmw2, wgt1, wgt2, ndw1, ndw2, wgtd1, wgtd2                           
  !!! USE mo_control,       ONLY: lwoa_echam
  !!!  USE sit_constants_mod,     ONLY: tmelt
  !!!  USE mo_physc2,        ONLY: ctfreez
  !!! USE mo_mpi,           ONLY: p_parallel_io, p_bcast, p_io, mpp_pe() 
  !!! USE mo_sst,           ONLY: nodepth, odepths, ot12, os12, ou12, ov12
  USE eos_ocean_mod,     ONLY: tmaxden
  IMPLICIT NONE
  real, INTENT(in):: pwlvl
  real, INTENT(in):: z(0:lkvl+1)
  INTEGER, INTENT(in):: nle
  real, INTENT(in):: pobswtb
  real, INTENT(in out):: pobswsb
  real, INTENT(in):: mixing_depth
  real, INTENT(in):: pobsseaice
  LOGICAL, INTENT(IN):: l_no_expolation_wt   ! logical for missing data
  LOGICAL, INTENT(IN):: l_no_expolation_ws   ! logical for missing data
  LOGICAL, INTENT(IN):: l_no_expolation_wu   ! logical for missing data
  LOGICAL, INTENT(IN):: l_no_expolation_wv   ! logical for missing data
  real, INTENT(out):: pobswt(0:lkvl+1)
  real, INTENT(out):: pobsws(0:lkvl+1)
  real, INTENT(out):: pobswu(0:lkvl+1)
  real, INTENT(out):: pobswv(0:lkvl+1)
  real, INTENT(in out) :: pctfreez2  
  LOGICAL  :: l_upperdata            ! =TRUE, if data of an upper level is available
  INTEGER  :: jk,kkk
  real:: ttt,ttt1,sss,sss1,uuu,uuu1,vvv,vvv1,depth
  IF (lprint2) then
    WRITE(nerr,*) ", I am in interpolation_godas"
    WRITE(nerr,*) "lwarning_msg=",lwarning_msg
  ENDIF
  IF ((.NOT.lwoa_echam).AND.(pobswtb.EQ.xmissing)) RETURN
  !!!  CALL pzcord  ! bjt 2010/2/21
  !!
  !! Note that the vertical index of g3b starts from 1 although the index in the sit_vdiff 
  !!   routine starts from 1,
  IF (lprint2) then
    WRITE(nerr,*) "nmw1=",nmw1,"nmw2=",nmw2,"wgt1=",wgt1,"wgt2=",wgt2
    !!! WRITE(nerr,2300) "pe,","lat,","lon,","step,","k,","z,", "ot1,", "ot2", "os1,", "os2"
  ENDIF
  DO jk=0,nle+1
    depth=(pwlvl-z(jk))
    IF (lwoa_echam) THEN
      !! initialized the water profile according to world ocean altas data (woa) data
      kkk=1
      DO WHILE ( (odepths(kkk).LE.depth) .AND. (kkk.LT.nodepth) ) 
         kkk=kkk+1
      END DO
      IF (kkk.GT.1) kkk=kkk-1              ! restore back one layer
    ENDIF
  !
  ! 1.0 water salinity 
  !
    l_upperdata=.FALSE.
    sss=pobswsb
    sss1=pobswsb
    IF ( lwoa_echam .AND.                                    &
         (os12(kkk,nmw1).NE.xmissing)      .AND. &
         (os12(kkk,nmw2).NE.xmissing)      .AND. &
         (os12(kkk+1,nmw1).NE.xmissing)    .AND. &
         (os12(kkk+1,nmw2).NE.xmissing)    .AND. &
         (depth.LE.odepths(nodepth)) ) THEN
      ! depth < max. obs. depth
      ! linearly interpolation (no extrapolation)
      l_upperdata=.TRUE.
      sss=wgt1*os12(kkk,nmw1)+wgt2*os12(kkk,nmw2)
      sss1=wgt1*os12(kkk+1,nmw1)+wgt2*os12(kkk+1,nmw2)
      pobsws(jk)=sss+ &
        (depth-odepths(kkk))/(odepths(kkk+1)-odepths(kkk))* (sss1-sss)
    ELSE
      ! depth > max obs. depth or missing observed data
      ! water salinity
      IF (l_no_expolation_ws) THEN
      ! no extrapolation
        pobsws(jk)=xmissing
      ELSE 
      ! assume to be sss1
        pobsws(jk)=sss1
   !!!      ! extrapolation
   !!!        pobsws(jk)=sss+ &
   !!!           (depth-odepths(kkk))/(odepths(kkk+1)-odepths(kkk))* (sss1-sss)
      ENDIF
    ENDIF
   !
   ! 1.1 modify pobswsb/freezing temp again according to initial ocean T/S profile.
   !     Note that pobswsb/freezing temp were firstly setup in ioinitial.f90
   !    
   !!!    IF (lstart.AND.jk.EQ.0) THEN
    IF (jk.EQ.0) THEN
      pobswsb=MERGE(pobsws(jk),pobswsb,pobsws(jk).NE.xmissing)
#ifdef CTFREEZE
#if defined (V9897)
      pctfreez2=tmelt+3.
#elif defined (TMELTS0)
      pctfreez2=MERGE(tmelts(pobswsb),tmelt,pobswsb.NE.xmissing)           
#elif defined (TMELTS1)
      pctfreez2=MERGE(tmelts(pobswsb)+1.,tmelt+1.,pobswsb.NE.xmissing)
#elif defined (TMELTS15)
      pctfreez2=MERGE(tmelts(pobswsb)+1.5_dp,tmelt+1.5_dp,pobswsb.NE.xmissing)
#elif defined (TMELTS2)
      pctfreez2=MERGE(tmelts(pobswsb)+2.,tmelt+2.,pobswsb.NE.xmissing)  ! v9.9003, there is 7600 ice grids, while the observation is 760 ice grid.    
#elif defined (TMELTS25)
      pctfreez2=MERGE(tmelts(pobswsb)+2.,tmelt+2.,pobswsb.NE.xmissing)  ! v9.9007.    
#elif defined (TMELTS3)
      pctfreez2=MERGE(tmelts(pobswsb)+3.,tmelt+3.,pobswsb.NE.xmissing)  ! v9.898 (too hot and too salty in S.H.)
#else
      pctfreez2=MERGE(tmelts(pobswsb),tmelt,pobswsb.NE.xmissing)
#endif
#endif
    ENDIF    
!
! 2.0 water temperature
!
    l_upperdata=.FALSE.
    ttt=pobswtb
    ttt1=pobswtb    
    IF ( lwoa_echam .AND.                                    &
         (ot12(kkk,nmw1).NE.xmissing)      .AND. &
         (ot12(kkk,nmw2).NE.xmissing)      .AND. &
         (ot12(kkk+1,nmw1).NE.xmissing)    .AND. &
         (ot12(kkk+1,nmw2).NE.xmissing)    .AND. &
         (depth.LE.odepths(nodepth)) ) THEN
      ! depth < max. obs. depth
      l_upperdata=.TRUE.
      ttt=wgt1*ot12(kkk,nmw1)+wgt2*ot12(kkk,nmw2)
      ttt1=wgt1*ot12(kkk+1,nmw1)+wgt2*ot12(kkk+1,nmw2)
      IF ( depth.LE.10. ) THEN
      ! depth < 10 m, note daily SST is avaiable from satellite
        IF (pobswtb.NE.xmissing) THEN
          pobswt(jk)=MAX(pctfreez2,pobswtb)
        ELSE
          pobswt(jk)=xmissing
        ENDIF
      ELSE
      ! depth < max. obs. depth
      ! linearly interpolation (no extrapolation)
        pobswt(jk)=MAX(pctfreez2, ttt+ &
          (depth-odepths(kkk))/(odepths(kkk+1)-odepths(kkk))* (ttt1-ttt) )
      ENDIF
    ELSE
      ! depth > max obs. depth or on observation
      ! water temperature
      IF (l_no_expolation_wt) THEN
      ! no extrapolation
        pobswt(jk)=xmissing
      ELSE 
      ! extrapolation
        IF (l_upperdata) THEN
          pobswt(jk)=MAX(pctfreez2,ttt+ &
             (depth-odepths(kkk))/(odepths(kkk+1)-odepths(kkk))* (ttt1-ttt) )
          IF ((tmaxden(pobswsb).GT.ttt1).AND.(tmaxden(pobswsb).GT.ttt)) THEN
            pobswt(jk)=MIN(tmaxden(pobsws(jk)),pobswt(jk))
          ELSEIF ((tmaxden(pobswsb).LT.ttt1).AND.(tmaxden(pobswsb).LT.ttt)) THEN
            pobswt(jk)=MAX(tmaxden(pobsws(jk)),pobswt(jk))
          ELSE                                      
            pobswt(jk)=tmaxden(pobsws(jk))
          ENDIF
        !!!ELSE
        !!!  ! set initial profile to be expontential decay to tmaxden
        !!!  pobswt(jk)=wt_maxden(depth,pobswtb,pobswsb,mixing_depth)                  
        ENDIF
        ! range check
        pobswt(jk)=MAX(pctfreez2,pobswt(jk))
      ENDIF
    ENDIF
   !
   ! 3.0 water u current
   !
    l_upperdata=.FALSE.
    uuu=0.
    uuu1=0.    
    IF ( lwoa_echam .AND.                                    &
         (ou12(kkk,nmw1).NE.xmissing)      .AND. &
         (ou12(kkk,nmw2).NE.xmissing)      .AND. &
         (ou12(kkk+1,nmw1).NE.xmissing)    .AND. &
         (ou12(kkk+1,nmw2).NE.xmissing)    .AND. &
         (depth.LE.odepths(nodepth)) ) THEN
      ! depth < max. obs. depth
      l_upperdata=.TRUE.
      uuu=wgt1*ou12(kkk,nmw1)+wgt2*ou12(kkk,nmw2)
      uuu1=wgt1*ou12(kkk+1,nmw1)+wgt2*ou12(kkk+1,nmw2)
      ! linearly interpolation (no extrapolation)
      pobswu(jk)=uuu+ &
        (depth-odepths(kkk))/(odepths(kkk+1)-odepths(kkk))* (uuu1-uuu)
    ELSE
      IF (l_no_expolation_wu) THEN
        ! no extrapolation
        pobswu(jk)=xmissing 
      ELSE
        pobswu(jk)=0.
   !!!     ! extrapolation
   !!!       pobswu(jk)=uuu+ &
   !!!          (depth-odepths(kkk))/(odepths(kkk+1)-odepths(kkk))* (uuu1-uuu)
      ENDIF
    ENDIF
   !
   ! 4.0 water v current
   !
    l_upperdata=.FALSE.
    vvv=0.
    vvv1=0.    
    IF ( lwoa_echam .AND.                                    &
         (ov12(kkk,nmw1).NE.xmissing)      .AND. &
         (ov12(kkk,nmw2).NE.xmissing)      .AND. &
         (ov12(kkk+1,nmw1).NE.xmissing)    .AND. &
         (ov12(kkk+1,nmw2).NE.xmissing)    .AND. &
         (depth.LE.odepths(nodepth)) ) THEN
      ! depth < max. obs. depth
      l_upperdata=.TRUE.
      vvv=wgt1*ov12(kkk,nmw1)+wgt2*ov12(kkk,nmw2)
      vvv1=wgt1*ov12(kkk+1,nmw1)+wgt2*ov12(kkk+1,nmw2)
      ! linearly interpolation (no extrapolation)
      pobswv(jk)=vvv+ &
        (depth-odepths(kkk))/(odepths(kkk+1)-odepths(kkk))* (vvv1-vvv)
    ELSE
      IF (l_no_expolation_wu) THEN
        ! no extrapolation
        pobswv(jk)=xmissing 
      ELSE
        pobswv(jk)=0.
   !!!     ! extrapolation
   !!!       pobswv(jk)=vvv+ &
   !!!          (depth-odepths(kkk))/(odepths(kkk+1)-odepths(kkk))* (vvv1-vvv)
      ENDIF
    ENDIF
  ENDDO
  !
  ! 5.0 Adjust pwt for accounting the ice grid for depth <= 10 m and the limit of pctfreez2
  !
  !  IF (.NOT.lsoil) pobswt(0:nle+1)=MERGE(MAX(pctfreez2,pobswt(0:nle+1)),xmissing,pobswt(0:nle+1).NE.xmissing)
  DO jk=0,nle+1
    IF (pobsseaice.GT.0.) THEN
      depth=(pwlvl-z(jk))
      IF ((depth.LE.10.).AND.(pobswt(jk).NE.xmissing)) pobswt(jk)=pctfreez2
    ENDIF
  END DO
  !!! IF (lprint2) then
  !!!   WRITE(nerr,*) "tmaxden=",tmaxden(pobswsb)
  !!!   WRITE(nerr,2300) "pe,","lat,","lon,","step,","k,","z,", "obswt,", "obsws", "obswu", "obswv"
  !!!   DO jk = 0, nle+1
  !!!     WRITE(nerr,2301) mpp_pe(),plat,plon,istep,jk,z(jk),pobswt(jk),pobsws(jk),pobswu(jk),pobswv(jk)
  !!!   END DO
  !!!   WRITE(nerr,*) "I am leaving interpolation_godas"
  !!! ENDIF
  !!!  2300 FORMAT(1X,1A4,2A9,2A4,1A11,4A9,2A10,10A9)
  !!!  2301 FORMAT(1X,1(I3,","),2(F8.2,","),2(I3,","),F10.0,9(F10.1,","),2(E10.2,","),&
  !!! &       1(F8.3,","),2(E9.2,","),10(F8.3,","))
END SUBROUTINE interpolation_godas
! **********************************************************************
END MODULE sit_vdiff_mod


