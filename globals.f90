module globals
  use typy
  implicit none

  ! Time/progress variables (GLOBAL)
  real(kind=rkind) :: tmp, tmp1, sim_time, start_time, end_time, time
  logical          :: www

  ! =====================================================
  ! === Geometry Structures =============================
  ! =====================================================

  !> Node coordinates and watershed mask
  type :: nodes_str
     real(kind=rkind), allocatable :: data(:,:)          ! (n_nodes, 2)
     logical, allocatable          :: watershed(:)     ! mask: inside watershed?
     integer(kind=ikind)           :: kolik
  end type nodes_str

  !> Hydrological balance per element
  type, public :: hydrobal_str
     real(kind=rkind) :: deltas  = 0.0_rkind   ! water balance residual
     real(kind=rkind) :: inflow  = 0.0_rkind   ! inflow (from upstream)
     real(kind=rkind) :: outflow = 0.0_rkind   ! outflow
     real(kind=rkind) :: Li      = 0.0_rkind   ! leakage
     real(kind=rkind) :: ET      = 0.0_rkind   ! evapotranspiration
     real(kind=rkind) :: Qgw     = 0.0_rkind   ! groundwater flow
     real(kind=rkind) :: Qsurf   = 0.0_rkind   ! surface runoff
  end type hydrobal_str

  !> Element properties and hydrological results
  type :: elements_str
     integer(kind=ikind), allocatable :: data(:,:)     ! element-node connectivity
     real(kind=rkind),    allocatable :: area(:)       ! area of each element
     integer(kind=ikind), allocatable :: material(:)   ! material or soil ID
     type(hydrobal_str),  allocatable :: hydrobal(:)   ! per-element hydro balance
     integer(kind=ikind)              :: kolik = 0     ! number of elements
     integer(kind=ikind), dimension(:,:), allocatable :: neighbours
  end type elements_str

  ! =====================================================
  ! === Global Variables ================================
  ! =====================================================

  type(nodes_str)    :: nodes
  type(elements_str) :: elements

  ! --- Simulation parameters ---
  integer(kind=ikind), parameter :: n_days = 1
  integer :: CN, Julian_day , time_step
  real(kind=rkind) :: phi, as, bs, z, alpha, sigma, gsc, ccrop

  ! =====================================================
  ! === Hydrology input and result arrays (element-based)
  ! =====================================================
  ! Dimensions: (elements%n, n_days)
  real(kind=rkind), allocatable :: precip(:,:), qinter(:,:), qout(:,:)
  real(kind=rkind), allocatable :: conduct(:,:), G(:,:), Tmax(:,:), Tmin(:,:), Tmean(:,:)
  real(kind=rkind), allocatable :: RHmax(:,:), RHmin(:,:), uz(:,:), soilcontent(:,:)

  ! --- Computed results (legacy arrays kept for exporting/visualizing) ---
  real(kind=rkind), allocatable :: Qsurf_result(:,:), ET_flux(:,:), &
                                   L_result(:,:), Qgw_result(:,:), deltas(:,:)

  integer, parameter :: terminal = 6

    !>structure with basic model configuration
  type, public :: configuration
    !> run dual yes or no
    character(len=1)    :: run_dual
    !> use damped newton for the modified pickard iteration y/n
    character(len=1)    :: damped_newton
    !> problem dimension 1 = 1D / 2 = 2D / 3 = 3D
    integer(1) :: dimen = 2
    !> mesh_type = 1 internal mesh generator (simple)
    !! mesh_type = 2 t3d mesh generator
    !< mesh type = 3 gmsh mesh generator
    integer(kind=ikind) :: mesh_type
    !> adapt time step to the observation time, or calculate the values of the observation time by linear approximation
    logical    :: adapt_observe
    !> parameter to decide if the code execution begins in some old backup
    logical    :: run_from_backup
    !> evaluate constitutive functions from table created at program init or directly?
    !! 0 - for direct evaluation
    !! 1 - to tabelarize the values and linearly approximate values between
    !<
    integer(kind=ikind) :: fnc_method
    !> length of interval in between the values in function table
    real(kind=rkind)    :: fnc_discr_length
    !>nonlinear iteration method
    !!0 - standard Picard method
    !! 1 - Schwarz-Picard method (dd-adaptivity)
    !!2 - no iterations
    !<
    integer(kind=ikind) :: it_method
    !> descriptor of the problem type (REstdH = standard Richards equation in total hydraulic head form
    character(len=256) :: name
    !> full name of the partial differential equation problem -- e.g. dual permeability Richards equation in total hydraulic head form
    character(len=4096) :: fullname
    !> if .true. then DRUtES solves rotational symmetric flow
    logical :: rotsym=.false.
    !> if .true. then DRUtES checks for integral mass balance errors, requires more computational power
    logical :: check4mass=.false.
  end type configuration

  !> program configuration
  type(configuration), public :: drutes_config

end module globals
