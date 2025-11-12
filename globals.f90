module globals
  use typy
  implicit none

  ! =====================================================
  ! === Geometry Structures =============================
  ! =====================================================

  !> Node coordinates and watershed mask
  type :: nodes_str
     real(kind=rkind), allocatable :: xy(:,:)          ! (n_nodes, 2)
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
     integer(kind=ikind), allocatable :: conn(:,:)     ! element-node connectivity
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
  integer :: CN, J, t
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

end module globals
