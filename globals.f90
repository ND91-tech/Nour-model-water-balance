module globals
  use typy
  implicit none

  integer, public :: terminal=5

  integer, parameter :: N_cells = 5
  integer, parameter :: n_days = 1

   ! Arrays
     real(kind=rkind), dimension(:,:), allocatable :: &
       dx, dy, precip, qinter, qout, &
       conduct, G, Tmax, Tmin, Tmean, &
       RHmax, RHmin, uz, soilcontent

  real(kind=rkind), dimension(:,:), allocatable :: Qsurf_result, ET_flux, L_result, Qgw_result, deltas

   ! Scalars
     integer :: CN, J
    real(rkind) ::  phi, as, bs, z, alpha, sigma, gsc, ccrop

  integer :: t, cell

  type, public :: date_str
    integer :: month
    integer :: day
    integer :: year
  end type date_str

  type, public :: indata_str
    type(date_str) :: date
    real(kind=rkind) :: value
  end type indata_str

  type(indata_str), allocatable :: rainfall, temp_mean, temp_min, temp_max

end module globals
