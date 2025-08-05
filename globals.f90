module globals
  use typy
  implicit none

  integer, public :: terminal=5

  integer, parameter :: N_cells = 5
  integer, parameter :: n_hours = 1

  real(kind=rkind), dimension(:,:), allocatable :: &
    dx, dy, precip, qinter, qout, conduct, G, Tmax, Tmin, Tmean, RHmax, &
    RHmin, uz, soilcontent, actvapress, satvapress, wind, &
    Rn, Rl, Rg, Ha, Ts, Ta

  real(kind=rkind), dimension(:,:), allocatable :: Qsurf_result, ET_flux, L_result, Qgw_result, deltas

  integer :: CN, z
  real(kind=rkind) :: sigma, ccrop

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
