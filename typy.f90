module typy
  integer, parameter :: rkind = selected_real_kind(15, 99)
      !> integer number specification
  integer, parameter, public :: ikind = selected_int_kind(10)

  integer, parameter, public :: lowikind=selected_int_kind(5)

  integer, parameter, public :: dprec=selected_real_kind(15,99)

  integer, parameter, public :: qprec=selected_real_kind(30,999)

  integer, parameter, public :: sprec=selected_real_kind(8,9)
end module typy
