
! Copyright 2008 Michal Kuraz, Petr Mayer, Copyright 2016  Michal Kuraz, Petr Mayer, Johanna Bloecher


! This file is part of DRUtES.
! DRUtES is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
! DRUtES is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
! GNU General Public License for more details.
! You should have received a copy of the GNU General Public License
! along with DRUtES. If not, see <http://www.gnu.org/licenses/>.

!> \file typy.f90
!! \brief Specification of real types.
!<


module typy

    integer, parameter, public :: lowikind=selected_int_kind(5)
    
    integer, parameter, public :: dprec=selected_real_kind(15,99) 
    
    integer, parameter, public :: qprec=selected_real_kind(30,999) 
    
    integer, parameter, public :: sprec=selected_real_kind(8,9)
    
    

    !> real number specification
    integer, parameter, public :: rkind = selected_real_kind(15,99)
    !> integer number specification
    integer, parameter, public :: ikind = selected_int_kind(10)
    !> long integers
    integer, parameter, public :: likind = selected_int_kind(16)
    !> unicode char type
    integer, parameter, public :: chkind=selected_char_kind("default")!"ASCII")!"ISO_10646")


   

end module typy



