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

!> \file debug_tools.f90
!! \brief Debug tools for debugging DRUtES in terminal. 
!<

!> Contains generic procedure printmtx for printing different types of arrays, and 
!! substitution for pause (depricated procedure)
!<


module debug_tools
  use globals
  private :: print_real_matrix, print_int_matrix, print_real_vector, print_int_vector, print_real_vector4
  public :: wait, print_filename



!   private :: print_quadpnt
  
  !> generic procedure, can be overloaded with different vector/matrix types
  interface printmtx
    module procedure print_real_matrix
    module procedure print_int_matrix
    module procedure print_real_vector
    module procedure print_char_vector
    module procedure print_int_vector
!     module procedure print_sparse_matrix
!     module procedure print_smartmatrix_i
!     module procedure print_smartarray_i
!     module procedure print_smartmatrix_r
!     module procedure print_smartarray_r
    module procedure print_logical_array
    module procedure print_real_vector4
!     module procedure print_quadpnt
  end interface printmtx
 

  contains
  
    function print_filename(unt) result(answer)
      use typy

      integer, intent(in) :: unt
      integer :: ids
      character(len=512) :: filename
      character(:), allocatable :: answer
      logical :: openedq
      
      ids = unt
      
      inquire(unit=ids, opened=openedq, name=filename)
      
      if (openedq) then
        answer = trim(filename)
      else
        answer = "file not opened"
      end if
      
    end function print_filename


    
        !> prints vector of characters
    subroutine print_char_vector(V, filunit, name)
      use typy
      
      !parametry
      character(len=*), dimension(:), intent(in) :: V  !<vektor k tisknuti
      integer, intent(in), optional :: filunit   
      character(len=*), intent(in), optional :: name

      integer :: filloc
      integer :: ierr
      logical :: op
      integer(kind=ikind) :: i
      
      if (present(name)) then
        open(newunit=filloc, file=name, action="write", status="replace", iostat=ierr)
        if (ierr /= 0) then
          print *, "unable to open dump file, called from debug_tools::printmtx"
          error stop
        end if
      else if (present(filunit)) then
        filloc = filunit
        inquire(unit=filloc, opened=op)
        if (.not. op) then
          print *, "file not opened, called from debug_tools::printmtx"
          error stop
        end if
      else
        filloc = terminal
      end if
     

      do i=lbound(V,1),ubound(V,1)
       write(unit=filloc, fmt=*)  i,  V(i)
      end do

      if (terminal /= filloc) then
        close(filloc)
      else
        call flush(terminal)
      end if
  
    end subroutine print_char_vector


    !> prints vector of reals
    subroutine print_real_vector(V, filunit, name)
    ! vytiskne vektor, pocet sloupcu tisku je nc
      use typy

      
      !parametry
      real(kind=rkind), dimension(:), intent(in) :: V  !<vektor k tisknuti
      integer, intent(in), optional :: filunit   
      character(len=*), intent(in), optional :: name

      integer :: filloc
      integer :: ierr
      logical :: op
      integer(kind=ikind) :: i
      
      if (present(name)) then
        open(newunit=filloc, file=name, action="write", status="replace", iostat=ierr)
        if (ierr /= 0) then
          print *, "unable to open dump file, called from debug_tools::printmtx"
          error stop
        end if
      else if (present(filunit)) then
        filloc = filunit
        inquire(unit=filloc, opened=op)
        if (.not. op) then
          print *, "file not opened, called from debug_tools::printmtx"
          error stop
        end if
      else
        filloc = terminal
      end if
     

      do i=lbound(V,1),ubound(V,1)
       write(unit=filloc, fmt=*)  i,  V(i)
      end do

      if (terminal /= filloc) then
        close(filloc)
      else
        call flush(terminal)
      end if
  
    end subroutine print_real_vector


        !> prints vector of single reals
    subroutine print_real_vector4(V, filunit, name)
      use typy
      
      !parametry
      real(4), dimension(:), intent(in) :: V  !<vektor k tisknuti
      integer, intent(in), optional :: filunit   
      character(len=*), intent(in), optional :: name

      integer :: filloc
      integer :: ierr
      logical :: op
      integer(kind=ikind) :: i
      
      if (present(name)) then
        open(newunit=filloc, file=name, action="write", status="replace", iostat=ierr)
        if (ierr /= 0) then
          print *, "unable to open dump file, called from debug_tools::printmtx"
          error stop
        end if
      else if (present(filunit)) then
        filloc = filunit
        inquire(unit=filloc, opened=op)
        if (.not. op) then
          print *, "file not opened, called from debug_tools::printmtx"
          error stop
        end if
      else
        filloc = terminal
      end if
     

      do i=lbound(V,1),ubound(V,1)
       write(unit=filloc, fmt=*) "row:", i, "value:", V(i)
      end do

      if (terminal /= filloc) then
        close(filloc)
      else
        call flush(terminal)
      end if
  
    end subroutine print_real_vector4

    !> prints vector of integers
    subroutine print_int_vector(V, filunit, name)
    ! vytiskne vektor, pocet sloupcu tisku je nc
      use typy

      
      !parametry
      integer(kind=ikind), dimension(:), intent(in) :: V  !<vektor k tisknuti
      integer, intent(in), optional :: filunit   
      character(len=*), intent(in), optional :: name

      integer :: filloc
      integer :: ierr
      logical :: op
      integer(kind=ikind) :: i
      
      if (present(name)) then
        open(newunit=filloc, file=name, action="write", status="replace", iostat=ierr)
        if (ierr /= 0) then
          print *, "unable to open dump file, called from debug_tools::printmtx"
          error stop
        end if
      else if (present(filunit)) then
        filloc = filunit
        inquire(unit=filloc, opened=op)
        if (.not. op) then
          print *, "file not opened, called from debug_tools::printmtx"
          error stop
        end if
      else
        filloc = terminal
      end if

      do i=lbound(V,1),ubound(V,1)
         write(unit=filloc, fmt=*) "row:", i, "value:", V(i)
      end do
  
      if (terminal /= filloc) then
        close(filloc)
      else
        call flush(terminal)
      end if
  

    end subroutine print_int_vector
    
    
    !> prints dense matrix of reals
    subroutine print_real_matrix(a, filunit, name)
      use typy


      real(kind=rkind), dimension(:,:), intent(in out) :: a
      integer, intent(in), optional :: filunit
      character(len=*), intent(in), optional :: name

      integer :: filloc
      integer :: ierr
      logical :: op
      integer(kind=ikind) :: i

      if (present(name)) then
        open(newunit=filloc, file=name, action="write", status="replace", iostat=ierr)
        if (ierr /= 0) then
          print *, "unable to open dump file, called from debug_tools::printmtx"
          error stop
        end if
      else if (present(filunit)) then
        filloc = filunit
        inquire(unit=filloc, opened=op)
        if (.not. op) then
          print *, "file not opened, called from debug_tools::printmtx"
          error stop
        end if
      else
        filloc = terminal
      end if


      do i=lbound(a,1), ubound(a,1)
        write(unit=filloc, fmt=*)  i, "|",  a(i,:)
      end do

      if (terminal /= filloc) then
        close(filloc)
      else
        call flush(terminal)
      end if

    end subroutine print_real_matrix

    !> prints dense matrix of integers
    subroutine print_int_matrix(a, filunit, name)
      use typy


      integer(kind=ikind), dimension(:,:), intent(in) :: a
      integer, intent(in), optional :: filunit
      character(len=*), intent(in), optional :: name

      integer :: filloc
      integer :: ierr
      logical :: op
      integer(kind=ikind) :: i

      if (present(name)) then
        open(newunit=filloc, file=name, action="write", status="replace", iostat=ierr)
        if (ierr /= 0) then
          print *, "unable to open dump file, called from debug_tools::printmtx"
          error stop
        end if
      else if (present(filunit)) then
        filloc = filunit
        inquire(unit=filloc, opened=op)
        if (.not. op) then
          print *, "file not opened, called from debug_tools::printmtx"
          error stop
        end if
      else
        filloc = terminal
      end if


      do i=lbound(a,1), ubound(a,1)
        write(unit=filloc, fmt=*)  i, "|",  a(i,:)
      end do

      if (terminal /= filloc) then
        close(filloc)
      else
        call flush(terminal)
      end if

    end subroutine print_int_matrix

    
    

    
    
    
    !> prints vector of logicals
    subroutine print_logical_array(array, filunit, name)
      use typy

      
      !parametry
      logical, dimension(:), intent(in) :: array  !<vektor k tisknuti
      integer, intent(in), optional :: filunit   
      character(len=*), intent(in), optional :: name

      integer :: filloc
      integer :: ierr
      logical :: op
      integer(kind=ikind) :: i
      
      if (present(name)) then
        open(newunit=filloc, file=name, action="write", status="replace", iostat=ierr)
        if (ierr /= 0) then
          print *, "unable to open dump file, called from debug_tools::printmtx"
          error stop
        end if
      else if (present(filunit)) then
        filloc = filunit
        inquire(unit=filloc, opened=op)
        if (.not. op) then
          print *, "file not opened, called from debug_tools::printmtx"
          error stop
        end if
      else
        filloc = terminal
      end if
      
      do i=1, ubound(array,1)
        write(unit=filloc, fmt=*) i, "|", array(i)
      end do
      
    end subroutine print_logical_array




    !> substitution of depricated function pause
    subroutine wait(ch) 
      use globals       
      character(len=*), optional, intent(in) :: ch
      
      if (present(ch)) then
        print *, "-------------"
        print *, trim(ch)
        print *, "-------------"
      end if

      print *, "press [ENTER] to continue"
  
      call flush(terminal)
  
      read(*,*)


    end subroutine wait
    
    
!   


end module debug_tools
