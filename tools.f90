module tools
  use typy
  use globals
  implicit none
contains
  ! =====================================================
  ! === Mesh reading & geometry section =================
  ! =====================================================
  subroutine read_mesh(filename)
    character(len=*), intent(in) :: filename
    integer :: unit, ios, n_nodes, n_elem
    integer :: id, n1, n2, n3, i
    real(kind=rkind) :: x, y
    character(len=256) :: line
    logical :: file_exists

    inquire(file=filename, exist=file_exists)
    if (.not. file_exists) then
       print *, " Error: File ", trim(filename), " not found."
       stop
    end if

    open(newunit=unit, file=filename, status="old", action="read", iostat=ios)
    if (ios /= 0) stop " Could not open mesh file."

    ! --- Read number of nodes ---
    do
       read(unit,'(A)',iostat=ios) line
       if (ios < 0) stop "Unexpected EOF before node count"
       if (len_trim(line) > 0 .and. line(1:1) /= "#") exit
    end do
    read(line,*) n_nodes
    nodes%kolik = n_nodes
    allocate(nodes%xy(n_nodes,2))
    i = 0
    do while (i < n_nodes)
       read(unit,'(A)',iostat=ios) line
       if (ios < 0) exit
       if (len_trim(line) == 0 .or. line(1:1) == "#") cycle
       read(line,*) id, x, y
       i = i + 1
       nodes%xy(i,1) = x
       nodes%xy(i,2) = y
    end do
    print *, "Read", i, "nodes."

    ! --- Read number of elements ---
    do
       read(unit,'(A)',iostat=ios) line
       if (ios < 0) stop "Unexpected EOF before element count"
       if (len_trim(line) > 0 .and. line(1:1) /= "#") exit
    end do
    read(line,*) n_elem
    elements%kolik = n_elem
    allocate(elements%conn(n_elem,3), elements%area(n_elem))

    i = 0
    do while (i < n_elem)
       read(unit,'(A)',iostat=ios) line
       if (ios < 0) exit
       if (len_trim(line) == 0 .or. line(1:1) == "#") cycle
       read(line,*) id, n1, n2, n3
       i = i + 1
       elements%conn(i,:) = [n1, n2, n3]
    end do
    print *, " Read", i, "triangular elements."
    close(unit)

    ! --- Allocate hydrological structure ---
    allocate(elements%hydrobal(n_elem))
    do i = 1, n_elem
       elements%hydrobal(i)%deltas  = 0.0_rkind
       elements%hydrobal(i)%inflow  = 0.0_rkind
       elements%hydrobal(i)%outflow = 0.0_rkind
       elements%hydrobal(i)%Li      = 0.0_rkind
       elements%hydrobal(i)%ET      = 0.0_rkind
       elements%hydrobal(i)%Qgw     = 0.0_rkind
       elements%hydrobal(i)%Qsurf   = 0.0_rkind
    end do
  end subroutine read_mesh


  ! =====================================================
  ! === Compute element areas ===========================
  ! =====================================================
  subroutine compute_areas()
    integer :: i
    integer(kind=ikind) :: n1, n2, n3
    real(kind=rkind) :: a(2), b(2), c(2)
    real(kind=rkind) :: total_area
    total_area = 0.0_rkind

    do i = 1, elements%kolik
       n1 = elements%conn(i,1)
       n2 = elements%conn(i,2)
       n3 = elements%conn(i,3)
       a = nodes%xy(n1,:)
       b = nodes%xy(n2,:)
       c = nodes%xy(n3,:)
       elements%area(i) = area_triangle(a,b,c)
       total_area = total_area + elements%area(i)
    end do

    print "(A,F12.3)", "🧩 Total mesh area = ", total_area
  end subroutine compute_areas


  pure function area_triangle(a,b,c) result(area)
    real(kind=rkind), intent(in) :: a(2), b(2), c(2)
    real(kind=rkind) :: area
    area = 0.5_rkind * abs(a(1)*(b(2)-c(2)) + b(1)*(c(2)-a(2)) + c(1)*(a(2)-b(2)))
  end function area_triangle


  ! =====================================================
  ! === NEW: Print element hydrological balance =========
  ! =====================================================
  subroutine print_element_balance()
    integer :: i
    if (.not. allocated(elements%hydrobal)) then
       print *, " No hydrological data to display."
       return
    end if

    print *, "-----------------------------------------------"
    print *, "Elem |    P     ET   Qsurf   Li    Qgw   Qin   Qout   ΔS"
    do i = 1, elements%kolik
       write(*,'(I4,8F8.3)') i, &
            precip(i,1), elements%hydrobal(i)%ET, elements%hydrobal(i)%Qsurf, &
            elements%hydrobal(i)%Li, elements%hydrobal(i)%Qgw, &
            elements%hydrobal(i)%inflow, elements%hydrobal(i)%outflow, &
            elements%hydrobal(i)%deltas
    end do
  end subroutine print_element_balance


  ! =====================================================
  ! === NEW: Export balance to CSV ======================
  ! =====================================================
  subroutine export_element_balance(filename)
    character(len=*), intent(in) :: filename
    integer :: i, unit

    if (.not. allocated(elements%hydrobal)) then
       print *, " No hydrological data to export."
       return
    end if

    open(newunit=unit, file=filename, status="replace", action="write")
    write(unit,'(A)') "Element,P,ET,Qsurf,Li,Qgw,Qin,Qout,DeltaS"
    do i = 1, elements%kolik
       write(unit,'(I4,8(",",F10.3))') i, precip(i,1), &
            elements%hydrobal(i)%ET, elements%hydrobal(i)%Qsurf, &
            elements%hydrobal(i)%Li, elements%hydrobal(i)%Qgw, &
            elements%hydrobal(i)%inflow, elements%hydrobal(i)%outflow, &
            elements%hydrobal(i)%deltas
    end do
    close(unit)
    print *, " Element balances saved to: ", trim(filename)
  end subroutine export_element_balance

end module tools
