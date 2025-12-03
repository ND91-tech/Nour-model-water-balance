! tools.f90
module tools
  use typy
  use globals
  use geom_tools      ! for find_neighbours
  implicit none
contains

  ! -----------------------------------------------------
  ! Read mesh file: id, x, y, z for nodes; triangles for elements
  ! -----------------------------------------------------
  subroutine read_mesh(filename)
    character(len=*), intent(in) :: filename
    integer :: unit, ios, n_nodes, n_elem
    integer :: id, n1, n2, n3, i
    real(kind=rkind) :: x, y, zloc
    character(len=256) :: line
    logical :: file_exists

    inquire(file=filename, exist=file_exists)
    if (.not. file_exists) then
       print *, " Error: File ", trim(filename), " not found."
       stop
    end if

    open(newunit=unit, file=filename, status="old", action="read", iostat=ios)
    if (ios /= 0) stop " Could not open mesh file."

    ! --- number of nodes ---
    do
       read(unit,'(A)',iostat=ios) line
       if (ios < 0) stop "Unexpected EOF before node count"
       if (len_trim(line) > 0 .and. line(1:1) /= "#") exit
    end do
    read(line,*) n_nodes
    nodes%kolik = n_nodes

    allocate(nodes%data(n_nodes,2))
    allocate(nodes%altitude(n_nodes))

    i = 0
    do while (i < n_nodes)
       read(unit,'(A)',iostat=ios) line
       if (ios < 0) exit
       if (len_trim(line) == 0 .or. line(1:1) == "#") cycle
       ! Expect: id  x   y   z
       read(line,*) id, x, y, zloc
       i = i + 1
       nodes%data(i,1)   = x
       nodes%data(i,2)   = y
       nodes%altitude(i) = zloc
    end do
    print *, " Read", i, "nodes."

    ! --- number of elements ---
    do
       read(unit,'(A)',iostat=ios) line
       if (ios < 0) stop "Unexpected EOF before element count"
       if (len_trim(line) > 0 .and. line(1:1) /= "#") exit
    end do
    read(line,*) n_elem
    elements%kolik = n_elem

    allocate(elements%data(n_elem,3))
    allocate(elements%area(n_elem))
    allocate(elements%material(n_elem))
    allocate(elements%avgalt(n_elem))
    allocate(elements%overflow(n_elem))
    allocate(elements%neighbours(n_elem,3))

    i = 0
    do while (i < n_elem)
       read(unit,'(A)',iostat=ios) line
       if (ios < 0) exit
       if (len_trim(line) == 0 .or. line(1:1) == "#") cycle
       read(line,*) id, n1, n2, n3
       i = i + 1
       elements%data(i,:)   = [n1, n2, n3]
       elements%material(i) = 1_ikind
    end do
    print *, " Read", i, "triangular elements."
    close(unit)

    ! Allocate hydrological structure if needed
    if (.not. allocated(elements%hydrobal)) then
       allocate(elements%hydrobal(n_elem))
    end if
  end subroutine read_mesh

  ! -----------------------------------------------------
  ! Compute triangle areas + total area
  ! -----------------------------------------------------
  subroutine compute_areas()
    integer(kind=ikind) :: i
    integer(kind=ikind) :: n1, n2, n3
    real(kind=rkind) :: a(2), b(2), c(2)
    real(kind=rkind) :: total_area

    total_area = 0.0_rkind

    do i = 1_ikind, elements%kolik
       n1 = elements%data(i,1)
       n2 = elements%data(i,2)
       n3 = elements%data(i,3)
       a  = nodes%data(n1,:)
       b  = nodes%data(n2,:)
       c  = nodes%data(n3,:)
       elements%area(i) = area_triangle(a,b,c)
       total_area       = total_area + elements%area(i)
    end do

    print "(A,F12.3)", " Total mesh area = ", total_area
  end subroutine compute_areas

  pure function area_triangle(a,b,c) result(area)
    real(kind=rkind), intent(in) :: a(2), b(2), c(2)
    real(kind=rkind) :: area
    area = 0.5_rkind * abs(a(1)*(b(2)-c(2)) + b(1)*(c(2)-a(2)) + c(1)*(a(2)-b(2)))
  end function area_triangle

  ! -----------------------------------------------------
  ! Compute mean elevation per element
  ! -----------------------------------------------------
  subroutine compute_avgalt()
    integer(kind=ikind) :: i, n1, n2, n3
    real(kind=rkind) :: z1, z2, z3

    do i = 1_ikind, elements%kolik
       n1 = elements%data(i,1)
       n2 = elements%data(i,2)
       n3 = elements%data(i,3)

       if (allocated(nodes%altitude)) then
          z1 = nodes%altitude(n1)
          z2 = nodes%altitude(n2)
          z3 = nodes%altitude(n3)
       else
          ! fallback: use y as pseudo-elevation
          z1 = nodes%data(n1,2)
          z2 = nodes%data(n2,2)
          z3 = nodes%data(n3,2)
       end if

       elements%avgalt(i) = (z1 + z2 + z3) / 3.0_rkind
    end do
  end subroutine compute_avgalt

  ! -----------------------------------------------------
  ! Initialise flow topology: neighbours, avgalt, downstream, order
  ! -----------------------------------------------------
  subroutine init_flow_topology()
    integer(kind=ikind) :: nel, i

    nel = elements%kolik
    if (nel <= 0_ikind) return

    ! Neighbour table
    call find_neighbours(elements, nodes)

  

    ! Average elevation already computed by compute_avgalt()

    ! Allocate downstream + flow order
    if (.not. allocated(downstream)) allocate(downstream(nel))
    if (.not. allocated(flow_order)) allocate(flow_order(nel))

    elements%overflow = 0.0_rkind
    downstream        = 0_ikind
    flow_order        = 0_ikind

    call build_downstream_graph()
    call build_flow_order()
  end subroutine init_flow_topology

  ! -----------------------------------------------------
  ! Downstream element = neighbour with lowest avgalt
  ! -----------------------------------------------------
  subroutine build_downstream_graph()
    integer(kind=ikind) :: i, j, nb, nel
    real(kind=rkind)    :: myz, bestz
    integer(kind=ikind) :: best_nb

    nel = elements%kolik

    do i = 1_ikind, nel
       myz     = elements%avgalt(i)
       bestz   = myz
       best_nb = 0_ikind

       do j = 1_ikind, ubound(elements%neighbours,2)
          nb = elements%neighbours(i,j)
          if (nb <= 0_ikind) cycle
          if (elements%avgalt(nb) < bestz) then
             bestz   = elements%avgalt(nb)
             best_nb = nb
          end if
       end do

       downstream(i) = best_nb  ! 0 if no lower neighbour → outlet
    end do
  end subroutine build_downstream_graph

  ! -----------------------------------------------------
  ! Flow order: sort elements from highest to lowest avgalt
  ! -----------------------------------------------------
  subroutine build_flow_order()
    integer(kind=ikind) :: nel, i, j, tmp_idx
    real(kind=rkind)    :: tmp_z
    integer(kind=ikind), allocatable :: idx(:)
    real(kind=rkind),    allocatable :: z(:)

    nel = elements%kolik
    allocate(idx(nel), z(nel))

    do i = 1_ikind, nel
       idx(i) = i
       z(i)   = elements%avgalt(i)
    end do

    ! simple selection sort: descending z
    do i = 1_ikind, nel-1
       do j = i+1_ikind, nel
          if (z(j) > z(i)) then
             tmp_z   = z(i);   z(i)   = z(j);   z(j)   = tmp_z
             tmp_idx = idx(i); idx(i) = idx(j); idx(j) = tmp_idx
          end if
       end do
    end do

    flow_order = idx

    deallocate(idx, z)
  end subroutine build_flow_order

  ! -----------------------------------------------------
  ! Optional: print element balance (for debugging)
  ! -----------------------------------------------------
  subroutine print_element_balance()
    integer(kind=ikind) :: i
    real(kind=rkind)    :: surplus

    if (.not. allocated(elements%hydrobal)) then
       print *, " No hydrological data to display."
       return
    end if

    print *, "--------------------------------------------------------------------------------------------------------------"
    print *, " Element |     P        ET      Qsurf       Li       Qgw      Qin     Surplus   Qout_elem  Overflow    deltaS"
    print *, "--------------------------------------------------------------------------------------------------------------"

    do i = 1_ikind, elements%kolik
       surplus = elements%hydrobal(i)%Qsurf + &
                 elements%hydrobal(i)%Li    + &
                 elements%hydrobal(i)%Qgw

       write(*,'(I7,9F12.6)') i, precip(i,1), elements%hydrobal(i)%ET, &
            elements%hydrobal(i)%Qsurf, elements%hydrobal(i)%Li, &
            elements%hydrobal(i)%Qgw, elements%hydrobal(i)%inflow, &
            surplus, elements%hydrobal(i)%outflow, &
            elements%hydrobal(i)%deltas
    end do
  end subroutine print_element_balance

  ! -----------------------------------------------------
  ! Export element balance to CSV, with Qout_elem & Qout_catchment
  ! -----------------------------------------------------
     subroutine export_element_balance(filename)
    character(len=*), intent(in) :: filename
    integer(kind=ikind) :: i
    integer :: unit
    real(kind=rkind) :: surplus, qout_catch

    if (.not. allocated(elements%hydrobal)) then
       print *, " No hydrological data to export."
       return
    end if

    open(newunit=unit, file=filename, status="replace", action="write")

    ! Header now includes:
    ! Area, z_avg, Downstream, and Qsurf_local
    write(unit,'(A)') "Element,Area,z_avg,Downstream," // &
                      "P,ET,Qsurf,Li,Qgw,Qin,Surplus," // &
                      "Qout_elem,Qout_catchment,Overflow,DeltaS,Qsurf_local"

    do i = 1_ikind, elements%kolik

       ! Local surplus (production term)
       surplus = elements%hydrobal(i)%Qsurf + &
                 elements%hydrobal(i)%Li    + &
                 elements%hydrobal(i)%Qgw

       ! Only outlet elements contribute to catchment outflow
       if (downstream(i) == 0_ikind) then
          qout_catch = elements%hydrobal(i)%outflow
       else
          qout_catch = 0.0_rkind
       end if

       write(unit,'(I4, ",", F10.3, ",", F10.3, ",", I4, 12(",",F10.3))')  &
            i,                              & ! Element
            elements%area(i),               & ! Area
            elements%avgalt(i),             & ! z_avg
            downstream(i),                  & ! Downstream (0 = outlet)
            precip(i,1),                    & ! P
            elements%hydrobal(i)%ET,        & ! ET
            elements%hydrobal(i)%Qsurf,     & ! Qsurf
            elements%hydrobal(i)%Li,        & ! Li
            elements%hydrobal(i)%Qgw,       & ! Qgw
            elements%hydrobal(i)%inflow,    & ! Qin
            surplus,                        & ! Surplus
            elements%hydrobal(i)%outflow,   & ! Qout_elem
            qout_catch,                     & ! Qout_catchment
            elements%overflow(i),           & ! Overflow
            elements%hydrobal(i)%deltas,    & ! DeltaS
            Qsurf_result(i,1)                 ! Qsurf_local
    end do

    close(unit)
    print *, " Element balances saved to: ", trim(filename)
  end subroutine export_element_balance



     ! -----------------------------------------------------
  ! Routing + local mass balance for one time step
  ! -----------------------------------------------------
  subroutine route_step(tstep)
    integer(kind=ikind), intent(in) :: tstep

    integer(kind=ikind) :: el, i, dwn
    real(kind=rkind)    :: old_storage, local_input, local_losses, surplus
    real(kind=rkind), allocatable :: overflow_total(:), storage_new(:)

    ! Allocate temporary arrays for this time step
    allocate(overflow_total(elements%kolik))
    allocate(storage_new(elements%kolik))

    ! Reset inflow/outflow at start of step
    elements%hydrobal(:)%inflow  = 0.0_rkind
    elements%hydrobal(:)%outflow = 0.0_rkind

    overflow_total = 0.0_rkind
    storage_new    = storage   ! start from previous storage

    ! 1) LOCAL BALANCE BEFORE ROUTING
    do el = 1_ikind, elements%kolik

       old_storage = storage(el)

       ! Inputs [mm]
       local_input = precip(el,tstep) + qinter(el,tstep) + old_storage

       ! Losses [mm]
       local_losses = elements%hydrobal(el)%ET + &
                      elements%hydrobal(el)%Li + &
                      elements%hydrobal(el)%Qgw

       if (local_losses >= local_input) then
          storage_new(el) = 0.0_rkind
          surplus         = 0.0_rkind
       else
          surplus = local_input - local_losses

          ! fill storage up to capacity
          if (surplus <= capacity(el)) then
             storage_new(el) = surplus
             surplus         = 0.0_rkind
          else
             storage_new(el) = capacity(el)
             surplus         = surplus - capacity(el)
          end if
       end if

       ! local overflow (to be routed)
       overflow_total(el) = surplus + Qsurf_result(el,tstep)
    end do

    ! 2) ROUTE FLOW ALONG DOWNSTREAM GRAPH (UPSTREAM → DOWNSTREAM)
    do i = 1_ikind, elements%kolik
       el  = flow_order(i)
       dwn = downstream(el)

       ! local outflow = inflow from upstream + own overflow
       elements%hydrobal(el)%outflow = elements%hydrobal(el)%inflow + &
                                       overflow_total(el)

       if (dwn > 0_ikind) then
          ! send to downstream element
          elements%hydrobal(dwn)%inflow = elements%hydrobal(dwn)%inflow + &
                                          elements%hydrobal(el)%outflow
       else
          ! outlet element → catchment outflow hydrograph
          outlet_Q(tstep) = outlet_Q(tstep) + elements%hydrobal(el)%outflow
       end if
    end do

    ! 3) FINAL MASS BALANCE PER ELEMENT
    do el = 1_ikind, elements%kolik
       old_storage           = storage(el)
       storage(el)           = storage_new(el)
       elements%overflow(el) = overflow_total(el)

       elements%hydrobal(el)%deltas = precip(el,tstep) + qinter(el,tstep) + &
                                      old_storage + elements%hydrobal(el)%inflow - &
                                      ( elements%hydrobal(el)%ET      + &
                                        elements%hydrobal(el)%Li      + &
                                        elements%hydrobal(el)%Qgw     + &
                                        storage(el)                   + &
                                        elements%hydrobal(el)%outflow )

       deltas(el,tstep) = elements%hydrobal(el)%deltas
    end do

    deallocate(overflow_total, storage_new)
  end subroutine route_step



end module tools
