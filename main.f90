program nour
  use typy
  use globals
  use initvals
  use hydrofnc
  use tools
  use geom_tools
  implicit none

  integer :: i
  character(len=256) :: mesh_file_name
  logical :: file_exists



  ! =====================================================
  ! === Step 0: File check ===============================
  ! =====================================================
  mesh_file_name = "mesh.txt"    ! Specify your mesh file

  print *, " Opening mesh file: ", trim(mesh_file_name)
  inquire(file=mesh_file_name, exist=file_exists)
  if (.not. file_exists) stop " Mesh file not found!"

  ! =====================================================
  ! === Step 1: Read mesh & compute geometry =============
  ! =====================================================
  call read_mesh(mesh_file_name)
  call compute_areas()

  print *, "-----------------------------------------------"
  print *, " Node Coordinates"
  do i = 1, ubound(nodes%data,1)
     print '(I4, 2F10.3)', i, nodes%data(i,1), nodes%data(i,2)
  end do

  print *, "-----------------------------------------------"
  print *, " Element Areas"
  do i = 1, elements%kolik
     print '(I4, F12.3)', i, elements%area(i)
  end do

  ! =====================================================
  ! === Step 2: Initialize hydrological inputs ===========
  ! =====================================================
  call init()

  ! =====================================================
  ! === Step 3: Compute hydrological balance =============
  ! =====================================================
  call compute_all()

  ! =====================================================
  ! === Step 4: Print element-based results ==============
  ! =====================================================
  print *, "-----------------------------------------------"
  print *, " Element |     P     ET   Qsurf    Li     Qgw    Qin   Qout    deltaS"
  do i = 1, elements%kolik
     print '(I7, 8F12.6)', i, precip(i,1), elements%hydrobal(i)%ET, &
            elements%hydrobal(i)%Qsurf, elements%hydrobal(i)%Li, &
            elements%hydrobal(i)%Qgw, elements%hydrobal(i)%inflow, &
            elements%hydrobal(i)%outflow, elements%hydrobal(i)%deltas
  end do

  ! =====================================================
  ! === Step 5: Export results ===========================
  ! =====================================================
  call export_element_balance("element_balance.csv")
  print *, "-----------------------------------------------"
  print *, "   Model run completed successfully."
  print *, "  Element balance results saved to element_balance.csv"

end program nour
