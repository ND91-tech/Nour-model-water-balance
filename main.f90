program nour
  use initvals
  use core_tools
  use hydrofnc

  implicit none


  call init()
  call compute_rootflux()

  print *, "  cell  hour    ET_flux   Qsurf    Leakage   GWflow    Delta"

  do cell = 1, N_cells
    do t = 1, n_hours

      print *, cell, t, ET_flux(cell,t), Qsurf_result(cell,t), &
            L_result(cell,t), Qgw_result(cell,t), deltas(cell,t)

    end do
  end do


end program nour
