module initvals
  use typy
  use globals
  implicit none
  contains
    subroutine init()
      allocate(dx(N_cells, n_hours), dy(N_cells, n_hours), precip(N_cells, n_hours), &
              qinter(N_cells, n_hours), qout(N_cells, n_hours), conduct(N_cells, n_hours), &
              G(N_cells, n_hours), Tmax(N_cells, n_hours), Tmin(N_cells, n_hours), &
              Tmean(N_cells, n_hours), RHmax(N_cells, n_hours), RHmin(N_cells, n_hours), &
              uz(N_cells, n_hours), soilcontent(N_cells, n_hours), &
              Rg(N_cells, n_hours), Ts(N_cells, n_hours), Ta(N_cells, n_hours), &
              Ha(N_cells, n_hours), actvapress(N_cells, n_hours), satvapress(N_cells, n_hours), &
              wind(N_cells, n_hours), Rn(N_cells, n_hours), Rl(N_cells, n_hours), &
              Qsurf_result(N_cells, n_hours), ET_flux(N_cells, n_hours), &
              L_result(N_cells, n_hours), Qgw_result(N_cells, n_hours), deltas(N_cells, n_hours))

      dx = reshape([1.0_rkind, 5.0_rkind, 4.0_rkind, 6.0_rkind, 3.0_rkind], [N_cells, n_hours])
      dy = reshape([6.0_rkind, 7.0_rkind, 10.0_rkind, 8.0_rkind, 3.0_rkind], [N_cells, n_hours])
      precip = reshape([0.0_rkind, 0.0_rkind, 17.0_rkind, 12.0_rkind, 9.0_rkind], [N_cells, n_hours])
      qinter = reshape([0.958_rkind, 0.67_rkind, 0.858_rkind, 0.985_rkind, 0.534_rkind], [N_cells, n_hours])
      qout = reshape([0.1_rkind, 0.24_rkind, 0.12_rkind, 0.09_rkind, 0.15_rkind], [N_cells, n_hours])
      conduct = reshape([0.014_rkind, 0.05_rkind, 0.034_rkind, 0.022_rkind, 0.012_rkind], [N_cells, n_hours])
      G = 0.0_rkind
      uz = reshape([10.0_rkind, 11.0_rkind, 9.0_rkind, 3.4_rkind, 2.5_rkind], [N_cells, n_hours])
      Rg = reshape([7.8_rkind, 5.6_rkind, 8.65_rkind, 12.3_rkind, 12.6_rkind], [N_cells, n_hours])
      Ts = reshape([10.4_rkind, 8.5_rkind, 9.6_rkind, 10.4_rkind, 11.45_rkind], [N_cells, n_hours])
      Ta = reshape([11.2_rkind, 9.7_rkind, 10.4_rkind, 10.78_rkind, 12.33_rkind], [N_cells, n_hours])
      Tmax = reshape([10.2_rkind, 8.8_rkind, 9.7_rkind, 14.2_rkind, 14.1_rkind], [N_cells, n_hours])
      Tmin = reshape([5.4_rkind, 2.4_rkind, 7.6_rkind, 10.4_rkind, 11.1_rkind], [N_cells, n_hours])
      Tmean = reshape([7.8_rkind, 5.6_rkind, 8.65_rkind, 12.3_rkind, 12.6_rkind], [N_cells, n_hours])
      RHmax = reshape([40.1_rkind, 45.5_rkind, 30.4_rkind, 33.5_rkind, 36.2_rkind], [N_cells, n_hours])
      RHmin = reshape([35.2_rkind, 40.2_rkind, 27.8_rkind, 35.7_rkind, 32.4_rkind], [N_cells, n_hours])
      Ha = reshape([45.3_rkind, 42.8_rkind, 34.1_rkind, 35.6_rkind, 36.8_rkind], [N_cells, n_hours])
      soilcontent = reshape([0.05_rkind, 0.07_rkind, 0.2_rkind, 0.25_rkind, 0.3_rkind], [N_cells, n_hours])

      CN = 98
      sigma = 5.67_rkind
      ccrop = 0.2_rkind
      z = 3
    end subroutine init

    subroutine data_reader(meteodata, filename)
      use typy
      use core_tools
      use globals

      type(indata_str), dimension(:), allocatable, intent(out) :: meteodata
      character(len=*), intent(in) :: filename

      integer :: fileid, ierr

      open(newunit=fileid, file=cut(filename), action="read", status="replace", iostat=ierr)



    end subroutine data_reader
end module initvals

