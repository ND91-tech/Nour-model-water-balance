module initvals
  use typy
  use debug_tools
  implicit none
  contains
    subroutine init()
      use readtools
      use globals



      integer :: filenodes, fileel 
      integer(kind=ikind) :: n, i, tmp

      open(newunit=filenodes, file='nodes.in.tsv', action="read", status="old")

      open(newunit=fileel, file='elements.in.tsv', action="read", status="old")

    

      call fileread(n, filenodes)

print *, n
      allocate(nodes%data(n,2))
      print *, "---"

      do i=1,n
        call comment(filenodes)
        read(filenodes,*) tmp, nodes%data(i,:)
      end do

      call printmtx(nodes%data)

      stop



      allocate(dx(N_cells, n_days), dy(N_cells, n_days), precip(N_cells, n_days), &
              qinter(N_cells, n_days), qout(N_cells, n_days), conduct(N_cells, n_days), &
              G(N_cells, n_days), Tmax(N_cells, n_days), Tmin(N_cells, n_days), &
              Tmean(N_cells, n_days), RHmax(N_cells, n_days), RHmin(N_cells, n_days), &
              uz(N_cells, n_days), soilcontent(N_cells, n_days), &
              Qsurf_result(N_cells, n_days), ET_flux(N_cells, n_days), &
              L_result(N_cells, n_days), Qgw_result(N_cells, n_days), deltas(N_cells, n_days))

              
      dx = reshape([1.0_rkind, 5.0_rkind, 4.0_rkind, 6.0_rkind, 3.0_rkind], [N_cells, n_days])
      dy = reshape([6.0_rkind, 7.0_rkind, 10.0_rkind, 8.0_rkind, 3.0_rkind], [N_cells, n_days])
      precip = reshape([0.0_rkind, 0.0_rkind, 17.0_rkind, 12.0_rkind, 9.0_rkind], [N_cells, n_days])
      qinter = reshape([0.958_rkind, 0.67_rkind, 0.858_rkind, 0.985_rkind, 0.534_rkind], [N_cells, n_days])
      qout = reshape([0.1_rkind, 0.24_rkind, 0.12_rkind, 0.09_rkind, 0.15_rkind], [N_cells, n_days])
      conduct = reshape([0.014_rkind, 0.05_rkind, 0.034_rkind, 0.022_rkind, 0.012_rkind], [N_cells, n_days])
      G = 0.0_rkind
      uz = reshape([10.0_rkind, 11.0_rkind, 9.0_rkind, 3.4_rkind, 2.5_rkind], [N_cells, n_days])
      Tmax = reshape([10.2_rkind, 8.8_rkind, 9.7_rkind, 14.2_rkind, 14.1_rkind], [N_cells, n_days])
      Tmin = reshape([5.4_rkind, 2.4_rkind, 7.6_rkind, 10.4_rkind, 11.1_rkind], [N_cells, n_days])
      Tmean = reshape([7.8_rkind, 5.6_rkind, 8.65_rkind, 12.3_rkind, 12.6_rkind], [N_cells, n_days])
      RHmax = reshape([40.1_rkind, 45.5_rkind, 30.4_rkind, 33.5_rkind, 36.2_rkind], [N_cells, n_days])
      RHmin = reshape([35.2_rkind, 40.2_rkind, 27.8_rkind, 35.7_rkind, 32.4_rkind], [N_cells, n_days])
      soilcontent = reshape([0.05_rkind, 0.07_rkind, 0.2_rkind, 0.25_rkind, 0.3_rkind], [N_cells, n_days])

      ! Initialize scalars
        CN = 98
        z = 3_rkind
        J = 172      ! Roughly corresponds to June 21 (summer solstice)
        phi = 0.872_rkind
        as = 0.25_rkind
        bs = 0.05_rkind
        alpha = 0.23_rkind
        sigma = 4.903e-5_rkind
        gsc = 0.0820_rkind
        ccrop = 0.8_rkind

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

