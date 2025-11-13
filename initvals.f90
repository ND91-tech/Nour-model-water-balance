module initvals
  use typy
  use globals
  implicit none
contains

  subroutine init()
    integer :: i

    ! =====================================================
    ! === Allocate hydrological arrays ====================
    ! =====================================================
    allocate( precip(elements%kolik, n_days), qinter(elements%kolik, n_days), qout(elements%kolik, n_days), &
          conduct(elements%kolik, n_days), G(elements%kolik, n_days), Tmax(elements%kolik, n_days), &
          Tmin(elements%kolik, n_days), Tmean(elements%kolik, n_days), RHmax(elements%kolik, n_days), &
          RHmin(elements%kolik, n_days), uz(elements%kolik, n_days), soilcontent(elements%kolik, n_days), &
          Qsurf_result(elements%kolik, n_days), ET_flux(elements%kolik, n_days), L_result(elements%kolik, n_days), &
          Qgw_result(elements%kolik, n_days), deltas(elements%kolik, n_days) )



    ! =====================================================
    ! === Allocate & initialize element-level hydrology ====
    ! =====================================================
if (elements%kolik > 0) then
   if (.not. allocated(elements%hydrobal)) then
      allocate(elements%hydrobal(elements%kolik))
   end if
   do i = 1, elements%kolik
      elements%hydrobal(i)%inflow  = 0.0_rkind
      elements%hydrobal(i)%outflow = 0.0_rkind
      elements%hydrobal(i)%Li      = 0.0_rkind
      elements%hydrobal(i)%ET      = 0.0_rkind
      elements%hydrobal(i)%Qgw     = 0.0_rkind
      elements%hydrobal(i)%Qsurf   = 0.0_rkind
      elements%hydrobal(i)%deltas  = 0.0_rkind
   end do
end if


    ! =====================================================
    ! === Example meteorological inputs ===================
    ! =====================================================
       precip = reshape([0.0_rkind, 0.0_rkind, 17.0_rkind, 12.0_rkind, 9.0_rkind, 7.0_rkind, &
      40.0_rkind, 0.0_rkind, 0.0_rkind, 3.0_rkind], [elements%kolik, n_days])
      
      qinter = reshape([0.0_rkind, 0.0_rkind, 0.015_rkind, 0.018_rkind, 0.02_rkind, 0.022_rkind, &
      0.025_rkind, 0.0235_rkind, 0.03_rkind, 0.031_rkind], [elements%kolik, n_days])
  
      qout = reshape([3.3_rkind, 3.3_rkind, 3.3_rkind, 3.3_rkind, 3.3_rkind, 3.3_rkind, &
      3.3_rkind, 3.3_rkind, 3.3_rkind, 3.3_rkind], [elements%kolik, n_days])
      
      conduct = 0.000005_rkind
      
      G = 0.0_rkind
      uz = reshape([4.38_rkind, 3.57_rkind, 4.026_rkind, 3.097_rkind, 4.14_rkind, &
      3.13_rkind, 3.92_rkind, 3.19_rkind, 3.98_rkind, 3.34_rkind], &
      [elements%kolik, n_days])
      
      Tmax = reshape([19.1_rkind, 15.3_rkind, 12.8_rkind, 11.8_rkind, 10.5_rkind, 15.2_rkind, &
      11.6_rkind, 14.6_rkind, 17.2_rkind, 16.4_rkind], &
      [elements%kolik, n_days])
      
      Tmin = reshape([5.4_rkind, 6.8_rkind, 8.8_rkind, 7.6_rkind, 8.4_rkind, 8.3_rkind, &
      8.8_rkind, 6.2_rkind, 4.8_rkind, 6.2_rkind],[elements%kolik, n_days])
      
      Tmean = reshape([12.25_rkind, 11.05_rkind, 10.8_rkind, 9.7_rkind, 9.45_rkind, 11.75_rkind, &
      10.2_rkind, 10.4_rkind, 11.0_rkind, 9.7_rkind],[elements%kolik, n_days])
      
      RHmax = reshape([84.0_rkind, 85.0_rkind, 76.0_rkind, 87.0_rkind, 92.0_rkind, &
      94.0_rkind, 97.0_rkind, 92.0_rkind, 93.0_rkind, 97.0_rkind], &
     [elements%kolik, n_days])
      
      RHmin = reshape([56.0_rkind, 64.0_rkind, 64.0_rkind, 77.0_rkind, 77.0_rkind, 76.0_rkind, &
      74.0_rkind, 59.0_rkind, 62.0_rkind, 61.0_rkind], [elements%kolik, n_days])
      
      soilcontent = reshape([0.05_rkind, 0.055_rkind, 0.062_rkind, 0.06_rkind, 0.04_rkind, 0.07_rkind, &
      0.09_rkind, 0.2_rkind, 0.25_rkind, 0.265_rkind], [elements%kolik, n_days])

    ! =====================================================
    ! === Hydrological constants ==========================
    ! =====================================================
    CN     = 98
    z      = 3.0_rkind
    Julian_day = 172
    phi    = 0.872_rkind
    as     = 0.25_rkind
    bs     = 0.05_rkind
    alpha  = 0.23_rkind
    sigma  = 4.903e-5_rkind
    gsc    = 0.0820_rkind
    ccrop  = 0.8_rkind

  end subroutine init

end module initvals
