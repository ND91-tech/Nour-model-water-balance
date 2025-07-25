module tools
  use typy
  use globals
  implicit none
contains

  function penman_monteith(cell, hour) result(ETref)
    integer, intent(in) :: cell, hour
    real(kind=rkind) :: ETref
    real(kind=rkind) :: L1, L2, L3, L4, P, gamma, delta
    real(kind=rkind) :: satvapress1, satvapress2, epsilon, alpha_local, w1
    real(kind=rkind) :: TmaxK, TminK, Rl_val, Rn_val

    satvapress1 = 0.6108_rkind * exp((17.27_rkind * Tmax(cell,hour)) / (Tmax(cell,hour) + 237.3_rkind))
    satvapress2 = 0.6108_rkind * exp((17.27_rkind * Tmin(cell,hour)) / (Tmin(cell,hour) + 237.3_rkind))
    satvapress(cell,hour) = 0.5_rkind * (satvapress1 + satvapress2)
    actvapress(cell,hour) = (satvapress1 * RHmax(cell,hour) + satvapress2 * RHmin(cell,hour)) / 200.0_rkind
    delta = 4098.0_rkind * satvapress(cell,hour) / ((Tmean(cell,hour) + 237.3_rkind)**2)
    P = 101.3_rkind * ((293.0_rkind - 0.0065_rkind * z) / 293.0_rkind)**5.26_rkind
    gamma = 0.000665_rkind * P

    wind(cell,hour) = uz(cell,hour) * 4.87_rkind / log(67.8_rkind*z - 5.42_rkind)
    w1 = wind(cell,hour)

    if (w1 < 0.1_rkind) then
      alpha_local = 0.25_rkind
    else if (w1 > 0.25_rkind) then
      alpha_local = 0.1_rkind
    else
      alpha_local = 0.1_rkind + (0.25_rkind - w1)
    end if

    epsilon = 0.9_rkind + 0.18_rkind * w1
    TmaxK = Ta(cell,hour) + 273.16_rkind
    TminK = Ts(cell,hour) + 273.16_rkind

    Rl_val = sigma * TmaxK**4 * (0.605_rkind + 0.408_rkind * sqrt(Ha(cell,hour)))
    Rn_val = (1.0_rkind - alpha_local) * Rg(cell,hour) + Rl_val - epsilon * sigma * TminK**4

    Rl(cell,hour) = Rl_val
    Rn(cell,hour) = Rn_val

    L1 = 0.408_rkind * delta * (Rn(cell,hour) - G(cell,hour))
    L2 = gamma * (900.0_rkind / (Tmean(cell,hour) + 273.0_rkind)) * wind(cell,hour) * &
         (satvapress(cell,hour) - actvapress(cell,hour))
    L3 = L1 + L2
    L4 = delta + gamma * (1.0_rkind + 0.34_rkind * wind(cell,hour))

    ETref = L3 / L4
  end function penman_monteith

  function surface_runoff(cell, hour) result(Qsurf)
    integer, intent(in) :: cell, hour
    real(kind=rkind) :: Qsurf
    real(kind=rkind) :: S, I

    S = (25400.0_rkind / CN) - 254.0_rkind
    I = 0.2_rkind * S

    if (precip(cell,hour) <= I) then
      Qsurf = 0.0_rkind
    else
      Qsurf = (precip(cell,hour) - I)**2 / (precip(cell,hour) - I + S)
    end if
  end function surface_runoff


  function leakage(cell, hour) result(Lres)
    integer, intent(in) :: cell, hour
    real(kind=rkind) :: Lres
    Lres = max(0.0_rkind, qinter(cell,hour) + precip(cell,hour) - &
               ET_flux(cell,hour) - Qsurf_result(cell,hour))
  end function leakage

  function ground_water(cell, hour) result(Qgw)
    integer, intent(in) :: cell, hour
    real(kind=rkind) :: Qgw
    Qgw = conduct(cell,hour) * soilcontent(cell,hour)
  end function ground_water

  subroutine compute_rootflux()
    do cell = 1, N_cells
      do t = 1, n_hours
        ET_flux(cell,t)      = penman_monteith(cell,t) * ccrop
        Qsurf_result(cell,t) = surface_runoff(cell,t)
        L_result(cell,t)     = leakage(cell,t)
        Qgw_result(cell,t)   = ground_water(cell,t)
        deltas(cell,t) = precip(cell,t) + qinter(cell,t) - ET_flux(cell,t) - &
                         qout(cell,t) - Qsurf_result(cell,t) - L_result(cell,t) - Qgw_result(cell,t)
      end do
    end do
  end subroutine compute_rootflux
end module tools

