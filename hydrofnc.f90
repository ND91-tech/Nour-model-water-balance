module hydrofnc
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

end module hydrofnc

module hydrofnc
  use typy
  use globals
  implicit none
  contains

  pure real(rkind) function esat(T)
      real(rkind), intent(in) :: T
      esat = 0.6108_rkind*exp(17.27_rkind*T/(T+237.3_rkind))
   end function esat


   pure real(rkind) function slope_vp(T)
      real(rkind), intent(in) :: T
      slope_vp = 4098._rkind*esat(T)/((T+237.3_rkind)**2)
   end function slope_vp


   pure real(rkind) function psychro_const(z_elev)
      real(rkind), intent(in) :: z_elev
      real(rkind) :: P
      P = 101.3_rkind*((293._rkind-0.0065_rkind*z_elev)/293._rkind)**5.26_rkind
      psychro_const = 0.000665_rkind*P
   end function psychro_const


    pure real(rkind) function wind2m(uz_meas,z_meas)
       real(rkind), intent(in)::uz_meas,z_meas
       wind2m = uz_meas * 4.87_rkind / log(67.8_rkind * z_meas / 10.0_rkind)
    end function wind2m



   pure real(rkind) function Ra_daily(phi,J)
      real(rkind), intent(in)::phi
      integer, intent(in)::J
      real(rkind)::dr,delta,ws
      dr    = 1._rkind+0.033_rkind*cos(2._rkind*phi*J/365._rkind)
      delta = 0.409_rkind*sin(2._rkind*phi*J/365._rkind-1.39_rkind)
      ws    = acos(-tan(phi)*tan(delta))
      Ra_daily=(24._rkind*60._rkind/phi)*gsc*dr*(ws*sin(phi)*sin(delta)+cos(phi)*cos(delta)*sin(ws))
   end function Ra_daily


   pure real(rkind) function Rnl_daily(TmaxK,TminK,ea,Rs,Rso)
      real(rkind), intent(in)::TmaxK,TminK,ea,Rs,Rso
      real(rkind)::tmp
      tmp = 0.5_rkind*(TmaxK**4+TminK**4)
      Rnl_daily = sigma*tmp*(0.34_rkind-0.14_rkind*sqrt(ea))* &
                  (1.35_rkind*min(Rs/Rso,1._rkind)-0.35_rkind)
   end function Rnl_daily


   pure real(rkind) function penman_monteith(cell,day) result(ET0)
      integer,intent(in)::cell,day
      real(rkind)::es_Tmax,es_Tmin,es,ea,delta,gamma,u2
      real(rkind)::Ra,Rs,Rso,Rns,Rnl,Rn,nN
      real(rkind)::G0,L1,L2,L3,L4
      es_Tmax = esat(Tmax(cell,day))
      es_Tmin = esat(Tmin(cell,day))
      es      = 0.5_rkind*(es_Tmax+es_Tmin)
      ea      = (es_Tmin*RHmax(cell,day)+es_Tmax*RHmin(cell,day))/200._rkind
      delta   = slope_vp(Tmean(cell,day))
      gamma   = psychro_const(z)
      u2      = wind2m(uz(cell,day),z)
      Ra      = Ra_daily(phi,J)
      nN      = 1._rkind
      Rs      = (as+bs*nN)*Ra
      Rso     = (0.75_rkind+2.e-5_rkind*z)*Ra
      Rns     = (1._rkind-alpha)*Rs
      Rnl     = Rnl_daily(Tmax(cell,day)+273.16_rkind, &
                          Tmin(cell,day)+273.16_rkind, ea, Rs, Rso)
      Rn = max(Rns - Rnl, 0.0_rkind)                                 !changed
      G0      = G(cell,day)
      L1 = 0.408_rkind*delta*(Rn-G0)
      L2 = gamma*(900._rkind/(Tmean(cell,day)+273._rkind))*u2*(es-ea)
      L3 = L1+L2
      L4 = delta+gamma*(1._rkind+0.34_rkind*u2)
      ET0 = max(L3 / L4, 0.0_rkind)                      !changed

   end function penman_monteith


   pure real(rkind) function surface_runoff(cell,day)
      integer,intent(in)::cell,day
      real(rkind)::S,I
      S=(25400._rkind/CN)-254._rkind
      I=0.2_rkind*S
      if(precip(cell,day)<=I)then
         surface_runoff=0._rkind
      else
         surface_runoff=(precip(cell,day)-I)**2/(precip(cell,day)-I+S)
      end if
   end function surface_runoff


   pure real(rkind) function ground_water(cell,day)
      integer,intent(in)::cell,day
      ground_water=conduct(cell,day)*soilcontent(cell,day)
   end function ground_water


   pure real(rkind) function leakage(cell,day)
      integer,intent(in)::cell,day
      leakage=max(0._rkind,qinter(cell,day)+precip(cell,day)- &
                 ET_flux(cell,day)-Qsurf_result(cell,day))
   end function leakage


   subroutine compute_all()

      do cell=1,N_cells
         do t=1,n_days
            ET_flux(cell,t)      = penman_monteith(cell,t)*ccrop
            Qsurf_result(cell,t) = surface_runoff(cell,t)
            L_result(cell,t)     = leakage(cell,t)
            Qgw_result(cell,t)   = ground_water(cell,t)
            deltas(cell,t)=precip(cell,t)+qinter(cell,t)-ET_flux(cell,t)-qout(cell,t)- &
                           Qsurf_result(cell,t)-L_result(cell,t)-Qgw_result(cell,t)
         end do
      end do
   end subroutine compute_all

end module hydrofnc

