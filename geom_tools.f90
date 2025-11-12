
module geom_tools
  contains
  
    subroutine find_neighbours(el, nd)
      use typy
      use globals
      use printtools
      use core_tools
      use debug_tools

      type(elements_str), intent(in out) :: el
      type(nodes_str), intent(in) :: nd
      
      integer(kind=ikind) :: i,j,k,l,m,upward,downward,pos, n, o
      
      el%neighbours = 0_ikind
      

      !set neighbours
      do i=1, el%kolik
        pos = 0
        j = i
        k = i

        call progressbar(int(100*i/el%kolik))

        okoli: do 
          j = min(j+1, el%kolik+1)
          k = max(k-1, 0_ikind)

          if (j <= el%kolik) then
            upward = 0
            moje1: do l=1,ubound(el%data,2)
                nasel1: do m=1,ubound(el%data,2)
                  if (el%data(i,l) == el%data(j,m) .and. i/=j) then
                    upward = upward + 1
                    if (upward == drutes_config%dimen) then 
                pos = pos + 1
                el%neighbours(i,pos) = j
                EXIT nasel1
                    end if
                  end if
              end do nasel1
            end do moje1
          end if

          if (k > 0) then
            downward = 0
            moje2: do l=1,ubound(el%data,2)
              nasel2: do m=1,ubound(el%data,2)
                  if (el%data(i,l) == el%data(k,m) .and. i /= k) then
                    downward = downward + 1
                    if (downward == drutes_config%dimen) then 
                pos = pos + 1
                el%neighbours(i,pos) = k
                EXIT nasel2
                    end if
                  end if
              end do nasel2 
            end do moje2
          end if

          if (pos == ubound(el%data,2) .or. (j == el%kolik+1 .and. k == 0_ikind)) then
            EXIT okoli
          end if
        end do okoli
      end do
            


    end subroutine find_neighbours

end module geom_tools
