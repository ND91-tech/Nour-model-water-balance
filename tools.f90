module tools

contains   
    function area_quad(a, b, c, d) result(area)
        use typy
        ! Computes the area of a quadrilateral given by points a, b, c, d (each dimension(2))
        ! Coordinates can be positive or negative and refer to any coordinate system.
        implicit none
        real(kind=rkind), intent(in) :: a(2), b(2), c(2), d(2)
        real(kind=rkind) :: area

        ! Local variables for area of triangles
        real(kind=rkind) :: area1, area2

        ! Shoelace formula for triangle area: |x1(y2-y3) + x2(y3-y1) + x3(y1-y2)| / 2
        area1 = 0.5_rkind * abs(a(1)*(b(2)-c(2)) + b(1)*(c(2)-a(2)) + c(1)*(a(2)-b(2)))
        area2 = 0.5_rkind * abs(a(1)*(c(2)-d(2)) + c(1)*(d(2)-a(2)) + d(1)*(a(2)-c(2)))

        area = area1 + area2
    end function area_quad
end module tools