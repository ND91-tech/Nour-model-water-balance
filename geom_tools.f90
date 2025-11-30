! geom_tools.f90
module geom_tools
  use typy
  use globals
  implicit none
contains

  ! -----------------------------------------------------
  ! Find neighbours: elements that share an edge
  ! -----------------------------------------------------
  subroutine find_neighbours(elements, nodes)
    type(elements_str), intent(inout) :: elements
    type(nodes_str),    intent(in)    :: nodes
    integer(kind=ikind) :: nel
    integer(kind=ikind) :: i, j, k, shared
    integer(kind=ikind) :: e1(3), e2(3)

    nel = elements%kolik
    if (nel <= 0) return

    if (.not. allocated(elements%neighbours)) then
       allocate(elements%neighbours(nel,3))
    end if
    elements%neighbours = 0_ikind

    do i = 1, nel
       e1 = elements%data(i,:)
       do j = 1, nel
          if (i == j) cycle
          e2 = elements%data(j,:)

          ! count shared nodes between element i and j
          shared = 0
          do k = 1, 3
             if (any(e2 == e1(k))) shared = shared + 1
          end do

          ! share at least 2 nodes => share an edge => neighbour
          if (shared >= 2) then
             ! put j into next free neighbour slot of i
             do k = 1, 3
                if (elements%neighbours(i,k) == 0) then
                   elements%neighbours(i,k) = j
                   exit
                end if
             end do
          end if
       end do
    end do
  end subroutine find_neighbours

end module geom_tools
