module core_tools
  public :: cut

  contains

     !> abreviation for adjustl(TRIM(ch))
  function cut(ch) result(out_ch)

    !> input character
    character(len=*), intent(in) :: ch
    !> output character without the leading free spaces and free spaces behind
    character(len=LEN(adjustl(TRIM(ch)))) :: out_ch

    out_ch=adjustl(TRIM(ch))

  end function cut


end module core_tools
