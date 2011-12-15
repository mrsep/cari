module numberformat
implicit none

type digit
  integer :: v
end type digit

character, parameter, dimension(0:35) :: digits &
  = (/ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', &
       'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', &
       'K', 'L', 'M', 'N', 'O', 'P', 'R', 'Q', 'S', 'T', &
       'U', 'V', 'W', 'X', 'Y', 'Z' /) 

!> type for number format
type nf
  !> base of the digits
  integer :: base
  !> quantity of bits for mantissa
  integer :: l
  !> quantity of bits for exponent
  integer :: e
  !> min and max exponent
  integer :: emin, emax
  !> existence of a hiddenbit
  logical :: hiddenbit
end type nf

type num
  logical :: sign
  type(digit), allocatable, dimension(:) :: mant
  type(digit), allocatable, dimension(:) :: expo
  type(nf), pointer :: numberformat
end type num

contains
  
  function declare(x) result(res)
    type(nf)  :: x
    type(num) :: res

    allocate(res%mant(0:x%l-1))
    allocate(res%expo(0:x%e-1))

    res%sign = .false.
    res%mant = digit(0)
    res%expo = 0
  end function
end module numberformat
