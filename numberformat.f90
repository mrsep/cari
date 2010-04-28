module numberformat
implicit none

type digit
  integer :: v
end type digit

type nf
  integer :: base
  integer :: l, e
  logical :: hiddenbit
end type nf

type num
  logical :: sign
  type(digit), allocatable, dimension(:) :: mant
  type(digit), allocatable, dimension(:) :: expo
end type num

contains
  
  function declare(x) result(res)
    type(nf)  :: x
    type(num) :: res

    allocate(res%mant(0:x%l-1))
    allocate(res%expo(0:x%e-1))

    res%sign = .false.
    res%mant = digit(0)
    res%expo = 
  end function
end module numberformat
