!> provides procedures to visualize fp# as their binary representation in memory
!!
!> \author Hans Peschke
!> \date 2009 - 2010

module viscari
use cari
implicit none

private

public :: bin_print, get_bits, set_bit, bin_sfmt, bin_dfmt, bin_efmt

character(len=1), parameter :: true  = 'x'  !> character for logical true
character(len=1), parameter :: false = '.'  !> character for logical false

!> format strings for binary output (h ... hidden bit)
character(len=22), parameter :: bin_sfmt = '(1A," ", 8A," h", 23A)'
character(len=22), parameter :: bin_dfmt = '(1A," ",11A," h", 52A)'
character(len=22), parameter :: bin_efmt = '(1A," ",15A," h",112A)'
character(len=22), parameter :: bin_qfmt = ''


!interface print_mant
!  module procedure print_mant_s, print_mant_d, print_mant_e
!end interface

interface bin_print
  module procedure bin_print_s, bin_print_d, bin_print_e
end interface

interface get_bits
  module procedure get_bits_s, get_bits_d, get_bits_e
end interface

interface set_bit
  module procedure set_bit_s, set_bit_d, set_bit_e
end interface

contains
  subroutine print_mant_d(x)
    real(dp), intent(in) :: x

    write(*,fmt=bin_dfmt) lg_subst_chr(get_bits(x), true, false)
  end subroutine print_mant_d

  subroutine bin_print_s(x)
    real(sp), intent(in) :: x

    write(*,fmt=bin_sfmt) lg_subst_chr(get_bits(x), true, false)
  end subroutine bin_print_s

  subroutine bin_print_d(x, biased)
    real(dp), intent(in) :: x
    logical, intent(in), optional :: biased

    write(*,fmt=bin_dfmt) lg_subst_chr(get_bits(x, biased), true, false)
  end subroutine bin_print_d
  
  subroutine bin_print_e(x)
    real(ep), intent(in) :: x

    write(*,fmt=bin_efmt) lg_subst_chr(get_bits(x), true, false)
  end subroutine bin_print_e

  !> converts an logical array to string representing
  ! write(*,fmt='(1A," ",8A," ",23A)') lg_subst_chr(r_get_bit(x), 'x', '_')
  pure function lg_subst_chr(lg, tch, fch) result(res)
    logical, intent(in), dimension(:) :: lg
    character(len=1), intent(in)      :: fch, tch
    character, dimension(size(lg, 1)) :: res, tmp

    res = fch ! set all elements of the out-array to FALSE
    tmp = tch ! set all elements of tmp to TRUE
    res = unpack(tmp, lg, res) ! set all elements of the out-array 
                               ! to tmp=tch if masked by lg
  end function lg_subst_chr

  ! only return the len least significand bits if len < bit_size(x)
  ! len >= bit_size(x)
  pure function int2bin(x, len) result(res)
    integer, intent(in)                :: x
    integer, intent(in), optional      :: len
    logical, allocatable, dimension(:) :: res
    ! local
    integer :: bs
    integer :: n, i

    bs = bit_size(x)
    
    if (present(len)) then
      n = len-1
    else 
      n = bs-1
    end if

    allocate(res(0:n))

    ! preset the msb of res, that are not affected by x
    if (n >= bs) res = (x < 0)

    ! dont run out of x, take all significands for result
    do i=0, min(n, bs-1)
      res(n-i) = btest(x, i)
    end do

    ! preserve sign if truncate, but may loss of almost all msb
    if (n < bs .and. x < 0) then
      res(0) = .true.
    end if
  end function int2bin

  ! only return the len least significand bits but preserve sign-bit
  pure function int2binhuman(x, len) result(res)
    integer, intent(in)                :: x
    integer, intent(in), optional      :: len
    logical, allocatable, dimension(:) :: res
    ! local
    integer :: n, bs
    
    bs = bit_size(x)

    if (present(len)) then
      n = len-1
    else
      n = bs-1
    end if

    allocate(res(0:n))
    
    res = .false.
    
    ! preseve sign bit
    if (x < 0) then
      if (n >= bs) then
        res(1+n-bs:n) = int2bin(2**(bs-1) - x)
        res(0:n-bs) = .false. ! this are the bits which are not affected by x
        res(1+n-bs) = .false. ! set the old signbit to .false.
      else
        res = int2bin(2**(bs-1) - x, n+1)
      end if
      res(0) = .true.  ! set the signbit to .true. "res(0) = x<0"
    else ! (x >= 0)
      res = int2bin(x, n+1)
      res(0) = .false. ! set the signbit to .false.
    end if
  end function int2binhuman

  function get_bits_s(x, biased, mirrored) result(res)
    real(sp), intent(in)          :: x
    logical, intent(in), optional :: biased, mirrored
    logical, dimension(0:31)      :: res
    ! local
    real(sp)     :: xtmp
    integer(i32) :: eqv
    integer      :: bs, i

    equivalence(xtmp, eqv)
    xtmp = x
    bs   = bit_size(eqv) - 1

    if (present(mirrored) .and. mirrored) then
      do i=0, bs
        res(i) = btest(eqv, i)
      end do
    else
      do i=0, bs
        res(bs-i) = btest(eqv, i)
      end do
    end if

    if (present(biased) .and. biased) then
      ! do nothing
    else
      res(1:expbits(x)) = int2binhuman(exponent(x), expbits(x))
    end if
  end function get_bits_s

  ! DANGER: dont use mirrored and biased = .not.
  function get_bits_d(x, biased, mirrored) result(res)
    real(dp), intent(in)          :: x
    logical, intent(in), optional :: biased, mirrored
    logical, dimension(0:63)      :: res
    ! local
    real(dp)     :: xtmp
    integer(i64) :: eqv
    integer      :: bs, i

    equivalence(xtmp, eqv)
    xtmp = x
    bs   = bit_size(eqv) - 1

    if (present(mirrored) .and. mirrored) then
      do i=0, bs
        res(i) = btest(eqv, i)
      end do
    else
      do i=0, bs
        res(bs-i) = btest(eqv, i)
      end do
    end if

    if (present(biased) .and. biased) then
      ! do nothing
    else
      res(1:expbits(x)) = int2binhuman(exponent(x), expbits(x))
    end if
  end function get_bits_d

  function get_bits_e(x, biased, mirrored) result(res)
    real(ep), intent(in)          :: x
    logical, intent(in), optional :: biased, mirrored
    logical, dimension(0:127)     :: res
    ! local
    real(ep)     :: xtmp
    integer(i64) :: eqv(2)
    integer      :: bs, i

    equivalence(xtmp, eqv)
    xtmp = x
    bs   = 63

    if (present(mirrored) .and. mirrored) then
      do i=0, bs
        res(i)    = btest(eqv(2), i)
        res(64+i) = btest(eqv(1), 64+i)
      end do
    else
      do i=0, bs
        res( bs-i) = btest(eqv(2), i)
        res(127-i) = btest(eqv(1), i)
      end do
    end if

    if (present(biased) .and. biased) then
      ! do nothing
    else
      res(1:expbits(x)) = int2binhuman(exponent(x), expbits(x))
    end if
  end function get_bits_e

  elemental function set_bit_s(x, b, n) result(res)
    real(sp), intent(in) :: x
    logical, intent(in)  :: b
    integer, intent(in)  :: n
    real(sp)             :: res
    ! local
    integer(i32) :: eqv
    real(sp)     :: tmp

    equivalence(tmp, eqv)
    tmp = x
    if (b) then
      eqv = ibset(eqv, 31-n)
    else
      eqv = ibclr(eqv, 31-n)
    end if
    res = tmp
  end function set_bit_s

  elemental function set_bit_d(x, b, n) result(res)
    real(dp), intent(in) :: x
    logical, intent(in)  :: b
    integer, intent(in)  :: n
    real(dp)             :: res
    ! local
    integer(i64) :: eqv
    real(dp)     :: tmp

    equivalence(tmp, eqv)
    tmp = x
    if (b) then
      eqv = ibset(eqv, 63-n)
    else
      eqv = ibclr(eqv, 63-n)
    end if
    res = tmp
  end function set_bit_d

  ! n in [0, 127]
  ! eqv(1): n in [0, 63]
  ! eqv(2): n in [64, 127]
  elemental function set_bit_e(x, b, n) result(res)
    real(ep), intent(in) :: x
    logical, intent(in)  :: b
    integer, intent(in)  :: n
    real(ep)             :: res
    ! local
    integer(i64) :: eqv(2)
    real(ep)     :: tmp

    equivalence(tmp, eqv)
    tmp = x
    if (b) then
      eqv(2-n/64) = ibset(eqv(2-n/64), 63-mod(n, 64))
    else
      eqv(2-n/64) = ibclr(eqv(2-n/64), 63-mod(n, 64))
    end if
    res = tmp
  end function set_bit_e

  ! plot x in fixed point look
  subroutine fixpoint_plot(x, names)
    real(sp), dimension(:) :: x
    integer :: i, n, bits
    !character(len=precision(x)-2), dimension(:) :: names
    character(len=1), dimension(:) :: names
    character(len=1), dimension(size(x, 1), -maxexponent(x) : minexponent(x) + precision(x)) :: label

    bits = maxexponent(x) - minexponent(x) + precision(x)
    n = size(x, 1)
    do i=1, n
      ! start-label
      label(i,-exponent(x(i))) = '<'

      ! name
      !label(i,-exponent(x(i))+1 : -exponent(x(i)) + precision(x(i)) - 1) = names(i)

      ! end-lanel
      label(i,-exponent(x(i)) + precision(x(i))) = '>'
    end do

    do i=1, n
      write(*,*) label(i,:)
!      write(*,*) x(i), names(i)
    end do
  end subroutine

end module viscari
