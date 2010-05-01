module ivalarith
use ieee_arithmetic
use cari
use ieeearith
implicit none
! This module implements a complete interval arithmetic as proposed by U. Kulisch 
! in 2008. It is motivated by a complete and inclusion-isotonic handling of 
! interval division [x]/[y] with 0 in [y]. This is not defined in normal interval 
! arithmetic. Without this restriction, the arithmetic has to deal with infinity 
! bounds and the empty interval. Another problem raises in the case if 
!             0 is an inner point of [y],                                 (A)
! because the result consists of two distinct intervals and therefore is not an 
! element of IR. There are several solutions of this problem:
! (1) The arguments and results of all operations are sets of intervals.
! (2) The case (A) is not allowed and the user has the option to split the
!     interval [yl, yu] in two dictinct intervals [yl, 0] and [0, yu] and
!     perform an extended interval division with infinity-bounds.
!
! U. Kulisch recommends solution (2) for an interval arithmetic standard, which
! is in work at IEEE.
! To prevent side effects and the use of exceptions, the result of a division in
! the presence of (A) is defined as [-Inf, +Inf] and a flag is raised. To represent 
! infinity bounds IEEE-Inf is used. We use ieee_arithmetic and its rounding modes to
! be as exact as possible.

! TODO - Problems:
! - How to handle the empty interval by all functions??? (inf, sup, mid, ...) 
! - FLAG_div_by_inner_zero --> raise IEEE-Exception div_by_zero
! - abs, inflate, Kulisch IVALarith functions
! - Test of arithmetic with Infinity
! - not accurate I/O

private

public :: extival, ival, inf, sup, mid, get, put, operator(+), operator(-),     &
          operator(*), operator(/), sqrt, operator(.isect.), operator(.ihull.), &
          operator(.sb.), operator(.sp.), operator(.dj.), operator(.in.),       &
          operator(.int.), is_bounded, is_empty, operator(==), operator(/=),    &
          get_flag_div_by_inner_zero, reset_flag_div_by_inner_zero,             &
          assignment(=), inner_zero_split, ival_inf, empty_ival, mag, radius,   &
          diam

logical :: FLAG_div_by_inner_zero
real(prec), parameter :: up = 1.0_prec, down = -1.0_prec

type extival
  private
  logical    :: empty
  real(prec) :: inf, sup
end type extival

type(extival), parameter :: empty_ival = extival(.true., 1.0_prec, -1.0_prec)

interface assignment (=)
  module procedure ival_assign_r
end interface

interface ival
  module procedure ival
end interface

interface inf
  module procedure inf_ival
end interface

interface sup
  module procedure sup_ival
end interface

interface mid
  module procedure mid_ival
end interface

interface get
  module procedure get_ival
end interface

interface put
  module procedure put_ival
end interface

interface operator (+)
  module procedure pos_ival, r_add_ival, ival_add_r, ival_add_ival
end interface

interface operator (-)
  module procedure neg_ival, r_sub_ival, ival_sub_r, ival_sub_ival
end interface

interface operator (*)
  module procedure r_mul_ival, ival_mul_r, ival_mul_ival
end interface

interface operator (/)
  module procedure r_div_ival, ival_div_r, ival_div_ival
end interface

interface sqrt
  module procedure sqrt_ival
end interface

interface operator (.isect.)
  module procedure ival_intersection_ival
end interface

interface operator (.ihull.)
  module procedure r_ihull_r, r_ihull_ival, ival_ihull_r, ival_ihull_ival
end interface

interface operator (.sb.)
  module procedure ival_subset_ival
end interface

interface operator (.sp.)
  module procedure ival_superset_ival
end interface

interface operator (.dj.)
  module procedure ival_disjoint_ival
end interface

interface operator (.in.)
  module procedure r_in_ival, ival_subset_ival
end interface

interface operator (.int.)
  module procedure r_interior_ival, ival_interior_ival
end interface

interface is_bounded
  module procedure ival_is_bounded
end interface

interface is_point
  module procedure ival_is_point
end interface

interface is_empty
  module procedure ival_is_empty
end interface

interface diam
  module procedure ival_diameter
end interface diam

interface mag
  module procedure ival_magnitude
end interface

interface radius
  module procedure ival_radius
end interface

interface operator (==)
  module procedure equal
end interface

interface operator (/=)
  module procedure notequal
end interface

contains

  pure function ival(lower, upper) result(res)
    real(prec), intent(in)           :: lower
    real(prec), intent(in), optional :: upper
    type(extival)                    :: res

    res%empty = .false.
    res%inf = lower
    if (present(upper)) then
      res%sup = upper
      if (is_empty(res)) res = empty_ival
    else ! point interval
      res%sup = lower
    end if
  end function ival

  pure subroutine ival_assign_r(x, p)
    type(extival), intent(out) :: x
    real(prec), intent(in)     :: p
    
    x = ival(p)
  end subroutine ival_assign_r
  
  function get_flag_div_by_inner_zero() result(res)
    logical :: res
    res = FLAG_div_by_inner_zero
  end function get_flag_div_by_inner_zero

  subroutine reset_flag_div_by_inner_zero()
    FLAG_div_by_inner_zero = .false.
  end subroutine reset_flag_div_by_inner_zero
  
  ! TODO x == empty ?? 
  pure function inf_ival(x) result(res)
    type(extival), intent(in) :: x
    real(prec)                :: res
    res = x%inf
  end function inf_ival

  ! TODO x == empty ?? 
  pure function sup_ival(x) result(res)
    type(extival), intent(in) :: x
    real(prec)                :: res
    res = x%sup
  end function sup_ival
 
  ! TODO x == empty ?? 
  pure function mid_ival(x) result(res)
    type(extival), intent(in) :: x
    real(prec)                :: res

    if (is_empty(x)) then
      res = ival_nan()
    else if (x%inf == -ival_inf()) then
      res = -ival_inf()
    else if (x%sup ==  ival_inf()) then
      res = ival_inf()
    else
      res = 0.5_prec * x%inf + 0.5_prec * x%sup
    end if
  end function mid_ival

  pure function ival_magnitude(x) result(res)
    type(extival), intent(in) :: x
    real(prec)                :: res
    res = max(abs(x%inf), abs(x%sup))
  end function ival_magnitude

  pure function ival_radius(x) result(res)
    type(extival), intent(in) :: x
    real(prec)                :: res
    res = 0.5_prec * abs(x%sup - x%inf)
  end function ival_radius
  
  pure function ival_diameter(x) result(res)
    type(extival), intent(in) :: x
    real(prec)                :: res
    res = abs(x%sup - x%inf)
  end function ival_diameter
  
  pure function ival_is_empty(x) result(res)
    type(extival), intent(in) :: x
    logical                   :: res
    res = x%empty .or. (x%sup < x%inf)
  end function ival_is_empty

  pure function ival_is_bounded(x) result(res)
    type(extival), intent(in) :: x
    logical                   :: res
    res = is_empty(x) .or. max(abs(x%inf), abs(x%sup)) < ival_inf()
  end function ival_is_bounded

  pure function ival_is_point(x) result(res)
    type(extival), intent(in) :: x
    logical                   :: res
    res = (x%inf == x%sup .and. .not. is_empty(x))
  end function ival_is_point
  
  ! interval infinity bound
  elemental function ival_inf() result(res)
    real(prec) :: res
    res = ieee_value(0.0_prec, ieee_positive_inf) ! res = Infinity
  end function ival_inf
 
  elemental function ival_nan() result(res)
    real(prec) :: res
    res = ieee_value(0.0_prec, ieee_quiet_nan) ! res = NaN
  end function ival_nan

  ! TODO - nearest??
  subroutine get_ival(x)
    type(extival), intent(out) :: x
    real(prec)                 :: i, s

    read(*,*) i, s ! ERROR-SOURCE
    x = ival(i, s)
  end subroutine get_ival

  subroutine put_ival(x, pre, post)
    type(extival), intent(in) :: x
    character(len=*), intent(in), optional :: pre, post

    if (present(pre)) write(*,fmt='(A)', advance='no') pre
    if (is_empty(x)) then
      write(*,*) '[-]'
    else if (is_point(x)) then
      write(*,fmt='("[ ",' // prec2fmts(prec) // '," ]")') x%inf
    else
      write(*,fmt='("[ ",' // prec2fmts(prec) // &
                   '", "'  // prec2fmts(prec) // &
                   '" ]")') x%inf, x%sup
    end if
    if (present(pre)) write(*,fmt='(A)', advance='no') post 
  end subroutine put_ival

  pure function pos_ival(x) result(res)
    type(extival), intent(in) :: x
    type(extival)             :: res
    res = x
  end function pos_ival
  
  pure function neg_ival(x) result(res)
    type(extival), intent(in) :: x
    type(extival)             :: res
    res = extival(x%empty, -x%sup, -x%inf)
  end function neg_ival
 
  function r_add_ival(p, y) result(res)
    type(extival), intent(in) :: y
    real(prec), intent(in)    :: p
    type(extival)             :: res
    real(prec)                :: id, su

    if (is_empty(y)) then
      res = empty_ival
    else
      id = p .addd. y%inf

      su = p .addu. y%sup
      
      res = ival(id, su)
    end if
  end function r_add_ival

  function ival_add_r(y, p) result(res)
    type(extival), intent(in) :: y
    real(prec), intent(in)    :: p
    type(extival)             :: res

    if (is_empty(y)) then
      res = empty_ival
    else
      res = ival(p .addd. y%inf, p .addu. y%sup)
    end if
  end function ival_add_r
  
  function ival_add_ival(x, y) result(res)
    type(extival), intent(in) :: x, y
    type(extival)             :: res

    if (is_empty(x) .or. is_empty(y)) then
      res = empty_ival
    else
      res = ival(x%inf .addd. y%inf, x%sup .addu. y%sup)
    end if
  end function ival_add_ival

  function r_sub_ival(p, y) result(res)
    type(extival), intent(in) :: y
    real(prec), intent(in)    :: p
    type(extival)             :: res

    if (is_empty(y)) then
      res = empty_ival
    else
      res = ival(p .subd. y%inf, p .subu. y%sup)
    end if
  end function r_sub_ival

  function ival_sub_r(y, p) result(res)
    type(extival), intent(in) :: y
    real(prec), intent(in)    :: p
    type(extival)             :: res

    if (is_empty(y)) then
      res = empty_ival
    else
      res = ival(y%inf .subd. p, y%sup .subu. p)
    end if
  end function ival_sub_r

  function ival_sub_ival(x, y) result(res)
    type(extival), intent(in) :: x, y
    type(extival)             :: res
    
    if (is_empty(x) .or. is_empty(y)) then
      res = empty_ival
    else
      res = ival(x%inf .subd. y%sup, x%sup .subu. y%inf)
    end if
  end function ival_sub_ival

  function r_mul_ival(p, y) result(res)
    type(extival), intent(in) :: y
    real(prec), intent(in)    :: p
    type(extival)             :: res
    real(prec)                :: id, sd
    real(prec)                :: iu, su

    if (is_empty(y)) then
      res = empty_ival
    else
      call ieee_set_rounding_mode(ieee_down)
      id = p * y%inf; sd = p * y%sup
      call ieee_set_rounding_mode(ieee_up)
      iu = p * y%inf; su = p * y%sup
      
      res = ival(min(id, sd), max(iu, su))
      call ieee_set_rounding_mode(ieee_nearest)
    end if
  end function r_mul_ival

  function ival_mul_r(y, p) result(res)
    type(extival), intent(in) :: y
    real(prec), intent(in)    :: p
    type(extival)             :: res
    real(prec)                :: id, sd
    real(prec)                :: iu, su

    if (is_empty(y)) then
      res = empty_ival
    else
      call ieee_set_rounding_mode(ieee_down)
      id = p * y%inf; sd = p * y%sup
      call ieee_set_rounding_mode(ieee_up)
      iu = p * y%inf; su = p * y%sup
      
      res = ival(min(id, sd), max(iu, su))
      call ieee_set_rounding_mode(ieee_nearest)
    end if
  end function ival_mul_r

  function ival_mul_ival(x, y) result(res)
    type(extival), intent(in) :: x, y
    type(extival)             :: res
    real(prec)                :: iid, isd, sid, ssd
    real(prec)                :: iiu, isu, siu, ssu

    if (is_empty(x) .or. is_empty(y)) then
      res = empty_ival
    else
      call ieee_set_rounding_mode(ieee_down)
      iid = x%inf * y%inf; isd = x%inf * y%sup
      sid = x%sup * y%inf; ssd = x%sup * y%sup
      call ieee_set_rounding_mode(ieee_up)
      iiu = x%inf * y%inf; isu = x%inf * y%sup
      siu = x%sup * y%inf; ssu = x%sup * y%sup
      
      res = ival(min(iid, isd, sid, ssd), &
                 max(iiu, isu, siu, ssu))

      call ieee_set_rounding_mode(ieee_nearest)
    end if
  end function ival_mul_ival

  function r_div_ival(p, y) result(res)
    type(extival), intent(in) :: y
    real(prec), intent(in)    :: p
    type(extival)             :: res

    res = ival(p) / y
  end function r_div_ival

  function ival_div_r(y, p) result(res)
    type(extival), intent(in) :: y
    real(prec), intent(in)    :: p
    type(extival)             :: res

    res = y / ival(p)
  end function ival_div_r

  function ival_div_ival(x, y) result(res)
    type(extival), intent(in) :: x, y
    type(extival)             :: res
    real(prec)                :: iid, isd, sid, ssd
    real(prec)                :: iiu, isu, siu, ssu

    if (0.0_prec .int. y) then
      ! user should had better split the interval!!!
      res = ival(-ival_inf(), ival_inf())
      FLAG_div_by_inner_zero = .true.
    else if (0.0_prec .in. y) then
      ! prevent div-by-zero!!! 
      if (x%sup < 0.0_prec) then
        if (y == ival(0.0_prec)) then
          res = empty_ival
        else if (y%inf < y%sup .and. y%sup == 0.0_prec) then
          call ieee_set_rounding_mode(ieee_down)
          sid = x%sup / y%inf
          res = ival(sid, ival_inf())
        else if (0.0_prec == y%inf .and. y%inf < y%sup) then
          call ieee_set_rounding_mode(ieee_up)
          ssu = x%sup / y%sup
          res = ival(-ival_inf(), ssu)
        end if
      else if (0.0_prec .in. x) then
        res = ival(-ival_inf(), ival_inf())
      else if (x%sup > 0.0_prec) then
        if (y == ival(0.0_prec)) then
          res = empty_ival
        else if (y%inf < y%sup .and. y%sup == 0.0_prec) then
          call ieee_set_rounding_mode(ieee_up)
          iiu = x%inf / y%inf
          res = ival(-ival_inf(), iiu)
        else if (0.0_prec == y%inf .and. y%inf < y%sup) then
          call ieee_set_rounding_mode(ieee_down)
          isd = x%inf / y%sup
          res = ival(isd, ival_inf())
        end if
      end if
    else  ! 0 not in y
      call ieee_set_rounding_mode(ieee_down)
      iid = x%inf / y%inf; isd = x%inf / y%sup
      sid = x%sup / y%inf; ssd = x%sup / y%sup
      call ieee_set_rounding_mode(ieee_up)
      iiu = x%inf / y%inf; isu = x%inf / y%sup
      siu = x%sup / y%inf; ssu = x%sup / y%sup
      
      res = ival(min(iid, isd, sid, ssd), &
                 max(iiu, isu, siu, ssu))
    end if
    call ieee_set_rounding_mode(ieee_nearest)
  end function ival_div_ival

  ! TODO sqrt of interval with negative numbers ???
  function sqrt_ival(x) result(res)
    type(extival), intent(in) :: x
    type(extival)             :: res

    if (is_empty(x)) then
      res = empty_ival
    else
      if (x%inf < 0.0_prec) then
        res = empty_ival
      end if
      res%empty = .false.
      call ieee_set_rounding_mode(ieee_down)
      res%inf = sqrt(x%inf)
      call ieee_set_rounding_mode(ieee_up)
      res%sup = sqrt(x%sup)
      call ieee_set_rounding_mode(ieee_nearest)
    end if
  end function sqrt_ival

  pure function ival_intersection_ival(x, y) result(res)
    type(extival), intent(in) :: x, y
    type(extival)             :: res

    if (is_empty(x) .or. is_empty(y)) then
      res = empty_ival
    else
      res = ival(max(x%inf, y%inf), min(x%sup, y%sup))
    end if
  end function ival_intersection_ival
  
  pure function r_ihull_r(x, y) result(res)
    real(prec), intent(in)    :: x, y
    type(extival)             :: res

    if (x <= y) then
      res = ival(x, y)
    else
      res = ival(y, x)
    end if
  end function r_ihull_r

  pure function r_ihull_ival(p, y) result(res)
    real(prec), intent(in)    :: p
    type(extival), intent(in) :: y
    type(extival)             :: res

    if (is_empty(y)) then
      res = ival(p)
    else
      res = ival(min(p, y%inf), max(p, y%sup))
    end if
  end function r_ihull_ival

  pure function ival_ihull_r(y, p) result(res)
    type(extival), intent(in) :: y
    real(prec), intent(in)    :: p
    type(extival)             :: res

    if (is_empty(y)) then
      res = ival(p)
    else
      res = ival(min(p, y%inf), max(p, y%sup))
    end if
  end function ival_ihull_r

  pure function ival_ihull_ival(x, y) result(res)
    type(extival), intent(in) :: x, y
    type(extival)             :: res
    logical                   :: x_empty

    x_empty = is_empty(x)
    if (x_empty .or. is_empty(y)) then
      if (x_empty) then
        res = y
      else
        res = x
      end if
    else
      res = ival(min(x%inf, y%inf), max(x%sup, y%sup))
    end if
  end function ival_ihull_ival
 
  pure function ival_subset_ival(x, y) result(res)
    type(extival), intent(in) :: x, y
    logical                   :: res
    logical                   :: x_empty, y_empty

    x_empty = is_empty(x); y_empty = is_empty(y)
    if (x_empty .or. is_empty(y)) then
      if (x_empty) then
        res = .true.
      else
        res = y_empty
      end if
    else
      res = (x%inf >= y%inf) .and. (x%sup <= y%sup)
    end if
  end function ival_subset_ival

  pure function ival_superset_ival(x, y) result(res)
    type(extival), intent(in) :: x, y
    logical                   :: res
    logical                   :: x_empty, y_empty

    x_empty = is_empty(x); y_empty = is_empty(y)
    if (x_empty .or. is_empty(y)) then
      if (y_empty) then
        res = .true.
      else
        res = x_empty
      end if
    else
      res = (x%inf <= y%inf) .and. (x%sup >= y%sup)
    end if
  end function ival_superset_ival

  pure function ival_disjoint_ival(x, y) result(res)
    type(extival), intent(in) :: x, y
    logical                   :: res

    if (is_empty(x) .or. is_empty(y)) then
      res = .true.
    else
      res = x%inf > y%sup .or. x%sup < y%inf
    end if
  end function ival_disjoint_ival

  pure function r_in_ival(p, y) result(res)
    real(prec), intent(in)    :: p
    type(extival), intent(in) :: y
    logical                   :: res

    if (is_empty(y)) then
      res = .false.
    else
      res = p >= y%inf .and. p <= y%sup
    end if
  end function r_in_ival

  pure function r_interior_ival(p, y) result(res)
    real(prec), intent(in)    :: p
    type(extival), intent(in) :: y
    logical                   :: res

    if (is_empty(y)) then
      res = .false.
    else
      res = p > y%inf .and. p < y%sup
    end if
  end function r_interior_ival

  pure function ival_interior_ival(x, y) result(res)
    type(extival), intent(in) :: x, y
    logical                   :: res

    if (is_empty(x)) then
      res = .true.
    else if (is_empty(y)) then
      res = .false.
    else
      res = x%inf > y%inf .and. x%sup < y%sup
    end if
  end function ival_interior_ival

  subroutine inner_zero_split(x, x1, x2)
    type(extival), intent(in)  :: x
    type(extival), intent(out) :: x1, x2

    if (0.0_prec .int. x) then
      x1 = extival(.false., x%inf, 0.0_prec)
      x2 = extival(.false., 0.0_prec, x%sup)
    else
      x1 = x
      x2 = empty_ival
    end if
  end subroutine inner_zero_split

  function equal(x, y) result(res)
    type(extival), intent(in) :: x, y
    logical                   :: res

    if (is_empty(x)) then
      res = is_empty(y)
    else
      res = x%inf == y%inf .and. x%sup == y%sup
    end if
  end function equal
  
  function notequal(x, y) result(res)
    type(extival), intent(in) :: x, y
    logical                   :: res

    res = .not. equal(x,y)
  end function notequal
end module ivalarith
