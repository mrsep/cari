! ivalarith.f90 - complete interval arithmetic as proposed by U. Kulisch in 2008
!
! Copyright (C) 2009 - 2011: Hans Peschke
!
! This file is part of cari - A Fortran module-library for Computer Arithmetic
!
! cari is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! cari is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with cari.  If not, see <http://www.gnu.org/licenses/>.

!> This module implements a complete interval arithmetic as proposed by U. Kulisch 
!! in 2008. 

! It is motivated by a complete and inclusion-isotonic handling of 
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

module ivalarith
use, intrinsic :: iso_c_binding
use ieee_arithmetic
use cari
use ieeearith
implicit none

! TODO - Problems:
! - How to handle the empty interval by all functions??? (inf, sup, mid, ...) 
! - FLAG_div_by_inner_zero --> raise IEEE-Exception div_by_zero
! - inflate, Kulisch IVALarith functions
! - Test of arithmetic with Infinity
! - not accurate I/O
! sqrt(ival) here???

! TODO - Implement
! order - handle the empty-interval
! use f2003 ROUND specifier for rounded read/write
! op** for positive integer, even/odd exponent
! ival**r, ival**int, ival**ival
! modular ordering: subset, value

private

public :: interval, ival, inf, sup, mid, get, put, operator(+), operator(-),    &
          operator(*), operator(/), sqrt, operator(.isect.), operator(.ihull.), &
          operator(.sb.), operator(.sp.), operator(.dj.), operator(.in.),       &
          operator(.int.), is_bounded, is_empty, operator(==), operator(/=),    &
          get_flag_div_by_inner_zero, reset_flag_div_by_inner_zero,             &
          assignment(=), inner_zero_split, ival_infinity, empty_ival, mag, diam,&
          radius, izero, ione, itwo, ihalf, neg_ival, operator(.dot.),          &
          operator(.idiv.), idiam, iradius, iinf, isup, mig, min, max, abs

!> error flag -> error handling
logical :: FLAG_div_by_inner_zero

!DEC$ OPTIONS /NOWARN
!> type for real fp-intervals
type, bind(c) :: interval
  private
  real(prec) :: inf, sup
end type
!DEC$ END OPTIONS

!> interval constants 
type(interval), parameter :: izero = interval(zero, zero)
type(interval), parameter :: ione  = interval(one, one)
type(interval), parameter :: itwo  = interval(two, two)
type(interval), parameter :: ihalf = interval(half, half)

!> dummy interval representing the empty set
type(interval), parameter :: empty_ival = interval(1.0_prec, -1.0_prec)

interface assignment (=)
  module procedure ival_assign_r
end interface

interface ival
  module procedure r_ival, i32_ival, i64_ival
end interface

interface inf
  module procedure inf_ival
end interface

interface sup
  module procedure sup_ival
end interface

interface iinf
  module procedure iinf_ival
end interface

interface isup
  module procedure isup_ival
end interface

interface get
  module procedure get_ival
end interface

interface put
  module procedure put_ival, put_ivals, put_ival_metric
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

interface operator (.idiv.)
  module procedure r_idiv_r
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

interface mid
  module procedure ival_mid
end interface

interface imid
  module procedure ival_imid
end interface

interface diam
  module procedure ival_diameter
end interface

interface idiam
  module procedure ival_idiameter
end interface

interface abs
  module procedure ival_abs
end interface abs

interface mag
  module procedure ival_magnitude
end interface

interface mig
  module procedure ival_mignitude
end interface

interface min
  module procedure ival_min
end interface

interface max
  module procedure ival_max
end interface

interface metric
  module procedure ival_metric
end interface

interface norm
  module procedure ival_norm
end interface

interface radius
  module procedure ival_radius
end interface

interface iradius
  module procedure ival_iradius
end interface

interface operator (<)
  module procedure ival_lesser_ival, ival_lesser_r, r_lesser_ival
end interface

interface operator (>)
  module procedure ival_greater_ival, ival_greater_r, r_greater_ival
end interface

interface operator (<=)
  module procedure ival_lessereq_ival, ival_lessereq_r, r_lessereq_ival
end interface

interface operator (>=)
  module procedure ival_greatereq_ival, ival_greatereq_r, r_greatereq_ival
end interface

interface operator (==)
  module procedure equal
end interface

interface operator (/=)
  module procedure notequal
end interface

interface operator (.dot.)
  module procedure ival_dot_ival
end interface

contains

  !> interval constructor for fp
  pure function r_ival(lower, upper) result(res)
    real(prec), intent(in)           :: lower
    real(prec), intent(in), optional :: upper
    type(interval)                   :: res

    res%inf = lower
    if (present(upper)) then
      res%sup = upper
      if (is_empty(res)) res = empty_ival
    else ! point interval
      res%sup = lower
    end if
  end function r_ival

  ! TODO fp-overflow???
  !> interval constructor for 32-Bit Integers
  pure function i32_ival(lower, upper) result(res)
    integer(i32), intent(in)           :: lower
    integer(i32), intent(in), optional :: upper
    type(interval)                     :: res

    res%inf = real(lower, prec)
    if (present(upper)) then
      res%sup = real(upper, prec)
      if (is_empty(res)) res = empty_ival
    else ! point interval
      res%sup = real(lower, prec)
    end if
  end function i32_ival

  ! TODO fp-overflow???
  !> interval constructor for 64-Bit Integers
  pure function i64_ival(lower, upper) result(res)
    integer(i64), intent(in)           :: lower
    integer(i64), intent(in), optional :: upper
    type(interval)                     :: res

    res%inf = real(lower, prec)
    if (present(upper)) then
      res%sup = real(upper, prec)
      if (is_empty(res)) res = empty_ival
    else ! point interval
      res%sup = real(lower, prec)
    end if
  end function i64_ival

  pure subroutine ival_assign_r(x, p)
    type(interval), intent(out) :: x
    real(prec), intent(in)      :: p
    
    x = ival(p)
  end subroutine ival_assign_r
  
  function get_flag_div_by_inner_zero() result(res)
    logical :: res
    res = FLAG_div_by_inner_zero
  end function get_flag_div_by_inner_zero

  subroutine reset_flag_div_by_inner_zero()
    FLAG_div_by_inner_zero = .false.
  end subroutine reset_flag_div_by_inner_zero
  
  pure function inf_ival(x) result(res)
    type(interval), intent(in) :: x
    real(prec)                 :: res

    if (.not. is_empty(x)) then 
      res = x%inf
    else
      res = ival_nan()
    end if
  end function inf_ival

  pure function sup_ival(x) result(res)
    type(interval), intent(in) :: x
    real(prec)                 :: res

    if (.not. is_empty(x)) then 
      res = x%sup
    else
      res = ival_nan()
    end if
  end function sup_ival

  pure function iinf_ival(x) result(res)
    type(interval), intent(in) :: x
    type(interval)             :: res

    res = ival(inf(x))
  end function iinf_ival

  pure function isup_ival(x) result(res)
    type(interval), intent(in) :: x
    type(interval)             :: res

    res = ival(sup(x))
  end function isup_ival
 
  ! special handling of inf-bounds or empty-interval not needed because
  ! inf(x) and sup(x) do the right job
  pure function ival_mid(x) result(res)
    type(interval), intent(in) :: x
    real(prec)                 :: res

    res = 0.5_prec * inf(x) + 0.5_prec * sup(x)
  end function ival_mid

  function ival_imid(x) result(res)
    type(interval), intent(in) :: x
    type(interval)             :: res

    res = ival(half .muld. (inf(x) .addd. x%sup), &
               half .mulu. (x%inf  .addu. x%sup))
  end function ival_imid

  pure function ival_abs(x) result(res)
    type(interval), intent(in) :: x
    type(interval)             :: res

    res = ival(mig(x), mag(x))
  end function ival_abs

  pure function ival_magnitude(x) result(res)
    type(interval), intent(in) :: x
    real(prec)                 :: res

    res = max(abs(inf(x)), abs(sup(x)))
  end function ival_magnitude

  pure function ival_mignitude(x) result(res)
    type(interval), intent(in) :: x
    real(prec)                 :: res
    
    if (zero .in. x) then
      res = zero
    else
      res = min(abs(inf(x)), abs(sup(x)))
    end if
  end function ival_mignitude

  pure function ival_max(x, y) result(res)
    type(interval), intent(in) :: x, y
    type(interval)             :: res

    res = ival(max(inf(x), inf(y)), max(x%sup, y%sup))
  end function ival_max

  pure function ival_min(x, y) result(res)
    type(interval), intent(in) :: x, y
    type(interval)             :: res

    res = ival(min(inf(x), inf(y)), min(x%sup, y%sup))
  end function ival_min

  function ival_radius(x) result(res)
    type(interval), intent(in) :: x
    real(prec)                 :: res

    res = half * abs(sup(x) .subu. x%inf)
  end function ival_radius

  function ival_iradius(x) result(res)
    type(interval), intent(in) :: x
    type(interval)             :: res

    res = half * ival(abs(sup(x) .subd. x%inf), abs(x%sup .subu. x%inf))
  end function ival_iradius
  
  function ival_diameter(x) result(res)
    type(interval), intent(in) :: x
    real(prec)                 :: res
  
    res = abs(sup(x) .subu. x%inf)
  end function ival_diameter
 
  function ival_idiameter(x) result(res)
    type(interval), intent(in) :: x
    type(interval)             :: res
  
    res = ival(abs(sup(x) .subd. x%inf), abs(x%sup .subu. x%inf))
  end function ival_idiameter

  ! possbile accuracy loss in '-'
  pure function ival_metric(x, y) result(res)
    type(interval), intent(in) :: x, y
    real(prec)                 :: res

    res = max(abs(inf(x) - inf(y)), abs(sup(x) - sup(y)))
  end function ival_metric

  pure function ival_norm(x) result(res)
    type(interval), intent(in) :: x
    real(prec)                 :: res

    res = ival_metric(x, izero)
  end function ival_norm

  pure function ival_is_empty(x) result(res)
    type(interval), intent(in) :: x
    logical                    :: res

    res = x%sup < x%inf
  end function ival_is_empty

  pure function ival_is_bounded(x) result(res)
    type(interval), intent(in) :: x
    logical                    :: res

    res = is_empty(x) .or. max(abs(x%inf), abs(x%sup)) < ival_infinity()
  end function ival_is_bounded

  ! [inf, inf] is not a point interval!
  pure function ival_is_point(x) result(res)
    type(interval), intent(in) :: x
    logical                    :: res
    res = is_bounded(x) .and. (x%inf == x%sup) .and. (.not. is_empty(x))
  end function ival_is_point
  
  ! interval infinity bound
  elemental function ival_infinity() result(res)
    real(prec) :: res
    res = ieee_value(0.0_prec, ieee_positive_inf) ! res = Infinity
  end function ival_infinity
 
  ! exceptional value for empty intervals
  elemental function ival_nan() result(res)
    real(prec) :: res
    res = ieee_value(0.0_prec, ieee_quiet_nan) ! res = NaN
  end function ival_nan

  ! TODO ERROR-SOURCE
  ! allow get of empty intervals?
  subroutine get_ival(x, pre, post, prompt)
    type(interval), intent(out) :: x
    logical, optional           :: prompt
    character(len=*), intent(in), optional :: pre, post
    real(prec)                  :: i, s

    if (present(pre)) write(*,fmt='(A)', advance='yes') pre
    if (present(prompt) .and. prompt) then
      write(*,fmt='(A)',advance='no') 'inf= '
      read(*,*) i
      write(*,fmt='(A)',advance='no') 'sup= '
      read(*,*) s
    else
      read(*,*) i, s ! ERROR-SOURCE
    end if
    x = ival(i, s)
    if (present(post)) write(*,fmt='(A)', advance='yes') post 
  end subroutine get_ival

  subroutine put_ival(x, pre, post, pdiam)
    type(interval), intent(in)             :: x
    character(len=*), intent(in), optional :: pre, post
    logical, intent(in), optional          :: pdiam
    character(len=prec2len(prec))          :: fm

    fm = prec2fmts(prec)
    if (present(pre)) write(*,fmt='(A)', advance='no') pre
    if (is_empty(x)) then
      write(*, fmt='(A)', advance='yes') '[-]'
    else if (is_point(x)) then
      write(*,fmt='("[ ",' // fm // '," ]")', advance='yes') x%inf
    else
      if (present(pdiam) .and. pdiam) then
        write(*,fmt='("[ ",'       // fm // &
                     ',", ",'      // fm // &
                     ',"; diam=",' // fm // &
                     '," ]")', advance='yes') x%inf, x%sup, diam(x)
      else
        write(*,fmt='("[ ",' // fm // &
                     '", "'  // fm // &
                     '" ]")', advance='yes') x%inf, x%sup
      end if
    end if
    if (present(post)) write(*,fmt='(A)', advance='yes') post 
  end subroutine put_ival

  subroutine put_ivals(x, pre, post, pdiam)
    type(interval), dimension(:), intent(in) :: x
    character(len=*), intent(in), optional   :: pre, post
    logical, intent(in), optional            :: pdiam
    integer :: i

    if (present(pre)) write(*,fmt='(A)', advance='no') pre
    do i=1, ubound(x,1)
      call put_ival(x(i), pdiam=pdiam)
    end do
    if (present(post)) write(*,fmt='(A)', advance='yes') post 
  end subroutine put_ivals

  subroutine put_ival_metric(x, y, xname, yname, pre, post)
    type(interval), intent(in)   :: x, y
    character(len=*), intent(in) :: xname, yname
    character(len=*), intent(in), optional :: pre, post
    character(len=prec2len(prec)) :: fm

    fm = prec2fmts(prec)
    if (present(pre)) write(*,fmt='(A)', advance='no') pre
    if (is_empty(x)) then
      write(*, fmt='(A)', advance='yes') '[-]'
    else if (is_point(x)) then
      write(*,fmt='("[ ",' // fm // ',"; d(",A,",",A,")= ",' // fm // ' ]")', &
              advance='yes') x%inf, trim(xname), trim(yname), ival_metric(x,y)       
    else
      write(*,fmt='("[ ",'   // fm // &
                    ',", ",' // fm // &
                    ',"; d(",A,",",A,")= ",' // fm // '," ]")', advance='yes') &
                    x%inf, x%sup, trim(xname), trim(yname), ival_metric(x,y) 
    end if
    if (present(post)) write(*,fmt='(A)', advance='yes') post 
  end subroutine put_ival_metric

  pure function pos_ival(x) result(res)
    type(interval), intent(in) :: x
    type(interval)             :: res
    res = x
  end function pos_ival
  
  pure function neg_ival(x) result(res)
    type(interval), intent(in) :: x
    type(interval)             :: res
    res = interval(-x%sup, -x%inf)
  end function neg_ival
 
  function r_add_ival(p, y) result(res)
    type(interval), intent(in) :: y
    real(prec), intent(in)     :: p
    type(interval)             :: res

    if (is_empty(y)) then
      res = empty_ival
    else
      res = ival(p .addd. y%inf, p .addu. y%sup)
    end if
  end function r_add_ival

  function ival_add_r(y, p) result(res)
    type(interval), intent(in) :: y
    real(prec), intent(in)     :: p
    type(interval)             :: res

    if (is_empty(y)) then
      res = empty_ival
    else
      res = ival(p .addd. y%inf, p .addu. y%sup)
    end if
  end function ival_add_r
  
  function ival_add_ival(x, y) result(res)
    type(interval), intent(in) :: x, y
    type(interval)             :: res

    if (is_empty(x) .or. is_empty(y)) then
      res = empty_ival
    else
      res = ival(x%inf .addd. y%inf, x%sup .addu. y%sup)
    end if
  end function ival_add_ival

  function r_sub_ival(p, y) result(res)
    type(interval), intent(in) :: y
    real(prec), intent(in)     :: p
    type(interval)             :: res

    if (is_empty(y)) then
      res = empty_ival
    else
      res = ival(p .subd. y%inf, p .subu. y%sup)
    end if
  end function r_sub_ival

  function ival_sub_r(y, p) result(res)
    type(interval), intent(in) :: y
    real(prec), intent(in)     :: p
    type(interval)             :: res

    if (is_empty(y)) then
      res = empty_ival
    else
      res = ival(y%inf .subd. p, y%sup .subu. p)
    end if
  end function ival_sub_r

  function ival_sub_ival(x, y) result(res)
    type(interval), intent(in) :: x, y
    type(interval)             :: res
    
    if (is_empty(x) .or. is_empty(y)) then
      res = empty_ival
    else
      res = ival(x%inf .subd. y%sup, x%sup .subu. y%inf)
    end if
  end function ival_sub_ival

  function r_mul_ival(p, y) result(res)
    type(interval), intent(in) :: y
    real(prec), intent(in)     :: p
    type(interval)             :: res
    real(prec)                 :: id, sd
    real(prec)                 :: iu, su

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
    type(interval), intent(in) :: y
    real(prec), intent(in)     :: p
    type(interval)             :: res
    real(prec)                 :: id, sd
    real(prec)                 :: iu, su

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
    type(interval), intent(in) :: x, y
    type(interval)             :: res
    real(prec)                 :: iid, isd, sid, ssd
    real(prec)                 :: iiu, isu, siu, ssu

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

  function r_idiv_r(x, y) result(res)
    real(prec), intent(in) :: x, y
    type(interval)         :: res

    res = ival(x .divd. y, x .divu. y)
  end function r_idiv_r

  function r_div_ival(p, y) result(res)
    type(interval), intent(in) :: y
    real(prec), intent(in)     :: p
    type(interval)             :: res

    res = ival(p) / y
  end function r_div_ival

  function ival_div_r(y, p) result(res)
    type(interval), intent(in) :: y
    real(prec), intent(in)     :: p
    type(interval)             :: res

    res = y / ival(p)
  end function ival_div_r

  ! TODO check it and try to understand it
  function ival_div_ival(x, y) result(res)
    type(interval), intent(in) :: x, y
    type(interval)             :: res
    real(prec)                 :: iid, isd, sid, ssd
    real(prec)                 :: iiu, isu, siu, ssu

    if (0.0_prec .int. y) then
      ! user should had better split the interval!!!
      res = ival(-ival_infinity(), ival_infinity())
      FLAG_div_by_inner_zero = .true.
    else if (0.0_prec .in. y) then
      ! prevent div-by-zero!!! 
      if (x%sup < 0.0_prec) then
        if (y == ival(0.0_prec)) then
          res = empty_ival
        else if (y%inf < y%sup .and. y%sup == 0.0_prec) then
          call ieee_set_rounding_mode(ieee_down)
          sid = x%sup / y%inf
          res = ival(sid, ival_infinity())
        else if (0.0_prec == y%inf .and. y%inf < y%sup) then
          call ieee_set_rounding_mode(ieee_up)
          ssu = x%sup / y%sup
          res = ival(-ival_infinity(), ssu)
        end if
      else if (0.0_prec .in. x) then
        res = ival(-ival_infinity(), ival_infinity())
      else if (x%sup > 0.0_prec) then
        if (y == ival(0.0_prec)) then
          res = empty_ival
        else if (y%inf < y%sup .and. y%sup == 0.0_prec) then
          call ieee_set_rounding_mode(ieee_up)
          iiu = x%inf / y%inf
          res = ival(-ival_infinity(), iiu)
        else if (0.0_prec == y%inf .and. y%inf < y%sup) then
          call ieee_set_rounding_mode(ieee_down)
          isd = x%inf / y%sup
          res = ival(isd, ival_infinity())
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
    type(interval), intent(in) :: x
    type(interval)             :: res

    if (is_empty(x)) then
      res = empty_ival
    else
      if (x%inf < 0.0_prec) then
        res = empty_ival
      end if
      call ieee_set_rounding_mode(ieee_down)
      res%inf = sqrt(x%inf)
      call ieee_set_rounding_mode(ieee_up)
      res%sup = sqrt(x%sup)
      call ieee_set_rounding_mode(ieee_nearest)
    end if
  end function sqrt_ival

  pure function ival_intersection_ival(x, y) result(res)
    type(interval), intent(in) :: x, y
    type(interval)             :: res

    if (is_empty(x) .or. is_empty(y)) then
      res = empty_ival
    else
      res = ival(max(x%inf, y%inf), min(x%sup, y%sup))
    end if
  end function ival_intersection_ival
  
  pure function r_ihull_r(x, y) result(res)
    real(prec), intent(in) :: x, y
    type(interval)         :: res

    if (x <= y) then
      res = ival(x, y)
    else
      res = ival(y, x)
    end if
  end function r_ihull_r

  pure function r_ihull_ival(p, y) result(res)
    real(prec), intent(in)     :: p
    type(interval), intent(in) :: y
    type(interval)             :: res

    if (is_empty(y)) then
      res = ival(p)
    else
      res = ival(min(p, y%inf), max(p, y%sup))
    end if
  end function r_ihull_ival

  pure function ival_ihull_r(y, p) result(res)
    type(interval), intent(in) :: y
    real(prec), intent(in)     :: p
    type(interval)             :: res

    if (is_empty(y)) then
      res = ival(p)
    else
      res = ival(min(p, y%inf), max(p, y%sup))
    end if
  end function ival_ihull_r

  pure function ival_ihull_ival(x, y) result(res)
    type(interval), intent(in) :: x, y
    type(interval)             :: res
    logical                    :: x_empty

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
    type(interval), intent(in) :: x, y
    logical                    :: res
    logical                    :: x_empty, y_empty

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
    type(interval), intent(in) :: x, y
    logical                    :: res
    logical                    :: x_empty, y_empty

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
    type(interval), intent(in) :: x, y
    logical                    :: res

    if (is_empty(x) .or. is_empty(y)) then
      res = .true.
    else
      res = x%inf > y%sup .or. x%sup < y%inf
    end if
  end function ival_disjoint_ival

  pure function r_in_ival(p, y) result(res)
    real(prec), intent(in)     :: p
    type(interval), intent(in) :: y
    logical                    :: res

    if (is_empty(y)) then
      res = .false.
    else
      res = p >= y%inf .and. p <= y%sup
    end if
  end function r_in_ival

  pure function r_interior_ival(p, y) result(res)
    real(prec), intent(in)     :: p
    type(interval), intent(in) :: y
    logical                    :: res

    if (is_empty(y)) then
      res = .false.
    else
      res = p > y%inf .and. p < y%sup
    end if
  end function r_interior_ival

  pure function ival_interior_ival(x, y) result(res)
    type(interval), intent(in) :: x, y
    logical                    :: res

    if (is_empty(x)) then
      res = .true.
    else if (is_empty(y)) then
      res = .false.
    else
      res = x%inf > y%inf .and. x%sup < y%sup
    end if
  end function ival_interior_ival

  !> helper function for extended interval division
  !! if zero is entirely in the ival, then break the 
  !! ival in two pieces 
  pure subroutine inner_zero_split(x, x1, x2)
    type(interval), intent(in)  :: x
    type(interval), intent(out) :: x1, x2

    if (zero .int. x) then
      x1 = ival(x%inf, zero)
      x2 = ival(zero, x%sup)
    else
      x1 = x
      x2 = empty_ival
    end if
  end subroutine inner_zero_split

  pure function equal(x, y) result(res)
    type(interval), intent(in) :: x, y
    logical                    :: res

    if (is_empty(x)) then
      res = is_empty(y)
    else
      res = x%inf == y%inf .and. x%sup == y%sup
    end if
  end function equal
  
  pure function notequal(x, y) result(res)
    type(interval), intent(in) :: x, y
    logical                    :: res

    res = .not. equal(x,y)
  end function notequal

  pure function ival_lesser_ival(l, r) result(res)
    type(interval), intent(in) :: l, r
    logical                    :: res
    
    if (is_empty(l) .and. is_empty(r)) then
      res = .true.
    else if (is_empty(l) .or. is_empty(r)) then
      res = .false.
    else
      res = l%sup < r%inf
    end if
  end function ival_lesser_ival

  pure function ival_lesser_r(l, r) result(res)
    type(interval), intent(in) :: l
    real(prec), intent(in)     :: r
    logical                    :: res

    res = l%sup < r
  end function ival_lesser_r

  pure function r_lesser_ival(l, r) result(res)
    type(interval), intent(in) :: r
    real(prec), intent(in)     :: l
    logical                    :: res

    res = l < r%inf
  end function r_lesser_ival

  pure function ival_greater_ival(l, r) result(res)
    type(interval), intent(in) :: l, r
    logical                    :: res

    res = l%inf > r%sup
  end function ival_greater_ival

  pure function ival_greater_r(l, r) result(res)
    type(interval), intent(in) :: l
    real(prec), intent(in)     :: r
    logical                    :: res

    res = l%inf > r
  end function ival_greater_r

  pure function r_greater_ival(l, r) result(res)
    type(interval), intent(in) :: r
    real(prec), intent(in)     :: l
    logical                    :: res

    res = l > r%sup
  end function r_greater_ival

  pure function ival_lessereq_ival(l, r) result(res)
    type(interval), intent(in) :: l, r
    logical                    :: res

    res = l%sup <= r%inf
  end function ival_lessereq_ival

  pure function ival_lessereq_r(l, r) result(res)
    type(interval), intent(in) :: l
    real(prec), intent(in)     :: r
    logical                    :: res

    res = l%sup <= r
  end function ival_lessereq_r

  pure function r_lessereq_ival(l, r) result(res)
    type(interval), intent(in) :: r
    real(prec), intent(in)     :: l
    logical                    :: res

    res = l <= r%inf
  end function r_lessereq_ival

  pure function ival_greatereq_ival(l, r) result(res)
    type(interval), intent(in) :: l, r
    logical                    :: res

    res = l%inf >= r%sup
  end function ival_greatereq_ival

  pure function ival_greatereq_r(l, r) result(res)
    type(interval), intent(in) :: l
    real(prec), intent(in)     :: r
    logical                    :: res

    res = l%inf >= r
  end function ival_greatereq_r

  pure function r_greatereq_ival(l, r) result(res)
    type(interval), intent(in) :: r
    real(prec), intent(in)     :: l
    logical                    :: res

    res = l >= r%sup
  end function r_greatereq_ival

  function ival_dot_ival(l, r) result(res)
    type(interval), intent(in), dimension(:) :: l, r
    type(interval)                           :: res
    integer :: i

    res = izero
    if (size(r, 1) /= size(l, 1)) then
      ! TODO error
    else
      do i=1, size(r, 1)
        res = res + l(i) * r(i)
      end do
    end if
  end function ival_dot_ival
end module ivalarith
