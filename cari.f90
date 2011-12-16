! cari.f90 - basic functions and constants for computer arithmetic
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

!> module with basic functions and constants for computer arithmetic
!!
!> \author Hans Peschke
!> \date 2009 - 2010

module cari
implicit none

!> TODO
! - output of -min_denorm, extremal normalized numbers in each format
! - the plan is to implement these functions also for general
! 	numberformats R := R(b, l, emin, emax) and interface them, independent of
! 	the available formats on the machine
!   -> fp# are element of instance of general numberformat type
!   -> some constants should be part of numberformat type, or inq functions
!      of them, or better safe the api and serve new module for general
!      numberformats??
! - arith expression parser???
! - verified I/O: 
!   - get with control of the rounding mode --> get of interval enclosures of
!     not representable real numbers, e.g. 0.1

! everything is public, but it's better to only import stuff really needed
public

!> real kind parameters
integer, parameter :: sp = kind(0.0)
integer, parameter :: dp = kind(0.0D0)
integer, parameter :: ep = selected_real_kind(18, 1000)
integer, parameter :: qp = kind(0.0Q0)

!> integer kind parameters
integer, parameter :: i8  = selected_int_kind(int(log10(2.0**8)))
integer, parameter :: i16 = selected_int_kind(int(log10(2.0**16)))
integer, parameter :: i32 = selected_int_kind(int(log10(2.0**32)))
integer, parameter :: i64 = selected_int_kind(int(log10(2.0**63)))

!> precision names
character(len=2), parameter, dimension(4) :: cprecs = (/ 'sp', 'dp', 'ep', 'qp' /)

!> global standard-kind parameter for fp#
integer, parameter :: prec = dp

!> standard-kind fp constants
real(prec), parameter :: zero = 0.0_prec
real(prec), parameter :: one  = 1.0_prec
real(prec), parameter :: two  = 2.0_prec
real(prec), parameter :: half = 0.5_prec

!> base of floating point system
integer, parameter :: base = radix(0.0_prec)

! needed len of a string to represent a real number with appropriate precision
! included sign, fractional point, exponent
! also available via generic function prec2len(.)
integer, parameter :: slen = 16
integer, parameter :: dlen = 25
integer, parameter :: elen = 30
integer, parameter :: qlen = 45

! format strings for exact output (default)    s0.dd...ddEse...e
! also available via prec2fmt(prec)
character(*), parameter :: sfmt = '(E16.9  E2)'   !  9 essential decimal digits
character(*), parameter :: dfmt = '(E25.17 E3)'   ! 17 essential decimal digits
character(*), parameter :: efmt = '(E30.21 E4)'   ! 21 essential decimal digits
character(*), parameter :: qfmt = '(E45.36 E4)'   ! 36 essential decimal digits

! format strings for exact output (scientific) sd.dd...ddEse...e
! also available via prec2fmts(prec)
character(*), parameter :: sfmts = '(ES15.8  E2)' !  9 essential decimal digits
character(*), parameter :: dfmts = '(ES24.16 E3)' ! 17 essential decimal digits
character(*), parameter :: efmts = '(ES29.20 E4)' ! 21 essential decimal digits
character(*), parameter :: qfmts = '(ES44.35 E4)' ! 36 essential decimal digits

!> maximal length of format string for fp#
integer, parameter ::  fmtlen = 11
!> maximal length of format string for fp# (scientific)
integer, parameter :: sfmtlen = 12

!> count of bits for exponent in the fp format
integer, parameter :: s_expbits = 8
integer, parameter :: d_expbits = 11
integer, parameter :: e_expbits = 15
integer, parameter :: q_expbits = 15

!> bias-value of the exponent (~maxexp)
integer, parameter :: sbias = 2**(s_expbits-1)-1
integer, parameter :: dbias = 2**(d_expbits-1)-1
integer, parameter :: ebias = 2**(e_expbits-1)-1
integer, parameter :: qbias = 2**(q_expbits-1)-1

type real_precision
  character(len=2)  :: name
  integer           :: prec, expprec, bias
  integer           :: strlen
  character(len=fmtlen)  :: fmt
  character(len=sfmtlen) :: sfmt
  character(len=22) :: bin_fmt
end type

interface put
  module procedure put_s, put_d, put_e!, put_i8, put_i16, put_i32, put_i64
end interface put

interface ulp
  module procedure ulp_s, ulp_d, ulp_e
end interface

interface bias
  module procedure bias_s, bias_d, bias_e
end interface

interface log2
  module procedure log2_s, log2_d, log2_e
end interface

interface pred
  module procedure pred_s, pred_d, pred_e
end interface

interface succ
  module procedure succ_s, succ_d, succ_e
end interface

interface expbits
  module procedure sexpbits, dexpbits, eexpbits
end interface

interface real2str
  module procedure real2str_s, real2str_d, real2str_e
end interface

interface relative_error
  module procedure relative_error_s, relative_error_d, relative_error_e
end interface

contains 

  !> returns human readable form of the floating point precision type
  elemental function prec2char(prec) result(res)
    integer, intent(in) :: prec
    character(len=2)    :: res

    if (prec == sp) then
      res = cprecs(1)
    else if (prec == dp) then
      res = cprecs(2)
    else if (prec == qp) then
      res = cprecs(4)
    else if (prec == ep) then
      res = cprecs(3)
    else
      res = '?p'
    end if
  end function prec2char
 
  !> returns the appropriate format string for the given floating-point
  !! precision
  !! if complete is not given, then a partial format is returned (for using as
  !! part in format string)
  elemental function prec2fmt(prec, complete) result(res)
    integer, intent(in)           :: prec
    logical, intent(in), optional :: complete
    character(len=fmtlen)         :: res

    if (present(complete) .and. complete) then
      if (prec == sp) then
        res = sfmt
      else if (prec == dp) then
        res = dfmt
      else if (prec == qp) then
        res = qfmt
      else if (prec == ep) then
        res = efmt
      else
        res = qfmt
      end if
    else
      if (prec == sp) then
        res = sfmt(2:6)
      else if (prec == dp) then
        res = dfmt(2:7)
      else if (prec == qp) then
        res = qfmt(2:7)
      else if (prec == ep) then
        res = efmt(2:7)
      else
        res = qfmt(2:7)
      end if
    end if
  end function prec2fmt

  !> returns the appropriate format string for the given floating-point
  !  precision
  elemental function prec2fmts(prec, complete) result(res)
    integer, intent(in)           :: prec
    logical, intent(in), optional :: complete
    character(len=sfmtlen)        :: res

    if (present(complete) .and. complete) then
      if (prec == sp) then
        res = sfmts
      else if (prec == dp) then
        res = dfmts
      else if (prec == qp) then
        res = qfmts
      else if (prec == ep) then
        res = efmts
      else
        res = qfmts
      end if
    else
      if (prec == sp) then
        res = sfmts(2:7)
      else if (prec == dp) then
        res = dfmts(2:8)
      else if (prec == qp) then
        res = qfmts(2:8)
      else if (prec == ep) then
        res = efmts(2:8)
      else
        res = qfmts(2:8)
      end if
    end if
  end function prec2fmts

  !> returns the len of the string needed for representing one fp#s in
  !! the given precision
  elemental function prec2len(prec) result(res)
    integer, intent(in) :: prec
    integer             :: res

    if (prec == sp) then
      res = slen
    else if (prec == dp) then
      res = dlen
    else if (prec == qp) then
      res = qlen
    else if (prec == ep) then
      res = elen
    else
      res = qlen
    end if
  end function prec2len
 
  !> outputs fp# with pre & post text in normal or scientific form
  subroutine put_s(x, pre, post, scientific)
    real(sp), intent(in)                   :: x
    character(len=*), intent(in), optional :: pre, post
    logical, intent(in), optional          :: scientific

    if (present(pre)) write(*,fmt='(A)', advance='no') pre
    write(*,*) real2str_s(x, scientific)
    if (present(post)) write(*,fmt='(A)', advance='yes') post 
  end subroutine put_s

  !> outputs fp# with pre & post text in normal or scientific form
  subroutine put_d(x, pre, post, scientific)
    real(dp), intent(in)                   :: x
    character(len=*), intent(in), optional :: pre, post
    logical, intent(in), optional          :: scientific

    if (present(pre)) write(*,fmt='(A)', advance='no') pre
    write(*,*) real2str_d(x, scientific)
    if (present(post)) write(*,fmt='(A)', advance='yes') post 
  end subroutine put_d
   
  !> outputs fp# with pre & post text in normal or scientific form
  subroutine put_e(x, pre, post, scientific)
    real(ep), intent(in)                   :: x
    character(len=*), intent(in), optional :: pre, post
    logical, intent(in), optional          :: scientific

    if (present(pre)) write(*,fmt='(A)', advance='no') pre
    write(*,*) real2str_e(x, scientific)
    if (present(post)) write(*,fmt='(A)', advance='yes') post 
  end subroutine put_e
  
  !> converts an sp-fp# to string
  elemental function real2str_s(x, scientific) result(res)
    real(sp), intent(in)           :: x
    logical, optional, intent(in)  :: scientific
    character(len=slen)            :: res

    if (present(scientific) .and. scientific) then
      write(res, fmt=sfmts) x
    else
      write(res, fmt=sfmt) x
    end if
  end function real2str_s
  
  !> converts an dp-fp# to string
  elemental function real2str_d(x, scientific) result(res)
    real(dp), intent(in)           :: x
    logical, optional, intent(in)  :: scientific
    character(len=dlen)            :: res

    if (present(scientific) .and. scientific) then
      write(res, fmt=dfmts) x
    else
      write(res, fmt=dfmt) x
    end if
  end function real2str_d
  
  !> converts an ep/qp-fp# to string
  function real2str_e(x, scientific) result(res)
    real(ep), intent(in)           :: x
    logical, optional, intent(in)  :: scientific
    character(len=prec2len(ep))    :: res

    if (present(scientific) .and. scientific) then
      write(res, fmt=prec2fmts(ep, .true.)) x
    else
      write(res, fmt=prec2fmt(ep, .true.)) x
    end if
  end function real2str_e

  !> calculates the predecessor fp# of x
  elemental function pred_s(x) result(res)
    real(sp), intent(in) :: x
    real(sp)             :: res

    res = nearest(x, -1.0_sp)
  end function pred_s

  !> calculates the predecessor fp# of x
  elemental function pred_d(x) result(res)
    real(dp), intent(in) :: x
    real(dp)             :: res

    res = nearest(x, -1.0_dp)
  end function pred_d
  
  !> calculates the predecessor fp# of x
  elemental function pred_e(x) result(res)
    real(ep), intent(in) :: x
    real(ep)             :: res

    res = nearest(x, -1.0_ep)
  end function pred_e
  
  !> calculates the successor fp# of x
  elemental function succ_s(x) result(res)
    real(sp), intent(in) :: x
    real(sp)             :: res

    res = nearest(x, +1.0_sp)
  end function succ_s

  !> calculates the successor fp# of x
  elemental function succ_d(x) result(res)
    real(dp), intent(in) :: x
    real(dp)             :: res

    res = nearest(x, +1.0_dp)
  end function succ_d
  
  !> calculates the successor fp# of x
  elemental function succ_e(x) result(res)
    real(ep), intent(in) :: x
    real(ep)             :: res

    res = nearest(x, +1.0_ep)
  end function succ_e

  !> calculates the unit in the last place of a sp fp#
  elemental function ulp_s(x) result(res)
    real(sp), intent(in) :: x
    real(sp)             :: res

    res = real(base, kind(x))**(exponent(x) - digits(x))  
  end function ulp_s

  !> calculates the unit in the last place of a dp fp#
  elemental function ulp_d(x) result(res)
    real(dp), intent(in) :: x
    real(dp)             :: res

    res = real(base, kind(x))**(exponent(x) - digits(x))  
  end function ulp_d

  !> calculates the unit in the last place of a ep or qp fp#
  elemental function ulp_e(x) result(res)
    real(ep), intent(in) :: x
    real(ep)             :: res

    res = real(base, kind(x))**(exponent(x) - digits(x))  
  end function ulp_e

!! there is no compiler supporting all formats in {s, d, e, q}
!! => one procedures argument kind is down/upgraded
!! => two module procedures with same signature for same generic name
!  function ulpq(x) result(res)
!    real(qp), intent(in) :: x
!    real                 :: res
!
!    res = real(b, kind(x))**(exponent(x) - digits(x))  
!  end function ulpq

  !> calculates the two-logarithm of a sp fp#
  pure function log2_s(x) result(res)
    real(sp), intent(in) :: x
    real(sp)             :: res

    res = log(x) / log(2.0_sp)
  end function log2_s

  !> calculates the two-logarithm of a dp fp#
  pure function log2_d(x) result(res)
    real(dp), intent(in) :: x
    real(dp)             :: res

    res = log(x) / log(2.0_dp)
  end function log2_d

  !> calculates the two-logarithm of a ep or qp fp#
  pure function log2_e(x) result(res)
    real(ep), intent(in) :: x
    real(ep)             :: res

    res = log(x) / log(2.0_ep)
  end function log2_e
  
  !> calculates the bias-value of the sp fp#-format
  pure function bias_s(x) result(res)
    real(sp), intent(in) :: x
    integer              :: res
    
    res = 2**(expbits(x) - 1) - 1
  end function bias_s

  !> calculates the bias-value of the dp fp#-format
  pure function bias_d(x) result(res)
    real(dp), intent(in) :: x
    integer              :: res
    
    res = 2**(expbits(x) - 1) - 1
  end function bias_d

  !> calculates the bias-value of the ep or qp fp#-format
  pure function bias_e(x) result(res)
    real(ep), intent(in) :: x
    integer              :: res
    
    res = 2**(expbits(x) - 1) - 1
  end function bias_e

  !> calculates the count of bits for the exponent of the sp fp#-format
  pure function sexpbits(x) result(res)
    real(sp), intent(in) :: x
    integer              :: res

    res = int(1 + log2(real(maxexponent(x))))
  end function sexpbits

  !> calculates the count of bits for the exponent of the dp fp#-format
  pure function dexpbits(x) result(res)
    real(dp), intent(in) :: x
    integer              :: res

    res = int(1 + log2(real(maxexponent(x))))
  end function dexpbits
  
  !> calculates the count of bits for the exponent of the ep or qp fp#-format
  pure function eexpbits(x) result(res)
    real(ep), intent(in) :: x
    integer              :: res

    res = int(1 + log2(real(maxexponent(x))))
  end function eexpbits

  !> calculates the relative error of appx to x in sp
  elemental function relative_error_s(x, appx) result(res)
    real(sp), intent(in) :: x, appx
    real(sp)             :: res

    res = abs((appx - x) / x)
  end function relative_error_s

  !> calculates the relative error of appx to x in dp
  elemental function relative_error_d(x, appx) result(res)
    real(dp), intent(in) :: x, appx
    real(dp)             :: res

    res = abs((appx - x) / x)
  end function relative_error_d

  !> calculates the relative error of appx to x in ep or qp
  elemental function relative_error_e(x, appx) result(res)
    real(ep), intent(in) :: x, appx
    real(ep)             :: res

    res = abs((appx - x) / x)
  end function relative_error_e

  subroutine assert(test, fail_string)
    logical      :: test
    character(*) :: fail_string

    if (.not. test) then
      write(*,*) '!!! ASSERTION failed !!!'
      write(*,*) 'fail-message: ', fail_string
      stop
    end if
  end subroutine assert
end module cari

