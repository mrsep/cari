!> module with basic functions and constants for computer arithmetic
!!
!> \author Hans Peschke
!> \date 2009 - 2010

module cari
implicit none

!> TODO
! - output of min_denorm
! - there are several issues with the i/o formats, esp. with gfortran
! - the plan is to implement these functions also for general
! 	numberformats R := R(b, l, emin, emax) and interface them
!   -> fp# are element of instance of general numberformat type
!   -> some constants should be part of numberformat type, or inq functions
!      of them, or better safe the api and serve new module for general
!      numberformats??
! - pred, succ
! - arith expression parser???
! - verified I/O

public

!> kind parameters
integer, parameter :: sp = kind(0.0)
integer, parameter :: dp = kind(0.0D0)
integer, parameter :: ep = selected_real_kind(18, 1000)
integer, parameter :: qp = kind(0.0Q0)

integer, parameter :: i8  = selected_int_kind(int(log10(2.0**8)))
integer, parameter :: i16 = selected_int_kind(int(log10(2.0**16)))
integer, parameter :: i32 = selected_int_kind(int(log10(2.0**32)))
integer, parameter :: i64 = selected_int_kind(int(log10(2.0**63)))

!> precision names
character(len=2), parameter, dimension(4) :: cprecs = (/ 'sp', 'dp', 'ep', 'qp' /)

!> global kind parameter
integer, parameter :: prec = dp

!> standard-kind fp constants
real(prec), parameter :: zero = 0.0_prec
real(prec), parameter :: one  = 1.0_prec
real(prec), parameter :: two  = 2.0_prec
real(prec), parameter :: half = 0.5_prec

!> base of floating point system
integer, parameter :: base = radix(0.0_prec)

!> needed len of a string to represent a real number with appropriate precision
!! prec2len()
integer, parameter :: slen = 15
integer, parameter :: dlen = 24
integer, parameter :: elen = 43
integer, parameter :: qlen = 43

!> format strings for exact output (default)
!! prec2fmt()
character(*), parameter :: sfmt = '(E15.8)'  !  8 notw. Dezimalstellen
character(*), parameter :: dfmt = '(E25.18)' ! 17 notw. Dezimalstellen
!> \bug problems with gfortran!
character(*), parameter :: efmt = '(E31.22)' ! 20 notw. Dezimalstellen
character(*), parameter :: qfmt = '(E43.35)' ! 35 notw. Dezimalstellen

!> format strings for exact output (scientific)
!! prec2fmts()
character(*), parameter :: sfmts = '(ES14.7)'  !  8 notw. Dezimalstellen
character(*), parameter :: dfmts = '(ES23.16)' ! 17 notw. Dezimalstellen
character(*), parameter :: efmts = '(ES26.19)' ! 20 notw. Dezimalstellen
character(*), parameter :: qfmts = '(ES42.34)' ! 35 notw. Dezimalstellen

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
  character(len=8)  :: fmt
  character(len=9)  :: sfmt
  character(len=22) :: bin_fmt
end type

interface ulp
  module procedure ulps, ulpd, ulpe
end interface

interface bias
  module procedure bias_s, bias_d, bias_e
end interface

interface log2
  module procedure log2_s, log2_d, log2_e
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

  !> returns human readable form of the floating point number type
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
      res = '??'
    end if
  end function prec2char
 
  !> returns the appropriate format string for the given floating-point
  !  precision
  elemental function prec2fmt(prec, complete) result(res)
    integer, intent(in)           :: prec
    logical, intent(in), optional :: complete
    character(len=8)              :: res

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
    character(len=9)              :: res

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

  !> returns the len of the string needed for representing one fp-number in
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
 
  !> converts an sp-fp to string
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
  
  !> converts an dp-fp to string
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
  
  ! TODO, gfortran
  !> converts an ep/qp-fp to string
  function real2str_e(x, scientific) result(res)
    real(ep), intent(in)           :: x
    logical, optional, intent(in)  :: scientific
    character(len=prec2len(ep))    :: res
    ! write(*,*) prec2len(ep), prec2fmt(ep, .true.)

    if (present(scientific) .and. scientific) then
      write(res, fmt=prec2fmts(ep, .true.)) x
    else
      write(res, fmt=prec2fmt(ep, .true.)) x
    end if
  end function real2str_e

  !> calculates the unit in the last place of a sp fp#
  elemental function ulps(x) result(res)
    real(sp), intent(in) :: x
    real(sp)             :: res

    res = real(base, kind(x))**(exponent(x) - digits(x))  
  end function ulps

  !> calculates the unit in the last place of a dp fp#
  elemental function ulpd(x) result(res)
    real(dp), intent(in) :: x
    real(dp)             :: res

    res = real(base, kind(x))**(exponent(x) - digits(x))  
  end function ulpd

  !> calculates the unit in the last place of a ep or qp fp#
  elemental function ulpe(x) result(res)
    real(ep), intent(in) :: x
    real(ep)             :: res

    res = real(base, kind(x))**(exponent(x) - digits(x))  
  end function ulpe

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

