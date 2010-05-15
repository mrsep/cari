!> Module with interfaces to numerical standard functions written in C from
!! W. Kraemer and W. Hofschuster. (Functions with one argument of type
!! REAL(fpkind) or TYPE(interval).
!!
!! Functions with two arguments (power and arctan2) are implemented in Fortran
!! within this module.
!!
!! This module consists of two parts:
!!
!! First Part:
!!
!! Interface blocks for the standard functions with one argument
!!
!! These functions are the 'fast standard functions' written in C
!! by Walter Kraemer and Werner Hofschuster.
!!
!!  SQR    SQRT
!!  EXP    EXP2   EXP10   EXPM1  LOG     LOG2    LOG10   LOG1P
!!  SIN    COS    TAN     COT    ASIN    ACOS    ATAN    ACOT
!!  SINH   COSH   TANH    COTH   ASINH   ACOSH   ATANH   ACOTH
!!
!!
!! Second part simple Fortran 90 implementations of the standard
!! functions with two arguments:
!!
!!   POW(p0,p1) = p0 ** p1       ( REAL(fpkind) and TYPE(interval) )
!!
!! ATAN2(p0,p1) = arctan(p0/p1)  ( REAL(fpkind) and TYPE(interval) )
!!
!! Note:
!!       POW for REAL(fpkind) arguments is identical with the
!!       standard Fortran 90 power operator p0 ** p1
!!       POW for TYPE(interval) uses exp and log for TYPE(interval)
!!       Some versions of POW for mixed types of operands are supplied
!!       also which partly use an iterated multiplication/squaring
!!       algorithm in the case of integer exponentes.
!!
!!       ATAN2 for REAL(fpkind) arguments is identical with the
!!       standard Fortran 90 ATAN2 function.
!!       ATAN2 for TYPE(interval) uses ATAN2 for TYPE(interval)
!!       and some simple logic around it.
!!       No mixed argument types are implemented as yet.
!!
!> \author Wolfgang V. Walter et al.
!> \date 09.10.1998

!> modified by Hans Peschke in May 2010
!! - updated to fi_lib version 1.2
!! - use original file- and function names and use fi_lib.h and macros for renaming
!! - remove the pointer-stuff
!! - use F2003 ISO-C-Bindings
module fi_lib
use cari, only : prec, one, zero
use ivalarith
implicit none

private

            ! generic names :
            !
public ::   sqr  , sqrt ,                                                         &
            exp  , exp2 , exp10 , expm1, log   , log2  , log10 , log1p ,          &
            sin  , cos  , tan   , cot  , asin  , acos  , atan  , acot  ,          &
            sinh , cosh , tanh  , coth , asinh , acosh , atanh , acoth ,          &
            pow  , atan2, operator(**)
            !
            ! specific names for real(prec) functions :
            !
public ::   xrsqr  , xrsqrt ,                                                             &
            xrexp  , xrexp2 , xrexp10 , xrexpm1, xrlog   , xrlog2  , xrlog10 , xrlog1p ,  &
            xrsin  , xrcos  , xrtan   , xrcot  , xrasin  , xracos  , xratan  , xracot  ,  &
            xrsinh , xrcosh , xrtanh  , xrcoth , xrasinh , xracosh , xratanh , xracoth ,  &
            xrpowr , xrpowg
            !
            ! specific names for type(interval) functions :
            !
public ::   xisqr  , xisqrt ,                                                             &
            xiexpo , xiexp2 , xiexp10 , xiexpm1, xilog   , xilog2  , xilog10 , xilog1p ,  &
            xisin  , xicos  , xitan   , xicot  , xiasin  , xiacos  , xiatan  , xiacot  ,  &
            xisinh , xicosh , xitanh  , xicoth , xiasinh , xiacosh , xiatanh , xiacoth ,  &
            xipowi , xipowg , xipowr  , xiatan2

private ::  xipatan2


  interface sqr
    function xrsqr(p0) result(res) bind(C)
      use cari, only : prec
      implicit none
      real(prec)                    :: res
      real(prec), value, intent(in) :: p0
    end function xrsqr

    function xisqr(p0) result(res) bind(C)
      use ivalarith, only : interval
      implicit none
      type(interval)                    :: res
      type(interval), value, intent(in) :: p0
    end function xisqr
  end interface

  interface sqrt
    function xrsqrt(p0) result(res) bind(C)
      use cari, only : prec
      implicit none
      real(prec)                    :: res
      real(prec), value, intent(in) :: p0
    end function xrsqrt

    function xisqrt(p0) result(res) bind(C)
      use ivalarith, only : interval
      implicit none
      type(interval)                    :: res
      type(interval), value, intent(in) :: p0
    end function xisqrt
  end interface

  interface exp
    function xrexp(p0) result(res) bind(C)
      use cari, only : prec
      implicit none
      real(prec)                    :: res
      real(prec), value, intent(in) :: p0
    end function xrexp

    function xiexpo(p0) result(res) bind(C)
      use ivalarith, only : interval
      implicit none
      type(interval)                    :: res
      type(interval), value, intent(in) :: p0
    end function xiexpo
  end interface

  interface exp2
    function xrexp2(p0) result(res) bind(C)
      use cari, only : prec
      implicit none
      real(prec)                    :: res
      real(prec), value, intent(in) :: p0
    end function xrexp2

    function xiexp2(p0) result(res) bind(C)
      use ivalarith, only : interval
      implicit none
      type(interval)                    :: res
      type(interval), value, intent(in) :: p0
    end function xiexp2
  end interface

  interface exp10
    function xrexp10(p0) result(res) bind(C)
      use cari, only : prec
      implicit none
      real(prec)                    :: res
      real(prec), value, intent(in) :: p0
    end function xrexp10
    
    function xiexp10(p0) result(res) bind(C)
      use ivalarith, only : interval
      implicit none
      type(interval)                    :: res
      type(interval), value, intent(in) :: p0
    end function xiexp10
  end interface

  interface expm1
    function xrexpm1(p0) result(res) bind(C)
      use cari, only : prec
      implicit none
      real(prec)                    :: res
      real(prec), value, intent(in) :: p0
    end function xrexpm1

    function xiexpm1(p0) result(res) bind(C)
      use ivalarith, only : interval
      implicit none
      type(interval)                    :: res
      type(interval), value, intent(in) :: p0
    end function xiexpm1
  end interface

  interface log
    function xrlog(p0) result(res) bind(C)
      use cari, only : prec
      implicit none
      real(prec)                    :: res
      real(prec), value, intent(in) :: p0
    end function xrlog

    function xilog(p0) result(res) bind(C)
      use ivalarith, only : interval
      implicit none
      type(interval)                    :: res
      type(interval), value, intent(in) :: p0
    end function xilog
  end interface

  interface log2
    function xrlog2(p0) result(res) bind(C)
      use cari, only : prec
      implicit none
      real(prec)                    :: res
      real(prec), value, intent(in) :: p0
    end function xrlog2

    function xilog2(p0) result(res) bind(C)
      use ivalarith, only : interval
      implicit none
      type(interval)                    :: res
      type(interval), value, intent(in) :: p0
    end function xilog2
  end interface

  interface log10
    function xrlog10(p0) result(res) bind(C)
      use cari, only : prec
      implicit none
      real(prec)                    :: res
      real(prec), value, intent(in) :: p0
    end function xrlog10

    function xilog10(p0) result(res) bind(C)
      use ivalarith, only : interval
      implicit none
      type(interval)                    :: res
      type(interval), value, intent(in) :: p0
    end function xilog10
  end interface

  interface log1p
    function xrlog1p(p0) result(res) bind(C)
      use cari, only : prec
      implicit none
      real(prec)                    :: res
      real(prec), value, intent(in) :: p0
    end function xrlog1p
  
    function xilog1p(p0) result(res) bind(C)
      use ivalarith, only : interval
      implicit none
      type(interval)                    :: res
      type(interval), value, intent(in) :: p0
    end function xilog1p
  end interface

  interface sin
    function xrsin(p0) result(res) bind(C)
      use cari, only : prec
      implicit none
      real(prec)                    :: res
      real(prec), value, intent(in) :: p0
    end function xrsin

    function xisin(p0) result(res) bind(C)
      use ivalarith, only : interval
      implicit none
      type(interval)                    :: res
      type(interval), value, intent(in) :: p0
    end function xisin
  end interface

  interface cos
    function xrcos(p0) result(res) bind(C)
      use cari, only : prec
      implicit none
      real(prec)                    :: res
      real(prec), value, intent(in) :: p0
    end function xrcos

    function xicos(p0) result(res) bind(C)
      use ivalarith, only : interval
      implicit none
      type(interval)                    :: res
      type(interval), value, intent(in) :: p0
    end function xicos
  end interface

  interface tan
    function xrtan(p0) result(res) bind(C)
      use cari, only : prec
      implicit none
      real(prec)                    :: res
      real(prec), value, intent(in) :: p0
    end function xrtan

    function xitan(p0) result(res) bind(C)
      use ivalarith, only : interval
      implicit none
      type(interval)                    :: res
      type(interval), value, intent(in) :: p0
    end function xitan
  end interface

  interface cot
    function xrcot(p0) result(res) bind(C)
      use cari, only : prec
      implicit none
      real(prec)                    :: res
      real(prec), value, intent(in) :: p0
    end function xrcot

    function xicot(p0) result(res) bind(C)
      use ivalarith, only : interval
      implicit none
      type(interval)                    :: res
      type(interval), value, intent(in) :: p0
    end function xicot
  end interface

  interface asin
    function xrasin(p0) result(res) bind(C)
      use cari, only : prec
      implicit none
      real(prec)                    :: res
      real(prec), value, intent(in) :: p0
    end function xrasin

    function xiasin(p0) result(res) bind(C)
      use ivalarith, only : interval
      implicit none
      type(interval)                    :: res
      type(interval), value, intent(in) :: p0
    end function xiasin
  end interface

  interface acos
    function xracos(p0) result(res) bind(C)
      use cari, only : prec
      implicit none
      real(prec)                    :: res
      real(prec), value, intent(in) :: p0
    end function xracos

    function xiacos(p0) result(res) bind(C)
      use ivalarith, only : interval
      implicit none
      type(interval)                    :: res
      type(interval), value, intent(in) :: p0
    end function xiacos
  end interface

  interface atan
    function xratan(p0) result(res) bind(C)
      use cari, only : prec
      implicit none
      real(prec)                    :: res
      real(prec), value, intent(in) :: p0
    end function xratan

    function xiatan(p0) result(res) bind(C)
      use ivalarith, only : interval
      implicit none
      type(interval)                    :: res
      type(interval), value, intent(in) :: p0
    end function xiatan
  end interface

  interface acot
    function xracot(p0) result(res) bind(C)
      use cari, only : prec
      implicit none
      real(prec)                    :: res
      real(prec), value, intent(in) :: p0
    end function xracot

    function xiacot(p0) result(res) bind(C)
      use ivalarith, only : interval
      implicit none
      type(interval)                    :: res
      type(interval), value, intent(in) :: p0
    end function xiacot
  end interface

  interface sinh
    function xrsinh(p0) result(res) bind(C)
      use cari, only : prec
      implicit none
      real(prec)                    :: res
      real(prec), value, intent(in) :: p0
    end function xrsinh

    function xisinh(p0) result(res) bind(C)
      use ivalarith, only : interval
      implicit none
      type(interval)                    :: res
      type(interval), value, intent(in) :: p0
    end function xisinh
  end interface

  interface cosh
    function xrcosh(p0) result(res) bind(C)
      use cari, only : prec
      implicit none
      real(prec)                    :: res
      real(prec), value, intent(in) :: p0
    end function xrcosh

    function xicosh(p0) result(res) bind(C)
      use ivalarith, only : interval
      implicit none
      type(interval)                    :: res
      type(interval), value, intent(in) :: p0
    end function xicosh
  end interface

  interface tanh
    function xrtanh(p0) result(res) bind(C)
      use cari, only : prec
      implicit none
      real(prec)                    :: res
      real(prec), value, intent(in) :: p0
    end function xrtanh

    function xitanh(p0) result(res) bind(C)
      use ivalarith, only : interval
      implicit none
      type(interval)                    :: res
      type(interval), value, intent(in) :: p0
    end function xitanh
  end interface

  interface coth
    function xrcoth(p0) result(res) bind(C)
      use cari, only : prec
      implicit none
      real(prec)                    :: res
      real(prec), value, intent(in) :: p0
    end function xrcoth

    function xicoth(p0) result(res) bind(C)
      use ivalarith, only : interval
      implicit none
      type(interval)                    :: res
      type(interval), value, intent(in) :: p0
    end function xicoth
  end interface

  interface asinh
    function xrasinh(p0) result(res) bind(C)
      use cari, only : prec
      implicit none
      real(prec)                    :: res
      real(prec), value, intent(in) :: p0
    end function xrasinh

    function xiasinh(p0) result(res) bind(C)
      use ivalarith, only : interval
      implicit none
      type(interval)                    :: res
      type(interval), value, intent(in) :: p0
    end function xiasinh
  end interface

  interface acosh
    function xracosh(p0) result(res) bind(C)
      use cari, only : prec
      implicit none
      real(prec)                    :: res
      real(prec), value, intent(in) :: p0
    end function xracosh

    function xiacosh(p0) result(res) bind(C)
      use ivalarith, only : interval
      implicit none
      type(interval)                    :: res
      type(interval), value, intent(in) :: p0
    end function xiacosh
  end interface

  interface atanh
    function xratanh(p0) result(res) bind(C)
      use cari, only : prec
      implicit none
      real(prec)                    :: res
      real(prec), value, intent(in) :: p0
    end function xratanh

    function xiatanh(p0) result(res) bind(C)
      use ivalarith, only : interval
      implicit none
      type(interval)                    :: res
      type(interval), value, intent(in) :: p0
    end function xiatanh
  end interface

  interface acoth
    function xracoth(p0) result(res) bind(C)
      use cari, only : prec
      implicit none
      real(prec)                    :: res
      real(prec), value, intent(in) :: p0
    end function xracoth
    
    function xiacoth(p0) result(res) bind(C)
      use ivalarith, only : interval
      implicit none
      type(interval)                    :: res
      type(interval), value, intent(in) :: p0
    end function xiacoth
  end interface

 !
 ! interface blocks and fortran 90 implementation for standard functions
 ! with two arguments.
 !
 ! pow (power),  ( real(fpkind) and type(interval) )
 ! also as ** operator available.
 !   for real(prec) and integer arguments this reduces to the standard f90
 !   ** - operator. in order to get an interval result for these operations
 !   one *must* use the pow-function instead of the operator ** !
 !
 ! atan2(y,x) = arctan(y/x)  ( real(fpkind) and type(interval) )
 !   for real(prec) arguments this reduces to the standard f90 atan2 - function
 !   in order to get an interval result for this function one *must* convert
 !   both arguments to type interval !
 !

  interface pow
    module procedure xrpowr, xipowi, xrpowg, xipowg, xipowr
  end interface

  interface operator (**)
    module procedure xipowi, xipowg, xipowr
  end interface

  interface atan2
    module procedure xiatan2
  end interface

contains

  function xrpowr(x, y) result(res)
    real(prec)             :: res
    real(prec), intent(in) :: x, y

    res = x ** y

    return
  end function xrpowr

  function xipowi(x, y) result(res)
    type(interval)             :: res
    type(interval), intent(in) :: x, y

    if ( diam(y) == 0.0_prec ) then
      res = xipowr(x, inf(y))
    else
      res = exp(y*log(x))
    end if

    return
  end function xipowi

  function xrpowg(x, y) result(res)
    type(interval)           :: res
    real(prec), intent(in)   :: x
    integer, intent(in)      :: y

    type(interval)           :: r,s
    integer                  :: j

    ! compute power by successive squaring and multiplying of x
    ! ( multiply x or 1/x according to sign(y) )

    if ( y == 0 ) then
      if ( x == 0.0_prec ) then
        res = ival( x ** y )  ! force runtime error: 0 ** 0
      else
        res = ival( 1.0_prec )
      end if
    else
      j = abs(y)
      r = ival( 1.0_prec )
      if ( y > 0 ) then
        s = ival( x )
      else
        s = 1.0_prec .idiv. x
      end if
      do
        if ( mod(j,2) == 1 ) r = r*s
        j = j / 2
        if ( j > 0 ) then
          s = sqr(s)
        else
          exit
        end if
      end do
      res = r
    end if

    return
  end function xrpowg

  function xipowg(x, y) result(res)
    type(interval)             :: res
    type(interval), intent(in) :: x
    integer, intent(in)        :: y

    if ( (y <= 0) .and. (0.0_prec .in. x) ) then
      ! illegal operands
      res = ival( real(0 ** y, prec) )   !  force runtime error:  0 ** y with y < 0
    else if ( ( mod(y,2) == 1 ) .or. .not. ( 0.0_prec .in. x) ) then
      ! function is monotone on x
      res = xrpowg(inf(x),y) .ihull. xrpowg(sup(x),y)
    else if ( -inf(x) > sup(x) ) then
      ! function not monotone: zero is minimum, inf(x) ** y is maximum
      res = 0.0_prec .ihull. xrpowg(inf(x),y)
    else
      ! function not monotone: zero is minimum, sup(x) ** y is maximum
      res = 0.0_prec .ihull. xrpowg(sup(x),y)
    end if

    return
  end function xipowg

  function xipowr(x, y) result(res)
    type(interval)             :: res
    type(interval), intent(in) :: x
    real(prec), intent(in)     :: y

    ! TODO: this is arbitrary (magic number), change it
    real(prec),parameter     :: max_integer_exponent = 32000.0_prec

    ! compute powers x ** y using interval power integer in the case the real
    ! exponent y contains a precise integer value below max_integer_exponent

    if ( ( abs(y) <= max_integer_exponent ) .and. ( y == int(y) ) ) then
      res = xipowg( x, int(y) )
    else
      res = exp( y * log(x) )
    end if

    return
  end function xipowr

  function xipatan2(y, x, pi) result(res)
    type(interval)             :: res
    real(prec), intent(in)     :: x,y
    type(interval), intent(in) :: pi

    if      ( sign(one,x) > zero ) then
      res = atan( y .idiv. x )
    else if ( sign(one,y) > zero ) then
      res = atan( y .idiv. x ) + pi
    else
      res = atan( y .idiv. x ) - pi
    end if

    return
  end function xipatan2

  function xiatan2(y, x) result(res)
    type(interval)             :: res
    type(interval), intent(in) :: x,y
    integer, parameter         :: below  =  0,            &  !
                                  lbound =  1,            &  ! position of zero
                                  inner  =  2,            &  ! with respect to
                                  exact  =  3,            &  ! a real interval
                                  ubound =  4,            &  !
                                  above  =  5                !
    integer                    :: x_zero, y_zero
    type(interval)             :: lo,up,                  &
                                  pi,pi_halve

    pi_halve = itwo*atan( ione )
    pi       = itwo*pi_halve

    ! determine position of zero relative to x-interval:
    if ( zero <  inf(x) ) then
      x_zero = below
    else if ( zero == inf(x) ) then
      if ( zero == sup(x) ) then
        x_zero = exact
      else
        x_zero = lbound
      end if
    else if ( zero <  sup(x) ) then
      x_zero = inner
    else if ( zero == sup(x) ) then
      x_zero = ubound
    else
      x_zero = above
    end if

    ! determine position of zero relative to y-interval:
    if ( zero <  inf(y) ) then
      y_zero = below
    else if ( zero == inf(y) ) then
      if ( zero == sup(y) ) then
        y_zero = exact
      else
        y_zero = lbound
      end if
    else if ( zero <  sup(y) ) then
      y_zero = inner
    else if ( zero == sup(y) ) then
      y_zero = ubound
    else
      y_zero = above
    end if

    ! for each of the 6x6 = 36 possible combinations determine which
    ! argument pair x,y is to be taken in xipatan2(y,x,pi) for the lower and
    ! the upper bound of the result (then mode_.=arg) or if the value
    ! of the bound can be specified a priori (then mode_.=val)
    select case (x_zero)

      case (above)
        select case (y_zero)
          case (below )
                          lo =  xipatan2( sup(y), sup(x), pi )
                          up =  xipatan2( inf(y), sup(x), pi )
          case (lbound)
                          lo =  xipatan2( sup(y), sup(x), pi )
                          up =  pi
          case (exact )
                          lo =  pi
                          up =  pi
          case (inner )
                          lo = -pi
                          up =  pi
          case (ubound)
                          lo = -pi
                          up =  xipatan2( inf(y), sup(x), pi )
          case (above )
                          lo =  xipatan2( sup(y), inf(x), pi )
                          up =  xipatan2( inf(y), sup(x), pi )
        end select ! (y_zero)

      case (ubound)   !
        select case (y_zero)
          case (below )
                          lo =  pi_halve
                          up =  xipatan2( inf(y), inf(x), pi )
          case (lbound)
                          lo =  pi_halve
                          up =  pi
          case (exact )
                          lo =  pi
                          up =  pi
          case (inner )
                          lo = -pi
                          up =  pi
          case (ubound)
                          lo = -pi
                          up = -pi_halve
          case (above )
                          lo =  xipatan2( sup(y), inf(x), pi )
                          up = -pi_halve
        end select ! (y_zero)

      case (inner)
        select case (y_zero)
          case (below )
                          lo =  xipatan2( inf(y), sup(x), pi )
                          up =  xipatan2( inf(y), inf(x), pi )
          case (lbound)
                          lo =  izero
                          up =  pi
          case (exact )
                          lo =  izero
                          up =  pi
          case (inner )
                          lo = -pi
                          up =  pi
          case (ubound)
                          lo = -pi
                          up =  izero
          case (above )
                          lo =  xipatan2( sup(y), inf(x), pi )
                          up =  xipatan2( sup(y), sup(x), pi )
        end select ! (y_zero)

      case (exact)
        select case (y_zero)
          case (below,lbound)
                          lo =  pi_halve
                          up =  pi_halve
          case (exact )
                          lo = -pi
                          up =  pi
          case (inner )
                          lo = -pi_halve
                          up =  pi_halve
          case (ubound,above)
                          lo = -pi_halve
                          up = -pi_halve
        end select ! (y_zero)

      case (lbound)
        select case (y_zero)
          case (below )
                          lo =  xipatan2( inf(y), sup(x), pi )
                          up =  pi_halve
          case (lbound)
                          lo =  izero
                          up =  pi_halve
          case (exact )
                          lo =  izero
                          up =  izero
          case (inner )
                          lo = -pi_halve
                          up =  pi_halve
          case (ubound)
                          lo = -pi_halve
                          up =  izero
          case (above )
                          lo = -pi_halve
                          up =  xipatan2( sup(y), sup(x), pi )
        end select ! (y_zero)

      case (below)
        select case (y_zero)
          case (below )
                          lo =  xipatan2( inf(y), sup(x), pi )
                          up =  xipatan2( sup(y), inf(x), pi )
          case (lbound)
                          lo =  izero
                          up =  xipatan2( sup(y), inf(x), pi )
          case (exact )
                          lo =  izero
                          up =  izero
          case (inner )
                          lo =  xipatan2( inf(y), inf(x), pi )
                          up =  xipatan2( sup(y), inf(x), pi )
          case (ubound)
                          lo =  xipatan2( inf(y), inf(x), pi )
                          up =  izero
          case (above )
                          lo =  xipatan2( inf(y), inf(x), pi )
                          up =  xipatan2( sup(y), sup(x), pi )
        end select ! (y_zero)

    end select ! (x_zero)

    ! compute final result :
    res = ival( inf(lo), sup(up) )

    return
  end function xiatan2
end module fi_lib
