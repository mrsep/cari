!> implements standard-operations with rounding control (up/down)
!!
!> \author Hans Peschke
!> \date 2009 - 2010

module ieeearith
use cari
use ieee_arithmetic
implicit none
public

interface operator (.addu.)
  module procedure r_addu_r
end interface 

interface operator (.addd.)
  module procedure r_addd_r
end interface 

interface operator (.subu.)
  module procedure r_subu_r
end interface 

interface operator (.subd.)
  module procedure r_subd_r
end interface 

interface operator (.mulu.)
  module procedure r_mulu_r
end interface 

interface operator (.muld.)
  module procedure r_muld_r
end interface 

interface operator (.divd.)
  module procedure r_divd_r
end interface 

interface operator (.divu.)
  module procedure r_divu_r
end interface 

contains
  function r_addu_r(x, y) result(res)
    real(prec), intent(in) :: x, y
    real(prec)             :: res

    call ieee_set_rounding_mode(ieee_up)
    res = x + y
    call ieee_set_rounding_mode(ieee_nearest)
  end function r_addu_r

  function r_addd_r(x, y) result(res)
    real(prec), intent(in) :: x, y
    real(prec)             :: res

    call ieee_set_rounding_mode(ieee_down)
    res = x + y
    call ieee_set_rounding_mode(ieee_nearest)
  end function r_addd_r

  function r_subu_r(x, y) result(res)
    real(prec), intent(in) :: x, y
    real(prec)             :: res

    call ieee_set_rounding_mode(ieee_up)
    res = x - y
    call ieee_set_rounding_mode(ieee_nearest)
  end function r_subu_r

  function r_subd_r(x, y) result(res)
    real(prec), intent(in) :: x, y
    real(prec)             :: res

    call ieee_set_rounding_mode(ieee_down)
    res = x - y
    call ieee_set_rounding_mode(ieee_nearest)
  end function r_subd_r

  function r_mulu_r(x, y) result(res)
    real(prec), intent(in) :: x, y
    real(prec)             :: res

    call ieee_set_rounding_mode(ieee_up)
    res = x * y
    call ieee_set_rounding_mode(ieee_nearest)
  end function r_mulu_r

  function r_muld_r(x, y) result(res)
    real(prec), intent(in) :: x, y
    real(prec)             :: res

    call ieee_set_rounding_mode(ieee_down)
    res = x * y
    call ieee_set_rounding_mode(ieee_nearest)
  end function r_muld_r

  function r_divu_r(x, y) result(res)
    real(prec), intent(in) :: x, y
    real(prec)             :: res

    call ieee_set_rounding_mode(ieee_up)
    res = x / y
    call ieee_set_rounding_mode(ieee_nearest)
  end function r_divu_r

  function r_divd_r(x, y) result(res)
    real(prec), intent(in) :: x, y
    real(prec)             :: res

    call ieee_set_rounding_mode(ieee_down)
    res = x / y
    call ieee_set_rounding_mode(ieee_nearest)
  end function r_divd_r
end module ieeearith
