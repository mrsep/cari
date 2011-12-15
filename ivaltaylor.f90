!> interval taylor arithmetic with forward mode for all basic mathematical
!! operations, elementary functions as well as arbitrary compositions of them
module ivaltaylor
use cari, only : prec, i64
use ivalarith
!use fi_lib ! imported in the specific functions implementations
implicit none

! TODO
! - problems:
!   - left side of op= is tmp, not defined, right side is not defined

! TODO to implement
! - expo(. , .) with all combinations of types: int, r, ival, it
! - nth-root (integer)
! - multivariatmultivariatee ???
! - erf, erfc from fi_lib
! - identity function

! TODO version 2
! - evaluation of a function, only given by its finite taylor series in x0, 
!   in a point x /= x0
!   - save the original expansio point, but how does it develop through
!     composition of it-functions??? -> inherit
!   - assert in binary-ops that both it are developped around the same
!     expansion-point
! - the maximum computed order of each it has to be stored in it
!   => no global var max_ord

private

! helper functions & entities for internal use only
private :: it_si_co, it_trig, essential_order, max_essential_order, imapping, &
         & max_ord, unbounded_order, not_defined

! for internal testing purposes or reference implementations
private :: it_sqr_simple, it_sqrt_simple, it_asin_simple

! public api: it management, types and abstract interfaces
public :: itaylor, init, order, const, expand, free, defined, get_order,     &
        & set_bounded_order, set_unbounded_order, eval, put, D, T,           &
        & get_taylor_coefficient, set_temporary, itmapping

! public api: binary operations
public :: assignment(=), operator(+), operator(-), operator(*), operator(/)

! public api: elementary functions with tested taylor series expansion
public ::  log, log2, log10, exp,   sqr,  sqrt,        &
        &  sin,  cos,  tan,  cot,  sinh,  cosh,  tanh, &
        & asin, acos, atan, acot, asinh, acosh, atanh

! problems:
public :: coth, acoth ! wolfram taylor series x=0.5 and 2.0

!> defines the maximal order of taylor-coefficients to be calculated
!! special-value -1 means that there is no bound in the order
!! special-value -2 means that the it is not defined
integer, parameter :: unbounded_order = -1
integer, parameter :: not_defined     = -2
integer            :: max_ord = unbounded_order

!> interval taylor type
type itaylor
  private
  type(interval), dimension(:), pointer :: cof => null()
  logical                               :: tmp   ! result of expression?
  type(interval)                        :: a     ! expansion point/interval !! v2
  integer                               :: order ! forced order             !! v2
end type itaylor

!> interface to elementary interval functions implemented in fi_lib
abstract interface
  function imapping(x) result(res) bind(C)
    use ivalarith, only : interval
    type(interval), value, intent(in) :: x
    type(interval)                    :: res
  end function imapping
end interface

!> interface to it-functions
abstract interface 
  function itmapping(x) result(res)
    use ivalarith,  only : interval
    import :: itaylor
    type(interval), intent(in) :: x
    type(itaylor)              :: res
  end function itmapping
end interface

interface init
  module procedure it_init
end interface

interface tmp_free
  module procedure it_tmp_free
end interface

interface free
  module procedure it_free
end interface

interface defined
  module procedure it_defined
end interface

interface set_temporary
  module procedure it_set_temporary
end interface

interface order
  module procedure it_order
end interface

interface get_order
  module procedure it_get_order
end interface

interface set_bounded_order
  module procedure it_set_bounded_order
end interface

interface set_unbounded_order
  module procedure it_set_unbounded_order
end interface

interface essential_order
  module procedure it_essential_order
end interface

interface max_essential_order
  module procedure it_max_essential_order
end interface

interface order_is_bounded
  module procedure it_order_is_bounded
end interface

interface order_is_unbounded
  module procedure it_order_is_unbounded
end interface

interface put
  module procedure it_put
end interface

!> initializes interval-constant
interface const
  module procedure it_const_i, it_const_r
end interface

!> initializes interval-constant
interface const
  module procedure const_it_i, const_it_r
end interface

!> initializes interval-variable 
interface expand
  module procedure it_var_i, it_var_r
end interface

!> initializes interval-variable 
interface expand
  module procedure var_it_i, var_it_r
end interface

interface assignment (=)
  module procedure it_assign_it, it_assign_i, it_assign_r
end interface

interface operator (+)
  module procedure add_it, it_add_it, it_add_i, i_add_it, it_add_r, r_add_it
end interface

interface operator (-)
  module procedure sub_it, it_sub_it, it_sub_i, i_sub_it, it_sub_r, r_sub_it
end interface

interface operator (*)
  module procedure it_mul_it, it_mul_i, i_mul_it, it_mul_r, r_mul_it
end interface

interface operator (/)
  module procedure it_div_it, it_div_i, i_div_it, it_div_r, r_div_it
end interface

interface eval
  module procedure it_eval
end interface

interface sqr
  module procedure it_sqr
end interface

interface sqrt
  module procedure it_sqrt
end interface

interface log
  module procedure it_log
end interface

interface log2
  module procedure it_log2
end interface

interface log10
  module procedure it_log10
end interface

interface exp
  module procedure it_exp
end interface

interface sin
  module procedure it_sin
end interface

interface cos
  module procedure it_cos
end interface

interface tan
  module procedure it_tan
end interface

interface cot
  module procedure it_cot
end interface

interface sinh
  module procedure it_sinh
end interface

interface cosh
  module procedure it_cosh
end interface

interface tanh
  module procedure it_tanh
end interface

interface coth
  module procedure it_coth
end interface

interface asin
  module procedure it_asin
end interface

interface acos
  module procedure it_acos
end interface

interface atan
  module procedure it_atan
end interface

interface acot
  module procedure it_acot
end interface

interface asinh
  module procedure it_asinh
end interface

interface acosh
  module procedure it_acosh
end interface

interface atanh
  module procedure it_atanh
end interface

interface acoth
  module procedure it_acoth
end interface

! op (**) it^int it^i, r^it, i^it, r>0
!interface value, D, derivative, integral, slope
!  module procedure horner...
!end interface

contains

  !> initialize it
  !! allocate cof only if ord present and >= 0
  subroutine it_init(it, ord)
    type(itaylor), intent(inout)  :: it
    integer, intent(in), optional :: ord

    it%tmp = .false.
    it%a = izero
    if (defined(it)) call free(it)
    it%cof => null()
    if (present(ord) .and. ord >= 0) allocate(it%cof(0:ord))
  end subroutine it_init

  !! for internal use only
  subroutine it_free(it)
    type(itaylor), intent(in) :: it
    if (associated(it%cof)) deallocate(it%cof)
  end subroutine it_free

  !> for internal use only: free tmp-operands of operators
  subroutine it_tmp_free(it1, it2)
    type(itaylor), intent(in)           :: it1
    type(itaylor), intent(in), optional :: it2
    
    if (it1%tmp .and. associated(it1%cof)) deallocate(it1%cof)
    if (present(it2) .and. it2%tmp .and. associated(it2%cof)) then
      deallocate(it2%cof)
    end if
  end subroutine it_tmp_free

  function it_defined(it) result(res)
    type(itaylor), intent(in) :: it
    logical                   :: res

    call tmp_free(it)
    res = associated(it%cof)
  end function it_defined

  subroutine it_set_temporary(it)
    type(itaylor), intent(inout) :: it
    it%tmp = .true.
  end subroutine it_set_temporary
  
  function it_order(it) result(res)
    type(itaylor), intent(in) :: it
    integer                   :: res
    
    if (defined(it)) then
      res = ubound(it%cof, 1)
    else
      res = not_defined
    end if
    call tmp_free(it)
  end function it_order

  function it_get_order() result(res)
    integer :: res
    res = max_ord
  end function it_get_order

  subroutine it_set_bounded_order(ord)
    integer, intent(in) :: ord
    if (ord >= 0) max_ord = ord
  end subroutine it_set_bounded_order

  subroutine it_set_unbounded_order()
    max_ord = -1
  end subroutine it_set_unbounded_order

  function it_essential_order(o1, o2) result(res)
    integer, intent(in)           :: o1
    integer, intent(in), optional :: o2
    integer                       :: res

    if (order_is_bounded()) then
      if (present(o2)) then
        res = min(max_ord, max(o1, o2))
      else
        res = min(max_ord, o1)
      end if
    else
      if (present(o2)) then
        res = max(o1, o2)
      else
        res = o1
      end if
    end if
  end function it_essential_order  

  function it_max_essential_order(o1, o2) result(res)
    integer, intent(in)           :: o1
    integer, intent(in), optional :: o2
    integer                       :: res

    if (order_is_bounded()) then
      res = max_ord
    else
      if (present(o2)) then
        res = max(o1, o2)
      else
        res = o1
      end if
    end if
  end function it_max_essential_order  
  
  function it_order_is_bounded() result(res)
    logical :: res
    res = max_ord >= 0
  end function it_order_is_bounded
  
  function it_order_is_unbounded() result(res)
    logical :: res
    res = max_ord < 0
  end function it_order_is_unbounded

  subroutine var_it_i(it, c)
    type(itaylor), intent(inout) :: it
    type(interval), intent(in)   :: c
    integer                      :: ord

    ord = essential_order(1)
    if (.not. defined(it)) call init(it, ord)
    
    it%tmp        = .false.
    it%cof        = izero
    it%cof(0:ord) = ione
    it%cof(0)     = c
    it%a          = c
  end subroutine var_it_i

  subroutine var_it_r(it, c)
    type(itaylor), intent(inout) :: it
    real(prec), intent(in)       :: c

    call expand(it, ival(c))
  end subroutine var_it_r

  function it_var_i(c) result(res)
    type(interval), intent(in) :: c
    type(itaylor)              :: res

    call expand(res, c)
    res%tmp = .true.
  end function it_var_i

  function it_var_r(c) result(res)
    real(prec), intent(in) :: c
    type(itaylor)          :: res

    call expand(res, ival(c))
    res%tmp = .true.
  end function it_var_r

  subroutine const_it_i(it, c)
    type(itaylor), intent(inout) :: it
    type(interval), intent(in)   :: c

    it%tmp = .false. ! TODO

    if (defined(it)) then
      it%cof(0:) = izero
      it%cof(0)  = c
      it%a       = izero
    else
      call init(it, 0)
      it%cof(0) = c
    end if
  end subroutine const_it_i

  subroutine const_it_r(it, c)
    type(itaylor), intent(inout) :: it
    real(prec), intent(in)       :: c

    call const(it, ival(c))
  end subroutine const_it_r

  function it_const_i(c) result(res)
    type(interval), intent(in) :: c
    type(itaylor)              :: res

    call const(res, c)
    res%tmp = .true.
  end function it_const_i

  function it_const_r(c) result(res)
    real(prec), intent(in) :: c
    type(itaylor)          :: res

    call const(res, ival(c))
    res%tmp = .true.
  end function it_const_r

  subroutine it_assign_it(l, e)
    type(itaylor), intent(inout) :: l
    type(itaylor), intent(in)    :: e
    integer                      :: ord

    !if (.not. defined(e)) ! TODO
    ord = essential_order(order(e))

    if (defined(l)) then ! copy, never link
      if (order(l) < order(e)) call init(l, ord)
      l%cof(ord: ) = izero
      l%cof(0:ord) = e%cof(0:ord)
    else ! link, if possible
         ! no, too dangerous
      call init(l, ord)
      l%cof(ord: ) = izero
      l%cof(0:ord) = e%cof(0:ord)
    end if
    l%a = e%a
    l%tmp = .false. ! TODO
    call tmp_free(e)
  end subroutine it_assign_it

  subroutine it_assign_i(l, i)
    type(itaylor), intent(inout) :: l
    type(interval), intent(in)   :: i

    l = const(i)
  end subroutine it_assign_i

  subroutine it_assign_r(l, r)
    type(itaylor), intent(inout) :: l
    real(prec), intent(in)       :: r

    l = const(ival(r))
  end subroutine it_assign_r

  function add_it(it) result(res)
    type(itaylor), intent(in) :: it
    type(itaylor)             :: res

    call init(res, order(it))
    res%cof = it%cof
    res%a   = it%a
    call tmp_free(it)
  end function add_it

  function it_add_it(l, r) result(res)
    type(itaylor), intent(in) :: l, r
    type(itaylor)             :: res
    integer :: ord, mino, i, ol, or

    ol = order(l); or = order(r)
    mino = min(ol, or)
    ord = essential_order(ol, or)

    if (order(res) < ord) call init(res, ord)

    ! in the case order(res) > ord
    res%cof(ord: ) = izero
    do i=0, mino
      res%cof(i) = l%cof(i) + r%cof(i)
    end do

    if (ol > or) then
      res%cof(mino+1:ol) = l%cof(mino+1:ol)
    else if (or > ol) then
      res%cof(mino+1:or) = r%cof(mino+1:or)
    end if

    res%tmp = .true.
    call tmp_free(l, r)
  end function it_add_it

  function it_add_i(l, r) result(res)
    type(itaylor), intent(in)  :: l
    type(interval), intent(in) :: r
    type(itaylor)              :: res

    res = l + const(r)
    
    res%tmp = .true.
    call tmp_free(l)
  end function it_add_i

  function it_add_r(l, r) result(res)
    type(itaylor), intent(in) :: l
    real(prec), intent(in)    :: r
    type(itaylor)             :: res

    res = l + const(ival(r))
    
    res%tmp = .true.
    call tmp_free(l)
  end function it_add_r

  function i_add_it(l, r) result(res)
    type(interval), intent(in) :: l
    type(itaylor), intent(in)  :: r
    type(itaylor)              :: res

    res = const(l) + r
    
    res%tmp = .true.
    call tmp_free(r)
  end function i_add_it

  function r_add_it(l, r) result(res)
    real(prec), intent(in)    :: l
    type(itaylor), intent(in) :: r
    type(itaylor)             :: res

    res = const(ival(l)) + r
    
    res%tmp = .true.
    call tmp_free(r)
  end function r_add_it

  function sub_it(it) result(res)
    type(itaylor), intent(in) :: it
    type(itaylor)             :: res
    integer :: i

    call init(res, order(it))
    do i=0, order(it)
      res%cof(i) = -it%cof(i)
    end do
    res%tmp = .true.
    call tmp_free(it)
  end function sub_it

  function it_sub_it(l, r) result(res)
    type(itaylor), intent(in) :: l, r
    type(itaylor)             :: res
    integer :: ord, mino, i, ol, or

    ol = order(l); or = order(r)
    mino = min(ol, or)
    ord = essential_order(ol, or)

    if (order(res) < ord) call init(res, ord)

    ! in the case order(res) > ord
    res%cof(ord: ) = izero
    do i=0, mino
      res%cof(i) = l%cof(i) - r%cof(i)
    end do

    if (ol > or) then
      res%cof(mino+1:ol) = l%cof(mino+1:ol)
    else if (or > ol) then
      do i = mino+1, or
        res%cof(i) = -r%cof(i)
      end do
    end if

    res%tmp = .true.
    call tmp_free(l, r)
  end function it_sub_it

  function it_sub_i(l, r) result(res)
    type(itaylor), intent(in)  :: l
    type(interval), intent(in) :: r
    type(itaylor)              :: res

    res = l - const(r)
    
    res%tmp = .true.
    call tmp_free(l)
  end function it_sub_i

  function it_sub_r(l, r) result(res)
    type(itaylor), intent(in) :: l
    real(prec), intent(in)    :: r
    type(itaylor)             :: res

    res = l - const(ival(r))
    
    res%tmp = .true.
    call tmp_free(l)
  end function it_sub_r

  function i_sub_it(l, r) result(res)
    type(interval), intent(in) :: l
    type(itaylor), intent(in)  :: r
    type(itaylor)              :: res

    res = const(l) - r
    
    res%tmp = .true.
    call tmp_free(r)
  end function i_sub_it

  function r_sub_it(l, r) result(res)
    real(prec), intent(in)    :: l
    type(itaylor), intent(in) :: r
    type(itaylor)             :: res

    res = const(ival(l)) - r
    
    res%tmp = .true.
    call tmp_free(r)
  end function r_sub_it

  function it_mul_it(l, r) result(res)
    type(itaylor), intent(in) :: l, r
    type(itaylor)             :: res
    integer :: ord, mino, k, ol, or

    ol = order(l); or = order(r)
    mino = min(ol, or)
    ord = essential_order(ol + or)

    if (order(res) < ord) call init(res, ord)

    do k=0, mino
      res%cof(k) = l%cof(0:k) .dot. r%cof(k:0:-1)
    end do

    do k=mino+1, ord
      res%cof(k) = l%cof(max(0,k-or) : min(ol,k)) .dot.  &
                   r%cof(min(or,k)   : max(0,k-ol) : -1)
    end do

    res%tmp = .true.
    call tmp_free(l, r)
  end function it_mul_it

  function it_mul_i(l, r) result(res)
    type(itaylor), intent(in)  :: l
    type(interval), intent(in) :: r
    type(itaylor)              :: res

    res = l * const(r)
    res%tmp = .true.
    call tmp_free(l)
  end function it_mul_i

  function it_mul_r(l, r) result(res)
    type(itaylor), intent(in) :: l
    real(prec), intent(in)    :: r
    type(itaylor)             :: res

    res = l * const(ival(r))
    res%tmp = .true.
    call tmp_free(l)
  end function it_mul_r

  function i_mul_it(l, r) result(res)
    type(interval), intent(in) :: l
    type(itaylor), intent(in)  :: r
    type(itaylor)              :: res

    res = const(l) * r
    res%tmp = .true.
    call tmp_free(r)
  end function i_mul_it

  function r_mul_it(l, r) result(res)
    real(prec), intent(in)    :: l
    type(itaylor), intent(in) :: r
    type(itaylor)             :: res

    res = const(ival(l)) * r
    res%tmp = .true.
    call tmp_free(r)
  end function r_mul_it

  function it_div_it(l, r) result(res)
    type(itaylor), intent(in) :: l, r
    type(itaylor)             :: res
    integer :: ord, mino, maxo, k, ol, or

    ol = order(l); or = order(r)
    mino = min(ol, or); maxo = max(or, ol)
    ord = max_essential_order(ol, or) ! = max_ord if ord is bounded

    if (order(res) < ord) call init(res, ord)

    ! both, l%cof(i) and r%cof(i) are defined
    do k=0, mino
      res%cof(k) = (l%cof(k) - (res%cof(0:k-1) .dot. r%cof(k:1:-1))) / r%cof(0)
    end do

    if (or > ol) then ! l%cof(i) is not defined (-> =0)
      do k=mino+1, maxo
        res%cof(k) = (       - (res%cof(0:k-1) .dot. r%cof(k:1:-1))) / r%cof(0)
      end do
    else if (ol > or) then
      do k=mino+1, maxo ! r%cof(i) is only for i<=or defined (/= 0)
        res%cof(k) = (l%cof(k) - (res%cof(k-or:k-1) .dot. r%cof(or:1:-1))) / r%cof(0)
      end do
    end if

    ! both, l%cof(i) and r%cof(i) are only defined (/= 0) for i<=ol,or
    do k=maxo+1, ord
      res%cof(k) = (           - (res%cof(k-or:k-1) .dot. r%cof(or:1:-1))) / r%cof(0)
    end do

    res%tmp = .true.
    call tmp_free(l, r)
  end function it_div_it

  function it_div_i(l, r) result(res)
    type(itaylor), intent(in)  :: l
    type(interval), intent(in) :: r
    type(itaylor)              :: res

    res = l / const(r)
    res%tmp = .true.
    call tmp_free(l)
  end function it_div_i

  function it_div_r(l, r) result(res)
    type(itaylor), intent(in) :: l
    real(prec), intent(in)    :: r
    type(itaylor)             :: res

    res = l / const(ival(r))
    res%tmp = .true.
    call tmp_free(l)
  end function it_div_r

  function i_div_it(l, r) result(res)
    type(interval), intent(in) :: l
    type(itaylor), intent(in)  :: r
    type(itaylor)              :: res

    res = const(l) / r
    res%tmp = .true.
    call tmp_free(r)
  end function i_div_it

  function r_div_it(l, r) result(res)
    real(prec), intent(in)    :: l
    type(itaylor), intent(in) :: r
    type(itaylor)             :: res

    res = const(ival(l)) / r
    res%tmp = .true.
    call tmp_free(r)
  end function r_div_it

  function it_eval(it) result(res)
    type(itaylor), intent(in) :: it
    type(interval)            :: res

    res = it%cof(0)
    call tmp_free(it)
  end function it_eval

  function get_taylor_coefficient(it, k) result(res)
    type(itaylor), intent(in) :: it
    integer, intent(in)       :: k
    type(interval)            :: res

    if (order(it) < k) then
      res = izero
    else
      res = it%cof(k)
    end if
    call tmp_free(it)
  end function get_taylor_coefficient
  
  !> output of it to stdout
  subroutine it_put(it)
    type(itaylor), intent(in) :: it
    integer                   :: i

    do i=0, order(it)
      call put(it%cof(i))
    end do
    call tmp_free(it)
  end subroutine it_put

! elemental functions

  !> it for square-root
  function it_sqrt(x) result(res)
    use fi_lib, only : xisqrt
    type(itaylor), intent(in) :: x
    type(itaylor)             :: res
    integer :: ord, ox, k, ko, ke

    ox = order(x)
    ord = max_essential_order(ox)
    ke = ord+1
    
    if (order(res) < ord) call init(res, ord)

    res%cof(0)        = xisqrt(x%cof(0))
    res%cof(1:ox)     = x%cof(1:ox)    ! because x is perhaps not defined for
    res%cof(ox+1:ord) = izero          ! all k=1..ord

    do ko=1, ord-1, 2
      k = (ko-1) / 2            ! res%cof(k_odd)
      res%cof(ko) = (res%cof(ko) - ival(2) * ( res%cof(1    : k) .dot.    &
                                               res%cof(ko-1 : ko-k : -1)) &
                    ) / (itwo * res%cof(0))

      ke = ko+1; k = (ke / 2) ! res%cof(k_even)
      res%cof(ke) = (res%cof(ke) - res%cof(k) * res%cof(k)               &
                                 - ival(2) * ( res%cof(1    : k-1) .dot. &
                                               res%cof(ke-1 : k+1 : -1)) &
                    ) / (itwo * res%cof(0))
    end do
    if (ord > ke) then ! ord is odd
      ko = ord; k = (ord-1)           ! res%cof(k_odd)
      res%cof(ko) = (res%cof(ko) - ival(2) * ( res%cof(1    : k) .dot.    &
                                               res%cof(ko-1 : ko-k : -1)) &
                     ) / (itwo * res%cof(0))
    end if

    res%tmp = .true.
    call tmp_free(x)
  end function it_sqrt

  !> it for square-root
  !! only for testing it_sqrt
  function it_sqrt_simple(x) result(res)
    use fi_lib, only : xisqrt
    type(itaylor), intent(in) :: x
    type(itaylor)             :: res
    integer :: ord, ox, k

    ox = order(x)
    ord = max_essential_order(ox) ! = max_ord if ord is bounded
    
    if (order(res) < ord) call init(res, ord)

    res%cof(0)        = xisqrt(x%cof(0))
    res%cof(1:ox)     = x%cof(1:ox)    ! because x is perhaps not defined for
    res%cof(ox+1:ord) = izero          ! all k=1..ord

    do k=1, ord
      res%cof(k) = (res%cof(k) - ( res%cof(1   : k-1) .dot. &
                                   res%cof(k-1 : 1 : -1)) &
                    ) / (itwo * res%cof(0))
    end do

    res%tmp = .true.
    call tmp_free(x)
  end function it_sqrt_simple
  
  !> it for square
  function it_sqr(x) result(res)
    use fi_lib, only : xisqr
    type(itaylor), intent(in) :: x
    type(itaylor)             :: res
    integer :: ord, ox, k, ko, ke

    ox = order(x)
    ord = essential_order(2*ox) ! = max_ord if ord is bounded
    ke = ord+1

    if (order(res) < ord) call init(res, ord)

    res%cof(0) = xisqr(x%cof(0))
    do ko=1, ord-1, 2
      k = (ko-1) / 2        ! res(k_odd)
      res%cof(ko) = ival(2) * ( x%cof(max(0,ko-ox) : k) .dot. &
                                x%cof(min(ox,ko)   : ko-k : -1))
      
      ke = ko+1; k = (ke / 2) ! res(k_even)
      res%cof(ke) = ival(2) * ( x%cof(max(0,ke-ox) : k-1) .dot. &
                                x%cof(min(ox,ke)   : k+1 : -1)) &
                    + (x%cof(k)) * x%cof(k)
    end do
    if (ord > ke) then ! ord is odd
      ko= ord; k = (ko-1) / 2 ! res(k_odd)
      res%cof(ko) = ival(2) * ( x%cof(max(0,ko-ox) : k) .dot. &
                                x%cof(min(ox,ko)   : ko-k : -1))
    end if
    res%tmp = .true.
    call tmp_free(x)
  end function it_sqr

  !> it for square
  !! straight forward implementation
  !! only for testing of it_sqr
  function it_sqr_simple(x) result(res)
    use fi_lib, only : xisqr
    type(itaylor), intent(in) :: x
    type(itaylor)             :: res
    integer :: k, ord, ox

    ox = order(x)
    ord = essential_order(2*ox)

    if (order(res) < ord) call init(res, ord)

    res%cof(0) = xisqr(x%cof(0))
    do k=1, ord
      res%cof(k) = x%cof(max(0,k-ox) : min(ox,k)) .dot. &
                   x%cof(min(ox,k)   : max(0,k-ox):-1)
    end do

    res%tmp = .true.
    call tmp_free(x)
  end function it_sqr_simple
 
  !> it for logarithm for base 2
  function it_log2(x) result(res)
    use fi_lib, only : xilog2, xilog
    type(itaylor), intent(in) :: x
    type(itaylor)             :: res
    type(itaylor)             :: tmp
    type(interval)            :: fac
    integer :: ord, ox, k

    ord = max_essential_order(ox) ! = max_ord if ord is bounded

    if (order(res) < ord) call init(res, ord)

    call init(tmp, ord)
    tmp = it_log(x)
    fac = ione / xilog(itwo)
    res%cof(0) = xilog2(x%cof(0))
    do k = 1, ord
      res%cof(k) = fac * tmp%cof(k)
    end do

    res%tmp = .true.
    call tmp_free(x)
  end function it_log2

  !> it for logarithm to base 10
  function it_log10(x) result(res)
    use fi_lib, only : xilog, xilog10
    type(itaylor), intent(in) :: x
    type(itaylor)             :: res
    type(itaylor)             :: tmp
    type(interval)            :: fac
    integer :: ord, ox, k

    ord = max_essential_order(ox) ! = max_ord if ord is bounded

    if (order(res) < ord) call init(res, ord)

    call init(tmp, ord)
    tmp = it_log(x)
    fac = ione / xilog(ival(10))
    res%cof(0) = xilog10(x%cof(0))
    do k = 1, ord
      res%cof(k) = fac * tmp%cof(k)
    end do

    res%tmp = .true.
    call tmp_free(x)
  end function it_log10

  !> it for natural logarithm
  !! using it_trig
  function it_log(x) result(res)
    use fi_lib, only : xilog
    type(itaylor), intent(in) :: x
    type(itaylor)             :: res

    res = it_trig(x, xilog, x)

    res%tmp = .true.
    call tmp_free(x)
  end function it_log

  !> it for exponential
  !> TODO  generalizable for general base: a^it
  function it_exp(x) result(res)
    use fi_lib, only : xiexpo
    type(itaylor), intent(in) :: x
    type(itaylor)             :: res
    integer :: ord, ox, k, j
  
    ox = order(x)
    ord = max_essential_order(ox) ! = max_ord if ord is bounded

    if (order(res) < ord) call init(res, ord)

    res%cof(0) = xiexpo(x%cof(0))
    do k=1, ord
      res%cof(k) = izero
      do j=max(0, k-ox), k-1
        res%cof(k) = res%cof(k) + ival(k-j) * res%cof(j) * x%cof(k-j)
      end do
      res%cof(k) = res%cof(k) / ival(k)
    end do
    res%tmp = .true.
    call tmp_free(x)
  end function it_exp

  !> general algorithm for sin, cos, sinh and cosh
  !> \param x    ... argument it
  !> \param sig  ... has to be wether plus or minus [1] according to
  !!                 the derivation of cfun: D(sfun) = sig*sfun
  !> \param si  ... result it for sin or sinh
  !> \param co  ... result it for cos or cosh
  !> \param sfun ... wether sin or sinh
  !> \param cfun ... wether cos or cosh
  subroutine it_si_co(x, sig, si, co, sfun, cfun)
    type(itaylor), intent(in)    :: x
    type(interval), intent(in)   :: sig
!    procedure(imapping)          :: sfun, cfun
    interface 
      function sfun(x) result(res) bind(C)
        use ivalarith, only : interval
        type(interval), value, intent(in) :: x
        type(interval)                    :: res
      end function sfun
    end interface
    interface 
      function cfun(x) result(res) bind(C)
        use ivalarith, only : interval
        type(interval), value, intent(in) :: x
        type(interval)                    :: res
      end function cfun
    end interface
    type(itaylor), intent(inout) :: si, co
    integer :: ord, ox, k, j

    ! assert abs(sig) = one

    ox = order(x)
    ord = max_essential_order(ox) ! = max_ord if ord is bounded

    if (order(si) < ord) call init(si, ord)
    if (order(co) < ord) call init(co, ord)
    
    si%cof(0) = sfun(x%cof(0))
    co%cof(0) = cfun(x%cof(0))
    do k=1, ord
      si%cof(k) = izero
      co%cof(k) = izero
      do j=max(0, k-ox), k-1
        si%cof(k) = si%cof(k) + ival(k-j) * co%cof(j) * x%cof(k-j)
        co%cof(k) = co%cof(k) + ival(k-j) * si%cof(j) * x%cof(k-j)
      end do
      si%cof(k) = si%cof(k) / ival(k)
      co%cof(k) = sig * co%cof(k) / ival(k)
    end do
    si%tmp = .false.
    co%tmp = .false.
    call tmp_free(x)
  end subroutine it_si_co

  !> it for sinus
  !! using it_si_co
  function it_sin(x) result(res)
    use fi_lib, only : xisin, xicos
    type(itaylor), intent(in) :: x
    type(itaylor)             :: res, tmp

    call it_si_co(x, -ione, res, tmp, xisin, xicos)
    
    res%tmp = .true.
    call tmp_free(x); call free(tmp)
  end function it_sin

  !> it for cosinus
  !! using it_si_co
  function it_cos(x) result(res)
    use fi_lib, only : xisin, xicos
    type(itaylor), intent(in) :: x
    type(itaylor)             :: res, tmp

    call it_si_co(x, -ione, tmp, res, xisin, xicos)
    
    res%tmp = .true.
    call tmp_free(x); call free(tmp)
  end function it_cos

  !> it for tangens
  !! using it_trig
  function it_tan(x) result(res)
    use fi_lib, only : xitan
    type(itaylor), intent(in) :: x
    type(itaylor)             :: res, xt

    xt = x
    res = it_trig(xt, xitan, sqr(cos(xt)))

    res%tmp = .true.
    call tmp_free(x); call free(xt)
  end function it_tan

  !> it for cotangens
  !! using it_trig
  function it_cot(x) result(res)
    use fi_lib, only : xicot
    type(itaylor), intent(in) :: x
    type(itaylor)             :: res, xt

    xt = x
    res = it_trig(xt, xicot, -sqr(sin(xt)))

    res%tmp = .true.
    call tmp_free(x); call free(xt)
  end function it_cot

  !> it for arcus sinus
  !! using it_trig
  function it_asin(x) result(res)
    use fi_lib, only : xiasin
    type(itaylor), intent(in) :: x
    type(itaylor)             :: res, xt

    xt = x
    res = it_trig(xt, xiasin, sqrt(ione - sqr(xt)))

    res%tmp = .true.
    call tmp_free(x); call free(xt)
  end function it_asin

  !> it for arcus sinus
  !! straight forward implementation
  !! for testing it_trig
  function it_asin_simple(x) result(res)
    use fi_lib, only : xiasin
    type(itaylor), intent(in) :: x
    type(itaylor)             :: res, g, xt
    integer :: k, j, ord, ox, og

    xt = x
    g  = sqrt(ione - sqr(xt))
    ox = order(x); og = order(g)
    ord = max_essential_order(ox) ! = max_ord if ord is bounded

    if (order(res) < ord) call init(res, ord)

    res%cof(0) = xiasin(xt%cof(0))
    do k=1, ox
      res%cof(k) = izero
      do j=max(1, k-og), k-1
        res%cof(k) = res%cof(k) + ival(j) * res%cof(j) * g%cof(k-j)
      end do
      res%cof(k) = ( xt%cof(k) - res%cof(k) / ival(k) ) / g%cof(0)
    end do

    do k=ox+1, ord
      res%cof(k) = izero
      do j=max(1, k-og), k-1
        res%cof(k) = res%cof(k) + ival(j) * res%cof(j) * g%cof(k-j)
      end do
      res%cof(k) = (            - res%cof(k) / ival(k) ) / g%cof(0)
    end do

    res%tmp = .true.
    call tmp_free(x, g); call free(xt)
  end function it_asin_simple

  !> it for arcus cosinus
  !! using it_trig
  function it_acos(x) result(res)
    use fi_lib, only : xiacos
    type(itaylor), intent(in) :: x
    type(itaylor)             :: res, xt

    xt = x
    res = it_trig(xt, xiacos, -sqrt(ione - sqr(xt)))

    res%tmp = .true.
    call tmp_free(x); call free(xt)
  end function it_acos

  !> it for arcus tangens
  !! usinf it_trig
  function it_atan(x) result(res)
    use fi_lib, only : xiatan
    type(itaylor), intent(in) :: x
    type(itaylor)             :: res, xt

    xt = x
    res = it_trig(xt, xiatan, ione + sqr(xt))

    res%tmp = .true.
    call tmp_free(x); call free(xt)
  end function it_atan

  !> it for arcus cotangens
  !! usinf it_trig
  function it_acot(x) result(res)
    use fi_lib, only : xiacot
    type(itaylor), intent(in) :: x
    type(itaylor)             :: res, xt

    xt = x
    res = it_trig(xt, xiacot, -(ione + sqr(xt)))

    res%tmp = .true.
    call tmp_free(x); call free(xt)
  end function it_acot

  !> it for sinus hyperbolicus
  !! using it_si_co
  function it_sinh(x) result(res)
    use fi_lib, only : xisinh, xicosh
    type(itaylor), intent(in) :: x
    type(itaylor)             :: res, tmp

    call it_si_co(x, ione, res, tmp, xisinh, xicosh)
    
    res%tmp = .true.
    call tmp_free(x); call free(tmp)
  end function it_sinh

  !> it for cosinus hyperbolicus
  !! using it_si_co
  function it_cosh(x) result(res)
    use fi_lib, only : xisinh, xicosh
    type(itaylor), intent(in) :: x
    type(itaylor)             :: res, tmp

    call it_si_co(x, ione, tmp, res, xisinh, xicosh)
    
    res%tmp = .true.
    call tmp_free(x); call free(tmp)
  end function it_cosh

  !> it for tangens hyperbolicus
  !! using it_trig
  function it_tanh(x) result(res)
    use fi_lib, only : xitanh
    type(itaylor), intent(in) :: x
    type(itaylor)             :: res, xt

    xt = x
    res = it_trig(xt, xitanh, sqr(cosh(xt)))

    res%tmp = .true.
    call tmp_free(x); call free(xt)
  end function it_tanh

  !> it for cotangens hyperbolicus
  !! using it-trig
  function it_coth(x) result(res)
    use fi_lib, only : xicoth
    type(itaylor), intent(in) :: x
    type(itaylor)             :: res, xt

    xt = x
    res = it_trig(xt, xicoth, -sqr(sinh(xt)))

    res%tmp = .true.
    call tmp_free(x); call free(xt)
  end function it_coth

  !> it for area sinus hyperbolicus
  !! using it-trig
  function it_asinh(x) result(res)
    use fi_lib, only : xiasinh
    type(itaylor), intent(in) :: x
    type(itaylor)             :: res, xt

    xt = x
    res = it_trig(xt, xiasinh, sqrt(sqr(xt) + ione))

    res%tmp = .true.
    call tmp_free(x); call free(xt)
  end function it_asinh

  !> it for area cosinus hyperbolicus
  !! using it-trig
  function it_acosh(x) result(res)
    use fi_lib, only : xiacosh
    type(itaylor), intent(in) :: x
    type(itaylor)             :: res, xt

    xt = x
    res = it_trig(xt, xiacosh, sqrt(sqr(xt) - ione))

    res%tmp = .true.
    call tmp_free(x); call free(xt)
  end function it_acosh

  !> it for area tangens hyperbolicus
  !! using it-trig
  function it_atanh(x) result(res)
    use fi_lib, only : xiatanh
    type(itaylor), intent(in) :: x
    type(itaylor)             :: res, xt

    xt = x
    res = it_trig(xt, xiatanh, ione - sqr(xt))

    res%tmp = .true.
    call tmp_free(x); call free(xt)
  end function it_atanh

  !> it for area cotangens hyperbolicus
  !! using it-trig
  function it_acoth(x) result(res)
    use fi_lib, only : xiacoth
    type(itaylor), intent(in) :: x
    type(itaylor)             :: res, xt

    xt = x
    res = it_trig(xt, xiacoth, ione - sqr(xt))

    res%tmp = .true.
    call tmp_free(x); call free(xt)
  end function it_acoth

  !> generalized algorithm for the computation of the taylor components of 
  !! several elemental functions: log, tan, cot, asin, acos, atan, acot, 
  !! tanh, coth, asinh, acosh, atanh, acoth
  !! it needs only the individual it of g := 1/f'(x)
  !> \param x ... expansion point
  !> \param f ... function to calculate taylor coef.
  !> \param g ... 1 / f'(x)
  function it_trig(x, f, g) result(res)
    type(itaylor), intent(in)  :: x, g
!    procedure(imapping)        :: f
    interface 
      function f(x) result(res) bind(C)
        use ivalarith, only : interval
        type(interval), value, intent(in) :: x
        type(interval)                    :: res
      end function f
    end interface

    type(itaylor)              :: res
    integer :: k, j, ord, ox, og

    ox = order(x); og = order(g)
    ord = max_essential_order(ox) ! = max_ord if ord is bounded

    if (order(res) < ord) call init(res, ord)

    res%cof(0) = f(x%cof(0))
    do k=1, ox
      res%cof(k) = izero
      do j=max(1, k-og), k-1
        res%cof(k) = res%cof(k) + ival(j) * res%cof(j) * g%cof(k-j)
      end do
      res%cof(k) = ( x%cof(k) - res%cof(k) / ival(k) ) / g%cof(0)
    end do

    do k=ox+1, ord
      res%cof(k) = izero
      do j=max(1, k-og), k-1
        res%cof(k) = res%cof(k) + ival(j) * res%cof(j) * g%cof(k-j)
      end do
      res%cof(k) = (            - res%cof(k) / ival(k) ) / g%cof(0)
    end do

    res%tmp = .true.
    call tmp_free(x, g)
  end function it_trig

  !> delivers n-th Taylor-coefficient of f in x0
  function T(f, x0, n) result(res)
    procedure(itmapping)       :: f
    type(interval), intent(in) :: x0
    integer, intent(in)        :: n
    integer                    :: old_order
    type(interval)             :: res

    ! recover the old order
    old_order = get_order()
    call set_bounded_order(n)
    res = get_taylor_coefficient(f(x0), n)
    call set_bounded_order(old_order)
  end function T

  !> delivers the value of the n-th derivative of f in x0
  function D(f, x0, n) result(res)
    procedure(itmapping)       :: f
    type(interval), intent(in) :: x0
    integer, intent(in)        :: n
    integer                    :: old_order
    type(interval)             :: res

    ! recover the old order
    old_order = get_order()
    call set_bounded_order(n)
    res = real(fac(n), prec) * get_taylor_coefficient(f(x0), n)
    call set_bounded_order(old_order)
  end function D

  !> evaluates the given taylor-coefficients as polynomial expansion
  !! needs the original expansion point a
  function P_horner(taylor, x) result(res)
    type(itaylor), intent(in)  :: taylor
    type(interval), intent(in) :: x
    type(interval)             :: res
    integer                    :: i, ot
    type(interval)             :: H

    ot = order(taylor)
    H = x - taylor%a ! centring of x
    res = taylor%cof(ot)
    do i=ot-1, 0, -1
      res = res * x + taylor%cof(i)
    end do
  end function P_horner

  ! factorial with 64 Bit
  function fac(n) result(res)
    integer, intent(in) :: n
    integer(i64)        :: res, i

    res = 1
    do i=1, n
      res = res * i
    end do
  end function fac
end module ivaltaylor
