!> Numerical inquiry functions for intrinsic REAL types: 
program realinq
use cari
implicit none

real(sp) :: s
real(dp) :: d
real(qp) :: q

  write(*,*)
  write(*,*) ' ===== Single Precision REAL Type (Default) ==== '
  write(*,*) 'KIND(s):   ', kind(s)
  write(*,*) 'RADIX(s):  ', radix(s)
  write(*,*) 'DIGITS(s): ', digits(s)
  write(*,*) 'MINEXPONENT(s): ', minexponent(s)
  write(*,*) 'MAXEXPONENT(s): ', maxexponent(s)
  write(*,*) 'HUGE(s):      ', huge(s)
  write(*,*) 'TINY(s):      ', tiny(s)
  write(*,*) 'EPSILON(s):   ', epsilon(s)
  write(*,*) 'PRECISION(s): ', precision(s)
  write(*,*) 'RANGE(s):     ', range(s)
  write(*,*)
  write(*,*) ' ===== Double Precision REAL Type ===== '
  write(*,*) 'KIND(d):   ', kind(d)
  write(*,*) 'RADIX(d):  ', radix(d)
  write(*,*) 'DIGITS(d): ', digits(d)
  write(*,*) 'MINEXPONENT(d): ', minexponent(d)
  write(*,*) 'MAXEXPONENT(d): ', maxexponent(d)
  write(*,*) 'HUGE(d):      ', huge(d)
  write(*,*) 'TINY(d):      ', tiny(d)
  write(*,*) 'EPSILON(d):   ', epsilon(d)
  write(*,*) 'PRECISION(d): ', precision(d)
  write(*,*) 'RANGE(d):     ', range(d)
  write(*,*)
  write(*,*) ' ===== Extended or Quadruple Precision REAL Type ===== '
  write(*,*) 'KIND(x):   ', kind(q)
  write(*,*) 'RADIX(x):  ', radix(q)
  write(*,*) 'DIGITS(x): ', digits(q)
  write(*,*) 'MINEXPONENT(x): ', minexponent(q)
  write(*,*) 'MAXEXPONENT(x): ', maxexponent(q)
  write(*,*) 'HUGE(x):      ', huge(q)
  write(*,*) 'TINY(x):      ', tiny(q)
  write(*,*) 'EPSILON(x):   ', epsilon(q)
  write(*,*) 'PRECISION(x): ', precision(q)
  write(*,*) 'RANGE(x):     ', range(q)
  write(*,*)
end program realinq
