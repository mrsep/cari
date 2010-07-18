program ulp_prog
use cari
implicit none

real(kind=sp) :: s
real(kind=dp) :: d
real(kind=qp) :: q

do while (q /= 42.0)
  write(*,*) 'Bitte geben Sie die zu analysierende Gleitkommazahl ein.'
  read(*,*) q
  s = q; d = q

  write(*,*)
  write(*,*) ' === Analyse mit Single Precision ==='
  call put( s,           ' Value:    ')
  call put( fraction(s), ' Mantisse: ')
  write(*,*) 'Exponent: ', exponent(s)
  call put( ulp(s),      ' ulp(x):   ')
  call put( pred(s),     ' pred(x):  ')
  call put( succ(s),     ' succ(x):  ')
 
  write(*,*)
  write(*,*) ' === Analyse mit Double Precision ==='
  call put( d,           ' Value:    ')
  call put( fraction(d), ' Mantisse: ')
  write(*,*) 'Exponent: ', exponent(d)
  call put( ulp(d),      ' ulp(x):   ')
  call put( pred(d),     ' pred(x):  ')
  call put( succ(d),     ' succ(x):  ')
!  call put( d*(one+ulp(one)), ' x*(1+e):  ')
!  call put( d+ulp(d),         ' x+ulp(x): ')
!  call put( d*(one-ulp(one)), ' x*(1-e):  ')
!  call put( d-ulp(d),         ' x-ulp(x): ')

  write(*,*)
  write(*,*) ' === Analyse mit Quadruple Precision ==='
  call put( q,           ' Value:    ')
  call put( fraction(q), ' Mantisse: ')
  write(*,*) 'Exponent: ', exponent(q)
  call put( ulp(q),      ' ulp(x):   ')
  call put( pred(q),     ' pred(x):  ')
  call put( succ(q),     ' succ(x):  ')
end do
end program ulp_prog

