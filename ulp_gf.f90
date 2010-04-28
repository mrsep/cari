program ulp_prog
use cari
implicit none

real(kind=sp) :: s
real(kind=dp) :: d
real(kind=ep) :: e

do while (e /= 42)
  write(*,*) 'Bitte geben Sie die zu analysierende Gleitkommazahl ein.'
  read(*,*) e
  s = e; d = e

  write(*,*)
  write(*,*) ' === Analyse mit Single Precision ==='
  write(*,*) 'Value:   ', s
  write(*,*) 'Mantisse: ', fraction(s)
  write(*,*) 'Exponent: ', exponent(s)
  write(*,*) 'ulp(x):   ', ulp(s)
  
  write(*,*)
  write(*,*) ' === Analyse mit Double Precision ==='
  write(*,*) 'Value:   ', d
  write(*,*) 'Mantisse: ', fraction(d)
  write(*,*) 'Exponent: ', exponent(d)
  write(*,*) 'ulp(x):   ', ulp(d)

  write(*,*)
  write(*,*) ' === Analyse mit Extended Precision ==='
  write(*,*) 'Value:   ', e
  write(*,*) 'Mantisse: ', fraction(e)
  write(*,*) 'Exponent: ', exponent(e)
  write(*,*) 'ulp(x):   ', ulp(e)
end do
end program ulp_prog

