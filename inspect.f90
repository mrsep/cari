program inspectfp
use cari
use viscari
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
  write(*,*) 'Value:   ', s
  write(*,*) 'Mantisse: ', fraction(s)
  write(*,*) 'Exponent: ', exponent(s)
  write(*,*) 'ulp(x):   ', ulp(s)
  call bin_print(s)
  
  write(*,*)
  write(*,*) ' === Analyse mit Double Precision ==='
  write(*,*) 'Value:   ', d
  write(*,*) 'Mantisse: ', fraction(d)
  write(*,*) 'Exponent: ', exponent(d)
  write(*,*) 'ulp(x):   ', ulp(d)
  call bin_print(d)

  write(*,*)
  write(*,*) ' === Analyse mit Quadruple Precision ==='
  write(*,*) 'Value:   ', q
  write(*,*) 'Mantisse: ', fraction(q)
  write(*,*) 'Exponent: ', exponent(q)
  write(*,*) 'ulp(x):   ', ulp(q)
  call bin_print(q)
  write(*,*)
end do
end program inspectfp

