program ulp_prog
use cari
implicit none

real(kind=sp) :: s
real(kind=dp) :: d
real(kind=qp) :: q

integer, parameter :: verbose_level = 0

  do while (q /= 42.0)
    write(*,*) 'Bitte geben Sie die zu analysierende Gleitkommazahl ein.'
    read(*,*) q
    s = q; d = q

    write(*,*)
    write(*,*) ' === Analyse mit Single Precision ==='
    call put( s,           ' Value:    ')
    call put( fraction(s), ' Mantisse: ')
    write(*,*)             'Exponent: ', exponent(s)
    call put( ulp(s),      ' ulp(x):   ')

    if (verbose_level >=1) then
      call put( succ(s),                  ' succ(x):  ')
      if (verbose_level >=2) then
        call put( s*(1.0_sp+ulp(1.0_sp)), ' x*(1+e):  ')
        call put( s+ulp(s),               ' x+ulp(x): ')
      end if
      call put( pred(s),                  ' pred(x):  ')
      if (verbose_level >=2) then
        call put( s*(1.0_sp-ulp(1.0_sp)), ' x*(1-e):  ')
        call put( s-ulp(s),               ' x-ulp(x): ')
      end if
    end if

   
    write(*,*)
    write(*,*) ' === Analyse mit Double Precision ==='
    call put( d,                ' Value:    ')
    call put( fraction(d),      ' Mantisse: ')
    write(*,*)                  'Exponent: ', exponent(d)
    call put( ulp(d),           ' ulp(x):   ')

    if (verbose_level >=1) then
      call put( succ(d),          ' succ(x):  ')
      if (verbose_level >=2) then
        call put( d*(one+ulp(one)), ' x*(1+e):  ')
        call put( d+ulp(d),         ' x+ulp(x): ')
      end if
      call put( pred(d),          ' pred(x):  ')
      if (verbose_level >=2) then
        call put( d*(one-ulp(one)), ' x*(1-e):  ')
        call put( d-ulp(d),         ' x-ulp(x): ')
      end if
    end if

    write(*,*)
    write(*,*) ' === Analyse mit Quadruple Precision ==='
    call put( q,           ' Value:    ')
    call put( fraction(q), ' Mantisse: ')
    write(*,*)             'Exponent: ', exponent(q)
    call put( ulp(q),      ' ulp(x):   ')

    if (verbose_level >=1) then
      call put( succ(q),                  ' succ(x):  ')
      if (verbose_level >=2) then
        call put( q*(1.0_qp+ulp(1.0_qp)), ' x*(1+e):  ')
        call put( q+ulp(q),               ' x+ulp(x): ')
      end if
      call put( pred(q),                  ' pred(x):  ')
      if (verbose_level >=2) then
        call put( q*(1.0_qp-ulp(1.0_qp)), ' x*(1-e):  ')
        call put( q-ulp(q),               ' x-ulp(x): ')
      end if
    end if

  end do
end program ulp_prog
