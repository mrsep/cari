! Numerical inquiry functions for intrinsic REAL types: 
!
    PROGRAM realinq

      IMPLICIT NONE

      INTEGER, PARAMETER :: sp = kind(1.0)
      INTEGER, PARAMETER :: dp = kind(1.0D0)
      INTEGER, PARAMETER :: xp = SELECTED_REAL_KIND(18,1000)
      INTEGER, PARAMETER :: qp = SELECTED_REAL_KIND(33,1000)

      REAL (KIND=sp) :: s     ! single precision
      REAL (KIND=dp) :: d     ! double precision
      REAL (KIND=xp) :: x     ! double extended or quadruple precision
      REAL (KIND=qp) :: q     ! double extended or quadruple precision

      WRITE(*,*)
      WRITE(*,*) ' ===== Single Precision REAL Type (Default) ==== '
      WRITE(*,*) 'KIND(s) : ', kind(s)
      WRITE(*,*) 'RADIX(s) : ', radix(s)
      WRITE(*,*) 'DIGITS(s) : ', digits(s)
      WRITE(*,*) 'MINEXPONENT(s) : ', minexponent(s)
      WRITE(*,*) 'MAXEXPONENT(s) : ', maxexponent(s)
      WRITE(*,*) 'HUGE(s) : ', huge(s)
      WRITE(*,*) 'TINY(s) : ', tiny(s)
      WRITE(*,*) 'EPSILON(s) : ', epsilon(s)
      WRITE(*,*) 'PRECISION(s) : ', precision(s)
      WRITE(*,*) 'RANGE(s) : ', range(s)
      WRITE(*,*)
      WRITE(*,*) ' ===== Double Precision REAL Type ===== '
      WRITE(*,*) 'KIND(d) : ', kind(d)
      WRITE(*,*) 'RADIX(d) : ', radix(d)
      WRITE(*,*) 'DIGITS(d) : ', digits(d)
      WRITE(*,*) 'MINEXPONENT(d) : ', minexponent(d)
      WRITE(*,*) 'MAXEXPONENT(d) : ', maxexponent(d)
      WRITE(*,*) 'HUGE(d) : ', huge(d)
      WRITE(*,*) 'TINY(d) : ', tiny(d)
      WRITE(*,*) 'EPSILON(d) : ', epsilon(d)
      WRITE(*,*) 'PRECISION(d) : ', precision(d)
      WRITE(*,*) 'RANGE(d) : ', range(d)
      WRITE(*,*)
      WRITE(*,*) ' ===== Extended or Quadruple Precision REAL Type ===== '
      WRITE(*,*) 'KIND(x) : ', kind(x)
      WRITE(*,*) 'RADIX(x) : ', radix(x)
      WRITE(*,*) 'DIGITS(x) : ', digits(x)
      WRITE(*,*) 'MINEXPONENT(x) : ', minexponent(x)
      WRITE(*,*) 'MAXEXPONENT(x) : ', maxexponent(x)
      WRITE(*,*) 'HUGE(x) : ', huge(x)
      WRITE(*,*) 'TINY(x) : ', tiny(x)
      WRITE(*,*) 'EPSILON(x) : ', epsilon(x)
      WRITE(*,*) 'PRECISION(x) : ', precision(x)
      WRITE(*,*) 'RANGE(x) : ', range(x)
      WRITE(*,*)
      WRITE(*,*) ' ===== Extended or Quadruple Precision REAL Type ===== '
      WRITE(*,*) 'KIND(x) : ', kind(q)
      WRITE(*,*) 'RADIX(x) : ', radix(q)
      WRITE(*,*) 'DIGITS(x) : ', digits(q)
      WRITE(*,*) 'MINEXPONENT(x) : ', minexponent(q)
      WRITE(*,*) 'MAXEXPONENT(x) : ', maxexponent(q)
      WRITE(*,*) 'HUGE(x) : ', huge(q)
      WRITE(*,*) 'TINY(x) : ', tiny(q)
      WRITE(*,*) 'EPSILON(x) : ', epsilon(q)
      WRITE(*,*) 'PRECISION(x) : ', precision(q)
      WRITE(*,*) 'RANGE(x) : ', range(q)
      WRITE(*,*)
    END PROGRAM realinq

