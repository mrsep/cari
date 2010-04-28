! Numerical inquiry functions for intrinsic INTEGER types: 
!
    PROGRAM intgrinq

      IMPLICIT NONE

      INTEGER, PARAMETER :: byte = SELECTED_INT_KIND(2)
      INTEGER, PARAMETER :: half = SELECTED_INT_KIND(4)
      INTEGER, PARAMETER :: full = SELECTED_INT_KIND(9)
      INTEGER, PARAMETER :: dble = SELECTED_INT_KIND(18)

      INTEGER (KIND=byte) :: b     ! 1-byte integer
      INTEGER (KIND=half) :: h     ! 2-byte integer
      INTEGER (KIND=full) :: f     ! 4-byte integer
      INTEGER (KIND=dble) :: d     ! 8-byte integer

      WRITE(*,*)
      WRITE(*,*) ' ===== 1-Byte INTEGER Type ==== '
      WRITE(*,*) 'KIND(b) : ', kind(b)
      WRITE(*,*) 'RADIX(b) : ', radix(b)
      WRITE(*,*) 'DIGITS(b) : ', digits(b)
      WRITE(*,*) 'HUGE(b) : ', huge(b)
      WRITE(*,*) 'RANGE(b) : ', range(b)
      WRITE(*,*)
      WRITE(*,*) ' ===== 2-Byte INTEGER Type ==== '
      WRITE(*,*) 'KIND(h) : ', kind(h)
      WRITE(*,*) 'RADIX(h) : ', radix(h)
      WRITE(*,*) 'DIGITS(h) : ', digits(h)
      WRITE(*,*) 'HUGE(h) : ', huge(h)
      WRITE(*,*) 'RANGE(h) : ', range(h)
      WRITE(*,*)
      WRITE(*,*) ' ===== 4-Byte INTEGER Type (Default) ==== '
      WRITE(*,*) 'KIND(f) : ', kind(f)
      WRITE(*,*) 'RADIX(f) : ', radix(f)
      WRITE(*,*) 'DIGITS(f) : ', digits(f)
      WRITE(*,*) 'HUGE(f) : ', huge(f)
      WRITE(*,*) 'RANGE(f) : ', range(f)
      WRITE(*,*)
      WRITE(*,*) ' ===== 8-Byte INTEGER Type ==== '
      WRITE(*,*) 'KIND(d) : ', kind(d)
      WRITE(*,*) 'RADIX(d) : ', radix(d)
      WRITE(*,*) 'DIGITS(d) : ', digits(d)
      WRITE(*,*) 'HUGE(d) : ', huge(d)
      WRITE(*,*) 'RANGE(d) : ', range(d)
      WRITE(*,*)
    END PROGRAM intgrinq

