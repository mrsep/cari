!> Numerical inquiry functions for intrinsic INTEGER types: 
program intgrinq
use cari
implicit none

integer(i8)  :: b     ! 1-byte integer
integer(i16) :: h     ! 2-byte integer
integer(i32) :: f     ! 4-byte integer
integer(i64) :: d     ! 8-byte integer

write(*,*)
write(*,*) ' ===== 1-Byte INTEGER Type ==== '
write(*,*) 'KIND(b):   ', kind(b)
write(*,*) 'RADIX(b):  ', radix(b)
write(*,*) 'DIGITS(b): ', digits(b)
write(*,*) 'HUGE(b):   ', huge(b)
write(*,*) 'RANGE(b):  ', range(b)
write(*,*)
write(*,*) ' ===== 2-Byte INTEGER Type ==== '
write(*,*) 'KIND(h):   ', kind(h)
write(*,*) 'RADIX(h):  ', radix(h)
write(*,*) 'DIGITS(h): ', digits(h)
write(*,*) 'HUGE(h):   ', huge(h)
write(*,*) 'RANGE(h):  ', range(h)
write(*,*)
write(*,*) ' ===== 4-Byte INTEGER Type (Default) ==== '
write(*,*) 'KIND(f):   ', kind(f)
write(*,*) 'RADIX(f):  ', radix(f)
write(*,*) 'DIGITS(f): ', digits(f)
write(*,*) 'HUGE(f):   ', huge(f)
write(*,*) 'RANGE(f):  ', range(f)
write(*,*)
write(*,*) ' ===== 8-Byte INTEGER Type ==== '
write(*,*) 'KIND(d):   ', kind(d)
write(*,*) 'RADIX(d):  ', radix(d)
write(*,*) 'DIGITS(d): ', digits(d)
write(*,*) 'HUGE(d):   ', huge(d)
write(*,*) 'RANGE(d):  ', range(d)
write(*,*)
end program intgrinq
