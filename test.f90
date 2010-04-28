program test
use cari
use viscari
implicit none

real(sp) :: a, b, sum, ea, eb

call fixpoint_plot((/ 1.0e-7, 2.0, 3.0e7 /), (/ 'a', 'b', 'c' /))

do
  read(*,*) a, b

  write(*,*) 'a= ', a
  call bin_print(a)
  write(*,*) 'b= ', b
  call bin_print(b)

  sum = a+b
  write(*,*) 'a+b=', sum
  call bin_print(sum)
  
  ea = (sum - b) - a
  write(*,*) 'ea=', ea
  call bin_print(ea)

  write(*,*) 'sum-b=', sum-b
  call bin_print(sum - b)

  eb = (sum - a) - b
  write(*,*) 'eb=', eb
  call bin_print(eb)
end do

end program test
