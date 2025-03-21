program nada

implicit none

integer:: i,N
real(kind=8)::x,y

open(1,file='salida')

do i=0,100
! print *, dble(i), dble(i*i)
   write(1,*) dble(i), dble(i*i)
end do

close(1)



end program
