program newtonlaw

implicit none

real(kind=8), allocatable, dimension(:) :: t, Temp, Texact, error, elol, texact2, temp2
real(kind=8) :: t0, tf, dt, temp0, k, ta
integer :: i, N, resolucion 

resolucion = 4
N = 50

N = 2**(resolucion-1)*N

allocate(t(0:N), Temp(0:N), Texact(0:N), Texact2(0:N), error(0:N), temp2(0:N), elol(0:N))

t0 = 0.0d0
tf = 25.0d0

dt = (tf-t0)/dble(N)

ta = 35.0
temp0 = 0.0d0 
k = 0.3d0

do i= 0, N
    t(i) = t0 + dble(i)*dt
enddo

print*, 'dt=', dt

temp(0) = temp0
do i=1, N
    temp(i+1) = temp(i) + k*(ta - temp(i))*dt   

end do

texact = Ta + (temp0-ta)*exp(-k*t)
error = temp- texact

do i=1, N
!    elol(i)=temp(i) + k*(ta-temp(i))*dt - (ta+(temp(i)-ta)*exp(-k*(t(i)+dt)))
end do

do i=0,N
texact2(i) = -( Ta + (1.0/exp(-k*t(i)))*(temp(i)-ta)*exp(-k*(t(i)+dt))) + (temp(i)+k*(ta-temp(i))*dt)
!elol =  -temp2 + texact2
end do


do i = 0, N, 2**(resolucion-1)
   
   print*, t(i), temp(i), texact(i), error(i), texact2(i) 
enddo

!   print*, temp(1) 


end program
