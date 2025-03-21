program newtonlaw

implicit none

real(kind=8), allocatable, dimension(:) :: t, Temp, Texact, error, elol, texact2, temp2
real(kind=8) :: t0, tf, dt, temp0, k, ta
integer :: i, j, N, resolucion 
real(kind=8) :: k1, k2

resolucion = 4
N = 50

N = 2**(resolucion-1)*N

allocate(t(0:N), Temp(0:N), Texact(0:N), Texact2(0:N), error(0:N), temp2(0:N), elol(0:N))

t0 = 0.0d0
tf = 30.0d0

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
    do j=1,2
       if(j.eq.1) then
          k1 = k*(Ta-temp(i-1))
          temp(i) = temp(i-1) + k1*dt
       else
          k2 = k*(Ta-temp(i))
!          temp(i) = temp(i-1) + k*(ta - temp(i))*dt   
       end if
          temp(i) = temp(i-1) + 0.5 * (k1 + k2) * dt   

    end do
end do

texact = Ta + (temp0-ta)*exp(-k*t)
error = -temp + texact


do i=1,N-1
!elol(i) = -( Ta + (1.0/exp(-k*t(i)))*(temp(i)-ta)*exp(-k*(t(i)+dt))) + (temp(i)+k*(ta-temp(i))*dt)
texact2(i+1) =  Ta + (1.0/exp(-k*t(i)))*(temp(i)-ta)*exp(-k*(t(i)+dt))
!temp2(i+1) = temp(i)+k*(ta-temp(i))*dt
    do j=1,2
       if(j.eq.1) then
          k1 = k*(Ta-temp(i))
          temp2(i+1) = temp(i) + k*(Ta-temp(i))*dt
       else
          k2 = k*(Ta-temp(i+1))
 !         temp2(i+1) = temp(i) + k*(ta - temp(i+1))*dt   
       end if
          temp2(i+1) = temp(i) + 0.5 * (k1 + k2) * dt
    end do


elol(i)=  -texact2(i+1) + temp2(i+1)
end do


do i = 0, N, 2**(resolucion-1)
   
   print*, t(i), temp2(i), texact2(i), elol(i), temp(i), texact(i), error(i) 
enddo

!   print*, temp(1) 


end program
