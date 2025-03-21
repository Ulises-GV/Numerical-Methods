program lorenzsistem
implicit none

real(kind=8) :: t0, tmax, dt, x0, y0, z0, a, b, c, k1x, k2x, k1y, k2y, k1z, k2z
real(kind=8) , allocatable, dimension (:) :: t, x, y, z
integer :: i,j,N, resolucion

resolucion = 4
N = 100000
t0 = 0.0d0
tmax = 100.0d0
N = 2**(resolucion-1)*N 
x0 = 7.0
y0 = 6.0
z0 = 5.0

a = 15.0
b = 30.0
c = 3.0


allocate(t(0:N), x(0:N), y(0:N), z(0:N))

dt = (tmax - t0)/dble(N)

do i=0,N
   t(i) = t0 + dble(i)*dt
end do

x(0) = x0
y(0) = y0
z(0) = z0

do i=1,N
   do j=1,2
      if(j.eq.1) then
           k1x = a*(y(i-1) - x(i-1))
           k1y = b*x(i-1) - y(i-1) - x(i-1)*z(i-1)
           k1z = x(i-1)*y(i-1) - c*z(i-1)
           x(i) = x(i-1) + k1x * dt
           y(i) = y(i-1) + k1y * dt
           z(i) = z(i-1) + k1z * dt
        else
           k2x = a*(y(i) - x(i))
           k2y = b*x(i) - y(i) - x(i)*z(i)
           k2z = x(i)*y(i) - c*z(i)
           x(i) = x(i-1) + 0.5d0 * (k1x + k2x) * dt
           y(i) = y(i-1) + 0.5d0 * (k1y + k2y) * dt
           z(i) = z(i-1) + 0.5d0 * (k1z + k2z) * dt
        end if
     end do
end do


open(1, file='chaos4.dat')
do i=0, N, 2**(resolucion-1)
   write(1,*) t(i), x(i)
end do    
close(1)

end program

