program osiladorarmonico

implicit none

real(kind=8):: t0, tmax, dt, x0, y0, omega, b, m
real(kind=8), allocatable, dimension (:) :: t, x, y
integer :: i, N, j, resolucion

resolucion = 4
N = 400
t0 = 0.0d0
tmax = 10.0
N = 2**(resolucion-1)*N
x0 = 1.0
y0 = 0.0
omega = 2.0
m = 0.5d0
b = 0.1d0

allocate(t(0:N),x(0:N),y(0:N))

dt = (tmax-t0)/dble(N)

do i=1,N
   t(i) = t0 + dt*dble(i)
end do

x(0) = x0
y(0) = y0
do i=1,N
   x(i) = x(i-1) + dt * y(i-1)
   y(i) = y(i-1) + dt * (-(b/m)*y(i-1) -omega**2 * x(i-1) )
end do

open(1, file='sal4.dat')
do i=0, N, 2**(resolucion-1)
   write(1,*) t(i), x(i)
end do
close(1)



end program 
