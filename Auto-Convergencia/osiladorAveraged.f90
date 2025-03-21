program osiladorAveraged
implicit none

real(kind=8) :: t0, tmax, dt, x0, y0, omega, b, m, k1x, k2x, k1y, k2y
real(kind=8), allocatable, dimension (:) :: t, x, y
integer :: i,j,N,resolucion

resolucion=4
N = 400
t0 = 0.0d0
tmax = 10.0d0
N = 2**(resolucion-1)*N
x0 = 1.0d0
y0 = 0.0d0
omega = 2.0d0
b = 0.1d0
m = 0.5d0

allocate(t(0:N), x(0:N), y(0:N))

dt = (tmax - t0)/dble(N)

do i=0,N
   t(i) = t0 + dble(i)*dt
end do 

x(0) = x0
y(0) = y0

do i=1,N
    do j=1,2
        if(j.eq.1) then
           k1x = y(i-1)
           k1y = -(b/m)*y(i-1) -omega**2 *x(i-1)
           x(i) = x(i-1) + k1x * dt
           y(i) = y(i-1) + k1y * dt
        else
           k2x = y(i)
           k2y = -(b/m)*y(i) -omega**2 * x(i)
           x(i) = x(i-1) + 0.5d0 * (k1x + k2x) * dt
           y(i) = y(i-1) + 0.5d0 * (k1y + k2y) * dt
        end if
     end do
end do


open(1, file='sal4.dat')
do i=0, N, 2**(resolucion-1)
   write(1,*) t(i), x(i), y(i)
end do    
close(1)


end program
