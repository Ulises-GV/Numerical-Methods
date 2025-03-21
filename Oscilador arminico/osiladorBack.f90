program osiladorbackward

implicit none

real(kind=8) :: t0,tmax, dt, x0, y0, omega
real(kind=8), allocatable, dimension (:) :: t, x, y, x_ex, y_ex, err_x, err_y
integer :: i, j, N, resolucion

N=100

resolucion=1
N = 2**(resolucion)*N
t0=0.0d0
tmax=5.0d0
x0 = 1.0d0
y0 = 0.0d0
omega = 2.0d0

allocate(t(0:N), x(0:N), y(0:N), x_ex(0:N), y_ex(0:N), err_x(0:N), err_y(0:N))

dt = (tmax - t0)/dble(N) 

do i=0,N
    t(i) = t0 + dble(i)*dt
end do

x(0) = x0
y(0) = y0
do i = 1, N
   do j=1,2
       if(j.eq.1) then
          x(i) = x(i-1) + dt * y(i-1)
          y(i) = y(i-1) + dt * (-omega**2*x(i-1) )
       else
          x(i) = x(i-1) + dt * y(i)
          y(i) = y(i-1) + dt * (-omega**2*x(i) )
       end if
    end do
end do
x_ex = x0 * cos(omega*t)
y_ex = -x0 * omega * sin(omega*t) 
err_x = x - x_ex
err_y = y - y_ex

open(1, file='ho_b.dat')
do i=0,N
   write(1,*) t(i), x(i), y(i), x_ex(i), y_ex(i), err_x(i), err_y(i)
end do
close(1)


end program
