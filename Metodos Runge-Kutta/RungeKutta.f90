!program RungeKutta
!implicit none

module numbers

real(kind=8), allocatable, dimension (:) :: t
real(kind=8), allocatable, dimension (:,:) :: u 
real(kind=8), allocatable, dimension (:) :: k1,k2,rhs
real(kind=8), allocatable, dimension (:) :: x_ex, y_ex, err_x, err_y

real(kind=8) :: x0, y0, omega, m, b, k, alpha, beta
real(kind=8) :: t0, tmax, dt
integer :: N, resolucion, NE

end module

program RungeKutta

use numbers
implicit none

integer :: i,j

resolucion = 3
NE = 2
N = 2000
t0 = 0.0
tmax = 100.0
N = 2**(resolucion-1)*N
x0 = 1.0
y0 = 0.0
m = 0.5
k = 2.0
b = 0.075
alpha = 0.5d0*b/m
beta = sqrt(abs(1.0d0 - 4.0d0*k*m/b**2))

allocate(t(0:N), u(1:NE,0:N),rhs(1:NE),k1(1:NE),k2(1:NE))
allocate(x_ex(0:N),y_ex(0:N),err_x(0:N),err_y(0:N))

dt = (tmax - t0)/dble(N)

do i=0,N
   t(i) = t0 + dt*dble(i)
end do

u(1,0) = x0
u(2,0) = y0

do i=1,N
   do j=1,2
      if(j.eq.1) then
          call calcrhs(t(i-1), u(:,i-1))
          k1 = rhs
       else
          !MIDPOINT
          !call calcrhs(t(i-1) + 0.5d0*dt, u(:,i-1) + 0.5d0*k1(:)*dt)
          !k2 = rhs
          !u(:,i) = u(:,i-1) + k2 * dt
          !RALSTON
          ! call calcrhs( t(i-1) + 0.75d0 * dt, u(:,i-1) + 0.75d0 * k1(:) * dt)
          ! k2 = rhs
          ! u(:,i) = u(:,i-1) + ( 1.0d0/3.0d0 * k1(:) + 2.0d0/3.0d0 * k2(:) ) * dt
          !HEUN
           call calcrhs( t(i-1) + dt, u(:,i-1) + k1(:) * dt)
           k2 = rhs
           u(:,i) = u(:,i-1) + 0.5d0 * ( k1(:) + k2(:) ) * dt
       end if
    end do
end do

! Exact solution
x_ex = exp(-alpha*t)*(sin(alpha*beta*t)/beta + cos(alpha*beta*t))
y_ex = -alpha*exp(-alpha*t) * (sin(alpha*beta*t)/beta + cos(alpha*beta*t)) &
+ alpha*exp(-alpha*t) * (cos(alpha*beta*t) - beta*sin(alpha*beta*t))
err_x = u(1,:) - x_ex
err_y = u(2,:) - y_ex
! Saving data to a file
open(1,file='heun3.dat')
do i=0,N,2**(resolucion-1)
write(1,*) t(i),u(1,i),u(2,i),x_ex(i),y_ex(i),err_x(i),err_y(i)
end do
close(1)

end program 

subroutine calcrhs(my_t,my_u)

use numbers
implicit none

real(kind=8), intent(in) :: my_t
real(kind=8), dimension(NE), intent(in) :: my_u

rhs(1) = my_u(2)
rhs(2) = - b / m * my_u(2) - k / m * my_u(1)

end subroutine calcrhs
