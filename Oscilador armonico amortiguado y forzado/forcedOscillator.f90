module numbers

real(kind=8), allocatable, dimension (:) :: t
real(kind=8), allocatable, dimension (:,:) :: u
real(kind=8), allocatable, dimension (:) :: k1, k2, rhs
real(kind=8), allocatable, dimension (:) :: x_ex, y_ex, err_x, err_y

real(kind=8) :: x0, y0, omega, m, b, k, F0 
real(kind=8) :: t0, tmax, dt
integer :: N, resolucion, NE

end module

program dampedAndForced

use numbers
implicit none

integer :: i,j

resolucion = 3
NE = 2
N = 8000
t0 = 0.0
tmax = 200.0
N = 2**(resolucion-1)*N
x0 = 1.0
y0 = 0.0
m = 0.5
k = 2.0
b = 0.05
F0 = 0.2
omega = 2.0

allocate(t(0:N), u(1:NE,0:N),rhs(1:NE),k1(1:NE),k2(1:NE))

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
            call calcrhs(t(i-1) + 0.5d0*dt, u(:,i-1) + 0.5d0*k1(:)*dt)
            k2 = rhs
            u(:,i) = u(:,i-1) + k2*dt
        end if
      end do
end do

open(1,file='midB3.dat')
do i=0,N, 2**(resolucion-1)
   write(1,*) t(i), u(1,i), u(2,i)
end do
close(1)

end program 

subroutine calcrhs(my_t,my_u)

use numbers
implicit none

real(kind=8), intent(in) :: my_t
real(kind=8), dimension(NE), intent(in) :: my_u

rhs(1) = my_u(2)
rhs(2) = - b/m * my_u(2) - k/m * my_u(1) + F0/m*sin(omega*my_t)

end subroutine calcrhs


