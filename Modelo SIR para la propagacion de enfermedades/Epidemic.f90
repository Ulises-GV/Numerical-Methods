module numbers 

real(kind=8), allocatable, dimension (:) :: t
real(kind=8), allocatable, dimension (:,:) :: u
real(kind=8), allocatable, dimension (:) :: k1,k2,rhs

real(kind=8) :: x0, y0, z0, m, a, b, c, d
real(kind=8) :: t0, tmax, dt
integer :: N, resolucion, NE

end module

program Predator

use numbers
implicit none

integer :: i,j

resolucion = 1
NE = 3
N = 10000
t0 = 0.0
tmax = 1000.0
N = 2**(resolucion-1)*N

!x0+y0=1


 x0 = 0.9
 y0 = 1-x0
 z0 = 0.0


!x0 = 3.0
!y0 = 2.0
m = 0.5

a = 1.0/60.0
c = 1.0/3.0
b = 3.0*(a+c)

!a=1.25
!b=0.7
!c=0.4


allocate(t(0:N), u(1:NE,0:N),rhs(1:NE),k1(1:NE),k2(1:NE))


dt = (tmax - t0)/dble(N)

do i=0,N
   t(i) = t0 + dt*dble(i)
end do

u(1,0) = x0
u(2,0) = y0
u(3,0) = z0

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

open(1,file='heun9.dat')
do i=0,N,2**(resolucion-1)
write(1,*) t(i),u(1,i),u(2,i),u(3,i)
end do
close(1)

end program

subroutine calcrhs(my_t,my_u)

use numbers
implicit none

real(kind=8), intent(in) :: my_t
real(kind=8), dimension(NE), intent(in) :: my_u

rhs(1) = a - a * my_u(1) - b * my_u(1) * my_u(2)
rhs(2) = b * my_u(1)*my_u(2) - c * my_u(2) - a * my_u(2)
rhs(3) = c*my_u(2) - a*my_u(3)

!rhs(1) = a * my_u(1) - b * my_u(1) * my_u(2)
!rhs(2) = - c *  my_u(2) + d * my_u(1) * my_u(2)

end subroutine calcrhs


