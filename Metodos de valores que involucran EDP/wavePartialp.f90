module numbers 

real(kind=8), allocatable, dimension (:) :: t, x, phi, phi_p, phi_pp

real(kind=8) :: xmin, xmax, A, o
real(kind=8) :: t0, tmax, dt, dx, CFL
integer :: Nx, Nt

end module

program waves

use numbers
implicit none

integer :: i,j,k

Nx = 200
t0 = 0.0
tmax = 4.0
Nt = 400

xmin = -1.0
xmax = 1.0

CFL = 0.9

A = 1.0
o = 0.1


allocate(t(0:Nx), x(0:Nx), phi(0:Nx), phi_p(0:Nx), phi_pp(0:Nx))

dx = (xmax - xmin)/dble(Nx)

dt = CFL*dx

do i=0,Nx
   x(i) = xmin + dx*dble(i)   
end do

  do j=0,Nx
     phi_p(j) =  A*exp((-x(j)**2/o**2))
     phi_pp(j) = phi_p(j) + (dt**2/2.0)*(-2*x(i)/o**2 * exp(-1.0/o**2 * x(i)**2) + (4.0*x(i)**2)/o**4 * (exp(-1.0/o**2 * x(i)**2)))    
!     phi_pp(j) = phi_p(j) + (dt**2/2.0)*(phi_p(j-1)-2.0*phi_p(j)+phi_p(j+1))/dx**2           !(dt**2/2.0)*(-2*x(i)/o**2 * exp(-1.0/o**2 * x(i)**2) + (4.0*x(i)**2)/o**4 * (exp(-1.0/o**2 * x(i)**2)))    
  end do


open(1,file='wave.dat')


do i=1,Nt

   print *, i,t(i) 
   t = t + dt

!Auxiliares

   do j=1,Nx-1
     
     phi(j) = CFL**2 * (phi_p(j+1) - 2.0d0 * phi_p(j) + phi_p(j-1)) + 2.0d0 * phi_p(j) - phi_pp(j)

   end do
   
   phi(0) = CFL**2 * (phi_p(1) - 2.0d0 * phi_p(0) + phi_p(Nx-1)) + 2.0d0 * phi_p(0) - phi_pp(0) 
   phi(Nx) = CFL**2 * (phi_p(1) - 2.0d0 * phi_p(Nx) + phi_p(Nx-1)) + 2.0d0 * phi_p(Nx) - phi_pp(Nx) 

   phi_pp = phi_p
   phi_p = phi

   
   if (mod(i,1).eq.0) then 
       do k = 0,Nx
          write(1,*) t(k), x(k), phi(k)
       end do   
       write(1,*)  ' ', ' '
       write(1,*)  ' ', ' '

   end if


   
end do
close(1)

end program



