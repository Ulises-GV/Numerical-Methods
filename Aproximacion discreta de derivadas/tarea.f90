program tarea

implicit none

real(kind=8), allocatable, dimension(:) :: x, f, df, error    
real(kind=8) :: mifuncion, xmin, xmax, dx
integer :: i, N

N = (1000.0)

allocate(x(0:N), f(0:N), df(0:N), error(0:N))
xmin = -5.0d0
xmax = 5.0d0
dx = (xmax-xmin)/dble(N)


do i =0,N

   x(i) = xmin + dble(i)*dx 
   f(i) = mifuncion(x(i))
end do

do i=0, N

    df(i) = (f(i-1) - 2*f(i) + f(i+1))/(dx**2)
end do

!df(N) = (f(N-2) - 4*f(N-1) + 3*f(N))/(2*dx)    
!df(0) = -(f(2) - 4*f(1) + 3*f(0))/(2*dx)    

do i = 0, N
   error(i) = df(i) - (-2*x(i)*sin(x(i))*exp(-x(i)**2) + cos(x(i))*exp(-x(i)**2))
   print *, x(i), f(i), df(i), error(i)
end do

    
end program   

real(kind=8) function mifuncion(xloc)

implicit none

real(kind=8) :: xloc

     mifuncion = exp(-xloc**2)*sin(xloc)
end

