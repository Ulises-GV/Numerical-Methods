program nada

implicit none

integer:: i,N
real(kind=8)::x
real(kind=4)::y
complex(kind=8)::z

! Esto es un comentario

i = 9
x = 3.5
y = 6.5
z = (1.0,2.0)

print *,'i=',i
print *,'x=',x
print *, 'y=',y
print *,'z=',z
print *, 'Re(z)=',real(z)
print *, 'Im(z)=',imag(z)
print *,'|z|=',sqrt(real(z)**2 + imag(z)**2)




end program
