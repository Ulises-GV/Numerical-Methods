
program vectores

implicit none

integer, allocatable, dimension (:) :: vec1,vec2,vecR
integer :: n,i,j, entrada, entrada2

n=10

allocate(vec1(1:n),vec2(1:n),vecR(1:n))

open(1,file='vector1.dat')
   do i=1,n
      read(1,*)entrada
      vec1(i) = entrada
   end do
close(2)

open(2,file='vector2.dat')
   do i=1,n
      read(2,*)entrada2
      vec2(i) = entrada2
   end do
close(2)

do i=1,n
   do j=1,n
      vecR(i) = vecR(i) + vec1(i)*vec2(j)
   end do
   print *, vecR(i)
end do


end program
