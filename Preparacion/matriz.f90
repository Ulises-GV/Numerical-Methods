
program matrices

implicit none

integer, allocatable, dimension (:,:) :: mat1,matR
integer, allocatable, dimension (:) :: vec1
integer :: n,i,j,k, entrada, entrada2

n=10

allocate(mat1(1:n,1:n),matR(1:n,1:n),vec1(1:n))

open(1,file='random.dat')
do i=1,n
   do j=1,n
       
       read(1,*)entrada
       mat1(i,j) = entrada    
   end do
end do
close(1)

open(2,file='random2.dat')
   do i=1,n
      read(2,*)entrada2
      vec1(i) = entrada2 
   end do
close(2)

do i=1,n
   do j=1,1
     do k=1,n
         matR(i,j) = matR(i,j) + mat1(i,k)*vec1(k)
     end do
     print *, matR(i,j)
   end do
end do

end program
