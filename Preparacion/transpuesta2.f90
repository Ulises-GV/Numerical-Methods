
program transpuesta

implicit none

integer, allocatable, dimension (:,:) :: mat1, matR, vec1
integer :: n,i,j,k, entrada, entrada2, resultado

n=10

allocate(mat1(1:n,1:n),matR(1:n,1:n),vec1(1:n,1))

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
   do j=1,1
      read(2,*)entrada2
      vec1(i,j) = entrada2 
    end do
end do
close(2)

do i=1,n
   do j=1,1
     do k=1,n
         matR(i,j) = matR(i,j) + vec1(k,1)*mat1(k,i)
     end do
!     print *, matR(i,j)
   end do
end do

!     print *, matR  


do i=1,n
    do k=1,n
     resultado = resultado + vec1(k,1)*matR(i,k)     
     end do
end do

     print *, resultado  


end program
