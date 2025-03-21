program productoM
  implicit none

  integer :: i,j,k,n,m,l,p,entranum, h
  integer,allocatable,dimension(:,:) :: A,B,C
  integer, allocatable, dimension (:) :: x,y,z 

print*, "escriba 1 si quiere hacer multiplicacion de matrices o 2 si quiere hacer suma de vectores"
read(*,*)h

if(h==1) then
	
  print*, "elige la primera matriz de nxm"
  read(*,*)n,m
  print*, "elige la segunda matriz de pxl"	
  read(*,*)l,p
  allocate(A(1:n,1:m),B(1:l,1:p),C(1:n,1:p))

if (m==l) then

    print*, "ingresa los números de la primera matriz A"
    do i=1,n
       do j=1,m
          read(*,*)entranum
          A(i,j)=entranum
        
       enddo
    enddo

    print*, "ingresa los números de la segunda matriz B"
    do j=1,l
       do i=1,p
          read(*,*)entranum
          B(j,i)=entranum
       enddo
    enddo

    C=0
  
!   print*, A
!   print*, B
  
    print*, "la multiplicación de matrices es: "
    do i=1,n
       do j=1,p
          do k=1,m
             C(i,j)=C(i,j)+A(i,k)*B(k,j)
          enddo
          print*, C(i,j)
       enddo
    enddo   
    else
	  print*, "error, el producto de la matriz no está definido"      
    endif

elseif(h==2) then

   print*, "elige la dimensión de los vectores"
     read(*,*)n 
     allocate (x(1:n),y(1:n),z(1:n))
     
   print*, "elige las coordenadas de tu primer vector"
   do i=1,n
     read(*,*)entranum
     x(i)=entranum
   enddo 
   print*, "elige las coordenadas de tu segundo vector"	
   do i=1,n
      read(*,*)entranum
      y(i)=entranum
   enddo

   print*, "el resultado de la suma es: " 
   do i=1,n
      z(i)=x(i)+y(i)
      print*,z(i) 
   enddo

else

   print*, "valor no permitido ingresa 1 o 2"

endif
end program productoM















