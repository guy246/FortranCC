
program Fibonacci
  implicit none
  integer :: n, i
  real :: U0, U1, U2, e
  print*, "Entrez la précision"
  read*, e
  U0=1
  U1=1
  i=0
  do while (abs((U1/U0)-1.6180339887)>e .and. i<50)
    U2=U1+U0
    U0=U1
    U1=U2
    i=i+1
  end do
  print*, "Le nombre d'or à ", e, " près est ", U1/U0
end program Fibonacci
