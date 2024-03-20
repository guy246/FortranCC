Soit e une précision donnée. On souhaite calculer une approxiamtion du nombre d'or à e près à l'aide de la suite de Fibonacci :
Un=Un-1+Un-2
U0=U1=1
c'est le rapport (Un+1)/Un qui converge vers le nombre d'or.
La précision sera défini par l'utilisateur. On se limtera à 50 itérations.

program Fibonacci
	implicit none
	!Déclaration des variables
	real :: U0=1.0, U1=1.0, Un, Un1, Un2, e
	integer :: i
	!L'utilisateur rentre la précision
	print*, "Entrez la précision"
	read*, e
	!Calcul de la suite de Fibonacci
	do i=1,50
		Un=U0+U1
		U0=U1
		U1=Un
		!Calcul de la précision
		if (abs((Un1/Un)-1.61803398875)<e) then
			print*, "Le nombre d'or est ", Un1/Un
			exit
		end if
		Un1=Un
	end do
end program Fibonacci
