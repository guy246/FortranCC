program Nagaoka
    implicit none
    integer :: i,n, nb
    real:: dlmin,dlmax,a,b,ecartmax,ecartmin
    real, dimension(:), allocatable :: k,s,D

    !Définir la taille des tableaux 
    n=21
    write(*,"(/,'Saisir le nombre de point pour appliquer le nombre de point'/)",advance="no")
    read(*,*) nb

    !allouez les tableaux
    allocate(k(n))
    allocate(s(n))
    allocate(D(n))

    ! Définissez les valeurs de k
	k = (/1.0, 0.818136, 0.688423, 0.595045, 0.52551, 0.471865, 0.429199, 0.394401, 0.365438, &
	0.340898, 0.319825, 0.301502, 0.285406, 0.271144, 0.258407, 0.246982, 0.236581, 0.227147, 0.218528, &
	0.210617,0.203315/)
	
	! Définissez dlmin et dlmax
	dlmin = 0
	dlmax = 10

    !définissez les valeurs de s
    s=(/(dlmin+(((i-4)*(dlmax-dlmin))/(n-1)),i=1,n)/)

    !appelle de la dichotomie
    do i=1,n
        call dichotomie(k(i),s(i))
    end do
	
    !appelle de la subroutine
    call coefficient_a_b(s,k,a,b)

    ! Calculez D et ecartmax
	D = 1.0/(1.0 + a*s/2.0)
	ecartmax = maxval(abs(s - D))
    ecartmin = minval(abs(s - D))
	
	! Affichez les résultats
	write(*,*) "K = ", D
	write(*,*) "Erreur relative = ", ecartmax
    write(*,*) "Erreur relative = ", ecartmin
	
	! Libérez la mémoire
	deallocate(k)
	deallocate(s)
	deallocate(D)
	
	stop "fin"
	contains
	
	subroutine coefficient_a_b(E, S, a, b)
        real, dimension(:), intent(in) :: E, S
        real, intent(out) :: a, b
        real :: moyE, moyS, moyES, moye2
        
        moyE = sum(E)/real(size(E))
        moyS = sum(S)/real(size(S))
        moyES = sum(E*S)/real(size(E*S))
        moye2 = sum(E**2)/real(size(E*E))
        
        a = (moyES - moyS*moyE)/(moye2 - moyE**2)
        b = (moyS*moye2 - moyES*moyE)/(moye2 - moyE**2)
        
        write(*,*) "a = ", a
        write(*,*) "b = ", b
        
        if (abs(b - 1.0) < 1.0E-6) then
        write(*,*) "b est bien égal à 1"
        end if
    end subroutine coefficient_a_b
    !Définir la fonction dichotomie avec a la valeur obtenue précédemment et c la valeur qui permet d'annuler la fonction f qui sera définie dans l'inerval [-0.1,0] ave une précision d'au moins 0.001
    !f(c)=1/(1+a*(dl/2)+c((dl/2)*(dl/2)))
    real function dichotomie(a,c)
        real, intent(in) :: a,c
        real :: f,dl,dlmin,dlmax
        dlmin = 0
        dlmax = 10
        dl = (dlmax-dlmin)/2
        f = 1/(1+a*(dl/2)+c*((dl/2)*(dl/2)))
        do while (abs(f) > 0.001)
            if (f > 0) then
                dlmin = dl
            else
                dlmax = dl
            end if
            dl = (dlmax+dlmin)/2
            f = 1/(1+a*(dl/2)+c*((dl/2)*(dl/2)))
        end do
        dichotomie = dl
    end function dichotomie
    
end program Nagaoka