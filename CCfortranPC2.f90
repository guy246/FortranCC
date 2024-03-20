module m_nagaoka
    implicit none
    contains
    real function moyenne (x)
        real, dimension(:), intent(in) :: x
        real :: moyenne
        moyenne = sum(x)/real(size(x))
    end function moyenne

    real function coefficient_a(E, S)
        real, dimension(:), intent(in) :: E, S
        real :: moyE, moyS, moyES, moye2
        coefficient_a = (moyenne(E*S) - moyenne(E)*moyenne(S))/(moyenne(E**2) - moyenne(E)**2)
    end function coefficient_a

    real function coefficient_b(E, S)
        real, dimension(:), intent(in) :: E, S
        real :: moyE, moyS, moyES, moye2
        coefficient_b = (moyenne(S)*moyenne(E**2) - moyenne(E*S)*moyenne(E))/(moyenne(E**2) - moyenne(E)**2)
    end function coefficient_b

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
end module m_nagaoka


program Nagaoka
    use m_nagaoka
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
	
    !appelez la fonction coefficient_a et coefficient_b
    a = coefficient_a(s,k)
    b = coefficient_b(s,k)
    write(*,"('a = ',f6.3)") a
    write(*,"('b = ',f6.3)") b

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
    
end program Nagaoka