module m_nagaoka
    implicit none
    contains
    real function moyenne (x)
        real, dimension(:), intent(in) :: x
        moyenne = sum(x)/real(size(x))
    end function moyenne

    real function coefficient_a(E, S)
        real, dimension(:), intent(in) :: E, S
        real :: moyE, moyS, moyES, moyeMM
        coefficient_a = (moyenne(E*S) - moyenne(E)*moyenne(S))/(moyenne(E**2) - moyenne(E)**2)
    end function coefficient_a

    real function coefficient_b(E, S)
        real, dimension(:), intent(in) :: E, S
        real :: moyE, moyS, moyES, moyeMM
        coefficient_b = (moyenne(S)*moyenne(E**2) - moyenne(E*S)*moyenne(E))/(moyenne(E**2) - moyenne(E)**2)
    end function coefficient_b

    !real function dichotomie(a,c)
    !    real, intent(in) :: a,c
    !    real :: f,dl,dlmin,dlmax
    !    dlmin = 0
    !    dlmax = 10
    !    dl = (dlmax-dlmin)/2
    !    f = 1/(1+a*(dl/2)+c*((dl/2)*(dl/2)))
    !    do while (abs(f) > 0.001)
    !        if (f > 0) then
    !            dlmin = dl
    !        else
    !            dlmax = dl
    !        end if
    !        dl = (dlmax+dlmin)/2
    !        f = 1/(1+a*(dl/2)+c*((dl/2)*(dl/2)))
    !    end do
    !    dichotomie = dl
    !end function dichotomie
end module m_nagaoka


program Nagaoka
    use m_nagaoka
    implicit none
    integer :: i,n
    real:: dlmin,dlmax,a,b,ecartmax,ecartmin
    real, dimension(:), allocatable :: k,s,D

    !Définir la taille des tableaux 
    n=21

    !allouez les tableaux
    allocate(k(n))
    allocate(s(n))
    allocate(D(n))

    !Définir les valeurs des tableaux
    k = (/0.818136, 0.788525, 0.760886, 0.735079, 0.710969, 0.688423, 0.667315, 0.647527, 0.62895, &
    0.611487, 0.595045, 0.579543, 0.564903, 0.551057, 0.537945, 0.52551, 0.513701, 0.502472, 0.491782, &
    0.481591, 0.471865/)

    !definissez dlmin et dlmax
    dlmin=0.5
    dlmax=2.5

    !définissez les valeurs de s
    s=(/(dlmin+(((i-1)*(dlmax-dlmin))/(n-1)),i=1,n)/)
    s=s/2

    !appelle de la dichotomie
    !do i=1,n
    !    c= dichotomie(k(i),s(i))
    !end do
	
    !appelez la fonction coefficient_a et coefficient_b
    a = coefficient_a(s,1/k)
    b = coefficient_b(s,1/k)
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
