program Nagaoka
    implicit none
    integer :: i,n,j
    real:: a,b,ecartmax,ecartmin,dlmin,dlmax
    real, dimension(:), allocatable :: k,dl,D

    !Définir la taille des tableaux 
    n=21

    !allouez les tableaux
    allocate(k(n))
    allocate(dl(n))
    allocate(D(n))

    !Définir les valeurs des tableaux
    k = (/0.818136, 0.788525, 0.760886, 0.735079, 0.710969, 0.688423, 0.667315, 0.647527, 0.62895, &
    0.611487, 0.595045, 0.579543, 0.564903, 0.551057, 0.537945, 0.52551, 0.513701, 0.502472, 0.491782, &
    0.481591, 0.471865/)

    !definissez dlmin et dlmax
    dlmin=0.5
    dlmax=2.5

    !définissez les valeurs de s
    dl=(/(dlmin+(((i-1)*(dlmax-dlmin))/(n-1)),i=1,n)/)
    write(*,*)dl
    dl=dl/2

    !afficher le vecteur dl
    write(*,"('dl :')")
    do i=1,n
        write(*,"(f8.3)") dl(i)
    end do
	
    !appelle de la subroutine
    call coefficient_a_b(dl,1/k,a,b)

    ! Calculez D et ecartmax
	D = 1.0/(1.0 + a*dl/2.0)
	ecartmax = maxval(abs(dl - D))
    ecartmin = minval(abs(dl - D))
	
	! Affichez les résultats
	write(*,*) "K = ", D
	write(*,*) "Erreur relative = ", ecartmax
    write(*,*) "Erreur relative = ", ecartmin
	
	! Libérez la mémoire
	deallocate(k)
	deallocate(dl)
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
        
    end subroutine coefficient_a_b
end program Nagaoka
