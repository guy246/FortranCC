program Nagaoka
    implicit none
    integer :: i,n
    real:: dlmin,dlmax,a,b,ecartmax,ecartmin,c,nk,ndl,nb
    real, dimension(:), allocatable :: k,dl,C,Km

    !Définir la taille des tableaux 
    a=0.89
    b=1
    n=21
    write(*,"(/,'Saisir le nombre de point pour appliquer le nombre de point'/)",advance="no")
    read*, nb

    !allouez les tableaux
    allocate(k(n))
    allocate(dl(n))
    allocate(C(n))
    allocate(km(n))

    ! Définissez les valeurs de k
	!k = (/1.0, 0.818136, 0.688423, 0.595045, 0.52551, 0.471865, 0.429199, 0.394401, 0.365438, &
	!0.340898, 0.319825, 0.301502, 0.285406, 0.271144, 0.258407, 0.246982, 0.236581, 0.227147, 0.218528, &
	!0.210617,0.203315/)

    ! Dl'utilisateur choisi le nombre de points
	do i=1,nb
        write(*,"(/,'Saisir la valeur de k')",advance="no")
        read*, nk
        write(*,"(/,'Saisir la valeur de dl')",advance="no")
        read*, ndl
        k(i)=nk
        dl(i)=ndl
    end do
	
	! Définissez dlmin et dlmax
	!dlmin = 0
	!dlmax = 10

    !définissez les valeurs de s
    !dl=(/(dlmin+(((i-1)*(dlmax-dlmin))/(n-1)),i=1,n)/)
    !dl=dl/2

    !faire un tableau avec les nouvelles valeurs de c de la dichotomie
    do i=1,n
        c= dichotomie(a,dl(i),k(i))
        C(i)=c
    end do
    write(*,*) "C = ", C

    !la droite moindre carré
    km=1/(1+a*dl+c*((dl)*(dl)))
    
    !affichez les valeurs de s et de D
    write(*,*) "s = ", dl
    write(*,*) "D = ", D

	
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

    !Définir la fonction dichotomie avec a la valeur obtenue précédemment et c la valeur qui permet d'annuler la fonction f qui sera définie dans l'inerval [-0.1,0] ave une précision d'au moins 0.001
    real function dichotomie(a,dl,k)
        real, intent(in)::dl,k,a
        real::e=0.001
        real::c1,c2,fc1,fc2
        c1=-0.1
        c2=0
        fc1=1/(1+a*dl+c1*((dl)*(dl)))-k
        fc2=1/(1+a*dl+c2*((dl)*(dl)))-k
        do while(abs(c2-c1)>e)
            c=(c1+c2)/2
            if(fc1*fc2<0) then
                c2=c
                fc2=1/(1+a*dl+c2*((dl)*(dl)))-k
            else
                c1=c
                fc1=1/(1+a*dl+c1*((dl)*(dl)))-k
            end if
        end do
        dichotomie=c
    end function dichotomie
    
end program Nagaoka
