real function dichotomie(a,dl,k)
        real,intent(in) :: dl,K,a
        real, parameter :: eps=0.0001
        integer :: i
        real :: x1,x2,fc,c,fx1,fx2
        integer,parameter :: ite=50
        i=1
        x1=-0.1
        x2=0.0
        c=(x1+x2)/2.0
        fx1=1/(1+a*dl+x1*(dl**2))-k
        fx2=1/(1+a*dl+x2*(dl**2))-k
        fc=1/(1+a*dl+c*(dl**2))-k
        write(*,"(f14.6)")fx1
        write(*,"(f14.6)")fx2
        write(*,"(f14.6)")fc
        !On limite nos itérations soit quand la précision est
        !atteinte ou à 50 itérations
        do while ((abs(x2-x1)>eps).OR.(i<ite))
            !On regarde si f(x1)*f(c) est negatif
            !et on réduit l''intervalle
            if (fx1*fc<0) then
                x2 = c
            else
                x1=c
            end if
            c =(x1+x2)/2
            fx1=1/(1+a*dl+x1*(dl**2))-k
            fx2=1/(1+a*dl+x2*(dl**2))-k
            fc=1/(1+a*dl+c*(dl**2))-k
            i=i+1
        end do
        dichotomie=c
    end function dichotomie

