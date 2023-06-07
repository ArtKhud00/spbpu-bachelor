      real function poli(F,X,Y,N)
        real F(7),X(7),S,P,Y
        integer N
        S=0
        do i=1,N,1
        P=1
        do j=1,N,1
        if(i.ne.j) P=P*(Y-X(j))/(X(i)-X(j))
        end do
        S=S+P*F(i)
        end do
        poli=S
        end
