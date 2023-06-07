        subroutine vvod (a,b,c,l1,l2)
        real l1,l2
        write(6,*)'Vvedite koef funk'
        read (5,*)a,b,c
        write(6,*)'Vvedite levuju granicu otrezka'
        read(5,*)l1
        write(6,*)'Vvedite pravuju granicu otrezka'
        read(5,*)l2
        end

        real function funk(xx,a,b,c)
        real xx,f
        f=a*xx/(b*xx+c)
        funk=f
        end
        
        real function poli(f,x,y,n)
        real f(n),x(n),s,p,y
        s=0
        do i=1,n,1
        p=1
        do j=1,n,1
        if(i.ne.j) p=p*(y-x(j))/(x(i)-x(j))
        end do
        s=s+p*f(i)
        end do
        poli=s
        end

        program lagrange
        integer n,i
        real y,f(25),x(25),p,a,b,c,d,h,poli,l1,l2,ff
        write(6,*)'Vvedite kolichestvo uzlov'
        read(5,*)n
        call vvod(a,b,c,l1,l2)
        h=(l2-l1)/(n-1)
        do i=1,n,1
        x(i)=l1+h*(i-1)
        xx=x(i)
        f(i)=funk(xx,a,b,c)
        write(6,*)'funk(i) =',f(i)
        end do
        h=(x(2)-x(1))/20
        xk=x(n)+(h/2)
        do xx=x(1),xk,h
        ff=funk(xx,a,b,c)
        p=poli(f,x,xx,n)
        y=ff-p
        write(6,*)'xx, R(x)=',xx,y
        end do
        read(5,*)d
        end
