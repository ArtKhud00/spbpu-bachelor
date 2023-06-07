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
        integer n,d
        real y,f(7),x(7),p,poli
        OPEN (13,FILE = 'dannie.txt',STATUS = 'OLD')
	n = 7
        read (13, *) x, f
        do  i = 1, n
	write (6, *) i, x(i), f(i)
	end do
	OPEN (2, FILE = 'polinom.txt',STATUS = 'NEW')
        do y=-4, 8, 0.5
        p=poli(f,x,y,n)
        write(2,*)y,p
        end do
        OPEN (33, FILE = 'function1.txt', STATUS = 'NEW')
	do i = 1, n
	write(33, *) x(i), f(i)
	end do
        read(5,*)d
        end
