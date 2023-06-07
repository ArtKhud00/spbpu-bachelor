      program spl
      integer N,e
      real X(7),Y(7),B(6),C(6),D(6),sp,U,SEVAL
      OPEN (13,FILE = 'dannie.txt',STATUS = 'OLD')
      N=7
      read (13, *) X, Y
      do  I = 1, N
      write (6, *) I, X(I), Y(I)
      end do
       call spline(N,X,Y,B,C,D)
       OPEN (2, FILE = 'spline.txt',STATUS = 'NEW')
       h=(X(2)-X(1))/20
       uk=X(N)+(h/4)
       do U=-4,uk,h
       sp=seval(N,U,X,Y,B,C,D)
       write(2,*)U,sp
       end do
       OPEN (33, FILE = 'function1.txt', STATUS = 'NEW')
	do I = 1, N
	write(33, *) X(I), Y(I)
	end do
        read(5,*)e
        end
       
