	PROGRAM pp
	INTEGER iwork(5)
	parameter n=1
	real x,work(6*n+3)
        external fun
        dt=0.01
        tn=0.0
	t=tn
	x=1.0
        tk=1.0
	re=1.e-6
	ae=0.0
	iflag=1
	OPEN (1,file='resh_DU.txt',status='NEW')
	do 1000 to=tn,tk,dt
c   интегрирование  на 1 шаг (dt)
75	CALL RKF45(fun,n,x,t,to,re,ae,iflag,work,iwork)
	go to(88,82,83,84,85,86,87,88),iflag
83	iflag=2
	re=re*10
	go to 75
84      iflag=2
	go to 75
85 	iflag=2
	ae=1.e-6
	go to 75
86	iflag=2
	re=re*10
	go to 75
87	iflag=2
	go to 75
88	stop111
82      T=TO
        RE=1.E-6
        AE=0.
c
C  вывод результатов моделирования
c
         WRITE (1,*)T,x
	 print 100,T,x
100     FORMAT(' T=',F10.6,' x=',F10.3)
1000	continue
	end

C  подпрограмма вычисления правых частей системы дифференциальных уравнениний
C  /вычисление вектора производных в точке Т/
        SUBROUTINE FUN(T,X,XP)
        REAL X,XP
        XP=-x
        RETURN
        END


