        SUBROUTINE FUN(T,X,XP)
        REAL X,XP
        XP=-x/(x+1)
        RETURN
        END


        subroutine start(WORK,h,fun,T,X)
	REAL K1,K2,K3,K4,K,WORK(4)
	REAL Xk,Xc
	INTEGER I
	Xk=X
	CALL FUN(T,Xk,WORK(1))
	do I=1,3,1
	CALL fun(T,Xk,K1)
	Xc=Xk-(h/2)*K1
	CALL fun(T,Xc,K2)
	Xc=Xk-(h/2)*K2
	CALL fun(T,Xc,K3)
	XC=Xk-h*K3
	CALL fun(T,XC,K4)
	K=K1+2*K2+2*K3+K4
	K=K/6
	Xk=Xk-h*K
        CALL fun(T,Xk,WORK(I+1))
	end do
	end


      PROGRAM pp
      INTEGER iwork(5),e
      real x,work(4)
      external fun
      dt=0.35
      tn=0.0
      T=tn
      x=6.0
      tk=30.0
      iflag=2
      OPEN (1,file='adam035.txt',status='NEW')
      CALL start(work,dt,fun,t,x)
      do TOUT=tn,tk,dt
      CALL adams4(fun,X,T,TOUT,work)
      T=TOUT
      WRITE(1,*)T,X
      !Write(6,*)"do",work
      do j=3,1,-1
      work(J+1)=work(J)
      END DO
      CALL FUN(T,X,work(1))
      !Write(6,*)"posle",work
      END DO
      end


      subroutine adams4(fun,X,T,TOUT,work)
      REAL P1,P2,P3,P4,P,work(4)
      h=TOUT-T
      P1=(55/24)*work(1)
      P2=(59/24)*work(2)
      P3=(37/24)*work(3)
      P4=(3/8)*work(4)
      P=P1-P2+P3-P4
      X=X+h*P
      end
