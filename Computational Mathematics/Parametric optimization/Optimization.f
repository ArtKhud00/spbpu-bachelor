       program gg
       external f111
       integer n1,z,ak,fl,ss
       real x(2),xx(2),pJ(2),pJ1(2),pJ2(2),V(2),b,razn(2),ff1,ff2,a1
       real b1,num,denom,q,f111,aa1,an,aq,y1,a2,b2,pp,le,le1,pr,pr1,io
       real tt, Tk4k, Tk4n
       real Functional,t3,t4,tdc,tnn,tm,kn,ke,km,kdc,j,ann,yz,io1
       common /x/x
       common/n/n1,ak
       common/v/V
       common/par/t3,t4,tdc,tnn,tm,kn,ke,km,kdc,j
       ak=0
       j=2
       kn=25
       ke=1.2
       kdc=0.1
       tdc=0.001
       tm=0.02
       km=1.5
       t3=0.35
       t4=0.0085
       tnn=0.003
       fl=0
       
       !!��砫쭮� �ਡ�������
       n1=2
       fl2=0
       x(1)=t3
       x(2)=t4
       xx(1)=x(1)
       xx(2)=x(2)
       do i=1,n1
       dx=0.01*xx(i)
       d2x=dx*2
       xx(i)=xx(i)+dx
       ff1=Xtrace(xx(1),xx(2),ak)
       xx(i)=xx(i)-d2x
       ff2=Xtrace(xx(1),xx(2),ak)
       pJ(i)=(ff1-ff2)/d2x
       xx(i)=xx(i)+dx
       end do
       do i=1,2,1
       V(i)=pJ(i)
       end do
       write(6,*)"gradient",pJ(1),pJ(2)
       write(13,*)t3,t4
       write(13,*)pJ(1),pJ(2)
       z=1
       io=Xtrace(x(1),x(2),ak)
       do while(z.ne.0)
       a1=-1000000
       b1=1000000
       !!��ࠢ���⢮
       do i=1,2,1
       le=-x(i)/(2*V(i))
       pr=x(i)/(V(i))
       if(le.gt.pr) then
       le1=le
       le=pr
       pr=le1
       end if
       if(le.gt.a1)then
       a1=le
       end if
       if(pr.lt.b1)then
       b1=pr
       end if
       end do
       a2=a1
       b2=b1
       write(6,*)"a ",a2
       write(6,*)"b ",b2
       aa=amin(f111,a2,b2)
       write(6,*)"alfa1= ",aa
       write(6,*)"OLD x"
       do i=1,2,1
       write(6,*)"X(",i,")= ",x(i)
       end do
       write(6,*)"old value ",io
       write(6,*)"old gradient ",pJ(1),pJ(2)
       !! ���� ������ ���祭�� X
       do i=1,2,1
       x(i)=x(i)+aa*V(i)
       end do
       write(6,*)"\nNEW x\n"
       do i=1,2,1
       write(6,*)"X(",i,")= ",x(i)
       end do
       !! ����� ���祭�� �㭪樮����
       io1=Xtrace(x(1),x(2),ak)
       write(6,*)"new value ",io1,"\n"
       !! ���� Pk+1
       xx(1)=x(1)
       xx(2)=x(2)
       do i=1,2,1
       dx=0.01*xx(i)
       d2x=dx*2
       xx(i)=xx(i)+dx
       ff1=Xtrace(xx(1),xx(2),ak)
       xx(i)=xx(i)-d2x
       ff2=Xtrace(xx(1),xx(2),ak)
       pJ2(i)=(ff1-ff2)/d2x
       xx(i)=xx(i)+dx
       end do
       write(13,*)x(1),x(2)
       write(13,*)pJ2(1),pJ2(2)
       write(6,*)"new gradient ",pJ2(1),pJ2(2)
       !!
       !! ���� beta
       do i=1,2,1
       razn(i)=pJ(i)-pJ2(i)
       end do
       num=0.0
       do i=1,2,1
       num=num+pJ2(i)*razn(i)
       end do
       denom=0.0
       do i=1,2,1
       denom=denom+pJ(i)*pJ(i)
       end do
       b=num/denom
       write(6,*)"beta",b
       !!
       write(6,*)"\nOLD V"
       do i=1,2,1
       write(6,*)"V(",i,") ",V(i)
       end do
       !! ���� ������ ���ࠢ����� ��᪠
       do i=1,2,1
       V(i)=pJ2(i)-b*V(i)
       pJ(i)=pJ2(i)
       end do
       write(6,*)"\nNEW V"
       do i=1,2,1
       write(6,*)"V(",i,") ",V(i)
       end do
       write(11,*)t3,t4
       io=io1
       write(6,*)"Continue?"
       read(5,*)z
       end do
       write(6,*)"Xmin=\n"
       do i=1,2,1
       write(6,*)x(i),"\n"
       end do
       write(6,*)t3,t4
       ak=5
       !c=Xtrace(t3,t4,ak)
       read(5,*)q
       end



       real function Xtrace(T31,T41,ak)
       INTEGER iwork(5),N,ak,n1
       real x(5),work(6*5+3),t3,t4,tdc,tnn,tm,kn,ke,km,kdc,j
	real nu,nu2
        real K,I1,II,xsd,xsd1,xsc
	common/par/t3,t4,tdc,tnn,tm,kn,ke,km,kdc,j
	common/L/Xm
	common/Vivod/nu,nu2
	!common/n/n1,ak
        external fun
        N=5
        Xm=18.4
        I1=0
	e=0.0
	e1=0.0
	e12=0.0
	T3=T31
	T4=T41
	k=0.0002
	dt=0.01
        Tn=0.0
	II=0
	t=tn
	DO 11 I=1,N
11	x(i)=0.0
        tk=6.28
	re=1.e-6
	ae=0.0
	xsc=0
	iflag=1

	do 1000 to=tn,tk,dt
c      ��������������  �� 1 ��� (dt)
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
82	xsd=18.4*sin(0.5*TO)/kdc
        !xsd=18.4*sin(0.5*TO)/kdc
	e=xsd-x(4)
	e1=e-xsc
	e=e*e
	e1=e1*e1
	!write(45,*)to,e
	!e1=e1*e1
	I1=e*dt+K*(e1/dt)
	II=I1+II
	xsc=e
	!write(6,*)'I=',II
	!write(77,*)TO,e
	T=TO
        RE=1.E-6
        AE=0.
100     FORMAT(F6.2, 5F10.3)
        !write(22,*)T,X(5)
1000	continue
	!write(6,*)"FUNC",II
        Xtrace=II
	end


        SUBROUTINE FUN(T,X,XP)
        integer n1,ak
        REAL X(5),XP(5),f,f2,g,c,d,e,w,t3,t4,tdc,tnn,tm,kn,ke,km,kdc,j
	common/par/t3,t4,tdc,tnn,tm,kn,ke,km,kdc,j
	common/kot/g,f2
        common/n/n1,ak
	w=2*sin(50*t)
        xs=18.4*sin(0.5*t)
	f=xs-X(5)
	f2=(t3/t4)*f
	g=X(1)+f2
	if (g.gt.10) then
	g=10
	end if
	if (g.lt.(-10)) then
	g=-10
	end if
	c=ke*X(4)
	d=X(2)-c
	e=X(3)-w
	XP(1)=f/t4
	if((X(1).ge.10).and.(xp(1).gt.0)) then
	xp(1)=0
	end if
	if((X(1).le.(-10)).and.(xp(1).lt.0)) then
	xp(1)=0
	end if
	xp(2)=kn*g-X(2)
	xp(2)=xp(2)/tnn
	xp(3)=km*d-X(3)
	xp(3)=xp(3)/tm
	xp(4)=e/J
	xp(5)=kdc*X(4)-X(5)
	xp(5)=xp(5)/tdc
	!if(ak.gt.4)then
	!write(96,*)T,xp(3)
	!write(97,*)t3,t4
	!end if
        RETURN
        END



      SUBROUTINE RKF45(F,NEQN,Y,T,TOUT,RELERR,ABSERR,
     *                 IFLAG,WORK,IWORK)
      EXTERNAL F
      INTEGER NEQN,IFLAG,IWORK(5)
      REAL Y(NEQN),T,TOUT,RELERR,ABSERR,WORK(1)
      INTEGER K1,K2,K3,K4,K5,K6,K1M
      K1M=NEQN+1
      K1=K1M+1
      K2=K1+NEQN
      K3=K2+NEQN
      K4=K3+NEQN
      K5=K4+NEQN
      K6=K5+NEQN
      CALL RKFS(F,NEQN,Y,T,TOUT,RELERR,ABSERR,IFLAG,
     *          WORK(1),WORK(K1M),WORK(K1),WORK(K2),
     *          WORK(K3),WORK(K4),WORK(K5),WORK(K6),
     *          WORK(K6+1),IWORK(1),IWORK(2),
     *          IWORK(3),IWORK(4),IWORK(5))
      RETURN
      END



      SUBROUTINE RKFS(F,NEQN,Y,T,TOUT,RELERR,ABSERR,IFLAG,
     *                YP,H,F1,F2,F3,F4,F5,SAVRE,SAVAE,
     *                NFE,KOP,INIT,JFLAG,KFLAG)
      EXTERNAL F
      LOGICAL HFAILD,OUTPUT
      INTEGER NEQN,IFLAG,NFE,KOP,INIT,JFLAG,KFLAG
      REAL Y(NEQN),T,TOUT,RELERR,ABSERR,H,YP(NEQN),
     *     F1(NEQN),F2(NEQN),F3(NEQN),F4(NEQN),F5(NEQN),
     *     SAVRE,SAVAE
      REAL A,AE,DT,EE,EEOET,ESTTOL,ET,HMIN,REMIN,
     *     RER,S,SCAL,TOL,TOLN,U26,EPSP1,EPS,YPK
      INTEGER K,MAXNFE,MFLAG
      REAL AMAX1,AMIN1
      DATA REMIN/1.E-12/
      DATA MAXNFE/3000/
      IF(NEQN.LT.1)GO TO 10
      IF((RELERR.LT.0.0).OR.(ABSERR.LT.0.0))GO TO 10
      MFLAG=IABS(IFLAG)
      IF((MFLAG.EQ.0).OR.(MFLAG.GT.8))GO TO 10
      IF(MFLAG.NE.1)GO TO 20
      EPS=1.0
    5 EPS=EPS/2.0
      EPSP1=EPS+1.
      IF(EPSP1.GT.1.)GO TO 5
      U26=26.*EPS
      GO TO 50
   10 IFLAG=8
      RETURN
   20 IF((T.EQ.TOUT).AND.(KFLAG.NE.3))GO TO 10
      IF(MFLAG.NE.2)GO TO 25
      IF((KFLAG.EQ.3).OR.(INIT.EQ.0))GO TO 45
      IF(KFLAG.EQ.4)GO TO 40
      IF((KFLAG.EQ.5).AND.(ABSERR.EQ.0.0))GO TO 30
      IF((KFLAG.EQ.6).AND.(RELERR.LE.SAVRE).AND.
     *(ABSERR.LE.SAVAE))GO TO 30
      GO TO 50
   25 IF(IFLAG.EQ.3)GO TO 45
      IF(IFLAG.EQ.4)GO TO 40
      IF((IFLAG.EQ.5).AND.(ABSERR.GT.0.0))GO TO 45
   30 PRINT 35
   35 FORMAT(/20X,48H�HTE�P�POBAH�E �PEPBAHO, �OCKO��K� �O���OBATE�� ,
     *11HHE B��O�H��/20X,34H�HCTP�K��� RKF45, COOTBETCTB����X ,
     *27H�HA�EH��M IFLAG=5,6,7 ��� 8)
      STOP
   40 NFE=0
      IF(MFLAG.EQ.2)GO TO 50
   45 IFLAG=JFLAG
      IF(KFLAG.EQ.3)MFLAG=IABS(IFLAG)
   50 JFLAG=IFLAG
      KFLAG=0
      SAVRE=RELERR
      SAVAE=ABSERR
      RER=2.*EPS+REMIN
      IF(RELERR.GE.RER)GO TO 55
      RELERR=RER
      IFLAG=3
      KFLAG=3
      RETURN
   55 DT=TOUT-T
      IF(MFLAG.EQ.1)GO TO 60
      IF(INIT.EQ.0)GO TO 65
      GO TO 80
   60 INIT=0
      KOP=0
      A=T
      CALL F(A,Y,YP)
      NFE=1
      IF(T.NE.TOUT)GO TO 65
      IFLAG=2
      RETURN
   65 INIT=1
      H=ABS(DT)
      TOLN=0.
      DO 70 K=1,NEQN
      TOL=RELERR*ABS(Y(K))+ABSERR
      IF(TOL.LE.0)GO TO 70
      TOLN=TOL
      YPK=ABS(YP(K))
      IF(YPK*H**5.GT.TOL)H=(TOL/YPK)**0.2
   70 CONTINUE
      IF(TOLN.LE.0.0)H=0.0
      H=AMAX1(H,U26*AMAX1(ABS(T),ABS(DT)))
      JFLAG=ISIGN(2,IFLAG)
   80 H=SIGN(H,DT)
      IF(ABS(H).GE.2.0*ABS(DT))KOP=KOP+1
      IF(KOP.NE.100)GO TO 85
      KOP=0
      IFLAG=7
      RETURN
   85 IF(ABS(DT).GT.U26*ABS(T))GO TO 95
      DO 90 K=1,NEQN
   90 Y(K)=Y(K)+DT*YP(K)
      A=TOUT
      CALL F(A,Y,YP)
      NFE=NFE+1
      GO TO 300
   95 OUTPUT=.FALSE.
      SCALE=2./RELERR
      AE=SCALE*ABSERR
  101 HFAILD=.FALSE.
      HMIN=U26*ABS(T)
      DT=TOUT-T
      IF(ABS(DT).GE.2.*ABS(H))GO TO 200
      IF(ABS(DT).GT.ABS(H))GO TO 150
      OUTPUT=.TRUE.
      H=DT
      GO TO 200
  150 H=0.5*DT
  200 IF(NFE.LE.MAXNFE)GO TO 220
      IFLAG=4
      KFLAG=4
      RETURN
  220 CALL FEHL(F,NEQN,Y,T,H,YP,F1,F2,F3,F4,F5,F1)
      NFE=NFE+5
      EEOET=0.
      DO 250 K=1,NEQN
      ET=ABS(Y(K))+ABS(F1(K))+AE
      IF(ET.GT.0.)GO TO 240
      IFLAG=5
      KFLAG=5
      RETURN
  240 EE=ABS((-2090.*YP(K)+(21970.*F3(K)-15048.*F4(K)))
     *  +(22528.*F2(K)-27360.*F5(K)))
  250 EEOET=AMAX1(EEOET,EE/ET)
      ESTTOL=ABS(H)*EEOET*SCALE/752400.
      IF(ESTTOL.LE.1.0)GO TO 260
      HFAILD=.TRUE.
      OUTPUT=.FALSE.
      S=0.1
      IF(ESTTOL.LT.59049.)S=0.9/ESTTOL**0.2
      H=S*H
      IF(ABS(H).GT.HMIN)GO TO 200
      IFLAG=6
      KFLAG=6
      RETURN
  260 T=T+H
      DO 270 K=1,NEQN
  270 Y(K)=F1(K)
      A=T
      CALL F(A,Y,YP)
      NFE=NFE+1
      S=5.
      IF(ESTTOL.GT.1.889568E-4)S=0.9/ESTTOL**0.2
      IF(HFAILD)S=AMIN1(S,1.0)
      H=SIGN(AMAX1(S*ABS(H),HMIN),H)
      IF(OUTPUT)GO TO 300
      IF(IFLAG.GT.0)GO TO 101
      IFLAG=-2
      RETURN
  300 T=TOUT
      IFLAG=2
      RETURN
      END


      SUBROUTINE FEHL(F,NEQN,Y,T,H,YP,F1,F2,F3,F4,F5,S)
      EXTERNAL F
      INTEGER NEQN
      REAL Y(NEQN),YP(NEQN),F1(NEQN),F2(NEQN),
     *     F3(NEQN),F4(NEQN),F5(NEQN),S(NEQN)
      REAL CH
      INTEGER K
C
      CH=H/4.0
      DO 221 K=1,NEQN
  221 F5(K)=Y(K)+CH*YP(K)
C
      CALL F(T+CH,F5,F1)
      CH=3.0*H/32.0
      DO 222 K=1,NEQN
  222 F5(K)=Y(K)+CH*(YP(K)+3.0*F1(K))
      CALL F(T+3.0*H/8.0,F5,F2)
C
      CH=H/2197.0
      DO 223 K=1,NEQN
  223 F5(K)=Y(K)+CH*(1932.0*YP(K)+(7296.0*F2(K)-7200.0*F1(K)))
      CALL F(T+12.0*H/13.0,F5,F3)
C
      CH=H/4104.0
      DO 224 K=1,NEQN
  224 F5(K)=Y(K)+CH*((8341.0*YP(K)-845.0*F3(K))+
     *      (29440.0*F2(K)-32832.0*F1(K)))
      CALL F(T+H,F5,F4)
C
      CH=H/20520.0
      DO 225 K=1,NEQN
  225 F1(K)=Y(K)+CH*((-6080.0*YP(K)+(9295.0*F3(K)-
     *      5643.0*F4(K)))+(41040.0*F1(K)-28352.0*F2(K)))
      CALL F(T+H/2.0,F1,F5)
      CH=H/7618050.0
      DO 230 K=1,NEQN
  230 S(K)=Y(K)+CH*((902880.0*YP(K)+(3855735.0*F3(K)-
     *     1371249.0*F4(K)))+(3953664.0*F2(K)+277020.0*F5(K)))
      RETURN
      END
      
      
      
      
       real function f111(a)
       !real J
       real x(2)
       real V(2)
       real x1(2)
       integer ak
       common /x/x
       common /v/V
       common /n/n1,ak
       do i=1,n1
       x1(i)=x(i)+a*V(i)

       end do
       f111=Xtrace(x1(1),x1(2),ak)
       end



       real function amin(f111,a,b)
       real a,a1,b,b1,e,e1,ff,ff1,ff2,xx,x1,x2,ae,re,a2,b2
       real x(2),V(2)
       common /x/x
       common /v/V
       integer fl
       ae=0.0000001
       fl=0
       e=0.009*abs(b-a)
       e1=0.006*abs(b-a)
       a1=a
       b1=b
       go to 33
33     a2=a1
       b2=b1
       g=abs(b2-a2)
       fl=fl+1
       do while((g.gt.e))
       xx=(a2+b2)/2
       x1=xx-e1
       x2=xx+e1
       ff1=f111(x1)
       ff2=f111(x2)
       if(ff1.lt.ff2)then
       b2=xx
       else
       a2=xx
       end if
       g=abs(b2-a2)
       end do
       ff1=f111(a1)
       ff2=f111(b1)
       ff=f111(xx)
       if(ff1.le.ff)then
       if (abs(ff1-ff).lt.ae)then
       go to 71
       end if
       a1=a1-e
       b1=b1-e
       go to 33
       end if
       if(ff2.le.ff)then
       if (abs(ff-ff2).lt.ae)then
       go to 71
       end if
       a1=a1+e
       b1=b1+e
       go to 33
       end if
71     xx=(a2+b2)/2
       write(6,*)"X",xx
       amin=xx
       end
      
      
      
      
