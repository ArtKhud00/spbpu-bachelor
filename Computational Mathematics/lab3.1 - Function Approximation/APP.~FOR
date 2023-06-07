	real function FUN(XX)
	common/massiv/X1(7),F(7)
	common I,N1
	if(I.NE.1) then
	FUN=(XX**(I-1))*poli(F,X1,XX,N1)
	else
	FUN=poli(F,X1,XX,N1)
	end if
	end
		

	program app
	parameter N=7
	real B(N),ABSERR,RELERR,RESULT,FLAG,ERREST,X1(7),F(7),A,B1,A1(N,N)
	real WORK(N),U,h,ff,COND
	integer IPVT(N)
	integer I,J,N1,N2,NOFUN
	external FUN
	common/massiv/X1,F
	common I,N1,J
	A=-4
	B1=8
	N1=7
	N2=N
	ABSERR=0.000001
	RELERR=ABSERR
	OPEN(12,FILE='dano.txt',status='OLD')
	read(12,*)X1,F
	do I=1,N2,1
	call quanc8(FUN,A,B1,ABSERR,RELERR,RESULT,ERREST,NOFUN,FLAG)
	B(I)=RESULT
	write(6,*)RESULT
	end do
	do I=1,N2,1
	do J=1,N2,1
	A1(I,J)=(B1**(I+J-1)-A**(I+J-1))/(I+J-1)
	end do
	end do
	call decomp(N,N,A1,COND,IPVT,WORK)
	call solve(N,N,A1,B,IPVT)
	write(6,*)B
	h=(X1(2)-X1(1))/20
	do U=-4,8,h
	ff=B(1)+B(2)*U+B(3)*(U**2)+B(4)*U**3+B(5)*U**4+B(6)*U**5+B(7)*U**6
	WRITE(21,*)U,ff
	end do
	end

