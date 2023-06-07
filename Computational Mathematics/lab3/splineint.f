
      real function FUN(XX)
      real X(7),F(7),B1(7),C(7),D(7)
      integer N
      common/massiv/ X,F,B1,C,D
      common N
      call SPLINE(N,X,F,B1,C,D)
      FUN=SEVAL(N,XX,X,F,B1,C,D)
      end
        
      program integr
      real A,B,ABSERR, RELERR, RESULT,ERREST,FLAG,X(7),F(7),B1(7),C(7),D(7)

        external FUN
	integer NOFUN,V,N
        common/massiv/ X,F,B1,C,D
	common N
	ABSERR=0.000001
	RELERR=ABSERR
	A=-4.0
	B=8.0
	N=7
	OPEN(13, FILE= 'dano.txt',STATUS='OLD')
	read(13,*)X,F
	write(6,*)X
	write(6,*)F

	call QUANC8(FUN,A,B,ABSERR,RELERR,RESULT,ERREST,NOFUN,FLAG)
	write(6,*)'Result= ',RESULT,' Err= ',ERREST,' # ',NOFUN,' F ',FLAG
	read(5,*)V
	end
