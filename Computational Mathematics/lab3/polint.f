      real function FUN(XX)
      real X(7),F(7)
      integer N
      common/massiv/ X,F
      common N
      FUN=poli(F,X,XX,N)
      end
        
      program integr
      real A,B,ABSERR, RELERR, RESULT,ERREST,FLAG,X(7),F(7)
      external FUN
	integer NOFUN,d,N
        common/massiv/ X,F
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
	read(5,*)d
	end
