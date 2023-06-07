      REAL FUNCTION SEVAL(N,U,X,Y,B,C,D)
      INTEGER N
      REAL U,X(N),Y(N),B(N),C(N),D(N)
C
C  üTA נOהנPOחPAMMA Bש‏יCלסET תHA‏EHיE Kץגי‏ECKOחO
C  CנלAךHA
C
C  SEVAL=Y(I)+B(I)*(U-X(I))+C(I)*(U-X(I))**2+
C             D(I)*(U-X(I))**3
C
C  חהE X(I).LT.U.LT.X(I+1). יCנOלרתץETCס CXEMA
C  חOPHEPA
C
C  ECלי U.LT.X(1), TO גEPETCס תHA‏EHיE I=1.
C  ECלי U.GE.X(N), TO גEPETCס תHA‏EHיE I=N.
C
C  BXOהHAס יHזOPMAדיס.
C
C     N     -‏יCלO תAהAHHשX TO‏EK
C     U     -AגCדיCCA, הלס KOTOPOך Bש‏יCלסETCס תHA‏EHיE
C            CנלAךHA
C     X,Y   -MACCיBש תAהAHHשX AגCדיCC י OPהיHAT
C     B,C,D -MACCיBש KOüזזידיEHTOB CנלAךHA, Bש‏יCלEHHשE
C            נOהנPOחPAMMOך SPLINE
C
C  ECלי נO CPABHEHיא C נPEהשהץ‎יM BשתOBOM U HE
C  HAXOהיTCס B TOM צE יHTEPBAלE, TO הלס PAתשCKAHיס
C  HץצHOחO יHTEPBAלA נPיMEHסETCס הBOי‏Hשך נOיCK.
C
      INTEGER I,J,K
      REAL DX
      DATA I/1/
      IF(I.GE.N) I=1
      IF(U.LT.X(I)) GO TO 10
      IF(U.LE.X(I+1)) GO TO 30
C
C  הBOי‏Hשך נOיCK
C
 10   I=1
      J=N+1
 20   K=(I+J)/2
      IF(U.LT.X(K))J=K
      IF(U.GE.X(K))I=K
      IF(J.GT.I+1)GO TO 20
C
C  Bש‏יCליTר CנלAךH
C
 30   DX=U-X(I)
      SEVAL=Y(I)+DX*(B(I)+DX*(C(I)+DX*D(I)))
      RETURN
      END
