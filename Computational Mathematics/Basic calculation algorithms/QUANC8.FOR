      SUBROUTINE QUANC8(FUN,A,B,ABSERR,RELERR,RESULT,
     *                  ERREST,NOFUN,FLAG)
C
      REAL FUN,A,B,ABSERR,RELERR,RESULT,ERREST,FLAG
      INTEGER NOFUN
C
C     OדEHיTר יHTEחPAל הלס FUN(X) OT A הO B C תAהAHHOך
C     נOלרתOBATEלEM TO‏HOCTרא
C     ABTOMATי‏ECKAס AהAנTיBHAס נPOחPAMMA, OCHOBAHHAס HA
C     זOPMץלE HראTOHA-KOTEדA 8-OחO נOPסהKA
C
C     BXOהHAס יHזOPMAדיס
C
C    FUN    -יMס BHEûHEך זץHKדיי,PEAליתץא‎Eך
C            Bש‏יCלEHיE נOהיHTEחPAלרHOך זץHKדיי
C
C    A      -HיצHיך  נPEהEל יHTEחPיPOBAHיס
C
C    B      -BEPXHיך נPEהEל יHTEחPיPOBAHיס (B MOצET
C            גשTר MEHרûE, ‏EM  A)
C
C    RELERR -OTHOCיTEלרHAס צEלAEMAס נOחPEûHOCTר
C
C    ABSERR -AגCOלאTHAס צEלAEMAס  נOחPEûHOCTר
C
C    BשXOהHAס יHזOPMAדיס
C
C    RESULT -נPיגליצEHHOE תHA‏EHיE יHTEחPAלA
C
C    ERREST -OדEHKA BEלי‏יHש הEךCTBיTEלרHOך OûיגKי
C
C    NOFUN  -‏יCלO תHA‏EHיך זץHKדיי,יCנOלרתOBAHHשX
C            נPי Bש‏יCלEHיי יHTEחPAלA.
C
C    FLAG   -יHהיKATOP HAהEצHOCTי. ECלי FLAG PABEH Hץלא,
C            TO RESULT ץהOBלETBOPסET תAהAHHOך חPAHידE
C            נOחPEûHOCTי. ECלי FLAG=XXX.YYY, TO  XXX-‏יCלO
C            יHTEPBAלOB הלס KOTOPשX HE גשלO CXOהיMOCTי,
C            A 0.YYY-הOלס OCHOBHOחO יHTEPBAלA, OCTABûAסCס
C            הלס OגPAגOTKי B TOT MOMEHT,KOחהA נOהנPOחPAMMA
C            נPיגליתילACר K נPEהEלרHOMץ ‏יCלץ הלס NOFUN.
C
      REAL W0,W1,W2,W3,W4,AREA,X0,F0,STONE,STEP,COR11,TEMP
      REAL QPREV,QNOW,QDIFF,QLEFT,ESTERR,TOLERR
      REAL QRIGHT(31),F(16),X(16),FSAVE(8,30),XSAVE(8,30)
C
C     *** üTAנ1 *** נPיCBOEHיE HA‏AלרHשX תHA‏EHיך
C     נEPEMEHHשM, HE תABיCס‎יM OT יHTEPBAלA.
C     חEHEPיPOBAHיE KOHCTAHT.
C
      INTEGER LEVMIN,LEVMAX,LEVOUT,NOMAX,NOFIN,LEV,NIM,I,J
      LEVMIN=1
      LEVMAX=30
      LEVOUT=6
      NOMAX=5000
      NOFIN=NOMAX-8*(LEVMAX-LEVOUT+2**(LEVOUT+1))
C
C     ECלי NOFUN הOCTיחAET תHA‏EHיס NOFIN, TO TPEBOחA
C
      W0=3956.0/14175.0
      W1=23552.0/14175.0
      W2=-3712.0/14175.0
      W3=41984.0/14175.0
      W4=-18160.0/14175.0
C
C     נPיCBOיTר HץלEBשE תHA‏EHיס נEPEMEHHשM CץMMAM
C
      FLAG=0.0
      RESULT=0.0
      COR11=0.0
      ERREST=0.0
      AREA=0.0
      NOFUN=0
      IF(A.EQ.B)RETURN
C
C     *** üTAנ 2 *** נPיCBOEHיE HA‏AלרHשX תHA‏EHיך
C     נEPEMEHHשM, תABיCס‎יM OT יHTEPBAלA, B
C     COOTBETCTBיי C נEPBשM יHTEPBAלOM
C
      LEV=0
      NIM=1
      X0=A
      X(16)=B
      QPREV=0.0
      F0=FUN(X0)
      STONE=(B-A)/16.0
      X(8)=(X0+X(16))/2.0
      X(4)=(X0+X(8))/2.0
      X(12)=(X(8)+X(16))/2.0
      X(2)=(X0+X(4))/2.0
      X(6)=(X(4)+X(8))/2.0
      X(10)=(X(8)+X(12))/2.0
      X(14)=(X(12)+X(16))/2.0
      DO 25 J=2,16,2
      F(J)=FUN(X(J))
   25 CONTINUE
      NOFUN=9
C
C     *** üTAנ 3 *** OCHOBHשE Bש‏יCלEHיס
C     TPEגץאTCס QPREV, X0, X2, X4,...,X16, F0, F2, F4,...
C     ,F16. Bש‏יCלסאTCס X1, X3,...,X15, F1, F3,...,F15,
C     QLEFT,QRIGHT,QNOW,QDIF,AREA
C
   30 X(1)=(X0+X(2))/2.0
      F(1)=FUN(X(1))
      DO 35 J=3,15,2
      X(J)=(X(J-1)+X(J+1))/2.0
      F(J)=FUN(X(J))
   35 CONTINUE
      NOFUN=NOFUN+8
      STEP=(X(16)-X0)/16.0
      QLEFT=(W0*(F0+F(8))+W1*(F(1)+F(7))+W2*
     *      (F(2)+F(6))+W3*(F(3)+F(5))+W4*F(4))*STEP
      QRIGHT(LEV+1)=(W0*(F(8)+F(16))+W1*(F(9)+F(15))+
     *       W2*(F(10)+F(14))+W3*(F(11)+F(13))+W4*F(12))*STEP
      QNOW=QLEFT+QRIGHT(LEV+1)
      QDIFF=QNOW-QPREV
      AREA=AREA+QDIFF
C
C     *** üTAנ 4 *** נPOBEPKA CXOהיMOCTי הלס יHTEPBAלA
C
      ESTERR=ABS(QDIFF)/1023.0
      TOLERR=AMAX1(ABSERR,RELERR*ABS(AREA))*(STEP/STONE)
      IF(LEV.LT.LEVMIN)GO TO 50
      IF(LEV.GE.LEVMAX)GO TO 62
      IF(NOFUN.GT.NOFIN)GO TO 60
      IF(ESTERR.LE.TOLERR)GO TO 70
C
C     *** üTAנ 5 *** CXOהיMOCTי HET.
C     ץCTAHOBיTר CלEהץא‎יך יHTEPBAל
C
   50 NIM=2*NIM
      LEV=LEV+1
C
C     תAנOMHיTר üלEMEHTש, OTHOCס‎יECס K נPABOך נOלOBיHE
C     יHTEPBAלA, הלס גץהץ‎EחO יCנOלרתOBAHיס.
C
      DO 52 I=1,8
      FSAVE(I,LEV)=F(I+8)
      XSAVE(I,LEV)=X(I+8)
   52 CONTINUE
C
C     COגPATר üלEMEHTש, OTHOCס‎יECס K לEBOך נOלOBיHE
C     יHTEPBAלA הלס HEMEהלEHHOחO יCנOלרתOBAHיס
C
      QPREV=QLEFT
      DO 55 I=1,8
      J=-I
      F(2*J+18)=F(J+9)
      X(2*J+18)=X(J+9)
   55 CONTINUE
      GO TO 30
C
C     *** üTAנ 6 *** "נOצAPHשך" PAתהEל
C     ‏יCלO תHA‏EHיך זץHKדיי גליתKO K TOMץ, ‏TOגש
C     נPEBשCיTר ץCTAHOBלEHHשך נPEהEל.
C
   60 NOFIN=2*NOFIN
      LEVMAX=LEVOUT
      FLAG=FLAG+(B-X0)/(B-A)
      GO TO 70
C
C     TEKץ‎EE נPEהEלרHOE תHA‏EHיE חלץגיHש הEלEHיס
C     נOנOלAM PABHO LEVMAX
C
   62 FLAG=FLAG+1.0
C
C     *** üTAנ 7 *** CXOהיMOCTר הלס יHTEPBAלA יMEET MECTO
C     נPיגABיTר O‏EPEהHשE CלAחAEMשE K נEPEMEHHשM CץMMAM.
C
   70 RESULT=RESULT+QNOW
      ERREST=ERREST+ESTERR
      COR11=COR11+QDIFF/1023.0
C
C     ץCTAHOBיTר CלEהץא‎יך יHTEPBAל.
C
   72 IF(NIM.EQ.2*(NIM/2))GO TO 75
      NIM=NIM/2
      LEV=LEV-1
      GO TO 72
   75 NIM=NIM+1
      IF(LEV.LE.0)GO TO 80
C
C     COגPATר üלEMEHTש, HEOגXOהיMשE הלס CלEהץא‎EחO יHTEBAלA
C
      QPREV=QRIGHT(LEV)
      X0=X(16)
      F0=F(16)
      DO 78 I=1,8
      F(2*I)=FSAVE(I,LEV)
      X(2*I)=XSAVE(I,LEV)
   78 CONTINUE
      GO TO 30
C
C     *** üTAנ 8 *** תAKלא‏יTEלרHשE OנEPAדיי י BשXOה
C
   80 RESULT=RESULT+COR11
C
C     OגECנE‏יTר, ‏TOגש תHA‏EHיE נEPEMEHHOך ERREST
C     גשלO HE MEHרûE ץPOBHס OKPץחלEHיך
C
      IF(ERREST.EQ.0.0)RETURN
   82 TEMP=ABS(RESULT)+ERREST
      IF(TEMP.NE.ABS(RESULT))RETURN
      ERREST=2.0*ERREST
      GO TO 82
      END
