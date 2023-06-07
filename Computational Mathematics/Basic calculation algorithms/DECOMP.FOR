      SUBROUTINE DECOMP(NDIM,N,A,COND,IPVT,WORK)
C
      INTEGER NDIM,N
      REAL A(NDIM,N),COND,WORK(N)
      INTEGER IPVT(N)
C
C     נPOחPAMMA Bש‏יCלסET PAתלOצEHיE BE‎ECTBEHHOך MATPידש
C     נOCPEהCTBOM חAץCCOBA יCKלא‏EHיס י OדEHיBAET
C     OגץCלOBלEHHOCTר  MATPידש.
C
C     OHA יCנOלרתץETCס הלס Bש‏יCלEHיס PEûEHיך
C     ליHEךHשX CיCTEM.
C
C     BXOהHAס יHזOPMAדיס.
C
C     NDIM -תAסBלEHHAס CTPO‏HAס PAתMEPHOCTר MACCיBA,
C           COהEPצA‎EחO A.
C
C     N    -נOPסהOK MATPידש.
C
C     A    -MATPידA,KOTOPץא HץצHO PAתלOציTר.
C
C     BשXOהHAס יHזOPMAדיס.
C
C     A     COהEPציT BEPXHאא TPEץחOלרHץא MATPידץ U
C           י ץ‏יTשBAא‎ץא נEPECTAHOBKי BEPCיא
C           HיצHEך TPEץחOלרHOך MATPידש I-L,TAKיE,
C           ‏TO (MATPידA  נEPECTAHOBOK) *A=L*U
C
C     COND -OדEHKA OגץCלOBלEHHOCTי A.
C           הלס ליHEךHOך CיCTEMש A*X=B יתMEHEHיס B A י B
C           MOחץT BשתBATר  יתMEHEHיס B X,גOלרûיE B COND PAת.
C           ECלי COND+1.0.EQ.COND, TO  A B נPEהEלAX MAûיHHOך
C           TO‏HOCTי סBלסETCס BשPOצהEHHOך MATPידEך. COND
C           נOלAחAETCס PABHשM 1.0E+32,ECלי OגHAPץצEHA TO‏HAס
C           BשPOצהEHHOCTר.
C
C     IPVT -BEKTOP BEהץ‎יX üלEMEHTOB.
C           IPVT(K)=יHהEKC K-ך BEהץ‎Eך CTPOKי
C           IPVT(N)=(-1)**(‏יCלO נEPECTAHOBOK)
C
C     PAגO‏EE נOלE. BEKTOP WORK הOלצEH גשTר OניCAH י
C             BKלא‏EH B BשתOB. EחO BXOהHOE COהEPצAHיE Oגש‏HO
C             HE הAET BAצHOך יHזOPMAדיי.
C
C     OנPEהEליTEלר MATPידש A MOצET גשTר נOלץ‏EH HA BשXOהE
C     נO זOPMץלE:
C          DET(A)=IPVT(N)*A(1,1)*A(2,2)*...*A(N,N).
C
      REAL EK,T,ANORM,YNORM,ZNORM
      INTEGER NM1,I,J,K,KP1,KB,KM1,M
C
      IPVT(N)=1
      IF(N.EQ.1)GO TO 80
      NM1=N-1
C
C     Bש‏יCליTר 1-HOPMץ MATPידש A
C
      ANORM=0.0
      DO 10 J=1,N
        T=0.0
        DO 5 I=1,N
          T=T+ABS(A(I,J))
    5   CONTINUE
        IF(T.GT.ANORM) ANORM=T
   10 CONTINUE
C
C     חAץCCOBO יCKלא‏EHיE C ‏ACTי‏HשM BשגOPOM
C     BEהץ‎EחO üלEMEHTA
C
      DO 35 K=1,NM1
        KP1=K+1
C
C       HAךTי BEהץ‎יך üלEMEHT
C
        M=K
        DO 15 I=KP1,N
          IF(ABS(A(I,K)).GT.ABS(A(M,K))) M=I
   15   CONTINUE
        IPVT(K)=M
        IF(M.NE.K)IPVT(N)=-IPVT(N)
        T=A(M,K)
        A(M,K)=A(K,K)
        A(K,K)=T
C
C       נPOנץCTיTר üTOT ûAח,ECלי BEהץ‎יך üלEMEHT PABEH Hץלא
C
        IF(T.EQ.0.0)GO TO 35
C
C       Bש‏יCליTר MHOציTEלי
C
        DO 20 I=KP1,N
          A(I,K)=-A(I,K)/T
   20   CONTINUE
C
C       נEPECTABלסTר י יCKלא‏ATר נO CTOלגדAM
C
        DO 30 J=KP1,N
          T=A(M,J)
          A(M,J)=A(K,J)
          A(K,J)=T
          IF(T.EQ.0.0)GO TO 30
          DO 25 I=KP1,N
            A(I,J)=A(I,J)+A(I,K)*T
   25     CONTINUE
   30   CONTINUE
   35 CONTINUE
C
C     COND=(1-HOPMA MATPידש A)*(OדEHKA הלס 1-HOPMש MATPידש,
C     OגPATHOך K A)
C     OדEHKA נOלץ‏AETCס נOCPEהCTBOM OהHOחO ûAחA METOהA
C     OגPATHשX יTEPAדיך הלס HAיMEHרûEחO CיHחץלסPHOחO
C     BEKTOPA. üTO TPEגץET PEûEHיס הBץX CיCTEM ץPABHEHיך,
C     (TPAHCנOHיPOBAHHAס הלס A) *Y=E י A*Z=Y, חהE E-BEKTOP
C     ית +1 י -1, BשגPAHHשך TAK, ‏TOגש MAKCיMיתיPOBATר
C     BEלי‏יHץ Y.
C     ESTIMATE=(1-HOPMA Z)/(1-HOPMA Y)
C
C     PEûיTר CיCTEMץ (TPAHCנOHיPOBAHHAס הלס A)*Y=E
C
      DO 50 K=1,N
        T=0.0
        IF(K.EQ.1)GO TO 45
        KM1=K-1
        DO 40 I=1,KM1
          T=T+A(I,K)*WORK(I)
   40   CONTINUE
   45   EK=1.0
        IF(T.LT.0.0)EK=-1.0
        IF(A(K,K).EQ.0.0)GO TO 90
        WORK(K)=-(EK+T)/A(K,K)
   50 CONTINUE
      DO 60 KB=1,NM1
        K=N-KB
        T=WORK(K)
        KP1=K+1
        DO 55 I=KP1,N
          T=T+A(I,K)*WORK(I)
   55   CONTINUE
        WORK(K)=T
        M=IPVT(K)
        IF(M.EQ.K)GO TO 60
        T=WORK(M)
        WORK(M)=WORK(K)
        WORK(K)=T
   60 CONTINUE
C
      YNORM=0.0
      DO 65 I=1,N
        YNORM=YNORM+ABS(WORK(I))
   65 CONTINUE
C
C     PEûיTר CיCTEMץ A*Z=Y
C
      CALL SOLVE(NDIM,N,A,WORK,IPVT)
C
      ZNORM=0.0
      DO 70 I=1,N
        ZNORM=ZNORM+ABS(WORK(I))
   70 CONTINUE
C
C     OדEHיTר OגץCלOBלEHHOCTר
C
      COND=ANORM*ZNORM/YNORM
      IF(COND.LT.1.0)COND=1.0
      RETURN
C
C     Cלץ‏Aך MATPידש 1*1
C
   80 COND=1.0
      IF(A(1,1).NE.0.0)RETURN
C
C     TO‏HAס BשPOצהEHHOCTר
C
   90 CONTINUE
      COND=1.0E+32
      RETURN
      END
