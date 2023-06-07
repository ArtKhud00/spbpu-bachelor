      SUBROUTINE RKFS(F,NEQN,Y,T,TOUT,RELERR,ABSERR,IFLAG,
     *                YP,H,F1,F2,F3,F4,F5,SAVRE,SAVAE,
     *                NFE,KOP,INIT,JFLAG,KFLAG)
C
C     METOה PץHחE-KץTTA-זEלרגEPחA ‏ETBEPTOחO-נסTOחO נOPסהKA
C     RKFS יHTEחPיPץET CיCTEMץ OגשKHOBEHHשX היזזE-
C     PEHדיAלרHשX ץPABHEHיך נEPBOחO נOPסהKA(CM. KOM-
C     MEHTAPיך K RKF45). MACCיBש YP,F1,F2,F3,F4 י F5
C     (PAתMEPHOCTי נO KPAךHEך MEPE NEQN) י נEPEMEH-
C     HשE H,SAVRE,SAVAE,NFE,KOP,INIT,JFLAG י KFLAG
C     יCנOלרתץאTCס BHץTPי נPOחPAMMש י BשHECEHש B
C     CניCOK BשתOBA,‏TOגש COXPAHיTר יX OנPEהEלEH-
C     HOCTר נPי נOBTOPHOM OגPA‎EHיי.נOüTOMץ יX תHA-
C     ‏EHיס HE הOלצHש  יתMEHסTרCס  נOלרתOBATEלEM.
C     BOתMOצHשך יHTEPEC נPEהCTABלסאT נAPAMETPש
C     YP  -נPOיתBOהHAס BEKTOPA PEûEHיס B TO‏KE T
C     H   -נPEהנOלAחAEMשך PAתMEP ûAחA הלס O‏EPEהHOחO üTAנA
C     NFE -C‏ET‏יK ‏יCלA Bש‏יCלEHיך זץHKדיי
C
      LOGICAL HFAILD,OUTPUT
C
      INTEGER NEQN,IFLAG,NFE,KOP,INIT,JFLAG,KFLAG
      REAL Y(NEQN),T,TOUT,RELERR,ABSERR,H,YP(NEQN),
     *     F1(NEQN),F2(NEQN),F3(NEQN),F4(NEQN),F5(NEQN),
     *     SAVRE,SAVAE
C
C     EXTERNAL F
C
      REAL A,AE,DT,EE,EEOET,ESTTOL,ET,HMIN,REMIN,
     *     RER,S,SCAL,TOL,TOLN,U26,EPSP1,EPS,YPK
C
      INTEGER K,MAXNFE,MFLAG
C
      REAL AMAX1,AMIN1
C
C     REMIN-üTO MיHיMAלרHOE הOנץCTיMOE תHA‏EHיE הלס
C     RELERR.נOנשTKי נOלץ‏יTר נO üTOך נOהנPOחPAMME
C     גOלEE BשCOKץא TO‏HOCTר Oגש‏HO CTOסT O‏EHר
C     הOPOחO י תA‏ACTץא גEתץCנEûHש.
C
      DATA REMIN/1.E-12/
C
C     CTOיMOCTר C‏ETA KOHTPOליPץETCס TPEגOBAHיEM,
C     ‏TOגש KOלי‏ECTBO Bש‏יCלEHיך זץHKדיי גשלO Oח-
C     PAHי‏EHO BEלי‏יHOך,נPיגליתיTEלרHO PABHOך תHA-
C     ‏EHיא נAPAMETPA MAXNFE.נPיHסTOE תהECר תHA‏E-
C     HיE נPיMEPHO COOTBETCTBץET 500 ûAחAM.
C
      DATA MAXNFE/3000/
C
C     נPOBEPיTר BXOהHשE נAPAMETPש
C
      IF(NEQN.LT.1)GO TO 10
      IF((RELERR.LT.0.0).OR.(ABSERR.LT.0.0))GO TO 10
      MFLAG=IABS(IFLAG)
      IF((MFLAG.EQ.0).OR.(MFLAG.GT.8))GO TO 10
      IF(MFLAG.NE.1)GO TO 20
C
C     נEPBשך BשתOB,Bש‏יCליTר MAûיHHOE üנCילOH
C
      EPS=1.0
    5 EPS=EPS/2.0
      EPSP1=EPS+1.
      IF(EPSP1.GT.1.)GO TO 5
      U26=26.*EPS
      GO TO 50
C
C     OûיגKA BXOהHOך יHזOPMAדיי
C
   10 IFLAG=8
      RETURN
C
C     נPOBEPיTר BOתMOצHOCTי נPOהOלצEHיס
C
   20 IF((T.EQ.TOUT).AND.(KFLAG.NE.3))GO TO 10
      IF(MFLAG.NE.2)GO TO 25
C
C     IFLAG=+2 ילי -2
C
      IF((KFLAG.EQ.3).OR.(INIT.EQ.0))GO TO 45
      IF(KFLAG.EQ.4)GO TO 40
      IF((KFLAG.EQ.5).AND.(ABSERR.EQ.0.0))GO TO 30
      IF((KFLAG.EQ.6).AND.(RELERR.LE.SAVRE).AND.
     *(ABSERR.LE.SAVAE))GO TO 30
      GO TO 50
C
C     IFLAG=3,4,5,6,7 ילי 8
C
   25 IF(IFLAG.EQ.3)GO TO 45
      IF(IFLAG.EQ.4)GO TO 40
      IF((IFLAG.EQ.5).AND.(ABSERR.GT.0.0))GO TO 45
C
C     יHTEחPיPOBAHיE HEלרתס נPOהOלצATר,נOCKOלרKץ נOלר-
C     תOBATEלר HE BשנOלHיל יHCTPץKדיך,COOTBETCTBץא‎יX
C     תHA‏EHיסM IFLAG=5,6,7 ילי 8
C
   30 PRINT 35
   35 FORMAT(/20X,48HיHTEחPיPOBAHיE נPEPBAHO, נOCKOלרKץ נOלרתOBATEלר ,
     *11HHE BשנOלHיל/20X,34HיHCTPץKדיך RKF45, COOTBETCTBץא‎יX ,
     *27HתHA‏EHיסM IFLAG=5,6,7 ילי 8)
      STOP
C
C     נEPEOנPEהEליTר C‏ET‏יK ‏יCלA Bש‏יCלEHיך זץHKדיי
C
   40 NFE=0
      IF(MFLAG.EQ.2)GO TO 50
C
C     נEPEOנPEהEליTר תHA‏EHיE FLAG,ץCTAHOBלEHHOE
C     נPי נPEהשהץ‎EM OגPA‎EHיי
C
   45 IFLAG=JFLAG
      IF(KFLAG.EQ.3)MFLAG=IABS(IFLAG)
C
C     COXPAHיTר BXOהHOE תHA‏EHיE IFLAG י ץCTAHOBיTר
C     תHA‏EHיE FLAG, COOTBETCTBץא‎EE נPOהOלצEHיא,
C     הלס גץהץ‎Eך נPOBEPKי
C
   50 JFLAG=IFLAG
      KFLAG=0
C
C     COXPAHיTר תHA‏EHיס RELERR י ABSERR הלס BXOהHOך
C     נPOBEPKי נPי נOCלEהץא‎יX OגPA‎EHיסX
C
      SAVRE=RELERR
      SAVAE=ABSERR
C
C     ץCTAHOBיTר תHA‏EHיE חPAHידש הלס OTHOCיTEלר-
C     HOך נOחPEûHOCTי,PABHOE KAK MיHיMץM 2*EPS+
C     REMIN,‏TOגש יתגEצATר TPץהHOCTEך,CBסתAHHשX
C     C TPEגOBAHיEM HEהOCTיציMOך TO‏HOCTי
C
      RER=2.*EPS+REMIN
      IF(RELERR.GE.RER)GO TO 55
C
C     תAהAHHAס חPAHידA OTHOCיTEלרHOך נOחPEûHOCTי
C     CליûKOM MAלA
C
      RELERR=RER
      IFLAG=3
      KFLAG=3
      RETURN
C
   55 DT=TOUT-T
C
      IF(MFLAG.EQ.1)GO TO 60
      IF(INIT.EQ.0)GO TO 65
      GO TO 80
C
C     נPיCBOEHיE HA‏AלרHשX תHA‏EHיך (יHידייPOBA-
C             HיE)-ץCTAHOBיTר תHA‏EHיE ץKAתATEלס
C             OKOH‏AHיס יHידייPOBAHיס,INIT
C             ץCTAHOBיTר תHA‏EHיE ץKAתATEלס Cליû-
C             KOM גOלרûOחO תATPEגOBAHHOחO ‏יCלA Bש-
C             XOהHשX TO‏EK,KOP
C             Bש‏יCליTר HA‏AלרHשE נPOיתBOהHשE
C             ץCTAHOBיTר תHA‏EHיE C‏ET‏יKA ‏יCלA
C             Bש‏יCלEHיך זץHKדיי,NFE
C             OדEHיTר HA‏EלרHץא BEלי‏יHץ ûAחA
C
   60 INIT=0
      KOP=0
C
      A=T
      CALL F(A,Y,YP)
      NFE=1
      IF(T.NE.TOUT)GO TO 65
      IFLAG=2
      RETURN
C
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
C
C     נPיCBOיTר BEלי‏יHE ûAחA תHAK,COOTBETCTBץא‎יך
C     יHTEחPיPOBAHיא B HAנPABלEHיי OT T K TOUT
C
   80 H=SIGN(H,DT)
C
C     נPOBEPKA, HACKOלרKO CEPרEתHO BליסHיE HA RKF45
C     CליûKOM גOלרûOחO תATPEגOBAHHOחO ‏יCלA BשXOה-
C     HשX TO‏EK
C
      IF(ABS(H).GE.2.0*ABS(DT))KOP=KOP+1
      IF(KOP.NE.100)GO TO 85
C
C     ‏PEתMEPHAס ‏ACTOTA BשXOהOB
C
      KOP=0
      IFLAG=7
      RETURN
C
   85 IF(ABS(DT).GT.U26*ABS(T))GO TO 95
C
C     ECלי O‏EHר גליתKO K TO‏KE BשXOהA,נPOüKCTPAנO-
C     ליPOBATר י BEPHץTרCס נO MECTץ BשתOBA
C
      DO 90 K=1,NEQN
   90 Y(K)=Y(K)+DT*YP(K)
      A=TOUT
      CALL F(A,Y,YP)
      NFE=NFE+1
      GO TO 300
C
C     נPיCBOיTר HA‏AלרHOE תHA‏EHיE יHהיKATOPץ TO‏Kי
C     BשXOהA
C
   95 OUTPUT=.FALSE.
C
C     ‏TOגש יתגEצATר HEOנPABהAHHOחO MAûיHHOחO Hץלס
C     נPי Bש‏יCלEHיי זץHKדיי OT חPAHיד נOחPEûHO-
C     CTEך,נPOMACûTAגיPOBATר üTי חPAHידש
C
      SCALE=2./RELERR
      AE=SCALE*ABSERR
C
C     נOûAחOBOE יHTEחPיPOBAHיE
C
  100 HFAILD=.FALSE.
C
C     ץCTAHOBיTר HAיMEHרûץא הOנץCTיMץא BEלי‏יHץ ûAחA
C
      HMIN=U26*ABS(T)
C
C     יCנPABיTר נPי HEOגXOהיMOCTי BEלי‏יHץ ûAחA,
C     ‏TOגש הOCTיחHץTר TO‏Kי BשXOהA. PACC‏יTATר HA
C     הBA ûAחA BנEPEה,‏TOגש יתגEצATר CליûKOM PEתKיX
C     יתMEHEHיך B BEלי‏יHE ûAחA י TEM CAMשM ץMEHר-
C     ûיTר BליסHיE BשXOהHשX TO‏EK HA נPOחPAMMץ.
C
      DT=TOUT-T
      IF(ABS(DT).GE.2.*ABS(H))GO TO 200
      IF(ABS(DT).GT.ABS(H))GO TO 150
C
C     CלEהץא‎יך ץCנEûHשך ûAח תABEPûיT יHTEחPיPO-
C     BAHיE הO ץKAתAHHOך TO‏Kי BשXOהA
C
      OUTPUT=.TRUE.
      H=DT
      GO TO 200
C
  150 H=0.5*DT
C
C
C
C     BHץTPEHHיך OהHOûAחOBשך יHTEחPATOP
C
C     חPAHידש נOחPEûHOCTEך גשלי נPOMACûTAגיPOBAHש,
C     ‏TOגש יתגEצATר HEOנPABהAHHOחO MAûיHHOחO Hץלס
C     נPי Bש‏יCלEHיי זץHKדיי OT HיX.
C     ‏TOגש יתגEצATר OגPA‎EHיס B Hץלר תHAMEHATEלס
C     B TECTE,OTHOCיTEלרHAס OûיגKA יתMEPסETCס  נO
C     OTHOûEHיא K CPEהHEMץ  ית BEלי‏יH PEûEHיס
C     B HA‏AלE י KOHדE ûAחA.
C     B זOPMץלE,OדEHיBAא‎Eך OûיגKץ,נPOיתBEהEHA
C     חPץנניPOBKA CלAחAEMשX,ץMEHרûAא‎Aס נOTEPא
C     BEPHשX תHAKOB.
C     ‏TOגש PAתלי‏ATר MEצהץ COגOך PAתHשE APחץMEHTש,
C     הלס H HE הOנץCKAאTCס תHA‏EHיס,MEHרûיE ץMHO-
C     צEHHOך HA 26 OûיגKי OKPץחלEHיס B T.
C     BBEהEHש נPAKTי‏ECKיE OחPAHי‏EHיס HA CKOPOCTר
C     יתMEHEHיס BEלי‏יHש ûAחA,‏TOגש CחלAהיTר נPO-
C     דECC BשגOPA üTOך BEלי‏יHש י יתגEצATר ‏PEתMEP-
C     HOחO EE PAתגPOCA B תAהA‏AX C HAPץûEHיEM HEנPE-
C     PשBHOCTי.
C     ית נPEהOCTOPOצHOCTי נPOחPAMMA גEPET 9/10 OT TOך
C     BEלי‏יHש ûAחA,KOTOPAס HץצHA נO EE OדEHKE.
C     ECליHA הAHHOM ûAחE גשלA HEץהA‏HAס נOנשTKA
C     TO נPי נלAHיPOBAHיי CלEהץא‎EחO ץBEלי‏EHיE
C     הליHש ûAחA HE הOנץCKAETCס. üTO נOBשûAET üזזEK-
C     TיBHOCTר נPOחPAMMש הלס תAהA‏ C PAתPשBAMי י
C     B Oג‎EM Cלץ‏AE,נOCKOלרKץ יCנOלרתץETCס לOKAלר-
C     HAס üKCTPAנOלסדיס י הOנOלHיTEלרHAס נPEהOCTO-
C     POצHOCTר KAצETCס OנPABהAHHOך.
C
C
C     נPOBEPיTר ‏יCלO Bש‏יCלEHיך נPOיתBOהHשX.ECלי
C     OHO HE נPEBשûAET ץCTAHOBלEHHOחO נPEהEלA,נO-
C     נPOגOBATר נPOהOלציTר יHTEחPיPOBAHיE C T הO T+H
C
  200 IF(NFE.LE.MAXNFE)GO TO 220
C
C     CליûKOM גOלרûAס PAגOTA
C
      IFLAG=4
      KFLAG=4
      RETURN
C
C     נPOהOלציTר נPיגליצEHHOE PEûEHיE HA OהיH ûAח הליHש H
C
  220 CALL FEHL(F,NEQN,Y,T,H,YP,F1,F2,F3,F4,F5,F1)
      NFE=NFE+5
C
C     Bש‏יCליTר י CPABHיTר הOנץCTיMשE חPAHידש י
C     OדEHKי לOKAלרHOך Oûיג,A תATEM CHסTר MACûTA-
C     גיPOBAHיE חPAHיד.תAMETרTE,‏TO OTHOCיTEלרHAס
C     OûיגKA יתMEPסETCס נO OTHOûEHיא K CPEהHEMץ ית
C     BEלי‏יH PEûEHיס B HA‏AלE י KOHדE ûAחA.
C
      EEOET=0.
      DO 250 K=1,NEQN
      ET=ABS(Y(K))+ABS(F1(K))+AE
      IF(ET.GT.0.)GO TO 240
C
C     HEנPABילרHAס חPAHידA נOחPEûHOCTי
C
      IFLAG=5
      KFLAG=5
      RETURN
C
  240 EE=ABS((-2090.*YP(K)+(21970.*F3(K)-15048.*F4(K)))
     *  +(22528.*F2(K)-27360.*F5(K)))
  250 EEOET=AMAX1(EEOET,EE/ET)
C
      ESTTOL=ABS(H)*EEOET*SCALE/752400.
C
      IF(ESTTOL.LE.1.0)GO TO 260
C
C
C     HEץהA‏Hשך ûAח
C            ץMEHרûיTר BEלי‏יHץ ûAחA י CHOBA נO-
C            נPOגOBATר
C            ץMEHרûEHיE OחPAHי‏יBAETCס CHיתץ MHO-
C            ציTEלEM 1/10
C
      HFAILD=.TRUE.
      OUTPUT=.FALSE.
      S=0.1
      IF(ESTTOL.LT.59049.)S=0.9/ESTTOL**0.2
      H=S*H
      IF(ABS(H).GT.HMIN)GO TO 200
C
C     תAהAHHAס חPAHידA OûיגKי HEהOCTיציMA הAצE נPי
C     HAיMEHרûEך הOנץCTיMOך BEלי‏יHE ûAחA
C
      IFLAG=6
      KFLAG=6
      RETURN
C
C
C     ץCנEûHשך ûAח
C            נOMECTיTר B MACCיB Y PEûEHיE B TO‏KE
C            T+H י Bש‏יCליTר נPOיתBOהHשE B üTOך
C            TO‏KE
C
  260 T=T+H
      DO 270 K=1,NEQN
  270 Y(K)=F1(K)
      A=T
      CALL F(A,Y,YP)
      NFE=NFE+1
C
C
C     BשגPATר BEלי‏יHץ CלEהץא‎EחO ûAחA
C     ץBEלי‏EHיE OחPAHי‏EHO MHOציTEלEM 5
C     ECלי HA הAHHOM ûAחE גשלA HEץהA‏HAס
C     נOנשTKA,TO הלס CלEהץא‎EחO HE הOנץ-
C     CKAETCס BשגOP גOלרûEך BEלי‏יHש ûAחA
C
      S=5.
      IF(ESTTOL.GT.1.889568E-4)S=0.9/ESTTOL**0.2
      IF(HFAILD)S=AMIN1(S,1.0)
      H=SIGN(AMAX1(S*ABS(H),HMIN),H)
C
C     KOHEד OהHOûAחOBOחO יHTEחPATOPA
C
C
C     HץצHO לי הEלATר O‏EPEהHOך ûAח
C
      IF(OUTPUT)GO TO 300
      IF(IFLAG.GT.0)GO TO 100
C
C
C     יHTEחPיPOBAHיE ץCנEûHO תABEPûEHO
C
C     PEציM OהHOûAחOBOחO יHTEחPיPOBAHיס
C
      IFLAG=-2
      RETURN
C
C     PEציM יHTEחPיPOBAHיס HA יHTEPBAלE
C
  300 T=TOUT
      IFLAG=2
      RETURN
C
      END
