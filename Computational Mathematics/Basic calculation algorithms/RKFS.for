      SUBROUTINE RKFS(F,NEQN,Y,T,TOUT,RELERR,ABSERR,IFLAG,
     *                YP,H,F1,F2,F3,F4,F5,SAVRE,SAVAE,
     *                NFE,KOP,INIT,JFLAG,KFLAG)
C
C     METO� P�H�E-K�TTA-�E���EP�A �ETBEPTO�O-��TO�O �OP��KA
C     RKFS �HTE�P�P�ET C�CTEM� O��KHOBEHH�X ����E-
C     PEH��A��H�X �PABHEH�� �EPBO�O �OP��KA(CM. KOM-
C     MEHTAP�� K RKF45). MACC�B� YP,F1,F2,F3,F4 � F5
C     (PA�MEPHOCT� �O KPA�HE� MEPE NEQN) � �EPEMEH-
C     H�E H,SAVRE,SAVAE,NFE,KOP,INIT,JFLAG � KFLAG
C     �C�O�����TC� BH�TP� �PO�PAMM� � B�HECEH� B
C     C��COK B��OBA,�TO�� COXPAH�T� �X O�PE�E�EH-
C     HOCT� �P� �OBTOPHOM O�PA�EH��.�O�TOM� �X �HA-
C     �EH�� HE �O��H�  ��MEH�T�C�  �O���OBATE�EM.
C     BO�MO�H�� �HTEPEC �PE�CTAB���T �APAMETP�
C     YP  -�PO��BO�HA� BEKTOPA PE�EH�� B TO�KE T
C     H   -�PE��O�A�AEM�� PA�MEP �A�A ��� O�EPE�HO�O �TA�A
C     NFE -C�ET��K ��C�A B���C�EH�� ��HK���
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
C     REMIN-�TO M�H�MA��HOE �O��CT�MOE �HA�EH�E ���
C     RELERR.�O��TK� �O����T� �O �TO� �O��PO�PAMME
C     �O�EE B�COK�� TO�HOCT� O���HO CTO�T O�EH�
C     �OPO�O � �A�ACT�� �E��C�E�H�.
C
      DATA REMIN/1.E-12/
C
C     CTO�MOCT� C�ETA KOHTPO��P�ETC� TPE�OBAH�EM,
C     �TO�� KO���ECTBO B���C�EH�� ��HK��� ���O O�-
C     PAH��EHO BE����HO�,�P������TE��HO PABHO� �HA-
C     �EH�� �APAMETPA MAXNFE.�P�H�TOE ��EC� �HA�E-
C     H�E �P�MEPHO COOTBETCTB�ET 500 �A�AM.
C
      DATA MAXNFE/3000/
C
C     �POBEP�T� BXO�H�E �APAMETP�
C
      IF(NEQN.LT.1)GO TO 10
      IF((RELERR.LT.0.0).OR.(ABSERR.LT.0.0))GO TO 10
      MFLAG=IABS(IFLAG)
      IF((MFLAG.EQ.0).OR.(MFLAG.GT.8))GO TO 10
      IF(MFLAG.NE.1)GO TO 20
C
C     �EPB�� B��OB,B���C��T� MA��HHOE ��C��OH
C
      EPS=1.0
    5 EPS=EPS/2.0
      EPSP1=EPS+1.
      IF(EPSP1.GT.1.)GO TO 5
      U26=26.*EPS
      GO TO 50
C
C     O���KA BXO�HO� �H�OPMA���
C
   10 IFLAG=8
      RETURN
C
C     �POBEP�T� BO�MO�HOCT� �PO�O��EH��
C
   20 IF((T.EQ.TOUT).AND.(KFLAG.NE.3))GO TO 10
      IF(MFLAG.NE.2)GO TO 25
C
C     IFLAG=+2 ��� -2
C
      IF((KFLAG.EQ.3).OR.(INIT.EQ.0))GO TO 45
      IF(KFLAG.EQ.4)GO TO 40
      IF((KFLAG.EQ.5).AND.(ABSERR.EQ.0.0))GO TO 30
      IF((KFLAG.EQ.6).AND.(RELERR.LE.SAVRE).AND.
     *(ABSERR.LE.SAVAE))GO TO 30
      GO TO 50
C
C     IFLAG=3,4,5,6,7 ��� 8
C
   25 IF(IFLAG.EQ.3)GO TO 45
      IF(IFLAG.EQ.4)GO TO 40
      IF((IFLAG.EQ.5).AND.(ABSERR.GT.0.0))GO TO 45
C
C     �HTE�P�POBAH�E HE���� �PO�O��AT�,�OCKO��K� �O��-
C     �OBATE�� HE B��O�H�� �HCTP�K���,COOTBETCTB����X
C     �HA�EH��M IFLAG=5,6,7 ��� 8
C
   30 PRINT 35
   35 FORMAT(/20X,48H�HTE�P�POBAH�E �PEPBAHO, �OCKO��K� �O���OBATE�� ,
     *11HHE B��O�H��/20X,34H�HCTP�K��� RKF45, COOTBETCTB����X ,
     *27H�HA�EH��M IFLAG=5,6,7 ��� 8)
      STOP
C
C     �EPEO�PE�E��T� C�ET��K ��C�A B���C�EH�� ��HK���
C
   40 NFE=0
      IF(MFLAG.EQ.2)GO TO 50
C
C     �EPEO�PE�E��T� �HA�EH�E FLAG,�CTAHOB�EHHOE
C     �P� �PE�����EM O�PA�EH��
C
   45 IFLAG=JFLAG
      IF(KFLAG.EQ.3)MFLAG=IABS(IFLAG)
C
C     COXPAH�T� BXO�HOE �HA�EH�E IFLAG � �CTAHOB�T�
C     �HA�EH�E FLAG, COOTBETCTB���EE �PO�O��EH��,
C     ��� �����E� �POBEPK�
C
   50 JFLAG=IFLAG
      KFLAG=0
C
C     COXPAH�T� �HA�EH�� RELERR � ABSERR ��� BXO�HO�
C     �POBEPK� �P� �OC�E�����X O�PA�EH��X
C
      SAVRE=RELERR
      SAVAE=ABSERR
C
C     �CTAHOB�T� �HA�EH�E �PAH��� ��� OTHOC�TE��-
C     HO� �O�PE�HOCT�,PABHOE KAK M�H�M�M 2*EPS+
C     REMIN,�TO�� ���E�AT� TP��HOCTE�,CB��AHH�X
C     C TPE�OBAH�EM HE�OCT���MO� TO�HOCT�
C
      RER=2.*EPS+REMIN
      IF(RELERR.GE.RER)GO TO 55
C
C     �A�AHHA� �PAH��A OTHOC�TE��HO� �O�PE�HOCT�
C     C���KOM MA�A
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
C     �P�CBOEH�E HA�A��H�X �HA�EH�� (�H����POBA-
C             H�E)-�CTAHOB�T� �HA�EH�E �KA�ATE��
C             OKOH�AH�� �H����POBAH��,INIT
C             �CTAHOB�T� �HA�EH�E �KA�ATE�� C���-
C             KOM �O���O�O �ATPE�OBAHHO�O ��C�A B�-
C             XO�H�X TO�EK,KOP
C             B���C��T� HA�A��H�E �PO��BO�H�E
C             �CTAHOB�T� �HA�EH�E C�ET��KA ��C�A
C             B���C�EH�� ��HK���,NFE
C             O�EH�T� HA�E��H�� BE����H� �A�A
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
C     �P�CBO�T� BE����HE �A�A �HAK,COOTBETCTB�����
C     �HTE�P�POBAH�� B HA�PAB�EH�� OT T K TOUT
C
   80 H=SIGN(H,DT)
C
C     �POBEPKA, HACKO��KO CEP�E�HO B���H�E HA RKF45
C     C���KOM �O���O�O �ATPE�OBAHHO�O ��C�A B�XO�-
C     H�X TO�EK
C
      IF(ABS(H).GE.2.0*ABS(DT))KOP=KOP+1
      IF(KOP.NE.100)GO TO 85
C
C     �PE�MEPHA� �ACTOTA B�XO�OB
C
      KOP=0
      IFLAG=7
      RETURN
C
   85 IF(ABS(DT).GT.U26*ABS(T))GO TO 95
C
C     EC�� O�EH� ����KO K TO�KE B�XO�A,�PO�KCTPA�O-
C     ��POBAT� � BEPH�T�C� �O MECT� B��OBA
C
      DO 90 K=1,NEQN
   90 Y(K)=Y(K)+DT*YP(K)
      A=TOUT
      CALL F(A,Y,YP)
      NFE=NFE+1
      GO TO 300
C
C     �P�CBO�T� HA�A��HOE �HA�EH�E �H��KATOP� TO�K�
C     B�XO�A
C
   95 OUTPUT=.FALSE.
C
C     �TO�� ���E�AT� HEO�PAB�AHHO�O MA��HHO�O H���
C     �P� B���C�EH�� ��HK��� OT �PAH�� �O�PE�HO-
C     CTE�,�POMAC�TA��POBAT� �T� �PAH���
C
      SCALE=2./RELERR
      AE=SCALE*ABSERR
C
C     �O�A�OBOE �HTE�P�POBAH�E
C
  100 HFAILD=.FALSE.
C
C     �CTAHOB�T� HA�MEH���� �O��CT�M�� BE����H� �A�A
C
      HMIN=U26*ABS(T)
C
C     �C�PAB�T� �P� HEO�XO��MOCT� BE����H� �A�A,
C     �TO�� �OCT��H�T� TO�K� B�XO�A. PACC��TAT� HA
C     �BA �A�A B�EPE�,�TO�� ���E�AT� C���KOM PE�K�X
C     ��MEHEH�� B BE����HE �A�A � TEM CAM�M �MEH�-
C     ��T� B���H�E B�XO�H�X TO�EK HA �PO�PAMM�.
C
      DT=TOUT-T
      IF(ABS(DT).GE.2.*ABS(H))GO TO 200
      IF(ABS(DT).GT.ABS(H))GO TO 150
C
C     C�E������ �C�E�H�� �A� �ABEP��T �HTE�P�PO-
C     BAH�E �O �KA�AHHO� TO�K� B�XO�A
C
      OUTPUT=.TRUE.
      H=DT
      GO TO 200
C
  150 H=0.5*DT
C
C
C
C     BH�TPEHH�� O�HO�A�OB�� �HTE�PATOP
C
C     �PAH��� �O�PE�HOCTE� ���� �POMAC�TA��POBAH�,
C     �TO�� ���E�AT� HEO�PAB�AHHO�O MA��HHO�O H���
C     �P� B���C�EH�� ��HK��� OT H�X.
C     �TO�� ���E�AT� O�PA�EH�� B H��� �HAMEHATE��
C     B TECTE,OTHOC�TE��HA� O���KA ��MEP�ETC�  �O
C     OTHO�EH�� K CPE�HEM�  �� BE����H PE�EH��
C     B HA�A�E � KOH�E �A�A.
C     B �OPM��E,O�EH�BA��E� O���K�,�PO��BE�EHA
C     �P����POBKA C�A�AEM�X,�MEH��A��A� �OTEP�
C     BEPH�X �HAKOB.
C     �TO�� PA����AT� ME��� CO�O� PA�H�E AP��MEHT�,
C     ��� H HE �O��CKA�TC� �HA�EH��,MEH���E �MHO-
C     �EHHO� HA 26 O���K� OKP���EH�� B T.
C     BBE�EH� �PAKT��ECK�E O�PAH��EH�� HA CKOPOCT�
C     ��MEHEH�� BE����H� �A�A,�TO�� C��A��T� �PO-
C     �ECC B��OPA �TO� BE����H� � ���E�AT� �PE�MEP-
C     HO�O EE PA��POCA B �A�A�AX C HAP��EH�EM HE�PE-
C     P�BHOCT�.
C     �� �PE�OCTOPO�HOCT� �PO�PAMMA �EPET 9/10 OT TO�
C     BE����H� �A�A,KOTOPA� H��HA �O EE O�EHKE.
C     EC��HA �AHHOM �A�E ���A HE��A�HA� �O��TKA
C     TO �P� ��AH�POBAH�� C�E����E�O �BE���EH�E
C     ���H� �A�A HE �O��CKAETC�. �TO �OB��AET ���EK-
C     T�BHOCT� �PO�PAMM� ��� �A�A� C PA�P�BAM� �
C     B O��EM C���AE,�OCKO��K� �C�O����ETC� �OKA��-
C     HA� �KCTPA�O����� � �O�O�H�TE��HA� �PE�OCTO-
C     PO�HOCT� KA�ETC� O�PAB�AHHO�.
C
C
C     �POBEP�T� ��C�O B���C�EH�� �PO��BO�H�X.EC��
C     OHO HE �PEB��AET �CTAHOB�EHHO�O �PE�E�A,�O-
C     �PO�OBAT� �PO�O���T� �HTE�P�POBAH�E C T �O T+H
C
  200 IF(NFE.LE.MAXNFE)GO TO 220
C
C     C���KOM �O���A� PA�OTA
C
      IFLAG=4
      KFLAG=4
      RETURN
C
C     �PO�O���T� �P�����EHHOE PE�EH�E HA O��H �A� ���H� H
C
  220 CALL FEHL(F,NEQN,Y,T,H,YP,F1,F2,F3,F4,F5,F1)
      NFE=NFE+5
C
C     B���C��T� � CPABH�T� �O��CT�M�E �PAH��� �
C     O�EHK� �OKA��HO� O���,A �ATEM CH�T� MAC�TA-
C     ��POBAH�E �PAH��.�AMET�TE,�TO OTHOC�TE��HA�
C     O���KA ��MEP�ETC� �O OTHO�EH�� K CPE�HEM� ��
C     BE����H PE�EH�� B HA�A�E � KOH�E �A�A.
C
      EEOET=0.
      DO 250 K=1,NEQN
      ET=ABS(Y(K))+ABS(F1(K))+AE
      IF(ET.GT.0.)GO TO 240
C
C     HE�PAB���HA� �PAH��A �O�PE�HOCT�
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
C     HE��A�H�� �A�
C            �MEH���T� BE����H� �A�A � CHOBA �O-
C            �PO�OBAT�
C            �MEH��EH�E O�PAH���BAETC� CH��� MHO-
C            ��TE�EM 1/10
C
      HFAILD=.TRUE.
      OUTPUT=.FALSE.
      S=0.1
      IF(ESTTOL.LT.59049.)S=0.9/ESTTOL**0.2
      H=S*H
      IF(ABS(H).GT.HMIN)GO TO 200
C
C     �A�AHHA� �PAH��A O���K� HE�OCT���MA �A�E �P�
C     HA�MEH��E� �O��CT�MO� BE����HE �A�A
C
      IFLAG=6
      KFLAG=6
      RETURN
C
C
C     �C�E�H�� �A�
C            �OMECT�T� B MACC�B Y PE�EH�E B TO�KE
C            T+H � B���C��T� �PO��BO�H�E B �TO�
C            TO�KE
C
  260 T=T+H
      DO 270 K=1,NEQN
  270 Y(K)=F1(K)
      A=T
      CALL F(A,Y,YP)
      NFE=NFE+1
C
C
C     B��PAT� BE����H� C�E����E�O �A�A
C     �BE���EH�E O�PAH��EHO MHO��TE�EM 5
C     EC�� HA �AHHOM �A�E ���A HE��A�HA�
C     �O��TKA,TO ��� C�E����E�O HE �O��-
C     CKAETC� B��OP �O���E� BE����H� �A�A
C
      S=5.
      IF(ESTTOL.GT.1.889568E-4)S=0.9/ESTTOL**0.2
      IF(HFAILD)S=AMIN1(S,1.0)
      H=SIGN(AMAX1(S*ABS(H),HMIN),H)
C
C     KOHE� O�HO�A�OBO�O �HTE�PATOPA
C
C
C     H��HO �� �E�AT� O�EPE�HO� �A�
C
      IF(OUTPUT)GO TO 300
      IF(IFLAG.GT.0)GO TO 100
C
C
C     �HTE�P�POBAH�E �C�E�HO �ABEP�EHO
C
C     PE��M O�HO�A�OBO�O �HTE�P�POBAH��
C
      IFLAG=-2
      RETURN
C
C     PE��M �HTE�P�POBAH�� HA �HTEPBA�E
C
  300 T=TOUT
      IFLAG=2
      RETURN
C
      END
