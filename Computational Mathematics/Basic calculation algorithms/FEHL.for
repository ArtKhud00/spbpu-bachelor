      SUBROUTINE FEHL(F,NEQN,Y,T,H,YP,F1,F2,F3,F4,F5,S)
C
C     METO� P�H�E-K�TTA-�E���EP�A �ETBEPTO�O-��TO�O �OP��KA
C
C     �O��PO�PAMMA FEHL �HTE�P�P�ET C�CTEM� �� NEQN
C     O��KHOBEHH�X ����EPEH��A��H�X �PABHEH�� �EPBO�O
C     �OP��KA C�E����E�O B��A
C
C          DY(I)/DT=F(T,Y(1),...Y(NEQN)),
C
C     ��E HA�A��H�E �HA�EH�� Y(I) � HA�A��H�E �PO��BO�H�E
C     YP(I) �A�AH� B HA�A��HO� TO�KE T. FEHL �PO�O��AET
C     PE�EH�E HA ��KC�POBAHH�� �A� H � �OME�AET B MACC�B
C     S(I) �P�����EH�E K PE�EH�� B TO�KE T+H, �ME��EE ��T��
C     �OP��OK TO�HOCT� (�OKA��H�� �OP��OK PABEH �ECT�).
C     F1,...F5-MACC�B� PA�MEPHOCT� NEQN,HEO�XO��M�E BH�TP�
C     �PO�PAMM�.
C          B �OPM��AX �PO��BE�EHA �P����POBKA C �E���
C     �MEH���T� �OTEP� BEPH�X �HAKOB.
C          �TO�� MO�HO ���O PA����AT� PA�H�E HE�AB�C�M�E
C     AP��MEHT�, �P� O�PA�EH�� K FEHL HE C�E��ET �A�ABAT�
C     ��� H �HA�EH�E,MEH��EE �MHO�EHHHO� HA 13 O���K�
C     OKP���EH�� B T.
C
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
C
C     B���C��T� �P�����EHHOE PE�EH�E B TO�KE T+H
C
      CH=H/7618050.0
      DO 230 K=1,NEQN
  230 S(K)=Y(K)+CH*((902880.0*YP(K)+(3855735.0*F3(K)-
     *     1371249.0*F4(K)))+(3953664.0*F2(K)+277020.0*F5(K)))
C
      RETURN
      END
