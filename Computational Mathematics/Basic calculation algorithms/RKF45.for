      SUBROUTINE RKF45(F,NEQN,Y,T,TOUT,RELERR,ABSERR,
     *                 IFLAG,WORK,IWORK)
C
C     METOд PхHзE-KхTTA жEмшвEPзA юETBEPTOзO-рсTOзO рOPсдKA
C
C     COCTABйTEмй рPOзPAMMщ-H.A.WATTS,L.F.SHAMPINE
C                SANDIA LABORATORIES
C              ALBUQUERQUE, NEW MEXICO
C
C     RKF45 рPEдHAъHAюEHA змABHщM OвPAъOM дмс PEыEHйс
C     HEцECTKйX й CмAвO цECTKйX дйжжEPEHгйAмшHщX хPABHEHйк,
C     KOздA BщюйCмEHйE рPOйъBOдHщX HE CмйыKOM дOPOзOCTOсэEE.
C     RKF45, BOOвэE зOBOPс,HE CмEдхET йCрOмшъOBATш
C     ECмй рOмшъOBATEма TPEвхETCс BщCOKAс TOюHOCTш
C
C     PEъаME
C
C     рOдрPOзPAMMA RKF45 йHTEзPйPхET CйCTEMх йъ NEQN
C     OвщKHOBEHHщX дйжжEPEHгйAмшHщX хPABHEHйк рEPBOзO
C     рOPсдKA CмEдхаэEзO BйдA:
C
C            DY(I)/DT=F(T,Y(1),Y(2),...,Y(NEQN),
C
C     здE Y(I) ъAдAHщ B T.
C     OвщюHO рOдрPOзPAMMх рPйMEHсаT дмс йHTEзPйPOBAHйс
C     OT T дO TOUT, OдHAKO EE MOцHO йCрOмшъOBATш й KAK
C     OдHOыAзOBщк йHTEзPATOP,юTOвщ рPOдOмцйTш PEыEHйE HA
C     OдйH ыAз B HAрPABмEHйй TOUT.HA BщXOдE рAPAMETPAM,
C     жйзхPйPхаэйM B CрйCKE BщъOBA, рPйCBAйBAаTCс ъHAюEHйс,
C     HEOвXOдйMщE дмс рPOдOмцEHйс йHTEзPйPOBAHйс. рOмшъO-
C     BATEма HхцHO мйыш EэE PAъ OвPATйTшCс K RKF45
C     (й,BOъMOцHO,OрPEдEмйTш HOBOE ъHAюEHйE дмс TOUT).
C     B дEкCTBйTEмшHOCTй RKF45-ьTO рPOзPAMMA йHTEPжEкCA,
C     KOTOPAс BщъщBAET рOдрPOзPAMMх RKFS, OCхэECTBмсаэха
C     рPOгECC PEыEHйс.RKFS B CBOа OюEPEдш BщъщBAET рOдрPOз-
C     PAMMх FEHL, KOTOPAс BщюйCмсET рPйвмйцEHHOE PEыEHйE
C     HA OдйH ыAз.
C
C     RKF45 йCрOмшъхET METOд PхHзE-KхTTA-жEмшвEPзA, OрйCAHHщк
C     B CмEдхаэEк рхвмйKAгйй:E.FEHLBERG,LOW-ORDER CLASSICAL
C     RUNGE-KUTTA FORMULAS WITH STEPSIZE CONTROL,NASA TR R-315
C
C     CTймш PAвOTщ рPOзPAMMщ RKF45 йммаCTPйPхETCс B CмEдхаэйX
C     рхвмйKAгйсX:L.F.SHAMPINE,H.A.WATTS,S.DAVENPORT, SOLVING
C     NON-STIFF ORDINARY DIFFERENTIAL EQUATIONS-THE STAT OF
C     THE ART,SANDIA LABORATORIES REPORT SAND75-0182,SIAM
C     REVIEW,18(1976), N3,376-411.
C
C     рAPAMETPщ рPOзPAMMщ:
C
C     F       -рOдрPOзPAMMA F(T,Y,YP) дмс BщюйCмEHйс
C              рPOйъBOдHщX YP(I)=DY(I)/DT
C     NEQN    -юйCмO йHTEзPйPхEMщX хPABHEHйк
C     Y(*)    -PEыEHйE B TOюKE T
C     T       -HEъABйCйMAс рEPEMEHHAс
C     TOUT    -TOюKA BщXOдA,B KOTOPOк HхцHO OрPEдEмйTш
C              ъHAюEHйE PEыEHйс
C     RELERR  -зPAHйгA OTHOCйTEмшHOк рOзPEыHOCTй
C              дмс TECTA мOKAмшHOк OыйвKй.
C     ABSERR  -зPAHйгA ABCOмаTHOк рOзPEыHOCTй
C              дмс TECTA мOKAмшHOк OыйвKй.
C              HA KAцдOM ыAзE рPOзPAMMA TPEвхET BщрOмHEHйс хCмOBйс
C              ABS(LOCAL ERROR).LE.RELERR*ABS(Y)+ABSERR
C              дмс KAцдOк KOMрOHEHTщ BEKTOPOB мOKAмшHOк
C              OыйвKй й PEыEHйс
C     IFLAG   -хKAъATEмш PEцйMA йHTEзPйPOBAHйс.
C     WORK(*) -MACCйB,COдEPцAэйк йHжOPMAгйа,BHхTPEHHаа дмс RKF45,
C              KOTOPAс HEOвXOдйMA рPй рOCмEдхаэйX BщъOBAX.EзO
C              PAъMEPHOCTш дOмцHA вщTш HE MEHшыE 3+6*NEQN
C     IWORK(*)-гEмщк MACCйB,COдEPцAэйк йHжOPMAгйа,BHхTPEHHаа дмс
C              RKF45,KOTOPAс HEOвXOдйMA рPй рOCмEдхаэйX BщъOBAX.
C              EзO PAъMEPHOCTш дOмцHA вщTш HE MEHшыE 5.
C
C     рEPBOE OвPAэEHйE K RKF45
C
C     рOмшъOBATEмш дOмцEH рPEдхCMOTPETш B CBOEк BщъщBAаэEк
C     рPOзPAMME рAMсTш дмс CмEдхаэйX MACCйBOB, жйзхPйPхаэйX
C     B CрйCKE BщъOBA- Y(NEQN), WORK(3+6*NEQN), IWORK(5);
C     KPOME TOзO, OH дOмцEH Oв'сBйTш F B OрEPATOPE EXTERNAL,
C     рOдзOTOBйTш рOдрPOзPAMMх F(T,Y,YP) й рPйCBOйTш HAюAмш-
C     HщE ъHAюEHйс рAPAMETPAM-
C
C     NEQN  -юйCмO йHTEзPйPхEMщX хPABHEHйк (NEQN.GE.1)
C     Y(*)  -BEKTOP HAюAмшHщX хCмOBйк
C     T     -HAюAмшHAс TOюKA йHTEзPйPOBAHйс,
C            T дOмцHO вщTш рEPEMEHHOк.
C     TOUT  -TOюKA BщXOдA,B KOTOPOк HхцHO HAкTй ъHAюEHйE
C            PEыEHйс. T=TOUT BOъMOцHO мйыш рPй рEPBOM
C            OвPAэEHйй.B ьTOM CмхюAE BщXOд йъ RKF45 рPOй-
C            CXOдйT CO ъHAюEHйEM рAPAMETPA IFLAG=2,ECмй
C            MOцHO рPOдOмцATш йHTEзPйPOBAHйE.
C     RELERR-зPAHйгA дмс OTHOCйTEмшHOк мOKAмшHщк рOзPEыHOCTEй.
C     ABSERR-зPAHйгA дмс AвCOмаTHOк    мOKAмшHщк рOзPEыHOCTEй.
C            ьTй зPAHйгщ дOмцHщ вщTш HEOTPйгATEмшHщ.
C            RELERR дOмцHA вщTш рEPEMEHHOк,A ABSERR MOцET
C            вщTш й KOHCTAHTOк.рPOзPAMME, BOOвэE зOBOPс
C            HE CмEдхET ъAдABATш зPAHйгх дмс OTHOCйTEмшHOк
C            OыйвKй,MEHшыха, юEM рPйMEPHO 1.E-7. дAвщ йъвEцATш
C            TPхдHOCTEк ,CBсъAHHщX C OюEHш BщCOKйMй ъAрPOCAMй
C            K TOюHOCTй, рPOзPAMMA TPEвхET,юTOвщ RELERR
C            вщмA вOмшыE, юEM HEKOTOPщк рAPAMETP OTHOCйTEмшHOк
C            OыйвKй,BщюйCмсEMщк BHхTPй EE й ъABйCсэйк OT
C            MAыйHщ.B юACTHOCTй,HE PAъPEыAETCс ъAдAHйE TOмшKO
C            AвCOмаTHOк OыйвKй.ECмй цE ъAдAHO ъHAюEHйE RELERR,
C            MEHшыEE дOрхCTйMOзO, TO RKF45 хBEмйюйBAET RELERR
C            HAдмEцAэйM OвPAъOM й BOъBPAэAET хрPABмEHйE рOмш-
C            ъOBATEма, рPEцдE юEM рPOдOмцATш йHTEзPйPOBAHйE.
C     IFLAG-+1,-1.ьTO хKAъATEмш HACTPOкKй рPOзPAMMщ дмс KAцдOк
C            HOBOк ъAдAюй. HOPMAмшHOE BXOдHOE ъHAюEHйE PABHO+1.
C            рOмшъOBATEмш дOмцEH ъAдABATш IFLAG=-1 мйыш B TOM
C            CмхюAE,  KOздA HEOвXOдйMO хрPABмEHйE OдHOыAзOBщM
C            йHTEзPATOPOM.B ьTOM CмхюAE RKF45 рщTAETCс рPOдOмцйTш
C            PEыEHйE HA OдйH ыAз B HAрPABмEHйй TOUT рPй KAцдOM
C            OюEPEдHOM BщъOBE. рOCKOмшKх ьTOT PEцйM PAвOTщ
C            BECшMA HEьKOHOMйюEH, EзO CмEдхET рPйMEHсTш
C            мйыш B CмхюAE KPAкHEк HEOвXOдйMOCTй.
C
C     йHжOPMAгйс HA BщXOдE
C
C     Y(*)    -PEыEHйE B TOюKE T
C     T       -рOCмEдHсс TOюKA,дOCTйзHхTAс рPй йHTEзPйPOBAHйй.
C     IFLAG=2 -рPййHTEзPйPOBAHйй дOCTйзHхTO TOUT.ьTO ъHAюEHйE
C              рAPAMETPA хKAъщBAET HA хCрEыHщк BщXOд й
C              сBмсETCс HOPMAмшHщM PEцйMOM дмс рPOдOмцEHйс
C              йHTEзPйPOBAHйс.
C          =3 -йHTEзPйPOBAHйE HE вщмO ъAKOHюEHO йъ-ъA TOзO,
C              юTO ъAдAHHOE ъHAюEHйE зPAHйгщ дмс OTHOCйTEмшHOк
C              OыйвKй OKAъAмOCш CмйыKOM MAмO. дмс рPOдOмцEHйс
C              йHTEзPйPOBAHйс RELERR вщмO HAдмEцAэйM OвPAъOM
C              хBEмйюEHO.
C          =4 -йHTEзPйPOBAHйE HE вщмO ъAKOHюEHO йъ-ъA TOзO,
C              юTO рOTPEвOBAмOCш вOмEE 3000 BщюйCмEHйк рPO-
C              йъBOдHOк.ьTO COOTBETCTBYET рPйвмйъйTEмшHO
C              500 ыAзAM.
C          =5 -йHTEзPйPOBAHйE HE вщмO ъAKOHюEHO йъ-ъA TOзO,
C              юTO PEыEHйE OвPATймOCш B HYмш,BCмEдCTBйE юEзO
C              TECT TOмшKO OTHOCйTEмшHOк OыйвKй HE рPOXOдйT.
C              дмс рPOдOмцEHйс HEOвXOдйMO HEHYмEBOE ъHAюEHйE
C              рAPAMETPA ABSERR. йCрOмшъOBAHйE HA OдйH ыAз
C              PEцйMA рOыAзOBOзO йHTEзPйPOBAHйс сBмсETCс
C              PAъYMHщM BщXOдOM йъ рOмOцEHйс.
C          =6 -йHTEзPйPOBAHйE  HE вщмO ъAKOHюEHO йъ-ъA TOзO,
C              юTO TPEвYEMAс TOюHOCTш HE MOзмA вщTш дOCTйзHхTA
C              дAцE рPй HAйMEHшыEк дOрхCTй MOк BEмйюйHE ыAзA.
C              рOмшъOBATEмш дOмцEH хBEмйюйTш зPAHйгх рOзPEы-
C              HOCTй,рPEцдE юEM MOцHO вхдET рOрщTATшCс
C              рPOдOмцATш йHTEзPйPOBAHйE.
C          =7 -рO BCEк BйдйMOCTй, RKF45 HEьжжEKTйBHA рPй
C              PEыEHйй ьTOк ъAдAюй. CмйыKOM вOмшыOE юйCмO
C              TPEвхEMщX BщXOдHщX TOюEK рPEрсTCTBхET BщвOPх
C              ECTECTBEHHOк BEмйюйHщ ыAзA.CмEдхET йCрOмшъOBATш
C              PEцйM рOыAзOBOзO йHTEзPйPOBAHйс.
C          =8 -HEрPABймшHOE ъAдAHйE BXOдHщX рAPAMETPOB.ьTO
C              ъHAюEHйE рOсBмсETCс,ECмй дOрхэEHA OдHA йъ
C              CмEдхаэйX OыйвOK-
C                              NEQN.LE.0
C                  T=TOUT  й  IFLAG.NE.+1  ймй -1
C                  RELERR  ймй  ABSERR.LT.0
C                  IFLAG.EQ.0  ймй .LT.-2  ймй .GT.8
C     WORK(*) -йHжOPMAгйс, KOTOPAс OвщюHO HE рPEдCTABмсET йHTE-
C              PECA дмс рOмшъOBATEмс, HO HEOвXOдйMA рPй рOCмE-
C              дхаэйX BщъOBAX. WORK(1),...,WORK(NEQN) COдEPцAT
C              рEPBщE рPOйъBOдHщE BEKTOPA PEыEHйс Y B TOюKE T.
C              WORK(NEQN+1) XPAHйT BEмйюйHх ыAзA H,C KOTOPOк
C              MOцHO рOрщTATшCс рPOBECTй CмEдхаэйк ыAз.
C     IWORK(*) -йHжOPMAгйс, KOTOPAс OвщюHO HE рPEдCTABмсET йHTE-
C               PECA дмс рOмшъOBATEмс, HO HEOвXOдйMA рPй рOCмE-
C               дхаэйX BщъOBAX. B IWORK(1) COдEPцйTCс
C               CюETюйK юйCмA BщюйCмEHйк рPOйъBOдHщX.
C
C     рOCмEдхаэйE OвPAэEHйс K RKF45
C
C          HA BщXOдE рOдрPOзPAMMщ RKF45 йMEETCс BCс йHжOPMAгйс,
C     HEOвXOдйMAс  дмс рPOдOмцEHйс йHTEзPйPOBAHйс.ECмй рPй
C     йHTEзPйPOBAHйй дOCTйзHхTO TOUT,TO рOмшъOBATEма дOCTA-
C     TOюHO OрPEдEмйTш HOBOE ъHAюEHйE  TOUT й CHOBA OвPATйTш-
C     Cс K RKF45.
C          B PEцйME рOыAзOBOзO йHTEзPйPOBAHйс (IFLAG=-2)
C     рOмшъOBATEмш дOмцEH йMETш B Bйдх,юTO KAцдщк ыAз
C     BщрOмHсETCс B HAрPABмEHйй TEKхэEзO ъHAюEHйс TOUT
C     (CйзHAмйъйPхEMOM  йъMEHEHйEM IFLAG HA 2). рOмшъOBATEмш
C     дOмцEH ъAдATш HOBOE ъHAюEHйE TOUT й рEPEOрPEдEмйTш
C     IFLAG HA -2, юTOвщ рPOдOмцATш B PEцйME рOыAзOBOзO
C     йHTEзPйPOBAHйс.
C          ECмй йHTEзPйPOBAHйE HE вщмO ъAKOHюEHO,HO
C     рOмшъOBATEмш XOюET рPOдOмцATш (CмхюAй IFLAG=3,4), OH
C     рOрPOCTх CHOBA OвPAэAETCс K RKF45.рPй IFLAG=3 рAPA-
C     METP RELERR вщм  йъMEHEH HAдмEцAэйM дмс рPOдOмцEHйс
C     йHTEзPйPOBAHйс OвPAъOM.B CмхюAE IFLAG=4 CюETюйK
C     юйCмA ъHAюEHйк жхHKгйй вхдET рEPEOрPEдEмEH HA 0, й
C     вхдхT PAъPEыEHщ EэE 3000 BщюйCмEHйк жхHKгйй.
C          OдHAKO B CмхюAE IFLAG=5, рPEцдE юEM MOцHO вхдET
C     рPOдOмцATш йHTEзPйPOBAHйE,рOмшъOBATEмш дOмцEH CHAюAмA
C     йъMEHйTш KPйTEPйк OыйвKй, ъAдAB рOмOцйTEмшHOE ъHAюEHйE
C     дмс ABSERR. ECмй OH HE CдEмAET ьTOзO, BщрOмHEHйE рPO-
C     зPAMMщ вхдET рPEKPAэEHO.
C        TOюHO TAK цE,B CмхюAE IFLAG=6,рPEцдE юEM рPOдOм-
C     цATш йHTEзPйPOBAHйE,рOмшъOBATEма HEOвXOдйMO рEPEOрPE-
C     дEмйTш IFLAG HA 2 (ймй -2, ECмй йCрOмшъхETCс PEцйM
C     рOыAзOBOзO йHTEзPйPOBAHйс) й хBEмйюйTш ъHAюEHйE дмс
C     ABSERR мйвO RELERR,мйвO й дмс TOзO,й дмс дPхзOзO.
C     ECмй ьTO HE вхдET CдEмAHO,BщрOмHEHйE рPOзPAMMщ
C     рPEKPAэAETCс. рOсBмEHйE IFLAG=6 хKAъщBAET HA HEPEзх-
C     мсPHOCTш (PEыEHйE вщCTPO MEHсETCс ймй, BOъMOцHO,
C     йMEETCс OCOвEHHOCTш),й юACTO B рOдOвHщX CмхюAсX
C     HE йMEET CMщCмA рPOдOмцATш йHTEзPйPOBAHйE.
C          ECмй вхдET рOмхюEHO ъHAюEHйE IFLAG=7,TO рOмшъO-
C     BATEмш дOмцEH рEPEкTй K PEцйMх рOыAзOBOзO йHTEзPйPO-
C     BAHйс C BEмйюйHOк ыAзA,OрPEдEмсEMOк рPOзPAMMOк, ймй
C     PACCMOTPETш BOъMOцHOCTTш рEPEXOдA HA рPOзPAMMщ METOдOB
C     AдAMCA.ECмй BCE цE рOмшъOBATEмш XOюET рPOдOмцATш
C     йHTEзPйPOBAHйE рO рOдрPOзPAMME RKF45,OH дOмцEH дO HOBOзO
C     OвPAэEHйс K HEк рEPEOрPEдEмйTш IFLAG HA 2.B рPOTйBHOM
C     CмхюAE BщрOмHEHйE рPOзPAMMщ вхдET рPEKPAэEHO.
C          ECмй рOмхюEHO ъHAюEHйE IFLAG=8,TO йHTEзPйPOBAHйE
C     HEмшъс рPOдOмцATш,рOKA HE вхдхT йCрPABмEHщ OыйвOюHщE
C     BXOдHщE рAPAMETPщ. HхцHO OTMETйTш,юTO MACCйBщ WORK й
C     IWORK COдEPцAT йHжOPMAгйа,HEOвXOдйMха дмс дAмшHEкыEзO
C     йHTEзPйPOBAHйс.рOьTOMх B ьTй MACCйBщ HEмшъс BHOCйTш
C     йъMEHEHйк.
C
      EXTERNAL F
      INTEGER NEQN,IFLAG,IWORK(5)
      REAL Y(NEQN),T,TOUT,RELERR,ABSERR,WORK(1)
C
C     ECмй TPAHCмсTOP рPOBEPсET йHдEKCщ, TO ъAMEHйTш
C     WORK(1) HA WORK(3+6*NEQN)
C
      INTEGER K1,K2,K3,K4,K5,K6,K1M
C
C     BщюйCмйTш йHдEKCщ дмс PACэEрмEHйс PAвOюEзO MACCйBA
C
      K1M=NEQN+1
      K1=K1M+1
      K2=K1+NEQN
      K3=K2+NEQN
      K4=K3+NEQN
      K5=K4+NEQN
      K6=K5+NEQN
C
C     ьTA рPOMEцYTOюHAс рPOзPAMMA рPOCTO COKPAэAET дмс
C     рOмшъOBATEмс дмйHHщк CрйCOK BщъOBA рYTEM PACэEрмEHйс
C     дBYX PAвOюйX MACCйBOB. ECмй ьTO HE COBMECTйMO C
C     TPAHCмсTOPOM,TO OH дOмцEH OвPAэATшCс HEрOCPEдCTBEHHO
C     K рOдрPOзPAMME RKFS .
C
      CALL RKFS(F,NEQN,Y,T,TOUT,RELERR,ABSERR,IFLAG,
     *          WORK(1),WORK(K1M),WORK(K1),WORK(K2),
     *          WORK(K3),WORK(K4),WORK(K5),WORK(K6),
     *          WORK(K6+1),IWORK(1),IWORK(2),
     *          IWORK(3),IWORK(4),IWORK(5))
      RETURN
      END
