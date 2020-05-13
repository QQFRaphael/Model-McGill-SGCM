      SUBROUTINE ALPDY (DALP,ALP,LSR,LM,EPSI)                           00000010
C                                                                       00000020
C     * JUL 16/79 - J.D.HENDERSON                                       00000030
C     * SETS DALP TO N-S DERIVATIVE OF LEGENDRE POLYNOMIALS IN ALP.     00000040
C     * EPSI CONTAINS PRECOMPUTED CONSTANTS.                            00000050
C     * LSR CONTAINS ROW LENGTH ONFO.                                   00000060
C                                                                       00000070
      REAL DALP(1),ALP(1),EPSI(1)                                       00000080
      INTEGER LSR(2,1)                                                  00000090
C--------------------------------------------------------------------   00000100
C                                                                       00000110
      DO 230 M=1,LM                                                     00000120
      MS=M-1                                                            00000130
      KL=LSR(2,M)                                                       00000140
      KR=LSR(2,M+1)-2                                                   00000150
C                                                                       00000160
      DO 220 K=KL,KR                                                    00000170
      FNS=FLOAT(MS+K-KL)                                                00000180
      ALPILM=0.                                                         00000190
      IF(K.GT.KL) ALPILM=ALP(K-1)                                       00000200
      DALP(K)=(FNS+1)*EPSI(K)*ALPILM - FNS*EPSI(K+1)*ALP(K+1)           00000210
  220 CONTINUE                                                          00000220
  230 DALP(KR+1)=0.                                                     00000230
C                                                                       00000240
      RETURN                                                            00000250
      END                                                               00000260
      SUBROUTINE ALPST (ALP,LSR,LM,SINLAT,EPSI)                         00000010
C                                                                       00000020
C     * JUL  2/79 - J.D.HENDERSON                                       00000030
C     * PUTS LEGENDRE POLYNOMIALS IN ALP FOR ONE LATITUDE.              00000040
C     * SINLAT IS THE SINE OF THE LATITUDE.                             00000050
C     * EPSI IS A FIELD OF PRECOMPUTED CONSTANTS.                       00000060
C     * LSR CONTAINS ROW LENGTH INFO FOR ALP,EPSI.                      00000070
C                                                                       00000080
      INTEGER LSR (2,1)                                                 00000090
      REAL ALP(1),EPSI(1)                                               00000100
      REAL*8 COS2,PROD,A,B,XSIN                                         00000110
C--------------------------------------------------------------------   00000120
      XSIN=SINLAT                                                       00000130
      COS2=1.0-XSIN**2                                                  00000140
      PROD=1.0D+70                                                      00000150
      A=1.                                                              00000160
      B=0.                                                              00000170
C                                                                       00000180
C     * LOOP OVER LONGITUDINAL WAVE NUMBERS.                            00000190
C                                                                       00000200
      DO 230 M=1,LM                                                     00000210
      FM=FLOAT(M-1)                                                     00000220
      IF(M.EQ.1) GO TO 210                                              00000230
      A=A+2.                                                            00000240
      B=B+2.                                                            00000250
      PROD=PROD*COS2*A/B                                                00000260
C                                                                       00000270
C     * COMPUTE THE FIRST VALUE IN THE ROW.                             00000280
C     * COMPUTE THE SECOND ELEMENT ONLY IF NEEDED.                      00000290
C                                                                       00000300
  210 KL=LSR(2,M)                                                       00000310
      KR=LSR(2,M+1)-1                                                   00000320
      ALP(KL)=1.0D-35*DSQRT(0.5*PROD)                                   00000330
      IF(KR.GT.KL) ALP(KL+1)=SQRT(2.*FM+3.)*SINLAT*ALP(KL)              00000340
C                                                                       00000350
C     * COMPUTE THE REST OF THE VALUES IN THE ROW IF NEEDED.            00000360
C                                                                       00000370
      KL2=KL+2                                                          00000380
      IF(KL2.GT.KR) GO TO 230                                           00000390
      DO 220 K=KL2,KR                                                   00000400
      ALP(K)=(SINLAT*ALP(K-1)-EPSI(K-1)*ALP(K-2))/EPSI(K)               00000410
  220 CONTINUE                                                          00000420
C                                                                       00000430
  230 CONTINUE                                                          00000440
      RETURN                                                            00000450
      END                                                               00000460
      SUBROUTINE BITRV (DATA,NPREV,N,NREM)                              00000730
C     SHUFFLE THE DATA BY BIT REVERSAL.                                 00000740
C     DIMENSION DATA(NPREV,N,NREM)                                      00000750
C     COMPLEX DATA                                                      00000760
C     EXCHANGE DATA(J1,J4REV,J5) WITH DATA(J1,J4,J5) FOR ALL J1 FROM 1  00000770
C     TO NPREV, ALL J4 FROM 1 TO N (WHICH MUST BE A POWER OF TWO), AND  00000780
C     ALL J5 FROM 1 TO NREM.  J4REV-1 IS THE BIT REVERSAL OF J4-1.  E.G.00000790
C     SUPPOSE N = 32.  THEN FOR J4-1 = 10011, J4REV-1 = 11001, ETC.     00000800
      IMPLICIT REAL*8(A-H,O-Z)                                          00000810
      DIMENSION DATA(1)                                                 00000830
      IP0=2                                                             00000840
      IP1=IP0*NPREV                                                     00000850
      IP4=IP1*N                                                         00000860
      IP5=IP4*NREM                                                      00000870
      I4REV=1                                                           00000880
C     I4REV = 1+(J4REV-1)*IP1                                           00000890
      DO 60 I4=1,IP4,IP1                                                00000900
C     I4 = 1+(J4-1)*IP1                                                 00000910
      IF (I4-I4REV) 10,30,30                                            00000920
 10   I1MAX=I4+IP1-IP0                                                  00000930
      DO 20 I1=I4,I1MAX,IP0                                             00000940
C     I1 = 1+(J1-1)*IP0+(J4-1)*IP1                                      00000950
      DO 20 I5=I1,IP5,IP4                                               00000960
C     I5 = 1+(J1-1)*IP0+(J4-1)*IP1+(J5-1)*IP4                           00000970
      I5REV=I4REV+I5-I4                                                 00000980
C     I5REV = 1+(J1-1)*IP0+(J4REV-1)*IP1+(J5-1)*IP4                     00000990
      TEMPR=DATA(I5)                                                    00001000
      TEMPI=DATA(I5+1)                                                  00001010
      DATA(I5)=DATA(I5REV)                                              00001020
      DATA(I5+1)=DATA(I5REV+1)                                          00001030
      DATA(I5REV)=TEMPR                                                 00001040
 20   DATA(I5REV+1)=TEMPI                                               00001050
C     ADD ONE WITH DOWNWARD CARRY TO THE HIGH ORDER BIT OF J4REV-1.     00001060
 30   IP2=IP4/2                                                         00001070
 40   IF (I4REV-IP2) 60,60,50                                           00001080
 50   I4REV=I4REV-IP2                                                   00001090
      IP2=IP2/2                                                         00001100
      IF (IP2-IP1) 60,40,40                                             00001110
 60   I4REV=I4REV+IP2                                                   00001120
      RETURN                                                            00001130
      END                                                               00001140
      SUBROUTINE CADD(SA,SB,CONST,LA)                                           
C                                                                               
C     ADD CONST * SA TO SB.                                                     
C                                                                               
      COMPLEX SA(1),SB(1),CONST                                                 
      DO 11 MN=1,LA                                                             
      SB(MN)=SB(MN)+SA(MN)*CONST                                                
   11 CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE COOL2 (DATA,NPREV,N,NREM,ISIGN)                        00001150
C     DISCRETE FOURIER TRANSFORM OF LENGTH N.  IN-PLACE COOLEY-TUKEY    00001160
C     ALGORITHM, BIT-REVERSED TO NORMAL ORDER, SANDE-TUKEY PHASE SHIFTS.00001170
C     DIMENSION DATA(NPREV,N,NREM)                                      00001180
C     COMPLEX DATA                                                      00001190
C     DATA(J1,K4,J5) = SUM(DATA(J1,J4,J5)*EXP(ISIGN*2*PI*I*(J4-1)*      00001200
C     (K4-1)/N)), SUMMED OVER J4 = 1 TO N FOR ALL J1 FROM 1 TO NPREV,   00001210
C     K4 FROM 1 TO N AND J5 FROM 1 TO NREM.  N MUST BE A POWER OF TWO.  00001220
C     METHOD--LET IPREV TAKE THE VALUES 1, 2 OR 4, 4 OR 8, ..., N/16,   00001230
C     N/4, N.  THE CHOICE BETWEEN 2 OR 4, ETC., DEPENDS ON WHETHER N IS 00001240
C     A POWER OF FOUR.  DEFINE IFACT = 2 OR 4, THE NEXT FACTOR THAT     00001250
C     IPREV MUST TAKE, AND IREM = N/(IFACT*IPREV).  THEN--              00001260
C     DIMENSION DATA(NPREV,IPREV,IFACT,IREM,NREM)                       00001270
C     COMPLEX DATA                                                      00001280
C     DATA(J1,J2,K3,J4,J5) = SUM(DATA(J1,J2,J3,J4,J5)*EXP(ISIGN*2*PI*I* 00001290
C     (K3-1)*((J3-1)/IFACT+(J2-1)/(IFACT*IPREV)))), SUMMED OVER J3 = 1  00001300
C     TO IFACT FOR ALL J1 FROM 1 TO NPREV, J2 FROM 1 TO IPREV, K3 FROM  00001310
C     1 TO IFACT, J4 FROM 1 TO IREM AND J5 FROM 1 TO NREM.  THIS IS     00001320
C     A PHASE-SHIFTED DISCRETE FOURIER TRANSFORM OF LENGTH IFACT.       00001330
C     FACTORING N BY FOURS SAVES ABOUT TWENTY FIVE PERCENT OVER FACTOR- 00001340
C     ING BY TWOS.  DATA MUST BE BIT-REVERSED INITIALLY.                00001350
C     IT IS NOT NECESSARY TO REWRITE THIS SUBROUTINE INTO COMPLEX       00001360
C     NOTATION SO LONG AS THE FORTRAN COMPILER USED STORES REAL AND     00001370
C     IMAGINARY PARTS IN ADJACENT STORAGE LOCATIONS.  IT MUST ALSO      00001380
C     STORE ARRAYS WITH THE FIRST SUBSCRIPT INCREASING FASTEST.         00001390
      IMPLICIT REAL*8(A-H,O-Z)                                          00001400
      DIMENSION DATA(1)                                                 00001420
      TWOPI=6.2831853072*FLOAT(ISIGN)                                   00001430
      IP0=2                                                             00001440
      IP1=IP0*NPREV                                                     00001450
      IP4=IP1*N                                                         00001460
      IP5=IP4*NREM                                                      00001470
      IP2=IP1                                                           00001480
C     IP2=IP1*IPROD                                                     00001490
      NPART=N                                                           00001500
 10   IF (NPART-2) 60,30,20                                             00001510
 20   NPART=NPART/4                                                     00001520
      GO TO 10                                                          00001530
C     DO A FOURIER TRANSFORM OF LENGTH TWO                              00001540
 30   IF (IP2-IP4) 40,160,160                                           00001550
 40   IP3=IP2*2                                                         00001560
C     IP3=IP2*IFACT                                                     00001570
      DO 50 I1=1,IP1,IP0                                                00001580
C     I1 = 1+(J1-1)*IP0                                                 00001590
      DO 50 I5=I1,IP5,IP3                                               00001600
C     I5 = 1+(J1-1)*IP0+(J4-1)*IP3+(J5-1)*IP4                           00001610
      I3A=I5                                                            00001620
      I3B=I3A+IP2                                                       00001630
C     I3 = 1+(J1-1)*IP0+(J2-1)*IP1+(J3-1)*IP2+(J4-1)*IP3+(J5-1)*IP4     00001640
      TEMPR=DATA(I3B)                                                   00001650
      TEMPI=DATA(I3B+1)                                                 00001660
      DATA(I3B)=DATA(I3A)-TEMPR                                         00001670
      DATA(I3B+1)=DATA(I3A+1)-TEMPI                                     00001680
      DATA(I3A)=DATA(I3A)+TEMPR                                         00001690
 50   DATA(I3A+1)=DATA(I3A+1)+TEMPI                                     00001700
      IP2=IP3                                                           00001710
C     DO A FOURIER TRANSFORM OF LENGTH FOUR (FROM BIT REVERSED ORDER)   00001720
 60   IF (IP2-IP4) 70,160,160                                           00001730
 70   IP3=IP2*4                                                         00001740
C     IP3=IP2*IFACT                                                     00001750
C     COMPUTE TWOPI THRU WR AND WI IN DOUBLE PRECISION, IF AVAILABLE.   00001760
      THETA=TWOPI/FLOAT(IP3/IP1)                                        00001770
      SINTH=SIN(THETA/2.)                                               00001780
      WSTPR=-2.*SINTH*SINTH                                             00001790
      WSTPI=SIN(THETA)                                                  00001800
      WR=1.                                                             00001810
      WI=0.                                                             00001820
      DO 150 I2=1,IP2,IP1                                               00001830
C     I2 = 1+(J2-1)*IP1                                                 00001840
      IF (I2-1) 90,90,80                                                00001850
 80   W2R=WR*WR-WI*WI                                                   00001860
      W2I=2.*WR*WI                                                      00001870
      W3R=W2R*WR-W2I*WI                                                 00001880
      W3I=W2R*WI+W2I*WR                                                 00001890
 90   I1MAX=I2+IP1-IP0                                                  00001900
      DO 140 I1=I2,I1MAX,IP0                                            00001910
C     I1 = 1+(J1-1)*IP0+(J2-1)*IP1                                      00001920
      DO 140 I5=I1,IP5,IP3                                              00001930
C     I5 = 1+(J1-1)*IP0+(J2-1)*IP1+(J4-1)*IP3+(J5-1)*IP4                00001940
      I3A=I5                                                            00001950
      I3B=I3A+IP2                                                       00001960
      I3C=I3B+IP2                                                       00001970
      I3D=I3C+IP2                                                       00001980
C     I3 = 1+(J1-1)*IP0+(J2-1)*IP1+(J3-1)*IP2+(J4-1)*IP3+(J5-1)*IP4     00001990
      IF (I2-1) 110,110,100                                             00002000
C     APPLY THE PHASE SHIFT FACTORS                                     00002010
 100  TEMPR=DATA(I3B)                                                   00002020
      DATA(I3B)=W2R*DATA(I3B)-W2I*DATA(I3B+1)                           00002030
      DATA(I3B+1)=W2R*DATA(I3B+1)+W2I*TEMPR                             00002040
      TEMPR=DATA(I3C)                                                   00002050
      DATA(I3C)=WR*DATA(I3C)-WI*DATA(I3C+1)                             00002060
      DATA(I3C+1)=WR*DATA(I3C+1)+WI*TEMPR                               00002070
      TEMPR=DATA(I3D)                                                   00002080
      DATA(I3D)=W3R*DATA(I3D)-W3I*DATA(I3D+1)                           00002090
      DATA(I3D+1)=W3R*DATA(I3D+1)+W3I*TEMPR                             00002100
 110  T0R=DATA(I3A)+DATA(I3B)                                           00002110
      T0I=DATA(I3A+1)+DATA(I3B+1)                                       00002120
      T1R=DATA(I3A)-DATA(I3B)                                           00002130
      T1I=DATA(I3A+1)-DATA(I3B+1)                                       00002140
      T2R=DATA(I3C)+DATA(I3D)                                           00002150
      T2I=DATA(I3C+1)+DATA(I3D+1)                                       00002160
      T3R=DATA(I3C)-DATA(I3D)                                           00002170
      T3I=DATA(I3C+1)-DATA(I3D+1)                                       00002180
      DATA(I3A)=T0R+T2R                                                 00002190
      DATA(I3A+1)=T0I+T2I                                               00002200
      DATA(I3C)=T0R-T2R                                                 00002210
      DATA(I3C+1)=T0I-T2I                                               00002220
      IF (ISIGN) 120,120,130                                            00002230
 120  T3R=-T3R                                                          00002240
      T3I=-T3I                                                          00002250
 130  DATA(I3B)=T1R-T3I                                                 00002260
      DATA(I3B+1)=T1I+T3R                                               00002270
      DATA(I3D)=T1R+T3I                                                 00002280
 140  DATA(I3D+1)=T1I-T3R                                               00002290
      TEMPR=WR                                                          00002300
      WR=WSTPR*TEMPR-WSTPI*WI+TEMPR                                     00002310
 150  WI=WSTPR*WI+WSTPI*TEMPR+WI                                        00002320
      IP2=IP3                                                           00002330
      GO TO 60                                                          00002340
 160  RETURN                                                            00002350
      END                                                               00002360
      SUBROUTINE DFGRDG(SA,SB,EPSI,LSR,LM)                              00000010
C                                                                       00000030
C     * 9/91 J. SHENG                                                           
C     * PRODUCES IN SB(LA) SPECTRAL COEFFICIENTS OF DIV ( F GRAD G )    00000050
C     * FROM SPECTRAL COEFFICIENTS OF G IN SA(LA). F IS THE CORIOLIS    00000060
C     * PARAMETER.                                                      00000070
C     * LSR CONTAINS ROW LENGTH INFO FOR SA, SB AND EPSI.                       
C                                                                               
      COMPLEX SA(1),SB(1)                                                       
      REAL EPSI(1)                                                              
      INTEGER LSR(2,1)                                                          
      OMEGA=7.292E-5                                                            
      OMEG2=2.0*OMEGA                                                           
      EARTH=6370949.0                                                           
      RAD2=EARTH*EARTH                                                          
      FACTOR=OMEG2/RAD2                                                         
      LA=LSR(1,LM+1)-1                                                          
      DO 11 MN=1,LA                                                             
      SB(MN)=(0.0,0.0)                                                          
   11 CONTINUE                                                                  
      DO 12 I=1,LM                                                              
      MN1=LSR(1,I)                                                              
      MN2=LSR(1,I+1)-1                                                          
      MS=I-1                                                                    
      DO 12 MN=MN1,MN2                                                          
      NS=MS+MN-MN1                                                              
      MNP1=MN+1                                                                 
      MNP=LSR(2,I)+MN-MN1+1                                                     
      IF(MNP1.LE.MN2)THEN                                                       
      SB(MNP1)=SB(MNP1)-NS*(NS+2)*EPSI(MNP)*FACTOR*SA(MN)                       
      ENDIF                                                                     
      MNM1=MN-1                                                                 
      MNS=LSR(2,I)+MN-MN1                                                       
      IF(MNM1.GE.MN1)THEN                                                       
      SB(MNM1)=SB(MNM1)-(NS*NS-1)*EPSI(MNS)*FACTOR*SA(MN)                       
      ENDIF                                                                     
   12 CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE DIMGT(LSR,LA,LR,LM,KTR,LRLMT)                          00000010
C                                                                       00000020
C     * JUL 16/79 - J.D.HENDERSON                                       00000030
C     * COMPUTES ROW LENGTH INFORMATION FOR SPECTRAL ARRAYS.            00000040
C     * LSR(1,M),LSR(2,M) = FIRST WORD IN ROW M OF SPECTRAL,ALP ARRAYS. 00000050
C                                                                       00000060
C     * LRLMT IS A 5 DIGIT NUMBER FORMED BY - 1000*LR+10*LM+KTR         00000070
C     *  WHERE  LR = LENGTH OF FIRST SPECTRAL ROW.                      00000080
C     *         LM = NUMBER OF SPECTRAL ROWS.                           00000090
C     *        KTR = TRUNCATION TYPE.                                   00000100
C     * LA = TOTAL LENGTH OF SPECTRAL ARRAY (COMPLEX WORDS).            00000110
C     * INVALID TRUNCATION TYPE RETURNS WITH LA=0.                      00000120
C                                                                       00000130
      INTEGER LSR(2,1)                                                  00000140
C--------------------------------------------------------------------   00000150
C     * DECOMPOSE LRLMT INTO LR,LM,KTR.                                 00000160
C                                                                       00000170
      LR=LRLMT/1000                                                     00000180
      LM=LRLMT/10-100*LR                                                00000190
      KTR=MOD(LRLMT,10)                                                 00000200
      LA=0                                                              00000210
C                                                                       00000220
C    * RHOMBOIDAL TRUNCATION (KTR = 0).                                 00000230
C                                                                       00000240
  210 IF(KTR.NE.0) GO TO 310                                            00000250
      LSR(1,1)=1                                                        00000260
      LSR(2,1)=1                                                        00000270
      LMP=LM+1                                                          00000280
      DO 250 M=2,LMP                                                    00000290
      LSR(1,M)=LSR(1,M-1)+LR                                            00000300
      LSR(2,M)=LSR(2,M-1)+LR+1                                          00000310
  250 CONTINUE                                                          00000320
      LA=LSR(1,LMP)-1                                                   00000330
      RETURN                                                            00000340
C                                                                       00000350
C     * TRIANGULAR TRUNCATION (KTR = 2).                                00000360
C                                                                       00000370
  310 IF(KTR.NE.2) RETURN                                               00000380
      LSR(1,1)=1                                                        00000390
      LSR(2,1)=1                                                        00000400
      LMP=LM+1                                                          00000410
      DO 350 M=2,LMP                                                    00000420
      LSR(1,M)=LSR(1,M-1)+LR-(M-2)                                      00000430
      LSR(2,M)=LSR(2,M-1)+LR-(M-3)                                      00000440
  350 CONTINUE                                                          00000450
      LA=LSR(1,LMP)-1                                                   00000460
      RETURN                                                            00000470
      END                                                               00000480
      SUBROUTINE EPSCAL(EPSI,LSR,LM)                                    00000010
C                                                                       00000020
C     * JUL  2/79 - J.D.HENDERSON                                       00000030
C     * COMPUTES ARRAY EPSILON USED IN THE SPECTRAL MODEL.              00000040
C     * EPSILON(N,M)=SQRT((N**2-M**2)/(4*N**2-1))                       00000050
C     * LSR CONTAINS ROW START INFO.                                    00000060
C                                                                       00000070
      REAL EPSI(1)                                                      00000080
      INTEGER LSR(2,1)                                                  00000090
C---------------------------------------------------------------------  00000100
      DO 220 M=1,LM                                                     00000110
      MS=M-1                                                            00000120
      KL=LSR(2,M)                                                       00000130
      KR=LSR(2,M+1)-1                                                   00000140
      K1=KL                                                             00000150
      IF(M.EQ.1) K1=2                                                   00000160
      DO 220 K=K1,KR                                                    00000170
      NS=MS+(K-KL)                                                      00000180
      FNUM=FLOAT(NS**2-MS**2)                                           00000190
      FDEN=FLOAT(4*NS**2-1)                                             00000200
  220 EPSI(K)=SQRT(FNUM/FDEN)                                           00000210
      EPSI(1)=0.                                                        00000220
C                                                                       00000230
      RETURN                                                            00000240
      END                                                               00000250
      SUBROUTINE FAST (SC,LSR,LM,CFC,ALP,WEIGHT)                        00000010
C                                                                       00000020
C     * APR  1/80 - J.D.HENDERSON                                       00000030
C     * CONTRIBUTION OF COMPLEX FOURIER COEFF IN CFC                    00000040
C     * TO LATITUDE INTEGRAL ADDED TO SPHERICAL HARMONICS IN SC.        00000050
C     * SPECTRAL HARMONICS MUST BE GLOBAL.                              00000060
C                                                                       00000070
C     * LSR(1,M)=START OF ROW M OF SC                                   00000080
C     * LSR(2,M)=START OF ROW M OF ALP                                  00000090
C     * LM = NUMBER OF ROWS IN SC,ALP ARRAYS.                           00000100
C     * ALP = ASSOCIATED LEGENDRE POLYNOMIALS FOR GIVEN LATITUDE.       00000110
C     * WEIGHT = GAUSSIAN WEIGHT FOR GIVEN LATITUDE.                    00000120
C                                                                       00000130
      COMPLEX SC(1),CFC(1)                                              00000140
      REAL ALP(1)                                                       00000150
      INTEGER LSR(2,1)                                                  00000160
C--------------------------------------------------------------------   00000170
C                                                                       00000180
      DO 220 M=1,LM                                                     00000190
      NA=LSR(2,M)-1                                                     00000200
      NL=LSR(1,M)                                                       00000210
      NR=LSR(1,M+1)-1                                                   00000220
      CFCR= REAL(CFC(M))*WEIGHT                                         00000230
      CFCI=AIMAG(CFC(M))*WEIGHT                                         00000240
C                                                                       00000250
      DO 220 N=NL,NR                                                    00000260
      NA=NA+1                                                           00000270
  220 SC(N)=SC(N)+CMPLX(CFCR*ALP(NA) ,CFCI*ALP(NA))                     00000280
C                                                                       00000290
      RETURN                                                            00000300
      END                                                               00000310
      SUBROUTINE FFGFW (GR,LGR,FW,LFW,IR,ILONG,WRKS,NL)                 00000010
C                                                                       00000020
C     *****   FEB 1976  -  JOHN D. HENDERSON  ****                      00000030
C     * CONVERTS IR COMPLEX FOURIER COEFFICIENTS IN FW(LFW,NL)          00000040
C     * TO ILONG EQUALLY SPACED GRID POINT VALUES IN GR(LGR,NL)         00000050
C     * BY THE FAST FOURIER SUBROUTINE FOR12S.                          00000060
C     * FOR12S IS WRITTEN IN COMPASS FOR A CDC 7600 COMPUTER.           00000070
C                                                                       00000080
      COMPLEX FW(LFW,NL),WRKS(1)                                        00000090
      DIMENSION GR(LGR,NL)                                              00000100
      COMPLEX AA,BB,CC                                                  00000110
C-----------------------------------------------------------------------00000120
      IRP1=IR+1                                                         00000130
      IF(MOD(ILONG,3).EQ.0) GO TO 100                                   00000140
C-----------------------------------------------------------------------00000150
C     * CASE 1 - ILONG IS A POWER OF 2.                                 00000160
C     * MAX IR IS ILONG/2.  WRKS=(ILONG+1) COMPLEX WORDS.               00000170
C                                                                       00000180
      IF(IR.GT.ILONG/2) WRITE(6,6010) IR,ILONG                          00000190
      IF(IR.GT.ILONG/2) RETURN                                          00000200
      ILG2=ILONG+2                                                      00000210
      NWMAX=ILONG/2  + 1                                                00000220
      WRKS(NWMAX+1)=(0.,0.)                                             00000230
C                                                                       00000240
      DO 50 L=1,NL                                                      00000250
C                                                                       00000260
C     * COPY WAVES 0 TO IR TO WRKS. FILL TO WAVE ILONG/2 WITH ZEROS.    00000270
C                                                                       00000280
      DO 20 J=IRP1,NWMAX                                                00000290
   20 WRKS(J)=(0.,0.)                                                   00000300
      DO 30 J=1,IRP1                                                    00000310
   30 WRKS(J)=FW(J,L)                                                   00000320
C                                                                       00000330
C     * PERFORM THE FAST FOURIER TRANSFORM.                             00000340
C                                                                       00000350
      N=ILONG                                                           00000360
      ISIGN=1                                                           00000370
      NTYP=-1                                                           00000380
      CALL FOUR2(WRKS,N,1,ISIGN,-1)                                     00000390
C                                                                       00000400
C     * COPY ILONG GRID POINTS FROM WRKS TO GR.                         00000410
C                                                                       00000420
      NX=0                                                              00000430
      DO 40 I=1,ILONG,2                                                 00000440
      NX=NX+1                                                           00000450
      GR(I  ,L)= REAL(WRKS(NX))                                         00000460
      GR(I+1,L)=AIMAG(WRKS(NX))                                         00000470
   40 CONTINUE                                                          00000480
   50 CONTINUE                                                          00000490
      RETURN                                                            00000500
C-----------------------------------------------------------------------00000510
C     * CASE 2 - ILONG IS 3 TIMES A POWER OF 2.                         00000520
C     * MAX VALUE OF IR IS ILONG/3.  WRKS=(ILONG/6+1)*8 COMPLEX WORDS.  00000530
C     * THIS SECTION ADDED BY ROGER DALEY, FEB 1976.                    00000540
C                                                                       00000550
  100 K3 = ILONG/3                                                      00000560
      IF(IR.GT.K3) WRITE(6,6010) IR,ILONG                               00000570
      IF(IR.GT.K3) RETURN                                               00000580
      K6 = K3/2                                                         00000590
      KP6 = K6 + 1                                                      00000600
      NWMAX = 2*KP6                                                     00000610
      WRKS(7*KP6+1) = (0.,0.)                                           00000620
C                                                                       00000630
      CALL PERM(WRKS(2*KP6+1),WRKS(3*KP6+1),AA,BB,KP6,K3)               00000640
C                                                                       00000650
      DO 150 L=1,NL                                                     00000660
C                                                                       00000670
      DO 120 J=IRP1,NWMAX                                               00000680
  120 WRKS(J) = (0.,0.)                                                 00000690
      DO 130 J=1,IRP1                                                   00000700
  130 WRKS(J) = FW(J,L)                                                 00000710
C                                                                       00000720
      DO 160 K=1,KP6                                                    00000730
      CC = CONJG(WRKS(K3+2-K))                                          00000740
      WRKS(4*KP6+K) = WRKS(K) + CC                                      00000750
      WRKS(5*KP6+K) = (WRKS(K)+BB*CC)*WRKS(2*KP6+K)                     00000760
  160 WRKS(6*KP6+K) = (WRKS(K) + AA*CC)*WRKS(3*KP6+K)                   00000770
C                                                                       00000780
      N=K3                                                              00000790
      ISIGN = 1                                                         00000800
      NTYP = -1                                                         00000810
      CALL FOUR2(WRKS(4*KP6+1),N,1,ISIGN,-1)                            00000820
      CALL FOUR2(WRKS(5*KP6+1),N,1,ISIGN,-1)                            00000830
      CALL FOUR2(WRKS(6*KP6+1),N,1,ISIGN,-1)                            00000840
      WRKS(7*KP6) = WRKS(6*KP6+1)                                       00000850
C                                                                       00000860
      DO 170 K=1,K6                                                     00000870
      KP = (K-1)*6                                                      00000880
      GR(KP+1,L)= REAL (WRKS(4*KP6+K))                                  00000890
      GR(KP+2,L) =REAL (WRKS(5*KP6+K))                                  00000900
      GR(KP+3,L)= AIMAG(WRKS(6*KP6+K))                                  00000910
      GR(KP+4,L)= AIMAG(WRKS(4*KP6+K))                                  00000920
      GR(KP+5,L)= AIMAG(WRKS(5*KP6+K))                                  00000930
  170 GR(KP+6,L)= REAL(WRKS(6*KP6+K+1))                                 00000940
C                                                                       00000950
  150 CONTINUE                                                          00000960
      RETURN                                                            00000970
C-----------------------------------------------------------------------00000980
 6010 FORMAT(34H ILLEGAL CALL TO FFGFW2..IR,ILONG=,2I8)                 00000990
      END                                                               00001000
      SUBROUTINE FFWFG (FW,LFW,GR,LGR,IR,ILONG,WRKS,NL)                 00000010
C                                                                       00000020
C     *****   FEB 1976  -  JOHN D. HENDERSON  ****                      00000030
C     * CONVERTS ILONG EQUALLY SPACED GRID POINT VALUES IN EACH LEVEL OF00000040
C     *  GR(LGR,NL) TO COMPLEX FOURIER COEFFICIENTS IN FW(LFW,NL)       00000050
C     *  TO WAVE NUMBER IR, USING THE FAST FOURIER ROUTINE FOR12S.      00000060
C     * FOR12S IS WRITTEN IN COMPASS FOR A CDC 7600 COMPUTER.           00000070
C                                                                       00000080
      COMPLEX FW(LFW,NL)                                                00000090
      DIMENSION GR(LGR,NL),WRKS(1)                                      00000100
      COMPLEX AA,BB                                                     00000110
C-----------------------------------------------------------------------00000120
      IRP1=IR+1                                                         00000130
      IF(MOD(ILONG,3).EQ.0) GO TO 100                                   00000140
C-----------------------------------------------------------------------00000150
C     * CASE 1 - ILONG IS A POWER OF 2.                                 00000160
C                                                                       00000170
      IF(IR.GT.ILONG/2) WRITE(6,6010) IR,ILONG                          00000180
      IF(IR.GT.ILONG/2) RETURN                                          00000190
      ILG2=ILONG+2                                                      00000200
      FLINV=1./FLOAT(ILONG)                                             00000210
      WRKS(ILG2+1)=0.                                                   00000220
C                                                                       00000230
      DO 50 L=1,NL                                                      00000240
C                                                                       00000250
C     * TRANSFER GR TO WRKS AND DIVIDE BY (2*ILONG).                    00000260
C                                                                       00000270
      DO 20 I=1,ILONG                                                   00000280
   20 WRKS(I)=GR(I,L)*FLINV                                             00000290
C                                                                       00000300
C     * PERFORM THE FAST FOURIER TRANSFORM.                             00000310
C                                                                       00000320
      N=ILONG                                                           00000330
      ISIGN=-1                                                          00000340
      NTYP=0                                                            00000350
      CALL FOUR2(WRKS,N,1,ISIGN,0)                                      00000360
C                                                                       00000370
C     * TRANSFER COMPLEX WAVES 0 TO IR FROM WRKS TO FW.                 00000380
C                                                                       00000390
      NX=-1                                                             00000400
      DO 40 J=1,IRP1                                                    00000410
      NX=NX+2                                                           00000420
   40 FW(J,L)=CMPLX(WRKS(NX  ),WRKS(NX+1  ))                            00000430
   50 CONTINUE                                                          00000440
      RETURN                                                            00000450
C-----------------------------------------------------------------------00000460
C     * CASE 2 - ILONG IS 3 TIMES A POWER OF 2.                         00000470
C     * MAX VALUE OF IR IS ILONG/3.  WRKS=(ILONG/6+1)*8 COMPLEX WORDS.  00000480
C     * THIS SECTION ADDED BY ROGER DALEY, FEB 1976.                    00000490
C                                                                       00000500
100   FLINV=1./FLOAT(ILONG)                                             00000510
      K3 = ILONG/3                                                      00000520
      IF(IR.GT.K3) WRITE(6,6010) IR,ILONG                               00000530
      IF(IR.GT.K3) RETURN                                               00000540
      K6 = K3/2                                                         00000550
      KP6 = K6 + 1                                                      00000560
      WRKS(14*KP6+1) = 0.                                               00000570
C                                                                       00000580
      CALL PERM(WRKS(4*KP6+1),WRKS(6*KP6+1),AA,BB,KP6,K3)               00000590
C                                                                       00000600
      DO 150 L=1,NL                                                     00000610
C                                                                       00000620
      DO 130 K=1,K3                                                     00000630
      KP = (K-1)*3                                                      00000640
      WRKS(8*KP6+K) = GR(KP+1,L)*FLINV                                  00000650
      WRKS(10*KP6+K) = GR(KP+2,L)*FLINV                                 00000660
  130 WRKS(12*KP6+K+1) = GR(KP+3,L)*FLINV                               00000670
      WRKS(12*KP6+1) = WRKS(12*KP6+K3+1)                                00000680
C                                                                       00000690
      N=K3                                                              00000700
      ISIGN = -1                                                        00000710
      NTYP = 0                                                          00000720
      CALL FOUR2(WRKS( 8*KP6+1),N,1,ISIGN,0)                            00000730
      CALL FOUR2(WRKS(10*KP6+1),N,1,ISIGN,0)                            00000740
      CALL FOUR2(WRKS(12*KP6+1),N,1,ISIGN,0)                            00000750
C                                                                       00000760
      CALL RCOM(WRKS(4*KP6+1),WRKS(6*KP6+1),AA,BB,WRKS(8*KP6+1),        00000770
     1   WRKS(10*KP6+1),WRKS(12*KP6+1),WRKS(1),KP6,K3)                  00000780
C                                                                       00000790
      NX = -1                                                           00000800
      DO 160 J=1,IRP1                                                   00000810
      NX = NX+2                                                         00000820
  160 FW(J,L) = CMPLX(WRKS(NX),WRKS(NX+1))                              00000830
C                                                                       00000840
  150 CONTINUE                                                          00000850
      RETURN                                                            00000860
C-----------------------------------------------------------------------00000870
 6010 FORMAT(34H ILLEGAL CALL TO FFWFG2..IR,ILONG=,2I8)                 00000880
      END                                                               00000890
      SUBROUTINE FIXRL (DATA,N,NREM,ISIGN,IFORM)                        00002370
C     FOR IFORM = 0, CONVERT THE TRANSFORM OF A DOUBLED-UP REAL ARRAY,  00002380
C     CONSIDERED COMPLEX, INTO ITS TRUE TRANSFORM.  SUPPLY ONLY THE     00002390
C     FIRST HALF OF THE COMPLEX TRANSFORM, AS THE SECOND HALF HAS       00002400
C     CONJUGATE SYMMETRY.  FOR IFORM = -1, CONVERT THE FIRST HALF       00002410
C     OF THE TRUE TRANSFORM INTO THE TRANSFORM OF A DOUBLED-UP REAL     00002420
C     ARRAY.  N MUST BE EVEN.                                           00002430
C     USING COMPLEX NOTATION AND SUBSCRIPTS STARTING AT ZERO, THE       00002440
C     TRANSFORMATION IS--                                               00002450
C     DIMENSION DATA(N,NREM)                                            00002460
C     ZSTP = EXP(ISIGN*2*PI*I/N)                                        00002470
C     DO 10 I2=0,NREM-1                                                 00002480
C     DATA(0,I2) = CONJ(DATA(0,I2))*(1+I)                               00002490
C     DO 10 I1=1,N/4                                                    00002500
C     Z = (1+(2*IFORM+1)*I*ZSTP**I1)/2                                  00002510
C     I1CNJ = N/2-I1                                                    00002520
C     DIF = DATA(I1,I2)-CONJ(DATA(I1CNJ,I2))                            00002530
C     TEMP = Z*DIF                                                      00002540
C     DATA(I1,I2) = (DATA(I1,I2)-TEMP)*(1-IFORM)                        00002550
C 10  DATA(I1CNJ,I2) = (DATA(I1CNJ,I2)+CONJ(TEMP))*(1-IFORM)            00002560
C     IF I1=I1CNJ, THE CALCULATION FOR THAT VALUE COLLAPSES INTO        00002570
C     A SIMPLE CONJUGATION OF DATA(I1,I2).                              00002580
      IMPLICIT REAL*8(A-H,O-Z)                                          00002590
      DIMENSION DATA(2)                                                 00002610
      TWOPI=6.283185307*FLOAT(ISIGN)                                    00002620
      IP0=2                                                             00002630
      IP1=IP0*(N/2)                                                     00002640
      IP2=IP1*NREM                                                      00002650
      IF (IFORM) 10,70,70                                               00002660
C     PACK THE REAL INPUT VALUES (TWO PER COLUMN)                       00002670
 10   J1=IP1+1                                                          00002680
      DATA(2)=DATA(J1)                                                  00002690
      IF (NREM-1) 70,70,20                                              00002700
 20   J1=J1+IP0                                                         00002710
      I2MIN=IP1+1                                                       00002720
      DO 60 I2=I2MIN,IP2,IP1                                            00002730
      DATA(I2)=DATA(J1)                                                 00002740
      J1=J1+IP0                                                         00002750
      IF (N-2) 50,50,30                                                 00002760
 30   I1MIN=I2+IP0                                                      00002770
      I1MAX=I2+IP1-IP0                                                  00002780
      DO 40 I1=I1MIN,I1MAX,IP0                                          00002790
      DATA(I1)=DATA(J1)                                                 00002800
      DATA(I1+1)=DATA(J1+1)                                             00002810
 40   J1=J1+IP0                                                         00002820
 50   DATA(I2+1)=DATA(J1)                                               00002830
 60   J1=J1+IP0                                                         00002840
 70   DO 80 I2=1,IP2,IP1                                                00002850
      TEMPR=DATA(I2)                                                    00002860
      DATA(I2)=DATA(I2)+DATA(I2+1)                                      00002870
 80   DATA(I2+1)=TEMPR-DATA(I2+1)                                       00002880
      IF (N-2) 200,200,90                                               00002890
 90   THETA=TWOPI/FLOAT(N)                                              00002900
      SINTH=SIN(THETA/2.)                                               00002910
      ZSTPR=-2.*SINTH*SINTH                                             00002920
      ZSTPI=SIN(THETA)                                                  00002930
      ZR=(1.-ZSTPI)/2.                                                  00002940
      ZI=(1.+ZSTPR)/2.                                                  00002950
      IF (IFORM) 100,110,110                                            00002960
 100  ZR=1.-ZR                                                          00002970
      ZI=-ZI                                                            00002980
 110  I1MIN=IP0+1                                                       00002990
      I1MAX=IP0*(N/4)+1                                                 00003000
      DO 190 I1=I1MIN,I1MAX,IP0                                         00003010
      DO 180 I2=I1,IP2,IP1                                              00003020
      I2CNJ=IP0*(N/2+1)-2*I1+I2                                         00003030
      IF (I2-I2CNJ) 150,120,120                                         00003040
 120  IF (ISIGN*(2*IFORM+1)) 130,140,140                                00003050
 130  DATA(I2+1)=-DATA(I2+1)                                            00003060
 140  IF (IFORM) 170,180,180                                            00003070
 150  DIFR=DATA(I2)-DATA(I2CNJ)                                         00003080
      DIFI=DATA(I2+1)+DATA(I2CNJ+1)                                     00003090
      TEMPR=DIFR*ZR-DIFI*ZI                                             00003100
      TEMPI=DIFR*ZI+DIFI*ZR                                             00003110
      DATA(I2)=DATA(I2)-TEMPR                                           00003120
      DATA(I2+1)=DATA(I2+1)-TEMPI                                       00003130
      DATA(I2CNJ)=DATA(I2CNJ)+TEMPR                                     00003140
      DATA(I2CNJ+1)=DATA(I2CNJ+1)-TEMPI                                 00003150
      IF (IFORM) 160,180,180                                            00003160
 160  DATA(I2CNJ)=DATA(I2CNJ)+DATA(I2CNJ)                               00003170
      DATA(I2CNJ+1)=DATA(I2CNJ+1)+DATA(I2CNJ+1)                         00003180
 170  DATA(I2)=DATA(I2)+DATA(I2)                                        00003190
      DATA(I2+1)=DATA(I2+1)+DATA(I2+1)                                  00003200
 180  CONTINUE                                                          00003210
      TEMPR=ZR-.5                                                       00003220
      ZR=ZSTPR*TEMPR-ZSTPI*ZI+ZR                                        00003230
 190  ZI=ZSTPR*ZI+ZSTPI*TEMPR+ZI                                        00003240
C     RECURSION SAVES TIME, AT A SLIGHT LOSS IN ACCURACY.  IF AVAILABLE,00003250
C     USE DOUBLE PRECISION TO COMPUTE ZR AND ZI.                        00003260
 200  IF (IFORM) 270,210,210                                            00003270
C     UNPACK THE REAL TRANSFORM VALUES (TWO PER COLUMN)                 00003280
 210  I2=IP2+1                                                          00003290
      I1=I2                                                             00003300
      J1=IP0*(N/2+1)*NREM+1                                             00003310
      GO TO 250                                                         00003320
 220  DATA(J1)=DATA(I1)                                                 00003330
      DATA(J1+1)=DATA(I1+1)                                             00003340
      I1=I1-IP0                                                         00003350
      J1=J1-IP0                                                         00003360
 230  IF (I2-I1) 220,240,240                                            00003370
 240  DATA(J1)=DATA(I1)                                                 00003380
      DATA(J1+1)=0.                                                     00003390
 250  I2=I2-IP1                                                         00003400
      J1=J1-IP0                                                         00003410
      DATA(J1)=DATA(I2+1)                                               00003420
      DATA(J1+1)=0.                                                     00003430
      I1=I1-IP0                                                         00003440
      J1=J1-IP0                                                         00003450
      IF (I2-1) 260,260,230                                             00003460
 260  DATA(2)=0.                                                        00003470
 270  RETURN                                                            00003480
      END                                                               00003490
      SUBROUTINE FMEAN (G,LI,LJ,AVRG,N)                                 00000010
C                                                                       00000020
C     *****   JAN 1975  -  JOHN D. HENDERSON  ****                      00000030
C     * CALCULATES MEAN OF G(LI,LJ) OMITTING N BORDER ROWS.             00000040
C                                                                       00000050
C     LEVEL 2,G                                                         00000060
      DIMENSION G(LI,LJ)                                                00000070
      REAL*8 SUM                                                        00000080
C-----------------------------------------------------------------------00000090
C                                                                       00000100
      IL=1+N                                                            00000110
      JL=1+N                                                            00000120
      IH=LI-N                                                           00000130
      JH=LJ-N                                                           00000140
      PTS=(IH-IL+1)*(JH-JL+1)                                           00000150
      SUM=0.                                                            00000160
C                                                                       00000170
      DO 20 J=JL,JH                                                     00000180
      DO 20 I=IL,IH                                                     00000190
   20 SUM=SUM+G(I,J)                                                    00000200
      AVRG=SUM/PTS                                                      00000210
C                                                                       00000220
      RETURN                                                            00000230
      END                                                               00000240
      SUBROUTINE FOUR2(XDATA,N,NDIM,ISIGN,IFORM)                        00000010
C     COOLEY-TUKEY FAST FOURIER TRANSFORM IN USASI BASIC FORTRAN.       00000020
C     MULTI-DIMENSIONAL TRANSFORM, EACH DIMENSION A POWER OF TWO,       00000030
C     COMPLEX OR REAL DATA.                                             00000040
C     TRANSFORM(K1,K2,...) = SUM(DATA(J1,J2,...)*EXP(ISIGN*2*PI*SQRT(-1)00000050
C     *((J1-1)*(K1-1)/N(1)+(J2-1)*(K2-1)/N(2)+...))), SUMMED FOR ALL    00000060
C     J1 AND K1 FROM 1 TO N(1), J2 AND K2 FROM 1 TO N(2),               00000070
C     ETC. FOR ALL NDIM SUBSCRIPTS.  NDIM MUST BE POSITIVE AND          00000080
C     EACH N(IDIM) MUST BE A POWER OF TWO.  ISIGN IS +1 OR -1.          00000090
C     LET NTOT = N(1)*N(2)*...*N(NDIM).  THEN A -1 TRANSFORM            00000100
C     FOLLOWED BY A +1 ONE (OR VICE VERSA) RETURNS NTOT                 00000110
C     TIMES THE ORIGINAL DATA.  IFORM = 1, 0 OR -1, AS DATA IS          00000120
C     COMPLEX, REAL OR THE FIRST HALF OF A COMPLEX ARRAY.  TRANSFORM    00000130
C     VALUES ARE RETURNED TO ARRAY DATA.  THEY ARE COMPLEX, REAL OR     00000140
C     THE FIRST HALF OF A COMPLEX ARRAY, AS IFORM = 1, -1 OR 0.         00000150
C     THE TRANSFORM OF A REAL ARRAY (IFORM = 0) DIMENSIONED N(1) BY N(2)00000160
C     BY ... WILL BE RETURNED IN THE SAME ARRAY, NOW CONSIDERED TO      00000170
C     BE COMPLEX OF DIMENSIONS N(1)/2+1 BY N(2) BY ....  NOTE THAT IF   00000180
C     IFORM = 0 OR -1, N(1) MUST BE EVEN, AND ENOUGH ROOM MUST BE       00000190
C     RESERVED.  THE MISSING VALUES MAY BE OBTAINED BY COMPLEX CONJUGA- 00000200
C     TION.  THE REVERSE TRANSFORMATION, OF A HALF COMPLEX ARRAY DIMEN- 00000210
C     SIONED N(1)/2+1 BY N(2) BY ..., IS ACCOMPLISHED BY SETTING IFORM  00000220
C     TO -1.  IN THE N ARRAY, N(1) MUST BE THE TRUE N(1), NOT N(1)/2+1. 00000230
C     THE TRANSFORM WILL BE REAL AND RETURNED TO THE INPUT ARRAY.       00000240
C     RUNNING TIME IS PROPORTIONAL TO NTOT*LOG2(NTOT), RATHER THAN      00000250
C     THE NAIVE NTOT**2.  FURTHERMORE, LESS ERROR IS BUILT UP.          00000260
C     WRITTEN BY NORMAN BRENNER OF MIT LINCOLN LABORATORY, JANUARY 1969.00000270
C     SEE-- IEEE AUDIO TRANSACTIONS (JUNE 1967), SPECIAL ISSUE ON FFT.  00000280
      IMPLICIT REAL*8(A-H,O-Z)                                          00000290
      REAL *4 XDATA                                                     00000310
      DIMENSION XDATA(1),N(1)                                           00000320
      DIMENSION DATA(600)                                               00000330
      DO 5 I=1,600                                                      00000340
5     DATA(I)=XDATA(I)                                                  00000350
      NTOT=1                                                            00000360
      DO 10 IDIM=1,NDIM                                                 00000370
 10   NTOT=NTOT*N(IDIM)                                                 00000380
      IF (IFORM) 70,20,20                                               00000390
 20   NREM=NTOT                                                         00000400
      DO 60 IDIM=1,NDIM                                                 00000410
      NREM=NREM/N(IDIM)                                                 00000420
      NPREV=NTOT/(N(IDIM)*NREM)                                         00000430
      NCURR=N(IDIM)                                                     00000440
      IF (IDIM-1+IFORM) 30,30,40                                        00000450
 30   NCURR=NCURR/2                                                     00000460
 40   CALL BITRV (DATA,NPREV,NCURR,NREM)                                00000470
      CALL COOL2 (DATA,NPREV,NCURR,NREM,ISIGN)                          00000480
      IF (IDIM-1+IFORM) 50,50,60                                        00000490
 50   CALL FIXRL (DATA,N(1),NREM,ISIGN,IFORM)                           00000500
      NTOT=(NTOT/N(1))*(N(1)/2+1)                                       00000510
 60   CONTINUE                                                          00000520
      DO 65 I=1,600                                                     00000530
65    XDATA(I)=DATA(I)                                                  00000540
      RETURN                                                            00000550
 70   NTOT=(NTOT/N(1))*(N(1)/2+1)                                       00000560
      NREM=1                                                            00000570
      DO 100 JDIM=1,NDIM                                                00000580
      IDIM=NDIM+1-JDIM                                                  00000590
      NCURR=N(IDIM)                                                     00000600
      IF (IDIM-1) 80,80,90                                              00000610
 80   NCURR=NCURR/2                                                     00000620
      CALL FIXRL (DATA,N(1),NREM,ISIGN,IFORM)                           00000630
      NTOT=NTOT/(N(1)/2+1)*N(1)                                         00000640
 90   NPREV=NTOT/(N(IDIM)*NREM)                                         00000650
      CALL BITRV (DATA,NPREV,NCURR,NREM)                                00000660
      CALL COOL2 (DATA,NPREV,NCURR,NREM,ISIGN)                          00000670
 100  NREM=NREM*N(IDIM)                                                 00000680
      DO 110 I=1,600                                                    00000690
110   XDATA(I)=DATA(I)                                                  00000700
      RETURN                                                            00000710
      END                                                               00000720
      SUBROUTINE GAUSSG(NZERO,XF,XWT,XSIA,XRAD,XWOCS)                   00000010
C                                                                       00000020
C     * THIS ROUTINE CALCULATES THE ROOTS (F) OF THE ORDINARY LEGENDRE  00000030
C     * POLYNOMIALS OF ORDER NZERO.  THE FIRST STEP IS TO MAKE AN       00000040
C     * INITIAL GUESS FOR EACH ROOT AND THEN TO USE THE ORDINARY        00000050
C     * LEGENDRE ALGORITHM (ORDLEG) AND NEWTONS METHOD TO REFINE        00000060
C     * THE SOLUTION UNTIL THE CRITERION XLIM IS SATISFIED.             00000070
C     *    F = COSINE OF COLATITUDE                                     00000080
C     *   WT = CORRESPONDING GAUSSIAN WEIGHT                            00000090
C     *  SIA = SINE OF COLATITUDE                                       00000100
C     *  RAD = COLATITUDE IN RADIANS                                    00000110
C     * WOCS = GAUSSIAN WEIGHT / COS(COLAT)**2                          00000120
C                                                                       00000130
      IMPLICIT REAL*8(A-H,O-Z)                                          00000140
      REAL *4 XF(1),XWT(1),XSIA(1),XRAD(1),XWOCS(1)                     00000160
      DIMENSION F( 100 ),WT( 100 ),SIA( 100 ),RAD( 100 ),               00000170
     1 WOCS( 100 )                                                      00000180
C-----------------------------------------------------------------------00000190
      XLIM=1.E-13                                                       00000200
      PI = 3.1415926535898                                              00000210
      IR = 2*NZERO                                                      00000220
      FI=FLOAT(IR)                                                      00000230
      FI1=FI+1.                                                         00000240
      FN=FLOAT(NZERO)                                                   00000250
C                                                                       00000260
      DO 20 I=1,NZERO                                                   00000270
      DOT=FLOAT(I-1)                                                    00000280
      F(I)=-PI*.5*(DOT+.5)/FN + PI*.5                                   00000290
      F(I) =  SIN(F(I))                                                 00000300
   20 CONTINUE                                                          00000310
C                                                                       00000320
      DN = FI/SQRT(4.*FI*FI-1.)                                         00000330
      DN1=FI1/SQRT(4.*FI1*FI1-1.)                                       00000340
      A = DN1*FI                                                        00000350
      B = DN*FI1                                                        00000360
      IRP = IR + 1                                                      00000370
      IRM = IR -1                                                       00000380
C                                                                       00000390
      DO 50 I=1,NZERO                                                   00000400
42    CALL ORDLED(G,F(I),IR)                                            00000410
      CALL ORDLED(GM,F(I),IRM)                                          00000420
      CALL ORDLED(GP,F(I),IRP)                                          00000430
      GT = (A*GP-B*GM)/(F(I)*F(I)-1.)                                   00000440
      FTEMP = F(I) - G/GT                                               00000450
      GTEMP = F(I) - FTEMP                                              00000460
      F(I) = FTEMP                                                      00000470
      IF( ABS(GTEMP).GT.XLIM) GO TO 42                                  00000480
   50 CONTINUE                                                          00000490
C                                                                       00000500
      DO 60 I=1,NZERO                                                   00000510
      A=2.*(1.-F(I)**2)                                                 00000520
      CALL ORDLED(B,F(I),IRM)                                           00000530
      B = B*B*FI*FI                                                     00000540
      WT(I)=A*(FI-.5)/B                                                 00000550
      RAD(I) =   ACOS(F(I))                                             00000560
      SIA(I) =  SIN(RAD(I))                                             00000570
      WOCS(I) =WT(I)/SIA(I)**2                                          00000580
   60 CONTINUE                                                          00000590
      DO 70 I=1,NZERO                                                   00000600
      XF(I)=F(I)                                                        00000610
      XWT(I)=WT(I)                                                      00000620
      XSIA(I)=SIA(I)                                                    00000630
      XRAD(I)=RAD(I)                                                    00000640
70    XWOCS(I)=WOCS(I)                                                  00000650
C                                                                       00000660
      RETURN                                                            00000670
      END                                                               00000680
      SUBROUTINE GGAST (SC,LSR,LM,LA, GG,ILG1,ILAT,ILEV,SL,WL,          00000010
     1                       ALP,EPSI,WRKS,WRKL)                        00000020
C                                                                       00000030
C     * JUL 31/79 - J.D.HENDERSON                                       00000040
C     * PRODUCES GLOBAL SPECTRAL COEFF SET SC(LA,ILEV)                  00000050
C     * FROM GLOBAL GAUSSIAN GRID SET GG(ILG1,ILAT,ILEV).               00000060
C     * LSR CONTAINS ROW LENGTH INFO FOR ALP,EPSI,SC.                   00000070
C     * ALP = WORK FIELD FOR LEGENDRE POLYNOMIALS.                      00000080
C     * EPSI = PRECOMPUTED CONSTANTS.                                   00000090
C     * SL = SIN(LATITUDE) OF GAUSSIAN ROWS FROM S.POLE TO N.POLE.      00000100
C     * WL = GAUSSIAN WEIGHTS                                           00000110
C                                                                       00000120
      COMPLEX SC(LA,ILEV)                                               00000130
      REAL GG(ILG1,ILAT,ILEV)                                           00000140
      REAL SL(1),WL(1)                                                  00000150
      REAL ALP(1),EPSI(1),WRKS(1)                                       00000160
      COMPLEX WRKL(1)                                                   00000170
      INTEGER LSR(2,1)                                                  00000180
C--------------------------------------------------------------------   00000190
      ILG=ILG1-1                                                        00000200
      ILGH=ILG/2                                                        00000210
      MAXF=LM-1                                                         00000220
      NP=LSR(1,LM+1)-1                                                  00000230
      DO 110 L=1,ILEV                                                   00000240
      DO 110 N=1,NP                                                     00000250
  110 SC(N,L)=(0.,0.)                                                   00000260
C                                                                       00000270
      DO 210 J=1,ILAT                                                   00000280
      CALL ALPST (ALP,LSR ,LM,SL(J),EPSI)                               00000290
C                                                                       00000300
      DO 205 L=1,ILEV                                                   00000310
      CALL FFWFG (WRKL,ILGH,GG(1,J,L),ILG1,MAXF,ILG,WRKS,1)             00000320
      CALL FAST (SC(1,L),LSR,LM,WRKL,ALP,WL(J))                         00000330
  205 CONTINUE                                                          00000340
C                                                                       00000350
  210 CONTINUE                                                          00000360
      RETURN                                                            00000370
      END                                                               00000380
      SUBROUTINE GGDYST(SC,LSR,LM,GG,ILG1,ILAT,SL,WL                    00000010
     1 ,ALP,DALP,EPSI,WRKS,WRKL)                                        00000020
C                                                                       00000030
C     * SEPT. 91 - J. SHENG                                             00000040
C     * PRODUCE GLOBAL SPECTRAL COEFF SET IN SC(LA) OF DGG/DY           00000050
C     * FROM GLOBAL GAUSSIAN GRID SET GG(ILG1,ILAT).                    00000060
C     * LSR CONTAINS ROW LENGTH INFO FOR ALP,EPSI,SC.                   00000070
C     * ALP = WORK FIELD FOR LEGENDRE POLYNOMIALS.                      00000080
C     * EPSI = PRECOMPUTED CONSTANTS.                                   00000090
C     * SL = SIN(LATITUDE) OF GAUSSIAN ROWS FROM S.POLE TO N.POLE.      00000100
C     * WL = GAUSSIAN WEIGHTS                                           00000110
C                                                                       00000120
      COMPLEX SC(1),WRKL(1)                                             00000130
      REAL GG(ILG1,ILAT)                                                00000140
      REAL SL(1),WL(1)                                                  00000150
      REAL ALP(1),DALP(1),EPSI(1),WRKS(1)                               00000160
      INTEGER LSR(2,1)                                                  00000180
C--------------------------------------------------------------------   00000190
      ILG=ILG1-1                                                        00000200
      ILGH=ILG/2                                                        00000210
      MAXF=LM-1                                                         00000220
      NP=LSR(1,LM+1)-1                                                  00000230
      DO 110 N=1,NP                                                     00000250
  110 SC(N)=CMPLX(0.,0.)                                                00000260
C                                                                       00000270
      DO 210 J=1,ILAT                                                   00000280
      CALL ALPST(ALP,LSR,LM,SL(J),EPSI)                                 00000290
      CALL ALPDY(DALP,ALP,LSR,LM,EPSI)                                  00000250
      CALL FFWFG(WRKL,ILGH,GG(1,J),ILG1,MAXF,ILG,WRKS,1)                00000320
      CALL FAST(SC,LSR,LM,WRKL,DALP,-WL(J))                             00000330
  210 CONTINUE                                                          00000360
      RETURN                                                            00000370
      END                                                               00000380
      SUBROUTINE GGILL (GLL,NLG1,NLAT,GG,ILG1,ILAT,SLAT,INTERP)         00000010
C                                                                       00000020
C     * MAY  1/80 - J.D.HENDERSON                                       00000030
C     * INTERPOLATES GLOBAL LAT-LONG GRID GLL(NLG1,NLAT)                00000040
C     * FROM GLOBAL GAUSSIAN GRID GG(ILG1,ILAT).                        00000050
C     * SLAT = LAT (DEG) OF GAUSSIAN GRID ROWS FROM THE S POLE.         00000060
C     * INTERP = (1,3) FOR (LINEAR,CUBIC) INTERPOLATION.                00000070
C     *          (OTHERWISE THE GRID GG IS SET TO ZERO).                00000080
C                                                                       00000090
C     LEVEL 2,GLL,GG                                                    00000100
      REAL GLL(NLG1,NLAT),GG(ILG1,ILAT)                                 00000110
      REAL SLAT(ILAT)                                                   00000120
C-----------------------------------------------------------------------00000130
      NLG=NLG1-1                                                        00000140
      DX=360./FLOAT(NLG)                                                00000150
      DY=180./FLOAT(NLAT-1)                                             00000160
C                                                                       00000170
      DO 320 J=1,NLAT                                                   00000180
      DLAT=FLOAT(J-1)*DY                                                00000190
      DO 310 I=1,NLG                                                    00000200
      DLON=FLOAT(I-1)*DX                                                00000210
      VAL=0.                                                            00000220
      IF(INTERP.EQ.1) CALL GGPNL (VAL,GG,ILG1,ILAT,DLAT,DLON,SLAT)      00000230
      IF(INTERP.EQ.3) CALL GGPNT (VAL,GG,ILG1,ILAT,DLAT,DLON,SLAT)      00000240
  310 GLL(I,J)=VAL                                                      00000250
  320 GLL(NLG1,J)=GLL(1,J)                                              00000260
C                                                                       00000270
C     * SET N POLE TO MEAN OF TOP ROW.                                  00000280
C     * SET S POLE TO MEAN OF BOTTOM ROW.                               00000290
C                                                                       00000300
      CALL FMEAN (GLL(1,NLAT),NLG,1,AVG,0)                              00000310
      DO 410 I=1,NLG1                                                   00000320
  410 GLL(I,NLAT)=AVG                                                   00000330
C                                                                       00000340
      CALL FMEAN (GLL(1,1),NLG,1,AVG,0)                                 00000350
      DO 510 I=1,NLG1                                                   00000360
  510 GLL(I,1)=AVG                                                      00000370
C                                                                       00000380
      RETURN                                                            00000390
      END                                                               00000400
      SUBROUTINE GGPNL (VAL,GG,ILG1,ILAT,DLAT,DLON,SLAT)                00000010
C                                                                       00000020
C     * MAY  1/80 - J.D.HENDERSON                                       00000030
C     * LINEAR INTERPOLATION AT POINT (DLON,DLAT)                       00000040
C     * IN GLOBAL GAUSSIAN GRID GG(ILG1,ILAT).                          00000050
C                                                                       00000060
C     LEVEL 2,GG                                                        00000070
      REAL GG(ILG1,ILAT)                                                00000080
      REAL SLAT(ILAT)                                                   00000090
C-----------------------------------------------------------------------00000100
      DI=360./FLOAT(ILG1-1)                                             00000110
      FI=DLON/DI+1.                                                     00000120
      I=INT(FI)                                                         00000130
      X=FI-FLOAT(I)                                                     00000140
C                                                                       00000150
      DO 110 K=1,ILAT                                                   00000160
      J=K-1                                                             00000170
  110 IF(DLAT.LT.SLAT(K)) GO TO 120                                     00000180
  120 IF(J.EQ.0) J=1                                                    00000190
      IF(DLAT.GT.SLAT(ILAT)) J=ILAT-1                                   00000200
      Y=(DLAT-SLAT(J))/(SLAT(J+1)-SLAT(J))                              00000210
C                                                                       00000220
      BOT=(1.-X)*GG(I,J)  +X*GG(I+1,J)                                  00000230
      TOP=(1.-X)*GG(I,J+1)+X*GG(I+1,J+1)                                00000240
C                                                                       00000250
      VAL=(1.-Y)*BOT+Y*TOP                                              00000260
C                                                                       00000270
      RETURN                                                            00000280
      END                                                               00000290
      SUBROUTINE GGPNT (VAL,GG,ILG1,ILAT,DLAT,DLON,WRKS)                00000010
C                                                                       00000020
C    *  WRITTEN BY ROGER DALEY - NOVEMBER 1975.                         00000030
C    *  THIS SUBROUTINE INTERPOLATEDS FROM A GLOBAL GAUSSIAN GRID       00000040
C    *  GG(ILG1,ILAT) TO A SINGLE POINT WITH LATITUDE (DEGREES) DLAT    00000050
C    *  AND LONGITUDE DLON. DLAT IS MEASURED FROM THE SOUTH POLE        00000060
C    *  AND DLON IS MEASURED EASTWARD FROM GREENWICH AND MUST SATISFY   00000070
C    *   (0.LE.DLON.LE.360).                                            00000080
C    *  BECAUSE THE GAUSSIAN LATITUDES ARE ONLY APPROXIMATELY EQUALLY   00000090
C     * SPACED, SOME SEARCHING WILL BE DONE IN THE GENERAL CASE.        00000100
C     * WRKS CONTAINS THE LATITUDES IN DEGREES FROM THE S. POLE.        00000110
C                                                                       00000120
C     LEVEL 2,GG                                                        00000130
      DIMENSION GG(ILG1,2)                                              00000140
      DIMENSION WRKS(2)                                                 00000150
C-----------------------------------------------------------------------00000160
C                                                                       00000170
      DX = 1./(WRKS(2)-WRKS(1))                                         00000180
      DINT = FLOAT(ILG1-1)/360.                                         00000190
      RX = DLON*DINT + 1.                                               00000200
      IRX = INT(RX)                                                     00000210
C                                                                       00000220
      RY = (DLAT-WRKS(1))*DX + 1.                                       00000230
      IF(DLAT.GT.90.) RY = (DLAT-WRKS(ILAT))*DX+ FLOAT(ILAT)            00000240
      IF(DLAT.LE.WRKS(2).OR.DLAT.GE.WRKS(ILAT -1)) GO TO 180            00000250
C                                                                       00000260
      N= INT(RY)                                                        00000270
      AG = WRKS(N)                                                      00000280
      AGP = WRKS(N+1)                                                   00000290
      AGM = WRKS(N-1)                                                   00000300
      RY = FLOAT(N) + (DLAT-AG)/(AGP-AG)                                00000310
      IF(DLAT.LT.AG) RY = FLOAT(N-1) + (DLAT-AGM)/(AG-AGM)              00000320
C                                                                       00000330
  180 CALL GINTC (VAL,GG,ILG1,ILAT,RX,RY)                               00000340
      IF(DLAT.GT.WRKS(1).AND.DLAT.LT.WRKS(ILAT)) RETURN                 00000350
C                                                                       00000360
C    *  SPECIAL CASE FOR DLAT NEAR NORTH POLE OR SOUTH POLE.            00000370
C                                                                       00000380
      IF(DLAT.GT.WRKS(1)) GO TO 196                                     00000390
      CALL FMEAN (GG(1,1),ILG1,1,RS1,0)                                 00000400
      CALL FMEAN (GG(1,2),ILG1,1,RS2,0)                                 00000410
      RSOUTH =  RS1 + (RS1-RS2)*DX*WRKS(1)                              00000420
      VAL = (RX-FLOAT(IRX))*GG(IRX,1) + (FLOAT(IRX+1)-RX)*GG(IRX+1,1)   00000430
      VAL = ((WRKS(1)-DLAT)*RSOUTH + DLAT*VAL)/WRKS(1)                  00000440
      RETURN                                                            00000450
C                                                                       00000460
  196 CALL FMEAN (GG(1,ILAT),ILG1,1,RN1,0)                              00000470
      CALL FMEAN (GG(1,ILAT-1),ILG1,1,RN2,0)                            00000480
      RNORTH =  RN1 + (RN1-RN2)*DX*WRKS(1)                              00000490
      VAL=(RX-FLOAT(IRX))*GG(IRX,ILAT)+(FLOAT(IRX+1)-RX)*GG(IRX+1,ILAT) 00000500
      VAL =((DLAT-WRKS(ILAT))*RNORTH + (180.-DLAT)*VAL)/                00000510
     1   (180.-WRKS(ILAT))                                              00000520
C                                                                       00000530
      RETURN                                                            00000540
      END                                                               00000550
      SUBROUTINE GINTC (VAL,G,NI,NJ,FI,FJ)                              00000660
C                                                                       00000670
C     *****   JAN 1975  -  JOHN D. HENDERSON                            00000680
C     * CUBIC INTERPOLATION AT POINT (FI,FJ) IN GRID G(NI,NJ).          00000690
C     * THE GRID MUST BE AT LEAST 4 BY 4 POINTS.                        00000700
C     * IF (FI,FJ) IS OUTSIDE THE GRID AN EXTRAPOLATION IS DONE.        00000710
C                                                                       00000720
      DIMENSION G(NI,1),AP(4),AQ(4),A(4)                                00000730
C-----------------------------------------------------------------------00000740
      SIXTH=1./6.                                                       00000750
C                                                                       00000760
C     * CALCULATE X-PARAMETERS AND Y-PARAMETERS.                        00000770
C                                                                       00000780
      I=INT(FI)                                                         00000790
      IF(I.GT.NI-2)  I=NI-2                                             00000800
      IF(I.LT.2)     I=2                                                00000810
      IM2=I-2                                                           00000820
      P=FI-FLOAT(I)                                                     00000830
      AP(1)=SIXTH*(-P*(P-1.)*(P-2.))                                    00000840
      AP(2)=  0.5*(   (P-1.)*(P+1.)*(P-2.))                             00000850
      AP(3)=  0.5*(-P*(P+1.)*(P-2.))                                    00000860
      AP(4)=SIXTH*( P*(P+1.)*(P-1.))                                    00000870
C                                                                       00000880
      J=INT(FJ)                                                         00000890
      IF(J.GT.NJ-2) J=NJ-2                                              00000900
      IF(J.LT.2)    J=2                                                 00000910
      JM2=J-2                                                           00000920
      Q=FJ-FLOAT(J)                                                     00000930
      AQ(1)=SIXTH*(-Q*(Q-1.)*(Q-2.))                                    00000940
      AQ(2)=  0.5*(   (Q-1.)*(Q+1.)*(Q-2.))                             00000950
      AQ(3)=  0.5*(-Q*(Q+1.)*(Q-2.))                                    00000960
      AQ(4)=SIXTH*( Q*(Q+1.)*(Q-1.))                                    00000970
C                                                                       00000980
C     * INTERPOLATE IN EACH ROW THEN IN THE RESULTING COL FOR VAL.      00000990
C                                                                       00001000
      DO 20 L=1,4                                                       00001010
      A(L)=0.                                                           00001020
      DO 20 K=1,4                                                       00001030
   20 A(L)=A(L)+AP(K)*G(IM2+K,JM2+L)                                    00001040
      VAL=AQ(1)*A(1)+AQ(2)*A(2)+AQ(3)*A(3)+AQ(4)*A(4)                   00001050
C                                                                       00001060
      RETURN                                                            00001070
      END                                                               00001080
      SUBROUTINE GINTL (VAL,F,NI,NJ,FI,FJ)                              00000420
C                                                                       00000430
C     * DEC 17/80 - J.D.HENDERSON                                       00000440
C     * LINEAR INTERPOLATION AT POINT (FI,FJ) IN F(NI,NJ).              00000450
C     * EXTRAPOLATION IS DONE IF POINT IS OUTSIDE THE GRID              00000460
C                                                                       00000470
      REAL F(NI,NJ)                                                     00000480
C--------------------------------------------------------------------   00000490
      I=INT(FI)                                                         00000500
      IF(I.LT.1) I=1                                                    00000510
      IF(I.GE.NI) I=NI-1                                                00000520
      X=FI-FLOAT(I)                                                     00000530
      J=INT(FJ)                                                         00000540
      IF(J.LT.1) J=1                                                    00000550
      IF(J.GE.NJ) J=NJ-1                                                00000560
      Y=FJ-FLOAT(J)                                                     00000570
C                                                                       00000580
      BOT=(1.-X)*F(I,J)  +X*F(I+1,J)                                    00000590
      TOP=(1.-X)*F(I,J+1)+X*F(I+1,J+1)                                  00000600
C                                                                       00000610
      VAL=(1.-Y)*BOT+Y*TOP                                              00000620
C                                                                       00000630
      RETURN                                                            00000640
      END                                                               00000650
      SUBROUTINE GWAQD (Q,D,LSR,LM,UGG,VGG,ILG1,ILAT,SL,WOSSL,          00000010
     1               ALP,DALP,EPSI,WRKS,WRKL)                           00000020
C                                                                       00000030
C     * JUL 18/79 - J.D.HENDERSON                                       00000040
C     * CONVERTS GLOBAL GAUSSIAN GRIDS OF U,V IN UGG,VGG(ILG1,ILAT)     00000050
C     * TO SPECTRAL COEFF OF VORTICITY (Q) AND DIVERGENCE (D).          00000060
C     * NOTE THAT U,V ARE ACTUALLY U,V*COS(LAT)/(EARTH RADIUS).         00000070
C                                                                       00000080
C     * LSR CONTAINS SPECTRAL ROW LENGTH INFO.                          00000090
C     * SL(ILAT) CONTAINS SIN(LAT) (S TO N).                            00000100
C     * WOSSL(ILAT) CONTAINS (GAUSSIAN WEIGHTS)/(SIN(LAT)**2).          00000110
C     * ALP IS A WORK FIELD FOR LEGENDRE POLYNOMIALS.                   00000120
C     * DALP IS FOR N-S DERIVATIVES OF ALP.                             00000130
C     * EPSI CONTAINS PRECOMPUTED CONSTANTS.                            00000140
C     * WRKS,WRKL ARE SCM,LCM WORK FIELDS OF LR COMPLEX WORDS.          00000150
C     * FFT REQUIRES THAT ILG BE (2**N) OR 3*(2**N).                    00000160
C                                                                       00000170
      COMPLEX Q(1),D(1),WRKL(1)                                         00000180
      REAL UGG(ILG1,1),VGG(ILG1,1)                                      00000190
      REAL ALP(1),DALP(1),EPSI(1)                                       00000200
      REAL SL(1),WOSSL(1),WRKS(1)                                       00000210
      INTEGER LSR(2,1)                                                  00000220
C---------------------------------------------------------------------  00000230
      ILG=ILG1-1                                                        00000240
      ILGH=ILG/2                                                        00000250
C                                                                       00000260
C     * INITIALIZE Q,D TO ZERO.                                         00000270
C                                                                       00000280
      NP=LSR(1,LM+1)-1                                                  00000290
      CALL SCOF (Q,NP,1,0)                                              00000300
      CALL SCOF (D,NP,1,0)                                              00000310
C                                                                       00000320
      DO 40 J=1,ILAT                                                    00000330
      WEIGHT=WOSSL(J)                                                   00000340
C                                                                       00000350
C     * COMPUTE ALP AND DALP.                                           00000360
C                                                                       00000370
      CALL ALPST (ALP, LSR,LM,SL(J),EPSI)                               00000380
      CALL ALPDY (DALP,ALP, LSR,LM,EPSI)                                00000390
C                                                                       00000400
C     * TERMS DEPENDING ON U.                                           00000410
C                                                                       00000420
      CALL FFWFG (WRKL,ILGH,UGG(1,J),ILG1,LM-1,ILG,WRKS,1)              00000430
      CALL FAST (Q,LSR,LM,WRKL,DALP, WEIGHT)                            00000440
      DO 22 M=1,LM                                                      00000450
      FMS=FLOAT(M-1)                                                    00000460
      WRKL(M)=CMPLX(-FMS*AIMAG(WRKL(M)),FMS*REAL(WRKL(M)))              00000470
   22 CONTINUE                                                          00000480
      CALL FAST (D,LSR,LM,WRKL, ALP, WEIGHT)                            00000490
C                                                                       00000500
C     * TERMS DEPENDING ON V.                                           00000510
C                                                                       00000520
      CALL FFWFG (WRKL,ILGH,VGG(1,J),ILG1,LM-1,ILG,WRKS,1)              00000530
      CALL FAST (D,LSR,LM,WRKL,DALP,-WEIGHT)                            00000540
      DO 24 M=1,LM                                                      00000550
      FMS=FLOAT(M-1)                                                    00000560
      WRKL(M)=CMPLX(-FMS*AIMAG(WRKL(M)),FMS*REAL(WRKL(M)))              00000570
   24 CONTINUE                                                          00000580
      CALL FAST (Q,LSR,LM,WRKL, ALP, WEIGHT)                            00000590
   40 CONTINUE                                                          00000600
C                                                                       00000610
      RETURN                                                            00000620
      END                                                               00000630
      SUBROUTINE INVLAP(SA,SB,LSR,LM)                                   00000010
C                                                                       00000030
C     * 9/91 J. SHENG                                                           
C     * PRODUCES IN SB(LA) SPECTRAL COEFFICIENTS OF F FROM SPECTRAL     00000050
C     * COEFFICIENTS OF LAPLACE F IN SA(LA).                            00000060
C     * LSR CONTAINS ROW LENGTH INFO FOR SA AND SB.                             
C                                                                               
      COMPLEX SA(1),SB(1)                                                       
      INTEGER LSR(2,1)                                                          
      EARTH=6370949.0                                                           
      RAD2=EARTH*EARTH                                                          
      DO 12 I=1,LM                                                              
      MN1=LSR(1,I)                                                              
      MN2=LSR(1,I+1)-1                                                          
      MS=I-1                                                                    
      DO 12 MN=MN1,MN2                                                          
      NS=MS+MN-MN1                                                              
      AN=FLOAT(NS)                                                              
      IF(MN.NE.1)SB(MN)=-SA(MN)*RAD2/(AN*(AN+1.0))                              
   12 CONTINUE                                                                  
      SB(1)=(0.0,0.0)                                                           
      RETURN                                                                    
      END                                                                       
      SUBROUTINE JACOB(GG,ILG1,ILAT,NN,SL,WOCS,SA,SB,SC,SD              00000010
     1 ,LSR,LM,LA,ALP,DALP,EPSI,WRKS,WRKL)                              00000020
C                                                                       00000030
C     * 9/91 J. SHENG                                                           
C     * PRODUCE IN SC(LA) SPECTRAL COEFF SET OF J(SA, SB)               00000050
C     * FROM SPECTRAL COEFF SETS IN SA(LA) AND SB(LA).                  00000060
C     * SD IS A WORKING SPACE.                                                  
C     * LONGITUDE IS ZERO AT THE LEFT AND POSITIVE TO THE RIGHT.        00000070
C     * LSR CONTAINS ROW LENGTH INFO FOR ALP, DALP, EPSI, SA, SB,       00000080
C     * SC AND SD.                                                              
C     * ALP = WORK FIELD FOR LEGENDRE POLYNOMIALS.                      00000090
C     * DALP = WORK FIELD FOR DERIVATIVE OF LEGENDRE POLYNOMIALS.       00000090
C     * EPSI = PRECOMPUTED CONSTANTS.                                   00000100
C     * SL = SIN(LATITUDE) OF GAUSSIAN ROWS FROM S.POLE TO N.POLE.      00000110
C                                                                       00000120
      COMPLEX SA(1),SB(1),SC(1),SD(1),WRKL(1)                           00000130
      REAL GG(ILG1,ILAT,NN),SL(1),WOCS(1),ALP(1),DALP(1),EPSI(1)        00000140
      REAL WRKS(1)                                                              
      INTEGER LSR(2,1)                                                  00000180
C                                                                               
      EARTH=6370949.0                                                           
      RAD2=EARTH*EARTH                                                          
      IF(NN.LT.4)THEN                                                           
      WRITE(6,*)'IN JACOB, NN TOO SMALL: ',NN                                   
      STOP 9                                                                    
      ENDIF                                                                     
      CALL STDXGG(GG(1,1,1),ILG1,ILAT,SL,SA,SD,LSR,LM                   00000010
     1 ,ALP,EPSI,WRKS,WRKL)                                             00000020
      CALL STDYGG(GG(1,1,2),ILG1,ILAT,SL,SA,LSR,LM                      00000010
     1 ,ALP,DALP,EPSI,WRKS,WRKL)                                        00000020
      CALL STDXGG(GG(1,1,3),ILG1,ILAT,SL,SB,SD,LSR,LM                   00000010
     1 ,ALP,EPSI,WRKS,WRKL)                                             00000020
      CALL STDYGG(GG(1,1,4),ILG1,ILAT,SL,SB,LSR,LM                      00000010
     1 ,ALP,DALP,EPSI,WRKS,WRKL)                                        00000020
      DO 11 J=1,ILAT                                                            
      DO 11 I=1,ILG1                                                            
      GG(I,J,1)=(GG(I,J,1)*GG(I,J,4)-GG(I,J,2)*GG(I,J,3))/RAD2                  
   11 CONTINUE                                                                  
      CALL GGAST(SC,LSR,LM,LA,GG,ILG1,ILAT,1,SL,WOCS                            
     1 ,ALP,EPSI,WRKS,WRKL)                                                     
      RETURN                                                            00000330
      END                                                               00000340
      SUBROUTINE JACOB2(GG,ILG1,ILAT,NN,SL,WOCS,SA,SB,SC,SD             00000010
     1 ,LSR,LM,LA,ALP,DALP,EPSI,WRKS,WRKL)                              00000020
C                                                                       00000030
C     * 9/91 J. SHENG                                                           
C     * PRODUCE IN SC(LA) SPECTRAL COEFF SET OF J(SA, SB)               00000050
C     * FROM SPECTRAL COEFF SETS IN SA(LA) AND SB(LA).                  00000060
C     * SD IS A WORKING SPACE.                                                  
C     * LONGITUDE IS ZERO AT THE LEFT AND POSITIVE TO THE RIGHT.        00000070
C     * LSR CONTAINS ROW LENGTH INFO FOR ALP, DALP, EPSI, SA, SB,       00000080
C     * SC AND SD.                                                              
C     * ALP = WORK FIELD FOR LEGENDRE POLYNOMIALS.                      00000090
C     * DALP = WORK FIELD FOR DERIVATIVE OF LEGENDRE POLYNOMIALS.       00000090
C     * EPSI = PRECOMPUTED CONSTANTS.                                   00000100
C     * SL = SIN(LATITUDE) OF GAUSSIAN ROWS FROM S.POLE TO N.POLE.      00000110
C                                                                       00000120
      COMPLEX SA(1),SB(1),SC(1),SD(1),WRKL(1)                           00000130
      REAL GG(ILG1,ILAT,NN),SL(1),WOCS(1),ALP(1),DALP(1),EPSI(1)        00000140
      REAL WRKS(1)                                                              
      INTEGER LSR(2,1)                                                  00000180
C                                                                               
      EARTH=6370949.0                                                           
      RAD2=EARTH*EARTH                                                          
      IF(NN.LT.3)THEN                                                           
      WRITE(6,*)'IN JACOB2, NN TOO SMALL: ',NN                                  
      STOP 9                                                                    
      ENDIF                                                                     
      CALL STAGG(GG(1,1,1),ILG1,ILAT,1,SL,SA,LSR,LM,LA                  00000010
     1 ,ALP,EPSI,WRKS,WRKL)                                             00000020
      CALL STDXGG(GG(1,1,2),ILG1,ILAT,SL,SB,SD,LSR,LM                   00000010
     1 ,ALP,EPSI,WRKS,WRKL)                                             00000020
      CALL STDYGG(GG(1,1,3),ILG1,ILAT,SL,SB,LSR,LM                      00000010
     1 ,ALP,DALP,EPSI,WRKS,WRKL)                                        00000020
      DO 11 J=1,ILAT                                                            
      DO 11 I=1,ILG1                                                            
      AA=GG(I,J,1)/RAD2                                                         
      GG(I,J,1)=-AA*GG(I,J,2)                                                   
      GG(I,J,2)=AA*GG(I,J,3)                                                    
   11 CONTINUE                                                                  
      CALL GGDYST(SC,LSR,LM,GG(1,1,1),ILG1,ILAT,SL,WOCS                         
     1 ,ALP,DALP,EPSI,WRKS,WRKL)                                                
      CALL GGAST(SD,LSR,LM,LA,GG(1,1,2),ILG1,ILAT,1,SL,WOCS                     
     1 ,ALP,EPSI,WRKS,WRKL)                                                     
      DO 22 M=1,LM                                                      00000110
      AM=FLOAT(M-1)                                                     00000120
      KL=LSR(1,M)                                                       00000130
      KR=LSR(1,M+1)-1                                                   00000140
      DO 22 K=KL,KR                                                     00000170
   22 SC(K)=SC(K)+CMPLX(-AM*AIMAG(SD(K)),AM*REAL(SD(K)))                00000210
      RETURN                                                            00000330
      END                                                               00000340
      SUBROUTINE LAP(SA,SB,LSR,LM)                                      00000010
C                                                                       00000030
C     * 9/91 J. SHENG                                                           
C     * PRODUCES IN SB(LA) SPECTRAL COEFFICIENTS OF LAPLACE F FROM      00000050
C     * SPECTRAL COEFFICIENTS OF F IN SA(LA).                           00000060
C     * LSR CONTAINS ROW LENGTH INFO FOR SA AND SB.                             
C                                                                               
      COMPLEX SA(1),SB(1)                                                       
      INTEGER LSR(2,1)                                                          
      EARTH=6370949.0                                                           
      RAD2=EARTH*EARTH                                                          
      DO 12 I=1,LM                                                              
      MN1=LSR(1,I)                                                              
      MN2=LSR(1,I+1)-1                                                          
      MS=I-1                                                                    
      DO 12 MN=MN1,MN2                                                          
      NS=MS+MN-MN1                                                              
      AN=FLOAT(NS)                                                              
      SB(MN)=-AN*(AN+1.0)*SA(MN)/RAD2                                           
   12 CONTINUE                                                                  
      SB(1)=(0.0,0.0)                                                           
      RETURN                                                                    
      END                                                                       
      SUBROUTINE LLIGG (GG,ILG1,ILAT,DLAT, GLL,NLG1,NLAT,INTERP)        00000010
C                                                                       00000020
C     * APR 30/80 - J.D.HENDERSON                                       00000030
C     * INTERPOLATES GLOBAL GAUSSIAN GRID GG(ILG1,ILAT)                 00000040
C     * FROM GLOBAL LAT-LONG GRID GLL(NLG1,NLAT).                       00000050
C     * DLAT = LAT (DEG) OF GG ROWS.                                    00000060
C     * INTERP = (1,3) FOR (LINEAR,CUBIC) INTERPOLATION.                00000070
C     *          (OTHERWISE THE GRID GG IS SET TO ZERO).                00000080
C                                                                       00000090
      REAL GG(ILG1,ILAT),GLL(NLG1,NLAT)                                 00000100
      REAL DLAT(ILAT)                                                   00000110
C--------------------------------------------------------------------   00000120
      ILG=ILG1-1                                                        00000130
      DEGX=360./FLOAT(NLG1-1)                                           00000140
      DEGY=180./FLOAT(NLAT-1)                                           00000150
C                                                                       00000160
C     * ROWS ARE DONE FROM SOUTH TO NORTH.                              00000170
C     * DROW = DEGREES OF ROW J FROM S POLE.                            00000180
C     * (X,Y) = COORD OF GG(I,J) IN LAT-LONG GRID.                      00000190
C                                                                       00000200
      DO 240 J=1,ILAT                                                   00000210
      DROW=DLAT(J)+90.                                                  00000220
      Y=DROW/DEGY+1.                                                    00000230
C                                                                       00000240
C     * POINTS ARE INTERPOLATED FROM LEFT TO RIGHT.                     00000250
C     * DLON = DEGREES OF POINT I EAST FROM GREENWICH.                  00000260
C                                                                       00000270
      DO 220 I=1,ILG                                                    00000280
      DLON=FLOAT(I-1)/FLOAT(ILG)*360.                                   00000290
      X=DLON/DEGX+1.                                                    00000300
      VAL=0.                                                            00000310
      IF(INTERP.EQ.1) CALL GINTL (VAL,GLL,NLG1,NLAT,X,Y)                00000320
      IF(INTERP.EQ.3) CALL GINTC (VAL,GLL,NLG1,NLAT,X,Y)                00000330
      GG(I,J)=VAL                                                       00000340
  220 CONTINUE                                                          00000350
C                                                                       00000360
      GG(ILG1,J)=GG(1,J)                                                00000370
  240 CONTINUE                                                          00000380
C                                                                       00000390
      RETURN                                                            00000400
      END
      SUBROUTINE MOVE1(AREAL,ACOMP,INDEX,IRC,NINDEX)
      INTEGER INDEX(NINDEX),IRC(NINDEX)
      REAL AREAL(1)
      COMPLEX ACOMP(1)
      DO 11 IJ=1,NINDEX
      IF(IRC(IJ).EQ.1)AREAL(IJ)=REAL(ACOMP(INDEX(IJ)))
      IF(IRC(IJ).EQ.2)AREAL(IJ)=AIMAG(ACOMP(INDEX(IJ)))
   11 CONTINUE
      RETURN
      END
      SUBROUTINE MOVE2(AREAL,ACOMP,INDEX,IRC,NINDEX,LA)
      INTEGER INDEX(NINDEX),IRC(NINDEX)
      REAL AREAL(1)
      COMPLEX ACOMP(1)
      DO 11 IJ=1,LA
      ACOMP(IJ)=(0.0,0.0)
   11 CONTINUE
      DO 12 IJ=1,NINDEX
      IF(IRC(IJ).EQ.1)
     1 ACOMP(INDEX(IJ))=ACOMP(INDEX(IJ))+CMPLX(AREAL(IJ),0.0)
      IF(IRC(IJ).EQ.2)
     1 ACOMP(INDEX(IJ))=ACOMP(INDEX(IJ))+CMPLX(0.0,AREAL(IJ))
   12 CONTINUE
      RETURN
      END
      SUBROUTINE ORDLED(SX,COA,IR)                                      00000010
      IMPLICIT REAL*8(A-H,O-Z)                                          00000020
      PI=3.1415926535898                                                00000040
      SQR2=SQRT(2.)                                                     00000050
      IRPP=IR+1                                                         00000060
      IRPPM=IRPP-1                                                      00000070
      DELTA=ACOS(COA)                                                   00000080
      SIA=SIN(DELTA)                                                    00000090
      THETA=DELTA                                                       00000100
      C1=SQR2                                                           00000110
      DO 20 N=1,IRPPM                                                   00000120
      FN=FLOAT(N)                                                       00000130
      FN2=2.*FN                                                         00000140
      FN2SQ=FN2*FN2                                                     00000150
      C1=C1*SQRT(1.0-1.0/FN2SQ)                                         00000160
20    CONTINUE                                                          00000170
      N=IRPPM                                                           00000180
      ANG=FN*THETA                                                      00000190
      S1=0.0                                                            00000200
      C4=1.0                                                            00000210
      A=-1.0                                                            00000220
      B=0.0                                                             00000230
      N1=N+1                                                            00000240
      DO 27 KK=1,N1,2                                                   00000250
      K=KK-1                                                            00000260
      IF(K.EQ.N) C4=C4*0.5                                              00000270
      S1=S1+C4*COS(ANG)                                                 00000280
      A=A+2.0                                                           00000290
      B=B+1.0                                                           00000300
      FK=FLOAT(K)                                                       00000310
      ANG=THETA*(FN-FK-2.0)                                             00000320
      C4=(A*(FN-B+1.0)/(B*(FN2-A)))*C4                                  00000330
27    CONTINUE                                                          00000340
      SX=S1*C1                                                          00000350
      RETURN                                                            00000360
      END                                                               00000370
      SUBROUTINE PERM(WORKA,WORKB,AA,BB,KP6,K3)                         00001190
C                                                                       00001200
C     *****   FEB 1976  -  ROGER DALEY   *****                          00001210
C    *   CALCULATES SETUP FIELD FOR 3 TIMES POWER OF 2 TRANSFORM        00001220
C                                                                       00001230
      COMPLEX WORKA(1),WORKB(1),AA,BB                                   00001240
C-----------------------------------------------------------------------00001250
C                                                                       00001260
      PI = 3.14159265358979                                             00001270
      FACT = 2.*PI/3.                                                   00001280
      AA = CMPLX(COS(FACT),SIN(FACT))                                   00001290
      BB = CONJG(AA)                                                    00001300
      FT = FACT/FLOAT(K3)                                               00001310
      DO 125 K=1,KP6                                                    00001320
      FK = FT*FLOAT(K-1)                                                00001330
      WORKA(K) = CMPLX(COS(FK),SIN(FK))                                 00001340
  125 WORKB(K) = CONJG(WORKA(K))                                        00001350
C                                                                       00001360
      RETURN                                                            00001370
      END                                                               00001380
      SUBROUTINE RCOM(WORKA,WORKB,AA,BB,DATA,DATB,DATC,FIN,KP6,K3)      00000010
      COMPLEX WORKA(1),WORKB(1),AA,BB,CC,DD                             00000020
      COMPLEX DATA(1),DATB(1),DATC(1),FIN(1)                            00000030
      DO 100 K=1,KP6                                                    00000040
      CC=WORKB(K)*DATB(K)                                               00000050
      DD=WORKA(K)*DATC(K)                                               00000060
      KP=K3+2-K                                                         00000070
      FIN(K)=DATA(K)+CC+DD                                              00000080
      FIN(KP)=CONJG(DATA(K)+AA*CC+BB*DD)                                00000090
100   CONTINUE                                                          00000100
      RETURN                                                            00000110
      END                                                               00000120
      SUBROUTINE RGAUSSJ(A,N,NP,B,M,MP)
      PARAMETER (NMAX=2000)
      DIMENSION A(NP,NP),B(NP,MP),IPIV(NMAX),INDXR(NMAX),INDXC(NMAX)
      IF(NMAX.LT.N)THEN
      WRITE(6,*)'FROM RGAUSSJ: NMAX IS SHORTER THAN N'
      WRITE(6,*)'NMAX=',NMAX,'       N=',N
      STOP 9
      ENDIF
      DO 11 J=1,N
        IPIV(J)=0
11    CONTINUE
      DO 22 I=1,N
        BIG=0.
        DO 13 J=1,N
          IF(IPIV(J).NE.1)THEN
            DO 12 K=1,N
              IF (IPIV(K).EQ.0) THEN
                IF (ABS(A(J,K)).GE.BIG)THEN
                  BIG=ABS(A(J,K))
                  IROW=J
                  ICOL=K
                ENDIF
              ELSE IF (IPIV(K).GT.1) THEN
                PAUSE 'Singular matrix'
              ENDIF
12          CONTINUE
          ENDIF
13      CONTINUE
        IPIV(ICOL)=IPIV(ICOL)+1
        IF (IROW.NE.ICOL) THEN
          DO 14 L=1,N
            DUM=A(IROW,L)
            A(IROW,L)=A(ICOL,L)
            A(ICOL,L)=DUM
14        CONTINUE
          DO 15 L=1,M
            DUM=B(IROW,L)
            B(IROW,L)=B(ICOL,L)
            B(ICOL,L)=DUM
15        CONTINUE
        ENDIF
        INDXR(I)=IROW
        INDXC(I)=ICOL
        IF (A(ICOL,ICOL).EQ.0.) PAUSE 'Singular matrix.'
        PIVINV=1./A(ICOL,ICOL)
        A(ICOL,ICOL)=1.
        DO 16 L=1,N
          A(ICOL,L)=A(ICOL,L)*PIVINV
16      CONTINUE
        DO 17 L=1,M
          B(ICOL,L)=B(ICOL,L)*PIVINV
17      CONTINUE
        DO 21 LL=1,N
          IF(LL.NE.ICOL)THEN
            DUM=A(LL,ICOL)
            A(LL,ICOL)=0.
            DO 18 L=1,N
              A(LL,L)=A(LL,L)-A(ICOL,L)*DUM
18          CONTINUE
            DO 19 L=1,M
              B(LL,L)=B(LL,L)-B(ICOL,L)*DUM
19          CONTINUE
          ENDIF
21      CONTINUE
22    CONTINUE
      DO 24 L=N,1,-1
        IF(INDXR(L).NE.INDXC(L))THEN
          DO 23 K=1,N
            DUM=A(K,INDXR(L))
            A(K,INDXR(L))=A(K,INDXC(L))
            A(K,INDXC(L))=DUM
23        CONTINUE
        ENDIF
24    CONTINUE
      RETURN
      END
      SUBROUTINE SCOF (SC,LR,LM,KIND)                                   00000010
C                                                                       00000020
C     *****   OCT 1975  -  JOHN D. HENDERSON   *****                    00000030
C     * ZERO OR DOUBLE THE COMPLEX SPECTRAL COEFFICIENTS IN SC(LR,LM)   00000040
C     * DEPENDING ON THE VALUE OF KIND.                                 00000050
C                                                                       00000060
      COMPLEX SC(LR,1)                                                  00000070
C-----------------------------------------------------------------------00000080
C                                                                       00000090
C     * IF KIND=0 SET ALL OF SC TO (0.,0.)                              00000100
C                                                                       00000110
      IF(KIND.NE.0) GO TO 30                                            00000120
      DO 20 M=1,LM                                                      00000130
      DO 20 N=1,LR                                                      00000140
   20 SC(N,M)=(0.,0.)                                                   00000150
C                                                                       00000160
C     * IF KIND=2 DOUBLE ALL OF SC                                      00000170
C                                                                       00000180
   30 IF(KIND.NE.2) GO TO 99                                            00000190
      DO 40 M=1,LM                                                      00000200
      DO 40 N=1,LR                                                      00000210
   40 SC(N,M)=SC(N,M)+SC(N,M)                                           00000220
C                                                                       00000230
   99 RETURN                                                            00000240
      END                                                               00000250
      SUBROUTINE STAF (CFC,SC,LSR,LM,ALP)                               00000010
C                                                                       00000020
C     * APR  1/80 - J.D.HENDERSON                                       00000030
C     * CONVERTS SPHERICAL HARMONICS IN SC TO FOURIER COEFF IN CFC      00000040
C     * AT ONE PARTICULAR LATITUDE.                                     00000050
C     * SPECTRAL HARMONICS MUST BE GLOBAL.                              00000060
C                                                                       00000070
C     * LSR(1,M)=START OF ROW M OF SC                                   00000080
C     * LSR(2,M)=START OF ROW M OF ALP                                  00000090
C     * LM = NUMBER OF ROWS IN SC,ALP ARRAYS.                           00000100
C     * ALP = ASSOCIATED LEGENDRE POLYNOMIALS FOR GIVEN LATITUDE.       00000110
C                                                                       00000120
      REAL *8 FCR,FCI,XALP,XRSC,XISC                                    00000130
      COMPLEX SC(1),CFC(1)                                              00000140
      REAL ALP(1)                                                       00000150
      INTEGER LSR(2,1)                                                  00000160
C--------------------------------------------------------------------   00000170
C                                                                       00000180
      DO 220 M=1,LM                                                     00000190
      NA=LSR(2,M)-1                                                     00000200
      NL=LSR(1,M)                                                       00000210
      NR=LSR(1,M+1)-1                                                   00000220
      FCR=0.                                                            00000230
      FCI=0.                                                            00000240
      DO 210 N=NL,NR                                                    00000250
      NA=NA+1                                                           00000260
      XALP=ALP(NA)                                                      00000270
      XRSC= REAL(SC(N))                                                 00000280
      XISC=AIMAG(SC(N))                                                 00000290
      FCR=FCR+XALP*XRSC                                                 00000300
210   FCI=FCI+XALP*XISC                                                 00000310
C                                                                       00000320
      SFCR=FCR                                                          00000330
      SFCI=FCI                                                          00000340
220   CFC(M)=CMPLX(SFCR,SFCI)                                           00000350
C                                                                       00000360
      RETURN                                                            00000370
      END                                                               00000380
      SUBROUTINE STAGG (GG,ILG1,ILAT,ILEV,SL, SC,LSR,LM,LA,             00000010
     1                       ALP,EPSI,WRKS,WRKL)                        00000020
C                                                                       00000030
C     * JUL 31/79 - J.D.HENDERSON                                       00000040
C     * PRODUCES GLOBAL GAUSSIAN GRID SET GG(ILG1,ILAT,ILEV)            00000050
C     * FROM GLOBAL SPECTRAL COEFF SET IN SC(LA,ILEV).                  00000060
C     * LONGITUDE IS ZERO AT THE LEFT AND POSITIVE TO THE RIGHT.        00000070
C     * LSR CONTAINS ROW LENGTH INFO FOR ALP,EPSI,SC.                   00000080
C     * ALP = WORK FIELD FOR LEGENDRE POLYNOMIALS.                      00000090
C     * EPSI = PRECOMPUTED CONSTANTS.                                   00000100
C     * SL = SIN(LATITUDE) OF GAUSSIAN ROWS FROM S.POLE TO N.POLE.      00000110
C                                                                       00000120
      COMPLEX SC(LA,ILEV)                                               00000130
      REAL GG(ILG1,ILAT,ILEV)                                           00000140
      REAL SL(1)                                                        00000150
      REAL ALP(1),EPSI(1),WRKS(1)                                       00000160
      COMPLEX WRKL(1)                                                   00000170
      INTEGER LSR(2,1)                                                  00000180
C--------------------------------------------------------------------   00000190
      ILG=ILG1-1                                                        00000200
      ILGH=ILG/2                                                        00000210
      MAXF=LM-1                                                         00000220
C                                                                       00000230
      DO 210 J=1,ILAT                                                   00000240
      CALL ALPST (ALP,LSR ,LM,SL(J),EPSI)                               00000250
C                                                                       00000260
      DO 205 L=1,ILEV                                                   00000270
      CALL STAF (WRKL,SC(1,L),LSR,LM,ALP)                               00000280
      CALL FFGFW (GG(1,J,L),ILG1,WRKL,ILGH,MAXF,ILG,WRKS,1)             00000290
  205 GG(ILG1,J,L)=GG(1,J,L)                                            00000300
C                                                                       00000310
  210 CONTINUE                                                          00000320
      RETURN                                                            00000330
      END                                                               00000340
      SUBROUTINE STDXGG(GG,ILG1,ILAT,SL,SC,SD,LSR,LM                    00000010
     1 ,ALP,EPSI,WRKS,WRKL)                                             00000020
C                                                                       00000030
C     * 9/91 J. SHENG                                                           
C     * PRODUCE GLOBAL GAUSSIAN GRID SET D GG(ILG1,ILAT)/DX             00000050
C     * FROM GLOBAL SPECTRAL COEFF SET OF GG IN SC(LA).                 00000060
C     * SD IS A WORKING SPACE FOR THE SPECTRAL COEFF OF DGG/DX.                 
C     * LONGITUDE IS ZERO AT THE LEFT AND POSITIVE TO THE RIGHT.        00000070
C     * LSR CONTAINS ROW LENGTH INFO FOR ALP, EPSI, SC AND SD.          00000080
C     * ALP = WORK FIELD FOR LEGENDRE POLYNOMIALS.                      00000090
C     * EPSI = PRECOMPUTED CONSTANTS.                                   00000100
C     * SL = SIN(LATITUDE) OF GAUSSIAN ROWS FROM S.POLE TO N.POLE.      00000110
C                                                                       00000120
      COMPLEX SC(1),SD(1),WRKL(1)                                       00000130
      REAL GG(ILG1,ILAT),SL(1),ALP(1),EPSI(1),WRKS(1)                   00000140
      INTEGER LSR(2,1)                                                  00000180
C                                                                               
      ILG=ILG1-1                                                                
      ILGH=ILG/2                                                                
      MAXF=LM-1                                                                 
C                                                                               
      DO 220 M=1,LM                                                     00000110
      AM=FLOAT(M-1)                                                     00000120
      KL=LSR(1,M)                                                       00000130
      KR=LSR(1,M+1)-1                                                   00000140
      DO 220 K=KL,KR                                                    00000170
  220 SD(K)=CMPLX(-AM*AIMAG(SC(K)),AM*REAL(SC(K)))                      00000210
C                                                                               
      DO 210 J=1,ILAT                                                   00000240
      CALL ALPST(ALP,LSR,LM,SL(J),EPSI)                                 00000250
      CALL STAF(WRKL,SD,LSR,LM,ALP)                                     00000280
      CALL FFGFW(GG(1,J),ILG1,WRKL,ILGH,MAXF,ILG,WRKS,1)                00000290
      GG(ILG1,J)=GG(1,J)                                                00000300
  210 CONTINUE                                                          00000320
      RETURN                                                            00000330
      END                                                               00000340
      SUBROUTINE STDYGG(GG,ILG1,ILAT,SL,SC,LSR,LM                       00000010
     1 ,ALP,DALP,EPSI,WRKS,WRKL)                                        00000020
C                                                                       00000030
C     * 9/91 J. SHENG                                                           
C     * PRODUCE GLOBAL GAUSSIAN GRID SET D GG(ILG1,ILAT)/DY             00000050
C     * FROM GLOBAL SPECTRAL COEFF SET OF GG IN SC(LA).                 00000060
C     * LONGITUDE IS ZERO AT THE LEFT AND POSITIVE TO THE RIGHT.        00000070
C     * LSR CONTAINS ROW LENGTH INFO FOR ALP, EPSI AND SC.              00000080
C     * DALP = WORK FIELD FOR DERIVATIVE OF LEGENDRE POLYNOMIALS.       00000090
C     * EPSI = PRECOMPUTED CONSTANTS.                                   00000100
C     * SL = SIN(LATITUDE) OF GAUSSIAN ROWS FROM S.POLE TO N.POLE.      00000110
C                                                                       00000120
      COMPLEX SC(1),WRKL(1)                                             00000130
      REAL GG(ILG1,ILAT),SL(1),ALP(1),DALP(1),EPSI(1),WRKS(1)           00000140
      INTEGER LSR(2,1)                                                  00000180
C                                                                               
      ILG=ILG1-1                                                        00000200
      ILGH=ILG/2                                                        00000210
      MAXF=LM-1                                                         00000220
C                                                                       00000230
      DO 210 J=1,ILAT                                                   00000240
      CALL ALPST(ALP,LSR,LM,SL(J),EPSI)                                 00000250
      CALL ALPDY(DALP,ALP,LSR,LM,EPSI)                                  00000250
      CALL STAF (WRKL,SC,LSR,LM,DALP)                                   00000280
      CALL FFGFW (GG(1,J),ILG1,WRKL,ILGH,MAXF,ILG,WRKS,1)               00000290
      GG(ILG1,J)=GG(1,J)                                                00000300
  210 CONTINUE                                                          00000320
      RETURN                                                            00000330
      END                                                               00000340
      SUBROUTINE TRIGL(ILATH,SR,WR,CR,RADR,WOSQ)                        00001390
C                                                                       00001400
C     * JAN 19/78 - J.D.HENDERSON                                       00001410
C     * THE ARGUMENT LIST IS THE SAME AS FOR GAUSSG.                    00001420
C     * GAUSSG FILLS ONLY THE N HEM ORDERED N TO S.                     00001430
C     * THIS ROUTINE MAKES THE ARRAYS GLOBAL AND ORDERED FROM S TO N.   00001440
C     *      SR=SIN(LAT),  CR=COS(LAT),  RADR=LATITUDE IN RADIANS.      00001450
C     *      WR = GAUSSIAN WEIGHTS,  WOSQ = WR/(SR**2).                 00001460
C                                                                       00001470
      REAL SR(1),WR(1),CR(1),RADR(1),WOSQ(1)                            00001480
C--------------------------------------------------------------------   00001490
C     * CR,WR,WOSQ ARE SYMMETRIC ABOUT THE EQUATOR.                     00001500
C     * SR AND RAD ARE ANTISYMMETRIC.                                   00001510
C                                                                       00001520
      PIH=3.14159265/2.                                                 00001530
      ILAT=ILATH*2                                                      00001540
      DO 150 J=1,ILATH                                                  00001550
      K=ILAT+1-J                                                        00001560
      CR(K)=CR(J)                                                       00001570
      WR(K)=WR(J)                                                       00001580
      WOSQ(K)=WOSQ(J)                                                   00001590
      SR(K)=SR(J)                                                       00001600
      SR(J)=-SR(J)                                                      00001610
      RADR(K)=PIH-RADR(J)                                               00001620
      RADR(J)=-RADR(K)                                                  00001630
  150 CONTINUE                                                          00001640
C                                                                       00001650
      RETURN                                                            00001660
      END                                                               00001670
