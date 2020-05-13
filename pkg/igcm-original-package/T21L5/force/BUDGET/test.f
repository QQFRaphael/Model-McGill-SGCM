      PROGRAM SPECAN

C*****channel 13 - input  (grid OR spectral)
C*****channel  9 - output (spectral OR grid)

C     ATMOSPHERIC MODELLING GROUP      UNIVERSITY OF READING               MLTRI.5     
C                                                                          PARAM1.2     
C     Determines model resolution                                          PARAM1.3     
C                                                                          PARAM1.4     
      PARAMETER(NN=21,MM=21,NHEM=2,NL=5,MOCT=1,MG=128,JG=32,NWJ2=121        G100.1     
     P         ,NCRAY=64,JGL=JG)                                           G100.2     
C                                                                          PARAM1.7     
C                                                                          PARAM2.2     
C     Sets basic constants, especially those needed for array dimensions   PARAM2.3     
C                                                                          PARAM2.4     
      PARAMETER(MH=2,PI=3.14159265359,PI2=2.0*PI                           PARAM2.5     
     +,NNP=NN+1,MGPP=MG+2,JGP=JG+1,JGG=JG*NHEM,JGGP=JGG+1,MJP=NWJ2+NWJ2    PARAM2.6     
     +,NLM=NL-1,NLP=NL+1,NLPP=NL+2,NLA=NL+3,NLB=NL+4,NL2=NL*NL             PARAM2.7     
     +,IDA=(MG+MG+MG)/2+1,IDB=NWJ2*NL,IDC=IDB+IDB,IDD=MGPP*NL              PARAM2.8     
     +,IDE=NL2*NN,IDF=NCRAY*(MG+1),IDG=JG*NL,IDH=JG*MG                     PARAM2.9     
     +,IDI=NNP/2,IDJ=IDI*IDI,IDK=NL*IDI,IDL=MGPP/2,IDM=NNP/2,IDN=IDM*NL    PARAM2.10    
     +,NWW=1+(MM-1)/MOCT)                                                  PARAM2.11    
      PARAMETER(IGA=NWJ2*NHEM,IGB=IDB*NHEM,IGC=MGPP*NHEM,IGD=IDD*NHEM      PARAM2.12    
     +,IGG=IDG*NHEM,IGL=IDL*NHEM,IGM=IDM*NHEM,IGN=IDN*NHEM                 PARAM2.13    
     +,IGO=IGA+IGA,IGP=IGB+IGB,NFTWG=5*NL+3,NFTGW=6*NL+2)                  PARAM2.14    
C                                                                          PARAM2.15    
      PARAMETER(IDDAF=NFTGW*IGC,IDDAG=NFTWG*IGC)                           MLTRI.10    
C                                                                          BLANK.2     
C     Basic planetary parameters for run plus information about            BLANK.3     
C     vertical grid structure                                              BLANK.4     
C                                                                          BLANK.5     
      COMMON        SQ(NNP),RSQ(NNP),SIGMAH(NLM),SIGMA(NL)                 BLANK.6     
     +              ,T01S2(NLM),T0(NL),ALPHA(NL),DSIGMA(NL),RDSIG(NL)      BLANK.7     
     +              ,TKP(NL),C(NL2),SQH(NNP)                               BLANK.8     
     +              ,MF,MFP,JZF,NF,NFP                                     BLANK.9     
     +              ,AKAP,GA,GASCON,RADEA,WW,PFAC,EZ,AIOCT                 BLANK.10    
     +              ,LRSTRT,LSHORT,LTVEC,LSTRETCH                          BLANK.11    
     +              ,LBALAN,LRESTIJ                                        BLANK.12    
     +              ,LNOISE                                                SC970203.1     
     +              ,LGPDAMP,LFCE,LFED,LFAN,LLIN,LMODE,LTRAIN              NICK.1     
      COMPLEX EZ,AIOCT                                                     BLANK.13    
      LOGICAL LRSTRT,LSHORT,LTVEC,LSTRETCH,LBALAN,LRESTIJ                  BLANK.14    
     +       ,LNOISE                                                       SC970203.2     
     +       ,LGPDAMP,LFCE,LFED,LFAN,LLIN,LMODE,LTRAIN                     NICK.2     
C                                                                          BLANK.15    
C                                                                          SPECTR.2     
C     Array ordering in SPECTR must correspond to that in GRIDP.           SPECTR.3     
C                                                                          SPECTR.4     
      COMMON/SPECTR/Z(IGB),D(IGB),T(IGB),SP(IGA)
      COMPLEX Z,D,T,SP
C                                                                          SPECTR.9     
C                                                                          GRIDP.2     
C     Array ordering in GRIDP must correspond to that in SPECTR.           GRIDP.3     
C     Real arrays: multi-level arrays are 1-dimensional.                   GRIDP.4     
C                                                                          GRIDP.5     
      COMMON/GRIDP/ CHIG(IGC,NL),SFG(IGC,NL),ZG(IGC,NL),DG(IGC,NL)
     *              ,TG(IGC,NL),SPG(IGC),DGT(IGC,NL),ZGT(IGC,NL)
C                                                                          LEGAU.2     
      COMMON/BATS/  BM1(IDE),AK(NNP),AQ(NL2),G(NL2),TAU(NL2)               BATS.6     
     +              ,KOUNT,KITS,KTOTAL,KRUN,BEGDAY,ITSPD,PNU21             BATS.7     
     +              ,DELT,DELT2,CV,CG,CT,PNU,PNU2                          BATS.8     

C     Legendre polynomials and information about gaussian latitudes        LEGAU.3     
C                                                                          LEGAU.4     
      COMMON/LEGAU/ ALPJ(MJP),DALPJ(MJP)                                   LEGAU.5     
     +              ,ALP(NWJ2,2,JGL),DALP(NWJ2,2,JGL)                      LEGAU.6     
     +              ,RLP(NWJ2,2,JGL),RDLP(NWJ2,2,JGL)                      LEGAU.7     
     +              ,SI(JGG),CS(JGG),SISQ(JGG),CSSQ(JGG),SECSQ(JGG)        LEGAU.8     
     +              ,ALAT(JGG),GWT(JGG),AW(JGG),JH,JL,JINC                 LEGAU.9     
C                                                                          COMFFT.2     
C     Constants and arrays needed for the fast Fourier transforms          COMFFT.3     
C                                                                          COMFFT.4     
      COMMON/COMFFT/TRIG(IDA),WORK(IDF),IFAX(10)
C                                                                          COMFFT.7     
C                                                                          POLYNO.2     
C     Polynomial used to aid vectorization of Legendre transforms          POLYNO.3     
C                                                                          POLYNO.4     
      COMMON/POLYNO/POLY(NWJ2,2,4),CMPA(IGL)                               POLYNO.5     
      COMPLEX CMPA                                                         POLYNO.6     
C                                                                          BALAN.8     
      LOGICAL LWTG,LGTW
      REAL TTG(MG,JGG,NL),SPSPG(MG,JGG)
      REAL ZZG(MG,JGG,NL),DDG(MG,JGG,NL)
      DIMENSION DAG(3*IGD+IGC)
      EQUIVALENCE (DAG(1),ZG(1,1))

	WW=7.292E-5
Cjxj
c     LWTG=.TRUE.
c     LGTW=.FALSE.
      LWTG=.FALSE.
      LGTW=.TRUE.

      REWIND 9
      REWIND 13

      CALL INISET
      CALL INIGAU

C*****time loop starts
      DO 199 NREC=1,1
C*****preset arrays to zero.
	print*,NREC

      DO 31 I=1,IGA
         SP(I)=0.0
 31   CONTINUE
      DO 32 I=1,IGB
         Z(I)=0.0
         D(I)=0.0
         T(I)=0.0
 32   CONTINUE

      DO 33 L=1,NL
      DO 33 J=1,JGG
      DO 33 I=1,MG
         ZZG(I,J,L)=0.
         DDG(I,J,L)=0.
         TTG(I,J,L)=0.
         SPSPG(I,J)=0.
 33   CONTINUE

C-------------------------------------------------------------
      IF (LWTG) THEN
C*****read in spectral data

      READ(13)Z,D,T,SP

CC----multiply by time interval
        DO  I=1,IGB
         Z(I)=Z(I)*DELT2
         D(I)=D(I)*DELT2
         T(I)=T(I)*DELT2
        END DO

        DO  I=1,IGA
         SP(I)=SP(I)*DELT2
        END DO

      END IF
C-------------------------------------------------------------

C-------------------------------------------------------------
      IF (LGTW) THEN
C*****read in grid point data

      READ(13)ZZG,DDG,TTG,SPSPG

C*****nondimensionalize, multiply U and V by cos lat
C*****and set T0 for temperature
      DO 34 L=1,NL
      DO 34 J=1,JGG
      DO 34 I=1,MG
      ZZG(I,J,L)=ZZG(I,J,L)/WW
      DDG(I,J,L)=DDG(I,J,L)/WW
 34   TTG(I,J,L)=TTG(I,J,L)/CT

      END IF
C-------------------------------------------------------------

C*****do spectral analysis

      NFT=3*NL+1
      NTR=NFT*NHEM
      NT=(NTR-1)/NCRAY
      NRST=NTR-NCRAY*NT

      IF (JGL.EQ.1) REWIND 25
      JL=1

C     Main loop over latitudes

      DO 5 IH=1,JG
         JH=IH
         IF(JGL.EQ.1) READ(25) ALP,DALP,RLP,RDLP

C-------------------------------------------------------------
         IF (LWTG) THEN
C        Go from spectral space to grid point space using
C        inverse Legendre and Fourier transforms

         CALL LTI
 
         DO 10 I=1,NT
            CALL FFT991(DAG(1+(I-1)*NCRAY*MGPP),WORK,TRIG,IFAX,1
     &                 ,MGPP,MG,NCRAY,1)
 10      CONTINUE
         CALL FFT991(DAG(1+ NT*NCRAY*MGPP),WORK,TRIG,IFAX,1
     &              ,MGPP,MG,NRST,1)

         CALL SHUFFLE(JH,ZG,ZZG,NL)
         CALL SHUFFLE(JH,DG,DDG,NL)
         CALL SHUFFLE(JH,TG,TTG,NL)
         CALL SHUFFLE(JH,SPG,SPSPG,1)

         END IF

C-------------------------------------------------------------

C-------------------------------------------------------------
         IF (LGTW) THEN

C        Go from grid point space to spectral space using
C        direct Legendre and Fourier transforms

         CALL SLICE(JH,ZG,ZZG,NL)
         CALL SLICE(JH,DG,DDG,NL)
         CALL SLICE(JH,TG,TTG,NL)
         CALL SLICE(JH,SPG,SPSPG,1)

         DO 20 I=1,NT
            CALL FFT991(DAG(1+(I-1)*NCRAY*MGPP),WORK,TRIG,IFAX,1
     &                 ,MGPP,MG,NCRAY,-1)
 20      CONTINUE
         CALL FFT991(DAG(1+ NT*NCRAY*MGPP),WORK,TRIG,IFAX,1
     &              ,MGPP,MG,NRST,-1)

         CALL LTD

         END IF
C-------------------------------------------------------------

         JL=JL+JINC

 5    CONTINUE

C-------------------------------------------------------------
      IF (LWTG) THEN

C*****redimensionalize, divide U and V by cos lat
C*****and set temperature in centigrade
      DO 35 L=1,NL
      DO 35 J=1,JGG
      DO 35 I=1,MG
	ZZG(I,J,L)=ZZG(I,J,L)*WW
	DDG(I,J,L)=DDG(I,J,L)*WW
35	TTG(I,J,L)=TTG(I,J,L)*CT

C*****output
      WRITE(9)ZZG,DDG,TTG,SPSPG
c     WRITE(9)SPSPG

      END IF
C-------------------------------------------------------------

C-------------------------------------------------------------
      IF (LGTW) THEN
C*****add planetary vorticity back on
CC----devide time interval to make it unit/s
        DO  I=1,IGB
         Z(I)=Z(I)/DELT2
         D(I)=D(I)/DELT2
         T(I)=T(I)/DELT2
        END DO

        DO  I=1,IGA
         SP(I)=SP(I)/DELT2
        END DO

C*****output

      RKOUNT=REAL(NREC-1)
	DAY=0.	
      RNTAPE=100.
      WRITE(9)Z,D,T,SP

      END IF
C-------------------------------------------------------------

 199  CONTINUE

      STOP
      END


      SUBROUTINE INISET                                                    INISET.2     
C                                                                          INISET.3     
C     Sets up various variables and arrays. Sets NAMELIST variables        INISET.4     
C     to their default settings, then reads NAMELIST                       INISET.5     
C                                                                          INISET.6     
C                                                                          PARAM1.2     
C     Determines model resolution                                          PARAM1.3     
C                                                                          PARAM1.4     
      PARAMETER(NN=21,MM=21,NHEM=2,NL=5,MOCT=1,MG=128,JG=32,NWJ2=121        G100.1     
     P         ,NCRAY=64,JGL=JG)                                           G100.2     
C                                                                          PARAM1.7     
C                                                                          PARAM2.2     
C     Sets basic constants, especially those needed for array dimensions   PARAM2.3     
C                                                                          PARAM2.4     
      PARAMETER(MH=2,PI=3.14159265359,PI2=2.0*PI                           PARAM2.5     
     +,NNP=NN+1,MGPP=MG+2,JGP=JG+1,JGG=JG*NHEM,JGGP=JGG+1,MJP=NWJ2+NWJ2    PARAM2.6     
     +,NLM=NL-1,NLP=NL+1,NLPP=NL+2,NLA=NL+3,NLB=NL+4,NL2=NL*NL             PARAM2.7     
     +,IDA=(MG+MG+MG)/2+1,IDB=NWJ2*NL,IDC=IDB+IDB,IDD=MGPP*NL              PARAM2.8     
     +,IDE=NL2*NN,IDF=NCRAY*(MG+1),IDG=JG*NL,IDH=JG*MG                     PARAM2.9     
     +,IDI=NNP/2,IDJ=IDI*IDI,IDK=NL*IDI,IDL=MGPP/2,IDM=NNP/2,IDN=IDM*NL    PARAM2.10    
     +,NWW=1+(MM-1)/MOCT)                                                  PARAM2.11    
      PARAMETER(IGA=NWJ2*NHEM,IGB=IDB*NHEM,IGC=MGPP*NHEM,IGD=IDD*NHEM      PARAM2.12    
     +,IGG=IDG*NHEM,IGL=IDL*NHEM,IGM=IDM*NHEM,IGN=IDN*NHEM                 PARAM2.13    
     +,IGO=IGA+IGA,IGP=IGB+IGB,NFTWG=5*NL+3,NFTGW=6*NL+2)                  PARAM2.14    
C                                                                          PARAM2.15    
C                                                                          BLANK.2     
C     Basic planetary parameters for run plus information about            BLANK.3     
C     vertical grid structure                                              BLANK.4     
C                                                                          BLANK.5     
      COMMON        SQ(NNP),RSQ(NNP),SIGMAH(NLM),SIGMA(NL)                 BLANK.6     
     +              ,T01S2(NLM),T0(NL),ALPHA(NL),DSIGMA(NL),RDSIG(NL)      BLANK.7     
     +              ,TKP(NL),C(NL2),SQH(NNP)                               BLANK.8     
     +              ,MF,MFP,JZF,NF,NFP                                     BLANK.9     
     +              ,AKAP,GA,GASCON,RADEA,WW,PFAC,EZ,AIOCT                 BLANK.10    
     +              ,LRSTRT,LSHORT,LTVEC,LSTRETCH                          BLANK.11    
     +              ,LBALAN,LRESTIJ                                        BLANK.12    
     +              ,LNOISE                                                SC970203.1     
     +              ,LGPDAMP,LFCE,LFED,LFAN,LLIN,LMODE,LTRAIN              NICK.1     
      COMPLEX EZ,AIOCT                                                     BLANK.13    
      LOGICAL LRSTRT,LSHORT,LTVEC,LSTRETCH,LBALAN,LRESTIJ                  BLANK.14    
     +       ,LNOISE                                                       SC970203.2     
     +       ,LGPDAMP,LFCE,LFED,LFAN,LLIN,LMODE,LTRAIN                     NICK.2     
C                                                                          BLANK.15    
C                                                                          SPECTR.2     
C     Array ordering in SPECTR must correspond to that in GRIDP.           SPECTR.3     
C                                                                          SPECTR.4     
      COMMON/SPECTR/Z(IGB),D(IGB),T(IGB),SP(IGA)
      COMPLEX Z,D,T,SP
C                                                                          SPECTR.9     
C                                                                          GRIDP.2     
C     Array ordering in GRIDP must correspond to that in SPECTR.           GRIDP.3     
C     Real arrays: multi-level arrays are 1-dimensional.                   GRIDP.4     
C                                                                          GRIDP.5     
      COMMON/GRIDP/ CHIG(IGD),SFG(IGD),ZG(IGD),DG(IGD)                     GRIDP.6     
     *              ,TG(IGD),SPG(IGC),DGT(IGD),ZGT(IGD)
C                                                                          COMFFT.2     
      COMMON/BATS/  BM1(IDE),AK(NNP),AQ(NL2),G(NL2),TAU(NL2)               BATS.6     
     +              ,KOUNT,KITS,KTOTAL,KRUN,BEGDAY,ITSPD,PNU21             BATS.7     
     +              ,DELT,DELT2,CV,CG,CT,PNU,PNU2                          BATS.8     

C     Constants and arrays needed for the fast Fourier transforms          COMFFT.3     
C                                                                          COMFFT.4     
      COMMON/COMFFT/TRIG(IDA),WORK(IDF),IFAX(10)
C                                                                          COMFFT.7     
C                                                                          POLYNO.2     
C     Polynomial used to aid vectorization of Legendre transforms          POLYNO.3     
C                                                                          POLYNO.4     
      COMMON/POLYNO/POLY(NWJ2,2,4),CMPA(IGL)                               POLYNO.5     
      COMPLEX CMPA                                                         POLYNO.6     
C                                                                          POLYNO.7     
C                                                                          INISET.53    
C     Set default values and override as desired through NAMELIST input    INISET.54    
C                                                                          INISET.55    
      GA=9.81                                                              INISET.56    
      GASCON=287.0                                                         INISET.57    
      RADEA=6371000.0                                                      INISET.58    
      AKAP=0.286                                                           INISET.59    
      WW=7.292E-5                                                          INISET.60    

      DELT2=4.*PI/48.
C                                                                          INISET.61    
C     Set resolution dependent quantities                                  INISET.110   
C                                                                          INISET.111   
      AMH=MH                                                               INISET.112   
      MF=MM-1                                                              INISET.113   
      MFP=MF+1                                                             INISET.114   
      MFPP=MFP+1                                                           INISET.115   
      NF=NN-1                                                              INISET.116   
      NFP=NF+1                                                             INISET.117   
      NFPP=NFP+1                                                           INISET.118   
      AIOCT=(0.,1.)*MOCT                                                   INISET.119   
C                                                                          INISET.120   
C     Set various spectral limits and coefficients which                   INISET.121   
C     depend on wavenumber                                                 INISET.122   
C                                                                          INISET.123   
      NW=1+MF/MOCT                                                         INISET.124   
      NWP=NW+1                                                             INISET.125   
      MJPP=MJP+NW                                                          INISET.126   
      MGP=MG+1                                                             INISET.127   
      MG2=MG/2                                                             INISET.128   
      RMG=1./MG                                                            INISET.129   
      JZF=MGPP-NW-NW                                                       INISET.130   
      DO 4 NP=1,NFPP                                                       INISET.131   
         SQ(NP)=NP*(NP-1)                                                  INISET.132   
         SQH(NP)=0.5*SQ(NP)                                                INISET.133   
         IF (NP.GT.1) THEN                                                 INISET.134   
            RSQ(NP)=1./SQ(NP)                                              INISET.135   
         ELSE                                                              INISET.136   
            RSQ(1)=0.                                                      INISET.137   
         ENDIF                                                             INISET.138   
    4 CONTINUE                                                             INISET.139   
C                                                                          INISET.140  
C                                                                          INISET.191   
C     Set dimensionalising factors                                         INISET.192   
C                                                                          INISET.193   
      EZ=1.0/SQRT(.375)                                                    INISET.194   
      CV=RADEA*WW                                                          INISET.195   
      CG=CV*CV                                                             INISET.196   
      CT=CG/GASCON                                                         INISET.197   
      PFAC=0.5*CV*CV*1.0E5/9.81                                            INISET.198   
      SQR2=SQRT(2.0)                                                       INISET.199   
      RSQR2=1.0/SQR2                                                       INISET.200   
      EAM1=SQR2/3.                                                         INISET.201   
      EAM2=SQRT(2./45.)                                                    INISET.202   
C                                                                          INISET.203   
C                                                                          INISET.218   
C     Calculate auxiliary values required by FFT991                        INISET.219   
C                                                                          INISET.220   
      CALL FAX(IFAX,MG,3)                                                  INISET.221   
      CALL FFTRIG(TRIG,MG,3)                                               INISET.222   
C                                                                          INISET.238   
C     Set up CMPA array to calculate x-derivative of half transforms       INISET.239   
C                                                                          INISET.240   
      DO 40 I=1,IGL                                                        INISET.241   
         CMPA(I)=0.0                                                       INISET.242   
   40 CONTINUE                                                             INISET.243   
      NROW=0                                                               INISET.244   
      DO 41 MP=1,MFP,MOCT                                                  INISET.245   
         NROW=NROW+1                                                       INISET.246   
         CMPA(NROW)=CMPLX(0.,REAL(MP-1))                                   INISET.247   
   41 CONTINUE                                                             INISET.248   
      IF(NHEM.EQ.2)THEN                                                    INISET.249   
         NROW=0                                                            INISET.250   
CDIR$    IVDEP                                                             INISET.251   
         DO 42 MP=1,MFP,MOCT                                               INISET.252   
            NROW=NROW+1                                                    INISET.253   
            CMPA(NROW+IDL)=CMPA(NROW)                                      INISET.254   
   42    CONTINUE                                                          INISET.255   
      END IF                                                               INISET.256   
C                                                                          INISET.257   
      END                                                                  INISET.258   
      SUBROUTINE INIGAU                                                    INIGAU.2     
C                                                                          INIGAU.3     
C     This subroutine calculates gaussian weights and latitudes            INIGAU.4     
C                                                                          INIGAU.5     
C                                                                          PARAM1.2     
C     Determines model resolution                                          PARAM1.3     
C                                                                          PARAM1.4     
      PARAMETER(NN=21,MM=21,NHEM=2,NL=5,MOCT=1,MG=128,JG=32,NWJ2=121        G100.1     
     P         ,NCRAY=64,JGL=JG)                                           G100.2     
C                                                                          PARAM1.7     
C                                                                          PARAM2.2     
C     Sets basic constants, especially those needed for array dimensions   PARAM2.3     
C                                                                          PARAM2.4     
      PARAMETER(MH=2,PI=3.14159265359,PI2=2.0*PI                           PARAM2.5     
     +,NNP=NN+1,MGPP=MG+2,JGP=JG+1,JGG=JG*NHEM,JGGP=JGG+1,MJP=NWJ2+NWJ2    PARAM2.6     
     +,NLM=NL-1,NLP=NL+1,NLPP=NL+2,NLA=NL+3,NLB=NL+4,NL2=NL*NL             PARAM2.7     
     +,IDA=(MG+MG+MG)/2+1,IDB=NWJ2*NL,IDC=IDB+IDB,IDD=MGPP*NL              PARAM2.8     
     +,IDE=NL2*NN,IDF=NCRAY*(MG+1),IDG=JG*NL,IDH=JG*MG                     PARAM2.9     
     +,IDI=NNP/2,IDJ=IDI*IDI,IDK=NL*IDI,IDL=MGPP/2,IDM=NNP/2,IDN=IDM*NL    PARAM2.10    
     +,NWW=1+(MM-1)/MOCT)                                                  PARAM2.11    
      PARAMETER(IGA=NWJ2*NHEM,IGB=IDB*NHEM,IGC=MGPP*NHEM,IGD=IDD*NHEM      PARAM2.12    
     +,IGG=IDG*NHEM,IGL=IDL*NHEM,IGM=IDM*NHEM,IGN=IDN*NHEM                 PARAM2.13    
     +,IGO=IGA+IGA,IGP=IGB+IGB,NFTWG=5*NL+3,NFTGW=6*NL+2)                  PARAM2.14    
C                                                                          PARAM2.15    
C                                                                          BLANK.2     
C     Basic planetary parameters for run plus information about            BLANK.3     
C     vertical grid structure                                              BLANK.4     
C                                                                          BLANK.5     
      COMMON        SQ(NNP),RSQ(NNP),SIGMAH(NLM),SIGMA(NL)                 BLANK.6     
     +              ,T01S2(NLM),T0(NL),ALPHA(NL),DSIGMA(NL),RDSIG(NL)      BLANK.7     
     +              ,TKP(NL),C(NL2),SQH(NNP)                               BLANK.8     
     +              ,MF,MFP,JZF,NF,NFP                                     BLANK.9     
     +              ,AKAP,GA,GASCON,RADEA,WW,PFAC,EZ,AIOCT                 BLANK.10    
     +              ,LRSTRT,LSHORT,LTVEC,LSTRETCH                          BLANK.11    
     +              ,LBALAN,LRESTIJ                                        BLANK.12    
     +              ,LNOISE                                                SC970203.1     
     +              ,LGPDAMP,LFCE,LFED,LFAN,LLIN,LMODE,LTRAIN              NICK.1     
      COMPLEX EZ,AIOCT                                                     BLANK.13    
      LOGICAL LRSTRT,LSHORT,LTVEC,LSTRETCH,LBALAN,LRESTIJ                  BLANK.14    
     +       ,LNOISE                                                       SC970203.2     
     +       ,LGPDAMP,LFCE,LFED,LFAN,LLIN,LMODE,LTRAIN                     NICK.2     
C                                                                          BLANK.15    
C                                                                          BATS.2     
C     Legendre polynomials and information about gaussian latitudes        LEGAU.3     
C                                                                          LEGAU.4     
      COMMON/LEGAU/ ALPJ(MJP),DALPJ(MJP)                                   LEGAU.5     
     +              ,ALP(NWJ2,2,JGL),DALP(NWJ2,2,JGL)                      LEGAU.6     
     +              ,RLP(NWJ2,2,JGL),RDLP(NWJ2,2,JGL)                      LEGAU.7     
     +              ,SI(JGG),CS(JGG),SISQ(JGG),CSSQ(JGG),SECSQ(JGG)        LEGAU.8     
     +              ,ALAT(JGG),GWT(JGG),AW(JGG),JH,JL,JINC                 LEGAU.9     
C                                                                          LEGAU.10    
C                                                                          INIGAU.11    
  230 FORMAT(/' *** ABORT *** PARAMETER JGL IS DIFFERENT FROM 1 OR JG')    INIGAU.12    
  202 FORMAT(' GAUSSIAN LATITUDES')                                        INIGAU.13    
  201 FORMAT(10F7.2)                                                       INIGAU.14    
C                                                                          INIGAU.15    
      IF(JGL.NE.1.AND.JGL.NE.JG) THEN                                      INIGAU.16    
         WRITE(2,230)                                                      INIGAU.17    
         STOP                                                              INIGAU.18    
      ENDIF                                                                INIGAU.19    
      IF(JGL.EQ.1) JINC=0                                                  INIGAU.20    
      IF(JGL.EQ.JG)JINC=1                                                  INIGAU.21    
      JL=1                                                                 INIGAU.22    
      DO 8 J=1,JG                                                          INIGAU.23    
         JH=J                                                              INIGAU.24    
         CALL GWTLT(SI(J),WEIGHT,J,JG)                                     INIGAU.25    
         SISQ(J)=SI(J)*SI(J)                                               INIGAU.26    
         CSSQ(J)=1.-SISQ(J)                                                INIGAU.27    
         SECSQ(J)=1./CSSQ(J)                                               INIGAU.28    
         CS(J)=SQRT(CSSQ(J))                                               INIGAU.29    
         ALAT(J)=ATAN(SI(J)/CS(J))*180.0/PI                                INIGAU.30    
         GWT(J)=WEIGHT/REAL(NHEM)                                          INIGAU.31    
         AW(J)=WEIGHT*2.0*SECSQ(J)                                         INIGAU.32    
C                                                                          INIGAU.33    
C        Compute Legendre functions at the current latitude.               INIGAU.34    
C                                                                          INIGAU.35    
         CALL LGNDRE(NN,MM,MOCT,ALPJ,DALPJ,MJP,1,SI(JH),CS(JH))            INIGAU.36    
C                                                                          INIGAU.37    
C        Reorder Legendre functions, separating even/odd functions.        INIGAU.38    
C                                                                          INIGAU.39    
         DO 58 K=1,2                                                       INIGAU.40    
            I=0                                                            INIGAU.41    
            II=K-2                                                         INIGAU.42    
            DO 57 M=0,MM-1,MOCT                                            INIGAU.43    
               DO 56 N=M,NN-1,2                                            INIGAU.44    
                  I=I+1                                                    INIGAU.45    
                  II=II+2                                                  INIGAU.46    
                  ALP(I,K,JL)=ALPJ(II)                                     INIGAU.47    
                  DALP(I,K,JL)=DALPJ(II)                                   INIGAU.48    
                  RLP(I,K,JL)=-RSQ(N+K)*ALP(I,K,JL)                        INIGAU.49    
                  RDLP(I,K,JL)=-RSQ(N+K)*DALP(I,K,JL)                      INIGAU.50    
   56          CONTINUE                                                    INIGAU.51    
   57       CONTINUE                                                       INIGAU.52    
   58    CONTINUE                                                          INIGAU.53    
C                                                                          INIGAU.54    
         IF(JGL.EQ.1) WRITE(25) ALP,DALP,RLP,RDLP                          INIGAU.55    
C                                                                          INIGAU.56    
         JL=JL+JINC                                                        INIGAU.57    
    8 CONTINUE                                                             INIGAU.58    
C                                                                          INIGAU.59    
      IF (NHEM.EQ.2) THEN                                                  INIGAU.60    
CDIR$    IVDEP                                                             INIGAU.61    
         DO 59 J=1,JG                                                      INIGAU.62    
            SI(JGGP-J)=-SI(J)                                              INIGAU.63    
            CS(JGGP-J)=CS(J)                                               INIGAU.64    
            SISQ(JGGP-J)=SISQ(J)                                           INIGAU.65    
            CSSQ(JGGP-J)=CSSQ(J)                                           INIGAU.66    
            SECSQ(JGGP-J)=SECSQ(J)                                         INIGAU.67    
            ALAT(JGGP-J)=-ALAT(J)                                          INIGAU.68    
            GWT(JGGP-J)=GWT(J)                                             INIGAU.69    
            AW(JGGP-J)=AW(J)                                               INIGAU.70    
   59    CONTINUE                                                          INIGAU.71    
      ENDIF                                                                INIGAU.72    
C                                                                          INIGAU.73    
C     Output the Gaussian latitudes                                        INIGAU.74    
C                                                                          INIGAU.75    
      WRITE(2,202)                                                         INIGAU.76    
      WRITE(2,201)(ALAT(J),J=1,JGG)                                        INIGAU.77    
C                                                                          INIGAU.78    
      END                                                                  INIGAU.79    
      SUBROUTINE HANAL(GV,GVW,SV,NLS,ITYPE)                                HANAL.2     
C                                                                          HANAL.3     
C     Performs a direct Legendre transform for a (set of) field(s)         HANAL.4     
C     having a total of NLS levels, from Fourier to spectral space.        HANAL.5     
C                                                                          HANAL.6     
C     The following types of Legendre function and thence types of         HANAL.7     
C     transform may be used:                                               HANAL.8     
C        ITYPE=1,2  :  ALP   :  ALPN(,,,1)   :  normal transform.          HANAL.9     
C        ITYPE=3,4  :  DALP  :  ALPN(,,,2)   :  y-derivative.              HANAL.10    
C        ITYPE=5,6  :  RLP   :  ALPN(,,,3)   :  del(-2).                   HANAL.11    
C        ITYPE=7,8  :  RDLP  :  ALPN(,,,4)   :  y-derivative of del(-2).   HANAL.12    
C     An even/odd value of ITYPE denotes a spectral field of even/odd      HANAL.13    
C     symmetry.                                                            HANAL.14    
C                                                                          HANAL.15    
C     A Fourier work array GVW prevents corruption of the input Fourier    HANAL.16    
C     array GV that would otherwise occur in global runs.                  HANAL.17    
C                                                                          HANAL.18    
C     NOTE: The y-derivative transforms use integration by parts and       HANAL.19    
C           are valid only if the input field has zero zonal mean at       HANAL.20    
C           both poles.                                                    HANAL.21    
C                                                                          HANAL.22    
C     Version for RSGUP3.                     Mike Blackburn,  05.01.95.   HANAL.23    
C     4R3 : try reversed loop ordering for global code.                    HANAL.24    
C     Work array now a dummy argument (ANSI). Mike Blackburn,  04.09.96.   HANAL.25    
C                                                                          HANAL.26    
C                                                                          PARAM1.2     
C     Determines model resolution                                          PARAM1.3     
C                                                                          PARAM1.4     
      PARAMETER(NN=21,MM=21,NHEM=2,NL=5,MOCT=1,MG=128,JG=32,NWJ2=121        G100.1     
     P         ,NCRAY=64,JGL=JG)                                           G100.2     
C                                                                          PARAM1.7     
C                                                                          PARAM2.2     
C     Sets basic constants, especially those needed for array dimensions   PARAM2.3     
C                                                                          PARAM2.4     
      PARAMETER(MH=2,PI=3.14159265359,PI2=2.0*PI                           PARAM2.5     
     +,NNP=NN+1,MGPP=MG+2,JGP=JG+1,JGG=JG*NHEM,JGGP=JGG+1,MJP=NWJ2+NWJ2    PARAM2.6     
     +,NLM=NL-1,NLP=NL+1,NLPP=NL+2,NLA=NL+3,NLB=NL+4,NL2=NL*NL             PARAM2.7     
     +,IDA=(MG+MG+MG)/2+1,IDB=NWJ2*NL,IDC=IDB+IDB,IDD=MGPP*NL              PARAM2.8     
     +,IDE=NL2*NN,IDF=NCRAY*(MG+1),IDG=JG*NL,IDH=JG*MG                     PARAM2.9     
     +,IDI=NNP/2,IDJ=IDI*IDI,IDK=NL*IDI,IDL=MGPP/2,IDM=NNP/2,IDN=IDM*NL    PARAM2.10    
     +,NWW=1+(MM-1)/MOCT)                                                  PARAM2.11    
      PARAMETER(IGA=NWJ2*NHEM,IGB=IDB*NHEM,IGC=MGPP*NHEM,IGD=IDD*NHEM      PARAM2.12    
     +,IGG=IDG*NHEM,IGL=IDL*NHEM,IGM=IDM*NHEM,IGN=IDN*NHEM                 PARAM2.13    
     +,IGO=IGA+IGA,IGP=IGB+IGB,NFTWG=5*NL+3,NFTGW=6*NL+2)                  PARAM2.14    
C                                                                          PARAM2.15    
C                                                                          BLANK.2     
C     Basic planetary parameters for run plus information about            BLANK.3     
C     vertical grid structure                                              BLANK.4     
C                                                                          BLANK.5     
      COMMON        SQ(NNP),RSQ(NNP),SIGMAH(NLM),SIGMA(NL)                 BLANK.6     
     +              ,T01S2(NLM),T0(NL),ALPHA(NL),DSIGMA(NL),RDSIG(NL)      BLANK.7     
     +              ,TKP(NL),C(NL2),SQH(NNP)                               BLANK.8     
     +              ,MF,MFP,JZF,NF,NFP                                     BLANK.9     
     +              ,AKAP,GA,GASCON,RADEA,WW,PFAC,EZ,AIOCT                 BLANK.10    
     +              ,LRSTRT,LSHORT,LTVEC,LSTRETCH                          BLANK.11    
     +              ,LBALAN,LRESTIJ                                        BLANK.12    
     +              ,LNOISE                                                SC970203.1     
     +              ,LGPDAMP,LFCE,LFED,LFAN,LLIN,LMODE,LTRAIN              NICK.1     
      COMPLEX EZ,AIOCT                                                     BLANK.13    
      LOGICAL LRSTRT,LSHORT,LTVEC,LSTRETCH,LBALAN,LRESTIJ                  BLANK.14    
     +       ,LNOISE                                                       SC970203.2     
     +       ,LGPDAMP,LFCE,LFED,LFAN,LLIN,LMODE,LTRAIN                     NICK.2     
C                                                                          BLANK.15    
C                                                                          LEGAU.2     
C     Legendre polynomials and information about gaussian latitudes        LEGAU.3     
C                                                                          LEGAU.4     
      COMMON/LEGAU/ ALPJ(MJP),DALPJ(MJP)                                   LEGAU.5     
     +              ,ALP(NWJ2,2,JGL),DALP(NWJ2,2,JGL)                      LEGAU.6     
     +              ,RLP(NWJ2,2,JGL),RDLP(NWJ2,2,JGL)                      LEGAU.7     
     +              ,SI(JGG),CS(JGG),SISQ(JGG),CSSQ(JGG),SECSQ(JGG)        LEGAU.8     
     +              ,ALAT(JGG),GWT(JGG),AW(JGG),JH,JL,JINC                 LEGAU.9     
C                                                                          LEGAU.10    
C                                                                          POLYNO.2     
C     Polynomial used to aid vectorization of Legendre transforms          POLYNO.3     
C                                                                          POLYNO.4     
      COMMON/POLYNO/POLY(NWJ2,2,4),CMPA(IGL)                               POLYNO.5     
      COMPLEX CMPA                                                         POLYNO.6     
C                                                                          POLYNO.7     
      COMPLEX     GV(IGL,NLS),GVW(IGL,NLS),SV(IGA,NLS)                     HANAL.32    
      REAL        ALPN(NWJ2,2,JGL,4)                                       HANAL.33    
      EQUIVALENCE (ALPN(1,1,1,1),ALP(1,1,1))                               HANAL.34    
C                                                                          HANAL.35    
 6900 FORMAT(/' ***ABORT : HANAL CALLED WITH INVALID TYPE =',I5)           HANAL.36    
C                                                                          HANAL.37    
C     Use ITYPE to define transform type and symmetry labels.              HANAL.38    
C     ISPAR is symmetry of spectral field    = 0 for D,T,SP etc.           HANAL.39    
C                                            = 1 for Z.                    HANAL.40    
C     IGPAR is symmetry of Fourier field: same as ISPAR unless transform   HANAL.41    
C                                         involves a d/dy.                 HANAL.42    
C                                                                          HANAL.43    
      IF (ITYPE.LE.0.OR.ITYPE.GT.8) THEN                                   HANAL.44    
         WRITE(6,6900) ITYPE                                               HANAL.45    
         CALL ABORT                                                        HANAL.46    
      ENDIF                                                                HANAL.47    
      IALP=(ITYPE+1)/2                                                     HANAL.48    
      ISPAR=MOD(ITYPE,2)                                                   HANAL.49    
      IGPAR=ISPAR                                                          HANAL.50    
      IF (IALP.EQ.2.OR.IALP.EQ.4) IGPAR=1-ISPAR                            HANAL.51    
C                                                                          HANAL.52    
C     For a global run, sum and difference the complete Fourier            HANAL.53    
C     transforms at the northern and southern latitude rows to give        HANAL.54    
C     the even and odd contributions.                                      HANAL.55    
C     Separate code for each symmetry:                                     HANAL.56    
C        IGPAR=0 : even (IA) to precede odd (IB).                          HANAL.57    
C        IGPAR=1 : odd (IA) to precede even (IB).                          HANAL.58    
C                                                                          HANAL.59    
      IF (NHEM.EQ.2) THEN                                                  HANAL.60    
         IF (IGPAR.EQ.0) THEN                                              HANAL.61    
            DO 10 IA=1,NWW                                                 HANAL.62    
               IB=IA+IDL                                                   HANAL.63    
               DO 10 L=1,NLS                                               HANAL.64    
                  GVW(IA,L)=0.5*(GV(IA,L)+GV(IB,L))                        HANAL.65    
                  GVW(IB,L)=0.5*(GV(IA,L)-GV(IB,L))                        HANAL.66    
   10       CONTINUE                                                       HANAL.67    
         ELSE                                                              HANAL.68    
            DO 20 IA=1,NWW                                                 HANAL.69    
               IB=IA+IDL                                                   HANAL.70    
               DO 20 L=1,NLS                                               HANAL.71    
                  GVW(IA,L)=0.5*(GV(IA,L)-GV(IB,L))                        HANAL.72    
                  GVW(IB,L)=0.5*(GV(IA,L)+GV(IB,L))                        HANAL.73    
   20       CONTINUE                                                       HANAL.74    
         ENDIF                                                             HANAL.75    
      ENDIF                                                                HANAL.76    
C                                                                          HANAL.77    
C     Set up the appropriate Gaussian weight for the current latitude,     HANAL.78    
C     dependent on transform type.                                         HANAL.79    
C     Assumes JH in /LEGAU/ contains latitude counter from calling loop.   HANAL.80    
C                                                                          HANAL.81    
      IF (IALP.EQ.1) AWT=AW(JH)*CSSQ(JH)                                   HANAL.82    
      IF (IALP.EQ.2) AWT=-AW(JH)                                           HANAL.83    
      IF (IALP.EQ.3) AWT=AW(JH)*CSSQ(JH)                                   HANAL.84    
      IF (IALP.EQ.4) AWT=-AW(JH)                                           HANAL.85    
C                                                                          HANAL.86    
C     Calculate POLY array in vector loop before main transform.           HANAL.87    
C                                                                          HANAL.88    
      DO 30 IHEM=1,NHEM                                                    HANAL.89    
         IA=(ISPAR+1)*(2-IHEM) + (2-ISPAR)*(IHEM-1)                        HANAL.90    
         DO 30 IP=1,NWJ2                                                   HANAL.91    
            POLY(IP,IHEM,IALP)=AWT*ALPN(IP,IA,JL,IALP)                     HANAL.92    
   30 CONTINUE                                                             HANAL.93    
C                                                                          HANAL.94    
C     Perform direct Legendre transform from the even and odd              HANAL.95    
C     parts of the Fourier transforms to spectral space.                   HANAL.96    
C     Separate code for NHEM=1,2 to increase efficiency.                   HANAL.97    
C                                                                          HANAL.98    
      IF (NHEM.EQ.1) THEN                                                  HANAL.99    
         IM=0                                                              HANAL.100   
         IP=0                                                              HANAL.101   
         DO 40 M=0,MM-1,MOCT                                               HANAL.102   
            IM=IM+1                                                        HANAL.103   
            DO 40 N=M,NN-1,2                                               HANAL.104   
               IP=IP+1                                                     HANAL.105   
               DO 40 L=1,NLS                                               HANAL.106   
                  SV(IP,L)=SV(IP,L) + POLY(IP,1,IALP)*GV(IM,L)             HANAL.107   
   40    CONTINUE                                                          HANAL.108   
      ELSE                                                                 HANAL.109   
         IM=0                                                              HANAL.110   
         IP=0                                                              HANAL.111   
         DO 50 M=0,MM-1,MOCT                                               HANAL.112   
            IM=IM+1                                                        HANAL.113   
            IPM=IP                                                         HANAL.114   
            DO 50 L=1,NLS                                                  HANAL.115   
               IP=IPM                                                      HANAL.116   
               DO 50 N=M,NN-1,2                                            HANAL.117   
               IP=IP+1                                                     HANAL.118   
               SV(IP     ,L)=SV(IP     ,L)+POLY(IP,1,IALP)*GVW(IM    ,L)   HANAL.119   
               SV(IP+NWJ2,L)=SV(IP+NWJ2,L)+POLY(IP,2,IALP)*GVW(IM+IDL,L)   HANAL.120   
   50    CONTINUE                                                          HANAL.121   
      ENDIF                                                                HANAL.122   
C                                                                          HANAL.123   
      RETURN                                                               HANAL.124   
      END                                                                  HANAL.125   
C     ******************************************************************   HANAL.126   
C     ******************************************************************   HANAL.127   
      SUBROUTINE HANAL1(GV,GVW,SV,NLS,ITYPE)                               HANAL1.2     
C                                                                          HANAL1.3     
C     Performs a direct Legendre transform for a single-level field,       HANAL1.4     
C     from Fourier to spectral space.                                      HANAL1.5     
C                                                                          HANAL1.6     
C     The following types of Legendre function and thence types of         HANAL1.7     
C     transform may be used:                                               HANAL1.8     
C        ITYPE=1,2  :  ALP   :  ALPN(,,,1)   :  normal transform.          HANAL1.9     
C        ITYPE=3,4  :  DALP  :  ALPN(,,,2)   :  y-derivative.              HANAL1.10    
C        ITYPE=5,6  :  RLP   :  ALPN(,,,3)   :  del(-2).                   HANAL1.11    
C        ITYPE=7,8  :  RDLP  :  ALPN(,,,4)   :  y-derivative of del(-2).   HANAL1.12    
C     An even/odd value of ITYPE denotes a spectral field of even/odd      HANAL1.13    
C     symmetry.                                                            HANAL1.14    
C                                                                          HANAL1.15    
C     A Fourier work array GVW prevents corruption of the input Fourier    HANAL1.16    
C     array GV that would otherwise occur in global runs.                  HANAL1.17    
C                                                                          HANAL1.18    
C     NOTE: The y-derivative transforms use integration by parts and       HANAL1.19    
C           are valid only if the input field has zero zonal mean at       HANAL1.20    
C           both poles.                                                    HANAL1.21    
C                                                                          HANAL1.22    
C     Version for RSGUP3.                     Mike Blackburn,  05.01.95.   HANAL1.23    
C     Work array now a dummy argument (ANSI). Mike Blackburn,  04.09.96.   HANAL1.24    
C                                                                          HANAL1.25    
C                                                                          PARAM1.2     
C     Determines model resolution                                          PARAM1.3     
C                                                                          PARAM1.4     
      PARAMETER(NN=21,MM=21,NHEM=2,NL=5,MOCT=1,MG=128,JG=32,NWJ2=121        G100.1     
     P         ,NCRAY=64,JGL=JG)                                           G100.2     
C                                                                          PARAM1.7     
C                                                                          PARAM2.2     
C     Sets basic constants, especially those needed for array dimensions   PARAM2.3     
C                                                                          PARAM2.4     
      PARAMETER(MH=2,PI=3.14159265359,PI2=2.0*PI                           PARAM2.5     
     +,NNP=NN+1,MGPP=MG+2,JGP=JG+1,JGG=JG*NHEM,JGGP=JGG+1,MJP=NWJ2+NWJ2    PARAM2.6     
     +,NLM=NL-1,NLP=NL+1,NLPP=NL+2,NLA=NL+3,NLB=NL+4,NL2=NL*NL             PARAM2.7     
     +,IDA=(MG+MG+MG)/2+1,IDB=NWJ2*NL,IDC=IDB+IDB,IDD=MGPP*NL              PARAM2.8     
     +,IDE=NL2*NN,IDF=NCRAY*(MG+1),IDG=JG*NL,IDH=JG*MG                     PARAM2.9     
     +,IDI=NNP/2,IDJ=IDI*IDI,IDK=NL*IDI,IDL=MGPP/2,IDM=NNP/2,IDN=IDM*NL    PARAM2.10    
     +,NWW=1+(MM-1)/MOCT)                                                  PARAM2.11    
      PARAMETER(IGA=NWJ2*NHEM,IGB=IDB*NHEM,IGC=MGPP*NHEM,IGD=IDD*NHEM      PARAM2.12    
     +,IGG=IDG*NHEM,IGL=IDL*NHEM,IGM=IDM*NHEM,IGN=IDN*NHEM                 PARAM2.13    
     +,IGO=IGA+IGA,IGP=IGB+IGB,NFTWG=5*NL+3,NFTGW=6*NL+2)                  PARAM2.14    
C                                                                          PARAM2.15    
C                                                                          BLANK.2     
C     Basic planetary parameters for run plus information about            BLANK.3     
C     vertical grid structure                                              BLANK.4     
C                                                                          BLANK.5     
      COMMON        SQ(NNP),RSQ(NNP),SIGMAH(NLM),SIGMA(NL)                 BLANK.6     
     +              ,T01S2(NLM),T0(NL),ALPHA(NL),DSIGMA(NL),RDSIG(NL)      BLANK.7     
     +              ,TKP(NL),C(NL2),SQH(NNP)                               BLANK.8     
     +              ,MF,MFP,JZF,NF,NFP                                     BLANK.9     
     +              ,AKAP,GA,GASCON,RADEA,WW,PFAC,EZ,AIOCT                 BLANK.10    
     +              ,LRSTRT,LSHORT,LTVEC,LSTRETCH                          BLANK.11    
     +              ,LBALAN,LRESTIJ                                        BLANK.12    
     +              ,LNOISE                                                SC970203.1     
     +              ,LGPDAMP,LFCE,LFED,LFAN,LLIN,LMODE,LTRAIN              NICK.1     
      COMPLEX EZ,AIOCT                                                     BLANK.13    
      LOGICAL LRSTRT,LSHORT,LTVEC,LSTRETCH,LBALAN,LRESTIJ                  BLANK.14    
     +       ,LNOISE                                                       SC970203.2     
     +       ,LGPDAMP,LFCE,LFED,LFAN,LLIN,LMODE,LTRAIN                     NICK.2     
C                                                                          BLANK.15    
C                                                                          LEGAU.2     
C     Legendre polynomials and information about gaussian latitudes        LEGAU.3     
C                                                                          LEGAU.4     
      COMMON/LEGAU/ ALPJ(MJP),DALPJ(MJP)                                   LEGAU.5     
     +              ,ALP(NWJ2,2,JGL),DALP(NWJ2,2,JGL)                      LEGAU.6     
     +              ,RLP(NWJ2,2,JGL),RDLP(NWJ2,2,JGL)                      LEGAU.7     
     +              ,SI(JGG),CS(JGG),SISQ(JGG),CSSQ(JGG),SECSQ(JGG)        LEGAU.8     
     +              ,ALAT(JGG),GWT(JGG),AW(JGG),JH,JL,JINC                 LEGAU.9     
C                                                                          LEGAU.10    
C                                                                          POLYNO.2     
C     Polynomial used to aid vectorization of Legendre transforms          POLYNO.3     
C                                                                          POLYNO.4     
      COMMON/POLYNO/POLY(NWJ2,2,4),CMPA(IGL)                               POLYNO.5     
      COMPLEX CMPA                                                         POLYNO.6     
C                                                                          POLYNO.7     
      COMPLEX GV(IGL),GVW(IGL),SV(IGA)                                     HANAL1.31    
      REAL ALPN(NWJ2,2,JGL,4)                                              HANAL1.32    
      EQUIVALENCE (ALPN(1,1,1,1),ALP(1,1,1))                               HANAL1.33    
C                                                                          HANAL1.34    
 6900 FORMAT(/' ***ABORT : HANAL1 CALLED WITH INVALID TYPE =',I5)          HANAL1.35    
 6910 FORMAT(/' ***ABORT : HANAL1 CALLED WITH NLS =',I3,' : MUST BE 1')    HANAL1.36    
C                                                                          HANAL1.37    
C     Check this is a single-level call.                                   HANAL1.38    
C                                                                          HANAL1.39    
      IF (NLS.NE.1) THEN                                                   HANAL1.40    
         WRITE(6,6910) NLS                                                 HANAL1.41    
         CALL ABORT                                                        HANAL1.42    
      ENDIF                                                                HANAL1.43    
C                                                                          HANAL1.44    
C     Use ITYPE to define transform type and symmetry labels.              HANAL1.45    
C     ISPAR is symmetry of spectral field    = 0 for D,T,SP etc.           HANAL1.46    
C                                            = 1 for Z.                    HANAL1.47    
C     IGPAR is symmetry of Fourier field: same as ISPAR unless transform   HANAL1.48    
C                                         involves a d/dy.                 HANAL1.49    
C                                                                          HANAL1.50    
      IF (ITYPE.LE.0.OR.ITYPE.GT.8) THEN                                   HANAL1.51    
         WRITE(6,6900) ITYPE                                               HANAL1.52    
         CALL ABORT                                                        HANAL1.53    
      ENDIF                                                                HANAL1.54    
      IALP=(ITYPE+1)/2                                                     HANAL1.55    
      ISPAR=MOD(ITYPE,2)                                                   HANAL1.56    
      IGPAR=ISPAR                                                          HANAL1.57    
      IF (IALP.EQ.2.OR.IALP.EQ.4) IGPAR=1-ISPAR                            HANAL1.58    
C                                                                          HANAL1.59    
C     For a global run, sum and difference the complete Fourier            HANAL1.60    
C     transforms at the northern and southern latitude rows to give        HANAL1.61    
C     the even and odd contributions.                                      HANAL1.62    
C     Separate code for each symmetry:                                     HANAL1.63    
C        IGPAR=0 : even (IA) to precede odd (IB).                          HANAL1.64    
C        IGPAR=1 : odd (IA) to precede even (IB).                          HANAL1.65    
C                                                                          HANAL1.66    
      IF (NHEM.EQ.2) THEN                                                  HANAL1.67    
         IF (IGPAR.EQ.0) THEN                                              HANAL1.68    
            DO 10 IA=1,NWW                                                 HANAL1.69    
               IB=IA+IDL                                                   HANAL1.70    
               GVW(IA)=0.5*(GV(IA)+GV(IB))                                 HANAL1.71    
               GVW(IB)=0.5*(GV(IA)-GV(IB))                                 HANAL1.72    
   10       CONTINUE                                                       HANAL1.73    
         ELSE                                                              HANAL1.74    
            DO 20 IA=1,NWW                                                 HANAL1.75    
               IB=IA+IDL                                                   HANAL1.76    
               GVW(IA)=0.5*(GV(IA)-GV(IB))                                 HANAL1.77    
               GVW(IB)=0.5*(GV(IA)+GV(IB))                                 HANAL1.78    
   20       CONTINUE                                                       HANAL1.79    
         ENDIF                                                             HANAL1.80    
      ENDIF                                                                HANAL1.81    
C                                                                          HANAL1.82    
C     Set up the appropriate Gaussian weight for the current latitude,     HANAL1.83    
C     dependent on transform type.                                         HANAL1.84    
C     Assumes JH in /LEGAU/ contains latitude counter from calling loop.   HANAL1.85    
C                                                                          HANAL1.86    
      IF (IALP.EQ.1) AWT=AW(JH)*CSSQ(JH)                                   HANAL1.87    
      IF (IALP.EQ.2) AWT=-AW(JH)                                           HANAL1.88    
      IF (IALP.EQ.3) AWT=AW(JH)*CSSQ(JH)                                   HANAL1.89    
      IF (IALP.EQ.4) AWT=-AW(JH)                                           HANAL1.90    
C                                                                          HANAL1.91    
C     Calculate POLY array in vector loop before main transform.           HANAL1.92    
C                                                                          HANAL1.93    
      DO 30 IHEM=1,NHEM                                                    HANAL1.94    
         IA=(ISPAR+1)*(2-IHEM) + (2-ISPAR)*(IHEM-1)                        HANAL1.95    
         DO 30 IP=1,NWJ2                                                   HANAL1.96    
            POLY(IP,IHEM,IALP)=AWT*ALPN(IP,IA,JL,IALP)                     HANAL1.97    
   30 CONTINUE                                                             HANAL1.98    
C                                                                          HANAL1.99    
C     Perform direct Legendre transform from the even and odd              HANAL1.100   
C     parts of the Fourier transforms to spectral space.                   HANAL1.101   
C     Separate code for NHEM=1,2 to increase efficiency.                   HANAL1.102   
C                                                                          HANAL1.103   
      IF (NHEM.EQ.1) THEN                                                  HANAL1.104   
         IM=0                                                              HANAL1.105   
         IP=0                                                              HANAL1.106   
         DO 40 M=0,MM-1,MOCT                                               HANAL1.107   
            IM=IM+1                                                        HANAL1.108   
            DO 40 N=M,NN-1,2                                               HANAL1.109   
               IP=IP+1                                                     HANAL1.110   
               SV(IP)=SV(IP) + POLY(IP,1,IALP)*GV(IM)                      HANAL1.111   
   40    CONTINUE                                                          HANAL1.112   
      ELSE                                                                 HANAL1.113   
         IM=0                                                              HANAL1.114   
         IP=0                                                              HANAL1.115   
         DO 50 M=0,MM-1,MOCT                                               HANAL1.116   
            IM=IM+1                                                        HANAL1.117   
            DO 50 N=M,NN-1,2                                               HANAL1.118   
               IP=IP+1                                                     HANAL1.119   
               SV(IP     )=SV(IP     ) + POLY(IP,1,IALP)*GVW(IM    )       HANAL1.120   
               SV(IP+NWJ2)=SV(IP+NWJ2) + POLY(IP,2,IALP)*GVW(IM+IDL)       HANAL1.121   
   50    CONTINUE                                                          HANAL1.122   
      ENDIF                                                                HANAL1.123   
C                                                                          HANAL1.124   
      RETURN                                                               HANAL1.125   
      END                                                                  HANAL1.126   
C     ******************************************************************   HANAL1.127   
C     ******************************************************************   HANAL1.128   
      SUBROUTINE HEXP(SV,GV,NLS,ITYPE)                                     HEXP.2     
C                                                                          HEXP.3     
C     Performs an indirect Legendre transform for a (set of) field(s)      HEXP.4     
C     having a total of NLS levels, from spectral to Fourier space.        HEXP.5     
C                                                                          HEXP.6     
C     The following types of Legendre function and thence types of         HEXP.7     
C     transform may be used:                                               HEXP.8     
C        ITYPE=1,2  :  ALP   :  ALPN(,,,1)   :  normal transform.          HEXP.9     
C        ITYPE=3,4  :  DALP  :  ALPN(,,,2)   :  y-derivative.              HEXP.10    
C        ITYPE=5,6  :  RLP   :  ALPN(,,,3)   :  del(-2).                   HEXP.11    
C        ITYPE=7,8  :  RDLP  :  ALPN(,,,4)   :  y-derivative of del(-2).   HEXP.12    
C     An even/odd value of ITYPE denotes a spectral field of even/odd      HEXP.13    
C     symmetry.                                                            HEXP.14    
C                                                                          HEXP.15    
C     Version for RSGUP3.                     Mike Blackburn,  10.01.95.   HEXP.16    
C                                                                          HEXP.17    
C                                                                          PARAM1.2     
C     Determines model resolution                                          PARAM1.3     
C                                                                          PARAM1.4     
      PARAMETER(NN=21,MM=21,NHEM=2,NL=5,MOCT=1,MG=128,JG=32,NWJ2=121        G100.1     
     P         ,NCRAY=64,JGL=JG)                                           G100.2     
C                                                                          PARAM1.7     
C                                                                          PARAM2.2     
C     Sets basic constants, especially those needed for array dimensions   PARAM2.3     
C                                                                          PARAM2.4     
      PARAMETER(MH=2,PI=3.14159265359,PI2=2.0*PI                           PARAM2.5     
     +,NNP=NN+1,MGPP=MG+2,JGP=JG+1,JGG=JG*NHEM,JGGP=JGG+1,MJP=NWJ2+NWJ2    PARAM2.6     
     +,NLM=NL-1,NLP=NL+1,NLPP=NL+2,NLA=NL+3,NLB=NL+4,NL2=NL*NL             PARAM2.7     
     +,IDA=(MG+MG+MG)/2+1,IDB=NWJ2*NL,IDC=IDB+IDB,IDD=MGPP*NL              PARAM2.8     
     +,IDE=NL2*NN,IDF=NCRAY*(MG+1),IDG=JG*NL,IDH=JG*MG                     PARAM2.9     
     +,IDI=NNP/2,IDJ=IDI*IDI,IDK=NL*IDI,IDL=MGPP/2,IDM=NNP/2,IDN=IDM*NL    PARAM2.10    
     +,NWW=1+(MM-1)/MOCT)                                                  PARAM2.11    
      PARAMETER(IGA=NWJ2*NHEM,IGB=IDB*NHEM,IGC=MGPP*NHEM,IGD=IDD*NHEM      PARAM2.12    
     +,IGG=IDG*NHEM,IGL=IDL*NHEM,IGM=IDM*NHEM,IGN=IDN*NHEM                 PARAM2.13    
     +,IGO=IGA+IGA,IGP=IGB+IGB,NFTWG=5*NL+3,NFTGW=6*NL+2)                  PARAM2.14    
C                                                                          PARAM2.15    
C                                                                          BLANK.2     
C     Basic planetary parameters for run plus information about            BLANK.3     
C     vertical grid structure                                              BLANK.4     
C                                                                          BLANK.5     
      COMMON        SQ(NNP),RSQ(NNP),SIGMAH(NLM),SIGMA(NL)                 BLANK.6     
     +              ,T01S2(NLM),T0(NL),ALPHA(NL),DSIGMA(NL),RDSIG(NL)      BLANK.7     
     +              ,TKP(NL),C(NL2),SQH(NNP)                               BLANK.8     
     +              ,MF,MFP,JZF,NF,NFP                                     BLANK.9     
     +              ,AKAP,GA,GASCON,RADEA,WW,PFAC,EZ,AIOCT                 BLANK.10    
     +              ,LRSTRT,LSHORT,LTVEC,LSTRETCH                          BLANK.11    
     +              ,LBALAN,LRESTIJ                                        BLANK.12    
     +              ,LNOISE                                                SC970203.1     
     +              ,LGPDAMP,LFCE,LFED,LFAN,LLIN,LMODE,LTRAIN              NICK.1     
      COMPLEX EZ,AIOCT                                                     BLANK.13    
      LOGICAL LRSTRT,LSHORT,LTVEC,LSTRETCH,LBALAN,LRESTIJ                  BLANK.14    
     +       ,LNOISE                                                       SC970203.2     
     +       ,LGPDAMP,LFCE,LFED,LFAN,LLIN,LMODE,LTRAIN                     NICK.2     
C                                                                          BLANK.15    
C                                                                          LEGAU.2     
C     Legendre polynomials and information about gaussian latitudes        LEGAU.3     
C                                                                          LEGAU.4     
      COMMON/LEGAU/ ALPJ(MJP),DALPJ(MJP)                                   LEGAU.5     
     +              ,ALP(NWJ2,2,JGL),DALP(NWJ2,2,JGL)                      LEGAU.6     
     +              ,RLP(NWJ2,2,JGL),RDLP(NWJ2,2,JGL)                      LEGAU.7     
     +              ,SI(JGG),CS(JGG),SISQ(JGG),CSSQ(JGG),SECSQ(JGG)        LEGAU.8     
     +              ,ALAT(JGG),GWT(JGG),AW(JGG),JH,JL,JINC                 LEGAU.9     
C                                                                          LEGAU.10    
      COMPLEX     SV(IGA,NLS),GV(IGL,NLS),TEMP                             HEXP.22    
      REAL        ALPN(NWJ2,2,JGL,4)                                       HEXP.23    
      EQUIVALENCE (ALPN(1,1,1,1),ALP(1,1,1))                               HEXP.24    
C                                                                          HEXP.25    
 6900 FORMAT(/' ***ABORT : HEXP CALLED WITH INVALID TYPE =',I5)            HEXP.26    
C                                                                          HEXP.27    
C     Use ITYPE to define transform type and symmetry labels.              HEXP.28    
C     ISPAR is symmetry of spectral field    = 0 for D,T,SP etc.           HEXP.29    
C                                            = 1 for Z.                    HEXP.30    
C     IGPAR is symmetry of Fourier field: same as ISPAR unless transform   HEXP.31    
C                                         involves a d/dy.                 HEXP.32    
C                                                                          HEXP.33    
      IF (ITYPE.LE.0.OR.ITYPE.GE.9) THEN                                   HEXP.34    
         WRITE(6,6900) ITYPE                                               HEXP.35    
         CALL ABORT                                                        HEXP.36    
      ENDIF                                                                HEXP.37    
      IALP=(ITYPE+1)/2                                                     HEXP.38    
      ISPAR=MOD(ITYPE,2)                                                   HEXP.39    
      IGPAR=ISPAR                                                          HEXP.40    
      IF (IALP.EQ.2.OR.IALP.EQ.4) IGPAR=1-ISPAR                            HEXP.41    
C                                                                          HEXP.42    
C     Perform inverse Legendre transform from spectral space to form       HEXP.43    
C     the even and odd contributions to the Fourier transforms.            HEXP.44    
C     Separate code for NHEM=1,2 to increase efficiency.                   HEXP.45    
C                                                                          HEXP.46    
      IF (NHEM.EQ.1) THEN                                                  HEXP.47    
         IA=ISPAR+1                                                        HEXP.48    
         IM=0                                                              HEXP.49    
         IP=0                                                              HEXP.50    
         DO 10 M=0,MM-1,MOCT                                               HEXP.51    
            IM=IM+1                                                        HEXP.52    
            DO 10 N=M,NN-1,2                                               HEXP.53    
               IP=IP+1                                                     HEXP.54    
               DO 10 L=1,NLS                                               HEXP.55    
                  GV(IM,L)=GV(IM,L)+ALPN(IP,IA,JL,IALP)*SV(IP,L)           HEXP.56    
   10    CONTINUE                                                          HEXP.57    
      ELSE                                                                 HEXP.58    
         IA=ISPAR+1                                                        HEXP.59    
         IB=2-ISPAR                                                        HEXP.60    
         IM=0                                                              HEXP.61    
         IP=0                                                              HEXP.62    
         DO 20 M=0,MM-1,MOCT                                               HEXP.63    
            IM=IM+1                                                        HEXP.64    
            IG=IM+IDL                                                      HEXP.65    
            DO 20 N=M,NN-1,2                                               HEXP.66    
               IP=IP+1                                                     HEXP.67    
               DO 20 L=1,NLS                                               HEXP.68    
                  GV(IM,L)=GV(IM,L)+ALPN(IP,IA,JL,IALP)*SV(IP     ,L)      HEXP.69    
                  GV(IG,L)=GV(IG,L)+ALPN(IP,IB,JL,IALP)*SV(IP+NWJ2,L)      HEXP.70    
   20    CONTINUE                                                          HEXP.71    
      ENDIF                                                                HEXP.72    
C                                                                          HEXP.73    
C     For a global run, sum and difference even and odd contributions      HEXP.74    
C     to give the complete Fourier transforms at the northern and          HEXP.75    
C     southern latitude rows.  Separate code for each symmetry:            HEXP.76    
C        IGPAR=0 : even (IA) precedes odd (IB).                            HEXP.77    
C        IGPAR=1 : odd (IA) precedes even (IB).                            HEXP.78    
C                                                                          HEXP.79    
      IF (NHEM.EQ.2) THEN                                                  HEXP.80    
         IF (IGPAR.EQ.0) THEN                                              HEXP.81    
            DO 30 IA=1,NWW                                                 HEXP.82    
               IB=IA+IDL                                                   HEXP.83    
               DO 30 L=1,NLS                                               HEXP.84    
                  TEMP=GV(IA,L)                                            HEXP.85    
                  GV(IA,L)=TEMP+GV(IB,L)                                   HEXP.86    
                  GV(IB,L)=TEMP-GV(IB,L)                                   HEXP.87    
   30       CONTINUE                                                       HEXP.88    
         ELSE                                                              HEXP.89    
            DO 40 IA=1,NWW                                                 HEXP.90    
               IB=IA+IDL                                                   HEXP.91    
               DO 40 L=1,NLS                                               HEXP.92    
                  TEMP=GV(IA,L)                                            HEXP.93    
                  GV(IA,L)=GV(IB,L)+TEMP                                   HEXP.94    
                  GV(IB,L)=GV(IB,L)-TEMP                                   HEXP.95    
   40       CONTINUE                                                       HEXP.96    
         ENDIF                                                             HEXP.97    
      ENDIF                                                                HEXP.98    
C                                                                          HEXP.99    
      RETURN                                                               HEXP.100   
      END                                                                  HEXP.101   
C     ******************************************************************   HEXP.102   
      SUBROUTINE HEXP1(SV,GV,NLS,ITYPE)                                    HEXP1.2     
C                                                                          HEXP1.3     
C     Performs an indirect Legendre transform for a single-level field,    HEXP1.4     
C     from spectral to Fourier space.                                      HEXP1.5     
C                                                                          HEXP1.6     
C     The following types of Legendre function and thence types of         HEXP1.7     
C     transform may be used:                                               HEXP1.8     
C        ITYPE=1,2  :  ALP   :  ALPN(,,,1)   :  normal transform.          HEXP1.9     
C        ITYPE=3,4  :  DALP  :  ALPN(,,,2)   :  y-derivative.              HEXP1.10    
C        ITYPE=5,6  :  RLP   :  ALPN(,,,3)   :  del(-2).                   HEXP1.11    
C        ITYPE=7,8  :  RDLP  :  ALPN(,,,4)   :  y-derivative of del(-2).   HEXP1.12    
C     An even/odd value of ITYPE denotes a spectral field of even/odd      HEXP1.13    
C     symmetry.                                                            HEXP1.14    
C                                                                          HEXP1.15    
C     Version for RSGUP3.                     Mike Blackburn,  10.01.95.   HEXP1.16    
C                                                                          HEXP1.17    
C                                                                          PARAM1.2     
C     Determines model resolution                                          PARAM1.3     
C                                                                          PARAM1.4     
      PARAMETER(NN=21,MM=21,NHEM=2,NL=5,MOCT=1,MG=128,JG=32,NWJ2=121        G100.1     
     P         ,NCRAY=64,JGL=JG)                                           G100.2     
C                                                                          PARAM1.7     
C                                                                          PARAM2.2     
C     Sets basic constants, especially those needed for array dimensions   PARAM2.3     
C                                                                          PARAM2.4     
      PARAMETER(MH=2,PI=3.14159265359,PI2=2.0*PI                           PARAM2.5     
     +,NNP=NN+1,MGPP=MG+2,JGP=JG+1,JGG=JG*NHEM,JGGP=JGG+1,MJP=NWJ2+NWJ2    PARAM2.6     
     +,NLM=NL-1,NLP=NL+1,NLPP=NL+2,NLA=NL+3,NLB=NL+4,NL2=NL*NL             PARAM2.7     
     +,IDA=(MG+MG+MG)/2+1,IDB=NWJ2*NL,IDC=IDB+IDB,IDD=MGPP*NL              PARAM2.8     
     +,IDE=NL2*NN,IDF=NCRAY*(MG+1),IDG=JG*NL,IDH=JG*MG                     PARAM2.9     
     +,IDI=NNP/2,IDJ=IDI*IDI,IDK=NL*IDI,IDL=MGPP/2,IDM=NNP/2,IDN=IDM*NL    PARAM2.10    
     +,NWW=1+(MM-1)/MOCT)                                                  PARAM2.11    
      PARAMETER(IGA=NWJ2*NHEM,IGB=IDB*NHEM,IGC=MGPP*NHEM,IGD=IDD*NHEM      PARAM2.12    
     +,IGG=IDG*NHEM,IGL=IDL*NHEM,IGM=IDM*NHEM,IGN=IDN*NHEM                 PARAM2.13    
     +,IGO=IGA+IGA,IGP=IGB+IGB,NFTWG=5*NL+3,NFTGW=6*NL+2)                  PARAM2.14    
C                                                                          PARAM2.15    
C                                                                          BLANK.2     
C     Basic planetary parameters for run plus information about            BLANK.3     
C     vertical grid structure                                              BLANK.4     
C                                                                          BLANK.5     
      COMMON        SQ(NNP),RSQ(NNP),SIGMAH(NLM),SIGMA(NL)                 BLANK.6     
     +              ,T01S2(NLM),T0(NL),ALPHA(NL),DSIGMA(NL),RDSIG(NL)      BLANK.7     
     +              ,TKP(NL),C(NL2),SQH(NNP)                               BLANK.8     
     +              ,MF,MFP,JZF,NF,NFP                                     BLANK.9     
     +              ,AKAP,GA,GASCON,RADEA,WW,PFAC,EZ,AIOCT                 BLANK.10    
     +              ,LRSTRT,LSHORT,LTVEC,LSTRETCH                          BLANK.11    
     +              ,LBALAN,LRESTIJ                                        BLANK.12    
     +              ,LNOISE                                                SC970203.1     
     +              ,LGPDAMP,LFCE,LFED,LFAN,LLIN,LMODE,LTRAIN              NICK.1     
      COMPLEX EZ,AIOCT                                                     BLANK.13    
      LOGICAL LRSTRT,LSHORT,LTVEC,LSTRETCH,LBALAN,LRESTIJ                  BLANK.14    
     +       ,LNOISE                                                       SC970203.2     
     +       ,LGPDAMP,LFCE,LFED,LFAN,LLIN,LMODE,LTRAIN                     NICK.2     
C                                                                          BLANK.15    
C                                                                          LEGAU.2     
C     Legendre polynomials and information about gaussian latitudes        LEGAU.3     
C                                                                          LEGAU.4     
      COMMON/LEGAU/ ALPJ(MJP),DALPJ(MJP)                                   LEGAU.5     
     +              ,ALP(NWJ2,2,JGL),DALP(NWJ2,2,JGL)                      LEGAU.6     
     +              ,RLP(NWJ2,2,JGL),RDLP(NWJ2,2,JGL)                      LEGAU.7     
     +              ,SI(JGG),CS(JGG),SISQ(JGG),CSSQ(JGG),SECSQ(JGG)        LEGAU.8     
     +              ,ALAT(JGG),GWT(JGG),AW(JGG),JH,JL,JINC                 LEGAU.9     
C                                                                          LEGAU.10    
      COMPLEX SV(IGA),GV(IGL),TEMP                                         HEXP1.22    
      REAL ALPN(NWJ2,2,JGL,4)                                              HEXP1.23    
      EQUIVALENCE (ALPN(1,1,1,1),ALP(1,1,1))                               HEXP1.24    
C                                                                          HEXP1.25    
 6900 FORMAT(/' ***ABORT : HEXP1 CALLED WITH INVALID TYPE =',I5)           HEXP1.26    
 6910 FORMAT(/' ***ABORT : HEXP1 CALLED WITH NLS =',I3,' : MUST BE 1')     HEXP1.27    
C                                                                          HEXP1.28    
C     Check this is a single-level call.                                   HEXP1.29    
C                                                                          HEXP1.30    
      IF (NLS.NE.1) THEN                                                   HEXP1.31    
         WRITE(6,6910) NLS                                                 HEXP1.32    
         CALL ABORT                                                        HEXP1.33    
      ENDIF                                                                HEXP1.34    
C                                                                          HEXP1.35    
C     Use ITYPE to define transform type and symmetry labels.              HEXP1.36    
C     ISPAR is symmetry of spectral field    = 0 for D,T,SP etc.           HEXP1.37    
C                                            = 1 for Z.                    HEXP1.38    
C     IGPAR is symmetry of Fourier field: same as ISPAR unless transform   HEXP1.39    
C                                         involves a d/dy.                 HEXP1.40    
C                                                                          HEXP1.41    
      IF (ITYPE.LE.0.OR.ITYPE.GE.9) THEN                                   HEXP1.42    
         WRITE(6,6900) ITYPE                                               HEXP1.43    
         CALL ABORT                                                        HEXP1.44    
      ENDIF                                                                HEXP1.45    
      IALP=(ITYPE+1)/2                                                     HEXP1.46    
      ISPAR=MOD(ITYPE,2)                                                   HEXP1.47    
      IGPAR=ISPAR                                                          HEXP1.48    
      IF (IALP.EQ.2.OR.IALP.EQ.4) IGPAR=1-ISPAR                            HEXP1.49    
C                                                                          HEXP1.50    
C     Perform inverse Legendre transform from spectral space to form       HEXP1.51    
C     the even and odd contributions to the Fourier transforms.            HEXP1.52    
C     Separate code for NHEM=1,2 to increase efficiency.                   HEXP1.53    
C                                                                          HEXP1.54    
      IF (NHEM.EQ.1) THEN                                                  HEXP1.55    
         IA=ISPAR+1                                                        HEXP1.56    
         IM=0                                                              HEXP1.57    
         IP=0                                                              HEXP1.58    
         DO 10 M=0,MM-1,MOCT                                               HEXP1.59    
            IM=IM+1                                                        HEXP1.60    
            DO 10 N=M,NN-1,2                                               HEXP1.61    
               IP=IP+1                                                     HEXP1.62    
               GV(IM)=GV(IM) + ALPN(IP,IA,JL,IALP)*SV(IP)                  HEXP1.63    
   10    CONTINUE                                                          HEXP1.64    
      ELSE                                                                 HEXP1.65    
         IA=ISPAR+1                                                        HEXP1.66    
         IB=2-ISPAR                                                        HEXP1.67    
         IM=0                                                              HEXP1.68    
         IP=0                                                              HEXP1.69    
         DO 20 M=0,MM-1,MOCT                                               HEXP1.70    
            IM=IM+1                                                        HEXP1.71    
            DO 20 N=M,NN-1,2                                               HEXP1.72    
               IP=IP+1                                                     HEXP1.73    
               GV(IM    )=GV(IM    ) + ALPN(IP,IA,JL,IALP)*SV(IP     )     HEXP1.74    
               GV(IM+IDL)=GV(IM+IDL) + ALPN(IP,IB,JL,IALP)*SV(IP+NWJ2)     HEXP1.75    
   20    CONTINUE                                                          HEXP1.76    
      ENDIF                                                                HEXP1.77    
C                                                                          HEXP1.78    
C     For a global run, sum and difference even and odd contributions      HEXP1.79    
C     to give the complete Fourier transforms at the northern and          HEXP1.80    
C     southern latitude rows.  Separate code for each symmetry:            HEXP1.81    
C        IGPAR=0 : even (IA) precedes odd (IB).                            HEXP1.82    
C        IGPAR=1 : odd (IA) precedes even (IB).                            HEXP1.83    
C                                                                          HEXP1.84    
      IF (NHEM.EQ.2) THEN                                                  HEXP1.85    
         IF (IGPAR.EQ.0) THEN                                              HEXP1.86    
            DO 30 IA=1,NWW                                                 HEXP1.87    
               IB=IA+IDL                                                   HEXP1.88    
               TEMP=GV(IA)                                                 HEXP1.89    
               GV(IA)=TEMP+GV(IB)                                          HEXP1.90    
               GV(IB)=TEMP-GV(IB)                                          HEXP1.91    
   30       CONTINUE                                                       HEXP1.92    
         ELSE                                                              HEXP1.93    
            DO 40 IA=1,NWW                                                 HEXP1.94    
               IB=IA+IDL                                                   HEXP1.95    
               TEMP=GV(IA)                                                 HEXP1.96    
               GV(IA)=GV(IB)+TEMP                                          HEXP1.97    
               GV(IB)=GV(IB)-TEMP                                          HEXP1.98    
   40       CONTINUE                                                       HEXP1.99    
         ENDIF                                                             HEXP1.100   
      ENDIF                                                                HEXP1.101   
C                                                                          HEXP1.102   
      RETURN                                                               HEXP1.103   
      END                                                                  HEXP1.104   
C     ******************************************************************   HEXP1.105   
      SUBROUTINE LGNDRE(NN,MM,MOCT,ALP,DALP,MJP,JL,SIJ,CSJ)                LGNDRE.2     
C                                                                          LGNDRE.3     
C     Calculates legendre polynomials (ALP) and their derivatives          LGNDRE.4     
C     (DALP) at the JL'th latitude (JL is in LEGAU) using                  LGNDRE.5     
C     recurrence relationships.                                            LGNDRE.6     
C                                                                          LGNDRE.7     
      REAL ALP(MJP,JL),DALP(MJP,JL)                                        LGNDRE.8     
C                                                                          LGNDRE.9     
      LM=2                                                                 LGNDRE.10    
C                                                                          LGNDRE.11    
C     Set P(0,0) and P(0,1)                                                LGNDRE.12    
C                                                                          LGNDRE.13    
      ALP(1,JL)=SQRT(.5)                                                   LGNDRE.14    
      F1M=SQRT(1.5)                                                        LGNDRE.15    
      ALP(2,JL)=F1M*SIJ                                                    LGNDRE.16    
      DALP(1,JL)=0.                                                        LGNDRE.17    
C                                                                          LGNDRE.18    
C     Loop over wavenumbers                                                LGNDRE.19    
C                                                                          LGNDRE.20    
      DO 1 M1=1,MM                                                         LGNDRE.21    
         M=M1-1                                                            LGNDRE.22    
         AM=M                                                              LGNDRE.23    
         A2M=M+M                                                           LGNDRE.24    
         E2=SQRT(A2M+3.)                                                   LGNDRE.25    
         IF (M.GT.0) THEN                                                  LGNDRE.26    
            F2M=-F1M*CSJ/SQRT(A2M)                                         LGNDRE.27    
            F1M=F2M*E2                                                     LGNDRE.28    
            IF (M.NE.MMO) GOTO 1                                           LGNDRE.29    
            LM=LM+1                                                        LGNDRE.30    
            ALP(LM,JL)=F2M                                                 LGNDRE.31    
            LM=LM+1                                                        LGNDRE.32    
            ALP(LM,JL)=F1M*SIJ                                             LGNDRE.33    
            DALP(LM-1,JL)=-AM*ALP(LM,JL)/E2                                LGNDRE.34    
         ENDIF                                                             LGNDRE.35    
         M2=M+2                                                            LGNDRE.36    
         MMO=M+MOCT                                                        LGNDRE.37    
         JFM=((NN-M1)/2)*2+M2-1                                            LGNDRE.38    
         IF (JFM.GE.M2) THEN                                               LGNDRE.39    
            K=LM-M2+1                                                      LGNDRE.40    
            AMSQ=AM*AM                                                     LGNDRE.41    
C                                                                          LGNDRE.42    
C           Loop over degree N                                             LGNDRE.43    
C                                                                          LGNDRE.44    
            DO 4 N=M2,JFM                                                  LGNDRE.45    
               AN=N                                                        LGNDRE.46    
               AN2=N*N                                                     LGNDRE.47    
               ANM2=(N-1)*(N-1)                                            LGNDRE.48    
               E1=SQRT((ANM2-AMSQ)/(4.*ANM2-1.))                           LGNDRE.49    
               E2=SQRT((4.*AN2-1.)/(AN2-AMSQ))                             LGNDRE.50    
               ALP(K+N,JL)=E2*(SIJ*ALP(K+N-1,JL)-E1*ALP(K+N-2,JL))         LGNDRE.51    
               DALP(K+N-1,JL)=(1.-AN)*ALP(K+N,JL)/E2+AN*E1*ALP(K+N-2,JL)   LGNDRE.52    
    4       CONTINUE                                                       LGNDRE.53    
            LM=LM+JFM-M2+1                                                 LGNDRE.54    
         ENDIF                                                             LGNDRE.55    
         DALP(LM,JL)=-AN*SIJ*ALP(LM,JL)+(AN+AN+1.0)*ALP(LM-1,JL)/E2        LGNDRE.56    
    1 CONTINUE                                                             LGNDRE.57    
      RETURN                                                               LGNDRE.58    
      END                                                                  LGNDRE.59    
      SUBROUTINE SPDEL2(Z,FILT,NWJ2,NN,MM,MOCT,NHEM,NL,IPAR,ITYPE)         SPDEL2.2     
C                                                                          SPDEL2.3     
C     Perform del**2 or del**(-2) operation on a spectral field.           SPDEL2.4     
C     The input spectral array is assumed to use the jagged triangular     SPDEL2.5     
C     truncation of the Reading baroclinic spectral models.  This is       SPDEL2.6     
C     also used in the diagnostics program for fields from both Reading    SPDEL2.7     
C     models and the UGCM.                                                 SPDEL2.8     
C                                                                          SPDEL2.9     
C     Input arguments:                                                     SPDEL2.10    
C        Z      - Complex array containing input spectral field.           SPDEL2.11    
C                 The truncation is assumed to be jagged triangular.       SPDEL2.12    
C                 i.e. symmetric (even) coefficients are included up       SPDEL2.13    
C                 to total wavenumber (NN-1), while anti-symmetric         SPDEL2.14    
C                 (odd) coefficients are included up to wavenumber NN.     SPDEL2.15    
C                 This gives equal numbers of even and odd coefficients    SPDEL2.16    
C                 in the truncated series.  Ordering is of increasing      SPDEL2.17    
C                 total wavenumber within increasing zonal wavenumber.     SPDEL2.18    
C        FILT   - Real array, unset, to receive filter coefficients.       SPDEL2.19    
C        NWJ2   - First dimension of Z: number of even or odd coeffs       SPDEL2.20    
C                 at a single level in the jagged triangular truncation.   SPDEL2.21    
C        NN     - Highest total wavenumber of input truncation.            SPDEL2.22    
C        MM     - Highest zonal wavenumber of input truncation.            SPDEL2.23    
C        MOCT   - Symmetry in longitude.  Only zonal wavenumbers 0,MOCT,   SPDEL2.24    
C                 2*MOCT,..,MM are included in the input truncation.       SPDEL2.25    
C        NHEM   - Symmetry in latitude: { 1 = hemispheric, only even or    SPDEL2.26    
C                                       {     odd coefficients included.   SPDEL2.27    
C                                       { 2 = global, both even and odd    SPDEL2.28    
C                                       {     coefficients included.       SPDEL2.29    
C        NL     - Number of levels in vertical.                            SPDEL2.30    
C        IPAR   - Parity of field: { IPAR=even for even hem symmetry,      SPDEL2.31    
C                                  { IPAR=odd  for  odd hem symmetry.      SPDEL2.32    
C        ITYPE  - Type of operation required: { +2, del**(+2),             SPDEL2.33    
C                                             { -2, del**(-2).             SPDEL2.34    
C     Output arguments:                                                    SPDEL2.35    
C        Z      - Filtered spectral field.  Ordering of spectral           SPDEL2.36    
C                 coefficients is unchanged.                               SPDEL2.37    
C        FILT   - Real array of filter coefficients.                       SPDEL2.38    
C        Other arguments unchanged.                                        SPDEL2.39    
C                                                                          SPDEL2.40    
C     Method:                                                              SPDEL2.41    
C        This routine can perform the following operations:                SPDEL2.42    
C     ITYPE=+2 : Del**(+2), in which the non-dimensional spectral          SPDEL2.43    
C                coefficient Z(n,m) is multiplied by -[n*(n+1)].           SPDEL2.44    
C     ITYPE=-2 : Del**(-2), in which the non-dimensional spectral          SPDEL2.45    
C                coefficient Z(n,m) is divided by -[n*(n+1)].              SPDEL2.46    
C                                                                          SPDEL2.47    
C     Author:                                                              SPDEL2.48    
C        Original version.                    Mike Blackburn,  25.11.94.   SPDEL2.49    
C        FILT is now a dummy array, (0:NN).   Mike Blackburn,  04.09.96.   SPDEL2.50    
C                                                                          SPDEL2.51    
      COMPLEX Z(NWJ2,NHEM,NL)                                              SPDEL2.52    
      REAL FILT(0:NN)                                                      SPDEL2.53    
C                                                                          SPDEL2.54    
C     Set up filter coefficients.                                          SPDEL2.55    
C     Note that FILT(n)=-n*(n+1) (or its inverse).                         SPDEL2.56    
C                                                                          SPDEL2.57    
      IF (ITYPE.EQ.2) THEN                                                 SPDEL2.58    
         DO 10 N=0,NN                                                      SPDEL2.59    
            FILT(N)=-FLOAT(N*(N+1))                                        SPDEL2.60    
   10    CONTINUE                                                          SPDEL2.61    
      ELSE IF (ITYPE.EQ.-2) THEN                                           SPDEL2.62    
         DO 20 N=0,NN                                                      SPDEL2.63    
            FILT(N)=-1./FLOAT(N*(N+1))                                     SPDEL2.64    
   20    CONTINUE                                                          SPDEL2.65    
      ELSE                                                                 SPDEL2.66    
         PRINT *,' ***SPDEL2: INVALID VALUE OF ITYPE SUPPLIED = '          SPDEL2.67    
     :          ,ITYPE,' : MUST BE +-2 FOR DEL**(+-2) OPERATION'           SPDEL2.68    
         CALL ABORT                                                        SPDEL2.69    
      ENDIF                                                                SPDEL2.70    
C                                                                          SPDEL2.71    
C     Del**(+-2) operation.                                                SPDEL2.72    
C     Counting for total wavenumber in inner loop is for even coeffs,      SPDEL2.73    
C     NOF increases total wavenumber for odd coeffs.                       SPDEL2.74    
C                                                                          SPDEL2.75    
      DO 30 IHEM=1,NHEM                                                    SPDEL2.76    
         NOF=1-MOD(IHEM+IPAR,2)                                            SPDEL2.77    
         DO 30 L=1,NL                                                      SPDEL2.78    
            I=0                                                            SPDEL2.79    
            DO 30 M=0,MM-1,MOCT                                            SPDEL2.80    
               DO 30 N=M,NN-1,2                                            SPDEL2.81    
                  I=I+1                                                    SPDEL2.82    
                  Z(I,IHEM,L)=Z(I,IHEM,L)*FILT(N+NOF)                      SPDEL2.83    
   30 CONTINUE                                                             SPDEL2.84    
C                                                                          SPDEL2.85    
C     Check that inner loop count is NWJ2.                                 SPDEL2.86    
C                                                                          SPDEL2.87    
      IF (I.NE.NWJ2) THEN                                                  SPDEL2.88    
         PRINT *,' ***SPDEL2: ERROR IN WAVENUMBER COUNTING: FINAL I = '    SPDEL2.89    
     :          ,I,' SHOULD EQUAL NWJ2 = ',NWJ2                            SPDEL2.90    
         CALL ABORT                                                        SPDEL2.91    
      ENDIF                                                                SPDEL2.92    
C                                                                          SPDEL2.93    
      RETURN                                                               SPDEL2.94    
      END                                                                  SPDEL2.95    
C     ******************************************************************   SPDEL2.96    
      SUBROUTINE LTI
                                                                           LTIUV.3     
C     Inverse Legendre transform for the diabatic part of the timestep.    LTIUV.4     
C     Transforms from spectral to Fourier space at the current latitude    LTIUV.5     
C     (pair).  In a global run the resulting arrays are complete           LTIUV.6     
C     (i.e. even+odd) Fourier coefficients at the northern & southern      LTIUV.7     
C     hemisphere rows.                                                     LTIUV.8     
C                                                                          LTIUV.9     
C     Calls the modular routine HEXP to transform U and V fields.          LTIUV.10    
                                                                           LTIUV.11    
C                                                                          PARAM1.2     
C     Determines model resolution                                          PARAM1.3     
C                                                                          PARAM1.4     
      PARAMETER(NN=21,MM=21,NHEM=2,NL=5,MOCT=1,MG=128,JG=32,NWJ2=121        G100.1     
     P         ,NCRAY=64,JGL=JG)                                           G100.2     
C                                                                          PARAM1.7     
C                                                                          PARAM2.2     
C     Sets basic constants, especially those needed for array dimensions   PARAM2.3     
C                                                                          PARAM2.4     
      PARAMETER(MH=2,PI=3.14159265359,PI2=2.0*PI                           PARAM2.5     
     +,NNP=NN+1,MGPP=MG+2,JGP=JG+1,JGG=JG*NHEM,JGGP=JGG+1,MJP=NWJ2+NWJ2    PARAM2.6     
     +,NLM=NL-1,NLP=NL+1,NLPP=NL+2,NLA=NL+3,NLB=NL+4,NL2=NL*NL             PARAM2.7     
     +,IDA=(MG+MG+MG)/2+1,IDB=NWJ2*NL,IDC=IDB+IDB,IDD=MGPP*NL              PARAM2.8     
     +,IDE=NL2*NN,IDF=NCRAY*(MG+1),IDG=JG*NL,IDH=JG*MG                     PARAM2.9     
     +,IDI=NNP/2,IDJ=IDI*IDI,IDK=NL*IDI,IDL=MGPP/2,IDM=NNP/2,IDN=IDM*NL    PARAM2.10    
     +,NWW=1+(MM-1)/MOCT)                                                  PARAM2.11    
      PARAMETER(IGA=NWJ2*NHEM,IGB=IDB*NHEM,IGC=MGPP*NHEM,IGD=IDD*NHEM      PARAM2.12    
     +,IGG=IDG*NHEM,IGL=IDL*NHEM,IGM=IDM*NHEM,IGN=IDN*NHEM                 PARAM2.13    
     +,IGO=IGA+IGA,IGP=IGB+IGB,NFTWG=5*NL+3,NFTGW=6*NL+2)                  PARAM2.14    
C                                                                          PARAM2.15    
C                                                                          BLANK.2     
C     Basic planetary parameters for run plus information about            BLANK.3     
C     vertical grid structure                                              BLANK.4     
C                                                                          BLANK.5     
      COMMON        SQ(NNP),RSQ(NNP),SIGMAH(NLM),SIGMA(NL)                 BLANK.6     
     +              ,T01S2(NLM),T0(NL),ALPHA(NL),DSIGMA(NL),RDSIG(NL)      BLANK.7     
     +              ,TKP(NL),C(NL2),SQH(NNP)                               BLANK.8     
     +              ,MF,MFP,JZF,NF,NFP                                     BLANK.9     
     +              ,AKAP,GA,GASCON,RADEA,WW,PFAC,EZ,AIOCT                 BLANK.10    
     +              ,LRSTRT,LSHORT,LTVEC,LSTRETCH                          BLANK.11    
     +              ,LBALAN,LRESTIJ                                        BLANK.12    
     +              ,LNOISE                                                SC970203.1     
     +              ,LGPDAMP,LFCE,LFED,LFAN,LLIN,LMODE,LTRAIN              NICK.1     
      COMPLEX EZ,AIOCT                                                     BLANK.13    
      LOGICAL LRSTRT,LSHORT,LTVEC,LSTRETCH,LBALAN,LRESTIJ                  BLANK.14    
     +       ,LNOISE                                                       SC970203.2     
     +       ,LGPDAMP,LFCE,LFED,LFAN,LLIN,LMODE,LTRAIN                     NICK.2     
C                                                                          BLANK.15    
C                                                                          GRIDP3.2     
C     Array ordering in GRIDP must correspond to that in SPECTR.           GRIDP3.3     
C     Complex arrays: multi-level arrays are 2-dimensional.                GRIDP3.4     
C                                                                          GRIDP3.5     
C                                                                          SPECTR.2     
C     Array ordering in SPECTR must correspond to that in GRIDP.           SPECTR.3     
C                                                                          SPECTR.4     
      COMMON/SPECTR/Z(IGB),D(IGB),T(IGB),SP(IGA)
      COMPLEX Z,D,T,SP
C                                                                          SPECTR.9     
C                                                                          GRIDP.2     
C     Array ordering in GRIDP must correspond to that in SPECTR.           GRIDP.3     
C     Real arrays: multi-level arrays are 1-dimensional.                   GRIDP.4     
C                                                                          GRIDP.5     
      COMMON/GRIDP/ CHIG(IGL,NL),SFG(IGL,NL),ZG(IGL,NL),DG(IGL,NL)
     *              ,TG(IGL,NL),SPG(IGL),DGT(IGL,NL),ZGT(IGL,NL)
      COMPLEX CHIG,SFG,ZG,DG,TG,SPG,DGT,ZGT

      COMMON/LEGAU/ ALPJ(MJP),DALPJ(MJP)                                   LEGAU.5     
     +              ,ALP(NWJ2,2,JGL),DALP(NWJ2,2,JGL)                      LEGAU.6     
     +              ,RLP(NWJ2,2,JGL),RDLP(NWJ2,2,JGL)                      LEGAU.7     
     +              ,SI(JGG),CS(JGG),SISQ(JGG),CSSQ(JGG),SECSQ(JGG)        LEGAU.8     
     +              ,ALAT(JGG),GWT(JGG),AW(JGG),JH,JL,JINC                 LEGAU.9     
C                                                                          LEGAU.10    
C                                                                          POLYNO.2     
C     Polynomial used to aid vectorization of Legendre transforms          POLYNO.3     
C                                                                          POLYNO.4     
      COMMON/POLYNO/POLY(NWJ2,2,4),CMPA(IGL)                               POLYNO.5     
      COMPLEX CMPA                                                         POLYNO.6     
C                                                                          POLYNO.7     
C                                                                          SPECTR.2     
                                                                           LTIUV.19    
C     Preset Fourier arrays.                                               LTIUV.20    
                                                                           LTIUV.21    
      DO 10 L=1,NL                                                         LTIUV.22    
         DO 10 I=1,IGL                                                     LTIUV.23    
            CHIG(I,L)=0.                                                   LTIUV.24    
            SFG(I,L)=0.                                                    LTIUV.25    
            ZG(I,L)=0.                                                     LTIUV.26    
            DG(I,L)=0.                                                     LTIUV.27    
            TG(I,L)=0.
            SPG(I)=0.
   10 CONTINUE                                                             LTIUV.28    
                                                                           LTIUV.29    
C        Wind components: calls to HEXP give following Fourier fields:     LTIUV.37    
C           SFG  :   streamfunction.                                       LTIUV.38    
C           CHIG :   velocity potential.                                   LTIUV.39    
C           UG   :   -U(rotational).                                       LTIUV.40    
C           VG   :   V(divergent).                                         LTIUV.41    
                                                                           LTIUV.42    
         CALL HEXP(Z,ZG  ,NL,2)                                            LTIUV.45    
         CALL HEXP(D,DG  ,NL,2)                                            LTIUV.46    
         CALL HEXP(T,TG  ,NL,2)
         CALL HEXP(SP,SPG, 1,2)                                            LTIUV.46    
                                                                           LTIUV.47    
C     Sum to give total winds.  CMPA takes x-derivative.                   LTIUV.54    
                                                                           LTIUV.55    
      RETURN                                                               LTIUV.62    
      END                                                                  LTIUV.63    

      SUBROUTINE LTD
                                                                           LTDUV.3     
C     Direct Legendre transform for the diabatic part of the timestep.     LTDUV.4     
C     Transforms from Fourier to spectral space at the current latitude    LTDUV.5     
C     (pair).  In a global run the input arrays are complete (even+odd)    LTDUV.6     
C     Fourier coefficients at the northern & southern hemisphere rows.     LTDUV.7     
C                                                                          LTDUV.8     
C     Calls the modular routine HANAL for tendencies of U and V to         LTDUV.9     
C     give grid point contributions to ZT and DT.                          LTDUV.10    
C                                                                          LTDUV.11    
C     The Fourier work array passed to HANAL must be dimensioned with      LTDUV.12    
C     (at least) the maximum number of levels used in the HANAL calls.     LTDUV.13    
                                                                           LTDUV.14    
C                                                                          PARAM1.2     
C     Determines model resolution                                          PARAM1.3     
C                                                                          PARAM1.4     
      PARAMETER(NN=21,MM=21,NHEM=2,NL=5,MOCT=1,MG=128,JG=32,NWJ2=121        G100.1     
     P         ,NCRAY=64,JGL=JG)                                           G100.2     
C                                                                          PARAM1.7     
C                                                                          PARAM2.2     
C     Sets basic constants, especially those needed for array dimensions   PARAM2.3     
C                                                                          PARAM2.4     
      PARAMETER(MH=2,PI=3.14159265359,PI2=2.0*PI                           PARAM2.5     
     +,NNP=NN+1,MGPP=MG+2,JGP=JG+1,JGG=JG*NHEM,JGGP=JGG+1,MJP=NWJ2+NWJ2    PARAM2.6     
     +,NLM=NL-1,NLP=NL+1,NLPP=NL+2,NLA=NL+3,NLB=NL+4,NL2=NL*NL             PARAM2.7     
     +,IDA=(MG+MG+MG)/2+1,IDB=NWJ2*NL,IDC=IDB+IDB,IDD=MGPP*NL              PARAM2.8     
     +,IDE=NL2*NN,IDF=NCRAY*(MG+1),IDG=JG*NL,IDH=JG*MG                     PARAM2.9     
     +,IDI=NNP/2,IDJ=IDI*IDI,IDK=NL*IDI,IDL=MGPP/2,IDM=NNP/2,IDN=IDM*NL    PARAM2.10    
     +,NWW=1+(MM-1)/MOCT)                                                  PARAM2.11    
      PARAMETER(IGA=NWJ2*NHEM,IGB=IDB*NHEM,IGC=MGPP*NHEM,IGD=IDD*NHEM      PARAM2.12    
     +,IGG=IDG*NHEM,IGL=IDL*NHEM,IGM=IDM*NHEM,IGN=IDN*NHEM                 PARAM2.13    
     +,IGO=IGA+IGA,IGP=IGB+IGB,NFTWG=5*NL+3,NFTGW=6*NL+2)                  PARAM2.14    
C                                                                          PARAM2.15    
C                                                                          BLANK.2     
C     Basic planetary parameters for run plus information about            BLANK.3     
C     vertical grid structure                                              BLANK.4     
C                                                                          BLANK.5     
      COMMON        SQ(NNP),RSQ(NNP),SIGMAH(NLM),SIGMA(NL)                 BLANK.6     
     +              ,T01S2(NLM),T0(NL),ALPHA(NL),DSIGMA(NL),RDSIG(NL)      BLANK.7     
     +              ,TKP(NL),C(NL2),SQH(NNP)                               BLANK.8     
     +              ,MF,MFP,JZF,NF,NFP                                     BLANK.9     
     +              ,AKAP,GA,GASCON,RADEA,WW,PFAC,EZ,AIOCT                 BLANK.10    
     +              ,LRSTRT,LSHORT,LTVEC,LSTRETCH                          BLANK.11    
     +              ,LBALAN,LRESTIJ                                        BLANK.12    
     +              ,LNOISE                                                SC970203.1     
     +              ,LGPDAMP,LFCE,LFED,LFAN,LLIN,LMODE,LTRAIN              NICK.1     
      COMPLEX EZ,AIOCT                                                     BLANK.13    
      LOGICAL LRSTRT,LSHORT,LTVEC,LSTRETCH,LBALAN,LRESTIJ                  BLANK.14    
     +       ,LNOISE                                                       SC970203.2     
     +       ,LGPDAMP,LFCE,LFED,LFAN,LLIN,LMODE,LTRAIN                     NICK.2     
C                                                                          BLANK.15    
C                                                                          GRIDP3.2     

C                                                                          SPECTR.2     
C     Array ordering in SPECTR must correspond to that in GRIDP.           SPECTR.3     
C                                                                          SPECTR.4     
      COMMON/SPECTR/Z(IGB),D(IGB),T(IGB),SP(IGA)
      COMPLEX Z,D,T,SP
C                                                                          SPECTR.9     
C                                                                          GRIDP.2     
C     Array ordering in GRIDP must correspond to that in SPECTR.           GRIDP.3     
C     Real arrays: multi-level arrays are 1-dimensional.                   GRIDP.4     
C                                                                          GRIDP.5     
      COMMON/GRIDP/ CHIG(IGL,NL),SFG(IGL,NL),ZG(IGL,NL),DG(IGL,NL)
     *              ,TG(IGL,NL),SPG(IGL),DGT(IGL,NL),ZGT(IGL,NL)
      COMPLEX CHIG,SFG,ZG,DG,TG,SPG,DGT,ZGT

C                                                                          LEGAU.2     
C     Legendre polynomials and information about gaussian latitudes        LEGAU.3     
C                                                                          LEGAU.4     
      COMMON/LEGAU/ ALPJ(MJP),DALPJ(MJP)                                   LEGAU.5     
     +              ,ALP(NWJ2,2,JGL),DALP(NWJ2,2,JGL)                      LEGAU.6     
     +              ,RLP(NWJ2,2,JGL),RDLP(NWJ2,2,JGL)                      LEGAU.7     
     +              ,SI(JGG),CS(JGG),SISQ(JGG),CSSQ(JGG),SECSQ(JGG)        LEGAU.8     
     +              ,ALAT(JGG),GWT(JGG),AW(JGG),JH,JL,JINC                 LEGAU.9     
C                                                                          LEGAU.10    
C                                                                          POLYNO.2     
C     Polynomial used to aid vectorization of Legendre transforms          POLYNO.3     
C                                                                          POLYNO.4     
      COMMON/POLYNO/POLY(NWJ2,2,4),CMPA(IGL)                               POLYNO.5     
      COMPLEX CMPA                                                         POLYNO.6     
C                                                                          POLYNO.7     
C                                                                          SPECTR.2     
C                                                                          SPECTR.9     
                                                                           LTDUV.22    
      COMPLEX GWORK(IGL,NL)                                                LTDUV.23    
                                                                           LTDUV.24    
C     Prepare Fourier arrays:
C     - change sign of terms which contribute negatively
C     - apply (1-mu**2) weighting,
C     - take zonal derivatives,
C     - make copies
                                                                           LTDUV.30    
C     legendre transfroms to give T and SP                                LTDUV.39    
	 CALL HANAL(ZG, GWORK,Z,NL,2)
	 CALL HANAL(DG, GWORK,D,NL,2)
         CALL HANAL(TG,GWORK,T,NL,2)                                     LTDUV.41    
         CALL HANAL(SPG,GWORK,SP,1,2)                                     LTDUV.41    


      RETURN                                                               LTDUV.46    
      END                                                                  LTDUV.47    
C***************************************************************

      SUBROUTINE SLICE(JH,XG,XXG,NNL)

C     Determines model resolution                                          PARAM1.3     
C                                                                          PARAM1.4     
      PARAMETER(NN=21,MM=21,NHEM=2,NL=5,MOCT=1,MG=128,JG=32,NWJ2=121        G100.1     
     P         ,NCRAY=64,JGL=JG)                                           G100.2     
C                                                                          PARAM1.7     
C                                                                          PARAM2.2     
C     Sets basic constants, especially those needed for array dimensions   PARAM2.3     
C                                                                          PARAM2.4     
      PARAMETER(MH=2,PI=3.14159265359,PI2=2.0*PI                           PARAM2.5     
     +,NNP=NN+1,MGPP=MG+2,JGP=JG+1,JGG=JG*NHEM,JGGP=JGG+1,MJP=NWJ2+NWJ2    PARAM2.6     
     +,NLM=NL-1,NLP=NL+1,NLPP=NL+2,NLA=NL+3,NLB=NL+4,NL2=NL*NL             PARAM2.7     
     +,IDA=(MG+MG+MG)/2+1,IDB=NWJ2*NL,IDC=IDB+IDB,IDD=MGPP*NL              PARAM2.8     
     +,IDE=NL2*NN,IDF=NCRAY*(MG+1),IDG=JG*NL,IDH=JG*MG                     PARAM2.9     
     +,IDI=NNP/2,IDJ=IDI*IDI,IDK=NL*IDI,IDL=MGPP/2,IDM=NNP/2,IDN=IDM*NL    PARAM2.10    
     +,NWW=1+(MM-1)/MOCT)                                                  PARAM2.11    
      PARAMETER(IGA=NWJ2*NHEM,IGB=IDB*NHEM,IGC=MGPP*NHEM,IGD=IDD*NHEM      PARAM2.12    
     +,IGG=IDG*NHEM,IGL=IDL*NHEM,IGM=IDM*NHEM,IGN=IDN*NHEM                 PARAM2.13    
     +,IGO=IGA+IGA,IGP=IGB+IGB,NFTWG=5*NL+3,NFTGW=6*NL+2)                  PARAM2.14    

      REAL XG(IGC,NL),XXG(MG,JGG,NL)

C     Transfer grid data to latitude slice format
      JJ=JGGP-JH

      DO 10 L=1,NNL

      DO 20 I=1,MG
 20   XG(I,L)=XXG(I,JH,L)

      DO 30 I=MG+1,MGPP
 30   XG(I,L)=0.

      DO 40 I=MGPP+1,IGC-2
      II=I-MGPP
 40   XG(I,L)=XXG(II,JJ,L)

      DO 50 I=IGC-1,IGC
 50   XG(I,L)=0.

 10   CONTINUE

      RETURN
      END
C***************************************************************

      SUBROUTINE SHUFFLE(JH,XG,XXG,NNL)

C     Determines model resolution                                          PARAM1.3     
C                                                                          PARAM1.4     
      PARAMETER(NN=21,MM=21,NHEM=2,NL=5,MOCT=1,MG=128,JG=32,NWJ2=121        G100.1     
     P         ,NCRAY=64,JGL=JG)                                           G100.2     
C                                                                          PARAM1.7     
C                                                                          PARAM2.2     
C     Sets basic constants, especially those needed for array dimensions   PARAM2.3     
C                                                                          PARAM2.4     
      PARAMETER(MH=2,PI=3.14159265359,PI2=2.0*PI                           PARAM2.5     
     +,NNP=NN+1,MGPP=MG+2,JGP=JG+1,JGG=JG*NHEM,JGGP=JGG+1,MJP=NWJ2+NWJ2    PARAM2.6     
     +,NLM=NL-1,NLP=NL+1,NLPP=NL+2,NLA=NL+3,NLB=NL+4,NL2=NL*NL             PARAM2.7     
     +,IDA=(MG+MG+MG)/2+1,IDB=NWJ2*NL,IDC=IDB+IDB,IDD=MGPP*NL              PARAM2.8     
     +,IDE=NL2*NN,IDF=NCRAY*(MG+1),IDG=JG*NL,IDH=JG*MG                     PARAM2.9     
     +,IDI=NNP/2,IDJ=IDI*IDI,IDK=NL*IDI,IDL=MGPP/2,IDM=NNP/2,IDN=IDM*NL    PARAM2.10    
     +,NWW=1+(MM-1)/MOCT)                                                  PARAM2.11    
      PARAMETER(IGA=NWJ2*NHEM,IGB=IDB*NHEM,IGC=MGPP*NHEM,IGD=IDD*NHEM      PARAM2.12    
     +,IGG=IDG*NHEM,IGL=IDL*NHEM,IGM=IDM*NHEM,IGN=IDN*NHEM                 PARAM2.13    
     +,IGO=IGA+IGA,IGP=IGB+IGB,NFTWG=5*NL+3,NFTGW=6*NL+2)                  PARAM2.14    

      REAL XG(IGC,NL),XXG(MG,JGG,NL)

C     Transfer grid data from latitude slice format
      JJ=JGGP-JH

      DO 10 L=1,NNL

      DO 20 I=1,MG
      II=I+MGPP
      XXG(I,JH,L)=XG(I,L)
 20   XXG(I,JJ,L)=XG(II,L)

 10   CONTINUE

      RETURN
      END
C***************************************************************

      SUBROUTINE WRTLVLS(XXG,NNL)

C     Determines model resolution                                          PARAM1.3     
C                                                                          PARAM1.4     
      PARAMETER(NN=21,MM=21,NHEM=2,NL=5,MOCT=1,MG=128,JG=32,NWJ2=121        G100.1     
     P         ,NCRAY=64,JGL=JG)                                           G100.2     
C                                                                          PARAM1.7     
C                                                                          PARAM2.2     
C     Sets basic constants, especially those needed for array dimensions   PARAM2.3     
C                                                                          PARAM2.4     
      PARAMETER(MH=2,PI=3.14159265359,PI2=2.0*PI                           PARAM2.5     
     +,NNP=NN+1,MGPP=MG+2,JGP=JG+1,JGG=JG*NHEM,JGGP=JGG+1,MJP=NWJ2+NWJ2    PARAM2.6     
     +,NLM=NL-1,NLP=NL+1,NLPP=NL+2,NLA=NL+3,NLB=NL+4,NL2=NL*NL             PARAM2.7     
     +,IDA=(MG+MG+MG)/2+1,IDB=NWJ2*NL,IDC=IDB+IDB,IDD=MGPP*NL              PARAM2.8     
     +,IDE=NL2*NN,IDF=NCRAY*(MG+1),IDG=JG*NL,IDH=JG*MG                     PARAM2.9     
     +,IDI=NNP/2,IDJ=IDI*IDI,IDK=NL*IDI,IDL=MGPP/2,IDM=NNP/2,IDN=IDM*NL    PARAM2.10    
     +,NWW=1+(MM-1)/MOCT)                                                  PARAM2.11    
      PARAMETER(IGA=NWJ2*NHEM,IGB=IDB*NHEM,IGC=MGPP*NHEM,IGD=IDD*NHEM      PARAM2.12    
     +,IGG=IDG*NHEM,IGL=IDL*NHEM,IGM=IDM*NHEM,IGN=IDN*NHEM                 PARAM2.13    
     +,IGO=IGA+IGA,IGP=IGB+IGB,NFTWG=5*NL+3,NFTGW=6*NL+2)                  PARAM2.14    

      REAL XXG(MG,JGG,NL),XG(MG,JGG)

C     writes out grid data one level at a time

      DO 10 L=1,NNL

      DO 20 J=1,JGG
      DO 20 I=1,MG
      XG(I,J)=XXG(I,J,L)
 20   CONTINUE
      WRITE(9)XG

 1    FORMAT(20(1X,F6.1))
      print*
      print*,'-----------------------------------------'
      print*
      DO J=1,JGG,4
      WRITE(6,1)(XG(I,J),I=1,MG,8)

      ENDDO

 10   CONTINUE

      RETURN
      END
