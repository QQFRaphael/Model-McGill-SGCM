       PROGRAM MLTRI                                                       MLTRI.2     
C                                                                          MLTRI.3     
C                                                                          MLTRI.4     
C     ATMOSPHERIC MODELLING GROUP      UNIVERSITY OF READING               MLTRI.5     
C                                                                          MLTRI.6     
C                                                                          MLTRI.7     
C                                                                          PARAM1.2     
C     Determines model resolution                                          PARAM1.3     
C                                                                          PARAM1.4     
      PARAMETER(NN=21,MM=21,NHEM=2,NL=5,MOCT=1,MG=64,JG=16,NWJ2=121        G100.1     
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
      COMMON/SPECTR/Z(IGB),D(IGB),T(IGB),SP(IGA),GS(IGA)                   SPECTR.5     
     *              ,SPA(IGA),VP(IGA),DTE(IGB),TT(IGB),DT(IGB),ZT(IGB)     SPECTR.6     
     *              ,ZMI(IGB),DMI(IGB),TMI(IGB),SPMI(IGA)                  SPECTR.7     
      COMPLEX Z,D,T,SP,GS,SPA,VP,DTE,TT,DT,ZT,ZMI,DMI,TMI,SPMI             SPECTR.8     
C                                                                          SPECTR.9     
C                                                                          GRIDP.2     
C     Array ordering in GRIDP must correspond to that in SPECTR.           GRIDP.3     
C     Real arrays: multi-level arrays are 1-dimensional.                   GRIDP.4     
C                                                                          GRIDP.5     
      COMMON/GRIDP/ CHIG(IGD),SFG(IGD),UG(IGD),VG(IGD)                     GRIDP.6     
     *              ,ZG(IGD),DG(IGD),TG(IGD)                               GRIDP.7     
     *              ,PLG(IGC),PJG(IGC),PMG(IGC)                            GRIDP.8     
     *              ,SPG(IGC),VPG(IGC),EG(IGD)                             GRIDP.9     
     *              ,TNLG(IGD),FUG(IGD),FVG(IGD),UTG(IGD)                  GRIDP.10    
     *              ,VTG(IGD),FVGT(IGD),FUGT(IGD)                          GRIDP.11    
C                                                                          GRIDP.12    
C                                                                          BATS.2     
C     Constant arrays and variables associated with time and vertical      BATS.3     
C     differencing. Also counters.                                         BATS.4     
C                                                                          BATS.5     
      COMMON/BATS/  BM1(IDE),AK(NNP),AQ(NL2),G(NL2),TAU(NL2)               BATS.6     
     +              ,KOUNT,KITS,KTOTAL,KRUN,BEGDAY,ITSPD,PNU21             BATS.7     
     +              ,DELT,DELT2,CV,CG,CT,PNU,PNU2                          BATS.8     
C                                                                          BATS.9     
C                                                                          LEGAU.2     
C     Legendre polynomials and information about gaussian latitudes        LEGAU.3     
C                                                                          LEGAU.4     
      COMMON/LEGAU/ ALPJ(MJP),DALPJ(MJP)                                   LEGAU.5     
     +              ,ALP(NWJ2,2,JGL),DALP(NWJ2,2,JGL)                      LEGAU.6     
     +              ,RLP(NWJ2,2,JGL),RDLP(NWJ2,2,JGL)                      LEGAU.7     
     +              ,SI(JGG),CS(JGG),SISQ(JGG),CSSQ(JGG),SECSQ(JGG)        LEGAU.8     
     +              ,ALAT(JGG),GWT(JGG),AW(JGG),JH,JL,JINC                 LEGAU.9     
C                                                                          LEGAU.10    
C                                                                          OUTCON.2     
C     Switches counters and constants controlling type and frequency of    OUTCON.3     
C     model output                                                         OUTCON.4     
C                                                                          OUTCON.5     
      COMMON/OUTCON/RNTAPE,NCOEFF,NLAT,INLAT,INSPC                         OUTCON.6     
     +              ,KOUNTP,KOUNTE,KOUNTH,KOUNTR                           OUTCON.7     
     +              ,KOUTP,KOUTE,KOUTH,KOUTR,DAY                           OUTCON.8     
     +              ,SQR2,RSQR2,EAM1,EAM2,TOUT1,TOUT2,RMG                  OUTCON.9     
     +              ,LSPO(NL),LGPO(NL)                                     OUTCON.10    
      LOGICAL LSPO,LGPO                                                    OUTCON.11    
C                                                                          OUTCON.12    
C                                                                          COMFFT.2     
C     Constants and arrays needed for the fast Fourier transforms          COMFFT.3     
C                                                                          COMFFT.4     
      COMMON/COMFFT/NTGW,NRSTGW,NTWG,NRSTWG                                COMFFT.5     
     +              ,TRIG(IDA),WORK(IDF),IFAX(10)                          COMFFT.6     
C                                                                          COMFFT.7     
C                                                                          POLYNO.2     
C     Polynomial used to aid vectorization of Legendre transforms          POLYNO.3     
C                                                                          POLYNO.4     
      COMMON/POLYNO/POLY(NWJ2,2,4),CMPA(IGL)                               POLYNO.5     
      COMPLEX CMPA                                                         POLYNO.6     
C                                                                          POLYNO.7     
C                                                                          RESTOR.2     
C     Restoration fields and timescale                                     RESTOR.3     
C                                                                          RESTOR.4     
      COMMON/RESTOR/ZRES(IGN),DRES(IGN),TRES(IGN),SPRES(IGM),DAMP          RESTOR.5     
     &,ZFCE(IGB),DFCE(IGB),TFCE(IGB),SPFCE(IGA)                            NICK.207   
     &,ZFED(IGB),DFED(IGB),TFED(IGB),SPFED(IGA)                            NICK.208   
     &,ZFAN(IGB),DFAN(IGB),TFAN(IGB),SPFAN(IGA)                            NICK.209   
     &,ZDMP(IGB),DDMP(IGB),TDMP(IGB),SPDMP(IGA)                            NICK.210   
     &,ZDMI(IGB),DDMI(IGB),TDMI(IGB),SPDMI(IGA)                            NICK.211   
     &,MASK(IGC,JG),GG(IGC,JG),CD(IGC,JG),AA1,AA2,AAT                      NICK.212   
      COMPLEX ZFCE,DFCE,TFCE,SPFCE,ZFED,DFED,TFED,SPFED                    NICK.213   
     &,ZFAN,DFAN,TFAN,SPFAN                                                NICK.214   
     &,ZDMP,DDMP,TDMP,SPDMP,ZDMI,DDMI,TDMI,SPDMI                           NICK.215   
C                                                                          RESTOR.6     
C                                                                          RESTIJ.2     
C     Restoration temperature field and constants which determine it,      RESTIJ.3     
C     also contains timescales                                             RESTIJ.4     
C                                                                          RESTIJ.5     
      COMMON/RESTIJ/TTRES(IGB)                                             RESTIJ.6     
     + ,DTNS,DTEP,DTTRP,FAC(NL),DDAMP(NL),TFRC(NL),YRLEN,TRS(NL)           RESTIJ.7     
     +  ,ALR,ZTROP,TGR                                                     RESTIJ.8     
      COMPLEX TTRES                                                        RESTIJ.9     
C                                                                          BALAN.2     
C     Constants and arrays needed for balancing                            BALAN.3     
C                                                                          BALAN.4     
      COMMON/BALAN/BFILT(NL),RGT0(NL),RG(NL2),TMEAN(NL)                    BALAN.5     
     +            ,EP1(IGA),EP2(IGA),KBAL,MFTBAL,SRGT0,LTBAL               BALAN.6     
      LOGICAL LTBAL                                                        BALAN.7     
C                                                                          BALAN.8     
      DIMENSION DAG(IDDAG),DAF(IDDAF)                                      MLTRI.22    
      DIMENSION DAGUV(2*IGD),DAFUV(2*IGD)                                  NICK.135   
      EQUIVALENCE (DAG(1),UG(1)),(DAF(1),SPG(1))                           MLTRI.23    
      EQUIVALENCE (DAGUV(1),UG(1)),(DAFUV(1),FUG(1))                       NICK.136   
C                                                                          MLTRI.24    
 2000 FORMAT(/' RESTART RECORD WRITTEN TO CHANNEL ',I3,/                   MLTRI.25    
     +        ' RKOUNT  RNTAPE  DAY  =',3F12.3)                            MLTRI.26    
 2010 FORMAT(/' HISTORY RECORD WRITTEN TO CHANNEL ',I3,/                   MLTRI.27    
     +        ' RKOUNT  RNTAPE  DAY  =',3F12.3)                            MLTRI.28    
 2020 FORMAT(/' RESTORATION RECORD WRITTEN TO CHANNEL ',I3,/               MLTRI.29    
     +        ' RKOUNT  RNTAPE  DAY  =',3F12.3)                            MLTRI.30    
C                                                                          MLTRI.31    
      REWIND 9                                                             NICK.95    
      LTRAIN=.FALSE.                                                       NICK.96    
      KTRAIN=0                                                             NICK.97    
                                                                           NICK.98    
C*****following calls are in place of the pointless routine, INITAL        NICK.99    
                                                                           NICK.100   
      CALL INISET                                                          NICK.101   
      CALL INIGAU                                                          NICK.102   
      CALL INISI                                                           NICK.103   
                                                                           NICK.104   
  998 FORMAT(1X,5(1X,F6.4))                                                NICK.105   
      PRINT*,'G MATRIX....'                                                NICK.106   
      PRINT*                                                               NICK.107   
      DO III=0,NL2-NL,NL                                                   NICK.108   
      WRITE(6,998)(G(III+J),J=1,NL)                                        NICK.109   
      ENDDO                                                                NICK.110   
      PRINT*                                                               NICK.111   
      PRINT*,'SCALED TAU MATRIX....'                                       NICK.112   
      PRINT*                                                               NICK.113   
      DO III=0,NL2-NL,NL                                                   NICK.114   
      WRITE(6,998)(TAU(III+J)/T0(1)/AKAP,J=1,NL)                           NICK.115   
      ENDDO                                                                NICK.116   
      PRINT*                                                               NICK.117   
                                                                           NICK.118   
      CALL INIRES                                                          NICK.119   
                                                                           NICK.120   
  111 CONTINUE                                                             NICK.121   
                                                                           NICK.122   
      CALL INISTR(KTRAIN)                                                  NICK.123   
                                                                           NICK.124   
      IF (LTRAIN) THEN                                                     NICK.125   
      KTRAIN=KTRAIN+1                                                      NICK.126   
      PRINT*                                                               NICK.127   
      PRINT*,'******************************'                              NICK.128   
      PRINT*,'MAKING ',KTRAIN,' TH FORECAST'                               NICK.129   
      PRINT*,'******************************'                              NICK.130   
      PRINT*                                                               NICK.131   
      END IF                                                               NICK.132   
 1    CONTINUE                                                             MLTRI.33    
C                                                                          MLTRI.34    
C     Adiabatic part of timestep. Preset tendencies to zero.               MLTRI.35    
C                                                                          MLTRI.36    
      DO 31 I=1,IGA                                                        MLTRI.37    
         VP(I) =0.0                                                        MLTRI.38    
         SPA(I)=0.0                                                        MLTRI.39    
 31   CONTINUE                                                             MLTRI.40    
      DO 32 I=1,IGB                                                        MLTRI.41    
         ZT(I)=0.0                                                         MLTRI.42    
         DT(I)=0.0                                                         MLTRI.43    
         TT(I)=0.0                                                         MLTRI.44    
         DTE(I)=0.0                                                        MLTRI.45    
 32   CONTINUE                                                             MLTRI.46    
C                                                                          MLTRI.47    
      IF (KOUNT.EQ.0) THEN                                                 MLTRI.48    
C                                                                          MLTRI.49    
C        Add white noise perturbation                                      MLTRI.50    
C                                                                          MLTRI.51    
         IF (LNOISE.AND..NOT.LRSTRT) CALL NOISE                            SC970203.5     
C                                                                          MLTRI.53    
C                                                                          MLTRI.57    
      ENDIF                                                                MLTRI.58    
C                                                                          MLTRI.59    
      IF (JGL.EQ.1) REWIND 25                                              MLTRI.60    
      KKOUT=KOUNT*(KOUNTP-KOUTP)                                           MLTRI.61    
      JL=1                                                                 MLTRI.62    
C                                                                          MLTRI.63    
C     Main loop over latitudes                                             MLTRI.64    
C                                                                          MLTRI.65    
      DO 5 IH=1,JG                                                         MLTRI.66    
         JH=IH                                                             MLTRI.67    
         IF(JGL.EQ.1) READ(25) ALP,DALP,RLP,RDLP                           MLTRI.68    
C                                                                          MLTRI.69    
C        Go from spectral space to grid point space using                  MLTRI.70    
C        inverse Legendre and Fourier transforms                           MLTRI.71    
C                                                                          MLTRI.72    
         CALL LTI                                                          MLTRI.73    
         DO 10 I=1,NTWG                                                    MLTRI.74    
            CALL FFT991(DAG(1+(I-1)*NCRAY*MGPP),WORK,TRIG,IFAX,1,MGPP,MG   MLTRI.75    
     +                 ,NCRAY,1)                                           MLTRI.76    
 10      CONTINUE                                                          MLTRI.77    
         CALL FFT991(DAG(1+ NTWG*NCRAY*MGPP),WORK,TRIG,IFAX,1,MGPP,MG      MLTRI.78    
     +              ,NRSTWG,1)                                             MLTRI.79    
C                                                                          MLTRI.80    
C        Calculate nonlinear terms                                         MLTRI.81    
C                                                                          MLTRI.82    
         CALL MGRMLT                                                       MLTRI.83    
C                                                                          MLTRI.84    
C        Save grid point fields for use in XSECT                           MLTRI.85    
C                                                                          MLTRI.86    
         IF (KKOUT.EQ.0.AND.NLAT.GT.0) WRITE(24)ZG,DG,UG,VG,TG,SPG         MLTRI.87    
C                                                                          MLTRI.88    
C        Go from grid point space to spectral space using                  MLTRI.89    
C        direct Legendre and Fourier transforms                            MLTRI.90    
C                                                                          MLTRI.91    
         DO 20 I=1,NTGW                                                    MLTRI.92    
            CALL FFT991(DAF(1+(I-1)*NCRAY*MGPP),WORK,TRIG,IFAX,1,MGPP,MG   MLTRI.93    
     +                 ,NCRAY,-1)                                          MLTRI.94    
 20      CONTINUE                                                          MLTRI.95    
         CALL FFT991(DAF(1+ NTGW*NCRAY*MGPP),WORK,TRIG,IFAX,1,MGPP,MG      MLTRI.96    
     +              ,NRSTGW,-1)                                            MLTRI.97    
         CALL LTD                                                          MLTRI.98    
         JL=JL+JINC                                                        MLTRI.99    
 5    CONTINUE                                                             MLTRI.100   
C                                                                          MLTRI.101   
      IF (LBALAN) THEN                                                     MLTRI.102   
C        Balance spectral fields                                           MLTRI.103   
C                                                                          MLTRI.104   
         IF (KOUNT.LT.0) THEN                                              MLTRI.105   
            KOUNT=KOUNT+1                                                  MLTRI.106   
            IF (.NOT.LTBAL) CALL BALANC                                    MLTRI.107   
            IF (     LTBAL) CALL TBAL                                      MLTRI.108   
            GO TO 1                                                        MLTRI.109   
         ENDIF                                                             MLTRI.110   
      ENDIF                                                                MLTRI.111   
C                                                                          MLTRI.112   
      IF (KKOUT.EQ.0.AND.NLAT.GT.0) REWIND 24                              MLTRI.113   
C                                                                          MLTRI.114   
C     First timestep - output history and diagnostics                      MLTRI.115   
C                                                                          MLTRI.116   
      IF (KOUNT.EQ.0) THEN                                                 MLTRI.117   
C     IF (LTRAIN.NE.1) REWIND 9                                            NICK.93    
      REWIND 9                                                             NICK.94    
         RKOUNT=KOUNT                                                      MLTRI.119   
         WRITE(9)RKOUNT,RNTAPE,DAY,Z,D,T,SP,RNTAPE                         MLTRI.120   
         WRITE(2,2010)9,RKOUNT,RNTAPE,DAY                                  MLTRI.121   
         IF (LRESTIJ) THEN                                                 MLTRI.122   
            WRITE(13)RKOUNT,RNTAPE,DAY,TTRES,RNTAPE                        MLTRI.123   
            WRITE(2,2020)13,RKOUNT,RNTAPE,DAY                              MLTRI.124   
         ENDIF                                                             MLTRI.125   
         CALL XSECT(INLAT)                                                 MLTRI.126   
         CALL SPOP                                                         MLTRI.127   
         CALL ENERGY                                                       MLTRI.128   
      ENDIF                                                                MLTRI.129   
      IF (LRESTIJ) THEN                                                    MLTRI.130   
C                                                                          MLTRI.131   
C        Write a restoration record                                        MLTRI.132   
C                                                                          MLTRI.133   
         IF (KOUTH.EQ.KOUNTH.OR.KOUTR.EQ.KOUNTR) THEN                      MLTRI.134   
            RKOUNT=KOUNT                                                   MLTRI.135   
            WRITE(13)RKOUNT,RNTAPE,DAY,TTRES,RNTAPE                        MLTRI.136   
            WRITE(2,2020)13,RKOUNT,RNTAPE,DAY                              MLTRI.137   
         ENDIF                                                             MLTRI.138   
      ENDIF                                                                MLTRI.139   
C                                                                          MLTRI.140   
C     Write a restart record                                               MLTRI.141   
C                                                                          MLTRI.142   
      IF (KOUTR.EQ.KOUNTR) THEN                                            MLTRI.143   
         RKOUNT=KOUNT                                                      MLTRI.144   
         WRITE(11)RKOUNT,RNTAPE,DAY,Z,D,T,SP,RNTAPE                        MLTRI.145   
     +          ,ZMI,DMI,TMI,SPMI,RNTAPE                                   MLTRI.146   
         WRITE(2,2000)11,RKOUNT,RNTAPE,DAY                                 MLTRI.147   
         KOUTR=0                                                           MLTRI.148   
      ENDIF                                                                MLTRI.149   
C                                                                          MLTRI.150   
C     Write a history record                                               MLTRI.151   
C                                                                          MLTRI.152   
      IF (KOUTH.EQ.KOUNTH) THEN                                            MLTRI.153   
         RKOUNT=KOUNT                                                      MLTRI.154   
         WRITE(9)RKOUNT,RNTAPE,DAY,Z,D,T,SP,RNTAPE                         MLTRI.155   
         WRITE(2,2010)9,RKOUNT,RNTAPE,DAY                                  MLTRI.156   
      PRINT*,'-------------   DAY = ',NINT(DAY)                            NICK.63    
      PRINT*,'Z(100) = ',Z(100)                                            NICK.64    
         KOUTH=0                                                           MLTRI.157   
      END IF                                                               MLTRI.158   
C                                                                          MLTRI.159   
C     Output diagnostics                                                   MLTRI.160   
C                                                                          MLTRI.161   
      IF (KOUTP.EQ.KOUNTP) THEN                                            MLTRI.162   
         CALL XSECT(INLAT)                                                 MLTRI.163   
         CALL SPOP                                                         MLTRI.164   
         KOUTP=0                                                           MLTRI.165   
      END IF                                                               MLTRI.166   
      IF (KOUTE.EQ.KOUNTE) THEN                                            MLTRI.167   
         CALL ENERGY                                                       MLTRI.168   
         KOUTE=0                                                           MLTRI.169   
      ENDIF                                                                MLTRI.170   
C                                                                          MLTRI.171   
      IF (KOUNT.LT.KTOTAL) THEN                                            MLTRI.172   
         KOUTP=KOUTP+1                                                     MLTRI.173   
         KOUTE=KOUTE+1                                                     MLTRI.174   
         KOUTH=KOUTH+1                                                     MLTRI.175   
         KOUTR=KOUTR+1                                                     MLTRI.176   
         KOUNT=KOUNT+1                                                     MLTRI.177   
         IF(KOUNT.EQ.1.AND.KITS.GT.0) DAY=DAY+DELT/PI2                     MLTRI.178   
         DAY=DAY+DELT/PI2                                                  MLTRI.179   
C                                                                          MLTRI.180   
C        Adiabatic part of timestep                                        MLTRI.181   
C                                                                          MLTRI.182   
         CALL TSTEP                                                        MLTRI.183   
C                                                                          MLTRI.184   
C        Diabatic part of timestep. Preset tendencies to zero.             MLTRI.185   
C                                                                          MLTRI.186   
         DO 33 I=1,IGB                                                     MLTRI.187   
            ZT(I)=0.0                                                      MLTRI.188   
            DT(I)=0.0                                                      MLTRI.189   
            TT(I)=0.0                                                      MLTRI.190   
   33    CONTINUE                                                          MLTRI.191   
         DO 34 I=1,IGA                                                     NICK.144   
            VP(I)=0.                                                       NICK.145   
   34    CONTINUE                                                          NICK.146   
                                                                           NICK.147   
      IF (LGPDAMP) THEN                                                    NICK.148   
                                                                           NICK.149   
      IF (JGL.EQ.1) REWIND 25                                              NICK.150   
      JL=1                                                                 NICK.151   
                                                                           NICK.152   
C     Main loop over latitudes                                             NICK.153   
                                                                           NICK.154   
      DO 105 IH=1,JG                                                       NICK.155   
         JH=IH                                                             NICK.156   
         IF(JGL.EQ.1) READ(25) ALP,DALP,RLP,RDLP                           NICK.157   
                                                                           NICK.158   
C        Go from spectral space to grid point space using                  NICK.159   
C        inverse Legendre and Fourier transforms                           NICK.160   
                                                                           NICK.161   
         CALL LTIUV                                                        NICK.162   
                                                                           NICK.163   
         NFT=2*NL                                                          NICK.164   
         NTR=NFT*NHEM                                                      NICK.165   
         NT=(NTR-1)/NCRAY                                                  NICK.166   
         NRST=NTR-NCRAY*NT                                                 NICK.167   
                                                                           NICK.168   
         DO 110 I=1,NT                                                     NICK.169   
            CALL FFT991(DAGUV(1+(I-1)*NCRAY*MGPP),WORK,TRIG,IFAX,1         NICK.170   
     &                 ,MGPP,MG,NCRAY,1)                                   NICK.171   
 110     CONTINUE                                                          NICK.172   
         CALL FFT991(DAGUV(1+ NT*NCRAY*MGPP),WORK,TRIG,IFAX,1              NICK.173   
     &              ,MGPP,MG,NRST,1)                                       NICK.174   
                                                                           NICK.175   
C        Calculate nonlinear terms                                         NICK.176   
                                                                           NICK.177   
         CALL DGRMLT                                                       NICK.178   
                                                                           NICK.179   
C        Go from grid point space to spectral space using                  NICK.180   
C        direct Legendre and Fourier transforms                            NICK.181   
                                                                           NICK.182   
         DO 120 I=1,NT                                                     NICK.183   
            CALL FFT991(DAFUV(1+(I-1)*NCRAY*MGPP),WORK,TRIG,IFAX,1         NICK.184   
     &                 ,MGPP,MG,NCRAY,-1)                                  NICK.185   
 120     CONTINUE                                                          NICK.186   
         CALL FFT991(DAFUV(1+ NT*NCRAY*MGPP),WORK,TRIG,IFAX,1              NICK.187   
     &              ,MGPP,MG,NRST,-1)                                      NICK.188   
                                                                           NICK.189   
         CALL LTDUV                                                        NICK.190   
                                                                           NICK.191   
         JL=JL+JINC                                                        NICK.192   
 105  CONTINUE                                                             NICK.193   
                                                                           NICK.194   
C     Make sure grid point damping hasn't given any global mean            NICK.195   
C     tendency to Z and D                                                  NICK.196   
                                                                           NICK.197   
      DO 35 K=1,NL                                                         NICK.198   
         I=IGA*(K-1)+1                                                     NICK.199   
         II=I+NWJ2                                                         NICK.200   
         ZT(II)=0.0                                                        NICK.201   
         DT(I)=0.0                                                         NICK.202   
   35 CONTINUE                                                             NICK.203   
                                                                           NICK.204   
      END IF                                                               NICK.205   
C                                                                          NICK.206   
         IF (LRESTIJ) CALL SETTEE                                          MLTRI.192   
         CALL DIFUSE                                                       MLTRI.193   
         CALL DSTEP                                                        MLTRI.194   
         IF (LRESTIJ) THEN                                                 MLTRI.195   
C                                                                          MLTRI.196   
C           Fix to maintain surface pressure                               MLTRI.197   
C                                                                          MLTRI.198   
            SP(1)=CMPLX(0.,0.)                                             MLTRI.199   
         ENDIF                                                             MLTRI.200   
C                                                                          MLTRI.201   
C     PRINT*,'FINISHED TIMESTEP, KOUNT = ',KOUNT                           NICK.65    
C     PRINT*,'Z(100) = ',Z(100)                                            NICK.66    
C        End of timestep                                                   MLTRI.202   
C                                                                          MLTRI.203   
        GO TO 1                                                            MLTRI.204   
      ENDIF                                                                MLTRI.205   
C                                                                          MLTRI.206   
C     Write the final restart record                                       MLTRI.207   
C                                                                          MLTRI.208   
      RKOUNT=KOUNT                                                         MLTRI.209   
      WRITE(12)RKOUNT,RNTAPE,DAY,Z,D,T,SP,RNTAPE,ZMI,DMI,TMI,SPMI,RNTAPE   MLTRI.210   
      WRITE(2,2000)12,RKOUNT,RNTAPE,DAY                                    MLTRI.211   
      IF (LRESTIJ) THEN                                                    MLTRI.212   
         WRITE(13)RKOUNT,RNTAPE,DAY,TTRES,RNTAPE                           MLTRI.213   
         WRITE(2,2020)13,RKOUNT,RNTAPE,DAY                                 MLTRI.214   
      ENDIF                                                                MLTRI.215   
C                                                                          MLTRI.216   
      IF (LTRAIN.AND.(KTRAIN.LT.90*51)) GO TO 111                          NICK.133   
      STOP                                                                 MLTRI.217   
      END                                                                  MLTRI.218   
      SUBROUTINE INITAL                                                    INITAL.2     
C                                                                          INITAL.3     
C     INITAL calls other initialisation routines.                          INITAL.4     
C                                                                          INITAL.5     
C                                                                          PARAM1.2     
C     Determines model resolution                                          PARAM1.3     
C                                                                          PARAM1.4     
      PARAMETER(NN=21,MM=21,NHEM=2,NL=5,MOCT=1,MG=64,JG=16,NWJ2=121        G100.1     
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
      CALL INISET                                                          INITAL.9     
      CALL INIGAU                                                          INITAL.10    
      CALL INISI                                                           INITAL.11    
      IF (LRESTIJ) THEN                                                    INITAL.12    
        CALL INIRESIJ                                                      INITAL.13    
      ELSE                                                                 INITAL.14    
        CALL INIRES                                                        INITAL.15    
      ENDIF                                                                INITAL.16    
      CALL INISTR                                                          INITAL.17    
      END                                                                  INITAL.18    
      SUBROUTINE INISET                                                    INISET.2     
C                                                                          INISET.3     
C     Sets up various variables and arrays. Sets NAMELIST variables        INISET.4     
C     to their default settings, then reads NAMELIST                       INISET.5     
C                                                                          INISET.6     
C                                                                          PARAM1.2     
C     Determines model resolution                                          PARAM1.3     
C                                                                          PARAM1.4     
      PARAMETER(NN=21,MM=21,NHEM=2,NL=5,MOCT=1,MG=64,JG=16,NWJ2=121        G100.1     
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
      COMMON/SPECTR/Z(IGB),D(IGB),T(IGB),SP(IGA),GS(IGA)                   SPECTR.5     
     *              ,SPA(IGA),VP(IGA),DTE(IGB),TT(IGB),DT(IGB),ZT(IGB)     SPECTR.6     
     *              ,ZMI(IGB),DMI(IGB),TMI(IGB),SPMI(IGA)                  SPECTR.7     
      COMPLEX Z,D,T,SP,GS,SPA,VP,DTE,TT,DT,ZT,ZMI,DMI,TMI,SPMI             SPECTR.8     
C                                                                          SPECTR.9     
C                                                                          BATS.2     
C     Constant arrays and variables associated with time and vertical      BATS.3     
C     differencing. Also counters.                                         BATS.4     
C                                                                          BATS.5     
      COMMON/BATS/  BM1(IDE),AK(NNP),AQ(NL2),G(NL2),TAU(NL2)               BATS.6     
     +              ,KOUNT,KITS,KTOTAL,KRUN,BEGDAY,ITSPD,PNU21             BATS.7     
     +              ,DELT,DELT2,CV,CG,CT,PNU,PNU2                          BATS.8     
C                                                                          BATS.9     
C                                                                          OUTCON.2     
C     Switches counters and constants controlling type and frequency of    OUTCON.3     
C     model output                                                         OUTCON.4     
C                                                                          OUTCON.5     
      COMMON/OUTCON/RNTAPE,NCOEFF,NLAT,INLAT,INSPC                         OUTCON.6     
     +              ,KOUNTP,KOUNTE,KOUNTH,KOUNTR                           OUTCON.7     
     +              ,KOUTP,KOUTE,KOUTH,KOUTR,DAY                           OUTCON.8     
     +              ,SQR2,RSQR2,EAM1,EAM2,TOUT1,TOUT2,RMG                  OUTCON.9     
     +              ,LSPO(NL),LGPO(NL)                                     OUTCON.10    
      LOGICAL LSPO,LGPO                                                    OUTCON.11    
C                                                                          OUTCON.12    
C                                                                          COMFFT.2     
C     Constants and arrays needed for the fast Fourier transforms          COMFFT.3     
C                                                                          COMFFT.4     
      COMMON/COMFFT/NTGW,NRSTGW,NTWG,NRSTWG                                COMFFT.5     
     +              ,TRIG(IDA),WORK(IDF),IFAX(10)                          COMFFT.6     
C                                                                          COMFFT.7     
C                                                                          POLYNO.2     
C     Polynomial used to aid vectorization of Legendre transforms          POLYNO.3     
C                                                                          POLYNO.4     
      COMMON/POLYNO/POLY(NWJ2,2,4),CMPA(IGL)                               POLYNO.5     
      COMPLEX CMPA                                                         POLYNO.6     
C                                                                          POLYNO.7     
C                                                                          RESTOR.2     
C     Restoration fields and timescale                                     RESTOR.3     
C                                                                          RESTOR.4     
      COMMON/RESTOR/ZRES(IGN),DRES(IGN),TRES(IGN),SPRES(IGM),DAMP          RESTOR.5     
     &,ZFCE(IGB),DFCE(IGB),TFCE(IGB),SPFCE(IGA)                            NICK.207   
     &,ZFED(IGB),DFED(IGB),TFED(IGB),SPFED(IGA)                            NICK.208   
     &,ZFAN(IGB),DFAN(IGB),TFAN(IGB),SPFAN(IGA)                            NICK.209   
     &,ZDMP(IGB),DDMP(IGB),TDMP(IGB),SPDMP(IGA)                            NICK.210   
     &,ZDMI(IGB),DDMI(IGB),TDMI(IGB),SPDMI(IGA)                            NICK.211   
     &,MASK(IGC,JG),GG(IGC,JG),CD(IGC,JG),AA1,AA2,AAT                      NICK.212   
      COMPLEX ZFCE,DFCE,TFCE,SPFCE,ZFED,DFED,TFED,SPFED                    NICK.213   
     &,ZFAN,DFAN,TFAN,SPFAN                                                NICK.214   
     &,ZDMP,DDMP,TDMP,SPDMP,ZDMI,DDMI,TDMI,SPDMI                           NICK.215   
C                                                                          RESTOR.6     
C                                                                          INISET.16    
      NAMELIST/INPPL/ GA,GASCON,RADEA,AKAP,WW                              INISET.17    
      NAMELIST/INPRN/ KRUN,BEGDAY,TSPD,KITS,PNU,TDISS                      INISET.18    
     + ,NDEL,T0,LRSTRT,LSTRETCH,LSHORT,LTVEC,LBALAN,LRESTIJ                INISET.19    
     + ,LNOISE                                                             SC970203.3     
     + ,LGPDAMP,LFCE,LFED,LFAN,LLIN,LMODE,LTRAIN                           NICK.3     
      NAMELIST/INPOP/RNTAPE,KOUNTH,KOUNTR,KOUNTP,KOUNTE                    INISET.20    
     + ,NCOEFF,NLAT,LGPO,LSPO                                              INISET.21    
C                                                                          INISET.22    
  205 FORMAT(/' *****RNTAPE*****',F12.3)                                   INISET.23    
  207 FORMAT(' PRINTED OUTPUT EVERY ',I3,' TIMESTEPS'/                     INISET.24    
     +       ' RMS QUANTITIES OUTPUT EVERY ',I3,' TIMESTEPS'/              INISET.25    
     +       ' HISTORY RECORD WRITTEN EVERY ',I3,' TIMESTEPS'/             INISET.26    
     +       ' RESTART RECORD WRITTEN EVERY ',I3,' TIMESTEPS')             INISET.27    
  209 FORMAT(' INTEGRATION WITH',I3,                                       INISET.28    
     +' LEVELS IN THE VERTICAL (NL=',I3,')'/                               INISET.29    
     +' JAGGED TRIANGULAR/TRAPEZOIDAL TRUNCATION AT TOTAL WAVENO. ',I3/    INISET.30    
     +' AND ZONAL WAVENO. ',I3,' (NN=',I3,' MM=',I3,')')                   INISET.31    
  221 FORMAT(' NO LATERAL DISSIPATION')                                    INISET.32    
  222 FORMAT(' DEL',I2,' LATERAL DISSIPATION ON VORTICITY, DIVERGENCE'/    INISET.33    
     + ' AND TEMPERATURE WITH DIFFUSION COEFFICIENT  ',E10.4,              INISET.34    
     + ' m**',I1,'/s'/' THE E-FOLDING TIME FOR SMALLEST RESOLVED',         INISET.35    
     + ' SCALE IS ',F5.3,' DAYS')                                          INISET.36    
  223 FORMAT(' NO TIME FILTER')                                            INISET.37    
  224 FORMAT(' ROBERT TIME FILTER WITH PARAMETER PNU ',F5.2)               INISET.38    
  280 FORMAT(' GLOBAL DOMAIN: BOTH EVEN AND ODD COEFFICIENTS INCLUDED',    INISET.39    
     +' (NHEM=',I1,')')                                                    INISET.40    
  281 FORMAT(' HEMISPHERIC DOMAIN: ONLY EVEN DIVERGENCE TEMPERATURE'/      INISET.41    
     +' SURFACE PRESSURE AND ODD VORTICITY COEFFICIENTS INCLUDED',         INISET.42    
     +' (NHEM=',I1,')')                                                    INISET.43    
  210 FORMAT(' ',I2,'-FOLD SYMMETRY IN LONGITUDE IMPOSED AND ONLY'/        INISET.44    
     +' 1 /',I2,' OF THE DOMAIN USED (MOCT=',I2,')')                       INISET.45    
  211 FORMAT(' NON LINEAR TERMS EVALUATED ON GRID OF ',I3,                 INISET.46    
     +' GAUSSIAN LATITUDES '/' AND ',I3,                                   INISET.47    
     +' EVENLY SPACED LONGITUDES (JG=',I3,' MG=',I3,')')                   INISET.48    
  212 FORMAT(' ECMWF ANGULAR MOMENTUM CONSERVING VERTICAL SCHEME')         INISET.49    
  225 FORMAT(/' ***ABORT*** CORRECT VALUE OF NWJ2',I5,' VALUE GIVEN',I5)   INISET.50    
  232 FORMAT(/' ***ABORT***  NLAT IS GREATER THAN JG*NHEM')                INISET.51    
  233 FORMAT(/' ***ABORT***NCOEFF IS GREATER THAN NN')                     INISET.52    
C                                                                          INISET.53    
C     Set default values and override as desired through NAMELIST input    INISET.54    
C                                                                          INISET.55    
      GA=9.81                                                              INISET.56    
      GASCON=287.0                                                         INISET.57    
      RADEA=6371000.0                                                      INISET.58    
      AKAP=0.286                                                           INISET.59    
      WW=7.292E-5                                                          INISET.60    
C                                                                          INISET.61    
      KRUN=0                                                               INISET.62    
      BEGDAY=0.0                                                           INISET.63    
      TSPD=24.0                                                            INISET.64    
      KITS=3                                                               INISET.65    
      PNU=0.02                                                             INISET.66    
      TDISS=0.25                                                           INISET.67    
      NDEL=6                                                               INISET.68    
      DO 17 L=1,NL                                                         INISET.69    
         T0(L)=250.0                                                       INISET.70    
   17 CONTINUE                                                             INISET.71    
      LRSTRT=.FALSE.                                                       INISET.72    
      LSTRETCH =.FALSE.                                                    INISET.73    
      LSHORT=.FALSE.                                                       INISET.74    
      LTVEC =.TRUE.                                                        INISET.75    
      LBALAN=.FALSE.                                                       INISET.76    
      LRESTIJ=.FALSE.                                                      INISET.77    
      LNOISE=.FALSE.                                                       SC970203.4     
      LNOISE=.FALSE.                                                       NICK.5     
      LGPDAMP=.TRUE.                                                       NICK.6     
      LFCE=.TRUE.                                                          NICK.7     
      LFED=.FALSE.                                                         NICK.8     
      LFAN=.FALSE.                                                         NICK.9     
      LLIN=.FALSE.                                                         NICK.10    
      LMODE=.FALSE.                                                        NICK.11    
C                                                                          INISET.78    
      RNTAPE=0.0                                                           INISET.79    
      KOUNTH=0                                                             INISET.80    
      KOUNTR=0                                                             INISET.81    
      KOUNTP=0                                                             INISET.82    
      KOUNTE=0                                                             INISET.83    
      NCOEFF=0                                                             INISET.84    
      NLAT=MIN(16,JGG)                                                     INISET.85    
      DO 18 I=1,NL                                                         INISET.86    
         LSPO(I)=.FALSE.                                                   INISET.87    
         LGPO(I)=.FALSE.                                                   INISET.88    
   18 CONTINUE                                                             INISET.89    
C                                                                          INISET.90    
C     Read NAMELISTs, overwrite defaults and write them out                INISET.91    
C                                                                          INISET.92    
      READ(7,INPPL)                                                        INISET.93    
      WRITE(2,INPPL)                                                       INISET.94    
      READ(7,INPRN)                                                        INISET.95    
      WRITE(2,INPRN)                                                       INISET.96    
      READ(7,INPOP)                                                        INISET.97    
      WRITE(2,INPOP)                                                       INISET.98    
C                                                                          INISET.99    
C*****check logic of LTRAIN, LFCE, LFED, LFAN and KRUN makes sense         NICK.12    
C     after reading namelists (don't force in a training run               NICK.13    
C     and traiing should be for one timestep only)                         NICK.14    
C                                                                          NICK.15    
C     IF (LTRAIN) THEN                                                     NICK.16    
C        IF (LFCE) THEN                                                    NICK.17    
C        PRINT*,'WARNING - LFCE OVERRIDE TO .FALSE. IN A TRAINING RUN'     NICK.18    
C        LFCE=.FALSE.                                                      NICK.19    
C        END IF                                                            NICK.20    
C        IF (LFED) THEN                                                    NICK.21    
C        PRINT*,'WARNING - LFED OVERRIDE TO .FALSE. IN A TRAINING RUN'     NICK.22    
C        LFED=.FALSE.                                                      NICK.23    
C        END IF                                                            NICK.24    
C        IF (LFAN) THEN                                                    NICK.25    
C        PRINT*,'WARNING - LFAN OVERRIDE TO .FALSE. IN A TRAINING RUN'     NICK.26    
C        LFAN=.FALSE.                                                      NICK.27    
C        END IF                                                            NICK.28    
C        IF (KRUN.NE.1) THEN                                               NICK.29    
C        PRINT*,'WARNING - KRUN OVERRIDE TO 1 IN A TRAINING RUN'           NICK.30    
C        KRUN=1                                                            NICK.31    
C        END IF                                                            NICK.32    
C     END IF                                                               NICK.33    
C     Write out details of model run                                       INISET.100   
C                                                                          INISET.101   
      WRITE(2,205)RNTAPE                                                   INISET.102   
      WRITE(2,209)NL,NL,NN,MM,NN,MM                                        INISET.103   
      IF(NHEM.EQ.2) WRITE(2,280) NHEM                                      INISET.104   
      IF(NHEM.EQ.1) WRITE(2,281) NHEM                                      INISET.105   
      WRITE(2,210) MOCT,MOCT,MOCT                                          INISET.106   
      WRITE(2,211)JG,MG,JG,MG                                              INISET.107   
      WRITE(2,212)                                                         INISET.108   
C                                                                          INISET.109   
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
C     Compute internal diffusion parameter                                 INISET.141   
C                                                                          INISET.142   
      IF(TDISS.EQ.0.0) THEN                                                INISET.143   
         AKK=0.0                                                           INISET.144   
      ELSE                                                                 INISET.145   
         AKK=WW*(RADEA**NDEL)/(2.0*PI*TDISS*((NN*(NN+1))**REAL(NDEL/2)))   INISET.146   
      END IF                                                               INISET.147   
      IF(AKK.EQ.0.0) WRITE(2,221)                                          INISET.148   
      IF(AKK.NE.0.0) WRITE(2,222) NDEL,AKK,NDEL,TDISS                      INISET.149   
      AKK=AKK/(WW*(RADEA**NDEL))                                           INISET.150   
      NDELH=NDEL/2                                                         INISET.151   
      DO 3 NP=1,NNP                                                        INISET.152   
         AK(NP)=AKK*(SQ(NP)**NDELH)                                        INISET.153   
    3 CONTINUE                                                             INISET.154   
C                                                                          INISET.155   
C     Set time variables and counters                                      INISET.156   
C                                                                          INISET.157   
      IF(PNU.EQ.0.0)WRITE(2,223)                                           INISET.158   
      IF(PNU.NE.0.0)WRITE(2,224)PNU                                        INISET.159   
      WRITE(2,207)KOUNTP,KOUNTE,KOUNTH,KOUNTR                              INISET.160   
C                                                                          INISET.161   
      IF(KOUNTP.EQ.0) KOUNTP=-999                                          INISET.162   
      IF(KOUNTE.EQ.0) KOUNTE=-999                                          INISET.163   
      IF(KOUNTH.EQ.0) KOUNTH=-999                                          INISET.164   
      IF(KOUNTR.EQ.0) KOUNTR=-999                                          INISET.165   
      DELT=PI2/TSPD                                                        INISET.166   
      PNU2=PNU+PNU                                                         INISET.167   
      PNU21=1.0-PNU2                                                       INISET.168   
      ITSPD=NINT(TSPD)                                                     INISET.169   
C                                                                          INISET.170   
C     Check variables make sense                                           INISET.171   
C                                                                          INISET.172   
      IF (NLAT.GT.JGG) THEN                                                INISET.173   
         WRITE(2,232)                                                      INISET.174   
         STOP                                                              INISET.175   
      ENDIF                                                                INISET.176   
      IF (NCOEFF.GT.NN) THEN                                               INISET.177   
         WRITE(2,233)                                                      INISET.178   
         STOP                                                              INISET.179   
      ENDIF                                                                INISET.180   
      NWJCH=0                                                              INISET.181   
      DO 310 MP=1,MFP,MOCT                                                 INISET.182   
         DO 320 JP=MP,NFP,MH                                               INISET.183   
            NWJCH=NWJCH+1                                                  INISET.184   
 320     CONTINUE                                                          INISET.185   
 310  CONTINUE                                                             INISET.186   
      IF(NWJ2.NE.NWJCH) THEN                                               INISET.187   
        WRITE(2,225) NWJCH,NWJ2                                            INISET.188   
        STOP                                                               INISET.189   
      ENDIF                                                                INISET.190   
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
C     Make T0 dimensionless                                                INISET.204   
C                                                                          INISET.205   
      DO 61 L=1,NL                                                         INISET.206   
        T0(L)=T0(L)/CT                                                     INISET.207   
   61 CONTINUE                                                             INISET.208   
C                                                                          INISET.209   
C     Set up arrays and variables for use in FFT routines                  INISET.210   
C                                                                          INISET.211   
      NTRWG=NFTWG*NHEM                                                     INISET.212   
      NTRGW=NFTGW*NHEM                                                     INISET.213   
      NTWG=(NTRWG-1)/NCRAY                                                 INISET.214   
      NTGW=(NTRGW-1)/NCRAY                                                 INISET.215   
      NRSTWG=NTRWG-NCRAY*NTWG                                              INISET.216   
      NRSTGW=NTRGW-NCRAY*NTGW                                              INISET.217   
C                                                                          INISET.218   
C     Calculate auxiliary values required by FFT991                        INISET.219   
C                                                                          INISET.220   
      CALL FAX(IFAX,MG,3)                                                  INISET.221   
      CALL FFTRIG(TRIG,MG,3)                                               INISET.222   
C                                                                          INISET.223   
C     Set output control variables and initialise WRSPS                    INISET.224   
C                                                                          INISET.225   
      INSPC=0                                                              INISET.226   
      DO 28 MP=1,NCOEFF,MOCT                                               INISET.227   
         DO 27 JP=MP,NCOEFF,MH                                             INISET.228   
            INSPC=INSPC+1                                                  INISET.229   
   27    CONTINUE                                                          INISET.230   
   28 CONTINUE                                                             INISET.231   
      CALL WRSPS(Z(1),1)                                                   INISET.232   
      IF (NLAT.NE.0) THEN                                                  INISET.233   
         INLAT=JGG/NLAT                                                    INISET.234   
      ELSE                                                                 INISET.235   
         INLAT=0                                                           INISET.236   
      ENDIF                                                                INISET.237   
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
      PARAMETER(NN=21,MM=21,NHEM=2,NL=5,MOCT=1,MG=64,JG=16,NWJ2=121        G100.1     
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
C     Constant arrays and variables associated with time and vertical      BATS.3     
C     differencing. Also counters.                                         BATS.4     
C                                                                          BATS.5     
      COMMON/BATS/  BM1(IDE),AK(NNP),AQ(NL2),G(NL2),TAU(NL2)               BATS.6     
     +              ,KOUNT,KITS,KTOTAL,KRUN,BEGDAY,ITSPD,PNU21             BATS.7     
     +              ,DELT,DELT2,CV,CG,CT,PNU,PNU2                          BATS.8     
C                                                                          BATS.9     
C                                                                          LEGAU.2     
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
      SUBROUTINE INISI                                                     INISI.2     
C                                                                          INISI.3     
C     Sets up arrays and variables for the vertical structure              INISI.4     
C     and the semi-implicit scheme                                         INISI.5     
C                                                                          INISI.6     
C                                                                          PARAM1.2     
C     Determines model resolution                                          PARAM1.3     
C                                                                          PARAM1.4     
      PARAMETER(NN=21,MM=21,NHEM=2,NL=5,MOCT=1,MG=64,JG=16,NWJ2=121        G100.1     
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
      PARAMETER(NLT=NL+NL)                                                 INISI.9     
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
C     Constant arrays and variables associated with time and vertical      BATS.3     
C     differencing. Also counters.                                         BATS.4     
C                                                                          BATS.5     
      COMMON/BATS/  BM1(IDE),AK(NNP),AQ(NL2),G(NL2),TAU(NL2)               BATS.6     
     +              ,KOUNT,KITS,KTOTAL,KRUN,BEGDAY,ITSPD,PNU21             BATS.7     
     +              ,DELT,DELT2,CV,CG,CT,PNU,PNU2                          BATS.8     
C                                                                          BATS.9     
C                                                                          OUTCON.2     
C     Switches counters and constants controlling type and frequency of    OUTCON.3     
C     model output                                                         OUTCON.4     
C                                                                          OUTCON.5     
      COMMON/OUTCON/RNTAPE,NCOEFF,NLAT,INLAT,INSPC                         OUTCON.6     
     +              ,KOUNTP,KOUNTE,KOUNTH,KOUNTR                           OUTCON.7     
     +              ,KOUTP,KOUTE,KOUTH,KOUTR,DAY                           OUTCON.8     
     +              ,SQR2,RSQR2,EAM1,EAM2,TOUT1,TOUT2,RMG                  OUTCON.9     
     +              ,LSPO(NL),LGPO(NL)                                     OUTCON.10    
      LOGICAL LSPO,LGPO                                                    OUTCON.11    
C                                                                          OUTCON.12    
C                                                                          INISI.13    
      DIMENSION H(NL),CR(NL),CI(NL),TBM1(NL2),WA(NL)                       SC961212.1     
      INTEGER IWA(NL)                                                      SC961212.2     
      DIMENSION TBME(NL,NL)                                                INISI.15    
      EQUIVALENCE (TBM1(1),TBME(1,1))                                      INISI.16    
C                                                                          INISI.17    
  203 FORMAT(' STANDARD HEIGHTS IN KM')                                    INISI.18    
  206 FORMAT(' GRAVITY WAVE SPEEDS IN M/SEC')                              INISI.19    
  208 FORMAT(' VALUES OF SIGMA AT HALF LEVELS')                            INISI.20    
  213 FORMAT(1X,8F8.4)                                                     INISI.21    
  214 FORMAT(' VALUES OF SIGMA AT FULL LEVELS')                            INISI.22    
  215 FORMAT(' BASIC STATE TEMPERATURES (NON-DIMENSIONAL)')                INISI.23    
  216 FORMAT(1X,8F8.3)                                                     INISI.24    
  217 FORMAT(5X,8F8.3)                                                     INISI.25    
C                                                                          INISI.26    
      STP=1.0/NL                                                           INISI.27    
      IF (.NOT. LSTRETCH) THEN                                             INISI.28    
         DO 23 L=1,NLM                                                     INISI.29    
            SIGMAH(L)=L*STP                                                INISI.30    
   23    CONTINUE                                                          INISI.31    
      ELSE                                                                 INISI.32    
         P=0.0                                                             INISI.33    
         T1=(.9375/.94-1.25)/(.9375*(SQRT(.9375)-1.0))                     INISI.34    
         T2=4.0+T1                                                         INISI.35    
         DO 25 L=1,NLM                                                     INISI.36    
            P=P+STP                                                        INISI.37    
            SIGMAH(L)=P*(2.0-P)*(1.0+0.25*SIN(PI2*(P**.6)))/               INISI.38    
     1                (5.0-T2*P+T1*(P**1.5))                               INISI.39    
   25    CONTINUE                                                          INISI.40    
      END IF                                                               INISI.41    
      S1=0.                                                                INISI.42    
      DO 60 L=1,NLM                                                        INISI.43    
         S2=SIGMAH(L)                                                      INISI.44    
         DSIGMA(L)=S2-S1                                                   INISI.45    
         SIGMA(L)=0.5*(S2+S1)                                              INISI.46    
         RDSIG(L)=0.5/DSIGMA(L)                                            INISI.47    
         S1=S2                                                             INISI.48    
   60 CONTINUE                                                             INISI.49    
      DSIGMA(NL)=1.-SIGMAH(NLM)                                            INISI.50    
      RDSIG(NL)=0.5/DSIGMA(NL)                                             INISI.51    
      SIGMA(NL)=0.5*(1.+SIGMAH(NLM))                                       INISI.52    
C                                                                          INISI.53    
C     This value, used in setting ALPHA(1), is irrelevant in the           INISI.54    
C     angular momentum conserving ECMWF scheme                             INISI.55    
C                                                                          INISI.56    
      S1=LOG(SIGMA(1)*SIGMA(1)/SIGMAH(1))                                  INISI.57    
      IG=1                                                                 INISI.58    
      T0M=T0(1)                                                            INISI.59    
      DO 61 L=1,NLM                                                        INISI.60    
         LP=L+1                                                            INISI.61    
         S2=LOG(SIGMAH(L))                                                 INISI.62    
         T0P=T0(LP)                                                        INISI.63    
         IG=IG+NL                                                          INISI.64    
         G(IG)=0.                                                          INISI.65    
         T01S2(L)=T0P-T0M                                                  INISI.66    
         ALPHA(L)=S2-S1                                                    INISI.67    
         TKP(L)=AKAP*T0M                                                   INISI.68    
         T0M=T0P                                                           INISI.69    
         S1=S2                                                             INISI.70    
   61 CONTINUE                                                             INISI.71    
      ALPHA(NL)=-S1                                                        INISI.72    
      TKP(NL)=AKAP*T0M                                                     INISI.73    
      G(1)=1.0                                                             INISI.74    
      DO 64 J=2,NL                                                         INISI.75    
         ALJ=ALPHA(J)                                                      INISI.76    
         IG=J                                                              INISI.77    
         LIM=J-1                                                           INISI.78    
         DO 62 I=1,LIM                                                     INISI.79    
            G(IG)=ALJ                                                      INISI.80    
            IG=IG+NL                                                       INISI.81    
   62    CONTINUE                                                          INISI.82    
         G(IG)=1.0-ALJ*SIGMAH(LIM)/DSIGMA(J)                               INISI.83    
         IF (J.LT.NL) THEN                                                 INISI.84    
            LIM=LIM+2                                                      INISI.85    
            DO 63 I=LIM,NL                                                 INISI.86    
               IG=IG+NL                                                    INISI.87    
               G(IG)=0.                                                    INISI.88    
   63       CONTINUE                                                       INISI.89    
         ENDIF                                                             INISI.90    
   64 CONTINUE                                                             INISI.91    
C*****redefine G matrix: hard wire hack old T scheme back in               NICK.67    
C*****ONLY VALID FOR equispaced levels and isothermal                      NICK.68    
C*****reference temperature - so TAU = AKAP*T0*(G transpose).              NICK.69    
                                                                           NICK.70    
C     DO L=1,NL-1                                                          NICK.71    
C     ALPHA(L)=LOG(SIGMA(L+1)/SIGMA(L)) / 2.                               NICK.72    
C     ENDDO                                                                NICK.73    
C     ALPHA(NL)=LOG(1./SIGMA(NL))                                          NICK.74    
C     DO J=2,NL                                                            NICK.75    
C     DO I=1,J-1                                                           NICK.76    
C     II=J+(I-1)*NL                                                        NICK.77    
C     G(II)=ALPHA(J-1)+ALPHA(J)                                            NICK.78    
C     ENDDO                                                                NICK.79    
C     ENDDO                                                                NICK.80    
C     DO I=1,NL                                                            NICK.81    
C     II=I+(I-1)*NL                                                        NICK.82    
C     G(II)=ALPHA(I)                                                       NICK.83    
C     ENDDO                                                                NICK.84    
                                                                           NICK.85    
C     G(1)=0.6017                                                          NICK.86    
C     G(7)=0.3510                                                          NICK.87    
C     G(13)=0.1737                                                         NICK.88    
C     G(19)=0.1122                                                         NICK.89    
C     G(25)=0.1276                                                         NICK.90    
C     print*,'input G(25) NOW!!'                                           NICK.91    
C     read(*,*)G(25)                                                       NICK.92    
      IC=-1                                                                INISI.92    
      DO 50 I=1,NL                                                         INISI.93    
         IC=IC+1                                                           INISI.94    
         JC=IC*NLP                                                         INISI.95    
         JCC=JC-NLM                                                        INISI.96    
         DO 51 J=I,NL                                                      INISI.97    
            JC=JC+1                                                        INISI.98    
            JCC=JCC+NL                                                     INISI.99    
            C(JCC)=G(JC)*DSIGMA(I)/DSIGMA(J)                               INISI.100   
   51    CONTINUE                                                          INISI.101   
   50 CONTINUE                                                             INISI.102   
      TT01S2=T01S2(1)                                                      INISI.103   
      TAU(1)=0.5*TT01S2*(SIGMAH(1)-1.0)+TKP(1)*C(1)                        INISI.104   
      DO 65 L=2,NL                                                         INISI.105   
         TAU(L)=0.5*TT01S2*DSIGMA(L)                                       INISI.106   
   65 CONTINUE                                                             INISI.107   
      SIG=SIGMAH(1)                                                        INISI.108   
      IT=NL                                                                INISI.109   
      DO 73 L=2,NL                                                         INISI.110   
         TTKP=TKP(L)                                                       INISI.111   
         TTM=TT01S2                                                        INISI.112   
         SIGM=SIG                                                          INISI.113   
         IF (L.LT.NL) THEN                                                 INISI.114   
            TT01S2=T01S2(L)                                                INISI.115   
            SIG=SIGMAH(L)                                                  INISI.116   
         ENDIF                                                             INISI.117   
         RDSIGL=RDSIG(L)                                                   INISI.118   
         DO 72 M=1,NL                                                      INISI.119   
            IT=IT+1                                                        INISI.120   
            IF( M.LT.L) THEN                                               INISI.121   
               TM=1.                                                       INISI.122   
               TMM=1.                                                      INISI.123   
            ELSEIF (M.EQ.L) THEN                                           INISI.124   
               TM=1.                                                       INISI.125   
               TMM=0.                                                      INISI.126   
            ELSE                                                           INISI.127   
               TM=0.                                                       INISI.128   
               TMM=0.                                                      INISI.129   
            ENDIF                                                          INISI.130   
            TTAU=TTM*(SIGM-TMM)                                            INISI.131   
            IF (L.LT.NL) TTAU=TTAU+TT01S2*(SIG-TM)                         INISI.132   
            TTAU=TTAU*RDSIGL*DSIGMA(M)                                     INISI.133   
            IF (M.LE.L) TTAU=TTAU+TTKP*C(IT)                               INISI.134   
            TAU(IT)=TTAU                                                   INISI.135   
   72    CONTINUE                                                          INISI.136   
   73 CONTINUE                                                             INISI.137   
      FAC=0.001*CG/GA                                                      INISI.138   
      IL=0                                                                 INISI.139   
      DO 78 L=1,NL                                                         INISI.140   
         HL=0.                                                             INISI.141   
         DO 77 M=1,NL                                                      INISI.142   
            IL=IL+1                                                        INISI.143   
            HL=HL+G(IL)*T0(M)                                              INISI.144   
   77    CONTINUE                                                          INISI.145   
         H(L)=HL*FAC                                                       INISI.146   
   78 CONTINUE                                                             INISI.147   
      IL=0                                                                 INISI.148   
      INS=1                                                                INISI.149   
      DO 81 L=1,NL                                                         INISI.150   
         DO 80 M=1,NL                                                      INISI.151   
            IN=INS                                                         INISI.152   
            IL=IL+1                                                        INISI.153   
            IM=M                                                           INISI.154   
            TAQ=T0(L)*DSIGMA(M)                                            INISI.155   
            DO 79 N=1,NL                                                   INISI.156   
               TAQ=TAQ+G(IN)*TAU(IM)                                       INISI.157   
               IN=IN+1                                                     INISI.158   
               IM=IM+NL                                                    INISI.159   
   79       CONTINUE                                                       INISI.160   
            AQ(IL)=TAQ                                                     INISI.161   
            TBM1(IL)=TAQ                                                   INISI.162   
   80    CONTINUE                                                          INISI.163   
         INS=INS+NL                                                        INISI.164   
   81 CONTINUE                                                             INISI.165   
      CALL QREIG(TBM1,NL,NL,NL,CR,CI)                                      INISI.166   
      DO 82 L=1,NL                                                         INISI.167   
         CR(L)=CV*SQRT(CR(L))                                              INISI.168   
   82 CONTINUE                                                             INISI.169   
C                                                                          INISI.170   
C     Write out vertical information                                       INISI.171   
C                                                                          INISI.172   
      WRITE(2,208)                                                         INISI.173   
      WRITE(2,217)(SIGMAH(L),L=1,NLM)                                      INISI.174   
      WRITE(2,214)                                                         INISI.175   
      WRITE(2,213)(SIGMA(L),L=1,NL)                                        INISI.176   
      WRITE(2,215)                                                         INISI.177   
      WRITE(2,216)(T0(L),L=1,NL)                                           INISI.178   
      WRITE(2,203)                                                         INISI.179   
      WRITE(2,216)(H(L),L=1,NL)                                            INISI.180   
      WRITE(2,206)                                                         INISI.181   
      WRITE(2,216)(CR(L),L=1,NL)                                           INISI.182   
      WRITE(2,*)                                                           INISI.183   
C                                                                          INISI.184   
C     Setup arrays for semi-implicit scheme                                INISI.185   
C                                                                          INISI.186   
      DELTSQ=DELT*DELT                                                     INISI.187   
      IBM1=0                                                               INISI.188   
      DO 11 IN=2,NNP                                                       INISI.189   
         RCN=RSQ(IN)                                                       INISI.190   
         IL=0                                                              INISI.191   
         DO 83 L=1,NL                                                      INISI.192   
            DO 84 M=1,NL                                                   INISI.193   
               IL=IL+1                                                     INISI.194   
               TBM1(IL)=AQ(IL)*DELTSQ                                      INISI.195   
               IF(M.EQ.L)TBM1(IL)=TBM1(IL)+RCN                             INISI.196   
   84       CONTINUE                                                       INISI.197   
   83    CONTINUE                                                          INISI.198   
         CALL MATINV(TBME,NL,NL,IWA,WA)                                    SC961212.3     
         DO 85 L=1,NL2                                                     INISI.200   
            IBM1=IBM1+1                                                    INISI.201   
            BM1(IBM1)=TBM1(L)                                              INISI.202   
   85    CONTINUE                                                          INISI.203   
   11 CONTINUE                                                             INISI.204   
      SFAC=0.5**KITS                                                       INISI.205   
      IF(LRSTRT.AND..NOT.LSHORT)SFAC=1.0                                   INISI.206   
      DELT=DELT*SFAC                                                       INISI.207   
      DELT2=DELT+DELT                                                      INISI.208   
      DELTSQ=DELT*DELT                                                     INISI.209   
      DO 87 L=1,NL2                                                        INISI.210   
         AQ(L)=AQ(L)*DELTSQ                                                INISI.211   
   87 CONTINUE                                                             INISI.212   
      TOUT1=0.                                                             INISI.213   
      TOUT2=0.                                                             INISI.214   
      DO 88 L=1,NL                                                         INISI.215   
         T0L=T0(L)                                                         INISI.216   
         DSIG=DSIGMA(L)*T0(L)                                              INISI.217   
         TOUT1=TOUT1+DSIG                                                  INISI.218   
         TOUT2=TOUT2+DSIG*T0L                                              INISI.219   
   88 CONTINUE                                                             INISI.220   
C                                                                          INISI.221   
      END                                                                  INISI.222   
      SUBROUTINE INIRESIJ                                                  INIRESIJ.2     
C                                                                          INIRESIJ.3     
C     Sets up restoration variables and arrays. Sets NAMELIST              INIRESIJ.4     
C     variables to their default settings, then reads NAMELIST             INIRESIJ.5     
C                                                                          INIRESIJ.6     
C                                                                          PARAM1.2     
C     Determines model resolution                                          PARAM1.3     
C                                                                          PARAM1.4     
      PARAMETER(NN=21,MM=21,NHEM=2,NL=5,MOCT=1,MG=64,JG=16,NWJ2=121        G100.1     
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
C                                                                          RESTIJ.2     
C     Restoration temperature field and constants which determine it,      RESTIJ.3     
C     also contains timescales                                             RESTIJ.4     
C                                                                          RESTIJ.5     
      COMMON/RESTIJ/TTRES(IGB)                                             RESTIJ.6     
     + ,DTNS,DTEP,DTTRP,FAC(NL),DDAMP(NL),TFRC(NL),YRLEN,TRS(NL)           RESTIJ.7     
     +  ,ALR,ZTROP,TGR                                                     RESTIJ.8     
      COMPLEX TTRES                                                        RESTIJ.9     
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
C     Constant arrays and variables associated with time and vertical      BATS.3     
C     differencing. Also counters.                                         BATS.4     
C                                                                          BATS.5     
      COMMON/BATS/  BM1(IDE),AK(NNP),AQ(NL2),G(NL2),TAU(NL2)               BATS.6     
     +              ,KOUNT,KITS,KTOTAL,KRUN,BEGDAY,ITSPD,PNU21             BATS.7     
     +              ,DELT,DELT2,CV,CG,CT,PNU,PNU2                          BATS.8     
C                                                                          BATS.9     
      DIMENSION RESTIM(NL)                                                 INIRESIJ.12    
C                                                                          INIRESIJ.13    
      NAMELIST/INPRSIJ/ TFRC,RESTIM,DTNS,DTEP,ALR,                         INIRESIJ.14    
     +                  DTTRP,ZTROP,TGR,YRLEN                              INIRESIJ.15    
C                                                                          INIRESIJ.16    
      TFRC(NL)=1.                                                          INIRESIJ.17    
      DO 19 L=1,NL-1                                                       INIRESIJ.18    
         TFRC(L) = 0.                                                      INIRESIJ.19    
 19   CONTINUE                                                             INIRESIJ.20    
      DO 20 L=1,NL                                                         INIRESIJ.21    
         RESTIM(L) = 15.0                                                  INIRESIJ.22    
 20   CONTINUE                                                             INIRESIJ.23    
      DTNS=0.                                                              INIRESIJ.24    
      DTEP=60.                                                             INIRESIJ.25    
      ALR=6.5E-03                                                          INIRESIJ.26    
      DTTRP=2.                                                             INIRESIJ.27    
      ZTROP=12.0E03                                                        INIRESIJ.28    
      TGR=288.                                                             INIRESIJ.29    
      YRLEN=0.                                                             INIRESIJ.30    
C                                                                          INIRESIJ.31    
      READ(7,INPRSIJ)                                                      INIRESIJ.32    
      WRITE(2,INPRSIJ)                                                     INIRESIJ.33    
C                                                                          INIRESIJ.34    
C     Dimensionless coefficient for Newtonian cooling friction             INIRESIJ.35    
C     and timestep. A day is 2*pi in non dimensional                       INIRESIJ.36    
C     units using omega as the unit of frquency.                           INIRESIJ.37    
C                                                                          INIRESIJ.38    
      DO 22 L=1,NL                                                         INIRESIJ.39    
         IF (RESTIM(L).GT.0.0) THEN                                        INIRESIJ.40    
            DDAMP(L)=1.0/(PI2*RESTIM(L))                                   INIRESIJ.41    
         ELSE                                                              INIRESIJ.42    
            DDAMP(L)=0.0                                                   INIRESIJ.43    
         ENDIF                                                             INIRESIJ.44    
         IF (TFRC(L).GT.0.0) THEN                                          INIRESIJ.45    
            TFRC(L)=1.0/(PI2*TFRC(L))                                      INIRESIJ.46    
         ELSE                                                              INIRESIJ.47    
            TFRC(L)=0.0                                                    INIRESIJ.48    
         ENDIF                                                             INIRESIJ.49    
 22   CONTINUE                                                             INIRESIJ.50    
C                                                                          INIRESIJ.51    
C     Make temperatures dimensionless                                      INIRESIJ.52    
C                                                                          INIRESIJ.53    
      DTNS=DTNS/CT                                                         INIRESIJ.54    
      DTEP=DTEP/CT                                                         INIRESIJ.55    
      DTTRP=DTTRP/CT                                                       INIRESIJ.56    
C                                                                          INIRESIJ.57    
C     Loop to set array FAC - this controls temperature gradients          INIRESIJ.58    
C     as a function of SIGMA in TTRES. It is a sine wave from one          INIRESIJ.59    
C     at SIGMA = 1 to zero at STPS (SIGMA at the tropopause).              INIRESIJ.60    
C                                                                          INIRESIJ.61    
C     First find SIGMA at ZTROP                                            INIRESIJ.62    
C                                                                          INIRESIJ.63    
      TTROP = TGR - ZTROP*ALR                                              INIRESIJ.64    
      STPS = (TTROP/TGR)**(GA/(ALR*GASCON))                                INIRESIJ.65    
      DO 600 L=1,NL                                                        INIRESIJ.66    
         THING=SIN(0.5*PI*(SIGMA(L)-STPS)/(1.-STPS))                       INIRESIJ.67    
         IF (THING.LT.0.) THEN                                             INIRESIJ.68    
            FAC(L)=0.                                                      INIRESIJ.69    
         ELSE                                                              INIRESIJ.70    
            FAC(L)=THING                                                   INIRESIJ.71    
         ENDIF                                                             INIRESIJ.72    
600   CONTINUE                                                             INIRESIJ.73    
C                                                                          INIRESIJ.74    
      END                                                                  INIRESIJ.75    
      SUBROUTINE INIRES                                                    INIRES.2     
C                                                                          INIRES.3     
C     Sets up restoration variables and arrays. Sets NAMELIST              INIRES.4     
C     variables to their default settings, then reads NAMELIST             INIRES.5     
C                                                                          INIRES.6     
C                                                                          PARAM1.2     
C     Determines model resolution                                          PARAM1.3     
C                                                                          PARAM1.4     
      PARAMETER(NN=21,MM=21,NHEM=2,NL=5,MOCT=1,MG=64,JG=16,NWJ2=121        G100.1     
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
C                                                                          RESTOR.2     
C     Restoration fields and timescale                                     RESTOR.3     
C                                                                          RESTOR.4     
      COMMON/RESTOR/ZRES(IGN),DRES(IGN),TRES(IGN),SPRES(IGM),DAMP          RESTOR.5     
     &,ZFCE(IGB),DFCE(IGB),TFCE(IGB),SPFCE(IGA)                            NICK.207   
     &,ZFED(IGB),DFED(IGB),TFED(IGB),SPFED(IGA)                            NICK.208   
     &,ZFAN(IGB),DFAN(IGB),TFAN(IGB),SPFAN(IGA)                            NICK.209   
     &,ZDMP(IGB),DDMP(IGB),TDMP(IGB),SPDMP(IGA)                            NICK.210   
     &,ZDMI(IGB),DDMI(IGB),TDMI(IGB),SPDMI(IGA)                            NICK.211   
     &,MASK(IGC,JG),GG(IGC,JG),CD(IGC,JG),AA1,AA2,AAT                      NICK.212   
      COMPLEX ZFCE,DFCE,TFCE,SPFCE,ZFED,DFED,TFED,SPFED                    NICK.213   
     &,ZFAN,DFAN,TFAN,SPFAN                                                NICK.214   
     &,ZDMP,DDMP,TDMP,SPDMP,ZDMI,DDMI,TDMI,SPDMI                           NICK.215   
C                                                                          RESTOR.6     
C                                                                          INIRES.10    
      NAMELIST/INPRS/ RESTIM,AA1,AA2,AAT                                   NICK.4     
C                                                                          INIRES.12    
      RESTIM=0.0                                                           INIRES.13    
      AA1=0.                                                               NICK.34    
      AA2=0.                                                               NICK.35    
      AAT=0.                                                               NICK.36    
C                                                                          INIRES.14    
      READ(7,INPRS)                                                        INIRES.15    
      WRITE(2,INPRS)                                                       INIRES.16    
C                                                                          INIRES.17    
C     Dimensionless coefficient for Newtonian cooling friction             INIRES.18    
C     and timestep. A day is 2*pi in non dimensional                       INIRES.19    
C     units using omega as the unit of frequency.                          INIRES.20    
C                                                                          INIRES.21    
      IF (RESTIM.GT.0.0) THEN                                              INIRES.22    
         DAMP=1.0/(PI2*RESTIM)                                             INIRES.23    
      ELSE                                                                 INIRES.24    
         DAMP=0.0                                                          INIRES.25    
      ENDIF                                                                INIRES.26    
C                                                                          INIRES.27    
      END                                                                  INIRES.28    
      SUBROUTINE INISTR(KTRAIN)                                            NICK.134   
C                                                                          INISTR.3     
C     Reads in data for a start/restart run                                INISTR.4     
C                                                                          INISTR.5     
C                                                                          PARAM1.2     
C     Determines model resolution                                          PARAM1.3     
C                                                                          PARAM1.4     
      PARAMETER(NN=21,MM=21,NHEM=2,NL=5,MOCT=1,MG=64,JG=16,NWJ2=121        G100.1     
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
      COMMON/SPECTR/Z(IGB),D(IGB),T(IGB),SP(IGA),GS(IGA)                   SPECTR.5     
     *              ,SPA(IGA),VP(IGA),DTE(IGB),TT(IGB),DT(IGB),ZT(IGB)     SPECTR.6     
     *              ,ZMI(IGB),DMI(IGB),TMI(IGB),SPMI(IGA)                  SPECTR.7     
      COMPLEX Z,D,T,SP,GS,SPA,VP,DTE,TT,DT,ZT,ZMI,DMI,TMI,SPMI             SPECTR.8     
C                                                                          SPECTR.9     
C                                                                          BATS.2     
C     Constant arrays and variables associated with time and vertical      BATS.3     
C     differencing. Also counters.                                         BATS.4     
C                                                                          BATS.5     
      COMMON/BATS/  BM1(IDE),AK(NNP),AQ(NL2),G(NL2),TAU(NL2)               BATS.6     
     +              ,KOUNT,KITS,KTOTAL,KRUN,BEGDAY,ITSPD,PNU21             BATS.7     
     +              ,DELT,DELT2,CV,CG,CT,PNU,PNU2                          BATS.8     
C                                                                          BATS.9     
C                                                                          OUTCON.2     
C     Switches counters and constants controlling type and frequency of    OUTCON.3     
C     model output                                                         OUTCON.4     
C                                                                          OUTCON.5     
      COMMON/OUTCON/RNTAPE,NCOEFF,NLAT,INLAT,INSPC                         OUTCON.6     
     +              ,KOUNTP,KOUNTE,KOUNTH,KOUNTR                           OUTCON.7     
     +              ,KOUTP,KOUTE,KOUTH,KOUTR,DAY                           OUTCON.8     
     +              ,SQR2,RSQR2,EAM1,EAM2,TOUT1,TOUT2,RMG                  OUTCON.9     
     +              ,LSPO(NL),LGPO(NL)                                     OUTCON.10    
      LOGICAL LSPO,LGPO                                                    OUTCON.11    
C                                                                          OUTCON.12    
C                                                                          RESTOR.2     
C     Restoration fields and timescale                                     RESTOR.3     
C                                                                          RESTOR.4     
      COMMON/RESTOR/ZRES(IGN),DRES(IGN),TRES(IGN),SPRES(IGM),DAMP          RESTOR.5     
     &,ZFCE(IGB),DFCE(IGB),TFCE(IGB),SPFCE(IGA)                            NICK.207   
     &,ZFED(IGB),DFED(IGB),TFED(IGB),SPFED(IGA)                            NICK.208   
     &,ZFAN(IGB),DFAN(IGB),TFAN(IGB),SPFAN(IGA)                            NICK.209   
     &,ZDMP(IGB),DDMP(IGB),TDMP(IGB),SPDMP(IGA)                            NICK.210   
     &,ZDMI(IGB),DDMI(IGB),TDMI(IGB),SPDMI(IGA)                            NICK.211   
     &,MASK(IGC,JG),GG(IGC,JG),CD(IGC,JG),AA1,AA2,AAT                      NICK.212   
      COMPLEX ZFCE,DFCE,TFCE,SPFCE,ZFED,DFED,TFED,SPFED                    NICK.213   
     &,ZFAN,DFAN,TFAN,SPFAN                                                NICK.214   
     &,ZDMP,DDMP,TDMP,SPDMP,ZDMI,DDMI,TDMI,SPDMI                           NICK.215   
C                                                                          RESTOR.6     
C                                                                          RESTIJ.2     
C     Restoration temperature field and constants which determine it,      RESTIJ.3     
C     also contains timescales                                             RESTIJ.4     
C                                                                          RESTIJ.5     
      COMMON/RESTIJ/TTRES(IGB)                                             RESTIJ.6     
     + ,DTNS,DTEP,DTTRP,FAC(NL),DDAMP(NL),TFRC(NL),YRLEN,TRS(NL)           RESTIJ.7     
     +  ,ALR,ZTROP,TGR                                                     RESTIJ.8     
      COMPLEX TTRES                                                        RESTIJ.9     
C                                                                          BALAN.2     
C     Constants and arrays needed for balancing                            BALAN.3     
C                                                                          BALAN.4     
      COMMON/BALAN/BFILT(NL),RGT0(NL),RG(NL2),TMEAN(NL)                    BALAN.5     
     +            ,EP1(IGA),EP2(IGA),KBAL,MFTBAL,SRGT0,LTBAL               BALAN.6     
      LOGICAL LTBAL                                                        BALAN.7     
C                                                                          BALAN.8     
C                                                                          LEGAU.2     
C     Legendre polynomials and information about gaussian latitudes        LEGAU.3     
C                                                                          LEGAU.4     
      COMMON/LEGAU/ ALPJ(MJP),DALPJ(MJP)                                   LEGAU.5     
     +              ,ALP(NWJ2,2,JGL),DALP(NWJ2,2,JGL)                      LEGAU.6     
     +              ,RLP(NWJ2,2,JGL),RDLP(NWJ2,2,JGL)                      LEGAU.7     
     +              ,SI(JGG),CS(JGG),SISQ(JGG),CSSQ(JGG),SECSQ(JGG)        LEGAU.8     
     +              ,ALAT(JGG),GWT(JGG),AW(JGG),JH,JL,JINC                 LEGAU.9     
C                                                                          LEGAU.10    
C                                                                          INISTR.15    
 2000 FORMAT(' ***WARNING*** KITS < 1 FOR AN INITIAL RUN.'/)               INISTR.16    
 2010 FORMAT(/' ***ABORT*** THE HISTORY RECORDS READ FROM CHANNEL '        INISTR.17    
     + ,I3,/' ARE NOT IN CORRECT FORMAT ')                                 INISTR.18    
 2020 FORMAT(/' ***ABORT*** THE RUN NUMBER IN THE HISTORY RECORD'/         INISTR.19    
     + ' IS NOT THE SAME AS RNTAPE ENTERED IN NAMELIST')                   INISTR.20    
 2030 FORMAT(/' ***ABORT*** CANNOT FIND THE CORRECT HISTORY RECORD.'/      INISTR.21    
     + ' LOOKING FOR DAY',F8.2/,' BUT THE NEAREST RECORD FOUND',           INISTR.22    
     + ' IS FOR DAY',F8.2)                                                 INISTR.23    
 2040 FORMAT(/' HISTORY RECORD READ FROM CHANNEL ',I3,/                    INISTR.24    
     + ' KOUNT  RMTAPE  DAY =',I8,2F12.3)                                  INISTR.25    
 2011 FORMAT(/' ***ABORT*** THE RESTART RECORDS READ FROM CHANNEL '        INISTR.26    
     + ,I3,/' ARE NOT IN CORRECT FORMAT ')                                 INISTR.27    
 2021 FORMAT(/' ***ABORT*** THE RUN NUMBER IN THE RESTART RECORD'/         INISTR.28    
     + ' IS NOT THE SAME AS RNTAPE ENTERED IN NAMELIST')                   INISTR.29    
 2031 FORMAT(/' ***ABORT*** CANNOT FIND THE CORRECT RESTART RECORD.'/      INISTR.30    
     + ' LOOKING FOR DAY',F8.2/,' BUT THE NEAREST RECORD FOUND',           INISTR.31    
     + ' IS FOR DAY',F8.2)                                                 INISTR.32    
 2041 FORMAT(/' RESTART RECORD READ FROM CHANNEL ',I3,/                    INISTR.33    
     + ' KOUNT  RMTAPE  DAY =',I8,2F12.3)                                  INISTR.34    
 2012 FORMAT(/' ***ABORT*** THE RESTORATION RECORDS READ FROM CHANNEL '    INISTR.35    
     + ,I3,/' ARE NOT IN CORRECT FORMAT ')                                 INISTR.36    
 2022 FORMAT(/' ***ABORT*** THE RUN NUMBER IN THE RESTORATION RECORD'/     INISTR.37    
     + ' IS NOT THE SAME AS RNTAPE ENTERED IN NAMELIST')                   INISTR.38    
 2032 FORMAT(/' ***ABORT*** CANNOT FIND THE CORRECT RESTORATION RECORD.'   INISTR.39    
     + /' LOOKING FOR DAY',F8.2/,' BUT THE NEAREST RECORD FOUND',          INISTR.40    
     + ' IS FOR DAY',F8.2)                                                 INISTR.41    
 2042 FORMAT(' RESTORATION RECORD READ FROM CHANNEL ',I3,/                 INISTR.42    
     + ' KOUNT  RMTAPE  DAY =',I8,2F12.3)                                  INISTR.43    
 2050 FORMAT(/' SPECTRAL ARRAYS ARE SET TO ZERO ')                         INISTR.44    
C                                                                          INISTR.45    
C     initialise grid point fields to zero and read if required            NICK.251   
C                                                                          NICK.252   
      IF (KTRAIN.EQ.0) THEN                                                NICK.253   
                                                                           NICK.254   
      DO J=1,JG                                                            NICK.255   
      DO I=1,IGC                                                           NICK.256   
         GG(I,J)=0                                                         NICK.257   
         MASK(I,J)=0                                                       NICK.258   
      ENDDO                                                                NICK.259   
      ENDDO                                                                NICK.260   
                                                                           NICK.261   
      READ(18)GG                                                           NICK.262   
      READ(19)MASK                                                         NICK.263   
                                                                           NICK.264   
C     work out area averages of terms in drag coeff for normalization      NICK.265   
                                                                           NICK.266   
      RMASKAVE=0.                                                          NICK.267   
      FHAVE=0.                                                             NICK.268   
      CSAVE=0.                                                             NICK.269   
      DO J=1,JG                                                            NICK.270   
      DO IHEM=1,NHEM                                                       NICK.271   
      DO II=1,MG                                                           NICK.272   
         I=II+(IHEM-1)*MGPP                                                NICK.273   
         RMASKAVE=RMASKAVE+REAL(MASK(I,J))*CS(J)/REAL(NHEM*MG*JG)          NICK.274   
         FH=MASK(I,J)*(1.-EXP(-GG(I,J)/1000.))                             NICK.275   
         FHAVE=FHAVE+FH*CS(J)/REAL(NHEM*MG*JG)                             NICK.276   
      ENDDO                                                                NICK.277   
      ENDDO                                                                NICK.278   
         CSAVE=CSAVE+CS(J)/REAL(JG)                                        NICK.279   
      ENDDO                                                                NICK.280   
      RMASKAVE=RMASKAVE/CSAVE                                              NICK.281   
      FHAVE=FHAVE/CSAVE                                                    NICK.282   
                                                                           NICK.283   
      PRINT*,'AREA MEAN OF LAND SEA MASK IS = ',RMASKAVE                   NICK.284   
      PRINT*,'AREA MEAN OF OROGRAPHIC DRAG FUNCTION IS = ',FHAVE           NICK.285   
                                                                           NICK.286   
C     define a normalized drag coefficient that makes standard drag        NICK.287   
C     a function of land/sea and orography with area average 1.            NICK.288   
                                                                           NICK.289   
      DO J=1,JG                                                            NICK.290   
      DO I=1,IGC                                                           NICK.291   
         FH=MASK(I,J)*(1.-EXP(-GG(I,J)/1000.))                             NICK.292   
         CD(I,J)=(1. + AA1*REAL(MASK(I,J)) + AA2*FH)                       NICK.293   
     &           /(1. + AA1*RMASKAVE + AA2*FHAVE)                          NICK.294   
      ENDDO                                                                NICK.295   
      ENDDO                                                                NICK.296   
                                                                           NICK.297   
      END IF                                                               NICK.298   
C                                                                          NICK.299   
C     Initialize spectral arrays to zero and overwrite as desired          INISTR.46    
C                                                                          INISTR.47    
      DO 1 I=1,IGA                                                         INISTR.48    
         SP(I)=(0.0,0.0)                                                   INISTR.49    
         SPMI(I)=(0.,0.)                                                   INISTR.50    
         GS(I)=(0.0,0.0)                                                   INISTR.51    
    1 CONTINUE                                                             INISTR.52    
      DO 5 I=1,IGB                                                         INISTR.53    
         Z(I)=(0.0,0.0)                                                    INISTR.54    
         D(I)=(0.0,0.0)                                                    INISTR.55    
         T(I)=(0.0,0.0)                                                    INISTR.56    
         ZMI(I)=(0.0,0.0)                                                  INISTR.57    
         DMI(I)=(0.0,0.0)                                                  INISTR.58    
         TMI(I)=(0.0,0.0)                                                  INISTR.59    
    5 CONTINUE                                                             INISTR.60    
C                                                                          INISTR.61    
      IF (.NOT.LRSTRT) THEN                                                INISTR.62    
C                                                                          INISTR.63    
C        Initial run                                                       INISTR.64    
C                                                                          INISTR.65    
         IF ( KITS .LT. 1) THEN                                            INISTR.66    
            WRITE(2,2000)                                                  INISTR.67    
         ENDIF                                                             INISTR.68    
         DAY=0.0                                                           INISTR.69    
         IF (KITS.EQ.0) THEN                                               INISTR.70    
            KTOTAL=KRUN                                                    INISTR.71    
            KOUTP=0                                                        INISTR.72    
            KOUTE=0                                                        INISTR.73    
            KOUTH=0                                                        INISTR.74    
            KOUTR=0                                                        INISTR.75    
         ELSE                                                              INISTR.76    
            KTOTAL=KRUN+KITS-1                                             INISTR.77    
            KOUTP=1-KITS                                                   INISTR.78    
            KOUTE=1-KITS                                                   INISTR.79    
            KOUTH=1-KITS                                                   INISTR.80    
            KOUTR=1-KITS                                                   INISTR.81    
         END IF                                                            INISTR.82    
C                                                                          INISTR.83    
C        Initialise restoration array                                      INISTR.84    
C                                                                          INISTR.85    
         IF (LRESTIJ) CALL SETZT                                           INISTR.86    
C                                                                          INISTR.87    
C        Initialise spectral arrays                                        INISTR.88    
C                                                                          INISTR.89    
         IF (LBALAN) THEN                                                  INISTR.90    
           CALL INIBAL                                                     INISTR.91    
           KOUNT=-KBAL                                                     INISTR.92    
         ELSE                                                              INISTR.93    
           IF (LRESTIJ) THEN                                               INISTR.94    
              CALL INISP                                                   INISTR.95    
           ELSE                                                            INISTR.96    
              WRITE (2,2050)                                               INISTR.97    
           ENDIF                                                           INISTR.98    
           KOUNT=0                                                         INISTR.99    
         ENDIF                                                             INISTR.100   
      ELSE                                                                 INISTR.101   
C                                                                          INISTR.102   
C        Code for restart and normal mode perturbation runs.               INISTR.103   
C        assume spectral data is set up non-dimensionalised                INISTR.104   
C        on a history (LSHORT) or restart (.NOT.LSHORT) record.            INISTR.105   
C                                                                          INISTR.106   
         ID=10                                                             INISTR.107   
         DAYNEAR=0.0                                                       INISTR.108   
         IF (LSHORT) THEN                                                  INISTR.109   
  180       READ(ID,END=1000)RKOUNT,RM1TAPE,DAY,Z,D,T,SP,RM2TAPE           INISTR.110   
            IF (ABS(RM1TAPE-RM2TAPE) .GT. 1.0E-03) THEN                    INISTR.111   
               WRITE(2,2010) ID                                            INISTR.112   
               STOP                                                        INISTR.113   
            ENDIF                                                          INISTR.114   
            IF (ABS(100.-RM2TAPE) .GT. 1.0E-03) THEN                       NICK.37    
               WRITE(2,2020)                                               INISTR.116   
               STOP                                                        INISTR.117   
            ENDIF                                                          INISTR.118   
            IF (ABS(DAY-BEGDAY) .GT. 1.0E-02) THEN                         INISTR.119   
               IF (ABS(DAY-BEGDAY) .LT. ABS(DAYNEAR-BEGDAY)) THEN          INISTR.120   
                  DAYNEAR = DAY                                            INISTR.121   
               ENDIF                                                       INISTR.122   
               GOTO 180                                                    INISTR.123   
            ELSE                                                           INISTR.124   
               GOTO 200                                                    INISTR.125   
            ENDIF                                                          INISTR.126   
 1000       WRITE(2,2030) BEGDAY,DAYNEAR                                   INISTR.127   
            STOP                                                           INISTR.128   
C                                                                          INISTR.129   
  200       KOUNT=NINT(RKOUNT)                                             INISTR.130   
            WRITE(2,2040) ID,KOUNT,RM1TAPE,BEGDAY                          INISTR.131   
            KOUNT=0                                                        INISTR.132   
            IF (KITS.EQ.0) THEN                                            INISTR.133   
               KTOTAL=KRUN                                                 INISTR.134   
               KOUTP=0                                                     INISTR.135   
               KOUTE=0                                                     INISTR.136   
               KOUTH=0                                                     INISTR.137   
               KOUTR=0                                                     INISTR.138   
            ELSE                                                           INISTR.139   
               KTOTAL=KRUN+KITS-1                                          INISTR.140   
               KOUTP=1-KITS                                                INISTR.141   
               KOUTE=1-KITS                                                INISTR.142   
               KOUTH=1-KITS                                                INISTR.143   
               KOUTR=1-KITS                                                INISTR.144   
            ENDIF                                                          INISTR.145   
            DO 183 I=1,IGA                                                 INISTR.146   
               SPMI(I)=SP(I)                                               INISTR.147   
  183       CONTINUE                                                       INISTR.148   
            DO 184 J=1,IGB                                                 INISTR.149   
               ZMI(J)=Z(J)                                                 INISTR.150   
               DMI(J)=D(J)                                                 INISTR.151   
               TMI(J)=T(J)                                                 INISTR.152   
  184       CONTINUE                                                       INISTR.153   
         ELSE                                                              INISTR.154   
  181       READ(ID,END=1001)RKOUNT,RM1TAPE,DAY,Z,D,T,SP,RM2TAPE           INISTR.155   
     +                      ,ZMI,DMI,TMI,SPMI,RM3TAPE                      INISTR.156   
            IF (ABS(RM1TAPE-RM2TAPE) .GT. 1.0E-03 .OR.                     INISTR.157   
     +          ABS(RM1TAPE-RM3TAPE) .GT. 1.0E-03) THEN                    INISTR.158   
               WRITE(2,2011) ID                                            INISTR.159   
               STOP                                                        INISTR.160   
            ENDIF                                                          INISTR.161   
            IF (ABS(100.-RM2TAPE) .GT. 1.0E-03) THEN                       NICK.38    
               WRITE(2,2021)                                               INISTR.163   
               STOP                                                        INISTR.164   
            ENDIF                                                          INISTR.165   
            IF (ABS(DAY-BEGDAY) .GT. 1.0E-02) THEN                         INISTR.166   
               IF (ABS(DAY-BEGDAY) .LT. ABS(DAYNEAR-BEGDAY)) THEN          INISTR.167   
                  DAYNEAR = DAY                                            INISTR.168   
               ENDIF                                                       INISTR.169   
               GOTO 181                                                    INISTR.170   
            ELSE                                                           INISTR.171   
               GOTO 201                                                    INISTR.172   
            ENDIF                                                          INISTR.173   
 1001       WRITE(2,2031) BEGDAY,DAYNEAR                                   INISTR.174   
            STOP                                                           INISTR.175   
C                                                                          INISTR.176   
  201       KOUNT=NINT(RKOUNT)                                             INISTR.177   
            WRITE(2,2041) ID,KOUNT,RM1TAPE,BEGDAY                          INISTR.178   
            KTOTAL=KOUNT+KRUN                                              INISTR.179   
            KTEMP=KOUNT                                                    INISTR.180   
            IF(KITS.GT.0) KTEMP=KOUNT+1-KITS                               INISTR.181   
            KOUTP=KTEMP-KOUNTP*(KTEMP/KOUNTP)                              INISTR.182   
            KOUTE=KTEMP-KOUNTE*(KTEMP/KOUNTE)                              INISTR.183   
            KOUTH=KTEMP-KOUNTH*(KTEMP/KOUNTH)                              INISTR.184   
            KOUTR=KTEMP-KOUNTR*(KTEMP/KOUNTR)                              INISTR.185   
         END IF                                                            INISTR.186   
C                                                                          INISTR.187   
C        Read in restoration state from separate file                      INISTR.188   
C                                                                          INISTR.189   
         IF (LRESTIJ) THEN                                                 INISTR.190   
            ID=13                                                          INISTR.191   
  182       READ(ID,END=1002)RKOUNT,RM1TAPE,DAY,TTRES,RM2TAPE              INISTR.192   
            IF (ABS(RM1TAPE-RM2TAPE) .GT. 1.0E-03) THEN                    INISTR.193   
              WRITE(2,2012) ID                                             INISTR.194   
              STOP                                                         INISTR.195   
            ENDIF                                                          INISTR.196   
            IF (ABS(RM1TAPE-RNTAPE) .GT. 1.0E-03) THEN                     INISTR.197   
               WRITE(2,2022)                                               INISTR.198   
               STOP                                                        INISTR.199   
            ENDIF                                                          INISTR.200   
            IF (ABS(DAY-BEGDAY) .GT. 1.0E-02) THEN                         INISTR.201   
               IF (ABS(DAY-BEGDAY) .LT. ABS(DAYNEAR-BEGDAY)) THEN          INISTR.202   
                 DAYNEAR = DAY                                             INISTR.203   
               ENDIF                                                       INISTR.204   
               GOTO 182                                                    INISTR.205   
            ELSE                                                           INISTR.206   
               GOTO 202                                                    INISTR.207   
            ENDIF                                                          INISTR.208   
 1002       WRITE(2,2032) BEGDAY,DAYNEAR                                   INISTR.209   
            STOP                                                           INISTR.210   
C                                                                          INISTR.211   
  202       WRITE(2,2042) ID,NINT(RKOUNT),RM1TAPE,BEGDAY                   INISTR.212   
         ELSE                                                              INISTR.213   
C     IF (KTRAIN.EQ.0) THEN                                                NICK.216   
         IF (LFCE) THEN                                                    NICK.217   
            REWIND(13)                                                     NICK.218   
            READ(13)ZFCE,DFCE,TFCE,SPFCE                                   NICK.219   
         END IF                                                            NICK.220   
         IF (LFED) THEN                                                    NICK.221   
            REWIND(14)                                                     NICK.222   
            READ(14)ZFED,DFED,TFED,SPFED                                   NICK.223   
C           IF (LLIN) THEN                                                 NICK.224   
C              DO I=1,IGB                                                  NICK.225   
C              ZFED(I)=ZFED(I)/1.E4                                        NICK.226   
C              DFED(I)=DFED(I)/1.E4                                        NICK.227   
C              TFED(I)=TFED(I)/1.E4                                        NICK.228   
C              ENDDO                                                       NICK.229   
C              DO I=1,IGA                                                  NICK.230   
C              SPFED(I)=SPFED(I)/1.E4                                      NICK.231   
C              ENDDO                                                       NICK.232   
C           ENDIF                                                          NICK.233   
         END IF                                                            NICK.234   
         IF (LFAN) THEN                                                    NICK.235   
            REWIND(15)                                                     NICK.236   
            READ(15)ZFAN,DFAN,TFAN,SPFAN                                   NICK.237   
            IF (LLIN) THEN                                                 NICK.238   
               DO I=1,IGB                                                  NICK.239   
               ZFAN(I)=ZFAN(I)/1.E4                                        NICK.240   
               DFAN(I)=DFAN(I)/1.E4                                        NICK.241   
               TFAN(I)=TFAN(I)/1.E4                                        NICK.242   
               ENDDO                                                       NICK.243   
               DO I=1,IGA                                                  NICK.244   
               SPFAN(I)=SPFAN(I)/1.E4                                      NICK.245   
               ENDDO                                                       NICK.246   
            ENDIF                                                          NICK.247   
         END IF                                                            NICK.248   
C     END IF                                                               NICK.249   
         ENDIF                                                             INISTR.218   
C                                                                          INISTR.219   
      END IF                                                               INISTR.220   
C                                                                          INISTR.221   
      END                                                                  INISTR.222   
      SUBROUTINE INIBAL                                                    INIBAL.2     
C                                                                          INIBAL.3     
C     This subroutine reads data used for balancing and                    INIBAL.4     
C     calculates arrays needed for balancing.                              INIBAL.5     
C                                                                          INIBAL.6     
C                                                                          PARAM1.2     
C     Determines model resolution                                          PARAM1.3     
C                                                                          PARAM1.4     
      PARAMETER(NN=21,MM=21,NHEM=2,NL=5,MOCT=1,MG=64,JG=16,NWJ2=121        G100.1     
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
      COMMON/SPECTR/Z(IGB),D(IGB),T(IGB),SP(IGA),GS(IGA)                   SPECTR.5     
     *              ,SPA(IGA),VP(IGA),DTE(IGB),TT(IGB),DT(IGB),ZT(IGB)     SPECTR.6     
     *              ,ZMI(IGB),DMI(IGB),TMI(IGB),SPMI(IGA)                  SPECTR.7     
      COMPLEX Z,D,T,SP,GS,SPA,VP,DTE,TT,DT,ZT,ZMI,DMI,TMI,SPMI             SPECTR.8     
C                                                                          SPECTR.9     
C                                                                          BATS.2     
C     Constant arrays and variables associated with time and vertical      BATS.3     
C     differencing. Also counters.                                         BATS.4     
C                                                                          BATS.5     
      COMMON/BATS/  BM1(IDE),AK(NNP),AQ(NL2),G(NL2),TAU(NL2)               BATS.6     
     +              ,KOUNT,KITS,KTOTAL,KRUN,BEGDAY,ITSPD,PNU21             BATS.7     
     +              ,DELT,DELT2,CV,CG,CT,PNU,PNU2                          BATS.8     
C                                                                          BATS.9     
C                                                                          OUTCON.2     
C     Switches counters and constants controlling type and frequency of    OUTCON.3     
C     model output                                                         OUTCON.4     
C                                                                          OUTCON.5     
      COMMON/OUTCON/RNTAPE,NCOEFF,NLAT,INLAT,INSPC                         OUTCON.6     
     +              ,KOUNTP,KOUNTE,KOUNTH,KOUNTR                           OUTCON.7     
     +              ,KOUTP,KOUTE,KOUTH,KOUTR,DAY                           OUTCON.8     
     +              ,SQR2,RSQR2,EAM1,EAM2,TOUT1,TOUT2,RMG                  OUTCON.9     
     +              ,LSPO(NL),LGPO(NL)                                     OUTCON.10    
      LOGICAL LSPO,LGPO                                                    OUTCON.11    
C                                                                          OUTCON.12    
C                                                                          RESTOR.2     
C     Restoration fields and timescale                                     RESTOR.3     
C                                                                          RESTOR.4     
      COMMON/RESTOR/ZRES(IGN),DRES(IGN),TRES(IGN),SPRES(IGM),DAMP          RESTOR.5     
     &,ZFCE(IGB),DFCE(IGB),TFCE(IGB),SPFCE(IGA)                            NICK.207   
     &,ZFED(IGB),DFED(IGB),TFED(IGB),SPFED(IGA)                            NICK.208   
     &,ZFAN(IGB),DFAN(IGB),TFAN(IGB),SPFAN(IGA)                            NICK.209   
     &,ZDMP(IGB),DDMP(IGB),TDMP(IGB),SPDMP(IGA)                            NICK.210   
     &,ZDMI(IGB),DDMI(IGB),TDMI(IGB),SPDMI(IGA)                            NICK.211   
     &,MASK(IGC,JG),GG(IGC,JG),CD(IGC,JG),AA1,AA2,AAT                      NICK.212   
      COMPLEX ZFCE,DFCE,TFCE,SPFCE,ZFED,DFED,TFED,SPFED                    NICK.213   
     &,ZFAN,DFAN,TFAN,SPFAN                                                NICK.214   
     &,ZDMP,DDMP,TDMP,SPDMP,ZDMI,DDMI,TDMI,SPDMI                           NICK.215   
C                                                                          RESTOR.6     
C                                                                          BALAN.2     
C     Constants and arrays needed for balancing                            BALAN.3     
C                                                                          BALAN.4     
      COMMON/BALAN/BFILT(NL),RGT0(NL),RG(NL2),TMEAN(NL)                    BALAN.5     
     +            ,EP1(IGA),EP2(IGA),KBAL,MFTBAL,SRGT0,LTBAL               BALAN.6     
      LOGICAL LTBAL                                                        BALAN.7     
C                                                                          BALAN.8     
      PARAMETER(IDI2=IDI+IDI,NLT=NL+NL)                                    INIBAL.14    
C                                                                          INIBAL.15    
      DIMENSION TBM1(NL2),WA1(IDI),WA2(NL)                                 SC961212.7     
      INTEGER IWA1(IDI),IWA2(NL)                                           SC961212.8     
      DIMENSION TBME(NL,NL)                                                INIBAL.17    
      EQUIVALENCE (TBM1(1),TBME(1,1))                                      INIBAL.18    
      DIMENSION AU(IDJ),SM(IDJ),PMNRE(IDJ)                                 INIBAL.19    
      DIMENSION AUE(IDI,IDI)                                               INIBAL.20    
      EQUIVALENCE (AU(1),AUE(1,1))                                         INIBAL.21    
C                                                                          INIBAL.22    
      DIMENSION ZDATN(IGN),ZDAT1(IGM)                                      INIBAL.23    
C                                                                          INIBAL.24    
      NAMELIST/INPBL/ KBAL,LTBAL,TMEAN                                     INIBAL.25    
      NAMELIST/TMPSP/ ZDATN,ZDAT1                                          INIBAL.26    
      NAMELIST/WVORT/ ZDATN                                                INIBAL.27    
C                                                                          INIBAL.28    
  219 FORMAT(/' BALANCING FROM TEMPERATURE AND SURFACE PRESSURE TO',       INIBAL.29    
     +       ' OBTAIN VORTICITY')                                          INIBAL.30    
  220 FORMAT(/' BALANCING FROM VORTICITY TO OBTAIN TEMPERATURE AND',       INIBAL.31    
     +       ' SURFACE PRESSURE')                                          INIBAL.32    
  234 FORMAT(/' ***ABORT*** BALANCING ATTEMPTED WITH OROGRAPHY'/           INIBAL.33    
     +       ' TEMPERATURE FIELD WOULD CONTAIN 2-GRID WAVE')               INIBAL.34    
C                                                                          INIBAL.35    
C     Set default values and override as desired through NAMELIST input    INIBAL.36    
C                                                                          INIBAL.37    
      KBAL=0                                                               INIBAL.38    
      LTBAL=.FALSE.                                                        INIBAL.39    
      DO 21 L=1,NL                                                         INIBAL.40    
         TMEAN(L)=250.0                                                    INIBAL.41    
   21 CONTINUE                                                             INIBAL.42    
C                                                                          INIBAL.43    
      REWIND(7)                                                            NICK.40    
      READ(7,INPBL)                                                        INIBAL.44    
      WRITE(2,INPBL)                                                       INIBAL.45    
C                                                                          INIBAL.46    
      IF (     LTBAL) WRITE(2,219)                                         INIBAL.47    
      IF (.NOT.LTBAL) WRITE(2,220)                                         INIBAL.48    
C                                                                          INIBAL.49    
C     Make TMEAN dimensionless                                             INIBAL.50    
C                                                                          INIBAL.51    
      DO 62 L=1,NL                                                         INIBAL.52    
        TMEAN(L)=TMEAN(L)/CT                                               INIBAL.53    
   62 CONTINUE                                                             INIBAL.54    
C                                                                          INIBAL.55    
C     Read zonally averaged state from NAMELIST WVORT or TMPSP.            INIBAL.56    
C     Assumes data is non-dimensionalised spectral coefficients            INIBAL.57    
C     and that temperature (if used) includes layer mean.                  INIBAL.58    
C                                                                          INIBAL.59    
C*****temporarily use DMP fields to read in initial data to be balanced    NICK.41    
            READ(10)RKOUNT,RM1TAPE,DAY,ZDMP,DDMP,TDMP,SPDMP,RM2TAPE        NICK.42    
            IF (ABS(RM1TAPE-RM2TAPE) .GT. 1.0E-03) THEN                    NICK.43    
               PRINT*,'RECORD WRONG LENGTH IN A BALANCING RUN'             NICK.44    
               STOP                                                        NICK.45    
            ENDIF                                                          NICK.46    
            IF (ABS(100.-RM2TAPE) .GT. 1.0E-03) THEN                       NICK.47    
               PRINT*,'RECORD IDENTIFIER WRONG IN A BALANCING RUN'         NICK.48    
               STOP                                                        NICK.49    
            ENDIF                                                          NICK.50    
      IF(.NOT.LTBAL) THEN                                                  INIBAL.60    
      DO K=1,IGB                                                           NICK.51    
      Z(K)=ZDMP(K)                                                         NICK.52    
      D(K)=DDMP(K)                                                         NICK.53    
      ENDDO                                                                NICK.54    
         I=1                                                               INIBAL.74    
         DO 170 L=1,NL                                                     INIBAL.75    
            T(I)=T(I)+SQR2*(TMEAN(L)-T0(L))                                INIBAL.76    
            I=I+IGA                                                        INIBAL.77    
  170    CONTINUE                                                          INIBAL.78    
      ELSE                                                                 INIBAL.79    
      DO K=1,IGB                                                           NICK.55    
      T(K)=TDMP(K)                                                         NICK.56    
      D(K)=DDMP(K)                                                         NICK.57    
      ENDDO                                                                NICK.58    
      DO K=1,IGA                                                           NICK.59    
      SP(K)=SPDMP(K)                                                       NICK.60    
      ENDDO                                                                NICK.61    
      IL=1                                                                 INIBAL.108   
      DO 174 L=1,NL                                                        INIBAL.109   
         Z(IL)=Z(IL)+EZ                                                    INIBAL.110   
         IL=IL+IGA                                                         INIBAL.111   
  174 CONTINUE                                                             INIBAL.112   
      ENDIF                                                                NICK.62    
C                                                                          INIBAL.113   
      IF (KBAL.EQ.0) RETURN                                                INIBAL.114   
C                                                                          INIBAL.115   
      IF(.NOT.LTBAL) THEN                                                  INIBAL.116   
C                                                                          INIBAL.117   
C        Set values required in BALANC.                                    INIBAL.118   
C        With orography the balanced temperature field contains a          INIBAL.119   
C        2-grid wave in the vertical. ABORT if this is attempted.          INIBAL.120   
C                                                                          INIBAL.121   
         MAXIND=ICAMAX(IGA,GS,1)                                           INIBAL.122   
         GSMAX=ABS(GS(MAXIND))                                             INIBAL.123   
         IF(GSMAX.GT.1.0E-10) THEN                                         INIBAL.124   
           WRITE(2,234)                                                    INIBAL.125   
           STOP                                                            INIBAL.126   
         ENDIF                                                             INIBAL.127   
         DO 90 L=1,NL2                                                     INIBAL.128   
            TBM1(L)=G(L)                                                   INIBAL.129   
   90    CONTINUE                                                          INIBAL.130   
         CALL MATINV(TBME,NL,NL,IWA2,WA2)                                  SC961212.9     
         DO 92 L=1,NL2                                                     INIBAL.132   
            RG(L)=TBM1(L)                                                  INIBAL.133   
   92    CONTINUE                                                          INIBAL.134   
         DO 91 L=1,NL                                                      INIBAL.135   
            BFILT(L)=0.                                                    INIBAL.136   
   91    CONTINUE                                                          INIBAL.137   
         BFILT(1)=1.                                                       INIBAL.138   
         BFILT(2)=1.0                                                      INIBAL.139   
         IF (NLM.GE.2) THEN                                                INIBAL.140   
            TEMPP=1.0                                                      INIBAL.141   
            DO 93 I=2,NLM                                                  INIBAL.142   
               DO 94 J=2,I                                                 INIBAL.143   
                  TEMP=BFILT(J)                                            INIBAL.144   
                  BFILT(J)=TEMP+TEMPP                                      INIBAL.145   
                  TEMPP=TEMP                                               INIBAL.146   
   94          CONTINUE                                                    INIBAL.147   
               BFILT(I+1)=1.0                                              INIBAL.148   
   93       CONTINUE                                                       INIBAL.149   
         ENDIF                                                             INIBAL.150   
         FACT=-1.0                                                         INIBAL.151   
         DO 95 I=2,NL                                                      INIBAL.152   
            BFILT(I)=BFILT(I)*FACT                                         INIBAL.153   
            FACT=-FACT                                                     INIBAL.154   
   95    CONTINUE                                                          INIBAL.155   
         SRGT0=0.                                                          INIBAL.156   
         IG=0                                                              INIBAL.157   
         DO 98 L=1,NL                                                      INIBAL.158   
            TRGT0=0.                                                       INIBAL.159   
            DO 97 M=1,NL                                                   INIBAL.160   
               IG=IG+1                                                     INIBAL.161   
               TRGT0=TRGT0+RG(IG)*T0(M)                                    INIBAL.162   
   97       CONTINUE                                                       INIBAL.163   
            RGT0(L)=TRGT0                                                  INIBAL.164   
            SRGT0=SRGT0+TRGT0*BFILT(L)                                     INIBAL.165   
   98    CONTINUE                                                          INIBAL.166   
      ELSE                                                                 INIBAL.167   
C                                                                          INIBAL.168   
C        Set values required in TBAL                                       INIBAL.169   
C                                                                          INIBAL.170   
         MWV1=1+MOCT                                                       INIBAL.171   
         MFTBAL=9                                                          INIBAL.172   
         REWIND 9                                                          INIBAL.173   
C                                                                          INIBAL.174   
         IE=0                                                              INIBAL.175   
         DO 410 MP=1,MFTBAL,MOCT                                           INIBAL.176   
            AM=MP-1                                                        INIBAL.177   
            AMSQ=AM*AM                                                     INIBAL.178   
            AN=AM+1.0                                                      INIBAL.179   
            DO 409 NP=MP,NFP,MH                                            INIBAL.180   
               IE=IE+1                                                     INIBAL.181   
               ANSQ=AN*AN                                                  INIBAL.182   
               EP2(IE)=2.0*SQRT((ANSQ-AMSQ)/                               INIBAL.183   
     +                 (4.0*ANSQ-1.0))*(1.0-1.0/AN)                        INIBAL.184   
               AN=AN+1.0                                                   INIBAL.185   
               ANSQ=AN*AN                                                  INIBAL.186   
               EP1(IE)=2.0*SQRT((ANSQ-AMSQ)/                               INIBAL.187   
     +                 (4.0*ANSQ-1.0))*(1.0+1.0/AN)                        INIBAL.188   
               AN=AN+1.0                                                   INIBAL.189   
 409        CONTINUE                                                       INIBAL.190   
 410     CONTINUE                                                          INIBAL.191   
         IE=(NFP-1)/2+1                                                    INIBAL.192   
         DO 1220 MP=MWV1,MFTBAL,MOCT                                       INIBAL.193   
            J2=(NFP-MP)/2+1                                                INIBAL.194   
            J22=J2*J2                                                      INIBAL.195   
            DO 1202 I=1,J22                                                INIBAL.196   
               AU(I)=0.0                                                   INIBAL.197   
               SM(I)=0.0                                                   INIBAL.198   
 1202       CONTINUE                                                       INIBAL.199   
            IJ=1                                                           INIBAL.200   
            DO 1205 J=1,J2                                                 INIBAL.201   
               IE=IE+1                                                     INIBAL.202   
               SM(IJ)=EP2(IE)                                              INIBAL.203   
               AU(IJ)=EP2(IE)*EP2(IE)+EP1(IE)*EP1(IE)                      INIBAL.204   
               IF(J.GT.1)AU(IJ-1)=EP1(IE-1)*EP2(IE)                        INIBAL.205   
               IF (J.NE.J2) THEN                                           INIBAL.206   
                  SM(IJ+1)=EP1(IE)                                         INIBAL.207   
                  AU(IJ+1)=EP1(IE)*EP2(IE+1)                               INIBAL.208   
               ENDIF                                                       INIBAL.209   
               IJ=IJ+J2+1                                                  INIBAL.210   
 1205       CONTINUE                                                       INIBAL.211   
            CALL MATINV(AUE,J2,J2,IWA1,WA1)                                SC961212.10    
            IJ=0                                                           INIBAL.213   
            IUS=1                                                          INIBAL.214   
            DO 1210 I=1,J2                                                 INIBAL.215   
               DO 1208 J=1,J2                                              INIBAL.216   
                  TAL=0.0                                                  INIBAL.217   
                  IJ=IJ+1                                                  INIBAL.218   
                  IU=IUS                                                   INIBAL.219   
                  IM=J                                                     INIBAL.220   
                  DO 1207 K=1,J2                                           INIBAL.221   
                     TAL=TAL+AU(IU)*SM(IM)                                 INIBAL.222   
                     IU=IU+1                                               INIBAL.223   
                     IM=IM+J2                                              INIBAL.224   
 1207             CONTINUE                                                 INIBAL.225   
                  PMNRE(IJ)=TAL                                            INIBAL.226   
 1208          CONTINUE                                                    INIBAL.227   
               IUS=IUS+J2                                                  INIBAL.228   
 1210       CONTINUE                                                       INIBAL.229   
            WRITE(9)(PMNRE(I),I=1,J22)                                     INIBAL.230   
 1220    CONTINUE                                                          INIBAL.231   
C                                                                          INIBAL.232   
         IF(NHEM.EQ.2) THEN                                                INIBAL.233   
            IE=1+NWJ2                                                      INIBAL.234   
            AN=2.0                                                         INIBAL.235   
            DO 420 NP=3,NFP,MH                                             INIBAL.236   
               IE=IE+1                                                     INIBAL.237   
               ANSQ=AN*AN                                                  INIBAL.238   
               EP2(IE)=2.0*SQRT(ANSQ/(4.0*ANSQ-1.0))*(1.0-1.0/AN)          INIBAL.239   
               AN=AN+1.0                                                   INIBAL.240   
               ANSQ=AN*AN                                                  INIBAL.241   
               EP1(IE)=2.0*SQRT(ANSQ/(4.0*ANSQ-1.0))*(1.0+1.0/AN)          INIBAL.242   
               AN=AN+1.0                                                   INIBAL.243   
  420       CONTINUE                                                       INIBAL.244   
            DO 430 MP=MWV1,MFTBAL,MOCT                                     INIBAL.245   
               AM=MP-1                                                     INIBAL.246   
               AMSQ=AM*AM                                                  INIBAL.247   
               AN=AM+1.0                                                   INIBAL.248   
               DO 429 NP=MP,NFP,MH                                         INIBAL.249   
                  IE=IE+1                                                  INIBAL.250   
                  ANSQ=AN*AN                                               INIBAL.251   
                  EP1(IE)=2.0*SQRT((ANSQ-AMSQ)/                            INIBAL.252   
     +                    (4.0*ANSQ-1.0))*(1.0+1.0/AN)                     INIBAL.253   
                  AN=AN+1.0                                                INIBAL.254   
                  ANSQ=AN*AN                                               INIBAL.255   
                  EP2(IE)=2.0*SQRT((ANSQ-AMSQ)/                            INIBAL.256   
     +                    (4.0*ANSQ-1.0))*(1.0-1.0/AN)                     INIBAL.257   
                  AN=AN+1.0                                                INIBAL.258   
  429          CONTINUE                                                    INIBAL.259   
  430       CONTINUE                                                       INIBAL.260   
            IE=1+NWJ2                                                      INIBAL.261   
            J2=IDM                                                         INIBAL.262   
            J22=J2*J2                                                      INIBAL.263   
            J2L=J2-1                                                       INIBAL.264   
            J22L=J2*J2L                                                    INIBAL.265   
            DO 1222 I=1,J22                                                INIBAL.266   
               AU(I)=0.0                                                   INIBAL.267   
               SM(I)=0.0                                                   INIBAL.268   
 1222       CONTINUE                                                       INIBAL.269   
            IJ=1                                                           INIBAL.270   
            DO 1225 J=1,J2L                                                INIBAL.271   
               IE=IE+1                                                     INIBAL.272   
               IJB=IJ+J-1                                                  INIBAL.273   
               SM(IJB)=EP2(IE)                                             INIBAL.274   
               AU(IJ)=EP1(IE)*EP1(IE) + EP2(IE)*EP2(IE)                    INIBAL.275   
               IF(J.GT.1) AU(IJ-1)=EP1(IE-1)*EP2(IE)                       INIBAL.276   
               SM(IJB+1)=EP1(IE)                                           INIBAL.277   
               IF(J.LT.J2L) AU(IJ+1)=EP1(IE)*EP2(IE+1)                     INIBAL.278   
               IJ=IJ+J2                                                    INIBAL.279   
 1225       CONTINUE                                                       INIBAL.280   
            CALL MATINV(AUE,J2L,J2L,IWA1,WA1)                              SC961212.11    
            IJ=0                                                           INIBAL.282   
            IUS=1                                                          INIBAL.283   
            DO 1230 I=1,J2L                                                INIBAL.284   
               DO 1228 J=1,J2                                              INIBAL.285   
                  TAL=0.0                                                  INIBAL.286   
                  IJ=IJ+1                                                  INIBAL.287   
                  IU=IUS                                                   INIBAL.288   
                  IM=J                                                     INIBAL.289   
                  DO 1227 K=1,J2L                                          INIBAL.290   
                     TAL=TAL + AU(IU)*SM(IM)                               INIBAL.291   
                     IU=IU+1                                               INIBAL.292   
                     IM=IM+J2                                              INIBAL.293   
 1227             CONTINUE                                                 INIBAL.294   
                  PMNRE(IJ)=TAL                                            INIBAL.295   
 1228          CONTINUE                                                    INIBAL.296   
               IUS=IUS+J2L                                                 INIBAL.297   
 1230       CONTINUE                                                       INIBAL.298   
            WRITE(9) (PMNRE(I),I=1,J22L)                                   INIBAL.299   
         ENDIF                                                             INIBAL.300   
C                                                                          INIBAL.301   
      ENDIF                                                                INIBAL.302   
C                                                                          INIBAL.303   
      END                                                                  INIBAL.304   
C     ******************************************************************   INIBAL.305   
      SUBROUTINE INISP                                                     INISP.2     
C                                                                          INISP.3     
C     Initialise spectral arrays                                           INISP.4     
C                                                                          INISP.5     
C                                                                          PARAM1.2     
C     Determines model resolution                                          PARAM1.3     
C                                                                          PARAM1.4     
      PARAMETER(NN=21,MM=21,NHEM=2,NL=5,MOCT=1,MG=64,JG=16,NWJ2=121        G100.1     
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
      COMMON/SPECTR/Z(IGB),D(IGB),T(IGB),SP(IGA),GS(IGA)                   SPECTR.5     
     *              ,SPA(IGA),VP(IGA),DTE(IGB),TT(IGB),DT(IGB),ZT(IGB)     SPECTR.6     
     *              ,ZMI(IGB),DMI(IGB),TMI(IGB),SPMI(IGA)                  SPECTR.7     
      COMPLEX Z,D,T,SP,GS,SPA,VP,DTE,TT,DT,ZT,ZMI,DMI,TMI,SPMI             SPECTR.8     
C                                                                          SPECTR.9     
C                                                                          OUTCON.2     
C     Switches counters and constants controlling type and frequency of    OUTCON.3     
C     model output                                                         OUTCON.4     
C                                                                          OUTCON.5     
      COMMON/OUTCON/RNTAPE,NCOEFF,NLAT,INLAT,INSPC                         OUTCON.6     
     +              ,KOUNTP,KOUNTE,KOUNTH,KOUNTR                           OUTCON.7     
     +              ,KOUTP,KOUTE,KOUTH,KOUTR,DAY                           OUTCON.8     
     +              ,SQR2,RSQR2,EAM1,EAM2,TOUT1,TOUT2,RMG                  OUTCON.9     
     +              ,LSPO(NL),LGPO(NL)                                     OUTCON.10    
      LOGICAL LSPO,LGPO                                                    OUTCON.11    
C                                                                          OUTCON.12    
C                                                                          RESTIJ.2     
C     Restoration temperature field and constants which determine it,      RESTIJ.3     
C     also contains timescales                                             RESTIJ.4     
C                                                                          RESTIJ.5     
      COMMON/RESTIJ/TTRES(IGB)                                             RESTIJ.6     
     + ,DTNS,DTEP,DTTRP,FAC(NL),DDAMP(NL),TFRC(NL),YRLEN,TRS(NL)           RESTIJ.7     
     +  ,ALR,ZTROP,TGR                                                     RESTIJ.8     
      COMPLEX TTRES                                                        RESTIJ.9     
C                                                                          INISP.12    
      I=1                                                                  INISP.13    
      DO 170 L=1,NL                                                        INISP.14    
         T(I)=T(I)+SQR2*(TRS(L)-T0(L))                                     INISP.15    
         TMI(I)=TMI(I)+SQR2*(TRS(L)-T0(L))                                 INISP.16    
         I=I+IGA                                                           INISP.17    
 170  CONTINUE                                                             INISP.18    
      IL=1                                                                 INISP.19    
      DO 174 L=1,NL                                                        INISP.20    
         Z(IL)=Z(IL)+EZ                                                    INISP.21    
         ZMI(IL)=ZMI(IL)+EZ                                                INISP.22    
         IL=IL+IGA                                                         INISP.23    
 174  CONTINUE                                                             INISP.24    
C                                                                          INISP.25    
      END                                                                  INISP.26    
      SUBROUTINE BALANC                                                    BALANC.2     
C                                                                          BALANC.3     
C                                                                          PARAM1.2     
C     Determines model resolution                                          PARAM1.3     
C                                                                          PARAM1.4     
      PARAMETER(NN=21,MM=21,NHEM=2,NL=5,MOCT=1,MG=64,JG=16,NWJ2=121        G100.1     
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
      COMMON/SPECTR/Z(IGB),D(IGB),T(IGB),SP(IGA),GS(IGA)                   SPECTR.5     
     *              ,SPA(IGA),VP(IGA),DTE(IGB),TT(IGB),DT(IGB),ZT(IGB)     SPECTR.6     
     *              ,ZMI(IGB),DMI(IGB),TMI(IGB),SPMI(IGA)                  SPECTR.7     
      COMPLEX Z,D,T,SP,GS,SPA,VP,DTE,TT,DT,ZT,ZMI,DMI,TMI,SPMI             SPECTR.8     
C                                                                          SPECTR.9     
C                                                                          BATS.2     
C     Constant arrays and variables associated with time and vertical      BATS.3     
C     differencing. Also counters.                                         BATS.4     
C                                                                          BATS.5     
      COMMON/BATS/  BM1(IDE),AK(NNP),AQ(NL2),G(NL2),TAU(NL2)               BATS.6     
     +              ,KOUNT,KITS,KTOTAL,KRUN,BEGDAY,ITSPD,PNU21             BATS.7     
     +              ,DELT,DELT2,CV,CG,CT,PNU,PNU2                          BATS.8     
C                                                                          BATS.9     
C                                                                          BALAN.2     
C     Constants and arrays needed for balancing                            BALAN.3     
C                                                                          BALAN.4     
      COMMON/BALAN/BFILT(NL),RGT0(NL),RG(NL2),TMEAN(NL)                    BALAN.5     
     +            ,EP1(IGA),EP2(IGA),KBAL,MFTBAL,SRGT0,LTBAL               BALAN.6     
      LOGICAL LTBAL                                                        BALAN.7     
C                                                                          BALAN.8     
      DIMENSION GP(NL)                                                     BALANC.10    
      COMPLEX TA,GP,GSI1,VPS,TK,SRGT                                       BALANC.11    
C                                                                          BALANC.12    
C**********************************************************************    BALANC.13    
C     2-grid vertical temperature wave is not removed if orography         BALANC.14    
C     is included. Program aborts in INITAL if attempted.                  BALANC.15    
C**********************************************************************    BALANC.16    
C                                                                          BALANC.17    
      I1=0                                                                 BALANC.18    
      DO 800 IHEM=1,NHEM                                                   BALANC.19    
         DO 3 MP=1,MFP,MOCT                                                BALANC.20    
            DO 4 IN=MP,NFP,MH                                              BALANC.21    
               I1=I1+1                                                     BALANC.22    
               INR=IN+IHEM-1                                               BALANC.23    
               IF (INR.GT.1) THEN                                          BALANC.24    
                  VPS=VP(I1)                                               BALANC.25    
                  K=I1                                                     BALANC.26    
                  GSI1=GS(I1)                                              BALANC.27    
                  IL=0                                                     BALANC.28    
                  DO 10 L=1,NL                                             BALANC.29    
                     TA=(0.,0.)                                            BALANC.30    
                     KK=I1                                                 BALANC.31    
                     DO 9 M=1,NL                                           BALANC.32    
                        IL=IL+1                                            BALANC.33    
                        TA=TA+G(IL)*TT(KK)                                 BALANC.34    
                        KK=KK+IGA                                          BALANC.35    
    9                CONTINUE                                              BALANC.36    
                     TA=(T0(L)*VPS-TA)*DELT - RSQ(INR)*DT(K)               BALANC.37    
                     GP(L)=TA-GSI1                                         BALANC.38    
                     K=K+IGA                                               BALANC.39    
   10             CONTINUE                                                 BALANC.40    
                  IL=0                                                     BALANC.41    
                  K=I1                                                     BALANC.42    
                  SRGT=(0.,0.)                                             BALANC.43    
                  DO 12 L=1,NL                                             BALANC.44    
                     TK=(0.,0.)                                            BALANC.45    
                     DO 11 M=1,NL                                          BALANC.46    
                        IL=IL+1                                            BALANC.47    
                        TK=TK+RG(IL)*GP(M)                                 BALANC.48    
   11                CONTINUE                                              BALANC.49    
                     T(K)=TK                                               BALANC.50    
                     SRGT=SRGT+BFILT(L)*TK                                 BALANC.51    
                     K=K+IGA                                               BALANC.52    
   12             CONTINUE                                                 BALANC.53    
                  SRGT=SRGT/SRGT0                                          BALANC.54    
                  SP(I1)=SRGT                                              BALANC.55    
                  K=I1                                                     BALANC.56    
                  DO 13 L=1,NL                                             BALANC.57    
                     T(K)=T(K)-RGT0(L)*SRGT                                BALANC.58    
                     K=K+IGA                                               BALANC.59    
   13             CONTINUE                                                 BALANC.60    
               ENDIF                                                       BALANC.61    
    4       CONTINUE                                                       BALANC.62    
    3    CONTINUE                                                          BALANC.63    
         I1=NWJ2                                                           BALANC.64    
  800 CONTINUE                                                             BALANC.65    
C                                                                          BALANC.66    
      IF (KOUNT.EQ.0) THEN                                                 BALANC.67    
         DO 2 I=1,IGA                                                      BALANC.68    
            SPMI(I)=SP(I)                                                  BALANC.69    
    2    CONTINUE                                                          BALANC.70    
         DO 5 J=1,IGB                                                      BALANC.71    
            ZMI(J)=Z(J)                                                    BALANC.72    
            DMI(J)=D(J)                                                    BALANC.73    
            TMI(J)=T(J)                                                    BALANC.74    
    5    CONTINUE                                                          BALANC.75    
      ENDIF                                                                BALANC.76    
C                                                                          BALANC.77    
      RETURN                                                               BALANC.78    
      END                                                                  BALANC.79    
      SUBROUTINE DIFUSE                                                    DIFUSE.2     
C                                                                          DIFUSE.3     
C     Calculates spectral tendencies from restoration (if included)        DIFUSE.4     
C     and biharmonic diffusion.                                            DIFUSE.5     
C                                                                          DIFUSE.6     
C                                                                          PARAM1.2     
C     Determines model resolution                                          PARAM1.3     
C                                                                          PARAM1.4     
      PARAMETER(NN=21,MM=21,NHEM=2,NL=5,MOCT=1,MG=64,JG=16,NWJ2=121        G100.1     
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
      COMMON/SPECTR/Z(IGB),D(IGB),T(IGB),SP(IGA),GS(IGA)                   SPECTR.5     
     *              ,SPA(IGA),VP(IGA),DTE(IGB),TT(IGB),DT(IGB),ZT(IGB)     SPECTR.6     
     *              ,ZMI(IGB),DMI(IGB),TMI(IGB),SPMI(IGA)                  SPECTR.7     
      COMPLEX Z,D,T,SP,GS,SPA,VP,DTE,TT,DT,ZT,ZMI,DMI,TMI,SPMI             SPECTR.8     
C                                                                          SPECTR.9     
C                                                                          BATS.2     
C     Constant arrays and variables associated with time and vertical      BATS.3     
C     differencing. Also counters.                                         BATS.4     
C                                                                          BATS.5     
      COMMON/BATS/  BM1(IDE),AK(NNP),AQ(NL2),G(NL2),TAU(NL2)               BATS.6     
     +              ,KOUNT,KITS,KTOTAL,KRUN,BEGDAY,ITSPD,PNU21             BATS.7     
     +              ,DELT,DELT2,CV,CG,CT,PNU,PNU2                          BATS.8     
C                                                                          BATS.9     
C                                                                          RESTOR.2     
C     Restoration fields and timescale                                     RESTOR.3     
C                                                                          RESTOR.4     
      COMMON/RESTOR/ZRES(IGN),DRES(IGN),TRES(IGN),SPRES(IGM),DAMP          RESTOR.5     
     &,ZFCE(IGB),DFCE(IGB),TFCE(IGB),SPFCE(IGA)                            NICK.207   
     &,ZFED(IGB),DFED(IGB),TFED(IGB),SPFED(IGA)                            NICK.208   
     &,ZFAN(IGB),DFAN(IGB),TFAN(IGB),SPFAN(IGA)                            NICK.209   
     &,ZDMP(IGB),DDMP(IGB),TDMP(IGB),SPDMP(IGA)                            NICK.210   
     &,ZDMI(IGB),DDMI(IGB),TDMI(IGB),SPDMI(IGA)                            NICK.211   
     &,MASK(IGC,JG),GG(IGC,JG),CD(IGC,JG),AA1,AA2,AAT                      NICK.212   
      COMPLEX ZFCE,DFCE,TFCE,SPFCE,ZFED,DFED,TFED,SPFED                    NICK.213   
     &,ZFAN,DFAN,TFAN,SPFAN                                                NICK.214   
     &,ZDMP,DDMP,TDMP,SPDMP,ZDMI,DDMI,TDMI,SPDMI                           NICK.215   
C                                                                          RESTOR.6     
C                                                                          RESTIJ.2     
C     Restoration temperature field and constants which determine it,      RESTIJ.3     
C     also contains timescales                                             RESTIJ.4     
C                                                                          RESTIJ.5     
      COMMON/RESTIJ/TTRES(IGB)                                             RESTIJ.6     
     + ,DTNS,DTEP,DTTRP,FAC(NL),DDAMP(NL),TFRC(NL),YRLEN,TRS(NL)           RESTIJ.7     
     +  ,ALR,ZTROP,TGR                                                     RESTIJ.8     
      COMPLEX TTRES                                                        RESTIJ.9     
C                                                                          DIFUSE.14    
                                                                           NICK.300   
C*****FORCING                                                              NICK.301   
                                                                           NICK.302   
      IF (LFCE) THEN                                                       NICK.303   
      DO 40 I=1,IGB                                                        NICK.304   
      ZT(I)=ZT(I) + ZFCE(I)                                                NICK.305   
      DT(I)=DT(I) + DFCE(I)                                                NICK.306   
      TT(I)=TT(I) + TFCE(I)                                                NICK.307   
   40 CONTINUE                                                             NICK.308   
      DO 41 I=1,IGA                                                        NICK.309   
      VP(I)=VP(I) + SPFCE(I)                                               NICK.310   
   41 CONTINUE                                                             NICK.311   
      END IF                                                               NICK.312   
                                                                           NICK.313   
      IF (LFED) THEN                                                       NICK.314   
      DO 42 I=1,IGB                                                        NICK.315   
      ZT(I)=ZT(I) - ZFED(I)                                                NICK.316   
      DT(I)=DT(I) - DFED(I)                                                NICK.317   
      TT(I)=TT(I) - TFED(I)                                                NICK.318   
   42 CONTINUE                                                             NICK.319   
      DO 43 I=1,IGA                                                        NICK.320   
      VP(I)=VP(I) - SPFED(I)                                               NICK.321   
   43 CONTINUE                                                             NICK.322   
      END IF                                                               NICK.323   
                                                                           NICK.324   
      IF (LFAN) THEN                                                       NICK.325   
      DO 44 I=1,IGB                                                        NICK.326   
C     ZT(I)=ZT(I) + ZFAN(I)                                                NICK.327   
C     DT(I)=DT(I) + DFAN(I)                                                NICK.328   
      TT(I)=TT(I) + TFAN(I)                                                NICK.329   
   44 CONTINUE                                                             NICK.330   
      DO 45 I=1,IGA                                                        NICK.331   
C     VP(I)=VP(I) + SPFAN(I)                                               NICK.332   
   45 CONTINUE                                                             NICK.333   
      END IF                                                               NICK.334   
                                                                           NICK.335   
      IF (LMODE.AND.KOUNT.GE.5) THEN                                       NICK.336   
C*****SCALING DOWN OF ANOMALY IF TOO BIG OR                                NICK.337   
C*****UP IF TOO SMALL FOR LINEAR MODE FINDER                               NICK.338   
      RNORM=0.                                                             NICK.339   
      DO I=IGA+1,2*IGA                                                     NICK.340   
      RNORM=RNORM+(ABS(Z(I)-ZDMP(I)))**2.                                  NICK.341   
      ENDDO                                                                NICK.342   
      RNORM=SQRT(RNORM)                                                    NICK.343   
      IF (KOUNT.EQ.5) THEN                                                 NICK.344   
      MODEFIND=0                                                           NICK.345   
      RNORM0=1.E-5                                                         NICK.346   
      PRINT*,'RNORM0=',RNORM0                                              NICK.347   
      ENDIF                                                                NICK.348   
      RATIO=RNORM/RNORM0                                                   NICK.349   
      PRINT*,' RATIO= ',RATIO,' RNORM= ',RNORM                             NICK.350   
C                                                                          NICK.351   
C*****MAKE SURE MODEL GETS ABOVE RNORM0 THRESHOLD BEFORE                   NICK.352   
C*****STARTING TO USE MODEFINDER                                           NICK.353   
      IF ((RNORM.GT.RNORM0).AND.(MODEFIND.EQ.0)) MODEFIND=1                NICK.354   
C                                                                          NICK.355   
      IF (((RATIO.GT.10.).OR.(RATIO.LT.0.1)).AND.(MODEFIND.GT.0)) THEN     NICK.356   
      PRINT*,'RNORM= ',RNORM,' RATIO= ',RATIO,' ADJUSTING ANOM'            NICK.357   
      WRITE(2,*)'KOUNT= ',KOUNT,' RNORM= ',RNORM,' RATIO= ',RATIO,         NICK.358   
     &' ADJUSTING ANOM'                                                    NICK.359   
      DO I=1,IGB                                                           NICK.360   
      Z(I)=(Z(I)-ZDMP(I))/RATIO + ZDMP(I)                                  NICK.361   
      D(I)=(D(I)-DDMP(I))/RATIO + DDMP(I)                                  NICK.362   
      T(I)=(T(I)-TDMP(I))/RATIO + TDMP(I)                                  NICK.363   
      ZMI(I)=(ZMI(I)-ZDMI(I))/RATIO + ZDMI(I)                              NICK.364   
      DMI(I)=(DMI(I)-DDMI(I))/RATIO + DDMI(I)                              NICK.365   
      TMI(I)=(TMI(I)-TDMI(I))/RATIO + TDMI(I)                              NICK.366   
      ENDDO                                                                NICK.367   
      DO I=1, IGA                                                          NICK.368   
      SP(I)=(SP(I)-SPDMP(I))/RATIO + SPDMP(I)                              NICK.369   
      SPMI(I)=(SPMI(I)-SPDMI(I))/RATIO + SPDMI(I)                          NICK.370   
      ENDDO                                                                NICK.371   
      ENDIF                                                                NICK.372   
                                                                           NICK.373   
      END IF                                                               NICK.374   
                                                                           NICK.375   
      IF(DAMP.GT.0.0) THEN                                                 NICK.376   
                                                                           NICK.377   
C*****DAMPING                                                              NICK.378   
                                                                           NICK.379   
C*****stratosphere                                                         NICK.380   
      DO 51 I=1,IGA                                                        NICK.381   
C     ZT(I)=ZT(I) - DAMP*Z(I)*0.05                                         NICK.382   
C     DT(I)=DT(I) - DAMP*D(I)*0.05                                         NICK.383   
      TT(I)=TT(I) - DAMP*T(I)*0.1                                          NICK.384   
   51 CONTINUE                                                             NICK.385   
                                                                           NICK.386   
C*****troposphere                                                          NICK.387   
      DO 52 I=IGA+1,(NL-1)*IGA                                             NICK.388   
C     ZT(I)=ZT(I) - DAMP*Z(I)*0.05                                         NICK.389   
C     DT(I)=DT(I) - DAMP*D(I)*0.05                                         NICK.390   
      TT(I)=TT(I) - DAMP*T(I)*0.1                                          NICK.391   
   52 CONTINUE                                                             NICK.392   
                                                                           NICK.393   
C*****boundary layer                                                       NICK.394   
      IF (LGPDAMP.NE.1) THEN                                               NICK.395   
      DO 53 I=(NL-1)*IGA+1,IGB                                             NICK.396   
      ZT(I)=ZT(I) - DAMP*Z(I)                                              NICK.397   
      DT(I)=DT(I) - DAMP*D(I)                                              NICK.398   
   53 CONTINUE                                                             NICK.399   
      END IF                                                               NICK.400   
                                                                           NICK.401   
      DO 54 I=(NL-1)*IGA+1,IGB                                             NICK.402   
      TT(I)=TT(I) - DAMP*T(I)*AAT                                          NICK.403   
   54 CONTINUE                                                             NICK.404   
                                                                           NICK.405   
C*****optional extra damping of upper level zonal means                    NICK.406   
C*****stratosphere                                                         NICK.407   
C     K=1                                                                  NICK.408   
C     DO I=1,IDM                                                           NICK.409   
C     II=IGA*(K-1)+I                                                       NICK.410   
C     ZT(II)=ZT(II) - DAMP*Z(II)*(1.-0.)                                   NICK.411   
C     DT(II)=DT(II) - DAMP*D(II)*(1.-0.)                                   NICK.412   
C     TT(II)=TT(II) - DAMP*T(II)*(1.-0.1)                                  NICK.413   
C     II=IGA*(K-1)+I+NWJ2                                                  NICK.414   
C     ZT(II)=ZT(II) - DAMP*Z(II)*(1.-0.)                                   NICK.415   
C     DT(II)=DT(II) - DAMP*D(II)*(1.-0.)                                   NICK.416   
C     TT(II)=TT(II) - DAMP*T(II)*(1.-0.)                                   NICK.417   
C     ENDDO                                                                NICK.418   
                                                                           NICK.419   
C*****troposphere                                                          NICK.420   
C     DO K=2,(NL-1)                                                        NICK.421   
C     DO I=1,IDM                                                           NICK.422   
C     II=IGA*(K-1)+I                                                       NICK.423   
C     ZT(II)=ZT(II) - DAMP*Z(II)*(0.1-0.)                                  NICK.424   
C     DT(II)=DT(II) - DAMP*D(II)*(0.1-0.)                                  NICK.425   
C     TT(II)=TT(II) - DAMP*T(II)*(0.1-0.1)                                 NICK.426   
C     II=IGA*(K-1)+I+NWJ2                                                  NICK.427   
C     ZT(II)=ZT(II) - DAMP*Z(II)*(0.1-0.)                                  NICK.428   
C     DT(II)=DT(II) - DAMP*D(II)*(0.1-0.)                                  NICK.429   
C     TT(II)=TT(II) - DAMP*T(II)*(0.1-0.1)                                 NICK.430   
C     ENDDO                                                                NICK.431   
C     ENDDO                                                                NICK.432   
                                                                           NICK.433   
C*****remove damping from planetary vorticity !                            NICK.434   
C*****stratosphere                                                         NICK.435   
      K=1                                                                  NICK.436   
      I=(K-1)*IGA + 1                                                      NICK.437   
C     ZT(I)=ZT(I) + DAMP*EZ*0.05                                           NICK.438   
                                                                           NICK.439   
C*****troposphere                                                          NICK.440   
      DO 61 K=2,NL-1                                                       NICK.441   
      I=(K-1)*IGA + 1                                                      NICK.442   
C     ZT(I)=ZT(I) + DAMP*EZ*0.05                                           NICK.443   
   61 CONTINUE                                                             NICK.444   
                                                                           NICK.445   
C*****boundary layer                                                       NICK.446   
      I=(NL-1)*IGA + 1                                                     NICK.447   
      IF (LGPDAMP.NE.1) ZT(I)=ZT(I) + DAMP*EZ                              NICK.448   
                                                                           NICK.449   
      ENDIF                                                                NICK.450   
C                                                                          DIFUSE.56    
C     Add in biharmonic diffusion if required                              DIFUSE.57    
C                                                                          DIFUSE.58    
      IF (AK(2).GT.0.0) THEN                                               DIFUSE.59    
         DO 820 IHEM=1,NHEM                                                DIFUSE.60    
            DO 821 L=1,NL                                                  DIFUSE.61    
               J=NWJ2*(IHEM-1)+(L-1)*IGA                                   DIFUSE.62    
               DO 822 MP=1,MFP,MOCT                                        DIFUSE.63    
                  DO 823 IN=MP,NFP,MH                                      DIFUSE.64    
                     J=J+1                                                 DIFUSE.65    
                     AKZ =AK(IN+2-IHEM)                                    DIFUSE.66    
                     AKDT=AK(IN-1+IHEM)                                    DIFUSE.67    
                     ZT(J)=ZT(J)-AKZ *Z(J)                                 DIFUSE.68    
                     DT(J)=DT(J)-AKDT*D(J)                                 DIFUSE.69    
                     TT(J)=TT(J)-AKDT*T(J)                                 DIFUSE.70    
823               CONTINUE                                                 DIFUSE.71    
822            CONTINUE                                                    DIFUSE.72    
821         CONTINUE                                                       DIFUSE.73    
820      CONTINUE                                                          DIFUSE.74    
C                                                                          DIFUSE.75    
C        No diffusion on EZ (planetary vorticity)                          DIFUSE.76    
C                                                                          DIFUSE.77    
         I=1                                                               DIFUSE.78    
         DO 30 L=1,NL                                                      DIFUSE.79    
            ZT(I)=ZT(I)+AK(2)*EZ                                           DIFUSE.80    
            I=I+IGA                                                        DIFUSE.81    
30       CONTINUE                                                          DIFUSE.82    
      ENDIF                                                                DIFUSE.83    
C                                                                          DIFUSE.84    
      RETURN                                                               DIFUSE.85    
      END                                                                  DIFUSE.86    
      SUBROUTINE DSTEP                                                     DSTEP.2     
C                                                                          DSTEP.3     
C     Diabatic part of timestep. Completion of time-filter.                DSTEP.4     
C     Note that only Z,D,T have diabatic tendencies at present.            DSTEP.5     
C                                                                          DSTEP.6     
C                                                                          PARAM1.2     
C     Determines model resolution                                          PARAM1.3     
C                                                                          PARAM1.4     
      PARAMETER(NN=21,MM=21,NHEM=2,NL=5,MOCT=1,MG=64,JG=16,NWJ2=121        G100.1     
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
C                                                                          SPECTR.2     
C     Array ordering in SPECTR must correspond to that in GRIDP.           SPECTR.3     
C                                                                          SPECTR.4     
      COMMON/SPECTR/Z(IGB),D(IGB),T(IGB),SP(IGA),GS(IGA)                   SPECTR.5     
     *              ,SPA(IGA),VP(IGA),DTE(IGB),TT(IGB),DT(IGB),ZT(IGB)     SPECTR.6     
     *              ,ZMI(IGB),DMI(IGB),TMI(IGB),SPMI(IGA)                  SPECTR.7     
      COMPLEX Z,D,T,SP,GS,SPA,VP,DTE,TT,DT,ZT,ZMI,DMI,TMI,SPMI             SPECTR.8     
C                                                                          SPECTR.9     
C                                                                          BATS.2     
C     Constant arrays and variables associated with time and vertical      BATS.3     
C     differencing. Also counters.                                         BATS.4     
C                                                                          BATS.5     
      COMMON/BATS/  BM1(IDE),AK(NNP),AQ(NL2),G(NL2),TAU(NL2)               BATS.6     
     +              ,KOUNT,KITS,KTOTAL,KRUN,BEGDAY,ITSPD,PNU21             BATS.7     
     +              ,DELT,DELT2,CV,CG,CT,PNU,PNU2                          BATS.8     
C                                                                          BATS.9     
      IF(KOUNT.GT.KITS) THEN                                               DSTEP.11    
C                                                                          DSTEP.12    
C       Ordinary centred timestep                                          DSTEP.13    
C                                                                          DSTEP.14    
        DO 10 I=1,IGB                                                      DSTEP.15    
           Z(I)=Z(I)+DELT2*ZT(I)                                           DSTEP.16    
           T(I)=T(I)+DELT2*TT(I)                                           DSTEP.17    
           D(I)=D(I)+DELT2*DT(I)                                           DSTEP.18    
           ZMI(I)=ZMI(I)+PNU*Z(I)                                          DSTEP.19    
           TMI(I)=TMI(I)+PNU*T(I)                                          DSTEP.20    
           DMI(I)=DMI(I)+PNU*D(I)                                          DSTEP.21    
   10   CONTINUE                                                           DSTEP.22    
      DO 20 I=2,IGA                                                        NICK.139   
      SP(I)=SP(I) + DELT2*VP(I)                                            NICK.140   
           SPMI(I)=SPMI(I)+PNU*SP(I)                                       DSTEP.24    
20      CONTINUE                                                           DSTEP.25    
        RETURN                                                             DSTEP.26    
      ELSE                                                                 DSTEP.27    
C                                                                          DSTEP.28    
C       Short initial timestep                                             DSTEP.29    
C                                                                          DSTEP.30    
        DO 60 I=1,IGB                                                      DSTEP.31    
           Z(I)=Z(I)+DELT2*ZT(I)                                           DSTEP.32    
           T(I)=T(I)+DELT2*TT(I)                                           DSTEP.33    
           D(I)=D(I)+DELT2*DT(I)                                           DSTEP.34    
   60   CONTINUE                                                           DSTEP.35    
        DELT=DELT2                                                         DSTEP.36    
        DELT2=DELT2+DELT2                                                  DSTEP.37    
        RETURN                                                             DSTEP.38    
      ENDIF                                                                DSTEP.39    
C                                                                          DSTEP.40    
      END                                                                  DSTEP.41    
      SUBROUTINE ENERGY                                                    ENERGY.2     
C                                                                          ENERGY.3     
C     Calculates various global diagnostic quantities                      ENERGY.4     
C     every itstp timesteps.                                               ENERGY.5     
C                                                                          ENERGY.6     
C                                                                          PARAM1.2     
C     Determines model resolution                                          PARAM1.3     
C                                                                          PARAM1.4     
      PARAMETER(NN=21,MM=21,NHEM=2,NL=5,MOCT=1,MG=64,JG=16,NWJ2=121        G100.1     
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
      COMMON/SPECTR/Z(IGB),D(IGB),T(IGB),SP(IGA),GS(IGA)                   SPECTR.5     
     *              ,SPA(IGA),VP(IGA),DTE(IGB),TT(IGB),DT(IGB),ZT(IGB)     SPECTR.6     
     *              ,ZMI(IGB),DMI(IGB),TMI(IGB),SPMI(IGA)                  SPECTR.7     
      COMPLEX Z,D,T,SP,GS,SPA,VP,DTE,TT,DT,ZT,ZMI,DMI,TMI,SPMI             SPECTR.8     
C                                                                          SPECTR.9     
C                                                                          OUTCON.2     
C     Switches counters and constants controlling type and frequency of    OUTCON.3     
C     model output                                                         OUTCON.4     
C                                                                          OUTCON.5     
      COMMON/OUTCON/RNTAPE,NCOEFF,NLAT,INLAT,INSPC                         OUTCON.6     
     +              ,KOUNTP,KOUNTE,KOUNTH,KOUNTR                           OUTCON.7     
     +              ,KOUTP,KOUTE,KOUTH,KOUTR,DAY                           OUTCON.8     
     +              ,SQR2,RSQR2,EAM1,EAM2,TOUT1,TOUT2,RMG                  OUTCON.9     
     +              ,LSPO(NL),LGPO(NL)                                     OUTCON.10    
      LOGICAL LSPO,LGPO                                                    OUTCON.11    
C                                                                          OUTCON.12    
C                                                                          BATS.2     
C     Constant arrays and variables associated with time and vertical      BATS.3     
C     differencing. Also counters.                                         BATS.4     
C                                                                          BATS.5     
      COMMON/BATS/  BM1(IDE),AK(NNP),AQ(NL2),G(NL2),TAU(NL2)               BATS.6     
     +              ,KOUNT,KITS,KTOTAL,KRUN,BEGDAY,ITSPD,PNU21             BATS.7     
     +              ,DELT,DELT2,CV,CG,CT,PNU,PNU2                          BATS.8     
C                                                                          BATS.9     
      COMPLEX TIG,SPIP,CTIG,CRGS                                           ENERGY.13    
C                                                                          ENERGY.14    
C     First remove planetary vorticity so Z contains relative vorticity    ENERGY.15    
C                                                                          ENERGY.16    
      I=1                                                                  ENERGY.17    
      DO 101 L=1,NL                                                        ENERGY.18    
         Z(I)=Z(I)-EZ                                                      ENERGY.19    
         I=I+IGA                                                           ENERGY.20    
  101 CONTINUE                                                             ENERGY.21    
C                                                                          ENERGY.22    
C     Calculate means - PSITOT RMS vorticity                               ENERGY.23    
C                       CHITOT RMS divergence                              ENERGY.24    
C                       TMPTOT RMS temperature                             ENERGY.25    
C                       TOTP  IE+PE potential energy                       ENERGY.26    
C                       AMSP mean surface pressure                         ENERGY.27    
C                                                                          ENERGY.28    
      PSITOT=0.0                                                           ENERGY.29    
      CHITOT=0.0                                                           ENERGY.30    
      TMPTOT=0.0                                                           ENERGY.31    
      TOTP=0.0                                                             ENERGY.32    
      TOTI=0.0                                                             ENERGY.33    
      IL=1                                                                 ENERGY.34    
      ST2B=0.                                                              ENERGY.35    
      ST=0.                                                                ENERGY.36    
      IOFS=0                                                               ENERGY.37    
      DO 800 IHEM=1,NHEM                                                   ENERGY.38    
         IG=IOFS                                                           ENERGY.39    
         DO 30 L=1,NL                                                      ENERGY.40    
            TPSITT=0.                                                      ENERGY.41    
            TCHITT=0.                                                      ENERGY.42    
            TTMPTT=0.                                                      ENERGY.43    
            TTOTI=0.                                                       ENERGY.44    
            DSIG=DSIGMA(L)                                                 ENERGY.45    
            DSIGH=0.5*DSIG                                                 ENERGY.46    
            IP=IOFS                                                        ENERGY.47    
            DO 10 JP=1,NFP,MH                                              ENERGY.48    
               IG=IG+1                                                     ENERGY.49    
               IP=IP+1                                                     ENERGY.50    
               RTIG=REAL(T(IG))                                            ENERGY.51    
               RZIG=REAL(Z(IG))                                            ENERGY.52    
               RDIG=REAL(D(IG))                                            ENERGY.53    
               TPSITT=TPSITT+RZIG*RZIG                                     ENERGY.54    
               TCHITT=TCHITT+RDIG*RDIG                                     ENERGY.55    
               TTMPTT=TTMPTT+RTIG*RTIG                                     ENERGY.56    
               RSPIP=REAL(SPA(IP))                                         ENERGY.57    
               IF (L.EQ.1) THEN                                            ENERGY.58    
                  TOTP=TOTP+0.5*RSPIP*REAL(GS(IP))                         ENERGY.59    
               ENDIF                                                       ENERGY.60    
               TTOTI=TTOTI+RSPIP*RTIG                                      ENERGY.61    
   10       CONTINUE                                                       ENERGY.62    
            PSITOT=PSITOT+DSIGH*TPSITT                                     ENERGY.63    
            CHITOT=CHITOT+DSIGH*TCHITT                                     ENERGY.64    
            TMPTOT=TMPTOT+DSIGH*TTMPTT                                     ENERGY.65    
            TOTI=TOTI+DSIGH*TTOTI                                          ENERGY.66    
            TPSITT=0.                                                      ENERGY.67    
            TCHITT=0.                                                      ENERGY.68    
            TTMPTT=0.                                                      ENERGY.69    
            TTOTI=0.                                                       ENERGY.70    
            DO 25 M=MOCT,MF,MOCT                                           ENERGY.71    
               DO 20 JP=M,NF,MH                                            ENERGY.72    
                  IG=IG+1                                                  ENERGY.73    
                  IP=IP+1                                                  ENERGY.74    
                  TIG=T(IG)                                                ENERGY.75    
                  CTIG=CONJG(TIG)                                          ENERGY.76    
                  SPIP=SPA(IP)                                             ENERGY.77    
                  TPSITT=TPSITT+REAL(Z(IG)*CONJG(Z(IG)))                   ENERGY.78    
                  TCHITT=TCHITT+REAL(D(IG)*CONJG(D(IG)))                   ENERGY.79    
                  TTMPTT=TTMPTT+REAL(TIG*CTIG)                             ENERGY.80    
                  IF (L.EQ.1) THEN                                         ENERGY.81    
                     CRGS=CONJG(GS(IP))                                    ENERGY.82    
                     TOTP=TOTP+REAL(SPIP*CRGS)                             ENERGY.83    
                  ENDIF                                                    ENERGY.84    
                  TTOTI=TTOTI+REAL(SPIP*CTIG)                              ENERGY.85    
   20          CONTINUE                                                    ENERGY.86    
   25       CONTINUE                                                       ENERGY.87    
            PSITOT=PSITOT+DSIG*TPSITT                                      ENERGY.88    
            CHITOT=CHITOT+DSIG*TCHITT                                      ENERGY.89    
            TMPTOT=TMPTOT+DSIG*TTMPTT                                      ENERGY.90    
            TOTI=TOTI+DSIG*TTOTI                                           ENERGY.91    
            IF (IHEM.EQ.1) THEN                                            ENERGY.92    
               RTL=REAL(T(IL))                                             ENERGY.93    
               ST2B=ST2B+T0(L)*RTL*DSIG                                    ENERGY.94    
               ST=ST+RTL*DSIG                                              ENERGY.95    
               IL=IL+IGA                                                   ENERGY.96    
            ENDIF                                                          ENERGY.97    
            IG=IG+IGA-NWJ2                                                 ENERGY.98    
   30    CONTINUE                                                          ENERGY.99    
         IOFS=NWJ2                                                         ENERGY.100   
  800 CONTINUE                                                             ENERGY.101   
      AMSP=1.0+REAL(SPA(1))*RSQR2                                          ENERGY.102   
      PSITOT=SQRT(PSITOT)                                                  ENERGY.103   
      CHITOT=SQRT(CHITOT)                                                  ENERGY.104   
      TMPTOT=SQRT(TMPTOT+TOUT2+ST2B*SQR2)                                  ENERGY.105   
      TOTP=TOTP+RSQR2*REAL(GS(1))+(AMSP*TOUT1+TOTI+RSQR2*ST)/AKAP          ENERGY.106   
C                                                                          ENERGY.107   
      IF (KOUTP .LT. KOUNTE .OR. KOUTH .LT. KOUNTE .OR.                    ENERGY.108   
     +    KOUTR .LT. KOUNTE .OR. KOUNT .EQ. 0) WRITE(2,40)                 ENERGY.109   
      WRITE (2,50) KOUNT,PSITOT,CHITOT,TMPTOT,TOTP,AMSP                    ENERGY.110   
   40 FORMAT(/3X,'KOUNT',3X,'RMSVORT',8X,'RMSDIV',8X,'RMSTEMP'             ENERGY.111   
     +       ,8X,'PE+IE',10X,'MSP')                                        ENERGY.112   
   50 FORMAT(I6,1X,4E15.6,F13.10)                                          ENERGY.113   
C                                                                          ENERGY.114   
C     Restore Z to absolute vorticity                                      ENERGY.115   
C                                                                          ENERGY.116   
      I=1                                                                  ENERGY.117   
      DO 102 L=1,NL                                                        ENERGY.118   
         Z(I)=Z(I)+EZ                                                      ENERGY.119   
         I=I+IGA                                                           ENERGY.120   
  102 CONTINUE                                                             ENERGY.121   
C                                                                          ENERGY.122   
      RETURN                                                               ENERGY.123   
      END                                                                  ENERGY.124   
C     ******************************************************************   ENERGY.125   
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
      PARAMETER(NN=21,MM=21,NHEM=2,NL=5,MOCT=1,MG=64,JG=16,NWJ2=121        G100.1     
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
      PARAMETER(NN=21,MM=21,NHEM=2,NL=5,MOCT=1,MG=64,JG=16,NWJ2=121        G100.1     
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
      SUBROUTINE HANALV(SPA,VP,DTE,TT,DT,ZT                                HANALV.2     
     *                 ,SPG,VPG,EG,TNLG,FUG,FVG,VTG,FVGT,FUGT)             HANALV.3     
C                                                                          HANALV.4     
C     Perform all the direct Legendre transforms for the adiabatic         HANALV.5     
C     part of the timestep at the current latitude (pair), in place        HANALV.6     
C     of separate calls to HANAL for individual transform types.           HANALV.7     
C                                                                          HANALV.8     
C     The following types of Legendre function and thence types of         HANALV.9     
C     transform may be used:                                               HANALV.10    
C        ITYPE=1,2  :  ALP   :  ALPN(,,,1)   :  normal transform.          HANALV.11    
C        ITYPE=3,4  :  DALP  :  ALPN(,,,2)   :  y-derivative.              HANALV.12    
C        ITYPE=5,6  :  RLP   :  ALPN(,,,3)   :  del(-2).                   HANALV.13    
C        ITYPE=7,8  :  RDLP  :  ALPN(,,,4)   :  y-derivative of del(-2).   HANALV.14    
C     An even/odd value of ITYPE denotes a spectral field of even/odd      HANALV.15    
C     symmetry.                                                            HANALV.16    
C                                                                          HANALV.17    
C     Maximum vector efficiency is achieved by chaining all multi-level    HANALV.18    
C     transforms in one loop and by chaining all single-level transforms   HANALV.19    
C     in a second loop.                                                    HANALV.20    
C                                                                          HANALV.21    
C     All dummy argument arrays are declared complex.                      HANALV.22    
C     All array dimensions are parameters.                                 HANALV.23    
C     Multi-level arrays are 3-dimensional.                                HANALV.24    
C                                                                          HANALV.25    
C     NOTE: The y-derivative transforms use integration by parts and       HANALV.26    
C           are valid only if the input field has zero zonal mean at       HANALV.27    
C           both poles.                                                    HANALV.28    
C                                                                          HANALV.29    
C     NOTE: *** THE INPUT FOURIER FIELDS ARE MODIFIED IF GLOBAL ***        HANALV.30    
C                                                                          HANALV.31    
C     Version for RSGUP3.                     Mike Blackburn,  12.01.95.   HANALV.32    
C                                                                          HANALV.33    
C                                                                          PARAM1.2     
C     Determines model resolution                                          PARAM1.3     
C                                                                          PARAM1.4     
      PARAMETER(NN=21,MM=21,NHEM=2,NL=5,MOCT=1,MG=64,JG=16,NWJ2=121        G100.1     
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
C                                                                          HANALV.39    
      COMPLEX     SPA(NWJ2,NHEM),VP(NWJ2,NHEM),DTE(NWJ2,NHEM,NL)           HANALV.40    
     *           ,TT(NWJ2,NHEM,NL),DT(NWJ2,NHEM,NL),ZT(NWJ2,NHEM,NL)       HANALV.41    
      COMPLEX     SPG(IDL,NHEM),VPG(IDL,NHEM),EG(IDL,NHEM,NL)              HANALV.42    
     *           ,TNLG(IDL,NHEM,NL),FUG(IDL,NHEM,NL),FVG(IDL,NHEM,NL)      HANALV.43    
     *           ,VTG(IDL,NHEM,NL),FVGT(IDL,NHEM,NL),FUGT(IDL,NHEM,NL)     HANALV.44    
      COMPLEX     TEMP                                                     HANALV.45    
      REAL        ALPN(NWJ2,2,JGL,4)                                       HANALV.46    
      EQUIVALENCE (ALPN(1,1,1,1),ALP(1,1,1))                               HANALV.47    
C                                                                          HANALV.48    
C     For a global run, sum and difference the complete Fourier            HANALV.49    
C     transforms at the northern and southern latitude rows to give        HANALV.50    
C     the even and odd contributions : E=(N+S)/2, O=(N-S)/2.               HANALV.51    
C     For Fourier fields symmetric about equator  : even precedes odd.     HANALV.52    
C     For Fourier fields asymmetric about equator : odd precedes even.     HANALV.53    
C                                                                          HANALV.54    
      IF (NHEM.EQ.2) THEN                                                  HANALV.55    
C                                                                          HANALV.56    
         DO 10 IM=1,NWW                                                    HANALV.57    
C           Surface pressure : symmetric.                                  HANALV.58    
            TEMP=SPG(IM,1)                                                 HANALV.59    
            SPG(IM,1)=0.5*(TEMP+SPG(IM,2))                                 HANALV.60    
            SPG(IM,2)=0.5*(TEMP-SPG(IM,2))                                 HANALV.61    
C           Surface pressure tendency : symmetric.                         HANALV.62    
            TEMP=VPG(IM,1)                                                 HANALV.63    
            VPG(IM,1)=0.5*(TEMP+VPG(IM,2))                                 HANALV.64    
            VPG(IM,2)=0.5*(TEMP-VPG(IM,2))                                 HANALV.65    
   10    CONTINUE                                                          HANALV.66    
C                                                                          HANALV.67    
         DO 20 IM=1,NWW                                                    HANALV.68    
            DO 20 L=1,NL                                                   HANALV.69    
C              Divergence tendency : energy term : symmetric.              HANALV.70    
               TEMP=EG(IM,1,L)                                             HANALV.71    
               EG(IM,1,L)=0.5*(TEMP+EG(IM,2,L))                            HANALV.72    
               EG(IM,2,L)=0.5*(TEMP-EG(IM,2,L))                            HANALV.73    
C              Temperature tendency : main + d/dx part : symmetric.        HANALV.74    
               TEMP=TNLG(IM,1,L)                                           HANALV.75    
               TNLG(IM,1,L)=0.5*(TEMP+TNLG(IM,2,L))                        HANALV.76    
               TNLG(IM,2,L)=0.5*(TEMP-TNLG(IM,2,L))                        HANALV.77    
C              Divergence tendency : d/dx part : symmetric.                HANALV.78    
               TEMP=FUG(IM,1,L)                                            HANALV.79    
               FUG(IM,1,L)=0.5*(TEMP+FUG(IM,2,L))                          HANALV.80    
               FUG(IM,2,L)=0.5*(TEMP-FUG(IM,2,L))                          HANALV.81    
C              Vorticity tendency : d/dx part : anti-symmetric.            HANALV.82    
               TEMP=FVG(IM,1,L)                                            HANALV.83    
               FVG(IM,1,L)=0.5*(TEMP-FVG(IM,2,L))                          HANALV.84    
               FVG(IM,2,L)=0.5*(TEMP+FVG(IM,2,L))                          HANALV.85    
C              Temperature tendency : d/dy part : anti-symmetric.          HANALV.86    
               TEMP=VTG(IM,1,L)                                            HANALV.87    
               VTG(IM,1,L)=0.5*(TEMP-VTG(IM,2,L))                          HANALV.88    
               VTG(IM,2,L)=0.5*(TEMP+VTG(IM,2,L))                          HANALV.89    
C              Divergence tendency : d/dy part : anti-symmetric.           HANALV.90    
               TEMP=FVGT(IM,1,L)                                           HANALV.91    
               FVGT(IM,1,L)=0.5*(TEMP-FVGT(IM,2,L))                        HANALV.92    
               FVGT(IM,2,L)=0.5*(TEMP+FVGT(IM,2,L))                        HANALV.93    
C              Vorticity tendency : d/dy part : symmetric.                 HANALV.94    
               TEMP=FUGT(IM,1,L)                                           HANALV.95    
               FUGT(IM,1,L)=0.5*(TEMP+FUGT(IM,2,L))                        HANALV.96    
               FUGT(IM,2,L)=0.5*(TEMP-FUGT(IM,2,L))                        HANALV.97    
   20    CONTINUE                                                          HANALV.98    
C                                                                          HANALV.99    
      ENDIF                                                                HANALV.100   
C                                                                          HANALV.101   
C     Set up the appropriate Gaussian weight for the current latitude,     HANALV.102   
C     dependent on transform type.                                         HANALV.103   
C     Assumes JH in /LEGAU/ contains latitude counter from calling loop.   HANALV.104   
C                                                                          HANALV.105   
      AW1256=AW(JH)*CSSQ(JH)                                               HANALV.106   
      AW3478=-AW(JH)                                                       HANALV.107   
C                                                                          HANALV.108   
C     Calculate POLY array in a vector loop before the main transforms,    HANALV.109   
C     for the required Legendre Function types.                            HANALV.110   
C     Both even and odd functions are required, irrespective of NHEM.      HANALV.111   
C     Second subscript of ALPN denotes odd or even subset of Legendre      HANALV.112   
C     functions, and depends of symmetry of spectral field.                HANALV.113   
C     Fourth subscript of ALPN is Legendre function type, (ITYPE+1)/2.     HANALV.114   
C                                                                          HANALV.115   
      DO 30 IHEM=1,2                                                       HANALV.116   
         DO 30 IP=1,NWJ2                                                   HANALV.117   
            POLY(IP,IHEM,1)=AW1256*ALPN(IP,IHEM,JL,1)                      HANALV.118   
            POLY(IP,IHEM,2)=AW3478*ALPN(IP,IHEM,JL,2)                      HANALV.119   
   30 CONTINUE                                                             HANALV.120   
C                                                                          HANALV.121   
C     Transform single-level fields.                                       HANALV.122   
C     Vectorisation is over total wavenumber for each zonal wavenumber.    HANALV.123   
C                                                                          HANALV.124   
      IM=0                                                                 HANALV.125   
      IP=0                                                                 HANALV.126   
      DO 40 M=0,MM-1,MOCT                                                  HANALV.127   
         IM=IM+1                                                           HANALV.128   
         DO 40 N=M,NN-1,2                                                  HANALV.129   
            IP=IP+1                                                        HANALV.130   
C           Surface pressure          : type 2.                            HANALV.131   
            SPA(IP,1)=SPA(IP,1) + POLY(IP,1,1)*SPG(IM,1)                   HANALV.132   
C           Surface pressure tendency : type 2.                            HANALV.133   
            VP (IP,1)=VP (IP,1) + POLY(IP,1,1)*VPG(IM,1)                   HANALV.134   
            IF (NHEM.EQ.2) THEN                                            HANALV.135   
C              Surface pressure          : type 2.                         HANALV.136   
               SPA(IP,2)=SPA(IP,2) + POLY(IP,2,1)*SPG(IM,2)                HANALV.137   
C              Surface pressure tendency : type 2.                         HANALV.138   
               VP (IP,2)=VP (IP,2) + POLY(IP,2,1)*VPG(IM,2)                HANALV.139   
            ENDIF                                                          HANALV.140   
   40 CONTINUE                                                             HANALV.141   
C                                                                          HANALV.142   
C     Transform multi-level fields.                                        HANALV.143   
C     Inner loop vectorisation is over total wavenumber, to access         HANALV.144   
C     spectral memory sequentially, avoiding skip distances being a        HANALV.145   
C     multiple of 8 (which causes memory bank conflicts on Cray vector     HANALV.146   
C     machines).                                                           HANALV.147   
C                                                                          HANALV.148   
      IM=0                                                                 HANALV.149   
      IP=0                                                                 HANALV.150   
      DO 50 M=0,MM-1,MOCT                                                  HANALV.151   
         IM=IM+1                                                           HANALV.152   
         IPM=IP                                                            HANALV.153   
         DO 50 L=1,NL                                                      HANALV.154   
            IP=IPM                                                         HANALV.155   
            DO 50 N=M,NN-1,2                                               HANALV.156   
               IP=IP+1                                                     HANALV.157   
C              Divergence tendency  : energy term      : type 2.           HANALV.158   
               DTE(IP,1,L)=DTE(IP,1,L) + POLY(IP,1,1)*EG  (IM,1,L)         HANALV.159   
C              Temperature tendency : main + d/dx part : type 2.           HANALV.160   
C              Temperature tendency : d/dy part        : type 4.           HANALV.161   
               TT (IP,1,L)=TT (IP,1,L) + POLY(IP,1,1)*TNLG(IM,1,L)         HANALV.162   
     *                                 + POLY(IP,1,2)*VTG (IM,1,L)         HANALV.163   
C              Divergence tendency  : d/dx part        : type 2.           HANALV.164   
C              Divergence tendency  : d/dy part        : type 4.           HANALV.165   
               DT (IP,1,L)=DT (IP,1,L) + POLY(IP,1,1)*FUG (IM,1,L)         HANALV.166   
     *                                 + POLY(IP,1,2)*FVGT(IM,1,L)         HANALV.167   
C              Vorticity tendency   : d/dx part        : type 1.           HANALV.168   
C              Vorticity tendency   : d/dy part        : type 3.           HANALV.169   
               ZT (IP,1,L)=ZT (IP,1,L) + POLY(IP,2,1)*FVG (IM,1,L)         HANALV.170   
     *                                 + POLY(IP,2,2)*FUGT(IM,1,L)         HANALV.171   
               IF (NHEM.EQ.2) THEN                                         HANALV.172   
C                 Divergence tendency  : energy term      : type 2.        HANALV.173   
                  DTE(IP,2,L)=DTE(IP,2,L) + POLY(IP,2,1)*EG  (IM,2,L)      HANALV.174   
C                 Temperature tendency : main + d/dx part : type 2.        HANALV.175   
C                 Temperature tendency : d/dy part        : type 4.        HANALV.176   
                  TT (IP,2,L)=TT (IP,2,L) + POLY(IP,2,1)*TNLG(IM,2,L)      HANALV.177   
     *                                    + POLY(IP,2,2)*VTG (IM,2,L)      HANALV.178   
C                 Divergence tendency  : d/dx part        : type 2.        HANALV.179   
C                 Divergence tendency  : d/dy part        : type 4.        HANALV.180   
                  DT (IP,2,L)=DT (IP,2,L) + POLY(IP,2,1)*FUG (IM,2,L)      HANALV.181   
     *                                    + POLY(IP,2,2)*FVGT(IM,2,L)      HANALV.182   
C                 Vorticity tendency   : d/dx part        : type 1.        HANALV.183   
C                 Vorticity tendency   : d/dy part        : type 3.        HANALV.184   
                  ZT (IP,2,L)=ZT (IP,2,L) + POLY(IP,1,1)*FVG (IM,2,L)      HANALV.185   
     *                                    + POLY(IP,1,2)*FUGT(IM,2,L)      HANALV.186   
               ENDIF                                                       HANALV.187   
   50 CONTINUE                                                             HANALV.188   
C                                                                          HANALV.189   
      RETURN                                                               HANALV.190   
      END                                                                  HANALV.191   
C     ******************************************************************   HANALV.192   
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
      PARAMETER(NN=21,MM=21,NHEM=2,NL=5,MOCT=1,MG=64,JG=16,NWJ2=121        G100.1     
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
      PARAMETER(NN=21,MM=21,NHEM=2,NL=5,MOCT=1,MG=64,JG=16,NWJ2=121        G100.1     
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
      SUBROUTINE HEXPV(Z,D,T,SP,CHIG,SFG,UG,VG,ZG,DG,TG,PLG,PJG)           HEXPV.2     
C                                                                          HEXPV.3     
C     Perform all the indirect Legendre transforms for the adiabatic       HEXPV.4     
C     part of the timestep at the current latitude (pair), in place        HEXPV.5     
C     of separate calls to HEXP for individual transform types.            HEXPV.6     
C                                                                          HEXPV.7     
C     The following types of Legendre function and thence types of         HEXPV.8     
C     transform may be used:                                               HEXPV.9     
C        ITYPE=1,2  :  ALP   :  ALPN(,,,1)   :  normal transform.          HEXPV.10    
C        ITYPE=3,4  :  DALP  :  ALPN(,,,2)   :  y-derivative.              HEXPV.11    
C        ITYPE=5,6  :  RLP   :  ALPN(,,,3)   :  del(-2).                   HEXPV.12    
C        ITYPE=7,8  :  RDLP  :  ALPN(,,,4)   :  y-derivative of del(-2).   HEXPV.13    
C     An even/odd value of ITYPE denotes a spectral field of even/odd      HEXPV.14    
C     symmetry.                                                            HEXPV.15    
C                                                                          HEXPV.16    
C     Maximum vector efficiency is achieved by chaining all multi-level    HEXPV.17    
C     transforms in one loop and by chaining all single-level transforms   HEXPV.18    
C     in a second loop.                                                    HEXPV.19    
C                                                                          HEXPV.20    
C     All dummy argument arrays are declared complex.                      HEXPV.21    
C     All array dimensions are parameters.                                 HEXPV.22    
C     Multi-level arrays are 3-dimensional.                                HEXPV.23    
C                                                                          HEXPV.24    
C     Version for RSGUP3.                     Mike Blackburn,  12.01.95.   HEXPV.25    
C                                                                          HEXPV.26    
C                                                                          PARAM1.2     
C     Determines model resolution                                          PARAM1.3     
C                                                                          PARAM1.4     
      PARAMETER(NN=21,MM=21,NHEM=2,NL=5,MOCT=1,MG=64,JG=16,NWJ2=121        G100.1     
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
C                                                                          HEXPV.31    
      COMPLEX     Z(NWJ2,NHEM,NL),D(NWJ2,NHEM,NL),T(NWJ2,NHEM,NL)          HEXPV.32    
     *           ,SP(NWJ2,NHEM)                                            HEXPV.33    
      COMPLEX     CHIG(IDL,NHEM,NL),SFG(IDL,NHEM,NL)                       HEXPV.34    
     *           ,UG(IDL,NHEM,NL),VG(IDL,NHEM,NL)                          HEXPV.35    
     *           ,ZG(IDL,NHEM,NL),DG(IDL,NHEM,NL),TG(IDL,NHEM,NL)          HEXPV.36    
     *           ,PLG(IDL,NHEM),PJG(IDL,NHEM)                              HEXPV.37    
      COMPLEX     TEMP                                                     HEXPV.38    
      REAL        ALPN(NWJ2,2,JGL,4)                                       HEXPV.39    
      EQUIVALENCE (ALPN(1,1,1,1),ALP(1,1,1))                               HEXPV.40    
C                                                                          HEXPV.41    
C     Transform multi-level fields to create even and odd contributions    HEXPV.42    
C     to the Fourier coefficients at the northern hemisphere latitude.     HEXPV.43    
C     Second subscript of ALPN denotes odd or even subset of Legendre      HEXPV.44    
C     functions, and depends of symmetry of spectral field.                HEXPV.45    
C     Fourth subscript of ALPN is Legendre function type, (ITYPE+1)/2.     HEXPV.46    
C                                                                          HEXPV.47    
      IM=0                                                                 HEXPV.48    
      IP=0                                                                 HEXPV.49    
      DO 10 M=0,MM-1,MOCT                                                  HEXPV.50    
         IM=IM+1                                                           HEXPV.51    
         DO 10 N=M,NN-1,2                                                  HEXPV.52    
            IP=IP+1                                                        HEXPV.53    
            DO 10 L=1,NL                                                   HEXPV.54    
C              Velocity potential    : type 6.                             HEXPV.55    
               CHIG(IM,1,L)=CHIG(IM,1,L) + ALPN(IP,1,JL,3)*D(IP,1,L)       HEXPV.56    
C              Streamfunction        : type 5.                             HEXPV.57    
               SFG (IM,1,L)=SFG (IM,1,L) + ALPN(IP,2,JL,3)*Z(IP,1,L)       HEXPV.58    
C              Zonal (rot) wind      : type 7.                             HEXPV.59    
               UG  (IM,1,L)=UG  (IM,1,L) + ALPN(IP,2,JL,4)*Z(IP,1,L)       HEXPV.60    
C              Merid (div) wind      : type 8.                             HEXPV.61    
               VG  (IM,1,L)=VG  (IM,1,L) + ALPN(IP,1,JL,4)*D(IP,1,L)       HEXPV.62    
C              (Relative) vorticity  : type 1.                             HEXPV.63    
               ZG  (IM,1,L)=ZG  (IM,1,L) + ALPN(IP,2,JL,1)*Z(IP,1,L)       HEXPV.64    
C              Divergence            : type 2.                             HEXPV.65    
               DG  (IM,1,L)=DG  (IM,1,L) + ALPN(IP,1,JL,1)*D(IP,1,L)       HEXPV.66    
C              Temperature           : type 2.                             HEXPV.67    
               TG  (IM,1,L)=TG  (IM,1,L) + ALPN(IP,1,JL,1)*T(IP,1,L)       HEXPV.68    
               IF (NHEM.EQ.2) THEN                                         HEXPV.69    
C                 Velocity potential    : type 6.                          HEXPV.70    
                  CHIG(IM,2,L)=CHIG(IM,2,L) + ALPN(IP,2,JL,3)*D(IP,2,L)    HEXPV.71    
C                 Streamfunction        : type 5.                          HEXPV.72    
                  SFG (IM,2,L)=SFG (IM,2,L) + ALPN(IP,1,JL,3)*Z(IP,2,L)    HEXPV.73    
C                 Zonal (rot) wind      : type 7.                          HEXPV.74    
                  UG  (IM,2,L)=UG  (IM,2,L) + ALPN(IP,1,JL,4)*Z(IP,2,L)    HEXPV.75    
C                 Merid (div) wind      : type 8.                          HEXPV.76    
                  VG  (IM,2,L)=VG  (IM,2,L) + ALPN(IP,2,JL,4)*D(IP,2,L)    HEXPV.77    
C                 (Relative) vorticity  : type 1.                          HEXPV.78    
                  ZG  (IM,2,L)=ZG  (IM,2,L) + ALPN(IP,1,JL,1)*Z(IP,2,L)    HEXPV.79    
C                 Divergence            : type 2.                          HEXPV.80    
                  DG  (IM,2,L)=DG  (IM,2,L) + ALPN(IP,2,JL,1)*D(IP,2,L)    HEXPV.81    
C                 Temperature           : type 2.                          HEXPV.82    
                  TG  (IM,2,L)=TG  (IM,2,L) + ALPN(IP,2,JL,1)*T(IP,2,L)    HEXPV.83    
               ENDIF                                                       HEXPV.84    
   10 CONTINUE                                                             HEXPV.85    
C                                                                          HEXPV.86    
C     Transform single-level fields to create even and odd contributions   HEXPV.87    
C     to the Fourier coefficients at the northern hemisphere latitude.     HEXPV.88    
C     Vectorisation is over total wavenumber for each zonal wavenumber.    HEXPV.89    
C                                                                          HEXPV.90    
      IM=0                                                                 HEXPV.91    
      IP=0                                                                 HEXPV.92    
      DO 20 M=0,MM-1,MOCT                                                  HEXPV.93    
         IM=IM+1                                                           HEXPV.94    
         DO 20 N=M,NN-1,2                                                  HEXPV.95    
            IP=IP+1                                                        HEXPV.96    
C           Log (surface pressure)    : type 2.                            HEXPV.97    
            PLG(IM,1)=PLG(IM,1) + ALPN(IP,1,JL,1)*SP(IP,1)                 HEXPV.98    
C           Merid gradient of ln (ps) : type 4.                            HEXPV.99    
            PJG(IM,1)=PJG(IM,1) + ALPN(IP,1,JL,2)*SP(IP,1)                 HEXPV.100   
            IF (NHEM.EQ.2) THEN                                            HEXPV.101   
C              Log (surface pressure)    : type 2.                         HEXPV.102   
               PLG(IM,2)=PLG(IM,2) + ALPN(IP,2,JL,1)*SP(IP,2)              HEXPV.103   
C              Merid gradient of ln (ps) : type 4.                         HEXPV.104   
               PJG(IM,2)=PJG(IM,2) + ALPN(IP,2,JL,2)*SP(IP,2)              HEXPV.105   
            ENDIF                                                          HEXPV.106   
   20 CONTINUE                                                             HEXPV.107   
C                                                                          HEXPV.108   
C     For a global run, sum and difference even and odd contributions      HEXPV.109   
C     to give the complete Fourier transforms at the northern and          HEXPV.110   
C     southern latitude rows: N=E+O, S=E-O.                                HEXPV.111   
C     For symmetric Fourier fields, even (IM,1) precedes odd (IM,2).       HEXPV.112   
C     For asymmetric Fourier fields, odd (IM,1) precedes even (IM,2).      HEXPV.113   
C                                                                          HEXPV.114   
      IF (NHEM.EQ.2) THEN                                                  HEXPV.115   
C                                                                          HEXPV.116   
         DO 30 IM=1,NWW                                                    HEXPV.117   
            DO 30 L=1,NL                                                   HEXPV.118   
C              Velocity potential : symmetric.                             HEXPV.119   
               TEMP=CHIG(IM,1,L)                                           HEXPV.120   
               CHIG(IM,1,L)=TEMP+CHIG(IM,2,L)                              HEXPV.121   
               CHIG(IM,2,L)=TEMP-CHIG(IM,2,L)                              HEXPV.122   
C              Streamfunction : anti-symmetric.                            HEXPV.123   
               TEMP=SFG(IM,1,L)                                            HEXPV.124   
               SFG(IM,1,L)=SFG(IM,2,L)+TEMP                                HEXPV.125   
               SFG(IM,2,L)=SFG(IM,2,L)-TEMP                                HEXPV.126   
C              Zonal (rotational) wind : symmetric.                        HEXPV.127   
               TEMP=UG(IM,1,L)                                             HEXPV.128   
               UG(IM,1,L)=TEMP+UG(IM,2,L)                                  HEXPV.129   
               UG(IM,2,L)=TEMP-UG(IM,2,L)                                  HEXPV.130   
C              Meridional (divergent) wind : anti-symmetric.               HEXPV.131   
               TEMP=VG(IM,1,L)                                             HEXPV.132   
               VG(IM,1,L)=VG(IM,2,L)+TEMP                                  HEXPV.133   
               VG(IM,2,L)=VG(IM,2,L)-TEMP                                  HEXPV.134   
C              Vorticity : anti-symmetric.                                 HEXPV.135   
               TEMP=ZG(IM,1,L)                                             HEXPV.136   
               ZG(IM,1,L)=ZG(IM,2,L)+TEMP                                  HEXPV.137   
               ZG(IM,2,L)=ZG(IM,2,L)-TEMP                                  HEXPV.138   
C              Divergence : symmetric.                                     HEXPV.139   
               TEMP=DG(IM,1,L)                                             HEXPV.140   
               DG(IM,1,L)=TEMP+DG(IM,2,L)                                  HEXPV.141   
               DG(IM,2,L)=TEMP-DG(IM,2,L)                                  HEXPV.142   
C              Temperature : symmetric.                                    HEXPV.143   
               TEMP=TG(IM,1,L)                                             HEXPV.144   
               TG(IM,1,L)=TEMP+TG(IM,2,L)                                  HEXPV.145   
               TG(IM,2,L)=TEMP-TG(IM,2,L)                                  HEXPV.146   
   30    CONTINUE                                                          HEXPV.147   
C                                                                          HEXPV.148   
         DO 40 IM=1,NWW                                                    HEXPV.149   
C           Log (surface pressure) : symmetric.                            HEXPV.150   
            TEMP=PLG(IM,1)                                                 HEXPV.151   
            PLG(IM,1)=TEMP+PLG(IM,2)                                       HEXPV.152   
            PLG(IM,2)=TEMP-PLG(IM,2)                                       HEXPV.153   
C           Meridional gradient of ln(ps) : anti-symmetric.                HEXPV.154   
            TEMP=PJG(IM,1)                                                 HEXPV.155   
            PJG(IM,1)=PJG(IM,2)+TEMP                                       HEXPV.156   
            PJG(IM,2)=PJG(IM,2)-TEMP                                       HEXPV.157   
   40    CONTINUE                                                          HEXPV.158   
C                                                                          HEXPV.159   
      ENDIF                                                                HEXPV.160   
C                                                                          HEXPV.161   
      RETURN                                                               HEXPV.162   
      END                                                                  HEXPV.163   
C     ******************************************************************   HEXPV.164   
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
      SUBROUTINE LTD                                                       LTD.2     
C                                                                          LTD.3     
C     Direct Legendre transform for the adiabatic part of the timestep.    LTD.4     
C     Transforms from Fourier to spectral space at the current latitude    LTD.5     
C     (pair).  In a global run the input arrays are complete (even+odd)    LTD.6     
C     Fourier coefficients at the northern & southern hemisphere rows.     LTD.7     
C                                                                          LTD.8     
C     Includes the option either to call the modular routine HANAL for     LTD.9     
C     each field to be transformed, or to call the fast vectorising        LTD.10    
C     routine HANALV to perform all transforms together.  The choice       LTD.11    
C     is controlled by logical LTVEC.                                      LTD.12    
C                                                                          LTD.13    
C     Each call to HANAL transforms fields having the same symmetry        LTD.14    
C     and type of Legendre Function.  HANAL1 is a separate routine         LTD.15    
C     with improved efficiency for single-level transforms.                LTD.16    
C                                                                          LTD.17    
C     The Fourier work array passed to HANAL must be dimensioned with      LTD.18    
C     (at least) the maximum number of levels used in the HANAL calls.     LTD.19    
C                                                                          LTD.20    
C     Version for RSGUP3.                     Mike Blackburn,  12.01.95.   LTD.21    
C     ANSI work arrays for HANAL,SPDEL2.      Mike Blackburn,  04.09.96.   LTD.22    
C                                                                          LTD.23    
C                                                                          PARAM1.2     
C     Determines model resolution                                          PARAM1.3     
C                                                                          PARAM1.4     
      PARAMETER(NN=21,MM=21,NHEM=2,NL=5,MOCT=1,MG=64,JG=16,NWJ2=121        G100.1     
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
      COMMON/GRIDP/ CHIG(IGL,NL),SFG(IGL,NL),UG(IGL,NL),VG(IGL,NL)         GRIDP3.6     
     *              ,ZG(IGL,NL),DG(IGL,NL),TG(IGL,NL)                      GRIDP3.7     
     *              ,PLG(IGL),PJG(IGL),PMG(IGL)                            GRIDP3.8     
     *              ,SPG(IGL),VPG(IGL),EG(IGL,NL)                          GRIDP3.9     
     *              ,TNLG(IGL,NL),FUG(IGL,NL),FVG(IGL,NL),UTG(IGL,NL)      GRIDP3.10    
     *              ,VTG(IGL,NL),FVGT(IGL,NL),FUGT(IGL,NL)                 GRIDP3.11    
      COMPLEX CHIG,SFG,UG,VG,ZG,DG,TG,PLG,PJG,PMG                          GRIDP3.12    
     *       ,SPG,VPG,EG,TNLG,FUG,FVG,UTG,VTG,FVGT,FUGT                    GRIDP3.13    
C                                                                          GRIDP3.14    
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
C     Array ordering in SPECTR must correspond to that in GRIDP.           SPECTR.3     
C                                                                          SPECTR.4     
      COMMON/SPECTR/Z(IGB),D(IGB),T(IGB),SP(IGA),GS(IGA)                   SPECTR.5     
     *              ,SPA(IGA),VP(IGA),DTE(IGB),TT(IGB),DT(IGB),ZT(IGB)     SPECTR.6     
     *              ,ZMI(IGB),DMI(IGB),TMI(IGB),SPMI(IGA)                  SPECTR.7     
      COMPLEX Z,D,T,SP,GS,SPA,VP,DTE,TT,DT,ZT,ZMI,DMI,TMI,SPMI             SPECTR.8     
C                                                                          SPECTR.9     
      REAL    FILT(0:NN)                                                   LTD.31    
      COMPLEX GWORK(IGL,3*NL+2)                                            LTD.32    
C                                                                          LTD.33    
C     Prepare Fourier arrays:                                              LTD.34    
C     - change sign of terms which contribute negatively to tendency,      LTD.35    
C     - apply (1-mu**2) weighting,                                         LTD.36    
C     - take zonal derivatives,                                            LTD.37    
C     - make copies of effective momentum tendencies.                      LTD.38    
C                                                                          LTD.39    
      DO 10 L=1,NL                                                         LTD.40    
         DO 10 I=1,IGL                                                     LTD.41    
            EG(I,L)=-0.5*EG(I,L)/CSSQ(JH)                                  LTD.42    
            VTG(I,L)=-VTG(I,L)                                             LTD.43    
            TNLG(I,L)=TNLG(I,L)-CMPA(I)*UTG(I,L)/CSSQ(JH)                  LTD.44    
            FVGT(I,L)=FVG(I,L)                                             LTD.45    
            FUGT(I,L)=-FUG(I,L)                                            LTD.46    
            FUG(I,L)=CMPA(I)*FUG(I,L)/CSSQ(JH)                             LTD.47    
            FVG(I,L)=CMPA(I)*FVG(I,L)/CSSQ(JH)                             LTD.48    
   10 CONTINUE                                                             LTD.49    
C                                                                          LTD.50    
      IF (LTVEC) THEN                                                      LTD.51    
C                                                                          LTD.52    
C        Call single routine to perform all transforms with maximum        LTD.53    
C        vector efficiency.                                                LTD.54    
C        *** NOTE : THE INPUT FOURIER FIELDS ARE MODIFIED IF GLOBAL ***    LTD.55    
C                                                                          LTD.56    
         CALL HANALV(SPA,VP,DTE,TT,DT,ZT                                   LTD.57    
     *              ,SPG,VPG,EG,TNLG,FUG,FVG,VTG,FVGT,FUGT)                LTD.58    
C                                                                          LTD.59    
      ELSE                                                                 LTD.60    
C                                                                          LTD.61    
C        Main transform of even fields:                                    LTD.62    
C        SPG to FUG (and SPA to DT) must be contiguous in common.          LTD.63    
C                                                                          LTD.64    
         CALL HANAL(SPG,GWORK,SPA,3*NL+2,2)                                LTD.65    
C                                                                          LTD.66    
C        Remaining transforms: VTG,FVGT (and TT,DT) must be contiguous.    LTD.67    
C                                                                          LTD.68    
         CALL HANAL(FVG,GWORK,ZT,NL,1)                                     LTD.69    
         CALL HANAL(VTG,GWORK,TT,2*NL,4)                                   LTD.70    
         CALL HANAL(FUGT,GWORK,ZT,NL,3)                                    LTD.71    
C                                                                          LTD.72    
      ENDIF                                                                LTD.73    
C                                                                          LTD.74    
C     At the last latitude, take del**2 of the energy term and add to      LTD.75    
C     the divergence tendency.                                             LTD.76    
C                                                                          LTD.77    
      IF (JH.EQ.JG) THEN                                                   LTD.78    
         CALL SPDEL2(DTE,FILT,NWJ2,NN,MM,MOCT,NHEM,NL,0,2)                 LTD.79    
         DO 20 I=1,IGB                                                     LTD.80    
            DT(I)=DT(I)+DTE(I)                                             LTD.81    
   20    CONTINUE                                                          LTD.82    
      ENDIF                                                                LTD.83    
C                                                                          LTD.84    
      RETURN                                                               LTD.85    
      END                                                                  LTD.86    
      SUBROUTINE LTI                                                       LTI.2     
C                                                                          LTI.3     
C     Inverse Legendre transform for the adiabatic part of the timestep.   LTI.4     
C     Transforms from spectral to Fourier space at the current latitude    LTI.5     
C     (pair).  In a global run the resulting arrays are complete           LTI.6     
C     (i.e. even+odd) Fourier coefficients at the northern & southern      LTI.7     
C     hemisphere rows.                                                     LTI.8     
C                                                                          LTI.9     
C     Includes the option either to call the modular routine HEXP for      LTI.10    
C     each field to be transformed, or to call the fast vectorising        LTI.11    
C     routine HEXPV to perform all transforms together.  The choice        LTI.12    
C     is controlled by logical LTVEC.                                      LTI.13    
C                                                                          LTI.14    
C     Each call to HEXP transforms fields having the same symmetry         LTI.15    
C     and type of Legendre Function.  HEXP1 is a separate routine          LTI.16    
C     with improved efficiency for single-level transforms.                LTI.17    
C                                                                          LTI.18    
C     Version for RSGUP3.                     Mike Blackburn,  12.01.95.   LTI.19    
C                                                                          LTI.20    
C                                                                          PARAM1.2     
C     Determines model resolution                                          PARAM1.3     
C                                                                          PARAM1.4     
      PARAMETER(NN=21,MM=21,NHEM=2,NL=5,MOCT=1,MG=64,JG=16,NWJ2=121        G100.1     
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
      COMMON/GRIDP/ CHIG(IGL,NL),SFG(IGL,NL),UG(IGL,NL),VG(IGL,NL)         GRIDP3.6     
     *              ,ZG(IGL,NL),DG(IGL,NL),TG(IGL,NL)                      GRIDP3.7     
     *              ,PLG(IGL),PJG(IGL),PMG(IGL)                            GRIDP3.8     
     *              ,SPG(IGL),VPG(IGL),EG(IGL,NL)                          GRIDP3.9     
     *              ,TNLG(IGL,NL),FUG(IGL,NL),FVG(IGL,NL),UTG(IGL,NL)      GRIDP3.10    
     *              ,VTG(IGL,NL),FVGT(IGL,NL),FUGT(IGL,NL)                 GRIDP3.11    
      COMPLEX CHIG,SFG,UG,VG,ZG,DG,TG,PLG,PJG,PMG                          GRIDP3.12    
     *       ,SPG,VPG,EG,TNLG,FUG,FVG,UTG,VTG,FVGT,FUGT                    GRIDP3.13    
C                                                                          GRIDP3.14    
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
C     Array ordering in SPECTR must correspond to that in GRIDP.           SPECTR.3     
C                                                                          SPECTR.4     
      COMMON/SPECTR/Z(IGB),D(IGB),T(IGB),SP(IGA),GS(IGA)                   SPECTR.5     
     *              ,SPA(IGA),VP(IGA),DTE(IGB),TT(IGB),DT(IGB),ZT(IGB)     SPECTR.6     
     *              ,ZMI(IGB),DMI(IGB),TMI(IGB),SPMI(IGA)                  SPECTR.7     
      COMPLEX Z,D,T,SP,GS,SPA,VP,DTE,TT,DT,ZT,ZMI,DMI,TMI,SPMI             SPECTR.8     
C                                                                          SPECTR.9     
C                                                                          LTI.28    
      COMPLEX CEZN                                                         LTI.29    
C                                                                          LTI.30    
C     Preset Fourier arrays.                                               LTI.31    
C                                                                          LTI.32    
      DO 10 L=1,NL                                                         LTI.33    
         DO 10 I=1,IGL                                                     LTI.34    
            CHIG(I,L)=0.                                                   LTI.35    
            SFG(I,L)=0.                                                    LTI.36    
            UG(I,L)=0.                                                     LTI.37    
            VG(I,L)=0.                                                     LTI.38    
            ZG(I,L)=0.                                                     LTI.39    
            DG(I,L)=0.                                                     LTI.40    
            TG(I,L)=0.                                                     LTI.41    
   10 CONTINUE                                                             LTI.42    
C                                                                          LTI.43    
      DO 20 I=1,IGL                                                        LTI.44    
         PLG(I)=0.                                                         LTI.45    
         PJG(I)=0.                                                         LTI.46    
   20 CONTINUE                                                             LTI.47    
C                                                                          LTI.48    
C     Remove planetary vorticity in spectral space, so all transforms      LTI.49    
C     use relative vorticity.                                              LTI.50    
C                                                                          LTI.51    
      DO 30 I=1,IGB,IGA                                                    LTI.52    
         Z(I)=Z(I)-EZ                                                      LTI.53    
   30 CONTINUE                                                             LTI.54    
C                                                                          LTI.55    
      IF (LTVEC) THEN                                                      LTI.56    
C                                                                          LTI.57    
C        Call single routine to perform all transforms with maximum        LTI.58    
C        vector efficiency.                                                LTI.59    
C                                                                          LTI.60    
         CALL HEXPV(Z,D,T,SP,CHIG,SFG,UG,VG,ZG,DG,TG,PLG,PJG)              LTI.61    
C                                                                          LTI.62    
      ELSE                                                                 LTI.63    
C                                                                          LTI.64    
C        Transform prognostic fields & meridional derivative of ln(ps).    LTI.65    
C        D,T,SP and Fourier equivalents must be contiguous in common.      LTI.66    
C                                                                          LTI.67    
         CALL HEXP(Z , ZG,  NL  ,1)                                        LTI.68    
         CALL HEXP(D , DG,2*NL+1,2)                                        LTI.69    
         CALL HEXP1(SP,PJG,    1,4)                                        LTI.70    
C                                                                          LTI.71    
C        Wind components: calls to HEXP give following Fourier fields:     LTI.72    
C           SFG  :   streamfunction.                                       LTI.73    
C           CHIG :   velocity potential.                                   LTI.74    
C           UG   :   -U(rotational).                                       LTI.75    
C           VG   :   V(divergent).                                         LTI.76    
C                                                                          LTI.77    
         CALL HEXP(Z,SFG ,NL,5)                                            LTI.78    
         CALL HEXP(D,CHIG,NL,6)                                            LTI.79    
         CALL HEXP(Z,UG  ,NL,7)                                            LTI.80    
         CALL HEXP(D,VG  ,NL,8)                                            LTI.81    
C                                                                          LTI.82    
      ENDIF                                                                LTI.83    
C                                                                          LTI.84    
C     Restore planetary vorticity in spectral space.                       LTI.85    
C                                                                          LTI.86    
      DO 40 I=1,IGB,IGA                                                    LTI.87    
         Z(I)=Z(I)+EZ                                                      LTI.88    
   40 CONTINUE                                                             LTI.89    
C                                                                          LTI.90    
C     Convert from relative to absolute vorticity in Fourier space:        LTI.91    
C     (real part of) m=0 coefficient only.                                 LTI.92    
C                                                                          LTI.93    
      CEZN=CMPLX(2.0*SI(JH),0.0)                                           LTI.94    
      DO 50 L=1,NL                                                         LTI.95    
         ZG(1,L)=ZG(1,L)+CEZN                                              LTI.96    
         IF (NHEM.EQ.2) ZG(1+IDL,L)=ZG(1+IDL,L)-CEZN                       LTI.97    
   50 CONTINUE                                                             LTI.98    
C                                                                          LTI.99    
C     Sum to give total winds.  CMPA takes x-derivative.                   LTI.100   
C                                                                          LTI.101   
      DO 60 L=1,NL                                                         LTI.102   
         DO 60 I=1,IGL                                                     LTI.103   
            UG(I,L)=CMPA(I)*CHIG(I,L)-UG(I,L)                              LTI.104   
            VG(I,L)=CMPA(I)* SFG(I,L)+VG(I,L)                              LTI.105   
   60 CONTINUE                                                             LTI.106   
C                                                                          LTI.107   
C     Zonal gradient of ln(ps).                                            LTI.108   
C                                                                          LTI.109   
      DO 70 I=1,IGL                                                        LTI.110   
         PMG(I)=CMPA(I)*PLG(I)                                             LTI.111   
   70 CONTINUE                                                             LTI.112   
C                                                                          LTI.113   
      RETURN                                                               LTI.114   
      END                                                                  LTI.115   
      SUBROUTINE MATINV(A,N,LDA,IWORK,WORK)                                MATINV.2     
C                                                                          MATINV.3     
C     This subroutine calculates the inverse of NxN array A.               MATINV.4     
C                                                                          MATINV.5     
C     Arguements:                                                          MATINV.6     
C                                                                          MATINV.7     
C       A     - Array of dimension (LDA,N). Contains inverse               MATINV.8     
C               of A on exit.                                              MATINV.9     
C       N     - Number of rows and columns of A.                           MATINV.10    
C       LDA   - Leading dimension of A.                                    MATINV.11    
C       IWORK - Integer array contains the pivot indices of A.             MATINV.12    
C       WORK  - Real array used as workspace for SGETRI.                   MATINV.13    
C                                                                          MATINV.14    
C     This subroutine replaces the Cray specific subroutine                MATINV.15    
C     MINV with portable LAPACK subroutines SGETRF and SGETRI.             MATINV.16    
C                                                                          MATINV.17    
      INTEGER N,LDA,IWORK(N),INFO                                          MATINV.18    
      REAL A(LDA,N),WORK(N)                                                MATINV.19    
C                                                                          MATINV.20    
      CALL SGETRF(N,N,A,LDA,IWORK,INFO)                                    MATINV.21    
      IF (INFO.NE.0) THEN                                                  MATINV.22    
         WRITE(*,*) 'Error: SGETRF returned INFO = ',INFO                  MATINV.23    
         STOP                                                              MATINV.24    
      ENDIF                                                                MATINV.25    
      CALL SGETRI(N,A,LDA,IWORK,WORK,N,INFO)                               MATINV.26    
      IF (INFO.NE.0) THEN                                                  MATINV.27    
         WRITE(*,*) 'Error: SGETRI returned INFO = ',INFO                  MATINV.28    
         STOP                                                              MATINV.29    
      ENDIF                                                                MATINV.30    
C                                                                          MATINV.31    
      END                                                                  MATINV.32    
      SUBROUTINE MGRMLT                                                    MGRMLT.2     
C                                                                          MGRMLT.3     
C     Computes nonlinear tendencies in grid point space                    MGRMLT.4     
C     for the present latitude                                             MGRMLT.5     
C                                                                          MGRMLT.6     
C                                                                          PARAM1.2     
C     Determines model resolution                                          PARAM1.3     
C                                                                          PARAM1.4     
      PARAMETER(NN=21,MM=21,NHEM=2,NL=5,MOCT=1,MG=64,JG=16,NWJ2=121        G100.1     
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
C                                                                          GRIDP2.2     
C     Array ordering in GRIDP must correspond to that in SPECTR.           GRIDP2.3     
C     Real arrays: multi-level arrays are 2-dimensional.                   GRIDP2.4     
C                                                                          GRIDP2.5     
      COMMON/GRIDP/ CHIG(IGC,NL),SFG(IGC,NL),UG(IGC,NL),VG(IGC,NL)         GRIDP2.6     
     *              ,ZG(IGC,NL),DG(IGC,NL),TG(IGC,NL)                      GRIDP2.7     
     *              ,PLG(IGC),PJG(IGC),PMG(IGC)                            GRIDP2.8     
     *              ,SPG(IGC),VPG(IGC),EG(IGC,NL)                          GRIDP2.9     
     *              ,TNLG(IGC,NL),FUG(IGC,NL),FVG(IGC,NL),UTG(IGC,NL)      GRIDP2.10    
     *              ,VTG(IGC,NL),FVGT(IGC,NL),FUGT(IGC,NL)                 GRIDP2.11    
C                                                                          GRIDP2.12    
      DIMENSION SDOTP(MG,NLM),SUMD(MG),TPTA(MG),TPTB(MG)                   MGRMLT.12    
      DIMENSION CC(NL,NL)                                                  MGRMLT.13    
      EQUIVALENCE (CC(1,1),C(1))                                           MGRMLT.14    
C                                                                          MGRMLT.15    
      IOFM=0                                                               MGRMLT.16    
C                                                                          MGRMLT.17    
C     Loop over hemispheres                                                MGRMLT.18    
C                                                                          MGRMLT.19    
      DO 800 IHEM=1,NHEM                                                   MGRMLT.20    
         DO 100 I=1,MG                                                     MGRMLT.21    
            J=I+IOFM                                                       MGRMLT.22    
            SUMD(I)=0.0                                                    MGRMLT.23    
            VPG(J)=0.0                                                     MGRMLT.24    
C                                                                          MGRMLT.25    
C           Change from Ln(PSTAR) to PSTAR                                 MGRMLT.26    
C                                                                          MGRMLT.27    
            SPG(J)=EXP(PLG(J))-1.0                                         MGRMLT.28    
C                                                                          MGRMLT.29    
 100     CONTINUE                                                          MGRMLT.30    
         DO 110 L=1,NLM                                                    MGRMLT.31    
            DO 120 I=1,MG                                                  MGRMLT.32    
              J=I+IOFM                                                     MGRMLT.33    
              SUMD(I)=SUMD(I)+DSIGMA(L)*DG(J,L)                            MGRMLT.34    
              VPG(J)=VPG(J)+DSIGMA(L)*SECSQ(JH)*(UG(J,L)*PMG(J)+           MGRMLT.35    
     1               VG(J,L)*PJG(J))                                       MGRMLT.36    
              SDOTP(I,L)=SUMD(I)+VPG(J)                                    MGRMLT.37    
 120        CONTINUE                                                       MGRMLT.38    
 110     CONTINUE                                                          MGRMLT.39    
         DO 121 I=1,MG                                                     MGRMLT.40    
            J=I+IOFM                                                       MGRMLT.41    
            SUMD(I)=SUMD(I)+DSIGMA(NL)*DG(J,NL)                            MGRMLT.42    
            VPG(J)=VPG(J)+DSIGMA(NL)*SECSQ(JH)*(UG(J,NL)*PMG(J)+           MGRMLT.43    
     1             VG(J,NL)*PJG(J))                                        MGRMLT.44    
 121     CONTINUE                                                          MGRMLT.45    
         DO 130 L=1,NLM                                                    MGRMLT.46    
            DO 140 I=1,MG                                                  MGRMLT.47    
               J=I+IOFM                                                    MGRMLT.48    
               SDOTP(I,L)=SIGMAH(L)*(SUMD(I)+VPG(J))-SDOTP(I,L)            MGRMLT.49    
 140        CONTINUE                                                       MGRMLT.50    
 130     CONTINUE                                                          MGRMLT.51    
         DO 150 I=1,MG                                                     MGRMLT.52    
            SUMD(I)=0.0                                                    MGRMLT.53    
 150     CONTINUE                                                          MGRMLT.54    
         DO 160 L=1,NL                                                     MGRMLT.55    
            DO 170 I=1,MG                                                  MGRMLT.56    
               TPTA(I)=0.0                                                 MGRMLT.57    
               TPTB(I)=0.0                                                 MGRMLT.58    
 170        CONTINUE                                                       MGRMLT.59    
            DO 180 LL=1,L                                                  MGRMLT.60    
               DO 190 I=1,MG                                               MGRMLT.61    
                  J=I+IOFM                                                 MGRMLT.62    
                  VGPG=SECSQ(JH)*(UG(J,LL)*PMG(J)+VG(J,LL)*PJG(J))         MGRMLT.63    
                  TPTA(I)=TPTA(I)+CC(LL,L)*VGPG                            MGRMLT.64    
                  TPTB(I)=TPTB(I)+CC(LL,L)*(VGPG+DG(J,LL))                 MGRMLT.65    
 190           CONTINUE                                                    MGRMLT.66    
 180        CONTINUE                                                       MGRMLT.67    
            DO 200 I=1,MG                                                  MGRMLT.68    
               J=I+IOFM                                                    MGRMLT.69    
               UTG(J,L)=UG(J,L)*TG(J,L)                                    MGRMLT.70    
               VTG(J,L)=VG(J,L)*TG(J,L)                                    MGRMLT.71    
               EG(J,L)=UG(J,L)*UG(J,L)+VG(J,L)*VG(J,L)                     MGRMLT.72    
 200        CONTINUE                                                       MGRMLT.73    
            IF (L.GT.1.AND.L.LT.NL) THEN                                   MGRMLT.74    
               DO 210 I=1,MG                                               MGRMLT.75    
                  J=I+IOFM                                                 MGRMLT.76    
                  TSUM=SECSQ(JH)*(UG(J,L)*PMG(J)+VG(J,L)*PJG(J))           MGRMLT.77    
                  SUMD(I)=SUMD(I)+TSUM*DSIGMA(L)                           MGRMLT.78    
                  TNLG(J,L)=TG(J,L)*DG(J,L)+AKAP*TG(J,L)*(TSUM-TPTB(I))+   MGRMLT.79    
     1                      TKP(L)*(TSUM-TPTA(I))-                         MGRMLT.80    
     2                      RDSIG(L)*(SDOTP(I,L)*(TG(J,L+1)-TG(J,L))+      MGRMLT.81    
     3                      SDOTP(I,L-1)*(TG(J,L)-                         MGRMLT.82    
     4                      TG(J,L-1))+VPG(J)*(T01S2(L)*SIGMAH(L)+         MGRMLT.83    
     5                      T01S2(L-1)*SIGMAH(L-1))-                       MGRMLT.84    
     6                      SUMD(I)*(T01S2(L-1)+T01S2(L))+                 MGRMLT.85    
     7                      TSUM*DSIGMA(L)*T01S2(L-1))                     MGRMLT.86    
                  FVG(J,L)=-UG(J,L)*ZG(J,L)-PJG(J)*TG(J,L)-                MGRMLT.87    
     1                     RDSIG(L)*(SDOTP(I,L)*(VG(J,L+1)-VG(J,L))+       MGRMLT.88    
     2                     SDOTP(I,L-1)*(VG(J,L)-VG(J,L-1)))               MGRMLT.89    
                  FUG(J,L)=VG(J,L)*ZG(J,L)-PMG(J)*TG(J,L)-                 MGRMLT.90    
     1                     RDSIG(L)*(SDOTP(I,L)*(UG(J,L+1)-UG(J,L))+       MGRMLT.91    
     2                     SDOTP(I,L-1)*(UG(J,L)-UG(J,L-1)))               MGRMLT.92    
 210           CONTINUE                                                    MGRMLT.93    
            ELSE                                                           MGRMLT.94    
               FAC=1.0                                                     MGRMLT.95    
               K=L                                                         MGRMLT.96    
               IF (L.EQ.NL) THEN                                           MGRMLT.97    
                  K=L-1                                                    MGRMLT.98    
                  FAC=0.0                                                  MGRMLT.99    
               ENDIF                                                       MGRMLT.100   
               DO 220 I=1,MG                                               MGRMLT.101   
                  J=I+IOFM                                                 MGRMLT.102   
                  TSUM=SECSQ(JH)*(UG(J,L)*PMG(J)+VG(J,L)*PJG(J))           MGRMLT.103   
                  SUMD(I)=SUMD(I)+TSUM*DSIGMA(L)*FAC                       MGRMLT.104   
                  TNLG(J,L)=TG(J,L)*DG(J,L)+AKAP*TG(J,L)*(TSUM-TPTB(I))+   MGRMLT.105   
     1                      TKP(L)*(TSUM-TPTA(I))-                         MGRMLT.106   
     2                      RDSIG(L)*(SDOTP(I,K)*(TG(J,K+1)-TG(J,K))+      MGRMLT.107   
     3                      T01S2(K)*(SIGMAH(K)*VPG(J)-SUMD(I)))           MGRMLT.108   
                  FVG(J,L)=-UG(J,L)*ZG(J,L)-PJG(J)*TG(J,L)-                MGRMLT.109   
     1                     RDSIG(L)*(SDOTP(I,K)*(VG(J,K+1)-VG(J,K)))       MGRMLT.110   
                  FUG(J,L)=VG(J,L)*ZG(J,L)-PMG(J)*TG(J,L)-                 MGRMLT.111   
     1                     RDSIG(L)*(SDOTP(I,K)*(UG(J,K+1)-UG(J,K)))       MGRMLT.112   
 220           CONTINUE                                                    MGRMLT.113   
            ENDIF                                                          MGRMLT.114   
 160     CONTINUE                                                          MGRMLT.115   
         IOFM=MGPP                                                         MGRMLT.116   
 800  CONTINUE                                                             MGRMLT.117   
      RETURN                                                               MGRMLT.118   
      END                                                                  MGRMLT.119   
C     ******************************************************************   MGRMLT.120   
      SUBROUTINE NOISE                                                     NOISE.2     
C                                                                          NOISE.3     
C     Adds white noise perturbation to ln(surface pressure)                NOISE.4     
C     balanced initial state at T=0.                                       NOISE.5     
C                                                                          NOISE.6     
C                                                                          PARAM1.2     
C     Determines model resolution                                          PARAM1.3     
C                                                                          PARAM1.4     
      PARAMETER(NN=21,MM=21,NHEM=2,NL=5,MOCT=1,MG=64,JG=16,NWJ2=121        G100.1     
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
C                                                                          SPECTR.2     
C     Array ordering in SPECTR must correspond to that in GRIDP.           SPECTR.3     
C                                                                          SPECTR.4     
      COMMON/SPECTR/Z(IGB),D(IGB),T(IGB),SP(IGA),GS(IGA)                   SPECTR.5     
     *              ,SPA(IGA),VP(IGA),DTE(IGB),TT(IGB),DT(IGB),ZT(IGB)     SPECTR.6     
     *              ,ZMI(IGB),DMI(IGB),TMI(IGB),SPMI(IGA)                  SPECTR.7     
      COMPLEX Z,D,T,SP,GS,SPA,VP,DTE,TT,DT,ZT,ZMI,DMI,TMI,SPMI             SPECTR.8     
C                                                                          SPECTR.9     
C                                                                          OUTCON.2     
C     Switches counters and constants controlling type and frequency of    OUTCON.3     
C     model output                                                         OUTCON.4     
C                                                                          OUTCON.5     
      COMMON/OUTCON/RNTAPE,NCOEFF,NLAT,INLAT,INSPC                         OUTCON.6     
     +              ,KOUNTP,KOUNTE,KOUNTH,KOUNTR                           OUTCON.7     
     +              ,KOUTP,KOUTE,KOUTH,KOUTR,DAY                           OUTCON.8     
     +              ,SQR2,RSQR2,EAM1,EAM2,TOUT1,TOUT2,RMG                  OUTCON.9     
     +              ,LSPO(NL),LGPO(NL)                                     OUTCON.10    
      LOGICAL LSPO,LGPO                                                    OUTCON.11    
C                                                                          OUTCON.12    
  200 FORMAT(' WHITE NOISE SURFACE PRESSURE PERTURBATION AT T=0'/)         NOISE.11    
C                                                                          NOISE.12    
C     Eps sets magnitude of the noise                                      NOISE.13    
C                                                                          NOISE.14    
      EPS=1.E-4                                                            NOISE.15    
      WRITE (2,200)                                                        NOISE.16    
      SCALE=EPS/SQRT(2.0)                                                  NOISE.17    
      IBAS=IDM+1                                                           NOISE.18    
      IEND=NWJ2                                                            NOISE.19    
      IDUM=-1                                                              SC961212.12    
      DO 800 IHEM=1,NHEM                                                   NOISE.20    
         DO 10 I=IBAS,IEND                                                 NOISE.21    
            SP(I)=SP(I)+SCALE*CMPLX(RANF(IDUM)-0.5,RANF(IDUM)-0.5)         SC961212.13    
            SPMI(I)=SP(I)                                                  NOISE.23    
10       CONTINUE                                                          NOISE.24    
         IBAS=IBAS+NWJ2                                                    NOISE.25    
         IEND=IEND+NWJ2                                                    NOISE.26    
800   CONTINUE                                                             NOISE.27    
C                                                                          NOISE.28    
      RETURN                                                               NOISE.29    
      END                                                                  NOISE.30    
      SUBROUTINE SETRES                                                    SETRES.2     
C                                                                          SETRES.3     
C                                                                          PARAM1.2     
C     Determines model resolution                                          PARAM1.3     
C                                                                          PARAM1.4     
      PARAMETER(NN=21,MM=21,NHEM=2,NL=5,MOCT=1,MG=64,JG=16,NWJ2=121        G100.1     
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
C                                                                          SPECTR.2     
C     Array ordering in SPECTR must correspond to that in GRIDP.           SPECTR.3     
C                                                                          SPECTR.4     
      COMMON/SPECTR/Z(IGB),D(IGB),T(IGB),SP(IGA),GS(IGA)                   SPECTR.5     
     *              ,SPA(IGA),VP(IGA),DTE(IGB),TT(IGB),DT(IGB),ZT(IGB)     SPECTR.6     
     *              ,ZMI(IGB),DMI(IGB),TMI(IGB),SPMI(IGA)                  SPECTR.7     
      COMPLEX Z,D,T,SP,GS,SPA,VP,DTE,TT,DT,ZT,ZMI,DMI,TMI,SPMI             SPECTR.8     
C                                                                          SPECTR.9     
C                                                                          RESTOR.2     
C     Restoration fields and timescale                                     RESTOR.3     
C                                                                          RESTOR.4     
      COMMON/RESTOR/ZRES(IGN),DRES(IGN),TRES(IGN),SPRES(IGM),DAMP          RESTOR.5     
     &,ZFCE(IGB),DFCE(IGB),TFCE(IGB),SPFCE(IGA)                            NICK.207   
     &,ZFED(IGB),DFED(IGB),TFED(IGB),SPFED(IGA)                            NICK.208   
     &,ZFAN(IGB),DFAN(IGB),TFAN(IGB),SPFAN(IGA)                            NICK.209   
     &,ZDMP(IGB),DDMP(IGB),TDMP(IGB),SPDMP(IGA)                            NICK.210   
     &,ZDMI(IGB),DDMI(IGB),TDMI(IGB),SPDMI(IGA)                            NICK.211   
     &,MASK(IGC,JG),GG(IGC,JG),CD(IGC,JG),AA1,AA2,AAT                      NICK.212   
      COMPLEX ZFCE,DFCE,TFCE,SPFCE,ZFED,DFED,TFED,SPFED                    NICK.213   
     &,ZFAN,DFAN,TFAN,SPFAN                                                NICK.214   
     &,ZDMP,DDMP,TDMP,SPDMP,ZDMI,DDMI,TDMI,SPDMI                           NICK.215   
C                                                                          RESTOR.6     
C                                                                          SETRES.8     
C     Set up restoration state from the KOUNT=0 zonally averaged           SETRES.9     
C     state and write this to FT13 for future use.                         SETRES.10    
C     This is only done when KOUNT=0 and DAMP.GT.0.0.                      SETRES.11    
C                                                                          SETRES.12    
 2200 FORMAT(/' RESTORATION RECORD WRITTEN TO CHANNEL ',I3)                SETRES.13    
C                                                                          SETRES.14    
      IF (DAMP.LE.0) RETURN                                                SETRES.15    
C                                                                          SETRES.16    
      DO 840 IHEM=1,NHEM                                                   SETRES.17    
         I=NWJ2*(IHEM-1)                                                   SETRES.18    
         IR=IDM*(IHEM-1)                                                   SETRES.19    
         DO 100 J=1,IDM                                                    SETRES.20    
            I=I+1                                                          SETRES.21    
            IR=IR+1                                                        SETRES.22    
            SPRES(IR)=SP(I)                                                SETRES.23    
  100    CONTINUE                                                          SETRES.24    
         DO 850 L=1,NL                                                     SETRES.25    
            I=NWJ2*(IHEM-1)+(L-1)*IGA                                      SETRES.26    
            IR=IDM*(IHEM-1)+(L-1)*IGM                                      SETRES.27    
            DO 860 J=1,IDM                                                 SETRES.28    
               I=I+1                                                       SETRES.29    
               IR=IR+1                                                     SETRES.30    
               ZRES(IR)=Z(I)                                               SETRES.31    
               DRES(IR)=D(I)                                               SETRES.32    
               TRES(IR)=T(I)                                               SETRES.33    
  860       CONTINUE                                                       SETRES.34    
  850    CONTINUE                                                          SETRES.35    
  840 CONTINUE                                                             SETRES.36    
C                                                                          SETRES.37    
      REWIND 13                                                            SETRES.38    
      WRITE(13)ZRES,DRES,TRES,SPRES                                        SETRES.39    
      WRITE(2,2200)13                                                      SETRES.40    
C                                                                          SETRES.41    
      END                                                                  SETRES.42    
      SUBROUTINE SETTEE                                                    SETTEE.2     
C                                                                          SETTEE.3     
C     Subroutine to give annual cycle of TRES if wanted                    SETTEE.4     
C                                                                          SETTEE.5     
C                                                                          PARAM1.2     
C     Determines model resolution                                          PARAM1.3     
C                                                                          PARAM1.4     
      PARAMETER(NN=21,MM=21,NHEM=2,NL=5,MOCT=1,MG=64,JG=16,NWJ2=121        G100.1     
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
C                                                                          OUTCON.2     
C     Switches counters and constants controlling type and frequency of    OUTCON.3     
C     model output                                                         OUTCON.4     
C                                                                          OUTCON.5     
      COMMON/OUTCON/RNTAPE,NCOEFF,NLAT,INLAT,INSPC                         OUTCON.6     
     +              ,KOUNTP,KOUNTE,KOUNTH,KOUNTR                           OUTCON.7     
     +              ,KOUTP,KOUTE,KOUTH,KOUTR,DAY                           OUTCON.8     
     +              ,SQR2,RSQR2,EAM1,EAM2,TOUT1,TOUT2,RMG                  OUTCON.9     
     +              ,LSPO(NL),LGPO(NL)                                     OUTCON.10    
      LOGICAL LSPO,LGPO                                                    OUTCON.11    
C                                                                          OUTCON.12    
C                                                                          RESTIJ.2     
C     Restoration temperature field and constants which determine it,      RESTIJ.3     
C     also contains timescales                                             RESTIJ.4     
C                                                                          RESTIJ.5     
      COMMON/RESTIJ/TTRES(IGB)                                             RESTIJ.6     
     + ,DTNS,DTEP,DTTRP,FAC(NL),DDAMP(NL),TFRC(NL),YRLEN,TRS(NL)           RESTIJ.7     
     +  ,ALR,ZTROP,TGR                                                     RESTIJ.8     
      COMPLEX TTRES                                                        RESTIJ.9     
C                                                                          SETTEE.10    
C     If YRLEN is zero then no seasonal cycle.                             SETTEE.11    
C                                                                          SETTEE.12    
      IF (NINT(YRLEN) .EQ. 0) THEN                                         SETTEE.13    
         YPHS = (1./SQRT(6.))                                              SETTEE.14    
      ELSE                                                                 SETTEE.15    
         YPHS=(1./SQRT(6.))*SIN(PI2*DAY/YRLEN)                             SETTEE.16    
      ENDIF                                                                SETTEE.17    
      IADB=NWJ2+1                                                          SETTEE.18    
      DO 10 L=1,NL                                                         SETTEE.19    
        IAD=IADB+(L-1)*IGA                                                 SETTEE.20    
        TTRES(IAD)=FAC(L)*DTNS*YPHS                                        SETTEE.21    
10    CONTINUE                                                             SETTEE.22    
      RETURN                                                               SETTEE.23    
      END                                                                  SETTEE.24    
      SUBROUTINE SETZT                                                     SETZT.2     
C                                                                          SETZT.3     
C     This subroutine sets up restoration temperature field.               SETZT.4     
C     The temperature at SIGMA = 1 is TGR, entered in Kelvin.              SETZT.5     
C     a lapse rate of ALR k/m is assumed under the tropopause and          SETZT.6     
C     zero above. The actual profile tends to this away from the           SETZT.7     
C     tropopause, with smooth interpolation depending on DTTRP             SETZT.8     
C     at the model tropopause. The height of                               SETZT.9     
C     the tropopause is given as ZTROP m.                                  SETZT.10    
C                                                                          SETZT.11    
C                                                                          PARAM1.2     
C     Determines model resolution                                          PARAM1.3     
C                                                                          PARAM1.4     
      PARAMETER(NN=21,MM=21,NHEM=2,NL=5,MOCT=1,MG=64,JG=16,NWJ2=121        G100.1     
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
C                                                                          RESTIJ.2     
C     Restoration temperature field and constants which determine it,      RESTIJ.3     
C     also contains timescales                                             RESTIJ.4     
C                                                                          RESTIJ.5     
      COMMON/RESTIJ/TTRES(IGB)                                             RESTIJ.6     
     + ,DTNS,DTEP,DTTRP,FAC(NL),DDAMP(NL),TFRC(NL),YRLEN,TRS(NL)           RESTIJ.7     
     +  ,ALR,ZTROP,TGR                                                     RESTIJ.8     
      COMPLEX TTRES                                                        RESTIJ.9     
C                                                                          BATS.2     
C     Constant arrays and variables associated with time and vertical      BATS.3     
C     differencing. Also counters.                                         BATS.4     
C                                                                          BATS.5     
      COMMON/BATS/  BM1(IDE),AK(NNP),AQ(NL2),G(NL2),TAU(NL2)               BATS.6     
     +              ,KOUNT,KITS,KTOTAL,KRUN,BEGDAY,ITSPD,PNU21             BATS.7     
     +              ,DELT,DELT2,CV,CG,CT,PNU,PNU2                          BATS.8     
C                                                                          BATS.9     
      DO 100 I=1,IGB                                                       SETZT.17    
         TTRES(I)=(0.0,0.0)                                                SETZT.18    
100   CONTINUE                                                             SETZT.19    
      DTTRP=DTTRP*CT                                                       SETZT.20    
      SIGPREV=1.                                                           SETZT.21    
      TPREV=TGR                                                            SETZT.22    
      ZPREV=0.                                                             SETZT.23    
      DO 150 L=NL,1,-1                                                     SETZT.24    
         ZP=ZPREV+(GASCON*TPREV/GA)*LOG(SIGPREV/SIGMA(L))                  SETZT.25    
         TP=TGR-ZTROP*ALR                                                  SETZT.26    
         TP=TP+SQRT((.5*ALR*(ZP-ZTROP))**2+DTTRP**2)                       SETZT.27    
         TP=TP-.5*ALR*(ZP-ZTROP)                                           SETZT.28    
         TPM=.5*(TPREV+TP)                                                 SETZT.29    
         ZPP=ZPREV+(GASCON*TPM/GA)*LOG(SIGPREV/SIGMA(L))                   SETZT.30    
         TPP=TGR-ZTROP*ALR                                                 SETZT.31    
         TPP=TPP+SQRT((.5*ALR*(ZPP-ZTROP))**2+DTTRP**2)                    SETZT.32    
         TPP=TPP-.5*ALR*(ZPP-ZTROP)                                        SETZT.33    
         TRS(L)=TPP                                                        SETZT.34    
         ZPREV=ZPREV+(.5*(TPP+TPREV)*GASCON/GA)*LOG(SIGPREV/SIGMA(L))      SETZT.35    
         TPREV=TPP                                                         SETZT.36    
         SIGPREV=SIGMA(L)                                                  SETZT.37    
150   CONTINUE                                                             SETZT.38    
C                                                                          SETZT.39    
      WRITE(2,2000)                                                        SETZT.40    
      WRITE(2,2010) TRS                                                    SETZT.41    
 2000 FORMAT(/' RESTORATION TEMPERATURE STRATIFICATION IN K ')             SETZT.42    
 2010 FORMAT(10F7.2)                                                       SETZT.43    
C                                                                          SETZT.44    
      DO 170 L=1,NL                                                        SETZT.45    
         TRS(L)=TRS(L)/CT                                                  SETZT.46    
 170  CONTINUE                                                             SETZT.47    
C                                                                          SETZT.48    
C     Now the latitudinal variation in TTRES is set up                     SETZT.49    
C     (this being in terms of a deviation from T0 which                    SETZT.50    
C     is usually constant with height)                                     SETZT.51    
C                                                                          SETZT.52    
      DO 200 L=1,NL                                                        SETZT.53    
         I=(L-1)*IGA                                                       SETZT.54    
         TTRES(I+1)=SQRT(2.)*(TRS(L)-T0(L))                                SETZT.55    
         TTRES(I+2)=-2./3.*SQRT(0.4)*DTEP*FAC(L)                           SETZT.56    
         TTRES(I+NWJ2+1)=(1./SQRT(6.))*DTNS*FAC(L)                         SETZT.57    
200   CONTINUE                                                             SETZT.58    
      DTTRP=DTTRP/CT                                                       SETZT.59    
C                                                                          SETZT.60    
      END                                                                  SETZT.61    
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
      SUBROUTINE SPOP                                                      SPOP.2     
C                                                                          SPOP.3     
C     Controls diagnostic output from model run.                           SPOP.4     
C     Outputs spectral coefficients.                                       SPOP.5     
C                                                                          SPOP.6     
C                                                                          PARAM1.2     
C     Determines model resolution                                          PARAM1.3     
C                                                                          PARAM1.4     
      PARAMETER(NN=21,MM=21,NHEM=2,NL=5,MOCT=1,MG=64,JG=16,NWJ2=121        G100.1     
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
      COMMON/SPECTR/Z(IGB),D(IGB),T(IGB),SP(IGA),GS(IGA)                   SPECTR.5     
     *              ,SPA(IGA),VP(IGA),DTE(IGB),TT(IGB),DT(IGB),ZT(IGB)     SPECTR.6     
     *              ,ZMI(IGB),DMI(IGB),TMI(IGB),SPMI(IGA)                  SPECTR.7     
      COMPLEX Z,D,T,SP,GS,SPA,VP,DTE,TT,DT,ZT,ZMI,DMI,TMI,SPMI             SPECTR.8     
C                                                                          SPECTR.9     
C                                                                          GRIDP.2     
C     Array ordering in GRIDP must correspond to that in SPECTR.           GRIDP.3     
C     Real arrays: multi-level arrays are 1-dimensional.                   GRIDP.4     
C                                                                          GRIDP.5     
      COMMON/GRIDP/ CHIG(IGD),SFG(IGD),UG(IGD),VG(IGD)                     GRIDP.6     
     *              ,ZG(IGD),DG(IGD),TG(IGD)                               GRIDP.7     
     *              ,PLG(IGC),PJG(IGC),PMG(IGC)                            GRIDP.8     
     *              ,SPG(IGC),VPG(IGC),EG(IGD)                             GRIDP.9     
     *              ,TNLG(IGD),FUG(IGD),FVG(IGD),UTG(IGD)                  GRIDP.10    
     *              ,VTG(IGD),FVGT(IGD),FUGT(IGD)                          GRIDP.11    
C                                                                          GRIDP.12    
C                                                                          BATS.2     
C     Constant arrays and variables associated with time and vertical      BATS.3     
C     differencing. Also counters.                                         BATS.4     
C                                                                          BATS.5     
      COMMON/BATS/  BM1(IDE),AK(NNP),AQ(NL2),G(NL2),TAU(NL2)               BATS.6     
     +              ,KOUNT,KITS,KTOTAL,KRUN,BEGDAY,ITSPD,PNU21             BATS.7     
     +              ,DELT,DELT2,CV,CG,CT,PNU,PNU2                          BATS.8     
C                                                                          BATS.9     
C                                                                          LEGAU.2     
C     Legendre polynomials and information about gaussian latitudes        LEGAU.3     
C                                                                          LEGAU.4     
      COMMON/LEGAU/ ALPJ(MJP),DALPJ(MJP)                                   LEGAU.5     
     +              ,ALP(NWJ2,2,JGL),DALP(NWJ2,2,JGL)                      LEGAU.6     
     +              ,RLP(NWJ2,2,JGL),RDLP(NWJ2,2,JGL)                      LEGAU.7     
     +              ,SI(JGG),CS(JGG),SISQ(JGG),CSSQ(JGG),SECSQ(JGG)        LEGAU.8     
     +              ,ALAT(JGG),GWT(JGG),AW(JGG),JH,JL,JINC                 LEGAU.9     
C                                                                          LEGAU.10    
C                                                                          OUTCON.2     
C     Switches counters and constants controlling type and frequency of    OUTCON.3     
C     model output                                                         OUTCON.4     
C                                                                          OUTCON.5     
      COMMON/OUTCON/RNTAPE,NCOEFF,NLAT,INLAT,INSPC                         OUTCON.6     
     +              ,KOUNTP,KOUNTE,KOUNTH,KOUNTR                           OUTCON.7     
     +              ,KOUTP,KOUTE,KOUTH,KOUTR,DAY                           OUTCON.8     
     +              ,SQR2,RSQR2,EAM1,EAM2,TOUT1,TOUT2,RMG                  OUTCON.9     
     +              ,LSPO(NL),LGPO(NL)                                     OUTCON.10    
      LOGICAL LSPO,LGPO                                                    OUTCON.11    
C                                                                          OUTCON.12    
C                                                                          SPOP.15    
 200  FORMAT(/' NUMBER OF TIME STEPS COMPLETED =',I5)                      SPOP.16    
 202  FORMAT(' SPECTRAL COEFFICIENTS (COEFF ; AMPLITUDE ; PHASE)')         SPOP.17    
 204  FORMAT(' VORTICITY AT LEVEL',I2)                                     SPOP.18    
 206  FORMAT(' DIVERGENCE AT LEVEL',I2)                                    SPOP.19    
 208  FORMAT(' PERTURBATION TEMPERATURE AT LEVEL',I2)                      SPOP.20    
 211  FORMAT(' LOG(SURFACE PRESSURE)')                                     SPOP.21    
C                                                                          SPOP.22    
      IF (NCOEFF.EQ.0) RETURN                                              SPOP.23    
C                                                                          SPOP.24    
C     Spectral coeficients are wanted                                      SPOP.25    
C                                                                          SPOP.26    
      WRITE (2,200) KOUNT                                                  SPOP.27    
      WRITE (2,202)                                                        SPOP.28    
C                                                                          SPOP.29    
C     Absolute vorticity                                                   SPOP.30    
C                                                                          SPOP.31    
      DO 18 L=1,NL                                                         SPOP.32    
         IF (LSPO(L)) THEN                                                 SPOP.33    
            WRITE (2,204) L                                                SPOP.34    
            CALL WRSPA(Z(1+(L-1)*IGA),1)                                   SPOP.35    
         ENDIF                                                             SPOP.36    
 18   CONTINUE                                                             SPOP.37    
C                                                                          SPOP.38    
C     Divergence                                                           SPOP.39    
C                                                                          SPOP.40    
      DO 28 L=1,NL                                                         SPOP.41    
         IF (LSPO(L)) THEN                                                 SPOP.42    
            WRITE (2,206) L                                                SPOP.43    
            CALL WRSPA(D(1+(L-1)*IGA),2)                                   SPOP.44    
         ENDIF                                                             SPOP.45    
 28   CONTINUE                                                             SPOP.46    
C                                                                          SPOP.47    
C     Temperature                                                          SPOP.48    
C                                                                          SPOP.49    
      DO 38 L=1,NL                                                         SPOP.50    
         IF (LSPO(L)) THEN                                                 SPOP.51    
            WRITE (2,208) L                                                SPOP.52    
            CALL WRSPA(T(1+(L-1)*IGA),2)                                   SPOP.53    
         ENDIF                                                             SPOP.54    
 38   CONTINUE                                                             SPOP.55    
C                                                                          SPOP.56    
C     Log (Surface Pressure)                                               SPOP.57    
C                                                                          SPOP.58    
      WRITE(2,211)                                                         SPOP.59    
      CALL WRSPA(SP(1),2)                                                  SPOP.60    
C                                                                          SPOP.61    
      RETURN                                                               SPOP.62    
      END                                                                  SPOP.63    
      SUBROUTINE TBAL                                                      TBAL.2     
C                                                                          TBAL.3     
C                                                                          PARAM1.2     
C     Determines model resolution                                          PARAM1.3     
C                                                                          PARAM1.4     
      PARAMETER(NN=21,MM=21,NHEM=2,NL=5,MOCT=1,MG=64,JG=16,NWJ2=121        G100.1     
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
      COMMON/SPECTR/Z(IGB),D(IGB),T(IGB),SP(IGA),GS(IGA)                   SPECTR.5     
     *              ,SPA(IGA),VP(IGA),DTE(IGB),TT(IGB),DT(IGB),ZT(IGB)     SPECTR.6     
     *              ,ZMI(IGB),DMI(IGB),TMI(IGB),SPMI(IGA)                  SPECTR.7     
      COMPLEX Z,D,T,SP,GS,SPA,VP,DTE,TT,DT,ZT,ZMI,DMI,TMI,SPMI             SPECTR.8     
C                                                                          SPECTR.9     
C                                                                          BATS.2     
C     Constant arrays and variables associated with time and vertical      BATS.3     
C     differencing. Also counters.                                         BATS.4     
C                                                                          BATS.5     
      COMMON/BATS/  BM1(IDE),AK(NNP),AQ(NL2),G(NL2),TAU(NL2)               BATS.6     
     +              ,KOUNT,KITS,KTOTAL,KRUN,BEGDAY,ITSPD,PNU21             BATS.7     
     +              ,DELT,DELT2,CV,CG,CT,PNU,PNU2                          BATS.8     
C                                                                          BATS.9     
C                                                                          BALAN.2     
C     Constants and arrays needed for balancing                            BALAN.3     
C                                                                          BALAN.4     
      COMMON/BALAN/BFILT(NL),RGT0(NL),RG(NL2),TMEAN(NL)                    BALAN.5     
     +            ,EP1(IGA),EP2(IGA),KBAL,MFTBAL,SRGT0,LTBAL               BALAN.6     
      LOGICAL LTBAL                                                        BALAN.7     
C                                                                          BALAN.8     
      DIMENSION PMNRE(IDJ)                                                 TBAL.10    
      COMPLEX ERR(IDK),VPS,GSI1,TA,SRGT                                    TBAL.11    
C                                                                          TBAL.12    
C***********************************************************************   TBAL.13    
C     Note that this scheme does not converge if wavenumbers M are         TBAL.14    
C     present for which there are only a small number of modes in the      TBAL.15    
C     truncation. For a small number of iterations the problem is not      TBAL.16    
C     serious but it may be removed altogether by limiting the range of    TBAL.17    
C     the 20 and 80 loops. For a given M, 7 modes are sufficient and 3     TBAL.18    
C     are insufficient. The loop terminator MFTBAL is set in INITAL.       TBAL.19    
C***********************************************************************   TBAL.20    
C                                                                          TBAL.21    
      REWIND 9                                                             TBAL.22    
      I1=0                                                                 TBAL.23    
      DO 20 MP=1,MFTBAL,MOCT                                               TBAL.24    
         J2=(NFP-MP)/2+1                                                   TBAL.25    
         J22=J2*J2                                                         TBAL.26    
         IJ=0                                                              TBAL.27    
         DO 3 IN=MP,NFP,MH                                                 TBAL.28    
            I1=I1+1                                                        TBAL.29    
            IF (IN.GT.1) THEN                                              TBAL.30    
               VPS=VP(I1)                                                  TBAL.31    
               K=I1                                                        TBAL.32    
               GSI1=GS(I1)                                                 TBAL.33    
               IL=0                                                        TBAL.34    
               DO 10 L=1,NL                                                TBAL.35    
                  TA=(0.,0.)                                               TBAL.36    
                  KK=I1                                                    TBAL.37    
                  SRGT=0.                                                  TBAL.38    
                  DO 9 M=1,NL                                              TBAL.39    
                     IL=IL+1                                               TBAL.40    
                     TA=TA+G(IL)*TT(KK)                                    TBAL.41    
                     SRGT=SRGT+G(IL)*T(KK)                                 TBAL.42    
                     KK=KK+IGA                                             TBAL.43    
    9             CONTINUE                                                 TBAL.44    
                  IJ=IJ+1                                                  TBAL.45    
                  ERR(IJ)=-DT(K)-SQ(IN)*                                   TBAL.46    
     +                    (SRGT+GSI1+T0(L)*SP(I1)+DELT*(TA-T0(L)*VPS))     TBAL.47    
                  K=K+IGA                                                  TBAL.48    
   10          CONTINUE                                                    TBAL.49    
            ENDIF                                                          TBAL.50    
    3    CONTINUE                                                          TBAL.51    
         IF (MP.GT.1) THEN                                                 TBAL.52    
            READ(9) (PMNRE(I),I=1,J22)                                     TBAL.53    
            IJ=0                                                           TBAL.54    
            DO 24 J=1,J2                                                   TBAL.55    
               IZJ=IZJ+1                                                   TBAL.56    
               IZ=IZJ                                                      TBAL.57    
               DO 23 L=1,NL                                                TBAL.58    
                  IK=L                                                     TBAL.59    
                  DO 22 K=1,J2                                             TBAL.60    
                     IJ=IJ+1                                               TBAL.61    
                     Z(IZ)=Z(IZ)+PMNRE(IJ)*ERR(IK)                         TBAL.62    
                     IK=IK+NL                                              TBAL.63    
   22             CONTINUE                                                 TBAL.64    
                  IZ=IZ+IGA                                                TBAL.65    
                  IJ=IJ-J2                                                 TBAL.66    
   23          CONTINUE                                                    TBAL.67    
               IJ=IJ+J2                                                    TBAL.68    
   24       CONTINUE                                                       TBAL.69    
         ELSE                                                              TBAL.70    
            IJS=(J2-2)*NL                                                  TBAL.71    
            IL=J2                                                          TBAL.72    
            DO 28 L=1,NL                                                   TBAL.73    
               IE=J2                                                       TBAL.74    
               IZ=IL                                                       TBAL.75    
               SRGT=0.                                                     TBAL.76    
               IJ=IJS+L                                                    TBAL.77    
               DO 26 J=2,J2                                                TBAL.78    
                  SRGT=(ERR(IJ)-EP2(IE)*SRGT)/EP1(IE-1)                    TBAL.79    
                  IJ=IJ-NL                                                 TBAL.80    
                  IZ=IZ-1                                                  TBAL.81    
                  Z(IZ)=Z(IZ)+SRGT                                         TBAL.82    
                  IE=IE-1                                                  TBAL.83    
   26          CONTINUE                                                    TBAL.84    
               IL=IL+IGA                                                   TBAL.85    
   28       CONTINUE                                                       TBAL.86    
            IZJ=J2                                                         TBAL.87    
         ENDIF                                                             TBAL.88    
   20 CONTINUE                                                             TBAL.89    
C                                                                          TBAL.90    
      IF (NHEM.EQ.2) THEN                                                  TBAL.91    
         I1=NWJ2                                                           TBAL.92    
         DO 80 MP=1,MFTBAL,MOCT                                            TBAL.93    
            J2=(NFP-MP)/2+1                                                TBAL.94    
            J2L=J2-1                                                       TBAL.95    
            J22L=J2*J2L                                                    TBAL.96    
            IJ=0                                                           TBAL.97    
            DO 60 IN=MP,NFP,MH                                             TBAL.98    
               I1=I1+1                                                     TBAL.99    
               VPS=VP(I1)                                                  TBAL.100   
               GSI1=GS(I1)                                                 TBAL.101   
               K=I1                                                        TBAL.102   
               IL=0                                                        TBAL.103   
               DO 58 L=1,NL                                                TBAL.104   
                  TA=(0.0,0.0)                                             TBAL.105   
                  SRGT=(0.0,0.0)                                           TBAL.106   
                  KK=I1                                                    TBAL.107   
                  DO 56 M=1,NL                                             TBAL.108   
                     IL=IL+1                                               TBAL.109   
                     TA=TA + G(IL)*TT(KK)                                  TBAL.110   
                     SRGT=SRGT + G(IL)*T(KK)                               TBAL.111   
                     KK=KK+IGA                                             TBAL.112   
   56             CONTINUE                                                 TBAL.113   
                  IJ=IJ+1                                                  TBAL.114   
                  ERR(IJ)=-DT(K)-SQ(IN+1)*(SRGT+GSI1+T0(L)*SP(I1)+         TBAL.115   
     +                    DELT*(TA-T0(L)*VPS))                             TBAL.116   
                  K=K+IGA                                                  TBAL.117   
   58          CONTINUE                                                    TBAL.118   
   60       CONTINUE                                                       TBAL.119   
            IF (MP.EQ.1) THEN                                              TBAL.120   
               READ(9) (PMNRE(I),I=1,J22L)                                 TBAL.121   
               IZJ=1+NWJ2                                                  TBAL.122   
               IJ=0                                                        TBAL.123   
               DO 64 J=1,J2L                                               TBAL.124   
                  IZJ=IZJ+1                                                TBAL.125   
                  IZ=IZJ                                                   TBAL.126   
                  DO 63 L=1,NL                                             TBAL.127   
                     IK=L                                                  TBAL.128   
                     DO 62 K=1,J2                                          TBAL.129   
                        IJ=IJ+1                                            TBAL.130   
                        Z(IZ)=Z(IZ) + PMNRE(IJ)*ERR(IK)                    TBAL.131   
                        IK=IK+NL                                           TBAL.132   
   62                CONTINUE                                              TBAL.133   
                     IZ=IZ+IGA                                             TBAL.134   
                     IJ=IJ-J2                                              TBAL.135   
   63             CONTINUE                                                 TBAL.136   
                  IJ=IJ+J2                                                 TBAL.137   
   64          CONTINUE                                                    TBAL.138   
            ELSE                                                           TBAL.139   
               IJS=(J2-1)*NL                                               TBAL.140   
               IL=I1                                                       TBAL.141   
               DO 78 L=1,NL                                                TBAL.142   
                  IE=I1                                                    TBAL.143   
                  IZ=IL                                                    TBAL.144   
                  SRGT=(0.0,0.0)                                           TBAL.145   
                  IJ=IJS+L                                                 TBAL.146   
                  DO 76 J=1,J2                                             TBAL.147   
                     SRGT=(ERR(IJ)-EP2(IE)*SRGT)/EP1(IE)                   TBAL.148   
                     Z(IZ)=Z(IZ)+SRGT                                      TBAL.149   
                     IJ=IJ-NL                                              TBAL.150   
                     IZ=IZ-1                                               TBAL.151   
                     IE=IE-1                                               TBAL.152   
   76             CONTINUE                                                 TBAL.153   
                  IL=IL+IGA                                                TBAL.154   
   78          CONTINUE                                                    TBAL.155   
            ENDIF                                                          TBAL.156   
   80    CONTINUE                                                          TBAL.157   
      ENDIF                                                                TBAL.158   
C                                                                          TBAL.159   
      IF(KOUNT.EQ.0)THEN                                                   TBAL.160   
         DO 4 I=1,IGA                                                      TBAL.161   
            SPMI(I)=SP(I)                                                  TBAL.162   
    4    CONTINUE                                                          TBAL.163   
         DO 8 J=1,IGB                                                      TBAL.164   
            ZMI(J)=Z(J)                                                    TBAL.165   
            DMI(J)=D(J)                                                    TBAL.166   
            TMI(J)=T(J)                                                    TBAL.167   
    8    CONTINUE                                                          TBAL.168   
      ENDIF                                                                TBAL.169   
C                                                                          TBAL.170   
      RETURN                                                               TBAL.171   
      END                                                                  TBAL.172   
      SUBROUTINE TSTEP                                                     TSTEP.2     
C                                                                          TSTEP.3     
C     Takes an adiabatic timestep in spectral space                        TSTEP.4     
C     either a centred semi-implicit timestep for KOUNT>KITS               TSTEP.5     
C     or an initial short timestep for KOUNT<KITS                          TSTEP.6     
C                                                                          TSTEP.7     
C     A time filter used for centred timestep.                             TSTEP.8     
C                                                                          TSTEP.9     
C                                                                          TSTEP.10    
C                                                                          PARAM1.2     
C     Determines model resolution                                          PARAM1.3     
C                                                                          PARAM1.4     
      PARAMETER(NN=21,MM=21,NHEM=2,NL=5,MOCT=1,MG=64,JG=16,NWJ2=121        G100.1     
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
      PARAMETER(NLT=NL+NL)                                                 TSTEP.13    
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
      COMMON/SPECTR/Z(IGB),D(IGB),T(IGB),SP(IGA),GS(IGA)                   SPECTR.5     
     *              ,SPA(IGA),VP(IGA),DTE(IGB),TT(IGB),DT(IGB),ZT(IGB)     SPECTR.6     
     *              ,ZMI(IGB),DMI(IGB),TMI(IGB),SPMI(IGA)                  SPECTR.7     
      COMPLEX Z,D,T,SP,GS,SPA,VP,DTE,TT,DT,ZT,ZMI,DMI,TMI,SPMI             SPECTR.8     
C                                                                          SPECTR.9     
C                                                                          BATS.2     
C     Constant arrays and variables associated with time and vertical      BATS.3     
C     differencing. Also counters.                                         BATS.4     
C                                                                          BATS.5     
      COMMON/BATS/  BM1(IDE),AK(NNP),AQ(NL2),G(NL2),TAU(NL2)               BATS.6     
     +              ,KOUNT,KITS,KTOTAL,KRUN,BEGDAY,ITSPD,PNU21             BATS.7     
     +              ,DELT,DELT2,CV,CG,CT,PNU,PNU2                          BATS.8     
C                                                                          BATS.9     
C                                                                          RESTOR.2     
C     Restoration fields and timescale                                     RESTOR.3     
C                                                                          RESTOR.4     
      COMMON/RESTOR/ZRES(IGN),DRES(IGN),TRES(IGN),SPRES(IGM),DAMP          RESTOR.5     
     &,ZFCE(IGB),DFCE(IGB),TFCE(IGB),SPFCE(IGA)                            NICK.207   
     &,ZFED(IGB),DFED(IGB),TFED(IGB),SPFED(IGA)                            NICK.208   
     &,ZFAN(IGB),DFAN(IGB),TFAN(IGB),SPFAN(IGA)                            NICK.209   
     &,ZDMP(IGB),DDMP(IGB),TDMP(IGB),SPDMP(IGA)                            NICK.210   
     &,ZDMI(IGB),DDMI(IGB),TDMI(IGB),SPDMI(IGA)                            NICK.211   
     &,MASK(IGC,JG),GG(IGC,JG),CD(IGC,JG),AA1,AA2,AAT                      NICK.212   
      COMPLEX ZFCE,DFCE,TFCE,SPFCE,ZFED,DFED,TFED,SPFED                    NICK.213   
     &,ZFAN,DFAN,TFAN,SPFAN                                                NICK.214   
     &,ZDMP,DDMP,TDMP,SPDMP,ZDMI,DDMI,TDMI,SPDMI                           NICK.215   
C                                                                          RESTOR.6     
      DIMENSION TBM1(NL2),D1(NL),WA(NL)                                    SC961212.4     
      INTEGER IWA(NL)                                                      SC961212.5     
      DIMENSION TBME(NL,NL)                                                TSTEP.19    
      EQUIVALENCE (TBM1(1),TBME(1,1))                                      TSTEP.20    
      COMPLEX TMPA(NL),TMPB(NL)                                            TSTEP.21    
      REAL RMPA(NLT),RMPB(NLT)                                             TSTEP.22    
      EQUIVALENCE (TMPA(1),RMPA(1)),(TMPB(1),RMPB(1))                      TSTEP.23    
      DIMENSION TTR(IGP),DTR(IGP),TMIR(IGP),D1R(NLT)                       TSTEP.24    
      EQUIVALENCE (TTR(1),TT(1)),(DTR(1),DT(1)),(TMIR(1),TMI(1))           TSTEP.25    
     +,(D1R(1),D1(1))                                                      TSTEP.26    
      COMPLEX ZPAV,TPAV,DPAV,DTI,SPAV                                      TSTEP.27    
      COMPLEX D1,SPPA,VPS,GSI1                                             TSTEP.28    
C                                                                          TSTEP.29    
      IF (KOUNT.GT.KITS) THEN                                              TSTEP.30    
C                                                                          TSTEP.31    
C        Ordinary centred semi-implicit timestep                           TSTEP.32    
C                                                                          TSTEP.33    
         I1=0                                                              TSTEP.34    
         DO 800 IHEM=1,NHEM                                                TSTEP.35    
            DO 4 MP=1,MFP,MOCT                                             TSTEP.36    
               IBM1=NL2*(MP-2 +IHEM-1)                                     TSTEP.37    
               DO 5 IN=MP,NFP,MH                                           TSTEP.38    
                  I1=I1+1                                                  TSTEP.39    
                  IF (IBM1.NE.-NL2) THEN                                   TSTEP.40    
                     SPPA=SPMI(I1)                                         TSTEP.41    
                     VPS=VP(I1)                                            TSTEP.42    
                     RCN=RSQ(IN+IHEM-1)                                    TSTEP.43    
                     GSI1=GS(I1)                                           TSTEP.44    
                     K=I1                                                  TSTEP.45    
                     CALL SGEMM('N','N',2,NL,NL,1.0,TTR(2*I1-1),IGO,       TSTEP.46    
     +                          G,NL,0.0,RMPA,2)                           TSTEP.47    
                     CALL SGEMM('N','N',2,NL,NL,1.0,TMIR(2*I1-1),IGO,      TSTEP.48    
     +                          G,NL,0.0,RMPB,2)                           TSTEP.49    
                     DO 12 L=1,NL                                          TSTEP.50    
                        D1(L)=RCN*DMI(K)+DELT*(TMPB(L)+GSI1+T0(L)*SPPA     TSTEP.51    
     1                       +RCN*DT(K)+DELT*(TMPA(L)-T0(L)*VPS))          TSTEP.52    
C                       D1(L)=RCN*DMI(K)+DELT*(TMPB(L)+GSI1+T0(L)*SPPA     NICK.141   
C    1                       +RCN*DT(K)+DELT*(TMPA(L)-T0(L)*VPS))          NICK.142   
C     IF (I1.EQ.2) PRINT*,L,D1(L)                                          NICK.143   
                        K=K+IGA                                            TSTEP.53    
   12                CONTINUE                                              TSTEP.54    
                     K=I1                                                  TSTEP.55    
                     CALL SGEMM('N','N',2,NL,NL,1.0,D1R,2,                 TSTEP.56    
     +                          BM1(IBM1+1),NL,0.0,DTR(2*I1-1),IGO)        TSTEP.57    
                     DO 14 L=1,NL                                          TSTEP.58    
                        VP(I1)=VP(I1)+DT(K)*DSIGMA(L)                      TSTEP.59    
                        K=K+IGA                                            TSTEP.60    
   14                CONTINUE                                              TSTEP.61    
                     IBM1=IBM1+NL2+NL2                                     TSTEP.62    
                     K=I1                                                  TSTEP.63    
                     CALL SGEMM('N','N',2,NL,NL,1.0,DTR(2*I1-1),IGO,       TSTEP.64    
     +                          TAU,NL,0.0,RMPA,2)                         TSTEP.65    
                     DO 16 L=1,NL                                          TSTEP.66    
                        TT(K)=TT(K)-TMPA(L)                                TSTEP.67    
                        K=K+IGA                                            TSTEP.68    
   16                CONTINUE                                              TSTEP.69    
                  ELSE                                                     TSTEP.70    
                     IBM1=NL2                                              TSTEP.71    
                  ENDIF                                                    TSTEP.72    
    5          CONTINUE                                                    TSTEP.73    
    4       CONTINUE                                                       TSTEP.74    
            I1=NWJ2                                                        TSTEP.75    
  800    CONTINUE                                                          TSTEP.76    
         DO 40 I=1,IGB                                                     TSTEP.77    
            ZPAV=ZMI(I)                                                    TSTEP.78    
            ZMI(I)=PNU21*Z(I)+PNU*ZPAV                                     TSTEP.79    
            Z(I)=ZPAV+DELT2*ZT(I)                                          TSTEP.80    
            TPAV=TMI(I)                                                    TSTEP.81    
            TMI(I)=PNU21*T(I)+PNU*TPAV                                     TSTEP.82    
            T(I)=TPAV+DELT2*TT(I)                                          TSTEP.83    
            DPAV=DMI(I)                                                    TSTEP.84    
            DTI=DT(I)                                                      TSTEP.85    
            DMI(I)=PNU21*D(I)+PNU*DPAV                                     TSTEP.86    
            D(I)=DTI+DTI-DPAV                                              TSTEP.87    
   40    CONTINUE                                                          TSTEP.88    
      DO 41 I=2,IGA                                                        NICK.137   
            SPAV=SPMI(I)                                                   TSTEP.90    
            SPMI(I)=PNU21*SP(I)+PNU*SPAV                                   TSTEP.91    
            SP(I)=SPAV-DELT2*VP(I)                                         TSTEP.92    
   41    CONTINUE                                                          TSTEP.93    
         RETURN                                                            TSTEP.94    
      ELSE                                                                 TSTEP.95    
C                                                                          TSTEP.96    
C        Initial short timestep                                            TSTEP.97    
C                                                                          TSTEP.98    
         I1=0                                                              TSTEP.99    
         DO 820 IHEM=1,NHEM                                                TSTEP.100   
            DO 8 MP=1,MFP,MOCT                                             TSTEP.101   
               DO 9 IN=MP,NFP,MH                                           TSTEP.102   
                  I1=I1+1                                                  TSTEP.103   
                  IF (I1+IHEM.NE.2) THEN                                   TSTEP.104   
                     SPPA=SPMI(I1)                                         TSTEP.105   
                     VPS=VP(I1)                                            TSTEP.106   
                     RCN=RSQ(IN+IHEM-1)                                    TSTEP.107   
                     GSI1=GS(I1)                                           TSTEP.108   
                     DO 21 IL=1,NL2                                        TSTEP.109   
                        TBM1(IL)=AQ(IL)                                    TSTEP.110   
   21                CONTINUE                                              TSTEP.111   
                     DO 30 L=1,NL                                          TSTEP.112   
                        TBME(L,L)=TBME(L,L)+RCN                            TSTEP.113   
   30                CONTINUE                                              TSTEP.114   
                    CALL MATINV(TBME,NL,NL,IWA,WA)                         SC961212.6     
                     K=I1                                                  TSTEP.116   
                     CALL SGEMM('N','N',2,NL,NL,1.0,TTR(2*I1-1),IGO,       TSTEP.117   
     +                          G,NL,0.0,RMPA,2)                           TSTEP.118   
                     CALL SGEMM('N','N',2,NL,NL,1.0,TMIR(2*I1-1),IGO,      TSTEP.119   
     +                          G,NL,0.0,RMPB,2)                           TSTEP.120   
                     DO 22 L=1,NL                                          TSTEP.121   
                        D1(L)=RCN*DMI(K)+DELT*(TMPB(L)+GSI1+T0(L)*SPPA+    TSTEP.122   
     1                        RCN*DT(K)+DELT*(TMPA(L)-T0(L)*VPS))          TSTEP.123   
                        K=K+IGA                                            TSTEP.124   
   22                CONTINUE                                              TSTEP.125   
                     K=I1                                                  TSTEP.126   
                     CALL SGEMM('N','N',2,NL,NL,1.0,D1R,2,                 TSTEP.127   
     +                          TBM1,NL,0.0,DTR(2*I1-1),IGO)               TSTEP.128   
                     DO 24 L=1,NL                                          TSTEP.129   
                        VP(I1)=VP(I1)+DT(K)*DSIGMA(L)                      TSTEP.130   
                        K=K+IGA                                            TSTEP.131   
   24                CONTINUE                                              TSTEP.132   
                     K=I1                                                  TSTEP.133   
                     CALL SGEMM('N','N',2,NL,NL,1.0,DTR(2*I1-1),IGO,       TSTEP.134   
     +                          TAU,NL,0.0,RMPA,2)                         TSTEP.135   
                     DO 26 L=1,NL                                          TSTEP.136   
                        TT(K)=TT(K)-TMPA(L)                                TSTEP.137   
                        K=K+IGA                                            TSTEP.138   
   26                CONTINUE                                              TSTEP.139   
                  ENDIF                                                    TSTEP.140   
    9          CONTINUE                                                    TSTEP.141   
    8       CONTINUE                                                       TSTEP.142   
            I1=NWJ2                                                        TSTEP.143   
  820    CONTINUE                                                          TSTEP.144   
         DO 42 I=1,IGB                                                     TSTEP.145   
            Z(I)=ZMI(I)+DELT2*ZT(I)                                        TSTEP.146   
            T(I)=TMI(I)+DELT2*TT(I)                                        TSTEP.147   
            D(I)=DT(I)+DT(I)-DMI(I)                                        TSTEP.148   
   42    CONTINUE                                                          TSTEP.149   
      DO 43 I=2,IGA                                                        NICK.138   
            SP(I)=SPMI(I)-DELT2*VP(I)                                      TSTEP.151   
   43    CONTINUE                                                          TSTEP.152   
         DO 29 L=1,NL2                                                     TSTEP.153   
            AQ(L)=AQ(L)*4.                                                 TSTEP.154   
   29    CONTINUE                                                          TSTEP.155   
         RETURN                                                            TSTEP.156   
      ENDIF                                                                TSTEP.157   
C                                                                          TSTEP.158   
      END                                                                  TSTEP.159   
      SUBROUTINE WRSPS(A,IA)                                               WRSPS.2     
C                                                                          WRSPS.3     
C     Prints spectral coefficients                                         WRSPS.4     
C                                                                          WRSPS.5     
C                                                                          PARAM1.2     
C     Determines model resolution                                          PARAM1.3     
C                                                                          PARAM1.4     
      PARAMETER(NN=21,MM=21,NHEM=2,NL=5,MOCT=1,MG=64,JG=16,NWJ2=121        G100.1     
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
      PARAMETER(RAD=180./PI)                                               WRSPS.8     
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
C                                                                          OUTCON.2     
C     Switches counters and constants controlling type and frequency of    OUTCON.3     
C     model output                                                         OUTCON.4     
C                                                                          OUTCON.5     
      COMMON/OUTCON/RNTAPE,NCOEFF,NLAT,INLAT,INSPC                         OUTCON.6     
     +              ,KOUNTP,KOUNTE,KOUNTH,KOUNTR                           OUTCON.7     
     +              ,KOUTP,KOUTE,KOUTH,KOUTR,DAY                           OUTCON.8     
     +              ,SQR2,RSQR2,EAM1,EAM2,TOUT1,TOUT2,RMG                  OUTCON.9     
     +              ,LSPO(NL),LGPO(NL)                                     OUTCON.10    
      LOGICAL LSPO,LGPO                                                    OUTCON.11    
C                                                                          OUTCON.12    
      COMPLEX A(NWJ2)                                                      WRSPS.11    
      CHARACTER COEFF(NWJ2,2)*8                                            WRSPS.12    
      REAL FP(IGA)                                                         WRSPS.13    
      INTEGER IP(IGA)                                                      WRSPS.14    
      SAVE COEFF,FP,IP                                                     WRSPS.15    
      COMPLEX POLAR,Z                                                      WRSPS.16    
      POLAR(Z)=CMPLX(ABS(Z),ATAN2(AIMAG(Z),REAL(Z)+1.0E-20)*RAD)           WRSPS.17    
      DATA COEFF/MJP*' (  ,  )'/                                           WRSPS.18    
      IG=0                                                                 WRSPS.19    
      I=0                                                                  WRSPS.20    
      DO 200 MP=1,MFP,MOCT                                                 WRSPS.21    
C                                                                          WRSPS.22    
C        Only print if MP<NCOEFF                                           WRSPS.23    
C                                                                          WRSPS.24    
         IF (MP.GT.NCOEFF) RETURN                                          WRSPS.25    
C                                                                          WRSPS.26    
         IBEG=IG                                                           WRSPS.27    
         DO 100 JP=MP,NCOEFF,MH                                            WRSPS.28    
            IG=IG+1                                                        WRSPS.29    
            I=I+1                                                          WRSPS.30    
            IP(I)=IG                                                       WRSPS.31    
            IF (MP.EQ.1) THEN                                              WRSPS.32    
               FP(I)=1.0                                                   WRSPS.33    
            ELSE                                                           WRSPS.34    
               FP(I)=2.0                                                   WRSPS.35    
            ENDIF                                                          WRSPS.36    
            WRITE(COEFF(I,1)(3:4),'(I2)')MP-1                              WRSPS.37    
            WRITE(COEFF(I,2)(3:4),'(I2)')MP-1                              WRSPS.38    
            WRITE(COEFF(I,1)(6:7),'(I2)')JP                                WRSPS.39    
            WRITE(COEFF(I,2)(6:7),'(I2)')JP-1                              WRSPS.40    
 100     CONTINUE                                                          WRSPS.41    
         IG=IBEG+(NFP-MP+2)/MH                                             WRSPS.42    
 200  CONTINUE                                                             WRSPS.43    
      RETURN                                                               WRSPS.44    
C******************************                                            WRSPS.45    
      ENTRY WRSPA(A,IA)                                                    WRSPS.46    
      IF (NHEM.EQ.1) THEN                                                  WRSPS.47    
         WRITE(2,1000)(COEFF(I,IA),POLAR(A(IP(I))*FP(I)),I=1,INSPC)        WRSPS.48    
      ELSE                                                                 WRSPS.49    
         WRITE(2,1000)(COEFF(I,2),POLAR(A(IP(I)+(2-IA)*NWJ2)*FP(I)),       WRSPS.50    
     1   COEFF(I,1),POLAR(A(IP(I)+(IA-1)*NWJ2)*FP(I)),I=1,INSPC)           WRSPS.51    
      ENDIF                                                                WRSPS.52    
 1000 FORMAT(3(A8,1X,1PE8.2,0PF7.1))                                       WRSPS.53    
      RETURN                                                               WRSPS.54    
      END                                                                  WRSPS.55    
C     ******************************************************************   WRSPS.56    
      SUBROUTINE XSECT(ISKIP)                                              XSECT.2     
C                                                                          XSECT.3     
C     This subroutine gives quick look sigma-latitude x-sections           XSECT.4     
C     of [U], [T] and [V*T*].                                              XSECT.5     
C                                                                          XSECT.6     
C                                                                          PARAM1.2     
C     Determines model resolution                                          PARAM1.3     
C                                                                          PARAM1.4     
      PARAMETER(NN=21,MM=21,NHEM=2,NL=5,MOCT=1,MG=64,JG=16,NWJ2=121        G100.1     
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
C     Constant arrays and variables associated with time and vertical      BATS.3     
C     differencing. Also counters.                                         BATS.4     
C                                                                          BATS.5     
      COMMON/BATS/  BM1(IDE),AK(NNP),AQ(NL2),G(NL2),TAU(NL2)               BATS.6     
     +              ,KOUNT,KITS,KTOTAL,KRUN,BEGDAY,ITSPD,PNU21             BATS.7     
     +              ,DELT,DELT2,CV,CG,CT,PNU,PNU2                          BATS.8     
C                                                                          BATS.9     
C                                                                          GRIDP.2     
C     Array ordering in GRIDP must correspond to that in SPECTR.           GRIDP.3     
C     Real arrays: multi-level arrays are 1-dimensional.                   GRIDP.4     
C                                                                          GRIDP.5     
      COMMON/GRIDP/ CHIG(IGD),SFG(IGD),UG(IGD),VG(IGD)                     GRIDP.6     
     *              ,ZG(IGD),DG(IGD),TG(IGD)                               GRIDP.7     
     *              ,PLG(IGC),PJG(IGC),PMG(IGC)                            GRIDP.8     
     *              ,SPG(IGC),VPG(IGC),EG(IGD)                             GRIDP.9     
     *              ,TNLG(IGD),FUG(IGD),FVG(IGD),UTG(IGD)                  GRIDP.10    
     *              ,VTG(IGD),FVGT(IGD),FUGT(IGD)                          GRIDP.11    
C                                                                          GRIDP.12    
C                                                                          LEGAU.2     
C     Legendre polynomials and information about gaussian latitudes        LEGAU.3     
C                                                                          LEGAU.4     
      COMMON/LEGAU/ ALPJ(MJP),DALPJ(MJP)                                   LEGAU.5     
     +              ,ALP(NWJ2,2,JGL),DALP(NWJ2,2,JGL)                      LEGAU.6     
     +              ,RLP(NWJ2,2,JGL),RDLP(NWJ2,2,JGL)                      LEGAU.7     
     +              ,SI(JGG),CS(JGG),SISQ(JGG),CSSQ(JGG),SECSQ(JGG)        LEGAU.8     
     +              ,ALAT(JGG),GWT(JGG),AW(JGG),JH,JL,JINC                 LEGAU.9     
C                                                                          LEGAU.10    
C                                                                          OUTCON.2     
C     Switches counters and constants controlling type and frequency of    OUTCON.3     
C     model output                                                         OUTCON.4     
C                                                                          OUTCON.5     
      COMMON/OUTCON/RNTAPE,NCOEFF,NLAT,INLAT,INSPC                         OUTCON.6     
     +              ,KOUNTP,KOUNTE,KOUNTH,KOUNTR                           OUTCON.7     
     +              ,KOUTP,KOUTE,KOUTH,KOUTR,DAY                           OUTCON.8     
     +              ,SQR2,RSQR2,EAM1,EAM2,TOUT1,TOUT2,RMG                  OUTCON.9     
     +              ,LSPO(NL),LGPO(NL)                                     OUTCON.10    
      LOGICAL LSPO,LGPO                                                    OUTCON.11    
C                                                                          OUTCON.12    
C                                                                          RESTOR.2     
C     Restoration fields and timescale                                     RESTOR.3     
C                                                                          RESTOR.4     
      COMMON/RESTOR/ZRES(IGN),DRES(IGN),TRES(IGN),SPRES(IGM),DAMP          RESTOR.5     
     &,ZFCE(IGB),DFCE(IGB),TFCE(IGB),SPFCE(IGA)                            NICK.207   
     &,ZFED(IGB),DFED(IGB),TFED(IGB),SPFED(IGA)                            NICK.208   
     &,ZFAN(IGB),DFAN(IGB),TFAN(IGB),SPFAN(IGA)                            NICK.209   
     &,ZDMP(IGB),DDMP(IGB),TDMP(IGB),SPDMP(IGA)                            NICK.210   
     &,ZDMI(IGB),DDMI(IGB),TDMI(IGB),SPDMI(IGA)                            NICK.211   
     &,MASK(IGC,JG),GG(IGC,JG),CD(IGC,JG),AA1,AA2,AAT                      NICK.212   
      COMPLEX ZFCE,DFCE,TFCE,SPFCE,ZFED,DFED,TFED,SPFED                    NICK.213   
     &,ZFAN,DFAN,TFAN,SPFAN                                                NICK.214   
     &,ZDMP,DDMP,TDMP,SPDMP,ZDMI,DDMI,TDMI,SPDMI                           NICK.215   
C                                                                          RESTOR.6     
      INTEGER LU(IGG),LT(IGG),LVT(IGG)                                     XSECT.15    
C                                                                          XSECT.16    
C     Set ISLT, The default being to have 16 I5 integers per level.        XSECT.17    
C                                                                          XSECT.18    
      IF (ISKIP.EQ.0) THEN                                                 XSECT.19    
         RETURN                                                            XSECT.20    
      ELSE IF (ISKIP.GT.0) THEN                                            XSECT.21    
         ISLT = ISKIP                                                      XSECT.22    
      ELSE                                                                 XSECT.23    
         ISLT = (NHEM*JG-1)/18 + 1                                         XSECT.24    
      ENDIF                                                                XSECT.25    
C                                                                          XSECT.26    
C     Output is wanted. Read grid point fields from stream 24              XSECT.27    
C                                                                          XSECT.28    
      REWIND 24                                                            XSECT.29    
      RMG=1./REAL(MG)                                                      XSECT.30    
      WRITE (2,104) DAY                                                    XSECT.31    
  104 FORMAT(/' CROSS SECTIONS FOR DAY',F6.1)                              XSECT.32    
      DO 10 J=1,JG                                                         XSECT.33    
C                                                                          XSECT.34    
C        Loop over latitudes to read gridpoint data, dimensionalise        XSECT.35    
C        and calculate zonal means                                         XSECT.36    
C                                                                          XSECT.37    
         SEC=ALAT(J)/57.29578                                              XSECT.38    
         SEC=10.*CV/COS(SEC)                                               XSECT.39    
         READ(24) ZG,DG,UG,VG,TG,SPG                                       XSECT.40    
C                                                                          XSECT.41    
C        Loop for [U] and [T] sections                                     XSECT.42    
C                                                                          XSECT.43    
         DO 11 L=1,NL                                                      XSECT.44    
            DO 12 IHEM=1,NHEM                                              XSECT.45    
               IP=(L-1)*IGC+(IHEM-1)*MGPP                                  XSECT.46    
               UB=0.                                                       XSECT.47    
               DO 30 I=1,MG                                                XSECT.48    
                  UB=UB+UG(IP+I)                                           XSECT.49    
   30          CONTINUE                                                    XSECT.50    
               UB=UB*RMG                                                   XSECT.51    
               TB=0.                                                       XSECT.52    
               DO 40 I=1,MG                                                XSECT.53    
                  TB=TB+TG(IP+I)                                           XSECT.54    
   40          CONTINUE                                                    XSECT.55    
               TB=TB*RMG+T0(L)                                             XSECT.56    
C                                                                          XSECT.57    
C              Check whether hemisphere or globe                           XSECT.58    
C                                                                          XSECT.59    
               IF (IHEM.EQ.1) THEN                                         XSECT.60    
                  JJ=J                                                     XSECT.61    
               ELSE                                                        XSECT.62    
                  JJ=JGGP-J                                                XSECT.63    
               ENDIF                                                       XSECT.64    
               IX=(L-1)*JGG+JJ                                             XSECT.65    
               LU(IX)=NINT(UB*SEC)                                         XSECT.66    
               LT(IX)=NINT(TB*CT)                                          XSECT.67    
   12       CONTINUE                                                       XSECT.68    
   11    CONTINUE                                                          XSECT.69    
C                                                                          XSECT.70    
C        Loop for temperature flux section                                 XSECT.71    
C                                                                          XSECT.72    
         DO 13 L=1,NL                                                      XSECT.73    
            DO 14 IHEM=1,NHEM                                              XSECT.74    
               IP=(L-1)*IGC+(IHEM-1)*MGPP                                  XSECT.75    
               VT=0.                                                       XSECT.76    
               DO 15 I=1,MG                                                XSECT.77    
                  VT=VT+TG(IP+I)*VG(IP+I)                                  XSECT.78    
   15          CONTINUE                                                    XSECT.79    
               VT=RMG*VT                                                   XSECT.80    
               TB=0.                                                       XSECT.81    
               DO 50 I=1,MG                                                XSECT.82    
                  TB=TB+TG(IP+I)                                           XSECT.83    
   50          CONTINUE                                                    XSECT.84    
               TB=TB*RMG                                                   XSECT.85    
               VB=0.                                                       XSECT.86    
               DO 60 I=1,MG                                                XSECT.87    
                  VB=VB+VG(IP+I)                                           XSECT.88    
   60          CONTINUE                                                    XSECT.89    
               VB=VB*RMG                                                   XSECT.90    
C                                                                          XSECT.91    
C              Check whether hemisphere or globe                           XSECT.92    
C                                                                          XSECT.93    
               IF (IHEM.EQ.1) THEN                                         XSECT.94    
                  JJ=J                                                     XSECT.95    
               ELSE                                                        XSECT.96    
                  JJ=JGGP-J                                                XSECT.97    
               ENDIF                                                       XSECT.98    
               IX=(L-1)*JGG+JJ                                             XSECT.99    
               LVT(IX)=NINT((VT-VB*TB)*SEC*CT)                             XSECT.100   
   14       CONTINUE                                                       XSECT.101   
   13    CONTINUE                                                          XSECT.102   
   10 CONTINUE                                                             XSECT.103   
C                                                                          XSECT.104   
C     Printing sections - separate loop for each field.                    XSECT.105   
C     First the zonal wind                                                 XSECT.106   
C                                                                          XSECT.107   
      WRITE (2,100)                                                        XSECT.108   
  100 FORMAT(' ZONAL WIND IN 0.1 m/s')                                     XSECT.109   
      DO 20 L=1,NL                                                         XSECT.110   
         J0=(L-1)*JGG                                                      XSECT.111   
         WRITE (2,101) (LU(J0+J),J=1,JGG,ISLT)                             XSECT.112   
   20 CONTINUE                                                             XSECT.113   
101   FORMAT(18I4)                                                         XSECT.114   
C                                                                          XSECT.115   
C     Secondly the temperature                                             XSECT.116   
C                                                                          XSECT.117   
      WRITE(2,102)                                                         XSECT.118   
  102 FORMAT(' TEMPERATURE IN K')                                          XSECT.119   
      DO 21 L=1,NL                                                         XSECT.120   
         J0=(L-1)*JGG                                                      XSECT.121   
         WRITE(2,101) (LT(J0+J),J=1,JGG,ISLT)                              XSECT.122   
   21 CONTINUE                                                             XSECT.123   
C                                                                          XSECT.124   
C     Thirdly temperature flux                                             XSECT.125   
C                                                                          XSECT.126   
      WRITE(2,103)                                                         XSECT.127   
  103 FORMAT(' POLEWARD TEMPERATURE FLUX IN 0.1Km/s')                      XSECT.128   
      DO 22 L=1,NL                                                         XSECT.129   
         J0=(L-1)*JGG                                                      XSECT.130   
         WRITE(2,101) (LVT(J0+J),J=1,JGG,ISLT)                             XSECT.131   
   22 CONTINUE                                                             XSECT.132   
C                                                                          XSECT.133   
      REWIND 24                                                            XSECT.134   
      RETURN                                                               XSECT.135   
      END                                                                  XSECT.136   
      SUBROUTINE LTIUV                                                     LTIUV.2     
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
      PARAMETER(NN=21,MM=21,NHEM=2,NL=5,MOCT=1,MG=64,JG=16,NWJ2=121        G100.1     
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
      COMMON/GRIDP/ CHIG(IGL,NL),SFG(IGL,NL),UG(IGL,NL),VG(IGL,NL)         GRIDP3.6     
     *              ,ZG(IGL,NL),DG(IGL,NL),TG(IGL,NL)                      GRIDP3.7     
     *              ,PLG(IGL),PJG(IGL),PMG(IGL)                            GRIDP3.8     
     *              ,SPG(IGL),VPG(IGL),EG(IGL,NL)                          GRIDP3.9     
     *              ,TNLG(IGL,NL),FUG(IGL,NL),FVG(IGL,NL),UTG(IGL,NL)      GRIDP3.10    
     *              ,VTG(IGL,NL),FVGT(IGL,NL),FUGT(IGL,NL)                 GRIDP3.11    
      COMPLEX CHIG,SFG,UG,VG,ZG,DG,TG,PLG,PJG,PMG                          GRIDP3.12    
     *       ,SPG,VPG,EG,TNLG,FUG,FVG,UTG,VTG,FVGT,FUGT                    GRIDP3.13    
C                                                                          GRIDP3.14    
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
C     Array ordering in SPECTR must correspond to that in GRIDP.           SPECTR.3     
C                                                                          SPECTR.4     
      COMMON/SPECTR/Z(IGB),D(IGB),T(IGB),SP(IGA),GS(IGA)                   SPECTR.5     
     *              ,SPA(IGA),VP(IGA),DTE(IGB),TT(IGB),DT(IGB),ZT(IGB)     SPECTR.6     
     *              ,ZMI(IGB),DMI(IGB),TMI(IGB),SPMI(IGA)                  SPECTR.7     
      COMPLEX Z,D,T,SP,GS,SPA,VP,DTE,TT,DT,ZT,ZMI,DMI,TMI,SPMI             SPECTR.8     
C                                                                          SPECTR.9     
                                                                           LTIUV.19    
C     Preset Fourier arrays.                                               LTIUV.20    
                                                                           LTIUV.21    
      DO 10 L=1,NL                                                         LTIUV.22    
         DO 10 I=1,IGL                                                     LTIUV.23    
            CHIG(I,L)=0.                                                   LTIUV.24    
            SFG(I,L)=0.                                                    LTIUV.25    
            UG(I,L)=0.                                                     LTIUV.26    
            VG(I,L)=0.                                                     LTIUV.27    
   10 CONTINUE                                                             LTIUV.28    
                                                                           LTIUV.29    
C     Remove planetary vorticity in spectral space, so all transforms      LTIUV.30    
C     use relative vorticity.                                              LTIUV.31    
                                                                           LTIUV.32    
      DO 30 I=1,IGB,IGA                                                    LTIUV.33    
         Z(I)=Z(I)-EZ                                                      LTIUV.34    
   30 CONTINUE                                                             LTIUV.35    
                                                                           LTIUV.36    
C        Wind components: calls to HEXP give following Fourier fields:     LTIUV.37    
C           SFG  :   streamfunction.                                       LTIUV.38    
C           CHIG :   velocity potential.                                   LTIUV.39    
C           UG   :   -U(rotational).                                       LTIUV.40    
C           VG   :   V(divergent).                                         LTIUV.41    
                                                                           LTIUV.42    
         CALL HEXP(Z,SFG ,NL,5)                                            LTIUV.43    
         CALL HEXP(D,CHIG,NL,6)                                            LTIUV.44    
         CALL HEXP(Z,UG  ,NL,7)                                            LTIUV.45    
         CALL HEXP(D,VG  ,NL,8)                                            LTIUV.46    
                                                                           LTIUV.47    
C     Restore planetary vorticity in spectral space.                       LTIUV.48    
                                                                           LTIUV.49    
      DO 40 I=1,IGB,IGA                                                    LTIUV.50    
         Z(I)=Z(I)+EZ                                                      LTIUV.51    
   40 CONTINUE                                                             LTIUV.52    
                                                                           LTIUV.53    
C     Sum to give total winds.  CMPA takes x-derivative.                   LTIUV.54    
                                                                           LTIUV.55    
      DO 60 L=1,NL                                                         LTIUV.56    
         DO 60 I=1,IGL                                                     LTIUV.57    
            UG(I,L)=CMPA(I)*CHIG(I,L)-UG(I,L)                              LTIUV.58    
            VG(I,L)=CMPA(I)* SFG(I,L)+VG(I,L)                              LTIUV.59    
   60 CONTINUE                                                             LTIUV.60    
                                                                           LTIUV.61    
      RETURN                                                               LTIUV.62    
      END                                                                  LTIUV.63    
      SUBROUTINE LTDUV                                                     LTDUV.2     
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
      PARAMETER(NN=21,MM=21,NHEM=2,NL=5,MOCT=1,MG=64,JG=16,NWJ2=121        G100.1     
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
      COMMON/GRIDP/ CHIG(IGL,NL),SFG(IGL,NL),UG(IGL,NL),VG(IGL,NL)         GRIDP3.6     
     *              ,ZG(IGL,NL),DG(IGL,NL),TG(IGL,NL)                      GRIDP3.7     
     *              ,PLG(IGL),PJG(IGL),PMG(IGL)                            GRIDP3.8     
     *              ,SPG(IGL),VPG(IGL),EG(IGL,NL)                          GRIDP3.9     
     *              ,TNLG(IGL,NL),FUG(IGL,NL),FVG(IGL,NL),UTG(IGL,NL)      GRIDP3.10    
     *              ,VTG(IGL,NL),FVGT(IGL,NL),FUGT(IGL,NL)                 GRIDP3.11    
      COMPLEX CHIG,SFG,UG,VG,ZG,DG,TG,PLG,PJG,PMG                          GRIDP3.12    
     *       ,SPG,VPG,EG,TNLG,FUG,FVG,UTG,VTG,FVGT,FUGT                    GRIDP3.13    
C                                                                          GRIDP3.14    
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
C     Array ordering in SPECTR must correspond to that in GRIDP.           SPECTR.3     
C                                                                          SPECTR.4     
      COMMON/SPECTR/Z(IGB),D(IGB),T(IGB),SP(IGA),GS(IGA)                   SPECTR.5     
     *              ,SPA(IGA),VP(IGA),DTE(IGB),TT(IGB),DT(IGB),ZT(IGB)     SPECTR.6     
     *              ,ZMI(IGB),DMI(IGB),TMI(IGB),SPMI(IGA)                  SPECTR.7     
      COMPLEX Z,D,T,SP,GS,SPA,VP,DTE,TT,DT,ZT,ZMI,DMI,TMI,SPMI             SPECTR.8     
C                                                                          SPECTR.9     
                                                                           LTDUV.22    
      COMPLEX GWORK(IGL,NL)                                                LTDUV.23    
                                                                           LTDUV.24    
C     Prepare Fourier arrays:                                              LTDUV.25    
C     - change sign of terms which contribute negatively to tendency,      LTDUV.26    
C     - apply (1-mu**2) weighting,                                         LTDUV.27    
C     - take zonal derivatives,                                            LTDUV.28    
C     - make copies of effective momentum tendencies.                      LTDUV.29    
                                                                           LTDUV.30    
      DO 10 L=1,NL                                                         LTDUV.31    
         DO 10 I=1,IGL                                                     LTDUV.32    
            FVGT(I,L)=FVG(I,L)                                             LTDUV.33    
            FUGT(I,L)=-FUG(I,L)                                            LTDUV.34    
            FUG(I,L)=CMPA(I)*FUG(I,L)/CSSQ(JH)                             LTDUV.35    
            FVG(I,L)=CMPA(I)*FVG(I,L)/CSSQ(JH)                             LTDUV.36    
   10 CONTINUE                                                             LTDUV.37    
                                                                           LTDUV.38    
C     legendre transfroms to give ZT and DT                                LTDUV.39    
                                                                           LTDUV.40    
         CALL HANAL(FVG,GWORK,ZT,NL,1)                                     LTDUV.41    
         CALL HANAL(FUGT,GWORK,ZT,NL,3)                                    LTDUV.42    
         CALL HANAL(FUG,GWORK,DT,NL,2)                                     LTDUV.43    
         CALL HANAL(FVGT,GWORK,DT,NL,4)                                    LTDUV.44    
                                                                           LTDUV.45    
      RETURN                                                               LTDUV.46    
      END                                                                  LTDUV.47    
      SUBROUTINE DGRMLT                                                    DGRMLT.2     
                                                                           DGRMLT.3     
C     Computes nonlinear tendencies in grid point space                    DGRMLT.4     
C     for the present latitude                                             DGRMLT.5     
                                                                           DGRMLT.6     
C                                                                          PARAM1.2     
C     Determines model resolution                                          PARAM1.3     
C                                                                          PARAM1.4     
      PARAMETER(NN=21,MM=21,NHEM=2,NL=5,MOCT=1,MG=64,JG=16,NWJ2=121        G100.1     
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
C                                                                          GRIDP2.2     
C     Array ordering in GRIDP must correspond to that in SPECTR.           GRIDP2.3     
C     Real arrays: multi-level arrays are 2-dimensional.                   GRIDP2.4     
C                                                                          GRIDP2.5     
      COMMON/GRIDP/ CHIG(IGC,NL),SFG(IGC,NL),UG(IGC,NL),VG(IGC,NL)         GRIDP2.6     
     *              ,ZG(IGC,NL),DG(IGC,NL),TG(IGC,NL)                      GRIDP2.7     
     *              ,PLG(IGC),PJG(IGC),PMG(IGC)                            GRIDP2.8     
     *              ,SPG(IGC),VPG(IGC),EG(IGC,NL)                          GRIDP2.9     
     *              ,TNLG(IGC,NL),FUG(IGC,NL),FVG(IGC,NL),UTG(IGC,NL)      GRIDP2.10    
     *              ,VTG(IGC,NL),FVGT(IGC,NL),FUGT(IGC,NL)                 GRIDP2.11    
C                                                                          GRIDP2.12    
C                                                                          RESTOR.2     
C     Restoration fields and timescale                                     RESTOR.3     
C                                                                          RESTOR.4     
      COMMON/RESTOR/ZRES(IGN),DRES(IGN),TRES(IGN),SPRES(IGM),DAMP          RESTOR.5     
     &,ZFCE(IGB),DFCE(IGB),TFCE(IGB),SPFCE(IGA)                            NICK.207   
     &,ZFED(IGB),DFED(IGB),TFED(IGB),SPFED(IGA)                            NICK.208   
     &,ZFAN(IGB),DFAN(IGB),TFAN(IGB),SPFAN(IGA)                            NICK.209   
     &,ZDMP(IGB),DDMP(IGB),TDMP(IGB),SPDMP(IGA)                            NICK.210   
     &,ZDMI(IGB),DDMI(IGB),TDMI(IGB),SPDMI(IGA)                            NICK.211   
     &,MASK(IGC,JG),GG(IGC,JG),CD(IGC,JG),AA1,AA2,AAT                      NICK.212   
      COMPLEX ZFCE,DFCE,TFCE,SPFCE,ZFED,DFED,TFED,SPFED                    NICK.213   
     &,ZFAN,DFAN,TFAN,SPFAN                                                NICK.214   
     &,ZDMP,DDMP,TDMP,SPDMP,ZDMI,DDMI,TDMI,SPDMI                           NICK.215   
C                                                                          RESTOR.6     
                                                                           DGRMLT.13    
      WSC=10./WW/RADEA                                                     DGRMLT.14    
      IOFM=0                                                               DGRMLT.15    
      DO 800 IHEM=1,NHEM                                                   DGRMLT.16    
         DO 160 L=1,NL                                                     DGRMLT.17    
                                                                           DGRMLT.18    
            IF (L.LT.NL) THEN                                              DGRMLT.19    
               DO 210 I=1,MG                                               DGRMLT.20    
                  J=I+IOFM                                                 DGRMLT.21    
                  FUG(J,L)=0.                                              DGRMLT.22    
                  FVG(J,L)=0.                                              DGRMLT.23    
 210           CONTINUE                                                    DGRMLT.24    
            ELSE                                                           DGRMLT.25    
               DO 220 I=1,MG                                               DGRMLT.26    
                  J=I+IOFM                                                 DGRMLT.27    
                                                                           DGRMLT.28    
C*****linear drag, just a function of terrain                              DGRMLT.29    
                  FUG(J,L)= -DAMP*UG(J,L) *CD(J,JH)                        DGRMLT.30    
                  FVG(J,L)= -DAMP*VG(J,L) *CD(J,JH)                        DGRMLT.31    
                                                                           DGRMLT.32    
C*****OR nonlinear drag: same as linear for wind speed = WSC               DGRMLT.33    
C     WSSQ=(UG(J,L)*UG(J,L)+VG(J,L)*VG(J,L))/CS(JH)/CS(JH)                 DGRMLT.34    
C     WS=SQRT(WSSQ)                                                        DGRMLT.35    
C     DRAG=DAMP  *CD(J,JH) *(WS/2. + WSSQ/2./WSC)                          DGRMLT.36    
C     FUG(J,L)= -DRAG * UG(J,L)/WS                                         DGRMLT.37    
C     FVG(J,L)= -DRAG * VG(J,L)/WS                                         DGRMLT.38    
                                                                           DGRMLT.39    
 220           CONTINUE                                                    DGRMLT.40    
            ENDIF                                                          DGRMLT.41    
                                                                           DGRMLT.42    
 160     CONTINUE                                                          DGRMLT.43    
         IOFM=MGPP                                                         DGRMLT.44    
 800  CONTINUE                                                             DGRMLT.45    
                                                                           DGRMLT.46    
      RETURN                                                               DGRMLT.47    
      END                                                                  DGRMLT.48    
