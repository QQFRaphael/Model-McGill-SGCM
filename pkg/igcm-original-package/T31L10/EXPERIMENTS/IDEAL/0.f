      PROGRAM MAKEANOM
      PARAMETER (LN=128,LA=64)
      REAL G(LN,LA),PROFILE(10),SIGMA(10)
      DIMENSION IHD(10)

      DATA SIGMA/0.05,0.15,0.25,0.35,0.45,0.55,0.65,0.75,0.85,0.95/

      PI=3.141592654
      DO L=1,10
      PROFILE(L)=PI * (1-SIGMA(L))*SIN(PI*(1-SIGMA(L)))
      PRINT*,SIGMA(L),PROFILE(L)
      ENDDO

      OPEN(10,FILE='gridanom',FORM='UNFORMATTED')
      REWIND(10)

      DO 10 NREC=1,31
      DO 20 J=1,LA
      DO 30 I=1,LN
      G(I,J)=0.
 30   CONTINUE
 20   CONTINUE
C*****THIS RANGE OF NREC GIVES HEATING ANOMALY ONLY,
C*****PICK OTHER RANGES FOR VORTICITY, DIVERGENCE 
C*****OR MASS SOURCE.
      IF ((NREC.GT.20).AND.(NREC.LT.31)) THEN

C-------------------------------------------------------------

C*****midlat source on dateline, 28.125N---> data.fanMJO2
      IRI=16
      IRJ=3
      IORIG=135
      IORI=IORIG*128/360 + 1
      IORJ=32
      DO 50 I=IORI-IRI,IORI+IRI
      DO 50 J=IORJ-IRJ,IORJ+IRJ
      X=REAL(I-IORI)/REAL(IRI)
      Y=REAL(J-IORJ)/REAL(IRJ)
      RR=SQRT(X**2. + Y**2.)
      DEGSPD=5.
      AMP=PROFILE(NREC-20) * DEGSPD / 86400.
      G(I,J)=AMP*(COS(RR*PI/2.))**2.
      IF (COS(RR*PI/2.).LT.0.) G(I,J)=0.
 50   CONTINUE

      END IF

      DO 40 I=1,10
 40   IHD(I)=0

      WRITE(10)IHD
      WRITE(10)G

 10   CONTINUE

      STOP
      END
