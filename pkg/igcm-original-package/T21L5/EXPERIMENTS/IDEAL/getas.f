C from rimbaud:/usr/people/hall/data/T31L10/makeanom.f
      PARAMETER (LN=128,LA=64)
      REAL G(LN,LA),PROFILE(5),SIGMA(5)
      DIMENSION IHD(10)

      DATA SIGMA/0.1,0.3,0.5,0.7,0.9/

      PI=3.141592654
      DO L=1,5
       PROFILE(L)=PI * (1-SIGMA(L))*SIN(PI*(1-SIGMA(L)))
      PRINT*,SIGMA(L),PROFILE(L)
      ENDDO

      OPEN(10,FILE='anom',FORM='UNFORMATTED')
      REWIND(10)

      DO 10 L=1,5
      DO 20 J=1,LA
      DO 30 I=1,LN
      G(I,J)=0.
 30   CONTINUE
 20   CONTINUE

CCCC Asia forcing

      IRI=8
      IRJ=6
      IORIG=165
      IORI=IORIG*90/360 + 1
      DO 52 I=IORI-IRI,IORI+IRI
      DO 52 J=32-IRJ,33+IRJ
      X=REAL(I-IORI)/REAL(IRI)
      Y=(REAL(J)-30.5)/(REAL(IRJ)+0.5)
C-------------------------------------------------------------
      RR=SQRT(X**2. + Y**2.)
C*****FOR HEATING ANOMALY, AMP IS IN DEGREES PER SECOND,
C*****PROFILE FUNCTION INTEGRATES TO 1 IN SIGMA. THEREFORE
C*****DEGSPD=VERTICALLY AVERAGED HEATING RATE IN DEGREES PER DAY
C     DEGSPD=5. * 1.E-4
      DEGSPD=2.
      AMP=PROFILE(L) * DEGSPD/12.
      G(I,J)=(0.5)*AMP*(COS(RR*PI/2.))**2.
      IF (COS(RR*PI/2.).LT.0.) G(I,J)=0.
 52   CONTINUE




      WRITE(10)G

 10   CONTINUE

      STOP
      END

