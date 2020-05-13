      PROGRAM MAKEANOM
      PARAMETER (LN=128,LA=64)
      REAL G(LN,LA),PROFILE(10),SIGMA(10)
      DIMENSION IHD(10)

      DATA SIGMA/0.05,0.15,0.25,0.35,0.45,0.55,0.65,0.75,0.85,0.95/

      PI=3.141592654
      DO L=1,10
      PROFILE(L)=PI * (1-SIGMA(L))*SIN(PI*(1-SIGMA(L)))
C     PROFILE(L)=9. * SIGMA(L)**8.
c      PROFILE(L)=5. * SIGMA(L)**4.
C     PROFILE(L)=11.35 * SIGMA(L)**4. *SIN(PI*SIGMA(L))
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

C*****localised source in the sahara
c      IRI=3
c      IRJ=2
c      IORI=8
c      IORJ=24
c      DO 50 I=IORI-IRI,IORI+IRI
c      DO 50 J=IORJ-IRJ,IORJ+IRJ
c      X=REAL(I-IORI)/REAL(IRI)
c      Y=REAL(J-IORJ)/REAL(IRJ)

C*****tropical source on dateline:
c      IRI=14
c      IRJ=4
c      IORIG=180
c      IORI=IORIG*128/360 + 1
c      DO 50 I=IORI-IRI,IORI+IRI
c      DO 50 J=32-IRJ,33+IRJ
c      X=REAL(I-IORI)/REAL(IRI)
c      Y=(REAL(J)-32.5)/(REAL(IRJ)+0.5)

C*****midlat source at 160E 40N (39.375N actually)
C     IRI=14
C     IRJ=7
C     IORI=58
C     IORJ=18
C     DO 50 I=IORI-IRI,IORI+IRI
C     DO 50 J=IORJ-IRJ,IORJ+IRJ
C     X=REAL(I-IORI)/REAL(IRI)
C     Y=REAL(J-IORJ)/REAL(IRJ)

C*****midlat source at 160E 33.75N
C     IRI=14
C     IRJ=7
C     IORI=58
C     IORJ=20
C     DO 50 I=IORI-IRI,IORI+IRI
C     DO 50 J=IORJ-IRJ,IORJ+IRJ
C     X=REAL(I-IORI)/REAL(IRI)
C     Y=REAL(J-IORJ)/REAL(IRJ)

C*****midlat source at 160E 28.125N
C     IRI=14
C     IRJ=7
C     IORI=58
C     IORJ=22
C     DO 50 I=IORI-IRI,IORI+IRI
C     DO 50 J=IORJ-IRJ,IORJ+IRJ
C     X=REAL(I-IORI)/REAL(IRI)
C     Y=REAL(J-IORJ)/REAL(IRJ)

C*****midlat source at 0E 28.125N  ---> data.fanMJO
c      IRI=14
c      IRJ=7
c      IORI=1
c      IORJ=22
c      DO 50 I=IORI-IRI,IORI+IRI
c      DO 50 J=IORJ-IRJ,IORJ+IRJ
c      X=REAL(I-IORI)/REAL(IRI)
c      Y=REAL(J-IORJ)/REAL(IRJ)

C*****midlat source at 30W 28.125N  ---> data.fanMJO1
c      IRI=14
c      IRJ=7
c      IORI=118
c      IORJ=22
c      DO 50 I=IORI-IRI,IORI+IRI
c      DO 50 J=IORJ-IRJ,IORJ+IRJ
c      X=REAL(I-IORI)/REAL(IRI)
c      Y=REAL(J-IORJ)/REAL(IRJ)

C*****midlat source on dateline, 28.125N---> data.fanMJO2
      IRI=14
      IRJ=7
      IORIG=180
      IORI=IORIG*128/360 + 1
      IORJ=22
      DO 50 I=IORI-IRI,IORI+IRI
      DO 50 J=IORJ-IRJ,IORJ+IRJ
      X=REAL(I-IORI)/REAL(IRI)
      Y=REAL(J-IORJ)/REAL(IRJ)


C*****midlat source at 180E 40N
C     IRI=14
C     IRJ=7
C     IORIG=180
C     IORI=IORIG*128/360 + 1
C     IORJ=18
C     DO 50 I=IORI-IRI,IORI+IRI
C     DO 50 J=IORJ-IRJ,IORJ+IRJ
C     X=REAL(I-IORI)/REAL(IRI)
C     Y=REAL(J-IORJ)/REAL(IRJ)

C*****midlat source at 200E 40N
C     IRI=14
C     IRJ=7
C     IORI=72
C     IORJ=18
C     DO 50 I=IORI-IRI,IORI+IRI
C     DO 50 J=IORJ-IRJ,IORJ+IRJ
C     X=REAL(I-IORI)/REAL(IRI)
C     Y=REAL(J-IORJ)/REAL(IRJ)

C-------------------------------------------------------------

      RR=SQRT(X**2. + Y**2.)
C*****FOR HEATING ANOMALY, AMP IS IN DEGREES PER SECOND, 
C*****PROFILE FUNCTION INTEGRATES TO 1 IN SIGMA. THEREFORE
C*****DEGSPD=VERTICALLY AVERAGED HEATING RATE IN DEGREES PER DAY
C     DEGSPD=5. * 1.E-4
      DEGSPD=5.
C     DEGSPD=2.5
      AMP=PROFILE(NREC-20) * DEGSPD / 86400.
      G(I,J)=AMP*(COS(RR*PI/2.))**2.
      IF (COS(RR*PI/2.).LT.0.) G(I,J)=0.
 50   CONTINUE

C*****SPECIFY SECOND HEATING AREA - 
C*****IF RUNNING A NHEM MODEL WITH NON-EQUATORIAL ANOMALY, 
C*****REMEMBER YOU NEED EITHER A MIRROR IMAGE IN THE S. 
C*****HEMISPHERE OR JUST TO MULTIPLY THE N.HEM ANOMALY BY TWO.

C     DO 60 I=IORI-IRI,IORI+IRI
C     DO 60 J=47-IRJ,47+IRJ
C     X=REAL(I-IORI)/REAL(IRI)
C     Y=REAL(J-47)/REAL(IRJ)
C     RR=SQRT(X**2. + Y**2.)
C     G(I,J)=AMP*(COS(RR*PI/2.))**2.
C     IF (COS(RR*PI/2.).LT.0.) G(I,J)=0.
C60   CONTINUE

      END IF

      DO 40 I=1,10
 40   IHD(I)=0

      WRITE(10)IHD
      WRITE(10)G

 10   CONTINUE

      STOP
      END
