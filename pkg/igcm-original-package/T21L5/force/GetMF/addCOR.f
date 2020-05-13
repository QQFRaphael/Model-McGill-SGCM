      PROGRAM CHECKHST
C*****compile with -r8
      PARAMETER (NWJ2=121,NHEM=2,NL=5,
     &IGA=NWJ2*NHEM,IGB=NL*IGA)
      COMPLEX Z(IGB),D(IGB),T(IGB),SP(IGA),
     &COR(IGA),CORM(IGA)

      OPEN(10,FILE='data.cor',
     &form='UNFORMATTED')
      OPEN(13,FILE='data.cor.ave',
     &form='UNFORMATTED')
      OPEN(11,FILE='OtoMUVTP4849after.dat.spec',
     &form='UNFORMATTED')
      OPEN(12,FILE='OtoM.dat',
     &form='UNFORMATTED')

      REWIND(10)
      REWIND(11)
      REWIND(12)
      REWIND(13)
Cjxj
      NRECS=3*182

      DO I=1,IGA
      CORM(I)=0.
      ENDDO

      PRINT*,'first pass through data'

      DO 10 NREC=1,NRECS
      READ(10)COR

      DO I=1,IGA
      CORM(I)=CORM(I) + COR(I)/REAL(NRECS)
      ENDDO

 10   CONTINUE

      WRITE(13)CORM

      PRINT*,'second pass through data'

      DO 20 NREC=1,NRECS
      READ(11)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RLTAPE
      PRINT*,RKOUNT,RMTAPE,DAY,RLTAPE

      DO L=1,NL
      DO I=1,IGA
      II=(L-1)*IGA + I
      D(II)=D(II) - CORM(I)
      ENDDO
      ENDDO

      RKOUNT=0.
      DAY=0.

      WRITE(12)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RMTAPE
 20   CONTINUE

      STOP
      END
