CC now changed to get 90 day average---

      PROGRAM CHECKHST
      PARAMETER (NWJ2=121,NHEM=2,NL=5,
     &IGA=NWJ2*NHEM,IGB=NL*IGA)
      COMPLEX*16 Z(IGB),D(IGB),T(IGB),SP(IGA)
      REAL*8 RKOUNT,RMTAPE,DAY,RLTAPE

CC---

        OPEN(10,FILE='fort.10',
     &form='UNFORMATTED',status='old')
      OPEN(11,FILE='fort.11',
     &form='UNFORMATTED',access='append')
CC----------------------
        do NY=1,30

      READ(10)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RLTAPE
	DAY=real(NY)
      WRITE(11)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RLTAPE
        end do

      STOP
      END
