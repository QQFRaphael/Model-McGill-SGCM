CC now changed to get 90 day average---

      PROGRAM CHECKHST
      PARAMETER (NWJ2=121,NHEM=2,NL=5,
     &IGA=NWJ2*NHEM,IGB=NL*IGA)
      COMPLEX*16 Z(IGB),D(IGB),T(IGB),SP(IGA)
      COMPLEX*16 ZA(IGB),DA(IGB),TA(IGB),SPA(IGA)
        REAL*8 RKOUNT,RMTAPE,DAY,RLTAPE
        character*2 cy

CC---

      OPEN(10,FILE='OUTpred.monthly',
     &form='UNFORMATTED')
      OPEN(11,FILE='ASCII',status='old')

	do kk=1,306

      READ(11,*)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RLTAPE
      WRITE(10)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RLTAPE

	end do


      STOP
      END
