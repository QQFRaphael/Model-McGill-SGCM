CC now changed to get 90 day average---

      PROGRAM CHECKHST
      PARAMETER (NWJ2=256,NHEM=2,NL=10,
     &IGA=NWJ2*NHEM,IGB=NL*IGA)
      COMPLEX*16 Z(IGB),D(IGB),T(IGB),SP(IGA)
      COMPLEX*16 Z0(IGB),D0(IGB),T0(IGB),SP0(IGA)
      COMPLEX*16 ZA(IGB),DA(IGB),TA(IGB),SPA(IGA)
      REAL*8 RKOUNT,RMTAPE,DAY,RLTAPE

CC---

        OPEN(10,FILE='fort.10',
     &form='UNFORMATTED',status='old')
      OPEN(11,FILE='fort.11',
     &form='UNFORMATTED',access='append')
CC----------------------


      do kk=1,6
      READ(10)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RLTAPE
      DAY=real(kk)
      WRITE(11)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RLTAPE
      end do

      STOP
      END
