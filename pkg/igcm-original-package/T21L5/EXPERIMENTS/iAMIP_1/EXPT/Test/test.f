CC now changed to get 90 day average---

      PROGRAM CHECKHST
      PARAMETER (NWJ2=121,NHEM=2,NL=5,
     &IGA=NWJ2*NHEM,IGB=NL*IGA)
      COMPLEX*16 Z(IGB),D(IGB),T(IGB),SP(IGA)
      COMPLEX*16 ZA(IGB),DA(IGB),TA(IGB),SPA(IGA)
        REAL*8 RKOUNT,RMTAPE,DAY,RLTAPE
        character*3 cy

CC---
        call getarg(1,cy)

        OPEN(10,FILE='fort.10',
     &form='UNFORMATTED',status='old')

cc convert from character to integer--
        read(cy,'(i3)')iy


      STOP
      END
