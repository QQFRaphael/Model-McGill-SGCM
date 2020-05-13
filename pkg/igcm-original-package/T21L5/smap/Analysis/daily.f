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
      OPEN(11,FILE='fort.11',
     &form='UNFORMATTED',access='append')

cc convert from character to integer--
        read(cy,'(i3)')iy

CC skip the first 30 days
      DO NR=1,30
      READ(10)
      end do
CC----------------------
        do NY=1,3*30
         NM=90*(iy-1)+NY

      READ(10)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RLTAPE
        DAY=REAL(NM)
        RKOUNT=0.
        WRITE(11)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RMTAPE

        end do

      STOP
      END
