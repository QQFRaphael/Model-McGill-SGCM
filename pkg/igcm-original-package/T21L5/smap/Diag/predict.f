CC now changed to get 90 day average---

      PROGRAM CHECKHST
      PARAMETER (NWJ2=121,NHEM=2,NL=5,
     &IGA=NWJ2*NHEM,IGB=NL*IGA)
      COMPLEX*16 Z(IGB),D(IGB),T(IGB),SP(IGA)
      REAL*8 RKOUNT,RMTAPE,DAY,RLTAPE
        character*3 cy
CC---
CC---
        call getarg(1,cy)
CC convert from character to integer--
        read(cy,'(i3)')iy




        OPEN(10,FILE='fort.10',
     &form='UNFORMATTED',status='old')
      OPEN(11,FILE='fort.11',
     &form='UNFORMATTED',access='append')
CC----------------------

CC skip the first 30 days
        do NY=1,30
        READ(10)
        end do



        do NY=1,182
        READ(10)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RLTAPE
	NM=182*(iy-1)+NY
	DAY=REAL(NM)
        WRITE(11)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RLTAPE
        end do



      STOP
      END
