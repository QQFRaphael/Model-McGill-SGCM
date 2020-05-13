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
cc convert from character to integer--
        read(cy,'(i3)')iy


        OPEN(10,FILE='fort.10',
     &form='UNFORMATTED',status='old')
      OPEN(11,FILE='fort.11',
     &form='UNFORMATTED',access='append')
CC----------------------
      DO NR=1,30
      READ(10)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RLTAPE
      END DO	

      DO I=1,IGB
      ZA(I)=0.
      DA(I)=0.
      TA(I)=0.
      ENDDO
      DO I=1,IGA
      SPA(I)=0.
      ENDDO

      DO 10 NR=1,90
      READ(10)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RLTAPE
      DO I=1,IGB
      ZA(I)=ZA(I)+Z(I)
      DA(I)=DA(I)+D(I)
      TA(I)=TA(I)+T(I)
      ENDDO
      DO I=1,IGA
      SPA(I)=SPA(I)+SP(I)
      ENDDO
 10   CONTINUE

      DO I=1,IGB
      ZA(I)=ZA(I)/REAL(90)
      DA(I)=DA(I)/REAL(90)
      TA(I)=TA(I)/REAL(90)
      ENDDO
      DO I=1,IGA
      SPA(I)=SPA(I)/REAL(90)
      ENDDO

        DAY=REAL(iy)
        RKOUNT=0.
        WRITE(11)RKOUNT,RMTAPE,DAY,ZA,DA,TA,SPA,RMTAPE
        PRINT*,'NM= ', iy



      STOP
      END
