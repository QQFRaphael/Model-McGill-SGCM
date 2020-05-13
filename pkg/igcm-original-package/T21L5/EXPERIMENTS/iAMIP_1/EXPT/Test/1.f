CC now changed to get 90 day average---

      PROGRAM CHECKHST
      PARAMETER (NWJ2=121,NHEM=2,NL=5,
     &IGA=NWJ2*NHEM,IGB=NL*IGA)
      COMPLEX*16 Z(IGB),D(IGB),T(IGB),SP(IGA)
      COMPLEX*16 ZA(IGB),DA(IGB),TA(IGB),SPA(IGA)
        REAL*8 RKOUNT,RMTAPE,DAY,RLTAPE

        OPEN(10,FILE='./exp28/OUTpred.monthly',
     &form='UNFORMATTED',status='old')
      OPEN(11,FILE='tem.dat',
     &form='UNFORMATTED')


CC skip the first 30 days
      DO NR=1,30
      READ(10)
      end do
CC----------------------
	iy=1
        do NY=1,1
        NM=1*(iy-1)+NY

      DO I=1,IGB
      ZA(I)=0.
      DA(I)=0.
      TA(I)=0.
      ENDDO
      DO I=1,IGA
      SPA(I)=0.
      ENDDO

      DO 10 NR=1,30
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
      ZA(I)=ZA(I)/REAL(30)
      DA(I)=DA(I)/REAL(30)
      TA(I)=TA(I)/REAL(30)
      ENDDO
      DO I=1,IGA
      SPA(I)=SPA(I)/REAL(30)
      ENDDO

        DAY=REAL(NM)
        RKOUNT=0.
      WRITE(11)RKOUNT,RMTAPE,DAY,ZA,DA,TA,SPA,RMTAPE

       PRINT*,'NM= ',NM
        end do

      STOP
      END
