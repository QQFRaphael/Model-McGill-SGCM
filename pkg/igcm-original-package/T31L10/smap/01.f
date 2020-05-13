      PROGRAM CHECKHST
      PARAMETER (NWJ2=256,NHEM=2,NL=10,IGA=NWJ2*NHEM,IGB=NL*IGA)
      REAL*8 RKOUNT,RMTAPE,DAY,RLTAPE
      COMPLEX*16 Z(IGB),D(IGB),T(IGB),SP(IGA)
      COMPLEX*16 ZA(IGB),DA(IGB),TA(IGB),SPA(IGA)


        OPEN(10,FILE=
     &'history_30',
     &form='UNFORMATTED',status='old')
      OPEN(11,
     & FILE='daily1.dat',
     & form='UNFORMATTED')



        do NY=1,1
      DO I=1,IGB
      ZA(I)=0.
      DA(I)=0.
      TA(I)=0.
      ENDDO
      DO I=1,IGA
      SPA(I)=0.
      ENDDO


      DO 10 NR=1,1
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
      ZA(I)=ZA(I)/REAL(1)
      DA(I)=DA(I)/REAL(1)
      TA(I)=TA(I)/REAL(1)
      ENDDO
      DO I=1,IGA
      SPA(I)=SPA(I)/REAL(1)
      ENDDO

        DAY=REAL(NY)
        RKOUNT=0.
        WRITE(11)RKOUNT,RMTAPE,DAY,ZA,DA,TA,SPA,RMTAPE

	end do


	stop
      END
