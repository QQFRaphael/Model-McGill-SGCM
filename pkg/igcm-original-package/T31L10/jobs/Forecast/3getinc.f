C
CC get initial confdition
      PARAMETER (NWJ2=121,NHEM=2,NL=5,
     &IGA=NWJ2*NHEM,IGB=NL*IGA)
      COMPLEX*16 Z(IGB),D(IGB),T(IGB),SP(IGA)
      COMPLEX*16 ZA(IGB),DA(IGB),TA(IGB),SPA(IGA)
      COMPLEX*16 ZI(IGB),DI(IGB),TI(IGB),SPI(IGA)
      COMPLEX*16 ZM(IGB),DM(IGB),TM(IGB),SPM(IGA)
	
	
	REAL*8 RKOUNT,RMTAPE,DAY
        character*3 cy
         call getarg(1,cy)
         read(cy,'(i3)') mp
          mm=30+(mp-1)*30
	  print*, mm


CCC 13 is the original initial condition, 11 is the climatology.
        OPEN(11,
     & FILE='/zemo2/jiaxj/Internal/Prediction/Data/OUTpred_clim.dat',
     &form='UNFORMATTED')
         READ(11)RKOUNT,RMTAPE,DAY,ZM,DM,TM,SPM,RMTAPE


        OPEN(12,
     & FILE='/zemo2/jiaxj/Internal/Prediction/Data/OUTpred_total.dat',
     &form='UNFORMATTED')
	 do kk=1,mm
         READ(12)
	 end do
         READ(12)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RMTAPE

        OPEN(13,
     &FILE='OUTpred_inc.dat',form='UNFORMATTED')

	 do k=1,30
         READ(13)
	 end do
         READ(13)RKOUNT,RMTAPE,DAY,ZI,DI,TI,SPI,RMTAPE

        DO I=1,IGB
      ZA(I)=(ZI(I)-ZM(I))/8.+Z(I)
      DA(I)=(DI(I)-DM(I))/8.+D(I)
      TA(I)=(TI(I)-TM(I))/8.+T(I)
      ENDDO
      DO I=1,IGA
      SPA(I)=(SPI(I)-SPM(I))/8.+SP(I)
      ENDDO



      RKOUNT=0.
      DAY=0.
        WRITE(10)RKOUNT,RMTAPE,DAY,ZA,DA,TA,SPA,RMTAPE


      STOP
      END
