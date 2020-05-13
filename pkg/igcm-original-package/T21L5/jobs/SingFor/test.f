 3C
CC get initial confdition
      PARAMETER (NWJ2=121,NHEM=2,NL=5,
     &IGA=NWJ2*NHEM,IGB=NL*IGA)
      COMPLEX*16 Z(IGB),D(IGB),T(IGB),SP(IGA)
      COMPLEX*16 ZA(IGB),DA(IGB),TA(IGB),SPA(IGA)
      COMPLEX*16 ZI(IGB),DI(IGB),TI(IGB),SPI(IGA)
      COMPLEX*16 ZM(IGB),DM(IGB),TM(IGB),SPM(IGA)
	
	
	REAL*8 RKOUNT,RMTAPE,DAY
	character*80 outfile
        character*3 cy

         call getarg(1,cy)
         read(cy,'(i3)') mp

	 print*, mp


CCC 13 is the original initial condition, 11 is the climatology.
	outfile='/zemo2/jiaxj/Internal/Prediction/Test/OUTpred_inc.dat'
      OPEN(13,FILE=
     &  outfile,form='UNFORMATTED',status='old')

        OPEN(12,
     & FILE='/zemo2/jiaxj/Internal/Prediction/Test/OUTpred_total.dat',
     &form='UNFORMATTED')
        OPEN(11,
     & FILE='/zemo2/jiaxj/Internal/Prediction/Test/OUTpred_clim.dat',
     &form='UNFORMATTED')
         READ(11)RKOUNT,RMTAPE,DAY,ZM,DM,TM,SPM,RMTAPE

        do kk=1,mp
	 READ(13)RKOUNT,RMTAPE,DAY,ZI,DI,TI,SPI,RMTAPE
         READ(12)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RMTAPE
	end do

        DO I=1,IGB
      ZA(I)=(Z(I)-ZM(I))/10.+ZI(I)
      DA(I)=(D(I)-DM(I))/10.+DI(I)
      TA(I)=(T(I)-TM(I))/10.+TI(I)
      ENDDO
      DO I=1,IGA
      SPA(I)=(SP(I)-SPM(I))/10.+SPI(I)
      ENDDO

CCCCCCCCCCCC	
      RKOUNT=0.
      DAY=0.
        WRITE(10)RKOUNT,RMTAPE,DAY,ZA,DA,TA,SPA,RMTAPE
	


      STOP
      END
