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

CCC 13 is the original initial condition, 11 is the climatology.

        OPEN(13,
     & FILE='/zemo2/jiaxj/Internal/Prediction/Data/fort.36')
	
	do kk=1,mp
	read(13,*) nn
	end do
	mm=nn-10
	print*, mm

        OPEN(12,
     & FILE='/zemo2/jiaxj/Internal/Prediction/Data/OUTpred_total.dat',
     &form='UNFORMATTED')
        DO I=1,IGB
      ZA(I)=0.
      DA(I)=0.
      TA(I)=0.
      ENDDO
      DO I=1,IGA
      SPA(I)=0.
      ENDDO

	 do kk=1,mm
         READ(12)
	 end do

	 do kk=1,5
	 READ(12)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RMTAPE
        DO I=1,IGB
      ZA(I)=ZA(I)+Z(I)/5.
      DA(I)=DA(I)+D(I)/5.
      TA(I)=TA(I)+T(I)/5.
      ENDDO
      DO I=1,IGA
      SPA(I)=SPA(I)+SP(I)/5.
      ENDDO
	end do


      RKOUNT=0.
      DAY=0.
        WRITE(10)RKOUNT,RMTAPE,DAY,ZA,DA,TA,SPA,RMTAPE


      STOP
      END
