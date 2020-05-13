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
	character*100 infile
        call getarg(1,cy)
     	infile='/mnt/climate/data/loach/jiaxj/Data/SGCM/OutData/AONAO/Prediction/NewdampData/OUTpred_total.dat'	

         read(cy,'(i3)') mp

CCC 13 is the original initial condition, 11 is the climatology.
        OPEN(11,
     & FILE='OUTpred_con.dat',form='UNFORMATTED')
         READ(11)RKOUNT,RMTAPE,DAY,ZM,DM,TM,SPM,RMTAPE

        OPEN(13,
     & FILE='/zemo2/jiaxj/Internal/Prediction/NewdampData/fort.35')
	
	do kk=1,mp
	read(13,*) nn
	end do
	mm=nn-28 
	print*, mm

        OPEN(12,FILE=infile, form='UNFORMATTED')
	 do kk=1,mm
         READ(12)
	 end do

         READ(12)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RMTAPE

        open(4,file='seed.dat')
         read(4,*)seed

         DO I=1,IGB
          pp=(ran(seed)-0.5)*(0.5)
          rewind(4)
          write(4,*)seed
          ZA(I)=ZM(I)*pp+Z(I)
          DA(I)=DM(I)*pp+D(I)
          TA(I)=TM(I)*pp+T(I)
         ENDDO
         DO I=1,IGA
          pp=(ran(seed)-0.5)*(0.5)
          rewind(4)
          write(4,*)seed
          SPA(I)=SPM(I)*pp+SP(I)
         ENDDO

      RKOUNT=0.
      DAY=0.
        WRITE(10)RKOUNT,RMTAPE,DAY,ZA,DA,TA,SPA,RMTAPE


      STOP
      END
