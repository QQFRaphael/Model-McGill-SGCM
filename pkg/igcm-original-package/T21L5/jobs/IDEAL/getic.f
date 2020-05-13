CC get initial confdition
      PARAMETER (NWJ2=121,NHEM=2,NL=5,
     &IGA=NWJ2*NHEM,IGB=NL*IGA)
      COMPLEX*16 Z(IGB),D(IGB),T(IGB),SP(IGA)
      COMPLEX*16 ZA(IGB),DA(IGB),TA(IGB),SPA(IGA)
      COMPLEX*16 ZI(IGB),DI(IGB),TI(IGB),SPI(IGA)
      COMPLEX*16 ZM(IGB),DM(IGB),TM(IGB),SPM(IGA)


        REAL*8 RKOUNT,RMTAPE,DAY

        character*21 outfile
        character*3 year

        call getarg(1,year)

        read(year,*)iyear

        open(4,file='seed.dat')
         read(4,*)iseed
CC

        nd=ran(iseed)*90
        ny=ran(iseed)*50

c       print*,iyear, nm,nc
        rewind(4)
        write(4,*)iseed


        OPEN(12,FILE=
     &'/mnt/climate/data/loach/jiaxj/Data/SGCM/Input/T21L5/'//
     &'UVTP4849after.dat.spec_corrected',
     &form='UNFORMATTED')


       do 22 im=1,(ny-1)*90
         read(12)
22      continue

        print*,iyear,nd,ny
      do 20 iyr=1,nd
        READ(12)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RMTAPE
20    CONTINUE

      DO I=1,IGB
      ZA(I)=Z(I)
      DA(I)=D(I)
      TA(I)=T(I)
      ENDDO
      DO I=1,IGA
      SPA(I)=SP(I)
      ENDDO


      RKOUNT=0.
      DAY=0.
      WRITE(10)RKOUNT,RMTAPE,DAY,ZA,DA,TA,SPA,RMTAPE



      STOP
      END

