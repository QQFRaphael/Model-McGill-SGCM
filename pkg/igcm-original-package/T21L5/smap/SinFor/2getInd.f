CC now changed to get 90 day average---

      PROGRAM CHECKHST
      PARAMETER (NWJ2=121,NHEM=2,NL=5,
     &IGA=NWJ2*NHEM,IGB=NL*IGA,nm=120,nn=50)
      COMPLEX*16 Z(IGB),D(IGB),T(IGB),SP(IGA)
      REAL*8 RKOUNT,RMTAPE,DAY,RLTAPE

CC---

        OPEN(10,FILE=
     &  '/mnt/climate/data/loach/jiaxj/Data/SGCM/OutData'//
     &  '/IDEAL/Clim/OUTpred_sum.dat',
     &   form='UNFORMATTED',status='old')
        OPEN(11,FILE=
     &  '/mnt/climate/data/loach/jiaxj/Data/SGCM/OutData'//
     &  '/IDEAL/Clim/OUTpred_ind.dat',
     &   form='UNFORMATTED')
CC----------------------

	do kk=1,nn
	 print*, kk
        do iy=1,30
        READ(10)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RLTAPE
	enddo
        do iy=1,90
        nday=iy+(kk-1)*90
        READ(10)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RLTAPE
	DAY=real(nday)
	WRITE(11)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RLTAPE
	enddo

	enddo



      STOP
      END
