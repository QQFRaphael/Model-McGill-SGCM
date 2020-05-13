      PROGRAM CHECKHST
      PARAMETER (NWJ2=121,NHEM=2,NL=5,
     &IGA=NWJ2*NHEM,IGB=NL*IGA,nn=1)
      COMPLEX*16 Z(IGB),D(IGB),T(IGB),SP(IGA)
      COMPLEX*16 ZA(IGB),DA(IGB),TA(IGB),SPA(IGA)
        REAL*8 RKOUNT,RMTAPE,DAY,RLTAPE


        open(17,file='OUTpred_per.dat'
     &          ,form='unformatted')

       open(21,
     &    file='OUTpred_inc.dat',
     &    form='unformatted',status='old')


        READ(21)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RLTAPE
	do NY=1,nn
        READ(21)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RLTAPE
        DAY=REAL(NY)
        RKOUNT=0.
        WRITE(17)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RMTAPE
	end do



	stop
	end
