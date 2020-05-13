      PROGRAM CHECKHST
      PARAMETER (NWJ2=121,NHEM=2,NL=5,
     &IGA=NWJ2*NHEM,IGB=NL*IGA)
      COMPLEX*16 Z(IGB),D(IGB),T(IGB),SP(IGA)
      COMPLEX*16 ZA(IGB),DA(IGB),TA(IGB),SPA(IGA)
        REAL*8 RKOUNT,RMTAPE,DAY,RLTAPE

        character*1 enum1
        character*2 enum2


        open(17,file='OUTpred_cont.dat'
     &          ,form='unformatted')


       open(21, file=
     &'/zemo2/jiaxj/Internal/Prediction/NewdampData/OUTpred_control.dat
     &',  form='unformatted',status='old')


	do NR=1,31
	 read(21)
	end do
	
	do NY=1,60

        READ(21)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RLTAPE
        DAY=REAL(NY)
        RKOUNT=0.
        WRITE(17)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RMTAPE

	end do



	stop
	end
