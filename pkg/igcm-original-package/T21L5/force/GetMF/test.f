
      PROGRAM GETFORCE
C*****compile with -r8
      PARAMETER (NWJ2=121,NHEM=2,NL=5,
     &IGA=NWJ2*NHEM,IGB=NL*IGA)
      COMPLEX Z(IGB),D(IGB),T(IGB),SP(IGA)
      COMPLEX ZA(IGB),DA(IGB),TA(IGB),SPA(IGA)
      COMPLEX Z0(IGB),D0(IGB),T0(IGB),SP0(IGA)
      COMPLEX ZF(IGB),DF(IGB),TF(IGB),SPF(IGA)


      OPEN(11,FILE=
     &'/mnt/climate/data/loach/jiaxj/Data/SGCM/Input/T21L5/'//
     &'UVTP4849after.dat.spec_corrected',
     & form='UNFORMATTED',status='old')


	do kk=1,100
        READ(11)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RLTAPE
	end do

        READ(11)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RLTAPE

	DAY=real(1)
        WRITE(13)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RLTAPE



      STOP
      END               
