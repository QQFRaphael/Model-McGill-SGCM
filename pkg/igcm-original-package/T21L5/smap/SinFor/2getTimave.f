CC now changed to get 90 day average---

      PROGRAM CHECKHST
      PARAMETER (NWJ2=121,NHEM=2,NL=5,
     &IGA=NWJ2*NHEM,IGB=NL*IGA,nm=90,nn=50)
      COMPLEX*16 Z(IGB),D(IGB),T(IGB),SP(IGA)
      COMPLEX*16 ZA(IGB),DA(IGB),TA(IGB),SPA(IGA)
      COMPLEX*16 Zt(IGB),Dt(IGB),Tt(IGB),SPt(IGA)
      REAL*8 RKOUNT,RMTAPE,DAY,RLTAPE

CC---

        OPEN(10,FILE='/mnt/climate/data/loach/jiaxj/Data/'//
     &  'SGCM/OutData/IDEAL/145W/OUTpred_sum.dat',
     &   form='UNFORMATTED',status='old')
        OPEN(11,FILE='/mnt/climate/data/loach/jiaxj/Data/SGCM'//
     &   '/OutData/IDEAL/145W/OUTpred_sum_90_Timeave.dat',
     &   form='UNFORMATTED')
CC----------------------

        do I=1,IGB
        Zt(I)=0.
        Dt(I)=0.
        Tt(I)=0.
        ENDDO
        DO I=1,IGA
        SPt(I)=0.
        ENDDO

	do kk=1,nn

        do iy=1,nm
        READ(10)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RLTAPE
        do I=1,IGB
        Zt(I)=Z(I)+Zt(I)/real(nm)
        Dt(I)=D(I)+Dt(I)/real(nm)
        Tt(I)=T(I)+Tt(I)/real(nm)
        ENDDO
        DO I=1,IGA
        SPt(I)=SP(I)+SPt(I)/real(nm)
        ENDDO
        end do
	DAY=real(kk)
	WRITE(11)RKOUNT,RMTAPE,DAY,ZA,DA,TA,SPA,RLTAPE
	end do


      STOP
      END
