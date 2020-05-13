CC now changed to get 90 day average---

      PROGRAM CHECKHST
      PARAMETER (NWJ2=121,NHEM=2,NL=5,
     &IGA=NWJ2*NHEM,IGB=NL*IGA,nm=90*50,nn=50)
      COMPLEX*16 Z(IGB),D(IGB),T(IGB),SP(IGA)
      COMPLEX*16 ZA(IGB),DA(IGB),TA(IGB),SPA(IGA)
      COMPLEX*16 Zt(IGB,nm),Dt(IGB,nm),Tt(IGB,nm),SPt(IGA,nm)
      REAL*8 RKOUNT,RMTAPE,DAY,RLTAPE

CC---

        OPEN(10,FILE='/mnt/climate/data/loach/jiaxj/Data/'//
     &  'SGCM/OutData/IDEAL/145W/OUTpred_sum.dat',
     &   form='UNFORMATTED',status='old')
        OPEN(11,FILE='/mnt/climate/data/loach/jiaxj/Data/SGCM'//
     &   '/OutData/IDEAL/145W/OUTpred_sum_90.dat',
     &   form='UNFORMATTED')
CC----------------------

        do iy=1,90
        do I=1,IGB
        Zt(I,iy)=0.
        Dt(I,iy)=0.
        Tt(I,iy)=0.
        ENDDO
        DO I=1,IGA
        SPt(I,iy)=0.
        ENDDO
        end do



        do iy=1,nm
	 print*, iy
        READ(10)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RLTAPE
	DAY=real(iy)
	WRITE(11)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RLTAPE
	END DO


      STOP
      END
