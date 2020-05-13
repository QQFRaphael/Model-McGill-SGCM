CC now changed to get 90 day average---

      PROGRAM CHECKHST
      PARAMETER (NWJ2=121,NHEM=2,NL=5,
     &IGA=NWJ2*NHEM,IGB=NL*IGA,nm=120,nn=30)
      COMPLEX*16 Z(IGB),D(IGB),T(IGB),SP(IGA)
      COMPLEX*16 ZA(IGB),DA(IGB),TA(IGB),SPA(IGA)
      COMPLEX*16 Zt(IGB,nm),Dt(IGB,nm),Tt(IGB,nm),SPt(IGA,nm)
      REAL*8 RKOUNT,RMTAPE,DAY,RLTAPE

CC---

        OPEN(10,FILE='OUTpred_sum.dat',
     &form='UNFORMATTED',status='old')
      OPEN(11,FILE='Enssum.dat',
     &form='UNFORMATTED')
CC----------------------

	do iy=1,120
        do I=1,IGB
        Zt(I,iy)=0.
        Dt(I,iy)=0.
        Tt(I,iy)=0.
        ENDDO
        DO I=1,IGA
        SPt(I,iy)=0.
        ENDDO
        end do

	do kk=1,nn
        do iy=1,nm
        READ(10)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RLTAPE
        do I=1,IGB
        Zt(I,iy)=Z(I)+Zt(I,iy)
        Dt(I,iy)=D(I)+Dt(I,iy)
        Tt(I,iy)=T(I)+Tt(I,iy)
        ENDDO
        DO I=1,IGA
        SPt(I,iy)=SP(I)+SPt(I,iy)
        ENDDO
        end do
	end do

        do iy=1,nm
        DO I=1,IGB
        Zt(I,iy)=Zt(I,iy)/real(nn)
        Dt(I,iy)=Dt(I,iy)/real(nn)
        Tt(I,iy)=Tt(I,iy)/real(nn)
        ENDDO
        DO I=1,IGA
        SPt(I,iy)=SPt(I,iy)/real(nn)
        ENDDO
        end do


	do iy=1,nm
        do I=1,IGB
         ZA(I)=Zt(I,iy)
         DA(I)=Dt(I,iy)
         TA(I)=Tt(I,iy)
        ENDDO
        DO I=1,IGA
         SPA(I)=SPt(I,iy)
        ENDDO
	
	DAY=real(iy)

	WRITE(11)RKOUNT,RMTAPE,DAY,ZA,DA,TA,SPA,RLTAPE

	END DO


      STOP
      END
