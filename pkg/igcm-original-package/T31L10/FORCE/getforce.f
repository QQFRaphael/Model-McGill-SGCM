
      PROGRAM GETFORCE
C*****compile with -r8
      PARAMETER (NWJ2=256,NHEM=2,NL=10,
     &IGA=NWJ2*NHEM,IGB=NL*IGA)
      COMPLEX Z(IGB),D(IGB),T(IGB),SP(IGA)
      COMPLEX ZA(IGB),DA(IGB),TA(IGB),SPA(IGA)
      COMPLEX Z0(IGB),D0(IGB),T0(IGB),SP0(IGA)
      COMPLEX ZF(IGB),DF(IGB),TF(IGB),SPF(IGA)


      OPEN(10,FILE='/diska/hlin/igcm/T31L10/STEP1/restart.12',
     & form='UNFORMATTED',status='old')
      OPEN(11,
     7FILE='/zemo2/hlin/igcm/T31L10/data/'//
     & 'ZDTPdjf.dat.spec_corrected',
     & form='UNFORMATTED',status='old')
      OPEN(13,FILE='FORCE_54DJF.dat_crtd',form='UNFORMATTED') 

      PI=3.14159265359
      DELT2=4.*PI/64.
      NRECS=90

	do iyr=1,54
	print*,iyr

CC 90 day average of 1-step state--
      DO I=1,IGB
      ZA(I)=0.
      DA(I)=0.
      TA(I)=0.
      ENDDO
      DO I=1,IGA
      SPA(I)=0.
      ENDDO

      DO 10 NREC=1,NRECS
      READ(10)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RLTAPE
c      PRINT*,RKOUNT,RMTAPE,DAY,RLTAPE

      DO I=1,IGB
      ZA(I)=ZA(I)+Z(I)/REAL(NRECS)
      DA(I)=DA(I)+D(I)/REAL(NRECS)
      TA(I)=TA(I)+T(I)/REAL(NRECS)
      ENDDO
      DO I=1,IGA
      SPA(I)=SPA(I)+SP(I)/REAL(NRECS)
      ENDDO

 10   CONTINUE

CC 90-day average of initial condition--
      DO I=1,IGB
      Z0(I)=0.
      D0(I)=0.
      T0(I)=0.
      ENDDO
      DO I=1,IGA
      SP0(I)=0.
      ENDDO

      DO 11 NREC=1,NRECS
      READ(11)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RLTAPE
c      PRINT*,RKOUNT,RMTAPE,DAY,RLTAPE

      DO I=1,IGB
      Z0(I)=Z0(I)+Z(I)/REAL(NRECS)
      D0(I)=D0(I)+D(I)/REAL(NRECS)
      T0(I)=T0(I)+T(I)/REAL(NRECS)
      ENDDO
      DO I=1,IGA
      SP0(I)=SP0(I)+SP(I)/REAL(NRECS)
      ENDDO

 11   CONTINUE        

CC tendency----
      DO I=1,IGB
      ZF(I)=(-ZA(I)+Z0(I))/DELT2
      DF(I)=(-DA(I)+D0(I))/DELT2
      TF(I)=(-TA(I)+T0(I))/DELT2
      ENDDO
      DO I=1,IGA
      SPF(I)=(-SPA(I)+SP0(I))/DELT2
      ENDDO

      WRITE(13)ZF,DF,TF,SPF  

	end do



      STOP
      END               
