      PARAMETER (NWJ2=121,NHEM=2,NL=5,IGA=NWJ2*NHEM,IGB=NL*IGA)
      REAL*8 RKOUNT,RMTAPE,DAY,RLTAPE
      REAL RKOUNT1,RMTAPE1,DAY1,RLTAPE1
      COMPLEX*16 Z(IGB),D(IGB),T(IGB),SP(IGA)
      COMPLEX*16 ZC(IGB),DC(IGB),TC(IGB),SPC(IGA),EZ,TADD
      COMPLEX ZA(IGB),DA(IGB),TA(IGB),SPA(IGA)
	character*60 inputf,outf

	call getarg(1,inputf)
	call getarg(2,outf)

      OPEN(10,FILE=inputf,
     &FORM='UNFORMATTED')

      OPEN(11,FILE=outf,FORM='UNFORMATTED')

 	RKOUNT1=1.
        DAY1=1.
	NRECS=90

	do iy=1,10
	print*,'iy=  ',iy

CC 90*51 day average 
      DO I=1,IGB
      ZA(I)=0.
      DA(I)=0.
      TA(I)=0.
      ENDDO
      DO I=1,IGA
      SPA(I)=0.
      ENDDO

	do NREC=1,NRECS
      READ(10)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RLTAPE
c      PRINT*,'READING ',RKOUNT,DAY
 
      DO I=1,IGB
      ZA(I)=ZA(I)+Z(I)/REAL(NRECS)
      DA(I)=DA(I)+D(I)/REAL(NRECS)
      TA(I)=TA(I)+T(I)/REAL(NRECS)
      ENDDO
      DO I=1,IGA
      SPA(I)=SPA(I)+SP(I)/REAL(NRECS)
      ENDDO
	end do
 
      WRITE(11)RKOUNT1,1.,DAY1,ZA,DA,TA,SPA,1.

	print*, RKOUNT1,1.,DAY1,ZA(1)

	RKOUNT1=RKOUNT1+48
	DAY1=DAY1+1

	end do

      END
