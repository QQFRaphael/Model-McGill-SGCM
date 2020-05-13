      PROGRAM CHECKHST
      PARAMETER (NWJ2=121,NHEM=2,NL=5,IGA=NWJ2*NHEM,IGB=NL*IGA)
      REAL*8 RKOUNT,RMTAPE,DAY,RLTAPE
      REAL RKOUNT1,RMTAPE1,DAY1,RLTAPE1
      COMPLEX*16 Z(IGB),D(IGB),T(IGB),SP(IGA)
      COMPLEX*16 ZC(IGB),DC(IGB),TC(IGB),SPC(IGA),EZ,TADD
      COMPLEX Z1(IGB),D1(IGB),T1(IGB),SP1(IGA)
      LOGICAL LTANGENT
	character*60 inputf,outf

        call getarg(1,inputf)

      OPEN(10,FILE=inputf,
     &FORM='UNFORMATTED')

 
      LTANGENT=.FALSE.
      SCALEUP=-1.
      READ(*,*)BEGDAY
      READ(*,*)ENDDAY
      READ(*,*)ISTEP
      PRINT*,'BEGDAY= ',BEGDAY,' ENDDAY= ',ENDDAY

      OPEN(11,FILE='data.res.sp',FORM='UNFORMATTED',status='new')
      REWIND(10)
      REWIND(11)



      DO K=1,1
      DAY=K*1.	
      READ(10)Z,D,T,SP
      IF (DAY-ENDDAY.GT.0.001) STOP
 
      DO I=1,IGB
      IF (SCALEUP.GT.0.) THEN
      Z1(I)=ZC(I) + (Z(I)-ZC(I))*SCALEUP
      D1(I)=DC(I) + (D(I)-DC(I))*SCALEUP
      T1(I)=TC(I) + (T(I)-TC(I))*SCALEUP
      ELSE
      Z1(I)=Z(I)
      D1(I)=D(I)
      T1(I)=T(I)
      END IF
      ENDDO
      DO I=1,IGA
      IF (SCALEUP.GT.0.) THEN
      SP1(I)=SPC(I) + (SP(I)-SPC(I))*SCALEUP
      ELSE
      SP1(I)=SP(I)
      END IF
      ENDDO
      DAY1=DAY
      RKOUNT1=0.
 
      WRITE(11)RKOUNT1,1.,DAY1,Z1,D1,T1,SP1,1.
      IF (ABS(DAY-ENDDAY).LT.0.001) STOP

      END DO

      END
