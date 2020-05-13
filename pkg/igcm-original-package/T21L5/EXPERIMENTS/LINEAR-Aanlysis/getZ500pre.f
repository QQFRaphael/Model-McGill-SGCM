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

      IREC=0
 100  CONTINUE
      READ(10)RKOUNT,RMTAPE,DAY
      IF (LTANGENT) READ(9)
      IF (DAY-BEGDAY.LT.-0.001) THEN
      IREC=IREC+1
      GO TO 100
      END IF

      IF (LTANGENT) REWIND(9)
      REWIND(10)
      DO 110 I=1,IREC
      IF (LTANGENT) READ(9)
 110  READ(10)

 120  CONTINUE
      READ(10)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RLTAPE
      IF (LTANGENT) 
     &READ(9)RKOUNT,RMTAPE,DAY,ZC,DC,TC,SPC,RLTAPE
C     DO IDDD=2,181
C     READ(10)
C     ENDDO
C     READ(10)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RLTAPE
      PRINT*,'READING ',RKOUNT,DAY
      IF (DAY-ENDDAY.GT.0.001) STOP

C     PRINT*,'RKOUNT = ',RKOUNT
C     PRINT*,'RMTAPE = ',RMTAPE
C     PRINT*,'DAY = ',DAY
C     PRINT*,'RLTAPE = ',RLTAPE
C     PRINT*,'Z(1),Z(IGB) = ',Z(1),' ',Z(IGB)
C     PRINT*,'D(1),D(IGB) = ',D(1),' ',D(IGB)
C     PRINT*,'T(1),T(IGB) = ',T(1),' ',T(IGB)
C     PRINT*,'SP(1),SP(IGA) = ',SP(1),' ',SP(IGA)
 
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
      RKOUNT1=RKOUNT
      RMTAPE1=RMTAPE
      RLTAPE1=RLTAPE
 
      WRITE(11)RKOUNT1,1.,DAY1,Z1,D1,T1,SP1,1.
      IF (ABS(DAY-ENDDAY).LT.0.001) STOP

      IF (ISTEP.GT.1) THEN
      DO I=1,ISTEP-1
      READ(10)RKOUNT,RMTAPE,DAY
      IF (LTANGENT) READ(9)
C     PRINT*,'SKIPPING ',RKOUNT,DAY
      ENDDO
      ENDIF

      GO TO 120 

      END
