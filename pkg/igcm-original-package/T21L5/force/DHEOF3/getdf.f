
      PROGRAM GETFORCE
C*****compile with -r8
      PARAMETER (NWJ2=121,NHEM=2,NL=5,
     &IGA=NWJ2*NHEM,IGB=NL*IGA)
      COMPLEX Z(IGB),D(IGB),T(IGB),SP(IGA)
      COMPLEX ZA(IGB),DA(IGB),TA(IGB),SPA(IGA)
      COMPLEX Z0(IGB),D0(IGB),T0(IGB),SP0(IGA)
      COMPLEX ZF(IGB),DF(IGB),TF(IGB),SPF(IGA)


      OPEN(10,FILE='/zemo2/jiaxj/igcm/DFEOF3/restart.12',
     & form='UNFORMATTED',status='old')

      OPEN(11,
     &FILE='/diskc/jiaxj/force/ZDTP_OtoM.dat.spec_corrected',
     & form='UNFORMATTED',status='old')
      OPEN(13,FILE='DFEOF3_OtoM.dat_crtd',form='UNFORMATTED') 

      PI=3.14159265359
      DELT2=4.*PI/48.
      NRECS=30

Cjxj

	ii=0
	do 200 kkk=1,13
	do 100 kk=1,3

CCC************

CC Oct
CC Monthly average of 1-step state--
      DO I=1,IGB
      ZA(I)=0.
      DA(I)=0.
      TA(I)=0.
      ENDDO
      DO I=1,IGA
      SPA(I)=0.
      ENDDO

      NRECS=31
      DO 201 NREC=1,NRECS
      READ(10)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RLTAPE
C       PRINT*,RKOUNT,RMTAPE,DAY,RLTAPE

      DO I=1,IGB
      ZA(I)=ZA(I)+Z(I)/REAL(NRECS)
      DA(I)=DA(I)+D(I)/REAL(NRECS)
      TA(I)=TA(I)+T(I)/REAL(NRECS)
      ENDDO
      DO I=1,IGA
      SPA(I)=SPA(I)+SP(I)/REAL(NRECS)
      ENDDO

201   CONTINUE

CC Monthly average of initial condition--
      DO I=1,IGB
      Z0(I)=0.
      D0(I)=0.
      T0(I)=0.
      ENDDO
      DO I=1,IGA
      SP0(I)=0.
      ENDDO

      DO 301 NREC=1,NRECS
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

301   CONTINUE        

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

CC

CC Nov
CC Monthly average of 1-step state--
      DO I=1,IGB
      ZA(I)=0.
      DA(I)=0.
      TA(I)=0.
      ENDDO
      DO I=1,IGA
      SPA(I)=0.
      ENDDO

      NRECS=30
      DO 202 NREC=1,NRECS
      READ(10)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RLTAPE
C       PRINT*,RKOUNT,RMTAPE,DAY,RLTAPE

      DO I=1,IGB
      ZA(I)=ZA(I)+Z(I)/REAL(NRECS)
      DA(I)=DA(I)+D(I)/REAL(NRECS)
      TA(I)=TA(I)+T(I)/REAL(NRECS)
      ENDDO
      DO I=1,IGA
      SPA(I)=SPA(I)+SP(I)/REAL(NRECS)
      ENDDO

202   CONTINUE

CC Monthly average of initial condition--
      DO I=1,IGB
      Z0(I)=0.
      D0(I)=0.
      T0(I)=0.
      ENDDO
      DO I=1,IGA
      SP0(I)=0.
      ENDDO

      DO 302 NREC=1,NRECS
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

302   CONTINUE        

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

CC Dec
CC Monthly average of 1-step state--
      DO I=1,IGB
      ZA(I)=0.
      DA(I)=0.
      TA(I)=0.
      ENDDO
      DO I=1,IGA
      SPA(I)=0.
      ENDDO

      NRECS=31
      DO 203 NREC=1,NRECS
      READ(10)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RLTAPE
C       PRINT*,RKOUNT,RMTAPE,DAY,RLTAPE

      DO I=1,IGB
      ZA(I)=ZA(I)+Z(I)/REAL(NRECS)
      DA(I)=DA(I)+D(I)/REAL(NRECS)
      TA(I)=TA(I)+T(I)/REAL(NRECS)
      ENDDO
      DO I=1,IGA
      SPA(I)=SPA(I)+SP(I)/REAL(NRECS)
      ENDDO

203   CONTINUE

CC Monthly average of initial condition--
      DO I=1,IGB
      Z0(I)=0.
      D0(I)=0.
      T0(I)=0.
      ENDDO
      DO I=1,IGA
      SP0(I)=0.
      ENDDO

      DO 303 NREC=1,NRECS
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

303   CONTINUE        

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

CC Jan
CC Monthly average of 1-step state--
      DO I=1,IGB
      ZA(I)=0.
      DA(I)=0.
      TA(I)=0.
      ENDDO
      DO I=1,IGA
      SPA(I)=0.
      ENDDO

      NRECS=31
      DO 204 NREC=1,NRECS
      READ(10)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RLTAPE
C       PRINT*,RKOUNT,RMTAPE,DAY,RLTAPE

      DO I=1,IGB
      ZA(I)=ZA(I)+Z(I)/REAL(NRECS)
      DA(I)=DA(I)+D(I)/REAL(NRECS)
      TA(I)=TA(I)+T(I)/REAL(NRECS)
      ENDDO
      DO I=1,IGA
      SPA(I)=SPA(I)+SP(I)/REAL(NRECS)
      ENDDO

204   CONTINUE

CC Monthly average of initial condition--
      DO I=1,IGB
      Z0(I)=0.
      D0(I)=0.
      T0(I)=0.
      ENDDO
      DO I=1,IGA
      SP0(I)=0.
      ENDDO

      DO 304 NREC=1,NRECS
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

304   CONTINUE        

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


CC Feb
CC Monthly average of 1-step state--
      DO I=1,IGB
      ZA(I)=0.
      DA(I)=0.
      TA(I)=0.
      ENDDO
      DO I=1,IGA
      SPA(I)=0.
      ENDDO

      NRECS=28
      DO 205 NREC=1,NRECS
      READ(10)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RLTAPE
C       PRINT*,RKOUNT,RMTAPE,DAY,RLTAPE

      DO I=1,IGB
      ZA(I)=ZA(I)+Z(I)/REAL(NRECS)
      DA(I)=DA(I)+D(I)/REAL(NRECS)
      TA(I)=TA(I)+T(I)/REAL(NRECS)
      ENDDO
      DO I=1,IGA
      SPA(I)=SPA(I)+SP(I)/REAL(NRECS)
      ENDDO

205   CONTINUE

CC Monthly average of initial condition--
      DO I=1,IGB
      Z0(I)=0.
      D0(I)=0.
      T0(I)=0.
      ENDDO
      DO I=1,IGA
      SP0(I)=0.
      ENDDO

      DO 305 NREC=1,NRECS
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

305   CONTINUE        

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

CC Mar
CC Monthly average of 1-step state--
      DO I=1,IGB
      ZA(I)=0.
      DA(I)=0.
      TA(I)=0.
      ENDDO
      DO I=1,IGA
      SPA(I)=0.
      ENDDO

      NRECS=31
      DO 206 NREC=1,NRECS
      READ(10)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RLTAPE
C       PRINT*,RKOUNT,RMTAPE,DAY,RLTAPE

      DO I=1,IGB
      ZA(I)=ZA(I)+Z(I)/REAL(NRECS)
      DA(I)=DA(I)+D(I)/REAL(NRECS)
      TA(I)=TA(I)+T(I)/REAL(NRECS)
      ENDDO
      DO I=1,IGA
      SPA(I)=SPA(I)+SP(I)/REAL(NRECS)
      ENDDO

206   CONTINUE

CC Monthly average of initial condition--
      DO I=1,IGB
      Z0(I)=0.
      D0(I)=0.
      T0(I)=0.
      ENDDO
      DO I=1,IGA
      SP0(I)=0.
      ENDDO

      DO 306 NREC=1,NRECS
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

306   CONTINUE        

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


100	continue

CCC************
CCC For the even year

CC Oct
CC Monthly average of 1-step state--
      DO I=1,IGB
      ZA(I)=0.
      DA(I)=0.
      TA(I)=0.
      ENDDO
      DO I=1,IGA
      SPA(I)=0.
      ENDDO

      NRECS=31
      DO 401 NREC=1,NRECS
      READ(10)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RLTAPE
C       PRINT*,RKOUNT,RMTAPE,DAY,RLTAPE

      DO I=1,IGB
      ZA(I)=ZA(I)+Z(I)/REAL(NRECS)
      DA(I)=DA(I)+D(I)/REAL(NRECS)
      TA(I)=TA(I)+T(I)/REAL(NRECS)
      ENDDO
      DO I=1,IGA
      SPA(I)=SPA(I)+SP(I)/REAL(NRECS)
      ENDDO

401   CONTINUE

CC Monthly average of initial condition--
      DO I=1,IGB
      Z0(I)=0.
      D0(I)=0.
      T0(I)=0.
      ENDDO
      DO I=1,IGA
      SP0(I)=0.
      ENDDO

      DO 501 NREC=1,NRECS
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

501   CONTINUE        

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

CC

CC Nov
CC Monthly average of 1-step state--
      DO I=1,IGB
      ZA(I)=0.
      DA(I)=0.
      TA(I)=0.
      ENDDO
      DO I=1,IGA
      SPA(I)=0.
      ENDDO

      NRECS=30
      DO 402 NREC=1,NRECS
      READ(10)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RLTAPE
C       PRINT*,RKOUNT,RMTAPE,DAY,RLTAPE

      DO I=1,IGB
      ZA(I)=ZA(I)+Z(I)/REAL(NRECS)
      DA(I)=DA(I)+D(I)/REAL(NRECS)
      TA(I)=TA(I)+T(I)/REAL(NRECS)
      ENDDO
      DO I=1,IGA
      SPA(I)=SPA(I)+SP(I)/REAL(NRECS)
      ENDDO

402   CONTINUE

CC Monthly average of initial condition--
      DO I=1,IGB
      Z0(I)=0.
      D0(I)=0.
      T0(I)=0.
      ENDDO
      DO I=1,IGA
      SP0(I)=0.
      ENDDO

      DO 502 NREC=1,NRECS
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

502   CONTINUE        

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

CC Dec
CC Monthly average of 1-step state--
      DO I=1,IGB
      ZA(I)=0.
      DA(I)=0.
      TA(I)=0.
      ENDDO
      DO I=1,IGA
      SPA(I)=0.
      ENDDO

      NRECS=31
      DO 403 NREC=1,NRECS
      READ(10)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RLTAPE
C       PRINT*,RKOUNT,RMTAPE,DAY,RLTAPE

      DO I=1,IGB
      ZA(I)=ZA(I)+Z(I)/REAL(NRECS)
      DA(I)=DA(I)+D(I)/REAL(NRECS)
      TA(I)=TA(I)+T(I)/REAL(NRECS)
      ENDDO
      DO I=1,IGA
      SPA(I)=SPA(I)+SP(I)/REAL(NRECS)
      ENDDO

403   CONTINUE

CC Monthly average of initial condition--
      DO I=1,IGB
      Z0(I)=0.
      D0(I)=0.
      T0(I)=0.
      ENDDO
      DO I=1,IGA
      SP0(I)=0.
      ENDDO

      DO 503 NREC=1,NRECS
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

503   CONTINUE        

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

CC Jan
CC Monthly average of 1-step state--
      DO I=1,IGB
      ZA(I)=0.
      DA(I)=0.
      TA(I)=0.
      ENDDO
      DO I=1,IGA
      SPA(I)=0.
      ENDDO

      NRECS=31
      DO 404 NREC=1,NRECS
      READ(10)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RLTAPE
C       PRINT*,RKOUNT,RMTAPE,DAY,RLTAPE

      DO I=1,IGB
      ZA(I)=ZA(I)+Z(I)/REAL(NRECS)
      DA(I)=DA(I)+D(I)/REAL(NRECS)
      TA(I)=TA(I)+T(I)/REAL(NRECS)
      ENDDO
      DO I=1,IGA
      SPA(I)=SPA(I)+SP(I)/REAL(NRECS)
      ENDDO

404   CONTINUE

CC Monthly average of initial condition--
      DO I=1,IGB
      Z0(I)=0.
      D0(I)=0.
      T0(I)=0.
      ENDDO
      DO I=1,IGA
      SP0(I)=0.
      ENDDO

      DO 504 NREC=1,NRECS
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

504   CONTINUE        

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


CC Feb
CC Monthly average of 1-step state--
      DO I=1,IGB
      ZA(I)=0.
      DA(I)=0.
      TA(I)=0.
      ENDDO
      DO I=1,IGA
      SPA(I)=0.
      ENDDO

      NRECS=29
      DO 405 NREC=1,NRECS
      READ(10)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RLTAPE
C       PRINT*,RKOUNT,RMTAPE,DAY,RLTAPE

      DO I=1,IGB
      ZA(I)=ZA(I)+Z(I)/REAL(NRECS)
      DA(I)=DA(I)+D(I)/REAL(NRECS)
      TA(I)=TA(I)+T(I)/REAL(NRECS)
      ENDDO
      DO I=1,IGA
      SPA(I)=SPA(I)+SP(I)/REAL(NRECS)
      ENDDO

405   CONTINUE

CC Monthly average of initial condition--
      DO I=1,IGB
      Z0(I)=0.
      D0(I)=0.
      T0(I)=0.
      ENDDO
      DO I=1,IGA
      SP0(I)=0.
      ENDDO

      DO 505 NREC=1,NRECS
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

505   CONTINUE        

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

CC Mar
CC Monthly average of 1-step state--
      DO I=1,IGB
      ZA(I)=0.
      DA(I)=0.
      TA(I)=0.
      ENDDO
      DO I=1,IGA
      SPA(I)=0.
      ENDDO

      NRECS=31
      DO 406 NREC=1,NRECS
      READ(10)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RLTAPE
C       PRINT*,RKOUNT,RMTAPE,DAY,RLTAPE

      DO I=1,IGB
      ZA(I)=ZA(I)+Z(I)/REAL(NRECS)
      DA(I)=DA(I)+D(I)/REAL(NRECS)
      TA(I)=TA(I)+T(I)/REAL(NRECS)
      ENDDO
      DO I=1,IGA
      SPA(I)=SPA(I)+SP(I)/REAL(NRECS)
      ENDDO

406   CONTINUE

CC Monthly average of initial condition--
      DO I=1,IGB
      Z0(I)=0.
      D0(I)=0.
      T0(I)=0.
      ENDDO
      DO I=1,IGA
      SP0(I)=0.
      ENDDO

      DO 506 NREC=1,NRECS
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

506   CONTINUE        

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


	ii=ii+1
	print*,ii
200	continue



      STOP
      END               
