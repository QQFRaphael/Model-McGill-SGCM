
      PROGRAM GETFORCE
C*****compile with -r8
      PARAMETER (NWJ2=121,NHEM=2,NL=5,
     &IGA=NWJ2*NHEM,IGB=NL*IGA)
      COMPLEX Z(IGB),D(IGB),T(IGB),SP(IGA)
      COMPLEX ZA(IGB),DA(IGB),TA(IGB),SPA(IGA)
      COMPLEX Z0(IGB),D0(IGB),T0(IGB),SP0(IGA)
      COMPLEX ZF(IGB),DF(IGB),TF(IGB),SPF(IGA)


      OPEN(11,FILE=
     &'/mnt/climate/data/loach/jiaxj/Data/SGCM/Input/'//
     &'T21L5/UVTP4849after.dat.spec_corrected',
     & form='UNFORMATTED',status='old')

      OPEN(13,FILE='UVTP1998.dat.DJFave',form='UNFORMATTED') 

      NRECS=90

Cjxj Skip the first 49 years data and get the data in 1997/1998, Elnino year


CC 90-day average of initial condition--
      DO I=1,IGB
      Z0(I)=0.
      D0(I)=0.
      T0(I)=0.
      ENDDO
      DO I=1,IGA
      SP0(I)=0.
      ENDDO


CCCCCCCCCC  
	do iyr=1,50*NRECS
         READ(11)
	enddo

      DO NREC=1,NRECS
      READ(11)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RLTAPE
      DO I=1,IGB
      Z0(I)=Z0(I)+Z(I)
      D0(I)=D0(I)+D(I)
      T0(I)=T0(I)+T(I)
      ENDDO
      DO I=1,IGA
      SP0(I)=SP0(I)+SP(I)
      ENDDO
      ENDDO
CCCCCCCCCCCCCCCCC

      DO I=1,IGB
      Z0(I)=Z0(I)/REAL(NRECS*1)
      D0(I)=D0(I)/REAL(NRECS*1)
      T0(I)=T0(I)/REAL(NRECS*1)
      ENDDO
      DO I=1,IGA
      SP0(I)=SP0(I)/REAL(NRECS*1)
      ENDDO

      WRITE(13)RKOUNT,RMTAPE,DAY,Z0,D0,T0,SP0,RLTAPE 


      STOP
      END               
