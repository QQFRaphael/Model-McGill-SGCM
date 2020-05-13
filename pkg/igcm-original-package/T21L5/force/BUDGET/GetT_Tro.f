	PARAMETER (MG=128,JGG=64)
        PARAMETER (NWJ2=121,NHEM=2,NL=5,
     &IGA=NWJ2*NHEM,IGB=NL*IGA,nm=51)
      REAL*8 Z(MG,JGG,NL),D(MG,JGG,NL),T(MG,JGG,NL),SP(MG,JGG)
      REAL*8 ZA(MG,JGG,NL),DA(MG,JGG,NL),TA(MG,JGG,NL),SPA(MG,JGG)


	OPEN(14,FILE='grid.dat',
     &	form='UNFORMATTED',status='old')
	OPEN(15,FILE='grid_clim.dat',
     &	form='UNFORMATTED',status='old')

	 
        READ(14)Z,D,T,SP
        READ(15)ZA,DA,TA,SPA

      DO K=1,NL
      DO I=1,MG
      DO J=1,19
        T(I,J,K)=TA(I,J,K)
      ENDDO
      ENDDO
      ENDDO

      DO K=1,NL
      DO I=1,MG
      DO J=47,64
        T(I,J,K)=TA(I,J,K)
      ENDDO
      ENDDO
      ENDDO


      WRITE(16)Z,D,T,SP	



	end 


