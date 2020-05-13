	PARAMETER (MG=128,JGG=64,NL=5,nm=51)


      REAL*8 Z(MG,JGG,NL),D(MG,JGG,NL),T(MG,JGG,NL),SP(MG,JGG)
      REAL ZA(MG,JGG),DA(MG,JGG),TA(MG,JGG),SPA(MG,JGG)
      real fll(73,37)

CCCCCCCCCCCCCC


	OPEN(14,FILE='pert.dat',
     &	form='UNFORMATTED',status='old')


        READ(14)Z,D,T,SP


        DO I=1,MG
        DO J=1,JGG
	 ZA(I,J)=0.
	ENDDO
	ENDDO


	do K=1,5
        DO I=1,MG
        DO J=1,JGG
	 ZA(I,J)=ZA(I,J)+Z(I,J,K)/real(5)
	ENDDO
	ENDDO
	ENDDO

	call gautogrid(ZA,fll)
	write(16) fll



        end


#include "/zemo2/jiaxj/force/DHEOF3/gausstogrid128.f"






