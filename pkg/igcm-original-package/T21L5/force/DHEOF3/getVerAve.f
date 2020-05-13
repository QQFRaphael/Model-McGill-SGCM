      PROGRAM GETFORCE
      PARAMETER (MG=128,JGG=64,NL=5,nm=1)
      REAL*8 U(MG,JGG,NL),V(MG,JGG,NL),T(MG,JGG,NL),SP(MG,JGG)
      REAL TA(MG,JGG)
      REAL T1(MG,JGG),T2(MG,JGG),T3(MG,JGG),T4(MG,JGG),T5(MG,JGG)
      real fll(73,37),bll(73,37)	

      OPEN(11,
     &FILE='gauss.dat',
     &form='UNFORMATTED',status='old')
	

      DO I=1,73
      DO J=1,37
       bll(I,J)=0.
      ENDDO
      ENDDO	

CCjxj
	
      DO 10 NREC=1,nm
        READ(11)U,V,T,SP

      DO I=1,MG
      DO J=1,JGG
        T1(I,J)=T(I,J,1)
        T2(I,J)=T(I,J,2)
        T3(I,J)=T(I,J,3)
        T4(I,J)=T(I,J,4)
        T5(I,J)=T(I,J,5)
      ENDDO
      ENDDO

      DO I=1,MG
      DO J=1,JGG
	 TA(I,J)=(T1(I,J)+T2(I,J)+T3(I,J)+T4(I,J)+T5(I,J))/5.
      ENDDO
      ENDDO	

      call gautogrid(TA,fll)


      DO I=1,73
      DO J=1,37
       bll(I,J)=bll(I,J)+fll(I,J)
      ENDDO
      ENDDO	

	write(33)fll
 10   CONTINUE

      DO I=1,73
      DO J=1,37
       bll(I,J)=bll(I,J)/real(nm)
      ENDDO
      ENDDO	

	 write(34,*)fll

      STOP
      END               


#include "./gausstogrid128.f"
