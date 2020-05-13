	PARAMETER (MG=128,JGG=64,NL=5,nm=1)

      REAL*8 Z(MG,JGG,NL),D(MG,JGG,NL),T(MG,JGG,NL),SP(MG,JGG)
      REAL*8 Z1(MG,JGG,NL),D1(MG,JGG,NL),T1(MG,JGG,NL),SP1(MG,JGG)
      REAL*8 ZA(JGG,NL),DA(JGG,NL),TA(JGG,NL),SPA(JGG)



 	OPEN(15,FILE='tro.dat',
     &	form='UNFORMATTED')

CCC spec.dat is the anomaly field obtaibed from FORCEanom_RegAO
CCC transformed by the program R2tste.sh 
CCC 
 	OPEN(14,FILE='spec.dat',
     &	form='UNFORMATTED',status='old')

        READ(14)Z,D,T,SP

      DO  K=1,NL
      DO  I=1,MG

      DO  J=1,21
        Z(I,J,K)=0.
        D(I,J,K)=0.
        T(I,J,K)=0.
      ENDDO
      DO  J=22,25
        Z(I,J,K)=Z(I,26,K)*(0.5**(26-J))
        D(I,J,K)=D(I,26,K)*(0.5**(26-J))
        T(I,J,K)=T(I,26,K)*(0.5**(26-J))
      ENDDO
      DO  J=40,43
        Z(I,J,K)=Z(I,39,K)*(0.5**(J-39))
        D(I,J,K)=D(I,39,K)*(0.5**(J-39))
        T(I,J,K)=T(I,39,K)*(0.5**(J-39))
      ENDDO
      DO  J=44,64
        Z(I,J,K)=0.
        D(I,J,K)=0.
        T(I,J,K)=0.
      ENDDO
      ENDDO
      ENDDO
     

      DO  I=1,MG
      DO  J=1,21
        SP(I,J)=0.
      ENDDO
      DO  J=22,25
        SP(I,J)=SP(I,26)*(0.5**(26-J))
      ENDDO
      DO  J=40,43
        SP(I,J)=SP(I,39)*(0.5**(J-39))
      ENDDO
      DO  J=44,64
        SP(I,J)=0.
      ENDDO
      ENDDO

CCC
C Get the zonal average

      DO  K=1,NL
      DO  J=1,JGG
        ZA(J,K)=0.
        DA(J,K)=0.
        TA(J,K)=0.
      ENDDO
      ENDDO
      DO  J=1,JGG
        SPA(J)=0.
      ENDDO

      DO  K=1,NL
      DO  J=1,JGG
      DO  I=1,MG
	ZA(J,K)=ZA(J,K)+Z(I,J,K)
	DA(J,K)=DA(J,K)+D(I,J,K)
	TA(J,K)=TA(J,K)+T(I,J,K)
      ENDDO
      ENDDO
      ENDDO
      DO  K=1,NL
      DO  J=1,JGG
        ZA(J,K)=ZA(J,K)/real(MG)
        DA(J,K)=DA(J,K)/real(MG)
        TA(J,K)=TA(J,K)/real(MG)
      ENDDO
      ENDDO


      DO  J=1,JGG
      DO  I=1,MG
        SPA(J)=SPA(J)+SP(I,J)
      ENDDO
      ENDDO
      DO  J=1,JGG
        SPA(J)=SPA(J)/real(MG)
      ENDDO


CCC
C Substract the zonal average


      DO  K=1,NL
      DO  J=1,JGG
      DO  I=1,MG
        Z1(I,J,K)=Z(I,J,K)-ZA(J,K)
        D1(I,J,K)=D(I,J,K)-DA(J,K)
        T1(I,J,K)=T(I,J,K)-TA(J,K)
      ENDDO
      ENDDO
      ENDDO


      DO  J=1,JGG
      DO  I=1,MG
        SP1(I,J)=SP(I,J)-SPA(J)
      ENDDO
      ENDDO

        write(15)Z1,D1,T1,SP1
        end



