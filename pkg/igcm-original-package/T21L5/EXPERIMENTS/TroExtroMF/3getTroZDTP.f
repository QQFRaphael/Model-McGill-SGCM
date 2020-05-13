	PARAMETER (MG=128,JGG=64,NL=5,nm=1)

      REAL*8 Z(MG,JGG,NL),D(MG,JGG,NL),T(MG,JGG,NL),SP(MG,JGG)




CCC spec.dat is the anomaly field obtaibed from FORCEanom_RegSVD2
CCC transformed by the program in /zemo2/jiaxj/force/BUDGET/tste.sh 
CCC 
 	OPEN(14,FILE='fort.14',form='UNFORMATTED',status='old')

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


        write(15)Z,D,T,SP
        end



