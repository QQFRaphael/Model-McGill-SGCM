	PARAMETER (MG=128,JGG=64,NL=5,nm=1)

      REAL*8 Z(MG,JGG,NL),D(MG,JGG,NL),T(MG,JGG,NL),SP(MG,JGG)
      REAL*8 Z1(MG,JGG,NL),D1(MG,JGG,NL),T1(MG,JGG,NL),SP1(MG,JGG)
      REAL*8 ZA(JGG,NL),DA(JGG,NL),TA(JGG,NL),SPA(JGG)



CCC use /zemo2/jiaxj/force/BUDGET/tste.sh to transfer FORCEanom_RegSVD2 to spec.dat. CCCCC
CCC spec.dat is the anomaly field obtaibed from FORCEanom_RegSVD2
CCC transformed by the program in /zemo2/jiaxj/force/BUDGET/tste.sh 
CCC 
 	OPEN(14,FILE='spec.dat',
     &	form='UNFORMATTED',status='old')

 	OPEN(15,FILE='tro.dat',form='UNFORMATTED')

        READ(14)Z,D,T,SP

      DO  K=1,NL
      DO  I=1,MG

      DO  J=1,21
        T(I,J,K)=0.
      ENDDO
      DO  J=22,25
        T(I,J,K)=T(I,26,K)*(0.5**(26-J))
      ENDDO
      DO  J=40,43
        T(I,J,K)=T(I,39,K)*(0.5**(J-39))
      ENDDO
      DO  J=44,64
        T(I,J,K)=0.
      ENDDO
      ENDDO
      ENDDO
     

        write(15)Z,D,T,SP
        end



