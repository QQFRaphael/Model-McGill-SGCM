	PARAMETER (MG=128,JGG=64,NL=5,nm=1)

      REAL*8 Z(MG,JGG,NL),D(MG,JGG,NL),T(MG,JGG,NL),SP(MG,JGG)
      REAL*8 Z1(MG,JGG,NL),D1(MG,JGG,NL),T1(MG,JGG,NL),SP1(MG,JGG)
      REAL*8 ZA(JGG,NL),DA(JGG,NL),TA(JGG,NL),SPA(JGG)



 	OPEN(15,FILE='pert.dat',form='UNFORMATTED')


        READ(15)Z,D,T,SP

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

        write(30)Z1,D1,T1,SP1
        end



