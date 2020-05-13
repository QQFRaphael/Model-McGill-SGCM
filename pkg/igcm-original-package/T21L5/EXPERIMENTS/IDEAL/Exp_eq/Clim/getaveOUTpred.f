C
C*****compile with -r8
      PARAMETER (NWJ2=121,NHEM=2,NL=5,
     &IGA=NWJ2*NHEM,IGB=NL*IGA)
      COMPLEX Z(IGB),D(IGB),T(IGB),SP(IGA)
      COMPLEX ZA(IGB),DA(IGB),TA(IGB),SPA(IGA)
      COMPLEX ZC(IGB),DC(IGB),TC(IGB),SPC(IGA)





        OPEN(10,FILE='OUTpred.monthly',
     &form='UNFORMATTED',status='old')
      OPEN(11,FILE='climate', form='UNFORMATTED')

CC----------------------
      DO I=1,IGB
      ZA(I)=0.
      DA(I)=0.
      TA(I)=0.
      ENDDO
      DO I=1,IGA
      SPA(I)=0.
      ENDDO

        do NY=1,50
      READ(10)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RLTAPE

      DO I=1,IGB
      ZA(I)=ZA(I)+Z(I)
      DA(I)=DA(I)+D(I)
      TA(I)=TA(I)+T(I)
      ENDDO
      DO I=1,IGA
      SPA(I)=SPA(I)+SP(I)
      ENDDO

      ENDDO


      DO I=1,IGB
      ZA(I)=ZA(I)/REAL(50)
      DA(I)=DA(I)/REAL(50)
      TA(I)=TA(I)/REAL(50)
      ENDDO
      DO I=1,IGA
      SPA(I)=SPA(I)/REAL(50)
      ENDDO

        DAY=REAL(0)
        RKOUNT=0.
      WRITE(11)RKOUNT,RMTAPE,DAY,ZA,DA,TA,SPA,RMTAPE



      STOP
      END
