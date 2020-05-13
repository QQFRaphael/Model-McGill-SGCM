      PROGRAM CHECKHST
      PARAMETER (NWJ2=256,NHEM=2,NL=10,IGA=NWJ2*NHEM,IGB=NL*IGA)
      COMPLEX*16 Z(IGB),D(IGB),T(IGB),SP(IGA)
      COMPLEX*16 ZA(IGB),DA(IGB),TA(IGB),SPA(IGA)
      COMPLEX*16 Z1(IGB),D1(IGB),T1(IGB),SP1(IGA)
      COMPLEX*16 ZS(IGB,180),DS(IGB,180),TS(IGB,180),SPS(IGA,180)
        REAL*8 RKOUNT,RMTAPE,DAY,RLTAPE

        open(17,
     & FILE='/zemo2/jiaxj/igcm/RESULT/CONTROLT31exp/hisTotal.dat',
     &         form='unformatted')
        open(18,
     & FILE='/zemo2/jiaxj/igcm/RESULT/CONTROLT31exp/hisEnsm.dat',
     &         form='unformatted')


	do K=1,180
      DO I=1,IGB
      ZS(I,K)=0.
      DS(I,K)=0.
      TS(I,K)=0.
      ENDDO
      DO I=1,IGA
      SPS(I,K)=0.
      ENDDO
	end do

	 do kkk=1,80	
	  print*, kkk
	 do kk=1,180
         read(17)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RMTAPE
      DO I=1,IGB
      ZS(I,kk)=ZS(I,kk)+Z(I)/real(80)
      DS(I,kk)=DS(I,kk)+D(I)/real(80)
      TS(I,kk)=TS(I,kk)+T(I)/real(80)
      ENDDO
      DO I=1,IGA
      SPS(I,kk)=SPS(I,kk)+SP(I)/real(80)
      ENDDO
	 end do
	 end do

	do kk=1,180
      DO I=1,IGB	
      Z1(I)=ZS(I,kk)
      D1(I)=DS(I,kk)
      T1(I)=TS(I,kk)
      ENDDO
      DO I=1,IGA
      SP1(I)=SPS(I,kk)
      ENDDO
	DAY=real(kk)
	write(18)RKOUNT,RMTAPE,DAY,Z1,D1,T1,SP1,RMTAPE
	end do






	stop
	end
