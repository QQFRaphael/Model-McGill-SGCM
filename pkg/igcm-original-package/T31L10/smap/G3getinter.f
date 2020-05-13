      PROGRAM CHECKHST
      PARAMETER (NWJ2=256,NHEM=2,NL=10,IGA=NWJ2*NHEM,IGB=NL*IGA)
      COMPLEX*16 Z(IGB),D(IGB),T(IGB),SP(IGA)
      COMPLEX*16 ZA(IGB),DA(IGB),TA(IGB),SPA(IGA)
      COMPLEX*16 Z1(IGB),D1(IGB),T1(IGB),SP1(IGA)
        REAL*8 RKOUNT,RMTAPE,RLTAPE,DAY

        open(16,
     & FILE='/zemo2/jiaxj/igcm/RESULT/CONTROLT31exp/hisTotal.dat',
     &          form='unformatted')
        open(17,
     & FILE='/zemo2/jiaxj/igcm/RESULT/CONTROLT31exp/hisEnsm.dat',
     &          form='unformatted')
        open(18,
     & FILE='/zemo2/jiaxj/igcm/RESULT/CONTROLT31exp/hisInter.dat',
     &         form='unformatted')



	 do kkk=1,80
	  print*, kkk
	 do kk=1,180
         read(16)RKOUNT,RMTAPE,DAY,Z1,D1,T1,SP1,RMTAPE
         read(17)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RMTAPE
      DO I=1,IGB
      ZA(I)=Z1(I)-Z(I)
      DA(I)=D1(I)-D(I)
      TA(I)=T1(I)-T(I)
      ENDDO
      DO I=1,IGA
      SPA(I)=SP1(I)-SP(I)
      ENDDO
	ll=(kkk-1)*180+kk
	DAY=real(ll)
	write(18)RKOUNT,RMTAPE,DAY,ZA,DA,TA,SPA,RMTAPE	
	 end do
	rewind(17)
	 end do








	stop
	end
