      PROGRAM CHECKHST
      PARAMETER (NWJ2=256,NHEM=2,NL=10,IGA=NWJ2*NHEM,IGB=NL*IGA)
      REAL*8 RKOUNT,RMTAPE,DAY,RLTAPE
      COMPLEX*16 Z(IGB),D(IGB),T(IGB),SP(IGA)
      COMPLEX*16 ZA(IGB),DA(IGB),TA(IGB),SPA(IGA)
        character*1 enum1
        character*2 enum2


        open(17,
     & FILE='/zemo2/jiaxj/igcm/RESULT/CONTROLT31exp/hisTotal.dat',
     &          form='unformatted',access='append')

        do ie=1,80

	if(ie.lt.10)then
	 write(enum1,'(i1)')ie
        OPEN(21,
     & FILE='/zemo2/jiaxj/igcm/RESULT/CONTROLT31exp/history'//enum1//'',
     &    form='unformatted',status='old')
	print*,'history'//enum1//''


       else if(ie.ge.10)then
          write(enum2,'(i2)')ie
        OPEN(21,
     & FILE='/zemo2/jiaxj/igcm/RESULT/CONTROLT31exp/history'//enum2//'',
     &      form='unformatted',status='old')
	print*,'history'//enum2//''

	  end if

	 
	do NR=1,30
	 read(21)
	end do
	
	do NY=1,180
	NM=180*(ie-1)+NY

        READ(21)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RLTAPE
        DAY=REAL(NM)
        RKOUNT=0.
        WRITE(17)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RMTAPE

	end do


	end do


	stop
	end



