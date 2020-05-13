      PROGRAM CHECKHST
      PARAMETER (NWJ2=121,NHEM=2,NL=5,
     &IGA=NWJ2*NHEM,IGB=NL*IGA,nn=1)
      COMPLEX*16 Z(IGB),D(IGB),T(IGB),SP(IGA)
      COMPLEX*16 ZA(IGB),DA(IGB),TA(IGB),SPA(IGA)
        REAL*8 RKOUNT,RMTAPE,DAY,RLTAPE
        character*1 enum1
        character*2 enum2
        character*3 enum3
         character*3 cy
         call getarg(1,cy)
         read(cy,'(i3)') mp



        open(17,file='OUTpred_total.dat'
     &          ,form='unformatted',access='append')

        do ie=1,mp

	if(ie.lt.10)then
	 write(enum1,'(i1)')ie
       open(21,
     &    file='OUTpred_'//enum1//'.dat',
     &    form='unformatted',status='old')
	print*,'OUTpred_'//enum1//''

       else if(ie.ge.10.and.ie.lt.100)then
          write(enum2,'(i2)')ie
       open(21,
     &   file='OUTpred_'//enum2//'.dat',
     &      form='unformatted',status='old')
	print*,'OUTpred_'//enum2//''

       else if(ie.ge.100)then
          write(enum3,'(i3)')ie
       open(21,
     &   file='OUTpred_inc'//enum3//'.dat',
     &      form='unformatted',status='old')
        print*,'OUTpred_'//enum3//''
	  end if

	 
	do NY=1,nn
	NM=nn*(ie-1)+NY
        READ(21)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RLTAPE
        DAY=REAL(NM)
        RKOUNT=0.
        WRITE(17)RKOUNT,RMTAPE,DAY,Z,D,T,SP,RMTAPE
	end do


	end do


	stop
	end
