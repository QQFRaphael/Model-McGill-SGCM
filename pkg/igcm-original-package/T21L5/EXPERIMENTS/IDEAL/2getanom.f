	PARAMETER ( mg=128,jg2=64,nl1=7,nl2=5)
	REAL*8 UDP(mg,jg2,nl2),VDP(mg,jg2,nl2),TDP(mg,jg2,nl2),
     &SPDP(mg,jg2)
	REAL*8 UDPn(mg,jg2,nl2),VDPn(mg,jg2,nl2),TDPn(mg,jg2,nl2),
     &SPDPn(mg,jg2)
	REAL*8 UDPx(mg,jg2,nl2),VDPx(mg,jg2,nl2),TDPx(mg,jg2,nl2),
     &SPDPx(mg,jg2)

	REAL TAN(mg,jg2)

	OPEN(13,FILE='anom',STATUS='OLD',FORM='UNFORMATTED')

	open(26,file='pert.dat',form='unformatted')

cc initialize
	 do j=1,jg2
         do i=1,mg
	 do k=1,nl2
	   UDP(i,j,k)=0.0
           VDP(i,j,k)=0.0
           TDP(i,j,k)=0.0
	 end do
	  SPDP(i,j)=0.0
         end do
        end do

	rewind(13)
         do k=1,nl2
	  READ(13)TAN
	 do j=1,jg2
         do i=1,mg
           TDP(i,j,k)=TAN(i,j)
         end do
         end do
        end do

	WRITE(26)UDP,VDP,TDP,SPDP

        stop
        end

