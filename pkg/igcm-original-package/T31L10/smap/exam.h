        subroutine exam(ng,aa,bb,y3,y4,nn)
	real y3(5000),y1(5000),y2(5000),y4(5000)
	real aa(5000),bb(5000)

	do kk=1,ng
	 y1(kk)=aa(kk)
	 y2(kk)=bb(kk)
	end do

	do k=1,ng
	  mm=aa(k)+bb(k)+10
	if (mm.gt.aa(k+1)) then
	   y1(k+1)=0.
	   y2(k+1)=0.
	else
	end if
	end do

	
	nn=0
	do k=1,ng
	if(y1(k).gt.0. ) then
	 nn=nn+1
	 y3(nn)=y1(k)
	 y4(nn)=y2(k)
	else
	end if
	end do


	 return

CCCCCCCCCCC

	 stop 
	 end 






