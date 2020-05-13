
	PARAMETER ( mg=73,jg2=37,nm=51*6)

        real olr(73,37),t1(73,37,nm)
	real c(nm),y1(nm),y2(nm),pot(mg,jg2)
	real rr(nm)

        OPEN(UNIT=25,FILE='RegDHEOF3',
     &  STATUS='OLD',FORM='UNFORMATTED')

        open(13,file='/zemo2/jiaxj/EOF/index',
     *    form='unformatted',status='old')

        read(13)rr
        read(13)c


	do iy=1,nm
        do i=1,mg
        do j=1,jg2
         t1(i,j,iy)=0.
        end do
        end do
	end do

         READ(25)olr
	do iy=1,nm
        do i=1,mg
        do j=1,jg2
         t1(i,j,iy)=olr(i,j)*c(iy)
        end do
        end do
	end do







	print*,'Congratulations!'
        end

#include "./gausstogrid128.f"

