        Parameter ( nm=51, ng=36*14)
      Real t(73,37),e(ng,10),c(4,nm),
     &      var(ng,nm), cct(ng,ng),fllp(73,37)
	real ttp(73,37,nm),tt(73,37,nm)
	real y1(nm),y2(nm),pot(73,37)



      open (11, file=
     *   'SLP.dat',form='unformatted',
     *    status='old')


        do 61 k=1,nm
	  read(11)t
         do 12 j=1,37
         do 12 i=1,73
	 ttp(i,j,k)=t(i,j)
12	continue
61      continue


CC*******Obtain the PCs********************

        open  (2, file='fort.86')

	do k=1,nm
 	read(2,*)y1(k)
	end do

         s=0.
        do k=1,nm
         s=s+y1(k)/float(nm)
        end do

        std=0.
        do k=1,nm
         std=std+(y1(k)-s)**2/float(nm-1)
        end do
        std=sqrt(std)
	print*,std
	
        do i=1,73
        do j=1,37
         do k=1,nm
          y2(k)=ttp(i,j,k)
         end do

         call regress(y1,y2,nm,a,b)

         pot(i,j)=a*std
        end do
        end do



	write(16)pot




      stop
      end



CC

        subroutine regress(x,y,nm,a,b)
        real x(nm),y(nm)
CC use linear function y=ax+b
CC
        x1=0.
        y1=0.
        do i=1,nm
        x1=x1+x(i)/float(nm)
        y1=y1+y(i)/float(nm)
        end do

        xy=0.
        do i=1,nm
        xy=xy+(x(i)-x1)*(y(i)-y1)
        end do

        x2=0.

        do i=1,nm
        x2=x2+(x(i)-x1)**2
        end do

        a=xy/x2
        b=y1-a*x1

        return
        end

C------------------------------
        subroutine detrend(y,nm,a,b)
        real x(51),y(nm)
CC use linear function y=ax+b
        do i=1,nm
        x(i)=i
        end do
CC
        x1=0.
        y1=0.
        do i=1,nm
        x1=x1+x(i)/float(nm)
        y1=y1+y(i)/float(nm)
        end do

        xy=0.
        do i=1,nm
        xy=xy+(x(i)-x1)*(y(i)-y1)
        end do

        x2=0.
        do i=1,nm
        x2=x2+(x(i)-x1)**2
        end do

        a=xy/x2
        b=y1-a*x1

        return
        end
CCCCCCCCCCCCCCC


        subroutine getstd(y,nm,r)
        real y(nm)
        s=0.
        do k=1,nm
        s=s+y(k)/float(nm)
        end do

        r=0.
        do k=1,nm
        r=r+(y(k)-s)**2
        end do

        r=sqrt(r/float(nm-1))

        return
        end






