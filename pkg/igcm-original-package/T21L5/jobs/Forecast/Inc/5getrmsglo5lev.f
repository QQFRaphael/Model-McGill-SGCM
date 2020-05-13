	parameter(M1=73,N=37,Nrun=10,L=15,nlen=5,nm=Nrun*nlen)
	real x(M1,N,nm),tt(M1,N),ss(M1,N),y(M1,N)
	real std(nm),ssd(nlen),x1(M1,N),x2(M1,N)
         character*3 cy
         call getarg(1,cy)
         read(cy,'(i3)') mp
	 mm=mp*nlen

 	open(12,file='fort.51
     & ', form='unformatted')
        open(13, 
     & file='fort.41',form='unformatted',status='old')

	



	do k=1,nm

           do i=1,M1
           do j=1,N
              x1(i,j)=0.
           end do
           end do

	  do kk=1,5
           read(12) tt
           read(13) ss
	   do i=1,M1
	   do j=1,N
	     x1(i,j)=x1(i,j)+(ss(i,j)-tt(i,j))**2/real(5) 
	   end do
	   end do
          end do

           do i=1,M1
           do j=1,N
	      x(i,j,k)=x1(i,j)
           end do
           end do
	  end do

	  do k=1,nm	
           do i=1,M1
           do j=1,N
              y(i,j)=x(i,j,k)
           end do
           end do

	    rr=0.
           do i=1,M1
           do j=1,N
              rr=rr+y(i,j)/real(M1*N)
           end do
           end do
	     std(k)=rr
c     write(34,*) std(k)
	end do

CCCCCCCCCCCCCCC

          do k1=1,nlen
             ssd(k1)=0.
           end do

           do k2=1,Nrun
           do k1=1,nlen
             ssd(k1)=ssd(k1)+std(k1+(k2-1)*nlen)/real(Nrun)
           end do
           end do

           do k1=1,nlen
             write(35,*)(k1-1)*5, ssd(k1)
           end do





        stop
        end



