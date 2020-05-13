        Parameter (nm=180*45, mk=18)
	Parameter (mmg=73,nng=37,nnl=15)
	real ssp(mmg,nnl,5000),ttp(mmg,nnl,mk)
	real t1(mmg,nnl),t2(mmg,nnl),t3(mmg,nnl),t4(mmg,nnl),t5(mmg,nnl)
	real wwp(mmg,nnl)
	real y1(5000)
	real y2(nm)


        character*3 cy
CC---
        call getarg(1,cy)
cc convert from character to integer--
        read(cy,'(i3)')mp




      open (13,
     * file='/zemo2/jiaxj/igcm/RESULT/CONTROLT31/Z500anomfilt.dat',
     * form='unformatted')

          do k=1,mk
           do i=1,mmg     
           do j=1,nnl   
            ttp(i,j,k)=0.
           end do      
           end do      
          end do

      open (41,file='fort.35',status='old')

	do k=1,mp
	 print*, k
	  read(41,*)y1(k), kk
	  mm=y1(k)-7
	  do nn=1,mm
	   read(13)
	  end do
	  
	  do nn=1,mk
	   read(13) t3
	   do i=1,mmg
	   do j=1,nnl
	    ssp(i,j,nn+mk*(k-1))=t3(i,j)
	   end do
	   end do
	  end do
	  rewind(13)
	end do


	  do l=1,mp
	  do k=1,mk
           do i=1,mmg
           do j=1,nnl
            ttp(i,j,k)=ttp(i,j,k)+ssp(i,j,k+(l-1)*mk)/real(mp)
           end do
           end do
          end do
          end do
	 


          do k=1,mk
           do i=1,mmg
           do j=1,nnl
            wwp(i,j)=ttp(i,j,k)
           end do
           end do
	  write(30) wwp
	
          end do




CCCCCCCCCCC

	 stop 
	 end 






