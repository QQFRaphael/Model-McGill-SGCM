        Parameter (nm=180*80, ng=36*14,mk=18)
	Parameter (mmg=73,nng=37,nnl=15)
	real ssp(mmg,nnl,5000),ttp(mmg,nnl,mk),zzp(mmg,nnl)
	real t1(mmg,nnl),t2(mmg,nnl),t3(mmg,nnl),t4(mmg,nnl)
	real t5(mmg,nnl),t6(mmg,nnl),t7(mmg,nnl),t8(mmg,nnl)
	real t9(mmg,nnl),t10(mmg,nnl),t11(mmg,nnl),t0(mmg,nnl)
	real wwp(mmg,nnl),y1(5000),y2(nm)

        character*3 cy
CC---
        call getarg(1,cy)
cc convert from character to integer--
        read(cy,'(i3)')mp

      open (11,
     *     file='/zemo2/jiaxj/igcm/RESULT/CONTROLT31exp/Z50anomfilt.dat'
     *     ,form='unformatted')
      open (12,
     *  file='/zemo2/jiaxj/igcm/RESULT/CONTROLT31exp/Z150anomfilt.dat'
     *     ,form='unformatted')
      open (13,
     * file='/zemo2/jiaxj/igcm/RESULT/CONTROLT31exp/Z250anomfilt.dat'
     *     ,form='unformatted')
      open (14,
     * file='/zemo2/jiaxj/igcm/RESULT/CONTROLT31exp/Z350anomfilt.dat'
     *     ,form='unformatted')
      open (15,
     * file='/zemo2/jiaxj/igcm/RESULT/CONTROLT31exp/Z450anomfilt.dat'
     *     ,form='unformatted')
      open (16,
     * file='/zemo2/jiaxj/igcm/RESULT/CONTROLT31exp/Z550anomfilt.dat'
     *     ,form='unformatted')
      open (17,
     *  file='/zemo2/jiaxj/igcm/RESULT/CONTROLT31exp/Z650anomfilt.dat'
     *     ,form='unformatted')
      open (18,
     * file='/zemo2/jiaxj/igcm/RESULT/CONTROLT31exp/Z750anomfilt.dat'
     *     ,form='unformatted')
      open (19,
     *  file='/zemo2/jiaxj/igcm/RESULT/CONTROLT31exp/Z850anomfilt.dat'
     *     ,form='unformatted')
      open (20,
     * file='/zemo2/jiaxj/igcm/RESULT/CONTROLT31exp/Z950anomfilt.dat'
     *     ,form='unformatted')

          do k=1,mk
           do i=1,mmg     
           do j=1,nnl   
            ttp(i,j,k)=0.
           end do      
           end do      
          end do


      open (41, file='fort.36',status='old')

	do k=1,mp
	print*, k

	  read(41,*)y1(k)
	  mm=y1(k)-7
	  do nn=1,mm
	   read(11)
	   read(12)
	   read(13)
	   read(14)
	   read(15)
	   read(16)
	   read(17)
	   read(18)
	   read(19)
	   read(20)
	  end do
	  
	  do nn=1,mk
	   read(11) t0
	   read(12) t1
	   read(13) t2
	   read(14) t3
	   read(15) t4
	   read(16) t5
	   read(17) t6
	   read(18) t7
	   read(19) t8
	   read(20) t9
	   do i=1,mmg
	   do j=1,nnl
	    ssp(i,j,nn+mk*(k-1))=(t1(i,j)+t2(i,j)+t3(i,j)+t4(i,j)+
     &          t5(i,j)+t6(i,j)+t7(i,j)+t8(i,j)+
     &          t9(i,j)+t0(i,j))/real(9)
	   end do
	   end do
	  end do
	  rewind(11)
	  rewind(12)
	  rewind(13)
	  rewind(14)
	  rewind(15)
	  rewind(16)
	  rewind(17)
	  rewind(18)
	  rewind(19)
	  rewind(20)
	end do

	
	  do k=1,mk*mp
           do i=1,mmg
           do j=1,nnl
            wwp(i,j)=ssp(i,j,k)
           end do
           end do
	    write(441) wwp
          end do
	 


	
CCCCCCCCCCC

	 stop 
	 end 






