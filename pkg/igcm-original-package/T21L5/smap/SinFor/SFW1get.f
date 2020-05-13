        Parameter (nm=182*51,mk=18,nday=182,nyear=51)
	Parameter (mmg=72,M=72,N=37,LL=15)
	real y1(nm)
	real um(mmg,N),vm(mmg,N)
	real sum(mmg,N,1800),svm(mmg,N,1800)
	real tum(mmg,N,mk),tvm(mmg,N,mk)
	real uf(mmg,N),vf(mmg,N),pot(mmg,N),td(mmg,N),tf(mmg,LL)
	real uave(mmg,LL),vave(mmg,LL)
	real suave(mmg,LL),svave(mmg,LL)



        character*3 cy
CC---
        call getarg(1,cy)
cc convert from character to integer--
        read(cy,'(i3)')mp


CCCCCCCCCCCCCCC


      open (13,file=
     *'/mnt/climate/data/loach/jiaxj/Data/NCEPDATA/Data/'//
     *'Z500/ulf_global.dat'
     *,form='unformatted')
      open (14,file=
     *'/mnt/climate/data/loach/jiaxj/Data/NCEPDATA/Data/'//
     *'Z500/vlf_global.dat'
     *,form='unformatted')
      open (37, file=
c    *'/zemo2/jiaxj/Internal/AOevlotion/NAO/ENSO/fort.52'
     *'/zemo2/jiaxj/Internal/AOevlotion/NAO/fort.31'
     &	,status='old')
      open (42, file='SFanom.dat',form='unformatted')

      open (24,file=
     *'/mnt/climate/data/loach/jiaxj/Data/NCEPDATA/Data/'//
     *'Z500/uave-ann.dat',form='unformatted')
      open (25,file=
     *'/mnt/climate/data/loach/jiaxj/Data/NCEPDATA/Data/'//
     *'Z500/vave-ann.dat',form='unformatted')
      open (26, file='UVclim.dat',form='unformatted')


           do j=1,N   
           do i=1,M
            do k=1,mk
            tum(i,j,k)=0.
            tvm(i,j,k)=0.
            end do      
             uf(i,j)=0.
             vf(i,j)=0.
           end do      
           end do



CCCCCCCCCCCCCCCCCCCCCCC

	do k=1,mp
c	  read(37,*)mm1,y1(k),nn1
 	  read(37,*)y1(k),nn1
	  mm=y1(k)-7
          do nn=1,mm
           read(13)
           read(14)
          end do
 
	  do nn=1,mk
           read(13)um
           read(14)vm
	   do j=1,N
	   do i=1,mmg
	    sum(i,j,nn+mk*(k-1))=um(i,j)
	    svm(i,j,nn+mk*(k-1))=vm(i,j)
	   end do
	   end do
	  end do
          rewind(13)
          rewind(14)
	end do

CCCCCCCCCCCCget the average of all cases
	  do l=1,mp
	  do k=1,mk
           do i=1,M
           do j=1,N
            tum(i,j,k)=tum(i,j,k)+sum(i,j,k+(l-1)*mk)/real(mp)
            tvm(i,j,k)=tvm(i,j,k)+svm(i,j,k+(l-1)*mk)/real(mp)
           end do
           end do
          end do
          end do
	 

	do kkk=1,mk
           do i=1,M
           do j=1,N
            uf(i,j)=tum(i,j,kkk)
            vf(i,j)=tvm(i,j,kkk)
           end do
           end do
           call relax1(uf,vf,pot,td,2)
           do i=1,M
           do j=1,LL
            tf(i,j)=td(i,j)
           end do
           end do
 	   write(42) tf
	end do
	  rewind(37)

CCCCCCCCC Get the climatological um and vm

           do j=1,LL
           do i=1,mmg
            suave(i,j)=0.
            svave(i,j)=0.
           end do
           end do


        do k=1,mp
c         read(37,*)mm1,y1(k),nn1
          read(37,*)y1(k),nn1
          mm=mm1-1948
          do nn=1,mm
           read(24)
           read(25)
          end do

           read(24)uave
           read(25)vave
           do j=1,LL
           do i=1,mmg
            suave(i,j)=suave(i,j)+uave(i,j)/real(mp)
            svave(i,j)=svave(i,j)+vave(i,j)/real(mp)
           end do
           end do
	   rewind(24)
	   rewind(25)
          end do

	  write(26) suave,svave



	 stop 
	 end 


#include "/zemo2/jiaxj/SUBROUTINES/relax2.h"


