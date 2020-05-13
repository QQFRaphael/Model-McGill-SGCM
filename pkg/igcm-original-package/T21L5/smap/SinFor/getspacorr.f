	Parameter (mmg=73,nng=15,nnl=15,nnn=mmg*nng)
	real t1(mmg,nnl),s1(mmg,37)
	real y1(mmg*nng),y2(mmg*nng)


      open (13,
     *file='/zemo2/jiaxj/force_diag/NCEP/fort.PNA-SLP',
     &form='unformatted')
c    *file='/zemo2/jiaxj/force_diag/NCEP/fort.PNA-Z500',form='unformatted')

      open (14,
     * file='fort.20', form='unformatted')

	   read(13) t1
	   read(14) s1
	
	     mm=0
           do i=1,mmg
           do j=1,nng
	     mm=mm+1
             y1(mm)=t1(i,j)
             y2(mm)=s1(i,j)
           end do
           end do

        call pearsn(y1,y2,nnn,c0,prob,z)

        print*, c0, prob
CCCCCCCCCCCCCCCCCCCCC

      open (15,
     * file='/zemo2/jiaxj/force_diag/NCEP/fort.AO', form='unformatted')

           read(15) t1

             mm=0
           do i=1,mmg
           do j=1,nng
             mm=mm+1
             y1(mm)=t1(i,j)
             y2(mm)=s1(i,j)
           end do
           end do

        call pearsn(y1,y2,nnn,c0,prob,z)

        print*, c0, prob





	 stop 
	 end 





#include "/diskc/hlin/recipes/pearsn.for"
#include "/diskc/hlin/recipes/betai.for"
#include "/diskc/hlin/recipes/betacf.for"
#include "/diskc/hlin/recipes/gammln.for"

