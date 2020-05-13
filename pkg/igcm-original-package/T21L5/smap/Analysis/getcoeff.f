	Parameter ( nm=51, ng=36*14, nEmax=2, M1=73,N1=37)
      Real t(M1,N1),tm(M1,15),t1(M1,N1),t2(M1,15,nm),
     &      var(ng,nm), cct(ng,ng), eign(ng,nEmax),c(4,nm),
     &	    olr(M1,N1),tt(M1,N1,nm),ts(M1,N1,nm),y1(nm),y2(nm)


      open(15,file=
     * '../HaiforceSVD2_Detrend_T/SLP.dat',
c    * '../HaiforceSVD2_Detrend/ExtroForce/SLP.dat',
c    * '../HaiforceSVD2_Detrend/SLP.dat',
     &  form='unformatted',status='old')

      open(23,file='/zemo2/jiaxj/force_diag/NCEP/EOF',
     & form='unformatted')

        do iy=1,nm
         READ(15)olr
        do i=1,M1
        do j=1,N1
         tt(i,j,iy)=olr(i,j)
        end do
        end do
        end do


	do 16 i=1,M1
	do 16 j=1,15
16	tm(i,j)=0.

	do 61 k=1,nm
	 do 12 i=1,M1
	 do 12 j=1,15
12	 tm(i,j)=tm(i,j)+tt(i,j,k)/float(nm)
61	continue


	 rewind (15)
	 do 62 k=1,nm
	 do i=1,M1
         do j=1,15
          t(i,j)=tt(i,j,k)
         end do
         end do

          ii=0
CC 80N-20N
        do 1 j=2,15
	 phi=90.-(j-1)*5.
	 phi=phi*3.1415926/180.
        do 1 i=1,36
          ii=ii+1
          i5=2*(i-1)+1
         var(ii,k)=(t(i5,j)-tm(i5,j))*sqrt(cos(phi))
1       continue
62      continue
        write(6,*) 'total gridpoints ',ii

        read(23)eign


        do 44 ij=1,4
        do 44 nn=1,nm
          c(ij,j)=0.0
          do 45 mi=1,ng
45          c(ij,nn)=c(ij,nn)+eign(mi,ij)*var(mi,nn)
44      continue


        do k=1,nm
         y1(k)=c(1,k)
        end do

 	call getstd(y1,nm,r1)
	print*,r1

CC Detrend
        call detrend(y1,nm,a1,b1)
         do k=1,nm
          y1(k)=y1(k)-a1*k-b1
         end do

        do k=1,nm
         y1(k)=y1(k)/real(r1)
         write(31,*)-y1(k)
        end do



      stop
      end


CCCCCCCCCCCCCCC

#include "/diska/hlin/qgmodel/EOF/tql2.f"
#include "/diska/hlin/qgmodel/EOF/tred2.f"
#include "/diska/hlin/qgmodel/EOF/pythag.f"
#include "/zemo2/jiaxj/SUBROUTINES/detrend.f"
#include "/zemo2/jiaxj/SUBROUTINES/getstd.f"


