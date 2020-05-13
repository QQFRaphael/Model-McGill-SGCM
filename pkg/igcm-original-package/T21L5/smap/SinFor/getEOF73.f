	Parameter ( nm=49, ng=36*14, nEmax=4)
      Real t(73,37),tm(73,37),t1(73,37),t2(73,37,nm),
     &      var(ng,nm), cct(ng,ng), eign(ng,nEmax),
     &	    olr(73,37),tt(73,37,nm),ts(73,37,nm),y1(nm)


      open(15,file=
     *'Ens_Z73.dat',
     &  form='unformatted',status='old')

      open(23,file='EOF',form='unformatted')

        do iy=1,nm
         READ(15)olr
        do i=1,73
        do j=1,37
         tt(i,j,iy)=olr(i,j)
        end do
        end do
        end do

cc detrend
        do i=1,73
        do j=1,37
          do iy=1,nm
	   y1(iy)=tt(i,j,iy)
	  end do
          call detrend(y1,nm,a1,b1)
          do k=1,nm
           y1(k)=y1(k)-a1*k-b1
	   ts(i,j,k)=y1(k)
          end do
	end do
	end do



	do 16 i=1,73
	do 16 j=1,37
16	tm(i,j)=0.

	do 61 k=1,nm
	 do 12 i=1,73
	 do 12 j=1,37
12	 tm(i,j)=tm(i,j)+ts(i,j,k)/float(nm)
61	continue


	 rewind (15)
	 do 62 k=1,nm
	 do i=1,73
         do j=1,37
          t(i,j)=ts(i,j,k)
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

c  Obtain covariance matrix, i.e. observation matrix multiplied by its transpose
      do 50 l=1,ng
      do 50 ll=1,ng
         cct(l,ll) = 0.0
         do 51 m=1,nm
  51        cct(l,ll) = cct(l,ll) + var(l,m)*var(ll,m)
            cct(l,ll) = cct(l,ll) / real(nm)
  50  continue

        write(*,*) 'sample covariance matrix'
        do 38 l=1,5
38      write(*,200) (cct(l,ll), ll=1,10)
 200  format (10f10.2)
	call eof(cct,eign)
        write(23)eign

      stop
      end


c  Program to calculate all eigenvalues and eigenvectors
c   of a real symmetric matrix of order nm using Eispack routines

        subroutine eof(c,ez)
      Parameter (nm=36*14, n=nm, nEmax=4)
      Real c(nm,nm),ez(nm,nEmax)
      Real a(nm,nm), w(nm), z(nm,nm), fv1(nm)


                do 10 i=1,nm
                do 10 j=1,nm
                   a(i,j) = c(i,j)
  10            continue
      call tred2(nm,n,a,w,fv1,z)
      call tql2(nm,n,w,fv1,z,ierr)
      if (ierr.ne.0) go to 99999

                trace = 0.0
                slamda = 0.0
                do 60 l=1,nm
                   trace = trace + a(l,l)
                   slamda = slamda + w(l)
  60            continue

      open  (2, file='percent.dat')
      write (2,*)   'trace = ',trace,'      sum of lamdas = ',slamda
      write (2,*)   'percentage'
      write (2,*) (w(l)/trace*100., l=nm,nm-50,-1)

        do k=1,nm
        do i=1,nEmax
          l=nm-i+1
          ez(k,i)=z(k,l)
        end do
        end do

99999 write (6,*) 'ierr = ', ierr
      return
      end

#include "/diska/hlin/qgmodel/EOF/tql2.f"
#include "/diska/hlin/qgmodel/EOF/tred2.f"
#include "/diska/hlin/qgmodel/EOF/pythag.f"
#include "/zemo2/jiaxj/SUBROUTINES/detrend.f"

