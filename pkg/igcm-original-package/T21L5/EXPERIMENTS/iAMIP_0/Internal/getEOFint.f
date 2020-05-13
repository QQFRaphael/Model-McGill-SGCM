	Parameter ( nm=51*30, ng=36*14, nEmax=10)
      Real t(72,37),tm(72,15),t1(72,37),t2(72,15,nm),ten(72,15,51),
     &      var(ng,nm), cct(ng,ng), eign(ng,nEmax)

      open(15,file=
     * 'Z500.dat',
     &   form='unformatted',status='old')

      open(23,file='Z500EOF',form='unformatted')

	rewind (15)
	do k=1,51
         do i=1,72
         do j=1,15
          ten(i,j,k)=0.
         end do
         end do
        end do
c ensemble mean
	 do it=1,30
	 do k=1,51
	  read(15)t1
	  do i=1,72
          do j=1,15
           ten(i,j,k)=ten(i,j,k)+t1(i,j)/30.
          end do
          end do
	 end do
	 end do

cc internal parts
	rewind(15)
	ii=0
	do it=1,30
        do k=1,51
          read(15)t1
	  ii=ii+1
          do i=1,72
          do j=1,15
           t2(i,j,ii)=t1(i,j)-ten(i,j,k)
          end do
          end do
         end do
         end do
	
cc average
	do 16 i=1,72
        do 16 j=1,15
16      tm(i,j)=0.
        do 61 k=1,nm
         do 12 i=1,72
         do 12 j=1,15
12       tm(i,j)=tm(i,j)+t2(i,j,k)/float(nm)
61      continue
	print*, tm

	do 62 k=1,nm
	 do i=1,72
         do j=1,15
          t(i,j)=t2(i,j,k)
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
        do 38 l=1,10
38      write(*,200) (cct(l,ll), ll=1,10)
 200  format (10f8.2)
	call eof(cct,eign)
        write(23)eign

      close (15)
      close (14)
      stop
      end


c  Program to calculate all eigenvalues and eigenvectors
c   of a real symmetric matrix of order nm using Eispack routines

        subroutine eof(c,ez)
      Parameter (nm=36*14, n=nm, nEmax=10)
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

      open  (2, file='percentEOF_Z500.dat')
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
