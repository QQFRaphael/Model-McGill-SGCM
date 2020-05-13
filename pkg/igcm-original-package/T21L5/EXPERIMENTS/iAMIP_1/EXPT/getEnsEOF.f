CC EOF  of Ensemble
        Parameter (nmonth=6,nEmax=4)
        Parameter (ng=36*14, ny=51)
	parameter(nm=51*6,ns=nm,M=73,N=37,Nrun=30)
        Real t(M,N),t1(M,N),t2(M,15,nm),
     &   var(ng,nm), cct(ng,ng), eign(ng,nEmax)
        real tm(M,N,nmonth)



	open(17,file='EOF.dat',form='unformatted')
c	open(21, file='/zemo2/jiaxj/data/Monthlymean/PRES.OtoM.dat'
c	open(21, file='/zemo2/jiaxj/data/Monthlymean/HGT.500.OtoM.dat'
c	open(21, file='Z500.dat'
 	open(21, file='SLP.dat'
     &  ,form='unformatted',status='old')

        do i=1,M
        do j=1,N
        do k=1,6
         tm(i,j,k)=0.
        end do
        end do
        end do

        do k=1,ny
         do mon=1,nmonth
           read(21)t
         do i=1,M
         do j=1,N
           tm(i,j,mon)=tm(i,j,mon)+t(i,j)/51.
         end do
         end do
         end do
        end do
        rewind (21)
C********tm is the monthly mean******


        do k=1,nm
         read(21)t1
         do i=1,M
         do j=1,15
          t2(i,j,k)=t1(i,j)
         end do
         end do
        end do


        do 62 k=1,ny
        do 62 mon=1,nmonth
           mm=mon+6*(k-1)
         do i=1,73
         do j=1,15
          t(i,j)=t2(i,j,mm)
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
         var(ii,mm)=(t(i5,j)-tm(i5,j,mon))*sqrt(cos(phi))
1       continue
62      continue
        write(6,*) 'total gridpoints ',ii

c  Obtain covariance matrix, i.e. observation matrix multiplied by its transpose


      do 50 l=1,ng
      do 50 ll=1,ng
         cct(l,ll) = 0.0
         do 51 nn=1,nm
  51        cct(l,ll) = cct(l,ll) + var(l,nn)*var(ll,nn)
         cct(l,ll) = cct(l,ll) / real(nm)
  50  continue

        write(*,*) 'sample covariance matrix'

        do 38 l=1,10
38      write(*,200) (cct(l,ll), ll=1,10)
 200  format (10f10.2)
        call eof(cct,eign)
        write(17)eign


	print*,'Good Job!'
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

