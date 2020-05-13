	Parameter ( nm=51, ng=73*17, nEmax=4)
      Real t(73,37),tm(73,37),t1(73,37),t2(73,37,nm),
     &      var(ng,nm), cct(ng,ng), eign(ng,nEmax),
     &      tmm(73,17),tx(73,17)

      open(15,file=
     * 'DH_51_T.dat',
     &  form='unformatted',status='old')

      open(23,file='EOF',form='unformatted')

	do 16 i=1,73
	do 16 j=1,37
16	tm(i,j)=0.

	do 61 k=1,nm
        read(15)t
	 do 12 i=1,73
	 do 12 j=1,37
12	 tm(i,j)=tm(i,j)+t(i,j)/float(nm)
61	continue
         do j=1,17
         do i=1,73
          j1=j+10
          tmm(i,j)=tm(i,j1)
         enddo
         enddo
	 rewind (15)

	 do 62 k=1,nm
	  read(15)t1
	 do i=1,73
         do j=1,17
	  j1=j+10
          tx(i,j)=t1(i,j1)
         end do
         end do

          ii=0
CC 80N-20N
        do 1 j=1,17
        do 1 i=1,73
          ii=ii+1
          var(ii,k)=tx(i,j)-tmm(i,j)
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
 200  format (10f7.4)
	call eof(cct,eign)
        write(23)eign

      stop
      end


c  Program to calculate all eigenvalues and eigenvectors
c   of a real symmetric matrix of order nm using Eispack routines

        subroutine eof(c,ez)
      Parameter (nm=73*17, n=nm, nEmax=4)
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
