	parameter(nm=51,ns=nm,M=72,M1=M+1,N=37,Nrun=30,
     *    cent=10.)
	parameter(NLON=64,NLAT=32)
	real pot(NLON,NLAT),st(NLON,NLAT),fll(M1,N)
	real x(M1,N),fllm(M1,N),tt(M,N),ss(NLON,NLAT)


	open(17,file='Z500.dat',form='unformatted')
	open(21, file='exp1/Z_monthly.dat'
     &  ,form='unformatted',status='old')




          do i=1,M1
          do j=1,N
            fllm(i,j)=0.
          end do
          end do


	do 20 kk=1,306
          read(21)st
          call gautogrid(st,fll)

          do i=1,M1
          do j=1,N
            fllm(i,j)=fllm(i,j)+fll(i,j)
          end do
          end do

          do i=1,NLON
          do j=1,NLAT
            st(i,j)=0.
          end do
          end do

          do i=1,M1
          do j=1,N
            fll(i,j)=0.
          end do
          end do



20	continue


          do i=1,M1
          do j=1,N
            fllm(i,j)=fllm(i,j)/real(306)
          end do
          end do


   	write(33)fllm




        stop
        end



#include "/diska/hlin/qgmodel/plot/gausstogrid.f"
