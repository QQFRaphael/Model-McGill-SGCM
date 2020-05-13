	parameter(nm=51,ns=nm,M=72,M1=M+1,N=37,Nrun=30,
     *    cent=10.)
	parameter(NLON=64,NLAT=32)
	real pot(NLON,NLAT),st(NLON,NLAT),fll(M1,N)
	real x(M1,N),fllm(M1,N),tt(M,N),ss(NLON,NLAT)


c	open(17,file='SLP.dat',form='unformatted')
	open(17,file='exp1/Z_monthly.dat',form='unformatted')

          do i=1,M1
          do j=1,N
            fllm(i,j)=0.
          end do
          end do


	do 20 kk=1,306
          read(17)st
          call gautogrid(st,fll)	
          do i=1,M1
          do j=1,N
            fllm(i,j)=fllm(i,j)+fll(i,j)/real(306)
          end do
          end do
20	continue



   	write(33)fllm




        stop
        end



#include "/diska/hlin/qgmodel/plot/gausstogrid.f"
