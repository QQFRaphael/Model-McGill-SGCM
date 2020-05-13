	parameter(NT=50,nm=51,M=144, N=73, mm=182)
	parameter(M1=72, N1=15)
	real pht(N,N1,NT)
	real phm(N,N1),sa(nm),
     *       ave(N,N1,nm),var(N,N1,nm)
	real avem(N,N1),varm(N,N1)
	real t1(M,N),t2(M,N),t3(M,N)
	real tt1(N,N1),tt2(N,N1),tt3(N,N1)


C
       	open(12,file=
     *'/mnt/climate/data/loach/jiaxj/Data/SGCM/OutData/'//
     *'IDEAL/145W/OUTpred_monthly_SLP.dat'
     *,form='unformatted')
       open(26,file='avevar.data',form='unformatted')

        do i=1,N
        do j=1,N1
	phm(i,j)=0.
	end do
	end do

        do 12 it=1,NT
         read(12)tt2
        do 23 i=1,N
        do 23 j=1,N1
         phm(i,j)=phm(i,j)+tt2(i,j)/real(NT)
         pht(i,j,it)=tt2(i,j)
23      continue
12      continue



CCCC get the  variance
        do 51 i=1,N
        do 51 j=1,N1
          do  k=1,NT
  	    s=pht(i,j,k)-phm(i,j)
            varm(i,j)=varm(i,j)+s*s/float(NT-1)
	  end do
51      continue



	write(26)phm,varm
	stop
        end


