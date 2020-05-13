	parameter(nm=51,ns=nm,M=72,M1=M+1,N=37,Nrun=30,
     *    cent=10.)
	real x(M,N),fllm(M1,N),tt(M,N),fll(M,N)


c	open(17,file='Z500.dat',form='unformatted')
        open(17,file='/zemo2/jiaxj/data/Monthlymean/PRES.OtoM.dat'
     &  ,form='unformatted')
c	open(17,file='SLP.dat',form='unformatted')


	do  kk=1,51
          do i=1,M1
          do j=1,N
            fllm(i,j)=0.
          end do
          end do

          read(17)tt
          read(17)tt
	
	do ii=1,3
          read(17)fll
          do i=1,M1
          do j=1,N
            fllm(i,j)=fllm(i,j)+fll(i,j)
          end do
          end do
	end do


          do i=1,M1
          do j=1,N
            fllm(i,j)=fllm(i,j)/3.
          end do
          end do
      
  
       
            write(33)fllm
            read(17)tt

	end do



        stop
        end


