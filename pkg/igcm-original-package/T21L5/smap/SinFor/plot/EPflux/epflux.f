      program BARNES
      parameter(M=72,N=17,L=M+1)
      real u0(M,N),v0(M,N),z0(L,37)
      real u1(36,17),v1(36,17),z1(36,17)

      open(21,
     & file='../../Wvector.dat',
     &    form='unformatted')

         read(21)u0
         read(21)v0


	 do i=1,M
	 do j=1,3
 	   u0(i,j)=0.
 	   v0(i,j)=0.
	 end do
	 end do


         do i=1,36
         do j=1,17
          u1(i,j)=u0(i*2-1,j)
          v1(i,j)=v0(i*2-1,j)
         end do
         end do

         write(30)((u1(i,j),i=1,36),j=1,17)
         write(30)((v1(i,j),i=1,36),j=1,17)



	stop
	end
