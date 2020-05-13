      program BARNES
      parameter(M=72,N=15,L=M+1)
      real u0(M,N),v0(M,N),z0(M,N)
      real u(M,N),v(M,N),z(L,N)
      real u1(36,15),v1(36,15),z1(36,15)

      open(21,
     & file='../Wvector.dat',
     &    form='unformatted')
      open(22,
     & file='../Zanom.dat',
     &    form='unformatted')

         read(21)u0
         read(21)v0
         read(22)z0
	  print*, z0

	 do i=1,M
	 do j=1,3
 	   u0(i,j)=0.
 	   v0(i,j)=0.
	 end do
	 end do


         do i=1,36
         do j=1,15
          u1(i,j)=u0(i*2-1,j)
          v1(i,j)=v0(i*2-1,j)
          z1(i,j)=z0(i*2-1,j)
         end do
         end do

         write(30)((u1(i,j),i=1,36),j=1,15)
         write(30)((v1(i,j),i=1,36),j=1,15)
         write(30)((z1(i,j),i=1,36),j=1,15)



	stop
	end
