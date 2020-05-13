      program BARNES
      parameter(M=72,N=19,L=M+1,NN=15,MM=36)
      real u(M,N),v(M,N),z(L,37)
      real u1(MM,N),v1(MM,N),z1(MM,N)

      open(21,file=
     &'../Wvector.dat',
     &    form='unformatted')

      open(22,file=
     &'../fort.20',
     &    form='unformatted')


         read(21)u
         read(21)v 
         read(22)z


         do i=1,MM
         do j=1,NN
          u1(i,j)=u(i*2-1,j)
          v1(i,j)=v(i*2-1,j)
          z1(i,j)=z(i*2-1,j)
         end do
         end do


	 do i=1,MM
	 do j=1,3
 	   u1(i,j)=0.
 	   v1(i,j)=0.
	 end do
	 end do

         write(30)((u1(i,j),i=1,MM),j=1,NN)
         write(30)((v1(i,j),i=1,MM),j=1,NN)
         write(30)((z1(i,j),i=1,MM),j=1,NN)



	stop
	end
