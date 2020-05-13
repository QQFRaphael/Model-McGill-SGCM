      program BARNES
      parameter(M=72,N=37,L=M+1,NN=19)
      real u0(M,NN),v0(M,NN),z0(L,N)
      real u1(36,NN),v1(36,NN),z1(36,NN)

      open(21,
     & file='/zemo2/jiaxj/igcm/T21L5/smap/SinFor/Wvector.dat',
     &    form='unformatted')
      open(22,
     & file='/zemo2/jiaxj/igcm/T21L5/smap/SinFor/fort.20',
     &    form='unformatted')

         read(21)u0
         read(21)v0
         read(22)z0

	 do i=1,M
	 do j=1,3
 	   u0(i,j)=0.
 	   v0(i,j)=0.
	 end do
	 end do


         do i=1,36
         do j=1,NN
          u1(i,j)=u0(i*2-1,j)
          v1(i,j)=v0(i*2-1,j)
          z1(i,j)=z0(i*2-1,j)
         end do
         end do

         write(30)((u1(i,j),i=1,36),j=1,NN)
         write(30)((v1(i,j),i=1,36),j=1,NN)
         write(30)((z1(i,j),i=1,36),j=1,NN)



	stop
	end
