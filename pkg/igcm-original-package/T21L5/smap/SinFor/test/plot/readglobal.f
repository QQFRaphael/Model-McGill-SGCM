      program BARNES
      parameter(M=73,N=15)
      real u(M,N),v(M,N)

      open(21,file='../fort.20'
     &          ,form='unformatted')


      open(22,file='fort.51'
     &          ,form='unformatted')


         do ii=1,1
         read(21)u
         read(22)v
         write(30)((v(i,j),i=1,73),j=1,15)
         write(30)((u(i,j),i=1,73),j=1,15)
         end do


        end
