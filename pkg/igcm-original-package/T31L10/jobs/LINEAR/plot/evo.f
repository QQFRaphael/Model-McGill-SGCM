      program BARNES
      parameter(M=73,N=15)
      real u(M,37),v(M,N)

      open(21,file='../fort.35'
     &          ,form='unformatted')


         read(21)
         read(21)

         do ii=1,6
         read(21)u
         write(30)((u(i,j),i=1,73),j=1,15)
         read(21)
         end do


        end
