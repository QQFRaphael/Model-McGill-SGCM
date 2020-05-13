      program BARNES
      parameter(M=73,N=15)
      real u(M,37),v(M,N)

      open(21,file='../First30/fort.20'
     &          ,form='unformatted')


         do ii=1,10
         read(21)u
         write(30)((u(i,j),i=1,73),j=1,15)
         end do


        end
