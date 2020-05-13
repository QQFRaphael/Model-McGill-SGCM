      program BARNES
      parameter(M=73,N=15)
      real u(M,37),v(M,37)

      open(21,file='../fort.20'
     &          ,form='unformatted')


         do ii=1,1
         read(21)u,v
         write(30)((v(i,j),i=1,73),j=1,15)
         end do


        end
