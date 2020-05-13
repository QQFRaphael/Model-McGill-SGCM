      program BARNES
      parameter(M=73,N=37)
      real u(M,37),v(M,N)

      open(21,file='../fort.20'
     &          ,form='unformatted')



         read(21)u
         write(30)((u(i,j),i=1,73),j=1,37)

         read(21)u
         write(30)((u(i,j),i=1,73),j=1,37)




        end
