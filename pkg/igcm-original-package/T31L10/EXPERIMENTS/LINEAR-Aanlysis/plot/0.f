
      program BARNES
      parameter(M=73,N=37,L=15)
      real u(M,N),v(M,L)


         open(21,file='fort.30')



        do kk=1,5
         read(21,*)((v(i,j),i=1,M),j=1,L)
         write(31)((v(i,j),i=1,M),j=1,L)

        end do

        end

