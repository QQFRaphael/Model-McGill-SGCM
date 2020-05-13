        Parameter ( nm=51, ng=36*14)
      Real t(72,37),tm(72,15),t1(72,37),t2(72,15,nm)
C#
        real pot(96,48),x0(73,37),pot1(96,48),pot2(96,48),
     *    x(73,37),fll(73,37),x1(73,37)

        character(len=120) infile
        character(len=120) outfile


        infile=trim('./Z500_monthly.dat')
        outfile=trim('./out.dat')

        open(15,file=infile,form='unformatted',status='old')
        rewind(15)
        open(101,file=outfile,form='unformatted')
        rewind(101)


cc read Z450 and Z550
c          read(15)
c          read(15)
c          read(15)pot1
          read(15)pot
cc Average to get Z500
c        do i=1,96
c        do j=1,48
c         pot(i,j)=(pot1(i,j)+pot2(i,j))/2.
c        end do
c        end do
cc Convert to lat-lon grid
          call gautogrid(pot,fll)
        do j=1,37
        do i=1,73
         x(i,j)=fll(i,j)
         x0(i,j)=fll(i,j)
         x1(i,j)=fll(i,j)
        end do
        end do

        do k=1,29
cc read Z450 and Z550
c          read(15)
c          read(15)
c          read(15)pot1
          read(15)pot
cc average to get Z300
c        do i=1,96
c        do j=1,48
c         pot(i,j)=(pot1(i,j)+pot2(i,j))/2.
c        end do
c        end do

cc Convert to lat-lon grid
          call gautogrid(pot,fll)
        do j=1,37
        do i=1,73
c         x(i,j)=(fll(i,j)-x0(i,j))*10000.
         x(i,j)=(fll(i,j)-x0(i,j))*1.E2
         x1(i,j)=x0(i,j)+x(i,j)
        end do
        end do

        write(101)x1

        end do
        close (15)
        close (101)
        stop
        end


        include "./smaplib/gausstogrid96X48.f"

