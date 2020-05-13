	subroutine selneg(pht,NT,ns,itp,aa1,cc1)

        real y1(NT),y2(NT),ylf(nt),pht(nt*ns)
	INTEGER ic(nt),ic1(nt)
	real aa1(500),bb1(500),cc1(500)
	real aa2(500),bb2(500),cc2(500)

            iTmax=11
            Hmin=-0.4
	    itp=0
	    itn=0
	    ntt=nt

        do 212 iy=1,ns

	 ipos=0
        do 122 it=1,NT
122     ylf(it)=pht(it+(iy-1)*180)

        do 38 k=1,nt
        if(ylf(k).le.Hmin)ic(k)=1
        if(ylf(k).gt.Hmin)ic(k)=0
38      continue

ccccccccccccccccccccccccccccccccccc
        ij=0
1001    ig=1
        k0=ij+1
        if(k0.ge.(ntt-5))go to 43
          n=0
          do 41 k=k0,ntt
          ig=ig*ic(k)
          n=n+1
          ij=k
          if(ig.eq.0.and.n.lt.iTmax)go to 1001
          if(ig.eq.0.and.n.ge.iTmax)then
           ipos=ipos+1
 	   aa1(ipos+itp)=(iy-1)*nt+k-n+1
 	   cc1(ipos+itp)=n-1
           go to 1001
          endif
41      continue
cccccccccccccccccccccccccccccccccccccccccccc
43      ij=0

	 itp=itp+ipos
212     continue

	 return

        stop
        end


