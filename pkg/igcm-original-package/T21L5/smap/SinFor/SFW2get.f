c11c W flux is defined by Takaya and Nakamura (1997, GRL)
        PARAMETER ( mg=72,jg2=37,nl1=7,nl2=5,nm=18, L=72,M=15)
        parameter(NLON=96,NLAT=48)
        real olr(72,37),UA(51,4),UB(51,4)
 	real um(L,M),vm(L,M)
        real x(L,M),fll(73,37),tf(73,37,nm),z9(73,37)
        real z(NLON,NLAT),z1(NLON,NLAT),zs(L,M)
        real z8(L,M,nm),y1(nm),y2(nm),pot(L,M),yt(L,M,1,nm)
        real psi_x(L,M),psi_y(L,M),ex(L,M),ey(L,M)
        real psi_xx(L,M),psi_xy(L,M),psi_yy(L,M)
        REAL dx(m),phi(m),psi(L,M),rpsi(L,M),
     2 cor(m),rcos(m),rsin(m),r2sin(m)
        DATA PHI1,PHI2,DELAM/20.,90.,5.0/

      open(24,file='UVclim.dat'
     * ,form='unformatted',status='old')


      open(26,file=
     * 'SFanom.dat',form='unformatted',status='old')
      open(23,file='Wvector.dat',form='unformatted')


CC constants
      G=9.81
      PI=4.0*ATAN(1.0)
      EARTH=6370949.0
      CRAD=PI/180.0
      DLAM=DELAM
      DPHI=(-PHI2+PHI1)/(M-1)
      RDLAM=CRAD*DLAM
      RDPHI=CRAD*DPHI
      DO 11 J=1,M
      PHI(J)=PHI2+(J-1)*DPHI
      PHI(J)=PHI(J)*CRAD
      RCOS(J)=COS(PHI(J))
      RSIN(J)=SIN(PHI(J))
        r2SIN(J)=SIN(2.*PHI(J))
      DX(J)=RDLAM*EARTH*RCOS(J)
      COR(J)=2.0*7.292E-5*RSIN(J)
   11 CONTINUE
	DY=RDPHI*EARTH

CCCCCCCC
         read(24)um,vm

        do iy=1,nm
           read(26)rpsi
           do j=1,M
           do i=1,mg
            psi(i,j)=rpsi(i,j)*(-1.)
           end do
           end do


ccccc Psi_x, Psi_y
c
      do j = 1,m
      jp1 = j + 1
      jm1 = j - 1
      if (j.eq.1) jm1 = 1
      if (j.eq.m) jp1 = m
      dyy = 2.*dy
      if (j.eq.1.or.j.eq.m) dyy = dy
      do i = 1,l
      ip1 = i + 1
      if (i.eq.l) ip1 = 1
      imn1 = i - 1
      if (i.eq.1) imn1 = l
      psi_x(i,j) = (psi(ip1,j)-psi(imn1,j))/(2.*dx(j))
      psi_y(i,j) = (psi(i,jp1)-psi(i,jm1))/dyy
        end do
        end do


cccc Psi_xx, Psi_xy, Psi_yy
        do j = 1,m
      jp1 = j + 1
      jm1 = j - 1
      if (j.eq.1) jm1 = 1
      if (j.eq.m) jp1 = m
      dyy = 2.*dy
      if (j.eq.1.or.j.eq.m) dyy = dy
      do i = 1,l
      ip1 = i + 1
      if (i.eq.l) ip1 = 1
      imn1 = i - 1
      if (i.eq.1) imn1 = l
      psi_xx(i,j) = (psi_x(ip1,j)-psi_x(imn1,j))/(2.*dx(j))
      psi_xy(i,j) = (psi_x(i,jp1)-psi_x(i,jm1))/dyy
      psi_yy(i,j) = (psi_y(i,jp1)-psi_y(i,jm1))/dyy
        end do
        end do


cc wave-activity flux Ex, Ey
        do j = 1,m
        do i = 1,l
         absu=sqrt(um(i,j)**2+vm(i,j)**2)
         bb=1./(2.*absu)
         ex(i,j)=um(i,j)*(psi_x(i,j)**2-psi(i,j)*psi_xx(i,j))+
     *           vm(i,j)*(psi_x(i,j)*psi_y(i,j)-psi(i,j)*psi_xy(i,j))
         ey(i,j)=um(i,j)*(psi_x(i,j)*psi_y(i,j)-psi(i,j)*psi_xy(i,j))+
     *           vm(i,j)*(psi_y(i,j)**2-psi(i,j)*psi_yy(i,j))
         ex(i,j)=bb*ex(i,j)
         ey(i,j)=bb*ey(i,j)
         r=sqrt(ex(i,j)**2+ey(i,j)**2)
         if(r.lt..5)then
         ex(i,j)=0.
         ey(i,j)=0.
         end if
        end do
        end do


        do j = m-2,m
        do i=1,L
c        ex(i,j)=0.
c        ey(i,j)=0.
        end do
        end do


        write(23)((ex(i,j),i=1,L),j=1,M)
        write(23)((ey(i,j),i=1,L),j=1,M)
        end do



        stop
        end









