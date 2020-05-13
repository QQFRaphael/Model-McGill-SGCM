C*****caclulates grid field of sfce msl pressure from 1000mb T and GPH
C*****save to Gaussian grid

      PARAMETER (mxb=3625,ix=144,jx=73,mg=128,jg2=64,nl1=11,nl2=10)
        parameter (ilon =144,M=144, N=73)
      parameter (jlat=73)

      real x1(ix,jx),x2(ix,jx),x3(ix,jx),x4(ix,jx)
      real x(ilon,jlat),bb(72,37)
      integer mdays(12)
      character year*4

      REAL uu(ix,jx),vv(ix,jx),tt(ix,jx),hh(ix,jx),
     &U(mg,jg2,nl1),V(mg,jg2,nl1),T(mg,jg2,nl1),SP(mg,jg2),WRK(mg,jg2),
     &U2(mg,jg2,nl2),V2(mg,jg2,nl2),T2(mg,jg2,nl2),WRK3(mg,jg2,nl2),
     &UX(mg,jg2,nl2),VX(mg,jg2,nl2),SPX(mg,jg2),WRK4(mg,jg2,nl2),
     &UY(mg,jg2,nl2),VY(mg,jg2,nl2),SPY(mg,jg2),PS(nl1),
     &Z(mg,jg2,nl2),D(mg,jg2,nl2)

      real u3(ix,jx,14),v3(ix,jx,14),t3(ix,jx,14)

      REAL*8 UDP(mg,jg2,nl2),VDP(mg,jg2,nl2),TDP(mg,jg2,nl2),
     &SPDP(mg,jg2)
      INTEGER nbf(mxb)

      DATA PS/50.,150.,250.,300.,400.,500.,600.,700.,850.,925.,1000./
      data mdays/31,28,31,30,31,30,31,31,30,31,30,31/

      GA=9.81
      GASCON=287.0

         call getarg(1,year)
         read(year,*)iyear

        open(11,file='testu.dat',form='unformatted')
        open(12,file='testv.dat',form='unformatted')
        open(13,file='testt.dat',form='unformatted')
        open(14,file='testz.dat',form='unformatted')

        open(10,file='out.dat',form='unformatted')

        do im=1,3
         if(im.eq.1)then
           iyear1=iyear
           imon=9
         else if(im.eq.2)then
           iyear1=iyear
           imon=10
         else if(im.eq.3)then
           iyear1=iyear
           imon=11
         end if
         
        do iday=1,mdays(imon)

        IYRMODYTM=1000000*iyear1+10000*imon+100*iday

        print*,IYRMODYTM

        read(14)x4
        CALL INTERP(x4,SP)
C     (this puts 1000 mb geopotential height into SP array)
        read(11)u3
        read(12)v3
        read(13)t3

        do K=1,11
         if(K.eq.1)ilev=14
         if(K.eq.2)ilev=11
         if(K.eq.3)ilev=9
         if(K.eq.4)ilev=8
         if(K.eq.5)ilev=7
         if(K.eq.6)ilev=6
         if(K.eq.7)ilev=5
         if(K.eq.8)ilev=4
         if(K.eq.9)ilev=3
         if(K.eq.10)ilev=2
         if(K.eq.11)ilev=1

         do i=1,ix
         do j=1,jx
          uu(i,j)=u3(i,j,ilev)
          vv(i,j)=v3(i,j,ilev)
          tt(i,j)=t3(i,j,ilev)
         end do
         end do

         CALL INTERP(uu,WRK)
         DO 50 J=1,jg2
         DO 50 I=1,mg
         U(I,J,K)=WRK(I,J)
 50      CONTINUE
C        WRITE(8,*)'ZONAL WIND, ',ilvl,'  MB'
C        WRITE(10)WRK

         CALL INTERP(vv,WRK)
         DO 51 J=1,jg2
         DO 51 I=1,mg
         V(I,J,K)=WRK(I,J)
 51      CONTINUE
C        WRITE(8,*)'MERIDIONAL WIND, ',ilvl,'  MB'
C        WRITE(10)WRK

         CALL INTERP(tt,WRK)
         DO 52 J=1,jg2
         DO 52 I=1,mg
         T(I,J,K)=WRK(I,J)
c            WRK(I,J)=WRK(I,J)-273.15
 52      CONTINUE
C        WRITE(8,*)'TEMPERATURE, ',ilvl,'  MB'
C        WRITE(10)WRK

C**********************************************************************
        end do
C           work out surface pressure and write out grid fields
            DO 100 J=1,jg2
            DO 100 I=1,mg
C           calculate log sp
            SP(I,J)=SP(I,J)*GA/GASCON/T(I,J,11)
 100        CONTINUE
C           WRITE(8,*)'LOG SURFACE PRESSURE'
C           WRITE(10)SP

C           do vertical interpolation onto desired sigma surfaces
            CALL VINTP(U,U2,PS,SP)
            CALL VINTP(V,V2,PS,SP)
            CALL VINTP(T,T2,PS,SP)

C           write output (double precision)

            DO 110 J=1,jg2
            DO 110 I=1,mg
            SPDP(I,J)=SP(I,J)
             DO K=1,nl2
            UDP(I,J,K)=U2(I,J,K)
            VDP(I,J,K)=V2(I,J,K)
            TDP(I,J,K)=T2(I,J,K)
             END DO
 110        CONTINUE

            WRITE(10)IYRMODYTM,UDP,VDP,TDP,SPDP,IYRMODYTM


      enddo
      enddo

      stop
      end


      SUBROUTINE INTERP(x2,Y)
      PARAMETER (mxb=3625,ix=144,jx=73,mg=128,jg2=64,nl1=7)
      REAL x1(ix,jx),x2(ix,jx),Y(mg,jg2),GLAT(jg2)

C     (data for jg2=64)
      DATA GLAT/87.9,85.1,82.3,79.5,76.7,73.9,71.2,68.4,
     &65.6,62.8,60.0,57.2,54.4,51.6,48.8,46.0,43.3,40.5,
     &37.7,34.9,32.1,29.3,26.5,23.7,20.9,18.1,15.3,12.6,
     &9.8,7.0,4.2,1.4,-1.4,-4.2,-7.0,-9.8,-12.6,-15.3,
     &-18.1,-20.9,-23.7,-26.5,-29.3,-32.1,-34.9,-37.7,
     &-40.5,-43.3,-46.0,-48.8,-51.6,-54.4,-57.2,-60.0,
     &-62.8,-65.6,-68.4,-71.2,-73.9,-76.7,-79.5,-82.3,
     &-85.1,-87.9/

C
      DO 60 J=1,jg2
      j2=0
 30   j2=j2+1
      IF (j2.GT.jx) THEN
         PRINT*,'RUN OUT OF LATITUDES IN INTERPOLATION'
         STOP
      END IF
      lj = 180.*(0.5 - REAL(j2-1)/REAL(jx-1))
      IF (lj.GT.GLAT(J)) GOTO 30
      j2=j2-1
      fj=1. - (GLAT(J)-lj)*REAL(jx-1)/180.

      NPPB=mg/16

      DO 50 IBLOCK=1,16
      II=1+(IBLOCK-1)*NPPB
C     1d problem
      I=II
      i2=(I-1)*9/NPPB + 1

      Y(I,J)=x2(i2,j2)*(1.-fj) + x2(i2,j2+1)*fj

C     2d problem
      DO 40 I=II+1,II+NPPB-1
      i2=(I-1)*9/NPPB + 1

      fi=(REAL(I-1)/REAL(mg) - REAL(i2-1)/REAL(ix))*REAL(ix)
      Y(I,J)=   x2(i2,j2)*(1. - fi)*(1. - fj)
     &        + x2(i2,j2+1)*(1. - fi)*fj
     &        + x2(i2+1,j2)*fi*(1. - fj)
     &        + x2(i2+1,j2+1)*fi*fj

 40   CONTINUE
 50   CONTINUE
 60   CONTINUE

      RETURN
      END

      SUBROUTINE VINTP(X1,X2,PS,SP)
      PARAMETER (mg=128,jg2=64,nl1=11,nl2=10)
      REAL X1(mg,jg2,nl1),X2(mg,jg2,nl2),PS(nl1),
     &SP(mg,jg2),SIGMA2(nl2),SIGMAH(nl2-1)

C*****CODE FOR EQUALLY SPACED LEVELS

      DSIGMA=1./REAL(nl2)
      SIGMA2(1)=DSIGMA/2.
      DO 5 L=2,nl2
      SIGMA2(L)=SIGMA2(L-1)+DSIGMA
 5    CONTINUE


      DO 1000 J=1,jg2
      DO 1000 I=1,mg

      DO 30 K=1,nl2
      PVAL=SIGMA2(K)*1000.*EXP(SP(I,J))

C*****if above highest level do linear extrapolation
      IF (PVAL.LT.PS(1)) THEN
         X2(I,J,K)=X1(I,J,1) + (X1(I,J,1)-X1(I,J,2))*
     &   ((PS(1)-PVAL)/(PS(2)-PS(1)))
C*****if below lowest level do linear extrapolation
      ELSE IF (PVAL.GE.PS(nl1)) THEN
         X2(I,J,K)=X1(I,J,nl1) + (X1(I,J,nl1)-X1(I,J,nl1-1))*
     &   ((PS(nl1)-PVAL)/(PS(nl1-1)-PS(nl1)))
C*****otherwise just do linear interpolation - moving up from the
C     bottom until you're in the right layer
      ELSE
         DO 40 KK=nl1,2,-1
         IF (PVAL.LT.PS(KK)) THEN
            X2(I,J,K)=X1(I,J,KK-1)+(X1(I,J,KK)-X1(I,J,KK-1))*
     &      ((PVAL-PS(KK-1))/(PS(KK)-PS(KK-1)))
         END IF
 40      CONTINUE
      END IF

 30   CONTINUE

 1000 CONTINUE

      RETURN
      END
 

        
        
