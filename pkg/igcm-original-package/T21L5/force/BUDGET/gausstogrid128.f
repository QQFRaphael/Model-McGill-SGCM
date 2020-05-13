C     Program :  Convert global lat-long gridpoint field
cc               from global Gaussian grid  -----Hai Lin 11/93
#include "/diska/hlin/qgmodel/force/spect.f"

cc    input:pot, output td
        subroutine gautogrid(gg1,fll)
C
      PARAMETER(ILG=128,ILG1=ILG+1,ILATH=32,ILAT=2*ILATH)
      PARAMETER(L=73,M=37,N=3,LM1=L-1,NP1=N+1,NM1=N-1)
      REAL SL(ILAT),WL(ILAT),SIA(ILAT),RAD(ILAT),WOCS(ILAT)
      REAL DLAT(ILAT),slat(ilat),AZ(5)
      REAL P1(NP1),P2(N),DP1(N),DP2(NM1),SIGMA(NM1)
      REAL fll(l,m),td(l-1,m)
      REAL GG(ILG1,ILAT),gg1(ILG,ILAT)
      COMMON /WORK1/G,R,CP,PI,EARTH,CRAD
      COMMON /WORK2/DLAM,DPHI,RDLAM,RDPHI,DX(M),DY
     2 ,PHI(M),COR(M),RCOS(M),RSIN(M)
      DATA SIGMA/3.73E-2,2.27E-2/
      DATA OMEGA/7.292E-5/

        do j=1,ILAT
        do i=1,ILG
          GG(i,j)=gg1(i,j)
        end do
          GG(129,j)=GG(1,j)
        end do
C
      CALL CONST1
      CALL CONST2(-90.0,90.0,5.0)
C
C
      OMEG2=2.0*OMEGA
      RAD2=EARTH*EARTH
C
C
C     CALCULATE CONSTANTS FOR THE GAUSSIAN GRID
C
      CALL GAUSSG(ILATH,SL,WL,SIA,RAD,WOCS)
      CALL TRIGL(ILATH,SL,WL,SIA,RAD,WOCS)
C
C
C     DLAT IS THE LATITUDE (IN DEGREES) OF THE CURRENT
C     GAUSSIAN ROW
C
      DO 96 J=1,ILAT
      DLAT(J)=180.0*RAD(J)/PI
      slat(j) = dlat(j) + 90.
   96 CONTINUE
CC Convert to Grid point
      call ggill (fll,l,m,gg,ilg1,ilat,slat,3)
        return
      END
C
      SUBROUTINE CONST1
      COMMON /WORK1/G,R,CP,PI,EARTH,CRAD
      G=9.81
      R=287.0
      CP=1004.0
      PI=4.0*ATAN(1.0)
      EARTH=6370949.0
      CRAD=PI/180.0
      RETURN
      END
C
      SUBROUTINE CONST2(PHI1,PHI2,DELAM)
      PARAMETER(M=37)
      COMMON /WORK1/G,R,CP,PI,EARTH,CRAD
      COMMON /WORK2/DLAM,DPHI,RDLAM,RDPHI,DX(M),DY
     2 ,PHI(M),COR(M),RCOS(M),RSIN(M)
      DLAM=DELAM
      DPHI=(PHI2-PHI1)/(M-1)
      RDLAM=CRAD*DLAM
      RDPHI=CRAD*DPHI
      DO 11 J=1,M
      PHI(J)=PHI1+(J-1)*DPHI
      PHI(J)=PHI(J)*CRAD
      RCOS(J)=COS(PHI(J))
      RSIN(J)=SIN(PHI(J))
      DX(J)=RDLAM*EARTH*RCOS(J)
      COR(J)=2.0*7.292E-5*RSIN(J)
   11 CONTINUE
      DY=RDPHI*EARTH
      RETURN
      END

