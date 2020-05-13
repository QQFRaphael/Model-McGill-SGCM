      PROGRAM MATRIX

      REAL AA(3,3),AAI(3,3),AM(3,3)

      DATA AA/1.,2.,1.,0.,1.,1.,4.,0.,1./

      PRINT*,AA

      DO J=1,3
      DO I=1,3
      PRINT*,AA(I,J)
      ENDDO
      ENDDO

      a = AA(1,1)
      b = AA(2,1)
      c = AA(1,2)
      d = AA(2,2)
      e = AA(1,3)
      f = AA(2,3)

      alf = b*c - a*d
      bet = c - a
      gam = b*e - a*f
      del = e - a

      r = bet*gam - alf*del

      AAI(1,1) = (r*(del - e) + (gam - b*del)*(e*bet - c*del))/a/del/r
      AAI(2,1) = (gam - b*del)/r
      AAI(3,1) = 1./del + bet*(b*del - gam)/del/r
      AAI(1,2) = (e*bet - c*del)/r
      AAI(2,2) = a*del/r
      AAI(3,2) = -a*bet/r
      AAI(1,3) = e/del + gam*(c*del - e*bet)/del/r
      AAI(2,3) = -a*gam/r
      AAI(3,3) = a*bet*gam/del/r - a/del

      PRINT*,AAI

      DO J=1,3
      DO I=1,3

      AM(I,J)=0.

      DO II=1,3
      AM(I,J) = AM(I,J) + AAI(II,J)*AA(I,II)
      ENDDO

      ENDDO
      ENDDO

      PRINT*,AM

      STOP
      END
