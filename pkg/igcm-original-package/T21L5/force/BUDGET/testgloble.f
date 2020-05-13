	parameter(nlag=1)
	real x0(73,37),tmp(73,37),x1(73,37)
        dimension dx1(4),dx2(4),dy1(4),dy2(4)
	real x(73,37)
        character*42 title(5)
        character*42 ttitle

       title(1)='100 mb'
       title(2)='300 mb'
       title(3)='500 mb'
       title(4)='700 mb'
       title(5)='900 mb'

                data dx1/.05,.55,.05,.55/,
     *       dx2/.47,.97,.47,.97/,
     *       dy1/.55,.55,.05,.05/,
     *       dy2/.97,.97,.47,.47/




        open(UNIT=25,FILE=
     *'fort.16', STATUS='OLD',FORM='UNFORMATTED')

	call opngks
        call mapsti ('LS',3) 

	do 999 kk=1,nlag
	read(25)x1
	do i=1,73
	do j=1,37
	  x(i,j)=x1(i,j)*24.
	end do
	end do




 	ttitle=title(kk)

        CALL SET(0.15,0.95,0.15,0.95,-1.,1.,-1.,1.,1)
c	CALL WTSTR(0.35,0.85,ttitle,2,0,-1)
        call wtstr (0.01,0.85,
     *  'Regression of DH to S2(t) ',2,0,0)
c    *  'Velocity Potential ',2,0,0)
        call wtstr (0.01,0.65,
     *  '(Vertically averaged ,Unit deg/day)',2,0,0)
c       call wtstr (0.01,0.65,
c    *  '(Vertical Averaged, Scaled by 10E+6)',2,0,0)

	CALL SETUSV('LW',3000)
        CALL SUPMAP(8,0.,180.,0.,90.,360.,-90.,0.,2,90.,2,0,IERS)
        CALL CPSETI ('SET - DO SET-CALL FLAG',0)
c     Pacific
        CALL CPSETR ('XC1 - X COORDINATE AT I = 1',0.)
        CALL CPSETR ('XCM - X COORDINATE AT I = M',360.)

        CALL CPSETR ('YC1 - Y COORDINATE AT J = 1',90.)
        CALL CPSETR ('YCN - Y COORDINATE AT J = N',-90.)
        CALL CPSETI ('MAP - MAPPING FLAG',1)
C       CALL CPSETR('ORV - OUT-OF-RANGE VALUE',1.E12)
c       CALL CPSETR('T2D - TENSION ON 2D SMOOTHING',2.)
c       CALL CPSETR ('HLS - HIGH/LOW LABEL CHARACTER SIZE',.015)
        CALL CPSETR('T2D - TENSION ON 2D SMOOTHING',2.)
        CALL CPSETR ('SPV - SPECIAL VALUE',0.)
        CALL CPSETR ('SPV - SPECIAL VALUE',8888.88)

	CALL SETUSV('LW',5000)
Cjxj
        CALL CPCNRC (x,73,73,37,0.,0.,.3,1,0,-585)
        call frame
999	continue
        call clsgks


	end
