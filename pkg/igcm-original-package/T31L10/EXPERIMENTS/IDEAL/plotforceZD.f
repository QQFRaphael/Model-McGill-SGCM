	PARAMETER ( mg=128,jg2=64,nl1=7,nl2=5)
	real*8 pot(mg,jg2)

	real x(73,37),potm(mg,jg2),fll(73,37)
	real tm(73,37)
	dimension ih(10)

	OPEN(UNIT=25,FILE=
     *'gridanom',
     &  STATUS='OLD',FORM='UNFORMATTED')

	call opngks

	do i=1,mg
	do j=1,jg2
	 potm(i,j)=0.
	end do
	end do

	do k=1,31
	READ(25)ih
	READ(25)pot
	
	if(k.gt.20)then
	do i=1,mg
	do j=1,jg2
	 potm(i,j)=potm(i,j)+pot(i,j)/10.
	end do
	end do
	end if

	end do
	
cc Convert to lat-lon grid
          call gautogrid(potm,fll)

CC unit per hour--------
	 do i=1,73
	 do j=1,37
	 tm(i,j)=fll(i,j)*86400.
	 end do
	 end do
	

CC
	do i=1,73
        do j=1,37
	x(i,j)=tm(i,j)
	end do
	end do 

	call mapsti ('LS',3)

c       CALL MAPPOS(0.05,0.95,0.1,0.85)
c        CALL SET(0.25,0.72,0.2,0.85,-1.,1.,-1.,1.,1)
	call wtstr (0.45,0.75,'Vertical averaged T forcing 
     *   with SST E2',2,0,0)


       CALL SUPMAP(8,0.,180.,0.,90.,360.,-90.,0.,2,10,2,0,IERS)
        CALL CPSETI ('SET - DO SET-CALL FLAG',0)
c     Pacific
        CALL CPSETR ('XC1 - X COORDINATE AT I = 1',0.)
        CALL CPSETR ('XCM - X COORDINATE AT I = M',360.)

        CALL CPSETR ('YC1 - Y COORDINATE AT J = 1',90.)
        CALL CPSETR ('YCN - Y COORDINATE AT J = N',-90.)
        CALL CPSETI ('MAP - MAPPING FLAG',1)
C       CALL CPSETR('ORV - OUT-OF-RANGE VALUE',1.E12)
c        CALL CPSETR ('HLS - HIGH/LOW LABEL CHARACTER SIZE',.03)
        CALL CPSETR('T2D - TENSION ON 2D SMOOTHING',2.)
c        call cpsetc('ILT',' ')

        CALL CPCNRC (x,73,73,37,0.,0.,0.2,1,0,-585)
c       CALL SET(0.05,0.95,0.375,0.625,0.,360.,0.,90.,1)
c        CALL LABMOD('(F5.0)','(F5.0)',0,0,0,0,0,0,0)
c        CALL PERIML(6,1,5,1)
        call frame
        call clsgks

        stop
        end


#include "/diska/hlin/igcm/force/gausstogrid128.f"
