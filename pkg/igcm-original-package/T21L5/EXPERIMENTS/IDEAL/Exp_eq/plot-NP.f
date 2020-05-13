C#
	parameter(ng=36*14, nMax=1)	
	dimension x1(4),x2(4),y1(4),y2(4)
	real pot(36,14),
     *    x(73,15),t(72,15),y(73,37),s(72,15)
        DIMENSION u(36,19),v(36,19)
        real u9(72,15),v9(72,15)


        character*50 chm(4)
	character*22 title(18)
	character*22 ttitle
	dimension DUM,rw(5000)
        integer iw(5000),iama(20000)
        external cpdrpl
C DEFINE COLORS BLACK, RED, GREEN, YELLOW, BLUE, MAGENTA, CYAN & WHITE
      DIMENSION RED(8),GREEN(8),BLUE(8)
      DATA BLUE   /0.,1.,0.,1.,0.,1.,0.,1./
      DATA GREEN /0.,0.,1.,1.,0.,0.,1.,1./
      DATA RED  /0.,0.,0.,0.,1.,1.,1.,1./


C Declare an array to hold dense data (5152=4x4x23x14).
C
         DIMENSION ZDNS(5000)
C Declare the required real and integer workspaces.
C
         DIMENSION RWRK(5000),IWRK(1000)
C Declare an array to hold an area map.
         DIMENSION IAMA(1000000)
C Declare the arrays needed by ARSCAM for x/y coordinates.
C
         DIMENSION XCRA(50000),YCRA(50000)
C Declare the arrays needed by ARSCAM for area and group identifiers.
C
         DIMENSION IARA(20),IGRA(20)
C Declare an array to hold the GKS "aspect source flags".
C
        DIMENSION IASF(13)
C
C
        EXTERNAL DRAWCL
        EXTERNAL FILL




        data x1/.05,.5,.05,.5/,
     *       x2/.45,.9,.45,.9/,
     *       y1/.5,.5,.05,.05/,
     *       y2/.9,.9,.45,.45/


        title(1)='(AO - (-AO))/2'
        title(2)='AO + (-AO)'
        title(3)='(NAO - (-NAO))/2'
        title(4)='NAO + (-NAO)'
        title(5)='S-4'
        title(6)='S-3'
        title(7)='S-2'
        title(8)='S-1'
        title(9)='S0'
        title(10)='S1'
        title(11)='S2'
        title(12)='S3'
        title(13)='S4'
        title(14)='S5'
        title(15)='S6'
        title(16)='S7'
        title(17)='S8'
        title(18)='S9'



 	open(11,file='Zave.dat'
     *      ,form='unformatted')



	mm=0
	iic=0
	do 999 nlag=1,1
 	read (11)t

 	do j=1,15
 	do i=1,72
     	 x(i,j)=t(i,j)
 	end do
     	 x(73,j)=x(1,j)
 	end do

        call opngks
	CALL GSCLIP (0)
	call gstxfp(-7,2)
C--- DEFINE THE COLOR TABLE -------------
      DO 10 I=1,8
      CALL GSCR(1,I,RED(I),GREEN(I),BLUE(I))
 10   CONTINUE
C--- DEFINE BACKGROUND COLOR ------------
      CALL GSCR(1,0,RED(8),GREEN(8),BLUE(8))
	CALL SFLUSH


	  if(iic.gt.4)iic=iic-4
	  iic=iic+1

C Force the use of medium-quality characters by the package PLOTCHAR.
C
         CALL PCSETI ('QU - QUALITY OF CHARACTERS',1)
         call set(0.,1.,0.,1.,0.,1.,0.,1.,1)
         ttitle=title(nlag)
        call WTstr (0.45,0.99,
     *  '' ,2,0,0)
c       CALL WTSTR(x1(iic)+0.05,y2(iic)+0.02,ttitle,2,0,-1)

        CALL MAPPOS(.1,0.9,.1,0.9)
c       CALL MAPPOS(x1(iic),x2(iic),y1(iic),y2(iic))
C Create an EZMAP background.
        CALL SETUSV('LW',3000)
        CALL MAPSTI('GR',20)
        CALL MAPSTI('DO',0)
        CALL MAPSTI('EL',1)
        call mapsti('LA',0)
        CALL MAPSTC('OU','CO')
        cALL MAPROJ('ST',90.,-90.,0.)
c        cALL MAPROJ('ST',90.,180.,0.)
        CALL MAPSET('AN',70.,70.,70.,70.)
        CALL MAPDRW

C
C Tell CONPACK that the SET call has been done, force it to generate X
C coordinates that are longitudes and Y coordinates that are latitudes,
C turn on mapping to an EZMAP background, define the out-of-range value
C (returned by MAPTRN for an unprojectable point), and put the
C informational label in a different place.
C
        CALL SETUSV('LW',8000)
        call pcseti ('QU',0)
        call cpsetc ('HLT',' ')
        CALL CPSETI ('MAP - MAPPING FLAG',1)
        CALL CPSETR ('XC1 - X COORDINATE AT I = 1',0.)
        CALL CPSETR ('XCM - X COORDINATE AT I = M',360.)
        CALL CPSETR ('YC1 - Y COORDINATE AT J = 1',90.)
        CALL CPSETR ('YCN - Y COORDINATE AT J = N',20.)
        CALL CPSETI ('SET - DO SET-CALL FLAG',0)
        CALL CPSETR ('ORV - OUT-OF-RANGE VALUE',1.E12)
        CALL CPSETI ('ILP - INFORMATIONAL LABEL POSITIONING',3)
        CALL CPSETR ('ILX - INFORMATIONAL LABEL X POSITION',.5)
        CALL CPSETR ('ILY - INFORMATIONAL LABEL Y POSITION',-.02)
        CALL CPSETR ('HLS - HIGH/LOW LABEL CHARACTER SIZE',.03)
        CALL CPSETR ('T2D - TENSION ON 2D SMOOTHING',2.)

cc Label size won't work for LLP=1!!
        call cpseti ('LLP',2)
        call cpsetr ('RC1',0.10)
        call cpsetr ('RC2',0.45)
        call cpsetr ('RC3',0.10)
c       call cpseti ('LLP',1)
Cjxj
cif(iic.eq.1.or.iic.eq.3)then
        CALL CPSETR('CIS - Contour Interval Specifier', 0.)
celse
c       CALL CPSETR('CIS - Contour Interval Specifier',10.0)
c	end if

        CALL CPSETI('LIS - Label Interval Specifier',2)
        CALL CPSETR('LLS - Line Label Size',0.02)
C   Turn off the information label:
        call cpsetc('ILT',' ')
        CALL CPSETR ('HLS - HIGH/LOW LABEL CHARACTER SIZE',0.030)
C perimeter is not drawn.
        CALL SETUSV('LW',8000)
        call cprect (x,73,73,15,rw,5000,iw,5000)
        call arinam (iama,20000)
        call cppkcl(x,rw,iw)
        call cpgeti ('NCL',nclv)
        write(*,*) nclv
       do 102 iclv = 1,nclv
        call cpseti ('PAI',iclv)
        call cpgetr ('CLV',clev)
        if (clev.lt.0.) then
        call cpseti ('CLD',52428)
        call cpseti ('CLC',2)
        else
        call cpseti ('CLC',5)
        if(abs(clev).lt.0.001)call cpseti('CLC',3)
        end if
c        CALL CPSETR('LLS - Line Label Size',0.015)

  102 continue
        call cplbam (x,rw,iw,iama)
        call cpcldm (x,rw,iw,iama,cpdrpl)
        CALL CPLBDR(x,RW,IW)




        if(iic.eq.4)call frame
999	continue
        call clsgks

        close (25)
        stop
        end

#include "/diskc/hlin/ncardir/velvct.f"
