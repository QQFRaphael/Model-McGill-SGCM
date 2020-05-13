C#
	parameter(ng=36*14, nMax=4)	
	dimension x1(4),x2(4),y1(4),y2(4)
	real pot(36,14),
     *    x(37,14),evt(ng,nMax)

        character*50 chm(4)
	character*22 title(4)
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

        title(1)=' '
        title(2)='EOF2 15% '
        title(3)='EOF3 11% '
        title(4)='EOF4 7%'


        open(11,file='EOF',form='unformatted')
	read (11) evt

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

         iic=0

         do 99 k=1,1
	 ii=0
	  if (k.eq.1) then 
         do 14 j=1,14
         do 14 i=1,36
         ii=ii+1
	  x(i,j)=-evt(ii,k)
14	continue
	  else
         do 15 j=1,14
         do 15 i=1,36
         ii=ii+1
	  x(i,j)=-evt(ii,k)
15	continue
	  end if

	do 41 j=1,14
41      x(37,j)=x(1,j)

	 iic=iic+1
	 if(iic.gt.4)iic=iic-4

C Force the use of medium-quality characters by the package PLOTCHAR.
C
         CALL PCSETI ('QU - QUALITY OF CHARACTERS',1)
         call set(0.,1.,0.,1.,0.,1.,0.,1.,1)
         ttitle=title(k)
        CALL WTSTR(x1(iic)+0.4,y2(iic)+0.02,ttitle,2,0,-1)
c       CALL WTSTR(0.2+0.05,0.8+0.02,ttitle,2,0,-1)
        call wtstr (0.45,0.99,
     *  '',2,0,0)


        CALL MAPPOS(0.2,0.8,0.2,0.8)
c       CALL MAPPOS(x1(iic),x2(iic),y1(iic),y2(iic))
C Create an EZMAP background.
        CALL SETUSV('LW',4000)
        CALL MAPSTI('GR',20)
c        CALL MAPSTI('DO',1)
        CALL MAPSTI('EL',1)
        call mapsti('LA',0)
        CALL MAPSTC('OU','CO')
        cALL MAPROJ('ST',90.,-90.,0.)
        CALL MAPSET('AN',70.,70.,70.,70.)
        CALL MAPDRW

C
C Tell CONPACK that the SET call has been done, force it to generate X
C coordinates that are longitudes and Y coordinates that are latitudes,
C turn on mapping to an EZMAP background, define the out-of-range value
C (returned by MAPTRN for an unprojectable point), and put the
C informational label in a different place.
C
        CALL SETUSV('LW',5000)
        call pcseti ('QU',0)
        call cpsetc ('HLT',' ')
        CALL CPSETI ('MAP - MAPPING FLAG',1)
        CALL CPSETR ('XC1 - X COORDINATE AT I = 1',0.)
        CALL CPSETR ('XCM - X COORDINATE AT I = M',360.)
        CALL CPSETR ('YC1 - Y COORDINATE AT J = 1',85.)
        CALL CPSETR ('YCN - Y COORDINATE AT J = N',20.)
        CALL CPSETI ('SET - DO SET-CALL FLAG',0)
        CALL CPSETR ('ORV - OUT-OF-RANGE VALUE',1.E12)
        CALL CPSETI ('ILP - INFORMATIONAL LABEL POSITIONING',3)
        CALL CPSETR ('ILX - INFORMATIONAL LABEL X POSITION',.5)
        CALL CPSETR ('ILY - INFORMATIONAL LABEL Y POSITION',-.02)
        CALL CPSETR ('HLS - HIGH/LOW LABEL CHARACTER SIZE',.015)
        CALL CPSETR ('T2D - TENSION ON 2D SMOOTHING',2.)

cc Label size won't work for LLP=1!!
        call cpseti ('LLP',2)
        call cpsetr ('RC1',0.10)
        call cpsetr ('RC2',0.45)
        call cpsetr ('RC3',0.10)
c       call cpseti ('LLP',1)
CCCjxj
        CALL CPSETR('CIS - Contour Interval Specifier',.02)
        CALL CPSETI('LIS - Label Interval Specifier',1)
        CALL CPSETR('LLS - Line Label Size',0.00)
C   Turn off the information label:
        call cpsetc('ILT',' ')
C perimeter is not drawn.
        CALL SETUSV('LW',5000)
        call cprect (x,37,37,14,rw,5000,iw,5000)
        call arinam (iama,20000)
        call cppkcl(X,rw,iw)
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
        call cplbam (X,rw,iw,iama)
        call cpcldm (X,rw,iw,iama,cpdrpl)
        CALL CPLBDR(X,RW,IW)

        if(iic.eq.4)call frame
c       call frame
99	continue
        call clsgks

        close (25)
        stop
        end


