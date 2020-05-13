      PROGRAM CONTROL
**    Function - to control the creation of a data file for input to the
**          Flux Program; user-written data will be read from unit 10,
**          and output data will be written to channel 11
**    Com changed - /ERRMSG/all elements
**    Calls - COMMRD,COMCHK,MCONRD,MCONCK,DIAGRD,DIAGCK,COMMWT,MCONWT,
**          DIAGWT,ERRSTP
**    Files used - 10(input data),11(namelist output),12(work file),
**          NCERR(error messages)
**    Author - R. Brugge, University of Reading

      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LOEXIS

**    Initialise /ERRMSG/
      NCERR=2
      NFATAL=0
      NWARN=0

**    Check that unit 10 exists
      INQUIRE(FILE='fort.10',ERR=100,EXIST=LOEXIS)
      IF(.NOT.LOEXIS) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6010)
6010     FORMAT(1X,'CONTROL: ERROR UNIT 10 DOES NOT EXIST')
         CALL ERRSTP
      ENDIF

*********************************************************
**    Notes : input datafile must contain UPPER-CASE only
**            no blank lines allowed in the input file
**            no blanks allowed after '=' sign
**            continuation symbol is a comma (,) as the
**                  final character
**            LOGICAL-type entries can be one of:
**                  .T.  T  .TRUE.  TRUE   (likewise for
**                  false)
**            multiple entries are separated by commas
**            keyword value ALL selects all possible
**                  values for an array
*********************************************************
**    Read global information relating to diagnostics
      CALL COMMRD
**    Check global information to unit 11
      CALL COMCHK

**    Read model/model job specifications
      CALL MCONRD
**    Check model/model job specifications
      CALL MCONCK

**    Now read information pertaining to diagnostic fields
      CALL DIAGRD
**    Check diagnostic field data
      CALL DIAGCK

**    Namelists only created if no fatal errors found
      IF(NFATAL.GT.0) CALL ERRSTP
**    Write out global information
      CALL COMMWT
**    Write out model/model job specifications
      CALL MCONWT
**    Write out diagnostic field data to unit 11
      CALL DIAGWT

**    The following provides a record that can be searched for to
**    check that the namelist file has been successfully composed
      WRITE(11,6100)
6100  FORMAT(1X,'******END-OF-NAMELIST-FILE*****')

      REWIND 11
      WRITE(NCERR,6200) NFATAL,NWARN
6200  FORMAT(1X,'***** ',I5,' FATAL ERRORS FOUND ',
     -      I5,' WARNING ERRORS FOUND ')
      STOP

100   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6000)
6000  FORMAT(1X,'CONTROL: ERROR FOUND WHEN SEARCHING FOR UNIT 10')
      CALL ERRSTP
      END
      SUBROUTINE AVGCHK(LPMASS,LPMSK,LPSIG,YPTYP,KERR)
**    Function - to check mask and weighting factors
**    Args in -
**               LPMASS  - .TRUE. if user has requested that SG
**                         fields be multiplied by the isentropic
**                         mass factor (sigma) (=LMASSW)
**               LPMSK   - .TRUE. if user has requested use of
**                         land/air mask in the calculation of
**                         averages (eg spatial and temporal) (=LAVMSK)
**               LPSIG   - .TRUE. if user has requested use of
**                         sigma-weighting when computing isentropic
**                         surface averages (=LAVSIG)
**               YPTYP   - CHARACTER*2 surface type
**               KERR    - Error flag
**    Args out -
**               LPMASS  - changed to .FALSE. if output is not on
**                         isentropic surfaces
**               LPMSK   - changed to .FALSE. if output is on model
**                         levels, and currently set to .FALSE. if
**                         output is on pressure levels
**               LPSIG   - changed to .FALSE. if output not on
**                         isentropic surfaces, or if LPMASS is .TRUE.
**               KERR    - Set to 999 if possible conflicting flags
**                         set by the user
**    Args for work - none
**    Com used - /ERRMSG/NCERR
**    Com changed - /ERRMSG/NFATAL,NWARN
**    Called by - COMCHK
**    Calls - none
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      CHARACTER*2 YPTYP
      LOGICAL LPMASS,LPMSK,LPSIG

**    It should be noted that the value of LLAIRL set in the flux
**    program should be related to which of the above logical flags
**    are allowed on the output surfaces.

**    Currently, land/air masks are not employed when computing
**    averages on pressure surfaces. This is for compatibility with
**    the old UFLUX2H program. Thus LPMSK should be set to .FALSE.
**    for compatibility when output is on pressure surfaces.

**    Currently, LPSIG and LPMSK are only employed in the
**    calculation of temporal, zonal mean and meridional mean averages.

**    Unless output surface type is isentropic, LPMASS must be .FALSE.
      IF(YPTYP.NE.'TH'.AND.LPMASS) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6000)
6000     FORMAT(1X,'AVGCHK: OUTPUT SURFACE TYPE NOT ISENTROPIC, BUT',
     -         ' USER HAS REQUESTED USE OF SIGMA-WEIGHTED FIELDS',
     -         /,' SIGMA-WEIGHTED FIELDS NOT PRODUCED; LMASSW=.FALSE.')
         LPMASS=.FALSE.
      ENDIF

**    Unless output surface type is isentropic, LPSIG must be .FALSE.
      IF(YPTYP.NE.'TH'.AND.LPSIG) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6010)
6010     FORMAT(1X,'AVGCHK: OUTPUT SURFACE TYPE NOT ISENTROPIC, BUT',
     -         ' USER HAS REQUESTED USE OF SIGMA-WEIGHTED AVERAGES',
     -         /,' SIGMA-WEIGHTED FIELDS NOT PRODUCED; LAVSIG=.FALSE.')
         LPSIG=.FALSE.
      ENDIF

**    When output is on model surfaces, there is no need to use the
**    land/air mask; thus LPMSK should be set to .FALSE.
      IF(YPTYP.EQ.'ET'.AND.LPMSK) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6020)
6020     FORMAT(1X,'AVGCHK: OUTPUT SURFACE TYPE IS SIGMA, SO',
     -         ' NO NEED TO SET LAVMSK=.TRUE.')
         LPMSK=.FALSE.
      ENDIF

**    (TEMPORARY CHANGE ONLY - see value of LLAIRL in UFLUX3 program)
**    Land/air mask not used when output is on pressure surfaces.
      IF(YPTYP.EQ.'IS'.AND.LPMSK) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6030)
6030     FORMAT(1X,'AVGCHK: OUTPUT SURFACE TYPE IS PRESSURE:',
     -         ' PROGRAM SETS LAVMSK=.TRUE. FOR COMPATIBILTY WITH ',
     -         /,' OLD VERSION OF FLUX PROGRAM')
         LPMSK=.FALSE.
      ENDIF

**    Possible error if user has set LMASSW and LAVSIG both .TRUE.
      IF(LPSIG.AND.LPMASS) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6040)
6040     FORMAT(1X,'AVGCHK: ERROR - BOTH LMASSW AND LAVSIG HAVE BEEN',
     -         ' SET .TRUE.')
         LPSIG=.FALSE.
         KERR=999
      ENDIF

**    Possible error if user has set LAVMSK and LAVSIG both .TRUE.
      IF(LPSIG.AND.LPMSK) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6050)
6050     FORMAT(1X,'AVGCHK: ERROR - BOTH LAVMSK AND LAVSIG HAVE BEEN',
     -         ' SET .TRUE.')
         LPMSK=.FALSE.
         KERR=999
      ENDIF

      RETURN
      END
      SUBROUTINE COMCHK
**    Function - to check global information from the head of the
**          user-created data file
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /COMPTH/all elements,/COMDAT/BEGDAY,ENDDAY,LWRITE,
**          LASCII,LUTF14,NFREQA,NFREQD,NFREQP,NASCII,
**          /ERRMSG/NCERR
**    Com changed - /ERRMSG/NFATAL,NWARN
**    Called by - CONTROL
**    Calls - ERRSTP
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      CHARACTER*80 YCPTHT,YCPTHH,YCPTHC
      COMMON/COMPTH/YCPTHT,YCPTHH,YCPTHC
      LOGICAL LWRITE,LASCII,LSCRAT,LUTF14,LROGR,LMASSW,LAVMSK,
     -      LAVSIG
      CHARACTER*2 YTYPSF
      CHARACTER*6 YDUM6
      CHARACTER*8 YNAME(2)
      COMMON /COMDAT/BEGDAY,ENDDAY,LWRITE,LASCII,LSCRAT,
     -      LUTF14,LROGR,NSIGFG,NFREQD,NFREQA,NFREQP,RNTAPE,
     -      NASCII,LMASSW,LAVMSK,LAVSIG,YNAME,YTYPSF,YDUM6
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LOEXIS
      CHARACTER*20 YOBLAN
      CHARACTER*80 YOBLNK
      DATA YOBLAN/'                    '/

      IERR=0

**    Check that unit 11 does not exist
      INQUIRE(FILE='fort.11',ERR=100,EXIST=LOEXIS)
      IF(LOEXIS) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6000)
6000     FORMAT(1X,'COMCHK: WARNING UNIT 11 ALREADY EXISTS',
     -         /,'       AND WILL BE OVERWRITTEN')
         REWIND 11
      ENDIF

**    Now check the data for namelist CONTRL
**    Path variables should not be empty
      YOBLNK=YOBLAN//YOBLAN//YOBLAN//YOBLAN
      IF((YCPTHT.EQ.YOBLNK).OR.(YCPTHH.EQ.YOBLNK)
     -      .OR.(YCPTHC.EQ.YOBLNK))THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6010)
6010     FORMAT(1X,'COMMWD: ONE OF THE PATH VARIABLES HAS NOT BEEN SET')
         IERR=999
      ENDIF

**    Check for possible errors in BEGDAY and ENDDAY
      IF((BEGDAY.LT.0.0.AND.BEGDAY.GT.-2.0).OR.
     -      (ENDDAY.LT.0.0.AND.ENDDAY.GT.-2.0)) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6020)
6020     FORMAT(1X,'ONE OR BOTH OF BEGDAY AND ENDDAY NOT SET')
         IERR=999
      ENDIF
      IF(ABS(ENDDAY).LT.ABS(BEGDAY)) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6030)
6030     FORMAT(1X,'BEGDAY .GT. ENDDAY')
         IERR=999
      ENDIF

**    Check that one or both of LWRITE and LASCII has been set to .TRUE.
      IF((.NOT.LWRITE).AND.(.NOT.LASCII)) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6040)
6040     FORMAT(1X,'NO OUTPUT REQUESTED')
         IERR=999
      ENDIF
**    If LASCII is .TRUE., ensure that LUTF14 is set
**    to .TRUE.
      IF(LASCII) THEN
         IF(.NOT.LUTF14)THEN
            NFATAL=NFATAL+1
            WRITE(NCERR,6060)
6060        FORMAT(1X,'LASCII = .TRUE., AND NO UTF VERSION REQUESTED')
            IERR=999
         ENDIF
      ENDIF

**    Check that NASCII is non-zero
      IF(NASCII.EQ.0) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6065)
6065     FORMAT(1X,'NASCII CANNOT BE ZERO - RESET TO 2')
         NASCII=2
      ENDIF

**    Check that NSIGFG is one of 2, 3, 4 or 5
      IF(NSIGFG.LT.2) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6080)
6080     FORMAT(1X,'NSIGFG TOO SMALL - RESET TO 2')
         NSIGFG=2
      ELSE IF(NSIGFG.GT.5) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6090)
6090     FORMAT(1X,'NSIGFG TOO LARGE - RESET TO 5')
         NSIGFG=5
      ENDIF

**    Check that NFREQD >0
      IF(NFREQD.LE.0) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6100)
6100     FORMAT(1X,'NFREQD MUST BE > 0')
         IERR=999
      ENDIF

**    Check that NFREQP and NFREQA are consistent with LWRITE,
**          LASCII
      IF(LASCII.AND.(NFREQA.LE.0)) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6110)
6110     FORMAT(1X,'UTF WRITING REQUIRED, BUT NFREQA <= 0',
     -         ': PROGRAM STOPPED')
         IERR=999
      ENDIF
      IF(LWRITE.AND.(NFREQP.LE.0)) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6120)
6120     FORMAT(1X,'*** WARNING : PRINTOUTS REQUIRED BUT NFREQP ',
     -         '<= 0: SOME PRINTOUTS MAY NOT BE PRODUCED')
      ENDIF

**    Check averaging and weighting masks
      CALL AVGCHK(LMASSW,LAVMSK,LAVSIG,YTYPSF,IERR)

CC    IF(IERR.EQ.999) STOP
      RETURN

100   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6070)
6070  FORMAT(1X,'COMCHK: ERROR FOUND WHEN SEARCHING FOR UNIT 11')
      CALL ERRSTP

      END
      SUBROUTINE COMMRD
**    Function - to process global information at the head of the
**          user-created data file for the preprocessor
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /ERRMSG/NCERR
**    Com changed - /ERRMSG/NFATAL
**    Called by - CONTROL
**    Calls - SEARCH,GLBDEF,EXTRAC,ERRSTP
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      CHARACTER*80 YOCREC
      LOGICAL LOFIND

      REWIND 10
**    Search for record containing 'COMMON'
**    Read  record
200   CONTINUE
      READ (10,5000,END=100,ERR=110)YOCREC
5000  FORMAT(A80)
      CALL SEARCH(YOCREC,'COMMON',LOFIND)
      IF(.NOT.LOFIND) GO TO 200

**    'COMMON' found; read following records and begin construction
**          of unit 11
**    But first set up default data values
      CALL GLBDEF
**    Read and interpret records until 'ENDCOMMON' found
300   CONTINUE
      READ(10,5000,END=120,ERR=130)YOCREC
      CALL SEARCH(YOCREC,'ENDCOMMON',LOFIND)
      IF(LOFIND) THEN
**       'ENDCOMMON' found
         RETURN
      ENDIF

      CALL EXTRAC(YOCREC)
      GO TO 300

100   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6000)
6000  FORMAT(1X,'COMMRD: EOF WHEN SEARCHING FOR **COMMON**')
      CALL ERRSTP

110   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6010)
6010  FORMAT(1X,'COMMRD: ERROR WHEN SEARCHING FOR **COMMON**')
      CALL ERRSTP

120   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6020)
6020  FORMAT(1X,'COMMRD: EOF WHEN SEARCHING FOR **COMMON** DATA')
      CALL ERRSTP

130   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6030)
6030  FORMAT(1X,'COMMRD: ERROR WHEN SEARCHING FOR **COMMON** DATA')
      CALL ERRSTP

      END
      SUBROUTINE COMMWT
**    Function - to write global information from the head of the
**          user-created data file
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /COMPTH/all elements,/COMDAT/all elements,
**          /DIAGTP/all elements
**    Com changed - none
**    Called by - CONTROL
**    Calls - none
**    Files read - none
**    Files written - 11
**    Author - R. Brugge, University of Reading

      CHARACTER*80 YCPTHT,YCPTHH,YCPTHC
      COMMON/COMPTH/YCPTHT,YCPTHH,YCPTHC
      LOGICAL LWRITE,LASCII,LSCRAT,LUTF14,LROGR,LMASSW,LAVMSK,
     -      LAVSIG
      CHARACTER*2 YTYPSF
      CHARACTER*6 YDUM6
      CHARACTER*8 YNAME(2)
      COMMON /COMDAT/BEGDAY,ENDDAY,LWRITE,LASCII,LSCRAT,
     -      LUTF14,LROGR,NSIGFG,NFREQD,NFREQA,NFREQP,RNTAPE,
     -      NASCII,LMASSW,LAVMSK,LAVSIG,YNAME,YTYPSF,YDUM6
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      NAMELIST /GLOBAL/BEGDAY,ENDDAY,YTYPSF,LWRITE,LASCII,LSCRAT,
     -      LUTF14,LROGR,NSIGFG,NFREQD,NFREQP,NFREQA,RNTAPE,
     -      YNAME,NASCII,LMASSW,LAVMSK,LAVSIG
      NAMELIST /FLXTYP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,
     -      LDTJ

      REWIND 11
      WRITE(11,6000)YCPTHT
      WRITE(11,6000)YCPTHH
      WRITE(11,6000)YCPTHC
6000  FORMAT(A)
      WRITE(11,GLOBAL)
      WRITE(11,FLXTYP)

      RETURN
      END
      SUBROUTINE GLBDEF
**    Function - to specify default global data and update
**          appropriate common blocks
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - none
**    Com changed - /COMPTH/all elements,/COMDAT/all elements,
**          /DIAGTP/all elements,/ALLOWC/all elements,/ALLOWI/
**          all elements
**    Called by - COMMRD
**    Calls - none
**    Files read - none
**    Files written - none
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNSFC=3,JPDTYP=9,JPTRNC=4)
      CHARACTER*2 YSFC,YDTYP
      COMMON /ALLOWC/YSFC(JPNSFC),YDTYP(JPDTYP)
      COMMON /ALLOWI/NITRNC(JPTRNC)
      CHARACTER*80 YCPTHT,YCPTHH,YCPTHC
      COMMON/COMPTH/YCPTHT,YCPTHH,YCPTHC
      LOGICAL LWRITE,LASCII,LSCRAT,LUTF14,LROGR,LMASSW,LAVMSK,
     -      LAVSIG
      CHARACTER*2 YTYPSF
      CHARACTER*6 YDUM6
      CHARACTER*8 YNAME(2)
      COMMON /COMDAT/BEGDAY,ENDDAY,LWRITE,LASCII,LSCRAT,
     -      LUTF14,LROGR,NSIGFG,NFREQD,NFREQA,NFREQP,RNTAPE,
     -      NASCII,LMASSW,LAVMSK,LAVSIG,YNAME,YTYPSF,YDUM6
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      CHARACTER*1 YOBLNK
**    Permitted values declared
      DATA YSFC(1)/'IS'/,YSFC(2)/'ET'/,YSFC(3)/'TH'/
      DATA YDTYP(1)/'PH'/,YDTYP(2)/'XP'/,YDTYP(3)/'CO'/,
     -      YDTYP(4)/'SG'/,YDTYP(5)/'TR'/,YDTYP(6)/'TF'/,
     -      YDTYP(7)/'OD'/,YDTYP(8)/'ZF'/,YDTYP(9)/'TJ'/
      DATA NITRNC/21,42,63,106/
**    Default values assigned
**    Switch on:  printing
**                UTF version 1.4
**                UTF creation
**                inclusion of orography in calculations
      DATA LWRITE/.TRUE./,LUTF14/.TRUE./,LASCII/.TRUE./
**    Switch off: use of scratch files for large arrays
**                UTF version 1.3
**                all diagnostics
      DATA LSCRAT/.FALSE./,LROGR
     -      /.TRUE./,LDPH/.FALSE./,LDXP/.FALSE./,
     -      LDCO/.FALSE./,LDSG/.FALSE./,LDTR/.FALSE./,LDTF/.FALSE./,
     -      LDOD/.FALSE./,LDZF/.FALSE./,LDTJ/.FALSE./
**    Default surface type is the model surface (ET) and to
**          YNAME
      DATA YTYPSF/'ET'/,YNAME(1)/'        '/,YNAME(2)/'        '/
**    Printout to 2 sig figs by default
      DATA NSIGFG/2/
**    Default job number
      DATA RNTAPE/0.0/
**    Default to 2 characters for UTF coding
      DATA NASCII/2/
**    No mass-weighted variables for theta-surface output
      DATA LMASSW/.FALSE./
**    Land/air masking in computation of isentropic averages
      DATA LAVMSK/.FALSE./
**    Isentropic sigma weighting switched off when computing averages
      DATA LAVSIG/.FALSE./

      YOBLNK=' '
**    Pad out directory pathnames to 80 characters - these must be
**          altered by the user
      WRITE(YCPTHT,'(80A1)')(YOBLNK,I=1,80)
      WRITE(YCPTHH,'(80A1)')(YOBLNK,I=1,80)
      WRITE(YCPTHC,'(80A1)')(YOBLNK,I=1,80)

**    Preset beginning and end days to negative values: these must be
**          altered by the user
      BEGDAY=-1.0
      ENDDAY=-1.0

**    Specify null values for frequencies of sampling, printouts and
**          UTF production - the former must be changed by the user
      NFREQD=-1
      NFREQP=0
      NFREQA=0
      RETURN
      END
      SUBROUTINE EXTRAC(YPCREC)
**    Function - to extract information from global data and update
**          appropriate common blocks
**    Args in -
**               YPCREC  - character*80 variable containing data
**    Args out - none
**    Args for work - none
**    Com used - /ALLOWC/all elements,/ALLOWI/all elements,
**          /ERRMSG/NCERR
**    Com changed - /COMPTH/all elements,/COMDAT/all elements,
**          /DIAGTP/all elements,/ERRMSG/NFATAL
**    Called by - COMMRD
**    Calls - SEARCH,LASTCH,RTRANS,ITRANS,LPROC,ERRSTP
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNSFC=3,JPDTYP=9,JPTRNC=4)
      CHARACTER*2 YSFC,YDTYP
      COMMON /ALLOWC/YSFC(JPNSFC),YDTYP(JPDTYP)
      COMMON /ALLOWI/NITRNC(JPTRNC)
      CHARACTER*80 YCPTHT,YCPTHH,YCPTHC
      COMMON/COMPTH/YCPTHT,YCPTHH,YCPTHC
      LOGICAL LWRITE,LASCII,LSCRAT,LUTF14,LROGR,LMASSW,LAVMSK,
     -      LAVSIG
      CHARACTER*2 YTYPSF
      CHARACTER*6 YDUM6
      CHARACTER*8 YNAME(2)
      COMMON /COMDAT/BEGDAY,ENDDAY,LWRITE,LASCII,LSCRAT,
     -      LUTF14,LROGR,NSIGFG,NFREQD,NFREQA,NFREQP,RNTAPE,
     -      NASCII,LMASSW,LAVMSK,LAVSIG,YNAME,YTYPSF,YDUM6
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LODTYP(JPDTYP)
      EQUIVALENCE(LDPH,LODTYP(1)),(LDXP,LODTYP(2)),
     -      (LDCO,LODTYP(3)),(LDSG,LODTYP(4)),(LDTR,LODTYP(5)),
     -      (LDTF,LODTYP(6)),(LDOD,LODTYP(7)),(LDZF,LODTYP(8)),
     -      (LDTJ,LODTYP(9))
      LOGICAL LOFIND,LOVALD
      EQUIVALENCE(YNAME,YONAME)
      CHARACTER*1 YOEQL
      CHARACTER*3 YOKW3
      CHARACTER*4 YOKW4
      CHARACTER*5 YOKW5
      CHARACTER*6 YOKW6
      CHARACTER*7 YOKW7
      CHARACTER*8 YONAME(2)
      CHARACTER*9 YOKW9
      CHARACTER*10 YOKW10
      CHARACTER*11 YOKW11
      CHARACTER*12 YOKW12
      CHARACTER*80 YPCREC

      YOEQL='='
**    Search for the keyword in CREC and then determine the field value
      LOFIND=.FALSE.

      YOKW11='PRINTSIGFIG'
      CALL SEARCH(YPCREC,YOKW11,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
         IF(IERR.EQ.998) GO TO 1000
         CALL ITRANS(YPCREC,IPOSN,IPOSX,NSIGFG,12)
         RETURN
      ENDIF

      YOKW10='ASCIICHARS'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
         IF(IERR.EQ.998) GO TO 1000
         CALL ITRANS(YPCREC,IPOSN,IPOSX,NASCII,12)
         RETURN
      ENDIF

      YOKW12='FREQSAMPLING'
      CALL SEARCH(YPCREC,YOKW12,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
         IF(IERR.EQ.998) GO TO 1000
         CALL ITRANS(YPCREC,IPOSN,IPOSX,NFREQD,12)
         RETURN
      ENDIF

      YOKW9='FREQPRINT'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
         IF(IERR.EQ.998) GO TO 1000
         CALL ITRANS(YPCREC,IPOSN,IPOSX,NFREQP,12)
         RETURN
      ENDIF

      YOKW7='FREQUTF'
      CALL SEARCH(YPCREC,YOKW7,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
         IF(IERR.EQ.998) GO TO 1000
         CALL ITRANS(YPCREC,IPOSN,IPOSX,NFREQA,12)
         RETURN
      ENDIF

      YOKW7='JOBNAME'
      CALL SEARCH(YPCREC,YOKW7,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
         IF(IERR.EQ.998) GO TO 1000
         ICHARN=IPOSX-IPOSN+1
         I1=IPOSN
         IY=MIN(8,ICHARN)
         I2=IY+IPOSN-1
         YONAME(1)(1:IY)=YPCREC(I1:I2)
         IF(ICHARN.GE.9) THEN
            I1=IPOSN+8
            IY=MIN(16,ICHARN)
            I2=IY+IPOSN-1
            YONAME(2)(1:IY-8)=YPCREC(I1:I2)
         ENDIF
         RETURN
      ENDIF

      YOKW7='TEMPDIR'
      CALL SEARCH(YPCREC,YOKW7,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
         IF(IERR.NE.0) GO TO 1000
         YCPTHT=YPCREC(IPOSN:IPOSX)
         RETURN
      ENDIF

**    Else
      YOKW7='HOMEDIR'
      CALL SEARCH(YPCREC,YOKW7,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
         IF(IERR.NE.0) GO TO 1000
         YCPTHH=YPCREC(IPOSN:IPOSX)
         RETURN
      ENDIF

**    Else
      YOKW10='CURRENTDIR'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
         IF(IERR.NE.0) GO TO 1000
         YCPTHC=YPCREC(IPOSN:IPOSX)
         RETURN
      ENDIF

**    Else
      YOKW6='BEGDAY'
      CALL SEARCH(YPCREC,YOKW6,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
         IF(IERR.NE.0) GO TO 1000
         CALL RTRANS(YPCREC,IPOSN,IPOSX,BEGDAY,12)
         RETURN
      ENDIF

**    Else
      YOKW9='JOBNUMBER'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
         IF(IERR.EQ.998) GO TO 1000
         CALL RTRANS(YPCREC,IPOSN,IPOSX,RNTAPE,12)
         RETURN
      ENDIF

**    Else
      YOKW6='ENDDAY'
      CALL SEARCH(YPCREC,YOKW6,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
         IF(IERR.NE.0) GO TO 1000
         CALL RTRANS(YPCREC,IPOSN,IPOSX,ENDDAY,12)
         RETURN
      ENDIF

**    Else
      YOKW9='LEVELTYPE'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         IPOSX=IPOSN+1
         LOVALD=.FALSE.
         DO 2000 K=1,JPNSFC
2000     IF(YPCREC(IPOSN:IPOSX).EQ.YSFC(K))LOVALD=.TRUE.
         IF(LOVALD) THEN
            YTYPSF=YPCREC(IPOSN:IPOSX)
         ELSE
            CALL INVALD(YPCREC)
         ENDIF
         RETURN
      ENDIF

**    Else
      YOKW5='PRINT'
      CALL SEARCH(YPCREC,YOKW5,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LWRITE)
         RETURN
      ENDIF

**    Else
      YOKW4='PLOT'
      CALL SEARCH(YPCREC,YOKW4,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LASCII)
         RETURN
      ENDIF

**    Else
      YOKW10='MASSWEIGHT'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LMASSW)
         RETURN
      ENDIF

**    Else
      YOKW11='MASKAVERAGE'
      CALL SEARCH(YPCREC,YOKW11,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LAVMSK)
         RETURN
      ENDIF

**    Else
      YOKW12='SIGMAAVERAGE'
      CALL SEARCH(YPCREC,YOKW12,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LAVSIG)
         RETURN
      ENDIF

**    Else
      YOKW7='SCRATCH'
      CALL SEARCH(YPCREC,YOKW7,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LSCRAT)
         RETURN
      ENDIF

**    Else
      YOKW9='OROGRAPHY'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LROGR)
         RETURN
      ENDIF

**    Else
      YOKW3='UTF'
      CALL SEARCH(YPCREC,YOKW3,LOFIND)
      IF(LOFIND) THEN
**       IPOS1 is the location of first character defining field value
         IPOS1=INDEX(YPCREC,YOEQL)+1
         IPOS3=IPOS1+2
         IF(YPCREC(IPOS1:IPOS3).EQ.'1.4') THEN
            LUTF14=.TRUE.
         ELSE
            CALL INVALD(YPCREC)
         ENDIF
         RETURN
      ENDIF

**    Else
      YOKW11='DIAGNOSTICS'
      CALL SEARCH(YPCREC,YOKW11,LOFIND)
      IF(LOFIND) THEN
**       I1 is the location of first character defining field value
         I1=INDEX(YPCREC,YOEQL)+1
**       I2 is the location of last character defining field value
         CALL LASTCH(YPCREC,80,I1,I2,IERR)
         IF(IERR.NE.0) GO TO 1000
         IF(((I2-I1+1).NE.2).AND.((MOD(I2-I1+2,3)).NE.0))
     -         CALL INVALD(YPCREC)
         IKOUNT=0
         DO 2500 J=1,JPDTYP
         DO 2500 I=I1,I2,3
         IF(YDTYP(J).EQ.YPCREC(I:I+1))THEN
            LODTYP(J)=.TRUE.
            IKOUNT=IKOUNT+1
         ENDIF
2500     CONTINUE
         IF(IKOUNT.NE.(I2-I1+2)/3) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6010) IKOUNT,YPCREC
6010     FORMAT(1X,'ONLY ',I3,' DATA VALUES EXTRACTED FROM ',/A)
         CALL ERRSTP
         ENDIF
      RETURN
      ENDIF

**    Else
**    Keyword not located
      NFATAL=NFATAL+1
      WRITE(NCERR,6020) YPCREC
6020  FORMAT(1X,'KEYWORD NOT LOCATED FOR PHRASE :'/A)
      CALL ERRSTP

1000  CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6000) YPCREC
6000  FORMAT(1X,'EXTRAC: ERROR IN GLOBAL DATA ENTRY',/A)
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE CHKVAL(LPCODE,KFIELD,KDIM1,KALLOW,KDIM2)
**    Function - to check the values in one array against the values
**          in another, and return a flag indicating if the first
**          is a subset of the second.
**    Args in -
**               KFIELD  - Array of values to be checked
**               KDIM1   - Number of values to be checked
**               KALLOW  - Array of permissible values
**               KDIM2   - Number of permissible values
**    Args out -
**               LPCODE  - .TRUE. if values in array KFIELD are a
**                         subset of those in KALLOW
**    Args for work - none
**    Com used - none
**    Com changed - none
**    Params used - none
**    Called by - CHKTF
**    Calls - none
**    Files read - none
**    Files written - none
**    Author - R. Brugge, University of Reading

      LOGICAL LPCODE,LOVALU
      DIMENSION KFIELD(KDIM1),KALLOW(KDIM2)

      LPCODE=.TRUE.
      DO 100 J=1,KDIM1
      LOVALU=.FALSE.
      DO 200 JC=1,KDIM2
      IF(KFIELD(J).EQ.KALLOW(JC)) LOVALU=.TRUE.
200   CONTINUE
      IF(.NOT.LOVALU) THEN
         LPCODE=.FALSE.
         RETURN
      ENDIF
100   CONTINUE

      RETURN
      END
      SUBROUTINE ERRSTP
**    Function - to stop program and print out message in the event
**          of job failure
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /ERRMSG/all elements
**    Com changed - none
**    Called by - DCODTF,SDCDTF,TFEXTR,TFPROC,FHEXTR,FHPROC,
**          FZEXTR,FZPROC,FMEXTR,FMPROC,FPEXTR,FPPROC,F3EXTR,
**          F3PROC,CONTROL,COMCHK,COMMRD,EXTRAC,IMULTP,INVALD,
**          ITRANM,LPROC,RMULTP,RTRANM,DIAGRD,PHEXTR,PHPROC,
**          MCONRD,MCEXTR,XPEXTR,XPPROC,LVPROC,
**          OREXTR,ORPROC,COEXTR,COPROC,SGEXTR,SGPROC,GHEXTR,
**          GHPROC,GZEXTR,GZPROC,GMEXTR,GMPROC,GPEXTR,GPPROC,
**          G3EXTR,G3PROC,TREXTR,TRPROC,THEXTR,THPROC,TZEXTR,
**          TZPROC,TMEXTR,TMPROC,TPEXTR,TPPROC,T3EXTR,T3PROC
**    Calls - none
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading
      COMMON /ERRMSG/NCERR,NFATAL,NWARN

      WRITE(NCERR,6200) NFATAL,NWARN
6200  FORMAT(1X,'***** ',I5,' FATAL ERRORS FOUND ',
     -      I5,' WARNING ERRORS FOUND ')
      WRITE(NCERR,6300)
6300  FORMAT(1X,'*********************************')
      WRITE(NCERR,6310)
6310  FORMAT(1X,'***** PRESMAP ABORTED PREMATURELY')
      WRITE(NCERR,6320)
6320  FORMAT(1X,'*********************************')

      STOP
      END
      SUBROUTINE FRSTCH(YPCREC,KNCHR,KI1,LPINTG)
**    Function - To locate the first non-blank character element in
**          a string and determine if it is an integer and/or a
**          'minus' sign
**    Args in -
**               YPCREC  - character variable
**               KNCHR   - number of elements in YPCREC
**    Args out -
**               KI1     - location of first non-blank element in
**                         YPCREC.  Returns a value of 0 if first KNCHR
**                         elements are blank
**               LPINTG  - .TRUE. if this element is an integer and/or
**                         a 'minus' sign
**    Args for work - none
**    Com used - none
**    Com changed - none
**    Called by - RMULTP,IMULTP
**    Calls - none
**    Author - R. Brugge, University of Reading

      CHARACTER*(*) YPCREC
      LOGICAL LPINTG

      KI1=0
      LPINTG=.FALSE.
      DO 100 J=1,KNCHR
      IF(YPCREC(J:J).NE.' ') THEN
         KI1=J
         GO TO 200
      ENDIF
100   CONTINUE
200   CONTINUE
      IF((YPCREC(KI1:KI1).EQ.'0').OR.(YPCREC(KI1:KI1).EQ.'1').OR.
     -      (YPCREC(KI1:KI1).EQ.'2').OR.(YPCREC(KI1:KI1).EQ.'3').OR.
     -      (YPCREC(KI1:KI1).EQ.'4').OR.(YPCREC(KI1:KI1).EQ.'5').OR.
     -      (YPCREC(KI1:KI1).EQ.'6').OR.(YPCREC(KI1:KI1).EQ.'7').OR.
     -      (YPCREC(KI1:KI1).EQ.'8').OR.(YPCREC(KI1:KI1).EQ.'9')) THEN
         LPINTG=.TRUE.
      ENDIF
      IF(YPCREC(KI1:KI1).EQ.'-') LPINTG=.TRUE.
      RETURN
      END
      SUBROUTINE IMULTP(YPCREC,YPEQL,YPCOMA,KX,KXDIM)
**    Function - to control the processing of a sequence of integer
**          data values in (possibly) a sequence of keyword data
**          records
**    Args in -
**               YPCREC  - character variable
**               YPCOMA  - character*1 containing ','
**               YPEQL   - character*1 containing '='
**               KXDIM   - dimension of KX
**    Args out -
**               KX      - integer array containing values extracted
**                         from keyword
**    Args for work - none
**    Com used - /ERRMSG/NCERR
**    Com changed - /ERRMSG/NFATAL
**    Called by - PHEXTR
**    Calls - LASTCH,ITRANM,FRSTCH,INVALD,ERRSTP
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      CHARACTER*(80) YPCREC
      CHARACTER*1 YPEQL,YPCOMA
      LOGICAL LOCONT,LOINTG
      DIMENSION KX(KXDIM)

**    I1 is the location of first character defining field value
      I1=INDEX(YPCREC,YPEQL)+1
**    IFIELD - number of data values for current field KX read in so far
      IFIELD=0
**    I2 is the location of last character defining field value
300   CONTINUE
      CALL LASTCH(YPCREC,80,I1,I2,IERR)
      IF(IERR.EQ.998) GO TO 1000
**    If the last character is a comma - expect a continuation line
      IF(YPCREC(I2:I2).EQ.YPCOMA) THEN
         LOCONT=.TRUE.
      ELSE
         LOCONT=.FALSE.
      ENDIF
      IF(.NOT.LOCONT) THEN
**       Decode a single line of data
         CALL ITRANM(YPCREC,I1,I2,YPCOMA,IFIELD,KX,KXDIM,12)
         RETURN
      ELSE
**       Decode one of several data lines
         CALL ITRANM(YPCREC,I1,I2-1,YPCOMA,IFIELD,KX,KXDIM,12)
**       Read the next line of data
         READ(10,5000,END=100,ERR=110)YPCREC
5000     FORMAT(A80)
**       Check the first non-blank character of YPCREC is an integer
         CALL FRSTCH(YPCREC,80,I1,LOINTG)
         IF(.NOT.LOINTG.OR.(I1.EQ.0)) THEN
            CALL INVALD(YPCREC)
         ELSE
            GO TO 300
         ENDIF
      ENDIF
      RETURN

1000  CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6000) YPCREC
6000  FORMAT(1X,'IMULTP: ERROR IN DATA ENTRY',/A)
      CALL ERRSTP

100   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6010)
6010  FORMAT(1X,'IMULTP: EOF WHEN SEARCHING FOR DATA')
      CALL ERRSTP

110   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6020)
6020  FORMAT(1X,'IMULTP: ERROR WHEN SEARCHING FOR DATA')
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE INTCPY(KXIN,KDIM,KXOUT)
**    Function - to copy contents of one integer array into another
**          array
**    Args in -
**               KXIN    - integer array to be copied
**               KDIM    - number of array elements to be copied
**    Args out -
**               KXOUT   - output array containing copy
**    Args for work - none
**    Com used - none
**    Com changed - none
**    Called by - PHEXTR
**    Calls - none
**    Author - R. Brugge, University of Reading

      DIMENSION KXIN(KDIM),KXOUT(KDIM)

      DO 1000 JJ=1,KDIM
1000  KXOUT(JJ)=KXIN(JJ)

      RETURN
      END
      SUBROUTINE INVALD(YPHRAS)
**    Function - to stop program and print out message in the event of
**          an incorrect user-supplied keyword value
**    Args in -
**               YPHRAS  - user-supplied keyword phrase
**    Args out - none
**    Args for work - none
**    Com used - /ERRMSG/NCERR
**    Com changed - /ERRMSG/NFATAL
**    Called by - EXTRAC,LPROC,RMULTP,IMULTP
**    Calls - ERRSTP
**    Files read - none
**    Files written - none
**    Author - R. Brugge, University of Reading

      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      CHARACTER *(*) YPHRAS

      NFATAL=NFATAL+1
      WRITE(NCERR,6000) YPHRAS
6000  FORMAT(1X,'INCORRECT VALUE IN PHRASE :',/A)
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE ITRANM(YPCREC,K1,K2,YPCOMA,KFIELD,KX,KXDIM,KUNIT)
**    Function - To transfer multiple data values to an integer array
**    Args in -
**               YPCREC   - character*80 variable
**               K1,K2    - data stored from locations K1 to K2 in
**                          YPCREC - both locations contain non-comma
**               KFIELD   - number of elements of array KX filled
**                          before call to ITRANM
**               KX       - array containing integer values
**               KXDIM    - dimension of KX
**               YPCOMA    - character element used to delimit values in
**                          YPCREC
**               KUNIT    - unit used as work space
**    Args out -
**               IFIELD   - number of elements of array KX filled
**                          upon exit from ITRANM
**               KX       - data from YPCREC has been added
**    Args for work - none
**    Com used - /ERRMSG/NCERR
**    Com changed - /ERRMSG/NFATAL
**    Called by - IMULTP
**    Calls - ITRANS,ERRSTP
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      CHARACTER*(*) YPCREC
      CHARACTER*1 YPCOMA
      DIMENSION KX(KXDIM)

      ISTART=K1
**    Locate first comma
100   ICC=INDEX(YPCREC(ISTART:K2),YPCOMA)
      IC=ISTART-1+ICC
      IF(ICC.EQ.0) THEN
         IEND=K2
      ELSE
         IEND=IC-1
      ENDIF
      KFIELD=KFIELD+1
      IF(KFIELD.GT.KXDIM) GO TO 2000
      CALL ITRANS(YPCREC,ISTART,IEND,KX(KFIELD),KUNIT)
      IF(ICC.EQ.0.OR.(ICC.EQ.K2))THEN
**       No more data to read
         RETURN
      ELSE
         ISTART=IC+1
         GO TO 100
      ENDIF
      RETURN

2000  CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6000)
6000  FORMAT(1X,'ATTEMPTING TO READ IN TOO MANY VALUES')
      WRITE(NCERR,6010)
6010  FORMAT(1X,'VALUES READ FOR CURRENT ARRAY:')
      WRITE(NCERR,*)(KX(J),J=1,KXDIM)
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE ITRANS(YPX,K1,K2,KXOUT,KUNIT)
**    Function - to transfer a numeric value stored in a character
**          string into a integer variable
**    Args in -
**               YPX     - character variable containing numeric data
**               K1,K2   - numeric data in YPX stored in locations K1 to
**                               K2 inclusive
**               KUNIT   - disk file used for work
**    Args out -
**               KXOUT   - integer value
**    Args for work - none
**    Com used - none
**    Com changed - none
**    Called by - EXTRAC,PHEXTR,ITRANM
**    Calls - none
**    Files read - KUNIT
**    Files written - KUNIT
**    Author - R. Brugge, University of Reading

      CHARACTER*(*) YPX

      REWIND KUNIT
      WRITE(KUNIT,6000) YPX(K1:K2)
6000  FORMAT(A)
      REWIND KUNIT
      READ(KUNIT,*) KXOUT
      REWIND KUNIT

      RETURN
      END
      SUBROUTINE LASTCH(YPA,KNC,K1,K2,KERR)
**    Function - to determine the location of the last significant
**          character in a character string; the search begins at a
**          specified location and continues until the end of the string
**          is reached
**    Args in -
**               YPA     - character variable containing data
**               KNC     - number of characters in string
**               K1      - starting location for search
**    Args out -
**               K2      - no non-zero characters between locations
**                         K1 and K2. Either location K2+1 contains a
**                         blank, or K2 corresponds to the end of the
**                         string
**               KERR    - 0 indicates no error
**                         999 indicates possible error (K2=K1)
**                         998 indicates error (K2.LT.K1)
**    Args for work - none
**    Com used - none
**    Com changed - none
**    Called by - EXTRAC,LPROC,RMULTP,PHEXTR
**    Calls - none
**    Files read - none
**    Files written - none
**    Author - R. Brugge, University of Reading

      CHARACTER*(*) YPA

      KERR=0
      DO 100 J=KNC,K1,-1
C      DO 100 J=K1,KNC
      IF(YPA(J:J).NE.' ') THEN
C      IF(YPA(J:J).EQ." ") THEN
         K2=J
C         K2=J-1
         IF(K2.EQ.K1)KERR=999
         IF(K2.LT.K1)KERR=998
         RETURN
      ENDIF
100   CONTINUE
C      K2=KNC
      K2=K1-1
      KERR=998

      RETURN
      END
      SUBROUTINE LATCHK(PLAT,KDIM,PDEFLT,YPDIAG,LPERR)
**    Function - to check validity of latitude values
**    Args in -
**               PLAT    - Latitude value(s)
**               KDIM    - Number of latitude values input
**               PDEFLT  - Default values to be used if input
**                         values are incorrect
**               YPDIAG  - Diagnostics for which latitudes are being
**                         checked
**               LPERR   - Error flag
**    Args out -
**               PLAT    - Latitude value(s)
**               LPERR   - Set to .TRUE. if PLAT values changed
**    Args for work - none
**    Com used - /ERRMSG/NCERR
**    Com changed - /ERRMSG/NFATAL
**    Params used - none
**    Called - by CHKPH
**    Calls - none
**    Files read - none
**    Files written - NCERR
**    Author - R Brugge, University of Reading

      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      DIMENSION PLAT(KDIM)
      LOGICAL LPERR
      CHARACTER*(*) YPDIAG

      IF(KDIM.EQ.1) THEN
         IF((PLAT(1).GT.90.0).OR.(PLAT(1).LT.-90.0)) THEN
            IF(PLAT(1).NE.PDEFLT) THEN
               NFATAL=NFATAL+1
               WRITE(NCERR,6000) YPDIAG
6000           FORMAT(1X,'E-W SLICES OF ',A,
     -               /'           FIELDS SWITCHED OFF')
               PLAT(1)=PDEFLT
               LPERR=.TRUE.
            ENDIF
         ENDIF
      ELSE IF(KDIM.EQ.2) THEN
         IF((PLAT(1).LT.-90.0).OR.(PLAT(2).GT.90.0).OR.
     -         (PLAT(2).LT.-90.0).OR.(PLAT(1).GT.90.0).OR.
     -         (PLAT(1).GT.PLAT(2))) THEN
            IF((PLAT(1).NE.PDEFLT).OR.PLAT(2).NE.PDEFLT) THEN
               NFATAL=NFATAL+1
               WRITE(NCERR,6000) YPDIAG
               LPERR=.TRUE.
               PLAT(1)=PDEFLT
               PLAT(2)=PDEFLT
            ENDIF
         ENDIF
         IF((PLAT(1).EQ.PDEFLT).OR.PLAT(2).EQ.PDEFLT) THEN
            PLAT(1)=PDEFLT
            PLAT(2)=PDEFLT
         ENDIF
      ENDIF

      RETURN
      END
      SUBROUTINE LEVCHK(PLEV,KDIM,PDEFLT,YPDIAG,KERR,LPERR,PMIN,PMAX)
**    Function - to check validity of horizontal level values
**    Args in -
**               PLAT    - Level value(s)
**               KDIM    - Number of level values input
**               PDEFLT  - Default values to be used if input
**                         values are incorrect
**               YPDIAG  - Diagnostics for which levels are being
**                         checked
**               KERR    - 1 if LPERR to be set to .TRUE. when an
**                         error is detected; else 0
**               LPERR   - error flag (set before call)
**               PMIN    - (range of P values that PLEV
**               PMAX    - (should lie between
**    Args out -
**               PLAT    - Level value(s)
**               LPERR   - set to .TRUE. if KERR=1 and an error detected
**                         in this routine
**    Args for work - none
**    Com used - /ERRMSG/NCERR
**    Com changed - /ERRMSG/NFATAL,NWARN
**    Com changed - none
**    Params used - none
**    Called by - CHKGH
**    Calls - none
**    Files read - none
**    Files written - NCERR
**    Author - R Brugge, University of Reading

      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      DIMENSION PLEV(KDIM)
      LOGICAL LPERR
      CHARACTER*(*) YPDIAG

      IF(KDIM.EQ.1) THEN
         IF((PLEV(1).GT.PMAX).OR.(PLEV(1).LT.PMIN)) THEN
            IF(PLEV(1).NE.PDEFLT) THEN
               NWARN=NWARN+1
               WRITE(NCERR,6000) YPDIAG
6000           FORMAT(1X,'WARNING: ARE ',A,
     -               '   LEVELS SET CORRECTLY?')
               IF(KERR.EQ.1) THEN
                  LPERR=.TRUE.
                  NFATAL=NFATAL+1
               ENDIF
            ENDIF
         ENDIF
      ELSE IF(KDIM.EQ.2) THEN
         IF((PLEV(1).LT.PMIN).OR.(PLEV(2).GT.PMAX).OR.
     -         (PLEV(2).LT.PMIN).OR.(PLEV(1).GT.PMAX).OR.
     -         (PLEV(1).GT.PLEV(2))) THEN
            IF((PLEV(1).NE.PDEFLT).OR.PLEV(2).NE.PDEFLT) THEN
               NWARN=NWARN+1
               WRITE(NCERR,6000) YPDIAG
               IF(KERR.EQ.1) THEN
                  LPERR=.TRUE.
                  NFATAL=NFATAL+1
               ENDIF
               IF(PLEV(2).LT.PLEV(1)) THEN
                  PLEV(1)=PDEFLT
                  PLEV(2)=PDEFLT
               ENDIF
            ENDIF
         ENDIF
         IF((PLEV(1).EQ.PDEFLT).OR.PLEV(2).EQ.PDEFLT) THEN
            PLEV(1)=PDEFLT
            PLEV(2)=PDEFLT
         ENDIF
      ENDIF

      RETURN
      END
      SUBROUTINE LNGCHK(PLONG,KDIM,PDEFLT,YPDIAG,LPERR)
**    Function - to check validity of longitude values
**    Args in -
**               PLONG   - Longitude value(s)
**               KDIM    - Number of longitude values input
**               PDEFLT  - Default values to be used if input
**                         values are incorrect
**               YPDIAG  - Diagnostics for which longitudes are being
**                         checked
**               LPERR   - Error flag
**    Args out -
**               PLONG   - Longitude value(s)
**               LPERR   - Set to .TRUE. if PLONG values changed
**    Args for work - none
**    Com used - /ERRMSG/NCERR
**    Com changed - /ERRMSG/NFATAL
**    Params used - none
**    Called - by CHKPH
**    Calls - none
**    Files read - none
**    Files written - NCERR
**    Author - R Brugge, University of Reading

      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      DIMENSION PLONG(KDIM)
      LOGICAL LPERR
      CHARACTER*(*) YPDIAG

      IF(KDIM.EQ.1) THEN
         IF((PLONG(1).GT.360.0).OR.(PLONG(1).LT.0.0)) THEN
            IF(PLONG(1).NE.PDEFLT) THEN
               NFATAL=NFATAL+1
               WRITE(NCERR,6000) YPDIAG
6000           FORMAT(1X,'N-S SLICES OF ',A,
     -               /'           FIELDS SWITCHED OFF')
               PLONG(1)=PDEFLT
               LPERR=.TRUE.
            ENDIF
         ENDIF
      ELSE IF(KDIM.EQ.2) THEN
         IF((PLONG(1).LT.0.0).OR.(PLONG(1).GT.360.0).OR.
     -         (PLONG(2).LT.0.0).OR.(PLONG(2).GT.360.0)) THEN
            IF((PLONG(1).NE.PDEFLT).OR.PLONG(2).NE.PDEFLT) THEN
               NFATAL=NFATAL+1
               WRITE(NCERR,6000) YPDIAG
               PLONG(1)=PDEFLT
               PLONG(2)=PDEFLT
               LPERR=.TRUE.
            ENDIF
         ENDIF
         IF((PLONG(1).EQ.PDEFLT).OR.PLONG(2).EQ.PDEFLT) THEN
            PLONG(1)=PDEFLT
            PLONG(2)=PDEFLT
         ENDIF
      ENDIF

      RETURN
      END
      SUBROUTINE LPROC(YPCREC,YPEQL,LPFLAG)
**    Function - to interpret a keyword phrase and return a
**          logical result
**    Args in -
**               YPCREC  - user-supplied keyword phrase
**               YPEQL   - character constant containing '='
**    Args out -
**               LPFLAG  - logical response
**    Args for work - none
**    Com used - /ERRMSG/NCERR
**    Com changed - /ERRMSG/NFATAL
**    Called by - EXTRAC,PHEXTR
**    Calls - LASTCH,INVALD,ERRSTP
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      CHARACTER*(*) YPCREC
      CHARACTER*1 YPEQL
      LOGICAL LPFLAG

**    I1 is the location of first character defining field value
      I1=INDEX(YPCREC,YPEQL)+1
**    I2 is the location of last character defining field value
      CALL LASTCH(YPCREC,80,I1,I2,IERR)
      IF(IERR.EQ.998) GO TO 1000
      IF((YPCREC(I1:I2).EQ.'T').OR.(YPCREC(I1:I2).EQ.'TRUE')
     -      .OR.(YPCREC(I1:I2).EQ.'.T.')
     -      .OR.(YPCREC(I1:I2).EQ.'.TRUE.')) THEN
         LPFLAG=.TRUE.
      ELSE IF((YPCREC(I1:I2).EQ.'F').OR.(YPCREC(I1:I2).EQ.'FALSE')
     -      .OR.(YPCREC(I1:I2).EQ.'.F.')
     -      .OR.(YPCREC(I1:I2).EQ.'.FALSE.')) THEN
         LPFLAG=.FALSE.
      ELSE
         CALL INVALD(YPCREC)
      ENDIF
      RETURN

1000  CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6000) YPCREC
6000  FORMAT(1X,'LPROC: ERROR IN GLOBAL DATA ENTRY',/A)
      CALL ERRSTP
      RETURN
      END
      SUBROUTINE RMULTP(YPCREC,YPEQL,YPCOMA,PX,KXDIM)
**    Function - to control the processing of a sequence of real
**          data values in (possibly) a sequence of keyword data
**          records
**    Args in -
**               YPCREC    - character variable
**               YPCOMA   - character*1 containing ','
**               YPEQL   - character*1 containing '='
**               KXDIM   - dimension of PX
**    Args out -
**               PX      - real array containing values extracted
**                         from keyword
**    Args for work - none
**    Com used - /ERRMSG/NCERR
**    Com changed - /ERRMSG/NFATAL
**    Called by - PHEXTR
**    Calls - LASTCH,RTRANM,FRSTCH,INVALD,ERRSTP
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      CHARACTER*(*) YPCREC
      CHARACTER*1 YPEQL,YPCOMA
      LOGICAL LOCONT,LOINTG
      DIMENSION PX(KXDIM)

**    I1 is the location of first character defining field value
      I1=INDEX(YPCREC,YPEQL)+1
**    IFIELD - number of data values for current field RX read in so far
      IFIELD=0
**    I2 is the location of last character defining field value
300   CALL LASTCH(YPCREC,80,I1,I2,IERR)
      IF(IERR.EQ.998) GO TO 1000
**    If the last character is a comma - expect a continuation line
      IF(YPCREC(I2:I2).EQ.YPCOMA) THEN
         LOCONT=.TRUE.
      ELSE
         LOCONT=.FALSE.
      ENDIF
      IF(.NOT.LOCONT) THEN
**       Decode a single line of data
         CALL RTRANM(YPCREC,I1,I2,YPCOMA,IFIELD,PX,KXDIM,12)
         RETURN
      ELSE
**       Decode one of several data lines
         CALL RTRANM(YPCREC,I1,I2-1,YPCOMA,IFIELD,PX,KXDIM,12)
**       Read the next line of data
         READ(10,5000,END=100,ERR=110)YPCREC
5000     FORMAT(A80)
**       Check the first non-blank character of YPCREC is an integer
         CALL FRSTCH(YPCREC,80,I1,LOINTG)
         IF(.NOT.LOINTG.OR.(I1.EQ.0)) THEN
            CALL INVALD(YPCREC)
         ELSE
            GO TO 300
         ENDIF
      ENDIF

1000  CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6000) YPCREC
6000  FORMAT(1X,'RMULTP: ERROR IN DATA ENTRY',/A)
      CALL ERRSTP

100   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6010)
6010  FORMAT(1X,'RMULTP: EOF WHEN SEARCHING FOR DATA')
      CALL ERRSTP

110   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6000)
6020  FORMAT(1X,'RMULTP: ERROR WHEN SEARCHING FOR DATA')
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE RTRANM(YPCREC,K1,K2,YPCOMA,KFIELD,PX,KXDIM,KUNIT)
**    Function - To transfer multiple data values to an real array
**    Args in -
**               YPCREC   - character*80 variable
**               K1,K2    - data stored from locations K1 to K2 in
**                          YPCREC - both locations contain non-comma
**               KFIELD   - number of elements of array PX filled
**                          before call to RTRANM
**               PX       - array containing real values
**               KXDIM    - dimension of PX
**               YPCOMA   - character element used to delimit values in
**                          YPCREC
**               KUNIT    - unit used as work space
**    Args out -
**               KFIELD   - number of elements of array PX filled
**                          upon exit from RTRANM
**               PX       - data from YPCREC has been added
**    Args for work - none
**    Com used - /ERRMSG/NCERR
**    Com changed - /ERRMSG/NFATAL
**    Called by - RMULTP
**    Calls - RTRANS,ERRSTP
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      CHARACTER*(*) YPCREC
      CHARACTER*1 YPCOMA
      DIMENSION PX(KXDIM)

      ISTART=K1
**    Locate first comma
100   ICC=INDEX(YPCREC(ISTART:K2),YPCOMA)
      IC=ISTART-1+ICC
      IF(ICC.EQ.0) THEN
         IEND=K2
      ELSE
         IEND=IC-1
      ENDIF
      KFIELD=KFIELD+1
      IF(KFIELD.GT.KXDIM) GO TO 2000
      CALL RTRANS(YPCREC,ISTART,IEND,PX(KFIELD),KUNIT)
      IF(ICC.EQ.0.OR.(ICC.EQ.K2))THEN
**       No more data to read
         RETURN
      ELSE
         ISTART=IC+1
         GO TO 100
      ENDIF

      RETURN

2000  CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6000)
6000  FORMAT(1X,'RTRANM ERROR: ATTEMPTING TO READ IN TOO MANY VALUES')
      WRITE(NCERR,6010)
6010  FORMAT(1X,'VALUES READ FOR CURRENT ARRAY:')
      WRITE(NCERR,*)(PX(J),J=1,KXDIM)
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE RTRANS(YPX,K1,K2,POUT,KUNIT)
**    Function - to transfer a numeric value stored in a character
**          string into a real variable
**    Args in -
**               YPX     - character variable containing numeric data
**               K1,K2   - numeric data in YPX stored in locations K1 to
**                               K2 inclusive
**               KUNIT   - disk file used for work
**    Args out -
**               POUT    - real value
**    Args for work - none
**    Com used - none
**    Com changed - none
**    Called by - EXTRAC,RTRANM
**    Calls - none
**    Files read - KUNIT
**    Files written - KUNIT
**    Author - R. Brugge, University of Reading

      CHARACTER*(*) YPX

      REWIND KUNIT
      WRITE(KUNIT,6000) YPX(K1:K2)
6000  FORMAT(A)
      REWIND KUNIT
      READ(KUNIT,*) POUT
      REWIND KUNIT

      RETURN
      END
      SUBROUTINE SEARCH(YPA,YPHRAS,LPFIND)
**    Function - to search for a phrase in a character string
**    Args in -
**               YPA     - character*80 variable
**               YPHRAS  - phrase to be located
**    Args out -
**               LPFIND  - .TRUE. if phrase found
**    Args for work - none
**    Com used - none
**    Com changed - none
**    Called by - COMMRD,PHEXTR,PHPROC,DIAGRD
**    Calls - none
**    Files read - none
**    Files written - none
**    Author - R. Brugge, University of Reading

      CHARACTER*80 YPA
      CHARACTER*(*) YPHRAS
      LOGICAL LPFIND

**    Determine number of characters in YPHRAS
      IPHR=LEN(YPHRAS)
**    Now determine if YPHRAS exists in YPA
      LPFIND=.FALSE.
      DO 100 J=1,81-IPHR
      IF(YPA(J:J+IPHR-1).EQ.YPHRAS) THEN
         LPFIND=.TRUE.
         GO TO 200
      ENDIF
100   CONTINUE
200   CONTINUE

      RETURN
      END
      SUBROUTINE DIAGCK
**    Function - control the checking of namelist data
**          pertaining to individual diagnostic fields
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /DIAGTP/LDCO,LDSG,LDTF
**          ,/ERRMSG/NCERR
**    Com changed - /DIAGTP/LDPH,LDXP,LDTR,/ERRMSG/NWARN,
**          NFATAL
**    Called by - CONTROL
**    Calls - CHKPH,CHKXP,CHKLV,CHKOR,CHKCO,CHKSG,CHKGH,
**          CHKGZ,CHKGM,CHKGP,CHKG3,CHKTR,CHKTH,CHKTZ,CHKTM,
**          CHKTP,CHKT3,CHKTF,CHKFH,CHKFZ,CHKFM,CHKFP,CHKF3,CHKOD,
**          CHKOH,CHKOZ,CHKOM,CHKOP,CHK03,CHKZF,CHKTJ
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LGCMTR,LHISPH,LHISXP,LDRY,LDIABH,LBL,LVD
      COMMON /MODELC/TSPD,NYEAR,NDAY,NMIN,LGCMTR,NL,TMEAN(JPNL),
     -      LHISPH,LHISXP,LDRY,LDIABH,BEGDYP,LBL,LVD,AKVV,CD

**    Check PH input data for diagnostic program
      IF(LDPH) CALL CHKPH
**    Check XP input data for diagnostic program
      IF(LDXP) CALL CHKXP
**    Check levels to be used for output
      CALL CHKLV
**    Check orography switches etc
      CALL CHKOR
**    Check CO input data for diagnostic program
      IF(LDCO) CALL CHKCO
**    Check SG input data for diagnostic program
      IF(LDSG) THEN
         CALL CHKSG
         CALL CHKGH
         CALL CHKGZ
         CALL CHKGM
         CALL CHKGP
         CALL CHKG3
      ENDIF

**    Check TR input data for diagnostic program
      IF(LDTR) THEN
         CALL CHKTR
         CALL CHKTH
         CALL CHKTZ
         CALL CHKTM
         CALL CHKTP
         CALL CHKT3
      ENDIF

**    Check TF input data for diagnostic program
      IF(LDTF) THEN
         CALL CHKTF
         CALL CHKFH
         CALL CHKFZ
         CALL CHKFM
         CALL CHKFP
         CALL CHKF3
      ENDIF

**    Check OD input data for diagnostic program
      IF(LDOD) THEN
         CALL CHKOD
         CALL CHKOH
         CALL CHKOZ
         CALL CHKOM
         CALL CHKOP
         CALL CHKO3
      ENDIF

**    Check ZF data input to diagnostic program
      IF(LDZF) CALL CHKZF

**    Check TJ data input to diagnostic program
      IF(LDTJ) CALL CHKTJ

      RETURN
      END
      SUBROUTINE DIAGRD
**    Function - to read and control the interpretation of data
**          pertaining to individual diagnostic fields
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /ERRMSG/NCERR
**    Com changed - /ERRMSG/NFATAL
**    Called by - CONTROL
**    Calls - PHPROC,SEARCH,XPPROC,LVPROC,ORPROC,COPROC,
**          SGPROC,GHPROC,GZPROC,GMPROC,GPPROC,G3PROC,TRPROC,ERRSTP,
**          THPROC,TZPROC,TMPROC,TPPROC,T3PROC,TFPROC,FHPROC,FZPROC,
**          FMPROC,FPPROC,F3PROC,ODPROX,OHPROC,OZPROC,OMPROC,OPPROC,
**          O3PROC,ZFPROC,TJPROC
**    Files read - 10
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      CHARACTER*80 YOCREC
      LOGICAL LOFIND

**    Read next record
1000  READ (10,5000,END=100,ERR=110)YOCREC
5000  FORMAT(A80)

**    Determine diagnostic type to which this block refers
**    Test for PH diagnostics
      CALL SEARCH(YOCREC,'$PH',LOFIND)
      IF(LOFIND) THEN
         CALL PHPROC
         GO TO 1000
      ENDIF

**    Test for XP diagnostics
      CALL SEARCH(YOCREC,'$XP',LOFIND)
      IF(LOFIND) THEN
         CALL XPPROC
         GO TO 1000
      ENDIF

**    Test for output level data
      CALL SEARCH(YOCREC,'$LV',LOFIND)
      IF(LOFIND) THEN
         CALL LVPROC
         GO TO 1000
      ENDIF

**    Test for orography data
      CALL SEARCH(YOCREC,'$OR',LOFIND)
      IF(LOFIND) THEN
         CALL ORPROC
         GO TO 1000
      ENDIF

**    Test for CO diagnostics
      CALL SEARCH(YOCREC,'$CO',LOFIND)
      IF(LOFIND) THEN
         CALL COPROC
         GO TO 1000
      ENDIF

**    Test for SG diagnostics
      CALL SEARCH(YOCREC,'$SG',LOFIND)
      IF(LOFIND) THEN
         CALL SGPROC
         GO TO 1000
      ENDIF

**    Test for (SG) GH diagnostics
      CALL SEARCH(YOCREC,'$GH',LOFIND)
      IF(LOFIND) THEN
         CALL GHPROC
         GO TO 1000
      ENDIF

**    Test for (SG) GZ diagnostics
      CALL SEARCH(YOCREC,'$GZ',LOFIND)
      IF(LOFIND) THEN
         CALL GZPROC
         GO TO 1000
      ENDIF

**    Test for (SG) GM diagnostics
      CALL SEARCH(YOCREC,'$GM',LOFIND)
      IF(LOFIND) THEN
         CALL GMPROC
         GO TO 1000
      ENDIF

**    Test for (SG) GP diagnostics
      CALL SEARCH(YOCREC,'$GP',LOFIND)
      IF(LOFIND) THEN
         CALL GPPROC
         GO TO 1000
      ENDIF

**    Test for (SG) G3 diagnostics
      CALL SEARCH(YOCREC,'$G3',LOFIND)
      IF(LOFIND) THEN
         CALL G3PROC
         GO TO 1000
      ENDIF

**    Test for TF diagnostics
      CALL SEARCH(YOCREC,'$TF',LOFIND)
      IF(LOFIND) THEN
         CALL TFPROC
         GO TO 1000
      ENDIF

**    Test for (TF) FH diagnostics
      CALL SEARCH(YOCREC,'$FH',LOFIND)
      IF(LOFIND) THEN
         CALL FHPROC
         GO TO 1000
      ENDIF

**    Test for (TF) FZ diagnostics
      CALL SEARCH(YOCREC,'$FZ',LOFIND)
      IF(LOFIND) THEN
         CALL FZPROC
         GO TO 1000
      ENDIF

**    Test for (TF) FM diagnostics
      CALL SEARCH(YOCREC,'$FM',LOFIND)
      IF(LOFIND) THEN
         CALL FMPROC
         GO TO 1000
      ENDIF

**    Test for (TF) FP diagnostics
      CALL SEARCH(YOCREC,'$FP',LOFIND)
      IF(LOFIND) THEN
         CALL FPPROC
         GO TO 1000
      ENDIF

**    Test for (TF) F3 diagnostics
      CALL SEARCH(YOCREC,'$F3',LOFIND)
      IF(LOFIND) THEN
         CALL F3PROC
         GO TO 1000
      ENDIF

**    Test for TR diagnostics
      CALL SEARCH(YOCREC,'$TR',LOFIND)
      IF(LOFIND) THEN
         CALL TRPROC
         GO TO 1000
      ENDIF

**    Test for (TR) TH diagnostics
      CALL SEARCH(YOCREC,'$TH',LOFIND)
      IF(LOFIND) THEN
         CALL THPROC
         GO TO 1000
      ENDIF

**    Test for (TR) TZ diagnostics
      CALL SEARCH(YOCREC,'$TZ',LOFIND)
      IF(LOFIND) THEN
         CALL TZPROC
         GO TO 1000
      ENDIF

**    Test for (TR) TM diagnostics
      CALL SEARCH(YOCREC,'$TM',LOFIND)
      IF(LOFIND) THEN
         CALL TMPROC
         GO TO 1000
      ENDIF

**    Test for (TR) TP diagnostics
      CALL SEARCH(YOCREC,'$TP',LOFIND)
      IF(LOFIND) THEN
         CALL TPPROC
         GO TO 1000
      ENDIF

**    Test for (TR) T3 diagnostics
      CALL SEARCH(YOCREC,'$T3',LOFIND)
      IF(LOFIND) THEN
         CALL T3PROC
         GO TO 1000
      ENDIF

**    Test for OD diagnostics
      CALL SEARCH(YOCREC,'$OD',LOFIND)
      IF(LOFIND) THEN
         CALL ODPROC
         GO TO 1000
      ENDIF

**    Test for (OD) OH diagnostics
      CALL SEARCH(YOCREC,'$OH',LOFIND)
      IF(LOFIND) THEN
         CALL OHPROC
         GO TO 1000
      ENDIF

**    Test for (OD) OZ diagnostics
      CALL SEARCH(YOCREC,'$OZ',LOFIND)
      IF(LOFIND) THEN
         CALL OZPROC
         GO TO 1000
      ENDIF

**    Test for (OD) OM diagnostics
      CALL SEARCH(YOCREC,'$OM',LOFIND)
      IF(LOFIND) THEN
         CALL OMPROC
         GO TO 1000
      ENDIF

**    Test for (OD) OP diagnostics
      CALL SEARCH(YOCREC,'$OP',LOFIND)
      IF(LOFIND) THEN
         CALL OPPROC
         GO TO 1000
      ENDIF

**    Test for (OD) O3 diagnostics
      CALL SEARCH(YOCREC,'$O3',LOFIND)
      IF(LOFIND) THEN
         CALL O3PROC
         GO TO 1000
      ENDIF

**    Test for ZF diagnostics
      CALL SEARCH(YOCREC,'$ZF',LOFIND)
      IF(LOFIND) THEN
         CALL ZFPROC
         GO TO 1000
      ENDIF

**    Test for TJ diagnostics
      CALL SEARCH(YOCREC,'$TJ',LOFIND)
      IF(LOFIND) THEN
         CALL TJPROC
         GO TO 1000
      ENDIF

**    Else - invalid diagnostic type or $XX phrase not read
      NFATAL=NFATAL+1
      WRITE(NCERR,6000)
6000  FORMAT(1X,'DIAGRD: INVALID RECORD FOUND WHEN SEARCHING FOR'
     -      ' START OF DATA BLOCK')
      CALL ERRSTP

**    End file reached
100   CONTINUE
      WRITE(NCERR,6010)
6010  FORMAT(1X,'DIAGRD: EOF FOUND IN INPUT DATA FILE')
      RETURN

**    Error encountered
110   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6020)
6020  FORMAT(1X,'DIAGRD: ERROR WHEN SEARCHING FOR INPUT DATA')
      CALL ERRSTP

      END
      SUBROUTINE DIAGWT
**    Function - to write out the namelist data
**          pertaining to individual diagnostic fields to
**          unit 11
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /PHDIAG/all elements,/XPDIAG/all elements,
**          /ORDIAG/all elements,
**          /GHDIAG/all elements,/GZDIAG/all elements,/GMDIAG/all
**          elements,/GPDIAG/all elements,/G3DIAG/all elements,
**          /THDIAG/all elements,/TZDIAG/all
**          elements,/TMDIAG/all elements,/TPDIAG/all elements,
**          /T3DIAG/all elements,/FHDIAG/all
**          elements,/FZDIAG/all elements,/FMDIAG/all elements,
**          /FPDIAG/all elements,/F3DIAG/all elements,/MODELC/NL,
**          /OHDIAG/all elements,/OZDIAG/all elements,/OMDIAG/all
**          elements,/OPDIAG/all elements,/O3DIAG/all elements,
**          /ZFDIAG/all elements,/TJDIAG/all elements
**    Com changed - none
**    Params used - /PARAM1/JPNL,/PARAMS/JPPHMX,JPXPMX,JPCOMX,
**            JPSGMX,JPTRMX,JPTFMX,JPODMX,JPOPPF,JPZFMX,JPTJMX
**    Called by - CONTROL
**    Calls - WLVNML,WCONML,WSGNML,WTRNML,WTFNML,WODNML,WZFNML
**    Files read - none
**    Files written - 11
**    Author - R. Brugge, University of ReadLng

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /F3DIAG/NCF3,NF3PL(JPTFMX)
      LOGICAL LDFHSL
      COMMON /FHDIAG/NFHPR(JPTFMX),NFHPL(JPTFMX),CINTFH(JPTFMX),LDFHSL
      LOGICAL LDFMMM
      COMMON /FMDIAG/LDFMMM,VFMEW,VFMEWA(2),NFMPR(JPTFMX),
     -      NFMPL(JPTFMX),CINTFM(JPTFMX)
      LOGICAL LDFPCP
      COMMON /FPDIAG/NFPPF,VFPPF(2,JPFPPF),NFPPR(JPTFMX),
     -      NFPPL(JPTFMX),LDFPCP
      LOGICAL LDFZZM
      COMMON /FZDIAG/LDFZZM,VFZNS,VFZNSA(2),NFZPR(JPTFMX),
     -      NFZPL(JPTFMX),CINTFZ(JPTFMX)
      COMMON /G3DIAG/NCG3,NG3PL(JPSGMX),NG3
      LOGICAL LDGHSL,LDGHCT,LDGHED
      COMMON /GHDIAG/LDGHSL,LDGHCT,LDGHED,VGHTS(2),
     -      NGHPR(JPSGMX),NGHPL(JPSGMX),NGH,NGHVEC(JPSGMX),
     -      CINTGH(JPSGMX)
      LOGICAL LDGMMM,LDGMTS
      COMMON /GMDIAG/LDGMMM,VGMEW,LDGMTS,VGMEWA(2),NGMPR(JPSGMX),
     -      NGMPL(JPSGMX),NGM,NGMVEC(JPSGMX),CINTGM(JPSGMX)
      LOGICAL LDGPCT,LDGPCP
      COMMON /GPDIAG/LDGPCT,NGPPF,VGPPF(2,JPGPPF),NGPPR(JPSGMX),
     -      NGPPL(JPSGMX),NGP,CINTGP(JPSGMX),LDGPCP
      LOGICAL LDGZZM,LDGZTS
      COMMON /GZDIAG/LDGZZM,VGZNS,LDGZTS,VGZNSA(2),NGZPR(JPSGMX),
     -      NGZPL(JPSGMX),NGZ,NGZVEC(JPSGMX),CINTGZ(JPSGMX)
      LOGICAL LGCMTR,LHISPH,LHISXP,LDRY,LDIABH,LBL,LVD
      COMMON /MODELC/TSPD,NYEAR,NDAY,NMIN,LGCMTR,NL,TMEAN(JPNL),
     -      LHISPH,LHISXP,LDRY,LDIABH,BEGDYP,LBL,LVD,AKVV,CD
      COMMON /O3DIAG/NCO3,NO3PL(JPODMX),NO3
      LOGICAL LDOHSL,LDOHCT,LDOHED
      COMMON /OHDIAG/LDOHSL,LDOHCT,LDOHED,VOHTS(2),
     -      NOHPR(JPODMX),NOHPL(JPODMX),NOH,NOHVEC(JPODMX),
     -      CINTOH(JPODMX)
      LOGICAL LDOMMM,LDOMTS
      COMMON /OMDIAG/LDOMMM,VOMEW,LDOMTS,VOMEWA(2),NOMPR(JPODMX),
     -      NOMPL(JPODMX),NOM,NOMVEC(JPODMX),CINTOM(JPODMX)
      LOGICAL LDOPCT,LDOPCP
      COMMON /OPDIAG/LDOPCT,NOPPF,VOPPF(2,JPOPPF),NOPPR(JPODMX),
     -      NOPPL(JPODMX),NOP,CINTOP(JPODMX),LDOPCP
      LOGICAL LROGPR,LROGPL,LMSKH,LMSKNS,LMSKWE
      COMMON /ORDIAG/LROGPR,LROGPL,CINTOR,LMSKH,LMSKNS,LMSKWE
      LOGICAL LDOZZM,LDOZTS
      COMMON /OZDIAG/LDOZZM,VOZNS,LDOZTS,VOZNSA(2),NOZPR(JPODMX),
     -      NOZPL(JPODMX),NOZ,NOZVEC(JPODMX),CINTOZ(JPODMX)
      LOGICAL LDPHLL,LDPHZM,LDPHMM,LDPHGM,LDPHTD,LDPHIN,
     -      LDPHTS
      COMMON /PHDIAG/NCPH,NPHPR(JPPHMX),NPHPL(JPPHMX),
     -      CINTPH(JPPHMX),LDPHLL,LDPHZM,LDPHMM,LDPHGM,LDPHTD,
     -      LDPHIN,VPHEW,VPHEWA(2),VPHEWS(2),VPHNS,VPHNSA(2),
     -      VPHNSS(2),VPHAA(4),VPHAS(4),LDPHTS,NPH
      COMMON /T3DIAG/NCT3,NT3PL(JPTRMX),NT3
      LOGICAL LDTHSL,LDTHCT,LDTHED,LTHDOB
      COMMON /THDIAG/LDTHSL,LDTHCT,LDTHED,VTHTS(2),
     -      NTHPR(JPTRMX),NTHPL(JPTRMX),NTH,NTHVEC(JPTRMX),
     -      CINTTH(JPTRMX),LTHDOB
      LOGICAL LUTFTJ,LBINTJ,LUTFLV
      CHARACTER*2 YTYPTJ
      CHARACTER*6 YTJDUM
      CHARACTER*80 YINBIN,YOPBIN
      COMMON /TJDIAG/NTJATT,NATTR(JPTJMX),LUTFTJ,LBINTJ,
     -      NGRPTJ,LUTFLV,YTYPTJ,YTJDUM,YINBIN,YOPBIN
      LOGICAL LDTMMM,LDTMTS
      COMMON /TMDIAG/LDTMMM,VTMEW,LDTMTS,VTMEWA(2),NTMPR(JPTRMX),
     -      NTMPL(JPTRMX),NTM,NTMVEC(JPTRMX),CINTTM(JPTRMX)
      LOGICAL LDTPCT,LDTPCP,LTPDOB
      COMMON /TPDIAG/LDTPCT,NTPPF,VTPPF(2,JPTPPF),NTPPR(JPTRMX),
     -      NTPPL(JPTRMX),NTP,CINTTP(JPTRMX),LDTPCP,LTPDOB
      LOGICAL LDTZZM,LDTZTS
      COMMON /TZDIAG/LDTZZM,VTZNS,LDTZTS,VTZNSA(2),NTZPR(JPTRMX),
     -      NTZPL(JPTRMX),NTZ,NTZVEC(JPTRMX),CINTTZ(JPTRMX)
      LOGICAL LDXPZM,LDXPLM,LDXPTD,LDXPIN,LDXPTS
      COMMON /XPDIAG/NCXP,NXPPR(JPXPMX),NXPPL(JPXPMX),
     -      CINTXP(JPXPMX),LDXPZM,LDXPLM,LDXPTD,LDXPIN,LDXPTS,NXP
      LOGICAL LZFEDF,LZFMNF,LDZFIN,LDZFTD,LDZFTS,LDZFZM,LDZFLM
      COMMON /ZFDIAG/NCZF,NZFPR(JPZFMX),NZFPL(JPZFMX),
     -      CINTZF(JPZFMX),NVZFHR(JPNL),
     -      NZF,FACTZF(2,JPZFMX),NFLDZF(4,JPZFMX),VZFSFC(JPNL),
     -      LZFEDF,LZFMNF,LDZFIN,LDZFTD,LDZFTS,LDZFZM,LDZFLM
      NAMELIST /F3DATA/NCF3,NF3PL
      NAMELIST /FHDATA/NFHPR,NFHPL,CINTFH,LDFHSL
      NAMELIST /FMDATA/LDFMMM,VFMEW,VFMEWA,NFMPR,NFMPL,CINTFM
      NAMELIST /FPDATA/NFPPF,VFPPF,NFPPR,NFPPL,LDFPCP
      NAMELIST /FZDATA/LDFZZM,VFZNS,VFZNSA,NFZPR,NFZPL,CINTFZ
      NAMELIST /G3DATA/NCG3,NG3PL,NG3
      NAMELIST /GHDATA/LDGHSL,LDGHCT,LDGHED,VGHTS,NGHPR,NGHPL,NGH,
     -      NGHVEC,CINTGH
      NAMELIST /GMDATA/LDGMMM,VGMEW,VGMEWA,LDGMTS,NGMPR,NGMPL,NGM,
     -      NGMVEC,CINTGM
      NAMELIST /GPDATA/NGPPF,VGPPF,LDGPCT,NGPPR,NGPPL,NGP,CINTGP,
     -      LDGPCP
      NAMELIST /GZDATA/LDGZZM,VGZNS,VGZNSA,LDGZTS,NGZPR,NGZPL,NGZ,
     -      NGZVEC,CINTGZ
      NAMELIST /O3DATA/NCO3,NO3PL,NO3
      NAMELIST /OHDATA/LDOHSL,LDOHCT,LDOHED,VOHTS,NOHPR,NOHPL,NOH,
     -      NOHVEC,CINTOH
      NAMELIST /OMDATA/LDOMMM,VOMEW,LDOMTS,VOMEWA,NOMPR,
     -      NOMPL,NOM,NOMVEC,CINTOM
      NAMELIST /OPDATA/LDOPCT,NOPPF,VOPPF,NOPPR,NOPPL,NOP,
     -      CINTOP,LDOPCP
      NAMELIST /ORDATA/LROGPR,LROGPL,CINTOR,LMSKH,LMSKNS,
     -      LMSKWE
      NAMELIST /OZDATA/LDOZZM,VOZNS,VOZNSA,LDOZTS,NOZPR,NOZPL,NOZ,
     -      NOZVEC,CINTOZ
      NAMELIST /PHDATA/NCPH,NPHPR,NPHPL,
     -      CINTPH,LDPHLL,LDPHZM,LDPHMM,LDPHGM,LDPHTD,LDPHIN,
     -      VPHEW,VPHEWA,VPHEWS,VPHNS,VPHNSA,VPHNSS,
     -      VPHAA,VPHAS,LDPHTS,NPH
      NAMELIST /T3DATA/NCT3,NT3PL,NT3
      NAMELIST /THDATA/LDTHSL,LDTHCT,LDTHED,VTHTS,NTHPR,NTHPL,NTH,
     -      NTHVEC,CINTTH,LTHDOB
      NAMELIST /TJDATA/NTJATT,NATTR,LUTFTJ,LBINTJ,
     -      NGRPTJ,LUTFLV,YTYPTJ,YINBIN,YOPBIN
      NAMELIST /TMDATA/LDTMMM,VTMEW,VTMEWA,LDTMTS,NTMPR,NTMPL,NTM,
     -      NTMVEC,CINTTM
      NAMELIST /TPDATA/NTPPF,VTPPF,LDTPCT,NTPPR,NTPPL,NTP,CINTTP,
     -      LDTPCP,LTPDOB
      NAMELIST /TZDATA/LDTZZM,VTZNS,VTZNSA,LDTZTS,NTZPR,NTZPL,NTZ,
     -      NTZVEC,CINTTZ
      NAMELIST /XPDATA/NCXP,NXPPR,NXPPL,CINTXP,LDXPZM,
     -      LDXPLM,LDXPTD,LDXPIN,LDXPTS,NXP
      NAMELIST /ZFDATA/NCZF,NZFPR,NZFPL,CINTZF,NZF,FACTZF,NFLDZF,
     -      LZFEDF,LZFMNF,LDZFIN,LDZFTD,LDZFTS,LDZFZM,LDZFLM,VZFSRC,
     -      NVZFHR

      WRITE(11,PHDATA)
      WRITE(11,XPDATA)
CC    WRITE(11,LVDATA)
      CALL WLVNML(NL)
      WRITE(11,ORDATA)
CC    WRITE(11,CODATA)
      CALL WCONML(NL)
CC    WRITE(11,SGDATA)
      CALL WSGNML(NL)
      WRITE(11,GHDATA)
      WRITE(11,GZDATA)
      WRITE(11,GMDATA)
      WRITE(11,GPDATA)
      WRITE(11,G3DATA)
CC    WRITE(11,TRDATA)
      CALL WTRNML(NL)
      WRITE(11,THDATA)
      WRITE(11,TZDATA)
      WRITE(11,TMDATA)
      WRITE(11,TPDATA)
      WRITE(11,T3DATA)
CC      WRITE(11,TFDATA)
      CALL WTFNML(NL)
      WRITE(11,FHDATA)
      WRITE(11,FZDATA)
      WRITE(11,FMDATA)
      WRITE(11,FPDATA)
      WRITE(11,F3DATA)
CC      WRITE(11,ODDATA)
      CALL WODNML(NL)
      WRITE(11,OHDATA)
      WRITE(11,OZDATA)
      WRITE(11,OMDATA)
      WRITE(11,OPDATA)
      WRITE(11,O3DATA)
CC      WRITE(11,ZFDATA)
      CALL WZFNML(NL)
      WRITE(11,TJDATA)

      RETURN
      END
      SUBROUTINE WCONML(KNL)
**    Function - to construct CODATA namelist
**    Args in -
**               KNL     - number of model levels
**    Args out - none
**    Args for work - none
**    Com used - /CODIAG/all elements
**    Com changed - none
**    Params used - /PARAM1/JPNL,/PARAMS/JPCOMX
**    Called by - DIAGWT
**    Calls - none
**    Files read - none
**    Files written - 11
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /CODIAG/ NCCO,NWAVET,NCOPR(JPCOMX),NVCOHR(JPNL)
CC    NAMELIST /CODATA/NCCO,NWAVET,NCOPR,NVCOHR

      WRITE(11,6000)
6000  FORMAT(' &CODATA')

      WRITE(11,*)'  ','NCCO = ',NCCO,', NWAVET = ',NWAVET,', '
      WRITE(11,*)'  ','NCOPR = '
      DO 100 J=1,JPCOMX,6
      IBEG=J
      IEND=MIN0(JPCOMX,J+5)
      IEL=IEND-IBEG+1
      IF(IEL.EQ.1) THEN
         WRITE(11,*)'  ',NCOPR(J),', '
      ELSE IF(IEL.EQ.2) THEN
         WRITE(11,*)'  ',NCOPR(J),', ',NCOPR(J+1),', '
      ELSE IF(IEL.EQ.3) THEN
         WRITE(11,*)'  ',NCOPR(J),', ',NCOPR(J+1),', ',
     -         NCOPR(J+2),', '
      ELSE IF(IEL.EQ.4) THEN
         WRITE(11,*)'  ',NCOPR(J),', ',NCOPR(J+1),', ',
     -         NCOPR(J+2),', ',NCOPR(J+3),', '
      ELSE IF(IEL.EQ.5) THEN
         WRITE(11,*)'  ',NCOPR(J),', ',NCOPR(J+1),', ',
     -         NCOPR(J+2),', ',NCOPR(J+3),', ',NCOPR(J+4),', '
      ELSE IF(IEL.EQ.6) THEN
         WRITE(11,*)'  ',NCOPR(J),', ',NCOPR(J+1),', ',
     -         NCOPR(J+2),', ',NCOPR(J+3),', ',NCOPR(J+4),', ',
     -         NCOPR(J+5),', '
      ENDIF
100   CONTINUE
      WRITE(11,*)'  ','NVCOHR = '
      DO 200 J=1,KNL,6
      IBEG=J
      IEND=MIN0(KNL,J+5)
      IEL=IEND-IBEG+1
      IF(IEL.EQ.1) THEN
         WRITE(11,*)'  ',NVCOHR(J),', '
      ELSE IF(IEL.EQ.2) THEN
         WRITE(11,*)'  ',NVCOHR(J),', ',NVCOHR(J+1),', '
      ELSE IF(IEL.EQ.3) THEN
         WRITE(11,*)'  ',NVCOHR(J),', ',NVCOHR(J+1),', ',
     -         NVCOHR(J+2),', '
      ELSE IF(IEL.EQ.4) THEN
         WRITE(11,*)'  ',NVCOHR(J),', ',NVCOHR(J+1),', ',
     -         NVCOHR(J+2),', ',NVCOHR(J+3),', '
      ELSE IF(IEL.EQ.5) THEN
         WRITE(11,*)'  ',NVCOHR(J),', ',NVCOHR(J+1),', ',
     -         NVCOHR(J+2),', ',NVCOHR(J+3),', ',NVCOHR(J+4),', '
      ELSE IF(IEL.EQ.6) THEN
         WRITE(11,*)'  ',NVCOHR(J),', ',NVCOHR(J+1),', ',
     -         NVCOHR(J+2),', ',NVCOHR(J+3),', ',NVCOHR(J+4),', ',
     -         NVCOHR(J+5),', '
      ENDIF
200   CONTINUE

      WRITE(11,6999)
6999  FORMAT(' &END')

      RETURN
      END
      SUBROUTINE WLVNML(KNL)
**    Function - to construct LVDATA namelist
**    Args in -
**               KNL     - number of model levels
**    Args out - none
**    Args for work - none
**    Com used - /LVDIAG/all elements
**    Com changed - none
**    Params used - /PARAM1/JPNL
**    Called by - DIAGWT
**    Calls - none
**    Files read - none
**    Files written - 11
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      COMMON /LVDIAG/OUTLEV(JPNL)
CC    NAMELIST /LVDATA/OUTLEV

      WRITE(11,6000)
6000  FORMAT(' &LVDATA  OUTLEV = ')

      DO 100 J=1,KNL,6
      IBEG=J
      IEND=MIN0(KNL,J+5)
      IEL=IEND-IBEG+1
      IF(IEL.EQ.1) THEN
         WRITE(11,*)'  ',OUTLEV(J),', '
      ELSE IF(IEL.EQ.2) THEN
         WRITE(11,*)'  ',OUTLEV(J),', ',OUTLEV(J+1),', '
      ELSE IF(IEL.EQ.3) THEN
         WRITE(11,*)'  ',OUTLEV(J),', ',OUTLEV(J+1),', ',
     -         OUTLEV(J+2),', '
      ELSE IF(IEL.EQ.4) THEN
         WRITE(11,*)'  ',OUTLEV(J),', ',OUTLEV(J+1),', ',
     -         OUTLEV(J+2),', ',OUTLEV(J+3),', '
      ELSE IF(IEL.EQ.5) THEN
         WRITE(11,*)'  ',OUTLEV(J),', ',OUTLEV(J+1),', ',
     -         OUTLEV(J+2),', ',OUTLEV(J+3),', ',OUTLEV(J+4),', '
      ELSE IF(IEL.EQ.6) THEN
         WRITE(11,*)'  ',OUTLEV(J),', ',OUTLEV(J+1),', ',
     -         OUTLEV(J+2),', ',OUTLEV(J+3),', ',OUTLEV(J+4),', ',
     -         OUTLEV(J+5),', '
      ENDIF
100   CONTINUE

      WRITE(11,6999)
6999  FORMAT(' &END')

      RETURN
      END
      SUBROUTINE WODNML(KNL)
**    Function - to construct ODDATA namelist
**    Args in -
**               KNL     - number of model levels
**    Args out - none
**    Args for work - none
**    Com used - /ODDIAG/all elements
**    Com changed - none
**    Params used - /PARAM1/JPNL
**    Called by - DIAGWT
**    Calls - none
**    Files read - none
**    Files written - 11
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      LOGICAL LDOH,LDOZ,LDOM,LDOP,LDO3,LDODTD,LDODTS,LDODIN
      COMMON /ODDIAG/LDOH,LDOZ,LDOM,LDOP,LDO3,LDODTD,LDODTS,LDODIN,
     -      NCOD,NVODHR(JPNL),VODSFC(JPNL)
CC    NAMELIST /ODDATA/LDOH,LDOZ,LDOM,LDOP,LDO3,LDODTD,LDODTS,LDODIN,
CC   -      NCOD,NVODHR,VODSFC

      WRITE(11,6000)
6000  FORMAT(' &ODDATA')

      WRITE(11,*)'  ','LDOH = ',LDOH,', ','LDOZ = ',LDOZ,', ',
     -      'LDOM = ',LDOM,', ','LDOP = ',LDOP,', ',
     -      'LDO3 = ',LDO3,', ','LDODTD = ',LDODTD,', '
      WRITE(11,*)'  ','LDODTS = ',LDODTS,', ','LDODIN = ',LDODIN,', ',
     -      'NCOD = ',NCOD,', '
      WRITE(11,*)'  ','NVODHR = '
      DO 100 J=1,KNL,6
      IBEG=J
      IEND=MIN0(KNL,J+5)
      IEL=IEND-IBEG+1
      IF(IEL.EQ.1) THEN
         WRITE(11,*)'  ',NVODHR(J),', '
      ELSE IF(IEL.EQ.2) THEN
         WRITE(11,*)'  ',NVODHR(J),', ',NVODHR(J+1),', '
      ELSE IF(IEL.EQ.3) THEN
         WRITE(11,*)'  ',NVODHR(J),', ',NVODHR(J+1),', ',
     -         NVODHR(J+2),', '
      ELSE IF(IEL.EQ.4) THEN
         WRITE(11,*)'  ',NVODHR(J),', ',NVODHR(J+1),', ',
     -         NVODHR(J+2),', ',NVODHR(J+3),', '
      ELSE IF(IEL.EQ.5) THEN
         WRITE(11,*)'  ',NVODHR(J),', ',NVODHR(J+1),', ',
     -         NVODHR(J+2),', ',NVODHR(J+3),', ',NVODHR(J+4),', '
      ELSE IF(IEL.EQ.6) THEN
         WRITE(11,*)'  ',NVODHR(J),', ',NVODHR(J+1),', ',
     -         NVODHR(J+2),', ',NVODHR(J+3),', ',NVODHR(J+4),', ',
     -         NVODHR(J+5),', '
      ENDIF
100   CONTINUE
      WRITE(11,*)'  ','VODSFC = '
      DO 200 J=1,KNL,6
      IBEG=J
      IEND=MIN0(KNL,J+5)
      IEL=IEND-IBEG+1
      IF(IEL.EQ.1) THEN
         WRITE(11,*)'  ',VODSFC(J),', '
      ELSE IF(IEL.EQ.2) THEN
         WRITE(11,*)'  ',VODSFC(J),', ',VODSFC(J+1),', '
      ELSE IF(IEL.EQ.3) THEN
         WRITE(11,*)'  ',VODSFC(J),', ',VODSFC(J+1),', ',
     -         VODSFC(J+2),', '
      ELSE IF(IEL.EQ.4) THEN
         WRITE(11,*)'  ',VODSFC(J),', ',VODSFC(J+1),', ',
     -         VODSFC(J+2),', ',VODSFC(J+3),', '
      ELSE IF(IEL.EQ.5) THEN
         WRITE(11,*)'  ',VODSFC(J),', ',VODSFC(J+1),', ',
     -         VODSFC(J+2),', ',VODSFC(J+3),', ',VODSFC(J+4),', '
      ELSE IF(IEL.EQ.6) THEN
         WRITE(11,*)'  ',VODSFC(J),', ',VODSFC(J+1),', ',
     -         VODSFC(J+2),', ',VODSFC(J+3),', ',VODSFC(J+4),', ',
     -         VODSFC(J+5),', '
      ENDIF
200   CONTINUE

      WRITE(11,6999)
6999  FORMAT(' &END')

      RETURN
      END
      SUBROUTINE WSGNML(KNL)
**    Function - to construct SGDATA namelist
**    Args in -
**               KNL     - number of model levels
**    Args out - none
**    Args for work - none
**    Com used - /SGDIAG/all elements
**    Com changed - none
**    Params used - /PARAM1/JPNL
**    Called by - DIAGWT
**    Calls - none
**    Files read - none
**    Files written - 11
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      LOGICAL LDGH,LDGZ,LDGM,LDGP,LDG3,LDSGTD,LDSGTS,LDSGIN
      COMMON /SGDIAG/LDGH,LDGZ,LDGM,LDGP,LDG3,LDSGTD,LDSGTS,LDSGIN,
     -      NCSG,NVSGHR(JPNL),VSGSFC(JPNL)
CC    NAMELIST /SGDATA/LDGH,LDGZ,LDGM,LDGP,LDG3,LDSGTD,LDSGTS,LDSGIN,
CC   -      NCSG,NVSGHR,VSGSFC

      WRITE(11,6000)
6000  FORMAT(' &SGDATA')

      WRITE(11,*)'  ','LDGH = ',LDGH,', ','LDGZ = ',LDGZ,', ',
     -      'LDGM = ',LDGM,', ','LDGP = ',LDGP,', ',
     -      'LDG3 = ',LDG3,', ','LDSGTD = ',LDSGTD,', '
      WRITE(11,*)'  ','LDSGTS = ',LDSGTS,', ','LDSGIN = ',LDSGIN,', ',
     -      'NCSG = ',NCSG,', '
      WRITE(11,*)'  ','NVSGHR = '
      DO 100 J=1,KNL,6
      IBEG=J
      IEND=MIN0(KNL,J+5)
      IEL=IEND-IBEG+1
      IF(IEL.EQ.1) THEN
         WRITE(11,*)'  ',NVSGHR(J),', '
      ELSE IF(IEL.EQ.2) THEN
         WRITE(11,*)'  ',NVSGHR(J),', ',NVSGHR(J+1),', '
      ELSE IF(IEL.EQ.3) THEN
         WRITE(11,*)'  ',NVSGHR(J),', ',NVSGHR(J+1),', ',
     -         NVSGHR(J+2),', '
      ELSE IF(IEL.EQ.4) THEN
         WRITE(11,*)'  ',NVSGHR(J),', ',NVSGHR(J+1),', ',
     -         NVSGHR(J+2),', ',NVSGHR(J+3),', '
      ELSE IF(IEL.EQ.5) THEN
         WRITE(11,*)'  ',NVSGHR(J),', ',NVSGHR(J+1),', ',
     -         NVSGHR(J+2),', ',NVSGHR(J+3),', ',NVSGHR(J+4),', '
      ELSE IF(IEL.EQ.6) THEN
         WRITE(11,*)'  ',NVSGHR(J),', ',NVSGHR(J+1),', ',
     -         NVSGHR(J+2),', ',NVSGHR(J+3),', ',NVSGHR(J+4),', ',
     -         NVSGHR(J+5),', '
      ENDIF
100   CONTINUE
      WRITE(11,*)'  ','VSGSFC = '
      DO 200 J=1,KNL,6
      IBEG=J
      IEND=MIN0(KNL,J+5)
      IEL=IEND-IBEG+1
      IF(IEL.EQ.1) THEN
         WRITE(11,*)'  ',VSGSFC(J),', '
      ELSE IF(IEL.EQ.2) THEN
         WRITE(11,*)'  ',VSGSFC(J),', ',VSGSFC(J+1),', '
      ELSE IF(IEL.EQ.3) THEN
         WRITE(11,*)'  ',VSGSFC(J),', ',VSGSFC(J+1),', ',
     -         VSGSFC(J+2),', '
      ELSE IF(IEL.EQ.4) THEN
         WRITE(11,*)'  ',VSGSFC(J),', ',VSGSFC(J+1),', ',
     -         VSGSFC(J+2),', ',VSGSFC(J+3),', '
      ELSE IF(IEL.EQ.5) THEN
         WRITE(11,*)'  ',VSGSFC(J),', ',VSGSFC(J+1),', ',
     -         VSGSFC(J+2),', ',VSGSFC(J+3),', ',VSGSFC(J+4),', '
      ELSE IF(IEL.EQ.6) THEN
         WRITE(11,*)'  ',VSGSFC(J),', ',VSGSFC(J+1),', ',
     -         VSGSFC(J+2),', ',VSGSFC(J+3),', ',VSGSFC(J+4),', ',
     -         VSGSFC(J+5),', '
      ENDIF
200   CONTINUE

      WRITE(11,6999)
6999  FORMAT(' &END')

      RETURN
      END
      SUBROUTINE WTFNML(KNL)
**    Function - to construct TFDATA namelist
**    Args in -
**               KNL     - number of model levels
**    Args out - none
**    Args for work - none
**    Com used - /TFDIAG/all elements
**    Com changed - none
**    Params used - /PARAM1/JPNL,/PARAMS/JPTFMX
**    Called by - DIAGWT
**    Calls - none
**    Files read - none
**    Files written - 11
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      LOGICAL LDFH,LDFZ,LDFM,LDFP,LDF3,LTFTOT,LTFHP,LTFLP
      COMMON /TFDIAG/LDFH,LDFZ,LDFM,LDFP,LDF3,NCTF,NVTFHR(JPNL),
     -      VTFSFC(JPNL),TFCUT,NTF,FACTTF(2,JPTFMX),NFLDTF(4,JPTFMX),
     -      LTFTOT,LTFHP,LTFLP
CC    NAMELIST /TFDATA/LDFH,LDFZ,LDFM,LDFP,LDF3,NCTF,NVTFHR,
CC   -      VTFSFC,TFCUT,NTF,FACTTF,NFLDTF,LTFTOT,LTFHP,LTFLP

      WRITE(11,6000)
6000  FORMAT(' &TFDATA')

      WRITE(11,*)'  ','LDFH = ',LDFH,', ','LDFZ = ',LDFZ,', ',
     -      'LDFM = ',LDFM,', ','LDFP = ',LDFP,', ',
     -      'LDF3 = ',LDF3,', ','NCTF = ',NCTF,', '
      WRITE(11,*)'  ','TFCUT = ',TFCUT,', ','NTF = ',NTF,', ',
     -      'LTFTOT = ',LTFTOT,', ','LTFHP = ',LTFHP,', ',
     -      'LTFLP = ',LTFLP,', '
      WRITE(11,*)'  ','NVTFHR = '
      DO 100 J=1,KNL,6
      IBEG=J
      IEND=MIN0(KNL,J+5)
      IEL=IEND-IBEG+1
      IF(IEL.EQ.1) THEN
         WRITE(11,*)'  ',NVTFHR(J),', '
      ELSE IF(IEL.EQ.2) THEN
         WRITE(11,*)'  ',NVTFHR(J),', ',NVTFHR(J+1),', '
      ELSE IF(IEL.EQ.3) THEN
         WRITE(11,*)'  ',NVTFHR(J),', ',NVTFHR(J+1),', ',
     -         NVTFHR(J+2),', '
      ELSE IF(IEL.EQ.4) THEN
         WRITE(11,*)'  ',NVTFHR(J),', ',NVTFHR(J+1),', ',
     -         NVTFHR(J+2),', ',NVTFHR(J+3),', '
      ELSE IF(IEL.EQ.5) THEN
         WRITE(11,*)'  ',NVTFHR(J),', ',NVTFHR(J+1),', ',
     -         NVTFHR(J+2),', ',NVTFHR(J+3),', ',NVTFHR(J+4),', '
      ELSE IF(IEL.EQ.6) THEN
         WRITE(11,*)'  ',NVTFHR(J),', ',NVTFHR(J+1),', ',
     -         NVTFHR(J+2),', ',NVTFHR(J+3),', ',NVTFHR(J+4),', ',
     -         NVTFHR(J+5),', '
      ENDIF
100   CONTINUE
      WRITE(11,*)'  ','VTFSFC = '
      DO 200 J=1,KNL,6
      IBEG=J
      IEND=MIN0(KNL,J+5)
      IEL=IEND-IBEG+1
      IF(IEL.EQ.1) THEN
         WRITE(11,*)'  ',VTFSFC(J),', '
      ELSE IF(IEL.EQ.2) THEN
         WRITE(11,*)'  ',VTFSFC(J),', ',VTFSFC(J+1),', '
      ELSE IF(IEL.EQ.3) THEN
         WRITE(11,*)'  ',VTFSFC(J),', ',VTFSFC(J+1),', ',
     -         VTFSFC(J+2),', '
      ELSE IF(IEL.EQ.4) THEN
         WRITE(11,*)'  ',VTFSFC(J),', ',VTFSFC(J+1),', ',
     -         VTFSFC(J+2),', ',VTFSFC(J+3),', '
      ELSE IF(IEL.EQ.5) THEN
         WRITE(11,*)'  ',VTFSFC(J),', ',VTFSFC(J+1),', ',
     -         VTFSFC(J+2),', ',VTFSFC(J+3),', ',VTFSFC(J+4),', '
      ELSE IF(IEL.EQ.6) THEN
         WRITE(11,*)'  ',VTFSFC(J),', ',VTFSFC(J+1),', ',
     -         VTFSFC(J+2),', ',VTFSFC(J+3),', ',VTFSFC(J+4),', ',
     -         VTFSFC(J+5),', '
      ENDIF
200   CONTINUE
      WRITE(11,*)'  ','FACTTF = '
      DO 300 J=1,JPTFMX,5
      IBEG=J
      IEND=MIN0(KNL,J+4)
      IEL=IEND-IBEG+1
      IF(IEL.EQ.1) THEN
         WRITE(11,*)'  ',FACTTF(1,J),', ',FACTTF(2,J),', '
      ELSE IF(IEL.EQ.2) THEN
         WRITE(11,*)'  ',FACTTF(1,J),', ',FACTTF(2,J),', ',
     -         FACTTF(1,J+1),', ',FACTTF(2,J+1),', '
      ELSE IF(IEL.EQ.3) THEN
         WRITE(11,*)'  ',FACTTF(1,J),', ',FACTTF(2,J),', ',
     -         FACTTF(1,J+1),', ',FACTTF(2,J+1),', ',
     -         FACTTF(1,J+2),', ',FACTTF(2,J+2),', '
      ELSE IF(IEL.EQ.4) THEN
         WRITE(11,*)'  ',FACTTF(1,J),', ',FACTTF(2,J),', ',
     -         FACTTF(1,J+1),', ',FACTTF(2,J+1),', ',
     -         FACTTF(1,J+2),', ',FACTTF(2,J+2),', ',
     -         FACTTF(1,J+3),', ',FACTTF(2,J+3),', '
      ELSE IF(IEL.EQ.5) THEN
         WRITE(11,*)'  ',FACTTF(1,J),', ',FACTTF(2,J),', ',
     -         FACTTF(1,J+1),', ',FACTTF(2,J+1),', ',
     -         FACTTF(1,J+2),', ',FACTTF(2,J+2),', ',
     -         FACTTF(1,J+3),', ',FACTTF(2,J+3),', ',
     -         FACTTF(1,J+4),', ',FACTTF(2,J+4),', '
      ENDIF
300   CONTINUE
      WRITE(11,*)'  ','NFLDTF = '
      DO 400 J=1,JPTFMX,5
      IBEG=J
      IEND=MIN0(KNL,J+4)
      IEL=IEND-IBEG+1
      IF(IEL.EQ.1) THEN
         WRITE(11,*)'  ',NFLDTF(1,J),', ',NFLDTF(2,J),', ',
     -         NFLDTF(3,J),', ',NFLDTF(4,J),', '
      ELSE IF(IEL.EQ.2) THEN
         WRITE(11,*)'  ',NFLDTF(1,J),', ',NFLDTF(2,J),', ',
     -         NFLDTF(3,J),', ',NFLDTF(4,J),', ',
     -         NFLDTF(1,J+1),', ',NFLDTF(2,J+1),', ',
     -         NFLDTF(3,J+1),', ',NFLDTF(4,J+1),', '
      ELSE IF(IEL.EQ.3) THEN
         WRITE(11,*)'  ',NFLDTF(1,J),', ',NFLDTF(2,J),', ',
     -         NFLDTF(3,J),', ',NFLDTF(4,J),', ',
     -         NFLDTF(1,J+1),', ',NFLDTF(2,J+1),', ',
     -         NFLDTF(3,J+1),', ',NFLDTF(4,J+1),', ',
     -         NFLDTF(1,J+2),', ',NFLDTF(2,J+2),', ',
     -         NFLDTF(3,J+2),', ',NFLDTF(4,J+2),', '
      ELSE IF(IEL.EQ.4) THEN
         WRITE(11,*)'  ',NFLDTF(1,J),', ',NFLDTF(2,J),', ',
     -         NFLDTF(3,J),', ',NFLDTF(4,J),', ',
     -         NFLDTF(1,J+1),', ',NFLDTF(2,J+1),', ',
     -         NFLDTF(3,J+1),', ',NFLDTF(4,J+1),', ',
     -         NFLDTF(1,J+2),', ',NFLDTF(2,J+2),', ',
     -         NFLDTF(3,J+2),', ',NFLDTF(4,J+2),', ',
     -         NFLDTF(1,J+3),', ',NFLDTF(2,J+3),', ',
     -         NFLDTF(3,J+3),', ',NFLDTF(4,J+3),', '
      ELSE IF(IEL.EQ.5) THEN
         WRITE(11,*)'  ',NFLDTF(1,J),', ',NFLDTF(2,J),', ',
     -         NFLDTF(3,J),', ',NFLDTF(4,J),', ',
     -         NFLDTF(1,J+1),', ',NFLDTF(2,J+1),', ',
     -         NFLDTF(3,J+1),', ',NFLDTF(4,J+1),', ',
     -         NFLDTF(1,J+2),', ',NFLDTF(2,J+2),', ',
     -         NFLDTF(3,J+2),', ',NFLDTF(4,J+2),', ',
     -         NFLDTF(1,J+3),', ',NFLDTF(2,J+3),', ',
     -         NFLDTF(3,J+3),', ',NFLDTF(4,J+3),', ',
     -         NFLDTF(1,J+4),', ',NFLDTF(2,J+4),', ',
     -         NFLDTF(3,J+4),', ',NFLDTF(4,J+4),', '
      ENDIF
400   CONTINUE

      WRITE(11,6999)
6999  FORMAT(' &END')

      RETURN
      END
      SUBROUTINE WTRNML(KNL)
**    Function - to construct TRDATA namelist
**    Args in -
**               KNL     - number of model levels
**    Args out - none
**    Args for work - none
**    Com used - /TRDIAG/all elements
**    Com changed - none
**    Params used - /PARAM1/JPNL
**    Called by - DIAGWT
**    Calls - none
**    Files read - none
**    Files written - 11
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      LOGICAL LDTH,LDTZ,LDTM,LDTP,LDT3,LDTRTD,LDTRTS,LDTRIN
      COMMON /TRDIAG/LDTH,LDTZ,LDTM,LDTP,LDT3,LDTRTD,LDTRTS,LDTRIN,
     -      NCTR,NVTRHR(JPNL),VTRSFC(JPNL)
CC      NAMELIST /TRDATA/LDTH,LDTZ,LDTM,LDTP,LDT3,LDTRTD,LDTRTS,LDTRIN,
CC   -      NCTR,NVTRHR,VTRSFC

      WRITE(11,6000)
6000  FORMAT(' &TRDATA')

      WRITE(11,*)'  ','LDTH = ',LDTH,', ','LDTZ = ',LDTZ,', ',
     -      'LDTM = ',LDTM,', ','LDTP = ',LDTP,', ',
     -      'LDT3 = ',LDT3,', ','LDTRTD = ',LDTRTD,', '
      WRITE(11,*)'  ','LDTRTS = ',LDTRTS,', ','LDTRIN = ',LDTRIN,', ',
     -      'NCTR = ',NCTR,', '
      WRITE(11,*)'  ','NVTRHR = '
      DO 100 J=1,KNL,6
      IBEG=J
      IEND=MIN0(KNL,J+5)
      IEL=IEND-IBEG+1
      IF(IEL.EQ.1) THEN
         WRITE(11,*)'  ',NVTRHR(J),', '
      ELSE IF(IEL.EQ.2) THEN
         WRITE(11,*)'  ',NVTRHR(J),', ',NVTRHR(J+1),', '
      ELSE IF(IEL.EQ.3) THEN
         WRITE(11,*)'  ',NVTRHR(J),', ',NVTRHR(J+1),', ',
     -         NVTRHR(J+2),', '
      ELSE IF(IEL.EQ.4) THEN
         WRITE(11,*)'  ',NVTRHR(J),', ',NVTRHR(J+1),', ',
     -         NVTRHR(J+2),', ',NVTRHR(J+3),', '
      ELSE IF(IEL.EQ.5) THEN
         WRITE(11,*)'  ',NVTRHR(J),', ',NVTRHR(J+1),', ',
     -         NVTRHR(J+2),', ',NVTRHR(J+3),', ',NVTRHR(J+4),', '
      ELSE IF(IEL.EQ.6) THEN
         WRITE(11,*)'  ',NVTRHR(J),', ',NVTRHR(J+1),', ',
     -         NVTRHR(J+2),', ',NVTRHR(J+3),', ',NVTRHR(J+4),', ',
     -         NVTRHR(J+5),', '
      ENDIF
100   CONTINUE
      WRITE(11,*)'  ','VTRSFC = '
      DO 200 J=1,KNL,6
      IBEG=J
      IEND=MIN0(KNL,J+5)
      IEL=IEND-IBEG+1
      IF(IEL.EQ.1) THEN
         WRITE(11,*)'  ',VTRSFC(J),', '
      ELSE IF(IEL.EQ.2) THEN
         WRITE(11,*)'  ',VTRSFC(J),', ',VTRSFC(J+1),', '
      ELSE IF(IEL.EQ.3) THEN
         WRITE(11,*)'  ',VTRSFC(J),', ',VTRSFC(J+1),', ',
     -         VTRSFC(J+2),', '
      ELSE IF(IEL.EQ.4) THEN
         WRITE(11,*)'  ',VTRSFC(J),', ',VTRSFC(J+1),', ',
     -         VTRSFC(J+2),', ',VTRSFC(J+3),', '
      ELSE IF(IEL.EQ.5) THEN
         WRITE(11,*)'  ',VTRSFC(J),', ',VTRSFC(J+1),', ',
     -         VTRSFC(J+2),', ',VTRSFC(J+3),', ',VTRSFC(J+4),', '
      ELSE IF(IEL.EQ.6) THEN
         WRITE(11,*)'  ',VTRSFC(J),', ',VTRSFC(J+1),', ',
     -         VTRSFC(J+2),', ',VTRSFC(J+3),', ',VTRSFC(J+4),', ',
     -         VTRSFC(J+5),', '
      ENDIF
200   CONTINUE

      WRITE(11,6999)
6999  FORMAT(' &END')

      RETURN
      END
      SUBROUTINE WZFNML(KNL)
**    Function - to construct ZFDATA namelist
**    Args in -
**               KNL     - number of model levels
**    Args out - none
**    Args for work - none
**    Com used - /ZFDIAG/all elements
**    Com changed - none
**    Params used - /PARAM1/JPNL,/PARAMS/JPZFMX
**    Called by - DIAGWT
**    Calls - none
**    Files read - none
**    Files written - 11
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      LOGICAL LZFEDF,LZFMNF,LDZFIN,LDZFTD,LDZFTS,LDZFZM,LDZFLM
      COMMON /ZFDIAG/NCZF,NZFPR(JPZFMX),NZFPL(JPZFMX),
     -      CINTZF(JPZFMX),NVZFHR(JPNL),
     -      NZF,FACTZF(2,JPZFMX),NFLDZF(4,JPZFMX),VZFSFC(JPNL),
     -      LZFEDF,LZFMNF,LDZFIN,LDZFTD,LDZFTS,LDZFZM,LDZFLM
CC    NAMELIST /ZFDATA/NCZF,NZFPR,NZFPL,CINTZF,NZF,FACTZF,NFLDZF,
CC   -      LZFEDF,LZFMNF,LDZFIN,LDZFTD,LDZFTS,LDZFZM,LDZFLM,VZFSRC,
CC   -      NVZFHR

      WRITE(11,6000)
6000  FORMAT(' &ZFDATA')

      WRITE(11,*)'  ','NZF = ',NZF,', ','LZFEDF = ',LZFEDF,', ',
     -      'LZFMNF = ',LZFMNF,', ','LDZFIN = ',LDZFIN,', ',
     -      'LDZFTD = ',LDZFTD,', '
      WRITE(11,*)'  ','LDZFTS = ',LDZFTS,', ','LDZFZM = ',LDZFZM,', ',
     -      'LDZFLM = ',LDZFLM,', ','NCZF = ',NCZF,', '
      WRITE(11,*)'  ','NZFPR = '
      DO 100 J=1,JPZFMX,6
      IBEG=J
      IEND=MIN0(JPZFMX,J+5)
      IEL=IEND-IBEG+1
      IF(IEL.EQ.1) THEN
         WRITE(11,*)'  ',NZFPR(J),', '
      ELSE IF(IEL.EQ.2) THEN
         WRITE(11,*)'  ',NZFPR(J),', ',NZFPR(J+1),', '
      ELSE IF(IEL.EQ.3) THEN
         WRITE(11,*)'  ',NZFPR(J),', ',NZFPR(J+1),', ',
     -         NZFPR(J+2),', '
      ELSE IF(IEL.EQ.4) THEN
         WRITE(11,*)'  ',NZFPR(J),', ',NZFPR(J+1),', ',
     -         NZFPR(J+2),', ',NZFPR(J+3),', '
      ELSE IF(IEL.EQ.5) THEN
         WRITE(11,*)'  ',NZFPR(J),', ',NZFPR(J+1),', ',
     -         NZFPR(J+2),', ',NZFPR(J+3),', ',NZFPR(J+4),', '
      ELSE IF(IEL.EQ.6) THEN
         WRITE(11,*)'  ',NZFPR(J),', ',NZFPR(J+1),', ',
     -         NZFPR(J+2),', ',NZFPR(J+3),', ',NZFPR(J+4),', ',
     -         NZFPR(J+5),', '
      ENDIF
100   CONTINUE
      WRITE(11,*)'  ','NZFPL = '
      DO 101 J=1,JPZFMX,6
      IBEG=J
      IEND=MIN0(JPZFMX,J+5)
      IEL=IEND-IBEG+1
      IF(IEL.EQ.1) THEN
         WRITE(11,*)'  ',NZFPL(J),', '
      ELSE IF(IEL.EQ.2) THEN
         WRITE(11,*)'  ',NZFPL(J),', ',NZFPL(J+1),', '
      ELSE IF(IEL.EQ.3) THEN
         WRITE(11,*)'  ',NZFPL(J),', ',NZFPL(J+1),', ',
     -         NZFPL(J+2),', '
      ELSE IF(IEL.EQ.4) THEN
         WRITE(11,*)'  ',NZFPL(J),', ',NZFPL(J+1),', ',
     -         NZFPL(J+2),', ',NZFPL(J+3),', '
      ELSE IF(IEL.EQ.5) THEN
         WRITE(11,*)'  ',NZFPL(J),', ',NZFPL(J+1),', ',
     -         NZFPL(J+2),', ',NZFPL(J+3),', ',NZFPL(J+4),', '
      ELSE IF(IEL.EQ.6) THEN
         WRITE(11,*)'  ',NZFPL(J),', ',NZFPL(J+1),', ',
     -         NZFPL(J+2),', ',NZFPL(J+3),', ',NZFPL(J+4),', ',
     -         NZFPL(J+5),', '
      ENDIF
101   CONTINUE
      WRITE(11,*)'  ','CINTZF = '
      DO 102 J=1,JPZFMX,6
      IBEG=J
      IEND=MIN0(JPZFMX,J+5)
      IEL=IEND-IBEG+1
      IF(IEL.EQ.1) THEN
         WRITE(11,*)'  ',CINTZF(J),', '
      ELSE IF(IEL.EQ.2) THEN
         WRITE(11,*)'  ',CINTZF(J),', ',CINTZF(J+1),', '
      ELSE IF(IEL.EQ.3) THEN
         WRITE(11,*)'  ',CINTZF(J),', ',CINTZF(J+1),', ',
     -         CINTZF(J+2),', '
      ELSE IF(IEL.EQ.4) THEN
         WRITE(11,*)'  ',CINTZF(J),', ',CINTZF(J+1),', ',
     -         CINTZF(J+2),', ',CINTZF(J+3),', '
      ELSE IF(IEL.EQ.5) THEN
         WRITE(11,*)'  ',CINTZF(J),', ',CINTZF(J+1),', ',
     -         CINTZF(J+2),', ',CINTZF(J+3),', ',CINTZF(J+4),', '
      ELSE IF(IEL.EQ.6) THEN
         WRITE(11,*)'  ',CINTZF(J),', ',CINTZF(J+1),', ',
     -         CINTZF(J+2),', ',CINTZF(J+3),', ',CINTZF(J+4),', ',
     -         CINTZF(J+5),', '
      ENDIF
102   CONTINUE
      WRITE(11,*)'  ','NVZFHR = '
      DO 103 J=1,KNL,6
      IBEG=J
      IEND=MIN0(KNL,J+5)
      IEL=IEND-IBEG+1
      IF(IEL.EQ.1) THEN
         WRITE(11,*)'  ',NVZFHR(J),', '
      ELSE IF(IEL.EQ.2) THEN
         WRITE(11,*)'  ',NVZFHR(J),', ',NVZFHR(J+1),', '
      ELSE IF(IEL.EQ.3) THEN
         WRITE(11,*)'  ',NVZFHR(J),', ',NVZFHR(J+1),', ',
     -         NVZFHR(J+2),', '
      ELSE IF(IEL.EQ.4) THEN
         WRITE(11,*)'  ',NVZFHR(J),', ',NVZFHR(J+1),', ',
     -         NVZFHR(J+2),', ',NVZFHR(J+3),', '
      ELSE IF(IEL.EQ.5) THEN
         WRITE(11,*)'  ',NVZFHR(J),', ',NVZFHR(J+1),', ',
     -         NVZFHR(J+2),', ',NVZFHR(J+3),', ',NVZFHR(J+4),', '
      ELSE IF(IEL.EQ.6) THEN
         WRITE(11,*)'  ',NVZFHR(J),', ',NVZFHR(J+1),', ',
     -         NVZFHR(J+2),', ',NVZFHR(J+3),', ',NVZFHR(J+4),', ',
     -         NVZFHR(J+5),', '
      ENDIF
103   CONTINUE
      WRITE(11,*)'  ','FACTZF = '
      DO 300 J=1,JPZFMX,5
      IBEG=J
      IEND=MIN0(JPZFMX,J+4)
      IEL=IEND-IBEG+1
      IF(IEL.EQ.1) THEN
         WRITE(11,*)'  ',FACTZF(1,J),', ',FACTZF(2,J),', '
      ELSE IF(IEL.EQ.2) THEN
         WRITE(11,*)'  ',FACTZF(1,J),', ',FACTZF(2,J),', ',
     -         FACTZF(1,J+1),', ',FACTZF(2,J+1),', '
      ELSE IF(IEL.EQ.3) THEN
         WRITE(11,*)'  ',FACTZF(1,J),', ',FACTZF(2,J),', ',
     -         FACTZF(1,J+1),', ',FACTZF(2,J+1),', ',
     -         FACTZF(1,J+2),', ',FACTZF(2,J+2),', '
      ELSE IF(IEL.EQ.4) THEN
         WRITE(11,*)'  ',FACTZF(1,J),', ',FACTZF(2,J),', ',
     -         FACTZF(1,J+1),', ',FACTZF(2,J+1),', ',
     -         FACTZF(1,J+2),', ',FACTZF(2,J+2),', ',
     -         FACTZF(1,J+3),', ',FACTZF(2,J+3),', '
      ELSE IF(IEL.EQ.5) THEN
         WRITE(11,*)'  ',FACTZF(1,J),', ',FACTZF(2,J),', ',
     -         FACTZF(1,J+1),', ',FACTZF(2,J+1),', ',
     -         FACTZF(1,J+2),', ',FACTZF(2,J+2),', ',
     -         FACTZF(1,J+3),', ',FACTZF(2,J+3),', ',
     -         FACTZF(1,J+4),', ',FACTZF(2,J+4),', '
      ENDIF
300   CONTINUE
      WRITE(11,*)'  ','NFLDZF = '
      DO 400 J=1,JPZFMX,5
      IBEG=J
      IEND=MIN0(JPZFMX,J+4)
      IEL=IEND-IBEG+1
      IF(IEL.EQ.1) THEN
         WRITE(11,*)'  ',NFLDZF(1,J),', ',NFLDZF(2,J),', ',
     -         NFLDZF(3,J),', ',NFLDZF(4,J),', '
      ELSE IF(IEL.EQ.2) THEN
         WRITE(11,*)'  ',NFLDZF(1,J),', ',NFLDZF(2,J),', ',
     -         NFLDZF(3,J),', ',NFLDZF(4,J),', ',
     -         NFLDZF(1,J+1),', ',NFLDZF(2,J+1),', ',
     -         NFLDZF(3,J+1),', ',NFLDZF(4,J+1),', '
      ELSE IF(IEL.EQ.3) THEN
         WRITE(11,*)'  ',NFLDZF(1,J),', ',NFLDZF(2,J),', ',
     -         NFLDZF(3,J),', ',NFLDZF(4,J),', ',
     -         NFLDZF(1,J+1),', ',NFLDZF(2,J+1),', ',
     -         NFLDZF(3,J+1),', ',NFLDZF(4,J+1),', ',
     -         NFLDZF(1,J+2),', ',NFLDZF(2,J+2),', ',
     -         NFLDZF(3,J+2),', ',NFLDZF(4,J+2),', '
      ELSE IF(IEL.EQ.4) THEN
         WRITE(11,*)'  ',NFLDZF(1,J),', ',NFLDZF(2,J),', ',
     -         NFLDZF(3,J),', ',NFLDZF(4,J),', ',
     -         NFLDZF(1,J+1),', ',NFLDZF(2,J+1),', ',
     -         NFLDZF(3,J+1),', ',NFLDZF(4,J+1),', ',
     -         NFLDZF(1,J+2),', ',NFLDZF(2,J+2),', ',
     -         NFLDZF(3,J+2),', ',NFLDZF(4,J+2),', ',
     -         NFLDZF(1,J+3),', ',NFLDZF(2,J+3),', ',
     -         NFLDZF(3,J+3),', ',NFLDZF(4,J+3),', '
      ELSE IF(IEL.EQ.5) THEN
         WRITE(11,*)'  ',NFLDZF(1,J),', ',NFLDZF(2,J),', ',
     -         NFLDZF(3,J),', ',NFLDZF(4,J),', ',
     -         NFLDZF(1,J+1),', ',NFLDZF(2,J+1),', ',
     -         NFLDZF(3,J+1),', ',NFLDZF(4,J+1),', ',
     -         NFLDZF(1,J+2),', ',NFLDZF(2,J+2),', ',
     -         NFLDZF(3,J+2),', ',NFLDZF(4,J+2),', ',
     -         NFLDZF(1,J+3),', ',NFLDZF(2,J+3),', ',
     -         NFLDZF(3,J+3),', ',NFLDZF(4,J+3),', ',
     -         NFLDZF(1,J+4),', ',NFLDZF(2,J+4),', ',
     -         NFLDZF(3,J+4),', ',NFLDZF(4,J+4),', '
      ENDIF
400   CONTINUE
      WRITE(11,*)'  ','VZFSFC = '
      DO 200 J=1,KNL,6
      IBEG=J
      IEND=MIN0(KNL,J+5)
      IEL=IEND-IBEG+1
      IF(IEL.EQ.1) THEN
         WRITE(11,*)'  ',VZFSFC(J),', '
      ELSE IF(IEL.EQ.2) THEN
         WRITE(11,*)'  ',VZFSFC(J),', ',VZFSFC(J+1),', '
      ELSE IF(IEL.EQ.3) THEN
         WRITE(11,*)'  ',VZFSFC(J),', ',VZFSFC(J+1),', ',
     -         VZFSFC(J+2),', '
      ELSE IF(IEL.EQ.4) THEN
         WRITE(11,*)'  ',VZFSFC(J),', ',VZFSFC(J+1),', ',
     -         VZFSFC(J+2),', ',VZFSFC(J+3),', '
      ELSE IF(IEL.EQ.5) THEN
         WRITE(11,*)'  ',VZFSFC(J),', ',VZFSFC(J+1),', ',
     -         VZFSFC(J+2),', ',VZFSFC(J+3),', ',VZFSFC(J+4),', '
      ELSE IF(IEL.EQ.6) THEN
         WRITE(11,*)'  ',VZFSFC(J),', ',VZFSFC(J+1),', ',
     -         VZFSFC(J+2),', ',VZFSFC(J+3),', ',VZFSFC(J+4),', ',
     -         VZFSFC(J+5),', '
      ENDIF
200   CONTINUE

      WRITE(11,6999)
6999  FORMAT(' &END')

      RETURN
      END
      SUBROUTINE CHKPH
**    Function - to check PH data input
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /COMDAT/LWRITE,LASCII,/ALOWPH/NPHFLD,/ERRMSG/NCERR,
**          /MODELC/LHISPH
**    Com changed - /PHDIAG/NPHPR,NPHPL,/PHDIAG/NPH,/DIAGTP/LDPH,
**          /ERRMSG/NFATAL,NWARN
**    Called by - DIAGCK
**    Calls - LATCHK,LNGCHK
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      PARAMETER(JPNL=100)
      COMMON /ALOWPH/NPHFLD(JPPHMX)
      LOGICAL LWRITE,LASCII,LSCRAT,LUTF14,LROGR,LMASSW,LAVMSK,
     -      LAVSIG
      CHARACTER*2 YTYPSF
      CHARACTER*6 YDUM6
      CHARACTER*8 YNAME(2)
      COMMON /COMDAT/BEGDAY,ENDDAY,LWRITE,LASCII,LSCRAT,
     -      LUTF14,LROGR,NSIGFG,NFREQD,NFREQA,NFREQP,RNTAPE,
     -      NASCII,LMASSW,LAVMSK,LAVSIG,YNAME,YTYPSF,YDUM6
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      LOGICAL LGCMTR,LHISPH,LHISXP,LDRY,LDIABH,LBL,LVD
      COMMON /MODELC/TSPD,NYEAR,NDAY,NMIN,LGCMTR,NL,TMEAN(JPNL),
     -      LHISPH,LHISXP,LDRY,LDIABH,BEGDYP,LBL,LVD,AKVV,CD
      LOGICAL LDPHLL,LDPHZM,LDPHMM,LDPHGM,LDPHTD,LDPHIN,
     -      LDPHTS
      COMMON /PHDIAG/NCPH,NPHPR(JPPHMX),NPHPL(JPPHMX),
     -      CINTPH(JPPHMX),LDPHLL,LDPHZM,LDPHMM,LDPHGM,LDPHTD,
     -      LDPHIN,VPHEW,VPHEWA(2),VPHEWS(2),VPHNS,VPHNSA(2),
     -      VPHNSS(2),VPHAA(4),VPHAS(4),LDPHTS,NPH
      LOGICAL LOERR,LOVALU,LOBOX

      LOERR=.FALSE.

      IF(LDPH.AND..NOT.LHISPH) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6500)
6500     FORMAT(1X,'CHKPH: PH HISTORY DATA DOES NOT EXIST; PH ',
     -         'DIAGNOSTICS TURNED OFF')
         LDPH=.FALSE.
         RETURN
      ENDIF

**    Check that printing has been requested - if not, ensure
**          NPHPR=0 for all elements
      IF(.NOT.LWRITE) THEN
         IF(NPHPR(1).NE.0) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6000)
6000        FORMAT(1X,'CHKPH: NPHPR VALUES CHANGED - NO PRINTING')
            DO 1000 K=1,JPPHMX
1000        NPHPR(K)=0
**          Reset value of NPH
            NPH=0
            DO 1001 K=1,JPPHMX
            IF(NPHPL(K).GT.0) NPH=NPH+1
1001        CONTINUE
         ENDIF
      ENDIF

**    Check that UTFs are requested - if not, ensure NPHPL=0
**          for all elements
      IF(.NOT.LASCII) THEN
         IF(NPHPL(1).NE.0) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6010)
6010        FORMAT(1X,'CHKPH: NPHPL VALUES CHANGED - NO PLOTTING')
            DO 1100 K=1,JPPHMX
1100        NPHPL(K)=0
**          Reset value of NPH
            NPH=0
            DO 1101 K=1,JPPHMX
            IF(NPHPR(K).GT.0) NPH=NPH+1
1101        CONTINUE
         ENDIF
      ENDIF

**    Check valid code numbers in NPHPR
      IF(NPHPR(1).NE.0) THEN
         DO 2000 K=1,JPPHMX
         LOVALU=.FALSE.
         DO 2100 J=1,JPPHMX
         IF((NPHPR(K).EQ.NPHFLD(J)).OR.(NPHPR(K).EQ.0)) LOVALU=.TRUE.
2100     CONTINUE
         IF(.NOT.LOVALU) THEN
            LOERR=.TRUE.
            NFATAL=NFATAL+1
            WRITE(NCERR,6020) NPHPR(K)
6020        FORMAT(1X,'CHKPH: ERROR IN NPHPR VALUE ',I4)
         ENDIF
2000     CONTINUE
      ENDIF

**    Check valid code numbers in NPHPL
      IF(NPHPL(1).NE.0) THEN
         DO 3000 K=1,JPPHMX
         LOVALU=.FALSE.
         DO 3100 J=1,JPPHMX
         IF((NPHPL(K).EQ.NPHFLD(J)).OR.(NPHPL(K).EQ.0)) LOVALU=.TRUE.
3100     CONTINUE
         IF(.NOT.LOVALU) THEN
            LOERR=.TRUE.
            NFATAL=NFATAL+1
            WRITE(NCERR,6030) NPHPL(K)
6030        FORMAT(1X,'CHKPH: ERROR IN NPHPL VALUE ',I4)
         ENDIF
3000     CONTINUE
      ENDIF

**    Switch off diagnostics if NPH is zero
      IF(NPH.EQ.0.AND.LDPH) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6035)
6035     FORMAT(1X,'CHKPH: PH DIAGNOSTICS SWITCHED OFF, NPH=0')
         LDPH=.FALSE.
         RETURN
      ENDIF
      IF(.NOT.LDPH) RETURN

**    Check valid latitudes for EW slices
      CALL LATCHK(VPHEW,1,-999.0,'VPHEW',LOERR)
      CALL LATCHK(VPHEWA,2,-999.0,'VPHEWA',LOERR)
      CALL LATCHK(VPHEWS,2,-999.0,'VPHEWS',LOERR)

**    Check valid longitudes for NS slices
      CALL LNGCHK(VPHNS,1,-999.0,'VPHNS',LOERR)
      CALL LNGCHK(VPHNSA,2,-999.0,'VPHNSA',LOERR)
      CALL LNGCHK(VPHNSS,2,-999.0,'VPHNSS',LOERR)

**    Check valid box limits
      CALL LATCHK(VPHAA(1),2,-999.0,'VPHAA',LOERR)
      CALL LATCHK(VPHAS(1),2,-999.0,'VPHAS',LOERR)
      CALL LNGCHK(VPHAA(3),2,-999.0,'VPHAA',LOERR)
      CALL LNGCHK(VPHAS(3),2,-999.0,'VPHAS',LOERR)

**    Switch off box calculations if one lat/long limit=-999.0
      LOBOX=.TRUE.
      DO 4000 J=1,4
      IF(VPHAA(J).LT.-900.0) LOBOX=.FALSE.
4000  CONTINUE
      IF(.NOT.LOBOX) THEN
         DO 4100 J=1,4
         VPHAA(J)=-999.0
4100     CONTINUE
      ENDIF

      LOBOX=.TRUE.
      DO 5000 J=1,4
      IF(VPHAS(J).LT.-900.0) LOBOX=.FALSE.
5000  CONTINUE
      IF(.NOT.LOBOX) THEN
         DO 5100 J=1,4
         VPHAS(J)=-999.0
5100     CONTINUE
      ENDIF

**    Check that NPH is non-zero, and less than JPPHMX
      IF(NPH.LE.0.OR.(NPH.GT.JPPHMX)) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6040)
6040     FORMAT(1X,'CHKPH : INVALID VALUE FOR NPH')
         LOERR=.TRUE.
      ENDIF

CC    IF(LOERR) STOP
      RETURN
      END
      SUBROUTINE PHEXTR(YPCREC)
**    Function - to extract information from PH data and update
**          appropriate common blocks
**    Args in -
**               YPCREC  - character*80 variable containing data
**    Args out - none
**    Args for work - none
**    Com used - /ALOWPH/NPHFLD,/ERRMSG/NCERR
**    Com changed - /PHDIAG/all elements,/ERRMSG/NFATAL
**    Called by - PHPROC
**    Calls - INTCPY,SEARCH,LASTCH,ITRANS,LPROC,IMULTP,RMULTP,ERRSTP
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /ALOWPH/NPHFLD(JPPHMX)
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDPHLL,LDPHZM,LDPHMM,LDPHGM,LDPHTD,LDPHIN,
     -      LDPHTS
      COMMON /PHDIAG/NCPH,NPHPR(JPPHMX),NPHPL(JPPHMX),
     -      CINTPH(JPPHMX),LDPHLL,LDPHZM,LDPHMM,LDPHGM,LDPHTD,
     -      LDPHIN,VPHEW,VPHEWA(2),VPHEWS(2),VPHNS,VPHNSA(2),
     -      VPHNSS(2),VPHAA(4),VPHAS(4),LDPHTS,NPH
      LOGICAL LOFIND,LOALL
      CHARACTER*1 YOCOM,YOEQL
      CHARACTER*2 YOKW2
      CHARACTER*4 YOKW4
      CHARACTER*5 YOKW5
      CHARACTER*7 YOKW7
      CHARACTER*9 YOKW9
      CHARACTER*10 YOKW10
      CHARACTER*11 YOKW11
      CHARACTER*13 YOKW13
      CHARACTER*14 YOKW14
      CHARACTER*80 YPCREC

      YOEQL='='
      YOCOM=','
**    Search for the keyword in YPCREC and then determine the field
**          value
      LOFIND=.FALSE.

      YOKW10='O/PCHANNEL'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
**       Note that IPOSN and IPOSX can take the same value
         IF(IERR.EQ.998) GO TO 1000
         CALL ITRANS(YPCREC,IPOSN,IPOSX,NCPH,12)
         RETURN
      ENDIF

      YOKW14='NUMBEROFFIELDS'
      CALL SEARCH(YPCREC,YOKW14,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
**       Note that IPOSN and IPOSX can take the same value
         IF(IERR.EQ.998) GO TO 1000
         CALL ITRANS(YPCREC,IPOSN,IPOSX,NPH,12)
         RETURN
      ENDIF

**    Else
      YOKW7='LATLONG'
      CALL SEARCH(YPCREC,YOKW7,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDPHLL)
         RETURN
      ENDIF

**    Else
      YOKW9='ZONALMEAN'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDPHZM)
         RETURN
      ENDIF

**    Else
      YOKW9='MERIDMEAN'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDPHMM)
         RETURN
      ENDIF

**    Else
      YOKW10='GLOBALMEAN'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDPHGM)
         RETURN
      ENDIF

**    Else
      YOKW9='TAVERAGED'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDPHTD)
         RETURN
      ENDIF

**    Else
      YOKW13='INSTANTANEOUS'
      CALL SEARCH(YPCREC,YOKW13,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDPHIN)
         RETURN
      ENDIF

**    Else
      YOKW10='PRINTFIELD'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            CALL INTCPY(NPHFLD,JPPHMX,NPHPR)
            RETURN
         ENDIF

         CALL IMULTP(YPCREC,YOEQL,YOCOM,NPHPR,JPPHMX)
         RETURN
      ENDIF

**    Else
      YOKW9='PLOTFIELD'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            CALL INTCPY(NPHFLD,JPPHMX,NPHPL)
            RETURN
         ENDIF

         CALL IMULTP(YPCREC,YOEQL,YOCOM,NPHPL,JPPHMX)
         RETURN
      ENDIF

**    Else
      YOKW2='CI'
      CALL SEARCH(YPCREC,YOKW2,LOFIND)
      IF(LOFIND) THEN
         CALL RMULTP(YPCREC,YOEQL,YOCOM,CINTPH,JPPHMX)
         RETURN
      ENDIF

**    Else
      YOKW7='WESLICE'
      CALL SEARCH(YPCREC,YOKW7,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
         IF(IERR.NE.0) GO TO 1000
         CALL RTRANS(YPCREC,IPOSN,IPOSX,VPHEW,12)
         RETURN
      ENDIF

**    Else
      YOKW9='WEAVERAGE'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
         CALL RMULTP(YPCREC,YOEQL,YOCOM,VPHEWA,2)
         RETURN
      ENDIF

**    Else
      YOKW5='WESUM'
      CALL SEARCH(YPCREC,YOKW5,LOFIND)
      IF(LOFIND) THEN
         CALL RMULTP(YPCREC,YOEQL,YOCOM,VPHEWS,2)
         RETURN
      ENDIF

**    Else
      YOKW7='NSSLICE'
      CALL SEARCH(YPCREC,YOKW7,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
         IF(IERR.NE.0) GO TO 1000
         CALL RTRANS(YPCREC,IPOSN,IPOSX,VPHNS,12)
         RETURN
      ENDIF

**    Else
      YOKW9='NSAVERAGE'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
         CALL RMULTP(YPCREC,YOEQL,YOCOM,VPHNSA,2)
         RETURN
      ENDIF

**    Else
      YOKW5='NSSUM'
      CALL SEARCH(YPCREC,YOKW5,LOFIND)
      IF(LOFIND) THEN
         CALL RMULTP(YPCREC,YOEQL,YOCOM,VPHNSS,2)
         RETURN
      ENDIF

**    Else
      YOKW11='AREAAVERAGE'
      CALL SEARCH(YPCREC,YOKW11,LOFIND)
      IF(LOFIND) THEN
         CALL RMULTP(YPCREC,YOEQL,YOCOM,VPHAA,4)
         RETURN
      ENDIF

**    Else
      YOKW7='AREASUM'
      CALL SEARCH(YPCREC,YOKW7,LOFIND)
      IF(LOFIND) THEN
         CALL RMULTP(YPCREC,YOEQL,YOCOM,VPHAS,4)
         RETURN
      ENDIF

**    Else
      YOKW7='TSERIES'
      CALL SEARCH(YPCREC,YOKW7,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDPHTS)
         RETURN
      ENDIF

**    Else
**    Keyword not located
      NFATAL=NFATAL+1
      WRITE(NCERR,6030) YPCREC
6030  FORMAT(1X,'KEYWORD NOT LOCATED FOR PHRASE :'/A)
      CALL ERRSTP

1000  CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6000) YPCREC
6000  FORMAT(1X,'PHEXTR: ERROR IN PH DATA ENTRY',/A)
      CALL ERRSTP

100   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6010)
6010  FORMAT(1X,'PHEXTR: EOF WHEN SEARCHING FOR PH DATA')
      CALL ERRSTP

110   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6020)
6020  FORMAT(1X,'PHEXTR: ERROR WHEN SEARCHING FOR PH DATA')
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE PHPROC
**    Function - to read and process the PH diagnostic data
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /DIAGTP/PH,/ERRMSG/NCERR
**    Com changed - /PHDIAG/all elements,/ALOWPH/NPHFLD,/ERRMSG/NFATAL,
**         NWARN
**    Called by - DIAGRD
**    Calls - SEARCH,PHEXTR,ERRSTP
**    Files read - 10
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /ALOWPH/NPHFLD(JPPHMX)
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDPHLL,LDPHZM,LDPHMM,LDPHGM,LDPHTD,LDPHIN,
     -      LDPHTS
      COMMON /PHDIAG/NCPH,NPHPR(JPPHMX),NPHPL(JPPHMX),
     -      CINTPH(JPPHMX),LDPHLL,LDPHZM,LDPHMM,LDPHGM,LDPHTD,
     -      LDPHIN,VPHEW,VPHEWA(2),VPHEWS(2),VPHNS,VPHNSA(2),
     -      VPHNSS(2),VPHAA(4),VPHAS(4),LDPHTS,NPH
      CHARACTER*80 YOCREC
      LOGICAL LOFIND
**    Specify the defaults
      DATA NCPH/3/,NPHPR/JPPHMX*0/,NPHPL/JPPHMX*0/
      PARAMETER(RNEG=-0.1)
      DATA CINTPH/JPPHMX*RNEG/
      DATA LDPHLL/.TRUE./,LDPHZM/.TRUE./,LDPHMM/.FALSE./
      DATA LDPHGM/.TRUE./
      DATA LDPHTD/.TRUE./,LDPHIN/.FALSE./
      PARAMETER(R999=-999.0)
      DATA VPHEW/R999/,VPHEWA/2*R999/,VPHEWS/2*R999/
      DATA VPHNS/R999/,VPHNSA/2*R999/,VPHNSS/2*R999/
      DATA VPHAA/4*R999/,VPHAS/4*R999/
      DATA LDPHTS/.FALSE./
      DATA NPH/0/
**    Define allowed field codes for PH
      DATA NPHFLD/1,2,3,4,5,6,7,8,9,10,11,12/

      IF(.NOT.LDPH) THEN
**       PH diagnostics not selected - print warning
         NWARN=NWARN+1
         WRITE(NCERR,6000)
6000     FORMAT(1X,'WARNING: - PH DIAGNOSTICS NOT SELECTED:',
     -         ' ALTHOUGH USER HAS SUPPLIED PH INPUT DATA')
      ENDIF

**    Read and interpret records until '$ENDPH' found
300   READ(10,5000,END=100,ERR=110)YOCREC
5000  FORMAT(A80)
      CALL SEARCH(YOCREC,'$ENDPH',LOFIND)
      IF(LOFIND) THEN
         WRITE(NCERR,6010)
6010     FORMAT(1X,'USER-SUPPLIED PH SELECTION DATA PROCESSED')
         RETURN
      ENDIF

      CALL PHEXTR(YOCREC)
      GO TO 300

100   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6020)
6020  FORMAT(1X,'PHPROC: EOF WHEN SEARCHING FOR PH DATA')
      CALL ERRSTP

110   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6030)
6030  FORMAT(1X,'PHPROC: ERROR WHEN SEARCHING FOR PH DATA')
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE MCONCK
**    Function - to check model/job constants
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /MODELC/TSPD,/COMDAT/BEGDAY,ENDDAY,NFREQD,
**          /ERRMSG/NCERR
**    Com changed - /ERRMSG/NFATAL,NWARN
**    Called by - CONTROL
**    Calls - none
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      LOGICAL LWRITE,LASCII,LSCRAT,LUTF14,LROGR,LMASSW,LAVMSK,
     -      LAVSIG
      CHARACTER*2 YTYPSF
      CHARACTER*6 YDUM6
      CHARACTER*8 YNAME(2)
      COMMON /COMDAT/BEGDAY,ENDDAY,LWRITE,LASCII,LSCRAT,
     -      LUTF14,LROGR,NSIGFG,NFREQD,NFREQA,NFREQP,RNTAPE,
     -      NASCII,LMASSW,LAVMSK,LAVSIG,YNAME,YTYPSF,YDUM6
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LGCMTR,LHISPH,LHISXP,LDRY,LDIABH,LBL,LVD
      COMMON /MODELC/TSPD,NYEAR,NDAY,NMIN,LGCMTR,NL,TMEAN(JPNL),
     -      LHISPH,LHISXP,LDRY,LDIABH,BEGDYP,LBL,LVD,AKVV,CD

      IERR=0

**    Check for possible errors in TSPD
      IF(TSPD.LE.0.0) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6000)
6000     FORMAT(1X,'TSPD MUST BE GREATER THAN 0.0')
         IERR=999
      ENDIF

**    Check consistency of BEGDAY, ENDDAY, TSPD and NFREQD.
      IF(BEGDAY.GE.0.0) THEN
      ISTART=NINT(BEGDAY*TSPD)
      IEND=NINT(ENDDAY*TSPD)
      ISMPLS=(IEND-ISTART)/NFREQD
      IEND2=ISTART+NFREQD*ISMPLS
      IF(IEND.NE.IEND2) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6025)BEGDAY,ENDDAY,TSPD,NFREQD
6025     FORMAT(1X,'MCONCK ERROR: ONE OF BEGDAY,ENDDAY,TSPD,NFREQD ',
     -         'SEEMS TO BE IN ERROR '/F12.4,1X,F12.4,1X,F12.1,I12)
         IERR=999
      ENDIF
      ENDIF

**    Check validity of NYEAR, NDAY and NMIN
      IMM=NYEAR-(NYEAR/100)*100
      IF((IMM.LT.1).OR.(IMM.GT.12)) THEN
         NFATAL=NFATAL+1
         IERR=999
         WRITE(NCERR,6030) IMM
6030     FORMAT(1X,'***ERROR : ',
     -         '      MONTH OF YEAR IS ',I5)
      ENDIF
      IDD=NDAY/100
      IHH=NDAY-(NDAY/100)*100
      IF((IDD.LT.1).OR.(IDD.GT.31)) THEN
         NFATAL=NFATAL+1
         IERR=999
         WRITE(NCERR,6040) IDD
6040     FORMAT(1X,'***ERROR : ',
     -         '      DAY OF MONTH IS ',I5)
      ENDIF
      IF((IHH.LT.0).OR.(IHH.GT.24)) THEN
         NFATAL=NFATAL+1
         IERR=999
         WRITE(NCERR,6050) IHH
6050     FORMAT(1X,'***ERROR : ',
     -         '      HOUR OF DAY IS ',I5)
      ENDIF
      IF((NMIN.LT.0).OR.(IMM.GT.59)) THEN
         NFATAL=NFATAL+1
         IERR=999
         WRITE(NCERR,6060) NMIN
6060     FORMAT(1X,'***ERROR : ',
     -         '      MINUTE TIME IS ',I5)
      ENDIF
      IF((IMM.EQ.2.AND.IDD.GT.29).OR.(IMM.EQ.4.AND.IDD.GT.30)
     -      .OR.(IMM.EQ.6.AND.IDD.GT.30).OR.(IMM.EQ.9.AND.IDD.GT.30)
     -      .OR.(IMM.EQ.11.AND.IDD.GT.30)) THEN
         NFATAL=NFATAL+1
         IERR=999
         WRITE(NCERR,6070) IMM,IDD
6070     FORMAT(1X,'***ERROR*** : ',
     -         '      MONTH AND DATE INCOMPATIBLE - ',2I5)
      ENDIF

**    Check that NL has been correctly set
      IF(NL.LE.0.OR.NL.GT.JPNL) THEN
         NFATAL=NFATAL+1
         IERR=999
         WRITE(NCERR,6080)NL
6080     FORMAT(1X,'***ERROR : NL = ',I5)
      ENDIF
      IF(NL.NE.19.AND.NL.NE.47) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6090)NL
6090     FORMAT(1X,'***WARNING : IS NL SET CORRECTLY?  NL = ',I5)
      ENDIF

CC    IF(IERR.EQ.999) STOP
      RETURN

      END
      SUBROUTINE MCONRD
**    Function - to process model/job constant information
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /ERRMSG/NCERR
**    Com changed - /ERRMSG/NFATAL
**    Called by - CONTROL
**    Calls - SEARCH,MCNDEF,MCEXTR,ERRSTP
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      CHARACTER*80 YOCREC
      LOGICAL LOFIND

**    Read next record
      READ (10,5000,END=100,ERR=110)YOCREC
5000  FORMAT(A80)
      CALL SEARCH(YOCREC,'MODELCONSTANTS',LOFIND)
      IF(.NOT.LOFIND) GO TO 140

**    Phrase found; read following records
**    But first set up default data values
      CALL MCNDEF
**    Read and interpret records until 'ENDMODELCONSTANTS' found
300   CONTINUE
      READ(10,5000,END=120,ERR=130)YOCREC
      CALL SEARCH(YOCREC,'ENDMODELCONSTANTS',LOFIND)
      IF(LOFIND) THEN
**       'ENDMODELCONSTANTS' found
         RETURN
      ENDIF

      CALL MCEXTR(YOCREC)
      GO TO 300

100   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6000)
6000  FORMAT(1X,'MCONRD: EOF WHEN SEARCHING FOR **MODELCONSTANTS**')
      CALL ERRSTP

110   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6010)
6010  FORMAT(1X,'MCONRD: ERROR WHEN SEARCHING FOR **MODELCONSTANTS**')
      CALL ERRSTP

120   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6020)
6020  FORMAT(1X,'MCONRD: EOF WHEN SEARCHING FOR **MODELCONSTANTS**',
     -      ' DATA')
      CALL ERRSTP

130   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6030)
6030  FORMAT(1X,'MCONRD: ERROR WHEN SEARCHING FOR',
     -      ' **MODELCONSTANTS** DATA')
      CALL ERRSTP

140   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6040)
6040  FORMAT(1X,'MCONRD: UNEXPECTED ENTRY WHEN SEARCHING FOR',
     -      ' **MODELCONSTANTS**')
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE MCONWT
**    Function - to write model/job information to the
**          user-created data file
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /MODELC/all elements
**    Com changed - none
**    Called by - CONTROL
**    Calls - none
**    Files read - none
**    Files written - 11
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      LOGICAL LGCMTR,LHISPH,LHISXP,LDRY,LDIABH,LBL,LVD
      COMMON /MODELC/TSPD,NYEAR,NDAY,NMIN,LGCMTR,NL,TMEAN(JPNL),
     -      LHISPH,LHISXP,LDRY,LDIABH,BEGDYP,LBL,LVD,AKVV,CD
CC      NAMELIST /SMSPEC/TSPD,NYEAR,NDAY,NMIN,LGCMTR,TMEAN,LHISPH,
CC     -      LHISXP,LDRY,LDIABH,BEGDYP,LBL,LVD,CD,AKVV

      WRITE(11,6000)
6000  FORMAT(' &SMSPEC')

      WRITE(11,*)'  ','TSPD = ',TSPD,', ','NYEAR = ',NYEAR,', ',
     -      'NDAY = ',NDAY,', ','NMIN = ',NMIN,', ',
     -      'LGCMTR = ',LGCMTR,', ','LHISPH = ',LHISPH,', ',
     -      'LHISXP = ',LHISXP,', ','LDRY = ',LDRY,', '
      WRITE(11,*)
     -      'LDIABH = ',LDIABH,', ','BEGDYP = ',BEGDYP,', ',
     -      'LBL = ',LBL,', ','LVD = ',LVD,', ',
     -      'CD = ',CD,', ','AKVV = ',AKVV,', '
      WRITE(11,*)'  ','TMEAN = '
      DO 100 J=1,NL,6
      IBEG=J
      IEND=MIN0(NL,J+5)
      IEL=IEND-IBEG+1
      IF(IEL.EQ.1) THEN
         WRITE(11,*)'  ',TMEAN(J),', '
      ELSE IF(IEL.EQ.2) THEN
         WRITE(11,*)'  ',TMEAN(J),', ',TMEAN(J+1),', '
      ELSE IF(IEL.EQ.3) THEN
         WRITE(11,*)'  ',TMEAN(J),', ',TMEAN(J+1),', ',
     -         TMEAN(J+2),', '
      ELSE IF(IEL.EQ.4) THEN
         WRITE(11,*)'  ',TMEAN(J),', ',TMEAN(J+1),', ',
     -         TMEAN(J+2),', ',TMEAN(J+3),', '
      ELSE IF(IEL.EQ.5) THEN
         WRITE(11,*)'  ',TMEAN(J),', ',TMEAN(J+1),', ',
     -         TMEAN(J+2),', ',TMEAN(J+3),', ',TMEAN(J+4),', '
      ELSE IF(IEL.EQ.6) THEN
         WRITE(11,*)'  ',TMEAN(J),', ',TMEAN(J+1),', ',
     -         TMEAN(J+2),', ',TMEAN(J+3),', ',TMEAN(J+4),', ',
     -         TMEAN(J+5),', '
      ENDIF
100   CONTINUE

      WRITE(11,6999)
6999  FORMAT(' &END')


      RETURN
      END
      SUBROUTINE MCNDEF
**    Function - to specify default global data and update
**          appropriate common blocks
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - none
**    Com changed - /MODELC/all elements
**    Called by - MCONRD
**    Calls - none
**    Files read - none
**    Files written - none
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      LOGICAL LGCMTR,LHISPH,LHISXP,LDRY,LDIABH,LBL,LVD
      COMMON /MODELC/TSPD,NYEAR,NDAY,NMIN,LGCMTR,NL,TMEAN(JPNL),
     -      LHISPH,LHISXP,LDRY,LDIABH,BEGDYP,LBL,LVD,AKVV,CD

**    Assume that the GCM simulation didn't contain the tracer update
      DATA LGCMTR/.FALSE./

**    Assume that GCM run did not output PH and XP data to
**    history file
      DATA LHISPH/.FALSE./,LHISXP/.FALSE./,LDRY/.FALSE./,
     -      LDIABH/.TRUE./

**    Switch off parameterised physical processes by default
      DATA LBL/.FALSE./,LVD/.FALSE./

**    Default values for drag coeeficient and vertical diffusion
**    coefficient
      DATA CD/0.001/,AKVV/1.0/

**    Assume 15th January 1987 at 1200GMT is the default start time
      DATA NYEAR/8701/,NDAY/1512/,NMIN/0/

**    Preset TSPD: this must be altered by the user
      TSPD=0.0

**    Preset NL; this must be reset by the user
      NL=-1

**    Preset BEGDYP to zero
      BEGDYP=0.0

**    Preset values of TMEAN
      DO 100 JL=1,JPNL
      TMEAN(JL)=0.0
100   CONTINUE

      RETURN
      END
      SUBROUTINE MCEXTR(YPCREC)
**    Function - to extract information from model/job and update
**          appropriate common blocks
**    Args in -
**               YPCREC  - character*80 variable containing data
**    Args out - none
**    Args for work - none
**    Com used - /ERRMSG/NCERR
**    Com changed - /MODELC/all elements
**    Called by - MCONRD
**    Calls - SEARCH,LASTCH,RTRANS,ERRSTP,RMULTP
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LGCMTR,LHISPH,LHISXP,LDRY,LDIABH,LBL,LVD
      COMMON /MODELC/TSPD,NYEAR,NDAY,NMIN,LGCMTR,NL,TMEAN(JPNL),
     -      LHISPH,LHISXP,LDRY,LDIABH,BEGDYP,LBL,LVD,AKVV,CD
      LOGICAL LOFIND
      CHARACTER*1 YOEQL,YOCOM
      CHARACTER*6 YOKW6
      CHARACTER*7 YOKW7
      CHARACTER*8 YOKW8
      CHARACTER*9 YOKW9
      CHARACTER*10 YOKW10
      CHARACTER*11 YOKW11
      CHARACTER*12 YOKW12
      CHARACTER*13 YOKW13
      CHARACTER*15 YOKW15
      CHARACTER*17 YOKW17
      CHARACTER*80 YPCREC

      YOEQL='='
      YOCOM=','
**    Search for the keyword in CREC and then determine the field value
      LOFIND=.FALSE.

      YOKW9='STARTYEAR'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
         IPOSN=INDEX(YPCREC,YOEQL)+1
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
         IF(IERR.NE.0) GO TO 1000
         CALL ITRANS(YPCREC,IPOSN,IPOSX,IVALUE,12)
**       Modify NYEAR
         IMONTH=NYEAR-(NYEAR/100)*100
         NYEAR=IVALUE*100+IMONTH
         RETURN
      ENDIF

**    Else
      YOKW13='REFERENCETEMP'
      CALL SEARCH(YPCREC,YOKW13,LOFIND)
      IF(LOFIND) THEN
         CALL RMULTP(YPCREC,YOEQL,YOCOM,TMEAN,NL)
         RETURN
      ENDIF

**    Else
      YOKW10='STARTMONTH'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
      IF(LOFIND) THEN
         IPOSN=INDEX(YPCREC,YOEQL)+1
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
         IF(IERR.NE.0) GO TO 1000
         CALL ITRANS(YPCREC,IPOSN,IPOSX,IVALUE,12)
**       Modify NYEAR
         IYEAR=NYEAR/100
         NYEAR=IYEAR*100+IVALUE
         RETURN
      ENDIF

**    Else
      YOKW8='STARTDAY'
      CALL SEARCH(YPCREC,YOKW8,LOFIND)
      IF(LOFIND) THEN
         IPOSN=INDEX(YPCREC,YOEQL)+1
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
         IF(IERR.NE.0) GO TO 1000
         CALL ITRANS(YPCREC,IPOSN,IPOSX,IVALUE,12)
**       Modify NDAY
         IMIN=NDAY-(NDAY/100)*100
         NDAY=IVALUE*100+IMIN
         RETURN
      ENDIF

**    Else
      YOKW9='STARTHOUR'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
         IPOSN=INDEX(YPCREC,YOEQL)+1
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
         IF(IERR.NE.0) GO TO 1000
         CALL ITRANS(YPCREC,IPOSN,IPOSX,IVALUE,12)
**       Modify NDAY
         IDAY=NDAY/100
         NDAY=IDAY*100+IVALUE
         RETURN
      ENDIF

**    Else
      YOKW8='STARTMIN'
      CALL SEARCH(YPCREC,YOKW8,LOFIND)
      IF(LOFIND) THEN
         IPOSN=INDEX(YPCREC,YOEQL)+1
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
         IF(IERR.NE.0) GO TO 1000
         CALL ITRANS(YPCREC,IPOSN,IPOSX,NMIN,12)
         RETURN
      ENDIF

**    Else
      YOKW9='STEPS/DAY'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
         IF(IERR.NE.0) GO TO 1000
         CALL RTRANS(YPCREC,IPOSN,IPOSX,TSPD,12)
         RETURN
      ENDIF

      YOKW13='DAYPARAMSTART'
      CALL SEARCH(YPCREC,YOKW13,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
         IF(IERR.NE.0) GO TO 1000
         CALL RTRANS(YPCREC,IPOSN,IPOSX,BEGDYP,12)
         RETURN
      ENDIF

**    Else
      YOKW15='BDYLAYERPHYSICS'
      CALL SEARCH(YPCREC,YOKW15,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LBL)
         RETURN
      ENDIF

**    Else
      YOKW17='VERTICALDIFFUSION'
      CALL SEARCH(YPCREC,YOKW17,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LVD)
         RETURN
      ENDIF

**    Else
      YOKW9='DRAGCOEFF'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
         IF(IERR.NE.0) GO TO 1000
         CALL RTRANS(YPCREC,IPOSN,IPOSX,CD,12)
         RETURN
      ENDIF

**    Else
      YOKW13='VERTDIFFCOEFF'
      CALL SEARCH(YPCREC,YOKW13,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
         IF(IERR.NE.0) GO TO 1000
         CALL RTRANS(YPCREC,IPOSN,IPOSX,AKVV,12)
         RETURN
      ENDIF

**    Else
      YOKW10='TRACERCODE'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LGCMTR)
         RETURN
      ENDIF

**    Else
      YOKW9='PHHISTORY'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LHISPH)
         RETURN
      ENDIF

**    Else
      YOKW9='XPHISTORY'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LHISXP)
         RETURN
      ENDIF

**    Else
      YOKW6='DRYRUN'
      CALL SEARCH(YPCREC,YOKW6,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDRY)
         RETURN
      ENDIF

**    Else
      YOKW12='DIABATICHEAT'
      CALL SEARCH(YPCREC,YOKW12,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDIABH)
         RETURN
      ENDIF

**    Else
      YOKW11='MODELLEVELS'
      CALL SEARCH(YPCREC,YOKW11,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
**       Note that IPOSN and IPOSX can take the same value
         IF(IERR.EQ.998) GO TO 1000
         CALL ITRANS(YPCREC,IPOSN,IPOSX,NL,12)
         RETURN
      ENDIF

**    Else
**    Keyword not located
      NFATAL=NFATAL+1
      WRITE(NCERR,6020) YPCREC
6020  FORMAT(1X,'KEYWORD NOT LOCATED FOR PHRASE :'/A)
      CALL ERRSTP

1000  CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6000) YPCREC
6000  FORMAT(1X,'MCEXTR: ERROR IN MODEL/JOB DATA ENTRY',/A)
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE CHKXP
**    Function - to check XP data input
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /COMDAT/LWRITE,LASCII,/ALOWXP/NXPFLD,/ERRMSG/NCERR,
**          /MODELC/LHISXP
**    Com changed - /XPDIAG/NXPPR,NXPPL,NXP,/DIAGTP/LDXP,
**          /ERRMSG/NWARN,NFATAL
**    Called by - DIAGCK
**    Calls - none
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      PARAMETER(JPNL=100)
      COMMON /ALOWXP/NXPFLD(JPXPMX)
      LOGICAL LWRITE,LASCII,LSCRAT,LUTF14,LROGR,LMASSW,LAVMSK,
     -      LAVSIG
      CHARACTER*2 YTYPSF
      CHARACTER*6 YDUM6
      CHARACTER*8 YNAME(2)
      COMMON /COMDAT/BEGDAY,ENDDAY,LWRITE,LASCII,LSCRAT,
     -      LUTF14,LROGR,NSIGFG,NFREQD,NFREQA,NFREQP,RNTAPE,
     -      NASCII,LMASSW,LAVMSK,LAVSIG,YNAME,YTYPSF,YDUM6
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LGCMTR,LHISPH,LHISXP,LDRY,LDIABH,LBL,LVD
      COMMON /MODELC/TSPD,NYEAR,NDAY,NMIN,LGCMTR,NL,TMEAN(JPNL),
     -      LHISPH,LHISXP,LDRY,LDIABH,BEGDYP,LBL,LVD,AKVV,CD
      LOGICAL LDXPZM,LDXPLM,LDXPTD,LDXPIN,LDXPTS
      COMMON /XPDIAG/NCXP,NXPPR(JPXPMX),NXPPL(JPXPMX),
     -      CINTXP(JPXPMX),LDXPZM,LDXPLM,LDXPTD,LDXPIN,LDXPTS,NXP
      LOGICAL LOERR,LOVALU

      LOERR=.FALSE.

      IF(LDXP.AND..NOT.LHISXP) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6500)
6500     FORMAT(1X,'CHKXP: XP HISTORY DATA DOES NOT EXIST; XP ',
     -         'DIAGNOSTICS TURNED OFF')
         LDXP=.FALSE.
         RETURN
      ENDIF

**    Check that printing has been requested - if not, ensure
**          NXPPR=0 for all elements
      IF(.NOT.LWRITE) THEN
         IF(NXPPR(1).NE.0) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6000)
6000        FORMAT(1X,'CHKXP: NXPPR VALUES CHANGED - NO PRINTING')
            DO 1000 K=1,JPXPMX
1000        NXPPR(K)=0
**          Reset value of NXP
            NXP=0
            DO 1001 J=1,JPXPMX
            IF(NXPPL(J).GT.0) NXP=NXP+1
1001        CONTINUE
         ENDIF
      ENDIF

**    Check that UTFs are requested - if not, ensure NXPPL=0
**          for all elements
      IF(.NOT.LASCII) THEN
         IF(NXPPL(1).NE.0) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6010)
6010        FORMAT(1X,'CHKXP: NXPPL VALUES CHANGED - NO PLOTTING')
            DO 1100 K=1,JPXPMX
1100        NXPPL(K)=0
**          Reset value of NXP
            NXP=0
            DO 1101 J=1,JPXPMX
            IF(NXPPR(J).GT.0) NXP=NXP+1
1101        CONTINUE
         ENDIF
      ENDIF

**    Switch of diagnostics if NXP is zero
      IF(NXP.EQ.0.AND.LDXP) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6035)
6035     FORMAT(1X,'CHKXP: XP DIAGNOSTICS SWITCHED OFF, NXP=0')
         LDXP=.FALSE.
         RETURN
      ENDIF
      IF(.NOT.LDXP) RETURN

**    Check valid code numbers in NXPPR
      IF(NXPPR(1).NE.0) THEN
         DO 2000 K=1,JPXPMX
         LOVALU=.FALSE.
         DO 2100 J=1,JPXPMX
         IF((NXPPR(K).EQ.NXPFLD(J)).OR.(NXPPR(K).EQ.0)) LOVALU=.TRUE.
2100     CONTINUE
         IF(.NOT.LOVALU) THEN
            LOERR=.TRUE.
            NFATAL=NFATAL+1
            WRITE(NCERR,6020) NXPPR(K)
6020        FORMAT(1X,'CHKXP: ERROR IN NXPPR VALUE ',I4)
         ENDIF
2000     CONTINUE
      ENDIF

**    Check valid code numbers in NXPPL
      IF(NXPPL(1).NE.0) THEN
         DO 3000 K=1,JPXPMX
         LOVALU=.FALSE.
         DO 3100 J=1,JPXPMX
         IF((NXPPL(K).EQ.NXPFLD(J)).OR.(NXPPL(K).EQ.0)) LOVALU=.TRUE.
3100     CONTINUE
         IF(.NOT.LOVALU) THEN
            LOERR=.TRUE.
            NFATAL=NFATAL+1
            WRITE(NCERR,6030) NXPPL(K)
6030        FORMAT(1X,'CHKXP: ERROR IN NXPPL VALUE ',I4)
         ENDIF
3000     CONTINUE
      ENDIF

**    Check that NXP is non-zero, and less than JPXPMX
      IF(NXP.LE.0.OR.(NXP.GT.JPXPMX)) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6040)
6040     FORMAT(1X,'CHKXP : INVALID VALUE FOR NXP')
         LOERR=.TRUE.
      ENDIF

CC    IF(LOERR) STOP
      RETURN
      END
      SUBROUTINE XPEXTR(YPCREC)
**    Function - to extract information from XP data and update
**          appropriate common blocks
**    Args in -
**               YPCREC  - character*80 variable containing data
**    Args out - none
**    Args for work - none
**    Com used - /ALOWXP/NXPFLD,/ERRMSG/NCERR
**    Com changed - /XPDIAG/all elements,/ERRMSG/NFATAL
**    Called by - XPPROC
**    Calls - INTCPY,SEARCH,LASTCH,ITRANS,LPROC,IMULTP,RMULTP,ERRSTP
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /ALOWXP/NXPFLD(JPXPMX)
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDXPZM,LDXPLM,LDXPTD,LDXPIN,LDXPTS
      COMMON /XPDIAG/NCXP,NXPPR(JPXPMX),NXPPL(JPXPMX),
     -      CINTXP(JPXPMX),LDXPZM,LDXPLM,LDXPTD,LDXPIN,LDXPTS,NXP
      LOGICAL LOFIND,LOALL
      CHARACTER*1 YOCOM,YOEQL
      CHARACTER*2 YOKW2
      CHARACTER*4 YOKW4
      CHARACTER*7 YOKW7
      CHARACTER*9 YOKW9
      CHARACTER*10 YOKW10
      CHARACTER*13 YOKW13
      CHARACTER*14 YOKW14
      CHARACTER*80 YPCREC

      YOEQL='='
      YOCOM=','
**    Search for the keyword in YPCREC and then determine the field
**          value
      LOFIND=.FALSE.

      YOKW10='O/PCHANNEL'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
**       Note that IPOSN and IPOSX can take the same value
         IF(IERR.EQ.998) GO TO 1000
         CALL ITRANS(YPCREC,IPOSN,IPOSX,NCXP,12)
         RETURN
      ENDIF

      YOKW14='NUMBEROFFIELDS'
      CALL SEARCH(YPCREC,YOKW14,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
**       Note that IPOSN and IPOSX can take the same value
         IF(IERR.EQ.998) GO TO 1000
         CALL ITRANS(YPCREC,IPOSN,IPOSX,NXP,12)
         RETURN
      ENDIF

**    Else
      YOKW9='ZONALMEAN'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDXPZM)
         RETURN
      ENDIF

**    Else
      YOKW9='LEVELMEAN'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDXPLM)
         RETURN
      ENDIF

**    Else
      YOKW9='TAVERAGED'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDXPTD)
         RETURN
      ENDIF

**    Else
      YOKW13='INSTANTANEOUS'
      CALL SEARCH(YPCREC,YOKW13,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDXPIN)
         RETURN
      ENDIF

**    Else
      YOKW10='PRINTFIELD'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            CALL INTCPY(NXPFLD,JPXPMX,NXPPR)
            RETURN
         ENDIF

         CALL IMULTP(YPCREC,YOEQL,YOCOM,NXPPR,JPXPMX)
         RETURN
      ENDIF

**    Else
      YOKW9='PLOTFIELD'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            CALL INTCPY(NXPFLD,JPXPMX,NXPPL)
            RETURN
         ENDIF

         CALL IMULTP(YPCREC,YOEQL,YOCOM,NXPPL,JPXPMX)
         RETURN
      ENDIF

**    Else
      YOKW2='CI'
      CALL SEARCH(YPCREC,YOKW2,LOFIND)
      IF(LOFIND) THEN
         CALL RMULTP(YPCREC,YOEQL,YOCOM,CINTXP,JPXPMX)
         RETURN
      ENDIF

**    Else
      YOKW7='TSERIES'
      CALL SEARCH(YPCREC,YOKW7,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDXPTS)
         RETURN
      ENDIF

**    Else
**    Keyword not located
      NFATAL=NFATAL+1
      WRITE(NCERR,6030) YPCREC
6030  FORMAT(1X,'KEYWORD NOT LOCATED FOR PHRASE :'/A)
      CALL ERRSTP

1000  CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6000) YPCREC
6000  FORMAT(1X,'XPEXTR: ERROR IN XP DATA ENTRY',/A)
      CALL ERRSTP

100   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6030)
6010  FORMAT(1X,'XPEXTR: EOF WHEN SEARCHING FOR XP DATA')
      CALL ERRSTP

110   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6020)
6020  FORMAT(1X,'XPEXTR: ERROR WHEN SEARCHING FOR XP DATA')
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE XPPROC
**    Function - to read and process the XP diagnostic data
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /DIAGTP/XP,/ERRMSG/NCERR
**    Com changed - /XPDIAG/all elements,/ALOWXP/NXPFLD,/ERRMSG/
**          NFATAL,NWARN
**    Called by - DIAGRD
**    Calls - SEARCH,XPEXTR,ERRSTP
**    Files read - 10
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /ALOWXP/NXPFLD(JPXPMX)
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDXPZM,LDXPLM,LDXPTD,LDXPIN,LDXPTS
      COMMON /XPDIAG/NCXP,NXPPR(JPXPMX),NXPPL(JPXPMX),
     -      CINTXP(JPXPMX),LDXPZM,LDXPLM,LDXPTD,LDXPIN,LDXPTS,NXP
      CHARACTER*80 YOCREC
      LOGICAL LOFIND
**    Specify the defaults
      DATA NCXP/3/,NXPPR/JPXPMX*0/,NXPPL/JPXPMX*0/
      PARAMETER(RNEG=-0.1)
      DATA CINTXP/JPXPMX*RNEG/
      DATA LDXPZM/.TRUE./
      DATA LDXPLM/.FALSE./
      DATA LDXPTD/.TRUE./,LDXPIN/.FALSE./
      DATA LDXPTS/.FALSE./
      DATA NXP/0/
**    Define allowed field codes for XP

      DO 100 J=1,JPXPMX
      NXPFLD(J)=J
100   CONTINUE

      IF(.NOT.LDXP) THEN
**       XP diagnostics not selected - print warning
         NWARN=NWARN+1
         WRITE(NCERR,6000)
6000     FORMAT(1X,'WARNING: - XP DIAGNOSTICS NOT SELECTED:',
     -         ' ALTHOUGH USER HAS SUPPLIED XP INPUT DATA')
      ENDIF

**    Read and interpret records until '$ENDXP' found
300   READ(10,5000,END=105,ERR=110)YOCREC
5000  FORMAT(A80)
      CALL SEARCH(YOCREC,'$ENDXP',LOFIND)
      IF(LOFIND) THEN
         WRITE(NCERR,6010)
6010     FORMAT(1X,'USER-SUPPLIED XP SELECTION DATA PROCESSED')
         RETURN
      ENDIF

      CALL XPEXTR(YOCREC)
      GO TO 300

105   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6020)
6020  FORMAT(1X,'XPPROC: EOF WHEN SEARCHING FOR XP DATA')
      CALL ERRSTP

110   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6030)
6030  FORMAT(1X,'XPPROC: ERROR WHEN SEARCHING FOR XP DATA')
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE CHKLV
**    Function - to check output-level data input by user
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /COMDAT/YTYPSF,/LVDIAG/OUTLEV,/ERRMSG/NCERR,
**          /MODELC/NL
**    Com changed - /ERRMSG/NFATAL,NWARN
**    Params used - /PARAM1/JPNL
**    Called by - DIAGCK
**    Calls - none
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      LOGICAL LWRITE,LASCII,LSCRAT,LUTF14,LROGR,LMASSW,LAVMSK,
     -      LAVSIG
      CHARACTER*2 YTYPSF
      CHARACTER*6 YDUM6
      CHARACTER*8 YNAME(2)
      COMMON /COMDAT/BEGDAY,ENDDAY,LWRITE,LASCII,LSCRAT,
     -      LUTF14,LROGR,NSIGFG,NFREQD,NFREQA,NFREQP,RNTAPE,
     -      NASCII,LMASSW,LAVMSK,LAVSIG,YNAME,YTYPSF,YDUM6
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      COMMON /LVDIAG/OUTLEV(JPNL)
      LOGICAL LGCMTR,LHISPH,LHISXP,LDRY,LDIABH,LBL,LVD
      COMMON /MODELC/TSPD,NYEAR,NDAY,NMIN,LGCMTR,NL,TMEAN(JPNL),
     -      LHISPH,LHISXP,LDRY,LDIABH,BEGDYP,LBL,LVD,AKVV,CD
      LOGICAL LOERR

      LOERR=.FALSE.

**    Check values of OUTLEV; values of 0.0 are allowed, so check for
**    values less than -1.0E-10
      ZTOL=-1.0E-10
      IF(YTYPSF.EQ.'IS') THEN
**       Pressure level output - values should be in mb
         DO 1000 J=1,NL
         IF(OUTLEV(J).LT.ZTOL) THEN
            NFATAL=NFATAL+1
            WRITE(NCERR,6000) OUTLEV(J)
6000        FORMAT(1X,'CHKLV: INVALID OUTPUT PRESSURE LEVEL : ',F12.3)
            LOERR=.TRUE.
         ELSE IF(OUTLEV(J).GT.1200.0) THEN
            NFATAL=NFATAL+1
            WRITE(NCERR,6000) OUTLEV(J)
            LOERR=.TRUE.
         ENDIF
1000     CONTINUE
      ELSE IF(YTYPSF.EQ.'TH') THEN
**       Isentropic level output - values should be in deg Kelvin
         DO 1100 J=1,NL
         IF(OUTLEV(J).LT.ZTOL) THEN
            NFATAL=NFATAL+1
            WRITE(NCERR,6010) OUTLEV(J)
6010        FORMAT(1X,'CHKLV: INVALID OUTPUT THETA LEVEL : ',F12.3,
     -            ' KELVIN')
            LOERR=.TRUE.
         ELSE IF(OUTLEV(J).LT.250.0.AND.OUTLEV(J).GT.-ZTOL) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6020) OUTLEV(J)
6020        FORMAT(1X,'CHKLV: WARNING, OUTPUT THETA LEVEL = ',F12.3,
     -            ' KELVIN')
         ELSE IF(OUTLEV(J).GT.5000.0) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6020) OUTLEV(J)
         ENDIF
1100     CONTINUE
      ENDIF
**    OUTLEV field not used when output is on model levels

CC    IF(LOERR) STOP
      RETURN
      END
      SUBROUTINE LVEXTR(YPCREC)
**    Function - to extract information from model output levels data
**          and update appropriate common blocks
**    Args in -
**               YPCREC  - character*80 variable containing data
**    Args out - none
**    Args for work - none
**    Com used - /ERRMSG/NCERR,/MODELC/NL
**    Com changed - /LVDIAG/all elements,/ERRMSG/NFATAL
**    Params used - /PARAM1/JPNL
**    Called by - LVPROC
**    Calls - RMULTP,ERRSTP
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      COMMON /LVDIAG/OUTLEV(JPNL)
      LOGICAL LGCMTR,LHISPH,LHISXP,LDRY,LDIABH,LBL,LVD
      COMMON /MODELC/TSPD,NYEAR,NDAY,NMIN,LGCMTR,NL,TMEAN(JPNL),
     -      LHISPH,LHISXP,LDRY,LDIABH,BEGDYP,LBL,LVD,AKVV,CD
      LOGICAL LOFIND
      CHARACTER*1 YOCOM,YOEQL
      CHARACTER*9 YOKW9
      CHARACTER*80 YPCREC

      YOEQL='='
      YOCOM=','
**    Search for the keyword in YPCREC and then determine the field
**          value
      LOFIND=.FALSE.

      YOKW9='O/PLEVELS'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
         CALL RMULTP(YPCREC,YOEQL,YOCOM,OUTLEV,NL)
         RETURN
      ENDIF

**    Else
**    Keyword not located
      NFATAL=NFATAL+1
      WRITE(NCERR,6030) YPCREC
6030  FORMAT(1X,'KEYWORD NOT LOCATED FOR PHRASE :'/A)
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE LVPROC
**    Function - to read and process the model output level data
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /COMDAT/YTYPSF,/ERRMSG/NCERR
**    Com changed - /LVDIAG/OUTLEV,/ERRMSG/NFATAL,NWARN
**    Params used - /PARAM1/JPNL
**    Called by - DIAGRD
**    Calls - SEARCH,LVEXTR,ERRSTP
**    Files read - 10
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      LOGICAL LWRITE,LASCII,LSCRAT,LUTF14,LROGR,LMASSW,LAVMSK,
     -      LAVSIG
      CHARACTER*2 YTYPSF
      CHARACTER*6 YDUM6
      CHARACTER*8 YNAME(2)
      COMMON /COMDAT/BEGDAY,ENDDAY,LWRITE,LASCII,LSCRAT,
     -      LUTF14,LROGR,NSIGFG,NFREQD,NFREQA,NFREQP,RNTAPE,
     -      NASCII,LMASSW,LAVMSK,LAVSIG,YNAME,YTYPSF,YDUM6
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      COMMON /LVDIAG/OUTLEV(JPNL)
      CHARACTER*80 YOCREC
      LOGICAL LOFIND
**    Specify the defaults
      DATA OUTLEV/JPNL*0.0/

      IF(YTYPSF.EQ.'ET') THEN
**       Model level output selected - print warning
         NWARN=NWARN+1
         WRITE(NCERR,6000)
6000     FORMAT(1X,'WARNING: - OUTPUT WILL BE ON MODEL LEVELS,'
     -         ' ALTHOUGH USER HAS SUPPLIED OUTPUT LEVEL DATA')
      ENDIF

**    Read and interpret records until '$ENDLV' found
300   READ(10,5000,END=100,ERR=110)YOCREC
5000  FORMAT(A80)
      CALL SEARCH(YOCREC,'$ENDLV',LOFIND)
      IF(LOFIND) THEN
         WRITE(NCERR,6010)
6010     FORMAT(1X,'USER-SUPPLIED LV SELECTION DATA PROCESSED')
         RETURN
      ENDIF

      CALL LVEXTR(YOCREC)
      GO TO 300

100   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6020)
6020  FORMAT(1X,'LVPROC: EOF WHEN SEARCHING FOR LV DATA')
      CALL ERRSTP

110   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6030)
6030  FORMAT(1X,'LVPROC: ERROR WHEN SEARCHING FOR LV DATA')
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE CHKOR
**    Function - to check OR data input
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /COMDAT/LWRITE,LASCII,YTYPSF,LROGR,/ERRMSG/NCERR
**    Com changed - /ORDIAG/LROGPR,LROGPL,LMSKH,LMSKNS,LMSKWE,
**          /ERRMSG/NWARN
**    Called by - DIAGCK
**    Calls - none
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LWRITE,LASCII,LSCRAT,LUTF14,LROGR,LMASSW,LAVMSK,
     -      LAVSIG
      CHARACTER*2 YTYPSF
      CHARACTER*6 YDUM6
      CHARACTER*8 YNAME(2)
      COMMON /COMDAT/BEGDAY,ENDDAY,LWRITE,LASCII,LSCRAT,
     -      LUTF14,LROGR,NSIGFG,NFREQD,NFREQA,NFREQP,RNTAPE,
     -      NASCII,LMASSW,LAVMSK,LAVSIG,YNAME,YTYPSF,YDUM6
      LOGICAL LROGPR,LROGPL,LMSKH,LMSKNS,LMSKWE
      COMMON /ORDIAG/LROGPR,LROGPL,CINTOR,LMSKH,LMSKNS,LMSKWE

**    Switch off the printing of orography if no printing has
**          been requested, or if orography not requested
      IF((.NOT.LROGR).AND.LROGPR) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6000)
6000     FORMAT(1X,'CHKOR: PRINTING OF OROGRAPHY SWITCHED OFF ',
     -         'SINCE OROGRAPHY NOT REQUESTED')
         LROGPR=.FALSE.
      ENDIF
      IF(LROGPR.AND.(.NOT.LWRITE)) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6010)
6010     FORMAT(1X,'CHKOR: PRINTING OF OROGRAPHY SWITCHED OFF ',
     -         'SINCE PRINTOUT NOT REQUESTED')
         LROGPR=.FALSE.
      ENDIF

**    Switch off the plotting of orography if no plotting has
**          been requested, or if orography not requested
      IF((.NOT.LROGR).AND.LROGPL) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6100)
6100     FORMAT(1X,'CHKOR: PLOTTING OF OROGRAPHY SWITCHED OFF ',
     -         'SINCE OROGRAPHY NOT REQUESTED')
         LROGPL=.FALSE.
      ENDIF
      IF(LROGPL.AND.(.NOT.LASCII)) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6110)
6110     FORMAT(1X,'CHKOR: PLOTTING OF OROGRAPHY SWITCHED OFF ',
     -         'SINCE UTFS NOT REQUESTED')
         LROGPL=.FALSE.
      ENDIF

**    Orography masks not allowed currently when output is on
**          isentropic or sigma surfaces.
      IF(YTYPSF.NE.'IS') THEN
         IF(LMSKH) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6200)
6200        FORMAT(1X,'CHKOR: HORIZONTAL OROGRAPHY MASK SWITCHED ',
     -         'OFF SINCE OUTPUT NOT ON ISOBARIC LEVELS')
            LMSKH=.FALSE.
         ENDIF
         IF(LMSKNS) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6210)
6210        FORMAT(1X,'CHKOR: NORTH-SOUTH OROGRAPHY MASK SWITCHED ',
     -         'OFF SINCE OUTPUT NOT ON ISOBARIC LEVELS')
            LMSKNS=.FALSE.
         ENDIF
         IF(LMSKWE) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6220)
6220        FORMAT(1X,'CHKOR: WEST-EAST OROGRAPHY MASK SWITCHED ',
     -         'OFF SINCE OUTPUT NOT ON ISOBARIC LEVELS')
            LMSKWE=.FALSE.
         ENDIF
      ENDIF

**    Orographic mask requires NFREQD>0 and LASCII=.TRUE.
      IF(NFREQD.LE.0.OR.(.NOT.LASCII)) THEN
         IF(LMSKH.OR.LMSKNS.OR.LMSKWE) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6300)
6300        FORMAT(1X,'CHKOR: OROGRAPHIC MASKS SWITCHED OFF SINCE'
     -            ' NO UTFS TO BE PRODUCED')
            LMSKH=.FALSE.
            LMSKNS=.FALSE.
            LMSKWE=.FALSE.
         ENDIF
      ENDIF

      RETURN
      END
      SUBROUTINE OREXTR(YPCREC)
**    Function - to extract information from OR data and update
**          appropriate common blocks
**    Args in -
**               YPCREC  - character*80 variable containing data
**    Args out - none
**    Args for work - none
**    Com used - /ERRMSG/NCERR
**    Com changed - /ORDIAG/all elements,/ERRMSG/NFATAL
**    Called by - ORPROC
**    Calls - SEARCH,LPROC,LASTCH,RTRANS,ERRSTP
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LROGPR,LROGPL,LMSKH,LMSKNS,LMSKWE
      COMMON /ORDIAG/LROGPR,LROGPL,CINTOR,LMSKH,LMSKNS,LMSKWE
      LOGICAL LOFIND
      CHARACTER*1 YOCOM,YOEQL
      CHARACTER*2 YOKW2
      CHARACTER*6 YOKW6
      CHARACTER*8 YOKW8
      CHARACTER*9 YOKW9
      CHARACTER*80 YPCREC

      YOEQL='='
      YOCOM=','
**    Search for the keyword in YPCREC and then determine the field
**          value
      LOFIND=.FALSE.

      YOKW9='PRINTOROG'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LROGPR)
         RETURN
      ENDIF

**    Else
      YOKW8='PLOTOROG'
      CALL SEARCH(YPCREC,YOKW8,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LROGPL)
         RETURN
      ENDIF

**    Else
      YOKW9='HORIZMASK'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LMSKH)
         RETURN
      ENDIF

**    Else
      YOKW6='NSMASK'
      CALL SEARCH(YPCREC,YOKW6,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LMSKNS)
         RETURN
      ENDIF

**    Else
      YOKW6='WEMASK'
      CALL SEARCH(YPCREC,YOKW6,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LMSKWE)
         RETURN
      ENDIF

**    Else
      YOKW2='CI'
      CALL SEARCH(YPCREC,YOKW2,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
         IF(IERR.EQ.998) GO TO 1000
         CALL RTRANS(YPCREC,IPOSN,IPOSX,CINTOR,12)
         RETURN
      ENDIF

**    Else
**    Keyword not located
      NFATAL=NFATAL+1
      WRITE(NCERR,6030) YPCREC
6030  FORMAT(1X,'KEYWORD NOT LOCATED FOR PHRASE :'/A)
      CALL ERRSTP

1000  CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6000) YPCREC
6000  FORMAT(1X,'OREXTR: ERROR IN OR DATA ENTRY',/A)
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE ORPROC
**    Function - to read and process the OR diagnostic data
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /ERRMSG/NCERR
**    Com changed - /ORDIAG/all elements,/ERRMSG/NFATAL
**    Called by - DIAGRD
**    Calls - SEARCH,OREXTR,ERRSTP
**    Files read - 10
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LROGPR,LROGPL,LMSKH,LMSKNS,LMSKWE
      COMMON /ORDIAG/LROGPR,LROGPL,CINTOR,LMSKH,LMSKNS,LMSKWE
      CHARACTER*80 YOCREC
      LOGICAL LOFIND
**    Specify the defaults
      DATA CINTOR/200.0/
      DATA LROGPR/.TRUE./,LROGPL/.TRUE./LMSKH/.FALSE./,
     -      LMSKNS/.FALSE./,LMSKWE/.FALSE./

**    Read and interpret records until '$ENDOR' found
300   READ(10,5000,END=100,ERR=110)YOCREC
5000  FORMAT(A80)
      CALL SEARCH(YOCREC,'$ENDOR',LOFIND)
      IF(LOFIND) THEN
         WRITE(NCERR,6010)
6010     FORMAT(1X,'USER-SUPPLIED OR SELECTION DATA PROCESSED')
         RETURN
      ENDIF

      CALL OREXTR(YOCREC)
      GO TO 300

100   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6020)
6020  FORMAT(1X,'ORPROC: EOF WHEN SEARCHING FOR OR DATA')
      CALL ERRSTP

110   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6030)
6030  FORMAT(1X,'ORPROC: ERROR WHEN SEARCHING FOR OR DATA')
      CALL ERRSTP

      END
      SUBROUTINE CHKCO
**    Function - to check CO data input
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /COMDAT/LWRITE,/ALOWCO/NCOFLD,/CODIAG/NWAVET,NCOPR,
**          NVCOHR,/ERRMSG/NCERR,/MODELC/NL
**    Com changed - /DIAGTP/LDCO,/ERRMSG/NFATAL,NWARN
**    Params used - /PARAM1/JPNL,/PARAMS/JPCOMX
**    Called by - DIAGCK
**    Calls - none
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /ALOWCO/NCOFLD(JPCOMX)
      COMMON /CODIAG/ NCCO,NWAVET,NCOPR(JPCOMX),NVCOHR(JPNL)
      LOGICAL LWRITE,LASCII,LSCRAT,LUTF14,LROGR,LMASSW,LAVMSK,
     -      LAVSIG
      CHARACTER*2 YTYPSF
      CHARACTER*6 YDUM6
      CHARACTER*8 YNAME(2)
      COMMON /COMDAT/BEGDAY,ENDDAY,LWRITE,LASCII,LSCRAT,
     -      LUTF14,LROGR,NSIGFG,NFREQD,NFREQA,NFREQP,RNTAPE,
     -      NASCII,LMASSW,LAVMSK,LAVSIG,YNAME,YTYPSF,YDUM6
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LGCMTR,LHISPH,LHISXP,LDRY,LDIABH,LBL,LVD
      COMMON /MODELC/TSPD,NYEAR,NDAY,NMIN,LGCMTR,NL,TMEAN(JPNL),
     -      LHISPH,LHISXP,LDRY,LDIABH,BEGDYP,LBL,LVD,AKVV,CD

      LOGICAL LOERR,LOVALU

      LOERR=.FALSE.

**    Check that printing has been requested - if not, switch off CO
**          diagnostics
      IF(.NOT.LWRITE) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6000)
6000     FORMAT(1X,'CHKCO: CO DIAGNOSTICS SWITCHED OFF SINCE NO ',
     -         'PRINTING HAS BEEN REQUESTED')
         LDCO=.FALSE.
         RETURN
      ENDIF

**    Check valid code numbers in NCOPR
      IF(NCOPR(1).NE.0) THEN
         DO 2000 K=1,JPCOMX
         LOVALU=.FALSE.
         DO 2100 J=1,JPCOMX
         IF((NCOPR(K).EQ.NCOFLD(J)).OR.(NCOPR(K).EQ.0)) LOVALU=.TRUE.
2100     CONTINUE
         IF(.NOT.LOVALU) THEN
            LOERR=.TRUE.
            NFATAL=NFATAL+1
            WRITE(NCERR,6020) NCOPR(K)
6020        FORMAT(1X,'CHKCO: ERROR IN NCOPR VALUE ',I4)
         ENDIF
2000     CONTINUE
      ELSE
**       Switch of CO diagnostics if no valid fields specified
         NWARN=NWARN+1
         WRITE(NCERR,6025)
6025     FORMAT(1X,'CHKCO: CO DIAGNOSTICS SWITCHED OFF - NO FIELDS'
     -         ' SELECTED')
         LDCO=.FALSE.
      ENDIF

**    Check that maximum total wavenumber requested is greater than 0
      IF(NWAVET.LT.1) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6010)
6010     FORMAT(1X,'CHKCO: ERROR, NWAVET<1')
         LOERR=.TRUE.
      ENDIF

**    Check that all NVCOHR values are valid
      LOVALU=.TRUE.
      DO 3000 J=1,NL
      IF((NVCOHR(J).LT.0).OR.(NVCOHR(J).GT.NL)) LOVALU=.FALSE.
3000  CONTINUE
      IF(.NOT.LOVALU) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6030)
6030     FORMAT(1X,'CHKCO: ERROR IN NVCOHR VALUES')
         LOERR=.TRUE.
      ENDIF

CC    IF(LOERR) STOP
      RETURN
      END
      SUBROUTINE COEXTR(YPCREC)
**    Function - to extract information from CO data and update
**          appropriate common blocks
**    Args in -
**               YPCREC  - character*80 variable containing data
**    Args out - none
**    Args for work - none
**    Com used - /ALOWCO/NCOFLD,/ERRMSG/NCERR,/MODELC/NL
**    Com changed - /CODIAG/all elements,/ERRMSG/NFATAL
**    Params used - /PARAM1/JPNL,/PARAMS/JPCOMX
**    Called by - COPROC
**    Calls - SEARCH,LASTCH,ITRANS,INTCPY,IMULTP,ERRSTP
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /ALOWCO/NCOFLD(JPCOMX)
      COMMON /CODIAG/ NCCO,NWAVET,NCOPR(JPCOMX),NVCOHR(JPNL)
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LGCMTR,LHISPH,LHISXP,LDRY,LDIABH,LBL,LVD
      COMMON /MODELC/TSPD,NYEAR,NDAY,NMIN,LGCMTR,NL,TMEAN(JPNL),
     -      LHISPH,LHISXP,LDRY,LDIABH,BEGDYP,LBL,LVD,AKVV,CD
      LOGICAL LOFIND,LOALL
      DIMENSION ILEV(JPNL)
      CHARACTER*1 YOCOM,YOEQL
      CHARACTER*4 YOKW4
      CHARACTER*7 YOKW7
      CHARACTER*8 YOKW8
      CHARACTER*10 YOKW10
      CHARACTER*80 YPCREC

      YOEQL='='
      YOCOM=','
      DO 100 JL=1,NL
      ILEV(JL)=JL
100   CONTINUE
**    Search for the keyword in YPCREC and then determine the field
**          value
      LOFIND=.FALSE.

      YOKW10='O/PCHANNEL'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
**       Note that IPOSN and IPOSX can take the same value
         IF(IERR.EQ.998) GO TO 1000
         CALL ITRANS(YPCREC,IPOSN,IPOSX,NCCO,12)
         RETURN
      ENDIF

      YOKW8='HIGHWAVE'
      CALL SEARCH(YPCREC,YOKW8,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
**       Note that IPOSN and IPOSX can take the same value
         IF(IERR.EQ.998) GO TO 1000
         CALL ITRANS(YPCREC,IPOSN,IPOSX,NWAVET,12)
         RETURN
      ENDIF

**    Else
      YOKW10='PRINTFIELD'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            CALL INTCPY(NCOFLD,JPCOMX,NCOPR)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NCOPR,JPCOMX)
         RETURN
      ENDIF

**    Else
      YOKW7='HRSLICE'
      CALL SEARCH(YPCREC,YOKW7,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all levels are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            CALL INTCPY(ILEV,NL,NVCOHR)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NVCOHR,NL)
         RETURN
      ENDIF

**    Else
**    Keyword not located
      NFATAL=NFATAL+1
      WRITE(NCERR,6030) YPCREC
6030  FORMAT(1X,'KEYWORD NOT LOCATED FOR PHRASE :'/A)
      CALL ERRSTP

1000  CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6000) YPCREC
6000  FORMAT(1X,'COEXTR: ERROR IN CO DATA ENTRY',/A)
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE COPROC
**    Function - to read and process the CO diagnostic data
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /DIAGTP/LDCO,/ERRMSG/NCERR
**    Com changed - /CODIAG/all elements,/ALOWCO/NCOFLD,/ERRMSG/
**          NFATAL,NWARN
**    Params used - /PARAM1/JPNL,/PARAM1/JPCOMX/
**    Called by - DIAGRD
**    Calls - SEARCH,COEXTR,ERRSTP
**    Files read - 10
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /ALOWCO/NCOFLD(JPCOMX)
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /CODIAG/ NCCO,NWAVET,NCOPR(JPCOMX),NVCOHR(JPNL)
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      CHARACTER*80 YOCREC
      LOGICAL LOFIND
**    Specify the defaults
      DATA NCCO/3/,NCOPR/JPCOMX*0/,NVCOHR/JPNL*0/
      DATA NWAVET/10/
**    Define allowed field codes for CO
      DATA NCOFLD/ 1, 2, 3, 4, 5, 6/

      IF(.NOT.LDCO) THEN
**       CO diagnostics not selected - print warning
         NWARN=NWARN+1
         WRITE(NCERR,6000)
6000     FORMAT(1X,'WARNING: - CO DIAGNOSTICS NOT SELECTED:',
     -         ' ALTHOUGH USER HAS SUPPLIED CO INPUT DATA')
      ENDIF

**    Read and interpret records until '$ENDCO' found
300   READ(10,5000,END=100,ERR=110)YOCREC
5000  FORMAT(A80)
      CALL SEARCH(YOCREC,'$ENDCO',LOFIND)
      IF(LOFIND) THEN
         WRITE(NCERR,6010)
6010     FORMAT(1X,'USER-SUPPLIED CO SELECTION DATA PROCESSED')
         RETURN
      ENDIF

      CALL COEXTR(YOCREC)
      GO TO 300

100   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6020)
6020  FORMAT(1X,'COPROC: EOF WHEN SEARCHING FOR CO DATA')
      CALL ERRSTP

110   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6030)
6030  FORMAT(1X,'COPROC: ERROR WHEN SEARCHING FOR CO DATA')
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE CHKSG
**    Function - to check SG data input
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /COMDAT/LWRITE,LASCII,BEGDAY,ENDDAY,
**          /SGDIAG/NVSGHR,VSGSFC,/MODELC/NL
**          /COMDAT/YTYPSF,/LVDIAG/OUTLEV,/ERRMSG/NCERR
**    Com changed - /DIAGTP/LDSG,/ERRMSG/NFATAL,NWARN
**    Params used - /PARAM1/JPNL,/PARAMS/JPSGMX
**    Called by - DIAGCK
**    Calls - none
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      LOGICAL LWRITE,LASCII,LSCRAT,LUTF14,LROGR,LMASSW,LAVMSK,
     -      LAVSIG
      CHARACTER*2 YTYPSF
      CHARACTER*6 YDUM6
      CHARACTER*8 YNAME(2)
      COMMON /COMDAT/BEGDAY,ENDDAY,LWRITE,LASCII,LSCRAT,
     -      LUTF14,LROGR,NSIGFG,NFREQD,NFREQA,NFREQP,RNTAPE,
     -      NASCII,LMASSW,LAVMSK,LAVSIG,YNAME,YTYPSF,YDUM6
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      COMMON /LVDIAG/OUTLEV(JPNL)
      LOGICAL LGCMTR,LHISPH,LHISXP,LDRY,LDIABH,LBL,LVD
      COMMON /MODELC/TSPD,NYEAR,NDAY,NMIN,LGCMTR,NL,TMEAN(JPNL),
     -      LHISPH,LHISXP,LDRY,LDIABH,BEGDYP,LBL,LVD,AKVV,CD
      LOGICAL LDGH,LDGZ,LDGM,LDGP,LDG3,LDSGTD,LDSGTS,LDSGIN
      COMMON /SGDIAG/LDGH,LDGZ,LDGM,LDGP,LDG3,LDSGTD,LDSGTS,LDSGIN,
     -      NCSG,NVSGHR(JPNL),VSGSFC(JPNL)
      LOGICAL LOERR,LOVALU

      LOERR=.FALSE.

**    Switch off time-series and time-averaging calculations if
**    ENDDAY is not greater than BEGDAY
      IF(ABS(ENDDAY).LE.ABS(BEGDAY)) THEN
         IF(LDSGTS) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6100)
6100        FORMAT(1X,'CHKSG: SG TIME-SERIES SWITCHED OFF, SINCE ',
     -            'ENDDAY.LE.BEGDAY')
            LDSGTS=.FALSE.
         ENDIF
         IF(LDSGTD) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6110)
6110        FORMAT(1X,'CHKSG: SG TIME-AVERAGES (D) SWITCHED OFF,',
     -            ' SINCE ENDDAY.LE.BEGDAY')
            LDSGTD=.FALSE.
         ENDIF
      ENDIF

**    NVSGHR values only used when output is on model surfaces, and
**    VSGSFC values only used when output is on non-model surfaces
      IF((NVSGHR(1).NE.0).AND.(YTYPSF.NE.'ET')) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6060)
6060     FORMAT(1X,'CHKSG: NVSGHR VALUE(S) SUPPLIED, BUT NOT USED')
      ENDIF
      IF((NVSGHR(1).LE.0).AND.(YTYPSF.EQ.'ET')) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6065)
6065     FORMAT(1X,'CHKSG: NVSGHR VALUES NOT SUPPLIED,'
     -         ' OUTPUT MAY BE SUPPRESSED')
      ENDIF
      IF((VSGSFC(1).GT.0.0).AND.(YTYPSF.EQ.'ET')) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6070)
6070     FORMAT(1X,'CHKSG: VSGSFC VALUE(S) SUPPLIED, BUT NOT USED')
      ENDIF
      IF((VSGSFC(1).LE.0.0).AND.(YTYPSF.NE.'ET')) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6075)
6075     FORMAT(1X,'CHKSG: VSGSFC VALUES NOT SUPPLIED,'
     -         ' OUTPUT MAY BE SUPPRESSED')
      ENDIF

**    Check that VSGSFC values are a subset of OUTLEV values if they are
**    to be used. ZDELTA is a small parameter to allow two REAL values
**    to be compared
      ZDELTA=1.0E-10
      IF(YTYPSF.NE.'ET') THEN
         DO 4000 K=1,NL
         IF(VSGSFC(K).GT.0.0) THEN
            ZSFCP=VSGSFC(K)+ZDELTA
            ZSFCM=VSGSFC(K)-ZDELTA
            LOVALU=.FALSE.
            DO 4100 J=1,NL
            IF(ZSFCM.LE.OUTLEV(J).AND.ZSFCP.GE.OUTLEV(J)) LOVALU=.TRUE.
4100        CONTINUE
            IF(.NOT.LOVALU) THEN
               LOERR=.TRUE.
               NFATAL=NFATAL+1
               WRITE(NCERR,6080) VSGSFC(K)
6080           FORMAT(1X,'CHKSG: VSGSFC CORRESPONDS TO INVALID LEVEL ',
     -               F12.3)
            ENDIF
         ENDIF
4000     CONTINUE
      ENDIF

**    Check that NVSGHR values lie in the range 1 to NL
      IF(YTYPSF.EQ.'ET') THEN
         DO 5000 J=1,NL
         IF(NVSGHR(J).NE.0) THEN
            IF(NVSGHR(J).LT.1.OR.NVSGHR(J).GT.NL) THEN
               NFATAL=NFATAL+1
               WRITE(NCERR,6090)
6090           FORMAT(1X,'CHKSG: INVALID NVSGHR VALUE')
               LOERR=.TRUE.
            ENDIF
         ENDIF
5000     CONTINUE
      ENDIF

CC    IF(LOERR) STOP
      RETURN
      END
      SUBROUTINE SGEXTR(YPCREC)
**    Function - to extract information from SG data and update
**          appropriate common blocks
**    Args in -
**               YPCREC  - character*80 variable containing data
**    Args out - none
**    Args for work - none
**    Com used - /ERRMSG/NCERR,/MODELC/NL
**    Com changed - /SGDIAG/all elements,/ERRMSG/NFATAL
**    Params used - /PARAM1/JPNL,/PARAMS/JPSGMX
**    Called by - SGPROC
**    Calls - SEARCH,LPROC,INTCPY,IMULTP,LASTCH,ITRANS,RMULTP,ERRSTP
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LGCMTR,LHISPH,LHISXP,LDRY,LDIABH,LBL,LVD
      COMMON /MODELC/TSPD,NYEAR,NDAY,NMIN,LGCMTR,NL,TMEAN(JPNL),
     -      LHISPH,LHISXP,LDRY,LDIABH,BEGDYP,LBL,LVD,AKVV,CD
      LOGICAL LDGH,LDGZ,LDGM,LDGP,LDG3,LDSGTD,LDSGTS,LDSGIN
      COMMON /SGDIAG/LDGH,LDGZ,LDGM,LDGP,LDG3,LDSGTD,LDSGTS,LDSGIN,
     -      NCSG,NVSGHR(JPNL),VSGSFC(JPNL)
      LOGICAL LOFIND,LOALL
      DIMENSION INLFLD(JPNL)
      CHARACTER*1 YOCOM,YOEQL
      CHARACTER*4 YOKW4
      CHARACTER*5 YOKW5
      CHARACTER*7 YOKW7
      CHARACTER*8 YOKW8
      CHARACTER*9 YOKW9
      CHARACTER*10 YOKW10
      CHARACTER*13 YOKW13
      CHARACTER*80 YPCREC

      YOEQL='='
      YOCOM=','
**    Search for the keyword in YPCREC and then determine the field
**          value
      LOFIND=.FALSE.

      YOKW10='HORIZONTAL'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDGH)
         RETURN
      ENDIF

**    Else
      YOKW5='ZONAL'
      CALL SEARCH(YPCREC,YOKW5,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDGZ)
         RETURN
      ENDIF

**    Else
      YOKW10='MERIDIONAL'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDGM)
         RETURN
      ENDIF

**    Else
      YOKW7='PROFILE'
      CALL SEARCH(YPCREC,YOKW7,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDGP)
         RETURN
      ENDIF

**    Else
      YOKW8='3DOUTPUT'
      CALL SEARCH(YPCREC,YOKW8,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDG3)
         RETURN
      ENDIF

**    Else
      YOKW9='TAVERAGED'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDSGTD)
         RETURN
      ENDIF

**    Else
      YOKW13='INSTANTANEOUS'
      CALL SEARCH(YPCREC,YOKW13,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDSGIN)
         RETURN
      ENDIF

**    Else
      YOKW7='TSERIES'
      CALL SEARCH(YPCREC,YOKW7,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDSGTS)
         RETURN
      ENDIF

**    Else
      YOKW10='O/PCHANNEL'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
**       Note that IPOSN and IPOSX can take the same value
         IF(IERR.EQ.998) GO TO 1000
         CALL ITRANS(YPCREC,IPOSN,IPOSX,NCSG,12)
         RETURN
      ENDIF

**    Else
      YOKW8='SGLEVELS'
      CALL SEARCH(YPCREC,YOKW8,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            DO 3500 JL=1,NL
3500        INLFLD(JL)=JL
            CALL INTCPY(INLFLD,NL,NVSGHR)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NVSGHR,NL)
         RETURN
      ENDIF

**    Else
      YOKW10='SGSURFACES'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
      IF(LOFIND) THEN
         CALL RMULTP(YPCREC,YOEQL,YOCOM,VSGSFC,NL)
         RETURN
      ENDIF

**    Else
**    Keyword not located
      NFATAL=NFATAL+1
      WRITE(NCERR,6030) YPCREC
6030  FORMAT(1X,'KEYWORD NOT LOCATED FOR PHRASE :'/A)
      CALL ERRSTP

1000  CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6000) YPCREC
6000  FORMAT(1X,'SGEXTR: ERROR IN SG DATA ENTRY',/A)
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE SGPROC
**    Function - to read and process the SG diagnostic data
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /DIAGTP/LDSG,/ERRMSG/NCERR
**    Com changed - /SGDIAG/all elements,/ALOWSG/NSGFLD,/ERRMSG/NFATAL,
**          NWARN
**    Called by - DIAGRD
**    Calls - SEARCH,SGEXTR,ERRSTP
**    Files read - 10
**    Files written - ERRSTP
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /ALOWSG/NSGFLD(JPSGMX)
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDGH,LDGZ,LDGM,LDGP,LDG3,LDSGTD,LDSGTS,LDSGIN
      COMMON /SGDIAG/LDGH,LDGZ,LDGM,LDGP,LDG3,LDSGTD,LDSGTS,LDSGIN,
     -      NCSG,NVSGHR(JPNL),VSGSFC(JPNL)
      CHARACTER*80 YOCREC
      LOGICAL LOFIND
**    Specify the defaults
      DATA NCSG/3/
      DATA LDGH/.FALSE./,LDGZ/.TRUE./,LDGM/.FALSE./
      DATA LDGP/.FALSE./,LDG3/.FALSE./
      DATA LDSGTS/.FALSE./,LDSGTD/.TRUE./,LDSGIN/.FALSE./
      DATA NVSGHR/JPNL*0/,VSGSFC/JPNL*0.0/

**    Define allowed field codes for SG
      DO 200 J=1,JPSGMX
      NSGFLD(J)=J
200   CONTINUE

      IF(.NOT.LDSG) THEN
**       SG diagnostics not selected - print warning
         NWARN=NWARN+1
         WRITE(NCERR,6000)
6000     FORMAT(1X,'WARNING: - SG DIAGNOSTICS NOT SELECTED:',
     -         ' ALTHOUGH USER HAS SUPPLIED SG INPUT DATA')
      ENDIF

**    Read and interpret records until '$ENDSG' found
300   READ(10,5000,END=100,ERR=110)YOCREC
5000  FORMAT(A80)
      CALL SEARCH(YOCREC,'$ENDSG',LOFIND)
      IF(LOFIND) THEN
         WRITE(NCERR,6010)
6010     FORMAT(1X,'USER-SUPPLIED SG SELECTION DATA PROCESSED')
         RETURN
      ENDIF

      CALL SGEXTR(YOCREC)
      GO TO 300

100   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6020)
6020  FORMAT(1X,'SGPROC: EOF WHEN SEARCHING FOR SG DATA')
      CALL ERRSTP

110   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6030)
6030  FORMAT(1X,'SGPROC: ERROR WHEN SEARCHING FOR SG DATA')
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE CHKGH
**    Function - to check GH data input
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /SGDIAG/NVSGHR,VSGSFC,/GHDIAG/LDGHSL,
**          VGHTS,NGHVEC,/DIAGTP/LDSG,/COMDAT/YTYPSF,
**          LWRITE,LASCII,/ALOWSG/NSGFLD,/ERRMSG/NCERR
**    Com changed - /GHDIAG/NGHPR,NGHPL,NGH,LDGHCT,/SGDIAG/LDGH,
**          /ERRMSG/NFATAL,NWARN
**    Params used - /PARAM1/JPNL,/PARAMS/JPSGMX
**    Called by - DIAGCK
**    Calls - LEVCHK,LATCHK
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /ALOWSG/NSGFLD(JPSGMX)
      LOGICAL LWRITE,LASCII,LSCRAT,LUTF14,LROGR,LMASSW,LAVMSK,
     -      LAVSIG
      CHARACTER*2 YTYPSF
      CHARACTER*6 YDUM6
      CHARACTER*8 YNAME(2)
      COMMON /COMDAT/BEGDAY,ENDDAY,LWRITE,LASCII,LSCRAT,
     -      LUTF14,LROGR,NSIGFG,NFREQD,NFREQA,NFREQP,RNTAPE,
     -      NASCII,LMASSW,LAVMSK,LAVSIG,YNAME,YTYPSF,YDUM6
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDGHSL,LDGHCT,LDGHED
      COMMON /GHDIAG/LDGHSL,LDGHCT,LDGHED,VGHTS(2),
     -      NGHPR(JPSGMX),NGHPL(JPSGMX),NGH,NGHVEC(JPSGMX),
     -      CINTGH(JPSGMX)
      LOGICAL LDGH,LDGZ,LDGM,LDGP,LDG3,LDSGTD,LDSGTS,LDSGIN
      COMMON /SGDIAG/LDGH,LDGZ,LDGM,LDGP,LDG3,LDSGTD,LDSGTS,LDSGIN,
     -      NCSG,NVSGHR(JPNL),VSGSFC(JPNL)
      LOGICAL LOERR,LOVALU

      LOERR=.FALSE.

**    SG/GH diagnostics require that LDSG and LDGH are set to .TRUE.
      IF((.NOT.LDSG).OR.(.NOT.LDGH)) THEN
         IF((LDGHSL).OR.(LDGHCT).OR.(VGHTS(1).GT.-90.0).OR.
     -         (VGHTS(2).GT.-90.0)) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6000)
6000        FORMAT(1X,'CHKGH: WARNING - GH DIAGNOSTICS REQUESTED BUT',
     -            ' HORIZONTAL LDSG/LDGH FLAGS NOT SET')
            RETURN
         ENDIF
      ENDIF

**    Check that printing has been requested - if not, ensure
**          NGHPR=0 for all elements
      IF(.NOT.LWRITE) THEN
         IF(NGHPR(1).NE.0) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6005)
6005        FORMAT(1X,'CHKGH: NGHPR VALUES CHANGED - NO PRINTING')
            DO 1000 K=1,JPSGMX
1000        NGHPR(K)=0
**          Reset value of NGH
            NGH=0
            DO 1001 K=1,JPSGMX
            IF(NGHPL(K).GT.0) NGH=NGH+1
1001        CONTINUE
         ENDIF
      ENDIF

**    Check that UTFs are requested - if not, ensure NGHPL=0
**          for all elements
      IF(.NOT.LASCII) THEN
         IF(NGHPL(1).NE.0) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6010)
6010        FORMAT(1X,'CHKGH: NGHPL VALUES CHANGED - NO PLOTTING')
            DO 1100 K=1,JPSGMX
1100        NGHPL(K)=0
**          Reset value of NGH
            NGH=0
            DO 1101 K=1,JPSGMX
            IF(NGHPR(K).GT.0) NGH=NGH+1
1101        CONTINUE
         ENDIF
      ENDIF

**    Check valid code numbers in NGHPR
      IF(NGHPR(1).NE.0) THEN
         DO 2000 K=1,JPSGMX
         LOVALU=.FALSE.
         DO 2100 J=1,JPSGMX
         IF((NGHPR(K).EQ.NSGFLD(J)).OR.(NGHPR(K).EQ.0)) LOVALU=.TRUE.
2100     CONTINUE
         IF(.NOT.LOVALU) THEN
            LOERR=.TRUE.
            NFATAL=NFATAL+1
            WRITE(NCERR,6020) NGHPR(K)
6020        FORMAT(1X,'CHKGH: ERROR IN NGHPR VALUE ',I4)
         ENDIF
2000     CONTINUE
      ENDIF

**    Check valid code numbers in NGHPL
      IF(NGHPL(1).NE.0) THEN
         DO 3000 K=1,JPSGMX
         LOVALU=.FALSE.
         DO 3100 J=1,JPSGMX
         IF((NGHPL(K).EQ.NSGFLD(J)).OR.(NGHPL(K).EQ.0)) LOVALU=.TRUE.
3100     CONTINUE
         IF(.NOT.LOVALU) THEN
            LOERR=.TRUE.
            NFATAL=NFATAL+1
            WRITE(NCERR,6030) NGHPL(K)
6030        FORMAT(1X,'CHKGH: ERROR IN NGHPL VALUE ',I4)
         ENDIF
3000     CONTINUE
      ENDIF

**    Switch off diagnostics if NGH is zero
      IF(NGH.EQ.0.AND.LDGH) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6035)
6035     FORMAT(1X,'CHKGH: GH DIAGNOSTICS SWITCHED OFF, NGH=0')
         LDGH=.FALSE.
         RETURN
      ENDIF
      IF(.NOT.LDGH) RETURN

**    Check that NGHVEC values correspond to NGHPL values
      IF(NGHVEC(1).NE.0) THEN
         DO 3500 K=1,JPSGMX
         LOVALU=.FALSE.
         DO 3600 J=1,JPSGMX
         IF((NGHVEC(K).EQ.NGHPL(J)).OR.(NGHVEC(K).EQ.0)) LOVALU=.TRUE.
3600     CONTINUE
         IF(.NOT.LOVALU) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6050)
6050        FORMAT(1X,'CHKGH: NGHVEC CORRESPONDS TO NON-PLOTTED FIELD')
         ENDIF
3500     CONTINUE
      ENDIF

**    Check that NGH is non-zero, and less than JPSGMX
      IF(NGH.LE.0.OR.(NGH.GT.JPSGMX)) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6040)
6040     FORMAT(1X,'CHKGH : INVALID VALUE FOR NGH')
         LOERR=.TRUE.
      ENDIF

**    Check that NVSGHR or VSGSFC have been set if LDGHSL is .TRUE.
      IF(LDGHSL.AND.(YTYPSF.EQ.'ET').AND.(NVSGHR(1).EQ.0)) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6055)
6055     FORMAT(1X,'CHKGH: ERROR - HORIZONTAL SLICE O/P REQUIRES'
     -         ' NVSGHR TO BE SPECIFIED FOR O/P ON SIGMA SURFACES')
         LOERR=.TRUE.
      ENDIF
      ZDELTA=1.0E-10
      IF(LDGHSL.AND.(YTYPSF.NE.'ET').AND.(VSGSFC(1).LE.ZDELTA)) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6060)
6060     FORMAT(1X,'CHKGH: ERROR - HORIZONTAL SLICE O/P REQUIRES'
     -         ' VSGSFC TO BE SPECIFIED FOR O/P ON NON-SIGMA SURFACES')
         LOERR=.TRUE.
      ENDIF

**    Check validity of VGHTS values
      CALL LATCHK(VGHTS,2,-999.0,'(SG) GH VGHTS',LOERR)

**    Column totals over the model domain can only be found when output
**    is on 'ET' surfaces
      IF((YTYPSF.NE.'ET').AND.LDGHCT) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6080)
6080     FORMAT(1X,'CHKGH: ERROR - COLUMN TOTALS NOT APPROPRIATE ',
     -         ' SINCE O/P NOT ON MODEL LEVELS')
         LDGHCT=.FALSE.
         LOERR=.TRUE.
      ENDIF

CC    IF(LOERR) STOP
      RETURN
      END
      SUBROUTINE GHEXTR(YPCREC)
**    Function - to extract information from GH data and update
**          appropriate common blocks
**    Args in -
**               YPCREC  - character*80 variable containing data
**    Args out - none
**    Args for work - none
**    Com used - /ALOWSG/NSGFLD,/ERRMSG/NCERR
**    Com changed - /GHDIAG/all elements,/ERRMSG/NFATAL
**    Params used - /PARAMS/JPSGMX
**    Called by - GHPROC
**    Calls - SEARCH,LPROC,RMULTP,IMULTP,INTCPY,ITRANS,LASTCH,ERRSTP
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /ALOWSG/NSGFLD(JPSGMX)
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDGHSL,LDGHCT,LDGHED
      COMMON /GHDIAG/LDGHSL,LDGHCT,LDGHED,VGHTS(2),
     -      NGHPR(JPSGMX),NGHPL(JPSGMX),NGH,NGHVEC(JPSGMX),
     -      CINTGH(JPSGMX)
      LOGICAL LOFIND,LOALL
      CHARACTER*1 YOCOM,YOEQL
      CHARACTER*2 YOKW2
      CHARACTER*4 YOKW4
      CHARACTER*5 YOKW5
      CHARACTER*8 YOKW8
      CHARACTER*9 YOKW9
      CHARACTER*10 YOKW10
      CHARACTER*14 YOKW14
      CHARACTER*80 YPCREC

      YOEQL='='
      YOCOM=','
**    Search for the keyword in YPCREC and then determine the field
**          value
      LOFIND=.FALSE.

      YOKW2='CI'
      CALL SEARCH(YPCREC,YOKW2,LOFIND)
      IF(LOFIND) THEN
         CALL RMULTP(YPCREC,YOEQL,YOCOM,CINTGH,JPSGMX)
         RETURN
      ENDIF


**    Else
      YOKW5='SLICE'
      CALL SEARCH(YPCREC,YOKW5,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDGHSL)
         RETURN
      ENDIF

**    Else
      YOKW8='COLTOTAL'
      CALL SEARCH(YPCREC,YOKW8,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDGHCT)
         RETURN
      ENDIF

**    Else
      YOKW4='EDDY'
      CALL SEARCH(YPCREC,YOKW4,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDGHED)
         RETURN
      ENDIF

**    Else
      YOKW9='HOVMOLLER'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
         CALL RMULTP(YPCREC,YOEQL,YOCOM,VGHTS,2)
         RETURN
      ENDIF

**    Else
      YOKW10='PRINTFIELD'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            CALL INTCPY(NSGFLD,JPSGMX,NGHPR)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NGHPR,JPSGMX)
         RETURN
      ENDIF

**    Else
      YOKW9='PLOTFIELD'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            CALL INTCPY(NSGFLD,JPSGMX,NGHPL)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NGHPL,JPSGMX)
         RETURN
      ENDIF

**    Else
      YOKW14='NUMBEROFFIELDS'
      CALL SEARCH(YPCREC,YOKW14,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
**       Note that IPOSN and IPOSX can take the same value
         IF(IERR.EQ.998) GO TO 1000
         CALL ITRANS(YPCREC,IPOSN,IPOSX,NGH,12)
         RETURN
      ENDIF

**    Else
      YOKW10='WINDVECTOR'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            CALL INTCPY(NSGFLD,JPSGMX,NGHVEC)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NGHVEC,JPSGMX)
         RETURN
      ENDIF

**    Else
**    Keyword not located
      NFATAL=NFATAL+1
      WRITE(NCERR,6030) YPCREC
6030  FORMAT(1X,'KEYWORD NOT LOCATED FOR PHRASE :'/A)
      CALL ERRSTP

1000  CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6000) YPCREC
6000  FORMAT(1X,'GHEXTR: ERROR IN GH DATA ENTRY',/A)
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE GHPROC
**    Function - to read and process the GH diagnostic data
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /SGDIAG/LDGH,/DIAGTP/LDSG,/ERRMSG/NCERR
**    Com changed - /GHDIAG/all elements,/ERRMSG/NFATAL,NWARN
**    Called by - DIAGRD
**    Calls - SEARCH,GHEXTR,ERRSTP
**    Files read - 10
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDGHSL,LDGHCT,LDGHED
      COMMON /GHDIAG/LDGHSL,LDGHCT,LDGHED,VGHTS(2),
     -      NGHPR(JPSGMX),NGHPL(JPSGMX),NGH,NGHVEC(JPSGMX),
     -      CINTGH(JPSGMX)
      LOGICAL LDGH,LDGZ,LDGM,LDGP,LDG3,LDSGTD,LDSGTS,LDSGIN
      COMMON /SGDIAG/LDGH,LDGZ,LDGM,LDGP,LDG3,LDSGTD,LDSGTS,LDSGIN,
     -      NCSG,NVSGHR(JPNL),VSGSFC(JPNL)
      CHARACTER*80 YOCREC
      LOGICAL LOFIND
**    Specify the defaults
      PARAMETER(R999=-999.0,RNEG=-0.1)
      DATA LDGHSL/.FALSE./,LDGHCT/.FALSE./,LDGHED/.FALSE./
      DATA VGHTS/2*R999/
      DATA NGH/0/,NGHVEC/JPSGMX*0/,NGHPR/JPSGMX*0/,NGHPL/JPSGMX*0/
      DATA CINTGH/JPSGMX*RNEG/

      IF(.NOT.(LDSG.AND.LDGH)) THEN
**       GH diagnostics not selected - print warning
         NWARN=NWARN+1
         WRITE(NCERR,6000)
6000     FORMAT(1X,'WARNING: - GH DIAGNOSTICS NOT SELECTED:',
     -         ' ALTHOUGH USER HAS SUPPLIED GH INPUT DATA')
      ENDIF

**    Read and interpret records until '$ENDGH' found
300   READ(10,5000,END=100,ERR=110)YOCREC
5000  FORMAT(A80)
      CALL SEARCH(YOCREC,'$ENDGH',LOFIND)
      IF(LOFIND) THEN
         WRITE(NCERR,6010)
6010     FORMAT(1X,'USER-SUPPLIED GH SELECTION DATA PROCESSED')
         RETURN
      ENDIF

      CALL GHEXTR(YOCREC)
      GO TO 300

100   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6020)
6020  FORMAT(1X,'GHPROC: EOF WHEN SEARCHING FOR GH DATA')
      CALL ERRSTP

110   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6030)
6030  FORMAT(1X,'GHPROC: ERROR WHEN SEARCHING FOR GH DATA')
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE CHKGZ
**    Function - to check GZ data input
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /SGDIAG/NVSGHR,VSGSFC,/GZDIAG/all elements,
**          /DIAGTP/LDSG,/COMDAT/YTYPSF,LWRITE,LASCII,ALOWSG/NSGFLD,
**          /ERRMSG/NFATAL,NWARN
**    Com changed - /GZDIAG/NGZPR,NGZPL,NGZ,/SGDIAG/LDGZ,/ERRMSG
**          NFATAL,NWARN
**    Params used - /PARAM1/JPNL,/PARAMS/JPSGMX
**    Called by - DIAGCK
**    Calls - LNGCHK
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /ALOWSG/NSGFLD(JPSGMX)
      LOGICAL LWRITE,LASCII,LSCRAT,LUTF14,LROGR,LMASSW,LAVMSK,
     -      LAVSIG
      CHARACTER*2 YTYPSF
      CHARACTER*6 YDUM6
      CHARACTER*8 YNAME(2)
      COMMON /COMDAT/BEGDAY,ENDDAY,LWRITE,LASCII,LSCRAT,
     -      LUTF14,LROGR,NSIGFG,NFREQD,NFREQA,NFREQP,RNTAPE,
     -      NASCII,LMASSW,LAVMSK,LAVSIG,YNAME,YTYPSF,YDUM6
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDGZZM,LDGZTS
      COMMON /GZDIAG/LDGZZM,VGZNS,LDGZTS,VGZNSA(2),NGZPR(JPSGMX),
     -      NGZPL(JPSGMX),NGZ,NGZVEC(JPSGMX),CINTGZ(JPSGMX)
      LOGICAL LDGH,LDGZ,LDGM,LDGP,LDG3,LDSGTD,LDSGTS,LDSGIN
      COMMON /SGDIAG/LDGH,LDGZ,LDGM,LDGP,LDG3,LDSGTD,LDSGTS,LDSGIN,
     -      NCSG,NVSGHR(JPNL),VSGSFC(JPNL)
      LOGICAL LOERR,LOVALU

      LOERR=.FALSE.

**    SG/GZ diagnostics require that LDSG and LDGZ are set to .TRUE.
      IF((.NOT.LDSG).OR.(.NOT.LDGZ)) THEN
         IF((LDGZZM).OR.(LDGZTS).OR.(VGZNS.GT.0.0).OR.
     -         (VGZNSA(1).GT.0.0).OR.(VGZNSA(2).GT.0.0)) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6000)
6000        FORMAT(1X,'CHKGZ: WARNING - GZ DIAGNOSTICS REQUESTED BUT',
     -            ' BOTH LDSG/LDGZ FLAGS NOT SET')
            RETURN
         ENDIF
      ENDIF

**    Check that printing has been requested - if not, ensure
**          NGZPR=0 for all elements
      IF(.NOT.LWRITE) THEN
         IF(NGZPR(1).NE.0) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6005)
6005        FORMAT(1X,'CHKGZ: NGZPR VALUES CHANGED - NO PRINTING')
            DO 1000 K=1,JPSGMX
1000        NGZPR(K)=0
**          Reset value of NGZ
            NGZ=0
            DO 1001 K=1,JPSGMX
            IF(NGZPL(K).GT.0) NGZ=NGZ+1
1001        CONTINUE
         ENDIF
      ENDIF

**    Check that UTFs are requested - if not, ensure NGZPL=0
**          for all elements
      IF(.NOT.LASCII) THEN
         IF(NGZPL(1).NE.0) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6010)
6010        FORMAT(1X,'CHKGZ: NGZPL VALUES CHANGED - NO PLOTTING')
            DO 1100 K=1,JPSGMX
1100        NGZPL(K)=0
**          Reset value of NGZ
            NGZ=0
            DO 1101 K=1,JPSGMX
            IF(NGZPR(K).GT.0) NGZ=NGZ+1
1101        CONTINUE
         ENDIF
      ENDIF

**    Check valid code numbers in NGZPR
      IF(NGZPR(1).NE.0) THEN
         DO 2000 K=1,JPSGMX
         LOVALU=.FALSE.
         DO 2100 J=1,JPSGMX
         IF((NGZPR(K).EQ.NSGFLD(J)).OR.(NGZPR(K).EQ.0)) LOVALU=.TRUE.
2100     CONTINUE
         IF(.NOT.LOVALU) THEN
            LOERR=.TRUE.
            NFATAL=NFATAL+1
            WRITE(NCERR,6020) NGZPR(K)
6020        FORMAT(1X,'CHKGZ: ERROR IN NGZPR VALUE ',I4)
         ENDIF
2000     CONTINUE
      ENDIF

**    Check valid code numbers in NGZPL
      IF(NGZPL(1).NE.0) THEN
         DO 3000 K=1,JPSGMX
         LOVALU=.FALSE.
         DO 3100 J=1,JPSGMX
         IF((NGZPL(K).EQ.NSGFLD(J)).OR.(NGZPL(K).EQ.0)) LOVALU=.TRUE.
3100     CONTINUE
         IF(.NOT.LOVALU) THEN
            LOERR=.TRUE.
            NFATAL=NFATAL+1
            WRITE(NCERR,6030) NGZPL(K)
6030        FORMAT(1X,'CHKGZ: ERROR IN NGZPL VALUE ',I4)
         ENDIF
3000     CONTINUE
      ENDIF

**    Switch off diagnostics if NGZ is zero
      IF(NGZ.EQ.0.AND.LDGZ) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6035)
6035     FORMAT(1X,'CHKGZ: GZ DIAGNOSTICS SWITCHED OFF, NGZ=0')
         LDGZ=.FALSE.
         RETURN
      ENDIF
      IF(.NOT.LDGZ) RETURN

**    Check that NGZVEC values correspond to NGZPL values
      IF(NGZVEC(1).NE.0) THEN
         DO 3500 K=1,JPSGMX
         LOVALU=.FALSE.
         DO 3600 J=1,JPSGMX
         IF((NGZVEC(K).EQ.NGZPL(J)).OR.(NGZVEC(K).EQ.0)) LOVALU=.TRUE.
3600     CONTINUE
         IF(.NOT.LOVALU) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6050)
6050        FORMAT(1X,'CHKGZ: NGZVEC CORRESPONDS TO NON-PLOTTED FIELD')
         ENDIF
3500     CONTINUE
      ENDIF

**    Check that NGZ is non-zero, and less than JPSGMX
      IF(NGZ.LE.0.OR.(NGZ.GT.JPSGMX)) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6040)
6040     FORMAT(1X,'CHKGZ : INVALID VALUE FOR NGZ')
         LOERR=.TRUE.
      ENDIF

**    Check that NVSGHR or VSGSFC have been set if LDGZTS is .TRUE.
      IF(LDGZTS.AND.(YTYPSF.EQ.'ET').AND.(NVSGHR(1).EQ.0)) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6060)
6060     FORMAT(1X,'CHKGZ: ERROR - ZONAL TIMESERIES O/P REQUIRES'
     -         ' NVSGHR TO BE SPECIFIED FOR O/P ON SIGMA SURFACES')
         LOERR=.TRUE.
      ENDIF
      ZDELTA=1.0E-10
      IF(LDGZTS.AND.(YTYPSF.NE.'ET').AND.(VSGSFC(1).LE.ZDELTA)) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6070)
6070     FORMAT(1X,'CHKGZ: ERROR - ZONAL TIMESERIES O/P REQUIRES'
     -         ' VSGSFC TO BE SPECIFIED FOR O/P ON NON-SIGMA SURFACES')
         LOERR=.TRUE.
      ENDIF

**    Check validity of VGZNS AND VGZNSA values
      CALL LNGCHK(VGZNS,1,-999.0,'(SG) GZ VGZNS',LOERR)
      CALL LNGCHK(VGZNSA,2,-999.0,'(SG) GZ VGZNSA',LOERR)

CC    IF(LOERR) STOP
      RETURN
      END
      SUBROUTINE GZEXTR(YPCREC)
**    Function - to extract information from GZ data and update
**          appropriate common blocks
**    Args in -
**               YPCREC  - character*80 variable containing data
**    Args out - none
**    Args for work - none
**    Com used - /ALOWSG/NSGFLD,/ERRMSG/NCERR
**    Com changed - /GZDIAG/all elements,/ERRMSG/NFATAL
**    Params used - /PARAMS/JPSGMX
**    Called by - GZPROC
**    Calls - SEARCH,LPROC,RMULTP,LASTCH,RTRANS,IMULTP,INTCPY,
**          ITRANS,LASTCH,ERRSTP
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /ALOWSG/NSGFLD(JPSGMX)
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDGZZM,LDGZTS
      COMMON /GZDIAG/LDGZZM,VGZNS,LDGZTS,VGZNSA(2),NGZPR(JPSGMX),
     -      NGZPL(JPSGMX),NGZ,NGZVEC(JPSGMX),CINTGZ(JPSGMX)
      LOGICAL LOFIND,LOALL
      CHARACTER*1 YOCOM,YOEQL
      CHARACTER*2 YOKW2
      CHARACTER*4 YOKW4
      CHARACTER*7 YOKW7
      CHARACTER*9 YOKW9
      CHARACTER*10 YOKW10
      CHARACTER*14 YOKW14
      CHARACTER*80 YPCREC

      YOEQL='='
      YOCOM=','
**    Search for the keyword in YPCREC and then determine the field
**          value
      LOFIND=.FALSE.

      YOKW2='CI'
      CALL SEARCH(YPCREC,YOKW2,LOFIND)
      IF(LOFIND) THEN
         CALL RMULTP(YPCREC,YOEQL,YOCOM,CINTGZ,JPSGMX)
         RETURN
      ENDIF


**    Else
      YOKW9='ZONALMEAN'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDGZZM)
         RETURN
      ENDIF

**    Else
      YOKW9='TIMESLICE'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDGZTS)
         RETURN
      ENDIF

**    Else
      YOKW9='NSAVERAGE'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
         CALL RMULTP(YPCREC,YOEQL,YOCOM,VGZNSA,2)
         RETURN
      ENDIF

**    Else
      YOKW7='NSSLICE'
      CALL SEARCH(YPCREC,YOKW7,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
         IF(IERR.NE.0) GO TO 1000
         CALL RTRANS(YPCREC,IPOSN,IPOSX,VGZNS,12)
         RETURN
      ENDIF

**    Else
      YOKW10='PRINTFIELD'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            CALL INTCPY(NSGFLD,JPSGMX,NGZPR)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NGZPR,JPSGMX)
         RETURN
      ENDIF

**    Else
      YOKW9='PLOTFIELD'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            CALL INTCPY(NSGFLD,JPSGMX,NGZPL)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NGZPL,JPSGMX)
         RETURN
      ENDIF

**    Else
      YOKW14='NUMBEROFFIELDS'
      CALL SEARCH(YPCREC,YOKW14,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
**       Note that IPOSN and IPOSX can take the same value
         IF(IERR.EQ.998) GO TO 1000
         CALL ITRANS(YPCREC,IPOSN,IPOSX,NGZ,12)
         RETURN
      ENDIF

**    Else
      YOKW10='WINDVECTOR'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            CALL INTCPY(NSGFLD,JPSGMX,NGZVEC)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NGZVEC,JPSGMX)
         RETURN
      ENDIF

**    Else
**    Keyword not located
      NFATAL=NFATAL+1
      WRITE(NCERR,6030) YPCREC
6030  FORMAT(1X,'KEYWORD NOT LOCATED FOR PHRASE :'/A)
      CALL ERRSTP

1000  CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6000) YPCREC
6000  FORMAT(1X,'GZEXTR: ERROR IN GZ DATA ENTRY',/A)
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE GZPROC
**    Function - to read and process the GZ diagnostic data
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /SGDIAG/LDGZ,/DIAGTP/LDSG,/ERRMSG/NCERR
**    Com changed - /GZDIAG/all elements,/ERRMSG/NFATAL,NWARN
**    Called by - DIAGRD
**    Calls - SEARCH,GZEXTR,ERRSTP
**    Files read - 10
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDGZZM,LDGZTS
      COMMON /GZDIAG/LDGZZM,VGZNS,LDGZTS,VGZNSA(2),NGZPR(JPSGMX),
     -      NGZPL(JPSGMX),NGZ,NGZVEC(JPSGMX),CINTGZ(JPSGMX)
      LOGICAL LDGH,LDGZ,LDGM,LDGP,LDG3,LDSGTD,LDSGTS,LDSGIN
      COMMON /SGDIAG/LDGH,LDGZ,LDGM,LDGP,LDG3,LDSGTD,LDSGTS,LDSGIN,
     -      NCSG,NVSGHR(JPNL),VSGSFC(JPNL)
      CHARACTER*80 YOCREC
      LOGICAL LOFIND
**    Specify the defaults
      PARAMETER(R999=-999.0,RNEG=-0.1)
      DATA LDGZZM/.FALSE./,LDGZTS/.FALSE./
      DATA VGZNS/R999/,VGZNSA/2*R999/
      DATA NGZ/0/,NGZVEC/JPSGMX*0/,NGZPR/JPSGMX*0/,NGZPL/JPSGMX*0/
      DATA CINTGZ/JPSGMX*RNEG/

      IF(.NOT.(LDSG.AND.LDGZ)) THEN
**       GZ diagnostics not selected - print warning
         NWARN=NWARN+1
         WRITE(NCERR,6000)
6000     FORMAT(1X,'WARNING: - GZ DIAGNOSTICS NOT SELECTED:',
     -         ' ALTHOUGH USER HAS SUPPLIED GZ INPUT DATA')
      ENDIF

**    Read and interpret records until '$ENDGZ' found
300   READ(10,5000,END=100,ERR=110)YOCREC
5000  FORMAT(A80)
      CALL SEARCH(YOCREC,'$ENDGZ',LOFIND)
      IF(LOFIND) THEN
         WRITE(NCERR,6010)
6010     FORMAT(1X,'USER-SUPPLIED GZ SELECTION DATA PROCESSED')
         RETURN
      ENDIF

      CALL GZEXTR(YOCREC)
      GO TO 300

100   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6020)
6020  FORMAT(1X,'GZPROC: EOF WHEN SEARCHING FOR GZ DATA')
      CALL ERRSTP

110   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6030)
6030  FORMAT(1X,'GZPROC: ERROR WHEN SEARCHING FOR GZ DATA')
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE CHKGM
**    Function - to check GM data input
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /SGDIAG/NVSGHR,VSGSFC,/GMDIAG/all elements,
**          /DIAGTP/LDSG,/COMDAT/YTYPSF,LASCII,LWRITE,/ALOWSG/NSGFLD,
**          /ERRMSG/NCERR
**    Com changed - /GMDIAG/NGMPR,NGMPL,NGM,/SGDIAG/LDGM,/ERRMSG/
**          NFATAL,NWARN
**    Params used - /PARAM1/JPNL,/PARAMS/JPSGMX
**    Called by - DIAGCK
**    Calls - LATCHK
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /ALOWSG/NSGFLD(JPSGMX)
      LOGICAL LWRITE,LASCII,LSCRAT,LUTF14,LROGR,LMASSW,LAVMSK,
     -      LAVSIG
      CHARACTER*2 YTYPSF
      CHARACTER*6 YDUM6
      CHARACTER*8 YNAME(2)
      COMMON /COMDAT/BEGDAY,ENDDAY,LWRITE,LASCII,LSCRAT,
     -      LUTF14,LROGR,NSIGFG,NFREQD,NFREQA,NFREQP,RNTAPE,
     -      NASCII,LMASSW,LAVMSK,LAVSIG,YNAME,YTYPSF,YDUM6
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDGMMM,LDGMTS
      COMMON /GMDIAG/LDGMMM,VGMEW,LDGMTS,VGMEWA(2),NGMPR(JPSGMX),
     -      NGMPL(JPSGMX),NGM,NGMVEC(JPSGMX),CINTGM(JPSGMX)
      LOGICAL LDGH,LDGZ,LDGM,LDGP,LDG3,LDSGTD,LDSGTS,LDSGIN
      COMMON /SGDIAG/LDGH,LDGZ,LDGM,LDGP,LDG3,LDSGTD,LDSGTS,LDSGIN,
     -      NCSG,NVSGHR(JPNL),VSGSFC(JPNL)
      LOGICAL LOERR,LOVALU

      LOERR=.FALSE.

**    SG/GM diagnostics require that LDSG and LDGM are set to .TRUE.
      IF((.NOT.LDSG).OR.(.NOT.LDGM)) THEN
         IF((LDGMMM).OR.(LDGMTS).OR.(VGMEW.GT.-90.0).OR.
     -         (VGMEWA(1).GT.-90.0).OR.(VGMEWA(2).GT.-90.0)) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6000)
6000        FORMAT(1X,'CHKGM: WARNING - GM DIAGNOSTICS REQUESTED BUT',
     -            ' BOTH LDSG/LDGM FLAGS NOT SET')
            RETURN
         ENDIF
      ENDIF

**    Check that printing has been requested - if not, ensure
**          NGMPR=0 for all elements
      IF(.NOT.LWRITE) THEN
         IF(NGMPR(1).NE.0) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6005)
6005        FORMAT(1X,'CHKGM: NGMPR VALUES CHANGED - NO PRINTING')
            DO 1000 K=1,JPSGMX
1000        NGMPR(K)=0
**          Reset value of NGM
            NGM=0
            DO 1001 K=1,JPSGMX
            IF(NGMPL(K).GT.0) NGM=NGM+1
1001        CONTINUE
         ENDIF
      ENDIF

**    Check that UTFs are requested - if not, ensure NGMPL=0
**          for all elements
      IF(.NOT.LASCII) THEN
         IF(NGMPL(1).NE.0) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6010)
6010        FORMAT(1X,'CHKGM: NGMPL VALUES CHANGED - NO PLOTTING')
            DO 1100 K=1,JPSGMX
1100        NGMPL(K)=0
**          Reset value of NGM
            NGM=0
            DO 1101 K=1,JPSGMX
            IF(NGMPR(K).GT.0) NGM=NGM+1
1101        CONTINUE
         ENDIF
      ENDIF

**    Check valid code numbers in NGMPR
      IF(NGMPR(1).NE.0) THEN
         DO 2000 K=1,JPSGMX
         LOVALU=.FALSE.
         DO 2100 J=1,JPSGMX
         IF((NGMPR(K).EQ.NSGFLD(J)).OR.(NGMPR(K).EQ.0)) LOVALU=.TRUE.
2100     CONTINUE
         IF(.NOT.LOVALU) THEN
            LOERR=.TRUE.
            NFATAL=NFATAL+1
            WRITE(NCERR,6020) NGMPR(K)
6020        FORMAT(1X,'CHKGM: ERROR IN NGMPR VALUE ',I4)
         ENDIF
2000     CONTINUE
      ENDIF

**    Check valid code numbers in NGMPL
      IF(NGMPL(1).NE.0) THEN
         DO 3000 K=1,JPSGMX
         LOVALU=.FALSE.
         DO 3100 J=1,JPSGMX
         IF((NGMPL(K).EQ.NSGFLD(J)).OR.(NGMPL(K).EQ.0)) LOVALU=.TRUE.
3100     CONTINUE
         IF(.NOT.LOVALU) THEN
            LOERR=.TRUE.
            NFATAL=NFATAL+1
            WRITE(NCERR,6030) NGMPL(K)
6030        FORMAT(1X,'CHKGM: ERROR IN NGMPL VALUE ',I4)
         ENDIF
3000     CONTINUE
      ENDIF

**    Switch off diagnostics if NGM is zero
      IF(NGM.EQ.0.AND.LDGM) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6035)
6035     FORMAT(1X,'CHKGM: GM DIAGNOSTICS SWITCHED OFF, NGM=0')
         LDGM=.FALSE.
         RETURN
      ENDIF
      IF(.NOT.LDGM) RETURN

**    Check that NGMVEC values correspond to NGMPL values
      IF(NGMVEC(1).NE.0) THEN
         DO 3500 K=1,JPSGMX
         LOVALU=.FALSE.
         DO 3600 J=1,JPSGMX
         IF((NGMVEC(K).EQ.NGMPL(J)).OR.(NGMVEC(K).EQ.0)) LOVALU=.TRUE.
3600     CONTINUE
         IF(.NOT.LOVALU) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6050)
6050        FORMAT(1X,'CHKGM: NGMVEC CORRESPONDS TO NON-PLOTTED FIELD')
         ENDIF
3500     CONTINUE
      ENDIF

**    Check that NGM is non-zero, and less than JPSGMX
      IF(NGM.LE.0.OR.(NGM.GT.JPSGMX)) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6040)
6040     FORMAT(1X,'CHKGM : INVALID VALUE FOR NGM')
         LOERR=.TRUE.
      ENDIF

**    Check that NVSGHR or VSGSFC have been set if LDGMTS is .TRUE.
      IF(LDGMTS.AND.(YTYPSF.EQ.'ET').AND.(NVSGHR(1).EQ.0)) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6060)
6060     FORMAT(1X,'CHKGM: ERROR - MERIDIONAL TIMESERIES O/P REQUIRES'
     -         ' NVSGHR TO BE SPECIFIED FOR O/P ON SIGMA SURFACES')
         LOERR=.TRUE.
      ENDIF
      ZDELTA=1.0E-10
      IF(LDGMTS.AND.(YTYPSF.NE.'ET').AND.(VSGSFC(1).LE.ZDELTA)) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6070)
6070     FORMAT(1X,'CHKGM: ERROR - MERIDIONAL TIMESERIES O/P REQUIRES'
     -         ' VSGSFC TO BE SPECIFIED FOR O/P ON NON-SIGMA SURFACES')
         LOERR=.TRUE.
      ENDIF

**    Check validity of VGMEW AND VGMEWA values
      CALL LATCHK(VGMEW,1,-999.0,'(SG) GM VGMEW',LOERR)
      CALL LATCHK(VGMEWA,2,-999.0,'(SG) GM VGMEWA',LOERR)

CC    IF(LOERR) STOP
      RETURN
      END
      SUBROUTINE GMEXTR(YPCREC)
**    Function - to extract information from GM data and update
**          appropriate common blocks
**    Args in -
**               YPCREC  - character*80 variable containing data
**    Args out - none
**    Args for work - none
**    Com used - /ALOWSG/NSGFLD,/ERRMSG/NCERR
**    Com changed - /GMDIAG/all elements,/ERRMSG/NFATAL
**    Params used - /PARAMS/JPSGMX
**    Called by - GMPROC
**    Calls - SEARCH,LPROC,RMULTP,LASTCH,RTRANS,IMULTP,INTCPY,ITRANS,
**          ERRSTP
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /ALOWSG/NSGFLD(JPSGMX)
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDGMMM,LDGMTS
      COMMON /GMDIAG/LDGMMM,VGMEW,LDGMTS,VGMEWA(2),NGMPR(JPSGMX),
     -      NGMPL(JPSGMX),NGM,NGMVEC(JPSGMX),CINTGM(JPSGMX)
      LOGICAL LOFIND,LOALL
      CHARACTER*1 YOCOM,YOEQL
      CHARACTER*2 YOKW2
      CHARACTER*4 YOKW4
      CHARACTER*7 YOKW7
      CHARACTER*9 YOKW9
      CHARACTER*10 YOKW10
      CHARACTER*14 YOKW14
      CHARACTER*80 YPCREC

      YOEQL='='
      YOCOM=','
**    Search for the keyword in YPCREC and then determine the field
**          value
      LOFIND=.FALSE.

      YOKW9='MERIDMEAN'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDGMMM)
         RETURN
      ENDIF

**    Else
      YOKW2='CI'
      CALL SEARCH(YPCREC,YOKW2,LOFIND)
      IF(LOFIND) THEN
         CALL RMULTP(YPCREC,YOEQL,YOCOM,CINTGM,JPSGMX)
         RETURN
      ENDIF


**    Else
      YOKW9='TIMESLICE'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDGMTS)
         RETURN
      ENDIF

**    Else
      YOKW9='WEAVERAGE'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
         CALL RMULTP(YPCREC,YOEQL,YOCOM,VGMEWA,2)
         RETURN
      ENDIF

**    Else
      YOKW7='WESLICE'
      CALL SEARCH(YPCREC,YOKW7,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
         IF(IERR.NE.0) GO TO 1000
         CALL RTRANS(YPCREC,IPOSN,IPOSX,VGMEW,12)
         RETURN
      ENDIF

**    Else
      YOKW10='PRINTFIELD'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            CALL INTCPY(NSGFLD,JPSGMX,NGMPR)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NGMPR,JPSGMX)
         RETURN
      ENDIF

**    Else
      YOKW9='PLOTFIELD'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            CALL INTCPY(NSGFLD,JPSGMX,NGMPL)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NGMPL,JPSGMX)
         RETURN
      ENDIF

**    Else
      YOKW14='NUMBEROFFIELDS'
      CALL SEARCH(YPCREC,YOKW14,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
**       Note that IPOSN and IPOSX can take the same value
         IF(IERR.EQ.998) GO TO 1000
         CALL ITRANS(YPCREC,IPOSN,IPOSX,NGM,12)
         RETURN
      ENDIF

**    Else
      YOKW10='WINDVECTOR'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            CALL INTCPY(NSGFLD,JPSGMX,NGMVEC)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NGMVEC,JPSGMX)
         RETURN
      ENDIF

**    Else
**    Keyword not located
      NFATAL=NFATAL+1
      WRITE(NCERR,6030) YPCREC
6030  FORMAT(1X,'KEYWORD NOT LOCATED FOR PHRASE :'/A)
      CALL ERRSTP

1000  CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6000) YPCREC
6000  FORMAT(1X,'GMEXTR: ERROR IN GM DATA ENTRY',/A)
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE GMPROC
**    Function - to read and process the GM diagnostic data
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /SGDIAG/LDGM,/DIAGTP/LDSG,/ERRMSG/NCERR
**    Com changed - /GMDIAG/all elements,/ERRMSG/NWARN,NFATAL
**    Called by - DIAGRD
**    Calls - SEARCH,GMEXTR,ERRSTP
**    Files read - 10
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDGMMM,LDGMTS
      COMMON /GMDIAG/LDGMMM,VGMEW,LDGMTS,VGMEWA(2),NGMPR(JPSGMX),
     -      NGMPL(JPSGMX),NGM,NGMVEC(JPSGMX),CINTGM(JPSGMX)
      LOGICAL LDGH,LDGZ,LDGM,LDGP,LDG3,LDSGTD,LDSGTS,LDSGIN
      COMMON /SGDIAG/LDGH,LDGZ,LDGM,LDGP,LDG3,LDSGTD,LDSGTS,LDSGIN,
     -      NCSG,NVSGHR(JPNL),VSGSFC(JPNL)
      CHARACTER*80 YOCREC
      LOGICAL LOFIND
**    Specify the defaults
      PARAMETER(R999=-999.0,RNEG=-0.1)
      DATA LDGMMM/.FALSE./,LDGMTS/.FALSE./
      DATA VGMEW/R999/,VGMEWA/2*R999/
      DATA NGM/0/,NGMVEC/JPSGMX*0/,NGMPR/JPSGMX*0/,NGMPL/JPSGMX*0/
      DATA CINTGM/JPSGMX*RNEG/

      IF(.NOT.(LDSG.AND.LDGM)) THEN
**       GM diagnostics not selected - print warning
         NWARN=NWARN+1
         WRITE(NCERR,6000)
6000     FORMAT(1X,'WARNING: - GM DIAGNOSTICS NOT SELECTED:',
     -         ' ALTHOUGH USER HAS SUPPLIED GM INPUT DATA')
      ENDIF

**    Read and interpret records until '$ENDGM' found
300   READ(10,5000,END=100,ERR=110)YOCREC
5000  FORMAT(A80)
      CALL SEARCH(YOCREC,'$ENDGM',LOFIND)
      IF(LOFIND) THEN
         WRITE(NCERR,6010)
6010     FORMAT(1X,'USER-SUPPLIED GM SELECTION DATA PROCESSED')
         RETURN
      ENDIF

      CALL GMEXTR(YOCREC)
      GO TO 300

100   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6020)
6020  FORMAT(1X,'GMPROC: EOF WHEN SEARCHING FOR GM DATA')
      CALL ERRSTP

110   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6030)
6030  FORMAT(1X,'GMPROC: ERROR WHEN SEARCHING FOR GM DATA')
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE CHKGP
**    Function - to check GP data input
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /GPDIAG/all elements,/DIAGTP/LDSG,
**          /COMDAT/LWRITE,LASCII,YTYPSF,/ALOWSG/,NSGFLD,/ERRMSG/NCERR
**    Com changed - /GPDIAG/NGPPR,NGPPL,NGP,LDGPCT,/SGDIAG/LDGP,
**          /ERRMSG/NFATAL/NWARN
**    Params used - /PARAM1/JPNL,/PARAMS/JPSGMX,JPGPPF
**    Called by - DIAGCK
**    Calls - LATCHK,LNGCHK
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /ALOWSG/NSGFLD(JPSGMX)
      LOGICAL LWRITE,LASCII,LSCRAT,LUTF14,LROGR,LMASSW,LAVMSK,
     -      LAVSIG
      CHARACTER*2 YTYPSF
      CHARACTER*6 YDUM6
      CHARACTER*8 YNAME(2)
      COMMON /COMDAT/BEGDAY,ENDDAY,LWRITE,LASCII,LSCRAT,
     -      LUTF14,LROGR,NSIGFG,NFREQD,NFREQA,NFREQP,RNTAPE,
     -      NASCII,LMASSW,LAVMSK,LAVSIG,YNAME,YTYPSF,YDUM6
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDGPCT,LDGPCP
      COMMON /GPDIAG/LDGPCT,NGPPF,VGPPF(2,JPGPPF),NGPPR(JPSGMX),
     -      NGPPL(JPSGMX),NGP,CINTGP(JPSGMX),LDGPCP
      LOGICAL LDGH,LDGZ,LDGM,LDGP,LDG3,LDSGTD,LDSGTS,LDSGIN
      COMMON /SGDIAG/LDGH,LDGZ,LDGM,LDGP,LDG3,LDSGTD,LDSGTS,LDSGIN,
     -      NCSG,NVSGHR(JPNL),VSGSFC(JPNL)
      LOGICAL LOERR,LOVALU

      LOERR=.FALSE.

**    SG/GP diagnostics require that LDSG and LDGP are set to .TRUE.
      IF((.NOT.LDSG).OR.(.NOT.LDGP)) THEN
         IF((LDGPCT).OR.(VGPPF(1,1).GT.-90.0).OR.(LDGPCP)) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6000)
6000        FORMAT(1X,'CHKGP: WARNING - GP DIAGNOSTICS REQUESTED BUT',
     -            ' BOTH LDSG/LDGP FLAGS NOT SET')
            RETURN
         ENDIF
      ENDIF

**    LDGPCT only valid if output is on model surfaces
      IF(YTYPSF.NE.'ET'.AND.LDGPCT) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6005)
6005     FORMAT(1X,'CHKGP: ERROR - GP COLUMN TOTALS CANNOT BE FOUND',
     -         'WHEN OUTPUT NOT ON ET SURFACES')
         LDGPCT=.FALSE.
         LOERR=.TRUE.
      ENDIF

**    Check that printing has been requested - if not, ensure
**          NGPPR=0 for all elements
      IF(.NOT.LWRITE) THEN
         IF(NGPPR(1).NE.0) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6060)
6060        FORMAT(1X,'CHKGP: NGPPR VALUES CHANGED - NO PRINTING')
            DO 1000 K=1,JPSGMX
1000        NGPPR(K)=0
**          Reset value of NGP
            NGP=0
            DO 1001 K=1,JPSGMX
            IF(NGPPL(K).GT.0) NGP=NGP+1
1001        CONTINUE
         ENDIF
      ENDIF

**    Check that UTFs are requested - if not, ensure NGPPL=0
**          for all elements
      IF(.NOT.LASCII) THEN
         IF(NGPPL(1).NE.0) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6010)
6010        FORMAT(1X,'CHKGP: NGPPL VALUES CHANGED - NO PLOTTING')
            DO 1100 K=1,JPSGMX
1100        NGPPL(K)=0
**          Reset value of NGP
            NGP=0
            DO 1101 K=1,JPSGMX
            IF(NGPPR(K).GT.0) NGP=NGP+1
1101        CONTINUE
         ENDIF
      ENDIF

**    Check valid code numbers in NGPPR
      IF(NGPPR(1).NE.0) THEN
         DO 2000 K=1,JPSGMX
         LOVALU=.FALSE.
         DO 2100 J=1,JPSGMX
         IF((NGPPR(K).EQ.NSGFLD(J)).OR.(NGPPR(K).EQ.0)) LOVALU=.TRUE.
2100     CONTINUE
         IF(.NOT.LOVALU) THEN
            LOERR=.TRUE.
            NFATAL=NFATAL+1
            WRITE(NCERR,6020) NGPPR(K)
6020        FORMAT(1X,'CHKGP: ERROR IN NGPPR VALUE ',I4)
         ENDIF
2000     CONTINUE
      ENDIF

**    Check valid code numbers in NGPPL
      IF(NGPPL(1).NE.0) THEN
         DO 3000 K=1,JPSGMX
         LOVALU=.FALSE.
         DO 3100 J=1,JPSGMX
         IF((NGPPL(K).EQ.NSGFLD(J)).OR.(NGPPL(K).EQ.0)) LOVALU=.TRUE.
3100     CONTINUE
         IF(.NOT.LOVALU) THEN
            LOERR=.TRUE.
            NFATAL=NFATAL+1
            WRITE(NCERR,6030) NGPPL(K)
6030        FORMAT(1X,'CHKGP: ERROR IN NGPPL VALUE ',I4)
         ENDIF
3000     CONTINUE
      ENDIF

**    Switch off diagnostics if NGP is zero
      IF(NGP.EQ.0.AND.LDGP) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6035)
6035     FORMAT(1X,'CHKGP: GP DIAGNOSTICS SWITCHED OFF, NGP=0')
         LDGP=.FALSE.
         RETURN
      ENDIF
      IF(.NOT.LDGP) RETURN

**    Check that NGP is non-zero, and less than JPSGMX
      IF(NGP.LE.0.OR.(NGP.GT.JPSGMX)) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6040)
6040     FORMAT(1X,'CHKGP : INVALID VALUE FOR NGP')
         LOERR=.TRUE.
      ENDIF

**    Check validity of profile coordinates (IPROFL is max no. of
**    profiles allowed)
      IF(NGPPF.LT.0.OR.NGPPF.GT.JPGPPF) THEN
         NFATAL=NFATAL+1
         LOERR=.TRUE.
         WRITE(NCERR,6050)
6050     FORMAT(1X,'CHKGP: ERROR - INVALID NGPPF')
      ENDIF

      IF(NGPPF.GT.0) THEN
         DO 100 JL=1,NGPPF
         CALL LATCHK(VGPPF(1,JL),1,-999.0,'(SG) GP VGPPF',LOERR)
         CALL LNGCHK(VGPPF(2,JL),1,-999.0,'(SG) GP VGPPF',LOERR)
         IF((VGPPF(1,JL).LT.-90.0).OR.(VGPPF(2,JL).LT.-90.0)) THEN
            NFATAL=NFATAL+1
            WRITE(NCERR,6080)
6080        FORMAT(1X,'CHKGP: ERROR - INVALID PROFILE COORDINATES')
            LOERR=.TRUE.
         ENDIF
100      CONTINUE
      ENDIF

CC    IF(LOERR) STOP
      RETURN
      END
      SUBROUTINE GPEXTR(YPCREC)
**    Function - to extract information from GP data and update
**          appropriate common blocks
**    Args in -
**               YPCREC  - character*80 variable containing data
**    Args out - none
**    Args for work - none
**    Com used - /ALOWSG/NSGFLD,/ERRMSG/NCERR
**    Com changed - /GPDIAG/all elements,/ERRMSG/NFATAL
**    Params used - /PARAMS/JPGPPF
**    Called by - GPPROC
**    Calls - SEARCH,LPROC,RMULTP,LASTCH,ITRANS,IMULTP,INTCPY,ERRSTP
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      COMMON /ALOWSG/NSGFLD(JPSGMX)
      LOGICAL LDGPCT,LDGPCP
      COMMON /GPDIAG/LDGPCT,NGPPF,VGPPF(2,JPGPPF),NGPPR(JPSGMX),
     -      NGPPL(JPSGMX),NGP,CINTGP(JPSGMX),LDGPCP
      LOGICAL LOFIND,LOALL
      CHARACTER*1 YOCOM,YOEQL
      CHARACTER*2 YOKW2
      CHARACTER*4 YOKW4
      CHARACTER*8 YOKW8
      CHARACTER*9 YOKW9
      CHARACTER*10 YOKW10
      CHARACTER*13 YOKW13
      CHARACTER*14 YOKW14
      CHARACTER*80 YPCREC

      YOEQL='='
      YOCOM=','
**    Search for the keyword in YPCREC and then determine the field
**          value
      LOFIND=.FALSE.

      YOKW8='COLTOTAL'
      CALL SEARCH(YPCREC,YOKW8,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDGPCT)
         RETURN
      ENDIF

**    Else
      YOKW10='COLPROFILE'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDGPCP)
         RETURN
      ENDIF

**    Else
      YOKW2='CI'
      CALL SEARCH(YPCREC,YOKW2,LOFIND)
      IF(LOFIND) THEN
         CALL RMULTP(YPCREC,YOEQL,YOCOM,CINTGP,JPSGMX)
         RETURN
      ENDIF


**    Else
      YOKW14='NUMBEROFCOORDS'
      CALL SEARCH(YPCREC,YOKW14,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
**       Note that IPOSN and IPOSX can take the same value
         IF(IERR.EQ.998) GO TO 1000
         CALL ITRANS(YPCREC,IPOSN,IPOSX,NGPPF,12)
         RETURN
      ENDIF

**    Else
      YOKW13='PROFILECOORDS'
      CALL SEARCH(YPCREC,YOKW13,LOFIND)
      IF(LOFIND) THEN
         CALL RMULTP(YPCREC,YOEQL,YOCOM,VGPPF,2*JPGPPF)
         RETURN
      ENDIF

**    Else
      YOKW10='PRINTFIELD'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            CALL INTCPY(NSGFLD,JPSGMX,NGPPR)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NGPPR,JPSGMX)
         RETURN
      ENDIF

**    Else
      YOKW9='PLOTFIELD'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            CALL INTCPY(NSGFLD,JPSGMX,NGPPL)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NGPPL,JPSGMX)
         RETURN
      ENDIF

**    Else
      YOKW14='NUMBEROFFIELDS'
      CALL SEARCH(YPCREC,YOKW14,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
**       Note that IPOSN and IPOSX can take the same value
         IF(IERR.EQ.998) GO TO 1000
         CALL ITRANS(YPCREC,IPOSN,IPOSX,NGP,12)
         RETURN
      ENDIF

**    Else
**    Keyword not located
      NFATAL=NFATAL+1
      WRITE(NCERR,6030) YPCREC
6030  FORMAT(1X,'KEYWORD NOT LOCATED FOR PHRASE :'/A)
      CALL ERRSTP

1000  CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6000) YPCREC
6000  FORMAT(1X,'GPEXTR: ERROR IN GP DATA ENTRY',/A)
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE GPPROC
**    Function - to read and process the GP diagnostic data
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /SGDIAG/LDGP,/DIAGTP/LDSG,/ERRMSG/NCERR
**    Com changed - /GPDIAG/all elements,/ERRMSG/NFATAL,NWARN
**    Params - JPGP2,RNEG,/PARAMS/JPGPPF
**    Called by - DIAGRD
**    Calls - SEARCH,GPEXTR,ERRSTP
**    Files read - 10
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDGPCT,LDGPCP
      COMMON /GPDIAG/LDGPCT,NGPPF,VGPPF(2,JPGPPF),NGPPR(JPSGMX),
     -      NGPPL(JPSGMX),NGP,CINTGP(JPSGMX),LDGPCP
      LOGICAL LDGH,LDGZ,LDGM,LDGP,LDG3,LDSGTD,LDSGTS,LDSGIN
      COMMON /SGDIAG/LDGH,LDGZ,LDGM,LDGP,LDG3,LDSGTD,LDSGTS,LDSGIN,
     -      NCSG,NVSGHR(JPNL),VSGSFC(JPNL)
      CHARACTER*80 YOCREC
      LOGICAL LOFIND
**    Specify the defaults
      PARAMETER(R999=-999.0,RNEG=-0.1,JPGP2=2*JPGPPF)
      DATA LDGPCT/.FALSE./,LDGPCP/.FALSE./
      DATA NGPPF/0/,VGPPF/JPGP2*R999/
      DATA NGP/0/,NGPPR/JPSGMX*0/,NGPPL/JPSGMX*0/
      DATA CINTGP/JPSGMX*RNEG/

      IF(.NOT.(LDSG.AND.LDGP)) THEN
**       GP diagnostics not selected - print warning
         NWARN=NWARN+1
         WRITE(NCERR,6000)
6000     FORMAT(1X,'WARNING: - GP DIAGNOSTICS NOT SELECTED:',
     -         ' ALTHOUGH USER HAS SUPPLIED GP INPUT DATA')
      ENDIF

**    Read and interpret records until '$ENDGP' found
300   READ(10,5000,END=100,ERR=110)YOCREC
5000  FORMAT(A80)
      CALL SEARCH(YOCREC,'$ENDGP',LOFIND)
      IF(LOFIND) THEN
         WRITE(NCERR,6010)
6010     FORMAT(1X,'USER-SUPPLIED GP SELECTION DATA PROCESSED')
         RETURN
      ENDIF

      CALL GPEXTR(YOCREC)
      GO TO 300

100   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6020)
6020  FORMAT(1X,'GPPROC: EOF WHEN SEARCHING FOR GP DATA')
      CALL ERRSTP

110   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6030)
6030  FORMAT(1X,'GPPROC: ERROR WHEN SEARCHING FOR GP DATA')
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE CHKG3
**    Function - to check G3 data input
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /SGDIAG/LDG3,/G3DIAG/NCG3,/DIAGTP/LDSG,/COMDAT/
**          LASCII,/ALOWSG/NSGFLD,/ERRMSG/NCERR
**    Com changed - /G3DIAG/NG3,NG3PL,LDG3,/ERRMSG/NFATAL,NWARN
**    Com changed - /G3DIAG/NG3,NG3PL,LDG3
**    Params used - /PARAM1/JPNL,/PARAMS/JPSGMX
**    Called by - DIAGCK
**    Calls - none
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /ALOWSG/NSGFLD(JPSGMX)
      LOGICAL LWRITE,LASCII,LSCRAT,LUTF14,LROGR,LMASSW,LAVMSK,
     -      LAVSIG
      CHARACTER*2 YTYPSF
      CHARACTER*6 YDUM6
      CHARACTER*8 YNAME(2)
      COMMON /COMDAT/BEGDAY,ENDDAY,LWRITE,LASCII,LSCRAT,
     -      LUTF14,LROGR,NSIGFG,NFREQD,NFREQA,NFREQP,RNTAPE,
     -      NASCII,LMASSW,LAVMSK,LAVSIG,YNAME,YTYPSF,YDUM6
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      COMMON /G3DIAG/NCG3,NG3PL(JPSGMX),NG3
      LOGICAL LDGH,LDGZ,LDGM,LDGP,LDG3,LDSGTD,LDSGTS,LDSGIN
      COMMON /SGDIAG/LDGH,LDGZ,LDGM,LDGP,LDG3,LDSGTD,LDSGTS,LDSGIN,
     -      NCSG,NVSGHR(JPNL),VSGSFC(JPNL)
      LOGICAL LOERR,LOVALU

      LOERR=.FALSE.

**    Check that UTFs are requested - if not, ensure NG3PL=0
**          for all elements
      IF(.NOT.LASCII) THEN
         IF(NG3PL(1).NE.0) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6010)
6010        FORMAT(1X,'CHKG3: NG3PL VALUES CHANGED - NO O/P')
            DO 1100 K=1,JPSGMX
1100        NG3PL(K)=0
**          Reset value of NG3
            NG3=0
         ENDIF
      ENDIF

**    Check valid code numbers in NG3PL
      IF(NG3PL(1).NE.0) THEN
         DO 3000 K=1,JPSGMX
         LOVALU=.FALSE.
         DO 3100 J=1,JPSGMX
         IF((NG3PL(K).EQ.NSGFLD(J)).OR.(NG3PL(K).EQ.0)) LOVALU=.TRUE.
3100     CONTINUE
         IF(.NOT.LOVALU) THEN
            LOERR=.TRUE.
            NFATAL=NFATAL+1
            WRITE(NCERR,6030) NG3PL(K)
6030        FORMAT(1X,'CHKG3: ERROR IN NG3PL VALUE ',I4)
         ENDIF
3000     CONTINUE
      ENDIF

**    Switch off diagnostics if NG3 is zero
      IF(NG3.EQ.0.AND.LDG3) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6035)
6035     FORMAT(1X,'CHKG3: G3 DIAGNOSTICS SWITCHED OFF, NG3=0')
         LDG3=.FALSE.
         RETURN
      ENDIF
      IF(.NOT.LDG3) RETURN

**    Check that NG3 is non-zero, and less than JPSGMX
      IF(NG3.LE.0.OR.(NG3.GT.JPSGMX)) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6040)
6040     FORMAT(1X,'CHKG3 : INVALID VALUE FOR NG3')
         LOERR=.TRUE.
      ENDIF

**    Check channel for output of 3-D fields
      IF(LDG3.AND.NCG3.LE.0) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6050)
6050     FORMAT(1X,'CHKG3: ERROR - G3 DIAGNOSTICS REQUESTED BUT',
     -         ' NCG3 CHANNEL NUMBER INVALID')
         LOERR=.TRUE.
      ENDIF

CC    IF(LOERR) STOP
      RETURN
      END
      SUBROUTINE G3EXTR(YPCREC)
**    Function - to extract information from G3 data and update
**          appropriate common blocks
**    Args in -
**               YPCREC  - character*80 variable containing data
**    Args out - none
**    Args for work - none
**    Com used - /ALOWSG/NSGFLD,/ERRMSG/NCERR
**    Com changed - /G3DIAG/NCG3,/ERRMSG/NFATAL
**    Params used - /PARAMS/JPSGMX
**    Called by - G3PROC
**    Calls - SEARCH,LASTCH,ITRANS,INTCPY,IMULTP,ERRSTP
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /ALOWSG/NSGFLD(JPSGMX)
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      COMMON /G3DIAG/NCG3,NG3PL(JPSGMX),NG3
      LOGICAL LOFIND,LOALL
      CHARACTER*1 YOCOM,YOEQL
      CHARACTER*4 YOKW4
      CHARACTER*9 YOKW9
      CHARACTER*10 YOKW10
      CHARACTER*14 YOKW14
      CHARACTER*80 YPCREC

      YOEQL='='
      YOCOM=','
**    Search for the keyword in YPCREC and then determine the field
**          value
      LOFIND=.FALSE.

      YOKW10='O/PCHANNEL'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
**       Note that IPOSN and IPOSX can take the same value
         IF(IERR.EQ.998) GO TO 1000
         CALL ITRANS(YPCREC,IPOSN,IPOSX,NCG3,12)
         RETURN
      ENDIF

**    Else
      YOKW9='PLOTFIELD'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            CALL INTCPY(NSGFLD,JPSGMX,NG3PL)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NG3PL,JPSGMX)
         RETURN
      ENDIF

**    Else
      YOKW14='NUMBEROFFIELDS'
      CALL SEARCH(YPCREC,YOKW14,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
**       Note that IPOSN and IPOSX can take the same value
         IF(IERR.EQ.998) GO TO 1000
         CALL ITRANS(YPCREC,IPOSN,IPOSX,NG3,12)
         RETURN
      ENDIF

**    Else
**    Keyword not located
      NFATAL=NFATAL+1
      WRITE(NCERR,6030) YPCREC
6030  FORMAT(1X,'KEYWORD NOT LOCATED FOR PHRASE :'/A)
      CALL ERRSTP

1000  CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6000) YPCREC
6000  FORMAT(1X,'G3EXTR: ERROR IN G3 DATA ENTRY',/A)
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE G3PROC
**    Function - to read and process the G3 diagnostic data
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /SGDIAG/LDG3,/DIAGTP/LDSG,/ERRMSG/NCERR
**    Com changed - /G3DIAG/NCG3,/ERRMSG/NFATAL/NWARN
**    Called by - DIAGRD
**    Calls - SEARCH,G3EXTR,ERRSTP
**    Files read - 10
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      COMMON /G3DIAG/NCG3,NG3PL(JPSGMX),NG3
      LOGICAL LDGH,LDGZ,LDGM,LDGP,LDG3,LDSGTD,LDSGTS,LDSGIN
      COMMON /SGDIAG/LDGH,LDGZ,LDGM,LDGP,LDG3,LDSGTD,LDSGTS,LDSGIN,
     -      NCSG,NVSGHR(JPNL),VSGSFC(JPNL)
      CHARACTER*80 YOCREC
      LOGICAL LOFIND
**    Specify the defaults
      DATA NCG3/1/
      DATA NG3/0/,NG3PL/JPSGMX*0/

      IF(.NOT.(LDSG.AND.LDG3)) THEN
**       G3 diagnostics not selected - print warning
         NWARN=NWARN+1
         WRITE(NCERR,6000)
6000     FORMAT(1X,'WARNING: - G3 DIAGNOSTICS NOT SELECTED:',
     -         ' ALTHOUGH USER HAS SUPPLIED G3 INPUT DATA')
      ENDIF

**    Read and interpret records until '$ENDG3' found
300   READ(10,5000,END=100,ERR=110)YOCREC
5000  FORMAT(A80)
      CALL SEARCH(YOCREC,'$ENDG3',LOFIND)
      IF(LOFIND) THEN
         WRITE(NCERR,6010)
6010     FORMAT(1X,'USER-SUPPLIED G3 SELECTION DATA PROCESSED')
         RETURN
      ENDIF

      CALL G3EXTR(YOCREC)
      GO TO 300

100   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6020)
6020  FORMAT(1X,'G3PROC: EOF WHEN SEARCHING FOR G3 DATA')
      CALL ERRSTP

110   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6030)
6030  FORMAT(1X,'G3PROC: ERROR WHEN SEARCHING FOR G3 DATA')
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE CHKTR
**    Function - to check TR data input
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /COMDAT/LWRITE,LASCII,BEGDAY,ENDDAY,
**          /TRDIAG/NVTRHR,VTRSFC,/MODELC/LGCMTR,NL,
**          /COMDAT/YTYPSF,/LVDIAG/OUTLEV,/ERRMSG/NCERR
**    Com changed - /DIAGTP/LDTR,/TRDIAG/LDTRTS,LDTRTD,/ERRMSG/
**          NFATAL,NWARN
**    Params used - /PARAM1/JPNL,/PARAMS/JPTRMX
**    Called by - DIAGCK
**    Calls - none
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      LOGICAL LWRITE,LASCII,LSCRAT,LUTF14,LROGR,LMASSW,LAVMSK,
     -      LAVSIG
      CHARACTER*2 YTYPSF
      CHARACTER*6 YDUM6
      CHARACTER*8 YNAME(2)
      COMMON /COMDAT/BEGDAY,ENDDAY,LWRITE,LASCII,LSCRAT,
     -      LUTF14,LROGR,NSIGFG,NFREQD,NFREQA,NFREQP,RNTAPE,
     -      NASCII,LMASSW,LAVMSK,LAVSIG,YNAME,YTYPSF,YDUM6
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      COMMON /LVDIAG/OUTLEV(JPNL)
      LOGICAL LGCMTR,LHISPH,LHISXP,LDRY,LDIABH,LBL,LVD
      COMMON /MODELC/TSPD,NYEAR,NDAY,NMIN,LGCMTR,NL,TMEAN(JPNL),
     -      LHISPH,LHISXP,LDRY,LDIABH,BEGDYP,LBL,LVD,AKVV,CD
      LOGICAL LDTH,LDTZ,LDTM,LDTP,LDT3,LDTRTD,LDTRTS,LDTRIN
      COMMON /TRDIAG/LDTH,LDTZ,LDTM,LDTP,LDT3,LDTRTD,LDTRTS,LDTRIN,
     -      NCTR,NVTRHR(JPNL),VTRSFC(JPNL)
      LOGICAL LOERR,LOVALU

      LOERR=.FALSE.


**    Tracer options only valid if the tracer update was included in the
**    GCM run that created the input data to this program.
      IF((.NOT.LGCMTR).AND.(LDTR)) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6034)
6034     FORMAT(1X,'CHKTR: TR DIAGNOSTICS SWITCHED OFF - NO TRACER ',
     -         'UPDATE IN GCM RUN')
         LDTR=.FALSE.
         RETURN
      ENDIF

**    Switch off time-series and time-averaging calculations if
**    ENDDAY is not greater than BEGDAY
      IF(ABS(ENDDAY).LE.ABS(BEGDAY)) THEN
         IF(LDTRTS) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6100)
6100        FORMAT(1X,'CHKTR: TR TIME-SERIES SWITCHED OFF, SINCE ',
     -            'ENDDAY.LE.BEGDAY')
            LDTRTS=.FALSE.
         ENDIF
         IF(LDTRTD) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6110)
6110        FORMAT(1X,'CHKTR: TR TIME-AVERAGES (D) SWITCHED OFF,',
     -            ' SINCE ENDDAY.LE.BEGDAY')
            LDTRTD=.FALSE.
         ENDIF
      ENDIF

**    NVTRHR values only used when output is on model surfaces, and
**    VTRSFC values only used when output is on non-model surfaces
      IF((NVTRHR(1).NE.0).AND.(YTYPSF.NE.'ET')) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6060)
6060     FORMAT(1X,'CHKTR: NVTRHR VALUE(S) SUPPLIED, BUT NOT USED')
      ENDIF
      IF((NVTRHR(1).LE.0).AND.(YTYPSF.EQ.'ET')) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6065)
6065     FORMAT(1X,'CHKTR: NVTRHR VALUES NOT SUPPLIED,'
     -         ' OUTPUT MAY BE SUPPRESSED')
      ENDIF
      IF((VTRSFC(1).GT.0.0).AND.(YTYPSF.EQ.'ET')) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6070)
6070     FORMAT(1X,'CHKTR: VTRSFC VALUE(S) SUPPLIED, BUT NOT USED')
      ENDIF
      IF((VTRSFC(1).LE.0.0).AND.(YTYPSF.NE.'ET')) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6075)
6075     FORMAT(1X,'CHKTR: VTRSFC VALUES NOT SUPPLIED,'
     -         ' OUTPUT MAY BE SUPPRESSED')
      ENDIF

**    Check that VTRSFC values are a subset of OUTLEV values if they are
**    to be used. ZDELTA is a small parameter to allow two REAL values
**    to be compared
      ZDELTA=1.0E-10
      IF(YTYPSF.NE.'ET') THEN
         DO 4000 K=1,NL
         IF(VTRSFC(K).GT.0.0) THEN
            ZSFCP=VTRSFC(K)+ZDELTA
            ZSFCM=VTRSFC(K)-ZDELTA
            LOVALU=.FALSE.
            DO 4100 J=1,NL
            IF(ZSFCM.LE.OUTLEV(J).AND.ZSFCP.GE.OUTLEV(J)) LOVALU=.TRUE.
4100        CONTINUE
            IF(.NOT.LOVALU) THEN
               NFATAL=NFATAL+1
               LOERR=.TRUE.
               WRITE(NCERR,6080) VTRSFC(K)
6080           FORMAT(1X,'CHKTR: VTRSFC CORRESPONDS TO INVALID LEVEL ',
     -               F12.3)
            ENDIF
         ENDIF
4000     CONTINUE
      ENDIF

**    Check that NVTRHR values lie in the range 1 to NL
      IF(YTYPSF.EQ.'ET') THEN
         DO 5000 J=1,NL
         IF(NVTRHR(J).NE.0) THEN
            IF(NVTRHR(J).LT.1.OR.NVTRHR(J).GT.NL) THEN
               NFATAL=NFATAL+1
               WRITE(NCERR,6090)
6090           FORMAT(1X,'CHKTR: INVALID NVTRHR VALUE')
               LOERR=.TRUE.
            ENDIF
         ENDIF
5000     CONTINUE
      ENDIF

CC    IF(LOERR) STOP
      RETURN
      END
      SUBROUTINE TREXTR(YPCREC)
**    Function - to extract information from TR data and update
**          appropriate common blocks
**    Args in -
**               YPCREC  - character*80 variable containing data
**    Args out - none
**    Args for work - none
**    Com used - /ERRMSG/NCERR,/MODELC/NL
**    Com changed - /TRDIAG/all elements,/ERRMSG/NFATAL
**    Params used - /PARAM1/JPNL,/PARAMS/JPTRMX
**    Called by - TRPROC
**    Calls - SEARCH,LPROC,INTCPY,IMULTP,LASTCH,ITRANS,RMULTP,
**          ERRSTP
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LGCMTR,LHISPH,LHISXP,LDRY,LDIABH,LBL,LVD
      COMMON /MODELC/TSPD,NYEAR,NDAY,NMIN,LGCMTR,NL,TMEAN(JPNL),
     -      LHISPH,LHISXP,LDRY,LDIABH,BEGDYP,LBL,LVD,AKVV,CD
      LOGICAL LDTH,LDTZ,LDTM,LDTP,LDT3,LDTRTD,LDTRTS,LDTRIN
      COMMON /TRDIAG/LDTH,LDTZ,LDTM,LDTP,LDT3,LDTRTD,LDTRTS,LDTRIN,
     -      NCTR,NVTRHR(JPNL),VTRSFC(JPNL)
      LOGICAL LOFIND,LOALL
      DIMENSION INLFLD(JPNL)
      CHARACTER*1 YOCOM,YOEQL
      CHARACTER*4 YOKW4
      CHARACTER*5 YOKW5
      CHARACTER*7 YOKW7
      CHARACTER*8 YOKW8
      CHARACTER*9 YOKW9
      CHARACTER*10 YOKW10
      CHARACTER*13 YOKW13
      CHARACTER*80 YPCREC

      YOEQL='='
      YOCOM=','
**    Search for the keyword in YPCREC and then determine the field
**          value
      LOFIND=.FALSE.

      YOKW10='HORIZONTAL'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDTH)
         RETURN
      ENDIF

**    Else
      YOKW5='ZONAL'
      CALL SEARCH(YPCREC,YOKW5,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDTZ)
         RETURN
      ENDIF

**    Else
      YOKW10='MERIDIONAL'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDTM)
         RETURN
      ENDIF

**    Else
      YOKW7='PROFILE'
      CALL SEARCH(YPCREC,YOKW7,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDTP)
         RETURN
      ENDIF

**    Else
      YOKW8='3DOUTPUT'
      CALL SEARCH(YPCREC,YOKW8,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDT3)
         RETURN
      ENDIF

**    Else
      YOKW9='TAVERAGED'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDTRTD)
         RETURN
      ENDIF

**    Else
      YOKW13='INSTANTANEOUS'
      CALL SEARCH(YPCREC,YOKW13,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDTRIN)
         RETURN
      ENDIF

**    Else
      YOKW7='TSERIES'
      CALL SEARCH(YPCREC,YOKW7,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDTRTS)
         RETURN
      ENDIF

**    Else
      YOKW10='O/PCHANNEL'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
**       Note that IPOSN and IPOSX can take the same value
         IF(IERR.EQ.998) GO TO 1000
         CALL ITRANS(YPCREC,IPOSN,IPOSX,NCTR,12)
         RETURN
      ENDIF

**    Else
      YOKW8='TRLEVELS'
      CALL SEARCH(YPCREC,YOKW8,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            DO 3500 JL=1,NL
3500        INLFLD(JL)=JL
            CALL INTCPY(INLFLD,NL,NVTRHR)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NVTRHR,NL)
         RETURN
      ENDIF

**    Else
      YOKW10='TRSURFACES'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
      IF(LOFIND) THEN
         CALL RMULTP(YPCREC,YOEQL,YOCOM,VTRSFC,NL)
         RETURN
      ENDIF

**    Else
**    Keyword not located
      NFATAL=NFATAL+1
      WRITE(NCERR,6030) YPCREC
6030  FORMAT(1X,'KEYWORD NOT LOCATED FOR PHRASE :'/A)
      CALL ERRSTP

1000  CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6000) YPCREC
6000  FORMAT(1X,'TREXTR: ERROR IN TR DATA ENTRY',/A)
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE TRPROC
**    Function - to read and process the TR diagnostic data
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /DIAGTP/LDTR,/ERRMSG/NCERR
**    Com changed - /TRDIAG/all elements,/ALOWTR/NTRFLD,/ERRMSG/
**          NFATAL,NWARN
**    Called by - DIAGRD
**    Calls - SEARCH,TREXTR,ERRSTP
**    Files read - 10
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /ALOWTR/NTRFLD(JPTRMX)
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDTH,LDTZ,LDTM,LDTP,LDT3,LDTRTD,LDTRTS,LDTRIN
      COMMON /TRDIAG/LDTH,LDTZ,LDTM,LDTP,LDT3,LDTRTD,LDTRTS,LDTRIN,
     -      NCTR,NVTRHR(JPNL),VTRSFC(JPNL)
      CHARACTER*80 YOCREC
      LOGICAL LOFIND
**    Specify the defaults
      DATA NCTR/3/
      DATA LDTH/.FALSE./,LDTZ/.TRUE./,LDTM/.FALSE./
      DATA LDTP/.FALSE./,LDT3/.FALSE./
      DATA LDTRTS/.FALSE./,LDTRTD/.TRUE./,LDTRIN/.FALSE./
      DATA NVTRHR/JPNL*0/,VTRSFC/JPNL*0.0/

**    Define allowed field codes for TR
      DO 200 JF=1,JPTRMX
      NTRFLD(JF)=100+JF
200   CONTINUE

      IF(.NOT.LDTR) THEN
**       TR diagnostics not selected - print warning
         NWARN=NWARN+1
         WRITE(NCERR,6000)
6000     FORMAT(1X,'WARNING: - TR DIAGNOSTICS NOT SELECTED:',
     -         ' ALTHOUGH USER HAS SUPPLIED TR INPUT DATA')
      ENDIF

**    Read and interpret records until '$ENDTR' found
300   READ(10,5000,END=100,ERR=110)YOCREC
5000  FORMAT(A80)
      CALL SEARCH(YOCREC,'$ENDTR',LOFIND)
      IF(LOFIND) THEN
         WRITE(NCERR,6010)
6010     FORMAT(1X,'USER-SUPPLIED TR SELECTION DATA PROCESSED')
         RETURN
      ENDIF

      CALL TREXTR(YOCREC)
      GO TO 300

100   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6020)
6020  FORMAT(1X,'TRPROC: EOF WHEN SEARCHING FOR TR DATA')
      CALL ERRSTP

110   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6030)
6030  FORMAT(1X,'TRPROC: ERROR WHEN SEARCHING FOR TR DATA')
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE CHKTH
**    Function - to check TH data input
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /TRDIAG/NVTRHR,VTRSFC,/THDIAG/LDTHSL,
**          VTHTS,NTHVEC,/DIAGTP/LDTR,/COMDAT/YTYPSF,
**          LWRITE,LASCII,/ALOWTR/NTRFLD,/ERRMSG/NCERR
**    Com changed - /THDIAG/NTHPR,NTHPL,NTH,LDTHCT,/TRDIAG/LDTH,
**          /ERRMSG/NWARN,NFATAL
**    Params used - /PARAM1/JPNL,/PARAMS/JPTRMX
**    Called by - DIAGCK
**    Calls - LEVCHK,LATCHK
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /ALOWTR/NTRFLD(JPTRMX)
      LOGICAL LWRITE,LASCII,LSCRAT,LUTF14,LROGR,LMASSW,LAVMSK,
     -      LAVSIG
      CHARACTER*2 YTYPSF
      CHARACTER*6 YDUM6
      CHARACTER*8 YNAME(2)
      COMMON /COMDAT/BEGDAY,ENDDAY,LWRITE,LASCII,LSCRAT,
     -      LUTF14,LROGR,NSIGFG,NFREQD,NFREQA,NFREQP,RNTAPE,
     -      NASCII,LMASSW,LAVMSK,LAVSIG,YNAME,YTYPSF,YDUM6
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDTHSL,LDTHCT,LDTHED,LTHDOB
      COMMON /THDIAG/LDTHSL,LDTHCT,LDTHED,VTHTS(2),
     -      NTHPR(JPTRMX),NTHPL(JPTRMX),NTH,NTHVEC(JPTRMX),
     -      CINTTH(JPTRMX),LTHDOB
      LOGICAL LDTH,LDTZ,LDTM,LDTP,LDT3,LDTRTD,LDTRTS,LDTRIN
      COMMON /TRDIAG/LDTH,LDTZ,LDTM,LDTP,LDT3,LDTRTD,LDTRTS,LDTRIN,
     -      NCTR,NVTRHR(JPNL),VTRSFC(JPNL)
      LOGICAL LOERR,LOVALU

      LOERR=.FALSE.

**    TR/TH diagnostics require that LDTR and LDTH are set to .TRUE.
      IF((.NOT.LDTR).OR.(.NOT.LDTH)) THEN
         IF((LDTHSL).OR.(LDTHCT).OR.(VTHTS(1).GT.-90.0).OR.
     -         (VTHTS(2).GT.-90.0)) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6000)
6000        FORMAT(1X,'CHKTH: WARNING - TH DIAGNOSTICS REQUESTED BUT',
     -            ' HORIZONTAL LDTR/LDTH FLAGS NOT SET')
            RETURN
         ENDIF
      ENDIF

**    Check that printing has been requested - if not, ensure
**          NTHPR=0 for all elements
      IF(.NOT.LWRITE) THEN
         IF(NTHPR(1).NE.0) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6005)
6005        FORMAT(1X,'CHKTH: NTHPR VALUES CHANGED - NO PRINTING')
            DO 1000 K=1,JPTRMX
1000        NTHPR(K)=0
**          Reset value of NTH
            NTH=0
            DO 1001 K=1,JPTRMX
            IF(NTHPL(K).GT.0) NTH=NTH+1
1001        CONTINUE
         ENDIF
      ENDIF

**    Check that UTFs are requested - if not, ensure NTHPL=0
**          for all elements
      IF(.NOT.LASCII) THEN
         IF(NTHPL(1).NE.0) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6010)
6010        FORMAT(1X,'CHKTH: NTHPL VALUES CHANGED - NO PLOTTING')
            DO 1100 K=1,JPTRMX
1100        NTHPL(K)=0
**          Reset value of NTH
            NTH=0
            DO 1101 K=1,JPTRMX
            IF(NTHPR(K).GT.0) NTH=NTH+1
1101        CONTINUE
         ENDIF
      ENDIF

**    Check valid code numbers in NTHPR
      IF(NTHPR(1).NE.0) THEN
         DO 2000 K=1,JPTRMX
         LOVALU=.FALSE.
         DO 2100 J=1,JPTRMX
         IF((NTHPR(K).EQ.NTRFLD(J)).OR.(NTHPR(K).EQ.0)) LOVALU=.TRUE.
2100     CONTINUE
         IF(.NOT.LOVALU) THEN
            LOERR=.TRUE.
            NFATAL=NFATAL+1
            WRITE(NCERR,6020) NTHPR(K)
6020        FORMAT(1X,'CHKTH: ERROR IN NTHPR VALUE ',I4)
         ENDIF
2000     CONTINUE
      ENDIF

**    Check valid code numbers in NTHPL
      IF(NTHPL(1).NE.0) THEN
         DO 3000 K=1,JPTRMX
         LOVALU=.FALSE.
         DO 3100 J=1,JPTRMX
         IF((NTHPL(K).EQ.NTRFLD(J)).OR.(NTHPL(K).EQ.0)) LOVALU=.TRUE.
3100     CONTINUE
         IF(.NOT.LOVALU) THEN
            LOERR=.TRUE.
            NFATAL=NFATAL+1
            WRITE(NCERR,6030) NTHPL(K)
6030        FORMAT(1X,'CHKTH: ERROR IN NTHPL VALUE ',I4)
         ENDIF
3000     CONTINUE
      ENDIF

**    Switch off diagnostics if NTH is zero
      IF(NTH.EQ.0.AND.LDTH) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6035)
6035     FORMAT(1X,'CHKTH: TH DIAGNOSTICS SWITCHED OFF, NTH=0')
         LDTH=.FALSE.
         RETURN
      ENDIF
      IF(.NOT.LDTH) RETURN

**    Check that NTHVEC values correspond to NTHPL values
      IF(NTHVEC(1).NE.0) THEN
         DO 3500 K=1,JPTRMX
         LOVALU=.FALSE.
         DO 3600 J=1,JPTRMX
         IF((NTHVEC(K).EQ.NTHPL(J)).OR.(NTHVEC(K).EQ.0)) LOVALU=.TRUE.
3600     CONTINUE
         IF(.NOT.LOVALU) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6050)
6050        FORMAT(1X,'CHKTH: NTHVEC CORRESPONDS TO NON-PLOTTED FIELD')
         ENDIF
3500     CONTINUE
      ENDIF

**    Check that NTH is non-zero, and less than JPTRMX
      IF(NTH.LE.0.OR.(NTH.GT.JPTRMX)) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6040)
6040     FORMAT(1X,'CHKTH : INVALID VALUE FOR NTH')
         LOERR=.TRUE.
      ENDIF

**    Check that NVTRHR or VTRSFC have been set if LDTHSL is .TRUE.
      IF(LDTHSL.AND.(YTYPSF.EQ.'ET').AND.(NVTRHR(1).EQ.0)) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6055)
6055     FORMAT(1X,'CHKTH: ERROR - HORIZONTAL SLICE O/P REQUIRES'
     -         ' NVTRHR TO BE SPECIFIED FOR O/P ON SIGMA SURFACES')
         LOERR=.TRUE.
      ENDIF
      ZDELTA=1.0E-10
      IF(LDTHSL.AND.(YTYPSF.NE.'ET').AND.(VTRSFC(1).LE.ZDELTA)) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6060)
6060     FORMAT(1X,'CHKTH: ERROR - HORIZONTAL SLICE O/P REQUIRES'
     -         ' VTRSFC TO BE SPECIFIED FOR O/P ON NON-SIGMA SURFACES')
         LOERR=.TRUE.
      ENDIF

**    Check validity of VTHTS values
      CALL LATCHK(VTHTS,2,-999.0,'(TR) TH VTHTS',LOERR)

**    Column totals over the model domain can only be found when output
**    is on 'ET' surfaces
      IF((YTYPSF.NE.'ET').AND.LDTHCT) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6080)
6080     FORMAT(1X,'CHKTH: ERROR - COLUMN TOTALS NOT APPROPRIATE ',
     -         ' SINCE O/P NOT ON MODEL LEVELS')
         LDTHCT=.FALSE.
         LOERR=.TRUE.
      ENDIF

CC    IF(LOERR) STOP
      RETURN
      END
      SUBROUTINE THEXTR(YPCREC)
**    Function - to extract information from TH data and update
**          appropriate common blocks
**    Args in -
**               YPCREC  - character*80 variable containing data
**    Args out - none
**    Args for work - none
**    Com used - /ALOWTR/NTRFLD,/ERRMSG/NCERR
**    Com changed - /THDIAG/all elements,/ERRMSG/NFATAL
**    Params used - /PARAMS/JPTRMX
**    Called by - THPROC
**    Calls - SEARCH,LPROC,RMULTP,IMULTP,INTCPY,ITRANS,LASTCH,ERRSTP
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /ALOWTR/NTRFLD(JPTRMX)
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDTHSL,LDTHCT,LDTHED,LTHDOB
      COMMON /THDIAG/LDTHSL,LDTHCT,LDTHED,VTHTS(2),
     -      NTHPR(JPTRMX),NTHPL(JPTRMX),NTH,NTHVEC(JPTRMX),
     -      CINTTH(JPTRMX),LTHDOB
      LOGICAL LOFIND,LOALL
      CHARACTER*1 YOCOM,YOEQL
      CHARACTER*2 YOKW2
      CHARACTER*4 YOKW4
      CHARACTER*5 YOKW5
      CHARACTER*8 YOKW8
      CHARACTER*9 YOKW9
      CHARACTER*10 YOKW10
      CHARACTER*11 YOKW11
      CHARACTER*14 YOKW14
      CHARACTER*80 YPCREC

      YOEQL='='
      YOCOM=','
**    Search for the keyword in YPCREC and then determine the field
**          value
      LOFIND=.FALSE.

      YOKW2='CI'
      CALL SEARCH(YPCREC,YOKW2,LOFIND)
      IF(LOFIND) THEN
         CALL RMULTP(YPCREC,YOEQL,YOCOM,CINTTH,JPTRMX)
         RETURN
      ENDIF

**    Else
      YOKW11='DOBSONUNITS'
      CALL SEARCH(YPCREC,YOKW11,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LTHDOB)
         RETURN
      ENDIF

**    Else
      YOKW5='SLICE'
      CALL SEARCH(YPCREC,YOKW5,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDTHSL)
         RETURN
      ENDIF

**    Else
      YOKW8='COLTOTAL'
      CALL SEARCH(YPCREC,YOKW8,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDTHCT)
         RETURN
      ENDIF

**    Else
      YOKW4='EDDY'
      CALL SEARCH(YPCREC,YOKW4,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDTHED)
         RETURN
      ENDIF

**    Else
      YOKW9='HOVMOLLER'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
         CALL RMULTP(YPCREC,YOEQL,YOCOM,VTHTS,2)
         RETURN
      ENDIF

**    Else
      YOKW10='PRINTFIELD'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            CALL INTCPY(NTRFLD,JPTRMX,NTHPR)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NTHPR,JPTRMX)
         RETURN
      ENDIF

**    Else
      YOKW9='PLOTFIELD'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            CALL INTCPY(NTRFLD,JPTRMX,NTHPL)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NTHPL,JPTRMX)
         RETURN
      ENDIF

**    Else
      YOKW14='NUMBEROFFIELDS'
      CALL SEARCH(YPCREC,YOKW14,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
**       Note that IPOSN and IPOSX can take the same value
         IF(IERR.EQ.998) GO TO 1000
         CALL ITRANS(YPCREC,IPOSN,IPOSX,NTH,12)
         RETURN
      ENDIF

**    Else
      YOKW10='WINDVECTOR'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            CALL INTCPY(NTRFLD,JPTRMX,NTHVEC)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NTHVEC,JPTRMX)
         RETURN
      ENDIF

**    Else
**    Keyword not located
      NFATAL=NFATAL+1
      WRITE(NCERR,6030) YPCREC
6030  FORMAT(1X,'KEYWORD NOT LOCATED FOR PHRASE :'/A)
      CALL ERRSTP

1000  CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6000) YPCREC
6000  FORMAT(1X,'THEXTR: ERROR IN TH DATA ENTRY',/A)
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE THPROC
**    Function - to read and process the TH diagnostic data
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /TRDIAG/LDTH,/DIAGTP/LDTR,/ERRMSG/NCERR
**    Com changed - /THDIAG/all elements,/ERRMSG/NFATAL,NWARN
**    Called by - DIAGRD
**    Calls - SEARCH,THEXTR,ERRSTP
**    Files read - 10
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDTHSL,LDTHCT,LDTHED,LTHDOB
      COMMON /THDIAG/LDTHSL,LDTHCT,LDTHED,VTHTS(2),
     -      NTHPR(JPTRMX),NTHPL(JPTRMX),NTH,NTHVEC(JPTRMX),
     -      CINTTH(JPTRMX),LTHDOB
      LOGICAL LDTH,LDTZ,LDTM,LDTP,LDT3,LDTRTD,LDTRTS,LDTRIN
      COMMON /TRDIAG/LDTH,LDTZ,LDTM,LDTP,LDT3,LDTRTD,LDTRTS,LDTRIN,
     -      NCTR,NVTRHR(JPNL),VTRSFC(JPNL)
      CHARACTER*80 YOCREC
      LOGICAL LOFIND
**    Specify the defaults
      PARAMETER(R999=-999.0,RNEG=-0.1)
      DATA LDTHSL/.FALSE./,LDTHCT/.FALSE./,LDTHED/.FALSE./,
     -      LTHDOB/.FALSE./
      DATA VTHTS/2*R999/
      DATA NTH/0/,NTHVEC/JPTRMX*0/,NTHPR/JPTRMX*0/,NTHPL/JPTRMX*0/
      DATA CINTTH/JPTRMX*RNEG/

      IF(.NOT.(LDTR.AND.LDTH)) THEN
**       TH diagnostics not selected - print warning
         NWARN=NWARN+1
         WRITE(NCERR,6000)
6000     FORMAT(1X,'WARNING: - TH DIAGNOSTICS NOT SELECTED:',
     -         ' ALTHOUGH USER HAS SUPPLIED TH INPUT DATA')
      ENDIF

**    Read and interpret records until '$ENDTH' found
300   READ(10,5000,END=100,ERR=110)YOCREC
5000  FORMAT(A80)
      CALL SEARCH(YOCREC,'$ENDTH',LOFIND)
      IF(LOFIND) THEN
         WRITE(NCERR,6010)
6010     FORMAT(1X,'USER-SUPPLIED TH SELECTION DATA PROCESSED')
         RETURN
      ENDIF

      CALL THEXTR(YOCREC)
      GO TO 300

100   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6020)
6020  FORMAT(1X,'THPROC: EOF WHEN SEARCHING FOR TH DATA')
      CALL ERRSTP

110   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6030)
6030  FORMAT(1X,'THPROC: ERROR WHEN SEARCHING FOR TH DATA')
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE CHKTZ
**    Function - to check TZ data input
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /TRDIAG/NVTRHR,VTRSFC,/TZDIAG/all elements,
**          /DIAGTP/LDTR,/COMDAT/YTYPSF,LWRITE,LASCII,ALOWTR/NTRFLD,
**          /ERRMSG/NCERR
**    Com changed - /TZDIAG/NTZPR,NTZPL,NTZ,/TRDIAG/LDTZ,
**          /ERRMSG/NFATAL,NWARN
**    Params used - /PARAM1/JPNL,/PARAMS/JPTRMX
**    Called by - DIAGCK
**    Calls - LNGCHK
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /ALOWTR/NTRFLD(JPTRMX)
      LOGICAL LWRITE,LASCII,LSCRAT,LUTF14,LROGR,LMASSW,LAVMSK,
     -      LAVSIG
      CHARACTER*2 YTYPSF
      CHARACTER*6 YDUM6
      CHARACTER*8 YNAME(2)
      COMMON /COMDAT/BEGDAY,ENDDAY,LWRITE,LASCII,LSCRAT,
     -      LUTF14,LROGR,NSIGFG,NFREQD,NFREQA,NFREQP,RNTAPE,
     -      NASCII,LMASSW,LAVMSK,LAVSIG,YNAME,YTYPSF,YDUM6
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDTZZM,LDTZTS
      COMMON /TZDIAG/LDTZZM,VTZNS,LDTZTS,VTZNSA(2),NTZPR(JPTRMX),
     -      NTZPL(JPTRMX),NTZ,NTZVEC(JPTRMX),CINTTZ(JPTRMX)
      LOGICAL LDTH,LDTZ,LDTM,LDTP,LDT3,LDTRTD,LDTRTS,LDTRIN
      COMMON /TRDIAG/LDTH,LDTZ,LDTM,LDTP,LDT3,LDTRTD,LDTRTS,LDTRIN,
     -      NCTR,NVTRHR(JPNL),VTRSFC(JPNL)
      LOGICAL LOERR,LOVALU

      LOERR=.FALSE.

**    TR/TZ diagnostics require that LDTR and LDTZ are set to .TRUE.
      IF((.NOT.LDTR).OR.(.NOT.LDTZ)) THEN
         IF((LDTZZM).OR.(LDTZTS).OR.(VTZNS.GT.0.0).OR.
     -         (VTZNSA(1).GT.0.0).OR.(VTZNSA(2).GT.0.0)) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6000)
6000        FORMAT(1X,'CHKTZ: WARNING - TZ DIAGNOSTICS REQUESTED BUT',
     -            ' BOTH LDTR/LDTZ FLAGS NOT SET')
            RETURN
         ENDIF
      ENDIF

**    Check that printing has been requested - if not, ensure
**          NTZPR=0 for all elements
      IF(.NOT.LWRITE) THEN
         IF(NTZPR(1).NE.0) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6005)
6005        FORMAT(1X,'CHKTZ: NTZPR VALUES CHANGED - NO PRINTING')
            DO 1000 K=1,JPTRMX
1000        NTZPR(K)=0
**          Reset value of NTZ
            NTZ=0
            DO 1001 K=1,JPTRMX
            IF(NTZPL(K).GT.0) NTZ=NTZ+1
1001        CONTINUE
         ENDIF
      ENDIF

**    Check that UTFs are requested - if not, ensure NTZPL=0
**          for all elements
      IF(.NOT.LASCII) THEN
         IF(NTZPL(1).NE.0) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6010)
6010        FORMAT(1X,'CHKTZ: NTZPL VALUES CHANGED - NO PLOTTING')
            DO 1100 K=1,JPTRMX
1100        NTZPL(K)=0
**          Reset value of NTZ
            NTZ=0
            DO 1101 K=1,JPTRMX
            IF(NTZPR(K).GT.0) NTZ=NTZ+1
1101        CONTINUE
         ENDIF
      ENDIF

**    Check valid code numbers in NTZPR
      IF(NTZPR(1).NE.0) THEN
         DO 2000 K=1,JPTRMX
         LOVALU=.FALSE.
         DO 2100 J=1,JPTRMX
         IF((NTZPR(K).EQ.NTRFLD(J)).OR.(NTZPR(K).EQ.0)) LOVALU=.TRUE.
2100     CONTINUE
         IF(.NOT.LOVALU) THEN
            LOERR=.TRUE.
            NFATAL=NFATAL+1
            WRITE(NCERR,6020) NTZPR(K)
6020        FORMAT(1X,'CHKTZ: ERROR IN NTZPR VALUE ',I4)
         ENDIF
2000     CONTINUE
      ENDIF

**    Check valid code numbers in NTZPL
      IF(NTZPL(1).NE.0) THEN
         DO 3000 K=1,JPTRMX
         LOVALU=.FALSE.
         DO 3100 J=1,JPTRMX
         IF((NTZPL(K).EQ.NTRFLD(J)).OR.(NTZPL(K).EQ.0)) LOVALU=.TRUE.
3100     CONTINUE
         IF(.NOT.LOVALU) THEN
            LOERR=.TRUE.
            NFATAL=NFATAL+1
            WRITE(NCERR,6030) NTZPL(K)
6030        FORMAT(1X,'CHKTZ: ERROR IN NTZPL VALUE ',I4)
         ENDIF
3000     CONTINUE
      ENDIF

**    Switch off diagnostics if NTZ is zero
      IF(NTZ.EQ.0.AND.LDTZ) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6035)
6035     FORMAT(1X,'CHKTZ: TZ DIAGNOSTICS SWITCHED OFF, NTZ=0')
         LDTZ=.FALSE.
         RETURN
      ENDIF
      IF(.NOT.LDTZ) RETURN

**    Check that NTZVEC values correspond to NTZPL values
      IF(NTZVEC(1).NE.0) THEN
         DO 3500 K=1,JPTRMX
         LOVALU=.FALSE.
         DO 3600 J=1,JPTRMX
         IF((NTZVEC(K).EQ.NTZPL(J)).OR.(NTZVEC(K).EQ.0)) LOVALU=.TRUE.
3600     CONTINUE
         IF(.NOT.LOVALU) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6050)
6050        FORMAT(1X,'CHKTZ: NTZVEC CORRESPONDS TO NON-PLOTTED FIELD')
         ENDIF
3500     CONTINUE
      ENDIF

**    Check that NTZ is non-zero, and less than JPTRMX
      IF(NTZ.LE.0.OR.(NTZ.GT.JPTRMX)) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6040)
6040     FORMAT(1X,'CHKTZ : INVALID VALUE FOR NTZ')
         LOERR=.TRUE.
      ENDIF

**    Check that NVTRHR or VTRSFC have been set if LDTZTS is .TRUE.
      IF(LDTZTS.AND.(YTYPSF.EQ.'ET').AND.(NVTRHR(1).EQ.0)) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6060)
6060     FORMAT(1X,'CHKTZ: ERROR - ZONAL TIMESERIES O/P REQUIRES'
     -         ' NVTRHR TO BE SPECIFIED FOR O/P ON SIGMA SURFACES')
         LOERR=.TRUE.
      ENDIF
      ZDELTA=1.0E-10
      IF(LDTZTS.AND.(YTYPSF.NE.'ET').AND.(VTRSFC(1).LE.ZDELTA)) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6070)
6070     FORMAT(1X,'CHKTZ: ERROR - ZONAL TIMESERIES O/P REQUIRES'
     -         ' VTRSFC TO BE SPECIFIED FOR O/P ON NON-SIGMA SURFACES')
         LOERR=.TRUE.
      ENDIF

**    Check validity of VTZNS AND VTZNSA values
      CALL LNGCHK(VTZNS,1,-999.0,'(TR) TZ VTZNS',LOERR)
      CALL LNGCHK(VTZNSA,2,-999.0,'(TR) TZ VTZNSA',LOERR)

CC    IF(LOERR) STOP
      RETURN
      END
      SUBROUTINE TZEXTR(YPCREC)
**    Function - to extract information from TZ data and update
**          appropriate common blocks
**    Args in -
**               YPCREC  - character*80 variable containing data
**    Args out - none
**    Args for work - none
**    Com used - /ALOWTR/NTRFLD,/ERRMSG/NCERR
**    Com changed - /TZDIAG/all elements,/ERRMSG/NFATAL,NWARN
**    Params used - /PARAMS/JPTRMX
**    Called by - TZPROC
**    Calls - SEARCH,LPROC,RMULTP,LASTCH,RTRANS,IMULTP,INTCPY,
**          ITRANS,LASTCH,ERRSTP
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /ALOWTR/NTRFLD(JPTRMX)
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDTZZM,LDTZTS
      COMMON /TZDIAG/LDTZZM,VTZNS,LDTZTS,VTZNSA(2),NTZPR(JPTRMX),
     -      NTZPL(JPTRMX),NTZ,NTZVEC(JPTRMX),CINTTZ(JPTRMX)
      LOGICAL LOFIND,LOALL
      CHARACTER*1 YOCOM,YOEQL
      CHARACTER*2 YOKW2
      CHARACTER*4 YOKW4
      CHARACTER*7 YOKW7
      CHARACTER*9 YOKW9
      CHARACTER*10 YOKW10
      CHARACTER*14 YOKW14
      CHARACTER*80 YPCREC

      YOEQL='='
      YOCOM=','
**    Search for the keyword in YPCREC and then determine the field
**          value
      LOFIND=.FALSE.

      YOKW2='CI'
      CALL SEARCH(YPCREC,YOKW2,LOFIND)
      IF(LOFIND) THEN
         CALL RMULTP(YPCREC,YOEQL,YOCOM,CINTTZ,JPTRMX)
         RETURN
      ENDIF


**    Else
      YOKW9='ZONALMEAN'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDTZZM)
         RETURN
      ENDIF

**    Else
      YOKW9='TIMESLICE'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDTZTS)
         RETURN
      ENDIF

**    Else
      YOKW9='NSAVERAGE'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
         CALL RMULTP(YPCREC,YOEQL,YOCOM,VTZNSA,2)
         RETURN
      ENDIF

**    Else
      YOKW7='NSSLICE'
      CALL SEARCH(YPCREC,YOKW7,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
         IF(IERR.NE.0) GO TO 1000
         CALL RTRANS(YPCREC,IPOSN,IPOSX,VTZNS,12)
         RETURN
      ENDIF

**    Else
      YOKW10='PRINTFIELD'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            CALL INTCPY(NTRFLD,JPTRMX,NTZPR)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NTZPR,JPTRMX)
         RETURN
      ENDIF

**    Else
      YOKW9='PLOTFIELD'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            CALL INTCPY(NTRFLD,JPTRMX,NTZPL)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NTZPL,JPTRMX)
         RETURN
      ENDIF

**    Else
      YOKW14='NUMBEROFFIELDS'
      CALL SEARCH(YPCREC,YOKW14,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
**       Note that IPOSN and IPOSX can take the same value
         IF(IERR.EQ.998) GO TO 1000
         CALL ITRANS(YPCREC,IPOSN,IPOSX,NTZ,12)
         RETURN
      ENDIF

**    Else
      YOKW10='WINDVECTOR'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            CALL INTCPY(NTRFLD,JPTRMX,NTZVEC)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NTZVEC,JPTRMX)
         RETURN
      ENDIF

**    Else
**    Keyword not located
      NFATAL=NFATAL+1
      WRITE(NCERR,6030) YPCREC
6030  FORMAT(1X,'KEYWORD NOT LOCATED FOR PHRASE :'/A)
      CALL ERRSTP

1000  CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6000) YPCREC
6000  FORMAT(1X,'TZEXTR: ERROR IN TZ DATA ENTRY',/A)
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE TZPROC
**    Function - to read and process the TZ diagnostic data
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /TRDIAG/LDTZ,/DIAGTP/LDTR,/ERRMSG/NCERR
**    Com changed - /TZDIAG/all elements,/ERRMSG/NFATAL,NWARN
**    Called by - DIAGRD
**    Calls - SEARCH,TZEXTR,ERRSTP
**    Files read - 10
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDTZZM,LDTZTS
      COMMON /TZDIAG/LDTZZM,VTZNS,LDTZTS,VTZNSA(2),NTZPR(JPTRMX),
     -      NTZPL(JPTRMX),NTZ,NTZVEC(JPTRMX),CINTTZ(JPTRMX)
      LOGICAL LDTH,LDTZ,LDTM,LDTP,LDT3,LDTRTD,LDTRTS,LDTRIN
      COMMON /TRDIAG/LDTH,LDTZ,LDTM,LDTP,LDT3,LDTRTD,LDTRTS,LDTRIN,
     -      NCTR,NVTRHR(JPNL),VTRSFC(JPNL)
      CHARACTER*80 YOCREC
      LOGICAL LOFIND
**    Specify the defaults
      PARAMETER(R999=-999.0,RNEG=-0.1)
      DATA LDTZZM/.FALSE./,LDTZTS/.FALSE./
      DATA VTZNS/R999/,VTZNSA/2*R999/
      DATA NTZ/0/,NTZVEC/JPTRMX*0/,NTZPR/JPTRMX*0/,NTZPL/JPTRMX*0/
      DATA CINTTZ/JPTRMX*RNEG/

      IF(.NOT.(LDTR.AND.LDTZ)) THEN
**       TZ diagnostics not selected - print warning
         NWARN=NWARN+1
         WRITE(NCERR,6000)
6000     FORMAT(1X,'WARNING: - TZ DIAGNOSTICS NOT SELECTED:',
     -         ' ALTHOUGH USER HAS SUPPLIED TZ INPUT DATA')
      ENDIF

**    Read and interpret records until '$ENDTZ' found
300   READ(10,5000,END=100,ERR=110)YOCREC
5000  FORMAT(A80)
      CALL SEARCH(YOCREC,'$ENDTZ',LOFIND)
      IF(LOFIND) THEN
         WRITE(NCERR,6010)
6010     FORMAT(1X,'USER-SUPPLIED TZ SELECTION DATA PROCESSED')
         RETURN
      ENDIF

      CALL TZEXTR(YOCREC)
      GO TO 300

100   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6020)
6020  FORMAT(1X,'TZPROC: EOF WHEN SEARCHING FOR TZ DATA')
      CALL ERRSTP

110   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6030)
6030  FORMAT(1X,'TZPROC: ERROR WHEN SEARCHING FOR TZ DATA')
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE CHKTM
**    Function - to check TM data input
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /TRDIAG/NVTRHR,VTRSFC,/TMDIAG/all elements,
**          /DIAGTP/LDTR,/COMDAT/YTYPSF,LASCII,LWRITE,/ALOWTR/NTRFLD,
**          /ERRMSG/NCERR
**    Com changed - /TMDIAG/NTMPR,NTMPL,NTM,/TRDIAG/LDTM,
**          /ERRMSG/NFATAL,NWARN
**    Params used - /PARAM1/JPNL,/PARAMS/JPTRMX
**    Called by - DIAGCK
**    Calls - LATCHK
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /ALOWTR/NTRFLD(JPTRMX)
      LOGICAL LWRITE,LASCII,LSCRAT,LUTF14,LROGR,LMASSW,LAVMSK,
     -      LAVSIG
      CHARACTER*2 YTYPSF
      CHARACTER*6 YDUM6
      CHARACTER*8 YNAME(2)
      COMMON /COMDAT/BEGDAY,ENDDAY,LWRITE,LASCII,LSCRAT,
     -      LUTF14,LROGR,NSIGFG,NFREQD,NFREQA,NFREQP,RNTAPE,
     -      NASCII,LMASSW,LAVMSK,LAVSIG,YNAME,YTYPSF,YDUM6
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDTMMM,LDTMTS
      COMMON /TMDIAG/LDTMMM,VTMEW,LDTMTS,VTMEWA(2),NTMPR(JPTRMX),
     -      NTMPL(JPTRMX),NTM,NTMVEC(JPTRMX),CINTTM(JPTRMX)
      LOGICAL LDTH,LDTZ,LDTM,LDTP,LDT3,LDTRTD,LDTRTS,LDTRIN
      COMMON /TRDIAG/LDTH,LDTZ,LDTM,LDTP,LDT3,LDTRTD,LDTRTS,LDTRIN,
     -      NCTR,NVTRHR(JPNL),VTRSFC(JPNL)
      LOGICAL LOERR,LOVALU

      LOERR=.FALSE.

**    TR/TM diagnostics require that LDTR and LDTM are set to .TRUE.
      IF((.NOT.LDTR).OR.(.NOT.LDTM)) THEN
         IF((LDTMMM).OR.(LDTMTS).OR.(VTMEW.GT.-90.0).OR.
     -         (VTMEWA(1).GT.-90.0).OR.(VTMEWA(2).GT.-90.0)) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6000)
6000        FORMAT(1X,'CHKTM: WARNING - TM DIAGNOSTICS REQUESTED BUT',
     -            ' BOTH LDTR/LDTM FLAGS NOT SET')
            RETURN
         ENDIF
      ENDIF

**    Check that printing has been requested - if not, ensure
**          NTMPR=0 for all elements
      IF(.NOT.LWRITE) THEN
         IF(NTMPR(1).NE.0) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6005)
6005        FORMAT(1X,'CHKTM: NTMPR VALUES CHANGED - NO PRINTING')
            DO 1000 K=1,JPTRMX
1000        NTMPR(K)=0
**          Reset value of NTM
            NTM=0
            DO 1001 K=1,JPTRMX
            IF(NTMPL(K).GT.0) NTM=NTM+1
1001        CONTINUE
         ENDIF
      ENDIF

**    Check that UTFs are requested - if not, ensure NTMPL=0
**          for all elements
      IF(.NOT.LASCII) THEN
         IF(NTMPL(1).NE.0) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6010)
6010        FORMAT(1X,'CHKTM: NTMPL VALUES CHANGED - NO PLOTTING')
            DO 1100 K=1,JPTRMX
1100        NTMPL(K)=0
**          Reset value of NTM
            NTM=0
            DO 1101 K=1,JPTRMX
            IF(NTMPR(K).GT.0) NTM=NTM+1
1101        CONTINUE
         ENDIF
      ENDIF

**    Check valid code numbers in NTMPR
      IF(NTMPR(1).NE.0) THEN
         DO 2000 K=1,JPTRMX
         LOVALU=.FALSE.
         DO 2100 J=1,JPTRMX
         IF((NTMPR(K).EQ.NTRFLD(J)).OR.(NTMPR(K).EQ.0)) LOVALU=.TRUE.
2100     CONTINUE
         IF(.NOT.LOVALU) THEN
            LOERR=.TRUE.
            NFATAL=NFATAL+1
            WRITE(NCERR,6020) NTMPR(K)
6020        FORMAT(1X,'CHKTM: ERROR IN NTMPR VALUE ',I4)
         ENDIF
2000     CONTINUE
      ENDIF

**    Check valid code numbers in NTMPL
      IF(NTMPL(1).NE.0) THEN
         DO 3000 K=1,JPTRMX
         LOVALU=.FALSE.
         DO 3100 J=1,JPTRMX
         IF((NTMPL(K).EQ.NTRFLD(J)).OR.(NTMPL(K).EQ.0)) LOVALU=.TRUE.
3100     CONTINUE
         IF(.NOT.LOVALU) THEN
            LOERR=.TRUE.
            NFATAL=NFATAL+1
            WRITE(NCERR,6030) NTMPL(K)
6030        FORMAT(1X,'CHKTM: ERROR IN NTMPL VALUE ',I4)
         ENDIF
3000     CONTINUE
      ENDIF

**    Switch off diagnostics if NTM is zero
      IF(NTM.EQ.0.AND.LDTM) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6035)
6035     FORMAT(1X,'CHKTM: TM DIAGNOSTICS SWITCHED OFF, NTM=0')
         LDTM=.FALSE.
         RETURN
      ENDIF
      IF(.NOT.LDTM) RETURN

**    Check that NTMVEC values correspond to NTMPL values
      IF(NTMVEC(1).NE.0) THEN
         DO 3500 K=1,JPTRMX
         LOVALU=.FALSE.
         DO 3600 J=1,JPTRMX
         IF((NTMVEC(K).EQ.NTMPL(J)).OR.(NTMVEC(K).EQ.0)) LOVALU=.TRUE.
3600     CONTINUE
         IF(.NOT.LOVALU) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6050)
6050        FORMAT(1X,'CHKTM: NTMVEC CORRESPONDS TO NON-PLOTTED FIELD')
         ENDIF
3500     CONTINUE
      ENDIF

**    Check that NTM is non-zero, and less than JPTRMX
      IF(NTM.LE.0.OR.(NTM.GT.JPTRMX)) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6040)
6040     FORMAT(1X,'CHKTM : INVALID VALUE FOR NTM')
         LOERR=.TRUE.
      ENDIF

**    Check that NVTRHR or VTRSFC have been set if LDTMTS is .TRUE.
      IF(LDTMTS.AND.(YTYPSF.EQ.'ET').AND.(NVTRHR(1).EQ.0)) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6060)
6060     FORMAT(1X,'CHKTM: ERROR - MERIDIONAL TIMESERIES O/P REQUIRES'
     -         ' NVTRHR TO BE SPECIFIED FOR O/P ON SIGMA SURFACES')
         LOERR=.TRUE.
      ENDIF
      ZDELTA=1.0E-10
      IF(LDTMTS.AND.(YTYPSF.NE.'ET').AND.(VTRSFC(1).LE.ZDELTA)) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6070)
6070     FORMAT(1X,'CHKTM: ERROR - MERIDIONAL TIMESERIES O/P REQUIRES'
     -         ' VTRSFC TO BE SPECIFIED FOR O/P ON NON-SIGMA SURFACES')
         LOERR=.TRUE.
      ENDIF

**    Check validity of VTMEW AND VTMEWA values
      CALL LATCHK(VTMEW,1,-999.0,'(TR) TM VTMEW',LOERR)
      CALL LATCHK(VTMEWA,2,-999.0,'(TR) TM VTMEWA',LOERR)

CC    IF(LOERR) STOP
      RETURN
      END
      SUBROUTINE TMEXTR(YPCREC)
**    Function - to extract information from TM data and update
**          appropriate common blocks
**    Args in -
**               YPCREC  - character*80 variable containing data
**    Args out - none
**    Args for work - none
**    Com used - /ALOWTR/NTRFLD,/ERRMSG/NCERR
**    Com changed - /TMDIAG/all elements,/ERRMSG/NFATAL
**    Params used - /PARAMS/JPTRMX
**    Called by - TMPROC
**    Calls - SEARCH,LPROC,RMULTP,LASTCH,RTRANS,IMULTP,INTCPY,ITRANS,
**          ERRSTP
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /ALOWTR/NTRFLD(JPTRMX)
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDTMMM,LDTMTS
      COMMON /TMDIAG/LDTMMM,VTMEW,LDTMTS,VTMEWA(2),NTMPR(JPTRMX),
     -      NTMPL(JPTRMX),NTM,NTMVEC(JPTRMX),CINTTM(JPTRMX)
      LOGICAL LOFIND,LOALL
      CHARACTER*1 YOCOM,YOEQL
      CHARACTER*2 YOKW2
      CHARACTER*4 YOKW4
      CHARACTER*7 YOKW7
      CHARACTER*9 YOKW9
      CHARACTER*10 YOKW10
      CHARACTER*14 YOKW14
      CHARACTER*80 YPCREC

      YOEQL='='
      YOCOM=','
**    Search for the keyword in YPCREC and then determine the field
**          value
      LOFIND=.FALSE.

      YOKW9='MERIDMEAN'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDTMMM)
         RETURN
      ENDIF

**    Else
      YOKW2='CI'
      CALL SEARCH(YPCREC,YOKW2,LOFIND)
      IF(LOFIND) THEN
         CALL RMULTP(YPCREC,YOEQL,YOCOM,CINTTM,JPTRMX)
         RETURN
      ENDIF


**    Else
      YOKW9='TIMESLICE'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDTMTS)
         RETURN
      ENDIF

**    Else
      YOKW9='WEAVERAGE'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
         CALL RMULTP(YPCREC,YOEQL,YOCOM,VTMEWA,2)
         RETURN
      ENDIF

**    Else
      YOKW7='WESLICE'
      CALL SEARCH(YPCREC,YOKW7,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
         IF(IERR.NE.0) GO TO 1000
         CALL RTRANS(YPCREC,IPOSN,IPOSX,VTMEW,12)
         RETURN
      ENDIF

**    Else
      YOKW10='PRINTFIELD'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            CALL INTCPY(NTRFLD,JPTRMX,NTMPR)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NTMPR,JPTRMX)
         RETURN
      ENDIF

**    Else
      YOKW9='PLOTFIELD'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            CALL INTCPY(NTRFLD,JPTRMX,NTMPL)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NTMPL,JPTRMX)
         RETURN
      ENDIF

**    Else
      YOKW14='NUMBEROFFIELDS'
      CALL SEARCH(YPCREC,YOKW14,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
**       Note that IPOSN and IPOSX can take the same value
         IF(IERR.EQ.998) GO TO 1000
         CALL ITRANS(YPCREC,IPOSN,IPOSX,NTM,12)
         RETURN
      ENDIF

**    Else
      YOKW10='WINDVECTOR'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            CALL INTCPY(NTRFLD,JPTRMX,NTMVEC)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NTMVEC,JPTRMX)
         RETURN
      ENDIF

**    Else
**    Keyword not located
      NFATAL=NFATAL+1
      WRITE(NCERR,6030) YPCREC
6030  FORMAT(1X,'KEYWORD NOT LOCATED FOR PHRASE :'/A)
      CALL ERRSTP

1000  CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6000) YPCREC
6000  FORMAT(1X,'TMEXTR: ERROR IN TM DATA ENTRY',/A)
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE TMPROC
**    Function - to read and process the TM diagnostic data
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /TRDIAG/LDTM,/DIAGTP/LDTR,/ERRMSG/NCERR
**    Com changed - /TMDIAG/all elements,/ERRMSG/NFATAL
**    Called by - DIAGRD
**    Calls - SEARCH,TMEXTR,CALL ERRSTP
**    Files read - 10
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDTMMM,LDTMTS
      COMMON /TMDIAG/LDTMMM,VTMEW,LDTMTS,VTMEWA(2),NTMPR(JPTRMX),
     -      NTMPL(JPTRMX),NTM,NTMVEC(JPTRMX),CINTTM(JPTRMX)
      LOGICAL LDTH,LDTZ,LDTM,LDTP,LDT3,LDTRTD,LDTRTS,LDTRIN
      COMMON /TRDIAG/LDTH,LDTZ,LDTM,LDTP,LDT3,LDTRTD,LDTRTS,LDTRIN,
     -      NCTR,NVTRHR(JPNL),VTRSFC(JPNL)
      CHARACTER*80 YOCREC
      LOGICAL LOFIND
**    Specify the defaults
      PARAMETER(R999=-999.0,RNEG=-0.1)
      DATA LDTMMM/.FALSE./,LDTMTS/.FALSE./
      DATA VTMEW/R999/,VTMEWA/2*R999/
      DATA NTM/0/,NTMVEC/JPTRMX*0/,NTMPR/JPTRMX*0/,NTMPL/JPTRMX*0/
      DATA CINTTM/JPTRMX*RNEG/

      IF(.NOT.(LDTR.AND.LDTM)) THEN
**       TM diagnostics not selected - print warning
         NWARN=NWARN+1
         WRITE(NCERR,6000)
6000     FORMAT(1X,'WARNING: - TM DIAGNOSTICS NOT SELECTED:',
     -         ' ALTHOUGH USER HAS SUPPLIED TM INPUT DATA')
      ENDIF

**    Read and interpret records until '$ENDTM' found
300   READ(10,5000,END=100,ERR=110)YOCREC
5000  FORMAT(A80)
      CALL SEARCH(YOCREC,'$ENDTM',LOFIND)
      IF(LOFIND) THEN
         WRITE(NCERR,6010)
6010     FORMAT(1X,'USER-SUPPLIED TM SELECTION DATA PROCESSED')
         RETURN
      ENDIF

      CALL TMEXTR(YOCREC)
      GO TO 300

100   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6020)
6020  FORMAT(1X,'TMPROC: EOF WHEN SEARCHING FOR TM DATA')
      CALL ERRSTP

110   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6030)
6030  FORMAT(1X,'TMPROC: ERROR WHEN SEARCHING FOR TM DATA')
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE CHKTP
**    Function - to check TP data input
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /TPDIAG/all elements,/DIAGTP/LDTR,/ERRMSG/NCERR,
**          /COMDAT/LWRITE,LASCII,YTYPSF,/ALOWTR/,NTRFLD
**    Com changed - /TPDIAG/NTPPR,NTPPL,NTP,LDTPCT,/TRDIAG/LDTP,
**          /ERRMSG/NFATAL,NWARN
**    Params used - /PARAM1/JPNL,/PARAMS/JPTRMX,JPTPPF
**    Called by - DIAGCK
**    Calls - LATCHK,LNGCHK
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /ALOWTR/NTRFLD(JPTRMX)
      LOGICAL LWRITE,LASCII,LSCRAT,LUTF14,LROGR,LMASSW,LAVMSK,
     -      LAVSIG
      CHARACTER*2 YTYPSF
      CHARACTER*6 YDUM6
      CHARACTER*8 YNAME(2)
      COMMON /COMDAT/BEGDAY,ENDDAY,LWRITE,LASCII,LSCRAT,
     -      LUTF14,LROGR,NSIGFG,NFREQD,NFREQA,NFREQP,RNTAPE,
     -      NASCII,LMASSW,LAVMSK,LAVSIG,YNAME,YTYPSF,YDUM6
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDTPCT,LDTPCP,LTPDOB
      COMMON /TPDIAG/LDTPCT,NTPPF,VTPPF(2,JPTPPF),NTPPR(JPTRMX),
     -      NTPPL(JPTRMX),NTP,CINTTP(JPTRMX),LDTPCP,LTPDOB
      LOGICAL LDTH,LDTZ,LDTM,LDTP,LDT3,LDTRTD,LDTRTS,LDTRIN
      COMMON /TRDIAG/LDTH,LDTZ,LDTM,LDTP,LDT3,LDTRTD,LDTRTS,LDTRIN,
     -      NCTR,NVTRHR(JPNL),VTRSFC(JPNL)
      LOGICAL LOERR,LOVALU

      LOERR=.FALSE.

**    TR/TP diagnostics require that LDTR and LDTP are set to .TRUE.
      IF((.NOT.LDTR).OR.(.NOT.LDTP)) THEN
         IF((LDTPCT).OR.(VTPPF(1,1).GT.-90.0).OR.(LDTPCP)) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6000)
6000        FORMAT(1X,'CHKTP: WARNING - TP DIAGNOSTICS REQUESTED BUT',
     -            ' BOTH LDTR/LDTP FLAGS NOT SET')
            RETURN
         ENDIF
      ENDIF

**    LDTPCT only valid if output is on model surfaces
      IF(YTYPSF.NE.'ET'.AND.LDTPCT) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6005)
6005     FORMAT(1X,'CHKTP: ERROR - TP COLUMN TOTALS CANNOT BE FOUND',
     -         'WHEN OUTPUT NOT ON ET SURFACES')
         LDTPCT=.FALSE.
         LOERR=.TRUE.
      ENDIF

**    Check that printing has been requested - if not, ensure
**          NTPPR=0 for all elements
      IF(.NOT.LWRITE) THEN
         IF(NTPPR(1).NE.0) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6060)
6060        FORMAT(1X,'CHKTP: NTPPR VALUES CHANGED - NO PRINTING')
            DO 1000 K=1,JPTRMX
1000        NTPPR(K)=0
**          Reset value of NTP
            NTP=0
            DO 1001 K=1,JPTRMX
            IF(NTPPL(K).GT.0) NTP=NTP+1
1001        CONTINUE
         ENDIF
      ENDIF

**    Check that UTFs are requested - if not, ensure NTPPL=0
**          for all elements
      IF(.NOT.LASCII) THEN
         IF(NTPPL(1).NE.0) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6010)
6010        FORMAT(1X,'CHKTP: NTPPL VALUES CHANGED - NO PLOTTING')
            DO 1100 K=1,JPTRMX
1100        NTPPL(K)=0
**          Reset value of NTP
            NTP=0
            DO 1101 K=1,JPTRMX
            IF(NTPPR(K).GT.0) NTP=NTP+1
1101        CONTINUE
         ENDIF
      ENDIF

**    Check valid code numbers in NTPPR
      IF(NTPPR(1).NE.0) THEN
         DO 2000 K=1,JPTRMX
         LOVALU=.FALSE.
         DO 2100 J=1,JPTRMX
         IF((NTPPR(K).EQ.NTRFLD(J)).OR.(NTPPR(K).EQ.0)) LOVALU=.TRUE.
2100     CONTINUE
         IF(.NOT.LOVALU) THEN
            LOERR=.TRUE.
            NFATAL=NFATAL+1
            WRITE(NCERR,6020) NTPPR(K)
6020        FORMAT(1X,'CHKTP: ERROR IN NTPPR VALUE ',I4)
         ENDIF
2000     CONTINUE
      ENDIF

**    Check valid code numbers in NTPPL
      IF(NTPPL(1).NE.0) THEN
         DO 3000 K=1,JPTRMX
         LOVALU=.FALSE.
         DO 3100 J=1,JPTRMX
         IF((NTPPL(K).EQ.NTRFLD(J)).OR.(NTPPL(K).EQ.0)) LOVALU=.TRUE.
3100     CONTINUE
         IF(.NOT.LOVALU) THEN
            LOERR=.TRUE.
            NFATAL=NFATAL+1
            WRITE(NCERR,6030) NTPPL(K)
6030        FORMAT(1X,'CHKTP: ERROR IN NTPPL VALUE ',I4)
         ENDIF
3000     CONTINUE
      ENDIF

**    Switch off diagnostics if NTP is zero
      IF(NTP.EQ.0.AND.LDTP) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6035)
6035     FORMAT(1X,'CHKTP: TP DIAGNOSTICS SWITCHED OFF, NTP=0')
         LDTP=.FALSE.
         RETURN
      ENDIF
      IF(.NOT.LDTP) RETURN

**    Check that NTP is non-zero, and less than JPTRMX
      IF(NTP.LE.0.OR.(NTP.GT.JPTRMX)) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6040)
6040     FORMAT(1X,'CHKTP : INVALID VALUE FOR NTP')
         LOERR=.TRUE.
      ENDIF

**    Check validity of profile coordinates (IPROFL is max no. of
**    profiles allowed)
      IF(NTPPF.LT.0.OR.NTPPF.GT.JPTPPF) THEN
         NFATAL=NFATAL+1
         LOERR=.TRUE.
         WRITE(NCERR,6050)
6050     FORMAT(1X,'CHKTP: ERROR - INVALID NTPPF')
      ENDIF

      IF(NTPPF.GT.0) THEN
         DO 100 JL=1,NTPPF
         CALL LATCHK(VTPPF(1,JL),1,-999.0,'(TR) TP VTPPF',LOERR)
         CALL LNGCHK(VTPPF(2,JL),1,-999.0,'(TR) TP VTPPF',LOERR)
         IF((VTPPF(1,JL).LT.-90.0).OR.(VTPPF(2,JL).LT.-90.0)) THEN
            NFATAL=NFATAL+1
            WRITE(NCERR,6080)
6080        FORMAT(1X,'CHKTP: ERROR - INVALID PROFILE COORDINATES')
            LOERR=.TRUE.
         ENDIF
100      CONTINUE
      ENDIF

CC    IF(LOERR) STOP
      RETURN
      END
      SUBROUTINE TPEXTR(YPCREC)
**    Function - to extract information from TP data and update
**          appropriate common blocks
**    Args in -
**               YPCREC  - character*80 variable containing data
**    Args out - none
**    Args for work - none
**    Com used - /ALOWTR/NTRFLD,/ERRMSG/NCERR
**    Com changed - /TPDIAG/all elements,/ERRMSG/NFATAL
**    Params used - /PARAMS/JPTPPF
**    Called by - TPPROC
**    Calls - SEARCH,LPROC,RMULTP,LASTCH,ITRANS,IMULTP,INTCPY,ERRSTP
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /ALOWTR/NTRFLD(JPTRMX)
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDTPCT,LDTPCP,LTPDOB
      COMMON /TPDIAG/LDTPCT,NTPPF,VTPPF(2,JPTPPF),NTPPR(JPTRMX),
     -      NTPPL(JPTRMX),NTP,CINTTP(JPTRMX),LDTPCP,LTPDOB
      LOGICAL LOFIND,LOALL
      CHARACTER*1 YOCOM,YOEQL
      CHARACTER*2 YOKW2
      CHARACTER*4 YOKW4
      CHARACTER*8 YOKW8
      CHARACTER*9 YOKW9
      CHARACTER*10 YOKW10
      CHARACTER*11 YOKW11
      CHARACTER*13 YOKW13
      CHARACTER*14 YOKW14
      CHARACTER*80 YPCREC

      YOEQL='='
      YOCOM=','
**    Search for the keyword in YPCREC and then determine the field
**          value
      LOFIND=.FALSE.

      YOKW8='COLTOTAL'
      CALL SEARCH(YPCREC,YOKW8,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDTPCT)
         RETURN
      ENDIF

**    Else
      YOKW10='COLPROFILE'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDTPCP)
         RETURN
      ENDIF

**    Else
      YOKW11='DOBSONUNITS'
      CALL SEARCH(YPCREC,YOKW11,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LTPDOB)
         RETURN
      ENDIF

**    Else
      YOKW2='CI'
      CALL SEARCH(YPCREC,YOKW2,LOFIND)
      IF(LOFIND) THEN
         CALL RMULTP(YPCREC,YOEQL,YOCOM,CINTTP,JPTRMX)
         RETURN
      ENDIF


**    Else
      YOKW14='NUMBEROFCOORDS'
      CALL SEARCH(YPCREC,YOKW14,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
**       Note that IPOSN and IPOSX can take the same value
         IF(IERR.EQ.998) GO TO 1000
         CALL ITRANS(YPCREC,IPOSN,IPOSX,NTPPF,12)
         RETURN
      ENDIF

**    Else
      YOKW13='PROFILECOORDS'
      CALL SEARCH(YPCREC,YOKW13,LOFIND)
      IF(LOFIND) THEN
         CALL RMULTP(YPCREC,YOEQL,YOCOM,VTPPF,2*JPTPPF)
         RETURN
      ENDIF

**    Else
      YOKW10='PRINTFIELD'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            CALL INTCPY(NTRFLD,JPTRMX,NTPPR)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NTPPR,JPTRMX)
         RETURN
      ENDIF

**    Else
      YOKW9='PLOTFIELD'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            CALL INTCPY(NTRFLD,JPTRMX,NTPPL)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NTPPL,JPTRMX)
         RETURN
      ENDIF

**    Else
      YOKW14='NUMBEROFFIELDS'
      CALL SEARCH(YPCREC,YOKW14,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
**       Note that IPOSN and IPOSX can take the same value
         IF(IERR.EQ.998) GO TO 1000
         CALL ITRANS(YPCREC,IPOSN,IPOSX,NTP,12)
         RETURN
      ENDIF

**    Else
**    Keyword not located
      NFATAL=NFATAL+1
      WRITE(NCERR,6030) YPCREC
6030  FORMAT(1X,'KEYWORD NOT LOCATED FOR PHRASE :'/A)
      CALL ERRSTP

1000  CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6000) YPCREC
6000  FORMAT(1X,'TPEXTR: ERROR IN TP DATA ENTRY',/A)
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE TPPROC
**    Function - to read and process the TP diagnostic data
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /TRDIAG/LDTP,/DIAGTP/LDTR,/ERRMSG/NCERR
**    Com changed - /TPDIAG/all elements,/ERRMSG/NWARN,NFATAL
**    Params - JPTP2,RNEG,/PARAMS/JPTPPF
**    Called by - DIAGRD
**    Calls - SEARCH,TPEXTR,ERRSTP
**    Files read - 10
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDTPCT,LDTPCP,LTPDOB
      COMMON /TPDIAG/LDTPCT,NTPPF,VTPPF(2,JPTPPF),NTPPR(JPTRMX),
     -      NTPPL(JPTRMX),NTP,CINTTP(JPTRMX),LDTPCP,LTPDOB
      LOGICAL LDTH,LDTZ,LDTM,LDTP,LDT3,LDTRTD,LDTRTS,LDTRIN
      COMMON /TRDIAG/LDTH,LDTZ,LDTM,LDTP,LDT3,LDTRTD,LDTRTS,LDTRIN,
     -      NCTR,NVTRHR(JPNL),VTRSFC(JPNL)
      CHARACTER*80 YOCREC
      LOGICAL LOFIND
**    Specify the defaults
      PARAMETER(R999=-999.0,RNEG=-0.1,JPTP2=2*JPTPPF)
      DATA LDTPCT/.FALSE./,LDTPCP/.FALSE./,LTPDOB/.FALSE./
      DATA NTPPF/0/,VTPPF/JPTP2*R999/
      DATA NTP/0/,NTPPR/JPTRMX*0/,NTPPL/JPTRMX*0/
      DATA CINTTP/JPTRMX*RNEG/

      IF(.NOT.(LDTR.AND.LDTP)) THEN
**       TP diagnostics not selected - print warning
         NWARN=NWARN+1
         WRITE(NCERR,6000)
6000     FORMAT(1X,'WARNING: - TP DIAGNOSTICS NOT SELECTED:',
     -         ' ALTHOUGH USER HAS SUPPLIED TP INPUT DATA')
      ENDIF

**    Read and interpret records until '$ENDTP' found
300   READ(10,5000,END=100,ERR=110)YOCREC
5000  FORMAT(A80)
      CALL SEARCH(YOCREC,'$ENDTP',LOFIND)
      IF(LOFIND) THEN
         WRITE(NCERR,6010)
6010     FORMAT(1X,'USER-SUPPLIED TP SELECTION DATA PROCESSED')
         RETURN
      ENDIF

      CALL TPEXTR(YOCREC)
      GO TO 300

100   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6020)
6020  FORMAT(1X,'TPPROC: EOF WHEN SEARCHING FOR TP DATA')
      CALL ERRSTP

110   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6030)
6030  FORMAT(1X,'TPPROC: ERROR WHEN SEARCHING FOR TP DATA')
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE CHKT3
**    Function - to check T3 data input
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /TRDIAG/LDT3,/T3DIAG/NCT3,/DIAGTP/LDTR,/COMDAT/
**          LASCII,/ALOWTR/NTRFLD,/ERRMSG/NCERR
**    Com changed - /T3DIAG/NT3,NT3PL,LDT3,/ERRMSG/NFATAL,NWARN
**    Params used - /PARAM1/JPNL,/PARAMS/JPTRMX
**    Called by - DIAGCK
**    Calls - none
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /ALOWTR/NTRFLD(JPTRMX)
      LOGICAL LWRITE,LASCII,LSCRAT,LUTF14,LROGR,LMASSW,LAVMSK,
     -      LAVSIG
      CHARACTER*2 YTYPSF
      CHARACTER*6 YDUM6
      CHARACTER*8 YNAME(2)
      COMMON /COMDAT/BEGDAY,ENDDAY,LWRITE,LASCII,LSCRAT,
     -      LUTF14,LROGR,NSIGFG,NFREQD,NFREQA,NFREQP,RNTAPE,
     -      NASCII,LMASSW,LAVMSK,LAVSIG,YNAME,YTYPSF,YDUM6
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      COMMON /T3DIAG/NCT3,NT3PL(JPTRMX),NT3
      LOGICAL LDTH,LDTZ,LDTM,LDTP,LDT3,LDTRTD,LDTRTS,LDTRIN
      COMMON /TRDIAG/LDTH,LDTZ,LDTM,LDTP,LDT3,LDTRTD,LDTRTS,LDTRIN,
     -      NCTR,NVTRHR(JPNL),VTRSFC(JPNL)
      LOGICAL LOERR,LOVALU

      LOERR=.FALSE.

**    Check that UTFs are requested - if not, ensure NT3PL=0
**          for all elements
      IF(.NOT.LASCII) THEN
         IF(NT3PL(1).NE.0) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6010)
6010        FORMAT(1X,'CHKT3: NT3PL VALUES CHANGED - NO O/P')
            DO 1100 K=1,JPTRMX
1100        NT3PL(K)=0
**          Reset value of NT3
            NT3=0
         ENDIF
      ENDIF

**    Check valid code numbers in NT3PL
      IF(NT3PL(1).NE.0) THEN
         DO 3000 K=1,JPTRMX
         LOVALU=.FALSE.
         DO 3100 J=1,JPTRMX
         IF((NT3PL(K).EQ.NTRFLD(J)).OR.(NT3PL(K).EQ.0)) LOVALU=.TRUE.
3100     CONTINUE
         IF(.NOT.LOVALU) THEN
            LOERR=.TRUE.
            NFATAL=NFATAL+1
            WRITE(NCERR,6030) NT3PL(K)
6030        FORMAT(1X,'CHKT3: ERROR IN NT3PL VALUE ',I4)
         ENDIF
3000     CONTINUE
      ENDIF

**    Switch off diagnostics if NT3 is zero
      IF(NT3.EQ.0.AND.LDT3) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6035)
6035     FORMAT(1X,'CHKT3: T3 DIAGNOSTICS SWITCHED OFF, NT3=0')
         LDT3=.FALSE.
         RETURN
      ENDIF
      IF(.NOT.LDT3) RETURN

**    Check that NT3 is non-zero, and less than JPTRMX
      IF(NT3.LE.0.OR.(NT3.GT.JPTRMX)) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6040)
6040     FORMAT(1X,'CHKT3 : INVALID VALUE FOR NT3')
         LOERR=.TRUE.
      ENDIF

**    Check channel for output of 3-D fields
      IF(LDT3.AND.NCT3.LE.0) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6050)
6050     FORMAT(1X,'CHKT3: ERROR - T3 DIAGNOSTICS REQUESTED BUT',
     -         ' NCT3 CHANNEL NUMBER INVALID')
         LOERR=.TRUE.
      ENDIF

CC    IF(LOERR) STOP
      RETURN
      END
      SUBROUTINE T3EXTR(YPCREC)
**    Function - to extract information from T3 data and update
**          appropriate common blocks
**    Args in -
**               YPCREC  - character*80 variable containing data
**    Args out - none
**    Args for work - none
**    Com used - /ALOWTR/NTRFLD,/ERRMSG/NCERR
**    Com changed - /T3DIAG/all elements,/ERRMSG/NFATAL
**    Params used - /PARAMS/JPTRMX
**    Called by - T3PROC
**    Calls - SEARCH,LASTCH,ITRANS,INTCPY,IMULTP,ERRSTP
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /ALOWTR/NTRFLD(JPTRMX)
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      COMMON /T3DIAG/NCT3,NT3PL(JPTRMX),NT3
      LOGICAL LOFIND,LOALL
      CHARACTER*1 YOCOM,YOEQL
      CHARACTER*4 YOKW4
      CHARACTER*9 YOKW9
      CHARACTER*10 YOKW10
      CHARACTER*14 YOKW14
      CHARACTER*80 YPCREC

      YOEQL='='
      YOCOM=','
**    Search for the keyword in YPCREC and then determine the field
**          value
      LOFIND=.FALSE.

      YOKW10='O/PCHANNEL'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
**       Note that IPOSN and IPOSX can take the same value
         IF(IERR.EQ.998) GO TO 1000
         CALL ITRANS(YPCREC,IPOSN,IPOSX,NCT3,12)
         RETURN
      ENDIF

**    Else
      YOKW9='PLOTFIELD'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            CALL INTCPY(NTRFLD,JPTRMX,NT3PL)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NT3PL,JPTRMX)
         RETURN
      ENDIF

**    Else
      YOKW14='NUMBEROFFIELDS'
      CALL SEARCH(YPCREC,YOKW14,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
**       Note that IPOSN and IPOSX can take the same value
         IF(IERR.EQ.998) GO TO 1000
         CALL ITRANS(YPCREC,IPOSN,IPOSX,NT3,12)
         RETURN
      ENDIF

**    Else
**    Keyword not located
      NFATAL=NFATAL+1
      WRITE(NCERR,6030) YPCREC
6030  FORMAT(1X,'KEYWORD NOT LOCATED FOR PHRASE :'/A)
      CALL ERRSTP

1000  CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6000) YPCREC
6000  FORMAT(1X,'T3EXTR: ERROR IN T3 DATA ENTRY',/A)
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE T3PROC
**    Function - to read and process the T3 diagnostic data
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /TRDIAG/LDT3,/DIAGTP/LDTR
**    Com changed - /T3DIAG/NCT3
**    Called by - DIAGRD
**    Calls - SEARCH,T3EXTR
**    Files read - 10
**    Files written - none
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      COMMON /T3DIAG/NCT3,NT3PL(JPTRMX),NT3
      LOGICAL LDTH,LDTZ,LDTM,LDTP,LDT3,LDTRTD,LDTRTS,LDTRIN
      COMMON /TRDIAG/LDTH,LDTZ,LDTM,LDTP,LDT3,LDTRTD,LDTRTS,LDTRIN,
     -      NCTR,NVTRHR(JPNL),VTRSFC(JPNL)
      CHARACTER*80 YOCREC
      LOGICAL LOFIND
**    Specify the defaults
      DATA NCT3/1/
      DATA NT3/0/,NT3PL/JPTRMX*0/

      IF(.NOT.(LDTR.AND.LDT3)) THEN
**       T3 diagnostics not selected - print warning
         NWARN=NWARN+1
         WRITE(NCERR,6000)
6000     FORMAT(1X,'WARNING: - T3 DIAGNOSTICS NOT SELECTED:',
     -         ' ALTHOUGH USER HAS SUPPLIED T3 INPUT DATA')
      ENDIF

**    Read and interpret records until '$ENDT3' found
300   READ(10,5000,END=100,ERR=110)YOCREC
5000  FORMAT(A80)
      CALL SEARCH(YOCREC,'$ENDT3',LOFIND)
      IF(LOFIND) THEN
         WRITE(NCERR,6010)
6010     FORMAT(1X,'USER-SUPPLIED T3 SELECTION DATA PROCESSED')
         RETURN
      ENDIF

      CALL T3EXTR(YOCREC)
      GO TO 300

100   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6020)
6020  FORMAT(1X,'T3PROC: EOF WHEN SEARCHING FOR T3 DATA')
      CALL ERRSTP

110   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6030)
6030  FORMAT(1X,'T3PROC: ERROR WHEN SEARCHING FOR T3 DATA')
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE CHKTF
**    Function - to check TF data input
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /COMDAT/BEGDAY,ENDDAY,YTYPSF,NFREQD,
**          /TFDIAG/NVTFHR,VTFSFC,/MODELC/LGCMTR,TSPD,NL,
**          /LVDIAG/OUTLEV,/TFDIAG/TFCUT,FACTTF,NTF,/ERRMSG/NCERR
**    Com changed - /DIAGTP/LDTF,/TFDIAG/NFLDTF,/ERRMSG/NFATAL,NWARN
**    Params used - /PARAM1/JPNL,/PARAMS/JPTRMX,JPSGMX,JPTFMX
**    Called by - DIAGCK
**    Calls - CHKVAL
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      PARAMETER(JPALOW=JPSGMX+JPTRMX)
      LOGICAL LWRITE,LASCII,LSCRAT,LUTF14,LROGR,LMASSW,LAVMSK,
     -      LAVSIG
      CHARACTER*2 YTYPSF
      CHARACTER*6 YDUM6
      CHARACTER*8 YNAME(2)
      COMMON /COMDAT/BEGDAY,ENDDAY,LWRITE,LASCII,LSCRAT,
     -      LUTF14,LROGR,NSIGFG,NFREQD,NFREQA,NFREQP,RNTAPE,
     -      NASCII,LMASSW,LAVMSK,LAVSIG,YNAME,YTYPSF,YDUM6
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      COMMON /LVDIAG/OUTLEV(JPNL)
      LOGICAL LGCMTR,LHISPH,LHISXP,LDRY,LDIABH,LBL,LVD
      COMMON /MODELC/TSPD,NYEAR,NDAY,NMIN,LGCMTR,NL,TMEAN(JPNL),
     -      LHISPH,LHISXP,LDRY,LDIABH,BEGDYP,LBL,LVD,AKVV,CD
      LOGICAL LDFH,LDFZ,LDFM,LDFP,LDF3,LTFTOT,LTFHP,LTFLP
      COMMON /TFDIAG/LDFH,LDFZ,LDFM,LDFP,LDF3,NCTF,NVTFHR(JPNL),
     -      VTFSFC(JPNL),TFCUT,NTF,FACTTF(2,JPTFMX),NFLDTF(4,JPTFMX),
     -      LTFTOT,LTFHP,LTFLP
      LOGICAL LOERR,LOVALU,LOEQN(JPTRMX),LOCODE
      DIMENSION ITFFLD(0:JPALOW)

      LOERR=.FALSE.

**    Switch off transient flux calculations if
**    ENDDAY is not greater than BEGDAY
      IF(ABS(ENDDAY).LE.ABS(BEGDAY)) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6100)
6100     FORMAT(1X,'CHKTF: TF CALCULATIONS SWITCHED OFF, SINCE ',
     -         'ENDDAY.LE.BEGDAY')
         LDTF=.FALSE.
      ENDIF

**    Ensure that TFCUT is not less than NFREQD/TSPD
      ZTFCUT=FLOAT(NFREQD)/TSPD
      IF((TFCUT-ZTFCUT).LT.-1E-5) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6105)
6105     FORMAT(1X,'CHKTF: TF CALCULATIONS SWITCHED OFF, SINCE ',
     -         'TFCUT.LE.NFREQD/TSPD')
         LDTF=.FALSE.
         RETURN
      ENDIF
      IF(BEGDAY.LT.-1.0) GO TO 8850

**    ISMP = no. of samples, ISMPLO = samples in each low pass,
**    ISMPHI = no. of high pass samples
      ISMP=NINT((ENDDAY-BEGDAY)*TSPD)/NFREQD+1
      ISMPLO=NINT(TFCUT*TSPD)/NFREQD
      ISMPHI=ISMP/ISMPLO
      ZEND=BEGDAY+FLOAT((ISMPHI*ISMPLO-1)*NFREQD)/TSPD

      IF(ISMP.LT.2) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6110)ISMP
6110     FORMAT(1X,'CHKTF WARNING: ONLY ',I5,' SAMPLES REQUESTED ',
     -         'FOR TRANSIENT FLUX CALCULATIONS'/
     -         ' TF DIAGNOSTICS SWITCHED OFF')
         LDTF=.FALSE.
      ENDIF
      IF(ISMPLO.LT.1) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6120)ISMPLO
6120     FORMAT(1X,'CHKTF WARNING: ',I5,' SAMPLES REQUESTED ',
     -         'FOR LOW-PASS TRANSIENT FLUX CALCULATIONS'/
     -         ' TF DIAGNOSTICS SWITCHED OFF')
         LDTF=.FALSE.
      ENDIF
      IF(ISMPHI.LT.1) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6130)ISMPHI
6130     FORMAT(1X,'CHKTF WARNING: ',I5,' SAMPLES REQUESTED ',
     -         'FOR HIGH-PASS TRANSIENT FLUX CALCULATIONS'/
     -         ' TF DIAGNOSTICS SWITCHED OFF')
         LDTF=.FALSE.
      ENDIF
      IF(ABS(ZEND-ENDDAY).GT.1.0E-5) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6140)
6140     FORMAT(1X,'CHKTF: DIAGNOSTIC PERIOD MUST BE A MULTIPLE OF ',
     -         'LOW PASS AVERAGING PERIOD'/
     -         '  TRANSIENT FLUXES SWITCHED OFF')
         LDTF=.FALSE.
      ENDIF
8850  CONTINUE

**    Switch off diagnostics if NTF is zero or too large
      IF(NTF.LE.0.AND.LDTF) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6035)
6035     FORMAT(1X,'CHKTF: TF DIAGNOSTICS SWITCHED OFF, NTF<=0')
         LDTF=.FALSE.
         RETURN
      ENDIF
      IF(NTF.GT.JPTFMX.AND.LDTF) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6036)
6036     FORMAT(1X,'CHKTF: TF DIAGNOSTICS SWITCHED OFF, NTF TOO LARGE')
         LDTF=.FALSE.
         RETURN
      ENDIF

      IF(.NOT.LDTF) RETURN

**    NVTFHR values only used when output is on model surfaces, and
**    VTFSFC values only used when output is on non-model surfaces
      IF((NVTFHR(1).NE.0).AND.(YTYPSF.NE.'ET')) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6060)
6060     FORMAT(1X,'CHKTF: NVTFHR VALUE(S) SUPPLIED, BUT NOT USED')
      ENDIF
      IF((NVTFHR(1).LE.0).AND.(YTYPSF.EQ.'ET')) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6065)
6065     FORMAT(1X,'CHKTF: NVTFHR VALUES NOT SUPPLIED,'
     -         ' OUTPUT MAY BE SUPPRESSED')
      ENDIF
      IF((VTFSFC(1).GT.0.0).AND.(YTYPSF.EQ.'ET')) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6070)
6070     FORMAT(1X,'CHKTF: VTFSFC VALUE(S) SUPPLIED, BUT NOT USED')
      ENDIF
      IF((VTFSFC(1).LE.0.0).AND.(YTYPSF.NE.'ET')) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6075)
6075     FORMAT(1X,'CHKTF: VTFSFC VALUES NOT SUPPLIED,'
     -         ' OUTPUT MAY BE SUPPRESSED')
      ENDIF

**    Check that VTFSFC values are a subset of OUTLEV values if they are
**    to be used. ZDELTA is a small parameter to allow two REAL values
**    to be compared
      ZDELTA=1.0E-10
      IF(YTYPSF.NE.'ET') THEN
         DO 4000 K=1,NL
         IF(VTFSFC(K).GT.0.0) THEN
            ZSFCP=VTFSFC(K)+ZDELTA
            ZSFCM=VTFSFC(K)-ZDELTA
            LOVALU=.FALSE.
            DO 4100 J=1,NL
            IF(ZSFCM.LE.OUTLEV(J).AND.ZSFCP.GE.OUTLEV(J)) LOVALU=.TRUE.
4100        CONTINUE
            IF(.NOT.LOVALU) THEN
               NFATAL=NFATAL+1
               WRITE(NCERR,6080) VTFSFC(K)
6080           FORMAT(1X,'CHKTF: VTFSFC CORRESPONDS TO INVALID LEVEL ',
     -               F12.3)
               LOERR=.TRUE.
            ENDIF
         ENDIF
4000     CONTINUE
      ENDIF

**    Check that NVTFHR values lie in the range 1 to NL
      IF(YTYPSF.EQ.'ET') THEN
         DO 5000 J=1,NL
         IF(NVTFHR(J).NE.0) THEN
            IF(NVTFHR(J).LT.1.OR.NVTFHR(J).GT.NL) THEN
               NFATAL=NFATAL+1
               WRITE(NCERR,6090)
6090           FORMAT(1X,'CHKTF: INVALID NVTFHR VALUE')
               LOERR=.TRUE.
            ENDIF
         ENDIF
5000     CONTINUE
      ENDIF

**    Permissible field codes are 1->14, 17->19 and
**          101->100+JPTRMX (the latter only if tracer code was used
**          in the original GCM run). A value of 0 can be used to
**          signify a null field.
      ITFFLD(0)=0
      DO 100 J=1,JPALOW
      IF(J.LE.JPSGMX) THEN
         ITFFLD(J)=J
      ELSE IF(LGCMTR) THEN
         ITFFLD(J)=100+J-JPSGMX
      ELSE
         ITFFLD(J)=0
      ENDIF
      IF((J.EQ.15).OR.(J.EQ.16).OR.(J.EQ.20).OR.(J.EQ.21)) ITFFLD(J)=0
100   CONTINUE

**    Now check each formula in turn, and determine its validity
      DO 200 J=1,JPTFMX
      LOEQN(J)=.TRUE.
**    First multiplicative factor
      IF((ABS(FACTTF(1,J)).LT.1.0E-10).AND.((NFLDTF(1,J).NE.0).OR.
     -      (NFLDTF(2,J).NE.0))) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6200) J
6200     FORMAT(1X,'CHKTF ERROR: FIRST FACTOR OF EQN ',I2,' IS ZERO')
         LOERR=.TRUE.
         LOEQN(J)=.FALSE.
         NFLDTF(1,J)=0
         NFLDTF(2,J)=0
      ENDIF
**    Second multiplicative factor
      IF((ABS(FACTTF(2,J)).LT.1.0E-10).AND.((NFLDTF(3,J).NE.0).OR.
     -      (NFLDTF(4,J).NE.0))) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6210) J
6210     FORMAT(1X,'CHKTF ERROR: SECOND FACTOR OF EQN ',I2,' IS ZERO')
         LOERR=.TRUE.
         LOEQN(J)=.FALSE.
         NFLDTF(3,J)=0
         NFLDTF(4,J)=0
      ENDIF
**    If both factors are zero, equation is invalid
      IF(ABS(FACTTF(1,J)).LT.1.0E-10.AND.
     -      ABS(FACTTF(2,J)).LT.1.0E-10) THEN
         LOEQN(J)=.FALSE.
         GO TO 200
      ENDIF
**    Check each field code
      CALL CHKVAL(LOCODE,NFLDTF(1,J),4,ITFFLD(0),JPALOW+1)
      IF(.NOT.LOCODE) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6220)J,NFLDTF(1,J),NFLDTF(2,J),NFLDTF(3,J),
     -         NFLDTF(4,J)
6220     FORMAT(1X,'CHKTF ERROR: EQN ',I2,' INVALID FIELD CODE ',
     -         /'ONE OF THE FOLLOWING IS INCORRECT ',4I5)
         LOERR=.TRUE.
         NFLDTF(1,J)=0
         NFLDTF(2,J)=0
         NFLDTF(3,J)=0
         NFLDTF(4,J)=0
         LOEQN(J)=.FALSE.
      ENDIF
200   CONTINUE
**    Finally, check validity of first NTF equations
      IVALID=0
      DO 300 J=1,NTF
      IF(LOEQN(J)) IVALID=IVALID+1
300   CONTINUE
      IF(NTF.NE.IVALID) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6300)
6300     FORMAT(1X,'CHKTF ERROR: EITHER NTF IS WRONG, OR ONE OF THE ',
     -         'FIRST NTF EQNS IS INVALID')
         LOERR=.TRUE.
      ENDIF

CC    IF(LOERR) STOP
      RETURN
      END
      SUBROUTINE DCODTF(YPCREC,YPEQL,YPCOM,KFLDTF,KDIM1,KDIM2,PFACT,
     -      KDIM3)
**    Function - to extract information from TF equation data and
**          update appropriate common blocks. Although originally
**          written for the TF diagnostics, the code is also valid for
**          the ZF diagnostics.
**    Args in -
**               YPCREC  - character*80 variable containing data
**               YPEQL   - character*1 variable containing '='
**               YPCOM   - character*1 variable containing ','
**               KDIM1   - first dimension of KFLDTF
**               KDIM2   - second dimension of KFLDTF and PFACT
**               KDIM3   - first dimension of PFACT
**    Args out -
**               KFLDTF  - array containing field codes of fields
**                         contained in transient flux eqns or in the
**                         zonal flux eqns.
**               PFACT   - array containing multiplicative factors
**                         contained in transient flux eqns or in the
**                         zonal flux eqns.
**    Args for work - none
**    Com used - /ERRMSG/NCERR
**    Com changed - /ERRMSG/NFATAL
**    Params used - none
**    Called by - TFEXTR,ZFEXTR
**    Calls - LASTCH,SDCDTF,FRSTCH,INVALD,ERRSTP
**    Files read - 10
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      CHARACTER*(1) YPEQL,YPCOM
      CHARACTER*(80) YPCREC
      LOGICAL LOCONT,LOINTG
      DIMENSION KFLDTF(KDIM1,KDIM2),PFACT(KDIM3,KDIM2)

**    I1 is the location of first character defining field value
      I1=INDEX(YPCREC,YPEQL)+1
**    IFIELD - number of data equations read in so far
      IFIELD=0
**    I2 is the location of last character defining field value
300   CALL LASTCH(YPCREC,80,I1,I2,IERR)
      IF(IERR.EQ.998) GO TO 1000
**    If the last character is a comma - expect a continuation line
      IF(YPCREC(I2:I2).EQ.YPCOM) THEN
         LOCONT=.TRUE.
      ELSE
         LOCONT=.FALSE.
      ENDIF
      IF(.NOT.LOCONT) THEN
**       Decode a single line of data
         CALL SDCDTF(YPCREC,I1,I2,YPCOM,IFIELD,KFLDTF,KDIM1,KDIM2,
     -         PFACT,KDIM3,KDIM2)
         RETURN
      ELSE
**       Decode one of several data lines
         CALL SDCDTF(YPCREC,I1,I2-1,YPCOM,IFIELD,KFLDTF,KDIM1,KDIM2,
     -         PFACT,KDIM3,KDIM2)
**       Read the next line of data
         READ(10,5000,END=100,ERR=110)YPCREC
5000     FORMAT(A80)
**       See if the first non-blank character of YPCREC is an integer
         CALL FRSTCH(YPCREC,80,I1,LOINTG)
         IF(LOINTG) THEN
            GO TO 300
         ELSE
            IF(I1.EQ.0) THEN
               CALL INVALD(YPCREC)
            ELSE
**             See if the first non-blank character of YPCREC is + or -
               IF((YPCREC(I1:I1).EQ.'+').OR.(YPCREC(I1:I1).EQ.'-')) THEN
                  GO TO 300
               ELSE
                  CALL INVALD(YPCREC)
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      RETURN

1000  CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6000) YPCREC
6000  FORMAT(1X,'DCODTF: ERROR IN DATA ENTRY',/A)
      CALL ERRSTP

100   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6010)
6010  FORMAT(1X,'DCODTF: EOF WHEN SEARCHING FOR DATA')
      CALL ERRSTP

110   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6020)
6020  FORMAT(1X,'DCODTF: ERROR WHEN SEARCHING FOR DATA')
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE SDCDTF(YPCREC,K1,K2,YPCOMA,KFIELD,KFLDTF,KDIM1,KDIM2,
     -      PFACT,KDIM3,KDIM4)
**    Function - to decode a single record of transient flux field
**          formulae or zonal mean flux field.
**    Args in -
**               YPCREC   - character*80 variable
**               K1,K2    - data stored from locations K1 to K2 in
**                          YPCREC - both locations contain non-comma
**               YPCOMA   - character element used to delimit values in
**                          YPCREC
**               KFIELD   - number of fields of array KFLDTF and PFACT
**                          determined before call to SDCDTF
**               KFLDTF   - array containing integer field codes
**               PFACT    - array containing multiplicative factors
**               KXDIM    - dimension of PX
**    Args out -
**               KFIELD   - number of elements of array PX filled
**                          upon exit from RTRANM
**               KFLDTF   - data from YPCREC has been added
**               PFACT    - data from YPCREC has been added
**    Args for work - none
**    Com used - /ERRMSG/NCERR
**    Com changed - /ERRMSG/NFATAL,NWARN
**    Called by - DCODTF
**    Calls - ERRSTP
**    Files read - 12
**    Files written - 12,NCERR
**    Author - R. Brugge, University of Reading

      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      CHARACTER*(*) YPCREC
      CHARACTER*(1) YPCOMA
      DIMENSION IXPOS(4)
      DIMENSION KFLDTF(KDIM1,KDIM2),PFACT(KDIM3,KDIM2)

**    Check dimensions
      IF((KDIM1.NE.4).OR.(KDIM3.NE.2)) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6000) KDIM1,KDIM3
6000     FORMAT(1X,'SDCDTF ERROR: KDIM1 AND/OR KDIM3 INCORRECT.',
     -         /'CURRENTLY SET TO ',I4,I4,
     -         ' SHOULD BE SET TO 4 AND 2 RESPECTIVELY')
         CALL ERRSTP
      ENDIF

**    Define start and end locations of current formula within record
      ISTART=K1
150   CONTINUE
      IF(ISTART.GE.K2) THEN
**       No more data to read
         RETURN
      ENDIF
      IF(KFIELD.GE.KDIM2) THEN
**       Enough data already extracted
         NWARN=NWARN+1
         WRITE(NCERR,6010)
6010     FORMAT(1X,'SDCDTF -- POSSIBLE ERROR : TOO MUCH DATA TO'
     -         ' EXTRACT')
         RETURN
      ENDIF
**    Locate next comma
      ICC=INDEX(YPCREC(ISTART:K2),YPCOMA)
      IC=ISTART-1+ICC
      IF(ICC.EQ.0) THEN
**       Comma not found
         IEND=K2
      ELSE
         IEND=IC-1
      ENDIF

**    Extract data
      KFIELD=KFIELD+1
**    A valid of string of data must contain either 2 or 4 X's
      ICNTX=0
      DO 110 J=1,4
      IXPOS(J)=0
110   CONTINUE
      DO 100 J=ISTART,IEND
      IF(YPCREC(J:J).EQ.'X') THEN
         ICNTX=ICNTX+1
         IXPOS(ICNTX)=J
      ENDIF
100   CONTINUE
      IF(ICNTX.NE.2.AND.ICNTX.NE.4) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6020) ICNTX,YPCREC
6020     FORMAT(1X,'SDCDTF ERROR: CURRENT FORMULA CONTAINS ',I4,
     -         ' UPPER-CASE MULTIPLICATION SIGNS -- SHOULD BE 2 OR 4'/
     -         A80)
         CALL ERRSTP
      ENDIF

**    Now work backwards along formula and retrieve data
      IUNIT=12
      REWIND IUNIT
      IF(ICNTX.EQ.4) THEN
**       Fourth field code number - positions IXPOS(4)+1:IEND
         WRITE(IUNIT,6600) YPCREC(IXPOS(4)+1:IEND)
6600     FORMAT(A)
         REWIND IUNIT
         READ(IUNIT,*) IXOUT
         REWIND IUNIT
         KFLDTF(4,KFIELD)=IXOUT
**       Third field code number - positions IXPOS(3)+1:IXPOS(4)-1
         WRITE(IUNIT,6600) YPCREC(IXPOS(3)+1:IXPOS(4)-1)
         REWIND IUNIT
         READ(IUNIT,*) IXOUT
         REWIND IUNIT
         KFLDTF(3,KFIELD)=IXOUT
**       Now search for a + or -, indicating start of second factor
         II=IXPOS(3)-1
200      CONTINUE
         IF(II.EQ.IXPOS(2)) GO TO 7000
         IF((YPCREC(II:II).EQ.'+').OR.(YPCREC(II:II).EQ.'-')) THEN
            ISGN=II
         ELSE
            II=II-1
            GO TO 200
         ENDIF
**       Second factor - positions ISGN:IXPOS(3)-1
         WRITE(IUNIT,6600) YPCREC(ISGN:IXPOS(3)-1)
         REWIND IUNIT
         READ(IUNIT,*) ZXOUT
         REWIND IUNIT
         PFACT(2,KFIELD)=ZXOUT
**       Second field code number - positions IXPOS(2)+1:ISGN-1
         WRITE(IUNIT,6600) YPCREC(IXPOS(2)+1:ISGN-1)
         REWIND IUNIT
         READ(IUNIT,*) IXOUT
         REWIND IUNIT
         KFLDTF(2,KFIELD)=IXOUT
      ELSE
**       Second field code number - positions IXPOS(2)+1:IEND
         WRITE(IUNIT,6600) YPCREC(IXPOS(2)+1:IEND)
         REWIND IUNIT
         READ(IUNIT,*) IXOUT
         REWIND IUNIT
         KFLDTF(2,KFIELD)=IXOUT
      ENDIF
**    First field code number - positions IXPOS(1)+1:IXPOS(2)-1
      WRITE(IUNIT,6600) YPCREC(IXPOS(1)+1:IXPOS(2)-1)
      REWIND IUNIT
      READ(IUNIT,*) IXOUT
      REWIND IUNIT
      KFLDTF(1,KFIELD)=IXOUT
**    First factor - positions ISTART:IXPOS(1)-1
      WRITE(IUNIT,6600) YPCREC(ISTART:IXPOS(1)-1)
      REWIND IUNIT
      READ(IUNIT,*) ZXOUT
      REWIND IUNIT
      PFACT(1,KFIELD)=ZXOUT

**    Update start location
      IF((ICC.EQ.0).OR.(ICC.EQ.K2)) THEN
**       No more data to read
         RETURN
      ELSE
         ISTART=IC+1
         GO TO 150
      ENDIF

7000  CONTINUE
**    Error when searching for start of second factor
      NFATAL=NFATAL+1
      WRITE(NCERR,6400)YPCREC
6400  FORMAT(1X,'SDCDTF ERROR: SECOND FACTOR IN EQN NOT FOUND :'/
     -      A80)
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE TFEXTR(YPCREC)
**    Function - to extract information from TF data and update
**          appropriate common blocks
**    Args in -
**               YPCREC  - character*80 variable containing data
**    Args out - none
**    Args for work - none
**    Com used - /ERRMSG/NCERR,/MODELC/NL
**    Com changed - /TFDIAG/all elements,/ERRMSG/NFATAL
**    Params used - /PARAM1/JPNL,/PARAMS/JPTFMX
**    Called by - TFPROC
**    Calls - SEARCH,LPROC,INTCPY,IMULTP,LASTCH,ITRANS,RMULTP,
**          RTRANS,DCODTF,ERRSTP
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LGCMTR,LHISPH,LHISXP,LDRY,LDIABH,LBL,LVD
      COMMON /MODELC/TSPD,NYEAR,NDAY,NMIN,LGCMTR,NL,TMEAN(JPNL),
     -      LHISPH,LHISXP,LDRY,LDIABH,BEGDYP,LBL,LVD,AKVV,CD
      LOGICAL LDFH,LDFZ,LDFM,LDFP,LDF3,LTFTOT,LTFHP,LTFLP
      COMMON /TFDIAG/LDFH,LDFZ,LDFM,LDFP,LDF3,NCTF,NVTFHR(JPNL),
     -      VTFSFC(JPNL),TFCUT,NTF,FACTTF(2,JPTFMX),NFLDTF(4,JPTFMX),
     -      LTFTOT,LTFHP,LTFLP
      LOGICAL LOFIND,LOALL
      DIMENSION INLFLD(JPNL)
      CHARACTER*1 YOCOM,YOEQL
      CHARACTER*4 YOKW4
      CHARACTER*5 YOKW5
      CHARACTER*6 YOKW6
      CHARACTER*7 YOKW7
      CHARACTER*8 YOKW8
      CHARACTER*9 YOKW9
      CHARACTER*10 YOKW10
      CHARACTER*14 YOKW14
      CHARACTER*80 YPCREC

      YOEQL='='
      YOCOM=','
**    Search for the keyword in YPCREC and then determine the field
**          value
      LOFIND=.FALSE.

      YOKW9='TOTALFLUX'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LTFTOT)
         RETURN
      ENDIF

      YOKW8='HIGHPASS'
      CALL SEARCH(YPCREC,YOKW8,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LTFHP)
         RETURN
      ENDIF

      YOKW7='LOWPASS'
      CALL SEARCH(YPCREC,YOKW7,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LTFLP)
         RETURN
      ENDIF

      YOKW10='HORIZONTAL'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDFH)
         RETURN
      ENDIF

**    Else
      YOKW5='ZONAL'
      CALL SEARCH(YPCREC,YOKW5,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDFZ)
         RETURN
      ENDIF

**    Else
      YOKW10='MERIDIONAL'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDFM)
         RETURN
      ENDIF

**    Else
      YOKW7='PROFILE'
      CALL SEARCH(YPCREC,YOKW7,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDFP)
         RETURN
      ENDIF

**    Else
      YOKW8='3DOUTPUT'
      CALL SEARCH(YPCREC,YOKW8,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDF3)
         RETURN
      ENDIF

**    Else
      YOKW10='O/PCHANNEL'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
**       Note that IPOSN and IPOSX can take the same value
         IF(IERR.EQ.998) GO TO 1000
         CALL ITRANS(YPCREC,IPOSN,IPOSX,NCTF,12)
         RETURN
      ENDIF

**    Else
      YOKW8='TFLEVELS'
      CALL SEARCH(YPCREC,YOKW8,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            DO 3500 JL=1,NL
3500        INLFLD(JL)=JL
            CALL INTCPY(INLFLD,NL,NVTFHR)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NVTFHR,NL)
         RETURN
      ENDIF

**    Else
      YOKW10='TFSURFACES'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
      IF(LOFIND) THEN
         CALL RMULTP(YPCREC,YOEQL,YOCOM,VTFSFC,NL)
         RETURN
      ENDIF

**    Else
      YOKW6='CUTOFF'
      CALL SEARCH(YPCREC,YOKW6,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
         IF(IERR.NE.0) GO TO 1000
         CALL RTRANS(YPCREC,IPOSN,IPOSX,TFCUT,12)
         RETURN
      ENDIF

**    Else
      YOKW14='NUMBEROFFIELDS'
      CALL SEARCH(YPCREC,YOKW14,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
**       Note that IPOSN and IPOSX can take the same value
         IF(IERR.EQ.998) GO TO 1000
         CALL ITRANS(YPCREC,IPOSN,IPOSX,NTF,12)
         RETURN
      ENDIF

**    Else
      YOKW7='FORMULA'
      CALL SEARCH(YPCREC,YOKW7,LOFIND)
      IF(LOFIND) THEN
         CALL DCODTF(YPCREC,YOEQL,YOCOM,NFLDTF,4,JPTFMX,FACTTF,2)
         RETURN
      ENDIF

**    Else
**    Keyword not located
      NFATAL=NFATAL+1
      WRITE(NCERR,6030) YPCREC
6030  FORMAT(1X,'KEYWORD NOT LOCATED FOR PHRASE :'/A)
      CALL ERRSTP

1000  CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6000) YPCREC
6000  FORMAT(1X,'TFEXTR: ERROR IN TF DATA ENTRY',/A)
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE TFPROC
**    Function - to read and process the TF diagnostic data
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /DIAGTP/LDTF,/ERRMSG/NCERR,NWARN
**    Com changed - /TFDIAG/all elements,/ERRMSG/NFATAL
**    Called by - DIAGRD
**    Calls - SEARCH,TFEXTR,ERRSTP
**    Files read - 10
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      PARAMETER(JPFACT=2*JPTFMX,JPFLD=4*JPTFMX)
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDFH,LDFZ,LDFM,LDFP,LDF3,LTFTOT,LTFHP,LTFLP
      COMMON /TFDIAG/LDFH,LDFZ,LDFM,LDFP,LDF3,NCTF,NVTFHR(JPNL),
     -      VTFSFC(JPNL),TFCUT,NTF,FACTTF(2,JPTFMX),NFLDTF(4,JPTFMX),
     -      LTFTOT,LTFHP,LTFLP
      CHARACTER*80 YOCREC
      LOGICAL LOFIND
**    Specify the defaults
      DATA NCTF/3/
      DATA LDFH/.FALSE./,LDFZ/.TRUE./,LDFM/.FALSE./
      DATA LDFP/.FALSE./,LDF3/.FALSE./
      DATA NVTFHR/JPNL*0/,VTFSFC/JPNL*0.0/
      DATA TFCUT/3.0/,NTF/0/
      DATA LTFTOT/.TRUE./,LTFHP/.TRUE./,LTFLP/.TRUE./
      DATA FACTTF/JPFACT*0.0/,NFLDTF/JPFLD*0/

      IF(.NOT.LDTF) THEN
**       TF diagnostics not selected - print warning
         NWARN=NWARN+1
         WRITE(NCERR,6000)
6000     FORMAT(1X,'WARNING: - TF DIAGNOSTICS NOT SELECTED:',
     -         ' ALTHOUGH USER HAS SUPPLIED TF INPUT DATA')
      ENDIF

**    Read and interpret records until '$ENDTF' found
300   READ(10,5000,END=100,ERR=110)YOCREC
5000  FORMAT(A80)
      CALL SEARCH(YOCREC,'$ENDTF',LOFIND)
      IF(LOFIND) THEN
         WRITE(NCERR,6010)
6010     FORMAT(1X,'USER-SUPPLIED TF SELECTION DATA PROCESSED')
         RETURN
      ENDIF

      CALL TFEXTR(YOCREC)
      GO TO 300

100   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6020)
6020  FORMAT(1X,'TFPROC: EOF WHEN SEARCHING FOR TF DATA')
      CALL ERRSTP

110   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6030)
6030  FORMAT(1X,'TFPROC: ERROR WHEN SEARCHING FOR TF DATA')
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE CHKFH
**    Function - to check FH data input
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /TFDIAG/LDTF,NTF,/COMDAT/LWRITE,LASCII,/DIAGTP
**          LDTF,/ERRMSG/NCERR
**    Com changed - /FHDIAG/NFHPR,NFHPL,/TFDIAG/LDFH,
**          /ERRMSG/NWARN,NFATAL
**    Params used - /PARAM1/JPNL,/PARAMS/JPTRMX
**    Called by - DIAGCK
**    Calls - none
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      LOGICAL LWRITE,LASCII,LSCRAT,LUTF14,LROGR,LMASSW,LAVMSK,
     -      LAVSIG
      CHARACTER*2 YTYPSF
      CHARACTER*6 YDUM6
      CHARACTER*8 YNAME(2)
      COMMON /COMDAT/BEGDAY,ENDDAY,LWRITE,LASCII,LSCRAT,
     -      LUTF14,LROGR,NSIGFG,NFREQD,NFREQA,NFREQP,RNTAPE,
     -      NASCII,LMASSW,LAVMSK,LAVSIG,YNAME,YTYPSF,YDUM6
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDFHSL
      COMMON /FHDIAG/NFHPR(JPTFMX),NFHPL(JPTFMX),CINTFH(JPTFMX),LDFHSL
      LOGICAL LDFH,LDFZ,LDFM,LDFP,LDF3,LTFTOT,LTFHP,LTFLP
      COMMON /TFDIAG/LDFH,LDFZ,LDFM,LDFP,LDF3,NCTF,NVTFHR(JPNL),
     -      VTFSFC(JPNL),TFCUT,NTF,FACTTF(2,JPTFMX),NFLDTF(4,JPTFMX),
     -      LTFTOT,LTFHP,LTFLP
      LOGICAL LOERR,LOVALU

      LOERR=.FALSE.

**    TF/FH diagnostics require that LDTF and LDFH are set to .TRUE.
      IF((.NOT.LDTF).OR.(.NOT.LDFH)) THEN
         IF(LDFHSL) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6000)
6000        FORMAT(1X,'CHKFH: WARNING - FH DIAGNOSTICS SWITCHED OFF',
     -            ' HORIZONTAL LDTF/LDFH FLAGS NOT BOTH SET')
            RETURN
         ENDIF
      ENDIF
      IF((.NOT.LDTF).OR.(.NOT.LDFH)) THEN
         LDFH=.FALSE.
         RETURN
      ENDIF

**    Check that printing has been requested - if not, ensure
**          NFHPR=0 for all elements
      IF(.NOT.LWRITE) THEN
         IF(NFHPR(1).NE.0) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6005)
6005        FORMAT(1X,'CHKFH: NFHPR VALUES CHANGED - NO PRINTING')
            DO 1000 K=1,JPTFMX
1000        NFHPR(K)=0
         ENDIF
      ENDIF

**    Check that UTFs are requested - if not, ensure NFHPL=0
**          for all elements
      IF(.NOT.LASCII) THEN
         IF(NFHPL(1).NE.0) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6010)
6010        FORMAT(1X,'CHKFH: NFHPL VALUES CHANGED - NO PLOTTING')
            DO 1100 K=1,JPTFMX
1100        NFHPL(K)=0
         ENDIF
      ENDIF

**    Check valid code numbers in NFHPR
      IF(NFHPR(1).NE.0) THEN
         DO 2000 K=1,NTF
         LOVALU=.FALSE.
         IF(NFHPR(K).LE.NTF.AND.NFHPR(K).GE.0) LOVALU=.TRUE.
         IF(.NOT.LOVALU) THEN
            NFATAL=NFATAL+1
            LOERR=.TRUE.
            WRITE(NCERR,6020) NFHPR(K)
6020        FORMAT(1X,'CHKFH: ERROR IN NFHPR VALUE ',I4)
         ENDIF
2000     CONTINUE
      ENDIF

**    Check valid code numbers in NFHPL
      IF(NFHPL(1).NE.0) THEN
         DO 3000 K=1,NTF
         LOVALU=.FALSE.
         IF(NFHPL(K).LE.NTF.AND.NFHPL(K).GE.0) LOVALU=.TRUE.
         IF(.NOT.LOVALU) THEN
            NFATAL=NFATAL+1
            LOERR=.TRUE.
            WRITE(NCERR,6030) NFHPL(K)
6030        FORMAT(1X,'CHKFH: ERROR IN NFHPL VALUE ',I4)
         ENDIF
3000     CONTINUE
      ENDIF

      IF(NTF.LT.JPTFMX) THEN
         DO 2001 J=NTF+1,JPTFMX
         NFHPL(J)=0
         NFHPR(J)=0
2001     CONTINUE
      ENDIF

      IF(NFHPL(1).EQ.0.AND.NFHPR(1).EQ.0) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6040)
6040     FORMAT(1X,'CHKFH: FH DIAGNOSTICS SWITCHED OFF, NO FIELDS',
     -         ' SELECTED FOR PRINT OR PLOT')
         LDFH=.FALSE.
      ENDIF

CC    IF(LOERR) STOP
      RETURN
      END
      SUBROUTINE FHEXTR(YPCREC)
**    Function - to extract information from FH data and update
**          appropriate common blocks
**    Args in -
**               YPCREC  - character*80 variable containing data
**    Args out - none
**    Args for work - none
**    Com used - /ERRMSG/NCERR
**    Com changed - /FHDIAG/all elements,/ERRMSG/NFATAL
**    Params used - /PARAMS/JPTFMX
**    Called by - THPROC
**    Calls - SEARCH,LPROC,RMULTP,IMULTP,INTCPY
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDFHSL
      COMMON /FHDIAG/NFHPR(JPTFMX),NFHPL(JPTFMX),CINTFH(JPTFMX),LDFHSL
      LOGICAL LOFIND,LOALL
      CHARACTER*1 YOCOM,YOEQL
      CHARACTER*2 YOKW2
      CHARACTER*4 YOKW4
      CHARACTER*5 YOKW5
      CHARACTER*9 YOKW9
      CHARACTER*10 YOKW10
      CHARACTER*80 YPCREC
      DIMENSION ITFFLD(JPTFMX)

      DO 100 J=1,JPTFMX
      ITFFLD(J)=J
100   CONTINUE

      YOEQL='='
      YOCOM=','
**    Search for the keyword in YPCREC and then determine the field
**          value
      LOFIND=.FALSE.

      YOKW2='CI'
      CALL SEARCH(YPCREC,YOKW2,LOFIND)
      IF(LOFIND) THEN
         CALL RMULTP(YPCREC,YOEQL,YOCOM,CINTFH,JPTFMX)
         RETURN
      ENDIF

**    Else
      YOKW5='SLICE'
      CALL SEARCH(YPCREC,YOKW5,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDFHSL)
         RETURN
      ENDIF

**    Else
      YOKW10='PRINTFIELD'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            CALL INTCPY(ITFFLD,JPTFMX,NFHPR)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NFHPR,JPTFMX)
         RETURN
      ENDIF

**    Else
      YOKW9='PLOTFIELD'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            CALL INTCPY(ITFFLD,JPTFMX,NFHPL)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NFHPL,JPTFMX)
         RETURN
      ENDIF

**    Else
**    Keyword not located
      NFATAL=NFATAL+1
      WRITE(NCERR,6030) YPCREC
6030  FORMAT(1X,'KEYWORD NOT LOCATED FOR PHRASE :'/A)
      CALL ERRSTP

1000  CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6000) YPCREC
6000  FORMAT(1X,'FHEXTR: ERROR IN FH DATA ENTRY',/A)
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE FHPROC
**    Function - to read and process the FH diagnostic data
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /TFDIAG/LDFH,/DIAGTP/LDTF,/ERRMSG/NCERR
**    Com changed - /FHDIAG/all elements,/ERRMSG/NFATAL,NWARN
**    Called by - DIAGRD
**    Calls - SEARCH,THEXTR,ERRSTP
**    Files read - 10
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDFHSL
      COMMON /FHDIAG/NFHPR(JPTFMX),NFHPL(JPTFMX),CINTFH(JPTFMX),LDFHSL
      LOGICAL LDFH,LDFZ,LDFM,LDFP,LDF3,LTFTOT,LTFHP,LTFLP
      COMMON /TFDIAG/LDFH,LDFZ,LDFM,LDFP,LDF3,NCTF,NVTFHR(JPNL),
     -      VTFSFC(JPNL),TFCUT,NTF,FACTTF(2,JPTFMX),NFLDTF(4,JPTFMX),
     -      LTFTOT,LTFHP,LTFLP
      CHARACTER*80 YOCREC
      LOGICAL LOFIND
**    Specify the defaults
      PARAMETER(RNEG=-0.1)
      DATA LDFHSL/.FALSE./
      DATA NFHPR/JPTFMX*0/,NFHPL/JPTFMX*0/
      DATA CINTFH/JPTFMX*RNEG/

      IF(.NOT.(LDTF.AND.LDFH)) THEN
**       FH diagnostics not selected - print warning
         NWARN=NWARN+1
         WRITE(NCERR,6000)
6000     FORMAT(1X,'WARNING: - FH DIAGNOSTICS NOT SELECTED:',
     -         ' ALTHOUGH USER HAS SUPPLIED FH INPUT DATA')
      ENDIF

**    Read and interpret records until '$ENDFH' found
300   READ(10,5000,END=100,ERR=110)YOCREC
5000  FORMAT(A80)
      CALL SEARCH(YOCREC,'$ENDFH',LOFIND)
      IF(LOFIND) THEN
         WRITE(NCERR,6010)
6010     FORMAT(1X,'USER-SUPPLIED FH SELECTION DATA PROCESSED')
         RETURN
      ENDIF

      CALL FHEXTR(YOCREC)
      GO TO 300

100   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6020)
6020  FORMAT(1X,'FHPROC: EOF WHEN SEARCHING FOR FH DATA')
      CALL ERRSTP

110   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6030)
6030  FORMAT(1X,'FHPROC: ERROR WHEN SEARCHING FOR FH DATA')
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE CHKFZ
**    Function - to check FZ data input
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /TFDIAG/NTF,/FZDIAG/VFZNS,VFZNSA,
**          /DIAGTP/LDTF,/COMDAT/LWRITE,LASCII,/ERRMSG/NCERR
**    Com changed - /FZDIAG/NFZPR,NFZPL,/TFDIAG/LDFZ,
**          /ERRMSG/NFATAL,NWARN
**    Params used - /PARAM1/JPNL,/PARAMS/JPTFMX
**    Called by - DIAGCK
**    Calls - LNGCHK
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      LOGICAL LWRITE,LASCII,LSCRAT,LUTF14,LROGR,LMASSW,LAVMSK,
     -      LAVSIG
      CHARACTER*2 YTYPSF
      CHARACTER*6 YDUM6
      CHARACTER*8 YNAME(2)
      COMMON /COMDAT/BEGDAY,ENDDAY,LWRITE,LASCII,LSCRAT,
     -      LUTF14,LROGR,NSIGFG,NFREQD,NFREQA,NFREQP,RNTAPE,
     -      NASCII,LMASSW,LAVMSK,LAVSIG,YNAME,YTYPSF,YDUM6
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDFZZM
      COMMON /FZDIAG/LDFZZM,VFZNS,VFZNSA(2),NFZPR(JPTFMX),
     -      NFZPL(JPTFMX),CINTFZ(JPTFMX)
      LOGICAL LDFH,LDFZ,LDFM,LDFP,LDF3,LTFTOT,LTFHP,LTFLP
      COMMON /TFDIAG/LDFH,LDFZ,LDFM,LDFP,LDF3,NCTF,NVTFHR(JPNL),
     -      VTFSFC(JPNL),TFCUT,NTF,FACTTF(2,JPTFMX),NFLDTF(4,JPTFMX),
     -      LTFTOT,LTFHP,LTFLP
      LOGICAL LOERR,LOVALU

      LOERR=.FALSE.

**    TF/FZ diagnostics require that LDTF and LDFZ are set to .TRUE.
      IF((.NOT.LDTF).OR.(.NOT.LDFZ)) THEN
         IF((LDFZZM).OR.(VFZNS.GT.0.0).OR.
     -         (VFZNSA(1).GT.0.0).OR.(VFZNSA(2).GT.0.0)) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6000)
6000        FORMAT(1X,'CHKFZ: WARNING - FZ DIAGNOSTICS SWITCHED OFF,',
     -            ' LDTF/LDTZ FLAGS NOT BOTH SET')
            RETURN
         ENDIF
      ENDIF
      IF((.NOT.LDTF).OR.(.NOT.LDFZ)) THEN
         LDFZ=.FALSE.
         RETURN
      ENDIF

**    Check that printing has been requested - if not, ensure
**          NFZPR=0 for all elements
      IF(.NOT.LWRITE) THEN
         IF(NFZPR(1).NE.0) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6005)
6005        FORMAT(1X,'CHKFZ: NFZPR VALUES CHANGED - NO PRINTING')
            DO 1000 K=1,JPTFMX
1000        NFZPR(K)=0
         ENDIF
      ENDIF

**    Check that UTFs are requested - if not, ensure NFZPL=0
**          for all elements
      IF(.NOT.LASCII) THEN
         IF(NFZPL(1).NE.0) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6010)
6010        FORMAT(1X,'CHKFZ: NFZPL VALUES CHANGED - NO PLOTTING')
            DO 1100 K=1,JPTFMX
1100        NFZPL(K)=0
         ENDIF
      ENDIF

**    Check valid code numbers in NFZPR
      IF(NFZPR(1).NE.0) THEN
         DO 2000 K=1,NTF
         LOVALU=.FALSE.
         IF(NFZPR(K).LE.NTF.AND.NFZPR(K).GE.0) LOVALU=.TRUE.
         IF(.NOT.LOVALU) THEN
            NFATAL=NFATAL+1
            LOERR=.TRUE.
            WRITE(NCERR,6020) NFZPR(K)
6020        FORMAT(1X,'CHKFZ: ERROR IN NFZPR VALUE ',I4)
         ENDIF
2000     CONTINUE
      ENDIF

**    Check valid code numbers in NFZPL
      IF(NFZPL(1).NE.0) THEN
         DO 3000 K=1,NTF
         LOVALU=.FALSE.
         IF(NFZPL(K).LE.NTF.AND.NFZPL(K).GE.0) LOVALU=.TRUE.
         IF(.NOT.LOVALU) THEN
            NFATAL=NFATAL+1
            LOERR=.TRUE.
            WRITE(NCERR,6030) NFZPL(K)
6030        FORMAT(1X,'CHKFZ: ERROR IN NFZPL VALUE ',I4)
         ENDIF
3000     CONTINUE
      ENDIF

      IF(NTF.LT.JPTFMX) THEN
         DO 2001 J=NTF+1,JPTFMX
         NFZPL(J)=0
         NFZPR(J)=0
2001     CONTINUE
      ENDIF

      IF(NFZPL(1).EQ.0.AND.NFZPR(1).EQ.0) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6040)
6040     FORMAT(1X,'CHKFZ: FZ DIAGNOSTICS SWITCHED OFF, NO FIELDS',
     -         ' SELECTED FOR PRINT OR PLOT')
         LDFZ=.FALSE.
      ENDIF

**    Check validity of VFZNS AND VFZNSA values
      CALL LNGCHK(VFZNS,1,-999.0,'(TF) FZ VFZNS',LOERR)
      CALL LNGCHK(VFZNSA,2,-999.0,'(TF) FZ VFZNSA',LOERR)

CC    IF(LOERR) STOP
      RETURN
      END
      SUBROUTINE FZEXTR(YPCREC)
**    Function - to extract information from FZ data and update
**          appropriate common blocks
**    Args in -
**               YPCREC  - character*80 variable containing data
**    Args out - none
**    Args for work - none
**    Com used - /ERRMSG/NCERR
**    Com changed - /FZDIAG/all elements,/ERRMSG/NFATAL
**    Params used - /PARAMS/JPTFMX
**    Called by - FZPROC
**    Calls - SEARCH,LPROC,RMULTP,LASTCH,RTRANS,IMULTP,INTCPY,
**          ERRSTP
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDFZZM
      COMMON /FZDIAG/LDFZZM,VFZNS,VFZNSA(2),NFZPR(JPTFMX),
     -      NFZPL(JPTFMX),CINTFZ(JPTFMX)
      LOGICAL LOFIND,LOALL
      CHARACTER*1 YOCOM,YOEQL
      CHARACTER*2 YOKW2
      CHARACTER*4 YOKW4
      CHARACTER*7 YOKW7
      CHARACTER*9 YOKW9
      CHARACTER*10 YOKW10
      CHARACTER*80 YPCREC
      DIMENSION ITFFLD(JPTFMX)

      DO 100 J=1,JPTFMX
      ITFFLD(J)=J
100   CONTINUE

      YOEQL='='
      YOCOM=','
**    Search for the keyword in YPCREC and then determine the field
**          value
      LOFIND=.FALSE.

      YOKW2='CI'
      CALL SEARCH(YPCREC,YOKW2,LOFIND)
      IF(LOFIND) THEN
         CALL RMULTP(YPCREC,YOEQL,YOCOM,CINTFZ,JPTFMX)
         RETURN
      ENDIF


**    Else
      YOKW9='ZONALMEAN'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDFZZM)
         RETURN
      ENDIF

**    Else
      YOKW9='NSAVERAGE'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
         CALL RMULTP(YPCREC,YOEQL,YOCOM,VFZNSA,2)
         RETURN
      ENDIF

**    Else
      YOKW7='NSSLICE'
      CALL SEARCH(YPCREC,YOKW7,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
         IF(IERR.NE.0) GO TO 1000
         CALL RTRANS(YPCREC,IPOSN,IPOSX,VFZNS,12)
         RETURN
      ENDIF

**    Else
      YOKW10='PRINTFIELD'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            CALL INTCPY(ITFFLD,JPTFMX,NFZPR)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NFZPR,JPTFMX)
         RETURN
      ENDIF

**    Else
      YOKW9='PLOTFIELD'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            CALL INTCPY(ITFFLD,JPTFMX,NFZPL)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NFZPL,JPTFMX)
         RETURN
      ENDIF

**    Else
**    Keyword not located
      NFATAL=NFATAL+1
      WRITE(NCERR,6030) YPCREC
6030  FORMAT(1X,'KEYWORD NOT LOCATED FOR PHRASE :'/A)
      CALL ERRSTP

1000  CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6000) YPCREC
6000  FORMAT(1X,'FZEXTR: ERROR IN FZ DATA ENTRY',/A)
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE FZPROC
**    Function - to read and process the FZ diagnostic data
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /TFDIAG/LDFZ,/DIAGTP/LDTF,/ERRMSG/NCERR
**    Com changed - /FZDIAG/all elements,/ERRMSG/NFATAL,NWARN
**    Called by - DIAGRD
**    Calls - SEARCH,FZEXTR,ERRSTP
**    Files read - 10
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDFZZM
      COMMON /FZDIAG/LDFZZM,VFZNS,VFZNSA(2),NFZPR(JPTFMX),
     -      NFZPL(JPTFMX),CINTFZ(JPTFMX)
      LOGICAL LDFH,LDFZ,LDFM,LDFP,LDF3,LTFTOT,LTFHP,LTFLP
      COMMON /TFDIAG/LDFH,LDFZ,LDFM,LDFP,LDF3,NCTF,NVTFHR(JPNL),
     -      VTFSFC(JPNL),TFCUT,NTF,FACTTF(2,JPTFMX),NFLDTF(4,JPTFMX),
     -      LTFTOT,LTFHP,LTFLP
      CHARACTER*80 YOCREC
      LOGICAL LOFIND
**    Specify the defaults
      PARAMETER(R999=-999.0,RNEG=-0.1)
      DATA LDFZZM/.FALSE./
      DATA VFZNS/R999/,VFZNSA/2*R999/
      DATA NFZPR/JPTFMX*0/,NFZPL/JPTFMX*0/
      DATA CINTFZ/JPTFMX*RNEG/

      IF(.NOT.(LDTF.AND.LDFZ)) THEN
**       FZ diagnostics not selected - print warning
         NWARN=NWARN+1
         WRITE(NCERR,6000)
6000     FORMAT(1X,'WARNING: - FZ DIAGNOSTICS NOT SELECTED:',
     -         ' ALTHOUGH USER HAS SUPPLIED FZ INPUT DATA')
      ENDIF

**    Read and interpret records until '$ENDFZ' found
300   READ(10,5000,END=100,ERR=110)YOCREC
5000  FORMAT(A80)
      CALL SEARCH(YOCREC,'$ENDFZ',LOFIND)
      IF(LOFIND) THEN
         WRITE(NCERR,6010)
6010     FORMAT(1X,'USER-SUPPLIED FZ SELECTION DATA PROCESSED')
         RETURN
      ENDIF

      CALL FZEXTR(YOCREC)
      GO TO 300

100   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6020)
6020  FORMAT(1X,'FZPROC: EOF WHEN SEARCHING FOR FZ DATA')
      CALL ERRSTP

110   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6030)
6030  FORMAT(1X,'FZPROC: ERROR WHEN SEARCHING FOR FZ DATA')
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE CHKFM
**    Function - to check FM data input
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /TFDIAG/NTF,/FMDIAG/VFMEW,VFMEWA,
**          /DIAGTP/LDTF,/COMDAT/LASCII,LWRITE,/ERRMSG/NCERR
**    Com changed - /FMDIAG/NFMPR,NFMPL/TFDIAG/LDFM,
**          /ERRMSG/NFATAL,NWARN
**    Params used - /PARAM1/JPNL,/PARAMS/JPTRMX
**    Called by - DIAGCK
**    Calls - LATCHK
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      LOGICAL LWRITE,LASCII,LSCRAT,LUTF14,LROGR,LMASSW,LAVMSK,
     -      LAVSIG
      CHARACTER*2 YTYPSF
      CHARACTER*6 YDUM6
      CHARACTER*8 YNAME(2)
      COMMON /COMDAT/BEGDAY,ENDDAY,LWRITE,LASCII,LSCRAT,
     -      LUTF14,LROGR,NSIGFG,NFREQD,NFREQA,NFREQP,RNTAPE,
     -      NASCII,LMASSW,LAVMSK,LAVSIG,YNAME,YTYPSF,YDUM6
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDFMMM
      COMMON /FMDIAG/LDFMMM,VFMEW,VFMEWA(2),NFMPR(JPTFMX),
     -      NFMPL(JPTFMX),CINTFM(JPTFMX)
      LOGICAL LDFH,LDFZ,LDFM,LDFP,LDF3,LTFTOT,LTFHP,LTFLP
      COMMON /TFDIAG/LDFH,LDFZ,LDFM,LDFP,LDF3,NCTF,NVTFHR(JPNL),
     -      VTFSFC(JPNL),TFCUT,NTF,FACTTF(2,JPTFMX),NFLDTF(4,JPTFMX),
     -      LTFTOT,LTFHP,LTFLP
      LOGICAL LOERR,LOVALU

      LOERR=.FALSE.

**    TF/FM diagnostics require that LDTF and LDFM are set to .TRUE.
      IF((.NOT.LDTF).OR.(.NOT.LDFM)) THEN
         IF((LDFMMM).OR.(VFMEW.GT.-90.0).OR.
     -         (VFMEWA(1).GT.-90.0).OR.(VFMEWA(2).GT.-90.0)) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6000)
6000        FORMAT(1X,'CHKFM: WARNING - FM DIAGNOSTICS SWITCHED OFF',
     -         ' LDTF/LDFM FLAGS NOT BOTH SET')
            RETURN
         ENDIF
      ENDIF
      IF((.NOT.LDTF).OR.(.NOT.LDFM)) THEN
         LDFM=.FALSE.
         RETURN
      ENDIF

**    Check that printing has been requested - if not, ensure
**          NFMPR=0 for all elements
      IF(.NOT.LWRITE) THEN
         IF(NFMPR(1).NE.0) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6005)
6005        FORMAT(1X,'CHKFM: NFMPR VALUES CHANGED - NO PRINTING')
            DO 1000 K=1,JPTFMX
1000        NFMPR(K)=0
         ENDIF
      ENDIF

**    Check that UTFs are requested - if not, ensure NFMPL=0
**          for all elements
      IF(.NOT.LASCII) THEN
         IF(NFMPL(1).NE.0) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6010)
6010        FORMAT(1X,'CHKFM: NFMPL VALUES CHANGED - NO PLOTTING')
            DO 1100 K=1,JPTFMX
1100        NFMPL(K)=0
         ENDIF
      ENDIF

**    Check valid code numbers in NFMPR
      IF(NFMPR(1).NE.0) THEN
         DO 2000 K=1,NTF
         LOVALU=.FALSE.
         IF(NFMPR(K).LE.NTF.AND.NFMPR(K).GE.0) LOVALU=.TRUE.
         IF(.NOT.LOVALU) THEN
            NFATAL=NFATAL+1
            LOERR=.TRUE.
            WRITE(NCERR,6020) NFMPR(K)
6020        FORMAT(1X,'CHKMM: ERROR IN NFMPR VALUE ',I4)
         ENDIF
2000     CONTINUE
      ENDIF

**    Check valid code numbers in NFMPL
      IF(NFMPL(1).NE.0) THEN
         DO 3000 K=1,NTF
         LOVALU=.FALSE.
         IF(NFMPL(K).LE.NTF.AND.NFMPL(K).GE.0) LOVALU=.TRUE.
         IF(.NOT.LOVALU) THEN
            NFATAL=NFATAL+1
            LOERR=.TRUE.
            WRITE(NCERR,6030) NFMPL(K)
6030        FORMAT(1X,'CHKFM: ERROR IN NFMPL VALUE ',I4)
         ENDIF
3000     CONTINUE
      ENDIF

      IF(NTF.LT.JPTFMX) THEN
         DO 2001 J=NTF+1,JPTFMX
         NFMPL(J)=0
         NFMPR(J)=0
2001     CONTINUE
      ENDIF

      IF(NFMPL(1).EQ.0.AND.NFMPR(1).EQ.0) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6040)
6040     FORMAT(1X,'CHKFM: FM DIAGNOSTICS SWITCHED OFF, NO FIELDS',
     -         ' SELECTED FOR PRINT OR PLOT')
         LDFM=.FALSE.
      ENDIF

**    Check validity of VFMEW AND VFMEWA values
      CALL LATCHK(VFMEW,1,-999.0,'(TF) FM VFMEW',LOERR)
      CALL LATCHK(VFMEWA,2,-999.0,'(TF) FM VFMEWA',LOERR)

CC    IF(LOERR) STOP

      RETURN
      END
      SUBROUTINE FMEXTR(YPCREC)
**    Function - to extract information from FM data and update
**          appropriate common blocks
**    Args in -
**               YPCREC  - character*80 variable containing data
**    Args out - none
**    Args for work - none
**    Com used - /ERRMSG/NCERR
**    Com changed - /FMDIAG/all elements,/ERRMSG/NFATAL
**    Params used - /PARAMS/JPTFMX
**    Called by - FMPROC
**    Calls - SEARCH,LPROC,RMULTP,LASTCH,RTRANS,IMULTP,INTCPY,ERRSTP
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDFMMM
      COMMON /FMDIAG/LDFMMM,VFMEW,VFMEWA(2),NFMPR(JPTFMX),
     -      NFMPL(JPTFMX),CINTFM(JPTFMX)
      LOGICAL LOFIND,LOALL
      CHARACTER*1 YOCOM,YOEQL
      CHARACTER*2 YOKW2
      CHARACTER*4 YOKW4
      CHARACTER*7 YOKW7
      CHARACTER*9 YOKW9
      CHARACTER*10 YOKW10
      CHARACTER*80 YPCREC
      DIMENSION ITFFLD(JPTFMX)

      DO 100 J=1,JPTFMX
      ITFFLD(J)=J
100   CONTINUE

      YOEQL='='
      YOCOM=','
**    Search for the keyword in YPCREC and then determine the field
**          value
      LOFIND=.FALSE.

      YOKW9='MERIDMEAN'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDFMMM)
         RETURN
      ENDIF

**    Else
      YOKW2='CI'
      CALL SEARCH(YPCREC,YOKW2,LOFIND)
      IF(LOFIND) THEN
         CALL RMULTP(YPCREC,YOEQL,YOCOM,CINTFM,JPTRMX)
         RETURN
      ENDIF


**    Else
      YOKW9='WEAVERAGE'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
         CALL RMULTP(YPCREC,YOEQL,YOCOM,VFMEWA,2)
         RETURN
      ENDIF

**    Else
      YOKW7='WESLICE'
      CALL SEARCH(YPCREC,YOKW7,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
         IF(IERR.NE.0) GO TO 1000
         CALL RTRANS(YPCREC,IPOSN,IPOSX,VFMEW,12)
         RETURN
      ENDIF

**    Else
      YOKW10='PRINTFIELD'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            CALL INTCPY(ITFFLD,JPTFMX,NFMPR)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NFMPR,JPTFMX)
         RETURN
      ENDIF

**    Else
      YOKW9='PLOTFIELD'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            CALL INTCPY(ITFFLD,JPTFMX,NFMPL)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NFMPL,JPTFMX)
         RETURN
      ENDIF

**    Else
**    Keyword not located
      NFATAL=NFATAL+1
      WRITE(NCERR,6030) YPCREC
6030  FORMAT(1X,'KEYWORD NOT LOCATED FOR PHRASE :'/A)
      CALL ERRSTP

1000  CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6000) YPCREC
6000  FORMAT(1X,'FMEXTR: ERROR IN FM DATA ENTRY',/A)
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE FMPROC
**    Function - to read and process the FM diagnostic data
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /TFDIAG/LDFM,/DIAGTP/LDTF,/ERRMSG/NCERR
**    Com changed - /FMDIAG/all elements,/ERRMSG/NFATAL,NWARN
**    Called by - DIAGRD
**    Calls - SEARCH,FMEXTR,ERRSTP
**    Files read - 10
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDFMMM
      COMMON /FMDIAG/LDFMMM,VFMEW,VFMEWA(2),NFMPR(JPTFMX),
     -      NFMPL(JPTFMX),CINTFM(JPTFMX)
      LOGICAL LDFH,LDFZ,LDFM,LDFP,LDF3,LTFTOT,LTFHP,LTFLP
      COMMON /TFDIAG/LDFH,LDFZ,LDFM,LDFP,LDF3,NCTF,NVTFHR(JPNL),
     -      VTFSFC(JPNL),TFCUT,NTF,FACTTF(2,JPTFMX),NFLDTF(4,JPTFMX),
     -      LTFTOT,LTFHP,LTFLP
      CHARACTER*80 YOCREC
      LOGICAL LOFIND
**    Specify the defaults
      PARAMETER(R999=-999.0,RNEG=-0.1)
      DATA LDFMMM/.FALSE./
      DATA VFMEW/R999/,VFMEWA/2*R999/
      DATA NFMPR/JPTFMX*0/,NFMPL/JPTFMX*0/
      DATA CINTFM/JPTFMX*RNEG/

      IF(.NOT.(LDTF.AND.LDFM)) THEN
**       FM diagnostics not selected - print warning
         NWARN=NWARN+1
         WRITE(NCERR,6000)
6000     FORMAT(1X,'WARNING: - FM DIAGNOSTICS NOT SELECTED:',
     -         ' ALTHOUGH USER HAS SUPPLIED FM INPUT DATA')
      ENDIF

**    Read and interpret records until '$ENDFM' found
300   READ(10,5000,END=100,ERR=110)YOCREC
5000  FORMAT(A80)
      CALL SEARCH(YOCREC,'$ENDFM',LOFIND)
      IF(LOFIND) THEN
         WRITE(NCERR,6010)
6010     FORMAT(1X,'USER-SUPPLIED FM SELECTION DATA PROCESSED')
         RETURN
      ENDIF

      CALL FMEXTR(YOCREC)
      GO TO 300

100   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6020)
6020  FORMAT(1X,'FMPROC: EOF WHEN SEARCHING FOR FM DATA')
      CALL ERRSTP

110   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6030)
6030  FORMAT(1X,'FMPROC: ERROR WHEN SEARCHING FOR FM DATA')
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE CHKFP
**    Function - to check FP data input
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /TPDIAG/NFPPR,NFPPL,NFPPF,VFPPF,/DIAGTP/LDTF,
**          /COMDAT/LWRITE,LASCII,/ERRMSG/NCERR
**    Com changed - /FPDIAG/NFPPR,NFPPL/TFDIAG/LDFP,/ERRMSG/
**          NFATAL,NWARN
**    Params used - /PARAM1/JPNL,/PARAMS/JPTFMX,JPFPPF
**    Called by - DIAGCK
**    Calls - LATCHK,LNGCHK
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      LOGICAL LWRITE,LASCII,LSCRAT,LUTF14,LROGR,LMASSW,LAVMSK,
     -      LAVSIG
      CHARACTER*2 YTYPSF
      CHARACTER*6 YDUM6
      CHARACTER*8 YNAME(2)
      COMMON /COMDAT/BEGDAY,ENDDAY,LWRITE,LASCII,LSCRAT,
     -      LUTF14,LROGR,NSIGFG,NFREQD,NFREQA,NFREQP,RNTAPE,
     -      NASCII,LMASSW,LAVMSK,LAVSIG,YNAME,YTYPSF,YDUM6
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDFPCP
      COMMON /FPDIAG/NFPPF,VFPPF(2,JPFPPF),NFPPR(JPTFMX),
     -      NFPPL(JPTFMX),LDFPCP
      LOGICAL LDFH,LDFZ,LDFM,LDFP,LDF3,LTFTOT,LTFHP,LTFLP
      COMMON /TFDIAG/LDFH,LDFZ,LDFM,LDFP,LDF3,NCTF,NVTFHR(JPNL),
     -      VTFSFC(JPNL),TFCUT,NTF,FACTTF(2,JPTFMX),NFLDTF(4,JPTFMX),
     -      LTFTOT,LTFHP,LTFLP
      LOGICAL LOERR,LOVALU

      LOERR=.FALSE.

**    TF/FP diagnostics require that LDTF and LDFP are set to .TRUE.
      IF((.NOT.LDTF).OR.(.NOT.LDFP)) THEN
         IF((VFPPF(1,1).GT.-90.0).OR.(LDFPCP)) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6000)
6000        FORMAT(1X,'CHKFP: WARNING - FP DIAGNOSTICS SWITCHED OFF',
     -            ' LDTF/LDFP FLAGS NOT BOTH SET')
            RETURN
         ENDIF
      ENDIF
      IF((.NOT.LDTF).OR.(.NOT.LDFP)) THEN
         LDFP=.FALSE.
         RETURN
      ENDIF

**    Check that printing has been requested - if not, ensure
**          NFPPR=0 for all elements
      IF(.NOT.LWRITE) THEN
         IF(NFPPR(1).NE.0) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6060)
6060        FORMAT(1X,'CHKFP: NFPPR VALUES CHANGED - NO PRINTING')
            DO 1000 K=1,JPTFMX
1000        NFPPR(K)=0
         ENDIF
      ENDIF

**    Check that UTFs are requested - if not, ensure NFPPL=0
**          for all elements
      IF(.NOT.LASCII) THEN
         IF(NFPPL(1).NE.0) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6010)
6010        FORMAT(1X,'CHKFP: NFPPL VALUES CHANGED - NO PLOTTING')
            DO 1100 K=1,JPTFMX
1100        NFPPL(K)=0
         ENDIF
      ENDIF

**    Check valid code numbers in NFPPR
      IF(NFPPR(1).NE.0) THEN
         DO 2000 K=1,NTF
         LOVALU=.FALSE.
         IF(NFPPR(K).LE.NTF.AND.NFPPR(K).GE.0) LOVALU=.TRUE.
         IF(.NOT.LOVALU) THEN
            LOERR=.TRUE.
            NFATAL=NFATAL+1
            WRITE(NCERR,6020) NFPPR(K)
6020        FORMAT(1X,'CHKFP: ERROR IN NFPPR VALUE ',I4)
         ENDIF
2000     CONTINUE
      ENDIF

**    Check valid code numbers in NFPPL
      IF(NFPPL(1).NE.0) THEN
         DO 3000 K=1,NTF
         LOVALU=.FALSE.
         IF(NFPPL(K).LE.NTF.AND.NFPPL(K).GE.0) LOVALU=.TRUE.
         IF(.NOT.LOVALU) THEN
            LOERR=.TRUE.
            NFATAL=NFATAL+1
            WRITE(NCERR,6030) NFPPL(K)
6030        FORMAT(1X,'CHKFP: ERROR IN NFPPL VALUE ',I4)
         ENDIF
3000     CONTINUE
      ENDIF

      IF(NTF.LT.JPTFMX) THEN
         DO 2001 J=NTF+1,JPTFMX
         NFPPL(J)=0
         NFPPR(J)=0
2001     CONTINUE
      ENDIF

      IF(NFPPL(1).EQ.0.AND.NFPPR(1).EQ.0) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6040)
6040     FORMAT(1X,'CHKFP: FP DIAGNOSTICS SWITCHED OFF, NO FIELDS',
     -         ' SELECTED FOR PRINT OR PLOT')
         LDFP=.FALSE.
      ENDIF

**    Check validity of profile coordinates
      IF(NFPPF.LT.0.OR.NFPPF.GT.JPFPPF) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6050)
6050     FORMAT(1X,'CHKFP: ERROR - INVALID NFPPF')
         LOERR=.TRUE.
      ENDIF

      IF(NFPPF.GT.0.AND.NFPPF.LE.JPFPPF) THEN
         DO 100 JL=1,NFPPF
         CALL LATCHK(VFPPF(1,JL),1,-999.0,'(TF) FP VFPPF',LOERR)
         CALL LNGCHK(VFPPF(2,JL),1,-999.0,'(TF) FP VFPPF',LOERR)
         IF((VFPPF(1,JL).LT.-90.0).OR.(VFPPF(2,JL).LT.-90.0)) THEN
            NFATAL=NFATAL+1
            WRITE(NCERR,6080)
6080        FORMAT(1X,'CHKFP: ERROR - INVALID PROFILE COORDINATES')
            LOERR=.TRUE.
         ENDIF
100      CONTINUE
      ENDIF

CC    IF(LOERR) STOP
      RETURN
      END
      SUBROUTINE FPEXTR(YPCREC)
**    Function - to extract information from FP data and update
**          appropriate common blocks
**    Args in -
**               YPCREC  - character*80 variable containing data
**    Args out - none
**    Args for work - none
**    Com used - /ERRMSG/NCERR
**    Com changed - /FPDIAG/all elements,/ERRMSG/NFATAL
**    Params used - /PARAMS/JPFPPF,JPTFMX
**    Called by - FPPROC
**    Calls - SEARCH,LPROC,RMULTP,LASTCH,ITRANS,IMULTP,INTCPY,ERRSTP
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDFPCP
      COMMON /FPDIAG/NFPPF,VFPPF(2,JPFPPF),NFPPR(JPTFMX),
     -      NFPPL(JPTFMX),LDFPCP
      LOGICAL LOFIND,LOALL
      CHARACTER*1 YOCOM,YOEQL
      CHARACTER*4 YOKW4
      CHARACTER*9 YOKW9
      CHARACTER*10 YOKW10
      CHARACTER*13 YOKW13
      CHARACTER*14 YOKW14
      CHARACTER*80 YPCREC
      DIMENSION ITFFLD(JPTFMX)

      DO 100 J=1,JPTFMX
      ITFFLD(J)=J
100   CONTINUE

      YOEQL='='
      YOCOM=','
**    Search for the keyword in YPCREC and then determine the field
**          value
      LOFIND=.FALSE.

**    Else
      YOKW10='COLPROFILE'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDFPCP)
         RETURN
      ENDIF

**    Else
      YOKW14='NUMBEROFCOORDS'
      CALL SEARCH(YPCREC,YOKW14,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
**       Note that IPOSN and IPOSX can take the same value
         IF(IERR.EQ.998) GO TO 1000
         CALL ITRANS(YPCREC,IPOSN,IPOSX,NFPPF,12)
         RETURN
      ENDIF

**    Else
      YOKW13='PROFILECOORDS'
      CALL SEARCH(YPCREC,YOKW13,LOFIND)
      IF(LOFIND) THEN
         CALL RMULTP(YPCREC,YOEQL,YOCOM,VFPPF,2*JPFPPF)
         RETURN
      ENDIF

**    Else
      YOKW10='PRINTFIELD'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            CALL INTCPY(ITFFLD,JPTFMX,NFPPR)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NFPPR,JPTFMX)
         RETURN
      ENDIF

**    Else
      YOKW9='PLOTFIELD'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            CALL INTCPY(ITFFLD,JPTFMX,NFPPL)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NFPPL,JPTFMX)
         RETURN
      ENDIF

**    Else
**    Keyword not located
      NFATAL=NFATAL+1
      WRITE(NCERR,6030) YPCREC
6030  FORMAT(1X,'KEYWORD NOT LOCATED FOR PHRASE :'/A)
      CALL ERRSTP

1000  CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6000) YPCREC
6000  FORMAT(1X,'FPEXTR: ERROR IN FP DATA ENTRY',/A)
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE FPPROC
**    Function - to read and process the FP diagnostic data
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /TFDIAG/LDFP,/DIAGTP/LDTF,/ERRMSG/NCERR
**    Com changed - /TFDIAG/all elements,/ERRMSG/NFATAL,NWARN
**    Params used - JPTP2,RNEG,/PARAM1/JPNL,/PARAMS/JPFPPF
**    Called by - DIAGRD
**    Calls - SEARCH,FPEXTR,ERRSTP
**    Files read - 10
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDFPCP
      COMMON /FPDIAG/NFPPF,VFPPF(2,JPFPPF),NFPPR(JPTFMX),
     -      NFPPL(JPTFMX),LDFPCP
      LOGICAL LDFH,LDFZ,LDFM,LDFP,LDF3,LTFTOT,LTFHP,LTFLP
      COMMON /TFDIAG/LDFH,LDFZ,LDFM,LDFP,LDF3,NCTF,NVTFHR(JPNL),
     -      VTFSFC(JPNL),TFCUT,NTF,FACTTF(2,JPTFMX),NFLDTF(4,JPTFMX),
     -      LTFTOT,LTFHP,LTFLP
      CHARACTER*80 YOCREC
      LOGICAL LOFIND
**    Specify the defaults
      PARAMETER(R999=-999.0,RNEG=-0.1,JPTP2=2*JPFPPF)
      DATA LDFPCP/.FALSE./
      DATA NFPPF/0/,VFPPF/JPTP2*R999/
      DATA NFPPR/JPTFMX*0/,NFPPL/JPTFMX*0/

      IF(.NOT.(LDTF.AND.LDFP)) THEN
**       FP diagnostics not selected - print warning
         NWARN=NWARN+1
         WRITE(NCERR,6000)
6000     FORMAT(1X,'WARNING: - FP DIAGNOSTICS NOT SELECTED:',
     -         ' ALTHOUGH USER HAS SUPPLIED TP INPUT DATA')
      ENDIF

**    Read and interpret records until '$ENDFP' found
300   READ(10,5000,END=100,ERR=110)YOCREC
5000  FORMAT(A80)
      CALL SEARCH(YOCREC,'$ENDFP',LOFIND)
      IF(LOFIND) THEN
         WRITE(NCERR,6010)
6010     FORMAT(1X,'USER-SUPPLIED FP SELECTION DATA PROCESSED')
         RETURN
      ENDIF

      CALL FPEXTR(YOCREC)
      GO TO 300

100   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6020)
6020  FORMAT(1X,'FPPROC: EOF WHEN SEARCHING FOR FP DATA')
      CALL ERRSTP

110   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6030)
6030  FORMAT(1X,'FPPROC: ERROR WHEN SEARCHING FOR FP DATA')
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE CHKF3
**    Function - to check F3 data input
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /TFDIAG/NTF,/F3DIAG/NCF3,/DIAGTP/LDTF,/COMDAT/
**          LASCII,/ERRMSG/NCERR
**    Com changed - /F3DIAG/NF3PL,/TFDIAG/LDF3,/ERRMSG/NFATAL,NWARN
**    Params used - /PARAM1/JPNL,/PARAMS/JPTFMX
**    Called by - DIAGCK
**    Calls - none
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      LOGICAL LWRITE,LASCII,LSCRAT,LUTF14,LROGR,LMASSW,LAVMSK,
     -      LAVSIG
      CHARACTER*2 YTYPSF
      CHARACTER*6 YDUM6
      CHARACTER*8 YNAME(2)
      COMMON /COMDAT/BEGDAY,ENDDAY,LWRITE,LASCII,LSCRAT,
     -      LUTF14,LROGR,NSIGFG,NFREQD,NFREQA,NFREQP,RNTAPE,
     -      NASCII,LMASSW,LAVMSK,LAVSIG,YNAME,YTYPSF,YDUM6
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      COMMON /F3DIAG/NCF3,NF3PL(JPTFMX)
      LOGICAL LDFH,LDFZ,LDFM,LDFP,LDF3,LTFTOT,LTFHP,LTFLP
      COMMON /TFDIAG/LDFH,LDFZ,LDFM,LDFP,LDF3,NCTF,NVTFHR(JPNL),
     -      VTFSFC(JPNL),TFCUT,NTF,FACTTF(2,JPTFMX),NFLDTF(4,JPTFMX),
     -      LTFTOT,LTFHP,LTFLP
      LOGICAL LOERR,LOVALU

      LOERR=.FALSE.

**    Check that UTFs are requested - if not, ensure NF3PL=0
**          for all elements
      IF(.NOT.LASCII) THEN
         IF(NF3PL(1).NE.0) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6010)
6010        FORMAT(1X,'CHKF3: NF3PL VALUES CHANGED - NO O/P')
            DO 1100 K=1,JPTFMX
1100        NF3PL(K)=0
         ENDIF
      ENDIF

**    Check valid code numbers in NF3PL
      IF(NF3PL(1).NE.0) THEN
         DO 3000 K=1,NTF
         LOVALU=.FALSE.
         IF(NF3PL(K).LE.NTF.AND.NF3PL(K).GE.0) LOVALU=.TRUE.
         IF(.NOT.LOVALU) THEN
            LOERR=.TRUE.
            NFATAL=NFATAL+1
            WRITE(NCERR,6030) NF3PL(K)
6030        FORMAT(1X,'CHKF3: ERROR IN NF3PL VALUE ',I4)
         ENDIF
3000     CONTINUE
      ENDIF

**    Switch off diagnostics if NF3PL(1) is zero
      IF(NF3PL(1).EQ.0.AND.LDF3) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6035)
6035     FORMAT(1X,'CHKF3: F3 DIAGNOSTICS SWITCHED OFF, NF3PL(1)=0')
         LDF3=.FALSE.
         RETURN
      ENDIF
      IF(.NOT.LDF3) RETURN

      IF(NTF.LT.JPTFMX) THEN
         DO 2001 J=NTF+1,JPTFMX
         NF3PL(J)=0
2001     CONTINUE
      ENDIF

**    Check channel for output of 3-D fields
      IF(LDF3.AND.NCF3.LE.0) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6050)
6050     FORMAT(1X,'CHKF3: ERROR - F3 DIAGNOSTICS REQUESTED BUT',
     -         ' NCF3 CHANNEL NUMBER INVALID')
         LOERR=.TRUE.
      ENDIF

CC    IF(LOERR) STOP
      RETURN
      END
      SUBROUTINE F3EXTR(YPCREC)
**    Function - to extract information from F3 data and update
**          appropriate common blocks
**    Args in -
**               YPCREC  - character*80 variable containing data
**    Args out - none
**    Args for work - none
**    Com used - /ERRMSG/NCERR
**    Com changed - /F3DIAG/all elements,/ERRMSG/NFATAL
**    Params used - /PARAMS/JPTFMX
**    Called by - T3PROC
**    Calls - SEARCH,LASTCH,ITRANS,INTCPY,IMULTP,ERRSTP
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      COMMON /F3DIAG/NCF3,NF3PL(JPTFMX)
      LOGICAL LOFIND,LOALL
      CHARACTER*1 YOCOM,YOEQL
      CHARACTER*4 YOKW4
      CHARACTER*9 YOKW9
      CHARACTER*10 YOKW10
      CHARACTER*80 YPCREC
      DIMENSION ITFFLD(JPTFMX)

      DO 100 J=1,JPTFMX
      ITFFLD(J)=J
100   CONTINUE

      YOEQL='='
      YOCOM=','
**    Search for the keyword in YPCREC and then determine the field
**          value
      LOFIND=.FALSE.

      YOKW10='O/PCHANNEL'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
**       Note that IPOSN and IPOSX can take the same value
         IF(IERR.EQ.998) GO TO 1000
         CALL ITRANS(YPCREC,IPOSN,IPOSX,NCF3,12)
         RETURN
      ENDIF

**    Else
      YOKW9='PLOTFIELD'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            CALL INTCPY(ITFFLD,JPTFMX,NF3PL)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NF3PL,JPTFMX)
         RETURN
      ENDIF

**    Else
**    Keyword not located
      NFATAL=NFATAL+1
      WRITE(NCERR,6030) YPCREC
6030  FORMAT(1X,'KEYWORD NOT LOCATED FOR PHRASE :'/A)
      CALL ERRSTP

1000  CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6000) YPCREC
6000  FORMAT(1X,'F3EXTR: ERROR IN T3 DATA ENTRY',/A)
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE F3PROC
**    Function - to read and process the F3 diagnostic data
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /TFDIAG/LDF3,/DIAGTP/LDTF,/ERRMSG/NCERR
**    Com changed - /F3DIAG/all elements,/ERRMSG/NFATAL
**    Called by - DIAGRD
**    Calls - SEARCH,F3EXTR,ERRSTP
**    Files read - 10
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      COMMON /F3DIAG/NCF3,NF3PL(JPTFMX)
      LOGICAL LDFH,LDFZ,LDFM,LDFP,LDF3,LTFTOT,LTFHP,LTFLP
      COMMON /TFDIAG/LDFH,LDFZ,LDFM,LDFP,LDF3,NCTF,NVTFHR(JPNL),
     -      VTFSFC(JPNL),TFCUT,NTF,FACTTF(2,JPTFMX),NFLDTF(4,JPTFMX),
     -      LTFTOT,LTFHP,LTFLP
      CHARACTER*80 YOCREC
      LOGICAL LOFIND
**    Specify the defaults
      DATA NCF3/1/
      DATA NF3PL/JPTFMX*0/

      IF(.NOT.(LDTF.AND.LDF3)) THEN
**       F3 diagnostics not selected - print warning
         NWARN=NWARN+1
         WRITE(NCERR,6000)
6000     FORMAT(1X,'WARNING: - F3 DIAGNOSTICS NOT SELECTED:',
     -         ' ALTHOUGH USER HAS SUPPLIED F3 INPUT DATA')
      ENDIF

**    Read and interpret records until '$ENDF3' found
300   READ(10,5000,END=100,ERR=110)YOCREC
5000  FORMAT(A80)
      CALL SEARCH(YOCREC,'$ENDF3',LOFIND)
      IF(LOFIND) THEN
         WRITE(NCERR,6010)
6010     FORMAT(1X,'USER-SUPPLIED F3 SELECTION DATA PROCESSED')
         RETURN
      ENDIF

      CALL F3EXTR(YOCREC)
      GO TO 300

100   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6020)
6020  FORMAT(1X,'F3PROC: EOF WHEN SEARCHING FOR F3 DATA')
      CALL ERRSTP

110   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6030)
6030  FORMAT(1X,'F3PROC: ERROR WHEN SEARCHING FOR F3 DATA')
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE CHKOD
**    Function - to check OD data input
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /COMDAT/LWRITE,LASCII,BEGDAY,ENDDAY,LMASSW,
**          /ODDIAG/NVODHR,VODSFC,/MODELC/NL
**          /COMDAT/YTYPSF,/LVDIAG/OUTLEV,/ERRMSG/NCERR
**    Com changed - /DIAGTP/LDOD,/ERRMSG/NFATAL,NWARN
**    Params used - /PARAM1/JPNL,/PARAMS/JPODMX
**    Called by - DIAGCK
**    Calls - none
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      LOGICAL LWRITE,LASCII,LSCRAT,LUTF14,LROGR,LMASSW,LAVMSK,
     -      LAVSIG
      CHARACTER*2 YTYPSF
      CHARACTER*6 YDUM6
      CHARACTER*8 YNAME(2)
      COMMON /COMDAT/BEGDAY,ENDDAY,LWRITE,LASCII,LSCRAT,
     -      LUTF14,LROGR,NSIGFG,NFREQD,NFREQA,NFREQP,RNTAPE,
     -      NASCII,LMASSW,LAVMSK,LAVSIG,YNAME,YTYPSF,YDUM6
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      COMMON /LVDIAG/OUTLEV(JPNL)
      LOGICAL LGCMTR,LHISPH,LHISXP,LDRY,LDIABH,LBL,LVD
      COMMON /MODELC/TSPD,NYEAR,NDAY,NMIN,LGCMTR,NL,TMEAN(JPNL),
     -      LHISPH,LHISXP,LDRY,LDIABH,BEGDYP,LBL,LVD,AKVV,CD
      LOGICAL LDOH,LDOZ,LDOM,LDOP,LDO3,LDODTD,LDODTS,LDODIN
      COMMON /ODDIAG/LDOH,LDOZ,LDOM,LDOP,LDO3,LDODTD,LDODTS,LDODIN,
     -      NCOD,NVODHR(JPNL),VODSFC(JPNL)
      LOGICAL LOERR,LOVALU

      LOERR=.FALSE.

**    OD diagnostics not permitted on isentropic surfaces when
**    fields have been multiplied by sigma (mass-weighted)
      IF(YTYPSF.EQ.'TH'.AND.LMASSW.AND.LDOD) THEN
         LOERR=.TRUE.
         NFATAL=NFATAL+1
         WRITE(NCERR,6010)
6010     FORMAT(1X,'CHKOD: ERROR - OD DIAGNOSTICS NOT ALLOWED ',
     -         'WITH SIGMA-WEIGHTED ISENTROPIC SURFACE DATA')
         LDOD=.FALSE.
      ENDIF

**    Switch off time-series and time-averaging calculations if
**    ENDDAY is not greater than BEGDAY
      IF(ABS(ENDDAY).LE.ABS(BEGDAY)) THEN
         IF(LDODTS) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6100)
6100        FORMAT(1X,'CHKOD: OD TIME-SERIES SWITCHED OFF, SINCE ',
     -            'ENDDAY.LE.BEGDAY')
            LDODTS=.FALSE.
         ENDIF
         IF(LDODTD) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6110)
6110        FORMAT(1X,'CHKOD: OD TIME-AVERAGES (D) SWITCHED OFF,',
     -            ' SINCE ENDDAY.LE.BEGDAY')
            LDODTD=.FALSE.
         ENDIF
      ENDIF

**    NVODHR values only used when output is on model surfaces, and
**    VODSFC values only used when output is on non-model surfaces
      IF((NVODHR(1).NE.0).AND.(YTYPSF.NE.'ET')) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6060)
6060     FORMAT(1X,'CHKOD: NVODHR VALUE(S) SUPPLIED, BUT NOT USED')
      ENDIF
      IF((NVODHR(1).LE.0).AND.(YTYPSF.EQ.'ET')) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6065)
6065     FORMAT(1X,'CHKOD: NVODHR VALUES NOT SUPPLIED,'
     -         ' OUTPUT MAY BE SUPPRESSED')
      ENDIF
      IF((VODSFC(1).GT.0.0).AND.(YTYPSF.EQ.'ET')) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6070)
6070     FORMAT(1X,'CHKOD: VODSFC VALUE(S) SUPPLIED, BUT NOT USED')
      ENDIF
      IF((VODSFC(1).LE.0.0).AND.(YTYPSF.NE.'ET')) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6075)
6075     FORMAT(1X,'CHKOD: VODSFC VALUES NOT SUPPLIED,'
     -         ' OUTPUT MAY BE SUPPRESSED')
      ENDIF

**    Check that VODSFC values are a subset of OUTLEV values if they are
**    to be used. ZDELTA is a small parameter to allow two REAL values
**    to be compared
      ZDELTA=1.0E-10
      IF(YTYPSF.NE.'ET') THEN
         DO 4000 K=1,NL
         IF(VODSFC(K).GT.0.0) THEN
            ZSFCP=VODSFC(K)+ZDELTA
            ZSFCM=VODSFC(K)-ZDELTA
            LOVALU=.FALSE.
            DO 4100 J=1,NL
            IF(ZSFCM.LE.OUTLEV(J).AND.ZSFCP.GE.OUTLEV(J)) LOVALU=.TRUE.
4100        CONTINUE
            IF(.NOT.LOVALU) THEN
               LOERR=.TRUE.
               NFATAL=NFATAL+1
               WRITE(NCERR,6080) VODSFC(K)
6080           FORMAT(1X,'CHKOD: VODSFC CORRESPONDS TO INVALID LEVEL ',
     -               F12.3)
            ENDIF
         ENDIF
4000     CONTINUE
      ENDIF

**    Check that NVODHR values lie in the range 1 to NL
      IF(YTYPSF.EQ.'ET') THEN
         DO 5000 J=1,NL
         IF(NVODHR(J).NE.0) THEN
            IF(NVODHR(J).LT.1.OR.NVODHR(J).GT.NL) THEN
               NFATAL=NFATAL+1
               WRITE(NCERR,6090)
6090           FORMAT(1X,'CHKOD: INVALID NVODHR VALUE')
               LOERR=.TRUE.
            ENDIF
         ENDIF
5000     CONTINUE
      ENDIF

CC    IF(LOERR) STOP
      RETURN
      END
      SUBROUTINE ODEXTR(YPCREC)
**    Function - to extract information from OD data and update
**          appropriate common blocks
**    Args in -
**               YPCREC  - character*80 variable containing data
**    Args out - none
**    Args for work - none
**    Com used - /ERRMSG/NCERR,/MODELC/NL
**    Com changed - /ODDIAG/all elements,/ERRMSG/NFATAL
**    Params used - /PARAM1/JPNL,/PARAMS/JPODMX
**    Called by - ODPROC
**    Calls - SEARCH,LPROC,INTCPY,IMULTP,LASTCH,ITRANS,RMULTP,ERRSTP
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LGCMTR,LHISPH,LHISXP,LDRY,LDIABH,LBL,LVD
      COMMON /MODELC/TSPD,NYEAR,NDAY,NMIN,LGCMTR,NL,TMEAN(JPNL),
     -      LHISPH,LHISXP,LDRY,LDIABH,BEGDYP,LBL,LVD,AKVV,CD
      LOGICAL LDOH,LDOZ,LDOM,LDOP,LDO3,LDODTD,LDODTS,LDODIN
      COMMON /ODDIAG/LDOH,LDOZ,LDOM,LDOP,LDO3,LDODTD,LDODTS,LDODIN,
     -      NCOD,NVODHR(JPNL),VODSFC(JPNL)
      LOGICAL LOFIND,LOALL
      DIMENSION INLFLD(JPNL)
      CHARACTER*1 YOCOM,YOEQL
      CHARACTER*4 YOKW4
      CHARACTER*5 YOKW5
      CHARACTER*7 YOKW7
      CHARACTER*8 YOKW8
      CHARACTER*9 YOKW9
      CHARACTER*10 YOKW10
      CHARACTER*13 YOKW13
      CHARACTER*80 YPCREC

      YOEQL='='
      YOCOM=','
**    Search for the keyword in YPCREC and then determine the field
**          value
      LOFIND=.FALSE.

      YOKW10='HORIZONTAL'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDOH)
         RETURN
      ENDIF

**    Else
      YOKW5='ZONAL'
      CALL SEARCH(YPCREC,YOKW5,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDOZ)
         RETURN
      ENDIF

**    Else
      YOKW10='MERIDIONAL'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDOM)
         RETURN
      ENDIF

**    Else
      YOKW7='PROFILE'
      CALL SEARCH(YPCREC,YOKW7,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDOP)
         RETURN
      ENDIF

**    Else
      YOKW8='3DOUTPUT'
      CALL SEARCH(YPCREC,YOKW8,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDO3)
         RETURN
      ENDIF

**    Else
      YOKW9='TAVERAGED'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDODTD)
         RETURN
      ENDIF

**    Else
      YOKW13='INSTANTANEOUS'
      CALL SEARCH(YPCREC,YOKW13,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDODIN)
         RETURN
      ENDIF

**    Else
      YOKW7='TSERIES'
      CALL SEARCH(YPCREC,YOKW7,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDODTS)
         RETURN
      ENDIF

**    Else
      YOKW10='O/PCHANNEL'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
**       Note that IPOSN and IPOSX can take the same value
         IF(IERR.EQ.998) GO TO 1000
         CALL ITRANS(YPCREC,IPOSN,IPOSX,NCOD,12)
         RETURN
      ENDIF

**    Else
      YOKW8='ODLEVELS'
      CALL SEARCH(YPCREC,YOKW8,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            DO 3500 JL=1,NL
3500        INLFLD(JL)=JL
            CALL INTCPY(INLFLD,NL,NVODHR)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NVODHR,NL)
         RETURN
      ENDIF

**    Else
      YOKW10='ODSURFACES'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
      IF(LOFIND) THEN
         CALL RMULTP(YPCREC,YOEQL,YOCOM,VODSFC,NL)
         RETURN
      ENDIF

**    Else
**    Keyword not located
      NFATAL=NFATAL+1
      WRITE(NCERR,6030) YPCREC
6030  FORMAT(1X,'KEYWORD NOT LOCATED FOR PHRASE :'/A)
      CALL ERRSTP

1000  CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6000) YPCREC
6000  FORMAT(1X,'ODEXTR: ERROR IN OD DATA ENTRY',/A)
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE ODPROC
**    Function - to read and process the OD diagnostic data
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /DIAGTP/LDOD,/ERRMSG/NCERR
**    Com changed - /ODDIAG/all elements,/ALOWOD/NODFLD,/ERRMSG/NFATAL,
**          NWARN
**    Called by - DIAGRD
**    Calls - SEARCH,ODEXTR,ERRSTP
**    Files read - 10
**    Files written - ERRSTP
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /ALOWOD/NODFLD(JPODMX)
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDOH,LDOZ,LDOM,LDOP,LDO3,LDODTD,LDODTS,LDODIN
      COMMON /ODDIAG/LDOH,LDOZ,LDOM,LDOP,LDO3,LDODTD,LDODTS,LDODIN,
     -      NCOD,NVODHR(JPNL),VODSFC(JPNL)
      CHARACTER*80 YOCREC
      LOGICAL LOFIND
**    Specify the defaults
      DATA NCOD/3/
      DATA LDOH/.FALSE./,LDOZ/.TRUE./,LDOM/.FALSE./
      DATA LDOP/.FALSE./,LDO3/.FALSE./
      DATA LDODTS/.FALSE./,LDODTD/.TRUE./,LDODIN/.FALSE./
      DATA NVODHR/JPNL*0/,VODSFC/JPNL*0.0/

**    Define allowed field codes for OD
      DO 200 J=1,JPODMX
      NODFLD(J)=J
200   CONTINUE

      IF(.NOT.LDOD) THEN
**       OD diagnostics not selected - print warning
         NWARN=NWARN+1
         WRITE(NCERR,6000)
6000     FORMAT(1X,'WARNING: - OD DIAGNOSTICS NOT SELECTED:',
     -         ' ALTHOUGH USER HAS SUPPLIED OD INPUT DATA')
      ENDIF

**    Read and interpret records until '$ENDOD' found
300   READ(10,5000,END=100,ERR=110)YOCREC
5000  FORMAT(A80)
      CALL SEARCH(YOCREC,'$ENDOD',LOFIND)
      IF(LOFIND) THEN
         WRITE(NCERR,6010)
6010     FORMAT(1X,'USER-SUPPLIED OD SELECTION DATA PROCESSED')
         RETURN
      ENDIF

      CALL ODEXTR(YOCREC)
      GO TO 300

100   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6020)
6020  FORMAT(1X,'ODPROC: EOF WHEN SEARCHING FOR OD DATA')
      CALL ERRSTP

110   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6030)
6030  FORMAT(1X,'ODPROC: ERROR WHEN SEARCHING FOR OD DATA')
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE CHKOH
**    Function - to check OH data input
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /ODDIAG/NVODHR,VODSFC,/OHDIAG/LDOHSL,
**          VOHTS,NOHVEC,/DIAGTP/LDOD,/COMDAT/YTYPSF,
**          LWRITE,LASCII,/ALOWOD/NODFLD,/ERRMSG/NCERR
**    Com changed - /OHDIAG/NOHPR,NOHPL,NOH,LDOHCT,/ODDIAG/LDOH,
**          /ERRMSG/NFATAL,NWARN
**    Params used - /PARAM1/JPNL,/PARAMS/JPODMX
**    Called by - DIAGCK
**    Calls - LEVCHK,LATCHK
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /ALOWOD/NODFLD(JPODMX)
      LOGICAL LWRITE,LASCII,LSCRAT,LUTF14,LROGR,LMASSW,LAVMSK,
     -      LAVSIG
      CHARACTER*2 YTYPSF
      CHARACTER*6 YDUM6
      CHARACTER*8 YNAME(2)
      COMMON /COMDAT/BEGDAY,ENDDAY,LWRITE,LASCII,LSCRAT,
     -      LUTF14,LROGR,NSIGFG,NFREQD,NFREQA,NFREQP,RNTAPE,
     -      NASCII,LMASSW,LAVMSK,LAVSIG,YNAME,YTYPSF,YDUM6
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDOH,LDOZ,LDOM,LDOP,LDO3,LDODTD,LDODTS,LDODIN
      COMMON /ODDIAG/LDOH,LDOZ,LDOM,LDOP,LDO3,LDODTD,LDODTS,LDODIN,
     -      NCOD,NVODHR(JPNL),VODSFC(JPNL)
      LOGICAL LDOHSL,LDOHCT,LDOHED
      COMMON /OHDIAG/LDOHSL,LDOHCT,LDOHED,VOHTS(2),
     -      NOHPR(JPODMX),NOHPL(JPODMX),NOH,NOHVEC(JPODMX),
     -      CINTOH(JPODMX)
      LOGICAL LOERR,LOVALU

      LOERR=.FALSE.

**    OD/OH diagnostics require that LDOD and LDOH are set to .TRUE.
      IF((.NOT.LDOD).OR.(.NOT.LDOH)) THEN
         IF((LDOHSL).OR.(LDOHCT).OR.(VOHTS(1).GT.-90.0).OR.
     -         (VOHTS(2).GT.-90.0)) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6000)
6000        FORMAT(1X,'CHKOH: WARNING - OH DIAGNOSTICS REQUESTED BUT',
     -            ' HORIZONTAL LDOD/LDOH FLAGS NOT SET')
            RETURN
         ENDIF
      ENDIF

**    Check that printing has been requested - if not, ensure
**          NOHPR=0 for all elements
      IF(.NOT.LWRITE) THEN
         IF(NOHPR(1).NE.0) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6005)
6005        FORMAT(1X,'CHKOH: NOHPR VALUES CHANGED - NO PRINTING')
            DO 1000 K=1,JPODMX
1000        NOHPR(K)=0
**          Reset value of NOH
            NOH=0
            DO 1001 K=1,JPODMX
            IF(NOHPL(K).GT.0) NOH=NOH+1
1001        CONTINUE
         ENDIF
      ENDIF

**    Check that UTFs are requested - if not, ensure NOHPL=0
**          for all elements
      IF(.NOT.LASCII) THEN
         IF(NOHPL(1).NE.0) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6010)
6010        FORMAT(1X,'CHKOH: NOHPL VALUES CHANGED - NO PLOTTING')
            DO 1100 K=1,JPODMX
1100        NOHPL(K)=0
**          Reset value of NOH
            NOH=0
            DO 1101 K=1,JPODMX
            IF(NOHPR(K).GT.0) NOH=NOH+1
1101        CONTINUE
         ENDIF
      ENDIF

**    Check valid code numbers in NOHPR
      IF(NOHPR(1).NE.0) THEN
         DO 2000 K=1,JPODMX
         LOVALU=.FALSE.
         DO 2100 J=1,JPODMX
         IF((NOHPR(K).EQ.NODFLD(J)).OR.(NOHPR(K).EQ.0)) LOVALU=.TRUE.
2100     CONTINUE
         IF(.NOT.LOVALU) THEN
            LOERR=.TRUE.
            NFATAL=NFATAL+1
            WRITE(NCERR,6020) NOHPR(K)
6020        FORMAT(1X,'CHKOH: ERROR IN NOHPR VALUE ',I4)
         ENDIF
2000     CONTINUE
      ENDIF

**    Check valid code numbers in NOHPL
      IF(NOHPL(1).NE.0) THEN
         DO 3000 K=1,JPODMX
         LOVALU=.FALSE.
         DO 3100 J=1,JPODMX
         IF((NOHPL(K).EQ.NODFLD(J)).OR.(NOHPL(K).EQ.0)) LOVALU=.TRUE.
3100     CONTINUE

         IF(.NOT.LOVALU) THEN
            LOERR=.TRUE.
            NFATAL=NFATAL+1
            WRITE(NCERR,6030) NOHPL(K)
6030        FORMAT(1X,'CHKOH: ERROR IN NOHPL VALUE ',I4)
         ENDIF
3000     CONTINUE
      ENDIF

**    Switch off diagnostics if NOH is zero
      IF(NOH.EQ.0.AND.LDOH) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6035)
6035     FORMAT(1X,'CHKOH: OH DIAGNOSTICS SWITCHED OFF, NOH=0')
         LDOH=.FALSE.
         RETURN
      ENDIF
      IF(.NOT.LDOH) RETURN

**    Check that NOHVEC values correspond to NOHPL values
      IF(NOHVEC(1).NE.0) THEN
         DO 3500 K=1,JPODMX
         LOVALU=.FALSE.
         DO 3600 J=1,JPODMX
         IF((NOHVEC(K).EQ.NOHPL(J)).OR.(NOHVEC(K).EQ.0)) LOVALU=.TRUE.
3600     CONTINUE
         IF(.NOT.LOVALU) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6050)
6050        FORMAT(1X,'CHKOH: NOHVEC CORRESPONDS TO NON-PLOTTED FIELD')
         ENDIF
3500     CONTINUE
      ENDIF

**    Check that NOH is non-zero, and less than JPODMX
      IF(NOH.LE.0.OR.(NOH.GT.JPODMX)) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6040)
6040     FORMAT(1X,'CHKOH : INVALID VALUE FOR NOH')
         LOERR=.TRUE.
      ENDIF

**    Check that NVODHR or VODSFC have been set if LDOHSL is .TRUE.
      IF(LDOHSL.AND.(YTYPSF.EQ.'ET').AND.(NVODHR(1).EQ.0)) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6055)
6055     FORMAT(1X,'CHKOH: ERROR - HORIZONTAL SLICE O/P REQUIRES'
     -         ' NVODHR TO BE SPECIFIED FOR O/P ON SIGMA SURFACES')
         LOERR=.TRUE.
      ENDIF
      ZDELTA=1.0E-10
      IF(LDOHSL.AND.(YTYPSF.NE.'ET').AND.(VODSFC(1).LE.ZDELTA)) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6060)
6060     FORMAT(1X,'CHKOH: ERROR - HORIZONTAL SLICE O/P REQUIRES'
     -         ' VODSFC TO BE SPECIFIED FOR O/P ON NON-SIGMA SURFACES')
         LOERR=.TRUE.
      ENDIF

**    Check validity of VOHTS values
      CALL LATCHK(VOHTS,2,-999.0,'(OD) OH VOHTS',LOERR)

**    Column totals over the model domain can only be found when output
**    is on 'ET' surfaces
      IF((YTYPSF.NE.'ET').AND.LDOHCT) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6080)
6080     FORMAT(1X,'CHKOH: ERROR - COLUMN TOTALS NOT APPROPRIATE ',
     -         ' SINCE O/P NOT ON MODEL LEVELS')
         LDOHCT=.FALSE.
         LOERR=.TRUE.
      ENDIF

CC    IF(LOERR) STOP
      RETURN
      END
      SUBROUTINE OHEXTR(YPCREC)
**    Function - to extract information from OH data and update
**          appropriate common blocks
**    Args in -
**               YPCREC  - character*80 variable containing data
**    Args out - none
**    Args for work - none
**    Com used - /ALOWOD/NODFLD,/ERRMSG/NCERR
**    Com changed - /OHDIAG/all elements,/ERRMSG/NFATAL
**    Params used - /PARAMS/JPODMX
**    Called by - OHPROC
**    Calls - SEARCH,LPROC,RMULTP,IMULTP,INTCPY,ITRANS,LASTCH,ERRSTP
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /ALOWOD/NODFLD(JPODMX)
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDOHSL,LDOHCT,LDOHED
      COMMON /OHDIAG/LDOHSL,LDOHCT,LDOHED,VOHTS(2),
     -      NOHPR(JPODMX),NOHPL(JPODMX),NOH,NOHVEC(JPODMX),
     -      CINTOH(JPODMX)
      LOGICAL LOFIND,LOALL
      CHARACTER*1 YOCOM,YOEQL
      CHARACTER*2 YOKW2
      CHARACTER*4 YOKW4
      CHARACTER*5 YOKW5
      CHARACTER*8 YOKW8
      CHARACTER*9 YOKW9
      CHARACTER*10 YOKW10
      CHARACTER*14 YOKW14
      CHARACTER*80 YPCREC

      YOEQL='='
      YOCOM=','
**    Search for the keyword in YPCREC and then determine the field
**          value
      LOFIND=.FALSE.

      YOKW2='CI'
      CALL SEARCH(YPCREC,YOKW2,LOFIND)
      IF(LOFIND) THEN
         CALL RMULTP(YPCREC,YOEQL,YOCOM,CINTOH,JPODMX)
         RETURN
      ENDIF

**    Else
      YOKW5='SLICE'
      CALL SEARCH(YPCREC,YOKW5,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDOHSL)
         RETURN
      ENDIF

**    Else
      YOKW8='COLTOTAL'
      CALL SEARCH(YPCREC,YOKW8,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDOHCT)
         RETURN
      ENDIF

**    Else
      YOKW4='EDDY'
      CALL SEARCH(YPCREC,YOKW4,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDOHED)
         RETURN
      ENDIF

**    Else
      YOKW9='HOVMOLLER'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
         CALL RMULTP(YPCREC,YOEQL,YOCOM,VOHTS,2)
         RETURN
      ENDIF

**    Else
      YOKW10='PRINTFIELD'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            CALL INTCPY(NODFLD,JPODMX,NOHPR)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NOHPR,JPODMX)
         RETURN
      ENDIF

**    Else
      YOKW9='PLOTFIELD'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            CALL INTCPY(NODFLD,JPODMX,NOHPL)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NOHPL,JPODMX)
         RETURN
      ENDIF

**    Else
      YOKW14='NUMBEROFFIELDS'
      CALL SEARCH(YPCREC,YOKW14,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
**       Note that IPOSN and IPOSX can take the same value
         IF(IERR.EQ.998) GO TO 1000
         CALL ITRANS(YPCREC,IPOSN,IPOSX,NOH,12)
         RETURN
      ENDIF

**    Else
      YOKW10='WINDVECTOR'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            CALL INTCPY(NODFLD,JPODMX,NOHVEC)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NOHVEC,JPODMX)
         RETURN
      ENDIF

**    Else
**    Keyword not located
      NFATAL=NFATAL+1
      WRITE(NCERR,6030) YPCREC
6030  FORMAT(1X,'KEYWORD NOT LOCATED FOR PHRASE :'/A)
      CALL ERRSTP

1000  CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6000) YPCREC
6000  FORMAT(1X,'OHEXTR: ERROR IN OH DATA ENTRY',/A)
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE OHPROC
**    Function - to read and process the OH diagnostic data
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /ODDIAG/LDOH,/DIAGTP/LDOD,/ERRMSG/NCERR
**    Com changed - /OHDIAG/all elements,/ERRMSG/NFATAL,NWARN
**    Called by - DIAGRD
**    Calls - SEARCH,OHEXTR,ERRSTP
**    Files read - 10
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDOH,LDOZ,LDOM,LDOP,LDO3,LDODTD,LDODTS,LDODIN
      COMMON /ODDIAG/LDOH,LDOZ,LDOM,LDOP,LDO3,LDODTD,LDODTS,LDODIN,
     -      NCOD,NVODHR(JPNL),VODSFC(JPNL)
      LOGICAL LDOHSL,LDOHCT,LDOHED
      COMMON /OHDIAG/LDOHSL,LDOHCT,LDOHED,VOHTS(2),
     -      NOHPR(JPODMX),NOHPL(JPODMX),NOH,NOHVEC(JPODMX),
     -      CINTOH(JPODMX)
      CHARACTER*80 YOCREC
      LOGICAL LOFIND
**    Specify the defaults
      PARAMETER(R999=-999.0,RNEG=-0.1)
      DATA LDOHSL/.FALSE./,LDOHCT/.FALSE./,LDOHED/.FALSE./
      DATA VOHTS/2*R999/
      DATA NOH/0/,NOHVEC/JPODMX*0/,NOHPR/JPODMX*0/,NOHPL/JPODMX*0/
      DATA CINTOH/JPODMX*RNEG/

      IF(.NOT.(LDOD.AND.LDOH)) THEN
**       OH diagnostics not selected - print warning
         NWARN=NWARN+1
         WRITE(NCERR,6000)
6000     FORMAT(1X,'WARNING: - OH DIAGNOSTICS NOT SELECTED:',
     -         ' ALTHOUGH USER HAS SUPPLIED OH INPUT DATA')
      ENDIF

**    Read and interpret records until '$ENDOH' found
300   READ(10,5000,END=100,ERR=110)YOCREC
5000  FORMAT(A80)
      CALL SEARCH(YOCREC,'$ENDOH',LOFIND)
      IF(LOFIND) THEN
         WRITE(NCERR,6010)
6010     FORMAT(1X,'USER-SUPPLIED OH SELECTION DATA PROCESSED')
         RETURN
      ENDIF

      CALL OHEXTR(YOCREC)
      GO TO 300

100   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6020)
6020  FORMAT(1X,'OHPROC: EOF WHEN SEARCHING FOR OH DATA')
      CALL ERRSTP

110   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6030)
6030  FORMAT(1X,'OHPROC: ERROR WHEN SEARCHING FOR OH DATA')
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE CHKOZ
**    Function - to check OZ data input
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /ODDIAG/NVODHR,VODSFC,/OZDIAG/all elements,
**          /DIAGTP/LDOD,/COMDAT/YTYPSF,LWRITE,LASCII,
**          ALOWOD/NODFLD,/ERRMSG/NFATAL,NWARN
**    Com changed - /OZDIAG/NOZPR,NOZPL,NOZ,/ODDIAG/LDOZ,/ERRMSG
**          NFATAL,NWARN
**    Params used - /PARAM1/JPNL,/PARAMS/JPODMX
**    Called by - DIAGCK
**    Calls - LNGCHK
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /ALOWOD/NODFLD(JPODMX)
      LOGICAL LWRITE,LASCII,LSCRAT,LUTF14,LROGR,LMASSW,LAVMSK,
     -      LAVSIG
      CHARACTER*2 YTYPSF
      CHARACTER*6 YDUM6
      CHARACTER*8 YNAME(2)
      COMMON /COMDAT/BEGDAY,ENDDAY,LWRITE,LASCII,LSCRAT,
     -      LUTF14,LROGR,NSIGFG,NFREQD,NFREQA,NFREQP,RNTAPE,
     -      NASCII,LMASSW,LAVMSK,LAVSIG,YNAME,YTYPSF,YDUM6
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDOH,LDOZ,LDOM,LDOP,LDO3,LDODTD,LDODTS,LDODIN
      COMMON /ODDIAG/LDOH,LDOZ,LDOM,LDOP,LDO3,LDODTD,LDODTS,LDODIN,
     -      NCOD,NVODHR(JPNL),VODSFC(JPNL)
      LOGICAL LDOZZM,LDOZTS
      COMMON /OZDIAG/LDOZZM,VOZNS,LDOZTS,VOZNSA(2),NOZPR(JPODMX),
     -      NOZPL(JPODMX),NOZ,NOZVEC(JPODMX),CINTOZ(JPODMX)
      LOGICAL LOERR,LOVALU

      LOERR=.FALSE.

**    OD/OZ diagnostics require that LDOD and LDOZ are set to .TRUE.
      IF((.NOT.LDOD).OR.(.NOT.LDOZ)) THEN
         IF((LDOZZM).OR.(LDOZTS).OR.(VOZNS.GT.0.0).OR.
     -         (VOZNSA(1).GT.0.0).OR.(VOZNSA(2).GT.0.0)) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6000)
6000        FORMAT(1X,'CHKOZ: WARNING - OZ DIAGNOSTICS REQUESTED BUT',
     -            ' BOTH LDOD/LDOZ FLAGS NOT SET')
            RETURN
         ENDIF
      ENDIF

**    Check that printing has been requested - if not, ensure
**          NOZPR=0 for all elements
      IF(.NOT.LWRITE) THEN
         IF(NOZPR(1).NE.0) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6005)
6005        FORMAT(1X,'CHKOZ: NOZPR VALUES CHANGED - NO PRINTING')
            DO 1000 K=1,JPODMX
1000        NOZPR(K)=0
**          Reset value of NOZ
            NOZ=0
            DO 1001 K=1,JPODMX
            IF(NOZPL(K).GT.0) NOZ=NOZ+1
1001        CONTINUE
         ENDIF
      ENDIF

**    Check that UTF output requested - if not, ensure NOZPL=0
**          for all elements
      IF(.NOT.LASCII) THEN
         IF(NOZPL(1).NE.0) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6010)
6010        FORMAT(1X,'CHKOZ: NOZPL VALUES CHANGED - NO PLOTTING')
            DO 1100 K=1,JPODMX
1100        NOZPL(K)=0
**          Reset value of NOZ
            NOZ=0
            DO 1101 K=1,JPODMX
            IF(NOZPR(K).GT.0) NOZ=NOZ+1
1101        CONTINUE
         ENDIF
      ENDIF

**    Check valid code numbers in NOZPR
      IF(NOZPR(1).NE.0) THEN
         DO 2000 K=1,JPODMX
         LOVALU=.FALSE.
         DO 2100 J=1,JPODMX
         IF((NOZPR(K).EQ.NODFLD(J)).OR.(NOZPR(K).EQ.0)) LOVALU=.TRUE.
2100     CONTINUE
         IF(.NOT.LOVALU) THEN
            LOERR=.TRUE.
            NFATAL=NFATAL+1
            WRITE(NCERR,6020) NOZPR(K)
6020        FORMAT(1X,'CHKOZ: ERROR IN NOZPR VALUE ',I4)
         ENDIF
2000     CONTINUE
      ENDIF

**    Check valid code numbers in NOZPL
      IF(NOZPL(1).NE.0) THEN
         DO 3000 K=1,JPODMX
         LOVALU=.FALSE.
         DO 3100 J=1,JPODMX
         IF((NOZPL(K).EQ.NODFLD(J)).OR.(NOZPL(K).EQ.0)) LOVALU=.TRUE.
3100     CONTINUE
         IF(.NOT.LOVALU) THEN
            LOERR=.TRUE.
            NFATAL=NFATAL+1
            WRITE(NCERR,6030) NOZPL(K)
6030        FORMAT(1X,'CHKOZ: ERROR IN NOZPL VALUE ',I4)
         ENDIF
3000     CONTINUE
      ENDIF

**    Switch off diagnostics if NOZ is zero
      IF(NOZ.EQ.0.AND.LDOZ) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6035)
6035     FORMAT(1X,'CHKOZ: OZ DIAGNOSTICS SWITCHED OFF, NOZ=0')
         LDOZ=.FALSE.
         RETURN
      ENDIF
      IF(.NOT.LDOZ) RETURN

**    Check that NOZVEC values correspond to NOZPL values
      IF(NOZVEC(1).NE.0) THEN
         DO 3500 K=1,JPODMX
         LOVALU=.FALSE.
         DO 3600 J=1,JPODMX
         IF((NOZVEC(K).EQ.NOZPL(J)).OR.(NOZVEC(K).EQ.0)) LOVALU=.TRUE.
3600     CONTINUE
         IF(.NOT.LOVALU) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6050)
6050        FORMAT(1X,'CHKOZ: NOZVEC CORRESPONDS TO NON-PLOTTED FIELD')
         ENDIF
3500     CONTINUE
      ENDIF

**    Check that NOZ is non-zero, and less than JPODMX
      IF(NOZ.LE.0.OR.(NOZ.GT.JPODMX)) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6040)
6040     FORMAT(1X,'CHKOZ : INVALID VALUE FOR NOZ')
         LOERR=.TRUE.
      ENDIF

**    Check that NVODHR or VODSFC have been set if LDOZTS is .TRUE.
      IF(LDOZTS.AND.(YTYPSF.EQ.'ET').AND.(NVODHR(1).EQ.0)) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6060)
6060     FORMAT(1X,'CHKOZ: ERROR - ZONAL TIMESERIES O/P REQUIRES'
     -         ' NVODHR TO BE SPECIFIED FOR O/P ON SIGMA SURFACES')
         LOERR=.TRUE.
      ENDIF
      ZDELTA=1.0E-10
      IF(LDOZTS.AND.(YTYPSF.NE.'ET').AND.(VODSFC(1).LE.ZDELTA)) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6070)
6070     FORMAT(1X,'CHKOZ: ERROR - ZONAL TIMESERIES O/P REQUIRES'
     -         ' VODSFC TO BE SPECIFIED FOR O/P ON NON-SIGMA SURFACES')
         LOERR=.TRUE.
      ENDIF

**    Check validity of VOZNS AND VOZNSA values
      CALL LNGCHK(VOZNS,1,-999.0,'(OD) OZ VOZNS',LOERR)
      CALL LNGCHK(VOZNSA,2,-999.0,'(OD) OZ VOZNSA',LOERR)

CC    IF(LOERR) STOP
      RETURN
      END
      SUBROUTINE OZEXTR(YPCREC)
**    Function - to extract information from OZ data and update
**          appropriate common blocks
**    Args in -
**               YPCREC  - character*80 variable containing data
**    Args out - none
**    Args for work - none
**    Com used - /ALOWOD/NODFLD,/ERRMSG/NCERR
**    Com changed - /OZDIAG/all elements,/ERRMSG/NFATAL
**    Params used - /PARAMS/JPODMX
**    Called by - OZPROC
**    Calls - SEARCH,LPROC,RMULTP,LASTCH,RTRANS,IMULTP,INTCPY,
**          ITRANS,LASTCH,ERRSTP
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /ALOWOD/NODFLD(JPODMX)
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDOZZM,LDOZTS
      COMMON /OZDIAG/LDOZZM,VOZNS,LDOZTS,VOZNSA(2),NOZPR(JPODMX),
     -      NOZPL(JPODMX),NOZ,NOZVEC(JPODMX),CINTOZ(JPODMX)
      LOGICAL LOFIND,LOALL
      CHARACTER*1 YOCOM,YOEQL
      CHARACTER*2 YOKW2
      CHARACTER*4 YOKW4
      CHARACTER*7 YOKW7
      CHARACTER*9 YOKW9
      CHARACTER*10 YOKW10
      CHARACTER*14 YOKW14
      CHARACTER*80 YPCREC

      YOEQL='='
      YOCOM=','
**    Search for the keyword in YPCREC and then determine the field
**          value
      LOFIND=.FALSE.

      YOKW2='CI'
      CALL SEARCH(YPCREC,YOKW2,LOFIND)
      IF(LOFIND) THEN
         CALL RMULTP(YPCREC,YOEQL,YOCOM,CINTOZ,JPODMX)
         RETURN
      ENDIF

**    Else
      YOKW9='ZONALMEAN'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDOZZM)
         RETURN
      ENDIF

**    Else
      YOKW9='TIMESLICE'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDOZTS)
         RETURN
      ENDIF

**    Else
      YOKW9='NSAVERAGE'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
         CALL RMULTP(YPCREC,YOEQL,YOCOM,VOZNSA,2)
         RETURN
      ENDIF

**    Else
      YOKW7='NSSLICE'
      CALL SEARCH(YPCREC,YOKW7,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
         IF(IERR.NE.0) GO TO 1000
         CALL RTRANS(YPCREC,IPOSN,IPOSX,VOZNS,12)
         RETURN
      ENDIF

**    Else
      YOKW10='PRINTFIELD'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            CALL INTCPY(NODFLD,JPODMX,NOZPR)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NOZPR,JPODMX)
         RETURN
      ENDIF

**    Else
      YOKW9='PLOTFIELD'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            CALL INTCPY(NODFLD,JPODMX,NOZPL)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NOZPL,JPODMX)
         RETURN
      ENDIF

**    Else
      YOKW14='NUMBEROFFIELDS'
      CALL SEARCH(YPCREC,YOKW14,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
**       Note that IPOSN and IPOSX can take the same value
         IF(IERR.EQ.998) GO TO 1000
         CALL ITRANS(YPCREC,IPOSN,IPOSX,NOZ,12)
         RETURN
      ENDIF

**    Else
      YOKW10='WINDVECTOR'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            CALL INTCPY(NODFLD,JPODMX,NOZVEC)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NOZVEC,JPODMX)
         RETURN
      ENDIF

**    Else
**    Keyword not located
      NFATAL=NFATAL+1
      WRITE(NCERR,6030) YPCREC
6030  FORMAT(1X,'KEYWORD NOT LOCATED FOR PHRASE :'/A)
      CALL ERRSTP

1000  CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6000) YPCREC
6000  FORMAT(1X,'OZEXTR: ERROR IN OZ DATA ENTRY',/A)
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE OZPROC
**    Function - to read and process the OZ diagnostic data
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /ODDIAG/LDOZ,/DIAGTP/LDOD,/ERRMSG/NCERR
**    Com changed - /OZDIAG/all elements,/ERRMSG/NFATAL,NWARN
**    Called by - DIAGRD
**    Calls - SEARCH,OZEXTR,ERRSTP
**    Files read - 10
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDOH,LDOZ,LDOM,LDOP,LDO3,LDODTD,LDODTS,LDODIN
      COMMON /ODDIAG/LDOH,LDOZ,LDOM,LDOP,LDO3,LDODTD,LDODTS,LDODIN,
     -      NCOD,NVODHR(JPNL),VODSFC(JPNL)
      LOGICAL LDOZZM,LDOZTS
      COMMON /OZDIAG/LDOZZM,VOZNS,LDOZTS,VOZNSA(2),NOZPR(JPODMX),
     -      NOZPL(JPODMX),NOZ,NOZVEC(JPODMX),CINTOZ(JPODMX)
      CHARACTER*80 YOCREC
      LOGICAL LOFIND
**    Specify the defaults
      PARAMETER(R999=-999.0,RNEG=-0.1)
      DATA LDOZZM/.FALSE./,LDOZTS/.FALSE./
      DATA VOZNS/R999/,VOZNSA/2*R999/
      DATA NOZ/0/,NOZVEC/JPODMX*0/,NOZPR/JPODMX*0/,NOZPL/JPODMX*0/
      DATA CINTOZ/JPODMX*RNEG/

      IF(.NOT.(LDOD.AND.LDOZ)) THEN
**       OZ diagnostics not selected - print warning
         NWARN=NWARN+1
         WRITE(NCERR,6000)
6000     FORMAT(1X,'WARNING: - OZ DIAGNOSTICS NOT SELECTED:',
     -         ' ALTHOUGH USER HAS SUPPLIED OZ INPUT DATA')
      ENDIF

**    Read and interpret records until '$ENDOZ' found
300   READ(10,5000,END=100,ERR=110)YOCREC
5000  FORMAT(A80)
      CALL SEARCH(YOCREC,'$ENDOZ',LOFIND)
      IF(LOFIND) THEN
         WRITE(NCERR,6010)
6010     FORMAT(1X,'USER-SUPPLIED OZ SELECTION DATA PROCESSED')
         RETURN
      ENDIF

      CALL OZEXTR(YOCREC)
      GO TO 300

100   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6020)
6020  FORMAT(1X,'OZPROC: EOF WHEN SEARCHING FOR OZ DATA')
      CALL ERRSTP

110   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6030)
6030  FORMAT(1X,'OZPROC: ERROR WHEN SEARCHING FOR OZ DATA')
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE CHKOM
**    Function - to check OM data input
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /ODDIAG/NVODHR,VODSFC,/OMDIAG/all elements,
**          /DIAGTP/LDOD,/COMDAT/YTYPSF,LASCII,LWRITE,/ALOWOD/NODFLD,
**          /ERRMSG/NCERR
**    Com changed - /OMDIAG/NOMPR,NOMPL,NOM,/ODDIAG/LDOM,/ERRMSG/
**          NFATAL,NWARN
**    Params used - /PARAM1/JPNL,/PARAMS/JPODMX
**    Called by - DIAGCK
**    Calls - LATCHK
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /ALOWOD/NODFLD(JPODMX)
      LOGICAL LWRITE,LASCII,LSCRAT,LUTF14,LROGR,LMASSW,LAVMSK,
     -      LAVSIG
      CHARACTER*2 YTYPSF
      CHARACTER*6 YDUM6
      CHARACTER*8 YNAME(2)
      COMMON /COMDAT/BEGDAY,ENDDAY,LWRITE,LASCII,LSCRAT,
     -      LUTF14,LROGR,NSIGFG,NFREQD,NFREQA,NFREQP,RNTAPE,
     -      NASCII,LMASSW,LAVMSK,LAVSIG,YNAME,YTYPSF,YDUM6
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDOH,LDOZ,LDOM,LDOP,LDO3,LDODTD,LDODTS,LDODIN
      COMMON /ODDIAG/LDOH,LDOZ,LDOM,LDOP,LDO3,LDODTD,LDODTS,LDODIN,
     -      NCOD,NVODHR(JPNL),VODSFC(JPNL)
      LOGICAL LDOMMM,LDOMTS
      COMMON /OMDIAG/LDOMMM,VOMEW,LDOMTS,VOMEWA(2),NOMPR(JPODMX),
     -      NOMPL(JPODMX),NOM,NOMVEC(JPODMX),CINTOM(JPODMX)
      LOGICAL LOERR,LOVALU

      LOERR=.FALSE.

**    OD/OM diagnostics require that LDOD and LDOM are set to .TRUE.
      IF((.NOT.LDOD).OR.(.NOT.LDOM)) THEN
         IF((LDOMMM).OR.(LDOMTS).OR.(VOMEW.GT.-90.0).OR.
     -         (VOMEWA(1).GT.-90.0).OR.(VOMEWA(2).GT.-90.0)) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6000)
6000        FORMAT(1X,'CHKOM: WARNING - OM DIAGNOSTICS REQUESTED BUT',
     -            ' BOTH LDOD/LDOM FLAGS NOT SET')
            RETURN
         ENDIF
      ENDIF

**    Check that printing has been requested - if not, ensure
**          NOMPR=0 for all elements
      IF(.NOT.LWRITE) THEN
         IF(NOMPR(1).NE.0) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6005)
6005        FORMAT(1X,'CHKOM: NOMPR VALUES CHANGED - NO PRINTING')
            DO 1000 K=1,JPODMX
1000        NOMPR(K)=0
**          Reset value of NOM
            NOM=0
            DO 1001 K=1,JPODMX
            IF(NOMPL(K).GT.0) NOM=NOM+1
1001        CONTINUE
         ENDIF
      ENDIF

**    Check that UTFs are requested - if not, ensure NOMPL=0
**          for all elements
      IF(.NOT.LASCII) THEN
         IF(NOMPL(1).NE.0) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6010)
6010        FORMAT(1X,'CHKOM: NOMPL VALUES CHANGED - NO PLOTTING')
            DO 1100 K=1,JPODMX
1100        NOMPL(K)=0
**          Reset value of NOM
            NOM=0
            DO 1101 K=1,JPODMX
            IF(NOMPR(K).GT.0) NOM=NOM+1
1101        CONTINUE
         ENDIF
      ENDIF

**    Check valid code numbers in NOMPR
      IF(NOMPR(1).NE.0) THEN
         DO 2000 K=1,JPODMX
         LOVALU=.FALSE.
         DO 2100 J=1,JPODMX
         IF((NOMPR(K).EQ.NODFLD(J)).OR.(NOMPR(K).EQ.0)) LOVALU=.TRUE.
2100     CONTINUE
         IF(.NOT.LOVALU) THEN
            LOERR=.TRUE.
            NFATAL=NFATAL+1
            WRITE(NCERR,6020) NOMPR(K)
6020        FORMAT(1X,'CHKOM: ERROR IN NOMPR VALUE ',I4)
         ENDIF
2000     CONTINUE
      ENDIF

**    Check valid code numbers in NOMPL
      IF(NOMPL(1).NE.0) THEN
         DO 3000 K=1,JPODMX
         LOVALU=.FALSE.
         DO 3100 J=1,JPODMX
         IF((NOMPL(K).EQ.NODFLD(J)).OR.(NOMPL(K).EQ.0)) LOVALU=.TRUE.
3100     CONTINUE
         IF(.NOT.LOVALU) THEN
            LOERR=.TRUE.
            NFATAL=NFATAL+1
            WRITE(NCERR,6030) NOMPL(K)
6030        FORMAT(1X,'CHKOM: ERROR IN NOMPL VALUE ',I4)
         ENDIF
3000     CONTINUE
      ENDIF

**    Switch off diagnostics if NOM is zero
      IF(NOM.EQ.0.AND.LDOM) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6035)
6035     FORMAT(1X,'CHKOM: OM DIAGNOSTICS SWITCHED OFF, NOM=0')
         LDOM=.FALSE.
         RETURN
      ENDIF
      IF(.NOT.LDOM) RETURN

**    Check that NOMVEC values correspond to NOMPL values
      IF(NOMVEC(1).NE.0) THEN
         DO 3500 K=1,JPODMX
         LOVALU=.FALSE.
         DO 3600 J=1,JPODMX
         IF((NOMVEC(K).EQ.NOMPL(J)).OR.(NOMVEC(K).EQ.0)) LOVALU=.TRUE.
3600     CONTINUE
         IF(.NOT.LOVALU) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6050)
6050        FORMAT(1X,'CHKOM: NOMVEC CORRESPONDS TO NON-PLOTTED FIELD')
         ENDIF
3500     CONTINUE
      ENDIF

**    Check that NOM is non-zero, and less than JPODMX
      IF(NOM.LE.0.OR.(NOM.GT.JPODMX)) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6040)
6040     FORMAT(1X,'CHKOM : INVALID VALUE FOR NOM')
         LOERR=.TRUE.
      ENDIF

**    Check that NVODHR or VODSFC have been set if LDOMTS is .TRUE.
      IF(LDOMTS.AND.(YTYPSF.EQ.'ET').AND.(NVODHR(1).EQ.0)) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6060)
6060     FORMAT(1X,'CHKOM: ERROR - MERIDIONAL TIMESERIES O/P REQUIRES'
     -         ' NVODHR TO BE SPECIFIED FOR O/P ON SIGMA SURFACES')
         LOERR=.TRUE.
      ENDIF
      ZDELTA=1.0E-10
      IF(LDOMTS.AND.(YTYPSF.NE.'ET').AND.(VODSFC(1).LE.ZDELTA)) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6070)
6070     FORMAT(1X,'CHKOM: ERROR - MERIDIONAL TIMESERIES O/P REQUIRES'
     -         ' VODSFC TO BE SPECIFIED FOR O/P ON NON-SIGMA SURFACES')
         LOERR=.TRUE.
      ENDIF

**    Check validity of VOMEW AND VOMEWA values
      CALL LATCHK(VOMEW,1,-999.0,'(OD) OM VOMEW',LOERR)
      CALL LATCHK(VOMEWA,2,-999.0,'(OD) OM VOMEWA',LOERR)

CC    IF(LOERR) STOP
      RETURN
      END
      SUBROUTINE OMEXTR(YPCREC)
**    Function - to extract information from OM data and update
**          appropriate common blocks
**    Args in -
**               YPCREC  - character*80 variable containing data
**    Args out - none
**    Args for work - none
**    Com used - /ALOWOD/NODFLD,/ERRMSG/NCERR
**    Com changed - /OMDIAG/all elements,/ERRMSG/NFATAL
**    Params used - /PARAMS/JPODMX
**    Called by - OMPROC
**    Calls - SEARCH,LPROC,RMULTP,LASTCH,RTRANS,IMULTP,INTCPY,ITRANS,
**          ERRSTP
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /ALOWOD/NODFLD(JPODMX)
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDOMMM,LDOMTS
      COMMON /OMDIAG/LDOMMM,VOMEW,LDOMTS,VOMEWA(2),NOMPR(JPODMX),
     -      NOMPL(JPODMX),NOM,NOMVEC(JPODMX),CINTOM(JPODMX)
      LOGICAL LOFIND,LOALL
      CHARACTER*1 YOCOM,YOEQL
      CHARACTER*2 YOKW2
      CHARACTER*4 YOKW4
      CHARACTER*7 YOKW7
      CHARACTER*9 YOKW9
      CHARACTER*10 YOKW10
      CHARACTER*14 YOKW14
      CHARACTER*80 YPCREC

      YOEQL='='
      YOCOM=','
**    Search for the keyword in YPCREC and then determine the field
**          value
      LOFIND=.FALSE.

      YOKW9='MERIDMEAN'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDOMMM)
         RETURN
      ENDIF

**    Else
      YOKW2='CI'
      CALL SEARCH(YPCREC,YOKW2,LOFIND)
      IF(LOFIND) THEN
         CALL RMULTP(YPCREC,YOEQL,YOCOM,CINTOM,JPODMX)
         RETURN
      ENDIF


**    Else
      YOKW9='TIMESLICE'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDOMTS)
         RETURN
      ENDIF

**    Else
      YOKW9='WEAVERAGE'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
         CALL RMULTP(YPCREC,YOEQL,YOCOM,VOMEWA,2)
         RETURN
      ENDIF

**    Else
      YOKW7='WESLICE'
      CALL SEARCH(YPCREC,YOKW7,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
         IF(IERR.NE.0) GO TO 1000
         CALL RTRANS(YPCREC,IPOSN,IPOSX,VOMEW,12)
         RETURN
      ENDIF

**    Else
      YOKW10='PRINTFIELD'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            CALL INTCPY(NODFLD,JPODMX,NOMPR)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NOMPR,JPODMX)
         RETURN
      ENDIF

**    Else
      YOKW9='PLOTFIELD'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            CALL INTCPY(NODFLD,JPODMX,NOMPL)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NOMPL,JPODMX)
         RETURN
      ENDIF

**    Else
      YOKW14='NUMBEROFFIELDS'
      CALL SEARCH(YPCREC,YOKW14,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
**       Note that IPOSN and IPOSX can take the same value
         IF(IERR.EQ.998) GO TO 1000
         CALL ITRANS(YPCREC,IPOSN,IPOSX,NOM,12)
         RETURN
      ENDIF

**    Else
      YOKW10='WINDVECTOR'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            CALL INTCPY(NODFLD,JPODMX,NOMVEC)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NOMVEC,JPODMX)
         RETURN
      ENDIF

**    Else
**    Keyword not located
      NFATAL=NFATAL+1
      WRITE(NCERR,6030) YPCREC
6030  FORMAT(1X,'KEYWORD NOT LOCATED FOR PHRASE :'/A)
      CALL ERRSTP

1000  CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6000) YPCREC
6000  FORMAT(1X,'OMEXTR: ERROR IN OM DATA ENTRY',/A)
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE OMPROC
**    Function - to read and process the OM diagnostic data
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /ODDIAG/LDOM,/DIAGTP/LDOD,/ERRMSG/NCERR
**    Com changed - /OMDIAG/all elements,/ERRMSG/NWARN,NFATAL
**    Called by - DIAGRD
**    Calls - SEARCH,OMEXTR,ERRSTP
**    Files read - 10
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDOH,LDOZ,LDOM,LDOP,LDO3,LDODTD,LDODTS,LDODIN
      COMMON /ODDIAG/LDOH,LDOZ,LDOM,LDOP,LDO3,LDODTD,LDODTS,LDODIN,
     -      NCOD,NVODHR(JPNL),VODSFC(JPNL)
      LOGICAL LDOMMM,LDOMTS
      COMMON /OMDIAG/LDOMMM,VOMEW,LDOMTS,VOMEWA(2),NOMPR(JPODMX),
     -      NOMPL(JPODMX),NOM,NOMVEC(JPODMX),CINTOM(JPODMX)
      CHARACTER*80 YOCREC
      LOGICAL LOFIND
**    Specify the defaults
      PARAMETER(R999=-999.0,RNEG=-0.1)
      DATA LDOMMM/.FALSE./,LDOMTS/.FALSE./
      DATA VOMEW/R999/,VOMEWA/2*R999/
      DATA NOM/0/,NOMVEC/JPODMX*0/,NOMPR/JPODMX*0/,NOMPL/JPODMX*0/
      DATA CINTOM/JPODMX*RNEG/

      IF(.NOT.(LDOD.AND.LDOM)) THEN
**       OM diagnostics not selected - print warning
         NWARN=NWARN+1
         WRITE(NCERR,6000)
6000     FORMAT(1X,'WARNING: - OM DIAGNOSTICS NOT SELECTED:',
     -         ' ALTHOUGH USER HAS SUPPLIED OM INPUT DATA')
      ENDIF

**    Read and interpret records until '$ENDOM' found
300   READ(10,5000,END=100,ERR=110)YOCREC
5000  FORMAT(A80)
      CALL SEARCH(YOCREC,'$ENDOM',LOFIND)
      IF(LOFIND) THEN
         WRITE(NCERR,6010)
6010     FORMAT(1X,'USER-SUPPLIED OM SELECTION DATA PROCESSED')
         RETURN
      ENDIF

      CALL OMEXTR(YOCREC)
      GO TO 300

100   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6020)
6020  FORMAT(1X,'OMPROC: EOF WHEN SEARCHING FOR OM DATA')
      CALL ERRSTP

110   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6030)
6030  FORMAT(1X,'OMPROC: ERROR WHEN SEARCHING FOR OM DATA')
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE CHKOP
**    Function - to check OP data input
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /OPDIAG/all elements,/DIAGTP/LDOD,
**          /COMDAT/LWRITE,LASCII,YTYPSF,/ALOWOD/,NODFLD,/ERRMSG/NCERR
**    Com changed - /OPDIAG/NOPPR,NOPPL,NOP,LDOPCT,/ODDIAG/LDOP,
**          /ERRMSG/NFATAL/NWARN
**    Params used - /PARAM1/JPNL,/PARAMS/JPODMX,JPOPPF
**    Called by - DIAGCK
**    Calls - LATCHK,LNGCHK
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /ALOWOD/NODFLD(JPODMX)
      LOGICAL LWRITE,LASCII,LSCRAT,LUTF14,LROGR,LMASSW,LAVMSK,
     -      LAVSIG
      CHARACTER*2 YTYPSF
      CHARACTER*6 YDUM6
      CHARACTER*8 YNAME(2)
      COMMON /COMDAT/BEGDAY,ENDDAY,LWRITE,LASCII,LSCRAT,
     -      LUTF14,LROGR,NSIGFG,NFREQD,NFREQA,NFREQP,RNTAPE,
     -      NASCII,LMASSW,LAVMSK,LAVSIG,YNAME,YTYPSF,YDUM6
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDOH,LDOZ,LDOM,LDOP,LDO3,LDODTD,LDODTS,LDODIN
      COMMON /ODDIAG/LDOH,LDOZ,LDOM,LDOP,LDO3,LDODTD,LDODTS,LDODIN,
     -      NCOD,NVODHR(JPNL),VODSFC(JPNL)
      LOGICAL LDOPCT,LDOPCP
      COMMON /OPDIAG/LDOPCT,NOPPF,VOPPF(2,JPOPPF),NOPPR(JPODMX),
     -      NOPPL(JPODMX),NOP,CINTOP(JPODMX),LDOPCP
      LOGICAL LOERR,LOVALU

      LOERR=.FALSE.

**    OD/OP diagnostics require that LDOD and LDOP are set to .TRUE.
      IF((.NOT.LDOD).OR.(.NOT.LDOP)) THEN
         IF((LDOPCT).OR.(VOPPF(1,1).GT.-90.0).OR.(LDOPCP)) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6000)
6000        FORMAT(1X,'CHKOP: WARNING - OP DIAGNOSTICS REQUESTED BUT',
     -            ' BOTH LDOD/LDOP FLAGS NOT SET')
            RETURN
         ENDIF
      ENDIF

**    LDOPCT only valid if output is on model surfaces
      IF(YTYPSF.NE.'ET'.AND.LDOPCT) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6005)
6005     FORMAT(1X,'CHKOP: ERROR - OP COLUMN TOTALS CANNOT BE FOUND',
     -         'WHEN OUTPUT NOT ON ET SURFACES')
         LDOPCT=.FALSE.
         LOERR=.TRUE.
      ENDIF

**    Check that printing has been requested - if not, ensure
**          NOPPR=0 for all elements
      IF(.NOT.LWRITE) THEN
         IF(NOPPR(1).NE.0) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6060)
6060        FORMAT(1X,'CHKOP: NOPPR VALUES CHANGED - NO PRINTING')
            DO 1000 K=1,JPODMX
1000        NOPPR(K)=0
**          Reset value of NOP
            NOP=0
            DO 1001 K=1,JPODMX
            IF(NOPPL(K).GT.0) NOP=NOP+1
1001        CONTINUE
         ENDIF
      ENDIF

**    Check that UTFs are requested - if not, ensure NOPPL=0
**          for all elements
      IF(.NOT.LASCII) THEN
         IF(NOPPL(1).NE.0) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6010)
6010        FORMAT(1X,'CHKOP: NOPPL VALUES CHANGED - NO PLOTTING')
            DO 1100 K=1,JPODMX
1100        NOPPL(K)=0
**          Reset value of NOP
            NOP=0
            DO 1101 K=1,JPODMX
            IF(NOPPR(K).GT.0) NOP=NOP+1
1101        CONTINUE
         ENDIF
      ENDIF

**    Check valid code numbers in NOPPR
      IF(NOPPR(1).NE.0) THEN
         DO 2000 K=1,JPODMX
         LOVALU=.FALSE.
         DO 2100 J=1,JPODMX
         IF((NOPPR(K).EQ.NODFLD(J)).OR.(NOPPR(K).EQ.0)) LOVALU=.TRUE.
2100     CONTINUE
         IF(.NOT.LOVALU) THEN
            LOERR=.TRUE.
            NFATAL=NFATAL+1
            WRITE(NCERR,6020) NOPPR(K)
6020        FORMAT(1X,'CHKOP: ERROR IN NOPPR VALUE ',I4)
         ENDIF
2000     CONTINUE
      ENDIF

**    Check valid code numbers in NOPPL
      IF(NOPPL(1).NE.0) THEN
         DO 3000 K=1,JPODMX
         LOVALU=.FALSE.
         DO 3100 J=1,JPODMX
         IF((NOPPL(K).EQ.NODFLD(J)).OR.(NOPPL(K).EQ.0)) LOVALU=.TRUE.
3100     CONTINUE
         IF(.NOT.LOVALU) THEN
            LOERR=.TRUE.
            NFATAL=NFATAL+1
            WRITE(NCERR,6030) NOPPL(K)
6030        FORMAT(1X,'CHKOP: ERROR IN NOPPL VALUE ',I4)
         ENDIF
3000     CONTINUE
      ENDIF

**    Switch off diagnostics if NOP is zero
      IF(NOP.EQ.0.AND.LDOP) THEN

         NWARN=NWARN+1
         WRITE(NCERR,6035)
6035     FORMAT(1X,'CHKOP: OP DIAGNOSTICS SWITCHED OFF, NOP=0')
         LDOP=.FALSE.
         RETURN
      ENDIF
      IF(.NOT.LDOP) RETURN

**    Check that NOP is non-zero, and less than JPODMX
      IF(NOP.LE.0.OR.(NOP.GT.JPODMX)) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6040)
6040     FORMAT(1X,'CHKOP : INVALID VALUE FOR NOP')
         LOERR=.TRUE.
      ENDIF

**    Check validity of profile coordinates (IPROFL is max no. of
**    profiles allowed)
      IF(NOPPF.LT.0.OR.NOPPF.GT.JPOPPF) THEN
         NFATAL=NFATAL+1
         LOERR=.TRUE.
         WRITE(NCERR,6050)
6050     FORMAT(1X,'CHKOP: ERROR - INVALID NOPPF')
      ENDIF

      IF(NOPPF.GT.0) THEN
         DO 100 JL=1,NOPPF
         CALL LATCHK(VOPPF(1,JL),1,-999.0,'(OD) OP VOPPF',LOERR)
         CALL LNGCHK(VOPPF(2,JL),1,-999.0,'(OD) OP VOPPF',LOERR)
         IF((VOPPF(1,JL).LT.-90.0).OR.(VOPPF(2,JL).LT.-90.0)) THEN
            NFATAL=NFATAL+1
            WRITE(NCERR,6080)
6080        FORMAT(1X,'CHKOP: ERROR - INVALID PROFILE COORDINATES')
            LOERR=.TRUE.
         ENDIF
100      CONTINUE
      ENDIF

CC    IF(LOERR) STOP
      RETURN
      END
      SUBROUTINE OPEXTR(YPCREC)
**    Function - to extract information from OP data and update
**          appropriate common blocks
**    Args in -
**               YPCREC  - character*80 variable containing data
**    Args out - none
**    Args for work - none
**    Com used - /ALOWOD/NODFLD,/ERRMSG/NCERR
**    Com changed - /OPDIAG/all elements,/ERRMSG/NFATAL
**    Params used - /PARAMS/JPOPPF
**    Called by - OPPROC
**    Calls - SEARCH,LPROC,RMULTP,LASTCH,ITRANS,IMULTP,INTCPY,ERRSTP
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /ALOWOD/NODFLD(JPODMX)
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDOPCT,LDOPCP
      COMMON /OPDIAG/LDOPCT,NOPPF,VOPPF(2,JPOPPF),NOPPR(JPODMX),
     -      NOPPL(JPODMX),NOP,CINTOP(JPODMX),LDOPCP
      LOGICAL LOFIND,LOALL
      CHARACTER*1 YOCOM,YOEQL
      CHARACTER*2 YOKW2
      CHARACTER*4 YOKW4
      CHARACTER*8 YOKW8
      CHARACTER*9 YOKW9
      CHARACTER*10 YOKW10
      CHARACTER*13 YOKW13
      CHARACTER*14 YOKW14
      CHARACTER*80 YPCREC

      YOEQL='='
      YOCOM=','
**    Search for the keyword in YPCREC and then determine the field
**          value
      LOFIND=.FALSE.

      YOKW8='COLTOTAL'
      CALL SEARCH(YPCREC,YOKW8,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDOPCT)
         RETURN
      ENDIF

**    Else
      YOKW10='COLPROFILE'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDOPCP)
         RETURN
      ENDIF

**    Else
      YOKW2='CI'
      CALL SEARCH(YPCREC,YOKW2,LOFIND)
      IF(LOFIND) THEN
         CALL RMULTP(YPCREC,YOEQL,YOCOM,CINTOP,JPODMX)
         RETURN
      ENDIF


**    Else
      YOKW14='NUMBEROFCOORDS'
      CALL SEARCH(YPCREC,YOKW14,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
**       Note that IPOSN and IPOSX can take the same value
         IF(IERR.EQ.998) GO TO 1000
         CALL ITRANS(YPCREC,IPOSN,IPOSX,NOPPF,12)
         RETURN
      ENDIF

**    Else
      YOKW13='PROFILECOORDS'
      CALL SEARCH(YPCREC,YOKW13,LOFIND)
      IF(LOFIND) THEN
         CALL RMULTP(YPCREC,YOEQL,YOCOM,VOPPF,2*JPOPPF)
         RETURN
      ENDIF

**    Else
      YOKW10='PRINTFIELD'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            CALL INTCPY(NODFLD,JPODMX,NOPPR)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NOPPR,JPODMX)
         RETURN
      ENDIF

**    Else
      YOKW9='PLOTFIELD'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            CALL INTCPY(NODFLD,JPODMX,NOPPL)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NOPPL,JPODMX)
         RETURN
      ENDIF

**    Else
      YOKW14='NUMBEROFFIELDS'
      CALL SEARCH(YPCREC,YOKW14,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
**       Note that IPOSN and IPOSX can take the same value
         IF(IERR.EQ.998) GO TO 1000
         CALL ITRANS(YPCREC,IPOSN,IPOSX,NOP,12)
         RETURN
      ENDIF

**    Else
**    Keyword not located
      NFATAL=NFATAL+1
      WRITE(NCERR,6030) YPCREC
6030  FORMAT(1X,'KEYWORD NOT LOCATED FOR PHRASE :'/A)
      CALL ERRSTP

1000  CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6000) YPCREC
6000  FORMAT(1X,'OPEXTR: ERROR IN OP DATA ENTRY',/A)
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE OPPROC
**    Function - to read and process the OP diagnostic data
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /ODDIAG/LDOP,/DIAGTP/LDOD,/ERRMSG/NCERR
**    Com changed - /OPDIAG/all elements,/ERRMSG/NFATAL,NWARN
**    Params - JPOP2,RNEG,/PARAMS/JPOPPF
**    Called by - DIAGRD
**    Calls - SEARCH,OPEXTR,ERRSTP
**    Files read - 10
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LDOH,LDOZ,LDOM,LDOP,LDO3,LDODTD,LDODTS,LDODIN
      COMMON /ODDIAG/LDOH,LDOZ,LDOM,LDOP,LDO3,LDODTD,LDODTS,LDODIN,
     -      NCOD,NVODHR(JPNL),VODSFC(JPNL)
      LOGICAL LDOPCT,LDOPCP
      COMMON /OPDIAG/LDOPCT,NOPPF,VOPPF(2,JPOPPF),NOPPR(JPODMX),
     -      NOPPL(JPODMX),NOP,CINTOP(JPODMX),LDOPCP
      CHARACTER*80 YOCREC
      LOGICAL LOFIND
**    Specify the defaults
      PARAMETER(R999=-999.0,RNEG=-0.1,JPOP2=2*JPOPPF)
      DATA LDOPCT/.FALSE./,LDOPCP/.FALSE./
      DATA NOPPF/0/,VOPPF/JPOP2*R999/
      DATA NOP/0/,NOPPR/JPODMX*0/,NOPPL/JPODMX*0/
      DATA CINTOP/JPODMX*RNEG/

      IF(.NOT.(LDOD.AND.LDOP)) THEN
**       OP diagnostics not selected - print warning
         NWARN=NWARN+1
         WRITE(NCERR,6000)
6000     FORMAT(1X,'WARNING: - OP DIAGNOSTICS NOT SELECTED:',
     -         ' ALTHOUGH USER HAS SUPPLIED OP INPUT DATA')
      ENDIF

**    Read and interpret records until '$ENDOP' found
300   READ(10,5000,END=100,ERR=110)YOCREC
5000  FORMAT(A80)
      CALL SEARCH(YOCREC,'$ENDOP',LOFIND)
      IF(LOFIND) THEN
         WRITE(NCERR,6010)
6010     FORMAT(1X,'USER-SUPPLIED OP SELECTION DATA PROCESSED')
         RETURN
      ENDIF

      CALL OPEXTR(YOCREC)
      GO TO 300

100   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6020)
6020  FORMAT(1X,'OPPROC: EOF WHEN SEARCHING FOR OP DATA')
      CALL ERRSTP

110   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6030)
6030  FORMAT(1X,'OPPROC: ERROR WHEN SEARCHING FOR OP DATA')
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE CHKO3
**    Function - to check O3 data input
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /ODDIAG/LDO3,/O3DIAG/NCO3,/DIAGTP/LDOD,/COMDAT/
**          LASCII,/ALOWOD/NODFLD,/ERRMSG/NCERR
**    Com changed - /O3DIAG/NO3,NO3PL,LDO3,/ERRMSG/NFATAL,NWARN
**    Com changed - /O3DIAG/NO3,NO3PL,LDO3
**    Params used - /PARAM1/JPNL,/PARAMS/JPODMX
**    Called by - DIAGCK
**    Calls - none
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /ALOWOD/NODFLD(JPODMX)
      LOGICAL LWRITE,LASCII,LSCRAT,LUTF14,LROGR,LMASSW,LAVMSK,
     -      LAVSIG
      CHARACTER*2 YTYPSF
      CHARACTER*6 YDUM6
      CHARACTER*8 YNAME(2)
      COMMON /COMDAT/BEGDAY,ENDDAY,LWRITE,LASCII,LSCRAT,
     -      LUTF14,LROGR,NSIGFG,NFREQD,NFREQA,NFREQP,RNTAPE,
     -      NASCII,LMASSW,LAVMSK,LAVSIG,YNAME,YTYPSF,YDUM6
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      COMMON /O3DIAG/NCO3,NO3PL(JPODMX),NO3
      LOGICAL LDOH,LDOZ,LDOM,LDOP,LDO3,LDODTD,LDODTS,LDODIN
      COMMON /ODDIAG/LDOH,LDOZ,LDOM,LDOP,LDO3,LDODTD,LDODTS,LDODIN,
     -      NCOD,NVODHR(JPNL),VODSFC(JPNL)
      LOGICAL LOERR,LOVALU

      LOERR=.FALSE.

**    Check that UTFs are requested - if not, ensure NO3PL=0
**          for all elements
      IF(.NOT.LASCII) THEN
         IF(NO3PL(1).NE.0) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6010)
6010        FORMAT(1X,'CHKO3: NO3PL VALUES CHANGED - NO O/P')
            DO 1100 K=1,JPODMX
1100        NO3PL(K)=0
**          Reset value of NO3
            NO3=0
         ENDIF
      ENDIF

**    Check valid code numbers in NO3PL
      IF(NO3PL(1).NE.0) THEN
         DO 3000 K=1,JPODMX
         LOVALU=.FALSE.
         DO 3100 J=1,JPODMX
         IF((NO3PL(K).EQ.NODFLD(J)).OR.(NO3PL(K).EQ.0)) LOVALU=.TRUE.
3100     CONTINUE
         IF(.NOT.LOVALU) THEN
            LOERR=.TRUE.
            NFATAL=NFATAL+1
            WRITE(NCERR,6030) NO3PL(K)
6030        FORMAT(1X,'CHKO3: ERROR IN NO3PL VALUE ',I4)
         ENDIF
3000     CONTINUE
      ENDIF

**    Switch off diagnostics if NO3 is zero
      IF(NO3.EQ.0.AND.LDO3) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6035)
6035     FORMAT(1X,'CHKO3: O3 DIAGNOSTICS SWITCHED OFF, NO3=0')
         LDO3=.FALSE.
         RETURN
      ENDIF
      IF(.NOT.LDO3) RETURN

**    Check that NO3 is non-zero, and less than JPODMX
      IF(NO3.LE.0.OR.(NO3.GT.JPODMX)) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6040)
6040     FORMAT(1X,'CHKO3 : INVALID VALUE FOR NO3')
         LOERR=.TRUE.
      ENDIF

**    Check channel for output of 3-D fields
      IF(LDO3.AND.NCO3.LE.0) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6050)
6050     FORMAT(1X,'CHKO3: ERROR - O3 DIAGNOSTICS REQUESTED BUT',
     -         ' NCO3 CHANNEL NUMBER INVALID')
         LOERR=.TRUE.
      ENDIF

CC    IF(LOERR) STOP
      RETURN
      END
      SUBROUTINE O3EXTR(YPCREC)
**    Function - to extract information from O3 data and update
**          appropriate common blocks
**    Args in -
**               YPCREC  - character*80 variable containing data
**    Args out - none
**    Args for work - none
**    Com used - /ALOWOD/NODFLD,/ERRMSG/NCERR
**    Com changed - /O3DIAG/NCO3,/ERRMSG/NFATAL
**    Params used - /PARAMS/JPODMX
**    Called by - O3PROC
**    Calls - SEARCH,LASTCH,ITRANS,INTCPY,IMULTP,ERRSTP
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /ALOWOD/NODFLD(JPODMX)
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      COMMON /O3DIAG/NCO3,NO3PL(JPODMX),NO3
      LOGICAL LOFIND,LOALL
      CHARACTER*1 YOCOM,YOEQL
      CHARACTER*4 YOKW4
      CHARACTER*9 YOKW9
      CHARACTER*10 YOKW10
      CHARACTER*14 YOKW14
      CHARACTER*80 YPCREC

      YOEQL='='
      YOCOM=','
**    Search for the keyword in YPCREC and then determine the field
**          value
      LOFIND=.FALSE.

      YOKW10='O/PCHANNEL'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
**       Note that IPOSN and IPOSX can take the same value
         IF(IERR.EQ.998) GO TO 1000
         CALL ITRANS(YPCREC,IPOSN,IPOSX,NCO3,12)
         RETURN
      ENDIF

**    Else
      YOKW9='PLOTFIELD'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            CALL INTCPY(NODFLD,JPODMX,NO3PL)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NO3PL,JPODMX)
         RETURN
      ENDIF

**    Else
      YOKW14='NUMBEROFFIELDS'
      CALL SEARCH(YPCREC,YOKW14,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
**       Note that IPOSN and IPOSX can take the same value
         IF(IERR.EQ.998) GO TO 1000
         CALL ITRANS(YPCREC,IPOSN,IPOSX,NO3,12)
         RETURN
      ENDIF

**    Else
**    Keyword not located
      NFATAL=NFATAL+1
      WRITE(NCERR,6030) YPCREC
6030  FORMAT(1X,'KEYWORD NOT LOCATED FOR PHRASE :'/A)
      CALL ERRSTP

1000  CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6000) YPCREC
6000  FORMAT(1X,'O3EXTR: ERROR IN O3 DATA ENTRY',/A)
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE O3PROC
**    Function - to read and process the O3 diagnostic data
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /ODDIAG/LDO3,/DIAGTP/LDOD,/ERRMSG/NCERR
**    Com changed - /O3DIAG/NCO3,/ERRMSG/NFATAL/NWARN
**    Called by - DIAGRD
**    Calls - SEARCH,O3EXTR,ERRSTP
**    Files read - 10
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      COMMON /O3DIAG/NCO3,NO3PL(JPODMX),NO3
      LOGICAL LDOH,LDOZ,LDOM,LDOP,LDO3,LDODTD,LDODTS,LDODIN
      COMMON /ODDIAG/LDOH,LDOZ,LDOM,LDOP,LDO3,LDODTD,LDODTS,LDODIN,
     -      NCOD,NVODHR(JPNL),VODSFC(JPNL)
      CHARACTER*80 YOCREC
      LOGICAL LOFIND
**    Specify the defaults
      DATA NCO3/1/
      DATA NO3/0/,NO3PL/JPODMX*0/

      IF(.NOT.(LDOD.AND.LDO3)) THEN
**       O3 diagnostics not selected - print warning
         NWARN=NWARN+1
         WRITE(NCERR,6000)
6000     FORMAT(1X,'WARNING: - O3 DIAGNOSTICS NOT SELECTED:',
     -         ' ALTHOUGH USER HAS SUPPLIED O3 INPUT DATA')
      ENDIF

**    Read and interpret records until '$ENDO3' found
300   READ(10,5000,END=100,ERR=110)YOCREC
5000  FORMAT(A80)
      CALL SEARCH(YOCREC,'$ENDO3',LOFIND)
      IF(LOFIND) THEN
         WRITE(NCERR,6010)
6010     FORMAT(1X,'USER-SUPPLIED O3 SELECTION DATA PROCESSED')
         RETURN
      ENDIF

      CALL O3EXTR(YOCREC)
      GO TO 300

100   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6020)
6020  FORMAT(1X,'O3PROC: EOF WHEN SEARCHING FOR O3 DATA')
      CALL ERRSTP

110   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6030)
6030  FORMAT(1X,'O3PROC: ERROR WHEN SEARCHING FOR O3 DATA')
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE CHKZF
**    Function - to check ZF data input
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /COMDAT/LWRITE,LASCII,BEGDAY,ENDDAY
**          /ZFDIAG/NVZFHR,VZFSFC,LZFEDF,LZFMNF,LDZFIN,LDZFLM,LDZFZM,
**          FACTZF,NZF,/MODELC/LGCMTR,NL
**          /COMDAT/YTYPSF,/LVDIAG/OUTLEV,/ERRMSG/NCERR
**    Com changed - /DIAGTP/LDZF,/ERRMSG/NFATAL,NWARN,/ZFDIAG/LDZFTS,
**          LDZFTD,NFLDZF
**    Params used - /PARAM1/JPNL,/PARAMS/JPSGMX,JPZFMX,JPTRMX
**    Called by - DIAGCK
**    Calls - none
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      PARAMETER(JPALOW=JPSGMX+JPTRMX)
      LOGICAL LWRITE,LASCII,LSCRAT,LUTF14,LROGR,LMASSW,LAVMSK,
     -      LAVSIG
      CHARACTER*2 YTYPSF
      CHARACTER*6 YDUM6
      CHARACTER*8 YNAME(2)
      COMMON /COMDAT/BEGDAY,ENDDAY,LWRITE,LASCII,LSCRAT,
     -      LUTF14,LROGR,NSIGFG,NFREQD,NFREQA,NFREQP,RNTAPE,
     -      NASCII,LMASSW,LAVMSK,LAVSIG,YNAME,YTYPSF,YDUM6
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      COMMON /LVDIAG/OUTLEV(JPNL)
      LOGICAL LGCMTR,LHISPH,LHISXP,LDRY,LDIABH,LBL,LVD
      COMMON /MODELC/TSPD,NYEAR,NDAY,NMIN,LGCMTR,NL,TMEAN(JPNL),
     -      LHISPH,LHISXP,LDRY,LDIABH,BEGDYP,LBL,LVD,AKVV,CD
      LOGICAL LZFEDF,LZFMNF,LDZFIN,LDZFTD,LDZFTS,LDZFZM,LDZFLM
      COMMON /ZFDIAG/NCZF,NZFPR(JPZFMX),NZFPL(JPZFMX),
     -      CINTZF(JPZFMX),NVZFHR(JPNL),
     -      NZF,FACTZF(2,JPZFMX),NFLDZF(4,JPZFMX),VZFSFC(JPNL),
     -      LZFEDF,LZFMNF,LDZFIN,LDZFTD,LDZFTS,LDZFZM,LDZFLM
      LOGICAL LOERR,LOVALU,LOEQN(JPTRMX),LOCODE
      DIMENSION IZFFLD(0:JPALOW)

      LOERR=.FALSE.

**    Switch off zonal mean fluxes if no output requested.
      IF((.NOT.LZFEDF).AND.(.NOT.LZFMNF)) THEN
         WRITE(6,6105)
6105     FORMAT(1X,'CHKZF: NEITHER MEAN OR EDDY COMPONENTS ',
     -         'REQUESTED; ZF CALCULATIONS TURNED OFF')
         NWARN=NWARN+1
         LDZF=.FALSE.
      ENDIF

      IF((.NOT.LDZFIN).AND.(.NOT.LDZFTS).AND.(.NOT.LDZFTD)) THEN
         WRITE(6,6110)
6110     FORMAT(1X,'CHKZF: NO TIME CONTROL FLAGS SET; ',
     -         'ZF CALCULATIONS TURNED OFF')
         LDZF=.FALSE.
         NWARN=NWARN+1
      ENDIF

      IF((.NOT.LDZFZM).AND.(.NOT.LDZFLM)) THEN
         WRITE(6,6115)
6115     FORMAT(1X,'CHKZF: NEITHER ZONAL MEAN OR LEVEL MEAN ',
     -         'OUTPUT REQUESTED; ZF CALCULATIONS TURNED OFF')
         LDZF=.FALSE.
         NWARN=NWARN+1
      ENDIF

**    Switch off time-series and time-averaging calculations if
**    ENDDAY is not greater than BEGDAY
      IF(ABS(ENDDAY).LE.ABS(BEGDAY)) THEN


         IF(LDZFTS) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6100)
6100        FORMAT(1X,'CHKZF: ZF TIME-SERIES SWITCHED OFF, SINCE ',
     -            'ENDDAY.LE.BEGDAY')
            LDZFTS=.FALSE.
         ENDIF
         IF(LDZFTD) THEN
            NWARN=NWARN+1
            WRITE(NCERR,6120)
6120        FORMAT(1X,'CHKZF: ZF TIME-AVERAGES (D) SWITCHED OFF,',
     -            ' SINCE ENDDAY.LE.BEGDAY')
            LDZFTD=.FALSE.
         ENDIF
      ENDIF

**    NVZFHR values only used when output is on model surfaces, and
**    VZFSFC values only used when output is on non-model surfaces
      IF((NVZFHR(1).NE.0).AND.(YTYPSF.NE.'ET')) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6060)
6060     FORMAT(1X,'CHKZF: NVZFHR VALUE(S) SUPPLIED, BUT NOT USED')
      ENDIF
      IF((NVZFHR(1).LE.0).AND.(YTYPSF.EQ.'ET')) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6065)
6065     FORMAT(1X,'CHKZF: NVZFHR VALUES NOT SUPPLIED,'
     -         ' OUTPUT MAY BE SUPPRESSED')
      ENDIF
      IF((VZFSFC(1).GT.0.0).AND.(YTYPSF.EQ.'ET')) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6070)
6070     FORMAT(1X,'CHKZF: VZFSFC VALUE(S) SUPPLIED, BUT NOT USED')
      ENDIF
      IF((VZFSFC(1).LE.0.0).AND.(YTYPSF.NE.'ET')) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6075)
6075     FORMAT(1X,'CHKZF: VZFSFC VALUES NOT SUPPLIED,'
     -         ' OUTPUT MAY BE SUPPRESSED')
      ENDIF

**    Check that VZFSFC values are a subset of OUTLEV values if they are
**    to be used. ZDELTA is a small parameter to allow two REAL values
**    to be compared
      ZDELTA=1.0E-10
      IF(YTYPSF.NE.'ET') THEN
         DO 4000 K=1,NL
         IF(VZFSFC(K).GT.0.0) THEN
            ZSFCP=VZFSFC(K)+ZDELTA
            ZSFCM=VZFSFC(K)-ZDELTA
            LOVALU=.FALSE.
            DO 4100 J=1,NL
            IF(ZSFCM.LE.OUTLEV(J).AND.ZSFCP.GE.OUTLEV(J)) LOVALU=.TRUE.
4100        CONTINUE
            IF(.NOT.LOVALU) THEN
               LOERR=.TRUE.
               NFATAL=NFATAL+1
               WRITE(NCERR,6080) VZFSFC(K)
6080           FORMAT(1X,'CHKZF: VZFSFC CORRESPONDS TO INVALID LEVEL ',
     -               F12.3)
            ENDIF
         ENDIF
4000     CONTINUE
      ENDIF

**    Check that NVZFHR values lie in the range 1 to NL
      IF(YTYPSF.EQ.'ET') THEN
         DO 5000 J=1,NL
         IF(NVZFHR(J).NE.0) THEN
            IF(NVZFHR(J).LT.1.OR.NVZFHR(J).GT.NL) THEN
               NFATAL=NFATAL+1
               WRITE(NCERR,6090)
6090           FORMAT(1X,'CHKZF: INVALID NVZFHR VALUE')
               LOERR=.TRUE.
            ENDIF
         ENDIF
5000     CONTINUE
      ENDIF

**    Permissible field codes are 1->14, 17->19 and
**          101->100+JPTRMX (the latter only if tracer code was used
**          in the original GCM run). A value of 0 can be used to
**          signify a null field.
      IZFFLD(0)=0
      DO 100 J=1,JPALOW
      IF(J.LE.JPSGMX) THEN
         IZFFLD(J)=J
      ELSE IF(LGCMTR) THEN
         IZFFLD(J)=100+J-JPSGMX
      ELSE
         IZFFLD(J)=0
      ENDIF
      IF((J.EQ.15).OR.(J.EQ.16).OR.(J.EQ.20).OR.(J.EQ.21)) IZFFLD(J)=0
100   CONTINUE

**    Now check each formula in turn, and determine its validity
      DO 200 J=1,JPZFMX
      LOEQN(J)=.TRUE.
**    First multiplicative factor
      IF((ABS(FACTZF(1,J)).LT.1.0E-10).AND.((NFLDZF(1,J).NE.0).OR.
     -      (NFLDZF(2,J).NE.0))) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6200) J
6200     FORMAT(1X,'CHKZF ERROR: FIRST FACTOR OF EQN ',I2,' IS ZERO')
         LOERR=.TRUE.
         LOEQN(J)=.FALSE.
         NFLDZF(1,J)=0
         NFLDZF(2,J)=0
      ENDIF
**    Second multiplicative factor
      IF((ABS(FACTZF(2,J)).LT.1.0E-10).AND.((NFLDZF(3,J).NE.0).OR.
     -      (NFLDZF(4,J).NE.0))) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6210) J
6210     FORMAT(1X,'CHKZF ERROR: SECOND FACTOR OF EQN ',I2,' IS ZERO')
         LOERR=.TRUE.
         LOEQN(J)=.FALSE.
         NFLDZF(3,J)=0
         NFLDZF(4,J)=0
      ENDIF
**    If both factors are zero, equation is invalid
      IF(ABS(FACTZF(1,J)).LT.1.0E-10.AND.
     -      ABS(FACTZF(2,J)).LT.1.0E-10) THEN
         LOEQN(J)=.FALSE.
         GO TO 200
      ENDIF
**    Check each field code
      CALL CHKVAL(LOCODE,NFLDZF(1,J),4,IZFFLD(0),JPALOW+1)
      IF(.NOT.LOCODE) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6220)J,NFLDZF(1,J),NFLDZF(2,J),NFLDZF(3,J),
     -         NFLDZF(4,J)
6220     FORMAT(1X,'CHKZF ERROR: EQN ',I2,' INVALID FIELD CODE ',
     -         /'ONE OF THE FOLLOWING IS INCORRECT ',4I5)
         LOERR=.TRUE.
         NFLDZF(1,J)=0
         NFLDZF(2,J)=0
         NFLDZF(3,J)=0
         NFLDZF(4,J)=0
         LOEQN(J)=.FALSE.
      ENDIF
200   CONTINUE
**    Finally, check validity of first NZF equations
      IVALID=0
      DO 300 J=1,NZF
      IF(LOEQN(J)) IVALID=IVALID+1
300   CONTINUE
      IF(NZF.NE.IVALID) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6300)
6300     FORMAT(1X,'CHKZF ERROR: EITHER NZF IS WRONG, OR ONE OF THE ',
     -         'FIRST NZF EQNS IS INVALID')
         LOERR=.TRUE.
      ENDIF

CC    IF(LOERR) STOP
      RETURN
      END
      SUBROUTINE ZFEXTR(YPCREC)
**    Function - to extract information from ZF data and update
**          appropriate common blocks
**    Args in -
**               YPCREC  - character*80 variable containing data
**    Args out - none
**    Args for work - none
**    Com used - /ERRMSG/NCERR,/MODELC/NL
**    Com changed - /ZFDIAG/all elements,/ERRMSG/NFATAL
**    Params used - /PARAM1/JPNL,/PARAMS/JPZFMX
**    Called by - ZFPROC
**    Calls - SEARCH,LPROC,INTCPY,IMULTP,LASTCH,ITRANS,RMULTP,ERRSTP,
**          DCODTF
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LGCMTR,LHISPH,LHISXP,LDRY,LDIABH,LBL,LVD
      COMMON /MODELC/TSPD,NYEAR,NDAY,NMIN,LGCMTR,NL,TMEAN(JPNL),
     -      LHISPH,LHISXP,LDRY,LDIABH,BEGDYP,LBL,LVD,AKVV,CD
      LOGICAL LZFEDF,LZFMNF,LDZFIN,LDZFTD,LDZFTS,LDZFZM,LDZFLM
      COMMON /ZFDIAG/NCZF,NZFPR(JPZFMX),NZFPL(JPZFMX),
     -      CINTZF(JPZFMX),NVZFHR(JPNL),
     -      NZF,FACTZF(2,JPZFMX),NFLDZF(4,JPZFMX),VZFSFC(JPNL),
     -      LZFEDF,LZFMNF,LDZFIN,LDZFTD,LDZFTS,LDZFZM,LDZFLM
      LOGICAL LOFIND,LOALL
      DIMENSION INLFLD(JPNL)
      CHARACTER*1 YOCOM,YOEQL
      CHARACTER*2 YOKW2
      CHARACTER*4 YOKW4
      CHARACTER*7 YOKW7
      CHARACTER*8 YOKW8
      CHARACTER*9 YOKW9
      CHARACTER*10 YOKW10
      CHARACTER*13 YOKW13
      CHARACTER*14 YOKW14
      CHARACTER*80 YPCREC

      YOEQL='='
      YOCOM=','
**    Search for the keyword in YPCREC and then determine the field
**          value
      LOFIND=.FALSE.

      YOKW9='ZONALMEAN'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDZFZM)
         RETURN
      ENDIF

**    Else
      YOKW9='LEVELMEAN'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDZFLM)
         RETURN
      ENDIF

**    Else
      YOKW8='EDDYFLUX'
      CALL SEARCH(YPCREC,YOKW8,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LZFEDF)
         RETURN
      ENDIF

**    Else
      YOKW8='MEANFLUX'
      CALL SEARCH(YPCREC,YOKW8,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LZFMNF)
         RETURN
      ENDIF

**    Else
      YOKW9='TAVERAGED'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDZFTD)
         RETURN
      ENDIF

**    Else
      YOKW13='INSTANTANEOUS'
      CALL SEARCH(YPCREC,YOKW13,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDZFIN)
         RETURN
      ENDIF

**    Else
      YOKW7='TSERIES'
      CALL SEARCH(YPCREC,YOKW7,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LDZFTS)
         RETURN
      ENDIF

**    Else
      YOKW10='O/PCHANNEL'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
**       Note that IPOSN and IPOSX can take the same value
         IF(IERR.EQ.998) GO TO 1000
         CALL ITRANS(YPCREC,IPOSN,IPOSX,NCZF,12)
         RETURN
      ENDIF

**    Else
      YOKW8='ZFLEVELS'
      CALL SEARCH(YPCREC,YOKW8,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            DO 3500 JL=1,NL
3500        INLFLD(JL)=JL
            CALL INTCPY(INLFLD,NL,NVZFHR)
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NVZFHR,NL)
         RETURN
      ENDIF

**    Else
      YOKW10='ZFSURFACES'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
      IF(LOFIND) THEN
         CALL RMULTP(YPCREC,YOEQL,YOCOM,VZFSFC,NL)
         RETURN
      ENDIF

**    Else
      YOKW2='CI'
      CALL SEARCH(YPCREC,YOKW2,LOFIND)
      IF(LOFIND) THEN
         CALL RMULTP(YPCREC,YOEQL,YOCOM,CINTZF,JPZFMX)
         RETURN
      ENDIF

**    Else
      YOKW14='NUMBEROFFIELDS'
      CALL SEARCH(YPCREC,YOKW14,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
**       Note that IPOSN and IPOSX can take the same value
         IF(IERR.EQ.998) GO TO 1000
         CALL ITRANS(YPCREC,IPOSN,IPOSX,NZF,12)
         RETURN
      ENDIF

**    Else
      YOKW7='FORMULA'
      CALL SEARCH(YPCREC,YOKW7,LOFIND)
      IF(LOFIND) THEN
         CALL DCODTF(YPCREC,YOEQL,YOCOM,NFLDZF,4,JPZFMX,FACTZF,2)
         RETURN
      ENDIF

**    Else
      YOKW10='PRINTFIELD'
      CALL SEARCH(YPCREC,YOKW10,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            NFATAL=NFATAL+1
            WRITE(NCERR,6035)
6035        FORMAT(1X,'ALL - INVALID ENTRY FOR ZF/PRINTFIELD')
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NZFPR,JPZFMX)
         RETURN
      ENDIF

**    Else
      YOKW9='PLOTFIELD'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
**    LOALL set to .TRUE. if '=ALL' occurs in phrase, indicating
**          all fields are selected
      IF(LOFIND) THEN
         YOKW4='=ALL'
         CALL SEARCH(YPCREC,YOKW4,LOALL)
         IF(LOALL) THEN
            NFATAL=NFATAL+1
            WRITE(NCERR,6036)
6036        FORMAT(1X,'ALL - INVALID ENTRY FOR ZF/PLOTFIELD')
            RETURN
         ENDIF
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NZFPL,JPZFMX)

         RETURN
      ENDIF

**    Else
**    Keyword not located
      NFATAL=NFATAL+1
      WRITE(NCERR,6030) YPCREC
6030  FORMAT(1X,'KEYWORD NOT LOCATED FOR PHRASE :'/A)
      CALL ERRSTP

1000  CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6000) YPCREC
6000  FORMAT(1X,'ZFEXTR: ERROR IN ZF DATA ENTRY',/A)
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE ZFPROC
**    Function - to read and process the ZF diagnostic data
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /DIAGTP/LDZF,/ERRMSG/NCERR
**    Com changed - /ZFDIAG/all elements,/ERRMSG/NFATAL,
**          NWARN
**    Called by - DIAGRD
**    Calls - SEARCH,ZFEXTR,ERRSTP
**    Files read - 10
**    Files written - ERRSTP
**    Author - R. Brugge, University of Reading

      PARAMETER(JPNL=100)
      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      PARAMETER(JPFACT=2*JPZFMX,JPFLD=4*JPZFMX)
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LZFEDF,LZFMNF,LDZFIN,LDZFTD,LDZFTS,LDZFZM,LDZFLM
      COMMON /ZFDIAG/NCZF,NZFPR(JPZFMX),NZFPL(JPZFMX),
     -      CINTZF(JPZFMX),NVZFHR(JPNL),
     -      NZF,FACTZF(2,JPZFMX),NFLDZF(4,JPZFMX),VZFSFC(JPNL),
     -      LZFEDF,LZFMNF,LDZFIN,LDZFTD,LDZFTS,LDZFZM,LDZFLM
      CHARACTER*80 YOCREC
      LOGICAL LOFIND
**    Specify the defaults
      PARAMETER(RNEG=-0.1)
      DATA NCZF/3/,NZF/0/
      DATA NZFPR/JPZFMX*0/,NZFPL/JPZFMX*0/
      DATA LDZFZM/.TRUE./,LDZFLM/.FALSE./
      DATA LZFEDF/.FALSE./,LZFMNF/.FALSE./
      DATA LDZFTS/.FALSE./,LDZFTD/.TRUE./,LDZFIN/.FALSE./
      DATA NVZFHR/JPNL*0/,VZFSFC/JPNL*0.0/
      DATA FACTZF/JPFACT*0.0/,NFLDZF/JPFLD*0/
      DATA CINTZF/JPZFMX*RNEG/

      IF(.NOT.LDZF) THEN
**       ZF diagnostics not selected - print warning
         NWARN=NWARN+1
         WRITE(NCERR,6000)
6000     FORMAT(1X,'WARNING: - ZF DIAGNOSTICS NOT SELECTED:',
     -         ' ALTHOUGH USER HAS SUPPLIED ZF INPUT DATA')
      ENDIF

**    Read and interpret records until '$ENDZF' found
300   READ(10,5000,END=100,ERR=110)YOCREC
5000  FORMAT(A80)
      CALL SEARCH(YOCREC,'$ENDZF',LOFIND)
      IF(LOFIND) THEN
         WRITE(NCERR,6010)
6010     FORMAT(1X,'USER-SUPPLIED ZF SELECTION DATA PROCESSED')
         RETURN
      ENDIF

      CALL ZFEXTR(YOCREC)
      GO TO 300

100   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6020)
6020  FORMAT(1X,'ZFPROC: EOF WHEN SEARCHING FOR ZF DATA')
      CALL ERRSTP

110   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6030)
6030  FORMAT(1X,'ZFPROC: ERROR WHEN SEARCHING FOR ZF DATA')
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE CHKTJ
**    Function - to check TJ data input
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /ERRMSG/NCERR,/TJDIAG/YINBIN,YOPBIN,LUTFLV,LUTFTJ,
**          NATTR,NTJATT,/COMDAT/YTYPSF,LUTF14,LASCII
**    Com changed - /ERRMSG/NFATAL.NWARN,/DIAGTP/LDTJ
**    Params used - /PARAMS/JPTJMX
**    Called by - DIAGCK
**    Calls - CHKVAL
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      LOGICAL LWRITE,LASCII,LSCRAT,LUTF14,LROGR,LMASSW,LAVMSK,
     -      LAVSIG
      CHARACTER*2 YTYPSF
      CHARACTER*6 YDUM6
      CHARACTER*8 YNAME(2)
      COMMON /COMDAT/BEGDAY,ENDDAY,LWRITE,LASCII,LSCRAT,
     -      LUTF14,LROGR,NSIGFG,NFREQD,NFREQA,NFREQP,RNTAPE,
     -      NASCII,LMASSW,LAVMSK,LAVSIG,YNAME,YTYPSF,YDUM6
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LUTFTJ,LBINTJ,LUTFLV
      CHARACTER*2 YTYPTJ
      CHARACTER*6 YTJDUM
      CHARACTER*80 YINBIN,YOPBIN
      COMMON /TJDIAG/NTJATT,NATTR(JPTJMX),LUTFTJ,LBINTJ,
     -      NGRPTJ,LUTFLV,YTYPTJ,YTJDUM,YINBIN,YOPBIN
      DIMENSION ITJFLD(0:JPTJMX)
      LOGICAL LOERR,LOVALS

      NFAT1=NFATAL

      IF(.NOT.LDTJ) RETURN

**    Check that trajectory diagnostics are compatible with diagnostic
**    level type.
      IF(YTYPSF.NE.'ET') THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6000)
6000     FORMAT(1X,'***CHKTJ: ERROR - TRAJECTORY DIAGNOSTICS NOT ',
     -         'POSSIBLE UNLESS SURFACE TYPE SET TO ET')
      ENDIF

**    TJ diagnostics require that YINBIN is not a null string
      LOERR=.TRUE.
      DO 100 J=1,80
      IF(YINBIN(J:J).NE.' ') LOERR=.FALSE.
100   CONTINUE
      IF(LOERR) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6010) YINBIN
6010     FORMAT(1X,'***CHKTJ: ERROR - INVALID INPUT BINARY DATA',
     -         'FILENAME FOR TRAJECTORIES'/'          ',A80)
      ENDIF

**    Number of attributes must be in the range 0 to JPTJMX inclusive
      IF(NTJATT.LT.0.OR.NTJATT.GT.JPTJMX) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6020)
6020     FORMAT(1X,'***CHKTJ: ERROR - INVALID NUMBER OF OUTPUT ',
     -         'ATTRIBUTES :',I8)
      ENDIF

**    Attribute code numbers must be valid for TJ diagnostics
      DO 200 J=0,JPTJMX
      ITJFLD(J)=0
200   CONTINUE
      DO 205 J=1,16
      ITJFLD(J)=J+3
205   CONTINUE
      DO 210 J=1,JPTRMX
      JJ=J+16
      ITJFLD(JJ)=100+J
210   CONTINUE
      CALL CHKVAL(LOVALS,NATTR,NTJATT,ITJFLD(0),JPTJMX+1)
      IF(.NOT.LOVALS) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6030)
6030     FORMAT(1X,'***CHKTJ: ERROR - INVALID TRAJECTORY ATTRIBUTE',
     -         ' DEFINED BY USER')
      ENDIF

**    Trajectory UTF's only available with UTF1.4
      IF(((.NOT.LUTF14).OR.(.NOT.LASCII)).AND.LUTFTJ) THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6040) LUTF14,LASCII,LUTFTJ
6040     FORMAT(1X,'***CHKTJ: ERROR - TRAJECTORY UTFS NOT AVALAIBLE '/
     -         'UTF VERSION 1.4 SELECTED : ',L1/
     -         'UTFS ENABLED             : ',L1/
     -         'TRAJECTORY UTF REQUESTED : ',L1)
      ENDIF

**    Binary output only available if YOPBIN is not a null string
      IF(LBINTJ) THEN
         LOERR=.TRUE.
         DO 300 J=1,80
         IF(YOPBIN(J:J).NE.' ') LOERR=.FALSE.
300      CONTINUE
         IF(LOERR) THEN
            NFATAL=NFATAL+1
            WRITE(NCERR,6050) YOPBIN
6050        FORMAT(1X,'***CHKTJ: ERROR - INVALID OUTPUT BINARY DATA',
     -            'FILENAME FOR TRAJECTORIES'/'          ',A80)
         ENDIF
      ENDIF

**    Trajectory 'level' type must be isobaric or isentropic
      IF(YTYPTJ.NE.'IS'.AND.YTYPTJ.NE.'TH') THEN
         NFATAL=NFATAL+1
         WRITE(NCERR,6060) YTYPTJ
6060     FORMAT(1X,'***CHKTJ: ERROR - INVALID SURFACE TYPE DEFINED ',
     -         'FOR TRAJECTORY LEVELS : ',A2)
      ENDIF

**    Switch off diagnostics if no output selected
      IF((.NOT.LBINTJ).AND.(.NOT.LUTFTJ)) THEN
         NWARN=NWARN+1
         WRITE(NCERR,6070)
         LDTJ=.FALSE.
6070     FORMAT(1X,'***CHKTJ: WARNING - TJ DIAGNOSTICS SWITCHED SINCE ',
     -         'NO TJ OUTPUT SELECTED')
      ENDIF

      IF(NFATAL.GT.NFAT1) THEN
**       Switch off TJ diagnstics
         NWARN=NWARN+1
         WRITE(NCERR,6080)
6080     FORMAT(1X,'***CHKTJ: WARNING - TRAJACTORY DIAGNOSTICS ',
     -         'SWITCHED OFF DUE TO PREVIOUS ERRORS')
         LDTJ=.FALSE.
      ENDIF

      RETURN
      END
      SUBROUTINE TJEXTR(YPCREC)
**    Function - to extract information from TJ data and update
**          appropriate common blocks
**    Args in -
**               YPCREC  - character*80 variable containing data
**    Args out - none
**    Args for work - none
**    Com used - /ERRMSG/NCERR
**    Com changed - /TJDIAG/all elements,/ERRMSG/NFATAL
**    Params used - /PARAMS/JPTJMX
**    Called by - TJPROC
**    Calls - SEARCH,LASTCH,INVALD,LPROC,ITRANS,IMULTP,ERRSTP
**    Files read - none
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LUTFTJ,LBINTJ,LUTFLV
      CHARACTER*2 YTYPTJ
      CHARACTER*6 YTJDUM
      CHARACTER*80 YINBIN,YOPBIN
      COMMON /TJDIAG/NTJATT,NATTR(JPTJMX),LUTFTJ,LBINTJ,
     -      NGRPTJ,LUTFLV,YTYPTJ,YTJDUM,YINBIN,YOPBIN
      LOGICAL LOFIND,LOVALD
      CHARACTER*1 YOCOM,YOEQL
      CHARACTER*9 YOKW9
      CHARACTER*11 YOKW11
      CHARACTER*12 YOKW12
      CHARACTER*13 YOKW13
      CHARACTER*21 YOKW21
      CHARACTER*22 YOKW22
      CHARACTER*80 YPCREC

      YOEQL='='
      YOCOM=','
**    Search for the keyword in YPCREC and then determine the field
**          value
      LOFIND=.FALSE.

      YOKW11='INPUTBINARY'
      CALL SEARCH(YPCREC,YOKW11,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
         IF(IERR.NE.0) GO TO 1000
         YINBIN=YPCREC(IPOSN:IPOSX)
         RETURN
      ENDIF

**    Else
      YOKW12='OUTPUTBINARY'
      CALL SEARCH(YPCREC,YOKW12,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
         IF(IERR.NE.0) GO TO 1000
         YOPBIN=YPCREC(IPOSN:IPOSX)
         RETURN
      ENDIF

**    Else
      YOKW12='UTFLEVELTYPE'
      CALL SEARCH(YPCREC,YOKW12,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         IPOSX=IPOSN+1
         LOVALD=.FALSE.
         IF(YPCREC(IPOSN:IPOSX).EQ.'IS')LOVALD=.TRUE.
         IF(YPCREC(IPOSN:IPOSX).EQ.'TH')LOVALD=.TRUE.
         IF(LOVALD) THEN
            YTYPTJ=YPCREC(IPOSN:IPOSX)
         ELSE
            CALL INVALD(YPCREC)
         ENDIF
         RETURN
      ENDIF

**    Else
      YOKW9='UTFOUTPUT'
      CALL SEARCH(YPCREC,YOKW9,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LUTFTJ)
         RETURN
      ENDIF

**    Else
      YOKW12='BINARYOUTPUT'
      CALL SEARCH(YPCREC,YOKW12,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LBINTJ)
         RETURN
      ENDIF

**    Else
      YOKW12='UTFLEVELINFO'
      CALL SEARCH(YPCREC,YOKW12,LOFIND)
      IF(LOFIND) THEN
         CALL LPROC(YPCREC,YOEQL,LUTFLV)
         RETURN
      ENDIF

**    Else
      YOKW21='NUMBEROFOUTATTRIBUTES'
      CALL SEARCH(YPCREC,YOKW21,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
**       Note that IPOSN and IPOSX can take the same value
         IF(IERR.EQ.998) GO TO 1000
         CALL ITRANS(YPCREC,IPOSN,IPOSX,NTJATT,12)
         RETURN
      ENDIF

**    Else
      YOKW13='OUTATTRIBUTES'
      CALL SEARCH(YPCREC,YOKW13,LOFIND)
      IF(LOFIND) THEN
         CALL IMULTP(YPCREC,YOEQL,YOCOM,NATTR,JPTJMX)
         RETURN
      ENDIF


**    Else
      YOKW22='UTFTRAJECTORYGROUPSIZE'
      CALL SEARCH(YPCREC,YOKW22,LOFIND)
      IF(LOFIND) THEN
**       IPOSN is the location of first character defining field value
         IPOSN=INDEX(YPCREC,YOEQL)+1
**       IPOSX is the location of last character defining field value
         CALL LASTCH(YPCREC,80,IPOSN,IPOSX,IERR)
**       Note that IPOSN and IPOSX can take the same value
         IF(IERR.EQ.998) GO TO 1000
         CALL ITRANS(YPCREC,IPOSN,IPOSX,NGRPTJ,12)
         RETURN
      ENDIF

**    Else
**    Keyword not located
      NFATAL=NFATAL+1
      WRITE(NCERR,6030) YPCREC
6030  FORMAT(1X,'KEYWORD NOT LOCATED FOR PHRASE :'/A)
      CALL ERRSTP

1000  CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6000) YPCREC
6000  FORMAT(1X,'TJEXTR: ERROR IN ZF DATA ENTRY',/A)
      CALL ERRSTP

      RETURN
      END
      SUBROUTINE TJPROC
**    Function - to read and process the TJ diagnostic data
**    Args in - none
**    Args out - none
**    Args for work - none
**    Com used - /DIAGTP/LDTJ,/ERRMSG/NCERR
**    Com changed - /TJDIAG/all elements,/ERRMSG/NFATAL,
**          NWARN
**    Called by - DIAGRD
**    Calls - SEARCH,TJEXTR,ERRSTP
**    Files read - 10
**    Files written - NCERR
**    Author - R. Brugge, University of Reading

      PARAMETER(JPPHMX=12,JPXPMX=38,JPCOMX=6,JPSGMX=21,
     -      JPGPPF=10,JPTRMX=50,JPTPPF=10,JPTFMX=10,JPFPPF=10,
     -      JPODMX=7,JPOPPF=10,JPZFMX=10,JPTJMX=JPSGMX+JPTRMX)
      LOGICAL LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /DIAGTP/LDPH,LDXP,LDCO,LDSG,LDTR,LDTF,LDOD,LDZF,LDTJ
      COMMON /ERRMSG/NCERR,NFATAL,NWARN
      LOGICAL LUTFTJ,LBINTJ,LUTFLV
      CHARACTER*2 YTYPTJ
      CHARACTER*6 YTJDUM
      CHARACTER*80 YINBIN,YOPBIN
      COMMON /TJDIAG/NTJATT,NATTR(JPTJMX),LUTFTJ,LBINTJ,
     -      NGRPTJ,LUTFLV,YTYPTJ,YTJDUM,YINBIN,YOPBIN
      CHARACTER*20 YONULL
      CHARACTER*80 YOCREC
      LOGICAL LOFIND
**    Specify the defaults
      DATA YONULL/'                    '/
      DATA NTJATT/0/
      DATA LUTFTJ/.FALSE./,LBINTJ/.FALSE./,LUTFLV/.FALSE./
      DATA YTYPTJ/'IS'/
      DATA NGRPTJ/25/
      DATA NATTR/JPTJMX*0/
      DATA  YINBIN/'                                                    
     -                            '/
      DATA  YOPBIN/'                                                    
     -                            '/
C      YINBIN=YONULL//YONULL//YONULL//YONULL
C      YOPBIN=YONULL//YONULL//YONULL//YONULL

      IF(.NOT.LDTJ) THEN
**       TJ diagnostics not selected - print warning
         NWARN=NWARN+1
         WRITE(NCERR,6000)
6000     FORMAT(1X,'WARNING: - TJ DIAGNOSTICS NOT SELECTED:',
     -         ' ALTHOUGH USER HAS SUPPLIED TJ INPUT DATA')
      ENDIF

**    Read and interpret records until '$ENDTJ' found
300   READ(10,5000,END=100,ERR=110)YOCREC
5000  FORMAT(A80)
      CALL SEARCH(YOCREC,'$ENDTJ',LOFIND)
      IF(LOFIND) THEN
         WRITE(NCERR,6010)
6010     FORMAT(1X,'USER-SUPPLIED TJ SELECTION DATA PROCESSED')
         RETURN
      ENDIF

      CALL TJEXTR(YOCREC)
      GO TO 300

100   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6020)
6020  FORMAT(1X,'TJPROC: EOF WHEN SEARCHING FOR TJ DATA')
      CALL ERRSTP

110   CONTINUE
      NFATAL=NFATAL+1
      WRITE(NCERR,6030)
6030  FORMAT(1X,'TJPROC: ERROR WHEN SEARCHING FOR TJ DATA')
      CALL ERRSTP

      RETURN
      END
