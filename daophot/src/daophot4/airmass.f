      PARAMETER (NOBS=15, MAXCOO=1000, MAXFLT=99)
      CHARACTER*143 LINE
      CHARACTER*72 VALUE, FILTER(0:MAXFLT), COMMNT
      CHARACTER POSITN*31
      CHARACTER*40 PREFIX, NAME, EXTEND, CAPS, CALNAM, OBJECT, SWITCH,
     .     CASE, INPUT
      CHARACTER*40 CNAME(MAXCOO)
      CHARACTER*8 BLANK, RAKEY, DECKEY, EQUKEY, FILKEY, UTKEY, EXPKEY,
     .     DATKEY
      CHARACTER RAMODE*4, RNDOFF*8
      CHARACTER*3 MONTH(12)
      DOUBLE PRECISION OLONG(0:NOBS+1), OLAT(0:NOBS+1)
      REAL DATE(3), EPOCH(MAXCOO), TSEC(MAXCOO), DSEC(MAXCOO)
      INTEGER JHR(MAXCOO), JTMIN(MAXCOO), IDEG(MAXCOO), IAMIN(MAXCOO)
      CHARACTER*1 SIGN(MAXCOO), S
      DOUBLE PRECISION HA, GJD, DELT, XLST, TIME
      LOGICAL DAY, DECIML, IMAGE, OGLE
      DATA BLANK /'        '/
      DATA OLONG /0., 123.4166667D0, 155.4716667D0, 70.815D0,
     .     111.6D0, 17.88D0, 116.8633D0, 118.06D0, 
     .     70.73D0, 70.405D0, 70.702D0, -149.00776D0, -149.0706D0, 
     .     109.889611D0, -18.4775D0, 70.1036273D0, 0./
      DATA OLAT  /0., 48.52D0, 19.82666667D0, -30.165D0, 
     .     31.96333333D0, 28.758D0, 33.3567D0, 34.2167D0, 
     .    -29.2567D0, -24.628D0, -29.003D0, -35.31805D0, -31.2733D0, 
     .     32.7015D0, -33.9343D0, 41.2806787D0, 0./
      DATA MONTH /'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul',
     .     'Aug', 'Sep', 'Oct', 'Nov', 'Dec'/
      DATA RAMODE /'NULL'/
      OGLE = .FALSE.
      OBJECT = ' '
      DO I=0,MAXFLT
         FILTER(I) = ' '
      END DO
      NAME = CASE('coords.dat')
      CALL INFILE (1, NAME, ISTAT)
      NCOO = 0
      IF (ISTAT .EQ. 0) THEN
  350    NCOO = NCOO + 1
         READ (1,100,END=550) CNAME(NCOO), JHR(NCOO), JTMIN(NCOO), 
     .        TSEC(NCOO), SIGN(NCOO), IDEG(NCOO), IAMIN(NCOO), 
     .        DSEC(NCOO), EPOCH(NCOO)
  100    FORMAT (A35, 2X, 2I3, F6.2, 1X, A1, I2, I3, F5.1, F7.1)
         GO TO 350
  550    NCOO = NCOO-1
      ELSE
         CALL OUTFIL (1, NAME, ISTAT)
      END IF
      NAME = ' '
      CALL GETNAM ('Output file name:', NAME)
      NAME = EXTEND(NAME, CASE('inf'))
      CALL INFILE (3, NAME, ISTAT)
      IF (ISTAT .EQ. 0) THEN
  900    READ (3,3,END=1000) LINE
    3    FORMAT (A)
         GO TO 900
      ELSE
         CALL OUTFIL (3, NAME, ISTAT)
      END IF
C
 1000 WRITE (6,600)
  600 FORMAT(/15X, '                        Geocentric = 0' /
     .        15X, 'Dominion Astrophysical Observatory = 1' /
     .        15X, '                         Mauna Kea = 2' /
     .        15X, '                      Cerro Tololo = 3' /
     .        15X, '                         Kitt Peak = 4' /
     .        15X, '                          La Palma = 5' /
     .        15X, '                           Palomar = 6' /
     .        15X, '                      Mount Wilson = 7' /
     .        15X, '                          La Silla = 8' /
     .        15X, '                           Paranal = 9' /
     .        15X, '                      Las Campanas = 10' /
     .        15X, '                     Mount Stromlo = 11' /
     .        15X, '                     Siding Spring = 12' /
     .        15X, '                      Mount Graham = 13' /
     .        15X, '                        Sutherland = 14' /
     .        15X, '                    Maria Mitchell = 15' /
     .        15X, '                             Other = 16' /)
      CALL GETDAT (' Enter observatory:', DATA, 1)
      IF (DATA .LE. -1.E38) CALL BYEBYE
      IOBS = NINT(DATA)
      IF ((IOBS .LT. 0) .OR. (IOBS .GT. NOBS)) THEN
C
C Another observatory.
C
         IOBS = NOBS+1
 1110    WRITE (6,601) 
  601    FORMAT (/' Enter longitude (western hemisphere > 0)',
     .        ' and latitude in format shown: '//
     .        'sddd mm.mm sdd mm.mm')
         CALL RDCHAR (5, LINE, N, ISTAT)
         READ (LINE(1:N),500,END=9100,ERR=1110) S, OLONG(IOBS),
     .        AM, SIGN(1), OLAT(IOBS), DM
  500    FORMAT (A1, F3.0, F6.2, 1X, A1, F2.0, F6.2)
         OLONG(IOBS) = OLONG(IOBS)+AM/60.D0
         OLAT(IOBS) = OLAT(IOBS)+DM/60.D0
         IF (S .EQ. '-') OLONG(IOBS) = -OLONG(IOBS)
         IF (SIGN(1) .EQ. '-') OLAT(IOBS) = -OLAT(IOBS)
      ELSE IF (IOBS .NE. 0) THEN
         WRITE (6,1) OLONG(IOBS), OLAT(IOBS)
    1    FORMAT (/' Longitude =', F9.3, '   Latitude =', F8.3/)
      END IF
      PHI = SNGL(OLAT(IOBS)/57.29577951D0)
 2001 CALL GETDAT ('Readout time (sec):', RDOUT, 1)
      IF (RDOUT .LT. 0.) THEN
         CALL TBLANK
         GO TO 1000
      END IF
 2003 CALL GETDAT ('Shutter-timing error (sec):', ERROR, 1)
      IF (ERROR .LT. -1.E38) THEN
         CALL TBLANK
         GO TO 2001
      END IF
      IMAGE = .FALSE.
      RAKEY = ' '
 2004 CALL GETCHR ('RA keyword:', RAKEY, N)
      IF (N .LE. -1) THEN
         CALL TBLANK
         GO TO 2003
      END IF
      RAKEY = CAPS(RAKEY)
C
      IF (RAKEY .EQ. 'RA_TARG') THEN
         DECKEY = 'DEC_TARG'
         FILKEY = 'FILTNAM1'
         DATKEY = 'DATE-OBS'
         UTKEY = 'TIME-OBS'
         EXPKEY = 'EXPTIME'
         DAY = .TRUE.
         IMAGE = .TRUE.
         FILTER(1) = 'F606W'
         FILTER(2) = 'F435W'
         FILTER(3) = 'F814W'
         FILTER(4) = 'F625W'
         FILTER(5) = 'F336W'
         GO TO 2010
      END IF
C
      IF (RAKEY .NE. BLANK) THEN
 2005    CALL GETCHR ('Dec keyword:', DECKEY, N)
         IF (N .LE. -1) THEN
            CALL TBLANK
            GO TO 2004
         END IF
         DECKEY = CAPS(DECKEY)
C
         IF (DECKEY .NE. 'DEC_TARG') THEN
            CALL GETCHR ('Equinox keyword:', EQUKEY, N)
            IF (N .LE. -1) THEN
               CALL TBLANK
               GO TO 2004
            END IF
            EQUKEY = CAPS(EQUKEY)
         END IF
         IMAGE = .TRUE.
      END IF
 2006 CALL GETCHR ('Date keyword:', DATKEY, N)
      IF (N .LE. -1) THEN
         CALL TBLANK
         GO TO 2004
      END IF
      DATKEY = CAPS(DATKEY)
C
      IF (DATKEY .EQ. BLANK) THEN
         DAY = .FALSE.
      ELSE
         DAY = .TRUE.
         IF (DATKEY .EQ. 'OGLE') THEN
            OGLE = .TRUE.
            DATKEY = 'DATE-OBS'
         END IF
      END IF
 2007 CALL GETCHR ('UT keyword:', UTKEY, N)
      IF (N .LE. -1) THEN
         CALL TBLANK
         GO TO 2006
      END IF
      UTKEY = CAPS(UTKEY)
C
      IF (UTKEY .NE. BLANK) IMAGE = .TRUE.
 2008 CALL GETCHR ('Filter keyword:', FILKEY, N)
      IF (N .LE. -1) THEN
         CALL TBLANK
         GO TO 2007
      END IF
      FILKEY = CAPS(FILKEY)
C
      IF (FILKEY .NE. BLANK) IMAGE = .TRUE.
 2009 CALL GETCHR ('Exposure keyword:', EXPKEY, N)
      IF (N .LE. -1) THEN
         CALL TBLANK
         GO TO 2008
      END IF
      EXPKEY = CAPS(EXPKEY)
C
      IF (EXPKEY .NE. BLANK) IMAGE = .TRUE.
 2010 CALL GETCHR ('Filename prefix:', PREFIX, NPRE)
      IF (NPRE .LT. 0) THEN
         CALL TBLANK
         GO TO 2009
      END IF
 2011 CALNAM = ' '
      CALL GETNAM ('Intended name of calibration file:', CALNAM)
      IF (CALNAM .EQ. 'END-OF-FILE') THEN
         CALL TBLANK
         GO TO 2010
      END IF
 2012 IF (.NOT. DAY) THEN
         CALL GETDAT ('UT date of observation (yyyy mm dd):', DATE, 3)
         IF (DATE(1) .LT. 0) THEN
            CALL TBLANK
            GO TO 2011
         END IF
         NYR = NINT(DATE(1))
         NMO = NINT(DATE(2))
         NDA = NINT(DATE(3))
      END IF
      IF ((UTKEY .EQ. BLANK) .OR. (EXPKEY .EQ. BLANK)) WRITE (6,612) 
  612 FORMAT (/' Enter the universal time and the celestial ',
     .     'coordinates in the format shown.' / 
     .     ' Enter Photometry file or Object name EXIT to exit.'//)
C
 2014 INPUT = OBJECT
      CALL GETNAM ('Object name:', INPUT)
      IF (INPUT .EQ. 'END-OF-FILE') THEN
         CALL TBLANK
         INPUT = ' '
         IF (NX .GT. 0) CALL CLPIC (1, ISTAT)
         GO TO 2011
      END IF
      IF (INPUT .EQ. 'EXIT') GO TO 9100
      OBJECT = INPUT
C
      NAME = ' '
 2013 CALL GETNAM ('Photometry file:', NAME)
      IF ((NAME .EQ. 'END-OF-FILE') .OR.
     .     (NAME .EQ. 'NEW_OBJECT')) THEN
         CALL TBLANK
C        IF (.NOT. DAY) GO TO 2014
         GO TO 2014
      END IF
      IF (NAME .EQ. 'EXIT') GO TO 9100
C
      NAME = SWITCH(NAME, ' ')
      IF (NPRE .GT. 0) NAME = PREFIX(1:NPRE)//NAME
      IF (IMAGE) THEN
         CALL ATTACH (NAME, NX, NY)
         IF (NX .LE. 0) THEN
            CALL STUPID ('Unable to open image '//NAME)
            GO TO 2013
         END IF
      END IF
      IF (DAY) THEN
         CALL SGET (1, DATKEY, VALUE, COMMNT)
         IF (OGLE) THEN
            DO I=2,69
               IF (VALUE(I:I+2) .EQ. 'Jun') THEN
                  NMO = 6
                  VALUE(I:I+2) = '   '
                  GO TO 21
               ELSE IF (VALUE(I:I+2) .EQ. 'Jul') THEN
                  NMO = 7
                  VALUE(I:I+2) = '   '
                  GO TO 21
               ELSE IF (VALUE(I:I+2) .EQ. 'Aug') THEN
                  NMO = 8
                  VALUE(I:I+2) = '   '
                  GO TO 21
               ELSE IF (VALUE(I:I+2) .EQ. 'Sep') THEN
                  NMO = 9
                  VALUE(I:I+2) = '   '
                  GO TO 21
               END IF
            END DO
            CALL GETDAT('UT date of observation (yyyy mm dd):',DATE,3)
            IF (DATE(1) .LT. 0) THEN
               CALL TBLANK
               GO TO 2011
            END IF
            NYR = NINT(DATE(1))
            NMO = NINT(DATE(2))
            NDA = NINT(DATE(3))
            GO TO 2713
 21         READ (VALUE,*) NDA, NYR
            GO TO 2713
         END IF
         IF (DATKEY .EQ. 'ARCFILE') THEN
            I = LENGTH(VALUE)
            VALUE = VALUE(6:I)//'     '
         END IF
         IF (IOBS .EQ. 1) THEN
            I = LENGTH(VALUE)
            IF (VALUE(1:1) .EQ. '"') VALUE = 
     .           VALUE(2:I)//'/'//COMMNT
            I = LENGTH(VALUE)
            IF (VALUE(I:I) .EQ. '"') VALUE(I:I) = ' '
         END IF
C        VALUE = RKEYC('DATA', DATKEY)
         DO I=1,72
            IF (VALUE(I:I) .EQ. '-') THEN
               VALUE(I:I) = ' '
               DO J=I+1,72
                  IF ((ICHAR(VALUE(J:J)) .LT. ICHAR('0')) .OR.
     .               (ICHAR(VALUE(J:J)) .GT. ICHAR('9')))
     .               VALUE(J:J) = ' '
               END DO
               READ (VALUE,*) NYR, NMO, NDA
               IF (NDA .GT. 31) READ (VALUE,*) NDA, NMO, NYR
               GO TO 2713
            END IF
C
            IF ((VALUE(I:I) .EQ. '''') .OR.
     .          (VALUE(I:I) .EQ. '/') .OR.
     .          (VALUE(I:I) .EQ. ':')) VALUE(I:I) = ' '
         END DO
c        READ (VALUE,*) NMO, NDA, NYR
         IF ((IOBS .EQ. 12) .AND. (DATKEY .EQ. 'UTDATE')) THEN
            READ (VALUE,*) NYR, NMO, NDA
         ELSE
            READ (VALUE,*) NDA, NMO, NYR
            IF (NDA .GT. 31) READ (VALUE,*) NYR, NMO, NDA
            if (prefix .eq. 'cmr2:') nmo = nmo+1
CCXYZ
CC
CC This is for the stupid idiots at ESO who never do anything
CC like anybody else!
CC
C         value = rkeyc('DATA', datkey)
C         read (value,666)nyr,nmo,nda
C  666    format (i4, x, i2, x, i2)
CCXYZ
         END IF
 2713    IF (NYR .LT. 1900) THEN
            IF (NYR .GT. 10) THEN
               NYR = NYR+1900
            ELSE
               NYR = NYR+2000
            END IF
         END IF
         WRITE (6,602)  NYR, MONTH(NMO), NDA
  602    FORMAT (46X, 'Date:', I6, 1X, A3, I3, I3.2)
      END IF
C
      IF ((NX .GT. 0) .AND. (RAKEY .NE. BLANK)) THEN
C
         IF (RAKEY .EQ. 'RA_TARG') THEN
            CALL RGET (1, RAKEY, R, COMMNT)
            R = R/15.
            IRAH = INT(R)
            IRAM = INT(60.*(R-IRAH))
            RAS = 3600.*(R-(IRAH+IRAM/60.))
         ELSE IF (RAKEY .EQ. 'RA-DEG') THEN
            CALL RGET (1, RAKEY, R, COMMNT)
            IRAH = INT(R)
            IRAM = INT(60.*(R-IRAH))
            RAS = 3600.*(R-(IRAH+IRAM/60.))
         ELSE
            CALL SGET (1, RAKEY, VALUE, COMMNT)
            IF (VALUE .EQ. 'ERROR!') GO TO 2015
            DECIML = .TRUE.
            DO I=1,72
               IF (VALUE(I:I) .EQ. ':') THEN
                  VALUE(I:I) = ' '
                  DECIML = .FALSE.
               END IF
            END DO
C
            READ (VALUE,*,IOSTAT=ISTAT) IRAH, IRAM, RAS
            IF (ISTAT .NE. 0) THEN
C
C Decimal hours.
C
               READ (VALUE,*,ERR=2015) RAS
C
C RA in decimal degrees?
C
               IF (IOBS .EQ. 11) THEN
C
C Stromlo --- RA in radians; convert to decimal hours
C
                  RAS = 3.819 718 634 * RAS
                  RAMODE = 'HOUR'
               ELSE IF (RAS .GT. 24.) THEN
                  IF (RAMODE .EQ. 'NULL') THEN
      CALL STUPID ('I am guessing that RA is in decimal degrees.')
                     RAMODE = 'DEGR'
                  ELSE IF (RAMODE .EQ. 'HOUR') THEN
                     CALL STUPID ('*** EMERGENCY ***')
      PRINT *, 'Until now I was guessing RA in decimal hours.'
      PRINT *, 'Now it looks more like decimal degrees.'
      PRINT *, 'You''d better redo previous images.'
      PRINT *
                     RAMODE = 'DEGR'
                  END IF
               ELSE
                  IF (RAMODE .EQ. 'NULL') THEN
      CALL STUPID ('I am guessing that RA is in decimal hours.')
                     RAMODE = 'HOUR'
                  END IF
               END IF
C
               IF (RAMODE .EQ. 'DEGR') RAS=RAS/15.
               IRAH = INT(RAS)
               IRAM = INT(60.*(RAS-IRAH))
               RAS = 60.*(60.*(RAS-IRAH)-IRAM)
            END IF
         END IF
C
         IF (RAS .GE. 59.95) THEN
            RAS = RAS-60.
            IRAM = IRAM+1
         END IF
         IF (IRAM .GE. 60) THEN
            IRAM = IRAM-60
            IRAH = IRAH+1
         END IF
         IF (IRAH .GE. 24) IRAH = IRAH-24
C
         IF (DECKEY .EQ. 'DEC_TARG') THEN
            CALL RGET (1, DECKEY, R, COMMNT)
            IF (R .LT. 0.) THEN
               S = '-'
               R = -R
            ELSE
               S = '+'
            END IF
            IDECD = INT(R)
            IDECM = 60.*(R-IDECD)
            DS = 3600.*(R-IDECD-IDECM/60.)
         ELSE IF (DECKEY .EQ. 'DEC-DEG') THEN
            CALL RGET (1, RAKEY, R, COMMNT)
            IF (R .LT. 0.) THEN
               S = '-' 
               R = -R
            ELSE 
               S = '+'
            END IF 
            IDECD = INT(R)
            IDECM = 60.*(R-IDECD)
            DS = 3600.*(R-IDECD-IDECM/60.)
         ELSE
            CALL SGET (1, DECKEY, VALUE, COMMNT)
            S = '+'
            DECIML = .TRUE.
            DO I=1,72
               IF ((VALUE(I:I) .EQ. '-') .AND.
     .              (VALUE(I-1:I-1) .NE. 'E')) THEN
                  S = '-'
                  VALUE(I:I) = ' '
               END IF
               IF (VALUE(I:I) .EQ. ':') THEN
                  VALUE(I:I) = ' '
                  DECIML = .FALSE.
               END IF
            END DO
C
            READ (VALUE,*,IOSTAT=ISTAT) IDECD, IDECM, DS
            IF (ISTAT .NE. 0) THEN
               READ (VALUE,*,ERR=2015) DS
               IF (IOBS .EQ. 11) DS = 57.295 229 51 * DS
               IDECD = INT(DS)
               IDECM = INT(60.*(DS-IDECD))
               DS = 60.*(60.*(DS-IDECD)-IDECM)
            END IF
         END IF
C
         IF (DS .GE. 59.5) THEN
            DS = DS-60.
            IDECM = IDECM+1
         END IF
         IF (IDECM .GE. 60) THEN
            IDECM = IDECM-60
            IDECD = IDECD+1
         END IF
         IF (IDECD .GE. 90) THEN
            IDECD = 179 - IDECD
            IDECM = 59 - IDECM
            DS = 60. - DS
         END IF
C
         IF (EQUKEY .EQ. BLANK) THEN
            EPCH = REAL(NYR) + REAL(NMO-1)/12. + 
     .           (REAL(NDA)-0.5)/365.25
         ELSE IF (DECKEY .EQ. 'DEC_TARG') THEN
            EPCH = 2000.0
         ELSE
            CALL SGET (1, EQUKEY, VALUE, COMMNT)
            READ (VALUE,*,IOSTAT=ISTAT) EPCH
            IF (ISTAT .NE. 0) THEN
               DO J=1,72
                  IF (((ICHAR(VALUE(J:J)) .LT. ICHAR('0')) .OR.
     .               (ICHAR(VALUE(j:J)) .GT. ICHAR('9'))) .AND.
     .               (VALUE(J:J) .NE. '.')) VALUE(J:J) = ' '
               END DO
               READ (VALUE,*,ERR=2717) EPCH
            END IF
            IF (EPCH .LT. 1000.) GO TO 2717
         END IF
         GO TO 2718
 2717    CALL GETDAT ('Equinox of the observation:', EPCH, 1)
      END IF
 2015 DO I=1,NCOO
         IF (OBJECT .EQ. CNAME(I)) THEN
            IRAH = JHR(I)
            IRAM = JTMIN(I)
            RAS = TSEC(I)
            S = SIGN(I)
            IDECD = IDEG(I)
            IDECM = IAMIN(I)
            DS = DSEC(I)
            EPCH = EPOCH(I)
            GO TO 2016
         END IF
      END DO
      CALL STUPID ('Please enter right ascension, '//
     .     'declination, and equinox in format shown.')
 5015 WRITE (6,61)
   61 FORMAT ('hh mm ss.s sdd mm ss yyyy')
      CALL RDCHAR (5, LINE, N, ISTAT)
      READ (LINE,5,END=2014,ERR=5016) IRAH, IRAM, RAS, S,
     .     IDECD, IDECM, DS, EPCH
    5 FORMAT (I2, I3, F5.1, 1X, A1, I2, I3, F3.0, F5.0)
      GO TO 5019
C
 5016 print*,'Decimal hours'
      READ (LINE,*,ERR=5015) RAS, DS, EPCH
      IF (DS .LT. 0) THEN
         S = '-'
         DS = -DS
      END IF
      IRAH = INT(RAS)
      RAS = 60.*(RAS-REAL(IRAH))
      IRAM = INT(RAM)
      RAS = 60.*(RAS-REAL(IRAM))
      IDECD = INT(DS)
      DS = 60.*(DS-REAL(IDECD))
      IDECM = INT(DS)
      DS = 60.*(DS-REAL(IDECM))
C
 5019 IF (S .NE. '-') S = '+'
C
 2718 DO I=1,NCOO
         IF (OBJECT .EQ. CNAME(I)) GO TO 2016
      END DO
      NCOO = NCOO+1
      CNAME(NCOO) = OBJECT
      CALL TBLANK
      IF (NINT(EPCH) .NE. 2000) THEN
         WRITE (6,62) IRAH, IRAM, RAS, 
     .          S, IDECD, IDECM, NINT(DS), NINT(EPCH)
         CALL PRCSS (IRAH, IRAM, RAS, S, IDECD, IDECM, DS,
     .        EPCH, 2000.0)
      END IF
      IF (S .NE. '-') S = '+'
      JHR(NCOO) = IRAH
      JTMIN(NCOO) = IRAM
      TSEC(NCOO) = RAS
      SIGN(NCOO) = S
      IDEG(NCOO) = IDECD
      IAMIN(NCOO) = IDECM
      DSEC(NCOO) = DS
      EPOCH(NCOO) = 2000.0
      WRITE (POSITN,101) JHR(NCOO), JTMIN(NCOO), TSEC(NCOO),
     .     SIGN(NCOO), IDEG(NCOO), IAMIN(NCOO), DSEC(NCOO),
     .     EPOCH(NCOO)
  101 FORMAT (2I3.2, F6.2, 1X, A1, I2.2, I3.2, F5.1, F7.1)
      IF (POSITN(8:8) .EQ. ' ') POSITN(8:8) = '0'
      IF (POSITN(21:21) .EQ. ' ') POSITN(21:21) = '0'
      WRITE (1,102) CNAME(NCOO), POSITN
  102 FORMAT (A35, 2X, A31)
C
 2016 CONTINUE
      WRITE (6,62) IRAH, IRAM, RAS, 
     .     S, IDECD, IDECM, NINT(DS), 2000
   62 FORMAT (38X, 'Coordinates =', 2I3.2, F5.1, 
     .     1X, A1, I2.2, 2I3.2, '  (', I4, ')')
      CALL TBLANK
      DO I=1,30
         IF (OBJECT(I:I) .NE. ' ') NOBJ = I
      END DO
      IF ((NX .GT. 0) .AND. (FILKEY .NE. BLANK)) THEN
  216    CALL SGET (1, FILKEY, VALUE, COMMNT)
         IF (VALUE .EQ. 'ERROR!') THEN
            IF (FILKEY .EQ. 'FILTNAM1') THEN
               FILKEY = 'FILTER1' 
               GO TO 216
            ELSE IF (FILKEY .EQ. 'FILTER1') THEN
               FILKEY = 'FILTER2'
               GO TO 216
            END IF
         END IF
 2017    IF (VALUE(1:1) .EQ. ' ') THEN
            VALUE = VALUE(2:LEN(VALUE))//' '
            GO TO 2017
         END IF
C
         IF (VALUE(1:5) .EQ. 'CLEAR') THEN
            IF (FILKEY .EQ. 'FILTER1') THEN
               FILKEY = 'FILTER2'
            ELSE IF (FILKEY .EQ. 'FILTER2') THEN
               FILKEY = 'FILTER1'
            END IF
            GO TO 216
         END IF
C
         DO I=0,MAXFLT
            IF (VALUE .EQ. FILTER(I)) THEN
               IFILT = I
C  67          FORMAT (44X, 'Filter: ', I1)
               GO TO 3019
            END IF
         END DO
         CALL STUPID (' '//FILKEY(1:LENGTH(FILKEY))//' = '//VALUE)
         CALL GETDAT ('Enter filter number:', COSZ, 1)
         IF (COSZ .LE. 0.) GO TO 2014
         IFILT = NINT(COSZ)
         FILTER(IFILT) = VALUE
         GO TO 3019
      END IF
 2018 CALL GETDAT ('Filter:', COSZ, 1)
      IF (COSZ .LE. 0) THEN
         NAME = ' '
         CALL TBLANK
         IF (NX .GT. 0) CALL CLPIC(1, ISTAT)
         GO TO 2013
      END IF
      IFILT = NINT(COSZ)
 3019 CONTINUE
C
C This is for the stupid idiots at ESO who didn't write the
C time of exposure start in the damn FITS header.
C
C We have to back-calculate the UT from the LST.
C
      IF (UTKEY .EQ. 'EXPOLST') THEN
         call sget (1, utkey, value, commnt)
         read (value,*) sec
         time = dble(sec)/3.6d3
         ihr = 0
         itmin = 0
         sec = 0.
C
C Calculate the Local Sidereal Time at 00:00:00 hrs UT on this
C date ==> xlst.
C
      print*,olong(iobs),olat(iobs)
         dec = rtosun(nyr, nmo, nda, ihr, itmin, sec,
     .        irah, iram, ras, s, idecd, idecm, ds,
     .        epch, olong(iobs), olat(iobs), xlst, ha, gjd,
     .        delt)
         xlst = 2.4d1*xlst
C
C VALUE is LST in seconds, TIME and XLST are LST in hours
C
         time = time - xlst
         if (time .lt. 0.d0) time = time + 24.d0
C
C TIME is now the amount of sidereal time which has elapsed
C since 00:00:00 hrs UT on this date.
C
         time = 0.997 269 566d0 * time
C
C TIME is now the amount of UT time which has elapsed
C since 00:00:00 hrs UT on this date.
C
         ihr = int(time)
         time = 6.d1*(time-dble(ihr))
         itmin = int(time)
         sec = 60.*sngl(time-dble(itmin))
         go to 2024
      END IF
C
C End of stupid ESO jerks.
C
C Get date and time of observation.
C
      IF ((NX .GT. 0) .AND. (UTKEY .NE. BLANK)) THEN
         CALL SGET (1, UTKEY, VALUE, COMMNT)
         IF ((UTKEY .EQ. 'DATE-OBS') .OR. (UTKEY .EQ. 'DATE_OBS') .OR.
     .        (UTKEY .EQ. 'ARCFILE')) THEN
            J = LENGTH(VALUE)
            DO I=5,J
               IF (VALUE(I:I) .EQ. 'T') THEN
                  VALUE = VALUE(I+1:J)
                  GO TO 3017
               END IF
            END DO
         END IF
 3017    DO I=71,1,-1
            IF (VALUE(I:I) .NE. ' ') THEN
C
C If the LAST non-blank character on the line is a colon, append a zero.
C
               IF (VALUE(I:I) .EQ. ':') THEN
                  N = I+1
                  VALUE(N:N) = '0'
                  GO TO 3018
               ELSE
                  N = I
                  GO TO 3018
               END IF
            END IF
         END DO
         GO TO 2020
C
 3018    CONTINUE
C
C Change colons to blanks.  On the other hand, if a period is encountered
C BEFORE a colon or a blank, treat the time as decimal hours.
C
         DECIML = .TRUE.
         DO I=1,N
            IF (DECIML) THEN
               IF (VALUE(I:I) .EQ. '.') THEN
                  IF ((IOBS .EQ. 8) .OR. (IOBS .EQ. 9)) THEN
                     IF ((UTKEY .EQ. 'TM_START') .OR.
     .                    (UTKEY .EQ. 'TM-START')) THEN
C
C In this case, UT is decimal seconds.
C
                        READ (VALUE,*,ERR=2020) SEC
                        SEC = SEC/3600.
                     ELSE
                        READ (VALUE,*,ERR=2020) SEC
                     END IF
                  ELSE
                     READ (VALUE,*,ERR=2020) SEC
                  END IF
                  IHR = INT(SEC)
                  SEC = 60.*(SEC - IHR)
                  ITMIN = INT(SEC)
                  SEC = 60.*(SEC - ITMIN)
                  WRITE (6,68) IHR, ITMIN, SEC
                  GO TO 2024
               ELSE 
                  IF ((VALUE(I:I) .EQ. ' ') .OR. 
     .                 (VALUE(I:I) .EQ. ':')) DECIML = .FALSE.
               END IF
            END IF
            IF (VALUE(I:I) .EQ. ':') VALUE(I:I) = ' '
         END DO
         IF (DECIML) THEN
            IF ((IOBS .EQ. 8) .OR. (IOBS .EQ. 9)) THEN
               IF ((UTKEY .EQ. 'TM_START') .OR.
     .              (UTKEY .EQ. 'TM-START')) THEN
C
C In this case, UT is decimal seconds.
C
                  READ (VALUE,*,ERR=2020) SEC
      print*,'Sec =', sec
                  SEC = SEC/3600.
            sec = sec - 4.
               END IF
            ELSE
C
C In this case, UT is decimal hours.
C
               READ (VALUE,*,ERR=2020) SEC
            END IF
            IHR = INT(SEC)
            SEC = 60.*(SEC - IHR)
            ITMIN = INT(SEC)
            SEC = 60.*(SEC - ITMIN)
         ELSE
C
C Special code for ODI.
C
            READ (VALUE,*,ERR=2020) IHR, ITMIN, SEC
            IF (UTKEY .EQ. 'TIME-MID') THEN
               CALL RGET (1, EXPKEY, T, COMMNT)
               T = 60.*(60.*IHR + ITMIN) + SEC - T/2.
               IHR = INT(T/3600.)
               T = (T - 3600.*IHR)
               ITMIN = INT(T/60.)
               SEC = T - 60.*ITMIN
            END IF
         END IF
CXYZ
C
C This is for the stupid idiots at ESO who never do anything
C like anybody else!
C
c        value = rkeyc('DATA', datkey)
c        read (value,667)ihr,itmin,sec
c 667    format (11x, i2, x, i2, x, f6.3)
c
         WRITE (6,68) IHR, ITMIN, SEC
   68    FORMAT (40X, 'UT (start): ', 2I3, F5.1)
         GO TO 2024
      END IF
 2020 CALL GETCHR ('UT (hh mm ss.s):', VALUE, I)
      DO I=1,LEN(VALUE)
         IF (VALUE(I:I) .EQ. ':') VALUE(I:I) = ' '
      END DO
      READ (VALUE,*,ERR=2020) IHR, ITMIN, SEC
C
 2024 IF (EXPKEY .NE. BLANK) THEN
         CALL SGET (1, EXPKEY, VALUE, COMMNT)
         DO I=1,72
            IF (VALUE(I:I) .NE. ' ') N=I
         END DO
         GO TO 3020
      END IF
C
 2019 CALL GETCHR ('Exposure:', VALUE, I)
      DO I=1,72
         IF (VALUE(I:I) .NE. ' ') N=I
      END DO
 3020 NEXP = 1
      J = 1
      IF (N .GT. 1) THEN
         DO I=2,N
            IF (VALUE(I:I) .EQ. 'x') THEN
               READ (VALUE(J:I-1),*,ERR=2019) NEXP
               J = I+1
            END IF
         END DO
      END IF
      L = J
      T = 0.
      DO I=L,N
         IF (VALUE(I:I) .EQ. ':') THEN
            READ (VALUE(J:I-1),*,ERR=2019) R
            J=I+1
            T = 60.*(T+R)
         END IF
      END DO
      READ (VALUE(J:N),*,ERR=3021) R
      T = T+R
 3021 DT = NEXP*T + (NEXP-1)*RDOUT
C
C DT is the total elapsed time from first shutter open to
C last shutter close.
C
      SEC = SEC+DT/2.
CC
CC Bulgaria
CC
C       ihr = ihr - 3
C       if (ihr .lt. 0) then
C          ihr = ihr+24
C          nda = nda - 1
C       end if
      DEC = RTOSUN(NYR, NMO, NDA, IHR, ITMIN, SEC,
     .     IRAH, IRAM, RAS, S, IDECD, IDECM, DS,
     .     EPCH, OLONG(IOBS), OLAT(IOBS), XLST, HA, GJD,
     .     DELT)
      E = DT/13788.6
      IF (HA .LT. -3.141592654D0) HA = HA+6.283185307D0
      IF (HA .GT. 3.141592654D0) HA = HA-6.283185307D0
C
C Angle, in radians, through which sky has rotated during
C exposure.
C
      DEC=0.01745329*(ABS(IDECD)+(IDECM+DS/60.)/60.)
      IF (S .EQ. '-') DEC=-DEC
      XLST = 24.D0*XLST
      LSTHR = INT(XLST)
      LSTMN = INT(60.*(XLST-LSTHR))
      LSTSC = NINT(60.*(60.*(XLST-LSTHR)-LSTMN))
      IF (LSTSC .GE. 60) THEN
         LSTSC = LSTSC-60
         LSTMN = LSTMN+1
      END IF
      IF (LSTMN .GE. 60) THEN
         LSTMN = LSTMN-60
         LSTHR = LSTHR+1
      END IF
      IF (LSTHR .GT. 24) LSTHR = LSTHR-24
      HANGLE = SNGL(HA)
      IF (IOBS .EQ. 0) THEN
         COSZ = 0.
      ELSE
         SDEC = SIN(DEC)
         CDEC = COS(DEC)
         SPHI = SIN(PHI)
         CPHI = COS(PHI)
         COSZ=(  SPHI*SDEC+CPHI*CDEC*COS(HANGLE-E/2.) +
     .       4.*(SPHI*SDEC+CPHI*CDEC*COS(HANGLE)) +
     .           SPHI*SDEC+CPHI*CDEC*COS(HANGLE+E/2.)) / 6.
C
         IF (COSZ .LE. 0.04796648) THEN
            WRITE (6,615) IHR, ITMIN, T
  615       FORMAT (/I2, I3, '  below horizon.'/)
C           GO TO 2013
         END IF
         X = (SDEC-SPHI*COSZ)
         Y = X/(CPHI*SIN(ACOS(COSZ)))
         Y = AMAX1(-1.,AMIN1(1.,Y))
         AZMTH = 57.2958*acos(Y)
         IF (HA .GT. 0.) AZMTH = 360.-AZMTH
C
C Harris's formula
C
         COSZ=(1.-0.0012*TAN(ACOS(COSZ))**2)/COSZ
         IF (COSZ .LE. 0.04796648) THEN
            WRITE (6,615) IHR, ITMIN, T
C           GO TO 2013
         END IF
C
C Crawford's formula.
C
C        SECZ=1./COSZ-1.
C        COSZ=SECZ+1.-SECZ*(SECZ*(SECZ*0.0008083+0.002875)+0.00186)
C
      END IF
      I = NINT(SEC)
 2022 CONTINUE
      IF (I .GE. 60) THEN
         I = I-60
         SEC = SEC-60.
         ITMIN = ITMIN+1
         GO TO 2022
      END IF
 2023 CONTINUE
      IF (ITMIN .GE. 60) THEN
         ITMIN = ITMIN - 60
         IHR = IHR + 1
         GO TO 2023
      END IF
      WRITE (6,616)
  616 FORMAT (/' UT (mid)     HJD (mid)    LST (mid)    HA',
     .     '       X      Az    Fil     Exp')
      IF (HANGLE .LT. 0) THEN
         S = 'E'
         HANGLE = -HANGLE
      ELSE
         S = 'W'
      END IF
      HANGLE = 3.819719*HANGLE
      J = INT(HANGLE)
      K = NINT(60.*(HANGLE-J))
      IF (K .GE. 60) THEN
         K = K-60
         J = J+1
      END IF
      WRITE (6,617) IHR, ITMIN, I, GJD+DELT+T/172800., LSTHR, LSTMN, 
     .     LSTSC, J, K, S, COSZ, NINT(AZMTH), IFILT, T
  617 FORMAT (3I3.2, F15.4, 2X, 3I3.2, I4, I3.2, 1X, A1, F8.3, I6.3, 
     .     I6, F10.1/)
      IF (SEC .GE. 30.) ITMIN = ITMIN+1
      DO I=0,4
         X = 10.**I
         Y = ANINT(X*(T+ERROR))
         IF (Y .GE. 9999.5) GO TO 7000
      END DO
 7000 LINE = ' '
      LINE(56:63) = RNDOFF (Y/X, 8, 5)
      WRITE (LINE(1:55),301) SWITCH(NAME, ' '), IFILT, IHR, ITMIN, 
     .     COSZ, AZMTH
  301 FORMAT (1X, A30, I3, I4.2, I3.2, F7.3, F7.1)
      IF (CALNAM(1:6) .EQ. 'PHOTOG') THEN
         WRITE (LINE(64:139),302) GJD+DELT+T/172800., 
     .           SWITCH(NAME, '.ptg'), OBJECT
      ELSE
         WRITE (LINE(64:139),302) GJD+DELT+T/172800., CALNAM, OBJECT
      END IF
  302 FORMAT (F14.4, 1X, A30, 1X, A30)
      WRITE (3,310) LINE(1:LENGTH(LINE))
  310 FORMAT (A)
      NAME = 'NEW_OBJECT'
      CALL TBLANK
      IF (NX .GT. 0) CALL CLPIC (1, ISTAT)
      GO TO 2013
C
 9100 CALL CLFILE (1)
      CALL CLFILE (3)
      CALL BYEBYE
      END!
c
c#####################################################################
c
      real function  rtosun  (nyr, nmo, nda, nhr, nmn, sec, irah, 
     .     iram, ras, sign, idecd, idecm, decs, epoch, olong, olat, 
     .     xlst, ha, gjd, delt)
      character*1 sign
      double precision rap, decp, du, utda, olong, olat, ha,
     .     delt, v1, gjd, xlst
      integer ndays(12)
      data ndays / 0, 31, 59, 90, 120, 151, 181, 212, 243, 273,
     .     304, 334 /
      utda = ndays(nmo) + nda + (nhr + (nmn + sec/60.)/60.)/24.
      if ((mod(nyr,4) .eq. 0) .and. (nmo .ge. 3)) utda = utda + 1.
c
c utda = days from start of observation year to
c      observation time
c
      du = 365 * (nyr-1900) + (nyr - 1901) / 4 - 0.5
c
c  du = DAYS SINCE 1900 jan 0.5  TO START OF current YEAR
c
      yrto = real(nyr) + utda / 365.25
c
c precess coord to observation date
c
      rap = (irah + (iram + ras/60.d0)/60.d0) / 3.819718634d0
      decp = (idecd + (idecm + decs/60.d0)/60.d0) / 57.29577951d0
c 
c rap and decp are coords on observation date in radians
c olong is observatory longitude in degrees
c  olat  "      "      latitude   "    "
c
c ha = returned hour angle in days
c delt = heliocentric JD correction in days
c v1 = reduction to sun
c
      call dop(rap, decp, du, utda, olong, olat, 
     .     xlst, ha, delt, v1)
      print*,xlst
c
c convert hour angle to radians
c
      ha = ha*6.283185307D0
      rtosun = sngl(v1)
      gjd = 2415020.d0 + du + utda 
c + delt
      return
      end!
c
c*******************************************************************
c
      subroutine dop(ra, dec, du, utda, olong, olat, 
     .     xlst, ha, delt, v1)
      implicit double precision (o-z, a-h)
      pi = 3.1415926535d0
      tpi = 2.d0 * pi
      hpi = pi / 2.d0
      rad = 180. / pi
      wlong = olong / 360.0
      colat = cos(olat / rad)
      ra1 = ra / tpi
      tu = du / 36525.0
      smd = du + utda
      t = smd / 36525.0
c gmst on jan 0.0 in days         
      start = ((6.d0 + (38.d0 / 60.d0)) + ((((45.836d0 + 129.1794d0) + (
     &8640184.54d0 * (tu - .7d0))) + (.0929d0 * (tu ** 2))) / 3600.d0))
     & / 24.d0
c gmst at obs time                   
      gst = start + (utda / 0.997269566d0)
c local mst obs time in days                     
      xlst = gst - wlong
c fractional part of above                  
      xlst = xlst - dint(xlst)
c calc diurnal velocity correction
c hour angle in days                                
      ha = xlst - ra1
c now calc correction for earth's orbital motion
c LATITUDE USED HERE   
    6 vrot = - (((0.463d0 * colat) * dcos(dec)) * dsin(tpi * ha))
      am = (((358.47583d0 + (0.985600267d0 * smd)) - (1.5d-4 * (t ** 2))
     &) - (3.d-6 * (t ** 3))) / rad
      pie = (((101.22083d0 + (4.70684d-5 * smd)) + (4.53d-4 * (t ** 2)))
     & + (3.d-6 * (t ** 3))) / rad
      e = (0.01675104d0 - (4.18d-5 * t)) - (1.26d-7 * (t ** 2))
      ai = (((23.452294d0 - (.0130125d0 * t)) - (1.64d-6 * (t ** 2))) + 
     &(5.03d-7 * (t ** 3))) / rad
      vs = ((am + (((2.d0 * e) - (.25d0 * (e ** 3))) * dsin(am))) + ((
     &1.25d0 * (e ** 2)) * dsin(2.d0 * am))) + (((13.d0 / 12.d0) * (e
     & ** 3)) * dsin(3.d0 * am))
c long of earth as seen from sun                     
      xlam = pie + vs
c get long and lat of star( along, beta in radians)
c long of sun from earth                            
      alam = xlam + pi
      call cord(0.d0, 0.d0, - hpi, hpi - ai, ra, dec, along, beta)
c correction in days
c helioc time      
      delt = - ((0.00577559d0 * dcos(alam - along)) * dcos(beta))
      bbb = (29.78486d0 * dcos(beta)) / dsqrt(1.d0 - (e ** 2))
      ccc = (bbb * e) * dsin(along - pie)
c now for moon
c correction for earth motion         
      ve = (bbb * dsin(alam - along)) + ccc
      omga = ((259.183275d0 - (.0529539222d0 * smd)) + (2.078d-3 * (t
     & ** 2))) + (2.d-6 * (t ** 3))
      omgar = omga / rad
      amon = ((270.434164d0 + (13.176396527d0 * smd)) - (1.133d-3 * (t
     & ** 2))) + (1.9d-6 * (t ** 3))
      gamp = ((334.329556d0 + (.1114040803d0 * smd)) - (1.0325d-2 * (t
     & ** 2))) - (1.2d-5 * (t ** 3))
      pim = (gamp - omga) / rad
      em = 0.054900489d0
      olamm = (amon - omga) / rad
      aim = 5.1453964d0 / rad
      amm = olamm - pim
      vsm = ((amm + (((2.d0 * em) - (.25d0 * (em ** 3))) * dsin(amm)))
     & + ((1.25d0 * (em ** 2)) * dsin(2.d0 * amm))) + (((13.d0 / 12.d0)
     & * (em ** 3)) * dsin(3.d0 * amm))
      alamm = pim + vsm
c vmon is correction for moon
      call cord(omgar, 0.d0, omgar - hpi, hpi - aim, along, beta, algm, 
     &betam)
      vmon = (1.2604d-2 * dcos(betam)) * (dsin(alamm - algm) - (em * 
     &dsin(pim - algm)))
      v1 = (ve + vmon) + vrot
      return 
      end
c****************************************************************
 	SUBROUTINE PRCSS(IHOUR,IMIN,SEC,SIGN,IDEG,IARCMN,ARCSEC,T1,T2)
C
C Coordinates are sent to the routine in the variables
C
C IHOUR   I*4   R.A. hours
C IMIN    I*4   R.A. minutes
C SEC     R*4   R.A. seconds
C SIGN    C*1   Dec. sign
C IDEG    I*4   Dec. degrees
C IARCMN  I*4   Dec. arcmin
C ARCSEC  R*4   Dec. arcsec
C T1      R*4   Epoch to precess FROM
C T2      r*4   Epoch to precess TO
C
C Results are returned in the same variables.
C
C Peter B. Stetson
C
	DOUBLE PRECISION CJ,SJ,Z,DZ,CA,SA,CD,SD
	DOUBLE PRECISION DCOS,DSIN,DASIN,DATAN,DABS
	CHARACTER*1 SIGN
 	DT1=T1-1850.
 	DT=T2-T1
 	CJ=-(2.0677D-10+1.79D-15*DT1+2.0265D-13*DT)*DT
 	CJ=(CJ-(4.135D-10+1.79D-15*DT1)*DT1+9.7210573D-5)*DT
 	SJ=DSIN(CJ)
 	CJ=DCOS(CJ)
 	Z=(1.46608D-10-1.309D-15*DT1+8.72422D-14*DT)*DT
 	Z=(Z+(2.909D-16*DT1+6.773817D-10)*DT1+1.11679474D-4)*DT
 	DZ=(3.8417D-10+3.2D-15*DT1+1.6D-15*DT)*DT*DT+Z
 	CA=(IHOUR+(IMIN+SEC/6.0D1)/6.0D1)*2.61799387799D-1+Z
 	SA=DSIN(CA)
 	CA=DCOS(CA)
 	CD=(IDEG+(IARCMN+ARCSEC/6.0D1)/6.0D1)*1.74532925199D-2
        IF(SIGN.EQ.'-')CD=-CD
 	SD=DSIN(CD)
 	CD=DCOS(CD)
 	Z=CJ*CD*CA-SJ*SD
 	SA=DATAN(CD*SA/Z)
 	IF(Z.LE.0.0D0)SA=SA+3.14159265359D0
 	SA=(SA+DZ)*3.819718634206D0
 	IF(SA.GE.2.4D1)SA=SA-2.4D1
 	IF(SA.LT.0.0D0)SA=SA+2.4D1
 	IHOUR=IDINT(SA)
 	SA=(SA-IHOUR)*6.0D1
 	IMIN=IDINT(SA)
 	SEC=(SA-IMIN)*6.0D1
 	SD=DASIN(CJ*SD+SJ*CD*CA)
 	SIGN=' '
 	IF(SD.LT.0.0D0)SIGN='-'
 	CD=DABS(SD)*5.72957795131D1
 	IDEG=IDINT(CD)
 	CD=(CD-IDEG)*6.0D1
 	IARCMN=IDINT(CD)
 	ARCSEC=(CD-IARCMN)*6.0D1
 	RETURN
 	END!
c
c****************************************************************
c
      subroutine cord(ao, bo, ap, bp, a1, b1, a2, b2)
      implicit double precision (o-z, a-h)
      sbo = dsin(bo)
      cbo = dcos(bo)
      sbp = dsin(bp)
      cbp = dcos(bp)
      sb1 = dsin(b1)
      cb1 = dcos(b1)
      sb2 = (sbp * sb1) + ((cbp * cb1) * dcos(ap - a1))
      b2 = asin(sb2)
      cb2 = dcos(b2)
      saa = (dsin(ap - a1) * cb1) / cb2
      caa = (sb1 - (sb2 * sbp)) / (cb2 * cbp)
      cbb = sbo / cbp
      sbb = dsin(ap - ao) * cbo
      ta202 = ((1.0d0 - (caa * cbb)) - (saa * sbb)) / ((saa * cbb) - (
     &caa * sbb))
      a2 = 2.d0 * datan(ta202)
      return 
      end
c****************************************************************
