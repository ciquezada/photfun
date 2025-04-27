C
C=======================================================================
C
C A program to transform instrumental CCD photometry from a number of
C frames to the standard system, and produce average indices.
C 
C This program is untidy and complicated, and is certainly not in its
C final form.
C
C             Official DAO version:  2004 February 25
C
C=======================================================================
C
      PARAMETER (MAXMAG=6, MAXNDX=6, MAXFOU=5, MAXTRM=10, NEXTRA=6)
c     PARAMETER (MAXSTR=800 000 00, MXFILE=2 000, MAXSTD=50 00)
      PARAMETER (MAXSTR=800 000 000, MXFILE=12 000, MAXSTD=50 000)
      PARAMETER (MAXOBS=3 000, MAXLST=3 000 000, MAXPER=10)
C
C MAXMAG is the maximum number of instrumental CCD bandpasses that can
C        handled.
C MAXNDX is the maximum number of photometric indices on the standard
C        photometric system that can be handled.
C MAXTRM is the maximum number of coefficients and terms that can be
C        included in each transformation equation.
C NEXTRA is the number of known observational quantities that can be
C        included in the transformation equations, besides the standard
C        photometric indices.  NEXTRA = 6: airmass (Q), time of 
C        observation (T), X coordinate, Y coordinate, Q*cos(azimuth) (R),
C        and Q*sin(azimuth) (S).
C MAXSTR is the maximum number of stars in all the images combined
C MAXOBS is the largest number of different observation files that can
C        be handled for a given filter.
C MXFILE is the largest number of different observation files that can
C        be included in a single reduction MXFILE <= MAXMAG*MAXOBS
C MAXSTD is the largest number of local standards that can be used.
C MAXLST is the largest number of stars in the star list.
C
      CHARACTER FILE(MXFILE)*40, CALNAM(MXFILE)*40, CFILE*40, TFILE*40, 
     .     PFILE*40, INFNAM(MXFILE)*40, SWITCH*40, EXTEND*40, LEFT*40,
     .     CHCKD(MXFILE)*40
      CHARACTER HEADER(2)*80, HEAD(2)*34, FNAME(MAXSTD)*16
      CHARACTER MASTER(MAXSTD)*12, FETID*12
      CHARACTER TEXT(21)*9, TVALID(MAXMAG)*5, SQUEEZ*9,
     .     RNDOFF*9, COMMAS*12
      CHARACTER*6 MAGLBL(MAXNDX)
      CHARACTER LINE*182, CASE*4, ANSWER*1, BELL*1, DSIGN*1,
     .     EXPAND*200, TEST*13, RA*13, DEC*12, SGN*1
      DOUBLE PRECISION DTAN, RHO
      DOUBLE PRECISION HJD(MXFILE), JDIN(MXFILE), H(MAXOBS,MAXMAG)
      REAL CFF(20)
      REAL XFET(MAXSTD), YFET(MAXSTD), ZERO(MXFILE), SZ(MXFILE)
      REAL INFT(MXFILE), INFQ(MXFILE), INFR(MXFILE), INFS(MXFILE),
     .     INFTIN(MXFILE)
C
C DAT, SIG and W should be dimensioned the larger of MAXOBS and MAXSTD
C
      REAL DAT(MAXSTD), SIG(MAXSTD), W(MAXLST,MAXMAG), RMIN(MAXSTD),
     .     WW(MAXOBS), CHICH(MAXOBS), SHARP(MAXOBS)
      REAL OBS(MAXOBS,MAXMAG), OBSE(MAXOBS,MAXMAG), FLUX(MAXMAG)
      REAL MAG(MAXMAG), MAGE(MAXMAG), NDX(MAXNDX)
      REAL AM(MXFILE), TIME(MXFILE), R(MXFILE), S(MXFILE), TIN(MXFILE)
      REAL STMAG(MAXMAG,MAXSTD), STERR(MAXMAG,MAXSTD), 
     .     EXTRA(MAXMAG), COSMIC(MXFILE)
      REAL COEFF(MAXTRM,MAXMAG,MXFILE)
      REAL X(MAXSTR), Y(MAXSTR), Z(MAXSTR), RAW(MAXSTR), RAWE(MAXSTR),
     .     CHI(MAXSTR), SHP(MAXSTR)
      REAL CLAMP(MAXMAG), OLD(MAXMAG), CORRECT(MAXMAG), RME1(MAXMAG)
      REAL OO(MAXOBS,MAXMAG), SS(MAXOBS,MAXMAG), PP(MXFILE)
      real obschi(maxobs,maxmag), obsshp(maxobs,maxmag)
      REAL OBSX(MAXOBS,MAXMAG), OBSY(MAXOBS,MAXMAG), OBSZ(MAXOBS,MAXMAG)
      REAL VLIMIT(3), OFFSET(3)
      REAL G(7), COLD(7), WARM(7)
C     REAL ALP(5), BET(4), GAM(5), DEL(4), ZET(5), ETA(4)
      INTEGER INFM(MXFILE), INFSET(MXFILE), NWHICH(MXFILE)
      INTEGER NOBS(MAXMAG), NEPOCH(MAXMAG), NVALID(MAXMAG), NRED(MAXMAG)
      INTEGER IFILE(MAXOBS,MAXMAG), IDF(MAXSTD), NINDX(MXFILE)
      INTEGER ITERM(MAXTRM,MAXNDX+NEXTRA,MAXMAG,MXFILE), IMAG(MXFILE)
      INTEGER MAGNDX(MAXMAG,MAXNDX,MXFILE) ! , NDXMAG(MAXNDX,MAXMAG)
      INTEGER IWHICH(MXFILE,MAXSTD), IDMAST(MAXSTD), NSTAR(MXFILE)
      INTEGER JWHICH(MXFILE), BEFORE(MXFILE)
      INTEGER NTERM(MAXMAG,MXFILE), ISET(MXFILE)
      INTEGER IPAIR(2,MXFILE), IARRNG(4)
      LOGICAL USE(MAXTRM,MAXMAG,MXFILE), VALID(MXFILE)
      LOGICAL DEFNDX(MAXNDX), PHOTOG(MXFILE)
      LOGICAL OBSPG(MAXOBS,MAXMAG)
      LOGICAL GETZERO, REDO, HST, RADEC, SCALE, SHOW, VAR
C
      DOUBLE PRECISION HJD0, RA0, DC0, CD0, SD0, DTWOPI, XI, ETA
      DOUBLE PRECISION DATE0, DDATE, OMEGA
      REAL KUR, LIMIT, TWOPI
      INTEGER ENOUGH
      DATA DEFNDX/MAXNDX*.FALSE./, DTWOPI/6.2831 85307 17959 D0/
      DATA SCALE /.FALSE./, RADEC / .FALSE. /, SHOW / .FALSE. /
      DATA CORRECT / MAXMAG*1. / , HST /.false./, OBSLIM / -99. /
      DATA DAMP /1./
      DATA WWW/3.0/     !Stopgap
      DATA IARRNG /1, 0, 3, 2/, ENOUGH /1/
      CALL FABORT
      BELL=CHAR(7)
      TWOPI = SNGL(DTWOPI)
C
C=======================================================================
C
C SECTION 1(A)
C
C Obtain the name of the file with the transfer table, and open it.
C
      IF (HST) THEN
         CALL STUPID ('Set up for HST reductions!')
         CFILE = 'unix:coldwarm.dat'
         CALL INFILE (1, CFILE, ISTAT)
         read (1,*) (cold(i), i=1,6)
         read (1,*) (warm(i), i=1,6)
         CALL CLFILE (1)
      END IF
      IF (OBSLIM .GT. -90.) THEN
         CALL STUPID ('Bright magnitude limit.')
         WRITE (6,*) OBSLIM
      END IF
      CALL TBLANK
      TFILE=' '
  850 CALL GETNAM ('Transfer file:', TFILE)
      IF ((TFILE .EQ. 'END-OF-FILE') .OR. (TFILE .EQ. 'EXIT')) THEN
         CALL OOPS
      END IF
C
      TFILE=EXTEND(TFILE, CASE('tfr'))
      CALL INFILE (1, TFILE, ISTAT)
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Unable to open input file '//TFILE)
             TFILE = 'EXIT'
         GO TO 850
      END IF
C
      JFILE=0
      GETZERO=.FALSE.
 1000 JFILE=JFILE+1
      CALL RDCHAR (1, LINE, J, ISTAT)
      IF (LINE(2:2) .EQ. '=') GO TO 1090
      IF (JFILE .GT. MXFILE) THEN
         CALL STUPID ('Too many files in '//TFILE)
         WRITE (6,65) MXFILE
         CALL CLFILE (1)
         CALL OOPS
      END IF
C
      IF (J .LE. 54) THEN
         READ (LINE,100) FILE(JFILE), ZERO(JFILE), SZ(JFILE)
  100    FORMAT (1X, A30, 2F9.4)
         IF (LINE(22:39) .EQ. ' ') ZERO(JFILE) = 99.999
      ELSE
         READ (LINE,101) FILE(JFILE), ZERO(JFILE), SZ(JFILE)
  101    FORMAT (1X, A40, 2F9.4)
         IF (LINE(32:49) .EQ. ' ') ZERO(JFILE) = 99.999
      END IF
      IF (ZERO(JFILE) .GE. 90.) THEN
         GETZERO=.TRUE.
      ELSE
         SZ(JFILE)=SZ(JFILE)**2
      END IF
      VALID(JFILE) = .TRUE.
      J = LENGTH(FILE(JFILE))
      I = J-3
      IF (FILE(JFILE)(I:J) .EQ. '.stc') VALID(JFILE) = .FALSE.
      IF (FILE(JFILE)(I:J) .EQ. '.mag') VALID(JFILE) = .FALSE.
      IF (FILE(JFILE)(I:J) .EQ. '.nmg') VALID(JFILE) = .FALSE.
      IF (FILE(JFILE)(I:J) .EQ. '.2ma') VALID(JFILE) = .FALSE.
      GO TO 1000
C
C Read the standard-star library
C
 1090 IF (.NOT. GETZERO) THEN
         NMAG = 0
         GO TO 1200
      END IF
      CFILE = SWITCH(TFILE, '.lib')
 1100 CALL GETNAM ('Library file:', CFILE)
      IF ((CFILE .EQ. 'END-OF-FILE') .OR. (CFILE .EQ. 'EXIT')) THEN
         CALL CLFILE (1)
         IF (CFILE .EQ. 'EXIT') THEN
            CALL OOPS
         ELSE
            GO TO 850
         END IF
      END IF
C
      CFILE = EXTEND(CFILE, CASE('lib'))
      CALL INFILE (2, CFILE, ISTAT)
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Error opening input file.')
         CFILE = 'EXIT'
         GO TO 1100
      END IF
C
C Read in the number of indices and and magnitudes, and
C the index labels.
C
      READ (2,210) NMAG, (MAGLBL(I), I=1,NMAG)
  210 FORMAT (I2, 21X, 6(3X, A6, 5X))
      NLINE = 69 + 18*NMAG
      IF (GETZERO) THEN
C
C Read star ID's.
C
         I=0
 1120    I=I+1
         IF (I .GT. MAXSTD) THEN
            CALL STUPID ('Too many standard stars!')
            CALL CLFILE (2)
            CALL OOPS
         END IF
C
 1121    READ (2, 211, END=1130, ERR=1121) MASTER(I), 
     .        (STMAG(J,I), STERR(J,I), J=1,NMAG)
  211    FORMAT (1X, A12, 10X, 6(F7.3, F7.4))
         XFET(I)=-1.1E38
         DO J=1,NMAG
            STERR(J,I)=STERR(J,I)**2
         END DO
         RMIN(I) = 1.1E38
         IDMAST(I) = 0
         FNAME(I) = ' '
         GO TO 1120
C
 1130    NSTD=I-1
         PRINT*,nstd,' standards'
         CALL CLFILE (2)
      END IF
C
 1200 CONTINUE
      NFORCE = 0
      CFILE = SWITCH(TFILE, '.frc')
      CALL INFILE (2, CFILE, ISTAT)
      IF (ISTAT .NE. 0) THEN
         CFILE = 'force.dat'
         CALL INFILE (2, CFILE, ISTAT)
      END IF
      IF (ISTAT .EQ. 0) THEN
         CALL CHECK (2, NL)
  700    NFORCE = NFORCE+1
  701    CALL RDCHAR (2, LINE, I, ISTAT)
         IF (ISTAT .GT. 0) GO TO 799
         IF (ISTAT .LT. 0) GO TO 701
         IF (LINE(1:1) .EQ. '0') THEN
            FNAME(NFORCE) = EXTEND(LINE(2:13), 'lcv')
            CALL RDCHAR (2, LINE, I, ISTAT)
         END IF
         READ (LINE,*,END=799) IDF(NFORCE)
         GO TO 700
  799    NFORCE = NFORCE-1
         CALL CLFILE (2)
      END IF
C
      NCHECK = 0
      NVARY = 0
C
C Obtain the observational information
C
      CFILE = SWITCH(TFILE, '.inf')
 1250 CALL GETNAM ('Information file:', CFILE)
      IF (CFILE .EQ. 'EXIT') THEN
         CALL CLFILE (1)
         CALL OOPS
      END IF
      IF (CFILE .EQ. 'END-OF-FILE') GO TO 1100
C
      CFILE = EXTEND(CFILE, CASE('inf'))
      CALL INFILE (2, CFILE, ISTAT)
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Unable to open input file '//CFILE)
         CFILE = 'EXIT'
         GO TO 1250
      END IF
C
      CALL TBLANK
      INFO=0
 1140 INFO=INFO+1
 1141 CALL RDINF (2, INFNAM(INFO), INFM(INFO), IHR, IMIN, INFQ(INFO), 
     .     AZ, INFTIN(INFO), JDIN(INFO), CALNAM(INFO), PFILE)
      IF (INFNAM(INFO) .EQ. 'END-OF-FILE') GO TO 1190
      IF (INFNAM(INFO) .EQ. 'ERROR') GO TO 1141
      INFSET(INFO) = INFM(INFO)/10
      INFM(INFO) = INFM(INFO) - 10*INFSET(INFO)
      IF (INFM(INFO) .GT. MAXMAG) GO TO 1141
      INFNAM(INFO) = SWITCH(INFNAM(INFO), ' ')
      INFT(INFO)=IHR+IMIN/60.
      INFQ(INFO)=INFQ(INFO)-1.
      AZ = AZ / 57.29578
      INFR(INFO) = INFQ(INFO) * COS(AZ)
      INFR(INFO) = INFQ(INFO) * SIN(AZ)
      INFTIN(INFO)=2.5*ALOG10(INFTIN(INFO))
      CALNAM(INFO)=EXTEND(CALNAM(INFO), CASE('clb'))
      IF (INFO .GE. 2) THEN
         DO I=1,INFO-1
            IF (CALNAM(INFO) .EQ. CALNAM(I)) GO TO 1142
         END DO
         CALL INFILE (0, CALNAM(INFO), ISTAT)
         IF (ISTAT .NE. 0) THEN
            WRITE (6,3) 'Unable to open input file '//CALNAM(INFO)
            write (9,3) CALNAM(INFO)
            ISTAT = 0
         END IF
         CALL CLFILE (0)
      END IF
C
 1142 IF (INFO .LT. MXFILE) THEN
         GO TO 1140
      ELSE
         READ (2,*,END=1191)
         CALL STUPID ('Too many lines in '//CFILE)
         WRITE (6,65) MXFILE
   65    FORMAT ('     Only', I5, ' are permitted.')
         CALL CLFILE (2)
         CALL OOPS
      END IF
C
 1190 INFO=INFO-1
 1191 CALL CLFILE (2)
C
C=======================================================================
C
C SECTION 1(B)
C
C-----------------------------------------------------------------------
C
C Do we want to look for variables?
C
 1800 NFILE=JFILE-1
      NPAIRS = 0
      CFILE = SWITCH(TFILE, CASE('.prs'))
 1850 CALL GETNAM ('Pair file:', CFILE)
      IF (CFILE .EQ. 'EXIT') THEN
         CALL CLFILE (1)
         CALL OOPS
      END IF
C
      IF (CFILE .EQ. 'END-OF-FILE') THEN
         CALL TBLANK
         GO TO 1035
      END IF
C
C Read in frame pairs, by name.
C
      CALL INFILE (2, CFILE, ISTAT)
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Error opening input file '//CFILE)
         CFILE = 'EXIT'
         GO TO 1850
      END IF
C
 1810 NPAIRS = NPAIRS + 1
 1815 READ (2,2,IOSTAT=ISTAT) CFILE
    2 FORMAT (1X, A30)
      IF (ISTAT .NE. 0) THEN
         NPAIRS = NPAIRS-1
         GO TO 1999
      END IF
      IF (CFILE .EQ. ' ') GO TO 1815
      CFILE = LEFT(CFILE)
      CFILE = SWITCH(CFILE, ' ')
      DO JFILE=1,NFILE
         IF (CFILE .EQ. SWITCH(FILE(JFILE),' ')) THEN
            IPAIR(1,NPAIRS)=JFILE
            GO TO 1820
         END IF
      END DO
      GO TO 1815
C
 1820 IPAIR(2,NPAIRS) = 0
      READ (2,2,IOSTAT=ISTAT) CFILE
      IF (ISTAT .NE. 0) GO TO 1999
      IF (CFILE .EQ. ' ') GO TO 1810
      CFILE = LEFT(CFILE)
      CFILE = SWITCH(CFILE, ' ')
      DO JFILE=1,NFILE
         IF (CFILE .EQ. SWITCH(FILE(JFILE), ' ')) THEN
            IPAIR(2,NPAIRS)=JFILE
            GO TO 1825
         END IF
      END DO
 1825 READ (2,2,END=1999)
      GO TO 1810
C
 1999 CALL CLFILE (2)
C
C-----------------------------------------------------------------------
C
 1035 IF (.NOT. GETZERO) GO TO 1210
      CFILE = SWITCH(TFILE, '.fet')
      CALL GETNAM ('FETCH file:', CFILE)
      IF (CFILE .EQ. 'END-OF-FILE') THEN
         CALL TBLANK
         GO TO 850
      END IF
      CFILE = EXTEND(CFILE, CASE('fet'))
      CALL INFILE (2, CFILE, ISTAT)
      IF (ISTAT .NE. 0) GO TO 1035
      CALL RDCHAR (2, LINE, J, IER)
      IF (LINE(1:4) .EQ. ' NL ') THEN
         READ (2,*) J, RCOL, RROW
         READ (2,*)
      ELSE IF (LINE(4:11) .NE. 'FILTERS:') THEN
         REWIND (2)
      END IF
 1040 READ (2,231,END=1050) ANSWER, FETID
  231 FORMAT (A1, A12)
      IF (ANSWER .NE. '0') GO TO 1040
      READ (2,*) J, XX, YY
      DO I=1,NSTD
         IF (FETID .EQ. MASTER(I)) THEN
            XFET(I)=XX
            YFET(I)=YY
            GO TO 1040
         END IF
      END DO
      GO TO 1040
C
 1050 CALL CLFILE (2)
      L = 0
 1051 L = L+1
 1052 IF (L .GT. NSTD) GO TO 1059
         IF (XFET(L) .LT. -1.E38) THEN
            IF (L .LT. NSTD) THEN
               MASTER(L) = MASTER(NSTD)
               XFET(L) = XFET(NSTD)
               YFET(L) = YFET(NSTD)
               DO J=1,NMAG
                  STMAG(J,L) = STMAG(J,NSTD)
                  STERR(J,L) = STERR(J,NSTD)
               END DO
            END IF
            NSTD = NSTD-1
            GO TO 1052
         END IF
         GO TO 1051
 1059 CONTINUE
C
C See whether the .fet file is in the .mch file.  If it is, transform
C the coordinates.
C
      PFILE = SWITCH(CFILE, CASE('.mch'))
      CALL INFILE (2, PFILE, ISTAT)
      IF (ISTAT .NE. 0) GO TO 1080
 1060 CONTINUE
      CALL RDMCH (2, PFILE, CFF, DMAG, SIG, MODE)
      IF (MODE .EQ. 1) GO TO 1070
      IF (PFILE .NE. CFILE) GO TO 1060
C
      CALL STUPID ('Transforming .fet file.')
      DO I=1,NSTD
         CALL GTFM (XFET(I), YFET(I), RCOL, RROW, MODE, CFF,
     .        XX, YY)
         XFET(I) = XX
         YFET(I) = YY
      END DO
 1070 CALL CLFILE (2)
C
 1080 CONTINUE
      CALL GETDAT ('Critical radius:', CRIT, 1)
      IF (CRIT .LE. 0.) GO TO 1035
      CRITSQ=CRIT**2
C
C-----------------------------------------------------------------------
C
 1210 CONTINUE
C
      CFILE = SWITCH(TFILE, '.stc')
      NH1 = -1
      OPEN (2, FILE=EXPAND(CFILE), STATUS='OLD', IOSTAT=ISTAT)
      L0 = 70 + NMAG*18
      IF (ISTAT .EQ. 0) THEN
         WRITE (6,11) CFILE(1:LENGTH(CFILE))
   11    format(/' Found ', a/)
         L1 = L0 + 28
         L3 = L1 - 22
         L4 = L1 - 9
         RADEC = .TRUE.
         CALL RDCHAR (2, HEADER(1), NH1, ISTAT)
         CALL RDCHAR (2, HEADER(2), NH2, ISTAT)
         CALL RDCHAR (2, LINE, NL, ISTAT)
         test = line(2:14)
         IF (test .EQ. 'Field Center:') go to 1085
         IF (test .EQ. 'Field center:') go to 1085
         CALL STUPID ('Not a proper standard-coordinates file..')
         CALL CLFILE (2)
 1085    CONTINUE
         READ (LINE(16:NL),8) IRH, IRM, RS, SGN, IDD, IDM, DS
    8    FORMAT (2(X, I2), X, F5.2, 1X, A1, I2, X, I2, X, F4.1)
         RA0 = 0.261799387 * (IRH + (IRM + RS/6.D1)/6.D1)
         DC0 = 0.0174532925 * (IDD + (IDM + DS/6.D1)/6.D1)
         IF (SGN .EQ. '-') DC0 = -DC0
         CD0 = DCOS(DC0)
         SD0 = DSIN(DC0)
         CALL GETDAT ('Offsets, scale:', offset, 3)
         IF (OFFSET(1) .GT. -1.E10) THEN
            SCALE = .TRUE.
         ELSE
            CALL TBLANK
         END IF
         CLOSE (2)
      ELSE
         WRITE (6,12) CFILE(1:LENGTH(CFILE))
   12    format (/'Failed to find ', a/)
         L1 = L0+4
      END IF
C
      CALL TBLANK
      DO 1150 JFILE=1,NFILE
      CFILE=SWITCH(FILE(JFILE), ' ')
      DO I=1,INFO
         IF (CFILE .EQ. INFNAM(I)) THEN
            CALL INFILE (2, CALNAM(I), ISTAT)
            IF (ISTAT .NE. 0) GO TO 1149
            L = LENGTH(CALNAM(I))
            IF (CALNAM(I)(L-2:L) .EQ. 'ptg') THEN
               CALL PGREAD (COEFF(1,1,JFILE))
               PHOTOG(I) = .TRUE.
            ELSE
               CALL READTFM (NINDX(JFILE), MAXMAG, MAGNDX(1,1,JFILE),
     .              NTERM(1,JFILE), ITERM(1,1,1,JFILE), 
     .              COEFF(1,1,JFILE), USE(1,1,JFILE), EXTRA)
               PHOTOG(I) = .FALSE.
               IF (.NOT. GETZERO) THEN
                  DO J=1,MAXNDX
                     DO JMAG=1,MAXMAG
                        IF (ABS(MAGNDX(JMAG,J,JFILE)) .GT. 1.E-10) THEN
                           NMAG = MIN0(MAXMAG, MAX0(NMAG, JMAG))
                        END IF
                     END DO
 1215             CONTINUE
                  END DO
               END IF
            END IF
            CALL CLFILE (2)
            IMAG(JFILE) = INFM(I)
            ISET(JFILE) = INFSET(I)
            COSMIC(JFILE) = EXTRA(iabs(IMAG(JFILE)))
            TIME(JFILE) = INFT(I)
            AM(JFILE) = INFQ(I)
            R(JFILE) = INFR(I)
            S(JFILE) = INFS(I)
            HJD(JFILE) = JDIN(I)
            TIN(JFILE) = INFTIN(I)
            GO TO 1150
         END IF
      END DO
      WRITE (6,3) 'Observational information not found for '//CFILE
 1149 IMAG(JFILE) = 0
 1150 CONTINUE
      CALL TBLANK
C
      HJD0 = 1.0D38
      DO JFILE=1,NFILE
         IF (IMAG(JFILE) .NE. 0) THEN
            IF (HJD(JFILE) .LT. HJD0) HJD0 = HJD(JFILE)
         END IF
      END DO
C
      DO JFILE=1,NFILE
         IF (IMAG(JFILE) .NE. 0) THEN
            HJD(JFILE) = HJD(JFILE) - HJD0
         END IF
      END DO
C
C-----------------------------------------------------------------------
C
      DO I=1,NMAG
         CORRECT(I) = 1.
      END DO
      VLIMIT(1) = 1.1E38
      VLIMIT(2) = 1.1E38
      VLIMIT(3) = 2.
      IF (NPAIRS .GT. 0) THEN
         CALL GETDAT 
     .        ('Limits on variability, weight, magnitude:', 
     .        VLIMIT, 3)
         IF (VLIMIT(1) .LT. -1.E2) THEN
            VLIMIT(1) = 1.E38
            CALL TBLANK
         END IF
      END IF
      CFILE = SWITCH(TFILE, CASE('.fnl'))
      CALL GETNAM ('Output file name:', CFILE)
      CALL OUTFIL (3, CFILE, ISTAT)
      IF (NH1 .GT. 0) THEN
         WRITE (3,3) HEADER(1)(1:NH1)
    3    FORMAT (A)
         WRITE (3,3) HEADER(2)(1:NH2)
      END IF
      IF (NPAIRS .GT. 0) THEN
         CFILE = SWITCH(CFILE, CASE('.vry'))
         CALL OUTFIL (9, CFILE, ISTAT)
         WRITE (9,3) HEADER(1)(1:NH1)
         WRITE (9,3) HEADER(2)(1:NH2)
      END IF
      N = 0
      IF (NPAIRS .GT. 1) THEN
         N = N+1
         HEAD(N) = '|---------- variability -----------'
         NOBS(N) = 34
         IF (RADEC) THEN
            N = N+1
            HEAD(N) = '|--- RA  (2000)  Dec ----'
            NOBS(N) = 25
         END IF
      ELSE IF (RADEC) THEN
         N = N+1
         HEAD(N) = '|--- RA  (2000)  Dec ---'
         NOBS(N) = 24
      END IF
C
      IF (N .EQ. 0) THEN
         WRITE (3,301) NMAG, (MAGLBL(I), '        ', I=1,NMAG),
     .     (' n   ', I=1,NMAG), ' chi  sharp '
  301    FORMAT (I2, ' FILTERS:', 17X, 21A)
         IF (NPAIRS .GT. 0) WRITE (3,301) NMAG, 
     .        (MAGLBL(I), '        ', I=1,NMAG),
     .     (' n   ', I=1,NMAG), ' chi  sharp '
      ELSE
         WRITE (3,301) NMAG, (MAGLBL(I), '        ', I=1,NMAG),
     .     (' n   ', I=1,NMAG), ' chi  sharp ',
     .     (HEAD(I)(1:NOBS(I)), I=1,N)
         IF (NPAIRS .GT. 0) WRITE (9,301) NMAG, 
     .     (MAGLBL(I), '        ', I=1,NMAG),
     .     (' n   ', I=1,NMAG), ' chi  sharp ',
     .     (HEAD(I)(1:NOBS(I)), I=1,N)
      END IF
C
      IF (GETZERO) THEN
         CFILE = SWITCH(CFILE, CASE('.zer'))
         CALL OUTFIL (2, CFILE, ISTAT)
         WRITE (2,1)
    1    FORMAT (8X, 'Std     Calc    Obs     Dif      Sig   ',
     .           'Wt      X         Y')
      END IF
C
C Read the input star list finding the standard stars.
C
 2050 IF (NFILE .LE. 18) THEN
         READ (1,120,END=2100) JD, XM, YM,
     .     (JWHICH(JFILE), JFILE=1,NFILE)
  120    FORMAT (I7, 2F9.2, 18I7)
      ELSE
         READ (1,121,END=2100) JD, XM, YM,
     .     (JWHICH(JFILE), JFILE=1,NFILE)
  121    FORMAT (I7, 2F9.2, 18I7 / (25X, 18I7))
      END IF
C
C Is this a local standard?
C
      J = 0
      RR = CRITSQ
      DO I=1,NSTD
         RAD=(XM-XFET(I))**2+(YM-YFET(I))**2
         IF ((RAD .LT. RR) .AND. (RAD .LT. RMIN(I))) THEN
            J = I
            RR = RAD
         END IF
      END DO
C
      IF (J .GT. 0) THEN
         IDMAST(J) = JD
         RMIN(J) = RR
         DO I=1,NFILE
            IWHICH(I,J) = JWHICH(I)
         END DO
      END IF
      GO TO 2050
C
 2100 CALL CLFILE (1)
C
      DO I=1,NSTD
         IF (IDMAST(I) .LE. 0) THEN
            WRITE (6,*) 'Not found in transfer file: '//MASTER(I)
         END IF
      END DO
C
C=======================================================================
C
C For each frame, determine the instrumental zero-point correction from
C the standard stars, if necessary.  Read all the stellar data into one
C massive data buffer.  NOTICE THAT IWHICH(JFILE,i) IS NUMBERED I=1,NSTD
C
      CALL TBLANK
      NTOT = 0
      DO 2900 JFILE=1,NFILE
      IF (.NOT. VALID(JFILE)) GO TO 2900
      IF ((iabs(IMAG(JFILE)) .GT. NMAG) .OR. (IMAG(JFILE) .EQ. 0)) THEN
         VALID(JFILE) = .FALSE.
         GO TO 2900
      END IF
      BEFORE(JFILE) = NTOT
C
C Read in all stars from this file.
C
      CALL INFILE (1, FILE(JFILE), ISTAT)
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Unable to open input file '//
     .        FILE(JFILE))
         CALL OOPS
      END IF
C
C-----------------------------------------------------------------------
C HST modifications
C-----------------------------------------------------------------------
C
      IF (HST) THEN
         DO I=2,30
            IF (FILE(JFILE)(I:I) .EQ. ':') GO TO 2802
         END DO
         LIMIT = -1.E10
         GO TO 2804
 2802    I = I-1
         IF (FILE(JFILE)(I:I) .EQ. 'p') THEN
            LIMIT = 100.
         ELSE IF (FILE(JFILE)(I:I) .EQ. 'w') THEN
            LIMIT = 75.
         ELSE
            LIMIT = -1.E10
         END IF
         IF (LIMIT .GT. 0.) THEN
            IF (HJD(JFILE)+HJD0 .LT. 49466.D0) THEN
                DO J=1,6
                   G(J) = WARM(J)
                END DO
            ELSE
                DO J=1,6
                   G(J) = COLD(J)
                END DO
            END IF
         END IF
      END IF
C-----------------------------------------------------------------------
C
 2804 CONTINUE
      READ (1,127)
      READ (1,127) RCOL, RROW, GAIN
  127 FORMAT (3X, 2F6.0, 32X, F8.1)
      READ (1,127)
      X0 = (RCOL+1.)/2.
      Y0 = (RROW+1.)/2.
      JMAG=iabs(IMAG(JFILE))
 2810 NTOT = NTOT+1
      IF (NTOT .GT. MAXSTR) THEN
         CALL STUPID ('Too many stellar observations!')
         CALL CLFILE (1)
         CALL CLFILE (2)
         CALL CLFILE (3)
         CALL CLFILE (9)
         CALL OOPS
      END IF
C
 2820 CALL RDCHAR (1, LINE, J, ISTAT)
      IF (ISTAT .GT. 0) GO TO 2830
      if (j .le. 1) go to 2820
      LINE = LINE(1:J)//' 0'
      READ (LINE,*) J, X(NTOT), Y(NTOT), RAW(NTOT), 
     .     SIGMA, SKY, CORR, CHI(NTOT), SHP(NTOT)
      IF (J .LE. 0) GO TO 2820
      IF (RAW(NTOT) .LT. OBSLIM) THEN
         RAW(NTOT) = 99.999
         RAWE(NTOT) = 9.999
      END IF
C
      IF (HST .AND. (LIMIT .GT. 0.)) THEN
         IF ((X(NTOT) .LT. LIMIT) .OR. 
     .       (Y(NTOT) .LT. LIMIT)) THEN
            RAW(NTOT) = 99.999
            RAWE(NTOT) = 9.999
         ELSE
C
C Compute and apply ramp corrections.
C
Cc            XXX = (X(NTOT) - 425.)/375.
Cc            YYY = (Y(NTOT) - 425.)/375.
            XXX = X(NTOT)/400.
            YYY = Y(NTOT)/400.
              
            SKY = GAIN*SKY
            SKYLOG = ALOG10(SQRT(DAMP + AMAX1(0., SKY)**2)
     .           / 1.E1)
            CTS = GAIN*10.**(0.4*(25.-RAW(NTOT))) / 1.E4
            CTS = ALOG10(CTS)
            YCTE = G(1) + G(5)*SKYLOG + G(3)*CTS
            YCTE = EXP(YCTE)
            XCTE = G(2) + G(6)*SKYLOG + G(4)*CTS
            XCTE = EXP(XCTE)
            CORR = 1.D0 + 0.01*YCTE*(G(7)+YYY) +
     .           0.01*XCTE*XXX
            CORR = -1.085736*ALOG(CORR)
            RAW(NTOT) = RAW(NTOT) + CORR
         END IF
      END IF
C
      IF (PHOTOG(JFILE)) THEN
         RAWE(NTOT) = SIGMA**2
         Z(NTOT) = (X(NTOT)-X0)**2 + (Y(NTOT)-Y0)**2
C
C Z is a radial distance
C
      ELSE
C
C Divide positional coordinate by 1000 for transformation equations.
C
         X(NTOT) = X(NTOT) * 0.001
         Y(NTOT) = Y(NTOT) * 0.001
         RAW(NTOT)=RAW(NTOT)+TIN(JFILE)
         RAWE(NTOT)=(CORRECT(JMAG)*SIGMA)**2 + 1.E-6
C
C Reduce weight of large CHI (possibly saturated).
C
         RAWE(NTOT) = RAWE(NTOT)*(10.+CHI(NTOT))/10.
      END IF
      GO TO 2810
C
 2830 NTOT = NTOT-1
      NSTAR(JFILE) = NTOT - BEFORE(JFILE)
      CALL CLFILE (1)
C
C Now, if necessary, compute the zero-point correction to the
C instrumental magnitudes from all the available local standards.
C
      IF (ZERO(JFILE) .GE. 99.) THEN
         IF (NSTD .LE. 0) THEN
            CALL STUPID ('No local standards found!')
            CALL OOPS
         END IF
         N=0
         DO 2850 I=1,NSTD
C
C If not matched.
C
            IF (IDMAST(I) .LE. 0) GO TO 2850
C
C If magnitude not defined.
C
            IF (ABS(STMAG(JMAG,I)) .GT. 50.) GO TO 2850
            ISTAR = IWHICH(JFILE,I)
C
C If doesn't appear in this image.
C
            IF (ISTAR .LE. 0) GO TO 2850
            ISTAR = BEFORE(JFILE) + ISTAR
            IF (ISTAR .GT. NTOT) THEN
               CALL STUPID ('Error in .tfr file')
               CALL OOPS
            END IF
C
C If invalid measurement.
C
            IF (ABS(RAW(ISTAR)) .GT. 50.) GO TO 2850
C
            CALL MAG2NDX (STMAG(1,I), STERR(1,I), NMAG,
     .           MAGNDX(1,1,JFILE), NINDX(JFILE), NDX, .false.)
            IF (PHOTOG(JFILE)) THEN
               DMAG = PGCALC(STMAG(JMAG,I), NDX(3),
     .              X(ISTAR), Y(ISTAR), Z(ISTAR),
     .              COEFF(1,1,JFILE), DCDS)
            ELSE
               DMAG = TRFM(NINDX(JFILE), NDX, AM(JFILE), TIME(JFILE), 
     .              X(ISTAR), Y(ISTAR), R(JFILE), S(JFILE),
     .              NTERM(JMAG,JFILE), ITERM(1,1,JMAG,JFILE), 
     .              COEFF(1,JMAG,JFILE), USE(1,JMAG,JFILE), .false.)
            END IF
C
            IF (ABS(DMAG) .LT. 50.) THEN
               N=N+1
               IF (PHOTOG(JFILE)) THEN
                  DAT(N) = STMAG(JMAG,I) + DMAG - RAW(ISTAR)
                  SIG(N) = STERR(JMAG,I)*(1.+DCDS)**2 + 
     .                  RAWE(ISTAR) + 1.E-4
               ELSE
                  DAT(N)=STMAG(JMAG,I) + DMAG - RAW(ISTAR)
                  SIG(N)=STERR(JMAG,I)+RAWE(ISTAR)+COSMIC(JFILE)
C
               END IF
            END IF
 2850    CONTINUE
         IF (N .LE. 0) THEN
            ZERO(JFILE) = 99.999
            WRITE (6,661) JFILE, 99.9999, 9.9999, 9.9999, 0, 0.,
     .           IMAG(JFILE), FILE(JFILE)
            WRITE (2,661) JFILE, 99.9999, 9.9999, 9.9999, 0, 0.,
     .           IMAG(JFILE), FILE(JFILE)
            GO TO 2900
         END IF
C
         ZERO(JFILE) = 0.
         ENIGHT = 0.001
         CALL RMEAN (DAT, SIG, N, 2., 2., 0.000001, ZERO(JFILE), 
     .        SZ(JFILE), RME1(1), ENIGHT, W, .FALSE.)
         IF (ENIGHT .LT. -1.) CALL OOPS
         SZ(JFILE)=SZ(JFILE)*(WWW+N*RME1(1)**2)/(WWW+N)
         N=0
         DMAG = 0.
         DO 2870 I=1,NSTD
            IF (ABS(STMAG(JMAG,I)) .GT. 50.) GO TO 2870
            ISTAR = IDMAST(I)
            IF (ISTAR .LE. 0) GO TO 2870
            ISTAR = IWHICH(JFILE,I)
            IF (ISTAR .LE. 0) GO TO 2870
            ISTAR = BEFORE(JFILE) + ISTAR
            IF (ABS(RAW(ISTAR)) .GT. 50.) GO TO 2870
            CALL MAG2NDX (STMAG(1,I), STERR(1,I), NMAG,
     .           MAGNDX(1,1,JFILE), NINDX(JFILE), NDX, .false.)
            IF (PHOTOG(JFILE)) THEN
               DMAG = PGCALC(STMAG(JMAG,I), NDX(3),
     .              X(ISTAR), Y(ISTAR), Z(ISTAR),
     .              COEFF(1,1,JFILE), DCDS)
                  WSI = STERR(JMAG,I)*(1.+DCDS)**2 + 
     .                  RAWE(ISTAR) + 1.E-4
            ELSE
               DMAG = TRFM(NINDX(JFILE), NDX, AM(JFILE), TIME(JFILE), 
     .              X(ISTAR), Y(ISTAR), R(JFILE), S(JFILE),
     .              NTERM(JMAG,JFILE), ITERM(1,1,JMAG,JFILE), 
     .              COEFF(1,JMAG,JFILE), USE(1,JMAG,JFILE), .false.)
                  WSI = STERR(JMAG,I)+RAWE(ISTAR)+COSMIC(JFILE)
            END IF
C
            IF (ABS(DMAG) .GE. 50.) GO TO 2870
            N = N+1
            DMAG = STMAG(JMAG,I) + DMAG
            WRITE (2,620) STMAG(JMAG,I), DMAG,
     .           RAW(ISTAR), DMAG-RAW(ISTAR), SQRT(WSI), 
     .           NINT(10.*W(N,1)), 1000.*X(ISTAR), 
     .           1000.*Y(ISTAR), MASTER(I), FILE(JFILE)
  620       FORMAT (5X, 5F8.3, I4, 2F10.3, 3X, A12, 2X, A40)
            DMAG = DMAG + W(N,1)
 2870    CONTINUE
         WRITE (2,621) ZERO(JFILE), SQRT(SZ(JFILE)), FILE(JFILE), 
     .        N, RME1(1)
 621     FORMAT (33X,'------- -------'/32X, 2F8.4, 2X, A30/
     .        33X, I5, F10.4/)
         WRITE (6,661) JFILE, ZERO(JFILE), SQRT(SZ(JFILE)), RME1(1), 
     .        N, DMAG, IMAG(JFILE), FILE(JFILE)
  661    FORMAT (I6, F9.4, F8.4, F9.4, I6, F8.1, I3, 2X, A40,'<')
         WRITE (2,661) JFILE, ZERO(JFILE), SQRT(SZ(JFILE)), RME1(1), 
     .        N, DMAG, IMAG(JFILE), FILE(JFILE)
         WRITE (2,*)
      END IF
C
C Add zero-point corrections to every star in every file.
C
      ISTAR = BEFORE(JFILE)
      DO 2890 I=1,NSTAR(JFILE)
         ISTAR = ISTAR+1
         IF (ABS(RAW(ISTAR)) .GT. 50.) GO TO 2890
         RAW(ISTAR)=RAW(ISTAR)+ZERO(JFILE)
         RAWE(ISTAR)=RAWE(ISTAR)+SZ(JFILE)
 2890 CONTINUE
C
 2900 CONTINUE
      CALL CLFILE (2)

C
C=======================================================================
C
C Now we are ready to work through the master list star by star.
C For each star, we will collect all the individual observations from
C the various input files, and find those standard magnitudes and colors
C which best fit the observed magnitudes.
C
      CALL TBLANK
      CALL TBLANK
      DO JMAG=1,MAXMAG
         NRED(JMAG)=0
      END DO
      CALL INFILE (1, TFILE, ISTAT)
 2930 CALL RDCHAR (1, LINE, J, ISTAT)
      IF (LINE(2:2) .NE. '=') GO TO 2930
      MXEPCH = 0
      IMASTR = 0
C
C BEGINNING OF  B I G  LOOP OVER STARS
C
 7900 IMASTR = IMASTR+1
 7901 IF (NFILE .LE. 18) THEN
         READ (1,120,END=8900) JD, XXX, YYY,
     .        (JWHICH(JFILE), JFILE=1,NFILE)
      ELSE
         READ (1,121,END=8900) JD, XXX, YYY,
     .        (JWHICH(JFILE), JFILE=1,NFILE)
      END IF
C
      IF (MOD(IMASTR,100) .EQ. 0) THEN
         WRITE (LINE,39) IMASTR
   39    FORMAT (I10)
         CALL OVRWRT (LINE(1:10), 2)
      END IF
C
C First, collect up all the observations of this star and sort by 
C filter.
C
      WSI = 0.
      KUR = 0.
      VARY = 0.
      VAR = .FALSE.
      WEIGHT = 0.
      DO JMAG=1,NMAG
         NOBS(JMAG) = 0
      END DO
C
      DO 3100 JFILE=1,NFILE
         NWHICH(JFILE) = 0
         IF (JWHICH(JFILE) .LE. 0) GO TO 3100
         IF (ZERO(JFILE) .GT. 90.) GO TO 3100
         ISTAR = BEFORE(JFILE) + JWHICH(JFILE)
         IF (ABS(RAW(ISTAR)) .GT. 50.) GO TO 3100
         JMAG = iabs(IMAG(JFILE))
         IF (JMAG .LE. 0) GO TO 3100
         N = NOBS(JMAG)+1
         IF (N .GT. MAXOBS) THEN
            CALL STUPID ('Sorry, too many images in this filter.')
            WRITE (6,*) JMAG, N, MAXOBS
            CALL OOPS
         END IF
         NOBS(JMAG) = N
         NWHICH(JFILE)  =  N
         IFILE(N,JMAG) = JFILE
         OBS(N,JMAG) = RAW(ISTAR)
         OBSE(N,JMAG) = RAWE(ISTAR)
         OBSCHI(N,JMAG) = CHI(ISTAR)
         OBSSHP(N,JMAG) = SHP(ISTAR)
         OBSPG(N,JMAG) = PHOTOG(JFILE)
         OBSX(N,JMAG) = X(ISTAR)
         OBSY(N,JMAG) = Y(ISTAR)
         OBSZ(N,JMAG) = Z(ISTAR)
 3100 CONTINUE
C
C Obtain first crude estimate of the standard magnitudes, which will be
C the mean of the instrumental magnitudes minus the transformations
C with all photometric indices (not counting airmass and time) unity
C (assumed to be close to the mean value of most colors).
C
      DO I=1,MAXNDX
         NDX(I) = 1.
      END DO
C
      DO 3150 JMAG=1,NMAG
 3120    IF (NOBS(JMAG) .LE. 0) THEN
            MAG(JMAG) = 99.999
            MAGE(JMAG) = 99.998
            GO TO 3150
         END IF
         DO I=1,NOBS(JMAG)
            JFILE = IFILE(I,JMAG)
            IF (OBSPG(I,JMAG)) THEN
               DAT(I) = OBS(I,JMAG) - PGCALC(20., 0.,
     .              OBSX(I,JMAG), OBSY(I,JMAG), OBSZ(I,JMAG),
     .              COEFF(1,1,JFILE), DCDS)  
               DAT(I) = DAT(I)/(1.+DCDS)
            ELSE
               DAT(I) = OBS(I,JMAG)-TRFM(NINDX(JFILE), NDX, AM(JFILE), 
     .             TIME(JFILE), OBSX(I,JMAG), OBSY(I,JMAG), R(JFILE),
     .             S(JFILE), NTERM(JMAG,JFILE), ITERM(1,1,JMAG,JFILE), 
     .             COEFF(1,JMAG,JFILE), USE(1,JMAG,JFILE), .false.)
            END IF
         END DO
         EXTRA(JMAG) = 0.
         MAG(JMAG) = 20.
         CALL RMEAN (DAT, OBSE(1,JMAG), NOBS(JMAG), 2., 2., 0.00001, 
     .       MAG(JMAG), MAGE(JMAG), RME1(JMAG), EXTRA(JMAG), WW, 
     .       .FALSE.)
         IF (EXTRA(JMAG) .LT. -1.) THEN
            MAG(JMAG) = 99.999
            MAGE(JMAG) = 9.9999
            GO TO 8000
         END IF
C
         EXTRA(JMAG) = 0.
         MAGE(JMAG)=MAGE(JMAG)*
     .        (WWW+NOBS(JMAG)*RME1(JMAG)**2)/(WWW+NOBS(JMAG))
         CLAMP(JMAG)=1.0
         OLD(JMAG)=0.
 3150 CONTINUE
C
C Beginning of iteration loop for this star.
C
 3499 NITER = 0
 3500 NITER = NITER+1
C
      REDO = .FALSE.
      SUMCHI = 0.
      SUMSHP = 0.
      SUMWTS = 0.
      DO 3700 JMAG=1,NMAG
         NVALID(JMAG) = 0
         IF (NOBS(JMAG) .LE. 0) THEN
            MAG(JMAG) = 99.999
            MAGE(JMAG) = 99.998
            GO TO 3700
         END IF
         FLUX(JMAG) = 10.**(0.4*(25.-MAG(JMAG)))
         J = 0
         I = 0
 3510    I = I+1
         IF (I .GT. NOBS(JMAG)) GO TO 3540
 3520    JFILE = IFILE(I,JMAG)
         CALL MAG2NDX (MAG, MAGE, NMAG, MAGNDX(1,1,JFILE),
     .        NINDX(JFILE), NDX, .false.)
C
         IF (OBSPG(I,JMAG)) THEN
            D = OBS(I,JMAG) - PGCALC(MAG(JMAG), 
     .           NDX(3), OBSX(I,JMAG), OBSY(I,JMAG), 
     .           OBSZ(I,JMAG), COEFF(1,1,JFILE), DCDS)  
            IF (ABS(D) .GT. 90.) GO TO 3510
C
            J = J+1
            DAT(J) = 10.**(0.4*(25.-D))
            SIG(J) = 0.8483037*OBSE(I,JMAG)*(DAT(I)/(1.+DCDS))**2
         ELSE
            D = TRFM(NINDX(JFILE), NDX, AM(JFILE), TIME(JFILE), 
     .           OBSX(I,JMAG), OBSY(I,JMAG), R(JFILE), S(JFILE),
     .           NTERM(JMAG,JFILE), ITERM(1,1,JMAG,JFILE), 
     .           COEFF(1,JMAG,JFILE), USE(1,JMAG,JFILE), .false.)
            IF (ABS(D) .GT. 50.) GO TO 3510
C
            J = J+1
            DAT(J) = OBS(I,JMAG) - D
            DAT(J) = 10.**(0.4*(25.-DAT(J) ))
            SIG(J) = 0.8483037*OBSE(I,JMAG)*DAT(J)**2
            ISTAR = BEFORE(JFILE) + JWHICH(JFILE)
            SHARP(J) = SHP(ISTAR)
            CHICH(J) = CHI(ISTAR)
         END IF
C
         DAT(J) = DAT(J) - FLUX(JMAG)
         IF (I .LT. NOBS(JMAG)) GO TO 3510
C
 3540    CONTINUE
         IF (J .LE. 0) THEN
CXYZ        MAG(JMAG) = 99.999
            MAGE(JMAG) = 99.998
            GO TO 3700
         END IF
         DMAG = 0.
         CALL RMEAN (DAT, SIG, J, 2., 2., 0.00003*FLUX(JMAG),
     .     DMAG, MAGE(JMAG), RME1(JMAG), EXTRA(JMAG), WW, .FALSE.)
         IF (EXTRA(JMAG) .LT. -1.) THEN
            MAG(JMAG) = 99.999
            MAGE(JMAG) = 9.9999
            GO TO 8000
         END IF
         DMAG = DMAG / (1. + ABS(DMAG)/FLUX(JMAG))
         DMAG = -2.5*ALOG10((FLUX(JMAG)+DMAG)/FLUX(JMAG))
         IF (DMAG*OLD(JMAG) .LT. 0.) CLAMP(JMAG)=0.7*CLAMP(JMAG)
         DMAG = CLAMP(JMAG)*DMAG
         D = MAG(JMAG)+DMAG
         DMAG = D-MAG(JMAG)                  !Defeats truncation error
         IF (MAX(ABS(DMAG),ABS(OLD(JMAG))) .GT. 3.E-5) REDO=.TRUE.
         MAG(JMAG) = D
         FSQ = FLUX(JMAG)**2
         MAGE(JMAG) = 1.1788231*MAGE(JMAG)/FSQ
         OLD(JMAG)=DMAG
C
C Compute weighted mean values of CHI and SHARP.
C
         NVALID(JMAG) = J
         DO 3990 I=1,J
            SIG(I) = 1.1788231*SIG(I)/FSQ
            WT = WW(I)/SIG(I)
            SUMCHI = SUMCHI+WT*CHICH(I)
            SUMSHP = SUMSHP+WT*SHARP(I)
            SUMWTS = SUMWTS+WT
 3990    CONTINUE
 3700 CONTINUE
      IF (REDO .AND. (NITER .LT. 2000)) GO TO 3500
C
C End of iteration loop for star.
C
      DO JMAG=1,NMAG
         IF (MAGE(JMAG) .LT. 25.) MAGE(JMAG)=MAGE(JMAG)*
     .        (WWW+NOBS(JMAG)*RME1(JMAG)**2)/(WWW+NOBS(JMAG))
C
C Convert MAGE and OBSE from variance to standard deviation.
C
         MAGE(JMAG)=SQRT(MAGE(JMAG))
         DO I=1,NOBS(JMAG)
            OBSE(I,JMAG) = SQRT(OBSE(I,JMAG))
         END DO
      END DO
      IF (SUMWTS .LE. 0.) GO TO 8000
C
      SUMCHI=SUMCHI/SUMWTS
      SUMSHP=SUMSHP/SUMWTS
C
C Determine whether the star has enough independent epochs to be treated
C as a variable.  Determine the date of the first and last observations
C considering only those filters that have at least seven epochs.
C
      DATE0 = 1.D38
      DDATE =-1.D38
      NP = 0
      DO JMAG=1,NMAG
         NEPOCH(JMAG) = 0
         IF (NOBS(JMAG) .GT. 0) THEN
            N = 0
            DO 3330 I=1,NOBS(JMAG)
               IF (OBS(I,JMAG) .GT. 50.) GO TO 3330
               XX = SNGL(HJD(IFILE(I,JMAG)))
               IF (I .EQ. 1) THEN
                  N = 1
                  PP(1) = XX
               ELSE
                  DO K=1,N
                     IF (ABS(XX-PP(K)) .LT. 0.5) GO TO 3330
                  END DO
                  N = N+1
                  PP(N) = XX
               END IF
 3330       CONTINUE
            IF (N .GT. MXEPCH) MXEPCH = N
            IF (N .GT. NP) NP = N
            NEPOCH(JMAG) = N
         END IF
C
         IF (NEPOCH(JMAG) .GE. ENOUGH) THEN
            DO I=1,NOBS(JMAG)
               IF (OBS(I,JMAG) .LT. 50.) THEN
                  OMEGA = HJD(IFILE(I,JMAG))
                  IF (OMEGA .LT. DATE0) DATE0 = OMEGA
                  IF (OMEGA .GT. DDATE) DDATE = OMEGA
               END IF
            END DO
         END IF
      END DO
      DDATE = DDATE-DATE0
C
C The star must have at least ENOUGH epochs in at least one filter
C to be considered further.
C
      IF (NP .LT. ENOUGH) GO TO 8000
      IF (NPAIRS .LE. 1) GO TO 8000
C
C=======================================================================
C
C COMPUTE VARIABILITY INDEX.
C
C Compute kurtosis index.
C
      DMAG = 0.
      SUMWTS = 0.
      DO JMAG=1,NMAG
         IF (NOBS(JMAG) .GE. 3) THEN
            SUMA = 0.
            SUMS = 0.
            N = 0
            DO I=1,NOBS(JMAG)
               XX = (OBS(I,JMAG)-MAG(JMAG))/OBSE(I,JMAG)
               SUMA = SUMA + ABS(XX)
               SUMS = SUMS + XX**2
               N = N+1
            END DO
            SUMA = (SUMA/N)**2
            SUMS = SUMS/N
            KUR = KUR+(NOBS(JMAG)-1.)*SUMA/SUMS
            DMAG = DMAG+NOBS(JMAG)-1.
         END IF
      END DO
      IF (DMAG .GT. 0.) THEN
         KUR = KUR/DMAG
      ELSE
         GO TO 8000
      END IF
C
C Compute modified Welch/Stetson index.
C
      IF (NPAIRS .LE. 0) GO TO 8000
      var = .false.
      if (nforce .gt. 0) then
         do i=1,nforce
            if (jd .eq. idf(i)) then
               if (fname(i) .eq. ' ') then
                  pfile = switch (cfile, pfile)
               else
                  pfile = switch(cfile, fname(i))
               end if
               var = .true.
               pfile = switch(pfile, '.wsi')
               call delfil (10, pfile, istat)
               call outfil (10, pfile, istat)
               go to 97
            end if
         end do
      end if
C
   97 continue
C
      DO JMAG=1,NMAG
         IF (MAG(JMAG) .LT. 50.) THEN
            FLUX(JMAG) = 10.**(0.4*(25.-MAG(JMAG)))
         ELSE
            FLUX(JMAG) = -1.
         END IF
         IF (NOBS(JMAG) .GE. 2) THEN
            OLD(JMAG) = SQRT(REAL(NOBS(JMAG))/REAL(NOBS(JMAG)-1))
         ELSE
            OLD(JMAG) = 0.
         END IF
      END DO
C
      DO 3350 I=1,NPAIRS
         N1 = NWHICH(IPAIR(1,I))
         IF (N1 .LE. 0) THEN
            O1 = 99.999
            GO TO 3335
         END IF
C
         J1 = iabs(IMAG(IPAIR(1,I)))
         O1 = OBS(N1,J1)
         IF (ABS(O1-20.) .GT. 20.) THEN
            O1 = 99.999
            GO TO 3335
         END IF
         JFILE = IFILE(N1,J1)
         CALL MAG2NDX (MAG, MAGE, NMAG, MAGNDX(1,1,JFILE),
     .        NINDX(JFILE), NDX, .false.)
         IF (OBSPG(N1,J1)) THEN
            D = PGCALC(MAG(J1), NDX(3), OBSX(N1,J1), 
     .           OBSY(N1,J1), OBSZ(N1,J1), COEFF(1,1,JFILE), DCDS)
         ELSE 
            D = TRFM(NINDX(JFILE), NDX,  AM(JFILE), TIME(JFILE), 
     .           OBSX(N1,J1), OBSY(N1,J1), R(JFILE), S(JFILE),
     .           NTERM(J1,JFILE), ITERM(1,1,J1,JFILE), 
     .           COEFF(1,J1,JFILE), USE(1,J1,JFILE), .FALSE.)
         END IF
         IF (D .GT. 50.) THEN
            O1 = 99.999
            GO TO 3335
         END IF
         O1 = O1 - D
         O1 = 10.**(0.4*(25.-O1))
         E1 = 0.921034*O1*OBSE(N1,J1)
         O1 = (O1-FLUX(J1))/E1
C
 3335    J2 = IPAIR(2,I)
         IF (J2 .EQ. 0) GO TO 3340
         N2 = NWHICH(J2)
         IF (N2 .LE. 0) GO TO 3340
C
         J2 = iabs(IMAG(IPAIR(2,I)))
         IF (J2 .LE. 0) GO TO 3340
         IF (NOBS(J2).LE.1) GO TO 3340
         O2 = OBS(N2,J2)
         IF (ABS(O2-20.) .GT. 20.) GO TO 3340
         JFILE = IFILE(N2,J2)
         CALL MAG2NDX (MAG, MAGE, NMAG, MAGNDX(1,1,JFILE),
     .        NINDX(JFILE), NDX, .false.)
         IF (OBSPG(N2,J2)) THEN
            D = PGCALC(MAG(J2), NDX(3), OBSX(N2,J2), 
     .           OBSY(N2,J2), OBSZ(N2,J2), COEFF(1,1,JFILE), DCDS)
         ELSE 
            D = TRFM(NINDX(JFILE), NDX, AM(JFILE), TIME(JFILE),
     .           OBSX(N2,J2), OBSY(N2,J2), R(JFILE), S(JFILE),
     .           NTERM(J2,JFILE), ITERM(1,1,J2,JFILE),
     .           COEFF(1,J2,JFILE), USE(1,J2,JFILE), .FALSE.)
         END IF
         IF (D .GT. 50.) GO TO 3340
         O2 = O2 - D
         O2 = 10.**(0.4*(25.-O2))
         E2 = 0.921034*O2*OBSE(N2,J2)
         O2 = (O2-FLUX(J2))/E2
C
         IF (O1 .GT. 50.) THEN
            D = OLD(J2)*ABS(O2)
            WSI = WSI + 0.5*(D-1.)
            WEIGHT = WEIGHT + 0.5
            if (var) write (10,*) 0., d, wsi, wt, 0., 0., o2, old(j2)
         ELSE
            D = OLD(J1)*OLD(J2)*O1*O2
            D = SIGN(SQRT(ABS(D)), D)
            WSI = WSI + D
            WEIGHT = WEIGHT + 1.
            if (var) write (10,*) old(j1)*o1, old(j2)*o2, wsi, wt,
     .            o1, old(j1), o2, old(j2)
         END IF
         GO TO 3350
C
 3340    IF (O1 .GT. 50.) GO TO 3350
         D = OLD(J1)*ABS(O1)
         WSI = WSI + 0.5*(D-1.)
         WEIGHT = WEIGHT + 0.5
         if (var) write (10,*) d, 0., wsi, weight, o1, old(j1), 0., 0.
 3350 CONTINUE
C
      IF (WEIGHT .GT. 0.) THEN
         WSI = WSI/WEIGHT
      ELSE
         GO TO 8000
      END IF
      VARY = 1.2533*KUR*WSI
C
C Is this star a variable?
C
 3355 CONTINUE
      if (var) call clfile (10, istat)
      VAR = .FALSE.
      IF ((MAG(1) .LT. VLIMIT(3)) .AND.
     .    (WEIGHT .GE. VLIMIT(2))) THEN
         NCHECK = NCHECK+1
         IF (VARY .LT. VLIMIT(1)) THEN
C
C Estimating m.e.1 of the photometry from NON-variable stars.
C
            DO JMAG=1,NMAG
               IF (NVALID(JMAG) .GT. 1) THEN
                  N=NRED(JMAG)+1
                  W(N,JMAG)=RME1(JMAG)**2/
     .                 (1.-0.702142/(NVALID(JMAG)-0.711815))
                  NRED(JMAG)=N
               END IF
            END DO
         ELSE
            VAR = .TRUE.
            NVARY = NVARY+1
         END IF
      END IF
CXYZ
C
C3359 CONTINUE
C
 8000 IF (RADEC) THEN
         XI = XXX / 206264.8062D0
         ETA = YYY / 206264.8062D0
         IF (TEST .EQ. 'Field Center:') THEN
            RHO = DSQRT(XI**2 + ETA**2)
            IF (RHO .GT. 1.D-12) THEN
               RHO = DTAN(RHO)/RHO
               XI = RHO*XI
               ETA = RHO*ETA
            END IF
         ELSE IF (TEST .EQ. 'field center:') THEN
            XI = DTAN(XI)                                        ! xyz
            ETA = DTAN(ETA)                                      ! xyz
         END IF
C
         I = 0
         DO JFILE=1,NFILE
            IF (VALID(JFILE) .AND. (JWHICH(JFILE) .GT. 0)) I = I+1
         END DO
         CALL RAD2AD (RA0, DC0, CD0, SD0, XI, ETA,
     .        IRH, IRM, RS, DSIGN, IDD, IDM, DS)
C
         IF (RS .GT. 59.995) THEN
            RS = RS-60.
            IRM = IRM+1
         END IF
         IF (IRM .GE. 60) THEN
            IRM = IRM-60
            IRH = IRH+1
         END IF
         IF (IRH .GE. 24) THEN
            IRH = IRH-24
         END IF
         WRITE (RA,691) IRH, IRM, RS
  691    FORMAT (I4.2, I3.2, F6.2)
         IF (RA(8:8) .EQ. '-') RA(8:8) = ' '
         IF (RA(9:9) .EQ. ' ') RA(9:9) = '0'
C
         IF (DS .LT. 0.) DS = 0.
         IF (DS .GT. 59.95) THEN
            DS = DS-60.
            IDM = IDM+1
         END IF
         IF (IDM .GE. 60) THEN
            IDM = IDM-60
            IDD = IDD+1
         END IF
         WRITE (DEC,692) DSIGN, IDD, IDM, DS
  692    FORMAT (1X, A1, I2.2, I3.2, F5.1)
         IF (DEC(8:8) .EQ. '-') DEC(8:8) = ' '
         IF (DEC(9:9) .EQ. ' ') DEC(9:9) = '0'
      END IF
C
      IF (SCALE) THEN
         XXX = OFFSET(3)*(XXX-OFFSET(1)-0.5)+0.5
         YYY = OFFSET(3)*(YYY-OFFSET(2)-0.5)+0.5
         TEXT(1) = RNDOFF (XXX, 9, 1)
         TEXT(2) = RNDOFF (YYY, 9, 1)
      ELSE
         TEXT(1) = RNDOFF (XXX, 9, 3)
         TEXT(2) = RNDOFF (YYY, 9, 3)
      END IF
      N = 2
      DO J=1,NMAG
         N = N+1
         TEXT(N) = RNDOFF(MAG(J), 7, 3)
         N = N+1
         TEXT(N) = RNDOFF(MAGE(J), 7, 4)
      END DO
      N = N+1
      TEXT(N) = RNDOFF(SUMCHI, 7, 3)
      N = N+1
      TEXT(N) = RNDOFF(SUMSHP, 7, 3)
      IF (NPAIRS .GE. 2) THEN
         N = N+1
         TEXT(N) = RNDOFF(WSI, 7, 3)
         N = N+1
         TEXT(N) = RNDOFF(KUR, 7, 3)
         N = N+1
         TEXT(N) = RNDOFF(VARY, 7, 3)
         N = N+1
         TEXT(N) = RNDOFF(WEIGHT, 7, 1)
         D = VARY*WEIGHT
         IF (D .GT. 1.0233E-10) THEN
            D = ALOG10(D)
         ELSE
            D = -9.999
         END IF
         N = N+1
         TEXT(N) = RNDOFF(D, 7, 4)
      END IF
C
      DO J=1,NMAG
         WRITE (TVALID(J),693) MIN0(9999, NVALID(J))
  693    FORMAT (I5)
      END DO
C
      TEST = ' '
      K = 1
      IF (NFORCE .GE. 0) THEN
         DO I=1,NFORCE
            IF (JD .EQ. IDF(I)) THEN
               IF (FNAME(I) .NE. ' ') THEN
                  TEST = SWITCH(FNAME(I), ' ')
                  K = LENGTH(TEST)
               ELSE
                  TEST = 'v'
                  K = 1
               END IF
               GO TO 696
            END IF
         END DO
      END IF
C
  696 IF (RADEC) THEN
         WRITE (3,698) JD, TEXT(1), TEXT(2), 
     .        (TEXT(J)(1:7), J=3,2*(NMAG+1)),
     .        (TVALID(J), J=1,NMAG), 
     .        (TEXT(J)(1:7), J=2*NMAG+3,N), RA, DEC, ' '//TEST(1:K+1)
      ELSE
         WRITE (3,698) JD, TEXT(1), TEXT(2), 
     .        (TEXT(J)(1:7), J=3,2*(NMAG+1)),
     .        (TVALID(J), J=1,NMAG), 
     .        (TEXT(J)(1:7), J=2*NMAG+3,N), ' '//TEST(1:K+1)
      END IF
  698 FORMAT (I7, 33A)
C
C-----------------------------------------------------------------------
C
      IF (VAR) THEN
         TEST = SQUEEZ(JD, K)
         WRITE (PFILE,695) TEST(1:K)
  695    FORMAT ('v', a, '.lcv')
         PFILE = SWITCH (CFILE, PFILE)
      END IF
C
      IF (NFORCE .GT. 0) THEN
         DO I=1,NFORCE
            IF (JD .EQ. IDF(I)) THEN
               IF (FNAME(I) .EQ. ' ') THEN
                  WRITE (6,6) idf(i)
    6             FORMAT (' forcing ', i7, '  ',a)
                  FETID = SQUEEZ(JD, K)
                  WRITE (PFILE,695) FETID(1:K)
                  PFILE = SWITCH (CFILE, PFILE)
               ELSE
                  WRITE (6,6) idf(i), fname(i)
                  PFILE = SWITCH(CFILE, FNAME(I))
               END IF
               VAR = .TRUE.
               GO TO 694
            END IF
         END DO
      END IF
C
  694 CONTINUE
      IF (VAR) THEN
         TEST = SWITCH(FNAME(I), ' ')
         K = LENGTH(TEST)
         IF (RADEC) THEN
            WRITE (9,698) JD, TEXT(1), TEXT(2), 
     .           (TEXT(J)(1:7), J=3,2*(NMAG+1)),
     .           (TVALID(J), J=1,NMAG), 
     .           (TEXT(J)(1:7), J=2*NMAG+3,N), RA, DEC,
     .           ' ', TEST(1:K+1)
         ELSE
            WRITE (9,698) JD, TEXT(1), TEXT(2), 
     .           (TEXT(J)(1:7), J=3,2*(NMAG+1)),
     .           (TVALID(J), J=1,NMAG), 
     .           (TEXT(J)(1:7), J=2*NMAG+3,N),
     .           ' ', TEST(1:K+1)
         END IF
         GO TO 3357
      END IF
      GO TO 7900
C
 3357 CALL DELFIL (10, PFILE, ISTAT)
      CALL OUTFIL (10, PFILE, ISTAT)
      N = 1
C
C Calculate all the calibrated magnitudes in all filters.
C
      DO JMAG=1,NMAG
         IF (NOBS(JMAG) .GT. 0) THEN
            DO I=1,NOBS(JMAG)
               JFILE = IFILE(I,JMAG)
               CALL MAG2NDX (MAG, MAGE, NMAG, MAGNDX(1,1,JFILE),
     .              NINDX(JFILE), NDX, .false.)
               IF (OBSPG(I,JMAG)) THEN
                  D = PGCALC(MAG(JMAG), NDX(3), OBSX(I,JMAG), 
     .                 OBSY(I,JMAG), OBSZ(I,JMAG), COEFF(1,1,JFILE), 
     .                 DCDS)
               ELSE 
                  D = TRFM(NINDX(JFILE), NDX, AM(JFILE), TIME(JFILE),
     .                 OBSX(I,JMAG), OBSY(I,JMAG), R(JFILE), S(JFILE),
     .                 NTERM(JMAG,JFILE), ITERM(1,1,JMAG,JFILE),
     .                 COEFF(1,JMAG,JFILE), USE(1,JMAG,JFILE), .false.)
               END IF
               OO(I,JMAG) = OBS(I,JMAG) - D
               SS(I,JMAG) = OBSE(I,JMAG)**2
               H(I,JMAG) = HJD(IFILE(I,JMAG))-DATE0
            END DO
         END IF
      END DO
      LOUNT = 0
      DO JMAG=1,NMAG
         KOUNT = 0
         DO I=1,NOBS(JMAG)
            JFILE = IFILE(I,JMAG)
            CALL MAG2NDX (MAG, MAGE, NMAG, MAGNDX(1,1,JFILE),
     .           NINDX(JFILE), NDX, .false.)
            K = INT(SNGL((HJD(JFILE)+HJD0)/1000.))
            DMAG = SNGL(HJD(JFILE)+HJD0-1.D3*K)
            IF (K .GT. 0) THEN
               TEXT(1) = RNDOFF(OBSX(I,JMAG), 9, 3)
               TEXT(2) = RNDOFF(OBSY(I,JMAG), 9, 3)
               TEXT(3) = RNDOFF(OBSCHI(I,JMAG), 9, 2)
               TEXT(4) = RNDOFF(OBSSHP(I,JMAG), 9, 3)
               WRITE (10,192) OO(I,JMAG), 
     .              SQRT(SS(I,JMAG)), JMAG, ISET(IFILE(I,JMAG)), K, 
     .              DMAG, (TEXT(L), L=1,4),  FILE(IFILE(I,JMAG))
  192          FORMAT (F7.3, F7.4, 2I3, I6, F9.4, 4A9, 2X, A40)
               KOUNT = KOUNT+1
            END IF
         END DO
         LOUNT = MAX0(LOUNT, KOUNT)
      END DO
      CALL CLFILE (10)
      IF (LOUNT .LE. 1) CALL DELFIL (10, PFILE, ISTAT)
      GO TO 7900
C
 8900 CALL CLFILE (1)
      WRITE (LINE,3) ' '
      CALL OVRWRT (LINE(1:10), 3)
      FETID = COMMAS(IMASTR, I)
      IF (NPAIRS .GE. 2) THEN
         TEXT(2) = COMMAS(NCHECK, J)
         TEXT(3) = COMMAS(NVARY, K)
         WRITE (6,650) FETID(1:I), TEXT(2)(1:J), TEXT(3)(1:K)
  650    FORMAT (/'Stars:  ', A, '    Checked:  ', A,
     .     '    Variables:  ', A)
      ELSE
         WRITE (6,651) TEXT(1)(1:I)
  651    FORMAT (/'Stars:  ', A)
      END IF
C
      DO JMAG=1,NMAG
         IF (NRED(JMAG) .GT. 3) THEN
            WRITE (6,699) JMAG, SQRT(PCTILE(W(1,JMAG), NRED(JMAG), 
     .          (NRED(JMAG)+1)/2)), NRED(JMAG)
  699       FORMAT (/' Median m.e.1: (', I1, ')', F8.3, I8, ' stars')
         END IF
      END DO
      CALL BYEBYE
      END!
C
C#######################################################################
C
      SUBROUTINE  PGREAD  (COEFF)
      REAL COEFF(*)
    1 READ (2,2,END=9000) I, X
    2 FORMAT (1X, I2, 3X, E15.6)
      COEFF(I) = X
      GO TO 1
 9000 RETURN
      END!
C
C#######################################################################
C
      REAL FUNCTION  PGCALC  (S, C, X, Y, R, A, DCDS)
C
C X = x, Y = y, R = radius**2, O = observed, C = color
C A = coefficients, NTERM is the number of terms,
C D is the derivative of the equation with respect to the terms.
C
      IMPLICIT NONE
      INTEGER MPTERM
      PARAMETER (MPTERM=10)
      REAL A(MPTERM)
C
      REAL S, O, C, X, Y, R, F, DCDS
C
C Basic equation:
C
C True = obs + a1 + a2*exp[-(obs+a4*obs**2+a5*obs**3)/a3] + a6*x + a7*y
C                 + a8*c + a9*m + a10*r**2
      O = S - 20.
      F = A(2)*EXP(-O*(O*(O*A(5)+A(4))+1.)/A(3))
      PGCALC = A(1) + F + A(6)*X + A(7)*Y + A(8)*C + A(9)*O + A(10)*R
      DCDS = A(9) - (O*(O*3.*A(5) + 2.*A(4)) + 1.)*F/A(3)
      RETURN
      END!
C
C########################################################################
C
      REAL FUNCTION  TRFM  (NINDX, NDX, Q, T, X, Y, R, S, NTERM, ITERM, 
     .     COEFF, USE, SHOW)
      IMPLICIT NONE
      INTEGER MAXTRM
      PARAMETER (MAXTRM=10)
      INTEGER JTERM, KINDX, NINDX, NTERM, ITERM(MAXTRM,*)
      DOUBLE PRECISION PRDCT, SUM, D
      REAL Q, T, X, Y, R, S, NDX(*), COEFF(*)
      LOGICAL USE(*)
      LOGICAL ZERO, SHOW
      SUM=0.D0
      if(show)PRINT*,'use: ',(use(jterm),jterm=1,nterm)
C
      DO 3130 JTERM=1,NTERM
      IF (USE(JTERM)) THEN
      if(show)PRINT*,'coeff: ',jterm,coeff(jterm)
C
C Compute the value of the term.
C
C First, figure in the airmass, time, position and azimuth terms.
C
         ZERO = .TRUE.
         PRDCT=1.D0
         IF (ITERM(JTERM,NINDX+1) .NE. 0) THEN
            PRDCT = PRDCT*DBLE(Q)**ITERM(JTERM,NINDX+1)
            ZERO = .FALSE.
         END IF
         IF (ITERM(JTERM,NINDX+2) .NE. 0) THEN
            PRDCT = PRDCT*DBLE(T)**ITERM(JTERM,NINDX+2)
            ZERO = .FALSE.
         END IF
         IF (ITERM(JTERM,NINDX+3) .NE. 0) THEN
            PRDCT = PRDCT*DBLE(X)**ITERM(JTERM,NINDX+3)
            ZERO = .FALSE.
         END IF
         IF (ITERM(JTERM,NINDX+4) .NE. 0) THEN
            PRDCT = PRDCT*DBLE(Y)**ITERM(JTERM,NINDX+4)
            ZERO = .FALSE.
         END IF
         IF (ITERM(JTERM,NINDX+5) .NE. 0) THEN
            PRDCT = PRDCT*DBLE(R)**ITERM(JTERM,NINDX+5)
            ZERO = .FALSE.
         END IF
         IF (ITERM(JTERM,NINDX+6) .NE. 0) THEN
            PRDCT = PRDCT*DBLE(S)**ITERM(JTERM,NINDX+6)
            ZERO = .FALSE.
         END IF
C
C Now include each of the standard photometric indices in the present
C term.
C
         DO 3120 KINDX=1,NINDX
            IF (ITERM(JTERM,KINDX) .NE. 0) THEN
               IF (NDX(KINDX) .LT. 50.) THEN
                  PRDCT =
     .                 PRDCT*DBLE(NDX(KINDX))**ITERM(JTERM,KINDX)
               ELSE
                  TRFM = 99.
                  RETURN
               END IF
               ZERO = .FALSE.
            END IF
 3120    CONTINUE                          ! End of loop over indices
C
C ZERO = .TRUE. means that this term contains only unity, i.e. this is the
C zero-point term, which need not be defined in the case of a CLOUDy
C reduction.
C
         IF ((.NOT. ZERO) .AND. (COEFF(JTERM) .LT. -1.E20)) THEN
C
C Coefficient needed, but unfortunately not defined.
C
            TRFM = 98.
            RETURN
         END IF
C
         D = DBLE(COEFF(JTERM))*PRDCT
         IF ((.NOT. ZERO) .AND. (DABS(D) .GT. 5.D0)) THEN
            TRFM = 97.
            RETURN
         END IF
         SUM = SUM+D
      if(show)PRINT*,'term: ',jterm, d, sum
      END IF
 3130 CONTINUE                             ! End of loop over terms
      TRFM=SNGL(SUM)
      if(show)read(5,*)
      RETURN
      END!
C
C#######################################################################
C
      SUBROUTINE  READTFM (NINDX, NMAG, MAGNDX, NTERM, 
     .     ITERM, COEFF, USE, COSMIC)
      
C
C=======================================================================
C
C Read in the transformation equations and any coefficient values.  
C
      IMPLICIT NONE
      INTEGER MAXTRM, MAXNDX, MAXMAG, NEXTRA
      PARAMETER (MAXTRM=10, MAXNDX=6, MAXMAG=6, NEXTRA=6)
      INTEGER ITERM(MAXTRM,MAXNDX+NEXTRA,*), NTERM(*)
      INTEGER MAGNDX(MAXMAG,*)
      INTEGER I, J, K, L, NCHR, NINDX, NMAG, ISTAT
      REAL COEFF(MAXTRM,*), COSMIC(*)
      LOGICAL FIX(MAXTRM,MAXMAG), USE(MAXTRM,*)
      CHARACTER LINE*133, SIGN*1, CHR*1
C
C Initialize the relevant arrays.
C
      DO I=1,MAXMAG
         COSMIC(I)=0.
         NTERM(I)=0
         DO J=1,MAXNDX
            MAGNDX(I,J)=0
         END DO
C
         DO J=1,MAXTRM
            COEFF(J,I)=-1.1E20
            DO K=1,MAXNDX+NEXTRA
               ITERM(J,K,I)=0
            END DO
         END DO
C
      END DO
C
C-----------------------------------------------------------------------
C
C Read input line by line.  The possible types of input are determined
C by the first non-blank character on the line.  (In the following,
C upper-case letters are to be taken literally, the lower-case 
C character "n" represents a numerical digit in the range 0-9, and
C the lower-case character "x" represents a letter from "A" to "F".
C
C The legal input lines will begin with:
C
C     (1) On = Mn + functions of (xn, In, X, T)
C     (2) In = functions of (Mn)
C     (3) Sn = numerical constant
C     (4) Zn = numerical constant character string
C     (5) xn = numerical constant
C
C These lines define, respectively:
C
C     (1) A transformation equation, defining the n-th observed 
C         magnitude as a function of the n-th standard magnitude
C         plus a transformation equation consisting of terms
C         involving products of the standard photometric indices,
C         and the airmass (X) and time (T) of the observation.
C
C     (2) The n-th standard photometric index, expressed as sums
C         and/or differences of the various standard magnitudes.
C
C     (3) The "nightly" standard error in the determination of the
C         n-th magnitude, over and above the photon and read noise.
C
C     (4) A photometric zero-point for CCD frame 'character string'
C         (cloudy-weather mode).
C
C     (5) One of the coefficients in one of the transformation
C         equations-- a real number.
C
C The numerical quantities defining all of these things will be
C stored in the arrays
C
C     (1) ITERM (term, index & X & T, magnitude)
C     (3) MAGNDX (magnitude, index)
C     (4) COSMIC (magnitude)
C     (5) ZERO (i, magnitude) = constant; FRAME (i, magnitude) = string
C     (6) COEFF (term, magnitude)
C
C respectively.  Note that in every case, the first non-blank
C character in the input will be a letter of the alphabet, the
C second one a numerical digit.  
C 
C ONE SPECIAL CASE:
C
C A line containing only the word CLOUD will be used to indicate that
C the user wants floating zero points.
C
C-----------------------------------------------------------------------
C
C Beginning of line-by-line loop.
C
 1000 CALL RDCHAR (2, LINE, NCHR, ISTAT)
      IF (ISTAT .GT. 0) GO TO 9000
      IF (NCHR .LT. 5) GO TO 9300
C
      IF (LINE(1:5) .EQ. 'CLOUD') THEN
         GO TO 1000
      END IF
C
C Place a colon at the end of the input to serve as a terminator.
C
      NCHR=NCHR+1
      LINE(NCHR:NCHR)=':'
C
C Get the first non-blank character from the input line, determining 
C from it whether a transformation equation ("O"), a magnitude ("M"), 
C an index ("I"), a standard error ("S") or a coefficient ("A" to "F") 
C is being defined.
C
      L=1
      SIGN=CHR(LINE,L)
C
      IF (SIGN .EQ. 'I') THEN
         CALL ILINE (LINE, L, NMAG, NINDX, MAGNDX)
C
C     ELSE IF (SIGN .EQ. 'M') THEN
C        CALL MLINE (LINE, L, NMAG, NINDX, NDXMAG)
C
      ELSE IF (SIGN .EQ. 'O') THEN
         CALL OLINE (LINE, L, NMAG, NINDX, NTERM, USE, ITERM, FIX)
C
      ELSE IF (SIGN .EQ. 'S') THEN
         CALL SLINE (LINE, L, COSMIC)
C
      ELSE IF (SIGN .EQ. 'Z') THEN
         GO TO 1000
C
      ELSE
         L=1
         CALL CLINE (LINE, L, COEFF, FIX)
      END IF
C
C This instruction has been confirmed to be grammatically correct and
C legible.  
C
      GO TO 1000
 9000 CONTINUE
C
C AT THIS POINT:
C
CC NDXMAG(INDEX,JMAG) contains the number of times that the INDEX-th 
C     standard index appears as an additive factor in the sum for
C     the JMAG-th magnitude.
C
C MAGNDX(JMAG,INDEX) contains the number of times that the JMAG-th
C     standard magnitude appears as an additive factor in the sum for
C     the INDEX-th standard index.
C
C ITERM(JTERM,KINDX,JMAG) contains the number of times that the
C     KINDX-th standard index appears as a multiplicative factor in the 
C     JTERM-th term of the transformation equation for the JMAG-th 
C     observational magnitude.
C
      RETURN
C
 9300 WRITE (6,7) LINE(1:NCHR)
    7 FORMAT (/' Incomplete line:'// 1X, A)
      CALL OOPS
      END!
C
C#######################################################################
C
      SUBROUTINE COEFFS (PERIOD, AMP, PHA)
      PARAMETER (MAXMAG=6, MAXFOU=5)
      REAL AMP(MAXFOU,MAXMAG), PHA(MAXFOU,MAXMAG)
      XX = ALOG10(PERIOD) - 1.
C
C Fourier amplitudes
C
      AMP(1,1) =  1.
      AMP(2,1) = -0.210 + (-0.091 - 0.333*XX)*XX
      AMP(3,1) = -0.102 + (-0.218 + 0.058*XX)*XX
      AMP(4,1) =  0.086 - 0.034*XX
      AMP(5,1) = -0.071 + 0.028*XX
      AMP(1,2) =  0.
      AMP(2,2) =  0.
      AMP(3,2) =  0.
      AMP(4,2) =  0.
      AMP(5,2) =  0.
      AMP(1,3) =  0.608 - 0.022*XX
      AMP(2,3) =  0.109 + 0.106*XX
      AMP(3,3) = -0.078 - 0.058*XX
      AMP(4,3) =  0.046
      AMP(5,3) =  0.
C
C Fourier phases
C
      PHA(1,1) =  0.
      PHA(1,2) =  0.
      PHA(1,3) =  -0.190 - 0.254*XX
      IF (XX .LT. -0.06) THEN
         PHA(2,1) =  1.843
         PHA(3,1) = -0.030
         PHA(2,2) =  0.
         PHA(3,2) =  0.
         PHA(2,3) = -1.340
         PHA(3,3) =  0.007
      ELSE
         PHA(2,1) =  1.180 + 0.506*XX
         PHA(3,1) = -2.147 + 3.218*XX
         PHA(2,2) =  0.
         PHA(3,2) =  0.
         PHA(2,3) =  -1.186 + 0.127*XX
         PHA(3,3) =  -2.946 + 4.823*XX
      END IF
      PHA(4,1) =  0.047
      PHA(5,1) =  1.164
      PHA(4,2) =  0.
      PHA(5,2) =  0.
      PHA(4,3) = -0.413
      PHA(5,3) =  0.
C
      RETURN
      END!
C
C#######################################################################
C
      SUBROUTINE CURVE (PHASE, AMP, PHA, MAG, DERIV, F)
      PARAMETER (MAXMAG=6, MAXFOU=5)
C
C      REAL ALP(*), BET(*), GAM(*), DEL(*), MAG(*), F(*)
C
      REAL AMP(MAXFOU,MAXMAG), PHA(MAXFOU,MAXMAG)
      REAL MAG(MAXMAG), F(MAXMAG)
      LOGICAL DERIV
      DATA TWOPI/6.283185/, NMAG/3/
C
      P = TWOPI*PHASE
C
      DO I=1,NMAG
         MAG(I) = 0.
         DO J=1,MAXFOU
            MAG(I) = MAG(I) + AMP(J,I)*COS(J*P+PHA(J,I))
         END DO
      END DO
C
      IF (.NOT. DERIV) RETURN
      DO I=1,NMAG
         F(I) = 0.
         DO J=1,MAXFOU
            F(I) = F(I) - J*AMP(J,I)*SIN(J*P+PHA(J,I))
         END DO
      END DO
C
C      P1 = TWOPI*PHASE
C      Q1 = P1+DEL(1)
CC
C      P2 = 2.*P1
C      Q2 = P2+DEL(2)
C      P2 = P2+GAM(2)
CC
C      P3 = 3.*P1
C      Q3 = P3+DEL(3)
C      P3 = P3+GAM(3)
CC
C      P4 = 4.*P1
C      Q4 = P4+DEL(4)
C      P4 = P4+GAM(4)
CC
C      P5 = 5.*P1+GAM(5)
CC
C      MAG(1) = COS(P1) + ALP(2)*COS(P2) + ALP(3)*COS(P3) + 
C     .    ALP(4)*COS(P4) + ALP(5)*COS(P5)
C      MAG(2) = BET(1)*COS(Q1) + BET(2)*COS(Q2) + BET(3)*COS(Q3) +
C     .    BET(4)*COS(Q4)
C
C      IF (DERIV) THEN
C         F(1) = SIN(P1) + 2.*ALP(2)*SIN(P2) + 3.*ALP(3)*SIN(P3) +
C     .       4.*ALP(4)*SIN(P4) + 5.*ALP(5)*SIN(P5)
C         F(2) = BET(1)*SIN(Q1) + 2.*BET(2)*SIN(Q2) + 
C     .       3.*BET(3)*SIN(Q3) + 4.*BET(4)*SIN(Q4)
C      END IF
CC
       RETURN
      END!
C
C#######################################################################
C
C Convert magnitudes to appropriate indices.
C
      SUBROUTINE  MAG2NDX  (MAG, MAGE, NMAG, MAGNDX, NINDX, NDX, SHOW)
C
      PARAMETER (MAXMAG=6, MAXNDX=6)
      REAL MAG(MAXMAG), MAGE(MAXMAG), NDX(MAXNDX)
      INTEGER MAGNDX(MAXMAG,MAXNDX)
      LOGICAL SHOW
C
      DO 2840 J=1,NINDX
         NDX(J) = 0.
         DO I=1,NMAG
            IF (MAGNDX(I,J) .NE. 0) THEN
               IF (MAGE(I) .LT. 25.) THEN
                  NDX(J) = NDX(J) + MAGNDX(I,J)*MAG(I)
               ELSE
                  NDX(J) = 99.999
                  GO TO 2840
               END IF
            END IF
         END DO
 2840 CONTINUE
C
      RETURN
      END!
