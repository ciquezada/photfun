C
C=======================================================================
C
C A program to average together instrumental CCD photometry 
C transformed to a standard system.
C This program is untidy and complicated, and is certainly not in its
C final form.
C
C             Official DAO version:  2004 September 14
C
C=======================================================================
C
      INTEGER MAXBUF
      PARAMETER (MAXBUF = 40 000 000)
      PARAMETER (MAXMAG=6, MAXNDX=6, MAXTRM=10, NEXTRA=6)
      PARAMETER (MAXSTR=400 000, MFRAME=1000, MXFILE=5000, MAXOBS=5000)
C
C MAXMAG is the maximum number of instrumental CCD bandpasses that can
C        handled.
C MAXNDX is the maximum number of photometric indices on the standard
C        photometric system that can be handled.
C MAXTRM is the maximum number of coefficients and terms that can be
C        included in each transformation equation.
C NEXTRA is the number of known observational quantities that can be
C        included in the transformation equations, besides the standard
C        photometric indices.  NEXTRA = 2: airmass (X), and time of 
C        observation (T).
C MAXSTR is the longest star-list that can be maintained.
C MFRAME is the largest number of CCD frames in a given photometric
C        bandpass that may be included for a given night.
C MXFILE is the largest number of different observation files that can 
C        be included in a single reduction.
C MAXOBS is the largest number of observations for a single 
C        star that can be held in memory at once.
C
      CHARACTER FILE(MXFILE)*40, TEXT(MAXOBS)*180, IDMAST(MAXSTR)*12
      CHARACTER FRAME(MFRAME,MAXMAG,MXFILE)*40, FRMID(MAXOBS)*40
      CHARACTER MAGLBL(MAXNDX)*6
      CHARACTER SMAG(MAXMAG)*7, SSIG(MAXMAG)*7, RNDOFF*7
      CHARACTER N1(MAXMAG)*5, N2(MAXMAG)*5
      CHARACTER INPUT*132, LINE*45, STARID*12, EX*7, CASE*4
      CHARACTER CFILE*40, LFILE*40, SWITCH*40, EXTEND*40, F*40,
     .     CURRNT*40, FORMER*40
      CHARACTER ANSWER*1, BELL*1, DISPLAY*1
      REAL SUMM(MAXMAG), SUMD(MAXMAG), SUMDW(MAXMAG), SUMW(MAXMAG)
      REAL SUMN(MAXMAG)
      REAL ME1(MAXMAG), WME1(MAXMAG), STFLUX(MAXMAG)
      REAL STMAG(MAXMAG), EXTRA(MAXMAG), COSMIC(MAXMAG,MXFILE)
      REAL CALNDX(MAXNDX), ZTERM(MAXMAG,MXFILE) ! , ERRNDX(MAXNDX)
      REAL COEFF(MAXTRM,MAXMAG,MXFILE) 
      REAL ZERO(MFRAME,MAXMAG,MXFILE), SZERO(MFRAME,MAXMAG,MXFILE)
      REAL OBSMAG(MAXOBS), OBSERR(MAXOBS), Q(MAXOBS), T(MAXOBS),
     .     X(MAXOBS), Y(MAXOBS), R(MAXOBS), S(MAXOBS),
     .     W(MAXOBS), CALMAG(MAXOBS), CLAMP(MAXMAG), OLDDEL(MAXMAG)
      INTEGER LFRAME, LOBS
      INTEGER NMAG, SBNRY
      INTEGER ITERM(MAXTRM,MAXNDX+NEXTRA,MAXMAG,MXFILE)
C     INTEGER ISTMAG(MAXNDX,MAXMAG)
      INTEGER MAGNDX(MAXMAG,MAXNDX,MXFILE), NINDX(MXFILE)
      INTEGER IMAG(MAXOBS), IFILE(MAXOBS), NOBS(MAXSTR,MXFILE)
      INTEGER N(MAXMAG), NTERM(MAXMAG,MXFILE), NME1(MAXMAG)
      INTEGER NCLR(MAXMAG)
      INTEGER NFRAME(MAXMAG,MXFILE)
      LOGICAL USE(MAXTRM,MAXMAG,MXFILE), DEFNDX(MAXNDX), CLOUD(MXFILE)
C
C-----------------------------------------------------------------------
C
C Define a big buffer to hold as much of the input data in memory as
C possible.
C
      CHARACTER FBUF(MAXBUF)*40
      REAL OBUF(MAXBUF), EBUF(MAXBUF), QBUF(MAXBUF), TBUF(MAXBUF),
     .     XBUF(MAXBUF), YBUF(MAXBUF), RBUF(MAXBUF), SBUF(MAXBUF)
      REAL G(7), COLD(7), WARM(7)
      INTEGER FIRST(MAXSTR), FINAL(MAXSTR), NEXT(MAXBUF)
      INTEGER IFBUF(MAXBUF), MBUF(MAXBUF), NSTAR(MXFILE)
      LOGICAL BUF, HST, LAST, SHOW
C
      character*1 flag
      REAL LIMIT, MAGLIM, SIGMAX
C
C-----------------------------------------------------------------------
C
      DATA LFRAME/0/, LOBS/0/, SIGMAX / 5 /
c     DATA LINE /' ---------------'/, NACRSS / 3 /, NWIDE / 38 /
      DATA LINE /' ---------------'/, NACRSS / 3 /, NWIDE / 42 /
      DATA NEXT /MAXBUF*0/
      DATA QZERO/1.00/
      DATA HST /.false./, DAMP /1./, SHOW/.false./
      BELL=CHAR(7)
C
C=======================================================================
C
C SECTION 1(a)
C
C Get the name of the standard-star library, and read the library in.
C
      IF (HST) THEN
         CALL STUPID ('Set for HST reductions!')
         CFILE = 'unix:coldwarm.dat'
         CALL INFILE (1, CFILE, ISTAT)
         read (1,*) (cold(i), i=1,6)
         read (1,*) (warm(i), i=1,6)
         CALL CLFILE (1)
      END IF
C
      CALL FABORT
      NWM1 = NWIDE-1
      NWM19 = NWIDE-19
      CALL TBLANK
      LFILE=' '
      CALL GETNAM ('File with standard-star library:', LFILE)
      IF (LFILE .EQ. 'END-OF-FILE') CALL BYEBYE
      LFILE = EXTEND(LFILE, CASE('lib'))
      CALL INFILE (2, LFILE, ISTAT)
      IF (ISTAT .NE. 0) GO TO 8000
C
C Read in the number of indices and filter, and the index labels.
C
      READ (2,210) NMAG, (MAGLBL(I), I=1,NMAG)
  210 FORMAT (I2, 19X, 6(5X, A6, 3X))
C
C Read star ID's.
C
      I=0
 1020 I=I+1
      READ (2, 211, END=1030, ERR=8100) IDMAST(I)
  211 FORMAT(1X, A12)
      GO TO 1020
C
 1030 IF (I .GT. MAXSTR) THEN
         CALL STUPID ('Too many stars!')
         CALL CLFILE (2)
         CALL OOPS
      END IF
C
      NSTDP1 = I
      NSTD = I-1
      CALL CLFILE (2)
C
C Alphabetize the star list
C
      CALL SQUICK (IDMAST, NSTD, NOBS)
      CFILE=SWITCH(LFILE, CASE('.net'))
      CALL GETNAM ('Output file name:', CFILE)
      IF ((CFILE .EQ. 'END-OF-FILE') .OR.
     .     (CFILE .EQ. 'EXIT')) CALL BYEBYE
      CALL OUTFIL (3, CFILE, ISTAT)
      CFILE=SWITCH(CFILE, CASE('.ave'))
      CALL OUTFIL (1, CFILE, ISTAT)
C
C=======================================================================
C
C SECTION 1(b)
C
C Get the input file names and read in all the star names, keeping
C track of how many times each star appears in each file.
C
C Obtain the name of the file with the observational data, and open it.
C
      NTOT = NSTD
      JFILE = 0
      IBUF = 0
 1000 JFILE=JFILE+1
C
      IF (JFILE .EQ. 1) THEN
         FILE(JFILE)=' '
      ELSE
         FILE(JFILE)='PROCEED'
      END IF
C
 1010 CALL GETNAM ('File with observations:', FILE(JFILE))
      IF (FILE(JFILE) .EQ. 'GIVE UP') CALL OOPS
      IF ((FILE(JFILE) .EQ. 'END-OF-FILE') .OR. 
     .     (FILE(JFILE) .EQ. 'PROCEED')) GO TO 2000
      if (jfile .gt. mxfile) then
         call stupid ('Too many input files!')
         call oops
      end if
      F = FILE(JFILE)
      DO I=2,30
         IF (F(I:I) .EQ. ';') F = F(1:I-1)//' '
      END DO
      F = EXTEND(F, CASE('obs'))
C
C Make sure the file is there, count the stars in it, then get the file
C with the transformation equations. 
C
      CALL INFILE (2, F, ISTAT)
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Error opening input file '//F)
         FILE(JFILE) = 'GIVE UP'
         GO TO 1010
      END IF
C
      READ (2,*)
      READ (2,*)                          !Midnight
      READ (2,*)                          !Labels
      NSTAR(JFILE) = 0
 1080 CALL RDCHAR (2, INPUT, I, ISTAT)
      IF (ISTAT .GT. 0) GO TO 1090
      IF (INPUT(4:14) .EQ. 'MAGNITUDES:') THEN
         READ (2,*)
         READ (2,*)
         GO TO 1080
      END IF
      READ (INPUT,213,END=1090) STARID, JMAG
      IF (JMAG .GT. NMAG) GO TO 1080
      IF (STARID .NE. ' ') NSTAR(JFILE) = NSTAR(JFILE)+1
      GO TO 1080
 1090 CALL CLFILE (2)
      IBUF = IBUF + NSTAR(JFILE)
      CALL TBLANK
      WRITE (6,212) NSTAR(JFILE), ' observations read from '//
     .     FILE(JFILE)(1:LENGTH(FILE(JFILE)))
  212 FORMAT (I8, A)
      WRITE (6,212) IBUF, ' total'
      CALL TBLANK
C
      CFILE=SWITCH(F, CASE('.clb'))
C     CALL GETNAM ('File with calibrations:', CFILE)
C     CFILE = EXTEND(CFILE, CASE('clb'))
      CALL INFILE (2, CFILE, ISTAT)
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Error opening input file '//CFILE)
         FILE(JFILE) = 'GIVE UP'
         GO TO 1010
      END IF
      NINDX(JFILE) = 0
      CALL RDTFM (NINDX(JFILE), NMAG, MAGNDX(1,1,JFILE), 
     .     NTERM(1,JFILE), ITERM(1,1,1,JFILE), 
     .     USE(1,1,JFILE), COEFF(1,1,JFILE), 
     .     COSMIC(1,JFILE), CLOUD(JFILE), NFRAME(1,JFILE), 
     .     FRAME(1,1,JFILE), ZERO(1,1,JFILE), SZERO(1,1,JFILE))
      CALL CLFILE (2)
C
C If the night is cloudy, make sure there aren't too many individual frames
C for the data array.
C
      IF (CLOUD(JFILE)) THEN
         DO JMAG=1,NMAG
            IF (NFRAME(JMAG,JFILE) .GT. MFRAME) THEN
               CALL STUPID ('Too many frames in '//MAGLBL(JMAG))
            END IF
            IF (NFRAME(JMAG,JFILE) .GT. LFRAME) LFRAME =
     .           NFRAME(JMAG,JFILE)
         END DO
         IF (LFRAME .GT. MFRAME) CALL OOPS
         GO TO 1000
      END IF
C
C For each transformation equation, identify the zero-point term (if
C any).  That is the coefficient of the term that does not contain
C any photometric index, Q, T, X, Y, R, or S.
C
      DO 1970 JMAG=1,NMAG
         DO 1950 JTERM=1,NTERM(JMAG,JFILE)
            DO I=1,NINDX(JFILE)+NEXTRA
               IF (ITERM(JTERM,I,JMAG,JFILE) .NE. 0) GO TO 1950
            END DO
C
C This term contains no indices, nor Q, T, X, Y, R, or S.  If
C COEFF < -1.E38, this term is not used.
C
            IF (COEFF(JTERM,JMAG,JFILE) .LT. -1.E38) GO TO 1950
            IF (ABS(COEFF(JTERM,JMAG,JFILE)) .GT. 1.E-6) THEN
               ZTERM(JMAG,JFILE)=COEFF(JTERM,JMAG,JFILE)
               GO TO 1970
            END IF
 1950    CONTINUE
C
C Every term contains an index, or Q, T, X, Y, R, or S.
C
         ZTERM(JMAG,JFILE)=0.
 1970 CONTINUE
      GO TO 1000
C
 2000 NFILE=JFILE-1
C
C Initialize the star-count accumulator.
C
      DO I=1,NTOT
         FIRST(I) = 0
      END DO
      CALL TBLANK
      CALL GETYN ('Include non-library stars?', ANSWER)
      CALL GETYN ('Display on terminal?', DISPLAY)
C
      CALL TBLANK
      WRITE (INPUT,1) NTOT, LFILE
    1 FORMAT (I11, 7X, A)
      CALL OVRWRT (INPUT(1:48), 2)
C
      IBUF = 0
      BUF = .TRUE.
      JSTART = 0
      DO 2900 JFILE=1,NFILE
      FILE(JFILE) = EXTEND(FILE(JFILE), 'obs')
      CALL INFILE (2, FILE(JFILE), ISTAT)
      IF (HST) THEN
         IF (FILE(JFILE)(1:4) .eq. 'warm') then
            do i=1,6
               g(i) = warm(i)
            end do
            g(7) =  0.
         ELSE
            do i=1,6
               g(i) = cold(i)
            end do
            g(7) =  0.
         END IF
      END IF
      IF (IBUF+NSTAR(JFILE) .GT. MAXBUF) THEN
         CALL STUPID ('Exceeded buffer size!')
         WRITE (6,1) IBUF+NSTAR(JFILE), FILE(JFILE)
         CALL CLFILE (2)
         CALL OOPS
         IF (BUF) THEN
            JSTART = JFILE
            BUF = .FALSE.
         END IF
         DO I=1,NTOT
            NOBS(I,JFILE)=0
         END DO
      END IF
C
C Read in the number of observational magnitudes.
C
      READ (2,*)                        ! Skip over first header line
      READ (2,*) THRS, TMIN
      TMID = THRS + TMIN/60.
      READ (2,*)                        ! Skip over third header line
C
C Read in the star-names, one by one, looking for each one in the
C star list.
C
 2120 CONTINUE
      IF (BUF) THEN 
 2121    CALL RDCHAR (2, INPUT, I, ISTAT)
         IF (ISTAT .GT. 0) GO TO 2890
         IF (ISTAT .LT. 0) GO TO 2121
         IF (INPUT(4:14) .EQ. 'MAGNITUDES:') THEN
            READ (2,*)
            READ (2,*)
            GO TO 2121
         END IF
         READ (INPUT,213,IOSTAT=ISTAT) STARID, JMAG, THRS, TMIN, 
     .        QQ, AZ, TINT, O, E, ZZ, XX, YY, SKY, F
  213    FORMAT (1X, A12, I3, F5.0, F3.0, F8.3, F8.1, F9.1, 
     .        F9.3, F8.4, 4F9.2, 2X, A30)
         IF (ISTAT .NE. 0) THEN
            SKY = 0.
            READ (INPUT,214,IOSTAT=ISTAT) STARID, JMAG, THRS, TMIN, 
     .           QQ, AZ, TINT, O, E, ZZ, XX, YY, F
  214       FORMAT (1X, A12, I3, F5.0, F3.0, F8.3, F8.1, F9.1, 
     .           F9.3, F8.4, 3F9.2, 2X, A30)
            IF (ISTAT .NE. 0) THEN
               AZ = 0.
               READ (INPUT,215,ERR=2121) STARID, JMAG, THRS, TMIN, 
     .              QQ, TINT, O, E, ZZ, XX, YY, F
  215          FORMAT (1X, A12, I3, F5.0, F3.0, F8.3, F9.1, 
     .              F9.3, F8.4, 3F9.2, 2X, A30)
            END IF
         END IF
         IF (E .GT. SIGMAX) GO TO 2120
         IF (JMAG .GT. NMAG) GO TO 2120
         O = O+ZZ
         IF (HST) THEN
            CURRNT = SWITCH(F, ' ')
            IF (CURRNT .NE. FORMER) THEN
               CALL INFILE (9, F, ISTAT)
               READ (9,*)
               READ (9,3) GAIN
    3          FORMAT (45X, F8.2)
               CALL CLFILE (9)
               FORMER = CURRNT
            END IF
C
            DO I=2,30
               IF (F(I:I) .EQ. ':') GO TO 2122
            END DO
            GO TO 2124
 2122       I = I-1
            IF (F(I:I) .EQ. 'p') THEN
               LIMIT = 100.
               MAGLIM = 12.6
            ELSE IF (F(I:I) .EQ. 'w') THEN
               LIMIT = 75.
               MAGLIM = 13.
            ELSE
               GO TO 2124
            END IF
            IF ((XX .LT. LIMIT) .OR. (YY.LT. LIMIT) .OR.
     .           (O .LT. MAGLIM)) THEN
               GO TO 2120
            END IF
C
C Compute and apply ramp corrections.
C
Cc          XX = (XX - 425.)/375.
Cc          YY = (YY - 425.)/375.
            xx = xx/400.
            yy = yy/400.
            SKY = GAIN*SKY
            SKYLOG = ALOG10(SQRT(DAMP + AMAX1(0., SKY)**2)
     .           / 1.E1)
            CTS = GAIN*10.**(0.4*(25.-O)) / 1.E4
            CTS = ALOG10(CTS)
            YCTE = G(1) + G(5)*SKYLOG + G(3)*CTS
            YCTE = EXP(YCTE)
            XCTE = G(2) + G(6)*SKYLOG + G(4)*CTS
            XCTE = EXP(XCTE)
            CORR = 1.D0 + 0.01*YCTE*(G(7)+YY) +
     .           0.01*XCTE*XX
            CORR = -1.085736*ALOG(CORR)
            O = O + CORR
         END IF
      ELSE
         READ (INPUT,213,IOSTAT=ISTAT) STARID, JMAG, THRS, TMIN, 
     .        QQ, AZ, TINT, O, E, ZZ, XX, YY, SKY, F
         IF (E .GT. SIGMAX) GO TO 2120
         IF (JMAG .GT. NMAG) GO TO 2120
      END IF
      F = SWITCH(F, ' ')
C
C Find this star in the master star-list.  If it is not there, append it
C to the end of the list.  Note that henceforth there will be 
C two numbers referring to the number of stars in the star list:
C 
C   NSTD is the total number of stars read in from the standard-star
C        library file.  These stars will have known photometric indices
C        on the standard system.
C
C   NTOT is the total number of different stars that have actually been
C        observed.  Stars which have been found in the observational 
C        data file but which were not in the standard-star library file
C        will have been appended onto the end of the list of standard
C        stars.  This is done for two reasons:  (1) The message 'Star 
C        not in standard list' will be typed out only the first time
C        each new star is encountered; and (2) photometric indices will
C        be transformed from the instrumental to the standard system
C        will be computed and printed out for these stars, even though
C        they could not be used to compute the transformations in the
C        first place.
C
 2124 I = SBNRY (IDMAST, NSTD, STARID)
      IF (I .GT. 0) GO TO 2160
C
C Star not found in the standard list.  If we are keeping
C non-library stars, check among the stars already added to the list.
C If it's not there, either, add it to the list.  
C
      IF (ANSWER .EQ. 'Y') THEN
         DO I=NSTDP1,NTOT
            IF (IDMAST(I) .EQ. STARID) GO TO 2160
         END DO
         NTOT=NTOT+1
         IF (NTOT .GT. MAXSTR) THEN
            CALL STUPID ('Too many different stars!')
            CALL CLFILE (2)
            CALL OOPS
         END IF
         IDMAST(NTOT)=STARID
         NOBS(NTOT,JFILE)=1
      ELSE
         GO TO 2120
      END IF
C
C Star found, or newly added to the list.
C
 2160 CONTINUE
      IF (BUF) THEN
C
C It will fit into the memory buffer.
C
         E = E**2
         IF (CLOUD(JFILE)) THEN
            DO JFRAME = 1,NFRAME(JMAG,JFILE)
               IF (F .EQ. FRAME(JFRAME,JMAG,JFILE)) GO TO 2163
            END DO
C
C This was a cloudy-weather reduction, but this frame doesn't have a
C zero-point --- probably there were no standards in it.  In any case,
C this star is worthless to us.  Might as well just read in the next
C star and forget this one.
C 
            GO TO 2120
C
C Correct the observed magnitude for the frame's zero-point.
C
 2163       O = O - ZERO(JFRAME,JMAG,JFILE)
            E = E + SZERO(JFRAME,JMAG,JFILE)**2
         END IF
C
C Now insert the star into the memory buffer.
C
         IBUF = IBUF+1
         IFBUF(IBUF) = JFILE
         MBUF(IBUF) = JMAG
         QBUF(IBUF) = QQ - QZERO
         TBUF(IBUF) = THRS + TMIN/60 - TMID
         IF (TBUF(IBUF) .GT. 12.) TBUF(IBUF) = TBUF(IBUF)-24.
         IF (TBUF(IBUF) .LT. -12.) TBUF(IBUF) = TBUF(IBUF)+24.
         XBUF(IBUF) = XX * 0.001
         YBUF(IBUF) = YY * 0.001
         AZ = AZ / 57.29578
         RBUF(IBUF) = QBUF(IBUF) * COS(AZ)
         SBUF(IBUF) = QBUF(IBUF) * SIN(AZ)
         OBUF(IBUF) = O + 2.5*ALOG10(TINT)
         EBUF(IBUF) = SQRT(1.E-6+E+COSMIC(JMAG,JFILE))
c        DO K=18,1,-1
c           IF (F(K:K) .EQ. ':') GO TO 2174
c        END DO
c        K = 0
c2174    FBUF(IBUF) = F(K+1:30)
         FBUF(IBUF) = F
         IF (FIRST(I) .EQ. 0) THEN
            FIRST(I) = IBUF
            FINAL(I) = IBUF
         ELSE
            K = FINAL(I)
            NEXT(K) = IBUF
            FINAL(I) = IBUF
         END IF
      ELSE
C
C The observation won't fit into the buffer.  Just take note of the fact 
C that this star is contained in this file, and leave it on disk for later.
C
         NOBS(I,JFILE)=NOBS(I,JFILE)+1
      END IF
      GO TO 2120
C
 2890 WRITE (INPUT,2) NTOT, JFILE, FILE(JFILE)(1:LENGTH(FILE(JFILE)))
    2 FORMAT (I11, I8, 2X, A)
      CALL OVRWRT (INPUT(1:51), 2)
      CALL CLFILE (2)
 2900 CONTINUE
      CALL TBLANK
C
      DO JMAG=1,NMAG
         NME1(JMAG) = 0
      END DO
C
C=======================================================================
C
C Beginning of big loop over stars
C
      WRITE (3,310) NMAG, (MAGLBL(I), I=1,NMAG)
  310 FORMAT (I2, ' FILTERS:', 10X, 6(5X, A6, 3X))
      DO 3999 JSTAR=1,NTOT
C
C Accumulate all observations of this star: first, from the buffer in
C memory; then from the remaining input files that wouldn't fit.
C
      M = 0
      IF (FIRST(JSTAR) .GT. 0) THEN
         IBUF = FIRST(JSTAR)
 3020    M = M+1
         if (m .gt. maxobs) then
            call stupid ('Too many observations!')
            PRINT*,idmast(jstar),maxobs
            call clfile (3)
            call oops
         end if
         IMAG(M) = MBUF(IBUF)
         IFILE(M) = IFBUF(IBUF)
         Q(M) = QBUF(IBUF)
         T(M) = TBUF(IBUF)
         X(M) = XBUF(IBUF)
         Y(M) = YBUF(IBUF)
         R(M) = RBUF(IBUF)
         S(M) = SBUF(IBUF)
         FRMID(M) = FBUF(IBUF)
         OBSMAG(M) = OBUF(IBUF)
         OBSERR(M) = EBUF(IBUF)
         IBUF = NEXT(IBUF)
         IF (IBUF .GT. 0) GO TO 3020
      END IF
      if (m .gt. lobs) lobs = m
      IF (JSTART .LE. 0) GO TO 3100
      DO 3099 JFILE=JSTART,NFILE
      IF (NOBS(JSTAR,JFILE) .LE. 0) GO TO 3099
      CALL INFILE (2, FILE(JFILE), ISTAT)
      IF (ISTAT .NE. 0) GO TO 8000
      READ (2,*)
      READ (2,*)
      READ (2,*)
      L=0
C
 3050 CALL RDCHAR (2, INPUT, I, ISTAT)
      IF (ISTAT .GT. 0) GO TO 8200
      IF (ISTAT .LT. 0) GO TO 3050
      IF (INPUT(4:14) .EQ. 'MAGNITUDES:') THEN
         READ (2,*)
         READ (2,*)
         GO TO 3050
      END IF
      READ (INPUT,213,IOSTAT=ISTAT) STARID, JMAG, THRS, TMIN, 
     .     QQ, AZ, TINT, O, E, ZZ, XX, YY, SKY, F
      IF (ISTAT .NE. 0) THEN
         SKY = 0.
         READ (INPUT,214,IOSTAT=ISTAT) STARID, JMAG, THRS, TMIN, 
     .        QQ, AZ, TINT, O, E, ZZ, XX, YY, F
         IF (ISTAT .NE. 0) THEN
            AZ = 0.
            READ (INPUT,215,ERR=3050) STARID, JMAG, THRS, TMIN, 
     .        QQ, TINT, O, E, ZZ, XX, YY, F
         END IF
      END IF
      O = O+ZZ
      F = SWITCH(F, ' ')
      IF (HST) THEN
         CURRNT = F
         IF (CURRNT .NE. FORMER) THEN
            CALL INFILE (9, F, ISTAT)
            READ (9,*)
            READ (9,3) GAIN
            CALL CLFILE (9)
            FORMER = CURRNT
         END IF
C
         DO I=2,30
            IF (F(I:I) .EQ. ':') GO TO 3052
         END DO
         GO TO 3054
 3052    I = I-1
         IF (F(I:I) .EQ. 'p') THEN
            LIMIT = 100.
         ELSE IF (F(I:I) .EQ. 'w') THEN
            LIMIT = 75.
         ELSE
            GO TO 3054
         END IF
      WRITE (6,697) (G(I), I=1,7)
  697 FORMAT (/7F11.5/)
         IF ((XX .LT. LIMIT) .OR. (YY.LT. LIMIT)) GO TO 3050
C
C Compute and apply ramp corrections.
C
Cc          XX = (XX - 425.)/375.
Cc          YY = (YY - 425.)/375.
            xx = xx/400.
            yy = yy/400.
            SKY = GAIN*SKY
            SKYLOG = ALOG10(SQRT(DAMP + AMAX1(0., SKY)**2)
     .           / 1.E1)
            CTS = GAIN*10.**(0.4*(25.-O)) / 1.E4
            CTS = ALOG10(CTS)
            YCTE = G(1) + G(5)*SKYLOG + G(3)*CTS
            YCTE = EXP(YCTE)
            XCTE = G(2) + G(6)*SKYLOG + G(4)*CTS
            XCTE = EXP(XCTE)
            CORR = 1.D0 + 0.01*YCTE*(G(7)+YY) +
     .           0.01*XCTE*XX
            O = O - 1.085736205D0*ALOG(CORR)
      END IF
C  
 3054 IF (JMAG .GT. NMAG) GO TO 3050
      IF (STARID .EQ. IDMAST(JSTAR)) THEN
         IF (CLOUD(JFILE)) THEN
C
C For a cloudy-weather reduction, get the zero-point for the frame
C of this observation.
C
            DO JFRAME=1,NFRAME(JMAG,JFILE)
               IF (F .EQ. FRAME(JFRAME,JMAG,JFILE)) GO TO 3055
            END DO
C
C This frame doesn't have a zero-point-- probably there were no 
C standards in it.  In any case it is useless to us.
C
            L=L+1
            IF (L .LT. NOBS(JSTAR,JFILE)) THEN
               GO TO 3050
            ELSE
               GO TO 3057
            END IF
         END IF
 3055    M=M+1
         IMAG(M)=JMAG - 10*int(jmag/10)
         IFILE(M)=JFILE
         Q(M) = QQ - QZERO
         T(M)=THRS+TMIN/60.-TMID
         IF (T(M) .GT. 12.) T(M) = T(M) - 24.
         IF (T(M) .LT. -12.) T(M) = T(M) + 24.
         X(M) = XX * 0.001
         Y(M) = YY * 0.001
         AZ = AZ / 57.29578
         R(M) = Q(M) * COS(AZ)
         S(M) = Q(M) * SIN(AZ)
c        DO K=18,1,-1
c           IF (F(K:K) .EQ. ':') GO TO 3056
c        END DO
c        K = 0
c3056    FRMID(M)=F(K+1:30)
         OBSMAG(M)=O+2.5*ALOG10(TINT)
         IF (CLOUD(JFILE)) OBSMAG(M)=OBSMAG(M)-ZERO(JFRAME,JMAG,JFILE)
         OBSERR(M)=SQRT(1.E-6 + E**2 +
     .        SZERO(JFRAME,JMAG,JFILE)**2 + COSMIC(JMAG,JFILE))
         L=L+1
         IF (L .LT. NOBS(JSTAR,JFILE)) GO TO 3050
      ELSE
         GO TO 3050
      END IF
 3057 CALL CLFILE (2)
 3099 CONTINUE
C
 3100 CONTINUE
      IF (M .LE. 0) GO TO 3999
C
C=======================================================================
C
C REVERSE THE TRANSFORMATION, using the observed magnitudes for the
C star to predict its indices on the standard system.
C
C In this section of the program, the following arrays will contain
C the following information for each star, as it is being reduced:
C
C STMAG(JMAG)    the COMPUTED magnitudes on the standard system,
C                 obtained from the OBSERVED magnitudes by reversing
C                 the transformation equations.
C
C CALNDX(INDEX)  the COMPUTED photometric indices on the standard
C                 system, obtained by taking the appropriate sums and
C                 differences of STMAG(JMAG).
C
C-----------------------------------------------------------------------
C
C Get an initial guess at the standard magnitudes for the star by
C simply subtracting the zero-points from the observed magnitudes.
C
C Zero the accumulators.
C 
      DO JMAG=1,NMAG
         SUMM(JMAG)=0.0
         SUMW(JMAG)=0.0
         EXTRA(JMAG)=0.
         CLAMP(JMAG)=1.
         OLDDEL(JMAG)=0.
      END DO
C
      DO 3200 I=1,M
         JMAG=IMAG(I)
         JFILE=IFILE(I)
         WT=1./(0.0025 + OBSERR(I)**2)
         IF (CLOUD(JFILE)) THEN
            SUMM(JMAG)=SUMM(JMAG)+WT*OBSMAG(I)
         ELSE
            IF (ZTERM(JMAG,JFILE) .LT. -1.E38) GO TO 3200
            SUMM(JMAG)=SUMM(JMAG)+WT*(OBSMAG(I)-ZTERM(JMAG,JFILE))
         END IF
         SUMW(JMAG)=SUMW(JMAG)+WT
 3200 CONTINUE
C
      DO JMAG=1,NMAG
         IF (SUMW(JMAG) .GT. 0.) THEN
            STMAG(JMAG)=SUMM(JMAG)/SUMW(JMAG)
         ELSE
            STMAG(JMAG) = 99.999
         END IF
      END DO
C
C-----------------------------------------------------------------------
C
C Beginning of iteration loop for this star.
C
      LAST=.FALSE.
      NITER = 0
 4100 NITER = NITER + 1
C
C Zero the accumulators.
C 
      DO JMAG=1,NMAG
         SUMM(JMAG)=0.0
         SUMD(JMAG)=0.0
         SUMDW(JMAG)=0.0
         SUMW(JMAG)=0.0
         SUMN(JMAG) = 0.0
         NCLR(JMAG) = 0
         N(JMAG)=0
         IF (STMAG(JMAG) .LT. 50.) THEN
            STFLUX(JMAG) = EXP(0.921034*(20.-STMAG(JMAG)))
         ELSE
            STFLUX(JMAG) = 1.1E38
         END IF
      END DO
C
C Perform another iteration, subtracting the transformation equation
C (employing the current estimate of the standard indices) from the
C observed magnitude, to get an estimate of the standard magnitude.
C
      DO 3920 I=1,M
C
C First, using the best current guess for each of the magnitudes on the
C standard system, compute the standard indices appropriate for this
C night.
C
         JMAG = IMAG(I)                              ! Which magnitude?
         JFILE = IFILE(I)
         DO 4110 INDEX=1,NINDX(JFILE)                ! Loop over indices
            DEFNDX(INDEX) = .FALSE.
            CALNDX(INDEX) = 0.0
C
            DO 4103 J=1,NMAG                   ! Loop over magnitudes
               IF (MAGNDX(J,INDEX,JFILE) .EQ. 0) GO TO 4103
                  IF (STMAG(J) .GT. 50.) GO TO 4107
                  CALNDX(INDEX) = CALNDX(INDEX)+
     .                 MAGNDX(J,INDEX,JFILE)*STMAG(J)
 4103       CONTINUE                       ! End of loop over magnitudes
            IF (CALNDX(INDEX) .LT. -5.) GO TO 4107
            DEFNDX(INDEX) = .TRUE.
            GO TO 4110
C
 4107       CALNDX(INDEX) = 99.9999
C
 4110    CONTINUE                             ! End of loop over indices
C
C DEFNDX(INDEX) is .FALSE. if (a) it is constructed from no magnitudes
C (i.e., if it hasn't been defined in the transformation file); or (b)
C if it has been defined but at least one of the constituent magnitudes
C has not been observed.
C
         IF (STFLUX(JMAG) .GT. 1.E38) GO TO 3920
         FLUX = TRFM(NINDX(JFILE), CALNDX, DEFNDX, Q(I), T(I), 
     .        X(I), Y(I), R(I), S(I),
     .        NTERM(JMAG,JFILE), ITERM(1,1,JMAG,JFILE), 
     .        USE(1,JMAG,JFILE), COEFF(1,JMAG,JFILE), .FALSE.)
         IF (FLUX .GT. 90.) THEN
            CALMAG(I) = OBSMAG(I)
            W(I) = -0.1
            GO TO 3920
         END IF
         CALMAG(I)=OBSMAG(I)-FLUX
c     if(jmag.eq.5)print666,file(jfile),q(i),t(i),x(i),y(i),flux,
c    .     calndx(5),calmag(i),stmag(5)
c 666 format(a40,8f10.3)
         IF (ABS(CALMAG(I)-20.) .GT. 25.) THEN
            PRINT*,'jmag =', jmag, stmag(1), stmag(3), calndx(2)
            PRINT*,'obsmag =', obsmag(i)
            PRINT*,'calmag =', calmag(i)
            WRITE (INPUT,99) IDMAST(JSTAR),
     .           (CALNDX(INDEX),INDEX=1,NINDX(JFILE))
   99       FORMAT (A12, 6F9.3)
            CALL STUPID ('Blowup:  '//INPUT(1:66)//FILE(JFILE))
            CALMAG(I) = OBSMAG(I)
            W(I) = -0.1
            GO TO 3920
         END IF
         FLUX = EXP(0.921034*(20.-CALMAG(I)))
         SIGSQ=0.8483037*(OBSERR(I)*STFLUX(JMAG))**2 + EXTRA(JMAG)
         WT = 1./SIGSQ
         DELTAM=FLUX-STFLUX(JMAG)
         W(I)=1./(1. + (0.4*ABS(DELTAM)*SQRT(WT))**3)
C        IF (LAST) THEN
C           SUMD(JMAG) = SUMD(JMAG) + 
C    .           ABS(DELTAM)/(1.0857362*FLUX*OBSERR(I))
C           SUMDW(JMAG) = SUMDW(JMAG) + 1.
C        ELSE
            SUMD(JMAG) = SUMD(JMAG) + W(I)*ABS(DELTAM)*SQRT(WT)
            SUMDW(JMAG) = SUMDW(JMAG) + W(I)
C        END IF
         WT=W(I)*WT
C
C Increment the accumulators.
C
         SUMN(JMAG) = SUMN(JMAG) + W(I)
         SUMM(JMAG) = SUMM(JMAG)+WT*DELTAM
         SUMW(JMAG) = SUMW(JMAG)+WT
         N(JMAG)=N(JMAG)+1
         IF (.NOT. CLOUD(JFILE)) NCLR(JMAG) = NCLR(JMAG)+1
 3920 CONTINUE
C
C All observations of this star have been figured in, so correct the
C current estimates of the magnitudes on the standard system.
C
      LAST=.TRUE.
C
      DO JMAG=1,NMAG
         IF ((SUMW(JMAG) .GT. 0.) .AND. (SUMN(JMAG) .GT. 1.E-10)) THEN
            DELTAM=SUMM(JMAG)/SUMW(JMAG)
            IF (NITER .GT. 395) PRINT *, DELTAM, OLDDEL(JMAG)
C
            IF (OLDDEL(JMAG)*DELTAM .LT. 0.) THEN
               CLAMP(JMAG)=CLAMP(JMAG)/2.
               IF (CLAMP(JMAG) .LE. 1.E-20) GO TO 3999
            ELSE
               CLAMP(JMAG) = AMIN1(1.5*CLAMP(JMAG), 10./REAL(NITER))
            END IF
C
            OLDDEL(JMAG)=DELTAM
            DELTAM = DELTAM/(1.+ABS(DELTAM)/(CLAMP(JMAG)*STFLUX(JMAG)))
            WT=STFLUX(JMAG)+DELTAM
C
C WT is the corrected flux
C
            IF (ABS(WT/STFLUX(JMAG)-1.) .GT. 5.E-5) LAST=.FALSE.
            IF (NITER .GT. 395) PRINT *, 
     .           ABS(WT/STFLUX(JMAG)-1.) , 5.E-5, LAST
            STFLUX(JMAG) = WT
C
            IF (SUMDW(JMAG) .GT. 0.) THEN
               ME1(JMAG) = 1.570796*(SUMD(JMAG)/SUMDW(JMAG))**2
C   
               IF (SUMDW(JMAG) .GT. 1.) THEN
                  WME1(JMAG) = SUMDW(JMAG) - 1.
                  ME1(JMAG) = ME1(JMAG) *
     .                 (SUMDW(JMAG)/WME1(JMAG))
               ELSE IF (N(JMAG) .GT. 1) THEN
                  WME1(JMAG) = REAL(N(JMAG)) - 1.
                  ME1(JMAG) = ME1(JMAG) *
     .                 (REAL(N(JMAG))/WME1(JMAG))
               ELSE
                  WME1(JMAG) = 0.
                  ME1(JMAG) = 1.
                  EXTRA(JMAG)=0.
                  GO TO 3922
               END IF
C
               EXTRA(JMAG)=AMAX1(0., EXTRA(JMAG) +
     .              (ME1(JMAG)-1.)*(SUMN(JMAG)/SUMW(JMAG)))
C
C EXTRA (= [add'l error]**2) in flux units
C
            ELSE
               WME1(JMAG) = 0.
               ME1(JMAG) = 1.
               EXTRA(JMAG)=0.
            END IF
 3922       CONTINUE
            STMAG(JMAG) = 20.-1.0857362*ALOG(STFLUX(JMAG))
C
C EXTRA and SUMW in magnitudes**2
C
         ELSE
            WME1(JMAG) = 0.
            ME1(JMAG) = -1.
            EXTRA(JMAG)=0.
            STMAG(JMAG) = 99.999
            SUMW(JMAG) = 9.9999
         END IF
      END DO
      IF (NITER .GE. 400) THEN
         WRITE (6,669) NITER, IDMAST(JSTAR),
     .        (STMAG(JMAG), JMAG=1,NMAG)
  669    FORMAT ('nonconvergence: ', I6, 2X, A12, 2X, 6F9.4)
         GO TO 3999
      END IF
      IF ((.NOT. LAST) .AND. (NITER .LT. 400)) GO TO 4100
C
C Solution has converged.
C
      DO JMAG=1,NMAG
         IF (STMAG(JMAG) .LT. 50.) GO TO 3995
      END DO
      GO TO 3999
 3995 CONTINUE
C
C Compute extra observational error and mean errors of magnitudes.
C
      DELTAM=0.
      XX = 0.
      DO JMAG=1,NMAG
         IF (STMAG(JMAG) .LT. 50.) THEN
            FLUX = 0.921034*EXP(0.921034*(20.-STMAG(JMAG)))
            SUMW(JMAG) = SQRT(1./SUMW(JMAG))/FLUX
C
C SUMW now = standard error of the mean.
C
C Compute variability index but IGNORE U.
C
            IF (EXTRA(JMAG) .GT. 0.) THEN
               EXTRA(JMAG) = SQRT(EXTRA(JMAG))/FLUX
               if (jmag .lt. 5) then
               DELTAM = DELTAM+EXTRA(JMAG)**2
               XX = XX+1.
               end if
            ELSE
               if (jmag .lt. 5) then
               DELTAM = DELTAM+(ME1(JMAG)-1.)*SUMW(JMAG)**2/SUMN(JMAG)
               XX = XX+1.
               end if
            END IF
            ME1(JMAG) = SQRT(ME1(JMAG))
         END IF
      END DO
      DELTAM = MIN(999.999, MAX(-999.999, DELTAM/XX))
      DELTAM = SIGN(SQRT(ABS(DELTAM)), DELTAM)
      DO JMAG=1,NMAG
         SMAG(JMAG) = RNDOFF(STMAG(JMAG), 7, 3)
         SSIG(JMAG) = RNDOFF(SUMW(JMAG), 7, 4)
         WRITE (N1(JMAG),33) NCLR(JMAG)
   33    FORMAT (I5)
         WRITE (N2(JMAG),33) N(JMAG)
      END DO
      WRITE (3,330) IDMAST(JSTAR), DELTAM, (SMAG(JMAG),
     .     SSIG(JMAG), JMAG=1,NMAG), (N1(JMAG), JMAG=1,NMAG),
     .     ' ', (N2(JMAG), JMAG=1,NMAG)
  330 FORMAT(1X, A12, F8.3, 1X, 25A)
C
      IF (DISPLAY .EQ. 'Y') WRITE (6,429) IDMAST(JSTAR), ' <<'
      WRITE (1,*)
      WRITE (1,*)
C
C Loop over output blocks.
C
      DO 5010 LLL=1,(NMAG-1)/NACRSS+1
      WRITE (1,429) IDMAST(JSTAR)
  429 FORMAT (1X, A12, A)
      DO L=1,NACRSS
         N(L)=0
      END DO
      J=0
      K=0
C
      LL = NACRSS*LLL - NACRSS
      DO I=1,M
         L = IMAG(I)-LL
         IF ((L .GE. 1) .AND. (L .LE. NACRSS)) THEN
            IF (L .GT. K) K=L
            N(L)=N(L)+1
            if(cloud(ifile(i)))then
               flag='-'
            else
               flag=' '
            end if
            WRITE (TEXT(N(L))(NWIDE*L-NWM1:NWIDE*L), 430) CALMAG(I), 
     .           OBSERR(I), NINT(10.*W(I)), FLAG, FRMID(I)(1:NWM19)
  430       FORMAT (F8.3, F7.3, I3, a1, A)
            IF (N(L) .GT. J) J=N(L)
         END IF
      END DO
      IF (K .LE. 0) GO TO 5010
C
      DO L=1,K
         IF (N(L) .LT. J) THEN
            DO I=N(L)+1, J
               WRITE (TEXT(I)(NWIDE*L-NWM1:NWIDE*L), 431),
     .               (' ', ISTAT=1,NWIDE)
 431           FORMAT (132A1)
            END DO
         END IF
      END DO
C
      DO I=1,J
         WRITE (1,432) IDMAST(JSTAR), TEXT(I)(1:NWIDE*K)
  432    FORMAT (1X, A12, 1X, 3A)
         IF (DISPLAY .EQ. 'Y') WRITE (6,432) TEXT(I)(1:NWIDE*K)
      END DO
      WRITE (1,433) IDMAST(JSTAR), (LINE(1:NWIDE), L=1,K)
  433 FORMAT (1X, A12, 1X, 3A)
      IF (DISPLAY .EQ. 'Y') WRITE (6,433) (LINE(1:NWIDE), L=1,K)
      IF (NITER .GE. 400) THEN
         GO TO 3999
      END IF
      DO L=1,K
         JMAG = LL+L
         IF (STMAG(JMAG) .LT. 50.) THEN
            IF (N(JMAG) .EQ. 1) THEN
               WRITE (TEXT(1)(NWIDE*L-NWM1:NWIDE*L), 436)
     .              STMAG(JMAG), SUMW(JMAG)
  436          FORMAT (1X, F8.4, F7.4, 19X)
            ELSE IF (EXTRA(JMAG) .GT. 0.) THEN
               I = MAX0(0, MIN0(4, 4-INT(ALOG10(EXTRA(JMAG))) ))
               EX = RNDOFF(EXTRA(JMAG), 7, 4)
               WRITE (TEXT(1)(NWIDE*L-NWM1:NWIDE*L), 434)
     .              STMAG(JMAG), SUMW(JMAG), 'extra =', EX
  434          FORMAT (1X, F8.4, F7.4, 2X, A7, A7, 3X)
            ELSE
               WRITE (TEXT(1)(NWIDE*L-NWM1:NWIDE*L), 435)
     .              STMAG(JMAG), SUMW(JMAG), 'm.e.1 =', ME1(JMAG)
  435          FORMAT (1X, F8.4, F7.4, 2X, A7, F6.3, 4X)
            END IF
         ELSE
            WRITE (TEXT(1)(NWIDE*L-NWM1:NWIDE*L), 431)
         END IF
      END DO
      WRITE (1,432) IDMAST(JSTAR), TEXT(1)(1:NWIDE*K)
      WRITE (1,432) IDMAST(JSTAR)
      WRITE (TEXT(1)(NWIDE*K+1:NWIDE*K+1),439) '<'
  439 FORMAT (A1)
      IF (DISPLAY .EQ. 'Y') WRITE (6,432) TEXT(1)(1:NWIDE*K+1)
      IF (DISPLAY .EQ. 'Y') WRITE (6,432) IDMAST(JSTAR)
 5010 CONTINUE
 3999 CONTINUE
C
C END OF BIG LOOP OVER STARS.
C
C=======================================================================
C
      CALL TBLANK
      print *,'           Total # of stars in catalog', ntot
      print *,'Maximum # of frames for a cloudy night', lframe
      print *,'Maximum # of observations for one star', lobs
      CALL BYEBYE
C
C=======================================================================
C
C SECTIONS 8 and 9
C
C Irrecoverable errors.
C
C 8000-8999:  Errors during disk I/O.
C
 8000 CALL STUPID ('Error opening input file.')
      CALL OOPS
C
 8100 CALL STUPID ('Error occured while reading data from disk file.')
      CALL OOPS
C
 8200 CALL STUPID ('Unexpected end of file.')
      CALL OOPS
      END!
C
C######################################################################
C
      REAL FUNCTION  TRFM (NINDX, STINDX, DEFNDX, 
     .     Q, T, X, Y, R, S, NTERM, ITERM, USE, COEFF, SHOW)
      IMPLICIT NONE
      INTEGER MAXTRM
      PARAMETER (MAXTRM=10)
      INTEGER JTERM, KINDX, NINDX, NTERM, ITERM(MAXTRM,*)
      DOUBLE PRECISION PRDCT, SUM
      REAL Q, T, X, Y, R, S, STINDX(*), COEFF(*)
      LOGICAL USE(*), DEFNDX(*), SHOW
      SUM=0.D0
C
      DO 3130 JTERM=1,NTERM
      IF (USE(JTERM)) THEN
         IF (COEFF(JTERM) .LT. -1.E38) THEN
            TRFM = 1.1E38
            RETURN
         END IF
C
C Compute the value of the term.
C
C First, figure in the airmass and time of the observation.
C
         PRDCT=1.D0
         IF (ITERM(JTERM,NINDX+1) .NE. 0) THEN
            PRDCT=PRDCT*DBLE(Q)**ITERM(JTERM,NINDX+1)
         END IF
C
         IF (ITERM(JTERM,NINDX+2) .NE. 0) THEN
            PRDCT=PRDCT*DBLE(T)**ITERM(JTERM,NINDX+2)
         END IF
C
         IF (ITERM(JTERM,NINDX+3) .NE. 0) THEN
            PRDCT=PRDCT*DBLE(X)**ITERM(JTERM,NINDX+3)
         END IF
C
         IF (ITERM(JTERM,NINDX+4) .NE. 0) THEN
            PRDCT=PRDCT*DBLE(Y)**ITERM(JTERM,NINDX+4)
         END IF
C
         IF (ITERM(JTERM,NINDX+5) .NE. 0) THEN
            PRDCT=PRDCT*DBLE(R)**ITERM(JTERM,NINDX+5)
         END IF
C
         IF (ITERM(JTERM,NINDX+6) .NE. 0) THEN
            PRDCT=PRDCT*DBLE(S)**ITERM(JTERM,NINDX+6)
         END IF
C
C
C Now include each of the standard photometric indices in the present
C term.
C
         DO 3120 KINDX=1,NINDX
            IF (ITERM(JTERM,KINDX) .NE. 0) THEN
               IF (.NOT. DEFNDX(KINDX)) THEN
                  TRFM = 1.1E38
                  RETURN
               END IF
               PRDCT = PRDCT*DBLE(STINDX(KINDX))**ITERM(JTERM,KINDX)
            END IF
 3120    CONTINUE                          ! End of loop over indices
C
         SUM=SUM+DBLE(COEFF(JTERM))*PRDCT
      IF (SHOW) PRINT*,JTERM,COEFF(JTERM),PRDCT,SNGL(SUM)
      END IF
 3130 CONTINUE                             ! End of loop over terms
      TRFM=SNGL(SUM)
      if (show) read (5,*)
      RETURN
      END!
C
C#######################################################################
C
      SUBROUTINE  RDTFM (NINDX, NMAG, MAGNDX, NTERM, ITERM, 
     .     USE, COEFF, COSMIC, CLOUD, NFRAME, FRAME, ZERO, SZERO)
      
C
C=======================================================================
C
C Read in the transformation equations and any coefficient values.  
C
      IMPLICIT NONE
      INTEGER MAXTRM, MAXNDX, MAXMAG, NEXTRA, MFRAME
      PARAMETER (MFRAME=1000, MAXTRM=10, MAXNDX=6, MAXMAG=6, NEXTRA=6)
      INTEGER ITERM(MAXTRM,MAXNDX+NEXTRA,MAXMAG), NTERM(MAXMAG)
      INTEGER MAGNDX(MAXMAG,MAXNDX) ! , ISTMAG(MAXNDX,MAXMAG)
      INTEGER NFRAME(MAXMAG)
      INTEGER I, J, K, L, NCHR, NINDX, NMAG, ISTAT
      REAL COEFF(MAXTRM,MAXMAG), COSMIC(MAXMAG) 
      REAL ZERO(MFRAME,MAXMAG), SZERO(MFRAME,MAXMAG)
      REAL Z, SZ
      LOGICAL CLOUD, FIX(MAXTRM,MAXMAG), USE(MAXTRM,MAXMAG)
      CHARACTER LINE*121, SIGN*1, CHR*1, FRAME(MFRAME,MAXMAG)*40, F*40
C
C Initialize the relevant arrays.
C
      CLOUD=.FALSE.
      DO I=1,MAXMAG
         COSMIC(I)=0.
         NTERM(I)=0
         DO J=1,MAXNDX
            MAGNDX(I,J)=0
C           ISTMAG(J,I)=0
         END DO
C
         DO J=1,MAXTRM
            COEFF(J,I)=-1.1E38
            DO K=1,MAXNDX+NEXTRA
               ITERM(J,K,I)=0
            END DO
         END DO
C
         NFRAME(I)=0
         DO J=1,MFRAME
            ZERO(J,I)=0.
            SZERO(J,I) = 0.
            FRAME(J,I)=' '
         END DO
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
C     (2) Mn = functions of (In)
C     (3) In = functions of (Mn)
C     (4) Sn = numerical constant
C     (5) Zn = numerical constant character string
C     (6) xn = numerical constant
C
C These lines define, respectively:
C
C     (1) A transformation equation, defining the n-th observed 
C         magnitude as a function of the n-th standard magnitude
C         plus a transformation equation consisting of terms
C         involving products of the standard photometric indices,
C         and the airmass (X) and time (T) of the observation.
C
C     (2) The n-th standard magnitude, expressed as sums and/or
C         differences of the various standard photometric indices.
C
C     (3) The n-th standard photometric index, expressed as sums
C         and/or differences of the various standard magnitudes.
C
C     (4) The "nightly" standard error in the determination of the
C         n-th magnitude, over and above the photon and read noise.
C
C     (5) A photometric zero-point for CCD frame 'character string'
C         (cloudy-weather mode).
C
C     (6) One of the coefficients in one of the transformation
C         equations-- a real number.
C
C The numerical quantities defining all of these things will be
C stored in the arrays
C
C     (1) ITERM (term, index & X & T, magnitude)
CC    (2) ISTMAG (index, magnitude)
C     (3) MAGNDX (magnitude, index)
C     (4) COSMIC (magnitude)
C     (5) ZERO (i, magnitude) = constant; SZERO, FRAME (i, magnitude) = string
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
         CLOUD=.TRUE.
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
      IF (SIGN .EQ. 'I') THEN
         CALL ILINE (LINE, L, NMAG, NINDX, MAGNDX)
C
C     ELSE IF (SIGN .EQ. 'M') THEN
C        CALL MLINE (LINE, L, NMAG, NINDX, ISTMAG)
C
      ELSE IF (SIGN .EQ. 'O') THEN
         CALL OLINE (LINE, L, NMAG, NINDX, NTERM, USE, ITERM, FIX)
C
      ELSE IF (SIGN .EQ. 'S') THEN
         CALL SLINE (LINE, L, COSMIC)
C
      ELSE IF (SIGN .EQ. 'Z') THEN
         READ (LINE(2:NCHR-1), 232) I, Z, SZ, F
  232    FORMAT (I1, 2X, 2F12.7, 60X, A)
         NFRAME(I)=NFRAME(I)+1
         IF (NFRAME(I) .GT. MFRAME) THEN
            CALL STUPID ('Frame limit exceeded: '//F)
            print*,1,i,z,sz,f,nframe(i)
            CALL CLFILE (2)
            CALL OOPS
         END IF
         J=NFRAME(I)
         FRAME(J,I)=F
         ZERO(J,I)=Z
         SZERO(J,I) = SZ
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
CC ISTMAG(INDEX,JMAG) contains the number of times that the INDEX-th 
CC     standard index appears as an additive factor in the sum for
CC     the JMAG-th magnitude.
CC
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
