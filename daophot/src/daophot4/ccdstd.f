C
C=======================================================================
C
C A program to compute the transformations from instrumental CCD 
C photometry to a standard system.
C This program is untidy and complicated, and is certainly not in its
C final form.
C
C             Official DAO version:  2004 September 14
C
C=======================================================================
C
      PARAMETER (MAXMAG=6, MAXNDX=6, MPTERM=10, NEXTRA=6)
      PARAMETER (MFRAME=2000, MAXSTR=1 000 000, MAXSTD=300 000)
C
C MAXMAG is the maximum number of instrumental CCD bandpasses that can
C        handled.
C MAXNDX is the maximum number of photometric indices on the standard
C        photometric system that can be handled.
C MPTERM is the maximum number of coefficients and terms that can be
C        included in each transformation equation.
C NEXTRA is the number of known observational quantities that can be
C        included in the transformation equations, besides the standard
C        photometric indices.  NEXTRA = 6: airmass (Q), time of 
C        observation (T), position in the chip (X,Y), and cos and sin
C        of azimuth (R,S).
C MAXSTD is the largest number of standard stars that can be held in
C        memory at once.
C MAXSTR is the largest number of stellar observations that can be
C        held in memory at once.
C MFRAME is the largest number of different CCD frames in each color 
C        that can be included in a single night's reduction.
C
      CHARACTER LINE*250
      CHARACTER FILE*40, RESIDS*40, CLBRTN*40, SWITCH*40, EXTEND*40, 
     .     TFMFILE*40, FRMID(MFRAME,MAXMAG)*40, FRAME(MAXSTR)*40
      CHARACTER STNDID(MAXSTD)*12, STARID(MAXSTR)*12
      CHARACTER INDLBL(MAXNDX)*6, MAGLBL(MAXMAG)*6, STRNG(MAXMAG)*7
      CHARACTER STRNG1*12, STRNG2*12
      CHARACTER TIME*7, RNDOFF*12, QUESTN*2, EXCLAM*2
      CHARACTER CASE*4, SGN*1
      DOUBLE PRECISION C(MPTERM+MFRAME,MPTERM+MFRAME), V(MPTERM+MFRAME), 
     .     U(MPTERM+MFRAME), TERM(MPTERM)
      REAL COSMIC(MAXMAG), ZERO(MFRAME,MAXMAG)
      REAL COEFF(MPTERM,MAXMAG), ERROR(MPTERM,MAXMAG)
      REAL CLAMP(MPTERM+MFRAME,MAXMAG), OLD(MPTERM+MFRAME,MAXMAG)
      REAL STINDX(MAXNDX,MAXSTD), STDERR(MAXMAG,MAXSTD)
      REAL OBSMAG(MAXSTR), OBSERR(MAXSTR), Q(MAXSTR), T(MAXSTR),
     .     TINT(MAXSTR), X(MAXSTR), Y(MAXSTR), SKY(MAXSTR),
     .     AZ(MAXSTR), R(MAXSTR), S(MAXSTR)
      REAL STMAG(MAXMAG,MAXSTD)
      REAL SFRAME(MFRAME), TFRAME(MFRAME), XFRAME(MFRAME),
     .     OFRAME(MFRAME), SUMW(MFRAME)
      real sumar(mframe)
      REAL G(7), WARM(7), COLD(7)
      INTEGER ITERM(MPTERM,MAXNDX+NEXTRA,MAXMAG)
      INTEGER MAGNDX(MAXMAG,MAXNDX)
      INTEGER ISTD(MAXSTR), IMAG(MAXSTR)
      INTEGER NTERM(MAXMAG), IFRAME(MAXSTR), IWT(MAXMAG)
      INTEGER ISOLVE(MPTERM), NFRAME(MAXMAG), NSTAN(MFRAME,MAXMAG),
     .     NSUM(MFRAME)
      LOGICAL FIX(MPTERM,MAXMAG), DEFNDX(MAXNDX)
      LOGICAL FIXZER(MFRAME,MAXMAG), VALID(MFRAME)
      LOGICAL HST, CLOUD, LAST, WROTE
      DATA DEFNDX /MAXNDX*.FALSE./
      DATA QZERO /1.00/
      DATA HST /.false./
C
C=======================================================================
C
C SECTION 1(a)
C
C Get the name of the standard-star library, and read the library in.
C
      CALL FABORT
      IF (HST) THEN
         CALL STUPID ('Set for HST reductions!')
         FILE = 'unix:coldwarm.dat'
         CALL INFILE (1, FILE, ISTAT)
         READ (1,*) (COLD(I), I=1,6)
         READ (1,*) (WARM(I), I=1,6)
         CLOSE (1)
      END IF
      FILE = 'ccdstd.out'
      CALL DELFIL (9, FILE, ISTAT)
      CALL OUTFIL (9, FILE, ISTAT)
C
 1000 CALL TBLANK
      FILE=' '
 1010 CALL GETNAM ('File with standard-star library:', FILE)
      IF (FILE .EQ. 'END-OF-FILE') GO TO 8500
      IF (FILE .EQ. 'EXIT') GO TO 8500
      IF (FILE .EQ. 'GIVE UP') CALL OOPS
      FILE = EXTEND(FILE, CASE('lib'))
      CALL INFILE (2, FILE, ISTAT)
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Error opening library.')
         FILE = 'GIVE UP'
         GO TO 1010
      END IF
C
      CALL RDLIB (2, MAXMAG, LIBMAG, INDLBL, NSTD, STNDID, 
     .     STMAG, STDERR)
      CALL CLFILE (2)
C
 1060 TFMFILE=' '
      CALL GETNAM ('File with transformation equations:', TFMFILE)
      IF (TFMFILE .EQ. 'EXIT') GO TO 8500
      IF (TFMFILE .EQ. 'END-OF-FILE') GO TO 1000
      TFMFILE = EXTEND(TFMFILE, CASE('tfm'))
C
C-----------------------------------------------------------------------
C
C SECTION 1(b)
C
C Read in the observational data for the standard stars.
C
C Obtain the name of the file with the observational data, and open it.
C
      FILE=' '
 1085 CALL GETNAM ('File with observations:', FILE)
      IF (FILE .EQ. 'EXIT') GO TO 8500
      IF (FILE .EQ. 'GIVE UP') CALL OOPS
      IF (FILE .EQ. 'END-OF-FILE') THEN
         CALL TBLANK
         GO TO 1060
      END IF
      FILE = EXTEND(FILE, CASE('obs'))
      CALL INFILE (2, FILE, ISTAT)
      IF (ISTAT .NE. 0) THEN
         CALL STUPID ('Unable to open '//FILE)
         FILE = 'GIVE UP'
         GO TO 1085
      END IF
C
      IF (HST) THEN
         IF (FILE(1:4) .eq. 'warm') then
               DO I=1,6
                  G(I) = WARM(I)
               END DO
               G(7) =  0.
         ELSE
            DO I=1,6
                  G(I) = COLD(I)
            END DO
            G(7) =  0.
         END IF
      END IF
C
      CLBRTN=SWITCH(FILE, CASE('.clb'))
      RESIDS=SWITCH(FILE, CASE('.rsd'))
      NMAG = LIBMAG
      CALL RDOBS (2, NMAG, MAGLBL, NSTAR, STARID, IMAG, T, Q, AZ,
     .     TINT, OBSMAG, OBSERR, X, Y, SKY, FRAME, HST, G)
      CALL CLFILE (2)
      DO JSTAR=1,NSTAR
         Q(JSTAR) = Q(JSTAR) - QZERO
         XX = AZ(JSTAR)/57.29578
         X(JSTAR) = X(JSTAR) * 0.001
         Y(JSTAR) = Y(JSTAR) * 0.001
         R(JSTAR) = Q(JSTAR) * COS(XX)
         S(JSTAR) = Q(JSTAR) * SIN(XX)
      END DO
C
C Find each star in the standard list.  If it is not there, append it
C to the end of the standard list.  Note that henceforth there will be 
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
      NTOT=NSTD
      DO 1190 JSTAR=1,NSTAR
         DO 1130 I=1,NTOT
         IF (STARID(JSTAR) .EQ. STNDID(I)) GO TO 1140
 1130    CONTINUE
C
C Star not found.
C
         NTOT=NTOT+1
         IF (NTOT .GT. MAXSTD) THEN
            CALL STUPID ('Too many different star ID''s!')
            CALL OOPS
         END IF
         STNDID(NTOT)=STARID(JSTAR)
         ISTD(JSTAR)=NTOT
         GO TO 1190
C
C Star found.
C
 1140    ISTD(JSTAR)=I
 1190 CONTINUE
C
      IF (NTOT .GT. NSTD) THEN
         WRITE (6,621)
  621    FORMAT (/' Stars not in the standard list:'/)
         WRITE (6,622) (STNDID(I), I=NSTD+1, NTOT)
  622    FORMAT (3X, A12, 1X, A12, 1X, A12, 1X, A12, 1X, A12, 1X, 
     .        A12)
      END IF
C
C Now assign sequential numbers to the various frame identifiers, and
C find out which frame each observation came from.
C
C In the event of a cloudy-weather-type reduction, we want to catalog
C only those frames containing at least one standard, so in setting
C up the frame list, we will ignore any non-standard star.
C
      DO JMAG=1,NMAG
         NFRAME(JMAG)=0
      END DO
      DO 1290 JSTAR=1,NSTAR
         JSTD = ISTD(JSTAR)
         IF (JSTD .GT. NSTD) GO TO 1290       ! Non-standard star
         JMAG = IMAG(JSTAR)
         IF (STDERR(JMAG,JSTD) .GT. 1.) GO TO 1290
         FILE = SWITCH(FRAME(JSTAR), ' ')
         IF (NFRAME(JMAG) .GE. 1) THEN
            DO JFRAME=NFRAME(JMAG),1,-1
               IF (FILE .EQ. FRMID(JFRAME,JMAG)) THEN
C
C This frame name has been encountered before.
C
                  IFRAME(JSTAR)=JFRAME
                  SFRAME(JFRAME) = SFRAME(JFRAME)+SKY(JSTAR)
                  NSTAN(JFRAME,JMAG)=NSTAN(JFRAME,JMAG)+1
                  GO TO 1290
               END IF
            END DO
         END IF
C
C This is a new, never before encountered, frame name.
C
         NFRAME(JMAG)=NFRAME(JMAG)+1
         JFRAME=NFRAME(JMAG)
         FRMID(JFRAME,JMAG)=SWITCH(FRAME(JSTAR), ' ')
         IFRAME(JSTAR)=JFRAME
         SFRAME(JFRAME)=SKY(JSTAR)
         XFRAME(JFRAME)=Q(JSTAR)+1.
         OFRAME(JFRAME)=TINT(JSTAR)
         NSTAN(JFRAME,JMAG)=1
 1290 CONTINUE
C
C But there might be some non-standard stars that are in the same
C frames as some standards, so we need to go through those stars again
C seeing whose frame-names we recognize.
C
      DO 1295 JSTAR=1,NSTAR
         IF (ISTD(JSTAR) .LE. NSTD) GO TO 1295
         JMAG=IMAG(JSTAR)
         DO JFRAME=1,NFRAME(JMAG)
            IF (FRAME(JSTAR) .EQ. FRMID(JFRAME,JMAG)) THEN
C
C This frame name has been encountered before.
C
               IFRAME(JSTAR)=JFRAME
               GO TO 1295
            END IF
         END DO
C
C This frame name is unrecognized.
C
         IFRAME(JSTAR)=0
 1295 CONTINUE
C
C=======================================================================
C
 1297 CALL INFILE (1, TFMFILE, ISTAT)
      IF (ISTAT .NE. 0) THEN
         CALL STUPID (
     .        'Unable to open file with transformation equations.')
         TFMFILE = 'GIVE UP'
         CALL GETNAM ('File with transformation equations:', TFMFILE)
         IF (TFMFILE .EQ. 'GIVE UP') CALL OOPS
         GO TO 1297
      END IF
C
      WROTE = .FALSE.
      CALL OUTFIL (2, RESIDS, ISTAT)
      CALL OUTFIL (3, CLBRTN, ISTAT)
      CALL READTFM (NINDX, LIBMAG, MAGNDX, NTERM, ITERM, COEFF, 
     .     FIX, COSMIC, CLOUD, ZERO, FIXZER)
C
      CALL CLFILE (1)
C
      CALL TBLANK
      IF (CLOUD) THEN
         WRITE (6,*) ' Cloudy-weather reduction mode.'
      ELSE
         WRITE (6,*) ' Clear-weather reduction mode.'
      END IF
      CALL TBLANK
C
C Compute standard indices as appropriate for this night from the
C standard magnitudes.
C
      DO 3020 JSTAR=1,NSTD
         DO 3010 KINDX=1,NINDX
            STINDX(KINDX,JSTAR) = 0.
            DO 3005 JMAG=1,LIBMAG
               IF (MAGNDX(JMAG,KINDX) .EQ. 0) GO TO 3005
               IF (ABS(STMAG(JMAG,JSTAR)) .GT. 50.) THEN
                  STINDX(KINDX,JSTAR) = 99.999
                  GO TO 3010
               END IF
               STINDX(KINDX,JSTAR) = STINDX(KINDX,JSTAR)+
     .              MAGNDX(JMAG,KINDX)*STMAG(JMAG,JSTAR)
 3005       CONTINUE
 3010    CONTINUE
 3020 CONTINUE
C
C=======================================================================
C
C COMPUTE THE LEAST-SQUARES TRANSFORMATIONS
C
CC First, for each standard-star observation, construct the standard 
CC magnitude in the appropriate bandpass from the standard photometric 
CC indices.
CC
C      DO 3020 JSTAR=1,NSTAR
C         JSTD=ISTD(JSTAR)
C         IF (JSTD .GT. NSTD) GO TO 3020
C         JMAG=IMAG(JSTAR)
C         STMAG(JSTAR)=0.0
C         ERR(JSTAR)=0.0
C         DO 3010 KINDX=1,NINDX
C            STMAG(JSTAR)=STMAG(JSTAR)+
C     .           ISTMAG(KINDX,JMAG)*STINDX(KINDX,JSTD)
C            ERR(JSTAR)=ERR(JSTAR)+
C     .           (ISTMAG(KINDX,JMAG)*STDERR(KINDX,JSTD))**2
C 3010    CONTINUE
C 3020 CONTINUE
C
C STMAG   is an array containing the standard magnitude for each 
C         observation
C ERR     is an array containing the variance of the standard magnitude
C         for each observation
C OBSMAG  is an array containing the instrumental magnitude for each
C         observation
C OBSERR  is an array containing the variance of the instrumental
C         magnitude for each observation
C
C Reduce the standard-star photometry, bandpass by bandpass.
C
      DO 3990 JMAG=1,NMAG
      DO J=1,NTERM(JMAG)+NFRAME(JMAG)
         OLD(J,JMAG) = 0.
         CLAMP(J,JMAG) = 1.
      END DO
      OLDCHI = 0.
      CHCLMP = 1.
      WRITE (2,230) MAGLBL(JMAG), 
     .     (INDLBL(KINDX), KINDX=1,NINDX)
  230 FORMAT(//' Fitting residuals in ', A6// 6X,  
     .     '(Computed magnitudes and residuals are on the', 
     .     ' instrumental system.)'// 3X,
     .     'T', 5X, 'Q', 4X, 'Az     Int',
     .     2X, ' Std    Obs    Comp    Resid  Sig     wt',
     .     '      X        Y       sky ', 3X, 6(A6, 1X))
      LAST = .FALSE.
      IEXP = 1
      IF (COSMIC(JMAG) .EQ. 0.) COSMIC(JMAG)=0.001
C
C Which of the terms are actually to be solved for, in the 
C transformation equation for this bandpass?
C
      MTERM=0
      DO 3105 I=1,NTERM(JMAG)
      IF (FIX(I,JMAG)) GO TO 3105
      MTERM=MTERM+1
      ISOLVE(MTERM)=I
 3105 CONTINUE
C
C NTERM(JMAG)  is the highest-numbered term included in the equation;
C MTERM        is the total number of unknowns to be solved for;
C ISOLVE       contains the identification numbers of those unknowns.
C
      FILE = SWITCH(RESIDS, ' ')
      WRITE (6,630) FILE(1:LENGTH(FILE)+1), MAGLBL(JMAG), 
     .     (ISOLVE(I)-1, I=1,MTERM)
  630 FORMAT (/' Reducing ', A, A6//
     .     ' Nightly        Coefficients.....'/'  Scatter ', 10I9)
C
C Beginning of iteration loop for this bandpass.
C
 3100 CONTINUE
c     IF (LAST) WRITE (6,631)
  631 FORMAT (//' Star', 10X, 
     .     'Standard Observed Computed Residual   Sigma     wt')
C
C Initialize matrices and accumulators.
C
      IF (CLOUD) THEN
         LTERM=MTERM+NFRAME(JMAG)
      ELSE
         LTERM=MTERM
      END IF
C
      DO I=1,LTERM
         V(I)=0.0D0
         DO J=1,LTERM
            C(I,J)=0.0D0
         END DO
      END DO
C
      DO I=1,NFRAME(JMAG)
         VALID(I) = .FALSE.
      END DO
C
      CHI = 0.0
      WCHI = 0.0
      SUMWT = 0.0
      NCHI = 0
C
C Set up the matrices for the least-squares solution.
C
C Loop over the observations, skipping over any stars that aren't
C standards and any observations not in this bandpass.
C
      NALL = 0
      IF (LAST) THEN
         NQUMA = 0
         NEXPO = 0
         DO I=1,NFRAME(JMAG)
            SUMW(I) = 0.
            sumar(i) = 0.
            nsum(i) = 0
         END DO
      END IF
      DO 3150 JSTAR=1,NSTAR
         JSTD=ISTD(JSTAR)

C
C Is this a standard star and is this observation in the bandpass 
C presently being reduced?
C
         IF (IMAG(JSTAR) .NE. JMAG) GO TO 3150
         IF ((JSTD .GT. NSTD) .OR. 
     .        (STDERR(JMAG,JSTD) .GT. 25.)) GO TO 3150
C
C Does this transformation require any undefined color indices?
C
         DO JTERM=1,NTERM(JMAG)
            DO KINDX=1,NINDX
               IF ((ITERM(JTERM,KINDX,JMAG) .NE. 0) .AND.
     .              (STINDX(KINDX,JSTD) .GT. 50.)) THEN
                  GO TO 3150
               END IF
            END DO
         END DO
         JFRAME=IFRAME(JSTAR)                         ! Which CCD frame?
         VALID(JFRAME) = .TRUE.
         DELTAM=STMAG(JMAG,JSTD)-OBSMAG(JSTAR)+
     .        TRFM(NINDX, STINDX(1,JSTD), Q(JSTAR), T(JSTAR), 
     .             X(JSTAR), Y(JSTAR), R(JSTAR), S(JSTAR),
     .             NTERM(JMAG), ITERM(1,1,JMAG), COEFF(1,JMAG), TERM)
C
C If this coefficient has been specified and fixed, do not include it
C in the least-squares solution.
C
         M=0
         DO 3130 JTERM=1,NTERM(JMAG)
            IF (FIX(JTERM,JMAG)) GO TO 3130
            M=M+1
            TERM(M)=TERM(JTERM)
 3130    CONTINUE                               ! End of loop over terms
C
         IF (CLOUD) THEN
            DELTAM=DELTAM+ZERO(JFRAME,JMAG)
         END IF
C
         WT=STDERR(JMAG,JSTD)+OBSERR(JSTAR)+COSMIC(JMAG)
         SIGMA=SQRT(WT)
         ADM = ABS(DELTAM)
         W = 1./(1. + (0.4*ADM/SIGMA)**IEXP)
         NALL = NALL+1
         WT = W/WT
         IF (LAST) THEN 
            tframe(jframe) = t(jstar)
            SUMW(JFRAME) = SUMW(JFRAME) + W
            sumar(jframe) = sumar(jframe) + w*adm
            NSUM(JFRAME) = NSUM(JFRAME) + 1
            SGN = '.'
            IF (ADM .GE. 2.*SIGMA) THEN
               IF (ADM .GE. 3.*SIGMA) THEN
                  SGN = '#'
                  NEXPO = NEXPO+1
               ELSE
                  SGN = '?'
                  NQUMA = NQUMA+1
               END IF
            END IF
            O = OBSMAG(JSTAR) - 2.5*ALOG10(TINT(JSTAR))
            IF (SGN .NE. '.') THEN
               WRITE (LINE,632) STARID(JSTAR), STMAG(JMAG,JSTD), 
     .              O, O+DELTAM, -DELTAM, SIGMA, 
     .              SGN, W, FRAME(JSTAR)
  632          FORMAT (1X, A12, 5F9.3, 1X, A1, F6.2, 2X, A30)
               DO K=132,2,-1
                  IF (LINE(K:K) .NE. ' ') GO TO 1551
               END DO
 1551          CONTINUE
C              WRITE (6,6) LINE(1:K)
    6          FORMAT (A)
            END IF
            DO I=1,NINDX
               STRNG(I) = RNDOFF(STINDX(I,JSTD), 7, 3)
            END DO
            TIME = RNDOFF(TINT(JSTAR), 7, 4)
            STRNG1 = RNDOFF(1000.*X(JSTAR), 9, 3)
            STRNG2 = RNDOFF(1000.*Y(JSTAR), 9, 3)
            WRITE (LINE,261) T(JSTAR), Q(JSTAR)+QZERO, AZ(JSTAR), 
     .           TIME, STMAG(JMAG,JSTD), O, O+DELTAM, 
     .           -DELTAM, MIN(SIGMA, 9.999), 
     .           SGN, W, STRNG1, STRNG2, SKY(JSTAR),
     .           (STRNG(I), I=1,NINDX), ' '//STARID(JSTAR), 
     .           ' '//FRAME(JSTAR)
  261       FORMAT (F6.2, F6.3, F6.1, A7, 3F7.3, F8.3, F6.3, 1X, A1, 
     .           F5.2, 2A9, F9.1, 8A)
            I = LENGTH(LINE)
            WRITE (2,6) LINE(1:I)
            WRITE (1,6) LINE(1:I)
         END IF
         IF (CLOUD) THEN
C
C For a cloudy-weather reduction, the size of the residual must be
C corrected for the number of stars upon which the frame's zero-point
C is based.
C
            IF (NSTAN(JFRAME,JMAG) .GT. 1) THEN
               SUMWT=SUMWT+WT
               RNSTAN=FLOAT(NSTAN(JFRAME,JMAG))
               WT=WT*RNSTAN/(RNSTAN-1.)
               CHI=CHI + W*ADM/SIGMA
               WCHI = WCHI + W
               NCHI = NCHI+1
            END IF
         ELSE
            SUMWT=SUMWT+WT
            CHI=CHI+W*ADM/SIGMA
            WCHI = WCHI + W
            NCHI = NCHI+1
         END IF
C
         DO I=1,M
            V(I)=V(I)+TERM(I)*DELTAM*WT
            DO J=1,M
               C(I,J)=C(I,J)+TERM(I)*TERM(J)*WT
            END DO
         END DO
C
         IF (CLOUD) THEN
            I=M+IFRAME(JSTAR)
            V(I)=V(I)+DELTAM*WT
            C(I,I)=C(I,I)+WT
            DO J=1,M
               C(I,J)=C(I,J)+TERM(J)*WT
               C(J,I)=C(J,I)+TERM(J)*WT
            END DO
         END IF
 3150 CONTINUE                                ! End of loop over stars
C
C IF CLOUDY WEATHER MODE:  remove from the design matrix any frames
C with no valid standard stars in this bandpass.
C
      IF (CLOUD) THEN
         DO I=NFRAME(JMAG),1,-1
            IF (.NOT. VALID(I)) THEN
               LTERM = LTERM-1
               L = MTERM+I
               IF (L .LE. LTERM) THEN
                  DO J=1,LTERM+1
                     DO K=L,LTERM
                        C(J,K) = C(J,K+1)
                     END DO
                  END DO
                  DO J=L,LTERM
                     V(J) = V(J+1)
                     DO K=1,LTERM
                        C(J,K) = C(J+1,K)
                     END DO
                  END DO
               END IF
            END IF
         END DO
      END IF
C
      IF ((LTERM .GE. NALL) .OR. (MTERM .GE. NCHI)) THEN
         CALL STUPID ('Not enough standards.')
         NFRAME(JMAG) = 0
         GO TO 3990
      END IF
      CALL DINVRS (C, MPTERM+MFRAME, LTERM, IFLAG)
      DO J=1,LTERM
         IF (C(J,J) .LE. 0.D0) IFLAG = 1
      END DO
      IF (IFLAG .NE. 0) THEN
         CALL STUPID ('Singular matrix.')
CC       CALL CLFILE (2)
CC       CALL CLFILE (3)
CC       FILE = 'GIVE UP'
CC       GO TO 1085
         GO TO 3990
      END IF
      CALL DVMUL (C, MPTERM+MFRAME, LTERM, V, U)
C
C Correct the coefficient estimates.
C
      IF (MTERM .GT. 0) THEN
         DO J=1,MTERM
            JTERM=ISOLVE(J)
            IF (U(J)*OLD(JTERM,JMAG) .LT. 0.)
     .           CLAMP(JTERM,JMAG) = 0.8*CLAMP(JTERM,JMAG)
            OLD(JTERM,JMAG) = U(J)
            U(J) = CLAMP(JTERM,JMAG)*U(J)
            COEFF(JTERM,JMAG)=COEFF(JTERM,JMAG)-U(J)
         END DO
      END IF
C
      IF (CLOUD) THEN
         J = MTERM
         DO JFRAME=1,NFRAME(JMAG)
            IF (VALID(JFRAME)) THEN
               J = J+1
               IF (U(J)*OLD(J,JMAG) .LT. 0.)
     .              CLAMP(J,JMAG) = 0.8*CLAMP(J,JMAG)
               OLD(J,JMAG) = U(J)
               U(J) = CLAMP(J,JMAG)*U(J)
               ZERO(JFRAME,JMAG)=ZERO(JFRAME,JMAG)-U(J)
            END IF
         END DO
      END IF
C
C Compute the nightly standard error.  At this point, CHI is the 
C weighted sum of the quantity [ABS(residual)/sigma].  NCHI is the 
C number of observations included in CHI.  If the sigma's are correct, 
C (PI/2) * (CHI/WCHI)**2 * NCHI / (NCHI - MTERM) should equal unity.  
C If it does not, then the sigmas will be appropriately increased or 
C decreased by adjusting the nightly error.
C
C SUMWT is the sum of the quantity [1./sigma**2].  Hence, SUMWT/NCHI 
C will be the average weight of an observation, NCHI/SUMWT will be
C some sort of average sigma (biassed toward the high-weight, 
C low-sigma observations). The square of the nightly error will be 
C altered by 
C
C        [(PI/2)*CHI**2/(NCHI-MTERM) - 1.0]*(NCHI/SUMWT).
C
      CHI = CHI/WCHI
      CHI = 1.570796 * CHI**2 * FLOAT(NCHI) / MAX(1., FLOAT(NCHI-MTERM))
      SUMWT=FLOAT(NCHI)/SUMWT
C
C CHI is now the standard error per unit weight.
C SUMWT is now the square of the average standard error per observation.
C Cease iteration when CHI has achieved the correct value or when
C the nightly error goes effectively to zero.
C
      IF (LAST) GO TO 3900
      CHI = CHI-1.0
      IF ((ABS(CHI) .LT. 2.*10.**(-IEXP)) .OR. 
     .     (COSMIC(JMAG) .LT. 1.E-8)) THEN 
         IF (IEXP .EQ. 1) THEN
            IEXP = 2
         ELSE IF (IEXP .EQ. 2) THEN
            IEXP = 3
         ELSE IF (IEXP .EQ. 3) THEN
            IEXP = 4
         ELSE IF (IEXP .EQ. 4) THEN
            LAST = .TRUE.
            FILE = EXTEND(SWITCH(RESIDS,MAGLBL(JMAG)),'rsd')
            CALL OUTFIL (1, FILE, ISTAT)
         END IF
      END IF
      CHI=CHI*SUMWT                               ! Correction to COSMIC
      IF (CHI*OLDCHI .LT. 0.0) THEN
         CHCLMP = 0.5*CHCLMP
      ELSE
         CHCLMP = MIN(1., 1.01*CHCLMP)
      END IF
      OLDCHI = CHI
      CHI=CHI/(1.0+1.5*ABS(CHI)/COSMIC(JMAG))     ! Clamp
      COSMIC(JMAG) = MAX(0., COSMIC(JMAG)+CHI*CHCLMP)
      WRITE (LINE,633) SQRT(COSMIC(JMAG)), (COEFF(ISOLVE(I),JMAG),
     .     I=1,M)
  633 FORMAT(F8.4,4X,10F9.4)
      CALL OVRWRT (LINE(1:12+9*M), 2)
      IF (LAST) THEN
         WRITE (LINE,638) (DSQRT(C(I,I)), I=1,M)
  638    FORMAT (9X,'+- ', 10F9.4)
         CALL OVRWRT (LINE(1:12+10*M), 4)
      END IF
      GO TO 3100
C
 3900 CALL CLFILE (1)
      FILE = SWITCH(FILE, ' ')
      IF (NQUMA .EQ. 1) THEN
         QUESTN = '  '
      ELSE
         QUESTN = '''s'
      END IF
      IF (NEXPO .EQ. 1) THEN
         EXCLAM = '  '
      ELSE
         EXCLAM = '''s'
      END IF
      WRITE (6,639) NALL, NQUMA, QUESTN, NEXPO, EXCLAM
  639 FORMAT (//1X, I7, ' standards:', I7, ' ?', A2, I7, ' #', A2/)
      WRITE (2,639) NALL, NQUMA, QUESTN, NEXPO, EXCLAM
      M=0
      WRITE (2,*)
      DO 3905 J=1,NTERM(JMAG)
      IF (COEFF(J,JMAG) .EQ. 0.) GO TO 3905
      IF (FIX(J,JMAG)) GO TO 3907
      M=M+1
      ERROR(J,JMAG) = DSQRT(C(M,M))
      WRITE (2,235) CHAR(JMAG+64), J-1, COEFF(J,JMAG), ERROR(J,JMAG)
  235 FORMAT (A1, I1, ' =', F9.4, ' +-', F7.4, 2F9.3, 3X, A12)
      GO TO 3905
 3907 WRITE (2,234) CHAR(JMAG+64), J-1, COEFF(J,JMAG)
  234 FORMAT (A1, I1, ' = ', F8.4)
 3905 CONTINUE
C
      IWT(JMAG) = NINT(0.001*NCHI/SUMWT)
      WRITE (2,*)
      WRITE (2,231) NCHI, SQRT(COSMIC(JMAG)), SQRT(SUMWT),
     .      IWT(JMAG), FILE(1:LENGTH(FILE))
  231 FORMAT (1X, I10, 
     .     ' stars, additional scatter =', F7.4,
     .     '  average std err ', F6.4,'  total weight ',
     .     I8, ' in ', A)
      WRITE (9,231) NCHI, SQRT(COSMIC(JMAG)), SQRT(SUMWT),
     .      IWT(JMAG), FILE(1:LENGTH(FILE))
      SUMQ = 0.
      SUMT = 0.
      SUM1 = 0.
      DO JSTAR=1,NSTAR
         JSTD=ISTD(JSTAR)
         IF ((JSTD .LE. NSTD) .AND. (IMAG(JSTAR) .EQ. JMAG) .AND.
     .        (STDERR(JMAG,JSTD) .LE. 1.)) THEN
            SUMQ = SUMQ+Q(JSTAR)
            SUMT = SUMT+T(JSTAR)
            SUM1 = SUM1 + 1.
         END IF
      END DO
      WRITE (2,239) SUMQ/SUM1+1., SUMT/SUM1
  239 FORMAT (//' Mean airmass =', F6.3, '   Mean time =', F7.3)
C
      DO 3910 I=1,NTERM(JMAG)
      IF (FIX(I,JMAG)) GO TO 3910
      STRNG1 = RNDOFF(COEFF(I,JMAG), 12, 7)
      STRNG2 = RNDOFF(ERROR(I,JMAG), 12, 7)
      WRITE (3,236) CHAR(64+JMAG), I-1, STRNG1, STRNG2, NCHI-MTERM
  236 FORMAT (A1, I1,' =', 2A12, 1X, '<<', I8, ' degrees of freedom')
 3910 CONTINUE
C
      IF (CLOUD) THEN
         WRITE (2,238) MAGLBL(JMAG)
  238    FORMAT (//' Photometric zero-points in ', A6 // 3X, 
     . 'Zero-point  +/-  sigma     <W>         T         X       sky',
     . '      n    Frame ID'/)
         J = MTERM
         DO I=1,NFRAME(JMAG)
            IF (VALID(I)) THEN
               J = J+1
               W = SUMW(I)/NSUM(I)
               adm = 1.2533*sumar(i)/sumw(i)
               STRNG1 = RNDOFF(ZERO(I,JMAG), 12, 7)
               STRNG2 = RNDOFF(SNGL(DSQRT(C(J,J))), 12, 7)
               WRITE (2,237) STRNG1, STRNG2,  adm, W,
     .              TFRAME(I), XFRAME(I), SFRAME(I)/NSUM(I),
     .              NSUM(I), FRMID(I,JMAG)
  237          FORMAT (1X, 2A12, 2F9.4, F11.2, F10.3, F9.1, I6, 
     .              4X, A30)
               WRITE (3,232) 'Z', JMAG, STRNG1, STRNG2,  
     .              NSUM(I), adm, W, TFRAME(I), XFRAME(I), 
     .              OFRAME(I), SFRAME(I)/NSUM(I),
     .              FRMID(I,JMAG)(1:LENGTH(FRMID(I,JMAG)))
  232          FORMAT (A1, I1,' =', 2A12, 1X, '<<', I6, 2F9.4,
     .              F7.2, F7.3, F9.3, F9.1, 1X, A)
            END IF
         END DO
      END IF
C
 3990 CONTINUE
      DO JMAG=1,NMAG
         IF (NFRAME(JMAG) .GE. 1) THEN
              STRNG1 = RNDOFF(SQRT(COSMIC(JMAG)), 12, 7)
              WROTE = .TRUE.
              WRITE (3,233) 'S', JMAG, STRNG1, IWT(JMAG), 
     .             SWITCH(CLBRTN, ' ')
  233         FORMAT (A1, I1,' =', A12, I12, 3X, '<<', 1X, A40)
         END IF
      END DO
C
      CALL CLFILE (2)
      CALL CLFILE (3)
      IF (.NOT. WROTE) CALL DELFIL (3, CLBRTN, ISTAT)
      CALL TBLANK
      FILE = 'EXIT'
      GO TO 1085
 8500 CONTINUE
      CALL CLFILE (9)
      CALL BYEBYE
      END!
C
C#######################################################################
C
      SUBROUTINE  RDOBS  (LUN, NMAG, MAGLBL, NSTAR, STARID, IMAG, 
     .     T, Q, AZ, TINT, OBSMAG, OBSERR, X, Y, SKY, 
     .     FRAME, HST, G)
      IMPLICIT NONE
      INTEGER I, J, K, N, LUN, JMAG, NMAG, NSTAR, ISTAT, 
     .     IMAG(*), LENGTH
      REAL T(*), Q(*), AZ(*), TINT(*), 
     .     OBSMAG(*), OBSERR(*), X(*), Y(*), SKY(*), ALOG10
      REAL TH, TM, ZERO, CTS, YCTE, XCTE, CORR, GAIN
      REAL TMID, LIMIT, SIGMAX
      REAL G(7)
      REAL DAMP, XX, YY, SKYLOG
      CHARACTER LINE*132, STARID(*)*12, FRAME(*)*40, MAGLBL(*)*6
      CHARACTER CURRNT*40, FORMER*40, SWITCH*40
      LOGICAL INVLD, HST
      DATA DAMP /1./, SIGMAX / 0.1 /
C
C Read in the number of observational magnitudes.
C
      INVLD = .TRUE.
      READ (LUN, 212, ERR=8100) K, (MAGLBL(I), I=1,K)
  212 FORMAT (1X, I1, 13X, 6(5X, A6))
      K = MIN(NMAG,K)
      DO I=1,K
  999    CONTINUE
         IF (MAGLBL(I)(1:1) .EQ. ' ') THEN
            MAGLBL(I) = MAGLBL(I)(2:5)//' '
            GO TO 999
         END IF
      END DO
C
C Read time of midnight.
C
      READ (LUN,*,IOSTAT=I) TH, TM
      IF (I .NE. 0) THEN
         CALL STUPID ('Unable to read time of midnight.')
         CALL CLFILE (LUN)
         CALL OOPS
      END IF
      TMID = TH + TM/60.
      READ (LUN,*)                        ! Skip over third header line
C
C Read in the stars, one by one.
C
      NSTAR=0
 1120 NSTAR=NSTAR+1
 1130 CALL RDCHAR (LUN, LINE, N, J)
      IF (J .GT. 0) GO TO 1900
      IF (J .LT. 0) GO TO 1130
      IF (LINE(4:14) .EQ. 'MAGNITUDES:') THEN
C
C Skip superfluous header.
C
         READ (LUN,*)
         READ (LUN,*)
         GO TO 1130
      END IF
C
C Azimuth and sky
C
      READ (LINE,213,IOSTAT=J) STARID(NSTAR), JMAG, 
     .     TH, TM, Q(NSTAR), AZ(NSTAR), TINT(NSTAR), OBSMAG(NSTAR), 
     .     OBSERR(NSTAR), ZERO, X(NSTAR), Y(NSTAR), 
     .     SKY(NSTAR), FRAME(NSTAR)
  213 FORMAT (1X, A12, I3, F5.0, F3.0, F8.3, F8.1, F9.1, 
     .     F9.3, F8.4, 4F9.2, 2X, A30)
      IF (J .NE. 0) THEN
C
C Azimuth and no sky
C
         SKY(NSTAR) = 0.
         READ (LINE,214,IOSTAT=J) STARID(NSTAR), JMAG, 
     .        TH, TM, Q(NSTAR), AZ(NSTAR), TINT(NSTAR), OBSMAG(NSTAR), 
     .        OBSERR(NSTAR), ZERO, X(NSTAR), Y(NSTAR), FRAME(NSTAR)
  214    FORMAT (1X, A12, I3, F5.0, F3.0, F8.3, F8.1, F9.1, 
     .        F9.3, F8.4, 3F9.2, 2X, A30)
C
C No azimuth and no sky
C
         IF (J .NE. 0) THEN
            AZ(NSTAR) = 0.
            READ (LINE,215,IOSTAT=J) STARID(NSTAR), JMAG, 
C           READ (LINE,215,ERR=1130) STARID(NSTAR), JMAG, 
     .           TH, TM, Q(NSTAR), TINT(NSTAR), OBSMAG(NSTAR), 
     .           OBSERR(NSTAR), ZERO, X(NSTAR), Y(NSTAR), 
     .           FRAME(NSTAR)
  215       FORMAT (1X, A12, I3, F5.0, F3.0, F8.3, F9.1, 
     .           F9.3, F8.4, 3F9.2, 2X, A30)
            IF (J .NE. 0) THEN
               PRINT*,line
               call oops
            end if
         END IF
      END IF
      jmag = jmag - 10*int(jmag/10)
      IF (OBSERR(NSTAR) .GT. SIGMAX) GO TO 1130
      OBSMAG(NSTAR) = OBSMAG(NSTAR) + ZERO
C
      IF (HST) THEN
         CURRNT = SWITCH(FRAME(NSTAR), ' ')
         IF (CURRNT .NE. FORMER) THEN
            CALL INFILE (3, FRAME(NSTAR), ISTAT)
            READ (3,*)
            READ (3,3) GAIN
    3       FORMAT (45X, F8.2)
            CALL CLFILE (3)
            FORMER = CURRNT
         END IF
C
         DO I=2,30
            IF (FRAME(NSTAR)(I:I) .EQ. ':') GO TO 1132
         END DO
         GO TO 1134
 1132    I = I-1
         IF (FRAME(NSTAR)(I:I) .EQ. 'p') THEN
            LIMIT = 100.
         ELSE IF (FRAME(NSTAR)(I:I) .EQ. 'w') THEN
            LIMIT = 75.
         ELSE
            GO TO 1134
         END IF
         IF ((X(NSTAR) .LT. LIMIT) .OR. (Y(NSTAR) .LT. LIMIT)) 
     .                                                        GO TO 1130
C
C Compute and apply ramp corrections.
C
Cc       XX = (X(NSTAR) - 425.)/375.
Cc       YY = (Y(NSTAR) - 425.)/375.
         XX = X(NSTAR)/400.
         YY = Y(NSTAR)/400.
         SKY(NSTAR) = GAIN*SKY(NSTAR)
         SKYLOG = ALOG10(SQRT(DAMP + AMAX1(0., SKY(NSTAR))**2)
     .     / 1.E1)
         CTS = GAIN*10.**(0.4*(25.-OBSMAG(NSTAR))) / 1.E4
         CTS = ALOG10(CTS)
         YCTE = G(1) + G(2)*SKYLOG + G(3)*CTS
         YCTE = EXP(YCTE)
Cc       XCTE = G(4) + G(5)*SKYLOG + G(6)*CTS
         XCTE = G(5) + G(6)*SKYLOG + G(7)*CTS
         XCTE = EXP(XCTE)
Cc       CORR = 1.D0 + 0.01*YCTE*(G(7)+YY) +
         CORR = 1.D0 + 0.01*YCTE*(G(4)+YY) +
     .        0.01*XCTE*XX
         OBSMAG(NSTAR) = OBSMAG(NSTAR) - 1.085736205D0*ALOG(CORR)
      END IF
 1134 IF ((JMAG .GT. K) .OR. (JMAG .LT. 1) .OR. 
     .    (Q(NSTAR) .LT. 0.) .OR.  (TINT(NSTAR) .LE. 0.) .OR.
     .    (OBSMAG(NSTAR) .GT. 90.)) GO TO 1190
      T(NSTAR)=TH+TM/60.-TMID
      IF (T(NSTAR) .GT. 12.) T(NSTAR) = T(NSTAR) - 24.
      IF (T(NSTAR) .LT. -12.) T(NSTAR) = T(NSTAR) + 24.
      IMAG(NSTAR) = JMAG - 10*int(jmag/10)
      OBSMAG(NSTAR)=OBSMAG(NSTAR) + 2.5*ALOG10(TINT(NSTAR))
      OBSERR(NSTAR)=OBSERR(NSTAR)**2
      GO TO 1120
C
 1190 IF (INVLD) CALL STUPID ('Invalid input data line(s); not used:')
      IF (JMAG .LT. 1) print *, 'mag', jmag, ' < 1'
      IF (Q(NSTAR) .LT. 0.) print *, 'q', q(nstar), ' < 0'
      IF (TINT(NSTAR) .LE. 0.) print *, 't', tint(nstar), ' <= 0'
      IF (OBSMAG(NSTAR) .GT. 90.) print *,'mag', obsmag(nstar), ' > 90.'
      WRITE (6,*) LINE(1:LENGTH(LINE))
      INVLD = .FALSE.
      GO TO 1130
C
 1900 NSTAR=NSTAR-1
      IF (.NOT. INVLD) CALL TBLANK
      NMAG = MIN(NMAG, K)
      RETURN
 8100 CALL STUPID ('Error occured while reading data from disk file.')
      CALL BYEBYE
      END!
C
C#######################################################################
C
      FUNCTION  TRFM 
     .     (NINDX, STINDX, Q, T, X, Y, R, S, NTERM, ITERM, COEFF, TERM)
      IMPLICIT NONE
      REAL TRFM
      INTEGER MPTERM
      PARAMETER (MPTERM=10)
      INTEGER JTERM, KINDX, NINDX, NTERM, ITERM(MPTERM,*)
      DOUBLE PRECISION PRDCT, SUM, TERM(*)
      REAL Q, T, X, Y, R, S, STINDX(*), COEFF(*)
      SUM=0.D0
C
      DO 3130 JTERM=1,NTERM
C
C Compute the value of the term.
C
C First, figure in the airmass and time of the observation.
C
         PRDCT=1.D0
         IF (ITERM(JTERM,NINDX+1) .NE. 0) PRDCT=PRDCT*
     .        DBLE(Q)**ITERM(JTERM,NINDX+1)
         IF (ITERM(JTERM,NINDX+2) .NE. 0) PRDCT=PRDCT*
     .        DBLE(T)**ITERM(JTERM,NINDX+2)
         IF (ITERM(JTERM,NINDX+3) .NE. 0) PRDCT=PRDCT*
     .        DBLE(X)**ITERM(JTERM,NINDX+3)
         IF (ITERM(JTERM,NINDX+4) .NE. 0) PRDCT=PRDCT*
     .        DBLE(Y)**ITERM(JTERM,NINDX+4)
         IF (ITERM(JTERM,NINDX+5) .NE. 0) PRDCT=PRDCT*
     .        DBLE(R)**ITERM(JTERM,NINDX+5)
         IF (ITERM(JTERM,NINDX+6) .NE. 0) PRDCT=PRDCT*
     .        DBLE(S)**ITERM(JTERM,NINDX+6)
C
C Now include each of the standard photometric indices in the present
C term.
C
         DO 3120 KINDX=1,NINDX
            IF (ITERM(JTERM,KINDX) .NE. 0) PRDCT=
     .           PRDCT*DBLE(STINDX(KINDX))**ITERM(JTERM,KINDX)
 3120    CONTINUE                          ! End of loop over indices
C
         TERM(JTERM)=PRDCT
         SUM=SUM+DBLE(COEFF(JTERM))*PRDCT
 3130 CONTINUE                             ! End of loop over terms
      TRFM=SNGL(SUM)
      RETURN
      END!
C
C#######################################################################
C
      SUBROUTINE  READTFM (NINDX, NMAG, MAGNDX, NTERM, 
     .     ITERM, COEFF, FIX, COSMIC, CLOUD, ZERO, FIXZER)
      
C
C=======================================================================
C
C Read in the transformation equations and any coefficient values.  
C
      IMPLICIT NONE
      INTEGER MPTERM, MAXNDX, MAXMAG, NEXTRA, MFRAME
      PARAMETER (MFRAME=2000, MPTERM=10, MAXNDX=6, MAXMAG=6, NEXTRA=6)
      INTEGER ITERM(MPTERM,MAXNDX+NEXTRA,MAXMAG), NTERM(MAXMAG)
      INTEGER MAGNDX(MAXMAG,MAXNDX)
      INTEGER I, J, K, L, NCHR, NINDX, NMAG, ISTAT
      REAL COEFF(MPTERM,MAXMAG), COSMIC(MAXMAG), ZERO(MFRAME,MAXMAG)
      LOGICAL USE(MPTERM,MAXMAG), FIX(MPTERM,MAXMAG)
      LOGICAL CLOUD, FIXZER(MFRAME,MAXMAG)
      CHARACTER LINE*133, SGN*1, CHR*1
C
C Initialize the relevant arrays.
C
      CLOUD=.FALSE.
      DO I=1,MAXMAG
         COSMIC(I)=0.
         NTERM(I)=0
         DO J=1,MAXNDX
            MAGNDX(I,J)=0
         END DO
C
         DO J=1,MPTERM
            COEFF(J,I)=0.
            FIX(J,I)=.TRUE.
            DO K=1,MAXNDX+NEXTRA
               ITERM(J,K,I)=0
            END DO
         END DO
C
         DO J=1,MFRAME
            ZERO(J,I)=0.
            FIXZER(J,I)=.FALSE.
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
C     (1) On = Mn + functions of (xn, In, Q, T, X, Y, R, S)
C     (2) In = functions of (Mn)
C     (3) Sn = numerical constant
C     (4) xn = numerical constant
C
C These lines define, respectively:
C
C     (1) A transformation equation, defining the n-th observed 
C         magnitude as a function of the n-th standard magnitude
C         plus a transformation equation consisting of terms
C         involving products of the standard photometric indices,
C         the airmass (Q) and time (T) of the observation,
C         the (X,Y) position of the star on the chip, and the
C         R = Q cos (Az) and S = Q sin (Az) of the observation.
C
C     (2) The n-th standard photometric index, expressed as sums
C         and/or differences of the various standard magnitudes.
C
C     (3) The "nightly" standard error in the determination of the
C         n-th magnitude, over and above the photon and read noise.
C
C     (4) One of the coefficients in one of the transformation
C         equations - a real number.
C
C The numerical quantities defining all of these things will be
C stored in the arrays
C
C     (1) ITERM (term, index, Q, T, X, Y, R, S > magnitude)
C     (2) MAGNDX (magnitude > index)
C     (3) COSMIC (magnitude)
C     (4) COEFF (term = value)
C
C respectively.  Note that in every case, the first non-blank
C character in the input will be a letter of the alphabet, the
C second one a numerical digit.  The logical array
C
C         FIX (term, magnitude)
C
C will be .TRUE. if COEFF(term, magnitude) has been defined in
C fashion (4) or if COEFF(term, magnitude) does not appear in any
C equation of type (1), .FALSE. if COEFF(term, magnitude) does appear
C in some equation of type (1) but is not defined in fashion (4).
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
      NINDX = 0
 1000 CALL RDCHAR (1, LINE, NCHR, ISTAT)
      IF (ISTAT .GT. 0) GO TO 9000
      IF (NCHR .LT. 5) GO TO 9300
C
      IF (LINE(1:5) .EQ. 'CLOUD') THEN
         CLOUD=.TRUE.
         WRITE (2,*) 'CLOUD'
         WRITE (3,320) 'CLOUD'
         GO TO 1000
      END IF
C
C Place a colon at the end of the input to serve as a terminator.
C
      NCHR=NCHR+1
      LINE(NCHR:NCHR)=':'
C
C Get the first non-blank character from the input line, determining 
C from it whether a transformation equation ("O"), 
C an index ("I"), a standard error ("S") or a coefficient ("A" to "F") 
C is being defined.
C
      L=1
      SGN=CHR(LINE,L)
      IF (SGN .EQ. 'I') THEN
         CALL ILINE (LINE, L, NMAG, NINDX, MAGNDX)
C
      ELSE IF (SGN .EQ. 'O') THEN
         CALL OLINE (LINE, L, NMAG, NINDX, NTERM, USE, ITERM, FIX)
C
      ELSE IF (SGN .EQ. 'S') THEN
         CALL SLINE (LINE, L, COSMIC)
C
      ELSE
         L=1
         CALL CLINE (LINE, L, COEFF, FIX)
      END IF
C
C This instruction has been confirmed to be grammatically correct and
C legible.  Copy it verbatim into the output files.
C
      WRITE (3,320) LINE(1:NCHR-1)
  320 FORMAT (A)
      WRITE (2,320) ' '//LINE(1:NCHR-1)
      GO TO 1000
 9000 CONTINUE
C
C AT THIS POINT:
C
C ISTMAG(INDEX,JMAG) contains the number of times that the INDEX-th 
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
C FIX(JTERM,JMAG) tells whether the coefficient has been externally
C     determined.
C
      RETURN
C
 9300 WRITE (6,7) LINE(1:NCHR)
    7 FORMAT (/' Incomplete line:'// 1X, A)
      CALL OOPS
      END!
