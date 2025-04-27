C
C#######################################################################
C
      SUBROUTINE  RMEAN  (X, S, N, A, B, TOL, 
     .     AVE, SIGSQ, RME1, COSMIC, W, SHOW)
C
C X is the vector of input data
C S is the vector of sigma**2
C AVE is the robust mean
C SIGSQ is the square of the standard error of the robust mean
C RME1 is the mean error of unit weight
C COSMIC is the square of the additional "cosmic" error
C W is the vector of fudge weights
C
      IMPLICIT NONE
      REAL X(*), S(*), W(*)
C
      DOUBLE PRECISION SUM, SUMS, SUMW, SUMD, SUMWT, DAVE, DELTA, D
      DOUBLE PRECISION SIG, WT, OLD, DABS, CLAMP
      REAL A, B, TOL, RITER, SNGL
      REAL RME1, COSMIC, AVE, SIGSQ
      INTEGER I, N
      LOGICAL SHOW
C
C-----------------------------------------------------------------------
C
      IF (N .LE. 0) RETURN
      IF (N .EQ. 1) THEN
         AVE=X(1)
         SIGSQ=S(1)
         RME1=1.
         COSMIC=0.
         W(1)=1.
         RETURN
      END IF
C
C First guess.
C
      SUM = 0.D0
      SUMW = 0.D0
      DO I=1,N
         D = 1./(S(I)+COSMIC)
         SUM = SUM + D*X(I)
         SUMW = SUMW + D
      END DO
      DAVE = SUM/SUMW
C
C Loop.
C
      RITER=0.
      OLD=0.
      CLAMP=1.D0
 2000 RITER=RITER+1.
      IF (RITER .GT. 1000.) THEN
         CALL STUPID ('*** RUNAWAY ***')
         DO I=1,N
            WRITE (6,*) I, X(I), SQRT(S(I)), W(I)
         END DO
         CALL STUPID (' ')
         AVE = -1.1E38
         COSMIC = -1.1E38
         RETURN
      END IF
      SUM=0.D0
      SUMD=0.D0
      SUMS=0.D0
      SUMW=0.D0
      SUMWT=0.D0
      DO I=1,N
         SIG=DBLE(SQRT(S(I) + COSMIC))
         D=DBLE(X(I))-DAVE
         W(I)=1./(1. + (DABS(D)/(A*SIG))**B)
         SUMS=SUMS+W(I)*DABS(D/SIG)
         SUMW=SUMW+W(I)
         SUM=SUM+1./SIG**2
         WT=W(I)/SIG**2
         SUMD=SUMD+WT*D
         SUMWT=SUMWT+WT
      END DO
      DELTA=SUMD/SUMWT
      IF (OLD*DELTA .LT. 0.) THEN
         CLAMP=0.5D0*CLAMP
      ELSE
         CLAMP=DMIN1(1.D0, 1.05D0*CLAMP)
      END IF
      DAVE=DAVE+CLAMP*DELTA
      IF (RITER .GT. 990.) WRITE (6,*) DAVE, DELTA, CLAMP, COSMIC
      SIGSQ=1./SUMWT
      OLD=DABS(OLD)
      D = ((DAVE-DELTA)-DAVE)*CLAMP
      IF ((ABS(D) .LE. OLD) .AND. (OLD .LT. TOL)) THEN
         GO TO 9000
      ELSE
         OLD=D
      END IF
      RME1=1.5708*REAL(N)*(SUMS/SUMW)**2/REAL(N-1)
      COSMIC=COSMIC+(RME1-1.)*FLOAT(N)/(SUM*RITER)
      COSMIC=MAX(0.,COSMIC)
      IF (SHOW) PRINT *, DAVE, DELTA, RME1, SQRT(COSMIC)
      GO TO 2000
C
 9000 RME1=1.2533*SUMS*SQRT(REAL(N)/(N-1.))/SUMW
      AVE = SNGL(DAVE)
      RETURN
      END!
C
C#######################################################################
C
      FUNCTION  CHR  (LINE,L)
      CHARACTER CHR*1
C
C Function CHR accepts a character string, and returns the first
C non-blank character at or following position L.  On return, 
C L points to the position following that non-blank character.
C
      CHARACTER LINE*133
      INTEGER L
  100 CHR=LINE(L:L)
      L=L+1
      IF (CHR .EQ. ' ') GO TO 100
C
      RETURN
      END!
C
C#######################################################################
C
      SUBROUTINE  ILINE  (LINE, L, NMAG, NINDX, MAGNDX)
      IMPLICIT NONE
      INTEGER MAXNDX, MAXMAG
      PARAMETER (MAXNDX=6, MAXMAG=6)
      INTEGER MAGNDX(MAXMAG,MAXNDX)
      INTEGER L, INDX, JMAG, NMAG, NINDX
      CHARACTER LINE*133, SIGN*1, CHR*1
C
C A standard index is being defined in terms of standard magnitudes.  
C
      INDX=ICHAR(CHR(LINE,L))-48                         ! Which index?
      IF ((INDX .LT. 1) .OR. (INDX .GT. MAXNDX)) CALL ERROR (LINE,L)
      IF (INDX .GT. NINDX) NINDX = INDX
C
C Look for the equals sign.
C
      IF (CHR(LINE,L) .NE. '=') CALL ERROR (LINE,L)
C
C Determine the first constituent magnitude.
C
      IF (CHR(LINE,L) .NE. 'M') CALL ERROR (LINE,L)
      JMAG=ICHAR(CHR(LINE,L))-48
      IF ((JMAG .LT. 1) .OR. (JMAG .GT. NMAG)) CALL ERROR (LINE,L)
      MAGNDX(JMAG,INDX)=MAGNDX(JMAG,INDX)+1
C
C Now include the rest of the magnitudes, if any.
C
 2130 SIGN=CHR(LINE,L)
      IF (SIGN .EQ. ':') RETURN
      IF ((SIGN .NE. '+') .AND. (SIGN .NE. '-')) CALL ERROR (LINE,L)
      IF (CHR(LINE,L) .NE. 'M') CALL ERROR (LINE,L)
      JMAG=ICHAR(CHR(LINE,L))-48
      IF ((JMAG .LT. 1) .OR. (JMAG .GT. NMAG)) CALL ERROR (LINE,L)
      IF (SIGN .EQ. '+') MAGNDX(JMAG,INDX)=MAGNDX(JMAG,INDX)+1
      IF (SIGN .EQ. '-') MAGNDX(JMAG,INDX)=MAGNDX(JMAG,INDX)-1
      GO TO 2130
C
      END!
C
C#####################################################################
C
      SUBROUTINE OLINE (LINE, L, NMAG, NINDX, NTERM, USE, ITERM, FIX)
      IMPLICIT NONE
      INTEGER MPTERM, MAXNDX, MAXMAG, NEXTRA
      PARAMETER (MPTERM=10, MAXNDX=6, MAXMAG=6, NEXTRA=6)
      INTEGER ITERM(MPTERM,MAXNDX+NEXTRA,MAXMAG), NTERM(MAXMAG)
      INTEGER L, JMAG, JTERM, INDX, NMAG, NINDX
      LOGICAL FIX(MPTERM,MAXMAG), USE(MPTERM,MAXMAG)
      CHARACTER LINE*133, SIGN*1, CHR*1
C
C A transformation equation, defining an observed magnitude in terms 
C of standard magnitudes and indices, is to be read in.
C
C Determine which magnitude is being defined.
C
      JMAG=ICHAR(CHR(LINE,L))-48
      DO JTERM=1,MPTERM
         USE(JTERM,JMAG) = .FALSE.
      END DO
      IF (JMAG .GT. NMAG) RETURN
      IF ((JMAG .LT. 1) .OR. (JMAG .GT. MAXMAG)) CALL ERROR (LINE,L)
      SIGN = CHR(LINE,L)
      IF (SIGN .NE. '=') CALL ERROR (LINE,L)
C
C Confirm that the JMAG-th observational magnitude is being defined
C in terms of the JMAG-th standard magnitude.
C
      SIGN = CHR(LINE,L)
      IF (SIGN .NE. 'M') CALL ERROR (LINE,L)
      SIGN = CHR(LINE,L)
      IF (ICHAR(SIGN)-48 .NE. JMAG) CALL ERROR (LINE,L)
C
C Now count and interpret the remaining terms of the transformation 
C equation.
C
      SIGN=CHR(LINE,L)
      if (sign .eq. ':') return
      IF (SIGN .NE. '+') CALL ERROR (LINE,L)
 1000 CONTINUE
C
C Confirm that the coefficient label is correct (e.g. "A" for JMAG=1,
C "B" for JMAG=2, etc.)
C
      IF (ICHAR(CHR(LINE,L))-64 .NE. JMAG) CALL ERROR (LINE,L)
C
C Get the number of the term and, if the coefficient name is valid,
C set FIX to .FALSE. to indicate that the coefficient is to be solved
C for.
C
      JTERM=ICHAR(CHR(LINE,L))-47
      IF ((JTERM .LT. 1) .OR. (JTERM .GT. MPTERM)) CALL ERROR (LINE,L)
      IF (JTERM .GT. NTERM(JMAG)) NTERM(JMAG)=JTERM
      FIX(JTERM,JMAG) = .FALSE.
      USE(JTERM,JMAG) = .TRUE.
C
 2000 SIGN=CHR(LINE,L)
      IF (SIGN .EQ. ':') RETURN
      IF (SIGN .EQ. '+') GO TO 1000               ! Beginning a new term
      IF (SIGN .NE. '*') CALL ERROR (LINE,L)
C
C Building up the current term by multiplying in indices.
C
      SIGN=CHR(LINE,L)
      IF (SIGN .EQ. 'Q') THEN
         INDX=NINDX+1
      ELSE IF (SIGN .EQ. 'T') THEN
         INDX=NINDX+2
      ELSE IF (SIGN .EQ. 'X') THEN
         INDX=NINDX+3
      ELSE IF (SIGN .EQ. 'Y') THEN
         INDX=NINDX+4
      ELSE IF (SIGN .EQ. 'R') THEN
         INDX=NINDX+5
      ELSE IF (SIGN .EQ. 'S') THEN
         INDX=NINDX+6
      ELSE IF (SIGN .EQ. 'I') THEN
         INDX=ICHAR(CHR(LINE,L))-48
         IF ((INDX .LT. 1) .OR. (INDX .GT. NINDX)) CALL ERROR (LINE,L)
      ELSE
         CALL ERROR (LINE,L)
      END IF
      ITERM(JTERM,INDX,JMAG)=ITERM(JTERM,INDX,JMAG)+1
      GO TO 2000
C
      END!
C
C#######################################################################
C
      SUBROUTINE  SLINE  (LINE, L, COSMIC)
      IMPLICIT NONE
      INTEGER MAXMAG
      PARAMETER (MAXMAG=6)
      INTEGER L, N, JMAG
      REAL COSMIC(MAXMAG)
      CHARACTER LINE*133, SIGN*1, CHR*1
C
C-----------------------------------------------------------------------
C
C The user wants to specify a numerical value for a standard error.
C
      JMAG=ICHAR(CHR(LINE,L))-48
      IF ((JMAG .LT. 1) .OR. (JMAG .GT. MAXMAG)) CALL ERROR (LINE,L)
      IF (CHR(LINE,L) .NE. '=') CALL ERROR (LINE,L)
      N=L
 1000 SIGN=CHR(LINE,N)
      IF (SIGN .NE. ':') GO TO 1000
      N=N-2
C
C At this point, L points at the character following the equals sign.
C                N points at the character preceding the colon.
C     
      READ (LINE(L:N),*,ERR=9600) COSMIC(JMAG)
      COSMIC(JMAG)=COSMIC(JMAG)**2
      RETURN
C
 9600 WRITE (6,6) LINE(1:N)
    6 FORMAT (/' Error decoding numerical constant.'// 1X, A)
      CALL OOPS
      END!
C
C#######################################################################
C
      SUBROUTINE  CLINE  (LINE, L, COEFF, FIX)
      IMPLICIT NONE
      INTEGER MPTERM, MAXMAG
      PARAMETER (MPTERM=10, MAXMAG=6)
      INTEGER L, N, JMAG, JTERM
      REAL COEFF(MPTERM,MAXMAG)
      LOGICAL FIX(MPTERM,MAXMAG)
      CHARACTER LINE*133, SIGN*1, CHR*1
C
C-----------------------------------------------------------------------
C
C The user wants to specify a numerical value for one of the 
C coefficients.
C
      JMAG=ICHAR(CHR(LINE,L))-64
      IF ((JMAG .LT. 1) .OR. (JMAG .GT. MAXMAG)) CALL ERROR (LINE,L)
      JTERM=ICHAR(CHR(LINE,L))-47
      IF ((JTERM .LT. 1) .OR. (JTERM .GT. MPTERM)) CALL ERROR (LINE,L)
      IF (CHR(LINE,L) .NE. '=') CALL ERROR (LINE,L)
      N=L
 1000 SIGN=CHR(LINE,N)
      IF (SIGN .NE. ':') GO TO 1000
      N=N-2
C
C At this point, L points at the character following the equals sign.
C                N points at the character preceding the colon.
C     
      READ (LINE(L:N),*,ERR=9600) COEFF(JTERM,JMAG)
      FIX(JTERM,JMAG)=.TRUE.
      RETURN
C
 9600 WRITE (6,6) LINE(1:N)
    6 FORMAT (/' Error decoding numerical constant.'// 1X, A)
      CALL OOPS
      END!
C
C#######################################################################
C
      SUBROUTINE ERROR (LINE,L)
      IMPLICIT NONE
      INTEGER I, L, N
      CHARACTER LINE*133, CHR*1
      N=L
 1000 IF (CHR(LINE,N) .NE. ':') GO TO 1000
      N=N-2
      WRITE (6,6) LINE(1:N), (' ', I=1,L-1), '^'
    6 FORMAT (/' Error interpreting equation:' //1X, A / 80A1)
      CALL OOPS
      END!
C
C#######################################################################
C
      SUBROUTINE  RDPTFM (NMAG, MAGNDX, NINDX, NPTFM, 
     .     IPTFM, PTFM, COSMIC)
      
C
C=======================================================================
C
C Read in the transformation equations and any coefficient values.  
C
      IMPLICIT NONE
      INTEGER MPTERM, MAXNDX, MAXMAG, NEXTRA, MFRAME
      PARAMETER (MFRAME=400, MPTERM=10, MAXNDX=6, MAXMAG=6, NEXTRA=6)
      INTEGER IPTFM(MPTERM,MAXNDX+NEXTRA,MAXMAG), NPTFM(MAXMAG)
      INTEGER MAGNDX(MAXMAG,MAXNDX)
      INTEGER I, J, K, L, NCHR, NINDX, NMAG, ISTAT
      REAL PTFM(MPTERM,MAXMAG), COSMIC(MAXMAG)
      LOGICAL FIX(MPTERM,MAXMAG), USE(MPTERM,MAXMAG)
      CHARACTER LINE*81, SIGN*1, CHR*1
C
C Initialize the relevant arrays.
C
      DO I=1,MAXMAG
         COSMIC(I)=0.
         NPTFM(I)=0
         DO J=1,MAXNDX
            MAGNDX(I,J)=0
         END DO
C
         DO J=1,MPTERM
            PTFM(J,I)=0.
            DO K=1,MAXNDX+NEXTRA
               IPTFM(J,K,I)=0
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
C     (1) IPTFM (term, index & X & T, magnitude)
C     (2) MAGNDX (magnitude, index)
C     (3) COSMIC (magnitude)
C     (4) ZERO (i, magnitude) = constant; FRAME (i, magnitude) = string
C     (5) PTFM (term, magnitude)
C
C respectively.  Note that in every case, the first non-blank
C character in the input will be a letter of the alphabet, the
C second one a numerical digit.  
C 
C-----------------------------------------------------------------------
C
C Beginning of line-by-line loop.
C
 1000 CALL RDCHAR (2, LINE, NCHR, ISTAT)
      IF (ISTAT .GT. 0) GO TO 9000
      IF (NCHR .LT. 5) GO TO 9300
C
C Place a colon at the end of the input to serve as a terminator.
C
      NCHR=NCHR+1
      LINE(NCHR:NCHR)=':'
C
C Get the first non-blank character from the input line, determining 
C from it whether a transformation equation ("O"), an index ("I"), a 
C standard error ("S") or a coefficient ("P") is being defined.
C
      L=1
      SIGN=CHR(LINE,L)
      IF (SIGN .EQ. 'I') THEN
         CALL ILINE (LINE, L, NMAG, NINDX, MAGNDX)
C
      ELSE IF (SIGN .EQ. 'O') THEN
         CALL OLINE (LINE, L, NMAG, NINDX, NPTFM, USE, IPTFM, FIX)
C
      ELSE IF (SIGN .EQ. 'S') THEN
         CALL SLINE (LINE, L, COSMIC)
C
      ELSE
         L=1
         CALL CLINE (LINE, L, PTFM, FIX)
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
C MAGNDX(JMAG,INDX) contains the number of times that the JMAG-th
C     standard magnitude appears as an additive factor in the sum for
C     the INDX-th standard index.
C
C IPTFM(JTERM,KINDX,JMAG) contains the number of times that the
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
      SUBROUTINE  NDX2MAG  (NDX, NDXE, NINDX, NDXMAG, 
     .     MAG, MAGE, NMAG)
C
C Starting with an array of photometric indices and their standard
C errors, convert these to an array of photometric magnitudes and
C their standard errors, using matrix NDXMAG.
C
      IMPLICIT NONE
      INTEGER MAXNDX
      PARAMETER (MAXNDX=6)
      REAL NDX(*), NDXE(*), MAG(*), MAGE(*)
      INTEGER NINDX, JMAG, NMAG, KINDX
      INTEGER NDXMAG(MAXNDX,*)
      DO 8000 JMAG=1,NMAG
      MAG(JMAG)=0.
      MAGE(JMAG)=0.
      DO KINDX=1,NINDX
         IF (NDXMAG(KINDX,JMAG) .NE. 0) THEN
            IF (NDX(KINDX) .LT. 50.) THEN
               MAG(JMAG)=MAG(JMAG)+NDXMAG(KINDX,JMAG)*NDX(KINDX)
               MAGE(JMAG)=MAGE(JMAG)+(NDXMAG(KINDX,JMAG)**2)*NDXE(KINDX)
            ELSE
               MAG(JMAG) = 99.9999
               MAGE(JMAG) = 99.998
               GO TO 8000
            END IF
         END IF
      END DO
 8000 CONTINUE
      RETURN
      END!
C
C######################################################################
C
      SUBROUTINE  RDLIB (LUN, MAXMAG, NMAG, MAGLBL, 
     .     NSTD, STNDID, STMAG, STDERR)
      IMPLICIT NONE
      CHARACTER LINE*132
      INTEGER I, J, LUN, MAXMAG, NSTD, NMAG
      CHARACTER STNDID(*)*12, WORD*12, MAGLBL(*)*6
      REAL STMAG(MAXMAG,*), STDERR(MAXMAG,*)
C
C Read in the number of magnitudes and their labels.
C
      READ (LUN,210) LINE
  210 FORMAT (A)
      READ (LINE,211) NMAG
  211 FORMAT (I2)
      READ (LINE,212) (MAGLBL(I), I=1,NMAG)
  212 FORMAT (21X, 6(5X, A6, 3X))
      WORD=' magnitudes:'
      IF (NMAG .EQ. 1) WORD=' magnitude: '
      WRITE (6,610) NMAG, WORD
  610 FORMAT (/' Library file contains data for ', I1, A12/)
      WRITE (6,611) (I, MAGLBL(I), I=1,NMAG)
  611 FORMAT (6(2X, '(', I1, ') ', A6, :))
      CALL TBLANK
C
C Read standard-star indices.
C
      I=0
 1020 I=I+1
 1025 READ (LUN, 213, END=1030, ERR=1025) STNDID(I),
     .     (STMAG(J,I), STDERR(J,I), J=1,NMAG)
  213 FORMAT(1X, A12, 9X, 12F7.3)
      DO J=1,NMAG
         IF (ABS(STMAG(J,I)) .GT. 50.) STDERR(J,I) = 9.9999
         STDERR(J,I) = STDERR(J,I)**2
      END DO
      GO TO 1020
C
 1030 NSTD=I-1
      RETURN
      END!
C
C#######################################################################
C
      SUBROUTINE  RDMCH  (LUN, FILE, COEFF, DMAG, SIG, MODE)
      CHARACTER LINE*360
      CHARACTER FILE*(*)
      REAL COEFF(*)
C
      CALL RDCHAR (LUN, LINE, NC, MODE)
      IF (MODE .EQ. 1) RETURN
      IF (MODE .NE. 0) GO TO 9000
      IF (NC .GT. 200) THEN
         READ (LINE,*,ERR=9000) FILE, (COEFF(I), I=1,6),
     .        DMAG, SIG, (COEFF(I), I=7,20)
         MODE = 20
      ELSE IF (NC .GT. 120) THEN
         READ (LINE,*,ERR=9000) FILE, (COEFF(I), I=1,6),
     .        DMAG, SIG, (COEFF(I), I=7,12)
         MODE = 12
      ELSE
         READ (LINE,*,ERR=9000) FILE, (COEFF(I), I=1,6),
     .        DMAG, SIG
         MODE = 6
      END IF
      RETURN
C
 9000 MODE = -1
      RETURN
      END!
C
C#######################################################################
C
      SUBROUTINE  RDINF  (LUN, FILE, IFILT, IHR, MIN, X, AZ, EXP, HJD,
     .     CLB, MCH)
      CHARACTER FILE*(*), CLB*(*), MCH*(*)
      CHARACTER*132 LINE
      DOUBLE PRECISION HJD
  100 CALL RDCHAR (LUN, LINE, IFILT, ISTAT)
      IF (ISTAT .GT. 0) THEN
         FILE = 'END-OF-FILE'
         RETURN
      END IF
      IF (LINE(1:1) .EQ. '#') GO TO 100
      IF ((ISTAT .LT. 0) .OR. (IFILT .LT. 103)) THEN
         CALL STUPID ('Error in input line.')
         WRITE (6,6) LINE(1:IFILT)
    6    FORMAT (A/)
         FILE = 'ERROR'
         RETURN
      END IF

C
      READ (LINE,101,IOSTAT=ISTAT) FILE, IFILT, IHR, MIN, X, AZ,
     .     EXP, HJD, CLB, MCH
  101 FORMAT (1X, A30, I3, I4, I3, F7.3, F7.1,
     .     F8.3, F14.4, 1X, A30, 1X, A30)
      IF (ISTAT .EQ. 0) RETURN
C
      READ (LINE,102,IOSTAT=ISTAT) FILE, IFILT, IHR, MIN, X, 
     .     EXP, HJD, CLB, MCH
  102 FORMAT (1X, A30, I3, I4, I3, F7.3,
     .        F8.3, F14.4, 1X, A30, 1X, A30)
      AZ = 0.
      IF (ISTAT .NE. 0) FILE = 'ERROR'
      RETURN
      END!
C
C#######################################################################
C
      SUBROUTINE  RAD2AD  (RA0, DC0, CD0, SD0, XI, ETA,
     .     IRH, IRM, RS, SIGN, IDD, IDM, DS)
C
      CHARACTER SIGN*1
      DOUBLE PRECISION RA0, DC0, CD0, SD0, PIOH, PIOD
      DOUBLE PRECISION XI, ETA, HRA, HDC, TWOPI
C     DATA    PI / 3.14159 26535 898 D00 /
      DATA TWOPI / 6.28318 53071 796 D00 /
      DATA  PIOH / 2.61799 38779 915 D-1 /      ! radians per hour
      DATA  PIOD / 1.74532 92519 943 D-2 /      ! radians per degree
C
      HDC = CD0 - ETA*SD0
      HRA = DATAN(XI/HDC)
      HDC = DATAN(COS(HRA)*(SD0 + ETA*CD0)/HDC)
c     HRA = DATAN( (XI/CD0)/(1.D0 - ETA*TD0) )
c     HDC = COS(HRA)*(ETA + TD0)/(1.D0 - ETA*TD0)
c     HDC = DATAN(HDC)
      HRA = (HRA+RA0)/PIOH
      HDC = HDC/PIOD
      IRH = IDINT(HRA)
      HRA = 6.D1*(HRA-IRH)
      IRM = IDINT(HRA)
      RS = REAL(6.D1*(HRA-IRM))
C
      IF (HDC .LT. 0.D0) THEN
         SIGN = '-'
         HDC = -HDC
      ELSE
         SIGN = '+'
      END IF
      IDD = IDINT(HDC)
      HDC = 6.D1*(HDC-IDD)
      IDM = IDINT(HDC)
      DS = REAL(6.D1*(HDC-IDM))
C
 3020 IF (RS .GE. 59.995) THEN
         RS = RS - 60.
         IRM = IRM+1
         GO TO 3020
      END IF
 3030 IF (IRM .GE. 60) THEN
         IRM = IRM - 60
         IRH = IRH + 1
         GO TO 3030
      END IF
 3040 IF (IRH .GE. 24) THEN
         IRH = IRH - 24
         GO TO 3040
      ELSE IF (IRH .LT. 0) THEN
         IRH = IRH + 24
         GO TO 3040
      END IF
C
 3050 IF (DS .GE. 59.95) THEN
         DS = DS - 60.
         IDM = IDM + 1
         GO TO 3050
      END IF
 3060 IF (IDM .GE. 60) THEN
         IDM = IDM - 60
         IDD = IDD+1
      END IF
      RETURN
      END!
