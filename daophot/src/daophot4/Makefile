F77 = gfortran
FFLAGS = -c -O2 -mcmodel=large
LFLAGS = -O2 -Wall -Wsurprising -defsym,mem_=0 -fbounds-check
HOSTLIBS = -lm 
FITLIB = ../cfitsio/libcfitsio.a

# RULES:
.SUFFIXES: .o .f
.f.o:
	$(F77) $(FFLAGS) $<

daophot: daophot.o pckpsf.o find.o fotometry.o \
        psf.o peak.o nstar.o fudge.o addstar.o substar.o \
        group.o sort.o lnxsubs.o fitsubs.o iosubs.o mathsubs.o
	$(F77) $(LFLAGS) -o daophot daophot.o pckpsf.o find.o fotometry.o \
        psf.o peak.o nstar.o fudge.o addstar.o substar.o \
        group.o sort.o lnxsubs.o fitsubs.o \
	iosubs.o mathsubs.o \
        $(IMFORT) $(HOSTLIBS) $(FITLIB)

allstar: allstar.o allstsubs.o lnxsubs.o \
	iosubs.o mathsubs.o fitsubs.o
	$(F77) $(LFLAGS) -o allstar allstar.o allstsubs.o \
	lnxsubs.o iosubs.o mathsubs.o fitsubs.o \
	$(IMFORT) $(HOSTLIBS) $(FITLIB)

daomaster: daomaster.o iosubs.o mathsubs.o lnxsubs.o dummysm.o
	$(F77) $(LFLAGS) -o daomaster daomaster.o iosubs.o mathsubs.o lnxsubs.o dummysm.o

daomatch: daomatch.o iosubs.o mathsubs.o lnxsubs.o
	$(F77) $(LFLAGS) -o daomatch daomatch.o iosubs.o mathsubs.o lnxsubs.o

montage2: montage2.o mathsubs.o iosubs.o lnxsubs.o fitsubs.o
	$(F77) $(LFLAGS) -o montage2 montage2.o mathsubs.o iosubs.o lnxsubs.o fitsubs.o \
        $(HOSTLIBS) $(IMFORT) $(FITLIB)

allframe: allframe.o fitsubs.o lnxsubs.o iosubs.o mathsubs.o
	$(F77) $(LFLAGS) -o allframe allframe.o fitsubs.o \
	lnxsubs.o iosubs.o mathsubs.o \
        $(IMFORT) $(HOSTLIBS) $(FITLIB)

daogrow: daogrow.o iosubs.o mathsubs.o lnxsubs.o
	$(F77) $(LFLAGS) -o daogrow daogrow.o iosubs.o mathsubs.o lnxsubs.o

daospec: daospec.o lnxsubs.o iosubs.o mathsubs.o fitsubs.o
	g77 $(LFLAGS) -o daospec daospec.o lnxsubs.o iosubs.o mathsubs.o fitsubs.o \
   $(IMFORT) $(HOSTLIBS) $(FITLIB)  -Bstatic -L/usr/local/lib -lplotsub -ldevices -lutils -Bdynamic /usr/X11R6/lib/libX11.a

