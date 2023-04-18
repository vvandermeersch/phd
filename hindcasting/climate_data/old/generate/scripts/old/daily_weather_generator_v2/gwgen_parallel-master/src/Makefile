FC = gfortran

#FCFLAGS = -warn all #-traceback -g -check -check noarg_temp_created -fpe0 #debugging

OBJS = parametersmod.o errormod.o coordsmod.o geohashmod.o statsmod.o randomdistmod.o utilitiesmod.o newsplinemod.o weathergenmod.o outputmod.o gwgenmod.o main.o

LDFLAGS = -I/c/msys64/mingw64/include -L/c/msys64/mingw64/lib
#LIBS = -ldcdflib

.SUFFIXES: .o .f90 .f .mod

%.o : %.c
	$(CC) $(CFLAGS) -c -o $(*F).o $(CPPFLAGS) $<

%.o : %.f
	$(FC) $(FCFLAGS) -c -o $(*F).o $(CPPFLAGS) $<

%.o : %.f90
	$(FC) $(FCFLAGS) -c -o $(*F).o $(CPPFLAGS) $<

all ::	weathergen

weathergen:	$(OBJS)
	$(FC) -o weathergen $(OBJS) $(LDFLAGS) $(LIBS)

clean ::	
	-rm weathergen *.mod *.o
