FC = ifort
FFLAGS =  -nowarn -std95 -g -traceback
INC = -I..
OBJECTS = $(patsubst %.for, %.o, $(wildcard *.for)) $(patsubst %.f90, %.o, $(wildcard *.f90)) $(patsubst %.FOR, %.o, $(wildcard *.FOR))
all: outdat.o ttutilprefs.o $(OBJECTS)

%.o: %.for 
	$(FC) -c $(FFLAGS) $(INC) $^

%.o: %.f90
	$(FC) -c $(FFLAGS) $(INC) $^

%.o: %.FOR
	$(FC) -c $(FFLAGS) $(INC) $^

outdat.o: 
	$(FC) -c $(FFLAGS) $(INC) outdat.f90

ttutilprefs.o:
	$(FC) -c $(FFLAGS) $(INC) ttutilprefs.f90

clean:
	rm $(TARGET) $(OBJECTS) *.mod
