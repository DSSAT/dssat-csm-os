FC = ifort
FFLAGS =  -nowarn -std95 -g -traceback
INC = -I.. -I../ttutil
OBJECTS = $(patsubst %.f90, %.o, $(wildcard *.f90)) $(patsubst %.for, %.o, $(wildcard *.for))
all: VARIABLE.o OR_ROOTG.o $(OBJECTS)

%.o: %.f90
	$(FC) -c $(FFLAGS) $(INC) $^

%.o: %.for
	$(FC) -c $(FFLAGS) $(INC) $^


VARIABLE.o:
	$(FC) -c $(FFLAGS) $(INC) VARIABLE.f90

OR_ROOTG.o:
	$(FC) -c $(FFLAGS) $(INC) OR_ROOTG.f90

clean:
	rm $(TARGET) $(OBJECTS) *.mod
