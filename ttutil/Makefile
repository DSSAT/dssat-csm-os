FC = ifort
FFLAGS =  -nowarn -traceback
INC = -I..
OBJECTS = $(patsubst %.for, %.obj, $(wildcard *.for)) $(patsubst %.f90, %.obj, $(wildcard *.f90)) $(patsubst %.FOR, %.obj, $(wildcard *.FOR))
all: outdat.obj ttutilprefs.obj $(OBJECTS)

%.obj: %.for 
	$(FC) -c $(FFLAGS) $(INC) $^

%.obj: %.f90
	$(FC) -c $(FFLAGS) $(INC) $^

%.obj: %.FOR
	$(FC) -c $(FFLAGS) $(INC) $^

outdat.obj: 
	$(FC) -c $(FFLAGS) $(INC) outdat.f90

ttutilprefs.obj:
	$(FC) -c $(FFLAGS) $(INC) ttutilprefs.f90

clean:
	del $(TARGET) $(OBJECTS) *.mod
