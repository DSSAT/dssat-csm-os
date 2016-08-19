FC = ifort
FFLAGS =  -nowarn -traceback
INC = -I.. -I../ttutil
OBJECTS = $(patsubst %.f90, %.obj, $(wildcard *.f90))
all: $(OBJECTS)

%.obj: %.f90
	$(FC) -c $(FFLAGS) $(INC) $^

clean:
	del $(TARGET) $(OBJECTS) 
