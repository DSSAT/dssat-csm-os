FC = ifort
FFLAGS =  -nowarn -std95 -g -traceback
INC = -I.. -I../ttutil
OBJECTS = $(patsubst %.f90, %.o, $(wildcard *.f90))
all: $(OBJECTS)

%.o: %.f90
	$(FC) -c $(FFLAGS) $(INC) $^

clean:
	rm $(TARGET) $(OBJECTS) 
