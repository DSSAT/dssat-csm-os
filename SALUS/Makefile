FC = ifort
FFLAGS =  -nowarn -std95 -g -traceback
INC = -I..
OBJECTS = $(patsubst %.for, %.o, $(wildcard *.for)) $(patsubst %.f90, %.o, $(wildcard *.f90)) $(patsubst %.FOR, %.o, $(wildcard *.FOR))

all: $(OBJECTS)

%.o: %.for
	$(FC) -c $(FFLAGS) $(INC) $^

%.o: %.f90
	$(FC) -c $(FFLAGS) $(INC) $^

%.o: %.FOR
	$(FC) -c $(FFLAGS) $(INC) $^

clean:
	rm $(TARGET) $(OBJECTS) 
