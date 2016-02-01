FC = ifort
FFLAGS =  -nowarn -g -std95 -traceback
TARGET = DSCSM046.EXE

OBJECTS = $(patsubst %.f90, %.o, $(wildcard *.f90)) $(patsubst %.for, %.o, $(wildcard *.for)) $(patsubst %.FOR, %.o, $(wildcard *.FOR)) $(patsubst %.F90, %.o, $(wildcard *.F90))

all: modules dirs $(TARGET)

$(TARGET): $(OBJECTS)
	$(FC) -o $@ SALUS/*.o ttutil/*.o ORYZA/*.o OP_OBS/*.o $^ $(FFLAGS)

%.o: %.f90 $(MODULES)
	$(FC) -c $(FFLAGS) $^ -o $@

%.o: %.F90 $(MODULES)
	$(FC) -c $(FFLAGS) $^ -o $@

%.o: %.for $(MODULES)
	$(FC) -c $(FFLAGS) $^ -o $@

%.o: %.FOR $(MODULES)
	$(FC) -c $(FFLAGS) $^ -o $@

dirs:
	cd SALUS; make
	cd ttutil; make
	cd ORYZA; make
	cd OP_OBS; make

clean:
	$(RM) $(TARGET) $(OBJECTS) *.mod
	cd SALUS; make clean
	cd ttutil; make clean
	cd ORYZA; make clean
	cd OP_OBS; make clean

modules:
	ifort -fixed -c ModuleDefs.for
	ifort -fixed -c OPHEAD.for
	ifort -fixed -c SoilMixing.for
	ifort -fixed -c SLigCeres.for
	ifort -fixed -c OPSUM.for
	ifort -fixed -c SC_CNG_mods.for
	ifort -fixed -c IPSOIL.for
