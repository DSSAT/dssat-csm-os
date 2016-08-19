FC = ifort
FFLAGS =  -nowarn -traceback
TARGET = DSCSM046.EXE

OBJECTS = $(patsubst %.f90, %.obj, $(wildcard *.f90)) $(patsubst %.for, %.obj, $(wildcard *.for)) $(patsubst %.FOR, %.obj, $(wildcard *.FOR)) $(patsubst %.F90, %.obj, $(wildcard *.F90))

all: modules dirs $(TARGET)

$(TARGET): $(OBJECTS)
	$(FC) -o $@ SALUS/*.obj ttutil/*.obj ORYZA/*.obj OP_OBS/*.obj $^ $(FFLAGS)
	
#	cd SALUS& $(FC) -o $@ *.obj $^ $(FFLAGS)
#	cd ttutil& $(FC) -o $@ *.obj $^ $(FFLAGS)
#	cd ORYZA& $(FC) -o $@ *.obj $^ $(FFLAGS)
#	cd OP_OBS& $(FC) -o $@ *.obj $^ $(FFLAGS)

%.obj: %.f90 $(MODULES)
	$(FC) -c $(FFLAGS) $^ -o $@

%.obj: %.F90 $(MODULES)
	$(FC) -c $(FFLAGS) $^ -o $@

%.obj: %.for $(MODULES)
	$(FC) -c $(FFLAGS) $^ -o $@

%.obj: %.FOR $(MODULES)
	$(FC) -c $(FFLAGS) $^ -o $@

dirs:
	cd SALUS& make
	cd ttutil& make
	cd ORYZA& make
	cd OP_OBS& make

clean:
	$(RM) $(TARGET) $(OBJECTS) *.mod
	cd SALUS& make clean
	cd ttutil& make clean
	cd ORYZA& make clean
	cd OP_OBS& make clean

modules:
	ifort -fixed -c ModuleDefs.for
	ifort -fixed -c OPHEAD.for
	ifort -fixed -c SoilMixing.for
	ifort -fixed -c SLigCeres.for
	ifort -fixed -c OPSUM.for
	ifort -fixed -c SC_CNG_mods.for
	ifort -fixed -c IPSOIL.for
