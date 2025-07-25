# Define the Fortran compiler and options
FC = gfortran
FFLAGS = -fimplicit-none -g -fbounds-check
FFLAGS = -fimplicit-none -g -fbounds-check  -Werror=line-truncation


# List of all object files
OBJS = typy.o globals.o initvals.o hydrofnc.o core_tools.o readtools.o debug_tools.o

# Target to build the final executable
all: $(OBJS) main.o
	$(FC) $(FFLAGS) -o testme main.o $(OBJS)

typy.o: typy.f90
	$(FC) $(FFLAGS) -c typy.f90

debug_tools.o: typy.o globals.o debug_tools.f90
	$(FC) $(FFLAGS) -c debug_tools.f90

core_tools.o: typy.o globals.o core_tools.f90
	$(FC) $(FFLAGS) -c core_tools.f90

readtools.o: typy.o debug_tools.o globals.o readtools.f90
	$(FC) $(FFLAGS) -c  readtools.f90


globals.o: typy.o globals.f90
	$(FC) $(FFLAGS) -c globals.f90


initvals.o: typy.o globals.o core_tools.o  initvals.f90
	$(FC) $(FFLAGS) -c initvals.f90

# Rules for building object files
hydrofnc.o: globals.o typy.o hydrofnc.f90
	$(FC) $(FFLAGS) -c hydrofnc.f90


main.o: $(OBJS) main.f90
	$(FC) $(FFLAGS) -c main.f90





# Clean rule to remove compiled files
clean:
	rm *.o *.mod

tar:
	tar -czf nour-`date -I`.tgz *.f90 makefile


