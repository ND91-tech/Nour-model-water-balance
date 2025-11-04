# ====== Compiler and Flags ======
FC = gfortran
FFLAGS = -fimplicit-none -g -fbounds-check
FFLAGS += -Werror=line-truncation

# ====== Object File List ======
OBJS = typy.o globals.o core_tools.o debug_tools.o readtools.o tools.o initvals.o hydrofnc.o main.o

# ====== Executable ======
TARGET = nour_model

# ====== Main build rule ======
$(TARGET): $(OBJS)
	@echo " Checking if $(TARGET).exe is running..."
	@tasklist /FI "IMAGENAME eq $(TARGET).exe" 2>NUL | find /I "$(TARGET).exe" >NUL && ( \
		echo "  $(TARGET).exe is currently running — killing it..." && \
		taskkill /IM $(TARGET).exe /F >NUL ) || echo " No running instance found."
	$(FC) $(FFLAGS) -o $(TARGET) $(OBJS)
	@echo " Build successful: $(TARGET).exe ready."

# ====== Compilation rules ======
typy.o: typy.f90
	$(FC) $(FFLAGS) -c typy.f90

globals.o: typy.o globals.f90
	$(FC) $(FFLAGS) -c globals.f90

core_tools.o: typy.o globals.o core_tools.f90
	$(FC) $(FFLAGS) -c core_tools.f90

debug_tools.o: typy.o globals.o core_tools.o debug_tools.f90
	$(FC) $(FFLAGS) -c debug_tools.f90

readtools.o: typy.o globals.o debug_tools.o core_tools.o readtools.f90
	$(FC) $(FFLAGS) -c readtools.f90

tools.o: typy.o globals.o debug_tools.o core_tools.o tools.f90
	$(FC) $(FFLAGS) -c tools.f90

initvals.o: typy.o globals.o tools.o core_tools.o debug_tools.o initvals.f90
	$(FC) $(FFLAGS) -c initvals.f90

hydrofnc.o: typy.o globals.o tools.o core_tools.o debug_tools.o hydrofnc.f90
	$(FC) $(FFLAGS) -c hydrofnc.f90

main.o: typy.o globals.o tools.o initvals.o hydrofnc.o main.f90
	$(FC) $(FFLAGS) -c main.f90

# ====== Clean rule ======
clean:
	@echo " Cleaning build files..."
	-del /Q *.o *.mod $(TARGET).exe 2>nul || true
	@echo " Clean complete."
