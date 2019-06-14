#
#  Makefile to create Project 2
#
COMPILER = ifort
CFLAGS   = -O2 -xHost
LIBS     = -lplplotf95d -lplplotf95cd

# Debug Flags
 CFLAGS = -O0 -traceback -warn interfaces -fpe0 -check all -g

#  Objects to create
#
OBJECTS = kinds.o \
	easyPLplot.o \
	rungeKutta.o \
	odeInt.o \
	astronomicalParams.o \
	rotationMatrix.o


#  Pattern rule(s)
#
%.o : %.f90
	$(COMPILER) $(CFLAGS) -c $<

# GravNbody Target
#
GravNbody: GravNbody.f90 $(OBJECTS)
	$(COMPILER) $(CFLAGS) -o $@ $< $(OBJECTS) $(LIBS)

#  Dependencies
#
kinds.o:

easyPLplot.o: kinds.o

rungeKutta.o: kinds.o

odeInt.o: rungeKutta.o kinds.o

astronomicalParams.o: kinds.o

rotationMatrix.o: kinds.o

# Clean up rule
#
clean:
	rm *.o *.mod

