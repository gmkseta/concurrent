reciprocal: main.o reciprocal.o
	g++ $(CFLAGS) -o reciprocal main.o reciprocal.o

main.o: main.c reciprocal.hpp
	gcc $(CFLAGS) -c main.c

reciprocal.o: reciprocal.cpp reciprocal.hpp
	g++ $(CFLAGS) -c reciprocal.cpp

clean:
	rm -f *.o reciprocal

# OBJECTS		= reciprocal.o
# LIBRARIES	=
# PROGRAMS	= main
#
# .PHONY:         all clean
#
# # Default target: build everything.
# all:            $(OBJECTS) $(LIBRARIES) $(PROGRAMS)
#
# # Clean up build products.
# clean:
# 	rm -f *.o $(LIBRARIES) $(PROGRAMS)
#
# # Additional dependencies.
# main:		reciprocal.o
