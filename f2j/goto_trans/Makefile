

#  *************
#  *** JAVAB ***
#  ****************************************************
#  *** Copyright (c) 1997                           ***
#  *** Aart J.C. Bik             Indiana University *** 
#  *** All Rights Reserved                          ***
#  ****************************************************
#  *** Please refer to the LICENSE file distributed ***
#  *** with this software for further details on    ***
#  *** the licensing terms and conditions.          ***
#  ***                                              ***
#  *** Please, report all bugs, comments, etc.      ***
#  *** to: ajcbik@extreme.indiana.edu               ***
#  ****************************************************
#  *** Makefile : javab construction
#  ***
#  ***
#  *** Your courtesy in mentioning the use of this bytecode tool
#  *** in any scientific work that presents results obtained
#  *** by using (extensions or modifications of) the tool
#  *** is highly appreciated.


#TARGET = $(HOME)/Bin/javab
TARGET = ../../bin/javab

# Flags
# *****

CC     = gcc
CFLAGS = -c -O -ansi -Wall
LFLAGS = -O

OBJS   = byte.o class.o dump.o main.o symtab.o

# Executable
# **********

$(TARGET): $(OBJS)
	$(CC) $(LFLAGS) -o $(TARGET) $(OBJS) 

# System Program
# **************

main.o : main.c class.h
	$(CC) $(CFLAGS) main.c

# Modules
# *******

symtab.o : symtab.c symtab.h
	$(CC) $(CFLAGS) symtab.c

byte.o  : byte.c class.h
	$(CC) $(CFLAGS) byte.c

class.o : class.c class.h
	$(CC) $(CFLAGS) class.c

dump.o  : dump.c class.h
	$(CC) $(CFLAGS) dump.c

# Cleanup
# *******

clean:
	rm -f $(OBJS)

realclean:  clean
	rm -f $(TARGET)
