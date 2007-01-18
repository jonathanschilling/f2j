

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


include make.def

TARGET = javab

OBJS   = byte.o class.o dump.o main.o symtab.o

# Executable
# **********

$(TARGET): $(OBJS)
	$(CC) $(CFLAGS) -o $(TARGET) $(OBJS) 

install: $(TARGET)
	install -d -m 755 $(F2J_BINDIR)
	install -m 755 $(TARGET) $(F2J_BINDIR)

# System Program
# **************

main.o : main.c class.h
	$(CC) -c $(CFLAGS) main.c

# Modules
# *******

symtab.o : symtab.c symtab.h
	$(CC) -c $(CFLAGS) symtab.c

byte.o  : byte.c class.h
	$(CC) -c $(CFLAGS) byte.c

class.o : class.c class.h
	$(CC) -c $(CFLAGS) class.c

dump.o  : dump.c class.h
	$(CC) -c $(CFLAGS) dump.c

# Cleanup
# *******

clean:
	rm -f $(OBJS)

realclean:  clean
	rm -f $(TARGET)
