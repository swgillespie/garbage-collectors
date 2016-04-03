CC := gcc
AR := ar

INCLUDEDIR := .
CFLAGS := -std=c99 -Wall -Werror -Wextra -pedantic -Wshadow -Wpointer-arith \
          -Wstrict-prototypes -Wmissing-prototypes -I$(INCLUDEDIR) -fPIC -g -DDEBUG
LIBNAME := libshorty_runtime.a


SOURCES := $(shell find . -name "*.c")
OBJECTS := $(SOURCES:.c=.o)

all: $(LIBNAME)

$(LIBNAME): $(OBJECTS)
	$(AR) rcs $(LIBNAME) $(OBJECTS)

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

clean:
	rm -f $(LIBNAME)
	rm -f $(OBJECTS)
	rm -f $(shell find . -name *~)
