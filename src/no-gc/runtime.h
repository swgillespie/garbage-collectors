/*
 * This file represents the runtime interface of shorty
 * for the "chapter0" exercise. This exercise does not actually
 * provide a real runtime - its "garbage collector" is simply
 * malloc without free and it provides some utility printing
 * methods for shorty code to call into.
 */

#ifndef __RUNTIME_H__
#define __RUNTIME_H__

#include <stdlib.h>
#include <stdint.h>

#ifdef DEBUG
 #define debug_write(...) printf(__VA_ARGS__)
#else
 #define debug_write(...)
#endif

/*
 * The format of the runtime type information provided by
 * the compiler to the runtime. The data here is used extensively
 * by the collector - it contains data that the collector can use
 * to precisely identify the locations of all pointers within a type.
 */
struct shorty_type_data {
  size_t num_pointers;
  size_t* offsets;
  const char* type_name;
};

/*
 * An object allocated by the shorty runtime looks a bit like this:
 *
 *        +--------------------------+
 *        | struct shorty_type_data* |
 *        +--------------------------+
 * ptr -> | data                     |
 *        |                          |
 *        |                          |
 *        +--------------------------+
 *
 * That is, the type information is located 8 bytes behind the pointer
 * returned by shorty_gc_malloc. This structure is the actual "allocation".
 */
struct allocation {
  struct shorty_type_data *type_info;
  uint8_t data[];
};

/*
 * Called by the running program before doing anything else.
 * It's the runtime's job to initialize itself in this function.
 */
void shorty_runtime_init(void);
/*
 * The main entry point into the shorty runtime and one of two
 * runtime functions currently known to the compiler. Allocates
 * "size" bytes on the managed heap and returns a pointer to the
 * start of the allocation */
void* shorty_gc_malloc(size_t size, struct shorty_type_data* type_data);

/*
 * Prints an integer to standard out.
 */
void shorty_print_int(int);

/*
 * Prints a string to standard out.
 */
void shorty_print_string(char*);

#endif // __RUNTIME_H__
