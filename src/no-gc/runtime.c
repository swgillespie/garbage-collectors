#include "runtime.h"
#include <stdio.h>
#include <limits.h>
#include <string.h>

void shorty_runtime_init() {
  // don't actually need to do anything
  debug_write("initializing shorty runtime\n");
  debug_write("shorty_type_data size: %zu\n", sizeof(struct shorty_type_data));
}

void* shorty_gc_malloc(size_t size, struct shorty_type_data *rtti) {
  if (size > SIZE_MAX - sizeof(void*)) {
    debug_write("allocation of size %zu failed, too large", size);
    return NULL;
  }

  size_t true_size = size + sizeof(void*);
  struct allocation *alloc = malloc(true_size);
  alloc->type_info = rtti;
  memset(alloc->data, 0, size);
  return alloc->data;
}

void shorty_print_int(int data) {
  printf("%d\n", data);
  fflush(stdout);
}

void shorty_print_string(char* str) {
  printf("%s\n", str);
  fflush(stdout);
}
