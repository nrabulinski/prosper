#include<stdio.h>
#include<stdbool.h>
#include<stdint.h>
#include<inttypes.h>

void print_bool(bool b) {
    printf(b ? "true\n" : "false\n");
}

void print_i32(int32_t i) {
    printf("%" PRIi32 "\n", i);
}

void print_u64(uint64_t i) {
    printf("%" PRIu64 "\n", i);
}

void print_f64(double f) {
    printf("%f\n", f);
}