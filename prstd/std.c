#include<stdio.h>
#include<stdbool.h>
#include<stdint.h>

void print_bool(bool b) {
    printf(b ? "true\n" : "false\n");
}

void print_i32(int32_t i) {
    printf("%d\n", i);
}

void print_f64(double f) {
    printf("%f\n", f);
}