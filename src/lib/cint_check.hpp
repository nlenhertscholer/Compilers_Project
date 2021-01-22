#pragma once

#include <iostream>

extern "C" void cint_add(int lhs, int rhs) {
    int sum;
    if(__builtin_sadd_overflow(lhs, rhs, &sum)) {
        std::cout << "error: signed integer overflow for addition has occurred" << std::endl;
        exit(1);
    }
}

extern "C" void cint_uneg(int val) {
    int res;
    if(__builtin_smul_overflow(val, -1, &res)) {
        std::cout << "error: signed unary negation overflow has occurred" << std::endl;
        exit(1);
    }
}

extern "C" void cint_minus(int lhs, int rhs) {
    int res;
    if(__builtin_ssub_overflow(lhs, rhs, &res)) {
        std::cout << "error: signed integer overflow for subtraction has occurred" << std::endl;
        exit(1);
    }
}

extern "C" void cint_mult(int lhs, int rhs) {
    int res;
    if(__builtin_smul_overflow(lhs, rhs, &res)) {
        std::cout << "error: signed integer overflow for multiplication has occurred" << std::endl;
        exit(1);
    }
}

extern "C" void cint_div(int lhs, int rhs) {
    int res;
    if(rhs == 0 || (rhs == -1 && __builtin_smul_overflow(lhs, -1, &res))) {
        std::cout << "error: division by zero or INT_MIN / -1 has occurred" << std::endl;
        exit(1);
    }
}