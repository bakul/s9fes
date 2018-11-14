/*
 * FFI: conversion functions between bignums and various C int types
 */
cell			make_ulong_integer(unsigned long long i);
cell			make_long_integer(long long i);
long			int32_value(char *src, cell x);
unsigned long		uint32_value(char *src, cell x);
long long		int64_value(char *src, cell x);
unsigned long long	uint64_value(char *src, cell x);
