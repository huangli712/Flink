## Introduction

It provides some utility subroutines and functions.

## Type

subroutines

## Source

`src/s_util.f90`

## Usage

(1) C-like assertion.

```fortran
subroutine s_assert(...)
subroutine s_assert2(...)
```

(2) Sort algorithm.

```fortran
subroutine s_sorter(...)
subroutine s_sorter2(...)
subroutine s_qsorter(...)
subroutine s_qscorer(...)
```

The `s_sorter()` and `s_sorter2()` subroutines implement the bubble algorithm, and the `s_qsorter()` subroutine implements the quick sort algorithm. The `s_qscorer()` subroutine is called by the `s_qsorter()` internally. DO NOT call it directly!

(3) Combination algrbra.

```fortran
subroutine s_combination(...)
```

(4) String manipulation.

```fortran
subroutine s_str_upcase(...)
subroutine s_str_lowcase(...)
subroutine s_str_count(...)
subroutine s_str_compress(...)
```

(5) Date time manipulation.

```fortran
subroutine s_time_builder(...)
subroutine s_time_analyzer(...)
```

## Theory
