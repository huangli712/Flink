## Introduction

It provides some utility subroutines and functions.

## Type

subroutines

## Source

`src/s_util.f90`

## Usage

(1) C-like assertion.

```fortran
subroutine s_assert(condition)
subroutine s_assert2(condition, message)
```

where `condition` is the logical condition that we have to assert, `message` is the additional message.

(2) Sort algorithm.

```fortran
subroutine s_sorter(nsize, list)
subroutine s_sorter2(nsize, list, indx)
subroutine s_qsorter(nsize, list)
subroutine s_qscorer(pstart, pend, nsize, list)
```

The `s_sorter()` and `s_sorter2()` subroutines implement the bubble algorithm, and the `s_qsorter()` subroutine implements the quick sort algorithm. The `s_qscorer()` subroutine is called by the `s_qsorter()` internally. DO NOT call it directly!

Here, `nsize` is size of the list, `list` is the dataset to be sorted. In `s_sorter2()`, the index of the dataset (`indx`) is also sorted.
 
(3) Combination algrbra.

```fortran
subroutine s_combination(ntiny, nlarg, value)
```

```fortran
     ! the small number
     integer, intent(in)  :: ntiny

     ! the large number
     integer, intent(in)  :: nlarg

     ! result value of the combination algebra
     integer, intent(out) :: value
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
