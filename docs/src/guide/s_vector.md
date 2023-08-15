## Introduction

It provides a lot of subroutines to manupulate the vectors.

## Type

subroutines

## Source

`src/s_vector.f90`

## Usage

(1) Mesh generation

```fortran
subroutine s_linspace_i(xmin, xmax, n, x)
subroutine s_linspace_d(xmin, xmax, n, x)
subroutine s_linspace_z(xmin, xmax, n, x)
```

To create a linear mesh `x` in interval [`xmin`, `xmax`], `n` is size of array `x`.

(2) Calculate sum of vector.

```fortran
subroutine s_cumsum_i(n, v, vsum)
subroutine s_cumsum_d(n, v, vsum)
subroutine s_cumsum_z(n, v, vsum)
```

Return the cumsum of an array `v`. `n` is size of array `v`, and `vsum` is cumsum of array `v`.

(3) Calculate product of vector.

```fortran
subroutine s_cumprod_i(...)
subroutine s_cumprod_d(...)
subroutine s_cumprod_z(...)
```

(4) Swap two vectors.

```fortran
subroutine s_swap_i(...)
subroutine s_swap_d(...)
subroutine s_swap_z(...)
```

(5) Linear mixing for vectors.

```fortran
subroutine s_mix_i(...)
subroutine s_mix_d(...)
subroutine s_mix_z(...)
```

(6) Convert diagonal elements of matrix to vector.

```fortran
subroutine s_vecadd_i(...)
subroutine s_vecadd_d(...)
subroutine s_vecadd_z(...)
```
