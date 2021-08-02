## Introduction

Provide some subroutines to calculate the special functions.

## Type

subroutines

## Source

`src/s_function.f90`

## Usage

(1) Orthogonal polynomial basis.

```fortran
subroutine s_leg_basis(...)
subroutine s_che_basis(...)
subroutine s_svd_basis(...)
subroutine s_svd_point(...)
```

(2) Spheric Bessel function.

```fortran
subroutine s_sph_jl(...)
```

(3) Bernstein polynomial.

```fortran
subroutine s_bezier(...)
```

(4) Some helper functions for s_svd_basis().

```fortran
function s_safe_exp(...)
function s_f_kernel(...)
function s_b_kernel(...)
```

## Theory
