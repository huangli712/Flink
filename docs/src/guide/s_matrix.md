## Introduction

It provides a lot of subroutines to manipulate the matrix and implement the linear algebra.

## Type

subroutines

## Source

`src/s_matrix.f90`

## Usage

(1) Build constants (0) matrix.

```fortran
subroutine s_zeros_i(...)
subroutine s_zeros_d(...)
subroutine s_zeros_z(...)
```

(2) Build constants (1) matrix.

```fortran
subroutine s_ones_i(...)
subroutine s_ones_d(...)
subroutine s_ones_z(...)
```

(3) Build constants (any values) matrix.

```fortran
subroutine s_any_i(...)
subroutine s_any_d(...)
subroutine s_any_z(...)
```

(4) Build diagonal matrix.

```fortran
subroutine s_eye_i(...)
subroutine s_eye_d(...)
subroutine s_eye_z(...)
```

(5) Build identity matrix.

```fortran
subroutine s_identity_i(...)
subroutine s_identity_d(...)
subroutine s_identity_z(...)
```

(6) Build diagonal matrix from vector.

```fortran
subroutine s_diag_i(...)
subroutine s_diag_d(...)
subroutine s_diag_z(...)
```

(7) Calculate trace for matrix.

```fortran
subroutine s_trace_d(...)
subroutine s_trace_z(...)
```

(8) Calculate determinant for matrix.

```fortran
subroutine s_det_d(...)
subroutine s_det_z(...)
```

(9) Calculate matrix inversion.

```fortran
subroutine s_inv_d(...)
subroutine s_inv_z(...)
```

(10) General eigensystem problem.

```fortran
subroutine s_eig_dg(...)
subroutine s_eig_zg(...)
subroutine s_eigvals_dg(...)
subroutine s_eigvals_zg(...)
```

(11) Symmetric eigensystem problem.

```fortran
subroutine s_eig_sy(...)
subroutine s_eig_he(...)
subroutine s_eigvals_sy(...)
subroutine s_eigvals_he(...)
```

(12) Linear equation solver.

```fortran
subroutine s_solve_dg(...)
subroutine s_solve_zg(...)
subroutine s_solve_sy(...)
subroutine s_solve_he(...)
```

(13) General singular value decomposition.

```fortran
subroutine s_svd_dg(...)
subroutine s_svd_zg(...)
```

Note: _i means integer version, _d real(dp) version, and _z complex(dp) version. _dg means real(dp) general version, _zg complex(dp) general version, _sy real(dp) symmetric version, _he complex(dp) Hermitian version.
