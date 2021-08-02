## Introduction

In this module, we implement some basic sparse matrix algebra. Now it supports double precision real and complex numbers.

## Type

module

## Source

`src/m_sparse.f90`

## Usage

(1) Import sparse module support.

```fortran
use sparse
```

(2) Convert normal matrix to sparse matrix.

```fortran
call sp_csr_to_dns(...)
```

(3) Convert sparse matrix to normal matrix.

```fortran
call sp_dns_to_csr(...)
```

(4) Perform sparse matrix - vector multiplication.

```fortran
call sp_csr_mv_vec(...)
```

(5) Perform sparse matrix - matrix multiplication.

```fortran
call sp_csr_mm_csr(...)
```

Specifically, if one of the matrix is diagonal matrix, then you can use

```fortran
call sp_dia_mm_csr(...)
```

or

```fortran
call sp_csr_mm_dia(...)
```

to accelerate the execution.
