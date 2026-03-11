It provides a lot of subroutines to manipulate the matrix and implement the linear algebra.

## Type

subroutines

## Source

`src/s_matrix.f90`

## Naming Convention

!!! note

    - `_i` means integer version
    - `_d` means real(dp) version
    - `_z` means complex(dp) version
    - `_dg` means real(dp) general version
    - `_zg` means complex(dp) general version
    - `_sy` means real(dp) symmetric version
    - `_he` means complex(dp) Hermitian version

## Usage

### Matrix Construction

```fortran
subroutine s_zeros_i(n, A)
subroutine s_zeros_d(n, A)
subroutine s_zeros_z(n, A)
```

Build a matrix with all elements set to zero.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of matrix (n-by-n) |
| `A` | `integer`/`real(dp)`/`complex(dp)` | `out` | Output matrix filled with zeros |

---

```fortran
subroutine s_ones_i(n, A)
subroutine s_ones_d(n, A)
subroutine s_ones_z(n, A)
```

Build a matrix with all elements set to one.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of matrix (n-by-n) |
| `A` | `integer`/`real(dp)`/`complex(dp)` | `out` | Output matrix filled with ones |

---

```fortran
subroutine s_any_i(n, i, A)
subroutine s_any_d(n, d, A)
subroutine s_any_z(n, z, A)
```

Build a matrix with all elements set to a specified value.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of matrix (n-by-n) |
| `i`/`d`/`z` | `integer`/`real(dp)`/`complex(dp)` | `in` | Initial value for all matrix elements |
| `A` | `integer`/`real(dp)`/`complex(dp)` | `out` | Output matrix filled with the specified value |

---

```fortran
subroutine s_eye_i(n, k, A)
subroutine s_eye_d(n, k, A)
subroutine s_eye_z(n, k, A)
```

Build a matrix with ones on the specified diagonal and zeros elsewhere.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of matrix (n-by-n) |
| `k` | `integer` | `in` | Index of the diagonal. `k=0` refers to the main diagonal, `k>0` refers to a lower diagonal, `k<0` refers to an upper diagonal |
| `A` | `integer`/`real(dp)`/`complex(dp)` | `out` | Output diagonal matrix |

---

```fortran
subroutine s_identity_i(n, A)
subroutine s_identity_d(n, A)
subroutine s_identity_z(n, A)
```

Build an identity matrix (ones on the main diagonal, zeros elsewhere).

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of matrix (n-by-n) |
| `A` | `integer`/`real(dp)`/`complex(dp)` | `out` | Output identity matrix |

---

```fortran
subroutine s_diag_i(n, v, A)
subroutine s_diag_d(n, v, A)
subroutine s_diag_z(n, v, A)
```

Build a diagonal matrix from a vector, placing the vector elements on the main diagonal.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of matrix and vector (n-by-n matrix, n-element vector) |
| `v` | `integer`/`real(dp)`/`complex(dp)` | `in` | Input vector of diagonal elements |
| `A` | `integer`/`real(dp)`/`complex(dp)` | `out` | Output diagonal matrix |

## Matrix Query

```fortran
subroutine s_trace_d(n, A, tr)
subroutine s_trace_z(n, A, tr)
```

Calculate the trace (sum of diagonal elements) of a matrix.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of matrix (n-by-n) |
| `A` | `real(dp)`/`complex(dp)` | `in` | Input matrix |
| `tr` | `real(dp)`/`complex(dp)` | `out` | Output trace value |

---

```fortran
subroutine s_det_d(ndim, dmat, ddet)
subroutine s_det_z(ndim, zmat, zdet)
```

Calculate the determinant of a matrix using LU factorization.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `ndim` | `integer` | `in` | Dimension of the matrix |
| `dmat`/`zmat` | `real(dp)`/`complex(dp)` | `inout` | Input matrix; on exit, destroyed and replaced with L and U factors |
| `ddet`/`zdet` | `real(dp)`/`complex(dp)` | `out` | Output determinant value |

---

```fortran
subroutine s_cond_d(n, A, cond, norm_type)
subroutine s_cond_z(n, A, cond, norm_type)
```

Estimate the condition number of a matrix using LAPACK.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Dimension of matrix (must be square) |
| `A` | `real(dp)`/`complex(dp)` | `in` | Input matrix |
| `cond` | `real(dp)` | `out` | Estimated condition number |
| `norm_type` | `character(len=1)` | `in`, optional | Norm type: `'1'` for 1-norm, `'I'` for infinity norm, `'F'` for Frobenius norm. Default: `'1'` |

---

```fortran
subroutine s_rank_d(m, n, A, rank, tol)
subroutine s_rank_z(m, n, A, rank, tol)
```

Estimate the rank of a matrix using SVD. Returns the number of singular values greater than the tolerance threshold.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `m` | `integer` | `in` | Number of rows of matrix A |
| `n` | `integer` | `in` | Number of columns of matrix A |
| `A` | `real(dp)`/`complex(dp)` | `inout` | Input matrix (will be destroyed by SVD) |
| `rank` | `integer` | `out` | Estimated rank of matrix A |
| `tol` | `real(dp)` | `in`, optional | Tolerance for singular value comparison. Default: `1.0e-8` |

---

```fortran
subroutine s_sparsity_ratio_i(n, A, ratio)
subroutine s_sparsity_ratio_d(n, A, ratio)
subroutine s_sparsity_ratio_z(n, A, ratio)
```

Calculate the sparsity ratio (ratio of zero elements) of a matrix.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of matrix (must be square) |
| `A` | `integer`/`real(dp)`/`complex(dp)` | `in` | Input matrix |
| `ratio` | `real(dp)` | `out` | Output sparsity ratio (0.0 to 1.0) |

## Matrix Manipulation

```fortran
subroutine s_inv_d(ndim, dmat)
subroutine s_inv_z(ndim, zmat)
```

Invert a matrix using LAPACK (LU factorization).

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `ndim` | `integer` | `in` | Dimension of the matrix |
| `dmat`/`zmat` | `real(dp)`/`complex(dp)` | `inout` | Input matrix; on exit, replaced with the inverse matrix |

---

```fortran
subroutine s_pinv_d(m, n, A, pinv, tol)
subroutine s_pinv_z(m, n, A, pinv, tol)
```

Compute the Moore-Penrose pseudo-inverse of a general matrix using SVD decomposition: `pinv(A) = V * Σ⁺ * U^T` (or `U^H` for complex).

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `m` | `integer` | `in` | Number of rows of matrix A |
| `n` | `integer` | `in` | Number of columns of matrix A |
| `A` | `real(dp)`/`complex(dp)` | `in` | Input matrix A |
| `pinv` | `real(dp)`/`complex(dp)` | `out` | Output pseudo-inverse matrix (n-by-m) |
| `tol` | `real(dp)` | `in`, optional | Tolerance for singular value cutoff. Default: `1.0e-12` |

## Eigenvalue Problems

```fortran
subroutine s_eig_dg(ldim, ndim, amat, eval, evec)
subroutine s_eig_zg(ldim, ndim, zmat, zeig, zvec)
```

Compute all eigenvalues and eigenvectors of a general matrix.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `ldim` | `integer` | `in` | Leading dimension of matrix |
| `ndim` | `integer` | `in` | Order of the matrix |
| `amat`/`zmat` | `real(dp)`/`complex(dp)` | `in` | Input general matrix |
| `eval`/`zeig` | `real(dp)`/`complex(dp)` | `out` | Output eigenvalues |
| `evec`/`zvec` | `real(dp)`/`complex(dp)` | `out` | Output eigenvectors |

---

```fortran
subroutine s_eigvals_dg(ldim, ndim, amat, eval)
subroutine s_eigvals_zg(ldim, ndim, zmat, zeig)
```

Compute only eigenvalues of a general matrix (no eigenvectors).

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `ldim` | `integer` | `in` | Leading dimension of matrix |
| `ndim` | `integer` | `in` | Order of the matrix |
| `amat`/`zmat` | `real(dp)`/`complex(dp)` | `in` | Input general matrix |
| `eval`/`zeig` | `real(dp)`/`complex(dp)` | `out` | Output eigenvalues |

---

```fortran
subroutine s_eig_sy(ldim, ndim, amat, eval, evec)
subroutine s_eig_he(ldim, ndim, amat, eval, evec)
```

Compute all eigenvalues and eigenvectors of a symmetric/Hermitian matrix.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `ldim` | `integer` | `in` | Leading dimension of matrix |
| `ndim` | `integer` | `in` | Order of the matrix |
| `amat` | `real(dp)`/`complex(dp)` | `in` | Input symmetric/Hermitian matrix |
| `eval` | `real(dp)` | `out` | Output eigenvalues (always real) |
| `evec` | `real(dp)`/`complex(dp)` | `out` | Output orthonormal eigenvectors |

---

```fortran
subroutine s_eigvals_sy(ldim, ndim, amat, eval)
subroutine s_eigvals_he(ldim, ndim, amat, eval)
```

Compute only eigenvalues of a symmetric/Hermitian matrix (no eigenvectors).

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `ldim` | `integer` | `in` | Leading dimension of matrix |
| `ndim` | `integer` | `in` | Order of the matrix |
| `amat` | `real(dp)`/`complex(dp)` | `in` | Input symmetric/Hermitian matrix |
| `eval` | `real(dp)` | `out` | Output eigenvalues (always real) |

---

```fortran
subroutine s_geig_sy(ldim, n, A, B, eval, evec)
subroutine s_geig_he(ldim, n, A, B, eval, evec)
```

Solve the generalized eigenvalue problem `A*x = λ*B*x`, where A and B are symmetric/Hermitian matrices.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `ldim` | `integer` | `in` | Leading dimension of matrices A and B |
| `n` | `integer` | `in` | Order of the matrices A and B |
| `A` | `real(dp)`/`complex(dp)` | `in` | Input matrix A (symmetric/Hermitian) |
| `B` | `real(dp)`/`complex(dp)` | `in` | Input matrix B (symmetric/Hermitian positive-definite) |
| `eval` | `real(dp)` | `out` | Output eigenvalues in ascending order |
| `evec` | `real(dp)`/`complex(dp)` | `out` | Output orthonormal/unitary eigenvectors |

## Linear Equation Solver

```fortran
subroutine s_solve_dg(n, nrhs, A, B)
subroutine s_solve_zg(n, nrhs, A, B)
subroutine s_solve_sy(n, nrhs, A, B)
subroutine s_solve_he(n, nrhs, A, B)
```

Solve a linear system `AX = B`. On exit, `B` is overwritten by the solution matrix `X`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Number of linear equations |
| `nrhs` | `integer` | `in` | Number of right-hand sides |
| `A` | `real(dp)`/`complex(dp)` | `inout` | Coefficient matrix A; on exit, overwritten by L and U factors |
| `B` | `real(dp)`/`complex(dp)` | `inout` | Right-hand side matrix B; on exit, overwritten by solution X |


## Matrix Decomposition

```fortran
subroutine s_svd_dg(m, n, min_mn, amat, umat, svec, vmat)
subroutine s_svd_zg(m, n, min_mn, amat, umat, svec, vmat)
```

Perform the singular value decomposition for a general `m`-by-`n` matrix `A`, where `A = U * Σ * V^T` (or `V^H` for complex).

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `m` | `integer` | `in` | Number of rows of matrix A |
| `n` | `integer` | `in` | Number of columns of matrix A |
| `min_mn` | `integer` | `in` | Minimal value of m and n |
| `amat` | `real(dp)`/`complex(dp)` | `inout` | Input matrix A; on exit, destroyed |
| `umat` | `real(dp)`/`complex(dp)` | `out` | Left vectors U |
| `svec` | `real(dp)` | `out` | Singular values Σ |
| `vmat` | `real(dp)`/`complex(dp)` | `out` | Right vectors V^T (or V^H) |

---

```fortran
subroutine s_qr_d(m, n, min_mn, amat, qmat, rmat)
subroutine s_qr_z(m, n, min_mn, amat, qmat, rmat)
```

Perform QR decomposition for a general `m`-by-`n` matrix `A`, where `A = Q * R`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `m` | `integer` | `in` | Number of rows of matrix A |
| `n` | `integer` | `in` | Number of columns of matrix A |
| `min_mn` | `integer` | `in` | Minimal value of m and n |
| `amat` | `real(dp)`/`complex(dp)` | `inout` | Input matrix A; on exit, destroyed |
| `qmat` | `real(dp)`/`complex(dp)` | `out` | Orthogonal/unitary matrix Q |
| `rmat` | `real(dp)`/`complex(dp)` | `out` | Upper triangular matrix R |

---

```fortran
subroutine s_cholesky_d(n, A, L)
subroutine s_cholesky_z(n, A, L)
```

Perform Cholesky decomposition for a symmetric/Hermitian positive-definite matrix `A`, where `A = L * L^T` (or `L * L^H`).

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Dimension of matrix (must be square) |
| `A` | `real(dp)`/`complex(dp)` | `inout` | Input symmetric/Hermitian positive-definite matrix A |
| `L` | `real(dp)`/`complex(dp)` | `out` | Lower triangular Cholesky factor L |

---

```fortran
subroutine s_lu_d(n, A, L, U, ipiv)
subroutine s_lu_z(n, A, L, U, ipiv)
```

Perform LU decomposition for a general matrix `A`, where `A = P * L * U`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Dimension of matrix (must be square) |
| `A` | `real(dp)`/`complex(dp)` | `inout` | Input matrix A; on exit, destroyed |
| `L` | `real(dp)`/`complex(dp)` | `out` | Lower triangular matrix L (unit diagonal) |
| `U` | `real(dp)`/`complex(dp)` | `out` | Upper triangular matrix U |
| `ipiv` | `integer` | `out` | Pivot indices representing permutation P |

---

```fortran
subroutine s_schur_d(ldim, n, A, T, Q)
subroutine s_schur_z(ldim, n, A, T, Q)
```

Perform Schur decomposition for a general matrix `A`, where `A = Q * T * Q^T` (or `Q * T * Q^H`).

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `ldim` | `integer` | `in` | Leading dimension of matrix A |
| `n` | `integer` | `in` | Order of the matrix A |
| `A` | `real(dp)`/`complex(dp)` | `inout` | Input matrix A; on exit, contains Schur form T |
| `T` | `real(dp)`/`complex(dp)` | `out` | Schur form T (block/upper triangular) |
| `Q` | `real(dp)`/`complex(dp)` | `out` | Orthogonal/unitary matrix Q |

## Matrix Property Checking

```fortran
subroutine s_is_symmetric_d(n, A, is_symmetric, tol)
```

Check if a real matrix is symmetric (`A = A^T`).

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of matrix (must be square) |
| `A` | `real(dp)` | `in` | Input matrix |
| `is_symmetric` | `logical` | `out` | `.true.` if matrix is symmetric, `.false.` otherwise |
| `tol` | `real(dp)` | `in`, optional | Tolerance for floating point comparison. Default: `1.0e-8` |

---

```fortran
subroutine s_is_hermitian_z(n, A, is_hermitian, tol)
```

Check if a complex matrix is Hermitian (`A = A^H`).

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of matrix (must be square) |
| `A` | `complex(dp)` | `in` | Input matrix |
| `is_hermitian` | `logical` | `out` | `.true.` if matrix is Hermitian, `.false.` otherwise |
| `tol` | `real(dp)` | `in`, optional | Tolerance for floating point comparison. Default: `1.0e-8` |

---

```fortran
subroutine s_is_skew_symmetric_d(n, A, is_skew_symmetric, tol)
```

Check if a real matrix is skew-symmetric (`A = -A^T`).

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of matrix (must be square) |
| `A` | `real(dp)` | `in` | Input matrix |
| `is_skew_symmetric` | `logical` | `out` | `.true.` if matrix is skew-symmetric, `.false.` otherwise |
| `tol` | `real(dp)` | `in`, optional | Tolerance for floating point comparison. Default: `1.0e-8` |

---

```fortran
subroutine s_is_skew_hermitian_z(n, A, is_skew_hermitian, tol)
```

Check if a complex matrix is skew-Hermitian (`A = -A^H`).

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of matrix (must be square) |
| `A` | `complex(dp)` | `in` | Input matrix |
| `is_skew_hermitian` | `logical` | `out` | `.true.` if matrix is skew-Hermitian, `.false.` otherwise |
| `tol` | `real(dp)` | `in`, optional | Tolerance for floating point comparison. Default: `1.0e-8` |

---

```fortran
subroutine s_is_diagonal_d(n, A, is_diagonal, tol)
subroutine s_is_diagonal_z(n, A, is_diagonal, tol)
```

Check if a matrix is diagonal (all off-diagonal elements are zero).

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of matrix (must be square) |
| `A` | `real(dp)`/`complex(dp)` | `in` | Input matrix |
| `is_diagonal` | `logical` | `out` | `.true.` if matrix is diagonal, `.false.` otherwise |
| `tol` | `real(dp)` | `in`, optional | Tolerance for floating point comparison. Default: `1.0e-8` |

---

```fortran
subroutine s_is_tridiagonal_d(n, A, is_tridiagonal, tol)
subroutine s_is_tridiagonal_z(n, A, is_tridiagonal, tol)
```

Check if a matrix is tridiagonal (only main diagonal and adjacent diagonals can be non-zero).

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of matrix (must be square) |
| `A` | `real(dp)`/`complex(dp)` | `in` | Input matrix |
| `is_tridiagonal` | `logical` | `out` | `.true.` if matrix is tridiagonal, `.false.` otherwise |
| `tol` | `real(dp)` | `in`, optional | Tolerance for floating point comparison. Default: `1.0e-8` |

---

```fortran
subroutine s_is_positive_definite_d(n, A, is_positive_definite)
subroutine s_is_positive_definite_z(n, A, is_positive_definite)
```

Check if a symmetric/Hermitian matrix is positive-definite using Cholesky decomposition.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Dimension of matrix (must be square) |
| `A` | `real(dp)`/`complex(dp)` | `in` | Input matrix (must be symmetric/Hermitian) |
| `is_positive_definite` | `logical` | `out` | `.true.` if matrix is positive-definite, `.false.` otherwise |

---

```fortran
subroutine s_is_positive_semidefinite_d(ldim, n, A, is_positive_semidefinite, tol)
subroutine s_is_positive_semidefinite_z(ldim, n, A, is_positive_semidefinite, tol)
```

Check if a symmetric/Hermitian matrix is positive-semidefinite (all eigenvalues ≥ 0).

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `ldim` | `integer` | `in` | Leading dimension of matrix A |
| `n` | `integer` | `in` | Order of the matrix A |
| `A` | `real(dp)`/`complex(dp)` | `in` | Input matrix (must be symmetric/Hermitian) |
| `is_positive_semidefinite` | `logical` | `out` | `.true.` if matrix is positive-semidefinite, `.false.` otherwise |
| `tol` | `real(dp)` | `in`, optional | Tolerance for eigenvalue comparison. Default: `1.0e-8` |

---

```fortran
subroutine s_is_upper_triangular_d(n, A, is_upper_triangular, tol)
subroutine s_is_upper_triangular_z(n, A, is_upper_triangular, tol)
```

Check if a matrix is upper triangular (all elements below main diagonal are zero).

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of matrix (must be square) |
| `A` | `real(dp)`/`complex(dp)` | `in` | Input matrix |
| `is_upper_triangular` | `logical` | `out` | `.true.` if matrix is upper triangular, `.false.` otherwise |
| `tol` | `real(dp)` | `in`, optional | Tolerance for floating point comparison. Default: `1.0e-8` |

---

```fortran
subroutine s_is_lower_triangular_d(n, A, is_lower_triangular, tol)
subroutine s_is_lower_triangular_z(n, A, is_lower_triangular, tol)
```

Check if a matrix is lower triangular (all elements above main diagonal are zero).

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of matrix (must be square) |
| `A` | `real(dp)`/`complex(dp)` | `in` | Input matrix |
| `is_lower_triangular` | `logical` | `out` | `.true.` if matrix is lower triangular, `.false.` otherwise |
| `tol` | `real(dp)` | `in`, optional | Tolerance for floating point comparison. Default: `1.0e-8` |

---

```fortran
subroutine s_is_singular_d(n, A, is_singular)
subroutine s_is_singular_z(n, A, is_singular)
```

Check if a matrix is singular (determinant is zero) using LU factorization.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of matrix (must be square) |
| `A` | `real(dp)`/`complex(dp)` | `in` | Input matrix |
| `is_singular` | `logical` | `out` | `.true.` if matrix is singular, `.false.` otherwise |

---

```fortran
subroutine s_is_orthogonal_d(n, Q, is_orthogonal, tol)
```

Check if a real matrix is orthogonal (`Q^T * Q = I`).

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of matrix (must be square) |
| `Q` | `real(dp)` | `in` | Input matrix to check |
| `is_orthogonal` | `logical` | `out` | `.true.` if matrix is orthogonal, `.false.` otherwise |
| `tol` | `real(dp)` | `in`, optional | Tolerance for floating point comparison. Default: `1.0e-8` |

---

```fortran
subroutine s_is_unitary_z(n, Q, is_unitary, tol)
```

Check if a complex matrix is unitary (`Q^H * Q = I`).

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|------------- |
| `n` | `integer` | `in` | Size of matrix (must be square) |
| `Q` | `complex(dp)` | `in` | Input matrix to check |
| `is_unitary` | `logical` | `out` | `.true.` if matrix is unitary, `.false.` otherwise |
| `tol` | `real(dp)` | `in`, optional | Tolerance for floating point comparison. Default: `1.0e-8` |

---

```fortran
subroutine s_is_normal_d(n, A, is_normal, tol)
subroutine s_is_normal_z(n, A, is_normal, tol)
```

Check if a matrix is normal (`A^T * A = A * A^T` or `A^H * A = A * A^H`).

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of matrix (must be square) |
| `A` | `real(dp)`/`complex(dp)` | `in` | Input matrix to check |
| `is_normal` | `logical` | `out` | `.true.` if matrix is normal, `.false.` otherwise |
| `tol` | `real(dp)` | `in`, optional | Tolerance for floating point comparison. Default: `1.0e-8` |

## Matrix Extraction and Concatenation

```fortran
subroutine s_extract_submatrix_d(m_src, n_src, A_src, i_start, j_start, m_sub, n_sub, A_sub)
subroutine s_extract_submatrix_z(m_src, n_src, A_src, i_start, j_start, m_sub, n_sub, A_sub)
```

Extract a submatrix from a source matrix.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `m_src` | `integer` | `in` | Number of rows of source matrix |
| `n_src` | `integer` | `in` | Number of columns of source matrix |
| `A_src` | `real(dp)`/`complex(dp)` | `in` | Source matrix |
| `i_start` | `integer` | `in` | Start row index (1-based) |
| `j_start` | `integer` | `in` | Start column index (1-based) |
| `m_sub` | `integer` | `in` | Number of rows of submatrix |
| `n_sub` | `integer` | `in` | Number of columns of submatrix |
| `A_sub` | `real(dp)`/`complex(dp)` | `out` | Extracted submatrix |

---

```fortran
subroutine s_concat_horiz_d(m_A, n_A, A, m_B, n_B, B, m_C, n_C, C)
subroutine s_concat_horiz_z(m_A, n_A, A, m_B, n_B, B, m_C, n_C, C)
```

Concatenate two matrices horizontally: `C = [A | B]`. Matrices A and B must have the same number of rows.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `m_A` | `integer` | `in` | Number of rows of matrix A |
| `n_A` | `integer` | `in` | Number of columns of matrix A |
| `A` | `real(dp)`/`complex(dp)` | `in` | First input matrix |
| `m_B` | `integer` | `in` | Number of rows of matrix B |
| `n_B` | `integer` | `in` | Number of columns of matrix B |
| `B` | `real(dp)`/`complex(dp)` | `in` | Second input matrix |
| `m_C` | `integer` | `in` | Number of rows of concatenated matrix C |
| `n_C` | `integer` | `in` | Number of columns of concatenated matrix C |
| `C` | `real(dp)`/`complex(dp)` | `out` | Concatenated matrix `[A | B]` |

---

```fortran
subroutine s_concat_vert_d(m_A, n_A, A, m_B, n_B, B, m_C, n_C, C)
subroutine s_concat_vert_z(m_A, n_A, A, m_B, n_B, B, m_C, n_C, C)
```

Concatenate two matrices vertically: `C = [A; B]`. Matrices A and B must have the same number of columns.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `m_A` | `integer` | `in` | Number of rows of matrix A |
| `n_A` | `integer` | `in` | Number of columns of matrix A |
| `A` | `real(dp)`/`complex(dp)` | `in` | First input matrix |
| `m_B` | `integer` | `in` | Number of rows of matrix B |
| `n_B` | `integer` | `in` | Number of columns of matrix B |
| `B` | `real(dp)`/`complex(dp)` | `in` | Second input matrix |
| `m_C` | `integer` | `in` | Number of rows of concatenated matrix C |
| `n_C` | `integer` | `in` | Number of columns of concatenated matrix C |
| `C` | `real(dp)`/`complex(dp)` | `out` | Concatenated matrix `[A; B]` |


## Special Matrix Construction

```fortran
subroutine s_upper_triangular_d(n, A)
subroutine s_upper_triangular_z(n, A)
```

Construct an upper triangular matrix by setting all elements below the main diagonal to zero.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of matrix (must be square) |
| `A` | `real(dp)`/`complex(dp)` | `inout` | Input/output matrix |

---

```fortran
subroutine s_lower_triangular_d(n, A)
subroutine s_lower_triangular_z(n, A)
```

Construct a lower triangular matrix by setting all elements above the main diagonal to zero.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of matrix (must be square) |
| `A` | `real(dp)`/`complex(dp)` | `inout` | Input/output matrix |

---

```fortran
subroutine s_tridiagonal_d(n, A)
subroutine s_tridiagonal_z(n, A)
```

Construct a tridiagonal matrix by setting all elements except main diagonal, super-diagonal, and sub-diagonal to zero.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of matrix (must be square) |
| `A` | `real(dp)`/`complex(dp)` | `inout` | Input/output matrix |

---

```fortran
subroutine s_hilbert_d(n, A)
```

Build a Hilbert matrix with elements `H(i,j) = 1 / (i + j - 1)`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of matrix (must be square) |
| `A` | `real(dp)` | `out` | Output Hilbert matrix |

---

```fortran
subroutine s_vandermonde_d(n, x, A)
subroutine s_vandermonde_z(n, x, A)
```

Build a Vandermonde matrix with elements `V(i,j) = x_i^(j-1)`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of matrix (must be square) |
| `x` | `real(dp)`/`complex(dp)` | `in` | Input vector of points |
| `A` | `real(dp)`/`complex(dp)` | `out` | Output Vandermonde matrix |

---

```fortran
subroutine s_pascal_d(n, A)
```

Build a Pascal matrix with elements `P(i,j) = C(i+j-2, j-1)`.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of matrix (must be square) |
| `A` | `real(dp)` | `out` | Output Pascal matrix |

---

```fortran
subroutine s_wilkinson_d(n, A)
```

Build a Wilkinson's W+ matrix. Main diagonal: `W(i,i) = n/2 + 1 - i`, super/sub-diagonal elements equal to 1.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of matrix (must be square) |
| `A` | `real(dp)` | `out` | Output Wilkinson's W+ matrix |

## Random Matrix Construction

```fortran
subroutine s_sparse_random_d(n, sparsity, A)
```

Build a sparse random matrix. The sparsity parameter controls the fraction of non-zero elements (0-1). Uses Fisher-Yates shuffle to guarantee exactly `nnz = int(n*n*sparsity)` non-zero elements.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of matrix (must be square) |
| `sparsity` | `real(dp)` | `in` | Sparsity level (1 = dense, 0 = sparse) |
| `A` | `real(dp)` | `out` | Output sparse random matrix |

---

```fortran
subroutine s_random_symmetric_d(n, A)
subroutine s_random_symmetric_z(n, A)
```

Build a random symmetric matrix `A = B + B^T`, where B is random.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of matrix (must be square) |
| `A` | `real(dp)`/`complex(dp)` | `out` | Output random symmetric matrix |

---

```fortran
subroutine s_random_hermitian_z(n, A)
```

Build a random Hermitian matrix `A = B + B^H`, where B is random complex.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of matrix (must be square) |
| `A` | `complex(dp)` | `out` | Output random Hermitian matrix |

---

```fortran
subroutine s_random_positive_definite_d(n, A)
subroutine s_random_positive_definite_z(n, A)
```

Build a random positive-definite matrix `A = B * B^T + I` (or `B * B^H + I` for complex), where B is random.

**Arguments:**

| Argument | Type | Intent | Description |
|----------|------|-------|-------------|
| `n` | `integer` | `in` | Size of matrix (must be square) |
| `A` | `real(dp)`/`complex(dp)` | `out` | Output random positive-definite matrix |
