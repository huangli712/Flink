# Sparse matrix

In this module, we implement some basic sparse matrix algebra. Now it supports both double precision real and complex numbers. The internal format of sparse matrix used in this module is CSR (compressed sparse row) format.

## Type

module

## Source

`src/m_sparse.f90`

## Usage

**(1)** Import sparse module support.

```fortran
use sparse
```

**(2)** Subroutines

* **csr_alloc**, allocate memory for sparse matrix.
* **csr_free**, free memory for sparse matrix.
* **csr_print**, print sparse matrix to terminal.
* **csr_dns**, convert sparse matrix to dense matrix.
* **dns_csr**, convert dense matrix to sparse matrix.
* **csr_csr**, copy sparse matrix to sparse matrix.
* **get_csr**, return the elements in sparse matrix.
* **csr_mv**, perform sparse matrix-vector multiplication.
* **csr_mm**, perform sparse matrix-matrix multiplication.
* **csr_md**, perform sparse matrix-diagonal matrix multiplication.
* **csr_dm**, perform diagonal matrix-sparse matrix multiplication.
* **csr_plus**, addition for two sparse matrices.

**(3)** Key parameters

* **nrows**, row dimension of dense matrix.
* **ncols**, column dimension of dense matrix.
* **nnz**, maximum number of nonzero elements allowed.

**(4)** Sparse matrix in CSR format

* Method 1

```fortran
! ia, ja, a, input matrix in compressed sparse row format
integer, intent(in) :: ia(nrows+1)
integer, intent(in) :: ja(nnz)
real(dp), intent(in) :: a(nnz)
```

or

```fortran
! ia, ja, a, input matrix in compressed sparse row format
integer, intent(in) :: ia(nrows+1)
integer, intent(in) :: ja(nnz)
complex(dp), intent(in) :: a(nnz)
```

* Method 2

```fortran
! sparse_t: abstract struct for sparse matrix
type, private, abstract :: sparse_t

    ! nrows: number of rows
    integer :: nrows = 0

    ! ncols: number of columns
    integer :: ncols = 0

    ! nnz: number of non-zero values
    integer :: nnz   = 0

end type sparse_t

! csr_t: sparse matrix in compressed sparse row format
type, private, extends(sparse_t) :: csr_t

    ! rowptr: matrix row pointer
    integer, allocatable :: rowptr(:)

    ! colptr: matrix column pointer
    integer, allocatable :: colptr(:)

end type csr_t

! csr_d: compressed sparse row format, real(dp) version
type, public, extends(csr_t) :: csr_d
    !
    real(dp), allocatable :: V(:)
    !
end type csr_d

! csr_z: compressed sparse row format, complex(dp) version
type, public, extends(csr_t) :: csr_z
    !
    complex(dp), allocatable :: V(:)
    !
end type csr_z
```

Note that only the `csr_d` and `csr_z` structs are public. The `csr_d` struct is for real sparse matrix, while `csr_z` struct is for complex sparse matrix.