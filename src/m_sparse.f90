!!!-----------------------------------------------------------------------
!!! project : flink @ sakura
!!! program : sparse
!!! source  : m_sparse.f90
!!! type    : module
!!! author  : li huang (email:huangli@caep.cn)
!!! history : 02/01/2010 by li huang (created)
!!!           01/08/2025 by li huang (last modified)
!!! purpose : the purpose of this module is to implement important sparse
!!!           matrix/vector operations, including matrix multiplication,
!!!           format conversion, etc. the internal format of sparse matrix
!!!           used in this module is CSR (compressed sparse row) format.
!!! status  : unstable
!!! comment : only support real(dp) and complex(dp) data types
!!!-----------------------------------------------------------------------

  module sparse
     implicit none

!!========================================================================
!!>>> declare local parameters                                         <<<
!!========================================================================

!! module parameters
     ! dp: number precision, double precision for real and complex number
     integer, private, parameter :: dp    = kind(1.0d0)

     ! mystd: device descriptor, console output
     integer, private, parameter :: mystd = 6

!!========================================================================
!!>>> declare global structures                                        <<<
!!========================================================================

     ! sparse_t: abstract struct for sparse matrix
     type, private, abstract :: sparse_t

         ! nrows: number of rows
         integer :: nrows = 0

         ! ncols: ! number of columns
         integer :: ncols = 0

         ! nnz: number of non-zero values
         integer :: nnz   = 0

         ! rowptr: matrix row pointer
         integer, allocatable :: rowptr(:)

         ! colptr: matrix column pointer
         integer, allocatable :: colptr(:)

     end type sparse_t

     ! csr_d: compressed sparse row format, real(dp) version
     type, public, extends(sparse_t) :: csr_d
         real(dp), allocatable :: V(:)
     end type csr_d

     ! csr_z: compressed sparse row format, complex(dp) version
     type, public, extends(sparse_t) :: csr_z
         complex(dp), allocatable :: V(:)
     end type csr_z

!!========================================================================
!!>>> declare accessibility for module routines                        <<<
!!========================================================================

     ! CSR -> allocation
     private :: csr_alloc_d ! real(dp) version
     private :: csr_alloc_z ! complex(dp) version
     !
     private :: csr_alloc_d_t
     private :: csr_alloc_z_t

     ! CSR -> deallocation
     private :: csr_free_d  ! real(dp) version
     private :: csr_free_z  ! complex(dp) version
     !
     private :: csr_free_d_t
     private :: csr_free_z_t

     ! CSR -> print
     private :: csr_print_d ! real(dp) version
     private :: csr_print_z ! complex(dp) version
     !
     private :: csr_print_d_t
     private :: csr_print_z_t

     ! CSR -> DNS (conversion)
     private :: csr_dns_d   ! real(dp) version
     private :: csr_dns_z   ! complex(dp) version
     !
     private :: csr_dns_d_t
     private :: csr_dns_z_t

     ! DNS -> CSR (conversion)
     private :: dns_csr_d   ! real(dp) version
     private :: dns_csr_z   ! complex(dp) version
     !
     private :: dns_csr_d_t
     private :: dns_csr_z_t

     ! CSR -> CSR (copy)
     private :: csr_csr_d   ! real(dp) version
     private :: csr_csr_z   ! complex(dp) version
     !
     private :: csr_csr_d_t
     private :: csr_csr_z_t

     ! CSR -> getter
     private :: get_csr_d   ! real(dp) version
     private :: get_csr_z   ! complex(dp) version
     !
     private :: get_csr_d_t
     private :: get_csr_z_t

     ! CSR -> setter
     private :: set_csr_d   ! real(dp) version
     private :: set_csr_z   ! complex(dp) version
     !
     private :: set_csr_d_t
     private :: set_csr_z_t

     ! CSR X VEC
     private :: csr_mv_d    ! real(dp) version
     private :: csr_mv_z    ! complex(dp) version
     !
     private :: csr_mv_d_t
     private :: csr_mv_z_t

     ! CSR X CSR
     private :: csr_mm_d    ! real(dp) version
     private :: csr_mm_z    ! complex(dp) version
     !
     private :: csr_mm_d_t
     private :: csr_mm_z_t

     ! CSR X DIA
     private :: csr_md_d    ! real(dp) version
     private :: csr_md_z    ! complex(dp) version
     !
     private :: csr_md_d_t
     private :: csr_md_z_t

     ! DIA X CSR
     private :: csr_dm_d    ! real(dp) version
     private :: csr_dm_z    ! complex(dp) version
     !
     private :: csr_dm_d_t
     private :: csr_dm_z_t

!!========================================================================
!!>>> declare interface and module procedure                           <<<
!!========================================================================

     public :: csr_alloc
     interface csr_alloc
         module procedure csr_alloc_d
         module procedure csr_alloc_z
         module procedure csr_alloc_d_t
         module procedure csr_alloc_z_t
     end interface csr_alloc

     public :: csr_free
     interface csr_free
         module procedure csr_free_d
         module procedure csr_free_z
         module procedure csr_free_d_t
         module procedure csr_free_z_t
     end interface csr_free

     public :: csr_print
     interface csr_print
         module procedure csr_print_d
         module procedure csr_print_z
         module procedure csr_print_d_t
         module procedure csr_print_z_t
     end interface csr_print

     public :: csr_dns
     interface csr_dns
         module procedure csr_dns_d
         module procedure csr_dns_z
         module procedure csr_dns_d_t
         module procedure csr_dns_z_t
     end interface csr_dns

     public :: dns_csr
     interface dns_csr
         module procedure dns_csr_d
         module procedure dns_csr_z
         module procedure dns_csr_d_t
         module procedure dns_csr_z_t
     end interface dns_csr

     public :: csr_csr
     interface csr_csr
         module procedure csr_csr_d
         module procedure csr_csr_z
         module procedure csr_csr_d_t
         module procedure csr_csr_z_t
     end interface csr_csr

     public :: get_csr
     interface get_csr
         module procedure get_csr_d
         module procedure get_csr_z
         module procedure get_csr_d_t
         module procedure get_csr_z_t
     end interface get_csr

     public :: set_csr
     interface set_csr
         module procedure set_csr_d
         module procedure set_csr_z
         module procedure set_csr_d_t
         module procedure set_csr_z_t
     end interface set_csr

     public :: csr_mv
     interface csr_mv
         module procedure csr_mv_d
         module procedure csr_mv_z
         module procedure csr_mv_d_t
         module procedure csr_mv_z_t
     end interface csr_mv

     public :: csr_mm
     interface csr_mm
         module procedure csr_mm_d
         module procedure csr_mm_z
         module procedure csr_mm_d_t
         module procedure csr_mm_z_t
     end interface csr_mm

     public :: csr_md
     interface csr_md
         module procedure csr_md_d
         module procedure csr_md_z
         module procedure csr_md_d_t
         module procedure csr_md_z_t
     end interface csr_md

     public :: csr_dm
     interface csr_dm
         module procedure csr_dm_d
         module procedure csr_dm_z
         module procedure csr_dm_d_t
         module procedure csr_dm_z_t
     end interface csr_dm

  contains ! encapsulated functionality

!!
!! @sub csr_alloc_d
!!
!!
!!
  subroutine csr_alloc_d()
  end subroutine csr_alloc_d

!!
!! @sub csr_alloc_z
!!
!!
!!
  subroutine csr_alloc_z(i)
     integer :: i
     i = 0
  end subroutine csr_alloc_z

!!
!! @sub csr_alloc_d_t
!!
!!
!!
  subroutine csr_alloc_d_t(i, j)
     integer :: i, j
     i = 0
     j = 0
  end subroutine csr_alloc_d_t

!!
!! @sub csr_alloc_z_t
!!
!!
!!
  subroutine csr_alloc_z_t(i, j, k)
     integer :: i, j, k
     i = 0
     j = 0
     k = 0
  end subroutine csr_alloc_z_t

!!
!! @sub
!!
!!
!!
  subroutine csr_free_d()
  end subroutine csr_free_d

!!
!! @sub
!!
!!
!!
  subroutine csr_free_z(i)
     integer :: i
     i = 0
  end subroutine csr_free_z

!!
!! @sub
!!
!!
!!
  subroutine csr_free_d_t(i, j)
     integer :: i, j
     i = 0
     j = 0
  end subroutine csr_free_d_t

!!
!! @sub
!!
!!
!!
  subroutine csr_free_z_t(i, j, k)
     integer :: i, j, k
     i = 0
     j = 0
     k = 0
  end subroutine csr_free_z_t

!!
!! @sub
!!
!!
!!
  subroutine csr_print_d()
  end subroutine csr_print_d

!!
!! @sub
!!
!!
!!
  subroutine csr_print_z(i)
     integer :: i
     i = 0
  end subroutine csr_print_z

!!
!! @sub
!!
!!
!!
  subroutine csr_print_d_t(i, j)
     integer :: i, j
     i = 0
     j = 0
  end subroutine csr_print_d_t

!!
!! @sub
!!
!!
!!
  subroutine csr_print_z_t(i, j, k)
     integer :: i, j, k
     i = 0
     j = 0
     k = 0
  end subroutine csr_print_z_t

!!
!! @sub csr_dns_d
!!
!! converts a row-stored sparse matrix into a densely stored one.
!!
  subroutine csr_dns_d(nrows, ncols, nnz, a, ja, ia, dns)
     implicit none

!! external arguments
     ! row dimension of dense matrix
     integer, intent(in)   :: nrows

     ! column dimension of dense matrix
     integer, intent(in)   :: ncols

     ! maximum number of nonzero elements allowed.
     ! this should be set to be the lengths of the arrays a and ja.
     integer, intent(in)   :: nnz

     ! a, ja, ia, input matrix in compressed sparse row format
     integer, intent(in)   :: ia(nrows+1)
     integer, intent(in)   :: ja(nnz)
     real(dp), intent(in)  :: a(nnz)

     ! array where to store dense matrix
     real(dp), intent(out) :: dns(nrows,ncols)

!! local variables
     ! loop index
     integer :: i
     integer :: j
     integer :: k

!! [body

     ! init dns matrix
     dns = 0.0_dp

     ! convert sparse matrix to dense matrix
     do i=1,nrows
         do k=ia(i),ia(i+1)-1
             j = ja(k)
             if ( j > ncols ) then
                 write(mystd,'(a)') 'sparse: error in csr_dns_d'
                 STOP
             endif ! back if ( j > ncols ) block
             dns(i,j) = a(k)
         enddo ! over k={ia(i),ia(i+1)-1} loop
     enddo ! over i={1,nrows} loop

!! body]

     return
  end subroutine csr_dns_d

!!
!! @sub csr_dns_z
!!
!! converts a row-stored sparse matrix into a densely stored one.
!!
  subroutine csr_dns_z(nrows, ncols, nnz, sa, ja, ia, dns)
     implicit none

!! external arguments
     ! row dimension of dense matrix
     integer, intent(in)      :: nrows

     ! column dimension of dense matrix
     integer, intent(in)      :: ncols

     ! maximum number of nonzero elements allowed.
     ! this should be set to be the lengths of the arrays sa and ja.
     integer, intent(in)      :: nnz

     ! sa, ja, ia, input matrix in compressed sparse row format
     integer, intent(in)      :: ia(nrows+1)
     integer, intent(in)      :: ja(nnz)
     complex(dp), intent(in)  :: sa(nnz)

     ! array where to store dense matrix
     complex(dp), intent(out) :: dns(nrows,ncols)

!! local variables
     ! loop index
     integer :: i
     integer :: j
     integer :: k

!! [body

     ! init dns matrix
     dns = dcmplx(0.0_dp, 0.0_dp)

     ! convert sparse matrix to dense matrix
     do i=1,nrows
         do k=ia(i),ia(i+1)-1
             j = ja(k)
             if ( j > ncols ) then
                 write(mystd,'(a)') 'sparse: error in csr_dns_z'
                 STOP
             endif ! back if ( j > ncols ) block
             dns(i,j) = sa(k)
         enddo ! over k={ia(i),ia(i+1)-1} loop
     enddo ! over i={1,nrows} loop

!! body]

     return
  end subroutine csr_dns_z

!!
!! @sub csr_dns_d_t
!!
!! converts a row-stored sparse matrix into a densely stored one.
!!
  subroutine csr_dns_d_t(csr, dns)
     implicit none

!! external arguments
     ! sparse matrix in CSR format
     type (csr_d), intent(in) :: csr

     ! array where to store dense matrix
     real(dp), intent(out) :: dns(csr%nrows,csr%ncols)

!! local variables
     ! loop index
     integer :: i
     integer :: j
     integer :: k

!! [body

     ! init dns matrix
     dns = 0.0_dp

     ! convert sparse matrix to dense matrix
     do i=1,csr%nrows
         do k=csr%rowptr(i),csr%rowptr(i+1)-1
             j = csr%colptr(k)
             if ( j > csr%ncols ) then
                 write(mystd,'(a)') 'sparse: error in csr_dns_d_t'
                 STOP
             endif ! back if ( j > csr%ncols ) block
             dns(i,j) = csr%V(k)
         enddo ! over k={csr%rowptr(i),csr%rowptr(i+1)-1} loop
     enddo ! over i={1,csr%nrows} loop

!! body]

     return
  end subroutine csr_dns_d_t

!!
!! @sub csr_dns_z_t
!!
!! converts a row-stored sparse matrix into a densely stored one.
!!
  subroutine csr_dns_z_t(csr, dns)
     implicit none

!! external arguments
     ! sparse matrix in CSR format
     type (csr_z), intent(in) :: csr

     ! array where to store dense matrix
     complex(dp), intent(out) :: dns(csr%nrows,csr%ncols)

!! local variables
     ! loop index
     integer :: i
     integer :: j
     integer :: k

!! [body

     ! init dns matrix
     dns = dcmplx(0.0_dp, 0.0_dp)

     ! convert sparse matrix to dense matrix
     do i=1,csr%nrows
         do k=csr%rowptr(i),csr%rowptr(i+1)-1
             j = csr%colptr(k)
             if ( j > csr%ncols ) then
                 write(mystd,'(a)') 'sparse: error in csr_dns_z_t'
                 STOP
             endif ! back if ( j > csr%ncols ) block
             dns(i,j) = csr%V(k)
         enddo ! over k={csr%rowptr(i),csr%rowptr(i+1)-1} loop
     enddo ! over i={1,csr%nrows} loop

!! body]

     return
  end subroutine csr_dns_z_t

!!
!! @sub dns_csr_d
!!
!! converts a densely stored matrix into a row orientied compactly
!! sparse matrix.
!!
  subroutine dns_csr_d(nrows, ncols, nnz, dns, a, ja, ia)
     implicit none

!! external arguments
     ! row dimension of dense matrix
     integer, intent(in)   :: nrows

     ! column dimension of dense matrix
     integer, intent(in)   :: ncols

     ! maximum number of nonzero elements allowed.
     ! this should be set to be the lengths of the arrays a and ja.
     integer, intent(in)   :: nnz

     ! input densely stored matrix
     real(dp), intent(in)  :: dns(nrows,ncols)

     ! a, ja, ia, output matrix in compressed sparse row format
     integer, intent(out)  :: ia(nrows+1)
     integer, intent(out)  :: ja(nnz)
     real(dp), intent(out) :: a(nnz)

!! local variables
     ! loop index
     integer :: i
     integer :: j
     integer :: k

!! [body

     ! init sparse matrix
     a = 0.0_dp
     ia = 0
     ja = 0

     k = 1
     ia(1) = 1
     do i=1,nrows
         do j=1,ncols
             if ( dns(i,j) == 0.0_dp ) CYCLE
             ja(k) = j
             a(k) = dns(i,j)
             k = k + 1
             if ( k > nnz ) then
                 write(mystd,'(a)') 'sparse: error in dns_csr_d'
                 STOP
             endif ! back if ( k > nnz ) block
         enddo ! over j={1,ncols} loop
         ia(i+1) = k
     enddo ! over i={1,nrows} loop

!! body]

     return
  end subroutine dns_csr_d

!!
!! @sub dns_csr_z
!!
!! converts a densely stored matrix into a row orientied compactly
!! sparse matrix.
!!
  subroutine dns_csr_z(nrows, ncols, nnz, dns, sa, ja, ia)
     implicit none

!! external arguments
     ! row dimension of dense matrix
     integer, intent(in)      :: nrows

     ! column dimension of dense matrix
     integer, intent(in)      :: ncols

     ! maximum number of nonzero elements allowed.
     ! this should be set to be the lengths of the arrays sa and ja.
     integer, intent(in)      :: nnz

     ! input densely stored matrix
     complex(dp), intent(in)  :: dns(nrows,ncols)

     ! sa, ja, ia, output matrix in compressed sparse row format
     integer, intent(out)     :: ia(nrows+1)
     integer, intent(out)     :: ja(nnz)
     complex(dp), intent(out) :: sa(nnz)

!! local variables
     ! loop index
     integer :: i
     integer :: j
     integer :: k

!! [body

     ! init sparse matrix
     sa = dcmplx(0.0_dp, 0.0_dp)
     ia = 0
     ja = 0

     k = 1
     ia(1) = 1
     do i=1,nrows
         do j=1,ncols
             if ( real( dns(i,j) ) == 0.0_dp .and. &
                & aimag( dns(i,j) ) == 0.0_dp ) CYCLE
             ja(k) = j
             sa(k) = dns(i,j)
             k = k + 1
             if ( k > nnz ) then
                 write(mystd,'(a)') 'sparse: error in dns_csr_z'
                 STOP
             endif ! back if ( k > nnz ) block
         enddo ! over j={1,ncols} loop
         ia(i+1) = k
     enddo ! over i={1,nrows} loop

!! body]

     return
  end subroutine dns_csr_z

  subroutine dns_csr_d_t()
  end subroutine dns_csr_d_t

  subroutine dns_csr_z_t(i)
     integer :: i
     i = 0
  end subroutine dns_csr_z_t

!!
!! @sub csr_csr_d
!!
!! copy data between two row orientied compactly sparse matrices.
!!
  subroutine csr_csr_d(nrows, nnz, a, ja, ia, b, jb, ib)
     implicit none

!! external arguments
     ! row dimension of dense matrix
     integer, intent(in)   :: nrows

     ! maximum number of nonzero elements allowed.
     ! this should be set to be the lengths of the arrays a and ja.
     integer, intent(in)   :: nnz

     ! a, ja, ia, input matrix in compressed sparse row format
     integer, intent(in)   :: ia(nrows+1)
     integer, intent(in)   :: ja(nnz)
     real(dp), intent(in)  :: a(nnz)

     ! b, jb, ib, output matrix in compressed sparse row format
     integer, intent(out)  :: ib(nrows+1)
     integer, intent(out)  :: jb(nnz)
     real(dp), intent(out) :: b(nnz)

!! local variables
     ! loop index
     integer :: i

!! [body

     do i=1,nrows+1
         ib(i) = ia(i)
     enddo ! over i={1,nrows+1} loop

     do i=ia(1),ia(nrows+1)-1
         jb(i) = ja(i)
     enddo ! over i={ia(1),ia(nrows+1)-1} loop

     do i=ia(1),ia(nrows+1)-1
         b(i) = a(i)
     enddo ! over i={ia(1),ia(nrows+1)-1} loop

!! body]

     return
  end subroutine csr_csr_d

!!
!! @sub csr_csr_z
!!
!! copy data between two row orientied compactly sparse matrices.
!!
  subroutine csr_csr_z(nrows, nnz, sa, ja, ia, sb, jb, ib)
     implicit none

!! external arguments
     ! row dimension of dense matrix
     integer, intent(in)      :: nrows

     ! maximum number of nonzero elements allowed.
     ! this should be set to be the lengths of the arrays sa and ja.
     integer, intent(in)      :: nnz

     ! sa, ja, ia, input matrix in compressed sparse row format
     integer, intent(in)      :: ia(nrows+1)
     integer, intent(in)      :: ja(nnz)
     complex(dp), intent(in)  :: sa(nnz)

     ! sb, jb, ib, output matrix in compressed sparse row format
     integer, intent(out)     :: ib(nrows+1)
     integer, intent(out)     :: jb(nnz)
     complex(dp), intent(out) :: sb(nnz)

!! local variables
     ! loop index
     integer :: i

!! [body

     do i=1,nrows+1
         ib(i) = ia(i)
     enddo ! over i={1,nrows+1} loop

     do i=ia(1),ia(nrows+1)-1
         jb(i) = ja(i)
     enddo ! over i={ia(1),ia(nrows+1)-1} loop

     do i=ia(1),ia(nrows+1)-1
         sb(i) = sa(i)
     enddo ! over i={ia(1),ia(nrows+1)-1} loop

!! body]

     return
  end subroutine csr_csr_z

  subroutine csr_csr_d_t()
  end subroutine csr_csr_d_t

  subroutine csr_csr_z_t(i)
     integer :: i
     i = 0
  end subroutine csr_csr_z_t

!!
!! @fun get_csr_d
!!
!! this function returns the element a(i,j) of matrix a.
!!
  real(dp) &
  function get_csr_d(i, j, nrows, nnz, a, ja, ia) result(elm)
     implicit none

!! external arguments
     ! the row index of the element sought
     integer, intent(in)  :: i

     ! the column index of the element sought
     integer, intent(in)  :: j

     ! row dimension of dense matrix
     integer, intent(in)  :: nrows

     ! maximum number of nonzero elements allowed.
     ! this should be set to be the lengths of the arrays a and ja.
     integer, intent(in)  :: nnz

     ! a, ja, ia, input matrix in compressed sparse row format
     integer, intent(in)  :: ia(nrows+1)
     integer, intent(in)  :: ja(nnz)
     real(dp), intent(in) :: a(nnz)

!! local variables
     ! loop index
     integer :: k

     ! memory address of a(i,j)
     integer :: addr

!! [body

     ! initialization
     addr = 0
     elm = 0.0_dp

     ! scan the row - exit as soon as a(i,j) is found
     do k=ia(i),ia(i+1)-1
         if ( ja(k) == j ) then
             addr = k
             EXIT
         endif ! back if ( ja(k) == j ) block
     enddo ! over k={ia(i),ia(i+1)-1} loop

     ! the required element is contained in sparse matrix
     if ( addr /= 0 ) then
         elm = a(addr)
     endif ! back if ( addr /= 0 ) block

!! body]

     return
  end function get_csr_d

!!
!! @fun get_csr_z
!!
!! this function returns the element sa(i,j) of matrix sa.
!!
  complex(dp) &
  function get_csr_z(i, j, nrows, nnz, sa, ja, ia) result(elm)
     implicit none

!! external arguments
     ! the row index of the element sought
     integer, intent(in)     :: i

     ! the column index of the element sought
     integer, intent(in)     :: j

     ! row dimension of dense matrix
     integer, intent(in)     :: nrows

     ! maximum number of nonzero elements allowed.
     ! this should be set to be the lengths of the arrays sa and ja.
     integer, intent(in)     :: nnz

     ! sa, ja, ia, input matrix in compressed sparse row format
     integer, intent(in)     :: ia(nrows+1)
     integer, intent(in)     :: ja(nnz)
     complex(dp), intent(in) :: sa(nnz)

!! local variables
     ! loop index
     integer :: k

     ! memory address of sa(i,j)
     integer :: addr

!! [body

     ! initialization
     addr = 0
     elm = dcmplx(0.0_dp, 0.0_dp)

     ! scan the row - exit as soon as sa(i,j) is found
     do k=ia(i),ia(i+1)-1
         if ( ja(k) == j ) then
             addr = k
             EXIT
         endif ! back if ( ja(k) == j ) block
     enddo ! over k={ia(i),ia(i+1)-1} loop

     ! the required element is contained in sparse matrix
     if ( addr /= 0 ) then
         elm = sa(addr)
     endif ! back if ( addr /= 0 ) block

!! body]

     return
  end function get_csr_z

  real(dp) &
  function get_csr_d_t() result(elm)
     elm = 0.0_dp
  end function get_csr_d_t

  complex(dp) &
  function get_csr_z_t(i) result(elm)
     integer :: i
     i = 0
     elm = dcmplx(0.0_dp, 0.0_dp)
  end function get_csr_z_t

  subroutine set_csr_d()
  end subroutine set_csr_d

  subroutine set_csr_z(i)
     integer :: i
     i = 0
  end subroutine set_csr_z

  subroutine set_csr_d_t(i, j)
     integer :: i, j
     i = 0
     j = 0
  end subroutine set_csr_d_t

  subroutine set_csr_z_t(i, j, k)
     integer :: i, j, k
     i = 0
     j = 0
     k = 0
  end subroutine set_csr_z_t

!!
!! @sub csr_mv_d
!!
!! multiplies a matrix by a vector using the dot product form.
!!
  subroutine csr_mv_d(nrows, ncols, nnz, a, ja, ia, x, y)
     implicit none

!! external arguments
     ! row dimension of dense matrix
     integer, intent(in)   :: nrows

     ! column dimension of dense matrix
     integer, intent(in)   :: ncols

     ! maximum number of nonzero elements allowed.
     ! this should be set to be the lengths of the arrays a and ja.
     integer, intent(in)   :: nnz

     ! a, ja, ia, input matrix in compressed sparse row format
     integer, intent(in)   :: ia(nrows+1)
     integer, intent(in)   :: ja(nnz)
     real(dp), intent(in)  :: a(nnz)

     ! vector, length equal to the column dimension of the dense matrix
     real(dp), intent(in)  :: x(ncols)

     ! vector, real array of length nrows, containing the product y = A . x
     real(dp), intent(out) :: y(nrows)

!! local variables
     ! loop index
     integer :: i
     integer :: k

!! [body

     ! zero out output vector
     y = 0.0_dp

     ! compute the inner product of row i with vector x
     do i=1,nrows
         do k=ia(i),ia(i+1)-1
             y(i) = y(i) + a(k) * x( ja(k) )
         enddo ! over k={ia(i),ia(i+1)-1} loop
     enddo ! over i={1,nrows} loop

!! body]

     return
  end subroutine csr_mv_d

!!
!! @sub csr_mv_z
!!
!! multiplies a matrix by a vector using the dot product form.
!!
  subroutine csr_mv_z(nrows, ncols, nnz, sa, ja, ia, sx, sy)
     implicit none

!! external arguments
     ! row dimension of dense matrix
     integer, intent(in)      :: nrows

     ! column dimension of dense matrix
     integer, intent(in)      :: ncols

     ! maximum number of nonzero elements allowed.
     ! this should be set to be the lengths of the arrays sa and ja.
     integer, intent(in)      :: nnz

     ! sa, ja, ia, input matrix in compressed sparse row format
     integer, intent(in)      :: ia(nrows+1)
     integer, intent(in)      :: ja(nnz)
     complex(dp), intent(in)  :: sa(nnz)

     ! vector, length equal to the column dimension of the dense matrix
     complex(dp), intent(in)  :: sx(ncols)

     ! vector, complex(dp) array of length nrows, containing the product y = A . x
     complex(dp), intent(out) :: sy(nrows)

!! local variables
     ! loop index
     integer :: i
     integer :: k

!! [body

     ! zero out output vector
     sy = dcmplx(0.0_dp, 0.0_dp)

     ! compute the inner product of row i with vector sx
     do i=1,nrows
         do k=ia(i),ia(i+1)-1
             sy(i) = sy(i) + sa(k) * sx( ja(k) )
         enddo ! over k={ia(i),ia(i+1)-1} loop
     enddo ! over i={1,nrows} loop

!! body]

     return
  end subroutine csr_mv_z

  subroutine csr_mv_d_t()
  end subroutine csr_mv_d_t

  subroutine csr_mv_z_t(i)
     integer :: i
     i = 0
  end subroutine csr_mv_z_t

!!
!! @sub csr_mm_d
!!
!! performs the matrix by matrix product C = A * B.
!!
  subroutine csr_mm_d(nrows, ndims, ncols, nnz, a, ja, ia, b, jb, ib, c, jc, ic)
     implicit none

!! external arguments
     ! the row dimension of matrix A = row dimension of matrix C
     integer, intent(in)   :: nrows

     ! the column dimension of matrix A = row dimension of matrix B
     integer, intent(in)   :: ndims

     ! the column dimension of matrix B = column dimension of matrix C
     integer, intent(in)   :: ncols

     ! the length of the arrays c and jc.
     !
     ! this subroutine will stop if the result matrix C has a number of
     ! elements that exceeds nnz.
     integer, intent(in)   :: nnz

     ! a, ja, ia, matrix A in compressed sparse row format
     integer, intent(in)   :: ia(nrows+1)
     integer, intent(in)   :: ja(nnz)
     real(dp), intent(in)  :: a(nnz)

     ! b, jb, ib, matrix B in compressed sparse row format
     integer, intent(in)   :: ib(ndims+1)
     integer, intent(in)   :: jb(nnz)
     real(dp), intent(in)  :: b(nnz)

     ! c, jc, ic, resulting matrix C in compressed sparse row format
     integer, intent(out)  :: ic(nrows+1)
     integer, intent(out)  :: jc(nnz)
     real(dp), intent(out) :: c(nnz)

!! local variables
     ! loop index
     integer :: i, j, k

     ! loop index
     integer :: ka, kb

     ! dummy integer variables
     integer :: p, q

     ! integer work array of length equal to the number of columns in
     ! matrix B, which is an array that has nonzero value if the column
     ! index already exist, in which case the value is the index of
     ! that column.
     integer :: iw(ncols)

     ! dummy real(dp) variables, used to improve the ratio of floating
     ! point operations to memory accesses.
     real(dp) :: atmp, btmp

!! [body

     ! init work array
     iw = 0

     ! init C sparse matrix
     ic(1) = 1

     q = 0
     do i=1,nrows
         do ka=ia(i),ia(i+1)-1
             j = ja(ka)
             atmp = a(ka)
             do kb=ib(j),ib(j+1)-1
                 k = jb(kb)
                 btmp = b(kb)

                 p = iw(k)
                 if ( p == 0 ) then
                     q = q + 1
                     iw(k) = q
                     jc(q) = k
                     c(q) = atmp * btmp
                 else
                     c(p) = c(p) + atmp * btmp
                 endif ! back if ( p == 0 ) block
             enddo ! over kb={ib(j),ib(j+1)-1} loop
         enddo ! over ka={ia(i),ia(i+1)-1} loop

         ! done this row i, so set work array to zero again
         do k=ic(i),q
             iw( jc( k ) ) = 0
         enddo ! over k={ic(i),q} loop
         ic(i+1) = q + 1
     enddo ! over i={1,nrows} loop

     ! check the number of nonzero elements
     if ( q > nnz ) then
         write(mystd,'(a)') 'sparse: error in csr_mm_d'
         STOP
     endif ! back if ( q > nnz ) block

!! body]

     return
  end subroutine csr_mm_d

!!
!! @sub csr_mm_z
!!
!! performs the matrix by matrix product C = A * B.
!!
  subroutine csr_mm_z(nrows, ndims, ncols, nnz, sa, ja, ia, sb, jb, ib, sc, jc, ic)
     implicit none

!! external arguments
     ! the row dimension of matrix A = row dimension of matrix C
     integer, intent(in)      :: nrows

     ! the column dimension of matrix A = row dimension of matrix B
     integer, intent(in)      :: ndims

     ! the column dimension of matrix B = column dimension of matrix C
     integer, intent(in)      :: ncols

     ! the length of the arrays sc and jc.
     !
     ! this subroutine will stop if the result matrix C has a number of
     ! elements that exceeds nnz.
     integer, intent(in)      :: nnz

     ! sa, ja, ia, matrix A in compressed sparse row format
     integer, intent(in)      :: ia(nrows+1)
     integer, intent(in)      :: ja(nnz)
     complex(dp), intent(in)  :: sa(nnz)

     ! sb, jb, ib, matrix B in compressed sparse row format
     integer, intent(in)      :: ib(ndims+1)
     integer, intent(in)      :: jb(nnz)
     complex(dp), intent(in)  :: sb(nnz)

     ! sc, jc, ic, resulting matrix C in compressed sparse row format
     integer, intent(out)     :: ic(nrows+1)
     integer, intent(out)     :: jc(nnz)
     complex(dp), intent(out) :: sc(nnz)

!! local variables
     ! loop index
     integer :: i, j, k

     ! loop index
     integer :: ka, kb

     ! dummy integer variables
     integer :: p, q

     ! integer work array of length equal to the number of columns in
     ! matrix B, which is an array that has nonzero value if the column
     ! index already exist, in which case the value is the index of
     ! that column.
     integer :: iw(ncols)

     ! dummy complex(dp) variables, used to improve the ratio of
     ! floating point operations to memory accesses.
     complex(dp) :: atmp, btmp

!! [body

     ! init work array
     iw = 0

     ! init C sparse matrix
     ic(1) = 1

     q = 0
     do i=1,nrows
         do ka=ia(i),ia(i+1)-1
             j = ja(ka)
             atmp = sa(ka)
             do kb=ib(j),ib(j+1)-1
                 k = jb(kb)
                 btmp = sb(kb)

                 p = iw(k)
                 if ( p == 0 ) then
                     q = q + 1
                     iw(k) = q
                     jc(q) = k
                     sc(q) = atmp * btmp
                 else
                     sc(p) = sc(p) + atmp * btmp
                 endif ! back if ( p == 0 ) block
             enddo ! over kb={ib(j),ib(j+1)-1} loop
         enddo ! over ka={ia(i),ia(i+1)-1} loop

         ! done this row i, so set work array to zero again
         do k=ic(i),q
             iw( jc( k ) ) = 0
         enddo ! over k={ic(i),q} loop
         ic(i+1) = q + 1
     enddo ! over i={1,nrows} loop

     ! check the number of nonzero elements
     if ( q > nnz ) then
         write(mystd,'(a)') 'sparse: error in csr_mm_z'
         STOP
     endif ! back if ( q > nnz ) block

!! body]

     return
  end subroutine csr_mm_z

  subroutine csr_mm_d_t()
  end subroutine csr_mm_d_t

  subroutine csr_mm_z_t(i)
     integer :: i
     i = 0
  end subroutine csr_mm_z_t

!!
!! @sub csr_md_d
!!
!! performs the matrix by matrix product B = A * Diag.
!!
  subroutine csr_md_d(nrows, nnz, a, ja, ia, diag, b, jb, ib)
     implicit none

!! external arguments
     ! the row dimension of dense matrix
     integer, intent(in)   :: nrows

     ! maximum number of nonzero elements allowed.
     ! this should be set to be the lengths of the arrays b and jb.
     integer, intent(in)   :: nnz

     ! a, ja, ia, matrix A in compressed sparse row format
     integer, intent(in)   :: ia(nrows+1)
     integer, intent(in)   :: ja(nnz)
     real(dp), intent(in)  :: a(nnz)

     ! diagonal matrix stored as a vector diag
     real(dp), intent(in)  :: diag(nrows)

     ! b, jb, ib, resulting matrix B in compressed sparse row format
     integer, intent(out)  :: ib(nrows+1)
     integer, intent(out)  :: jb(nnz)
     real(dp), intent(out) :: b(nnz)

!! local variables
     ! loop index
     integer :: i
     integer :: k

     ! loop index
     integer :: k1
     integer :: k2

!! [body

     ! init B sparse matrix
     b = 0.0_dp
     ib = 0
     jb = 0

     ! scale each element
     do i=1,nrows
         k1 = ia(i)
         k2 = ia(i+1) - 1
         do k=k1,k2
             b(k) = a(k) * diag( ja(k) )
         enddo ! over k={k1,k2} loop
     enddo ! over i={1,nrows} loop

     do i=1,nrows+1
         ib(i) = ia(i)
     enddo ! over i={1,nrows+1} loop

     do k=ia(1),ia(nrows+1)-1
         jb(k) = ja(k)
     enddo ! over k={ia(1),ia(nrows+1)-1} loop

!! body]

     return
  end subroutine csr_md_d

!!
!! @sub csr_md_z
!!
!! performs the matrix by matrix product B = A * Diag.
!!
  subroutine csr_md_z(nrows, nnz, sa, ja, ia, diag, sb, jb, ib)
     implicit none

!! external arguments
     ! the row dimension of dense matrix
     integer, intent(in)      :: nrows

     ! maximum number of nonzero elements allowed.
     ! this should be set to be the lengths of the arrays sb and jb.
     integer, intent(in)      :: nnz

     ! sa, ja, ia, matrix A in compressed sparse row format
     integer, intent(in)      :: ia(nrows+1)
     integer, intent(in)      :: ja(nnz)
     complex(dp), intent(in)  :: sa(nnz)

     ! diagonal matrix stored as a vector diag
     complex(dp), intent(in)  :: diag(nrows)

     ! sb, jb, ib, resulting matrix B in compressed sparse row format
     integer, intent(out)     :: ib(nrows+1)
     integer, intent(out)     :: jb(nnz)
     complex(dp), intent(out) :: sb(nnz)

!! local variables
     ! loop index
     integer :: i
     integer :: k

     ! loop index
     integer :: k1
     integer :: k2

!! [body

     ! init B sparse matrix
     sb = dcmplx(0.0_dp, 0.0_dp)
     ib = 0
     jb = 0

     ! scale each element
     do i=1,nrows
         k1 = ia(i)
         k2 = ia(i+1) - 1
         do k=k1,k2
             sb(k) = sa(k) * diag( ja(k) )
         enddo ! over k={k1,k2} loop
     enddo ! over i={1,nrows} loop

     do i=1,nrows+1
         ib(i) = ia(i)
     enddo ! over i={1,nrows+1} loop

     do k=ia(1),ia(nrows+1)-1
         jb(k) = ja(k)
     enddo ! over k={ia(1),ia(nrows+1)-1} loop

!! body]

     return
  end subroutine csr_md_z

  subroutine csr_md_d_t()
  end subroutine csr_md_d_t

  subroutine csr_md_z_t(i)
     integer :: i
     i = 0
  end subroutine csr_md_z_t

!!
!! @sub csr_dm_d
!!
!! performs the matrix by matrix product B = Diag * A.
!!
  subroutine csr_dm_d(nrows, nnz, diag, a, ja, ia, b, jb, ib)
     implicit none

!! external arguments
     ! the row dimension of dense matrix
     integer, intent(in)   :: nrows

     ! maximum number of nonzero elements allowed.
     ! this should be set to be the lengths of the arrays b and jb.
     integer, intent(in)   :: nnz

     ! diagonal matrix stored as a vector diag
     real(dp), intent(in)  :: diag(nrows)

     ! a, ja, ia, matrix A in compressed sparse row format
     integer, intent(in)   :: ia(nrows+1)
     integer, intent(in)   :: ja(nnz)
     real(dp), intent(in)  :: a(nnz)

     ! b, jb, ib, resulting matrix B in compressed sparse row format
     integer, intent(out)  :: ib(nrows+1)
     integer, intent(out)  :: jb(nnz)
     real(dp), intent(out) :: b(nnz)

!! local variables
     ! loop index
     integer :: i
     integer :: k

     ! loop index
     integer :: k1
     integer :: k2

!! [body

     ! init B sparse matrix
     b = 0.0_dp
     ib = 0
     jb = 0

     ! normalize each row
     do i=1,nrows
         k1 = ia(i)
         k2 = ia(i+1) - 1
         do k=k1,k2
             b(k) = a(k) * diag(i)
         enddo ! over k={k1,k2} loop
     enddo ! over i={1,nrows} loop

     do i=1,nrows+1
         ib(i) = ia(i)
     enddo ! over i={1,nrows+1} loop

     do k=ia(1),ia(nrows+1)-1
         jb(k) = ja(k)
     enddo ! over k={ia(1),ia(nrows+1)-1} loop

!! body]

     return
  end subroutine csr_dm_d

!!
!! @sub csr_dm_z
!!
!! performs the matrix by matrix product B = Diag * A.
!!
  subroutine csr_dm_z(nrows, nnz, diag, sa, ja, ia, sb, jb, ib)
     implicit none

!! external arguments
     ! the row dimension of dense matrix
     integer, intent(in)      :: nrows

     ! maximum number of nonzero elements allowed.
     ! this should be set to be the lengths of the arrays sb and jb.
     integer, intent(in)      :: nnz

     ! diagonal matrix stored as a vector diag
     complex(dp), intent(in)  :: diag(nrows)

     ! sa, ja, ia, matrix A in compressed sparse row format
     integer, intent(in)      :: ia(nrows+1)
     integer, intent(in)      :: ja(nnz)
     complex(dp), intent(in)  :: sa(nnz)

     ! sb, jb, ib, resulting matrix B in compressed sparse row format
     integer, intent(out)     :: ib(nrows+1)
     integer, intent(out)     :: jb(nnz)
     complex(dp), intent(out) :: sb(nnz)

!! local variables
     ! loop index
     integer :: i
     integer :: k

     ! loop index
     integer :: k1
     integer :: k2

!! [body

     ! init B sparse matrix
     sb = dcmplx(0.0_dp, 0.0_dp)
     ib = 0
     jb = 0

     ! normalize each row
     do i=1,nrows
         k1 = ia(i)
         k2 = ia(i+1) - 1
         do k=k1,k2
             sb(k) = sa(k) * diag(i)
         enddo ! over k={k1,k2} loop
     enddo ! over i={1,nrows} loop

     do i=1,nrows+1
         ib(i) = ia(i)
     enddo ! over i={1,nrows+1} loop

     do k=ia(1),ia(nrows+1)-1
         jb(k) = ja(k)
     enddo ! over k={ia(1),ia(nrows+1)-1} loop

!! body]

     return
  end subroutine csr_dm_z

  subroutine csr_dm_d_t()
  end subroutine csr_dm_d_t

  subroutine csr_dm_z_t(i)
     integer :: i
     i = 0
  end subroutine csr_dm_z_t

  end module sparse
