!!!-----------------------------------------------------------------------
!!! project : flink @ sakura
!!! program : sparse
!!! source  : m_sparse.f90
!!! type    : module
!!! author  : li huang (email:huangli@caep.cn)
!!! history : 02/01/2010 by li huang (created)
!!!           01/07/2025 by li huang (last modified)
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

     ! CSR -> DNS
     private :: csr_dns_d ! real(dp) version
     private :: csr_dns_z ! complex(dp) version
     !
     private :: csr_dns_d_t
     private :: csr_dns_z_t

     ! DNS -> CSR
     private :: dns_csr_d ! real(dp) version
     private :: dns_csr_z ! complex(dp) version
     !
     private :: dns_csr_d_t
     private :: dns_csr_z_t

     ! CSR -> CSR
     private :: csr_csr_d ! real(dp) version
     private :: csr_csr_z ! complex(dp) version
     !
     private :: csr_csr_d_t
     private :: csr_csr_z_t

!!========================================================================
!!>>> declare interface and module procedure                           <<<
!!========================================================================

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

  contains ! encapsulated functionality

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
                 write(mystd,'(a)') 'sparse: error in dns_crs_d'
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

  subroutine dns_csr_z_t()
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

  subroutine csr_csr_z_t()
  end subroutine csr_csr_z_t

  end module sparse
