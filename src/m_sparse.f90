!!!-----------------------------------------------------------------------
!!! project : flink @ sakura
!!! program : sparse
!!! source  : m_sparse.f90
!!! type    : module
!!! author  : li huang (email:huangli@caep.cn)
!!! history : 02/01/2010 by li huang (created)
!!!           01/02/2025 by li huang (last modified)
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
!!>>> declare global parameters                                        <<<
!!========================================================================

!! module parameters
     ! dp: number precision, double precision for real and complex number
     integer, private, parameter :: dp    = kind(1.0d0)

     ! mystd: device descriptor, console output
     integer, private, parameter :: mystd = 6

     type, public, abstract :: sparse_t
         integer :: nrows = 0 ! number of rows
         integer :: ncols = 0 ! number of columns
         integer :: nnz   = 0 ! number of non-zero values
         integer, allocatable :: rowptr(:) ! matrix row pointer
         integer, allocatable :: colptr(:) ! matrix column pointer
     end type sparse_t

     !! CSR: Compressed sparse row or Yale format
     type, public, extends(sparse_t) :: csr_d
         real(dp), allocatable :: Vd(:) 
     end type csr_d

     type, public, extends(sparse_t) :: csr_z
         complex(dp), allocatable :: Vz(:)
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

  end module sparse
