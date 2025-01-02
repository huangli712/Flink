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

!!========================================================================
!!>>> declare accessibility for module routines                        <<<
!!========================================================================

! CSR -> DNS
     private :: csr_dns_d ! real(dp) version
     private :: csr_dns_z ! complex(dp) version

!!========================================================================
!!>>> declare interface and module procedure                           <<<
!!========================================================================


  contains ! encapsulated functionality

  end module sparse
