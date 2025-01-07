!!
!! @sub sp_matmul_amudia
!!
!! performs the matrix by matrix product B = A * Diag.
!!
  subroutine sp_matmul_amudia(nrow, nmax, a, ja, ia, diag, b, jb, ib)
     implicit none

!! external arguments
     ! the row dimension of dense matrix
     integer, intent(in)   :: nrow

     ! maximum number of nonzero elements allowed.
     ! this should be set to be the lengths of the arrays b and jb.
     integer, intent(in)   :: nmax

     ! a, ja, ia, matrix A in compressed sparse row format
     integer, intent(in)   :: ia(nrow+1)
     integer, intent(in)   :: ja(nmax)
     real(dp), intent(in)  :: a(nmax)

     ! diagonal matrix stored as a vector diag
     real(dp), intent(in)  :: diag(nrow)

     ! b, jb, ib, resulting matrix B in compressed sparse row format
     integer, intent(out)  :: ib(nrow+1)
     integer, intent(out)  :: jb(nmax)
     real(dp), intent(out) :: b(nmax)

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
     do i=1,nrow
         k1 = ia(i)
         k2 = ia(i+1) - 1
         do k=k1,k2
             b(k) = a(k) * diag( ja(k) )
         enddo ! over k={k1,k2} loop
     enddo ! over i={1,nrow} loop

     do i=1,nrow+1
         ib(i) = ia(i)
     enddo ! over i={1,nrow+1} loop

     do k=ia(1),ia(nrow+1)-1
         jb(k) = ja(k)
     enddo ! over k={ia(1),ia(nrow+1)-1} loop

!! body]

     return
  end subroutine sp_matmul_amudia

!!
!! @sub sp_matmul_amudia_z
!!
!! performs the matrix by matrix product B = A * Diag.
!!
  subroutine sp_matmul_amudia_z(nrow, nmax, sa, ja, ia, diag, sb, jb, ib)
     implicit none

!! external arguments
     ! the row dimension of dense matrix
     integer, intent(in)      :: nrow

     ! maximum number of nonzero elements allowed.
     ! this should be set to be the lengths of the arrays sb and jb.
     integer, intent(in)      :: nmax

     ! sa, ja, ia, matrix A in compressed sparse row format
     integer, intent(in)      :: ia(nrow+1)
     integer, intent(in)      :: ja(nmax)
     complex(dp), intent(in)  :: sa(nmax)

     ! diagonal matrix stored as a vector diag
     complex(dp), intent(in)  :: diag(nrow)

     ! sb, jb, ib, resulting matrix B in compressed sparse row format
     integer, intent(out)     :: ib(nrow+1)
     integer, intent(out)     :: jb(nmax)
     complex(dp), intent(out) :: sb(nmax)

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
     do i=1,nrow
         k1 = ia(i)
         k2 = ia(i+1) - 1
         do k=k1,k2
             sb(k) = sa(k) * diag( ja(k) )
         enddo ! over k={k1,k2} loop
     enddo ! over i={1,nrow} loop

     do i=1,nrow+1
         ib(i) = ia(i)
     enddo ! over i={1,nrow+1} loop

     do k=ia(1),ia(nrow+1)-1
         jb(k) = ja(k)
     enddo ! over k={ia(1),ia(nrow+1)-1} loop

!! body]

     return
  end subroutine sp_matmul_amudia_z

!!
!! @sub sp_matmul_diamua
!!
!! performs the matrix by matrix product B = Diag * A.
!!
  subroutine sp_matmul_diamua(nrow, nmax, diag, a, ja, ia, b, jb, ib)
     implicit none

!! external arguments
     ! the row dimension of dense matrix
     integer, intent(in)   :: nrow

     ! maximum number of nonzero elements allowed.
     ! this should be set to be the lengths of the arrays b and jb.
     integer, intent(in)   :: nmax

     ! diagonal matrix stored as a vector diag
     real(dp), intent(in)  :: diag(nrow)

     ! a, ja, ia, matrix A in compressed sparse row format
     integer, intent(in)   :: ia(nrow+1)
     integer, intent(in)   :: ja(nmax)
     real(dp), intent(in)  :: a(nmax)

     ! b, jb, ib, resulting matrix B in compressed sparse row format
     integer, intent(out)  :: ib(nrow+1)
     integer, intent(out)  :: jb(nmax)
     real(dp), intent(out) :: b(nmax)

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
     do i=1,nrow
         k1 = ia(i)
         k2 = ia(i+1) - 1
         do k=k1,k2
             b(k) = a(k) * diag(i)
         enddo ! over k={k1,k2} loop
     enddo ! over i={1,nrow} loop

     do i=1,nrow+1
         ib(i) = ia(i)
     enddo ! over i={1,nrow+1} loop

     do k=ia(1),ia(nrow+1)-1
         jb(k) = ja(k)
     enddo ! over k={ia(1),ia(nrow+1)-1} loop

!! body]

     return
  end subroutine sp_matmul_diamua

!!
!! @sub sp_matmul_diamua_z
!!
!! performs the matrix by matrix product B = Diag * A.
!!
  subroutine sp_matmul_diamua_z(nrow, nmax, diag, sa, ja, ia, sb, jb, ib)
     implicit none

!! external arguments
     ! the row dimension of dense matrix
     integer, intent(in)      :: nrow

     ! maximum number of nonzero elements allowed.
     ! this should be set to be the lengths of the arrays sb and jb.
     integer, intent(in)      :: nmax

     ! diagonal matrix stored as a vector diag
     complex(dp), intent(in)  :: diag(nrow)

     ! sa, ja, ia, matrix A in compressed sparse row format
     integer, intent(in)      :: ia(nrow+1)
     integer, intent(in)      :: ja(nmax)
     complex(dp), intent(in)  :: sa(nmax)

     ! sb, jb, ib, resulting matrix B in compressed sparse row format
     integer, intent(out)     :: ib(nrow+1)
     integer, intent(out)     :: jb(nmax)
     complex(dp), intent(out) :: sb(nmax)

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
     do i=1,nrow
         k1 = ia(i)
         k2 = ia(i+1) - 1
         do k=k1,k2
             sb(k) = sa(k) * diag(i)
         enddo ! over k={k1,k2} loop
     enddo ! over i={1,nrow} loop

     do i=1,nrow+1
         ib(i) = ia(i)
     enddo ! over i={1,nrow+1} loop

     do k=ia(1),ia(nrow+1)-1
         jb(k) = ja(k)
     enddo ! over k={ia(1),ia(nrow+1)-1} loop

!! body]

     return
  end subroutine sp_matmul_diamua_z
