!!!-----------------------------------------------------------------------
!!! project : flink @ sakura
!!! program : s_linspace_i
!!!           s_linspace_d
!!!           s_linspace_z
!!!           s_cumsum_i
!!!           s_cumsum_d
!!!           s_cumsum_z
!!!           s_cumprod_i
!!!           s_cumprod_d
!!!           s_cumprod_z
!!!           s_swap_i
!!!           s_swap_d
!!!           s_swap_z
!!!           s_mix_i
!!!           s_mix_d
!!!           s_mix_z
!!!           s_vecadd_i
!!!           s_vecadd_d
!!!           s_vecadd_z
!!!           s_dot_i
!!!           s_dot_d
!!!           s_dot_z
!!!           s_diff_i
!!!           s_diff_d
!!!           s_diff_z
!!!           s_stats_i
!!!           s_stats_d
!!!           s_stats_z
!!!           s_cross_i
!!!           s_cross_d
!!!           s_cross_z
!!!           s_distance_i
!!!           s_distance_d
!!!           s_distance_z
!!!           s_unique_i
!!!           s_unique_d
!!!           s_unique_z
!!!           s_intersect_i
!!!           s_intersect_d
!!!           s_intersect_z
!!! source  : s_vector.f90
!!! type    : subroutines
!!! author  : li huang (email:huangli@caep.cn)
!!! history : 07/10/2014 by li huang (created)
!!!           03/02/2026 by li huang (last modified)
!!! purpose : these subroutines are designed for vectors or arrays. they
!!!           can be used to manipulate grid and mesh.
!!! status  : unstable
!!! comment :
!!!-----------------------------------------------------------------------

!!========================================================================
!!>>> mesh generation                                                  <<<
!!========================================================================

!!
!! @sub s_linspace_i
!!
!! create a linear mesh x in interval [xmin, xmax], integer version.
!!
  subroutine s_linspace_i(xmin, xmax, n, x)
     use constants, only : dp

     implicit none

!! external arguments
     ! left boundary
     integer, intent(in)  :: xmin

     ! right boundary
     integer, intent(in)  :: xmax

     ! size of array x
     integer, intent(in)  :: n

     ! output array, containing the linear mesh
     integer, intent(out) :: x(n)

!! local variables
     ! loop index
     integer :: i

!! [body

     do i=1,n
         x(i) = ( xmax - xmin ) * real(i - 1, dp) / real(n - 1, dp) + xmin
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_linspace_i

!!
!! @sub s_linspace_d
!!
!! create a linear mesh x in interval [xmin, xmax], real(dp) version.
!!
  subroutine s_linspace_d(xmin, xmax, n, x)
     use constants, only : dp

     implicit none

!! external arguments
     ! left boundary
     real(dp), intent(in)  :: xmin

     ! right boundary
     real(dp), intent(in)  :: xmax

     ! size of array x
     integer,  intent(in)  :: n

     ! output array, containing the linear mesh
     real(dp), intent(out) :: x(n)

!! local variables
     ! loop index
     integer :: i

!! [body

     do i=1,n
         x(i) = ( xmax - xmin ) * real(i - 1, dp) / real(n - 1, dp) + xmin
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_linspace_d

!!
!! @sub s_linspace_z
!!
!! create a linear mesh x in interval [xmin, xmax], complex(dp) version.
!!
  subroutine s_linspace_z(xmin, xmax, n, x)
     use constants, only : dp

     implicit none

!! external arguments
     ! left boundary
     complex(dp), intent(in)  :: xmin

     ! right boundary
     complex(dp), intent(in)  :: xmax

     ! size of array x
     integer,  intent(in)     :: n

     ! output array, containing the linear mesh
     complex(dp), intent(out) :: x(n)

!! local variables
     ! loop index
     integer :: i

!! [body

     do i=1,n
         x(i) = ( xmax - xmin ) * real(i - 1, dp) / real(n - 1, dp) + xmin
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_linspace_z

!!========================================================================
!!>>> sum operations                                                   <<<
!!========================================================================

!!
!! @sub s_cumsum_i
!!
!! return the cumulative sum of an integer array.
!!
  subroutine s_cumsum_i(n, v, vsum)
     implicit none

!! external arguments
     ! size of array v
     integer, intent(in)  :: n

     ! input integer array
     integer, intent(in)  :: v(n)

     ! cumulative sum of array v
     integer, intent(out) :: vsum(n)

!! local variables
     ! loop index
     integer :: i

!! [body

     vsum(1) = v(1)
     !
     do i=2,n
         vsum(i) = vsum(i-1) + v(i)
     enddo ! over i={2,n} loop

!! body]

     return
  end subroutine s_cumsum_i

!!
!! @sub s_cumsum_d
!!
!! return the cumulative sum of a real(dp) array.
!!
  subroutine s_cumsum_d(n, v, vsum)
     use constants, only : dp

     implicit none

!! external arguments
     ! size of array v
     integer, intent(in)   :: n

     ! input real(dp) array
     real(dp), intent(in)  :: v(n)

     ! cumulative sum of array v
     real(dp), intent(out) :: vsum(n)

!! local variables
     ! loop index
     integer :: i

!! [body

     vsum(1) = v(1)
     !
     do i=2,n
         vsum(i) = vsum(i-1) + v(i)
     enddo ! over i={2,n} loop

!! body]

     return
  end subroutine s_cumsum_d

!!
!! @sub s_cumsum_z
!!
!! return the cumulative sum of a complex(dp) array.
!!
  subroutine s_cumsum_z(n, v, vsum)
     use constants, only : dp

     implicit none

!! external arguments
     ! size of array v
     integer, intent(in)      :: n

     ! input complex(dp) array
     complex(dp), intent(in)  :: v(n)

     ! cumulative sum of array v
     complex(dp), intent(out) :: vsum(n)

!! local variables
     ! loop index
     integer :: i

!! [body

     vsum(1) = v(1)
     !
     do i=2,n
         vsum(i) = vsum(i-1) + v(i)
     enddo ! over i={2,n} loop

!! body]

     return
  end subroutine s_cumsum_z

!!========================================================================
!!>>> prod operations                                                  <<<
!!========================================================================

!!
!! @sub s_cumprod_i
!!
!! return the cumulative product of an integer array.
!!
  subroutine s_cumprod_i(n, v, vprod)
     implicit none

!! external arguments
     ! size of array v
     integer, intent(in)  :: n

     ! input integer array
     integer, intent(in)  :: v(n)

     ! cumulative product of array v
     integer, intent(out) :: vprod(n)

!! local variables
     ! loop index
     integer :: i

!! [body

     vprod(1) = v(1)
     !
     do i=2,n
         vprod(i) = vprod(i-1) * v(i)
     enddo ! over i={2,n} loop

!! body]

     return
  end subroutine s_cumprod_i

!!
!! @sub s_cumprod_d
!!
!! return the cumulative product of a real(dp) array.
!!
  subroutine s_cumprod_d(n, v, vprod)
     use constants, only : dp

     implicit none

!! external arguments
     ! size of array v
     integer, intent(in)   :: n

     ! input real(dp) array
     real(dp), intent(in)  :: v(n)

     ! cumulative product of array v
     real(dp), intent(out) :: vprod(n)

!! local variables
     ! loop index
     integer :: i

!! [body

     vprod(1) = v(1)
     !
     do i=2,n
         vprod(i) = vprod(i-1) * v(i)
     enddo ! over i={2,n} loop

!! body]

     return
  end subroutine s_cumprod_d

!!
!! @sub s_cumprod_z
!!
!! return the cumulative product of a complex(dp) array.
!!
  subroutine s_cumprod_z(n, v, vprod)
     use constants, only : dp

     implicit none

!! external arguments
     ! size of array v
     integer, intent(in)      :: n

     ! input complex(dp) array
     complex(dp), intent(in)  :: v(n)

     ! cumulative product of array v
     complex(dp), intent(out) :: vprod(n)

!! local variables
     ! loop index
     integer :: i

!! [body

     vprod(1) = v(1)
     !
     do i=2,n
         vprod(i) = vprod(i-1) * v(i)
     enddo ! over i={2,n} loop

!! body]

     return
  end subroutine s_cumprod_z

!!========================================================================
!!>>> swap operations                                                  <<<
!!========================================================================

!!
!! @sub s_swap_i
!!
!! exchange two integer vectors.
!!
  subroutine s_swap_i(n, ix, iy)
     implicit none

!! external arguments
     ! dimension of integer vector
     integer, intent(in)    :: n

     ! integer vector X
     integer, intent(inout) :: ix(n)

     ! integer vector Y
     integer, intent(inout) :: iy(n)

!! local variables
     ! dummy integer vector
     integer :: it(n)

!! [body

     it = ix
     ix = iy
     iy = it

!! body]

     return
  end subroutine s_swap_i

!!
!! @sub s_swap_d
!!
!! exchange two real(dp) vectors.
!!
  subroutine s_swap_d(n, dx, dy)
     use constants, only : dp

     implicit none

!! external arguments
     ! dimension of real(dp) vector
     integer, intent(in)     :: n

     ! real(dp) vector X
     real(dp), intent(inout) :: dx(n)

     ! real(dp) vector Y
     real(dp), intent(inout) :: dy(n)

!! local variables
     ! dummy real(dp) vector
     real(dp) :: dt(n)

!! [body

     dt = dx
     dx = dy
     dy = dt

!! body]

     return
  end subroutine s_swap_d

!!
!! @sub s_swap_z
!!
!! exchange two complex(dp) vectors.
!!
  subroutine s_swap_z(n, zx, zy)
     use constants, only : dp

     implicit none

!! external arguments
     ! dimension of complex(dp) vector
     integer, intent(in)        :: n

     ! complex(dp) vector X
     complex(dp), intent(inout) :: zx(n)

     ! complex(dp) vector Y
     complex(dp), intent(inout) :: zy(n)

!! local variables
     ! dummy complex(dp) vector
     complex(dp) :: zt(n)

!! [body

     zt = zx
     zx = zy
     zy = zt

!! body]

     return
  end subroutine s_swap_z

!!========================================================================
!!>>> mix operations                                                   <<<
!!========================================================================

!!
!! @sub s_mix_i
!!
!! linear mixing for two integer vectors.
!!
  subroutine s_mix_i(n, ix, iy, alpha)
     use constants, only : dp
     use constants, only : one

     implicit none

!! external arguments
     ! dimension of integer vector
     integer, intent(in)    :: n

     ! mixing parameter
     real(dp), intent(in)   :: alpha

     ! integer vector X
     integer, intent(in)    :: ix(n)

     ! integer vector Y
     integer, intent(inout) :: iy(n)

!! [body

     iy = int( real(ix) * (one - alpha) + real(iy) * alpha )

!! body]

     return
  end subroutine s_mix_i

!!
!! @sub s_mix_d
!!
!! linear mixing for two real(dp) vectors.
!!
  subroutine s_mix_d(n, dx, dy, alpha)
     use constants, only : dp
     use constants, only : one

     implicit none

!! external arguments
     ! dimension of real(dp) vector
     integer, intent(in)     :: n

     ! mixing parameter
     real(dp), intent(in)    :: alpha

     ! real(dp) vector X
     real(dp), intent(in)    :: dx(n)

     ! real(dp) vector Y
     real(dp), intent(inout) :: dy(n)

!! [body

     dy = dx * (one - alpha) + dy * alpha

!! body]

     return
  end subroutine s_mix_d

!!
!! @sub s_mix_z
!!
!! linear mixing for two complex(dp) vectors.
!!
  subroutine s_mix_z(n, zx, zy, alpha)
     use constants, only : dp
     use constants, only : one

     implicit none

!! external arguments
     ! dimension of complex(dp) vector
     integer, intent(in)        :: n

     ! mixing parameter
     real(dp), intent(in)       :: alpha

     ! complex(dp) vector X
     complex(dp), intent(in)    :: zx(n)

     ! complex(dp) vector Y
     complex(dp), intent(inout) :: zy(n)

!! [body

     zy = zx * (one - alpha) + zy * alpha

!! body]

     return
  end subroutine s_mix_z

!!========================================================================
!!>>> vector add operations                                            <<<
!!========================================================================

!!
!! @sub s_vecadd_i
!!
!! add diagonal elements of a matrix to a vector, integer version.
!!
  subroutine s_vecadd_i(n, ix, iy, alpha)
     use constants, only : dp

     implicit none

!! external arguments
     ! dimension of integer vector
     integer, intent(in)    :: n

     ! prefactor
     real(dp), intent(in)   :: alpha

     ! integer vector X
     integer, intent(inout) :: ix(n)

     ! integer matrix Y
     integer, intent(in)    :: iy(n,n)

!! local variables
     ! loop index
     integer :: i

!! [body

     do i=1,n
         ix(i) = ix(i) + int( alpha * iy(i,i) )
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_vecadd_i

!!
!! @sub s_vecadd_d
!!
!! add diagonal elements of a matrix to a vector, real(dp) version.
!!
  subroutine s_vecadd_d(n, dx, dy, alpha)
     use constants, only : dp

     implicit none

!! external arguments
     ! dimension of real(dp) vector
     integer, intent(in)     :: n

     ! prefactor
     real(dp), intent(in)    :: alpha

     ! real(dp) vector X
     real(dp), intent(inout) :: dx(n)

     ! real(dp) matrix Y
     real(dp), intent(in)    :: dy(n,n)

!! local variables
     ! loop index
     integer :: i

!! [body

     do i=1,n
         dx(i) = dx(i) + alpha * dy(i,i)
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_vecadd_d

!!
!! @sub s_vecadd_z
!!
!! add diagonal elements of a matrix to a vector, complex(dp) version.
!!
  subroutine s_vecadd_z(n, zx, zy, alpha)
     use constants, only : dp

     implicit none

!! external arguments
     ! dimension of complex(dp) vector
     integer, intent(in)        :: n

     ! prefactor
     real(dp), intent(in)       :: alpha

     ! complex(dp) vector X
     complex(dp), intent(inout) :: zx(n)

     ! complex(dp) matrix Y
     complex(dp), intent(in)    :: zy(n,n)

!! local variables
     ! loop index
     integer :: i

!! [body

     do i=1,n
         zx(i) = zx(i) + alpha * zy(i,i)
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_vecadd_z

!!========================================================================
!!>>> vector dot operations                                            <<<
!!========================================================================

!!
!! @sub s_dot_i
!!
!! compute dot product of two integer vectors.
!!
  subroutine s_dot_i(n, ix, iy, val)
     implicit none

!! external arguments
     ! dimension of integer vectors
     integer, intent(in)  :: n

     ! integer vector X
     integer, intent(in)  :: ix(n)

     ! integer vector Y
     integer, intent(in)  :: iy(n)

     ! dot product result
     integer, intent(out) :: val

!! local variables
     ! loop index
     integer :: i

!! [body

     if (n <= 0) then
         val = 0
         return
     endif
     !
     val = 0
     !
     do i=1,n
         val = val + ix(i) * iy(i)
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_dot_i

!!
!! @sub s_dot_d
!!
!! compute dot product of two real(dp) vectors.
!!
  subroutine s_dot_d(n, dx, dy, val)
     use constants, only : dp
     use constants, only : zero

     implicit none

!! external arguments
     ! dimension of real(dp) vectors
     integer, intent(in)   :: n

     ! real(dp) vector X
     real(dp), intent(in)  :: dx(n)

     ! real(dp) vector Y
     real(dp), intent(in)  :: dy(n)

     ! dot product result
     real(dp), intent(out) :: val

!! local variables
     ! loop index
     integer :: i

!! [body

     if (n <= 0) then
         val = zero
         return
     endif
     !
     val = zero
     !
     do i=1,n
         val = val + dx(i) * dy(i)
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_dot_d

!!
!! @sub s_dot_z
!!
!! compute dot product of two complex(dp) vectors.
!!
  subroutine s_dot_z(n, zx, zy, val)
     use constants, only : dp
     use constants, only : czero

     implicit none

!! external arguments
     ! dimension of complex(dp) vectors
     integer, intent(in)      :: n

     ! complex(dp) vector X
     complex(dp), intent(in)  :: zx(n)

     ! complex(dp) vector Y
     complex(dp), intent(in)  :: zy(n)

     ! dot product result
     complex(dp), intent(out) :: val

!! local variables
     ! loop index
     integer :: i

!! [body

     if (n <= 0) then
         val = czero
         return
     endif
     !
     val = czero
     !
     do i=1,n
         val = val + conjg(zx(i)) * zy(i)
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_dot_z

!!========================================================================
!!>>> vector diff operations                                           <<<
!!========================================================================

!!
!! @sub s_diff_i
!!
!! compute finite differences of an integer vector.
!! result size is n-1: diff(i) = v(i+1) - v(i)
!!
  subroutine s_diff_i(n, iv, diff)
     implicit none

!! external arguments
     ! size of input array
     integer, intent(in)  :: n

     ! input integer array
     integer, intent(in)  :: iv(n)

     ! finite differences, size n-1
     integer, intent(out) :: diff(n-1)

!! local variables
     ! loop index
     integer :: i

!! [body
!!
     if (n < 2) return
     !
     do i=1,n-1
         diff(i) = iv(i+1) - iv(i)
     enddo ! over i={1,n-1} loop

!! body]

     return
  end subroutine s_diff_i

!!
!! @sub s_diff_d
!!
!! compute finite differences of a real(dp) vector.
!! result size is n-1: diff(i) = v(i+1) - v(i)
!!
  subroutine s_diff_d(n, dv, diff)
     use constants, only : dp

     implicit none

!! external arguments
     ! size of input array
     integer, intent(in)   :: n

     ! input real(dp) array
     real(dp), intent(in)  :: dv(n)

     ! finite differences, size n-1
     real(dp), intent(out) :: diff(n-1)

!! local variables
     ! loop index
     integer :: i

!! [body
!!
     if (n < 2) return
     !
     do i=1,n-1
         diff(i) = dv(i+1) - dv(i)
     enddo ! over i={1,n-1} loop

!! body]

     return
  end subroutine s_diff_d

!!
!! @sub s_diff_z
!!
!! compute finite differences of a complex(dp) vector.
!! result size is n-1: diff(i) = v(i+1) - v(i)
!!
  subroutine s_diff_z(n, zv, diff)
     use constants, only : dp

     implicit none

!! external arguments
     ! size of input array
     integer, intent(in)      :: n

     ! input complex(dp) array
     complex(dp), intent(in)  :: zv(n)

     ! finite differences, size n-1
     complex(dp), intent(out) :: diff(n-1)

!! local variables
     ! loop index
     integer :: i

!! [body
!!
     if (n < 2) return
     !
     do i=1,n-1
         diff(i) = zv(i+1) - zv(i)
     enddo ! over i={1,n-1} loop

!! body]

     return
  end subroutine s_diff_z

!!========================================================================
!!>>> vector statistics operation                                      <<<
!!========================================================================

!!
!! @sub s_stats_i
!!
!! compute mean and standard deviation of an integer vector.
!!
  subroutine s_stats_i(n, iv, mean, stddev)
     use constants, only : dp
     use constants, only : zero

     implicit none

!! external arguments
     ! size of array
     integer, intent(in)   :: n

     ! input integer array
     integer, intent(in)   :: iv(n)

     ! mean value
     real(dp), intent(out) :: mean

     ! standard deviation
     real(dp), intent(out) :: stddev

!! local variables
     ! loop index
     integer  :: i

     ! sum of squared deviations
     real(dp) :: sum_sq

     ! sum of values
     real(dp) :: sum_val

!! [body

     if (n <= 0) then
         mean = zero
         stddev = zero
         return
     endif

     ! compute mean
     sum_val = zero
     do i=1,n
         sum_val = sum_val + real(iv(i), dp)
     enddo
     mean = sum_val / real(n, dp)

     ! compute standard deviation
     if (n == 1) then
         stddev = zero
     else
         sum_sq = zero
         do i=1,n
             sum_sq = sum_sq + (real(iv(i), dp) - mean)**2
         enddo
         stddev = sqrt(sum_sq / real(n - 1, dp))
     endif

!! body]

     return
  end subroutine s_stats_i

!!
!! @sub s_stats_d
!!
!! compute mean and standard deviation of a real(dp) vector.
!!
  subroutine s_stats_d(n, dv, mean, stddev)
     use constants, only : dp
     use constants, only : zero

     implicit none

!! external arguments
     ! size of array
     integer, intent(in)   :: n

     ! input real(dp) array
     real(dp), intent(in)  :: dv(n)

     ! mean value
     real(dp), intent(out) :: mean

     ! standard deviation
     real(dp), intent(out) :: stddev

!! local variables
     ! loop index
     integer  :: i

     ! sum of squared deviations
     real(dp) :: sum_sq

     ! sum of values
     real(dp) :: sum_val

!! [body

     if (n <= 0) then
         mean = zero
         stddev = zero
         return
     endif

     ! compute mean
     sum_val = zero
     do i=1,n
         sum_val = sum_val + dv(i)
     enddo
     mean = sum_val / real(n, dp)

     ! compute standard deviation
     if (n == 1) then
         stddev = zero
     else
         sum_sq = zero
         do i=1,n
             sum_sq = sum_sq + (dv(i) - mean)**2
         enddo
         stddev = sqrt(sum_sq / real(n - 1, dp))
     endif

!! body]

     return
  end subroutine s_stats_d

!!
!! @sub s_stats_z
!!
!! compute mean and standard deviation of a complex(dp) vector.
!!
  subroutine s_stats_z(n, zv, mean, stddev)
     use constants, only : dp
     use constants, only : zero, czero

     implicit none

!! external arguments
     ! size of array
     integer, intent(in)      :: n

     ! input complex(dp) array
     complex(dp), intent(in)  :: zv(n)

     ! mean value
     complex(dp), intent(out) :: mean

     ! standard deviation
     real(dp), intent(out)    :: stddev

!! local variables
     ! loop index
     integer  :: i

     ! sum of squared deviations
     real(dp) :: sum_sq

     ! sum of values
     complex(dp) :: sum_val

!! [body

     if (n <= 0) then
         mean = czero
         stddev = zero
         return
     endif

     ! compute mean
     sum_val = czero
     do i=1,n
         sum_val = sum_val + zv(i)
     enddo
     mean = sum_val / real(n, dp)

     ! compute standard deviation (root mean square distance from mean)
     if (n == 1) then
         stddev = zero
     else
         sum_sq = zero
         do i=1,n
             sum_sq = sum_sq + abs(zv(i) - mean)**2
         enddo
         stddev = sqrt(sum_sq / real(n - 1, dp))
     endif

!! body]

     return
  end subroutine s_stats_z

!!========================================================================
!!>>> vector outer product operations                                  <<<
!!========================================================================

!!
!! @sub s_outer_i
!!
!! compute outer product of two integer vectors: a(i,j) = x(i) * y(j)
!!
  subroutine s_outer_i(n, m, ix, iy, ia)
     implicit none

!! external arguments
     ! size of input vector x
     integer, intent(in)  :: n

     ! size of input vector y
     integer, intent(in)  :: m

     ! input integer vector x
     integer, intent(in)  :: ix(n)

     ! input integer vector y
     integer, intent(in)  :: iy(m)

     ! output matrix: outer product a = x * y^T
     integer, intent(out) :: ia(n,m)

!! local variables
     ! loop indices
     integer :: i, j

!! [body

     if (n <= 0 .or. m <= 0) return
     !
     do i=1,n
         do j=1,m
             ia(i,j) = ix(i) * iy(j)
         enddo ! over j={1,m} loop
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_outer_i

!!
!! @sub s_outer_d
!!
!! compute outer product of two real(dp) vectors: a(i,j) = x(i) * y(j)
!!
  subroutine s_outer_d(n, m, dx, dy, da)
     use constants, only : dp

     implicit none

!! external arguments
     ! size of input vector x
     integer, intent(in)   :: n

     ! size of input vector y
     integer, intent(in)   :: m

     ! input real(dp) vector x
     real(dp), intent(in)  :: dx(n)

     ! input real(dp) vector y
     real(dp), intent(in)  :: dy(m)

     ! output matrix: outer product a = x * y^T
     real(dp), intent(out) :: da(n,m)

!! local variables
     ! loop indices
     integer :: i, j

!! [body

     if (n <= 0 .or. m <= 0) return
     !
     do i=1,n
         do j=1,m
             da(i,j) = dx(i) * dy(j)
         enddo ! over j={1,m} loop
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_outer_d

!!
!! @sub s_outer_z
!!
!! compute outer product of two complex(dp) vectors: a(i,j) = x(i) * y(j)
!!
  subroutine s_outer_z(n, m, zx, zy, za)
     use constants, only : dp

     implicit none

!! external arguments
     ! size of input vector x
     integer, intent(in)      :: n

     ! size of input vector y
     integer, intent(in)      :: m

     ! input complex(dp) vector x
     complex(dp), intent(in)  :: zx(n)

     ! input complex(dp) vector y
     complex(dp), intent(in)  :: zy(m)

     ! output matrix: outer product a = x * y^T
     complex(dp), intent(out) :: za(n,m)

!! local variables
     ! loop indices
     integer :: i, j

!! [body

     if (n <= 0 .or. m <= 0) return
     !
     do i=1,n
         do j=1,m
             za(i,j) = zx(i) * zy(j)
         enddo ! over j={1,m} loop
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_outer_z

!!========================================================================
!!>>> vector cross product operations                                  <<<
!!========================================================================

!!
!! @sub s_cross_i
!!
!! compute cross product of two 3d integer vectors.
!! c = a x b, where c(1) = a(2)*b(3) - a(3)*b(2)
!!                    c(2) = a(3)*b(1) - a(1)*b(3)
!!                    c(3) = a(1)*b(2) - a(2)*b(1)
!!
  subroutine s_cross_i(ix, iy, iz)
     implicit none

!! external arguments
     ! input 3d integer vector a
     integer, intent(in)  :: ix(3)

     ! input 3d integer vector b
     integer, intent(in)  :: iy(3)

     ! output 3d integer vector: cross product
     integer, intent(out) :: iz(3)

!! [body

     iz(1) = ix(2) * iy(3) - ix(3) * iy(2)
     iz(2) = ix(3) * iy(1) - ix(1) * iy(3)
     iz(3) = ix(1) * iy(2) - ix(2) * iy(1)

!! body]

     return
  end subroutine s_cross_i

!!
!! @sub s_cross_d
!!
!! compute cross product of two 3d real(dp) vectors.
!! c = a x b, where c(1) = a(2)*b(3) - a(3)*b(2)
!!                    c(2) = a(3)*b(1) - a(1)*b(3)
!!                    c(3) = a(1)*b(2) - a(2)*b(1)
!!
  subroutine s_cross_d(dx, dy, dz)
     use constants, only : dp

     implicit none

!! external arguments
     ! input 3d real(dp) vector a
     real(dp), intent(in)  :: dx(3)

     ! input 3d real(dp) vector b
     real(dp), intent(in)  :: dy(3)

     ! output 3d real(dp) vector: cross product
     real(dp), intent(out) :: dz(3)

!! [body

     dz(1) = dx(2) * dy(3) - dx(3) * dy(2)
     dz(2) = dx(3) * dy(1) - dx(1) * dy(3)
     dz(3) = dx(1) * dy(2) - dx(2) * dy(1)

!! body]

     return
  end subroutine s_cross_d

!!
!! @sub s_cross_z
!!
!! compute cross product of two 3d complex(dp) vectors.
!! c = a x b, where c(1) = a(2)*b(3) - a(3)*b(2)
!!                    c(2) = a(3)*b(1) - a(1)*b(3)
!!                    c(3) = a(1)*b(2) - a(2)*b(1)
!!
  subroutine s_cross_z(zx, zy, zz)
     use constants, only : dp

     implicit none

!! external arguments
     ! input 3d complex(dp) vector a
     complex(dp), intent(in)  :: zx(3)

     ! input 3d complex(dp) vector b
     complex(dp), intent(in)  :: zy(3)

     ! output 3d complex(dp) vector: cross product
     complex(dp), intent(out) :: zz(3)

!! [body

     zz(1) = zx(2) * zy(3) - zx(3) * zy(2)
     zz(2) = zx(3) * zy(1) - zx(1) * zy(3)
     zz(3) = zx(1) * zy(2) - zx(2) * zy(1)

!! body]

     return
  end subroutine s_cross_z

!!========================================================================
!!>>> vector distance operations                                       <<<
!!========================================================================

!!
!! @sub s_distance_i
!!
!! compute euclidean distance between two integer vectors.
!! dist = sqrt( sum_i (x(i) - y(i))^2 )
!!
  subroutine s_distance_i(n, ix, iy, dist)
     use constants, only : dp
     use constants, only : zero

     implicit none

!! external arguments
     ! size of vectors
     integer, intent(in)   :: n

     ! input integer vector x
     integer, intent(in)   :: ix(n)

     ! input integer vector y
     integer, intent(in)   :: iy(n)

     ! euclidean distance: ||x - y||
     real(dp), intent(out) :: dist

!! local variables
     ! loop index
     integer :: i

     ! squared differences sum
     real(dp) :: sum_sq

!! [body

     if (n <= 0) then
         dist = zero
         return
     endif
     !
     sum_sq = zero
     do i=1,n
         sum_sq = sum_sq + real(ix(i) - iy(i), dp)**2
     enddo ! over i={1,n} loop
     dist = sqrt(sum_sq)

!! body]

     return
  end subroutine s_distance_i

!!
!! @sub s_distance_d
!!
!! compute euclidean distance between two real(dp) vectors.
!! dist = sqrt( sum_i (x(i) - y(i))^2 )
!!
  subroutine s_distance_d(n, dx, dy, dist)
     use constants, only : dp
     use constants, only : zero

     implicit none

!! external arguments
     ! size of vectors
     integer, intent(in)   :: n

     ! input real(dp) vector x
     real(dp), intent(in)  :: dx(n)

     ! input real(dp) vector y
     real(dp), intent(in)  :: dy(n)

     ! euclidean distance: ||x - y||
     real(dp), intent(out) :: dist

!! local variables
     ! loop index
     integer :: i

     ! squared differences sum
     real(dp) :: sum_sq

!! [body

     if (n <= 0) then
         dist = zero
         return
     endif
     !
     sum_sq = zero
     do i=1,n
         sum_sq = sum_sq + (dx(i) - dy(i))**2
     enddo ! over i={1,n} loop
     dist = sqrt(sum_sq)

!! body]

     return
  end subroutine s_distance_d

!!
!! @sub s_distance_z
!!
!! compute euclidean distance between two complex(dp) vectors.
!! dist = sqrt( sum_i |x(i) - y(i)|^2 )
!!
  subroutine s_distance_z(n, zx, zy, dist)
     use constants, only : dp
     use constants, only : zero

     implicit none

!! external arguments
     ! size of vectors
     integer, intent(in)      :: n

     ! input complex(dp) vector x
     complex(dp), intent(in)  :: zx(n)

     ! input complex(dp) vector y
     complex(dp), intent(in)  :: zy(n)

     ! euclidean distance: ||x - y||
     real(dp), intent(out)    :: dist

!! local variables
     ! loop index
     integer :: i

     ! squared differences sum
     real(dp) :: sum_sq

!! [body

     if (n <= 0) then
         dist = zero
         return
     endif
     !
     sum_sq = zero
     do i=1,n
         sum_sq = sum_sq + abs(zx(i) - zy(i))**2
     enddo ! over i={1,n} loop
     dist = sqrt(sum_sq)

!! body]

     return
  end subroutine s_distance_z

!!========================================================================
!!>>> vector unique operations                                         <<<
!!========================================================================

!!
!! @sub s_unique_i
!!
!! return unique elements from an integer array, preserving order.
!! returns the number of unique elements in m.
!!
  subroutine s_unique_i(n, ix, m, iy)
     implicit none

!! external arguments
     ! size of input array
     integer, intent(in)  :: n

     ! input integer array
     integer, intent(in)  :: ix(n)

     ! number of unique elements (output)
     integer, intent(out) :: m

     ! unique elements (output, size n, but only m elements are valid)
     integer, intent(out) :: iy(n)

!! local variables
     ! loop indices
     integer :: i, j

     ! flag for uniqueness
     logical :: is_unique

!! [body

     if (n <= 0) then
         m = 0
         return
     endif
     !
     m = 0
     do i=1,n
         is_unique = .true.
         do j=1,m
             if (ix(i) == iy(j)) then
                 is_unique = .false.
                 exit
             endif
         enddo ! over j={1,m} loop
         if (is_unique) then
             m = m + 1
             iy(m) = ix(i)
         endif
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_unique_i

!!
!! @sub s_unique_d
!!
!! return unique elements from a real(dp) array, preserving order.
!! returns the number of unique elements in m.
!!
  subroutine s_unique_d(n, dx, m, dy)
     use constants, only : dp
     use constants, only : eps8

     implicit none

!! external arguments
     ! size of input array
     integer, intent(in)   :: n

     ! input real(dp) array
     real(dp), intent(in)  :: dx(n)

     ! number of unique elements (output)
     integer, intent(out)  :: m

     ! unique elements (output, size n, but only m elements are valid)
     real(dp), intent(out) :: dy(n)

!! local variables
     ! loop indices
     integer :: i, j

     ! flag for uniqueness
     logical :: is_unique

!! [body

     if (n <= 0) then
         m = 0
         return
     endif
     !
     m = 0
     do i=1,n
         is_unique = .true.
         do j=1,m
             if (abs(dx(i) - dy(j)) < eps8) then
                 is_unique = .false.
                 exit
             endif
         enddo ! over j={1,m} loop
         if (is_unique) then
             m = m + 1
             dy(m) = dx(i)
         endif
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_unique_d

!!
!! @sub s_unique_z
!!
!! return unique elements from a complex(dp) array, preserving order.
!! returns the number of unique elements in m.
!!
  subroutine s_unique_z(n, zx, m, zy)
     use constants, only : dp
     use constants, only : eps8

     implicit none

!! external arguments
     ! size of input array
     integer, intent(in)      :: n

     ! input complex(dp) array
     complex(dp), intent(in)  :: zx(n)

     ! number of unique elements (output)
     integer, intent(out)     :: m

     ! unique elements (output, size n, but only m elements are valid)
     complex(dp), intent(out) :: zy(n)

!! local variables
     ! loop indices
     integer :: i, j

     ! flag for uniqueness
     logical :: is_unique

!! [body

     if (n <= 0) then
         m = 0
         return
     endif
     !
     m = 0
     do i=1,n
         is_unique = .true.
         do j=1,m
             if (abs(zx(i) - zy(j)) < eps8) then
                 is_unique = .false.
                 exit
             endif
         enddo ! over j={1,m} loop
         if (is_unique) then
             m = m + 1
             zy(m) = zx(i)
         endif
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_unique_z

!!========================================================================
!!>>> vector intersect operations                                      <<<
!!========================================================================

!!
!! @sub s_intersect_i
!!
!! compute intersection of two integer vectors.
!! returns common elements in iz, with k elements total.
!! each element appears only once (unique).
!!
  subroutine s_intersect_i(n, ix, m, iy, k, iz)
     implicit none

!! external arguments
     ! size of first vector
     integer, intent(in)  :: n

     ! first input integer vector x
     integer, intent(in)  :: ix(n)

     ! size of second vector
     integer, intent(in)  :: m

     ! second input integer vector y
     integer, intent(in)  :: iy(m)

     ! number of intersection elements (output)
     integer, intent(out) :: k

     ! intersection elements (output, size n, but only k valid)
     integer, intent(out) :: iz(n)

!! local variables
     ! loop indices
     integer :: i, j, l

     ! flag for membership
     logical :: in_y
     logical :: already_added

!! [body

     if (n <= 0 .or. m <= 0) then
         k = 0
         return
     endif
     !
     k = 0
     do i=1,n
         ! check if ix(i) is in iy
         in_y = .false.
         do j=1,m
             if (ix(i) == iy(j)) then
                 in_y = .true.
                 exit
             endif
         enddo ! over j={1,m} loop
         !
         if (in_y) then
             ! check if already added to result
             already_added = .false.
             do l=1,k
                 if (ix(i) == iz(l)) then
                     already_added = .true.
                     exit
                 endif
             enddo ! over l={1,k} loop
             if (.not. already_added) then
                 k = k + 1
                 iz(k) = ix(i)
             endif
         endif
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_intersect_i

!!
!! @sub s_intersect_d
!!
!! compute intersection of two real(dp) vectors.
!! returns common elements in dz, with k elements total.
!! each element appears only once (unique).
!!
  subroutine s_intersect_d(n, dx, m, dy, k, dz)
     use constants, only : dp
     use constants, only : eps8

     implicit none

!! external arguments
     ! size of first vector
     integer, intent(in)   :: n

     ! first input real(dp) vector x
     real(dp), intent(in)  :: dx(n)

     ! size of second vector
     integer, intent(in)   :: m

     ! second input real(dp) vector y
     real(dp), intent(in)  :: dy(m)

     ! number of intersection elements (output)
     integer, intent(out)  :: k

     ! intersection elements (output, size n, but only k valid)
     real(dp), intent(out) :: dz(n)

!! local variables
     ! loop indices
     integer :: i, j, l

     ! flag for membership
     logical :: in_y
     logical :: already_added

!! [body

     if (n <= 0 .or. m <= 0) then
         k = 0
         return
     endif
     !
     k = 0
     do i=1,n
         ! check if dx(i) is in dy
         in_y = .false.
         do j=1,m
             if (abs(dx(i) - dy(j)) < eps8) then
                 in_y = .true.
                 exit
             endif
         enddo ! over j={1,m} loop
         !
         if (in_y) then
             ! check if already added to result
             already_added = .false.
             do l=1,k
                 if (abs(dx(i) - dz(l)) < eps8) then
                     already_added = .true.
                     exit
                 endif
             enddo ! over l={1,k} loop
             if (.not. already_added) then
                 k = k + 1
                 dz(k) = dx(i)
             endif
         endif
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_intersect_d

!!
!! @sub s_intersect_z
!!
!! compute intersection of two complex(dp) vectors.
!! returns common elements in zz, with k elements total.
!! each element appears only once (unique).
!!
  subroutine s_intersect_z(n, zx, m, zy, k, zz)
     use constants, only : dp
     use constants, only : eps8

     implicit none

!! external arguments
     ! size of first vector
     integer, intent(in)      :: n

     ! first input complex(dp) vector x
     complex(dp), intent(in)  :: zx(n)

     ! size of second vector
     integer, intent(in)      :: m

     ! second input complex(dp) vector y
     complex(dp), intent(in)  :: zy(m)

     ! number of intersection elements (output)
     integer, intent(out)     :: k

     ! intersection elements (output, size n, but only k valid)
     complex(dp), intent(out) :: zz(n)

!! local variables
     ! loop indices
     integer :: i, j, l

     ! flag for membership
     logical :: in_y
     logical :: already_added

!! [body

     if (n <= 0 .or. m <= 0) then
         k = 0
         return
     endif
     !
     k = 0
     do i=1,n
         ! check if zx(i) is in zy
         in_y = .false.
         do j=1,m
             if (abs(zx(i) - zy(j)) < eps8) then
                 in_y = .true.
                 exit
             endif
         enddo ! over j={1,m} loop
         !
         if (in_y) then
             ! check if already added to result
             already_added = .false.
             do l=1,k
                 if (abs(zx(i) - zz(l)) < eps8) then
                     already_added = .true.
                     exit
                 endif
             enddo ! over l={1,k} loop
             if (.not. already_added) then
                 k = k + 1
                 zz(k) = zx(i)
             endif
         endif
     enddo ! over i={1,n} loop

!! body]

     return
  end subroutine s_intersect_z
