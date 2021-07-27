!!
!!
!! Introduction
!! ============
!!
!! 1. forward FFT, from \tau -> i\omega
!! ------------------------------------
!!
!! subroutine s_fft_forward(...)
!!
!! 2. backward FFT, from i\omega -> \tau
!! -------------------------------------
!!
!! subroutine s_fft_backward(...)
!! subroutine s_fft_tails(...)
!!
!! note: the s_fft_tails() subroutine is called by the s_fft_backward()
!! subroutine internally. DO NOT call it directly!
!!
!!
