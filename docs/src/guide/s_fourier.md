## Introduction

Support forward and backward fourier transformation. We usually use the following subroutines to perform fourier transformation for the Green's function.

## Type

subroutines

## Source

`src/s_fourier.f90`

## Usage

(1) Forward FFT, from ``G(\tau) \to G(i\omega_n)``.

```fortran
subroutine s_fft_forward(...)
```

(2) Backward FFT, from ``G(i\omega_n) \to G(\tau)``

```fortran
subroutine s_fft_backward(...)
subroutine s_fft_tails(...)
```

!!! note

    The `s_fft_tails()` subroutine is called by the `s_fft_backward()` subroutine internally. DO NOT call it directly!

## Theory
