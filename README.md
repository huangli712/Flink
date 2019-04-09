# FLINK

The FLINK project is a collection of some useful fortran modules and subroutines, which are widely used in the other numerical programs. 

## Version

v0.9.0 (devel)

## License

GNU General Public License Version 3

## Features

* Modules
    * Physical and numerical constants (m\_constants.f90)
    * Linked list (m\_linkedlist.f90)
    * Message passing interface (m\_mpi.f90)
    * Sparse matrix (m\_sparse.f90)
    * Configuration file (m\_parser.f90)
    * Pseudo random number generator (m\_spring.f90)
    * Stack (m\_stack.f90)
    * Version information (m\_version.f90)

* Subroutines
    * Warning and error messages (s\_error.f90)
    * Fourier transformation (s\_fourier.f90)
    * Special functions (s\_function.f90)
    * Numerical integrl (s\_integrator.f90)
    * Matrix computation (s\_matrix.f90)
    * Spline interpolation (s\_spline.f90)
    * Some auxiliary subroutines, such as sorting algorithm (s\_util.f90)
    * Vector computation (s\_vector.90)

## Installation

```sh
$ cd flink/build
$ editor make.sys
$ make
```

## Documentation

See the comments in the source codes.

## Development

The FLINK software package is developed and maintained by the FLINK Developer Team.

Find a bug? Want to contribute? Want new features? Great! Please contact with us as soon as possible.

## Reference

N/A

## Contact

```sh
Li Huang
Institute of Materials, China Academy of Engineering Physics, Sichuan Jiangyou, PRC
email: lihuang.dmft at gmail.com
```
