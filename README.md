# Flink

The Flink library is a collection of fortran modules and subroutines for scientific computing.

## Version

v1.0.0-devel.210720

## License

GNU General Public License Version 3

## Features

* Modules
    * Physical and numerical constants (**m\_constants.f90**)
    * Linked list (**m\_linkedlist.f90**)
    * Message passing interface (**m\_mpi.f90**)
    * Sparse matrix (**m\_sparse.f90**)
    * Configuration file (**m\_parser.f90**)
    * Pseudo random number generator (**m\_spring.f90**)
    * Stack (**m\_stack.f90**)
    * Analytical tetrahedron method (**m\_tetra.f90**)
    * Version information (**m\_version.f90**)

* Subroutines
    * Warning and error messages (**s\_error.f90**)
    * Fourier transformation (**s\_fourier.f90**)
    * Special functions (**s\_function.f90**)
    * Numerical integral (**s\_integrator.f90**)
    * Matrix computation (**s\_matrix.f90**)
    * Spline interpolation (**s\_spline.f90**)
    * Some auxiliary subroutines, such as sorting algorithm (**s\_util.f90**)
    * Vector computation (**s\_vector.90**)

## Installation

```sh
$ cd flink/build
$ editor make.sys
$ make
```
Once the compilation is finished, we can see **libflink.a** and a lot of ***.mod files** under the **flink/src** directory. Then we need to link them with our executable programs manually.

## Documentation

See the comments in the source codes.

## Development

The Flink library is developed and maintained by the Flink Developer Team.

Find a bug? Want to contribute? Want new features? Great! Please contact with us as soon as possible.

## Reference

N/A

## Contact

```sh
Li Huang
Institute of Materials, China Academy of Engineering Physics, Sichuan Jiangyou, PRC
email: lihuang.dmft at gmail.com
```
