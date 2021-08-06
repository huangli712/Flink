As mentioned before, almost all of my Fortran codes depend on the *Flink* library. So, if you want to try my codes, perhaps you have to compile and install *Flink* at first. Do not be nervous. It is quite easy.

At first, you have to download the latest source codes from the official repository of [*Flink*](https://github.com/huangli712/flink). If every thing is OK, you will have a compressed file, such as `flink.zip` or `flink.tar.gz`. And then you need to decompress it with `unzip` or `tar`. The next step is to compile *Flink*. Please execute the following three commands in your terminal.

```sh
$ cd flink/build
$ editor make.inc
$ make
```

Once the compilation is finished (It takes about a few seconds usually), we can find `libflink.a` and a lot of `*.mod` files under the `flink/src` directory. That is all.

!!! note

    In order to compile *Flink*, you have to ensure that a MPI-enabled Fortran compiler (which is compatibe with Fortran 2003 standard), and a numerical library which implements the BLAS and LAPACK interfaces (such as Intel MKL), are installed and configured correctly in your system. You can setup your compiling environment via editing the `build/make.inc` file.

    See [make.inc](appendix/make.md) for more details.
