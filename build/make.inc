##
##
## Introduction
## ============
##
## This is the official configuration file for the building system. You
## should modify it to fulfill your requirements. The make.inc file is
## the key component of the building system. If it is not configured
## correctly, the building system won't work correctly as well.
##
## Author
## ======
##
## This building system is designed, created, and maintained by
##
## Li Huang // email: huangli@caep.cn
##
## History
## =======
##
## 05/11/2015 by li huang (created)
## 06/22/2024 by li huang (last modified)
##
##

# Fortran compiler, linker, and archiver
#-------------------------------------------------------------------------
F90    = mpifort
LINKER = $(F90)
ARCHIVER = ar -ruv

# Fortran preprocessor options
#-------------------------------------------------------------------------
MPI    = -DMPI
OMP    = #-qopenmp
FPP    = -fpp
CPP    = $(FPP) $(MPI) $(OMP)

# Machine tuning options
#-------------------------------------------------------------------------
CHECK  = -nogen-interfaces -warn all #-check all -traceback -g
MTUNE  = -O3 -xHost

# Flags for fortran compiler and linker
#-------------------------------------------------------------------------
FFLAGS = -c $(CPP) $(CHECK) $(MTUNE)
LFLAGS = $(OMP)
