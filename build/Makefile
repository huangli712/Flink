##
##
## Introduction
## ============
##
## This is the top Makefile for the FLINK software package. You can use
## it to build all of the executable programs and libraries. Please do
## not modify it by yourself even you are very familiar with FLINK.
##
## Usage
## =====
##
## 1. get help
## -----------
##
## 'make help'
##
## 2. quick build
## --------------
##
## 'make' or 'make flink'
##
## 3. clean the directories
## ------------------------
##
## 'make clean'
##
## Author
## ======
##
## This building system is designed, created, and maintained by
##
## Li Huang // email: lihuang.dmft@gmail.com
##
## History
## =======
##
## 04/09/2019 by li huang (created)
## 04/09/2019 by li huang (last modified)
##
##

# targets for help
help:
	@echo '  Interacting Quantum Impurity Solver Toolkit Building System'
	@echo
	@echo
	@echo '  targets:'
	@echo '     help              print short usage information         '
	@echo '     help-more         print full usage information          '
	@echo
	@echo '     all               build all components and tools        '
	@echo '     clean             clean all directories                 '
	@echo
	@echo '     solver            build impurity solver components      '
	@echo '     clean-solver      clean objects for impurity solvers    '
	@echo
	@echo '     tool              build auxiliary tool components       '
	@echo '     clean-tool        clean objects for auxiliary tools     '

# all-in-one building target
all: base solver tool
