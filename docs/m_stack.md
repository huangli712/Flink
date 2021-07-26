!!
!!
!! Introduction
!! ============
!!
!! In this module, we implement two types of stack, istack and gstack. The
!! istack type was designed to deal with integer numbers only. However,
!! gstack is a generic type stack. More specifically, it supports the
!! following four data types:
!!     integer,
!!     logical,
!!     real(dp),
!!     complex(dp).
!! The usages, subroutine parameters for these two stack types are almost
!! identical. To implement gstack type, we generally use the unlimited
!! polymorphic features in fortran 2003/2008 standard. Noted that not all
!! fortran compilers can support these features. This module was tested
!! using intel fortran compiler only. We do not guarantee it can work/be
!! compiled correctly for using the other fortran compilers. So please
!! use it carefully. In the iqist project, so far we only use the istack
!! type. However, in the future, we will turn to the gstack type.
!!
!! Usage
!! =====
!!
!! 1. import stack support
!! -----------------------
!!
!! use stack
!!
!! 2. declare stack
!! ----------------
!!
!! type (istack) :: is
!! type (gstack) :: gs
!!
!! 3. create stack
!! ---------------
!!
!! call istack_create(is, 1024)
!! call gstack_create(gs, 1, 1024) ! create stack to support integer
!! call gstack_create(gs, .true., 1024) ! create stack to support logical
!! call gstack_create(gs, 1.0_dp, 1024) ! create stack to support real(dp)
!! call gstack_create(gs, (1.0_dp, 1.0_dp), 1024) ! create stack to support complex(dp)
!!
!! note: In istack_create(), the second parameter is the capacity of the
!! stack. However, in gstack_create(), the second parameter means the data
!! type that gstack will manipulate, and the third parameter will be used
!! to determine the capacity. It is an optional parameter.
!!
!! 4. push element
!! ---------------
!!
!! call istack_push(is, 1)
!! call gstack_push(gs, 1)
!! call gstack_push(gs, .true.)
!! call gstack_push(gs, 2.0_dp)
!! call gstack_push(gs, (1.0_dp, 1.0_dp))
!!
!! 5. pop element
!! --------------
!!
!! call istack_pop(is, i) ! i is an integer
!! call gstack_pop(is, j) ! j can be integer, logical, real(dp), and complex(dp)
!!
!! 6. check the status of stack
!! ----------------------------
!!
!! print *, istack_isfull(is)
!! print *, istack_isempty(is)
!! print *, istack_getsize(is)
!! print *, istack_getrest(is)
!!
!! print *, gstack_isfull(gs)
!! print *, gstack_isempty(gs)
!! print *, gstack_getsize(gs)
!! print *, gstack_getrest(gs)
!!
!! The above three function calls will tell you whether the stack is full,
!! whether it is empty, and its capacity.
!!
!! 7. clean the stack
!! ------------------
!!
!! call istack_clean(is)
!! call gstack_clean(gs)
!!
!! note: This operation will reset the top position of the stack, instead
!! of releasing the memory of it. So you can still use the stack after that.
!!
!! 8. destroy the stack
!! --------------------
!!
!! call istack_destroy(is)
!! call gstack_destroy(gs)
!!
!! note: When the stack was destroyed, you can not use it any more.
!!
!!