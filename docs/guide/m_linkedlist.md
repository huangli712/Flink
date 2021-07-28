## Introduction

This implementation of generic linked list in fortran 90 was taken from

> Jason R. Blevins's code<br/>
> journal: ACM Fortran Forum 28(3), 2-7, 2009.<br/>
> website: http://jblevins.org/research/generic-list.

Of course, we have adapted the original code to fulfill our requirement. This linked list is capable of storing data of an any type by using the generic programming techniques. To access the data stored in the nodes, we have to use the intrinsic `transfer()` subroutine.

## Usage

1. include linked list support
```fortran
use linkedlist
```

2. define user own data type
```fortran
type data_t
     ! place your definition here
end type data_t
```

3. define pointer to data
```fortran
type (data_t), pointer  :: data_ptr => null()
```

!! 4. define pointer to list
!! -------------------------
!!
!! type (list_t), pointer  :: list_ptr => null()
!!
!! 5. prepare data
!! ---------------
!!
!! allocate(data_ptr)
!! data_ptr%something = something
!!
!! 6. create a linked list
!! -----------------------
!!
!! call list_init(list_ptr, transfer(data_ptr, list_d))
!!
!! here list_d is a public variable in linkedlist module.
!!
!! 7. insert new node
!! ------------------
!!
!! call list_insert(list_ptr, transfer(data_ptr, list_d))
!!
!! list_insert() will always insert the new node after the given node,
!! i.e., list_ptr. If you want to update the data for the given node,
!! please use the following code:
!!
!! call list_put(list_ptr, transfer(data_ptr, list_d))
!!
!! 8. visit next node
!! ------------------
!!
!! curr => list_next(curr)
!!
!! here curr is a list_t type pointer.
!!
!! 9. retrieve data stored in the node
!! -----------------------------------
!!
!! data_ptr  = transfer(list_get(curr), data_ptr)
!! something = data_ptr%something
!!
!! here curr is a list_t type pointer, it points to the current node.
!!
!! 10. free memory for this linked list
!! ------------------------------------
!!
!! call list_free(list_ptr)
!!
!!
