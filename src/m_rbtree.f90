  module rbtree
     implicit none

     ! Define constants for node colors in the red-black tree
     integer, parameter :: RED = 1
     integer, parameter :: BLACK = 0

     ! Define a derived type for the nodes of the red-black tree
     type node_t

         ! Key of the node (using integer for demonstration purposes)
         integer :: key

         ! Value associated with the key (using integer for demonstration purposes)
         integer :: value

         ! Pointers to the left and right child nodes, initialized to null
         type(node_t), pointer :: left => null()
         type(node_t), pointer :: right => null()

         ! Color of the node, either RED or BLACK
         integer :: color

         ! Size of the subtree rooted at this node, for maintaining tree balance
         integer :: size

     end type node_t     

     ! Define a derived type for the red-black binary search tree (BST)
     type tree_t

         ! Pointer to the root node of the tree, initialized to null
         type(node_t), pointer :: root => null()

     end type tree_t

     ! Function to check if a node is red
     logical function isRed(x)
         implicit none

         ! The node to check
         type(node_t), intent(in) :: x

         if ( associated(x) ) then
             isRed = (x%color == RED)
         else
             isRed = .false.
         end if

         return
     end function isRed

    ! Function to get the size of the subtree rooted at a given node
    integer function size(node)
        type(Node), intent(in) :: node  ! The node whose subtree size is to be calculated
        if (associated(node)) then
            size = node%size
        else
            size = 0  ! If the node is null, the size of the subtree is 0
        end if
    end function size
  end module rbtree
