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

  contains

     ! Function to check if a node is red
     logical function isRed(x)
         implicit none

         ! The node to check
         type(node_t), pointer, intent(in) :: x

         if ( associated(x) ) then
             isRed = (x%color == RED)
         else
             isRed = .false.
         end if

         return
     end function isRed

     ! Function to get the size of the subtree rooted at a given node
     integer function size(x)
         implicit none

         ! The node whose subtree size is to be calculated
         type(node_t), pointer, intent(in) :: x

         if ( associated(x) ) then
             size = x%size
         else
             size = 0
         end if

         return
     end function size

     ! Function to get the value associated with the given key
     integer function get(tree, key) result(value)
         implicit none

         ! The red-black tree to search
         class(tree_t), intent(in) :: tree

         ! The key to search for
         integer, intent(in) :: key

         type(node_t), pointer :: current

         ! Initialize the current pointer to the root of the tree
         current => tree%root

         ! Search for the key in the tree
         do while (associated(current))
             if (key < current%key) then
                 current => current%left
             else if (key > current%key) then
                 current => current%right
             else
                 ! Key found, return the associated value
                 value = current%value
                 return
             end if
         end do

         ! If we reach here, the key was not found
         ! Assuming -1 is an invalid value for demonstration
         value = -1

         return
     end function get

     ! Function to check if the symbol table contains the given key
     logical function contains(tree, key)
         implicit none

         ! The red-black tree to search
         class(tree_t), intent(in) :: tree

         ! The key to search for
         integer, intent(in) :: key

         ! Call the get function and check if the returned value is valid
         ! Assuming -1 is an invalid value
         contains = (get(tree, key) /= -1)

         return
     end function contains

     function rotateRight(h) result(x)
         implicit none

         type(node_t), pointer, intent(inout) :: h

         type(node_t), pointer :: x
       
         x => h%left
         h%left => x%right
         x%right => h
         x%color = h%color
         h%color = RED
         x%size = h%size
         h%size = size(h%left) + size(h%right) + 1
 
         return
     end function rotateRight

     function rotateLeft(h) result(x)
         implicit none

         type(node_t), pointer, intent(inout) :: h

         type(node_t), pointer :: x

         x => h%right
         h%right => x%left
         x%left => h
         x%color = h%color
         h%color = RED
         x%size = h%size
         h%size = size(h%left) + size(h%right) + 1

         return
     end function rotateLeft

  end module rbtree
