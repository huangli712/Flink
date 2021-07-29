!!!-----------------------------------------------------------------------
!!! project : flink @ sakura
!!! program : stack
!!! source  : m_stack.f90
!!! type    : module
!!! author  : li huang (email:lihuang.dmft@gmail.com)
!!! history : 09/14/2009 by li huang (created)
!!!           07/30/2021 by li huang (last modified)
!!! purpose : the purpose of this module is to define a stack-type (LIFO)
!!!           data structure in fortran version.
!!! status  : unstable
!!! comment :
!!!-----------------------------------------------------------------------

  module stack
     implicit none

!!========================================================================
!!>>> declare global parameters                                        <<<
!!========================================================================

!! module parameters
     ! dp: number precision, double precision for reals
     integer, private, parameter :: dp    = kind(1.0d0)

     ! mystd: device descriptor, console output
     integer, private, parameter :: mystd = 6

     ! stack size limit, default value
     integer, private, parameter :: limit = 1024

!!========================================================================
!!>>> declare global data structure                                    <<<
!!========================================================================

!! module structs
     ! define integer type stack
     type istack

         ! top position of stack
         integer :: top

         ! size of allocatable array
         integer :: nsize

         ! allocatable array, which is used to store elements in stack.
         integer, allocatable :: item(:)

     end type istack

     ! define generic type stack
     type gstack

         ! top position of stack
         integer :: top

         ! size of allocatable array
         integer :: nsize

         ! allocatable array, which is used to store elements in stack.
         ! note: it is an unlimited polymorphic object.
         class(*), allocatable :: item(:)

     end type gstack

!!========================================================================
!!>>> declare accessibility for module routines                        <<<
!!========================================================================

     public :: istack_create
     public :: istack_clean
     public :: istack_destroy

     public :: gstack_create
     public :: gstack_clean
     public :: gstack_destroy

     public :: istack_copyer
     public :: istack_setter
     public :: istack_getter

     public :: gstack_copyer
     public :: gstack_setter
     public :: gstack_getter

     public :: istack_push
     public :: istack_pop

     public :: gstack_push
     public :: gstack_pop

     public :: istack_display

     public :: gstack_display

     public :: istack_gettop
     public :: istack_getrest
     public :: istack_getsize

     public :: gstack_gettop
     public :: gstack_getrest
     public :: gstack_getsize

     public :: istack_isfull
     public :: istack_isempty

     public :: gstack_isfull
     public :: gstack_isempty

  contains ! encapsulated functionality

!!
!! @sub istack_create
!!
!! create and initialize an integer type stack.
!!
  subroutine istack_create(s, n)
     implicit none

!! external arguments
     ! size of stack
     integer, optional, intent(in) :: n

     ! integer type stack
     type (istack), intent(out)    :: s

!! [body

     ! determine the capacity of stack
     if ( present (n) ) then
         s%nsize = n
     else
         s%nsize = limit
     endif ! back if ( present (n) ) block

     ! setup the top position
     s%top = 0

     ! allocate memory for item array
     allocate(s%item(s%nsize))

!! body]

     return
  end subroutine istack_create

!!
!! @sub gstack_create
!!
!! create and initialize a generic type stack.
!!
  subroutine gstack_create(s, t, n)
     implicit none

!! external arguments
     ! size of stack
     integer, optional, intent(in) :: n

     ! mold for the elements in the stack
     class(*), intent(in)          :: t

     ! generic type stack
     type (gstack), intent(out)    :: s

!! [body

     ! determine the capacity of stack
     if ( present (n) ) then
         s%nsize = n
     else
         s%nsize = limit
     endif ! back if ( present (n) ) block

     ! setup the top position
     s%top = 0

     ! allocate memory for item array
     select type (t)
         type is (integer)
             allocate(s%item(s%nsize), source = 0)

         type is (logical)
             allocate(s%item(s%nsize), source = .true.)

         type is (real(dp))
             allocate(s%item(s%nsize), source = 0.0_dp)

         type is (complex(dp))
             allocate(s%item(s%nsize), source = (0.0_dp, 0.0_dp))
     end select

!! body]

     return
  end subroutine gstack_create

!!
!! @sub istack_clean
!!
!! reset the integer type stack, clean all its elements.
!!
  subroutine istack_clean(s)
     implicit none

!! external arguments
     ! integer type stack
     type (istack), intent(inout) :: s

!! [body

     ! reset top position
     s%top = 0

!! body]

     return
  end subroutine istack_clean

!!
!! @sub gstack_clean
!!
!! reset the generic type stack, clean all its elements.
!!
  subroutine gstack_clean(s)
     implicit none

!! external arguments
     ! generic type stack
     type (gstack), intent(inout) :: s

!! [body

     ! reset top position
     s%top = 0

!! body]

     return
  end subroutine gstack_clean

!!
!! @sub istack_destroy
!!
!! destroy and finalize an integer type stack.
!!
  subroutine istack_destroy(s)
     implicit none

!! external arguments
     ! integer type stack
     type (istack), intent(inout) :: s

!! [body

     ! deallocate memory
     if ( allocated(s%item) ) deallocate(s%item)

     ! reset top position
     s%top = 0

!! body]

     return
  end subroutine istack_destroy

!!
!! @sub gstack_destroy
!!
!! destroy and finalize a generic type stack.
!!
  subroutine gstack_destroy(s)
     implicit none

!! external arguments
     ! generic type stack
     type (gstack), intent(inout) :: s

!! [body

     ! deallocate memory
     if ( allocated(s%item) ) deallocate(s%item)

     ! reset top position
     s%top = 0

!! body]

     return
  end subroutine gstack_destroy

!!
!! @sub istack_copyer
!!
!! copy an istack object to another.
!!
  subroutine istack_copyer(sa, sb)
     implicit none

!! external arguments
     ! integer type stack, input
     type (istack), intent(in)    :: sa

     ! integer type stack, output
     type (istack), intent(inout) :: sb

!! [body

     ! check nsize at first
     if ( sa%nsize /= sb%nsize ) then
         write(mystd,'(a)') 'istack: the sizes of two stacks are not equal'
         STOP
     endif ! back if ( sa%nsize /= sb%nsize ) block

     ! sync the data
     sb%top = sa%top
     sb%item = sa%item

!! body]

     return
  end subroutine istack_copyer

!!
!! @sub gstack_copyer
!!
!! copy an gstack object to another.
!!
  subroutine gstack_copyer(sa, sb)
     implicit none

!! external arguments
     ! generic type stack, input
     type (gstack), intent(in)    :: sa

     ! generic type stack, output
     type (gstack), intent(inout) :: sb

!! [body

     ! check nsize at first
     if ( sa%nsize /= sb%nsize ) then
         write(mystd,'(a)') 'gstack: the sizes of two stacks are not equal'
         STOP
     endif ! back if ( sa%nsize /= sb%nsize ) block

     ! sync the data
     sb%top = sa%top
     !
     select type (v => sb%item)
         type is (integer)
             select type (u => sa%item)
                 type is (integer)
                     v = u
             end select

         type is (logical)
             select type (u => sa%item)
                 type is (logical)
                     v = u
             end select

         type is (real(dp))
             select type (u => sa%item)
                 type is (real(dp))
                     v = u
             end select

         type is (complex(dp))
             select type (u => sa%item)
                 type is (complex(dp))
                     v = u
             end select
     end select

!! body]

     return
  end subroutine gstack_copyer

!!
!! @sub istack_setter
!!
!! update the item's value of stack at specified position.
!!
  subroutine istack_setter(s, pos, item)
     implicit none

!! external arguments
     ! integer type stack
     type (istack), intent(inout) :: s

     ! position of the element to be updated
     integer, intent(in)          :: pos

     ! elements to be setted
     integer, intent(in)          :: item

!! [body

     if ( pos < 1 .or. pos > s%nsize ) then
         write(mystd,'(a)') 'istack: the position is not correct'
         STOP
     else
         s%item(pos) = item
     endif ! back if ( pos < 1 .or. pos > s%nsize ) block

!! body]

     return
  end subroutine istack_setter

!!
!! @sub gstack_setter
!!
!! update the item's value of stack at specified position.
!!
  subroutine gstack_setter(s, pos, item)
     implicit none

!! external arguments
     ! generic type stack
     type (gstack), intent(inout) :: s

     ! position of the element to be updated
     integer, intent(in)          :: pos

     ! elements to be setted
     class(*), intent(in)         :: item

!! [body

     if ( pos < 1 .or. pos > s%nsize ) then
         write(mystd,'(a)') 'gstack: the position is not correct'
         STOP
     else
         select type (v => s%item)
             type is (integer)
                 select type (item)
                     type is (integer)
                         v(pos) = item
                 end select

             type is (logical)
                 select type (item)
                     type is (logical)
                         v(pos) = item
                 end select

             type is (real(dp))
                 select type (item)
                     type is (real(dp))
                         v(pos) = item
                 end select

             type is (complex(dp))
                 select type (item)
                     type is (complex(dp))
                         v(pos) = item
                 end select
         end select
     endif ! back if ( pos < 1 .or. pos > s%nsize ) block

!! body]

     return
  end subroutine gstack_setter

!!
!! @sub istack_getter
!!
!! return the item's value of stack at specified position.
!!
  subroutine istack_getter(s, pos, item)
     implicit none

!! external arguments
     ! integer type stack
     type (istack), intent(in) :: s

     ! position of the item
     integer, intent(in)       :: pos

     ! the item's value
     integer, intent(out)      :: item

!! [body

     if ( pos < 1 .or. pos > s%nsize ) then
         write(mystd,'(a)') 'istack: the position is not correct'
         STOP
     else
         item = s%item(pos)
     endif ! back if ( pos < 1 .or. pos > s%nsize ) block

!! body]

     return
  end subroutine istack_getter

!!
!! @sub gstack_getter
!!
!! return the item's value of stack at specified position.
!!
  subroutine gstack_getter(s, pos, item)
     implicit none

!! external arguments
     ! generic type stack
     type (gstack), intent(in) :: s

     ! position of the item
     integer, intent(in)       :: pos

     ! the item's value
     class(*), intent(out)     :: item

!! [body

     if ( pos < 1 .or. pos > s%nsize ) then
         write(mystd,'(a)') 'gstack: the position is not correct'
         STOP
     else
         select type (v => s%item)
             type is (integer)
                 select type (item)
                     type is (integer)
                         item = v(pos)
                 end select

             type is (logical)
                 select type (item)
                     type is (logical)
                         item = v(pos)
                 end select

             type is (real(dp))
                 select type (item)
                     type is (real(dp))
                         item = v(pos)
                 end select

             type is (complex(dp))
                 select type (item)
                     type is (complex(dp))
                         item = v(pos)
                 end select
         end select
     endif ! back if ( pos < 1 .or. pos > s%nsize ) block

!! body]

     return
  end subroutine gstack_getter

!!
!! @sub istack_push
!!
!! push item on top of stack.
!!
  subroutine istack_push(s, item)
     implicit none

!! external arguments
     ! integer type stack
     type (istack), intent(inout) :: s

     ! elements to be pushed in the stack
     integer, intent(in)          :: item

!! [body

     if ( s%top == s%nsize ) then
         write(mystd,'(a)') 'istack: the stack is full, can not push any item on it'
         STOP
     else
         s%top = s%top + 1
         s%item(s%top) = item
     endif ! back if ( s%top == s%nsize ) block

!! body]

     return
  end subroutine istack_push

!!
!! @sub gstack_push
!!
!! push item on top of stack.
!!
  subroutine gstack_push(s, item)
     implicit none

!! external arguments
     ! generic type stack
     type (gstack), intent(inout) :: s

     ! elements to be pushed in the stack
     class(*), intent(in)         :: item

!! [body

     if ( s%top == s%nsize ) then
         write(mystd,'(a)') 'gstack: the stack is full, can not push any item on it'
         STOP
     else
         s%top = s%top + 1
         select type (v => s%item)
             type is (integer)
                 select type (item)
                     type is (integer)
                         v(s%top) = item
                 end select

             type is (logical)
                 select type (item)
                     type is (logical)
                         v(s%top) = item
                 end select

             type is (real(dp))
                 select type (item)
                     type is (real(dp))
                         v(s%top) = item
                 end select

             type is (complex(dp))
                 select type (item)
                     type is (complex(dp))
                         v(s%top) = item
                 end select
         end select
     endif ! back if ( s%top == s%nsize ) block

!! body]

     return
  end subroutine gstack_push

!!
!! @sub istack_pop
!!
!! pop off item from the top of stack.
!!
  subroutine istack_pop(s, item)
     implicit none

!! external arguments
     ! integer type stack
     type (istack), intent(inout) :: s

     ! the top item in the stack
     integer, intent(out)         :: item

!! [body

     if ( s%top == 0 ) then
         write(mystd,'(a)') 'istack: the stack is empty, can not pop off any item from it'
         STOP
     else
         item = s%item(s%top)
         s%top = s%top - 1
     endif ! back if ( s%top == 0 ) block

!! body]

     return
  end subroutine istack_pop

!!
!! @sub gstack_pop
!!
!! pop off item from the top of stack.
!!
  subroutine gstack_pop(s, item)
     implicit none

!! external arguments
     ! generic type stack
     type (gstack), intent(inout) :: s

     ! the top item in the stack
     class(*), intent(out)        :: item

!! [body

     if ( s%top == 0 ) then
         write(mystd,'(a)') 'gstack: the stack is empty, can not pop off any item from it'
         STOP
     else
         select type (v => s%item)
             type is (integer)
                 select type (item)
                     type is (integer)
                         item = v(s%top)
                 end select

             type is (logical)
                 select type (item)
                     type is (logical)
                         item = v(s%top)
                 end select

             type is (real(dp))
                 select type (item)
                     type is (real(dp))
                         item = v(s%top)
                 end select

             type is (complex(dp))
                 select type (item)
                     type is (complex(dp))
                         item = v(s%top)
                 end select
         end select
         s%top = s%top - 1
     endif ! back if ( s%top == 0 ) block

!! body]

     return
  end subroutine gstack_pop

!!
!! @sub istack_display
!!
!! display the top item in the stack without pop it off.
!!
  subroutine istack_display(s, item)
     implicit none

!! external arguments
     ! integer type stack
     type (istack), intent(in) :: s

     ! the top item in the stack
     integer, intent(out)      :: item

!! [body

     if ( s%top == 0 ) then
         write(mystd,'(a)') 'istack: the stack is empty, can not return the top item of it'
         STOP
     else
         item = s%item(s%top)
     endif ! back if ( s%top == 0 ) block

!! body]

     return
  end subroutine istack_display

!!
!! @sub gstack_display
!!
!! display the top item in the stack without pop it off.
!!
  subroutine gstack_display(s, item)
     implicit none

!! external arguments
     ! generic type stack
     type (gstack), intent(in) :: s

     ! the top item in the stack
     class(*), intent(out)     :: item

!! [body

     if ( s%top == 0 ) then
         write(mystd,'(a)') 'gstack: the stack is empty, can not return the top item of it'
         STOP
     else
         select type (v => s%item)
             type is (integer)
                 select type (item)
                     type is (integer)
                         item = v(s%top)
                 end select

             type is (logical)
                 select type (item)
                     type is (logical)
                         item = v(s%top)
                 end select

             type is (real(dp))
                 select type (item)
                     type is (real(dp))
                         item = v(s%top)
                 end select

             type is (complex(dp))
                 select type (item)
                     type is (complex(dp))
                         item = v(s%top)
                 end select
         end select
     endif ! back if ( s%top == 0 ) block

!! body]

     return
  end subroutine gstack_display

!!
!! @fun istack_gettop
!!
!! return the top position of the stack, i.e, the number of items stored
!! in the stack currently.
!!
  integer &
  function istack_gettop(s) result (t)
     implicit none

!! external arguments
     ! integer type stack
     type (istack), intent(in) :: s

!! [body

     t = s%top

!! body]

     return
  end function istack_gettop

!!
!! @fun gstack_gettop
!!
!! return the top position of the stack, i.e, the number of items stored
!! in the stack currently.
!!
  integer &
  function gstack_gettop(s) result (t)
     implicit none

!! external arguments
     ! generic type stack
     type (gstack), intent(in) :: s

!! [body

     t = s%top

!! body]

     return
  end function gstack_gettop

!!
!! @fun istack_getrest
!!
!! return the number of empty sites of the stack.
!!
  integer &
  function istack_getrest(s) result (r)
     implicit none

!! external arguments
     ! integer type stack
     type (istack), intent(in) :: s

!! [body

     r = s%nsize - s%top

!! body]

     return
  end function istack_getrest

!!
!! @fun gstack_getrest
!!
!! return the number of empty sites of the stack.
!!
  integer &
  function gstack_getrest(s) result (r)
     implicit none

!! external arguments
     ! generic type stack
     type (gstack), intent(in) :: s

!! [body

     r = s%nsize - s%top

!! body]

     return
  end function gstack_getrest

!!
!! @fun istack_getsize
!!
!! return the actual capacity of the stack.
!!
  integer &
  function istack_getsize(s) result (n)
     implicit none

!! external arguments
     ! integer type stack
     type (istack), intent(in) :: s

!! [body

     n = s%nsize

!! body]

     return
  end function istack_getsize

!!
!! @fun gstack_getsize
!!
!! return the actual capacity of the stack.
!!
  integer &
  function gstack_getsize(s) result (n)
     implicit none

!! external arguments
     ! generic type stack
     type (gstack), intent(in) :: s

!! [body

     n = s%nsize

!! body]

     return
  end function gstack_getsize

!!
!! @fun istack_isfull
!!
!! check whether the stack is full of items.
!!
  logical &
  function istack_isfull(s) result (b)
     implicit none

!! external arguments
     ! integer type stack
     type (istack), intent(in) :: s

!! [body

     b = ( s%top == s%nsize )

!! body]

     return
  end function istack_isfull

!!
!! @fun gstack_isfull
!!
!! check whether the stack is full of items.
!!
  logical &
  function gstack_isfull(s) result (b)
     implicit none

!! external arguments
     ! generic type stack
     type (gstack), intent(in) :: s

!! [body

     b = ( s%top == s%nsize )

!! body]

     return
  end function gstack_isfull

!!
!! @fun istack_isempty
!!
!! check whether the stack is empty.
!!
  logical &
  function istack_isempty(s) result (b)
     implicit none

!! external arguments
     ! integer type stack
     type (istack), intent(in) :: s

!! [body

     b = ( s%top == 0 )

!! body]

     return
  end function istack_isempty

!!
!! @fun gstack_isempty
!!
!! check whether the stack is empty.
!!
  logical &
  function gstack_isempty(s) result (b)
     implicit none

!! external arguments
     ! generic type stack
     type (gstack), intent(in) :: s

!! [body

     b = ( s%top == 0 )

!! body]

     return
  end function gstack_isempty

  end module stack
