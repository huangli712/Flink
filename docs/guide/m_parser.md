!!
!!
!! Introduction
!! ============
!!
!! The original input file format for iQIST is not very good. We need a
!! flexible, convenient, and powerful input file format, and corresponding
!! file reader and parser. Thus, we redesign the input file format and
!! implement this file parser.
!!
!! Input File Format
!! =================
!!
!! 1. anything after "#" and "!" character can be treated as comments and
!!    will be ignored completely.
!!
!!    example:
!!
!!    # this is a comment line
!!    ! this is a comment line
!!
!!    nband = 4 # this is in line comment
!!    norbs = 8 ! this is in line comment
!!
!! 2. it is not case sensitive.
!!
!!    example:
!!
!!    Nband = 4
!!    NORBS = 8
!!    NspiN = 2
!!
!! 3. the key and value pair is separated by "=" or ":" character.
!!
!!    example:
!!
!!    nband = 4 ! you can use nband : 4
!!    norbs : 8 ! you can use norbs = 8
!!
!! 4. any space will be ignored. any blank lines will be skipped as well.
!!
!!    example:
!!
!!    n b a n d = 4 ! it is valid
!!    no   rb s = 8 ! it is valid
!!
!! 5. you can only use one line to define one key-value pair.
!!
!!    example
!!
!!    nband = 4 norbs = 8  ! it is not valid
!!    nband = 4, norbs = 8 ! it is not valid
!!    nband = 4; norbs = 8 ! it is not valid
!!    nband =              !
!!    4                    ! it is not valid
!!
!! 6. in the value part, now only integer, real(dp), logical, and character
!!    data type are support.
!!
!!    example:
!!
!!    nband = 4        ! integer type
!!    mune  = 4.0      ! real(dp) type
!!    isscf = .true.   ! logical type, you can also use .false., T, F
!!    model = anderson ! character type, do not use "" or '' characters to quote it
!!
!! 7. in the value part, a vector is also support. the items in the vector
!!    should be separated by "," character.
!!
!!    example:
!!
!!    nband = 1, 2, 3, 4                   ! 4 items
!!    mune = 0.0, -1.0, 2.0                ! 3 items
!!    isscf = .true., .true., F, T, .true. ! 5 items
!!    model = anderson, hubbard            ! 2 items
!!
!! 8. an empty input file is acceptable.
!!
!! 9. if one key occurs in the input file for more than 1 times, only the
!!    last occurrence is recognized.
!!
!! Usage
!! =====
!!
!! 1. import parser support
!! ------------------------
!!
!! use parser
!!
!! 2. create instance for parser
!! -----------------------------
!!
!! call p_create()
!!
!! 3. parse the input file
!! -----------------------
!!
!! call p_parse(file_name)
!!
!! note: in the mpi environment, only the master node can execute this
!! command. you should broadcast the data manually.
!!
!! 4. extract parameters
!! ---------------------
!!
!! integer :: nband = 2            ! default value
!! real(dp) :: mune = 10.0_dp      ! default value
!! logical :: symm(2)              ! default value
!! symm(1) = .true.
!! symm(2) = .false.
!! call p_get('nband', nband)      ! get single value
!! call p_get('mune', mune)        ! get single value
!! call p_get_vec('symm', symm, 2) ! get array
!!
!! note: that if the desired parameter is not contained in the config file,
!! then the default value will not be changed.
!!
!! note: in the mpi environment, only the master node can execute these
!! commands.
!!
!! note: the parser DO NOT check the correctness (including number of
!! values, key's name, and datatype of value) of the input file. So, please
!! always monitor the output of ctqmc code which use this parser to parse
!! the input file.
!!
!! 5. destroy parser
!! -----------------
!!
!! call p_destroy()
!!
!! 6. broadcast the parameters read from input file
!! ------------------------------------------------
!!
!! do not forget to broadcast all of the parameters from master node to
!! children nodes. please see the comments in m_mpi.f90
!!
!!
