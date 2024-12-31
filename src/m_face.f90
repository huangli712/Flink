  module face
     implicit none

     ! parameters
     character(26), private, parameter :: UPPER_ALPHABET='ABCDEFGHIJKLMNOPQRSTUVWXYZ' !< Upper case alphabet.
     character(26), private, parameter :: LOWER_ALPHABET='abcdefghijklmnopqrstuvwxyz' !< Lower case alphabet.
     character(1),  private, parameter :: NL=new_line('a')                            !< New line character.
     character(1),  private, parameter :: ESCAPE=achar(27)                            !< "\" character.

     ! codes
     character(2), private, parameter :: CODE_START=ESCAPE//'['               !< Start ansi code, "\[".
     character(1), private, parameter :: CODE_END='m'                         !< End ansi code, "m".
     character(4), private, parameter :: CODE_CLEAR=CODE_START//'0'//CODE_END !< Clear all styles, "\[0m".

     ! styles codes
     character(17), private, parameter :: STYLES(1:2,1:16)=reshape([&
         'BOLD_ON          ', '1  ', & ! Bold on.
         'ITALICS_ON       ', '3  ', & ! Italics on.
         'UNDERLINE_ON     ', '4  ', & ! Underline on.
         'INVERSE_ON       ', '7  ', & ! Inverse on: reverse foreground and background colors.
         'STRIKETHROUGH_ON ', '9  ', & ! Strikethrough on.
         'BOLD_OFF         ', '22 ', & ! Bold off.
         'ITALICS_OFF      ', '23 ', & ! Italics off.
         'UNDERLINE_OFF    ', '24 ', & ! Underline off.
         'INVERSE_OFF      ', '27 ', & ! Inverse off: reverse foreground and background colors.
         'STRIKETHROUGH_OFF', '29 ', & ! Strikethrough off.
         'FRAMED_ON        ', '51 ', & ! Framed on.
         'ENCIRCLED_ON     ', '52 ', & ! Encircled on.
         'OVERLINED_ON     ', '53 ', & ! Overlined on.
         'FRAMED_OFF       ', '54 ', & ! Framed off.
         'ENCIRCLED_OFF    ', '54 ', & ! Encircled off.
         'OVERLINED_OFF    ', '55 '  & ! Overlined off.
         ], [2,16]) !< Styles.

     ! colors codes
     character(15), private, parameter :: COLORS_FG(1:2,1:17)=reshape([&
         'BLACK          '  , '30 ', & ! Black.
         'RED            '  , '31 ', & ! Red.
         'GREEN          '  , '32 ', & ! Green.
         'YELLOW         '  , '33 ', & ! Yellow.
         'BLUE           '  , '34 ', & ! Blue.
         'MAGENTA        '  , '35 ', & ! Magenta.
         'CYAN           '  , '36 ', & ! Cyan.
         'WHITE          '  , '37 ', & ! White.
         'DEFAULT        '  , '39 ', & ! Default (white).
         'BLACK_INTENSE  '  , '90 ', & ! Black intense.
         'RED_INTENSE    '  , '91 ', & ! Red intense.
         'GREEN_INTENSE  '  , '92 ', & ! Green intense.
         'YELLOW_INTENSE '  , '93 ', & ! Yellow intense.
         'BLUE_INTENSE   '  , '94 ', & ! Blue intense.
         'MAGENTA_INTENSE'  , '95 ', & ! Magenta intense.
         'CYAN_INTENSE   '  , '96 ', & ! Cyan intense.
         'WHITE_INTENSE  '  , '97 '  & ! White intense.
         ], [2,17]) !< Foreground colors.

     character(15), private, parameter :: COLORS_BG(1:2,1:17)=reshape([&
         'BLACK          '  , '40 ', & ! Black.
         'RED            '  , '41 ', & ! Red.
         'GREEN          '  , '42 ', & ! Green.
         'YELLOW         '  , '43 ', & ! Yellow.
         'BLUE           '  , '44 ', & ! Blue.
         'MAGENTA        '  , '45 ', & ! Magenta.
         'CYAN           '  , '46 ', & ! Cyan.
         'WHITE          '  , '47 ', & ! White.
         'DEFAULT        '  , '49 ', & ! Default (black).
         'BLACK_INTENSE  '  , '100', & ! Black intense.
         'RED_INTENSE    '  , '101', & ! Red intense.
         'GREEN_INTENSE  '  , '102', & ! Green intense.
         'YELLOW_INTENSE '  , '103', & ! Yellow intense.
         'BLUE_INTENSE   '  , '104', & ! Blue intense.
         'MAGENTA_INTENSE'  , '105', & ! Magenta intense.
         'CYAN_INTENSE   '  , '106', & ! Cyan intense.
         'WHITE_INTENSE  '  , '107'  & ! White intense.
         ], [2,17]) !< Background colors.

   pure function colorize_default(string, color_fg, color_bg, style) result(colorized)
   !< Colorize and stylize strings, DEFAULT kind.
   character(len=*), intent(in)           :: string    !< Input string.
   character(len=*), intent(in), optional :: color_fg  !< Foreground color definition.
   character(len=*), intent(in), optional :: color_bg  !< Background color definition.
   character(len=*), intent(in), optional :: style     !< Style definition.
   character(len=:), allocatable          :: colorized !< Colorized string.
   integer(int32)                         :: i         !< Counter.

   colorized = string
   if (present(color_fg)) then
      i = color_index(upper(color_fg))
      if (i>0) colorized = CODE_START//trim(COLORS_FG(2, i))//CODE_END//colorized//CODE_CLEAR
   endif
   if (present(color_bg)) then
      i = color_index(upper(color_bg))
      if (i>0) colorized = CODE_START//trim(COLORS_BG(2, i))//CODE_END//colorized//CODE_CLEAR
   endif
   if (present(style)) then
      i = style_index(upper(style))
      if (i>0) colorized = CODE_START//trim(STYLES(2, i))//CODE_END//colorized//CODE_CLEAR
   endif
   endfunction colorize_default

  end module face
