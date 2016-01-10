REM > exvirt.bas
REN
REM © A.Thoukydides, 1995, 1996, 1997, 1998, 1999, 2016
REM
REM Virtualise is free software: you can redistribute it and/or modify it
REM under the terms of the GNU General Public License as published by the Free
REM Software Foundation, either version 3 of the License, or (at your option)
REM any later version.
REM
REM Virtualise is distributed in the hope that it will be useful, but WITHOUT
REM ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
REM FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
REM more details.
REM
REM You should have received a copy of the GNU General Public License along
REM with Virtualise. If not, see <http://www.gnu.org/licenses/>.

REM General configuration
area_size% = 1024 * 1000
area_name$ = "Example Sprite Area"
sprite_file$ = "TestSprite"
sprite_name$ = "test_sprite"
sprite_width% = 1000
sprite_height% = 1000
sprite_mode% = 28

REM Should virtual memory be used
use_virtual% = FNyn("Use virtual memory")

REM Create a dynamic area
SYS "OS_DynamicArea", 0, -1, area_size%, -1, &80, -1, 0, 0, area_name$ TO , area_number%,, area_base%

REM Initialise a user sprite area within the area
!area_base% = area_size%
area_base%!8 = 16
SYS "OS_SpriteOp", 256 + 9, area_base%

REM Create a sprite within the area
SYS "OS_SpriteOp", 256 + 15, area_base%, sprite_name$, 1, sprite_width%, sprite_height%, sprite_mode%

REM Convert the area to virtual memory
IF use_virtual% THEN
    PRINT "Starting virtual memory...";
    SYS "Virtualise_Start", area_number%, -1, 0
    PRINT " done"
ENDIF

REM Enter a loop to allow the timing test to be repeated
REPEAT

    REM Time how long it takes to draw a pattern
    PRINT "Starting plot to sprite...";
    time_start% = TIME
    FOR loop% = 0 TO 100
        SYS "OS_SpriteOp", 256 + 42, area_base%, sprite_name$, RND(sprite_width%) - 1, RND(sprite_height%) - 1, RND(64) - 1, 0
    NEXT
    time_end% = TIME
    PRINT " done"
    PRINT "Plot to sprite took "; (time_end% - time_start%); " centi-seconds"
    
UNTIL NOT(FNyn("Repeat time plot"))

REM Convert the area back to normal memory
IF use_virtual% THEN
    PRINT "Disabling virtual memory...";
    SYS "Virtualise_End", area_number%
    PRINT " done"
ENDIF

REM Save the sprite area
PRINT "Saving sprite area...";
SYS "OS_SpriteOp", 256 + 12, area_base%, sprite_file$
PRINT " done"

REM Remove the dynamic area
SYS "OS_DynamicArea", 1, area_number%
END

REM Get a Yes/No response
DEFFNyn(text$)
LOCAL result%
    PRINT text$; "? (Y/N)";
    REPEAT
        key$ = GET$
    UNTIL INSTR("YyNn", key$)
    IF INSTR("Yy", key$) THEN
        PRINT " Yes"
        result% = TRUE
    ELSE
        PRINT " No"
        result% = FALSE
    ENDIF
= result%
