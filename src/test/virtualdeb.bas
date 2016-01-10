REM > virtualdeb.bas
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

file$ = "<Wimp$ScrapDir>.Debug"
DIM r%(15)
last$ = ""

OSCLI("VirtualDebugStop")
OSCLI("VirtualDebug { > " + file$ + " }")
file% = OPENIN(file$)
WHILE NOT(EOF#file%)
    line$ = GET$#file%
    IF LEFT$(line$, 12) = "Debug record" THEN
        dummy$ = GET$#file%
        type$ = MID$(line$, 15)
        type$ = LEFT$(type$, INSTR(type$, "'") - 1)
        FOR reg% = 0 TO 15
            IF (reg% MOD 4) = 0 THEN
                line$ = GET$#file%
                dummy$ = GET$#file%
            ELSE
                line$ = MID$(line$, 16)
            ENDIF
            r%(reg%) = EVAL("&" + MID$(line$, 7))
        NEXT
        CASE type$ OF
            WHEN "S_V"
                PRINT "Start #"; r%(0)
            WHEN "E_V"
                PRINT "End #"; r%(0)
            WHEN "C_V"
                PRINT "Change #"; r%(0); " by "; r%(1)
            WHEN "L_V"
                PRINT "Lock @"; r%(0); " "; r%(1); "-"; r%(2)
            WHEN "U_P"
                PRINT "Unlock @"; r%(0); " "; r%(1); "-"; r%(2)
            WHEN "A_I"
                PRINT "Abort &"; ~r%(1); " "; last$
            WHEN "A_D"
                last$ = "(" + STR$r%(0) + ") &" + STR$~r%(1) + "-&" +STR$~r%(2)
            WHEN "DA_R"
                PRINT "Remove #"; r%(1)
            WHEN "DA_C"
                PRINT "Create"
            OTHERWISE
                PRINT type$; " !!!"
        ENDCASE
    ENDIF
ENDWHILE
CLOSE#file%
OSCLI("VirtualDebugStart")
