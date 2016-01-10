REM > patchzap.bas
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

SYS "OS_DynamicArea", 3, -1 TO , number%
WHILE number% <> -1
    SYS "OS_DynamicArea", 2, number% TO ,,,, flags%,,,, name$
    IF (name$ = "Zap files") OR (name$ = "Zap bitmaps") THEN
        PRINT "Found '"; name$; "' ("; number%; ")";
        IF flags% AND (1 << 31) THEN
            SYS "Virtualise_End", number%
            PRINT " - Virtual memory disabled"
        ELSE
            SYS "Virtualise_Start", number%, -1, 0
            PRINT " - Virtual memory enabled"
        ENDIF
    ENDIF
    SYS "OS_DynamicArea", 3, number% TO , number%
ENDWHILE
END
