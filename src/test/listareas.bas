REM > listareas.bas
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
    SYS "OS_DynamicArea", 2, number% TO ,,,,,,,, name$
    PRINT "&";~number%, name$
    SYS "OS_DynamicArea", 3, number% TO , number%
ENDWHILE
END
