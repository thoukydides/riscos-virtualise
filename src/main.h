/*
    File        : main.h
    Date        : 23-Aug-99
    Author      : © A.Thoukydides, 1995, 1996, 1997, 1998, 1999, 2016
    Description : The main !Virtualis program.

    License     : Virtualise is free software: you can redistribute it and/or
                  modify it under the terms of the GNU General Public License
                  as published by the Free Software Foundation, either
                  version 3 of the License, or (at your option) any later
                  version.

                  Virtualise is distributed in the hope that it will be useful,
                  but WITHOUT ANY WARRANTY; without even the implied warranty
                  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See
                  the GNU General Public License for more details.

                  You should have received a copy of the GNU General Public
                  License along with Virtualise. If not, see
                  <http://www.gnu.org/licenses/>.
*/

#ifndef main_h
#define main_h

// Include oslib header files
#include "messagetrans.h"
#include "toolbox.h"
#include "wimp.h"

// Include project header files
#include "demo.h"

// The name of a sprite which can be used in error boxes
#define AppSprite "!virtualis"

// The latest wimp version known about
#define WimpVersion wimp_VERSION_RO35

// Global quit flag
extern int quit;

// Toolbox and wimp variables
extern char res_directory[256];         // GSTrans'd resource directory
extern toolbox_block id_block;          // The toolbox event ID block
extern messagetrans_control_block message_block;// Toolbox message block
extern osspriteop_area *sprite_area;    // Sprite area pointer
extern int wimp_version;                // The current wimp version
extern wimp_t task_handle;              // Task handle for this task
extern char *task_name;                 // Name of the task
extern char *yes, *no;                  // Yes and no strings for icons
extern toolbox_o task_icon;             // Object ID for the main iconbar icon
extern toolbox_o task_menu;             // Object ID for the main iconbar menu

/*
    Parameters  : dest  - Pointer to string in which to place result.
                  size  - The length of the destination string.
                  token - The token to lookup.
                  arg0  - The optional first parameter.
                  arg1  - The optional second parameter.
                  arg2  - The optional third parameter.
                  arg3  - The optional fourth parameter.
    Returns     : void
    Description : Lookup the specified token in the message file opened by
                  the toolbox, substituting up to four parameters.
*/
void lookup_token(char *dest, int size, const char *token,
                  const char *arg0 = 0, const char *arg1 = 0,
                  const char *arg2 = 0, const char *arg3 = 0);


/*
    Parameters  : token - The token to lookup.
                  arg0  - The optional first parameter.
                  arg1  - The optional second parameter.
                  arg2  - The optional third parameter.
                  arg3  - The optional fourth parameter.
    Returns     : char  - Pointer to the token (in the RMA).
    Description : Lookup the specified token in the message file opened by
                  the toolbox, substituting up to four parameters.
*/
char *lookup_token(const char *token, const char *arg0 = 0,
                   const char *arg1 = 0, const char *arg2 = 0,
                   const char *arg3 = 0);

#endif
