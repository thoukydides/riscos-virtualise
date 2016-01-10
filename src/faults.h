/*
    File        : faults.h
    Date        : 23-Aug-99
    Author      : © A.Thoukydides, 1995, 1996, 1997, 1998, 1999, 2016
    Description : A list of dynamic areas within a window.

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

#ifndef faults_h
#define faults_h

// Include oslib header files
#include "toolbox.h"

// Component IDs for the various fields
#define FAULTS_FIELD_LDR 0x0
#define FAULTS_FIELD_STR 0x1
#define FAULTS_FIELD_LDM 0x2
#define FAULTS_FIELD_STM 0x3
#define FAULTS_FIELD_LDC 0x4
#define FAULTS_FIELD_STC 0x5
#define FAULTS_FIELD_SWP 0x6

// A page faults window class
class PageFaults
{
public:

    // The public part of the class

    /*
        Parameters  : win   - The object ID of the window in which the list
                              should appear.
        Returns     : -
        Description : Constructor function.
    */
    PageFaults(toolbox_o win);

    /*
        Parameters  : -
        Returns     : -
        Description : Destructor function.
    */
    ~PageFaults(void);

private:

    // The private part of the class

    toolbox_o myid;                     // The object ID of the list window

    /*
        Parameters  : event_code    - The event number.
                      wimp_block    - The wimp poll block.
                      id_block      - The toolbox ID block.
                      handle        - An unused handle.
        Returns     : int           - Was the event claimed.
        Description : Handle wimp null polls.
    */
    static bool handler_wimp_null(wimp_event_no event_code, wimp_block *block,
                                  toolbox_block *id_block, void *handle);

    /*
        Parameters  : cmp   - The component ID of the icon to update.
                      type  - The class of instruction to place in the icon.
        Returns     : void
        Description : Update one of the page fault icons.
    */
    void update(toolbox_c cmp, int type);
};

#endif
