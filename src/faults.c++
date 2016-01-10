/*
    File        : faults.c++
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

// Include the header file for this module
#include "faults.h"

// Include clib header files
#include <stdio.h>
#include <string.h>

// Include oslib header files
#include "displayfield.h"
extern "C" {
#include "event.h"
}

// Include project header files
#include "virtual.h"

/*
    Parameters  : win   - The object ID of the window in which the list
                          should appear.
    Returns     : -
    Description : Constructor function.
*/
PageFaults::PageFaults(toolbox_o win)
{
    // Copy the window ID
    myid = win;


    // Install handlers
    event_register_wimp_handler(event_ANY, wimp_NULL_REASON_CODE,
                                handler_wimp_null, this);

    // Ensure that null polls get through
    wimp_poll_flags mask;
    event_get_mask(&mask);
    mask &= ~wimp_MASK_NULL;
    event_set_mask(mask);
}

/*
    Parameters  : -
    Returns     : -
    Description : Destructor function.
*/
PageFaults::~PageFaults(void)
{
    // Deinstall handlers
    event_deregister_wimp_handler(event_ANY, wimp_NULL_REASON_CODE,
                                  handler_wimp_null, this);
}

/*
    Parameters  : event_code    - The event number.
                  wimp_block    - The wimp poll block.
                  id_block      - The toolbox ID block.
                  handle        - An unused handle.
    Returns     : int           - Was the event claimed.
    Description : Handle wimp null polls.
*/
bool PageFaults::handler_wimp_null(wimp_event_no event_code, wimp_block *block,
                                   toolbox_block *id_block, void *handle)
{
    PageFaults *this_ptr = (PageFaults *) handle;

    NOT_USED(event_code);
    NOT_USED(block);
    NOT_USED(id_block);

    // Update all the icons
    this_ptr->update(FAULTS_FIELD_LDR, virtualise_FAULT_LDR);
    this_ptr->update(FAULTS_FIELD_STR, virtualise_FAULT_STR);
    this_ptr->update(FAULTS_FIELD_LDM, virtualise_FAULT_LDM);
    this_ptr->update(FAULTS_FIELD_STM, virtualise_FAULT_STM);
    this_ptr->update(FAULTS_FIELD_LDC, virtualise_FAULT_LDC);
    this_ptr->update(FAULTS_FIELD_STC, virtualise_FAULT_STC);
    this_ptr->update(FAULTS_FIELD_SWP, virtualise_FAULT_SWP);

    // Do not claim the event
    return FALSE;
}

/*
    Parameters  : cmp   - The component ID of the icon to update.
                  type  - The class of instruction to place in the icon.
    Returns     : void
    Description : Update one of the page fault icons.
*/
void PageFaults::update(toolbox_c cmp, int type)
{
    os_error *er;
    char str1[12], str2[12];
    int count;

    er = xvirtualise_miscop_faults(type, &count);
    if (er) str1[0] = 0;
    else sprintf(str1, "%i", count);

    displayfield_get_value(0, myid, cmp, str2, 12);
    if (strcmp(str1, str2)) displayfield_set_value(0, myid, cmp, str1);
}
