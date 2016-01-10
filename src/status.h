/*
    File        : status.c++
    Date        : 23-Aug-99
    Author      : © A.Thoukydides, 1995, 1996, 1997, 1998, 1999, 2016
    Description : The status of all the pages in a particular dynamic area
                  within a window.

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

#ifndef status_h
#define status_h

// Include oslib header files
#include "os.h"
#include "toolbox.h"
#include "wimp.h"

// Include project header files
#include "virtual.h"

// Component IDs for the various fields
#define STATUS_BUTTON_UPDATE 0x0
#define STATUS_FIELD_NUMBER 0x1
#define STATUS_FIELD_NAME 0x2

// A dynamic area page status window class
class Status
{
public:

    // The public part of the class

    /*
        Parameters  : area  - The number of the dynamic area to open window for.
        Returns     : -
        Description : Constructor function.
    */
    Status(os_dynamic_area_no area);

    /*
        Parameters  : -
        Returns     : -
        Description : Destructor function.
    */
    ~Status(void);

    /*
        Parameters  : area  - The number of the dynamic area to open window
                              for.
        Returns     : void
        Description : Opens a window for the specified dynamic area. If one
                      already exists then it is reopened.
    */
    static void open(os_dynamic_area_no area);

private:

    // The private part of the class

    toolbox_o win_id;                   // The object ID of the window
    toolbox_o tool_id;                  // The object ID of the toolbox
    os_dynamic_area_no num;             // Number of the dynamic area
    int pages;                          // Number of pages
    byte *status;                       // Status of each page
    Status *next;                       // Pointer to the next object
    static Status *head;                // Head of linked list of objects
    int page_size;                      // Size of memory pages in bytes
    int per_mb;                         // Pages per megabyte
    int mb_step;                        // Vertical step per megabyte
    int scale;                          // Scale for this window
    static int def_scale;               // The default scale
    static const wimp_colour colours[virtualise_PAGE_NUM];// The colour table


    /*
        Parameters  : event_code    - The event number.
                      action        - The toolbox event.
                      id_block      - The toolbox ID block.
                      handle        - An unused handle.
        Returns     : int           - Was the event claimed.
        Description : Handle menu about to be shown toolbox events.
    */
    static bool handler_tb_menu_shown(bits event_code, toolbox_action *action,
                                      toolbox_block *id_block, void *handle);

    /*
        Parameters  : event_code    - The event number.
                      action        - The toolbox event.
                      id_block      - The toolbox ID block.
                      handle        - An unused handle.
        Returns     : int           - Was the event claimed.
        Description : Handle menu selection toolbox events.
    */
    static bool handler_tb_menu_selection(bits event_code,
                                          toolbox_action *action,
                                          toolbox_block *id_block,
                                          void *handle);
    /*
        Parameters  : event_code    - The event number.
                      action        - The toolbox event.
                      id_block      - The toolbox ID block.
                      handle        - An unused handle.
        Returns     : int           - Was the event claimed.
        Description : Handle window hidden toolbox events.
    */
    static bool handler_tb_window_hidden(bits event_code,
                                         toolbox_action *action,
                                         toolbox_block *id_block,
                                         void *handle);

    /*
        Parameters  : event_code    - The event number.
                      action        - The toolbox event.
                      id_block      - The toolbox ID block.
                      handle        - An unused handle.
        Returns     : int           - Was the event claimed.
        Description : Handle action button clicked toolbox events.
    */
    static bool handler_tb_action_button(bits event_code,
                                         toolbox_action *action,
                                         toolbox_block *id_block,
                                         void *handle);

    /*
        Parameters  : void
        Returns     : void
        Description : Update the details for the page status window.
    */
    void update(void);

    /*
        Parameters  : event_code    - The event number.
                      wimp_block    - The wimp poll block.
                      id_block      - The toolbox ID block.
                      handle        - An unused handle.
        Returns     : int           - Was the event claimed.
        Description : Handle wimp redraw requests.
    */
    static bool handler_wimp_redraw(wimp_event_no event_code,
                                    wimp_block *block,
                                    toolbox_block *id_block, void *handle);

    /*
        Parameters  : block - Pointer to window redraw block.
        Returns     : void
        Description : Redraw the single rectangle within the redraw request
                      block.
    */
    void redraw(wimp_draw *block);
};

#endif
