/*
    File        : automatic.h
    Date        : 23-Aug-99
    Author      : © A.Thoukydides, 1995, 1996, 1997, 1998, 1999, 2016
    Description : A list of dynamic areas within a window for which virtual
                  memory is enabled by default.

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

#ifndef automatic_h
#define automatic_h

// Include oslib header files
#include "toolbox.h"
extern "C" {
#include "event.h"
}

// Component IDs for the various fields
#define AUTO_FIELD_NAME 0
#define AUTO_FIELD_LIMIT 1
#define AUTO_NUM_FIELDS 2

// Component IDs for menu entries
#define AUTO_MENU_SELECT 0x100
#define AUTO_MENU_CLEAR 0x101
#define AUTO_MENU_LIMIT 0x300
#define AUTO_MENU_ADD 0x301
#define AUTO_MENU_REMOVE 0x302
#define AUTO_MENU_SAVE 0x303

// Component IDs for dialogue box
#define AUTO_LIMIT_SET 0
#define AUTO_LIMIT_CANCEL 1
#define AUTO_LIMIT_SIZE 2

// A dynamic area list window class
class AutomaticList
{
public:

    // The public part of the class

    /*
        Parameters  : win   - The object ID of the window in which the list
                              should appear.
        Returns     : -
        Description : Constructor function.
    */
    AutomaticList(toolbox_o win);

    /*
        Parameters  : -
        Returns     : -
        Description : Destructor function.
    */
    ~AutomaticList(void);

    /*
        Parameters  : void
        Returns     : void
        Description : Update the dynamic area list.
    */
    void update(void);

private:

    // The private part of the class

    struct AreaRecord
    {
        char *name;                     // The dynamic area name
        char *var;                      // The equivalent variable name
        bool updated;                   // Has this record been altered
        bool selected;                  // Is this area selected
        bool menu_selected;             // Items selected when menu opened
        bool menu_select;               // Is it selected for duration of menu
        int limit;                      // The size limit for this area
        toolbox_c id[AUTO_NUM_FIELDS];  // The gadget IDs for the various fields
        toolbox_o myid;                 // The object ID of the list window
        AreaRecord *next;               // The next dynamic area in the list

        // Constructor function
        AreaRecord(toolbox_o win, const char *title);

        // Destructor function
        ~AreaRecord(void);

        // Set vertical position of the gadgets
        int pos(int start);

        // Update the numeric value field
        void update(int value);

        // Set the selection status
        void select(bool sel);

        // Check if an area variable name matches
        int compare(const char *title);
    };

    bool menu_select;                   // Is selection for duration of menu
    toolbox_o myid;                     // The object ID of the list window
    toolbox_o limitid;                  // The object ID of the limit dialogue
    toolbox_o dataid;                   // The object ID of the database menu
    AreaRecord *head;                   // Head of list of dynamic areas

    /*
        Parameters  : event_code    - The event number.
                      action        - The toolbox event.
                      id_block      - The toolbox ID block.
                      handle        - An unused handle.
        Returns     : int           - Was the event claimed.
        Description : Handle action button toolbox events.
    */
    static bool handler_tb_action(bits event_code, toolbox_action *action,
                                  toolbox_block *id_block, void *handle);

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
        Parameters  : event_code    - The event number.
                      wimp_block    - The wimp poll block.
                      id_block      - The toolbox ID block.
                      handle        - An unused handle.
        Returns     : int           - Was the event claimed.
        Description : Handle mouse clicks in the dynamic area list.
    */
    static bool handler_mouse_click(wimp_event_no event_code, wimp_block *block,
                                    toolbox_block *id_block, void *handle);

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
        Description : Handle menu completed toolbox events.
    */
    static bool handler_tb_menu_completed(bits event_code,
                                          toolbox_action *action,
                                          toolbox_block *id_block,
                                          void *handle);

    /*
        Parameters  : id    - The menu object ID.
        Returns     : void
        Description : Copy the selection status of areas and sets the menu
                      flags appropriately.
    */
    void prepare_menu(toolbox_o id);
};

#endif
