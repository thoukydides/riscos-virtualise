/*
    File        : configure.h
    Date        : 23-Aug-99
    Author      : © A.Thoukydides, 1995, 1996, 1997, 1998, 1999, 2016
    Description : Configuration window for Virtualise module.

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

#ifndef configure_h
#define configure_h

// Include oslib header files
#include "toolbox.h"
extern "C" {
#include "event.h"
}

// Component IDs for the various icons
#define CONFIGURE_ICON_SET 0
#define CONFIGURE_ICON_READ 1
#define CONFIGURE_ICON_CLAIM 2
#define CONFIGURE_ICON_LEAVE 3
#define CONFIGURE_ICON_LIMIT 4
#define CONFIGURE_ICON_POLICY 5
#define CONFIGURE_ICON_MULTI_READS 6
#define CONFIGURE_ICON_MULTI_WRITES 7
#define CONFIGURE_ICON_SWAP_WRITE 8
#define CONFIGURE_ICON_SWAP_DRAG 9
#define CONFIGURE_ICON_SAVE 10
#define CONFIGURE_ICON_LOAD_BOOT 11
#define CONFIGURE_ICON_LOAD_FRONTEND 12
#define CONFIGURE_ICON_KEEP 13
#define CONFIGURE_ICON_SELECT 14
#define CONFIGURE_ICON_ADJUST 15
#define CONFIGURE_ICON_DISC 16
#define CONFIGURE_ICON_SWAP_PURGE 17

// Saved configuration file directory
#define CONFIGURATION_DIRECTORY "<Virtualise$ConfigDir>"

// A dynamic area list window class
class Configure
{
public:

    // The public part of the class

    /*
        Parameters  : win   - The object ID of the configuration window.
        Returns     : -
        Description : Constructor function.
    */
    Configure(toolbox_o win);

    /*
        Parameters  : -
        Returns     : -
        Description : Destructor function.
    */
    ~Configure(void);

private:

    // The private part of the class

    toolbox_o myid;                     // The object ID of the configure window
    toolbox_o paneid;                   // The object ID of the pane window
    int select_op;                      // Operation performed by select
    int adjust_op;                      // Operation performed by adjust
    int purge_state;                    // Is purging enabled

    /*
        Parameters  : event_code    - The event number.
                      action        - The toolbox event.
                      id_block      - The toolbox ID block.
                      handle        - An unused handle.
        Returns     : int           - Was the event claimed.
        Description : Handle menu about to be shown toolbox events.
    */
    static bool handler_tb_action(bits event_code, toolbox_action *action,
                                  toolbox_block *id_block, void *handle);

    /*
        Parameters  : event_code    - The event number.
                      action        - The toolbox event.
                      id_block      - The toolbox ID block.
                      handle        - An unused handle.
        Returns     : int           - Was the event claimed.
        Description : Handle draggable object drag ended toolbox events.
    */
    static bool handler_tb_drag(bits event_code, toolbox_action *action,
                                toolbox_block *id_block, void *handle);

    /*
        Parameters  : message   - The wimp message.
                      handle    - An unused handle.
        Returns     : int       - Was the event claimed.
        Description : Handle SaveDesktop wimp message events.
    */
    static bool handler_message_desk(wimp_message *message, void *handle);

    /*
        Parameters  : message   - The wimp message.
                      handle    - An unused handle.
        Returns     : int       - Was the event claimed.
        Description : Handle DataSaveAck wimp message events.
    */
    static bool handler_message_save(wimp_message *message, void *handle);

    /*
        Parameters  : message   - The wimp message.
                      handle    - An unused handle.
        Returns     : int       - Was the event claimed.
        Description : Handle DataLoad wimp message events.
    */
    static bool handler_message_load(wimp_message *message, void *handle);

    /*
        Parameters  : void
        Returns     : void
        Description : Restore the values from the configuration file if
                      possible.
    */
    void restore(void);

    /*
        Parameters  : void
        Returns     : void
        Description : Save the values currently specified.
    */
    void save(void);

    /*
        Parameters  : void
        Returns     : void
        Description : Set the values currently specified.
    */
    void set(void);

    /*
        Parameters  : void
        Returns     : void
        Description : Read the current settings, and update the number range
                      fields appropriately.
    */
    void read(void);

};

#endif
