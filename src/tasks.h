/*
    File        : tasks.h
    Date        : 23-Aug-99
    Author      : © A.Thoukydides, 1996, 1997, 1998, 1999, 2016
    Description : Control over swapping of tasks to disc.

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

#ifndef tasks_h
#define tasks_h

// Include oslib header files
#include "toolbox.h"
extern "C" {
#include "event.h"
}

// Component IDs for the various fields
#define TASKS_FIELD_NAME 0
#define TASKS_FIELD_SIZE 1
#define TASKS_FIELD_SWAPPED 2
#define TASKS_NUM_FIELDS 3

// A task list window class
class TaskList
{
public:

    // The public part of the class

    /*
        Parameters  : win   - The object ID of the window in which the list
                              should appear.
        Returns     : -
        Description : Constructor function.
    */
    TaskList(toolbox_o win);

    /*
        Parameters  : -
        Returns     : -
        Description : Destructor function.
    */
    ~TaskList(void);

    /*
        Parameters  : void
        Returns     : void
        Description : Update the task list.
    */
    void update(void);

private:

    // The private part of the class

    struct TaskRecord
    {
        wimp_t handle;                  // The task handle
        char *name;                     // The task name
        int size;                       // The size of this task
        int status;                     // The status of this task
        toolbox_c id[TASKS_NUM_FIELDS]; // The gadget IDs for the various fields
        toolbox_o myid;                 // The object ID of the list window
        TaskRecord *next;               // The next task in the list
        static TaskRecord *head;        // Head of list of tasks

        // Constructor function
        TaskRecord(toolbox_o win, wimp_t task, const char *title);

        // Destructor function
        ~TaskRecord(void);

        // Update the positions and window size
        void update(void);

        // Set vertical position of the gadgets
        int pos(int start);

        // Update the numeric value field
        void update_size(int value);

        // Update the status field
        void update_status(int value);
    };

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
        Parameters  : message   - The wimp message.
                      handle    - An unused handle.
        Returns     : int       - Was the event claimed.
        Description : Handle TaskInitialise wimp message events.
    */
    static bool handler_message_init(wimp_message *message, void *handle);

    /*
        Parameters  : message   - The wimp message.
                      handle    - An unused handle.
        Returns     : int       - Was the event claimed.
        Description : Handle TaskCloseDown wimp message events.
    */
    static bool handler_message_close(wimp_message *message, void *handle);

    /*
        Parameters  : message   - The wimp message.
                      handle    - An unused handle.
        Returns     : int       - Was the event claimed.
        Description : Handle SlotSize wimp message events.
    */
    static bool handler_message_size(wimp_message *message, void *handle);
};

#endif
