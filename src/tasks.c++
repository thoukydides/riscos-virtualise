/*
    File        : tasks.c++
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

// Include header file for this module
#include "tasks.h"

// Include clib header files
#include <stdio.h>
#include <string.h>

// Include oslib header files
#include "button.h"
#include "displayfield.h"
#include "gadget.h"
#include "os.h"
#include "taskmanager.h"
#include "virtual.h"
#include "window.h"
#include "wimp.h"
#include "wimpspriteop.h"

// Include project header files
#include "demo.h"
#include "main.h"

// Demo task names
#define DEMO_TASK1 "Compo"
#define DEMO_TASK2 "ProArt24DEMO"

// Head of list of tasks
TaskList::TaskRecord *TaskList::TaskRecord::head = NULL;

/*
    Parameters  : win   - The object ID of the window in which the list
                          should appear.
    Returns     : -
    Description : Constructor function.
*/
TaskList::TaskList(toolbox_o win)
{
    int context;
    taskmanager_task details;
    char *end;

    // Copy the window ID
    myid = win;

    // Install handlers
    event_register_wimp_handler(event_ANY, wimp_NULL_REASON_CODE,
                                handler_wimp_null, this);
    event_register_wimp_handler(myid, wimp_MOUSE_CLICK,
                                handler_mouse_click, this);
    event_register_message_handler(message_TASK_INITIALISE,
                                   handler_message_init, this);
    event_register_message_handler(message_TASK_CLOSE_DOWN,
                                   handler_message_close, this);
    event_register_message_handler(message_SLOT_SIZE,
                                   handler_message_size, this);

    // Ensure that null polls get through
    wimp_poll_flags mask;
    event_get_mask(&mask);
    mask &= ~wimp_MASK_NULL;
    event_set_mask(mask);

    // Construct initial table
    context = taskmanager_enumerate_tasks(0, &details, sizeof(details), &end);
    while ((char *) &details < end)
    {
        // Add line to list
        TaskRecord *task = new TaskRecord(myid, details.task, details.name);

        // Set the task size
        task->update_size(details.slot_size);

        // Find details of the next task
        context = taskmanager_enumerate_tasks(context, &details,
                                              sizeof(details), &end);
    }
}

/*
    Parameters  : -
    Returns     : -
    Description : Destructor function.
*/
TaskList::~TaskList(void)
{
    // Deinstall handlers
    event_deregister_message_handler(message_SLOT_SIZE,
                                     handler_message_size, this);
    event_deregister_message_handler(message_TASK_CLOSE_DOWN,
                                     handler_message_close, this);
    event_deregister_message_handler(message_TASK_INITIALISE,
                                     handler_message_init, this);
    event_deregister_wimp_handler(myid, wimp_MOUSE_CLICK,
                                  handler_mouse_click, this);
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
bool TaskList::handler_wimp_null(wimp_event_no event_code,
                                 wimp_block *block,
                                 toolbox_block *id_block, void *handle)
{
    TaskList *this_pointer = (TaskList *) handle;

    NOT_USED(event_code);
    NOT_USED(block);
    NOT_USED(id_block);

    // Pass on to the actual routine
    this_pointer->update();

    // Do not claim the event
    return FALSE;
}

/*
    Parameters  : event_code    - The event number.
                  wimp_block    - The wimp poll block.
                  id_block      - The toolbox ID block.
                  handle        - An unused handle.
    Returns     : int           - Was the event claimed.
    Description : Handle mouse clicks in the dynamic area list.
*/
bool TaskList::handler_mouse_click(wimp_event_no event_code,
                                   wimp_block *block,
                                   toolbox_block *id_block,
                                   void *handle)
{
    TaskList *list = (TaskList *) handle;
    os_error *er = NULL;

    NOT_USED(event_code);

    // Find which task was clicked on
    int i = 0;
    TaskRecord *task = TaskList::TaskRecord::head;
    while (task && ((task->myid != id_block->this_obj)
                    || (task->id[i] != id_block->this_cmp)))
    {
        if (TASKS_NUM_FIELDS <= ++i)
        {
            i = 0;
            task = task->next;
        }
    }
    if (!task) return FALSE;

#ifdef DEMO

    // Check if it is a demo program
    if ((strcmp(task->name, DEMO_TASK1) != 0)
        && (strcmp(task->name, DEMO_TASK2) != 0))
    {
        // Show the demo information window
        demo_window->show();
        return FALSE;
    }

#endif

    // Toggle the state of the task
    if (task->status) er = xvirtualise_miscop_thaw(task->handle);
    else er = xvirtualise_miscop_freeze(task->handle);
    if (er) wimp_report_error_by_category(er, wimp_ERROR_BOX_CATEGORY_INFO << 9,
                                          task_name, AppSprite,
                                          wimpspriteop_AREA, 0);

    // Check if any special action should be taken
    if (block->pointer.buttons & wimp_CLICK_ADJUST)
    {
        // Close the window
        toolbox_hide_object(0, list->myid);
    }

    // Claim the event
    return TRUE;
}

/*
    Parameters  : message   - The wimp message.
                  handle    - An unused handle.
    Returns     : int       - Was the event claimed.
    Description : Handle TaskInitialise wimp message events.
*/
bool TaskList::handler_message_init(wimp_message *message, void *handle)
{
    TaskList *this_ptr = (TaskList *) handle;

    // Add a new line to the display
    TaskRecord *task = new TaskRecord(this_ptr->myid, message->sender,
                                      ((wimp_message_task_initialise *)
                                      &message->data)->task_name);

    // Set the task size
    task->update_size(((wimp_message_task_initialise *) &message->data)->size);

    // Do not claim the event
    return FALSE;
}

/*
    Parameters  : message   - The wimp message.
                  handle    - An unused handle.
    Returns     : int       - Was the event claimed.
    Description : Handle TaskCloseDown wimp message events.
*/
bool TaskList::handler_message_close(wimp_message *message, void *handle)
{
    TaskList *this_ptr = (TaskList *) handle;

    // Find the relevant record
    TaskRecord *task = TaskList::TaskRecord::head;
    while (task && (task->handle != message->sender)) task = task->next;
    if (!task) return FALSE;

    // Remove a line from the display
    delete task;

    // Do not claim the event
    return FALSE;
}

/*
    Parameters  : message   - The wimp message.
                  handle    - An unused handle.
    Returns     : int       - Was the event claimed.
    Description : Handle SlotSize wimp message events.
*/
bool TaskList::handler_message_size(wimp_message *message, void *handle)
{
    TaskList *this_ptr = (TaskList *) handle;

    // Find the relevant record
    TaskRecord *task = TaskList::TaskRecord::head;
    while (task && (task->handle != message->sender)) task = task->next;
    if (!task) return FALSE;

    // Change a line of the display
    task->update_size(((wimp_message_slot_size *) &message->data)->new_curr);

    // Update the other window details
    this_ptr->update();

    // Do not claim the event
    return FALSE;
}

/*
    Parameters  : win       - The object ID of the list window.
                  task      - The task handle.
                  title     - The name of the task.
    Returns     : -
    Description : Constructor function.
*/
TaskList::TaskRecord::TaskRecord(toolbox_o win, wimp_t task,
                                 const char *title)
{
    int i, s, grey;
    toolbox_id desc;
    gadget_OBJECT(sizeof(button_gadget)) gnew;
    gadget_object *gorig;
    TaskRecord **prev;

    // Copy the area details
    myid = win;
    handle = task;
    i = 0;
    while ((31 < title[i]) || (title[i] < 0)) i++;
    name = new char[i + 1];
    strncpy(name, title, i);
    name[i] = 0;
    size = -1;
    status = -1;

    // Check if it is a special case
    grey = !strcmp(name, "Task Manager")
           || !strcmp(name, "Filer")
           || !strcmp(name, "Free")
           || !strcmp(name, "Virtualise");

    // Create the gadgets for the various fields
    desc = toolbox_template_look_up(0, "TaskGadget");
    for (i = 0; i < TASKS_NUM_FIELDS; i++)
    {
        gorig = window_extract_gadget_info(0, (toolbox_resource_file_object *) desc, i, &s);
        memcpy(&gnew, gorig, s);
        gnew.cmp = -1;
        id[i] = window_add_gadget(0, myid, (gadget_object *) &gnew);

        if (grey) button_set_flags(0, myid, id[i], 0, wimp_ICON_SHADED);
    }

    // Set the value for the static field
    button_set_value(0, myid, id[TASKS_FIELD_NAME], name);

    // Find where this record should be inserted
    prev = &head;
    while (*prev && (strcmp((*prev)->name, title) < 0))
    {
        prev = &((*prev)->next);
    }

    // Link this record in
    next = *prev;
    *prev = this;

    // Update the window size and details
    update();
    update_size(0);
    update_status(FALSE);
}

/*
    Parameters  : -
    Returns     : -
    Description : Constructor function.
*/
TaskList::TaskRecord::~TaskRecord(void)
{
    int i;
    TaskRecord **prev;

    // Unlink record
    prev = &head;
    while (*prev && (*prev != this)) prev = &((*prev)->next);
    *prev = next;

    // Remove the gadgets from the window
    for (i = 0; i < TASKS_NUM_FIELDS; i++)
    {
        window_remove_gadget(0, myid, id[i]);
    }

    // Free any used memory
    delete[] name;

    // Update the window size
    update();
}

/*
    Parameters  : start - The position to place the top of the gadgets.
    Returns     : int   - The position of the bottom of the gadget.
    Description : Set the vertical position of the gadgets.
*/
int TaskList::TaskRecord::pos(int start)
{
    int i, height;
    os_box bbox;

    for (i = 0; i < TASKS_NUM_FIELDS; i++)
    {
        gadget_get_bbox(0, myid, id[i], &bbox);
        height = bbox.y1 - bbox.y0;
        if (bbox.y1 != start)
        {
            bbox.y1 = start;
            bbox.y0 = start - height;
            gadget_move_gadget(0, myid, id[i], &bbox);
        }
    }
    return start - height;
}

/*
    Parameters  : value - The new value of the field.
    Returns     : void
    Description : Update the numeric value field.
*/
void TaskList::TaskRecord::update_size(int value)
{
    value /= 1024;
    if (value != size)
    {
        char str[9];
        sprintf(str, "%iK", value);
        button_set_value(0, myid, id[TASKS_FIELD_SIZE], str);
        size = value;
    }
}

/*
    Parameters  : value - The new value of the field.
    Returns     : void
    Description : Update the status field.
*/
void TaskList::TaskRecord::update_status(int value)
{
    if (value != status)
    {
        button_set_value(0, myid, id[TASKS_FIELD_SWAPPED],
                         value ? yes : no);
        status = value;
    }
}

/*
    Parameters  : void
    Returns     : void
    Description : Update the size of the window and the positions of the
                  gadgets.
*/
void TaskList::TaskRecord::update(void)
{
    static int last = 0;
    int pos = 0;
    TaskRecord *ptr;

    // Update the positions
    ptr = head;
    while (ptr)
    {
        pos = ptr->pos(pos);
        ptr = ptr->next;
    }

    // Update the window extent
    if (last != pos)
    {
        os_box bbox;
        window_get_extent(0, myid, &bbox);
        bbox.y0 = pos;
        window_set_extent(0, myid, &bbox);
        last = pos;
    }
}

/*
    Parameters  : void
    Returns     : void
    Description : Update the swapped state of the tasks.
*/
void TaskList::update(void)
{
    TaskRecord *task = TaskRecord::head;
    os_error *er;
    int status;

    // Find the relevant record
    while (task)
    {
        // Check status of the task
        er = xvirtualise_miscop_status(task->handle, &status);
        if (er || (status == -1))
        {
            task->update_status(FALSE);
        }
        else
        {
            task->update_status(TRUE);
            task->update_size(status);
        }

        // Update pointer for next task
        task = task->next;
    }
}
