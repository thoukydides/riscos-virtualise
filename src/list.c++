/*
    File        : list.c++
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
#include "list.h"

// Include clib header files
#include <stdio.h>
#include <string.h>

// Include c++lib header files
#include "fstream.h"

// Include oslib header files
#include "actionbutton.h"
#include "button.h"
#include "displayfield.h"
#include "gadget.h"
#include "menu.h"
#include "numberrange.h"
#include "optionbutton.h"
#include "os.h"
#include "osfile.h"
#include "osfscontrol.h"
#include "radiobutton.h"
#include "window.h"
#include "wimp.h"
#include "wimpspriteop.h"

// Include project header files
#include "database.h"
#include "demo.h"
#include "main.h"
#include "status.h"
#include "virtual.h"

// Text to append for files to create or modify
#define RUN_MAIN ".!Run"
#define RUN_COPY ".PreVM !Run"

// Filenames for module copy
#define MODULE_SRC ".Virtualise"
#define MODULE_DEST "<Choices$Write>.Boot.PreDesk.Virtualise"

// Name suffix for demo
#define DEMO_SUFFIX "DEMO"

/*
    Parameters  : win   - The object ID of the window in which the list
                          should appear.
    Returns     : -
    Description : Constructor function.
*/
DynamicAreaList::DynamicAreaList(toolbox_o win)
{
    // Copy the window ID
    myid = win;

    // No dynamic area records yet
    head = NULL;
    menu_select = FALSE;

    // Create the dialogue windows
    patchid = toolbox_create_object(0, (toolbox_id) "PatchWin");
    deleteid = toolbox_create_object(0, (toolbox_id) "DelWin");

    // Install handlers
    event_register_wimp_handler(event_ANY, wimp_NULL_REASON_CODE,
                                handler_wimp_null, this);
    event_register_wimp_handler(myid, wimp_MOUSE_CLICK,
                                handler_mouse_click, this);
    event_register_toolbox_handler(event_ANY,
                                   action_MENU_ABOUT_TO_BE_SHOWN,
                                   handler_tb_menu_shown, this);
    event_register_toolbox_handler(event_ANY,
                                   action_MENU_DIALOGUE_COMPLETED,
                                   handler_tb_menu_completed, this);
    event_register_toolbox_handler(event_ANY,
                                   action_MENU_SELECTION,
                                   handler_tb_menu_selection, this);
    event_register_message_handler(message_DATA_LOAD, handler_message_load,
                                   this);
    event_register_toolbox_handler(patchid, action_ACTION_BUTTON_SELECTED,
                                   handler_tb_action, this);
    event_register_toolbox_handler(patchid, action_RADIO_BUTTON_STATE_CHANGED,
                                   handler_tb_radio, this);
    event_register_toolbox_handler(deleteid, action_ACTION_BUTTON_SELECTED,
                                   handler_tb_action, this);

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
DynamicAreaList::~DynamicAreaList(void)
{
    // Deinstall handlers
    event_deregister_toolbox_handler(deleteid, action_ACTION_BUTTON_SELECTED,
                                     handler_tb_action, this);
    event_deregister_toolbox_handler(patchid,
                                     action_RADIO_BUTTON_STATE_CHANGED,
                                     handler_tb_radio, this);
    event_deregister_toolbox_handler(patchid, action_ACTION_BUTTON_SELECTED,
                                     handler_tb_action, this);
    event_deregister_message_handler(message_DATA_LOAD, handler_message_load,
                                     this);
    event_deregister_toolbox_handler(event_ANY,
                                     action_MENU_DIALOGUE_COMPLETED,
                                     handler_tb_menu_completed, this);
    event_deregister_toolbox_handler(event_ANY,
                                     action_MENU_SELECTION,
                                     handler_tb_menu_selection, this);
    event_deregister_toolbox_handler(event_ANY,
                                     action_MENU_ABOUT_TO_BE_SHOWN,
                                     handler_tb_menu_shown, this);
    event_deregister_wimp_handler(myid, wimp_MOUSE_CLICK,
                                  handler_mouse_click, this);
    event_deregister_wimp_handler(event_ANY, wimp_NULL_REASON_CODE,
                                  handler_wimp_null, this);

    // Destroy the dialogue objects
    toolbox_delete_object(0, deleteid);
    toolbox_delete_object(0, patchid);
}

/*
    Parameters  : message   - The wimp message.
                  handle    - An unused handle.
    Returns     : int       - Was the event claimed.
    Description : Handle DataLoad wimp message events.
*/
bool DynamicAreaList::handler_message_load(wimp_message *message, void *handle)
{
    DynamicAreaList *this_ptr = (DynamicAreaList *) handle;

    // Check which window it was aimed at
    if (message->data.data_xfer.w
        != window_get_wimp_handle(0, this_ptr->myid))
    {
        // Not the right window
        return FALSE;
    }

#ifdef DEMO

    // Show the demo information window
    demo_window->show();

#else

    // Check if it was an application directory
    if (message->data.data_xfer.file_type == 0x2000)
    {
        int i;

        // Choose a sprite name
        i = strlen(message->data.data_xfer.file_name);
        while ((0 < i) && (message->data.data_xfer.file_name[i - 1] != '.'))
        {
            i--;
        }

        if (xwimpspriteop_read_sprite_size(&message->data.data_xfer
                                           .file_name[i], NULL, NULL,
                                           NULL, NULL))
        {
            button_set_value(0, this_ptr->patchid, LIST_MODIFY_SPRITE,
                             "application");
        }
        else
        {
            button_set_value(0, this_ptr->patchid, LIST_MODIFY_SPRITE,
                             &message->data.data_xfer.file_name[i]);
        }

        // Choose and set the default size
        if (!xvirtualise_configure(-1, -1, -1, &i))
        {
            numberrange_set_value(0, this_ptr->patchid, LIST_MODIFY_SIZE,
                                  (i + 512 * 1024) / (1024 * 1024));
        }

        // Open the dialogue box to handle it
        displayfield_set_value(0, this_ptr->patchid, LIST_MODIFY_APP,
                               message->data.data_xfer.file_name);
        toolbox_show_object(toolbox_SHOW_AS_MENU, this_ptr->patchid,
                            toolbox_POSITION_TOP_LEFT,
                            (toolbox_position *) &message->data.data_xfer.pos,
                            this_ptr->myid, 0);
    }

#endif

    // Claim the event
    return TRUE;
}

/*
    Parameters  : event_code    - The event number.
                  action        - The toolbox event.
                  id_block      - The toolbox ID block.
                  handle        - An unused handle.
    Returns     : int           - Was the event claimed.
    Description : Handle radio button state changed toolbox events.
*/
bool DynamicAreaList::handler_tb_radio(bits event_code, toolbox_action *action,
                                       toolbox_block *id_block, void *handle)
{
    DynamicAreaList *this_ptr = (DynamicAreaList *) handle;

    NOT_USED(event_code);
    NOT_USED(action);
    NOT_USED(id_block);

    // Reflect the selection state in the optional options
    if (radiobutton_get_state(0, this_ptr->patchid, LIST_MODIFY_ENABLE, NULL))
    {
        // Enable option is selected
        gadget_set_flags(0, this_ptr->patchid, LIST_MODIFY_SIZE,
                         gadget_get_flags(0, this_ptr->patchid,
                                          LIST_MODIFY_SIZE)
                         & ~gadget_FADED);
        gadget_set_flags(0, this_ptr->patchid, LIST_MODIFY_UNINSTALL,
                         gadget_get_flags(0, this_ptr->patchid,
                                          LIST_MODIFY_UNINSTALL)
                         | gadget_FADED);
    }
    else
    {
        // Disable option is selected
        gadget_set_flags(0, this_ptr->patchid, LIST_MODIFY_SIZE,
                         gadget_get_flags(0, this_ptr->patchid,
                                          LIST_MODIFY_SIZE)
                         | gadget_FADED);
        gadget_set_flags(0, this_ptr->patchid, LIST_MODIFY_UNINSTALL,
                         gadget_get_flags(0, this_ptr->patchid,
                                          LIST_MODIFY_UNINSTALL)
                         & ~gadget_FADED);
    }

    // Claim the event
    return TRUE;
}

/*
    Parameters  : event_code    - The event number.
                  action        - The toolbox event.
                  id_block      - The toolbox ID block.
                  handle        - An unused handle.
    Returns     : int           - Was the event claimed.
    Description : Handle action button toolbox events.
*/
bool DynamicAreaList::handler_tb_action(bits event_code,
                                        toolbox_action *action,
                                        toolbox_block *id_block, void *handle)
{
    DynamicAreaList *this_ptr = (DynamicAreaList *) handle;

    NOT_USED(event_code);
    NOT_USED(action);
    NOT_USED(id_block);

    if ((id_block->this_obj == this_ptr->deleteid)
        && (action->flags & actionbutton_SELECTED_DEFAULT))
    {
        os_error *er;
        AreaRecord *ptr = this_ptr->head;

        while (ptr)
        {
            if (ptr->selected)
            {
                er = xosdynamicarea_delete(ptr->area);
                if (er) wimp_report_error_by_category(er,
                                        wimp_ERROR_BOX_CATEGORY_ERROR << 9,
                                        task_name, AppSprite,
                                        wimpspriteop_AREA, 0);
            }
            ptr = ptr->next;
        }
    }
    else if ((id_block->this_obj == this_ptr->patchid)
             && (action->flags & actionbutton_SELECTED_DEFAULT))
    {
        os_error *er, er2;
        char run[256], run2[256];
        int modified;
        fileswitch_object_type type;

        // Generate names of files
        displayfield_get_value(0, this_ptr->patchid, LIST_MODIFY_APP, run,
                               sizeof(run));
        strcat(run, RUN_MAIN);
        displayfield_get_value(0, this_ptr->patchid, LIST_MODIFY_APP, run2,
                               sizeof(run2));
        strcat(run2, RUN_COPY);

        // Check if application has already been modified
        er = xosfile_read_no_path(run2, &type, NULL, NULL, NULL, NULL);
        modified = (!er) && (type != fileswitch_NOT_FOUND);

        // Attempt to perform the selected operation
        if (radiobutton_get_state(0, this_ptr->patchid, LIST_MODIFY_ENABLE,
                                  NULL))
        {
            // Check if there are any selected areas
            AreaRecord *ptr = this_ptr->head;
            int any = FALSE;
            while (ptr)
            {
                any |= ptr->selected;
                ptr = ptr->next;
            }
            if (any)
            {
            int limit, i;
            char name[256];

            // Construct source file name
            strcpy(name, res_directory);
            strcat(name, MODULE_SRC);

            // Attempt to install the module
            er = xosfscontrol_copy(name, MODULE_DEST,
                                   osfscontrol_COPY_FORCE
                                   | osfscontrol_COPY_NEWER, 0, 0, 0, 0, NULL);
            if (er) wimp_report_error_by_category(er,
                                      wimp_ERROR_BOX_CATEGORY_ERROR << 9,
                                      task_name, AppSprite,
                                      wimpspriteop_AREA, 0);

            // Read the area size limit
            limit = numberrange_get_value(0, this_ptr->patchid,
                                          LIST_MODIFY_SIZE);

            // Rename the existing !Run file if required
            if (!modified)
            {
                er = xosfscontrol_rename(run, run2);
                if (er)
                {
                    wimp_report_error_by_category(er,
                                      wimp_ERROR_BOX_CATEGORY_ERROR << 9,
                                      task_name, AppSprite,
                                      wimpspriteop_AREA, 0);
                    return TRUE;
                }
            }

            // Create the new !Run file
            ofstream file(run);
            if (!file)
            {
                // Attempt to rename the original file
                er = xosfscontrol_rename(run2, run);

                // Generate a suitable error message
                if (er)
                {
                    lookup_token(er2.errmess, sizeof(er2.errmess), "FailRun2");
                }
                else
                {
                    lookup_token(er2.errmess, sizeof(er2.errmess), "FailRun");
                }
                er2.errnum = 0;
                wimp_report_error_by_category(&er2,
                                  wimp_ERROR_BOX_CATEGORY_ERROR << 9,
                                  task_name, AppSprite,
                                  wimpspriteop_AREA, 0);
                return TRUE;
            }

            // Write the required commands to the new !Run file
            file << "| Set system variables to enable virtual memory\n";

            AreaRecord *ptr = this_ptr->head;
            while (ptr)
            {
                if (ptr->selected)
                {
                    i = 0;
                    while (ptr->name[i])
                    {
                        if ((ptr->name[i] == '#')
                            || (ptr->name[i] == '*')
                            || (ptr->name[i] == ' '))
                        {
                            name[i] = '_';
                        }
                        else name[i] = ptr->name[i];
                        i++;
                    }
                    name[i] = 0;
                    file << "SetEval Virtualise$Area_" << name
                         << " " << limit << "\n";
                }
                ptr = ptr->next;
            }

            file << "\n| Run the original !Run file\n"
                 << "Run <Obey$Dir>" << RUN_COPY << " %*0";

            // Set the file type suitably
            file.close();
            er = xosfile_set_type(run, 0xfeb);
            }
            else
            {
                // Generate an error if no areas are selected to modify
                er2.errnum = 0;
                lookup_token(er2.errmess, sizeof(er2.errmess), "NotArea");
                wimp_report_error_by_category(&er2,
                                    wimp_ERROR_BOX_CATEGORY_INFO << 9,
                                    task_name, AppSprite,
                                    wimpspriteop_AREA, 0);
            }
        }
        else if (radiobutton_get_state(0, this_ptr->patchid,
                                       LIST_MODIFY_DISABLE, NULL))
        {
            // Deinstall module if required
            if (optionbutton_get_state(0, this_ptr->patchid,
                                       LIST_MODIFY_UNINSTALL))
            {
                xosfscontrol_wipe(MODULE_DEST, osfscontrol_WIPE_FORCE,
                                  0, 0, 0, 0);
            }

            // Check if application has been modified
            if (modified)
            {
                // Delete existing !Run file
                er = xosfscontrol_wipe(run, osfscontrol_WIPE_FORCE, 0, 0, 0, 0);
                if (er)
                {
                    wimp_report_error_by_category(er,
                                        wimp_ERROR_BOX_CATEGORY_ERROR << 9,
                                        task_name, AppSprite,
                                        wimpspriteop_AREA, 0);
                    return TRUE;
                }

                // Rename original !Run file
                er = xosfscontrol_rename(run2, run);
                if (er)
                {
                    wimp_report_error_by_category(er,
                                        wimp_ERROR_BOX_CATEGORY_ERROR << 9,
                                        task_name, AppSprite,
                                        wimpspriteop_AREA, 0);
                    return TRUE;
                }
            }
            else
            {
                // Generate an error if application has not been modified
                er2.errnum = 0;
                lookup_token(er2.errmess, sizeof(er2.errmess), "NotMod");
                wimp_report_error_by_category(&er2,
                                    wimp_ERROR_BOX_CATEGORY_INFO << 9,
                                    task_name, AppSprite,
                                    wimpspriteop_AREA, 0);
            }
        }
    }

    // Claim the event
    return TRUE;
}

/*
    Parameters  : event_code    - The event number.
                  action        - The toolbox event.
                  id_block      - The toolbox ID block.
                  handle        - An unused handle.
    Returns     : int           - Was the event claimed.
    Description : Handle menu about to be shown toolbox events.
*/
bool DynamicAreaList::handler_tb_menu_shown(bits event_code,
                                            toolbox_action *action,
                                            toolbox_block *id_block,
                                            void *handle)
{
    DynamicAreaList *list = (DynamicAreaList *) handle;

    NOT_USED(event_code);
    NOT_USED(action);

    if (id_block->ancestor_obj == list->myid)
    {
        toolbox_block find_id;

        // Try to find which component of which object produced the menu
        window_get_pointer_info(0, 0, 0, 0,
                                &find_id.this_obj, &find_id.this_cmp);

        // Fake a mouse click at that point
        wimp_block block;
        block.pointer.buttons = wimp_CLICK_MENU;
        handler_mouse_click(wimp_MOUSE_CLICK, &block, &find_id, handle);

        // Fade out items if no areas selected
        list->prepare_menu(id_block->this_obj);
    }

    // Do not claim the event
    return FALSE;
}

/*
    Parameters  : event_code    - The event number.
                  action        - The toolbox event.
                  id_block      - The toolbox ID block.
                  handle        - An unused handle.
    Returns     : int           - Was the event claimed.
    Description : Handle menu selection toolbox events.
*/
bool DynamicAreaList::handler_tb_menu_selection(bits event_code,
                                                toolbox_action *action,
                                                toolbox_block *id_block,
                                                void *handle)
{
    DynamicAreaList *list = (DynamicAreaList *) handle;
    os_error *er;

    NOT_USED(event_code);
    NOT_USED(action);

    if (id_block->ancestor_obj == list->myid)
    {
        // It is the menu for this window
        AreaRecord *ptr = list->head;
        switch (id_block->this_cmp)
        {
            case LIST_MENU_VIRTUALISE:
                // Either enable or disable virtual memory
                if (menu_get_tick(0, id_block->this_obj, LIST_MENU_VIRTUALISE))
                {
                    // Disable virtual memory
                    while (ptr)
                    {
                        if (ptr->menu_selected)
                        {
                            er = xvirtualise_end(ptr->area);
                            if (er) wimp_report_error_by_category(er,
                                            wimp_ERROR_BOX_CATEGORY_INFO << 9,
                                            task_name, AppSprite,
                                            wimpspriteop_AREA, 0);
                        }
                        ptr = ptr->next;
                    }
                }
                else
                {
                    // Enable virtual memory
                    while (ptr)
                    {
                        if (ptr->menu_selected)
                        {
#ifdef DEMO

                            if (!strstr(ptr->name, DEMO_SUFFIX))
                            {
                                demo_window->show();
                            }
                            else

#else

                            if (TRUE)

#endif

                            {
                                er = xvirtualise_start(ptr->area);
                                if (er) wimp_report_error_by_category(er,
                                            wimp_ERROR_BOX_CATEGORY_INFO << 9,
                                            task_name, AppSprite,
                                            wimpspriteop_AREA, 0);
                            }
                        }
                        ptr = ptr->next;
                    }
                }
                list->prepare_menu(id_block->this_obj);
                break;

            case LIST_MENU_REMOVE:
                // Remove all the selected areas
                while (ptr)
                {
                    ptr->select(ptr->menu_selected);
                    ptr = ptr->next;
                }
                list->menu_select = FALSE;
                {
                    wimp_pointer point;

                    wimp_get_pointer_info(&point);
                    toolbox_show_object(toolbox_SHOW_AS_MENU, list->deleteid,
                                        toolbox_POSITION_TOP_LEFT,
                                        (toolbox_position *) &point.pos,
                                        list->myid, 0);
                }
                list->prepare_menu(id_block->this_obj);
                break;

            case LIST_MENU_MAP:
                // Open map windows for the selected dynamic areas
                while (ptr)
                {
                    if (ptr->menu_selected)
                    {
                        Status::open(ptr->area);
                    }
                    ptr = ptr->next;
                }
                list->prepare_menu(id_block->this_obj);
                break;

            case LIST_MENU_SELECT:
                // Select all dynamic areas
                while (ptr)
                {
                    ptr->select(TRUE);
                    ptr = ptr->next;
                }
                list->menu_select = FALSE;
                list->prepare_menu(id_block->this_obj);
                break;

            case LIST_MENU_CLEAR:
                // Clear all dynamic areas
                while (ptr)
                {
                    ptr->select(FALSE);
                    ptr = ptr->next;
                }
                list->menu_select = FALSE;
                list->prepare_menu(id_block->this_obj);
                break;

            default:
                break;
        }
    }

    // Do not claim the event
    return FALSE;
}

/*
    Parameters  : event_code    - The event number.
                  action        - The toolbox event.
                  id_block      - The toolbox ID block.
                  handle        - An unused handle.
    Returns     : int           - Was the event claimed.
    Description : Handle menu completed toolbox events.
*/
bool DynamicAreaList::handler_tb_menu_completed(bits event_code,
                                                toolbox_action *action,
                                                toolbox_block *id_block,
                                                void *handle)
{
    DynamicAreaList *list = (DynamicAreaList *) handle;

    NOT_USED(event_code);
    NOT_USED(action);
    NOT_USED(id_block);

    if (id_block->ancestor_obj == list->myid)
    {
        // It is the dynamic area menu, so clear any temporary selection
        if (list->menu_select)
        {
            AreaRecord *ptr = list->head;
            while (ptr)
            {
                ptr->select(FALSE);
                ptr = ptr->next;
            }
            list->menu_select = FALSE;
        }
    }

    // Do not claim the event
    return FALSE;
}

/*
    Parameters  : id    - The menu object ID.
    Returns     : void
    Description : Copy the selection status of areas and sets the menu flags
                  appropriately.
*/
void DynamicAreaList::prepare_menu(toolbox_o id)
{
    // Ensure that the list is accurate
    update();

    // Fade out items if no areas selected
    bool any = FALSE, vm = FALSE, all = TRUE;
    AreaRecord *ptr = head;
    while (ptr)
    {
        if (256 <= ptr->area) all &= ptr->selected;
        if (ptr->selected)
        {
            any = TRUE;
            vm |= ptr->val[LIST_FIELD_VM];
        }
        ptr->menu_selected = ptr->selected;
        ptr = ptr->next;
    }
    menu_set_fade(0, id, LIST_MENU_VIRTUALISE, !any);
    menu_set_fade(0, id, LIST_MENU_REMOVE, !any);
    menu_set_fade(0, id, LIST_MENU_MAP, !any);
    menu_set_fade(0, id, LIST_MENU_SELECT, all);
    menu_set_fade(0, id, LIST_MENU_CLEAR, !any);
    menu_set_tick(0, id, LIST_MENU_VIRTUALISE, vm);
}

/*
    Parameters  : event_code    - The event number.
                  wimp_block    - The wimp poll block.
                  id_block      - The toolbox ID block.
                  handle        - An unused handle.
    Returns     : int           - Was the event claimed.
    Description : Handle wimp null polls.
*/
bool DynamicAreaList::handler_wimp_null(wimp_event_no event_code,
                                        wimp_block *block,
                                        toolbox_block *id_block, void *handle)
{
    DynamicAreaList *this_pointer = (DynamicAreaList *) handle;

    NOT_USED(event_code);
    NOT_USED(block);
    NOT_USED(id_block);

    // Pass on to the actual routine
    this_pointer->update();

    // Do not claim the event
    return FALSE;
}

/*
    Parameters  : void
    Returns     : void
    Description : Update the dynamic area list.
*/
void DynamicAreaList::update(void)
{
    AreaRecord *ptr, *prev;
    os_error *er;

    // Mark all records as not updated
    ptr = head;
    while (ptr)
    {
        ptr->updated = FALSE;
        ptr = ptr->next;
    }

    // Enumerate all the dynamic areas
    int logical, maximum, physical;
    char *name;
    os_area_flags flags;
    os_dynamic_area_no context = osdynamicarea_enumerate(-1);
    while (context != -1)
    {
        // Read details about the area
        osdynamicarea_read(context, &logical, 0, &flags, &maximum, 0, 0, &name);
        er = xvirtualise_miscop_physical(context, &physical);
        if (er)
        {
            physical = logical;
            flags &= ~os_AREA_VIRTUALISED;
        }

        // Find this area and insert a new record if not found
        int cmp = -1;
        prev = NULL;
        ptr = head;
        while (ptr && (0 < (cmp = ptr->compare(context, name))))
        {
            prev = ptr;
            ptr = ptr->next;
        }

        // Add a new record if it was not found
        if (cmp)
        {

            ptr = new AreaRecord(myid, context, name);
            if (prev)
            {
                ptr->next = prev->next;
                prev->next = ptr;
            }
            else
            {
                ptr->next = head;
                head = ptr;
            }

            // And add it to the database
            Database::add(name);
        }

        // Update the values
        ptr->updated = TRUE;
        ptr->update(LIST_FIELD_LOGICAL, logical);
        ptr->update(LIST_FIELD_PHYSICAL, physical);
        ptr->update(LIST_FIELD_MAXIMUM, maximum);
        ptr->update(LIST_FIELD_VM, flags & os_AREA_VIRTUALISED);

        // Find next dynamic area
        context = osdynamicarea_enumerate(context);
    }

    // Remove any un-referenced records
    prev = NULL;
    ptr = head;
    while (ptr)
    {
        if (!ptr->updated)
        {
            // Remove this record
            AreaRecord *old = ptr;
            if (prev) ptr = prev->next = ptr->next;
            else ptr = head = ptr->next;
            delete old;
        }
        else
        {
            prev = ptr;
            ptr = ptr->next;
        }
    }

    // Update the positions
    int pos = 0;
    ptr = head;
    while (ptr)
    {
        pos = ptr->pos(pos);
        ptr = ptr->next;
    }

    // Update the window extent
    os_box bbox;
    window_get_extent(0, myid, &bbox);
    if (bbox.y0 != pos)
    {
        bbox.y0 = pos;
        window_set_extent(0, myid, &bbox);
    }
}

/*
    Parameters  : win       - The object ID of the list window.
                  number    - The number of the dynamic area.
                  title     - The name of the dynamic area.
    Returns     : -
    Description : Constructor function.
*/
DynamicAreaList::AreaRecord::AreaRecord(toolbox_o win,
                                        os_dynamic_area_no number,
                                        const char *title)
{
    int i, size;
    toolbox_id desc;
    gadget_OBJECT(sizeof(button_gadget)) gnew;
    gadget_object *gorig;

    // Copy the area details
    myid = win;
    area = number;
    name = new char[strlen(title) + 1];
    strcpy(name, title);

    // Create the gadgets for the various fields
    desc = toolbox_template_look_up(0, "AreasGadget");
    for (i = 0; i < LIST_NUM_FIELDS; i++)
    {
        gorig = window_extract_gadget_info(0, (toolbox_resource_file_object *) desc, i, &size);
        memcpy(&gnew, gorig, size);
        gnew.cmp = -1;
        id[i] = window_add_gadget(0, myid, (gadget_object *) &gnew);
        if (area < 256) button_set_flags(0, myid, id[i], 0, wimp_ICON_SHADED);
        val[i] = -1;
    }

    // Set the values for the static fields
    char str[11];
    sprintf(str, "&%X", area);
    button_set_value(0, myid, id[LIST_FIELD_NUMBER], str);
    button_set_value(0, myid, id[LIST_FIELD_NAME], name);

    // Initialise selection status
    selected = FALSE;
}

/*
    Parameters  : -
    Returns     : -
    Description : Constructor function.
*/
DynamicAreaList::AreaRecord::~AreaRecord(void)
{
    int i;

    // Remove the gadgets from the window
    for (i = 0; i < LIST_NUM_FIELDS; i++)
    {
        window_remove_gadget(0, myid, id[i]);
    }

    // Free any used memory
    delete[] name;
}

/*
    Parameters  : start - The position to place the top of the gadgets.
    Returns     : int   - The position of the bottom of the gadget.
    Description : Set the vertical position of the gadgets.
*/
int DynamicAreaList::AreaRecord::pos(int start)
{
    int i, height;
    os_box bbox;

    for (i = 0; i < LIST_NUM_FIELDS; i++)
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
    Parameters  : field - The field to update.
                  value - The new value of the field.
    Returns     : void
    Description : Update a numeric value field.
*/
void DynamicAreaList::AreaRecord::update(int field, int value)
{
    if (value != val[field])
    {
        if (field == LIST_FIELD_VM)
        {
            button_set_value(0, myid, id[field], value ? yes : no);
        }
        else
        {
            char str[9];
            sprintf(str, "%iK", value / 1024);
            button_set_value(0, myid, id[field], str);
        }
        val[field] = value;
    }
}

/*
    Parameters  : number    - The number of the dynamic area.
                  title     - The name of the dynamic area.
    Returns     : int       - -1 if the area comes before this.
                              0 if the area matches.
                              1 if the area comes after this.
    Description : Check if an area number and name match.
*/
int DynamicAreaList::AreaRecord::compare(os_dynamic_area_no number,
                                         const char *title)
{
    int cmp = strcmp(title, name);
    if (!cmp && (number != area)) cmp = number < area ? -1 : 1;

    return cmp;
}

/*
    Parameters  : event_code    - The event number.
                  wimp_block    - The wimp poll block.
                  id_block      - The toolbox ID block.
                  handle        - An unused handle.
    Returns     : int           - Was the event claimed.
    Description : Handle mouse clicks in the dynamic area list.
*/
bool DynamicAreaList::handler_mouse_click(wimp_event_no event_code,
                                          wimp_block *block,
                                          toolbox_block *id_block,
                                          void *handle)
{
    DynamicAreaList *list = (DynamicAreaList *) handle;

    NOT_USED(event_code);

    // Find which dynamic area was clicked on
    int i = 0;
    AreaRecord *area = list->head;
    while (area && ((area->myid != id_block->this_obj)
                    || (area->id[i] != id_block->this_cmp)))
    {
        if (LIST_NUM_FIELDS <= ++i)
        {
            i = 0;
            area = area->next;
        }
    }
    if (!area) return FALSE;

    // Modify selection state based upon which button was pressed
    if (block->pointer.buttons & wimp_DOUBLE_SELECT)
    {
        // Double click so open status window
        Status::open(area->area);
    }
    if (block->pointer.buttons & wimp_DOUBLE_ADJUST)
    {
        // Double click so open update window and close list window
        Status::open(area->area);
        toolbox_hide_object(0, list->myid);
    }
    else if (block->pointer.buttons & wimp_CLICK_MENU)
    {
        // If none selected then select this one until menu closed
        bool any = FALSE;
        AreaRecord *ptr = list->head;
        while (ptr)
        {
            any |= ptr->selected;
            ptr = ptr->next;
        }
        if (!any)
        {
            area->select(TRUE);
            list->menu_select = TRUE;
        }

        // Create and open the menu
    }
    else if (block->pointer.buttons & wimp_SINGLE_ADJUST)
    {
        // Just toggle this one
        area->select(!area->selected);
    }
    else
    {
        // If not selected then select this one and deselect all others
        if (!area->selected)
        {
            AreaRecord *ptr = list->head;
            while (ptr)
            {
                ptr->select(ptr == area);
                ptr = ptr->next;
            }
        }
    }

    // Claim the event
    return TRUE;
}

/*
    Parameters  : sel   - Should this area be selected.
    Returns     : void
    Description : Set the selection status.
*/
void DynamicAreaList::AreaRecord::select(bool sel)
{
    if ((sel != selected) && (256 <= area))
    {
        selected = sel;
        int i;
        for (i = 0; i < LIST_NUM_FIELDS; i++)
        {
            button_set_flags(0, myid, id[i], wimp_ICON_SELECTED,
                             sel ? wimp_ICON_SELECTED : 0);
        }
    }
}
