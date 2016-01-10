/*
    File        : automatic.c++
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

// Include header file for this module
#include "automatic.h"

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
#include "configure.h"
#include "database.h"
#include "main.h"

// Saved database file
#define AUTO_FILE ".Automatic"

// Filenames for module copy
#define MODULE_SRC ".Virtualise"
#define MODULE_DEST "<Choices$Write>.Boot.PreDesk.Virtualise"

/*
    Parameters  : var   - Variable to receive the variable name.
                  name  - The dynamic area name.
    Returns     : void
    Description : Convert a dynamic area name into the equivalent variable.
*/
static void to_variable(char *var, const char *name)
{
    // Set the variable stub
    strcpy(var, "Virtualise$Area_");
    var += strlen(var);

    // Convert the area name
    while (*name)
    {
        if ((*name == '#')
            || (*name == '*')
            || (*name == ' '))
        {
            *var = '_';
        }
        else *var = *name;
        var++;
        name++;
    }
    *var = 0;
}

/*
    Parameters  : win   - The object ID of the window in which the list
                          should appear.
    Returns     : -
    Description : Constructor function.
*/
AutomaticList::AutomaticList(toolbox_o win)
{
    toolbox_o menuid;

    // Copy the window ID
    myid = win;

    // Read the ID of the menu and the database menu and limit window IDs
    menuid = window_get_menu(0, myid);
    limitid = menu_get_sub_menu_show(0, menuid, AUTO_MENU_LIMIT);
    dataid = menu_get_sub_menu_show(0, menuid, AUTO_MENU_ADD);

    // No temporary selection
    menu_select = FALSE;

    // No dynamic area records yet
    head = NULL;

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
    event_register_toolbox_handler(limitid, action_ACTION_BUTTON_SELECTED,
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
AutomaticList::~AutomaticList(void)
{
    // Deinstall handlers
    event_deregister_toolbox_handler(limitid, action_ACTION_BUTTON_SELECTED,
                                     handler_tb_action, this);
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
}


/*
    Parameters  : event_code    - The event number.
                  action        - The toolbox event.
                  id_block      - The toolbox ID block.
                  handle        - An unused handle.
    Returns     : int           - Was the event claimed.
    Description : Handle action button toolbox events.
*/
bool AutomaticList::handler_tb_action(bits event_code,
                                      toolbox_action *action,
                                      toolbox_block *id_block, void *handle)
{
    AutomaticList *this_ptr = (AutomaticList *) handle;

    NOT_USED(event_code);
    NOT_USED(action);
    NOT_USED(id_block);

    if (action->flags & actionbutton_SELECTED_DEFAULT)
    {
        int limit;

        // Read the area size limit
        limit = numberrange_get_value(0, this_ptr->limitid, AUTO_LIMIT_SIZE);

        // Set the size of the selected dynamic areas
        AreaRecord *ptr = this_ptr->head;
        while (ptr)
        {
            if (ptr->selected)
            {
                os_set_var_val(ptr->var, (const byte *) &limit, sizeof(limit),
                               0, os_VARTYPE_NUMBER, NULL);
            }
            ptr = ptr->next;
        }

        // Update the display to reflect the changes
        this_ptr->update();
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
bool AutomaticList::handler_tb_menu_shown(bits event_code,
                                          toolbox_action *action,
                                          toolbox_block *id_block,
                                          void *handle)
{
    AutomaticList *list = (AutomaticList *) handle;

    NOT_USED(event_code);
    NOT_USED(action);

    if (id_block->parent_obj == list->myid)
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
bool AutomaticList::handler_tb_menu_selection(bits event_code,
                                              toolbox_action *action,
                                              toolbox_block *id_block,
                                              void *handle)
{
    AutomaticList *list = (AutomaticList *) handle;
    os_error *er;

    NOT_USED(event_code);
    NOT_USED(action);

    if (id_block->this_obj == list->dataid)
    {
        char name[256], var[256];
        int limit = 256;

        // It is the menu of areas to add
        menu_get_entry_text(0, id_block->this_obj, id_block->this_cmp, name,
                            sizeof(name));
        to_variable(var, name);
        os_set_var_val(var, (const byte *) &limit, sizeof(limit), 0,
                       os_VARTYPE_NUMBER, NULL);
    }
    else if (id_block->ancestor_obj == list->myid)
    {
        // It is the menu for this window
        AreaRecord *ptr = list->head;
        switch (id_block->this_cmp)
        {
            case AUTO_MENU_REMOVE:
                // Remove names of dynamic areas
                while (ptr)
                {
                    if (ptr->selected)
                    {
                        xos_set_var_val(ptr->var, NULL, -1, 0,
                                        os_VARTYPE_NUMBER, NULL, NULL);
                    }
                    ptr = ptr->next;
                }
                list->update();
                break;

            case AUTO_MENU_SAVE:
                // Save the list of dynamic areas
                {
                    char name[256];
                    ofstream file(CONFIGURATION_DIRECTORY AUTO_FILE);
                    if (file)
                    {
                        while (ptr)
                        {
                            file << "SetEval " << ptr->var
                                 << " " << ptr->limit << "\n";
                            ptr = ptr->next;
                        }

                        // Set the file type suitably
                        file.close();
                        er = xosfile_set_type(CONFIGURATION_DIRECTORY
                                              AUTO_FILE, 0xfeb);
                    }

                    // Construct source file name
                    strcpy(name, res_directory);
                    strcat(name, MODULE_SRC);

                    // Attempt to install the module
                    er = xosfscontrol_copy(name, MODULE_DEST,
                                           osfscontrol_COPY_FORCE
                                           | osfscontrol_COPY_NEWER,
                                           0, 0, 0, 0, NULL);
                    if (er) wimp_report_error_by_category(er,
                                          wimp_ERROR_BOX_CATEGORY_ERROR << 9,
                                          task_name, AppSprite,
                                          wimpspriteop_AREA, 0);
                }

                break;

            case AUTO_MENU_SELECT:
                // Select all dynamic areas
                while (ptr)
                {
                    ptr->select(TRUE);
                    ptr = ptr->next;
                }
                list->menu_select = FALSE;
                list->prepare_menu(id_block->this_obj);
                break;

            case AUTO_MENU_CLEAR:
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
bool AutomaticList::handler_tb_menu_completed(bits event_code,
                                              toolbox_action *action,
                                              toolbox_block *id_block,
                                              void *handle)
{
    AutomaticList *list = (AutomaticList *) handle;

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
void AutomaticList::prepare_menu(toolbox_o id)
{
    // Ensure that the list is accurate
    update();

    // Fade out items if no areas selected
    bool any = FALSE, all = TRUE;

    AreaRecord *ptr = head;
    while (ptr)
    {
        all &= ptr->selected;
        any |= ptr->selected;
        ptr->menu_selected = ptr->selected;
        ptr = ptr->next;
    }

    menu_set_fade(0, id, AUTO_MENU_LIMIT, !any);
    menu_set_fade(0, id, AUTO_MENU_REMOVE, !any);
    menu_set_fade(0, id, AUTO_MENU_SELECT, all);
    menu_set_fade(0, id, AUTO_MENU_CLEAR, !any);
}

/*
    Parameters  : event_code    - The event number.
                  wimp_block    - The wimp poll block.
                  id_block      - The toolbox ID block.
                  handle        - An unused handle.
    Returns     : int           - Was the event claimed.
    Description : Handle wimp null polls.
*/
bool AutomaticList::handler_wimp_null(wimp_event_no event_code,
                                      wimp_block *block,
                                      toolbox_block *id_block, void *handle)
{
    AutomaticList *this_pointer = (AutomaticList *) handle;

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
bool AutomaticList::handler_mouse_click(wimp_event_no event_code,
                                        wimp_block *block,
                                        toolbox_block *id_block,
                                        void *handle)
{
    AutomaticList *list = (AutomaticList *) handle;

    NOT_USED(event_code);

    // Find which dynamic area was clicked on
    int i = 0;
    AreaRecord *area = list->head;
    while (area && ((area->myid != id_block->this_obj)
                    || (area->id[i] != id_block->this_cmp)))
    {
        if (AUTO_NUM_FIELDS <= ++i)
        {
            i = 0;
            area = area->next;
        }
    }
    if (!area) return FALSE;

    if (block->pointer.buttons & wimp_CLICK_MENU)
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
    else if (block->pointer.buttons & wimp_CLICK_ADJUST)
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
    Parameters  : void
    Returns     : void
    Description : Update the dynamic area list.
*/
void AutomaticList::update(void)
{
    static int last = 0;
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
    int maximum;
    os_var_type type;
    int context, used;
    er = xos_read_var_val("Virtualise$Area_*", (char *) &maximum,
                          sizeof(maximum), 0, os_VARTYPE_NUMBER, &used,
                          &context, &type);
    while (!er && used)
    {
        // Only accept the variable if the type is correct
        if (type == os_VARTYPE_NUMBER)
        {
            // Find this area and insert a new record if not found
            int cmp = -1;
            prev = NULL;
            ptr = head;
            while (ptr && (0 < (cmp = ptr->compare((char *) context))))
            {
                prev = ptr;
                ptr = ptr->next;
            }

            // Add a new record if it was not found
            if (cmp)
            {
                ptr = new AreaRecord(myid, (char *) context);
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
            }

            // Update the values
            ptr->updated = TRUE;
            ptr->update(maximum);
        }

        // Find next dynamic area
        er = xos_read_var_val("Virtualise$Area_*", (char *) &maximum,
                              sizeof(maximum), context, os_VARTYPE_NUMBER,
                              &used, &context, &type);
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
    Parameters  : win       - The object ID of the list window.
                  title     - The name of the dynamic area.
                  size      - The size limit for this dynamic area.
    Returns     : -
    Description : Constructor function.
*/
AutomaticList::AreaRecord::AreaRecord(toolbox_o win, const char *title)
{
    int i, s;
    toolbox_id desc;
    gadget_OBJECT(sizeof(button_gadget)) gnew;
    gadget_object *gorig;
    char *ptr;

    // Copy the area details
    myid = win;
    limit = -1;
    var = new char[strlen(title) + 1];
    strcpy(var, title);
    ptr = Database::lookup(title, to_variable);
    if (ptr)
    {
        name = new char[strlen(ptr) + 1];
        strcpy(name, ptr);
    }
    else
    {
        name = new char[strlen(title) + 1];
        strcpy(name, title + 16);
    }

    // Create the gadgets for the various fields
    desc = toolbox_template_look_up(0, "AutoGadget");
    for (i = 0; i < AUTO_NUM_FIELDS; i++)
    {
        gorig = window_extract_gadget_info(0, (toolbox_resource_file_object *) desc, i, &s);
        memcpy(&gnew, gorig, s);
        gnew.cmp = -1;
        id[i] = window_add_gadget(0, myid, (gadget_object *) &gnew);
    }

    // Set the values for the static field
    button_set_value(0, myid, id[AUTO_FIELD_NAME], name);

    // Initialise selection status
    selected = FALSE;
}

/*
    Parameters  : -
    Returns     : -
    Description : Constructor function.
*/
AutomaticList::AreaRecord::~AreaRecord(void)
{
    int i;

    // Remove the gadgets from the window
    for (i = 0; i < AUTO_NUM_FIELDS; i++)
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
int AutomaticList::AreaRecord::pos(int start)
{
    int i, height;
    os_box bbox;

    for (i = 0; i < AUTO_NUM_FIELDS; i++)
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
void AutomaticList::AreaRecord::update(int value)
{
    if (value != limit)
    {
        char str[9];
        sprintf(str, "%iMB", value);
        button_set_value(0, myid, id[AUTO_FIELD_LIMIT], str);
        limit = value;
    }
}

/*
    Parameters  : title     - The name of the dynamic area.
    Returns     : int       - -1 if the area comes before this.
                              0 if the area matches.
                              1 if the area comes after this.
    Description : Check if an area variable name matches.
*/
int AutomaticList::AreaRecord::compare(const char *title)
{
    return strcmp(title, var);
}

/*
    Parameters  : sel   - Should this area be selected.
    Returns     : void
    Description : Set the selection status.
*/
void AutomaticList::AreaRecord::select(bool sel)
{
    if (sel != selected)
    {
        selected = sel;
        int i;
        for (i = 0; i < AUTO_NUM_FIELDS; i++)
        {
            button_set_flags(0, myid, id[i], wimp_ICON_SELECTED,
                             sel ? wimp_ICON_SELECTED : 0);
        }
    }
}
