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

// Include the header file for this module
#include "status.h"

// Include clib header files
#include <stdio.h>
#include <string.h>

// Include oslib header files
#include "actionbutton.h"
#include "displayfield.h"
#include "menu.h"
#include "toolbox.h"
#include "window.h"
extern "C" {
#include "event.h"
}

// Include project header files
#include "virtual.h"

// Size of items within the main window
#define PAGES_ACROSS 128
#define PAGES_SIZE 6
#define PAGES_STEP 8
#define PAGES_GAP 12

// Head of linked list of objects
Status *Status::head = NULL;

// The default scale
int Status::def_scale = 2;

// Colour translation table
const wimp_colour Status::colours[virtualise_PAGE_NUM] =
{
    wimp_COLOUR_LIGHT_GREY,             // Not used much
    wimp_COLOUR_LIGHT_GREY,
    wimp_COLOUR_MID_LIGHT_GREY,
    wimp_COLOUR_MID_LIGHT_GREY,
    wimp_COLOUR_MID_LIGHT_GREY,
    wimp_COLOUR_MID_DARK_GREY,
    wimp_COLOUR_MID_DARK_GREY,
    wimp_COLOUR_MID_DARK_GREY,
    wimp_COLOUR_DARK_GREY,
    wimp_COLOUR_DARK_GREY,
    wimp_COLOUR_DARK_GREY,
    wimp_COLOUR_VERY_DARK_GREY,
    wimp_COLOUR_VERY_DARK_GREY,
    wimp_COLOUR_VERY_DARK_GREY,
    wimp_COLOUR_BLACK,
    wimp_COLOUR_BLACK,                  // Used a lot
    wimp_COLOUR_WHITE,                  // Invalid page
    wimp_COLOUR_LIGHT_BLUE,             // Unused page
    wimp_COLOUR_LIGHT_GREEN,            // Page is on disc
    wimp_COLOUR_RED                     // Page is locked
};

/*
    Parameters  : area  - The number of the dynamic area to open window for.
    Returns     : -
    Description : Constructor function.
*/
Status::Status(os_dynamic_area_no area)
{
    // Store the area number and initialise the structure
    num = area;
    status = NULL;

    // Add this window to the list
    next = head;
    head = this;

    // Create and open the window
    win_id = toolbox_create_object(0, (toolbox_id) "MapWin");
    window_get_tool_bars(window_TOOL_BAR_EBL, win_id,
                         NULL, NULL, &tool_id, NULL);

    // Set the area number field
    char num_str[11];
    sprintf(num_str, "&%X", area);
    displayfield_set_value(0, tool_id, STATUS_FIELD_NUMBER, num_str);

    // Copy the default scale for this window
    scale = def_scale;

    // Install handlers
    event_register_wimp_handler(win_id, wimp_REDRAW_WINDOW_REQUEST,
                                handler_wimp_redraw, this);
    event_register_toolbox_handler(win_id,
                                   action_WINDOW_DIALOGUE_COMPLETED,
                                   handler_tb_window_hidden, this);
    event_register_toolbox_handler(tool_id,
                                   action_ACTION_BUTTON_SELECTED,
                                   handler_tb_action_button, this);
    event_register_toolbox_handler(event_ANY,
                                   action_MENU_ABOUT_TO_BE_SHOWN,
                                   handler_tb_menu_shown, this);
    event_register_toolbox_handler(event_ANY,
                                   action_MENU_SELECTION,
                                   handler_tb_menu_selection, this);

    // Update the other information
    update();
}

/*
    Parameters  : -
    Returns     : -
    Description : Destructor function.
*/
Status::~Status(void)
{
    Status *ptr, *prev;

    // Deinstall handlers
    event_deregister_toolbox_handler(event_ANY,
                                     action_MENU_SELECTION,
                                     handler_tb_menu_selection, this);
    event_deregister_toolbox_handler(event_ANY,
                                     action_MENU_ABOUT_TO_BE_SHOWN,
                                     handler_tb_menu_shown, this);
    event_deregister_toolbox_handler(tool_id,
                                     action_ACTION_BUTTON_SELECTED,
                                     handler_tb_action_button, this);
    event_deregister_toolbox_handler(win_id,
                                     action_WINDOW_DIALOGUE_COMPLETED,
                                     handler_tb_window_hidden, this);
    event_deregister_wimp_handler(win_id, wimp_REDRAW_WINDOW_REQUEST,
                                  handler_wimp_redraw, this);

    // Free any memory claimed
    if (status) delete[] status;
    status = NULL;

    // Unlink from the list
    prev = NULL;
    ptr = head;
    while (ptr != this)
    {
        prev = ptr;
        ptr = ptr->next;
    }
    if (prev) prev->next = next;
    else head = next;
}

/*
    Parameters  : event_code    - The event number.
                  action        - The toolbox event.
                  id_block      - The toolbox ID block.
                  handle        - An unused handle.
    Returns     : int           - Was the event claimed.
    Description : Handle window hidden toolbox events.
*/
bool Status::handler_tb_window_hidden(bits event_code, toolbox_action *action,
                                      toolbox_block *id_block, void *handle)
{
    Status *this_ptr = (Status *) handle;

    NOT_USED(event_code);
    NOT_USED(action);
    NOT_USED(id_block);

    // Delete the object
    delete this_ptr;

    // Delete the object
    toolbox_delete_object(0, id_block->this_obj);

    // Claim the event
    return FALSE;
}

/*
    Parameters  : event_code    - The event number.
                  action        - The toolbox event.
                  id_block      - The toolbox ID block.
                  handle        - An unused handle.
    Returns     : int           - Was the event claimed.
    Description : Handle menu about to be shown toolbox events.
*/
bool Status::handler_tb_menu_shown(bits event_code, toolbox_action *action,
                                   toolbox_block *id_block, void *handle)
{
    Status *this_ptr = (Status *) handle;

    NOT_USED(event_code);
    NOT_USED(action);

    if (id_block->ancestor_obj == this_ptr->win_id)
    {
        // Set tick as appropriate
        menu_set_tick(0, id_block->this_obj, 1, this_ptr->scale == 1);
        menu_set_tick(0, id_block->this_obj, 2, this_ptr->scale == 2);
        menu_set_tick(0, id_block->this_obj, 4, this_ptr->scale == 4);
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
bool Status::handler_tb_menu_selection(bits event_code, toolbox_action *action,
                                       toolbox_block *id_block, void *handle)
{
    Status *this_ptr = (Status *) handle;

    NOT_USED(event_code);
    NOT_USED(action);

    if (id_block->ancestor_obj == this_ptr->win_id)
    {
        // It is the menu for this window
        char name[12];

        // Find which menu it is
        toolbox_get_template_name(0, id_block->this_obj, name, sizeof(name));

        if (!strcmp(name, "MapScale"))
        {
            // It is the scale window
            this_ptr->def_scale = this_ptr->scale = id_block->this_cmp;
            this_ptr->update();
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
    Description : Handle action button clicked toolbox events.
*/
bool Status::handler_tb_action_button(bits event_code, toolbox_action *action,
                                      toolbox_block *id_block, void *handle)
{
    Status *this_ptr = (Status *) handle;

    NOT_USED(event_code);
    NOT_USED(action);
    NOT_USED(id_block);

    // Update the window
    this_ptr->update();

    // Claim the event
    return FALSE;
}

/*
    Parameters  : area  - The number of the dynamic area to open window for.
    Returns     : void
    Description : Opens a window for the specified dynamic area. If one
                  already exists then it is reopened.
*/
void Status::open(os_dynamic_area_no area)
{
    Status *ptr = head;

    while ((ptr) && (ptr->num != area)) ptr = ptr->next;
    if (ptr)
    {
        ptr->update();
        toolbox_show_object(0, ptr->win_id, toolbox_POSITION_DEFAULT,
                            NULL, 0, 0);
    }
    else ptr = new Status(area);
}

/*
    Parameters  : void
    Returns     : void
    Description : Update the details for the page status window.
*/
void Status::update(void)
{
    os_error *er;
    int size, limit;
    bits flags;
    char *name;
    os_box bbox;

    // Read the memory page size
    os_read_mem_map_info(&page_size, NULL);
    per_mb = 1024 * 1024 / page_size;
    mb_step = scale * scale * PAGES_STEP * (per_mb / PAGES_ACROSS) + PAGES_GAP;

    // Read the area details
    er = xosdynamicarea_read(num, &size, NULL, &flags, &limit, NULL, NULL,
                             &name);
    if (er)
    {
        // Area does not exist, so fade the icons and free any memory.
        if (status) delete[] status;
        status = NULL;
        pages = 0;
        displayfield_set_value(0, tool_id, STATUS_FIELD_NAME, "");
        gadget_set_flags(0, tool_id, STATUS_FIELD_NAME, gadget_FADED);

        // Reduce the window size and force the window to be redrawn
        window_get_extent(0, win_id, &bbox);
        wimp_force_redraw(window_get_wimp_handle(0, win_id),
                          bbox.x0, bbox.y0, bbox.x1, bbox.y1);
        bbox.y0 = bbox.y1;
        window_set_extent(0, win_id, &bbox);
        toolbox_show_object(0, win_id, toolbox_POSITION_DEFAULT, NULL, 0, 0);

        return;
    }

    // Set the dynamic area name
    displayfield_set_value(0, tool_id, STATUS_FIELD_NAME, name);
    gadget_set_flags(0, tool_id, STATUS_FIELD_NAME, 0);

    // Process the dynamic area size
    size /= page_size;
    limit /= page_size;
    if (pages != limit)
    {
        if (status) delete[] status;
        status = NULL;
        pages = limit;
    }
    if (!status) status = new byte[pages];

    // Get the page status
    er = xvirtualise_miscop_pages(num, 0, pages, status);
    if (er)
    {
        int i;
        for (i = 0; i < pages; i++)
        {
            status[i] = i < size ? virtualise_PAGE_USED_MAX
                                 : virtualise_PAGE_NONE;
        }
    }

    // Set the window size
    window_get_extent(0, win_id, &bbox);

    // Force the window to be redrawn
    wimp_force_redraw(window_get_wimp_handle(0, win_id),
                      bbox.x0, bbox.y0, bbox.x1, bbox.y1);

    // Modify the window size
    bbox.y0 = -mb_step * (1 + (pages - 1) / per_mb) - PAGES_GAP;
    window_set_extent(0, win_id, &bbox);

    // Force the window to be redrawn again
    wimp_force_redraw(window_get_wimp_handle(0, win_id),
                      bbox.x0, bbox.y0, bbox.x1, bbox.y1);
    toolbox_show_object(0, win_id, toolbox_POSITION_DEFAULT, NULL, 0, 0);
}

/*
    Parameters  : event_code    - The event number.
                  wimp_block    - The wimp poll block.
                  id_block      - The toolbox ID block.
                  handle        - An unused handle.
    Returns     : int           - Was the event claimed.
    Description : Handle wimp redraw requests.
*/
bool Status::handler_wimp_redraw(wimp_event_no event_code, wimp_block *block,
                                 toolbox_block *id_block, void *handle)
{
    Status *this_ptr = (Status *) handle;
    bool more;

    NOT_USED(event_code);
    NOT_USED(id_block);

    more = wimp_redraw_window(&block->redraw);
    while (more)
    {
        this_ptr->redraw(&block->redraw);
        more = wimp_get_rectangle(&block->redraw);
    }

    // Claim the event
    return TRUE;
}

/*
    Parameters  : block - Pointer to window redraw block.
    Returns     : void
    Description : Redraw the single rectangle within the redraw request block.
*/
void Status::redraw(wimp_draw *block)
{
    int xoff = block->box.x0 - block->xscroll;
    int yoff = block->box.y1 - block->yscroll;
    int y0 = block->clip.y0 - yoff;
    int y1 = block->clip.y1 - yoff;
    char str[20];
    int i, mb;
    int col_last, col_this;
    int pages_across = PAGES_ACROSS / scale;
    int pages_step = PAGES_STEP * scale;
    int pages_size = PAGES_SIZE * scale;

    // Exit if area does not exist
    if (!status) return;

    // Plot the memory axis text
    wimptextop_set_colour(os_COLOUR_BLACK, os_COLOUR_VERY_LIGHT_GREY);
    for (mb = 0; mb < 2 + (pages - 1) / per_mb; mb++)
    {
        int y = - mb_step * mb;

        if ((y0 < y + mb_step) && (y - mb_step < y1))
        {
            sprintf(str, "%iMB ", mb);
            wimptextop_paint(wimptextop_RJUSTIFY, str, xoff, y + yoff);
        }
    }

    // Do the actual plotting here
    col_last = -1;
    for (i = 0; i < pages; i++)
    {
        int x = (i % pages_across) * pages_step;
        int y = -(i / pages_across) * pages_step
                - PAGES_GAP * (i / per_mb);
        if ((y0 < y) && (y - pages_size < y1))
        {
            col_this = colours[status[i]];
            if (col_last != col_this) wimp_set_colour(col_last = col_this);
            os_plot(os_MOVE_TO, x + xoff, y + yoff);
            os_plot(os_PLOT_RECTANGLE | os_PLOT_BY,
                    pages_size - 2, -(pages_size - 2));
        }
        else i += pages_across - 1;
    }
}
