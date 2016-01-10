/*
    File        : configure.c++
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

// Include header file for this module
#include "configure.h"

// Inlcude clib header files
#include <string.h>

// Include c++lib header files
#include "fstream.h"

// Include oslib header files
#include "actionbutton.h"
#include "draggable.h"
#include "fileswitch.h"
#include "iconbar.h"
#include "macros.h"
#include "menu.h"
#include "numberrange.h"
#include "osfile.h"
#include "osfscontrol.h"
#include "optionbutton.h"
#include "osgbpb.h"
#include "radiobutton.h"
#include "stringset.h"
#include "wimpspriteop.h"
#include "writablefield.h"

// Include project header files
#include "main.h"
#include "virtual.h"

// Scale factor between value in number range icon and value in kB
#define SCALE_FACTOR_NUMERATOR (1024 * 1024)
#define SCALE_FACTOR_DENOMINATOR (10)

// Saved configuration file
#define CONFIGURATION_FILE ".Config"

// Filenames for module copy
#define MODULE_SRC ".Virtualise"
#define MODULE_DEST "<Choices$Write>.Boot.PreDesk.Virtualise"

/*
    Parameters  : win   - The object ID of the configuration window.
    Returns     : -
    Description : Constructor function.
*/
Configure::Configure(toolbox_o win)
{
    // Copy the window ID
    myid = win;

    // Obtain ID of the pane window
    window_get_tool_bars(window_TOOL_BAR_ITL, myid, NULL, &paneid, NULL, NULL);

    // Install handlers
    event_register_toolbox_handler(myid, action_ACTION_BUTTON_SELECTED,
                                   handler_tb_action, this);
    event_register_toolbox_handler(paneid, action_DRAGGABLE_DRAG_ENDED,
                                   handler_tb_drag, this);
    event_register_message_handler(message_DATA_SAVE_ACK, handler_message_save,
                                   this);
    event_register_message_handler(message_DATA_LOAD, handler_message_load,
                                   this);
    event_register_message_handler(message_SAVE_DESKTOP, handler_message_desk,
                                   this);

    // Read configuration file
    restore();
}

/*
    Parameters  : -
    Returns     : -
    Description : Destructor function.
*/
Configure::~Configure(void)
{
    // Deinstall handlers
    event_deregister_message_handler(message_SAVE_DESKTOP, handler_message_desk,
                                     this);
    event_deregister_message_handler(message_DATA_LOAD, handler_message_load,
                                     this);
    event_deregister_message_handler(message_DATA_SAVE_ACK,
                                     handler_message_save, this);
    event_deregister_toolbox_handler(paneid, action_DRAGGABLE_DRAG_ENDED,
                                     handler_tb_drag, this);
    event_deregister_toolbox_handler(myid, action_ACTION_BUTTON_SELECTED,
                                     handler_tb_action, this);
}

/*
    Parameters  : event_code    - The event number.
                  action        - The toolbox event.
                  id_block      - The toolbox ID block.
                  handle        - An unused handle.
    Returns     : int           - Was the event claimed.
    Description : Handle menu about to be shown toolbox events.
*/
bool Configure::handler_tb_action(bits event_code, toolbox_action *action,
                                  toolbox_block *id_block, void *handle)
{
    Configure *this_ptr = (Configure *) handle;

    NOT_USED(event_code);
    NOT_USED(action);

    // Pass on to correct routine
    if (id_block->this_cmp == CONFIGURE_ICON_SET) this_ptr->set();
    else if (id_block->this_cmp == CONFIGURE_ICON_READ) this_ptr->read();
    else if (id_block->this_cmp == CONFIGURE_ICON_SAVE) this_ptr->save();

    // Claim event
    return TRUE;
}

/*
    Parameters  : event_code    - The event number.
                  action        - The toolbox event.
                  id_block      - The toolbox ID block.
                  handle        - An unused handle.
    Returns     : int           - Was the event claimed.
    Description : Handle draggable object drag ended toolbox events.
*/
bool Configure::handler_tb_drag(bits event_code, toolbox_action *action,
                                toolbox_block *id_block, void *handle)
{
    Configure *this_ptr = (Configure *) handle;
    draggable_action_drag_ended *drag = (draggable_action_drag_ended *)
                                         &(action->data);
    wimp_message msg;

    NOT_USED(event_code);
    NOT_USED(id_block);

    // Send message to find pathname
    msg.size = sizeof(wimp_message);
    msg.your_ref = 0;
    msg.action = message_DATA_SAVE;
    msg.data.data_xfer.w = drag->ids.wimp.w;
    msg.data.data_xfer.i = drag->ids.wimp.i;
    msg.data.data_xfer.pos = drag->pos;
    msg.data.data_xfer.est_size = 0;
    msg.data.data_xfer.file_type = 0x1000;
    msg.data.data_xfer.file_name[0] = 0;
    wimp_send_message(wimp_USER_MESSAGE_RECORDED, &msg,
                      (wimp_t) drag->ids.wimp.w);

    // Claim event
    return TRUE;
}

/*
    Parameters  : message   - The wimp message.
                  handle    - An unused handle.
    Returns     : int       - Was the event claimed.
    Description : Handle SaveDesktop wimp message events.
*/
bool Configure::handler_message_desk(wimp_message *message, void *handle)
{
    Configure *this_ptr = (Configure *) handle;

    // Run res_directory
    osgbpb_write(message->data.save_desktop.file, (byte *) "Run ", 4);
    osgbpb_write(message->data.save_desktop.file, (byte *) res_directory,
                 strlen(res_directory));
    os_bput(os_VDU_LINE_FEED, message->data.save_desktop.file);

    // Claim the event
    return TRUE;
}

/*
    Parameters  : message   - The wimp message.
                  handle    - An unused handle.
    Returns     : int       - Was the event claimed.
    Description : Handle DataSaveAck wimp message events.
*/
bool Configure::handler_message_save(wimp_message *message, void *handle)
{
    Configure *this_ptr = (Configure *) handle;

    if (message->data.data_xfer.est_size != -1)
    {
        int len = strlen(message->data.data_xfer.file_name);
        if (0 < len) message->data.data_xfer.file_name[len - 1] = 0;
        writablefield_set_value(0, this_ptr->paneid, CONFIGURE_ICON_SWAP_WRITE,
                                message->data.data_xfer.file_name);
    }

    // Claim the event
    return TRUE;
}

/*
    Parameters  : message   - The wimp message.
                  handle    - An unused handle.
    Returns     : int       - Was the event claimed.
    Description : Handle DataLoad wimp message events.
*/
bool Configure::handler_message_load(wimp_message *message, void *handle)
{
    Configure *this_ptr = (Configure *) handle;

    // Check which window it was aimed at
    if ((message->data.data_xfer.w
         != window_get_wimp_handle(0, this_ptr->myid))
        && (message->data.data_xfer.w
            != window_get_wimp_handle(0, this_ptr->paneid)))
    {
        // Not the right window
        return FALSE;
    }

    if ((message->data.data_xfer.file_type != 0x1000)
        && (message->data.data_xfer.file_type != 0x2000))
    {
        int len = strlen(message->data.data_xfer.file_name);
        while ((0 < len) && (message->data.data_xfer.file_name[len] != '.'))
        {
            len--;
        }
        message->data.data_xfer.file_name[len] = 0;
    }
    writablefield_set_value(0, this_ptr->paneid, CONFIGURE_ICON_SWAP_WRITE,
                            message->data.data_xfer.file_name);

    // Claim the event
    return TRUE;
}

/*
    Parameters  : void
    Returns     : void
    Description : Restore the values from the configuration file.
*/
void Configure::restore(void)
{
    char path[256];
    int limit, claim, leave, policy, reads, writes;
    int keep, disc, select, adjust;
    int purge;
    fileswitch_object_type file_type;

    // Default some values
    adjust_op = 2;
    select_op = 0;
    purge_state = TRUE;

    // Check if the module is loaded automatically
    if (!xosfile_read_stamped_no_path(MODULE_DEST, &file_type, NULL, NULL,
                                      NULL, NULL, NULL))
    {
        radiobutton_set_state(0, paneid,
                              file_type == fileswitch_IS_FILE
                              ? CONFIGURE_ICON_LOAD_BOOT
                              : CONFIGURE_ICON_LOAD_FRONTEND, TRUE);
    }

    // Open configuration file
    ifstream file(CONFIGURATION_DIRECTORY CONFIGURATION_FILE);
    if (file.bad()) return;

    // Read the current settings from the configuration file
    if (file >> path >> limit >> claim >> leave >> policy >> reads >> writes)
    {
        // Set the values for the icons
        writablefield_set_value(0, paneid, CONFIGURE_ICON_SWAP_WRITE, path);
        numberrange_set_value(0, paneid, CONFIGURE_ICON_LIMIT, limit);
        numberrange_set_value(0, paneid, CONFIGURE_ICON_CLAIM, claim);
        numberrange_set_value(0, paneid, CONFIGURE_ICON_LEAVE, leave);
        stringsetsetselected_index(0, paneid, CONFIGURE_ICON_POLICY, policy);
        optionbutton_set_state(0, paneid, CONFIGURE_ICON_MULTI_READS, reads);
        optionbutton_set_state(0, paneid, CONFIGURE_ICON_MULTI_WRITES, writes);
    }
    else return;

    // Read the new configuration items
    if (file >> keep >> disc >> select >> adjust)
    {
        // Set the values for the icons
        numberrange_set_value(0, paneid, CONFIGURE_ICON_KEEP, keep);
        numberrange_set_value(0, paneid, CONFIGURE_ICON_DISC, disc);
        stringsetsetselected_index(0, paneid, CONFIGURE_ICON_SELECT, select);
        stringsetsetselected_index(0, paneid, CONFIGURE_ICON_ADJUST, adjust);
        select_op = select;
        adjust_op = adjust;
    }

    // Read the even newer configuration item
    if (file >> purge)
    {
        // Set the values for the icons
        optionbutton_set_state(0, paneid, CONFIGURE_ICON_SWAP_PURGE, purge);
        purge_state = purge;
    }

    // Send the new values to the module
    set();
}

/*
    Parameters  : void
    Returns     : void
    Description : Save the values currently specified.
*/
void Configure::save(void)
{
    char path[256];

    // Ensure that the directory exists
    xosfile_create_dir(CONFIGURATION_DIRECTORY, 0);

    // Open configuration file
    ofstream file(CONFIGURATION_DIRECTORY CONFIGURATION_FILE);
    if (file.bad()) return;

    // Write the current settings to the configuration file
    writablefield_get_value(0, paneid, CONFIGURE_ICON_SWAP_WRITE, path, 256);
    file << path << '\n'
         << numberrange_get_value(0, paneid, CONFIGURE_ICON_LIMIT) << ' '
         << numberrange_get_value(0, paneid, CONFIGURE_ICON_CLAIM) << ' '
         << numberrange_get_value(0, paneid, CONFIGURE_ICON_LEAVE) << '\n'
         << stringsetgetselected_index(0, paneid, CONFIGURE_ICON_POLICY) << ' '
         << optionbutton_get_state(0, paneid, CONFIGURE_ICON_MULTI_READS) << ' '
         << optionbutton_get_state(0, paneid, CONFIGURE_ICON_MULTI_WRITES)
         << '\n'
         << numberrange_get_value(0, paneid, CONFIGURE_ICON_KEEP) << ' '
         << numberrange_get_value(0, paneid, CONFIGURE_ICON_DISC) << '\n'
         << stringsetgetselected_index(0, paneid, CONFIGURE_ICON_SELECT) << ' '
         << stringsetgetselected_index(0, paneid, CONFIGURE_ICON_ADJUST)
         << '\n'
         << optionbutton_get_state(0, paneid, CONFIGURE_ICON_SWAP_PURGE)
         << '\n';
}

/*
    Parameters  : void
    Returns     : void
    Description : Set the values currently specified.
*/
void Configure::set(void)
{
    os_error *er;
    char path[256];

    select_op = stringsetgetselected_index(0, paneid, CONFIGURE_ICON_SELECT);
    adjust_op = stringsetgetselected_index(0, paneid, CONFIGURE_ICON_ADJUST);
    iconbar_set_show(iconbar_SELECT | iconbar_ADJUST, task_icon,
                     menu_get_click_show(0, task_menu, select_op, NULL),
                     menu_get_click_show(0, task_menu, adjust_op, NULL));

    purge_state = optionbutton_get_state(0, paneid, CONFIGURE_ICON_SWAP_PURGE);

    if (radiobutton_get_state(0, paneid, CONFIGURE_ICON_LOAD_BOOT, NULL))
    {
        char name[256];

        // Construct source file name
        strcpy(name, res_directory);
        strcat(name, MODULE_SRC);

        // Attempt to install the module
        er = xosfscontrol_copy(name, MODULE_DEST,
                               osfscontrol_COPY_FORCE
                               | osfscontrol_COPY_NEWER, 0, 0, 0, 0, NULL);
    }
    else
    {
        // Delete the module from the boot directory
        xosfscontrol_wipe(MODULE_DEST, osfscontrol_WIPE_FORCE, 0, 0, 0, 0);
    }

    writablefield_get_value(0, paneid, CONFIGURE_ICON_SWAP_WRITE, path, 256);
    os_set_var_val(virtualise_SWAP_VARIABLE, (byte *) path, strlen(path), 0,
                   os_VARTYPE_MACRO, NULL);

    er = xvirtualise_configure((int)(((unsigned) numberrange_get_value(0,
                                      paneid,
                                      CONFIGURE_ICON_LIMIT)
                                      * SCALE_FACTOR_NUMERATOR)
                                     / SCALE_FACTOR_DENOMINATOR),
                               (int)(((unsigned) numberrange_get_value(0,
                                     paneid,
                                     CONFIGURE_ICON_CLAIM)
                                     * SCALE_FACTOR_NUMERATOR)
                                     / SCALE_FACTOR_DENOMINATOR),
                               (int)(((unsigned) numberrange_get_value(0,
                                     paneid,
                                     CONFIGURE_ICON_LEAVE)
                                     * SCALE_FACTOR_NUMERATOR)
                                     / SCALE_FACTOR_DENOMINATOR));
    if (er)
    {
        er->errnum = 0;
        wimp_report_error_by_category(er, wimp_ERROR_BOX_CATEGORY_INFO << 9,
                                      task_name, AppSprite,
                                      wimpspriteop_AREA, 0);
        return;
    }

    er = xvirtualise_miscop_policy(stringsetgetselected_index(0, paneid,
                                                       CONFIGURE_ICON_POLICY)
                                   | (optionbutton_get_state(0, paneid,
                                        CONFIGURE_ICON_MULTI_READS)
                                        ? virtualise_POLICY_FLAGS_READS : 0)
                                   | (optionbutton_get_state(0, paneid,
                                        CONFIGURE_ICON_MULTI_WRITES)
                                        ? virtualise_POLICY_FLAGS_WRITES : 0));
    if (er)
    {
        er->errnum = 0;
        wimp_report_error_by_category(er, wimp_ERROR_BOX_CATEGORY_INFO << 9,
                                      task_name, AppSprite,
                                      wimpspriteop_AREA, 0);
        return;
    }

    er = xvirtualise_user_configure(-1, -1, -1,
                                    (int)(((unsigned)
                                           numberrange_get_value(0, paneid,
                                           CONFIGURE_ICON_KEEP)
                                           * SCALE_FACTOR_NUMERATOR)
                                           / SCALE_FACTOR_DENOMINATOR),
                                    numberrange_get_value(0, paneid,
                                           CONFIGURE_ICON_DISC)
                                    * 1024 * 1024);
    if (er)
    {
        er->errnum = 0;
        wimp_report_error_by_category(er, wimp_ERROR_BOX_CATEGORY_INFO << 9,
                                      task_name, AppSprite,
                                      wimpspriteop_AREA, 0);
        return;
    }

    read();
}

/*
    Parameters  : void
    Returns     : void
    Description : Read the current settings, and update the number range fields
                  appropriately.
*/
void Configure::read(void)
{
    os_error *er;
    fileswitch_object_type file_type;
    int used, policy, limit, claim, leave, keep, disc;
    os_var_type type;
    char path[256];

    stringsetsetselected_index(0, paneid, CONFIGURE_ICON_SELECT, select_op);
    stringsetsetselected_index(0, paneid, CONFIGURE_ICON_ADJUST, adjust_op);
    optionbutton_set_state(0, paneid, CONFIGURE_ICON_SWAP_PURGE, purge_state);

    er = xosfile_read_stamped_no_path(MODULE_DEST, &file_type, NULL, NULL,
                                      NULL, NULL, NULL);
    if (er)
    {
        er->errnum = 0;
        wimp_report_error_by_category(er, wimp_ERROR_BOX_CATEGORY_INFO << 9,
                                      task_name, AppSprite,
                                      wimpspriteop_AREA, 0);
        return;
    }
    radiobutton_set_state(0, paneid,
                          file_type == fileswitch_IS_FILE
                          ? CONFIGURE_ICON_LOAD_BOOT
                          : CONFIGURE_ICON_LOAD_FRONTEND, TRUE);

    er = xos_read_var_val(virtualise_SWAP_VARIABLE, path, 256, 0,
                          os_VARTYPE_STRING, &used, NULL, &type);
    if (!er)
    {
        path[used] = 0;
        writablefield_set_value(0, paneid, CONFIGURE_ICON_SWAP_WRITE, path);
    }

    er = xvirtualise_configure(-1, -1, -1, &limit, &claim, &leave);
    if (er)
    {
        er->errnum = 0;
        wimp_report_error_by_category(er, wimp_ERROR_BOX_CATEGORY_INFO << 9,
                                      task_name, AppSprite,
                                      wimpspriteop_AREA, 0);
        return;
    }
    numberrange_set_value(0, paneid, CONFIGURE_ICON_LIMIT,
                          (int)(((unsigned) limit * SCALE_FACTOR_DENOMINATOR
                                 + SCALE_FACTOR_NUMERATOR / 2)
                                / SCALE_FACTOR_NUMERATOR));
    numberrange_set_value(0, paneid, CONFIGURE_ICON_CLAIM,
                          (int)(((unsigned) claim * SCALE_FACTOR_DENOMINATOR
                                 + SCALE_FACTOR_NUMERATOR / 2)
                                / SCALE_FACTOR_NUMERATOR));
    numberrange_set_value(0, paneid, CONFIGURE_ICON_LEAVE,
                          (int)(((unsigned) leave * SCALE_FACTOR_DENOMINATOR
                                 + SCALE_FACTOR_NUMERATOR / 2)
                                / SCALE_FACTOR_NUMERATOR));
    er = xvirtualise_miscop_policy(-1, &policy);
    if (er)
    {
        er->errnum = 0;
        wimp_report_error_by_category(er, wimp_ERROR_BOX_CATEGORY_INFO << 9,
                                      task_name, AppSprite,
                                      wimpspriteop_AREA, 0);
        return;
    }
    stringsetsetselected_index(0, paneid, CONFIGURE_ICON_POLICY,
                               policy & ~virtualise_POLICY_FLAGS_MASK);
    optionbutton_set_state(0, paneid, CONFIGURE_ICON_MULTI_READS,
                           BOOL(policy & virtualise_POLICY_FLAGS_READS));
    optionbutton_set_state(0, paneid, CONFIGURE_ICON_MULTI_WRITES,
                           BOOL(policy & virtualise_POLICY_FLAGS_WRITES));

    er = xvirtualise_user_configure(-1, -1, -1, -1, -1,
                                    NULL, NULL, NULL, &keep, &disc);
    if (er)
    {
        er->errnum = 0;
        wimp_report_error_by_category(er, wimp_ERROR_BOX_CATEGORY_INFO << 9,
                                      task_name, AppSprite,
                                      wimpspriteop_AREA, 0);
        return;
    }
    numberrange_set_value(0, paneid, CONFIGURE_ICON_KEEP,
                          (int)(((unsigned) keep * SCALE_FACTOR_DENOMINATOR
                                 + SCALE_FACTOR_NUMERATOR / 2)
                                / SCALE_FACTOR_NUMERATOR));
    numberrange_set_value(0, paneid, CONFIGURE_ICON_DISC,
                          disc / (1024 * 1024));
}
