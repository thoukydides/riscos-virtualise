/*
    File        : main.c++
    Date        : 23-Aug-99
    Author      : © A.Thoukydides, 1995, 1996, 1997, 1998, 1999, 2016
    Description : The main !Virtualis program.

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

// Inlcude header file for this module
#include "main.h"

// Include clib header files
#include "kernel.h"
#include <setjmp.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Include oslib header files
#include "button.h"
#include "gadget.h"
#include "iconbar.h"
#include "proginfo.h"
#include "quit.h"
#include "wimp.h"
#include "wimpspriteop.h"
extern "C" {
#include "event.h"
}

// Include project header files
#include "automatic.h"
#include "configure.h"
#include "database.h"
#include "demo.h"
#include "faults.h"
#include "list.h"
#include "protection.h"
#include "tasks.h"

// The directory which contains the resource files
#define ResDirectory "<Virtualise$Dir>"

// Global quit flag
int quit = 0;

// Toolbox and wimp variables
char res_directory[256];                // GSTrans'd resource directory
toolbox_block id_block;                 // The toolbox event ID block
messagetrans_control_block message_block;// Toolbox message block
osspriteop_area *sprite_area;           // Sprite area pointer
int wimp_version;                       // The current wimp version
wimp_t task_handle;                     // Task handle for this task
char *task_name;                        // Name of the task
char *yes, *no;                         // Yes and no strings for icons
toolbox_o task_icon;                    // Object ID for the main iconbar icon
toolbox_o task_menu;                    // Object ID for the main iconbar menu

// Wimp messages of interest, or 0 for all
static wimp_message_list wimp_messages[] = {0};

// Array of toolbox events of interest, or 0 for all events
static toolbox_action_list toolbox_events[] = {0};

// The objects attached to windows
static DynamicAreaList *list_window = NULL;
static Configure *configure_window = NULL;
static PageFaults *faults_window = NULL;
static Database *auto_menu = NULL;
static AutomaticList *auto_window = NULL;
static TaskList *task_window = NULL;

// Time between polls
static int poll_interval = 100;

// Jump position for signal handling
static jmp_buf signal_jump;

// Flag to store whether an error is being handled
static sig_atomic_t handler_active;

// Error message for nested errors
static const os_error fatal_error = {0, "A serious error has occurred within the error handler. Application must quit immediately."};

/*
    Parameters  : dest  - Pointer to string in which to place result.
                  size  - The length of the destination string.
                  token - The token to lookup.
                  arg0  - The optional first parameter.
                  arg1  - The optional second parameter.
                  arg2  - The optional third parameter.
                  arg3  - The optional fourth parameter.
    Returns     : void
    Description : Lookup the specified token in the message file opened by
                  the toolbox, substituting up to four parameters.
*/
void lookup_token(char *dest, int size, const char *token, const char *arg0,
                  const char *arg1, const char *arg2, const char *arg3)
{
    messagetrans_lookup(&message_block, token, dest, size,
                        arg0, arg1, arg2, arg3, 0);
}

/*
    Parameters  : token - The token to lookup.
                  arg0  - The optional first parameter.
                  arg1  - The optional second parameter.
                  arg2  - The optional third parameter.
                  arg3  - The optional fourth parameter.
    Returns     : char  - Pointer to the token (in the RMA).
    Description : Lookup the specified token in the message file opened by
                  the toolbox, substituting up to four parameters.
*/
char *lookup_token(const char *token, const char *arg0, const char *arg1,
                   const char *arg2, const char *arg3)
{
    return messagetrans_lookup(&message_block, token, 0, 0,
                               arg0, arg1, arg2, arg3, 0);
}

/*
    Parameters  : message   - The wimp message.
                  handle    - An unused handle.
    Returns     : int       - Was the event claimed.
    Description : Handle quit wimp message events.
*/
bool handler_message_quit(wimp_message *message, void *handle)
{
    NOT_USED(message);
    NOT_USED(handle);

    // Set the global quit flag
    quit = 1;

    // Claim the event
    return TRUE;
}

/*
    Parameters  : event_code    - The event number.
                  action        - The toolbox event.
                  id_block      - The toolbox ID block.
                  handle        - An unused handle.
    Returns     : int           - Was the event claimed.
    Description : Handle quit toolbox events.
*/
bool handler_tb_quit(bits event_code, toolbox_action *action,
                     toolbox_block *id_block, void *handle)
{
    NOT_USED(event_code);
    NOT_USED(action);
    NOT_USED(id_block);
    NOT_USED(handle);

    // Set the global quit flag
    quit = 1;

    // Claim the event
    return TRUE;
}

/*
    Parameters  : event_code    - The event number.
                  action        - The toolbox event.
                  id_block      - The toolbox ID block.
                  handle        - An unused handle.
    Returns     : int           - Was the event claimed.
    Description : Remove handlers from a deleted list object.
*/
bool handler_tb_deleted_list(bits event_code, toolbox_action *action,
                             toolbox_block *id_block, void *handle)
{
    NOT_USED(event_code);
    NOT_USED(action);
    NOT_USED(id_block);
    NOT_USED(handle);

    // Remove the object
    delete list_window;

    // Claim the event
    return TRUE;
}

/*
    Parameters  : event_code    - The event number.
                  action        - The toolbox event.
                  id_block      - The toolbox ID block.
                  handle        - An unused handle.
    Returns     : int           - Was the event claimed.
    Description : Remove handlers from a deleted configuration object.
*/
bool handler_tb_deleted_configure(bits event_code, toolbox_action *action,
                                  toolbox_block *id_block, void *handle)
{
    NOT_USED(event_code);
    NOT_USED(action);
    NOT_USED(id_block);
    NOT_USED(handle);

    // Remove the object
    delete configure_window;

    // Claim the event
    return TRUE;
}

/*
    Parameters  : event_code    - The event number.
                  action        - The toolbox event.
                  id_block      - The toolbox ID block.
                  handle        - An unused handle.
    Returns     : int           - Was the event claimed.
    Description : Remove handlers from a deleted page faults object.
*/
bool handler_tb_deleted_faults(bits event_code, toolbox_action *action,
                               toolbox_block *id_block, void *handle)
{
    NOT_USED(event_code);
    NOT_USED(action);
    NOT_USED(id_block);
    NOT_USED(handle);

    // Remove the object
    delete faults_window;

    // Claim the event
    return TRUE;
}

/*
    Parameters  : event_code    - The event number.
                  action        - The toolbox event.
                  id_block      - The toolbox ID block.
                  handle        - An unused handle.
    Returns     : int           - Was the event claimed.
    Description : Remove handlers from a deleted database menu.
*/
bool handler_tb_deleted_data(bits event_code, toolbox_action *action,
                             toolbox_block *id_block, void *handle)
{
    NOT_USED(event_code);
    NOT_USED(action);
    NOT_USED(id_block);
    NOT_USED(handle);

    // Remove the object
    delete auto_menu;

    // Claim the event
    return TRUE;
}

/*
    Parameters  : event_code    - The event number.
                  action        - The toolbox event.
                  id_block      - The toolbox ID block.
                  handle        - An unused handle.
    Returns     : int           - Was the event claimed.
    Description : Remove handlers from a deleted automatic list window.
*/
bool handler_tb_deleted_auto(bits event_code, toolbox_action *action,
                             toolbox_block *id_block, void *handle)
{
    NOT_USED(event_code);
    NOT_USED(action);
    NOT_USED(id_block);
    NOT_USED(handle);

    // Remove the object
    delete auto_window;

    // Claim the event
    return TRUE;
}

/*
    Parameters  : event_code    - The event number.
                  action        - The toolbox event.
                  id_block      - The toolbox ID block.
                  handle        - An unused handle.
    Returns     : int           - Was the event claimed.
    Description : Remove handlers from a deleted task list window.
*/
bool handler_tb_deleted_task(bits event_code, toolbox_action *action,
                             toolbox_block *id_block, void *handle)
{
    NOT_USED(event_code);
    NOT_USED(action);
    NOT_USED(id_block);
    NOT_USED(handle);

    // Remove the object
    delete task_window;

    // Claim the event
    return TRUE;
}

#ifdef DEMO

/*
    Parameters  : event_code    - The event number.
                  action        - The toolbox event.
                  id_block      - The toolbox ID block.
                  handle        - An unused handle.
    Returns     : int           - Was the event claimed.
    Description : Remove handlers from a deleted demo object.
*/
bool handler_tb_deleted_demo(bits event_code, toolbox_action *action,
                             toolbox_block *id_block, void *handle)
{
    NOT_USED(event_code);
    NOT_USED(action);
    NOT_USED(id_block);
    NOT_USED(handle);

    // Remove the object
    delete demo_window;

    // Claim the event
    return TRUE;
}

#endif

/*
    Parameters  : event_code    - The event number.
                  action        - The toolbox event.
                  id_block      - The toolbox ID block.
                  handle        - An unused handle.
    Returns     : int           - Was the event claimed.
    Description : Attach handlers to an auto-created object.
*/
bool handler_tb_autocreated(bits event_code, toolbox_action *action,
                            toolbox_block *id_block, void *handle)
{
    NOT_USED(event_code);
    NOT_USED(handle);

    if (!strcmp(action->data.created.name, "AreasWin"))
    {
        // It is the dynamic area list window, so create the handling object
        list_window = new DynamicAreaList(id_block->this_obj);
        event_register_toolbox_handler(id_block->this_obj,
                                       action_OBJECT_DELETED,
                                       handler_tb_deleted_list, NULL);
    }
    else if (!strcmp(action->data.created.name, "ConfWin"))
    {
        // It is the configuration window, so create the handling object
        configure_window = new Configure(id_block->this_obj);
        event_register_toolbox_handler(id_block->this_obj,
                                       action_OBJECT_DELETED,
                                       handler_tb_deleted_configure, NULL);
    }
    else if (!strcmp(action->data.created.name, "FaultsWin"))
    {
        // It is the page faults window, so create the handling object
        faults_window = new PageFaults(id_block->this_obj);
        event_register_toolbox_handler(id_block->this_obj,
                                       action_OBJECT_DELETED,
                                       handler_tb_deleted_faults, NULL);
    }
    else if (!strcmp(action->data.created.name, "DataMenu"))
    {
        // It is the dynamic area database menu
        auto_menu = new Database(id_block->this_obj);
        event_register_toolbox_handler(id_block->this_obj,
                                       action_OBJECT_DELETED,
                                       handler_tb_deleted_data, NULL);
    }
    else if (!strcmp(action->data.created.name, "AutoWin"))
    {
        // It is the dynamic area database menu
        auto_window = new AutomaticList(id_block->this_obj);
        event_register_toolbox_handler(id_block->this_obj,
                                       action_OBJECT_DELETED,
                                       handler_tb_deleted_auto, NULL);
    }
    else if (!strcmp(action->data.created.name, "TaskWin"))
    {
        // It is the dynamic area database menu
        task_window = new TaskList(id_block->this_obj);
        event_register_toolbox_handler(id_block->this_obj,
                                       action_OBJECT_DELETED,
                                       handler_tb_deleted_task, NULL);
    }

#ifdef DEMO

    else if (!strcmp(action->data.created.name, "DemoWin"))
    {
        // It is the demo window, so create the handling object
        demo_window = new Demo(id_block->this_obj);
        event_register_toolbox_handler(id_block->this_obj,
                                       action_OBJECT_DELETED,
                                       handler_tb_deleted_demo, NULL);
    }

#endif

    // Do not claim the event - it is too generally useful
    return FALSE;
}

/*
    Parameters  : event_code    - The event number.
                  action        - The toolbox event.
                  id_block      - The toolbox ID block.
                  handle        - An unused handle.
    Returns     : int           - Was the event claimed.
    Description : Set the version number field of the ProgInfo window.
*/
bool handler_tb_proginfo(bits event_code, toolbox_action *action,
                         toolbox_block *id_block, void *handle)
{
    NOT_USED(event_code);
    NOT_USED(action);
    NOT_USED(handle);

    proginfo_set_version(0, id_block->this_obj, lookup_token("Version"));
    proginfo_set_licence_type(0, id_block->this_obj, REGISTER_LICENCE);

    // Claim the event
    return TRUE;
}

/*
    Parameters  : event_code    - The event number.
                  action        - The toolbox event.
                  id_block      - The toolbox ID block.
                  handle        - An unused handle.
    Returns     : int           - Was the event claimed.
    Description : Start the rest of the program.
*/
bool handler_tb_start(bits event_code, toolbox_action *action,
                      toolbox_block *id_block, void *handle)
{
    NOT_USED(event_code);
    NOT_USED(action);
    NOT_USED(id_block);
    NOT_USED(handle);

    // Create the iconbar icon
    task_icon = toolbox_create_object(0, (toolbox_id) "IBarIcon");
    task_menu = iconbar_get_menu(0, task_icon);

    // Do not claim the event
    return FALSE;
}

/*
    Parameters  : void
    Returns     : void
    Description : Initialise the toolbox system.
*/
void tbox_initialise(void)
{
    int used;

    // Reset the quit flag
    quit = 0;

    // GSTrans the resource directory name
    os_gs_trans(ResDirectory, res_directory, sizeof(res_directory), &used);
    res_directory[used] = 0;

    // Initialise event library
    event_initialise(&id_block);

    // Not interested in keypresses - toolbox handles key shortcuts
    event_set_mask(wimp_QUEUE_KEY);

    // Register message handlers for Quit
    event_register_message_handler(message_QUIT, handler_message_quit, NULL);

    // Register toolbox handlers
    event_register_toolbox_handler(event_ANY, action_QUIT_QUIT,
                                   handler_tb_quit, NULL);
    event_register_toolbox_handler(event_ANY, action_OBJECT_AUTO_CREATED,
                                   handler_tb_autocreated, NULL);
    event_register_toolbox_handler(event_ANY,
                                   action_PROG_INFO_ABOUT_TO_BE_SHOWN,
                                   handler_tb_proginfo, NULL);
    event_register_toolbox_handler(event_ANY, action_PROT_START,
                                   handler_tb_start, NULL);

    // All handlers setup, so initialise as a toolbox task
    task_handle = toolbox_initialise(0, WimpVersion, wimp_messages,
                                     toolbox_events, res_directory,
                                     &message_block, &id_block,
                                     &wimp_version, &sprite_area);

    // Get the name of the task to use with error reports
    task_name = lookup_token("_TaskName");

    // Translate other common strings
    yes = lookup_token("Yes:Yes");
    no = lookup_token("No:No");
}

/*
    Parameters  : sig   - The signal that was raised.
    Returns     : void
    Description : Signal handler.
*/
extern "C" void signal_handler(int sig)
{
    // Check if it is a nested error
    if (handler_active)
    {
        // Take panic action
        wimp_report_error(&fatal_error, wimp_ERROR_BOX_OK_ICON, "application");
        os_exit(&fatal_error, EXIT_FAILURE);
    }

    // Set flag to detect nested errors
    handler_active = TRUE;

    // Resume execution from just before the main program loop
    longjmp(signal_jump, sig);
}

/*
    Parameters  : sig   - The signal that was raised.
    Returns     : void
    Description : Display a suitable error for the signal that was raised,
                  and offer the user the opportunity to continue or quit the
                  program.
*/
void signal_error(int sig)
{
    os_error er;
    char msg[256];

    // Choose an appropriate error message
    switch (sig)
    {
        case SIGFPE:
            lookup_token(msg, sizeof(msg), "SigFPE");
            break;

        case SIGILL:
            lookup_token(msg, sizeof(msg), "SigIll");
            break;

        case SIGSEGV:
            lookup_token(msg, sizeof(msg), "SigSegV");
            break;

        case SIGSTAK:
            lookup_token(msg, sizeof(msg), "SigStack");
            break;

        case SIGOSERROR:
            {
                _kernel_oserror *ptr;
                ptr = _kernel_last_oserror();
                lookup_token(msg, sizeof(msg), "SigErr", ptr->errmess);
            }
            break;

        default:
            lookup_token(msg, sizeof(msg), "SigUnk");
            break;
    }

    // Construct the error block
    er.errnum = 0;
    lookup_token(er.errmess, sizeof(er.errmess), "SigTpl:%0", msg);

    // Display the error message
    if (wimp_report_error_by_category(&er, 0, task_name, AppSprite,
                                      wimpspriteop_AREA,
                                      lookup_token("SigBut:Continue,Quit"))
        != 3)
    {
        // Exit if the Quit option was selected
        xwimp_create_menu((wimp_menu *) -1, 0, 0);
        os_exit(&er, EXIT_FAILURE);
    }
}

/*
    Parameters  : argc  - The number of command line arguments.
                  argv  - The command line arguments.
    Returns     : int   - The return code.
    Description : The main program!
*/
int main(int argc, char *argv[])
{
    wimp_block poll_block;
    wimp_event_no event_code;
    os_t old_time, new_time;
    int sig;

    // Initialise as a toolbox task
    tbox_initialise();

    // Allow the rest of the program to start
    toolbox_action action;
    action.size = 16;
    action.action_no = action_PROT_START;
    action.flags = 0;
    toolbox_raise_toolbox_event(0, toolbox_NULL_OBJECT,
                                toolbox_NULL_COMPONENT, &action);

    // Set the jump point for signal handling
    sig = setjmp(signal_jump);
    if (sig) signal_error(sig);

    // Clear the nested handler flag
    handler_active = FALSE;

    // Install signal handlers
    signal(SIGFPE, signal_handler);
    signal(SIGILL, signal_handler);
    signal(SIGSEGV, signal_handler);
    signal(SIGSTAK, signal_handler);
    signal(SIGOSERROR, signal_handler);

    // Keep polling until quit flag is set
    old_time = os_read_monotonic_time();
    while (!quit)
    {
        event_poll_idle(&event_code, &poll_block, old_time, 0);
        if (event_code == wimp_NULL_REASON_CODE)
        {
            new_time = os_read_monotonic_time();
            while (0 <= new_time - old_time) old_time += poll_interval;
        }
    }

    // If the program gets this far then everything went alright
    return EXIT_SUCCESS;
}
