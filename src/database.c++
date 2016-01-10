/*
    File        : database.c++
    Date        : 23-Aug-99
    Author      : © A.Thoukydides, 1996, 1997, 1998, 1999, 2016
    Description : A database and menu of all prior dynamic areas.

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
#include "database.h"

// Include clib header files
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

// Include c++lib header files
#include "fstream.h"

// Include oslib header files
#include "osfile.h"

// Include project header files
#include "configure.h"

// Saved database file
#define DATABASE_FILE ".Database"

// Data structure for list of dynamic areas
struct DatabaseAreas
{
    char *area;                         // Name of this dynamic area
    static DatabaseAreas *head;         // Head of list
    DatabaseAreas *next, *prev;         // Next and previous entries
    toolbox_c comp;                     // Component ID to use
    static toolbox_c index;             // Current component ID

    /*
        Parameters  : name  - The name of the dynamic area to add.
        Returns     : -
        Description : Constructor function.
    */
    DatabaseAreas(const char *name)
    {
        // Allocate space for the area name
        area = new char[strlen(name) + 1];

        // Copy the area name
        strcpy(area, name);

        // Set the component ID
        comp = index++;

        // Find where this entry should be added
        prev = NULL;
        next = head;
        while (next && (strcmp(next->area, name) < 0))
        {
            prev = next;
            next = next->next;
        }

        // Add this entry to the list
        if (prev) prev->next = this;
        else head = this;
        if (next) next->prev = this;
    }

    /*
        Parameters  : void
        Returns     : -
        Description : Destructor function.
    */
    ~DatabaseAreas(void)
    {
        // Release the memory claimed
        delete[] area;

        // Remove this entry from the list
        if (prev) prev->next = next;
        else head = next;
        if (next) next->prev = this;
    }

    /*
        Parameters  : name  - The dynamic area name to check.
        Returns     : int   - Is the specified name present.
        Description : Check whether a particular dynamic area name is in the
                      database.
    */
    static int check(const char *name)
    {
        DatabaseAreas *ptr = head;

        while (ptr && strcmp(ptr->area, name)) ptr = ptr->next;

        return ptr != NULL;
    }
};

// The head of the list of areas
DatabaseAreas *DatabaseAreas::head = NULL;

// Current component ID
toolbox_c DatabaseAreas::index = 1;

// The head of the list of menus
Database *Database::head = NULL;

// A flag to tell whether the database has been loaded
static int loaded = FALSE;

/*
    Parameters  : void
    Returns     : void
    Description : Save the dynamic area database.
*/
static void save(void)
{
    // Ensure that the directory exists
    xosfile_create_dir(CONFIGURATION_DIRECTORY, 0);

    // Open the database file
    ofstream file(CONFIGURATION_DIRECTORY DATABASE_FILE);
    if (!file.bad())
    {
        // Write the database
        DatabaseAreas *ptr = DatabaseAreas::head;

        while (ptr)
        {
            file << ptr->area << '\n';
            ptr = ptr->next;
        }
    }
}

/*
    Parameters  : menu  - The object ID of the menu in which the list
                          should appear.
    Returns     : -
    Description : Constructor function.
*/
Database::Database(toolbox_o menu)
{
    // Store the object ID for the menu
    menu_id = menu;

    // Add this object to the list
    prev = NULL;
    next = head;
    head = this;
    if (next) next->prev = this;

    // If this is the first one then load the database
    if (!loaded)
    {
        // Set the database loaded flag
        loaded = TRUE;

        // Load the database file
        ifstream file(CONFIGURATION_DIRECTORY DATABASE_FILE);
        if (!file.bad())
        {
            char name[256], c;

            // Read the database
            while (file.get(name, sizeof(name)) && file.get(c) && c == '\n')
            {
                if (name[0]) Database::add(name);
            }
        }
    }

    // Install an exit handler
    atexit(save);
}

/*
    Parameters  : -
    Returns     : -
    Description : Destructor function.
*/
Database::~Database(void)
{
    // Remove this object from the list
    if (prev) prev->next = next;
    else head = next;
    if (next) next->prev = prev;
}

/*
    Parameters  : name  - The name of the dynamic area to add.
    Returns     : void
    Description : Update the dynamic area list.
*/
void Database::add(const char *name)
{
    // Check if it is in the database
    if (!DatabaseAreas::check(name))
    {
        // Add it to the database
        DatabaseAreas *item = new DatabaseAreas(name);

        // For each menu, add a suitable item
        Database *ptr = head;
        while (ptr)
        {
            // Add the item to this menu
            ptr->add_item(item->area, item->comp,
                          item->prev ? item->prev->comp : menu_ADD_AT_START);

            // Move onto the next menu
            ptr = ptr->next;
        }
    }
}

/*
    Parameters  : name  - The dynamic area name.
                  comp  - The omponent ID for this entry.
                  at    - The component ID to add this after, or
                          menu_ADD_AT_START if this should be the first entry.
    Returns     : void
    Description : Add a menu entry.
*/
void Database::add_item(const char *name, toolbox_c comp, menu_add_at at)
{
    menu_entry_object entry;

    entry.flags = 0;
    entry.cmp = comp;
    entry.text = (char *) name;
    entry.text_limit = strlen(name) + 1;
    entry.click_object_name = NULL;
    entry.sub_menu_object_name = NULL;
    entry.sub_menu_action = 0;
    entry.click_action = 0;
    entry.help = NULL;
    entry.help_limit = 0;

    menu_add_entry(0, menu_id, at, &entry);
}

/*
    Parameters  : title - The modified dynamic area name to find.
                  conv  - A function to convert an original name into the
                          modified equivalent.
    Returns     : char  - Resulting name.
    Description : Try to restore a modified area name by matching within
                  the database.
*/
char *Database::lookup(const char *title, void (* conv)(char *, const char *))
{
    DatabaseAreas *ptr = DatabaseAreas::head;
    char name[256];
    int i;

    while (ptr)
    {
        conv(name, ptr->area);
        i = 0;
        while (name[i] && title[i] && (tolower(name[i]) == tolower(title[i])))
        {
            i++;
        }
        if (!name[i] && !title[i]) return ptr->area;
        ptr = ptr->next;
    }
    return NULL;
}
