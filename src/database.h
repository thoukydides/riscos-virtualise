/*
    File        : database.h
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

#ifndef database_h
#define database_h

// Include oslib header files
#include "menu.h"
#include "toolbox.h"

// A dynamic area list window class
class Database
{
public:

    // The public part of the class

    /*
        Parameters  : menu  - The object ID of the menu in which the list
                              should appear.
        Returns     : -
        Description : Constructor function.
    */
    Database(toolbox_o menu);

    /*
        Parameters  : -
        Returns     : -
        Description : Destructor function.
    */
    ~Database(void);

    /*
        Parameters  : name  - The name of the dynamic area to add.
        Returns     : void
        Description : Update the dynamic area list.
    */
    static void add(const char *name);

    /*
        Parameters  : title - The modified dynamic area name to find.
                      conv  - A function to convert an original name into the
                              modified equivalent.
        Returns     : char  - Resulting name.
        Description : Try to restore a modified area name by matching within
                      the database.
    */
    static char *lookup(const char *title, void (* conv)(char *, const char *));

private:

    // The private part of the class

    toolbox_o menu_id;                  // Object ID of the menu to maintain
    static Database *head;              // Head of list of menus
    Database *next, *prev;              // Pointer to the next and previous menu

    /*
        Parameters  : name  - The dynamic area name.
                      comp  - The omponent ID for this entry.
                      at    - The component ID to add this after, or
                              menu_ADD_AT_START if this should be the first
                              entry.
        Returns     : void
        Description : Add a menu entry.
    */
    void add_item(const char *name, toolbox_c comp = toolbox_NULL_COMPONENT,
                  menu_add_at at = menu_ADD_AT_END);
};

#endif
