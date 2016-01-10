/*
    File        : demo.c++
    Date        : 23-Aug-99
    Author      : © A.Thoukydides, 1995, 1996, 1997, 1998, 1999, 2016
    Description : A demo version window.

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

#ifdef DEMO

// Include header file for this module
#include "demo.h"

Demo *demo_window;

/*
    Parameters  : win   - The object ID of the window in which the list
                          should appear.
    Returns     : -
    Description : Constructor function.
*/
Demo::Demo(toolbox_o win)
{
    // Copy the window ID
    myid = win;
}

/*
    Parameters  : -
    Returns     : -
    Description : Destructor function.
*/
Demo::~Demo(void)
{
}

/*
    Parameters  : void
    Returns     : void
    Description : Show the demo window.
*/
void Demo::show(void)
{
    toolbox_show_object(toolbox_SHOW_AS_MENU, myid, toolbox_POSITION_DEFAULT,
                        NULL, toolbox_NULL_OBJECT, toolbox_NULL_COMPONENT);
}

#endif
