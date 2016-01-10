/*
    File        : demo.h
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

#ifndef demo_h
#define demo_h

#ifdef DEMO

// Include oslib header files
#include "toolbox.h"

// A demo window class
class Demo
{
public:

    // The public part of the class

    /*
        Parameters  : win   - The object ID of the window in which the list
                              should appear.
        Returns     : -
        Description : Constructor function.
    */
    Demo(toolbox_o win);

    /*
        Parameters  : -
        Returns     : -
        Description : Destructor function.
    */
    ~Demo(void);

    /*
        Parameters  : void
        Returns     : void
        Description : Show the demo window.
    */
    void show(void);

private:

    // The private part of the class

    toolbox_o myid;                     // The object ID of the window
};

// A specific window object
extern Demo *demo_window;

#endif

#endif
