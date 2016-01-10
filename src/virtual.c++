/*
    File        : virtual.c++
    Date        : 23-Aug-99
    Author      : © A.Thoukydides, 1995, 1996, 1997, 1998, 1999, 2016
    Description : Interface to the Virtualise module.

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
#include "virtual.h"

// Include clib header files
#include "kernel.h"

/*
    Parameters  : limit     - The amount of logical address space, in bytes,
                              allocated for dynamic areas when -1 is passed to
                              OS_DynamicArea 0, or -1 to read current value.
                  claim     - Amount of memory to claim automatically, in
                              bytes, or -1 to read current value.
                  free      - Amount of memory to leave free, in bytes, or -1
                              to read current value.
                  rlimit    - Optional variable to receive address space limit
                              in bytes.
                  rclaim    - Optional variable to receive the amount of
                              memory to claim, in bytes.
                  rfree     - Optional variable to receive the amount of
                              memory to leave free, in bytes.
    Returns     : os_error  - Pointer to a standard error block.
    Description : Controls how much real memory to claim and leave free.
                  This calls XVirtualise_Configure.
*/
os_error *xvirtualise_configure(int limit, int claim, int free, int *rlimit,
                                int *rclaim, int *rfree)
{
    _kernel_oserror *er;
    _kernel_swi_regs regs;

    regs.r[0] = limit;
    regs.r[1] = claim;
    regs.r[2] = free;
    er = _kernel_swi(Virtualise_Configure, &regs, &regs);
    if (rlimit) *rlimit = regs.r[0];
    if (rclaim) *rclaim = regs.r[1];
    if (rfree) *rfree = regs.r[2];

    return (os_error *) er;
}

/*
    Parameters  : limit     - The amount of logical address space, in bytes,
                              allocated for dynamic areas when -1 is passed to
                              OS_DynamicArea 0, or -1 to read current value.
                  claim     - Amount of memory to claim automatically, in
                              bytes, or -1 to read current value.
                  free      - Amount of memory to leave free, in bytes, or -1
                              to read current value.
                  rlimit    - Optional variable to receive address space limit
                              in bytes.
                  rclaim    - Optional variable to receive the amount of
                              memory to claim, in bytes.
                  rfree     - Optional variable to receive the amount of
                              memory to leave free, in bytes.
    Returns     : void
    Description : Controls how much real memory to claim and leave free.
                  This calls Virtualise_Configure.
*/
void virtualise_configure(int limit, int claim, int free, int *rlimit,
                          int *rclaim, int *rfree)
{
    _kernel_swi_regs regs;

    regs.r[0] = limit;
    regs.r[1] = claim;
    regs.r[2] = free;
     _kernel_swi(Virtualise_Configure | _kernel_NONX, &regs, &regs);
    if (rlimit) *rlimit = regs.r[0];
    if (rclaim) *rclaim = regs.r[1];
    if (rfree) *rfree = regs.r[2];
}

/*
    Parameters  : area      - The dynamic area number.
                  max       - Optional maximum size of area (logical address
                              space), or -1 to use maximum size of the dynamic
                              area.
                  name      - Optional pathname to use for the swap file. If
                              this is a null pointer then a name is chosen
                              automatically.
    Returns     : os_error  - Pointer to a standard error block.
    Description : Start virtual memory in an existing dynamic area.
                  This calls SWI XVirtualise_Start.
*/
os_error *xvirtualise_start(os_dynamic_area_no area, int max,
                            const char *name)
{
    _kernel_swi_regs regs;

    regs.r[0] = area;
    regs.r[1] = max;
    regs.r[2] = (int) name;
    return (os_error *) _kernel_swi(Virtualise_Start, &regs, &regs);
}

/*
    Parameters  : area      - The dynamic area number.
                  max       - Optional maximum size of area (logical address
                              space), or -1 to use maximum size of the dynamic
                              area.
                  name      - Optional pathname to use for the swap file. If
                              this is a null pointer then a name is chosen
                              automatically.
    Returns     : void
    Description : Start virtual memory in an existing dynamic area.
                  This calls SWI Virtualise_Start.
*/
void virtualise_start(os_dynamic_area_no area, int max, const char *name)
{
    _kernel_swi_regs regs;

    regs.r[0] = area;
    regs.r[1] = max;
    regs.r[2] = (int) name;
    _kernel_swi(Virtualise_Start | _kernel_NONX, &regs, &regs);
}

/*
    Parameters  : area      - The dynamic area number.
    Returns     : os_error  - Pointer to a standard error block.
    Description : Disables virtual memory for a dynamic area.
                  This calls SWI XVirtualise_End.
*/
os_error *xvirtualise_end(os_dynamic_area_no area)
{
    _kernel_swi_regs regs;

    regs.r[0] = area;
    return (os_error *) _kernel_swi(Virtualise_End, &regs, &regs);
}

/*
    Parameters  : area      - The dynamic area number.
    Returns     : void
    Description : Disables virtual memory for a dynamic area.
                  This calls SWI Virtualise_End.
*/
void virtualise_end(os_dynamic_area_no area)
{
    _kernel_swi_regs regs;

    regs.r[0] = area;
    _kernel_swi(Virtualise_End | _kernel_NONX, &regs, &regs);
}

/*
    Parameters  : start     - Start of range of addresses to lock (inclusive).
                  end       - End of range of addresses to lock (excusive).
    Returns     : os_error  - Pointer to a standard error block.
    Description : Locks pages of a dynamic area for which virtual memory has
                  been enabled.
                  This calls SWI XVirtualise_Lock.
*/
os_error *xvirtualise_lock(int start, int end)
{
    _kernel_swi_regs regs;

    regs.r[0] = start;
    regs.r[1] = end;
    return (os_error *) _kernel_swi(Virtualise_Lock, &regs, &regs);
}

/*
    Parameters  : start     - Start of range of addresses to lock (inclusive).
                  end       - End of range of addresses to lock (excusive).
    Returns     : void
    Description : Locks pages of a dynamic area for which virtual memory has
                  been enabled.
                  This calls SWI Virtualise_Lock.
*/
void virtualise_lock(int start, int end)
{
    _kernel_swi_regs regs;

    regs.r[0] = start;
    regs.r[1] = end;
    _kernel_swi(Virtualise_Lock | _kernel_NONX, &regs, &regs);
}

/*
    Parameters  : start     - Start of range of addresses to unlock (inclusive).
                  end       - End of range of addresses to unlock (excusive).
    Returns     : os_error  - Pointer to a standard error block.
    Description : Unlock pages of a dynamic area previously locked using
                  virtualise_lock.
                  This calls SWI XVirtualise_Unlock.
*/
os_error *xvirtualise_unlock(int start, int end)
{
    _kernel_swi_regs regs;

    regs.r[0] = start;
    regs.r[1] = end;
    return (os_error *) _kernel_swi(Virtualise_Unlock, &regs, &regs);
}

/*
    Parameters  : start     - Start of range of addresses to unlock (inclusive).
                  end       - End of range of addresses to unlock (excusive).
    Returns     : void
    Description : Unlock pages of a dynamic area previously locked using
                  virtualise_lock.
                  This calls SWI Virtualise_Unlock.
*/
void virtualise_unlock(int start, int end)
{
    _kernel_swi_regs regs;

    regs.r[0] = start;
    regs.r[1] = end;
    _kernel_swi(Virtualise_Unlock | _kernel_NONX, &regs, &regs);
}

/*
    Parameters  : area      - The area number to check.
                  physical  - The physical size of the area in bytes.
    Returns     : os_error  - Pointer to a standard error block.
    Description : Read the physical size of a dynamic area.
                  This calls SWI XVirtualise_MiscOp with reason code
                  VirtualiseMiscOp_AreaPhysicalSize.
*/
os_error *xvirtualise_miscop_physical(os_dynamic_area_no area, int *physical)
{
    _kernel_oserror *er;
    _kernel_swi_regs regs;

    regs.r[0] = VirtualiseMiscOp_AreaPhysicalSize;
    regs.r[1] = area;
    er = _kernel_swi(Virtualise_MiscOp, &regs, &regs);
    if (physical) *physical = regs.r[1];

    return (os_error *) er;
}

/*
    Parameters  : area  - The area number to check.
    Returns     : int   - The physical size of the area in bytes.
    Description : Read the physical size of a dynamic area.
                  This calls SWI Virtualise_MiscOp with reason code
                  VirtualiseMiscOp_AreaPhysicalSize.
*/
int virtualise_miscop_physical(os_dynamic_area_no area)
{
    _kernel_swi_regs regs;

    regs.r[0] = VirtualiseMiscOp_AreaPhysicalSize;
    regs.r[1] = area;
     _kernel_swi(Virtualise_MiscOp | _kernel_NONX, &regs, &regs);

    return regs.r[1];
}

/*
    Parameters  : policy    - The replacement policy to use, or -1 to read.
                  rpolicy   - The returned replacement policy type.
    Returns     : os_error  - Pointer to a standard error block.
    Description : Set or read the page replacement policy.
                  This calls SWI XVirtualise_MiscOp with reason code
                  VirtualiseMiscOp_ReplacementPolicy.
*/
os_error *xvirtualise_miscop_policy(int policy, int *rpolicy)
{
    _kernel_oserror *er;
    _kernel_swi_regs regs;

    regs.r[0] = VirtualiseMiscOp_ReplacementPolicy;
    regs.r[1] = policy;
    er = _kernel_swi(Virtualise_MiscOp, &regs, &regs);
    if (rpolicy) *rpolicy = regs.r[1];

    return (os_error *) er;
}

/*
    Parameters  : policy    - The replacement policy to use, or -1 to read.
    Returns     : int       - The returned replacement policy.
    Description : Set or read the page replacement policy.
                  This calls SWI Virtualise_MiscOp with reason code
                  VirtualiseMiscOp_ReplacementPolicy.
*/
int virtualise_miscop_policy(int policy)
{
    _kernel_swi_regs regs;

    regs.r[0] = VirtualiseMiscOp_ReplacementPolicy;
    regs.r[1] = policy;
     _kernel_swi(Virtualise_MiscOp | _kernel_NONX, &regs, &regs);

    return regs.r[1];
}

/*
    Parameters  : type      - The class of instruction to return the number
                              of page faults for.
                  count     - The returned number of page faults.
    Returns     : os_error  - Pointer to a standard error block.
    Description : Read the number of serviced page faults caused by a
                  particular class of instruction.
                  This calls SWI XVirtualise_MiscOp with reason code
                  VirtualiseMiscOp_PageFaults.
*/
os_error *xvirtualise_miscop_faults(int type, int *count)
{
    _kernel_oserror *er;
    _kernel_swi_regs regs;

    regs.r[0] = VirtualiseMiscOp_PageFaults;
    regs.r[1] = type;
    er = _kernel_swi(Virtualise_MiscOp, &regs, &regs);
    if (count) *count = regs.r[1];

    return (os_error *) er;
}

/*
    Parameters  : type  - The class of instruction to return the number
                          of page faults for.
    Returns     : int   - The returned number of page faults.
    Description : Read the number of serviced page faults caused by a
                  particular class of instruction.
                  This calls SWI Virtualise_MiscOp with reason code
                  VirtualiseMiscOp_PageFaults.
*/
int virtualise_miscop_faults(int type)
{
    _kernel_swi_regs regs;

    regs.r[0] = VirtualiseMiscOp_PageFaults;
    regs.r[1] = type;
    _kernel_swi(Virtualise_MiscOp | _kernel_NONX, &regs, &regs);

    return regs.r[1];
}

/*
    Parameters  : area      - The number of the dynamic area to interrogate.
                  start     - Index of the first page to return details for.
                  end       - Index of page after the last one to process.
                  buffer    - Pointer to the buffer to receive the results.
    Returns     : os_error  - Pointer to a standard error block.
    Description : Find the status of the specified pages within a dynamic area.
                  On exit the buffer contains a single byte for each of the
                  pages specified indicating the status of that page.
                  This calls SWI XVirtualise_MiscOp with reason code
                  VirtualiseMiscOp_PageStatus.
*/
os_error *xvirtualise_miscop_pages(os_dynamic_area_no area,
                                   int start, int end, byte *buffer)
{
    _kernel_swi_regs regs;

    regs.r[0] = VirtualiseMiscOp_PageStatus;
    regs.r[1] = area;
    regs.r[2] = start;
    regs.r[3] = end;
    regs.r[4] = (int) buffer;
    return (os_error *) _kernel_swi(Virtualise_MiscOp, &regs, &regs);
}

/*
    Parameters  : area      - The number of the dynamic area to interrogate.
                  start     - Index of the first page to return details for.
                  end       - Index of page after the last one to process.
                  buffer    - Pointer to the buffer to receive the results.
    Returns     : void
    Description : Find the status of the specified pages within a dynamic area.
                  On exit the buffer contains a single byte for each of the
                  pages specified indicating the status of that page.
                  This calls SWI Virtualise_MiscOp with reason code
                  VirtualiseMiscOp_PageStatus.
*/
void virtualise_miscop_pages(os_dynamic_area_no area,
                             int start, int end, byte *buffer)
{
    _kernel_swi_regs regs;

    regs.r[0] = VirtualiseMiscOp_PageStatus;
    regs.r[1] = area;
    regs.r[2] = start;
    regs.r[3] = end;
    regs.r[4] = (int) buffer;
    _kernel_swi(Virtualise_MiscOp | _kernel_NONX, &regs, &regs);
}

/*
    Parameters  : task      - The handle of the task to suspend.
    Returns     : os_error  - Pointer to a standard error block.
    Description : Attempt to swap the specified task to disc.
                  This calls SWI XVirtualise_MiscOp with reason code
                  VirtualiseMiscOp_TaskFreeze.
*/
os_error *xvirtualise_miscop_freeze(wimp_t task)
{
    _kernel_swi_regs regs;

    regs.r[0] = VirtualiseMiscOp_TaskFreeze;
    regs.r[1] = (int) task;
    return (os_error *) _kernel_swi(Virtualise_MiscOp, &regs, &regs);
}

/*
    Parameters  : task      - The handle of the task to suspend.
    Returns     : void
    Description : Attempt to swap the specified task to disc.
                  This calls SWI Virtualise_MiscOp with reason code
                  VirtualiseMiscOp_TaskFreeze.
*/
void virtualise_miscop_freeze(wimp_t task)
{
    _kernel_swi_regs regs;

    regs.r[0] = VirtualiseMiscOp_TaskFreeze;
    regs.r[1] = (int) task;
    _kernel_swi(Virtualise_MiscOp | _kernel_NONX, &regs, &regs);
}

/*
    Parameters  : task      - The handle of the task to restart.
    Returns     : os_error  - Pointer to a standard error block.
    Description : Attempt to restore a task previously swapped to disc.
                  This calls SWI XVirtualise_MiscOp with reason code
                  VirtualiseMiscOp_TaskThaw.
*/
os_error *xvirtualise_miscop_thaw(wimp_t task)
{
    _kernel_swi_regs regs;

    regs.r[0] = VirtualiseMiscOp_TaskThaw;
    regs.r[1] = (int) task;
    return (os_error *) _kernel_swi(Virtualise_MiscOp, &regs, &regs);
}

/*
    Parameters  : task      - The handle of the task to restart.
    Returns     : void
    Description : Attempt to restore a task previously swapped to disc.
                  This calls SWI Virtualise_MiscOp with reason code
                  VirtualiseMiscOp_TaskThaw.
*/
void virtualise_miscop_thaw(wimp_t task)
{
    _kernel_swi_regs regs;

    regs.r[0] = VirtualiseMiscOp_TaskThaw;
    regs.r[1] = (int) task;
    _kernel_swi(Virtualise_MiscOp | _kernel_NONX, &regs, &regs);
}

/*
    Parameters  : task      - The handle of the task to check.
                  status    - Variable to receive the status of the task.
    Returns     : os_error  - Pointer to a standard error block.
    Description : Discover the status of a task.
                  This calls SWI XVirtualise_MiscOp with reason code
                  VirtualiseMiscOp_TaskStatus.
*/
os_error *xvirtualise_miscop_status(wimp_t task, int *status)
{
    _kernel_oserror *er;
    _kernel_swi_regs regs;

    regs.r[0] = VirtualiseMiscOp_TaskStatus;
    regs.r[1] = (int) task;
    er = _kernel_swi(Virtualise_MiscOp, &regs, &regs);
    if (status) *status = regs.r[0];

    return (os_error *) er;
}

/*
    Parameters  : task  - The handle of the task to check.
    Returns     : int   - Variable to receive the status of the task.
    Description : Discover the status of a task.
                  This calls SWI XVirtualise_MiscOp with reason code
                  VirtualiseMiscOp_TaskStatus.
*/
int virtualise_miscop_status(wimp_t task)
{
    _kernel_swi_regs regs;

    regs.r[0] = VirtualiseMiscOp_TaskStatus;
    regs.r[1] = (int) task;
    _kernel_swi(Virtualise_MiscOp | _kernel_NONX, &regs, &regs);

    return regs.r[0];
}
