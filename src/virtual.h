/*
    File        : virtual.h
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

#ifndef virtual_h
#define virtual_h

// Include oslib header files
#include "os.h"
#include "wimp.h"

// Extra flag bit for OS_DynamicArea to indicate that virtual memory is active
#define os_AREA_VIRTUALISED ((os_area_flags) (1<<31))

// Name of swap file directory variable
#define virtualise_SWAP_VARIABLE "Virtualise$SwapDir"

// SWI names and numbers
#define Virtualise_Configure 0x4B6C0
#define Virtualise_Start 0x4B6C1
#define Virtualise_End 0x4B6C2
#define Virtualise_Lock 0x4B6C3
#define Virtualise_Unlock 0x4B6C4
#define Virtualise_MiscOp 0x4B6C5
#define Virtualise_UserConfigure 0x4B6C6

// Reason codes for Virtualise_MiscOp
#define VirtualiseMiscOp_AreaPhysicalSize 0x0
#define VirtualiseMiscOp_ReplacementPolicy 0x1
#define VirtualiseMiscOp_PageFaults 0x2
#define VirtualiseMiscOp_PageStatus 0x3
#define VirtualiseMiscOp_TaskFreeze 0x4
#define VirtualiseMiscOp_TaskThaw 0x5
#define VirtualiseMiscOp_TaskStatus 0x6

// Page replacement policies and flags
#define virtualise_POLICY_NFU 0x0
#define virtualise_POLICY_FIFO 0x1
#define virtualise_POLICY_RANDOM 0x2
#define virtualise_POLICY_FLAGS_MASK 0xFF000000
#define virtualise_POLICY_FLAGS_READS 0x01000000
#define virtualise_POLICY_FLAGS_WRITES 0x02000000

// Page fault classes
#define virtualise_FAULT_LDR 0x0
#define virtualise_FAULT_STR 0x1
#define virtualise_FAULT_LDM 0x2
#define virtualise_FAULT_STM 0x3
#define virtualise_FAULT_LDC 0x4
#define virtualise_FAULT_STC 0x5
#define virtualise_FAULT_SWP 0x6

// Possible status of a page of virtual memory
#define virtualise_PAGE_USED_MIN 0x00
#define virtualise_PAGE_USED_MAX 0x0F
#define virtualise_PAGE_NONE 0x10
#define virtualise_PAGE_UNUSED 0x11
#define virtualise_PAGE_DISC 0x12
#define virtualise_PAGE_LOCKED 0x13
#define virtualise_PAGE_NUM 0x14

#ifdef __cplusplus
extern "C" {
#endif

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
os_error *xvirtualise_configure(int limit, int claim = -1, int free = -1,
                                int *rlimit = NULL, int *rclaim = NULL,
                                int *rfree = NULL);

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
void virtualise_configure(int limit, int claim = -1, int free = -1,
                          int *rlimit = NULL, int *rclaim = NULL,
                          int *rfree = NULL);

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
os_error *xvirtualise_start(os_dynamic_area_no area, int max = -1,
                            const char *name = 0);

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
void virtualise_start(os_dynamic_area_no area, int max = -1,
                      const char *name = 0);

/*
    Parameters  : area      - The dynamic area number.
    Returns     : os_error  - Pointer to a standard error block.
    Description : Disables virtual memory for a dynamic area.
                  This calls SWI XVirtualise_End.
*/
os_error *xvirtualise_end(os_dynamic_area_no area);

/*
    Parameters  : area      - The dynamic area number.
    Returns     : void
    Description : Disables virtual memory for a dynamic area.
                  This calls SWI Virtualise_End.
*/
void virtualise_end(os_dynamic_area_no area);

/*
    Parameters  : start     - Start of range of addresses to lock (inclusive).
                  end       - End of range of addresses to lock (excusive).
    Returns     : os_error  - Pointer to a standard error block.
    Description : Locks pages of a dynamic area for which virtual memory has
                  been enabled.
                  This calls SWI XVirtualise_Lock.
*/
os_error *xvirtualise_lock(int start, int end);

/*
    Parameters  : start     - Start of range of addresses to lock (inclusive).
                  end       - End of range of addresses to lock (excusive).
    Returns     : void
    Description : Locks pages of a dynamic area for which virtual memory has
                  been enabled.
                  This calls SWI Virtualise_Lock.
*/
void virtualise_lock(int start, int end);

/*
    Parameters  : start     - Start of range of addresses to unlock (inclusive).
                  end       - End of range of addresses to unlock (excusive).
    Returns     : os_error  - Pointer to a standard error block.
    Description : Unlock pages of a dynamic area previously locked using
                  virtualise_lock.
                  This calls SWI XVirtualise_Unlock.
*/
os_error *xvirtualise_unlock(int start, int end);

/*
    Parameters  : start     - Start of range of addresses to unlock (inclusive).
                  end       - End of range of addresses to unlock (excusive).
    Returns     : void
    Description : Unlock pages of a dynamic area previously locked using
                  virtualise_lock.
                  This calls SWI Virtualise_Unlock.
*/
void virtualise_unlock(int start, int end);

/*
    Parameters  : area      - The area number to check.
                  physical  - The physical size of the area in bytes.
    Returns     : os_error  - Pointer to a standard error block.
    Description : Read the physical size of a dynamic area.
                  This calls SWI XVirtualise_MiscOp with reason code
                  VirtualiseMiscOp_AreaPhysicalSize.
*/
os_error *xvirtualise_miscop_physical(os_dynamic_area_no area,
                                      int *physical = NULL);

/*
    Parameters  : area  - The area number to check.
    Returns     : int   - The physical size of the area in bytes.
    Description : Read the physical size of a dynamic area.
                  This calls SWI Virtualise_MiscOp with reason code
                  VirtualiseMiscOp_AreaPhysicalSize.
*/
int virtualise_miscop_physical(os_dynamic_area_no area);

/*
    Parameters  : policy    - The replacement policy to use, or -1 to read.
                  rpolicy   - The returned replacement policy type.
    Returns     : os_error  - Pointer to a standard error block.
    Description : Set or read the page replacement policy.
                  This calls SWI XVirtualise_MiscOp with reason code
                  VirtualiseMiscOp_ReplacementPolicy.
*/
os_error *xvirtualise_miscop_policy(int policy, int *rpolicy = NULL);

/*
    Parameters  : policy    - The replacement policy to use, or -1 to read.
    Returns     : int       - The returned replacement policy.
    Description : Set or read the page replacement policy.
                  This calls SWI Virtualise_MiscOp with reason code
                  VirtualiseMiscOp_ReplacementPolicy.
*/
int virtualise_miscop_policy(int policy = -1);

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
os_error *xvirtualise_miscop_faults(int type, int *count = NULL);

/*
    Parameters  : type  - The class of instruction to return the number
                          of page faults for.
    Returns     : int   - The returned number of page faults.
    Description : Read the number of serviced page faults caused by a
                  particular class of instruction.
                  This calls SWI Virtualise_MiscOp with reason code
                  VirtualiseMiscOp_PageFaults.
*/
int virtualise_miscop_faults(int type);

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
                                   int start, int end, byte *buffer);

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
                             int start, int end, byte *buffer);

/*
    Parameters  : task      - The handle of the task to suspend.
    Returns     : os_error  - Pointer to a standard error block.
    Description : Attempt to swap the specified task to disc.
                  This calls SWI XVirtualise_MiscOp with reason code
                  VirtualiseMiscOp_TaskFreeze.
*/
os_error *xvirtualise_miscop_freeze(wimp_t task);

/*
    Parameters  : task      - The handle of the task to suspend.
    Returns     : void
    Description : Attempt to swap the specified task to disc.
                  This calls SWI Virtualise_MiscOp with reason code
                  VirtualiseMiscOp_TaskFreeze.
*/
void virtualise_miscop_freeze(wimp_t task);

/*
    Parameters  : task      - The handle of the task to restart.
    Returns     : os_error  - Pointer to a standard error block.
    Description : Attempt to restore a task previously swapped to disc.
                  This calls SWI XVirtualise_MiscOp with reason code
                  VirtualiseMiscOp_TaskThaw.
*/
os_error *xvirtualise_miscop_thaw(wimp_t task);

/*
    Parameters  : task      - The handle of the task to restart.
    Returns     : void
    Description : Attempt to restore a task previously swapped to disc.
                  This calls SWI Virtualise_MiscOp with reason code
                  VirtualiseMiscOp_TaskThaw.
*/
void virtualise_miscop_thaw(wimp_t task);

/*
    Parameters  : task      - The handle of the task to check.
                  status    - Variable to receive the status of the task.
    Returns     : os_error  - Pointer to a standard error block.
    Description : Discover the status of a task.
                  This calls SWI XVirtualise_MiscOp with reason code
                  VirtualiseMiscOp_TaskStatus.
*/
os_error *xvirtualise_miscop_status(wimp_t task, int *status);

/*
    Parameters  : task  - The handle of the task to check.
    Returns     : int   - Variable to receive the status of the task.
    Description : Discover the status of a task.
                  This calls SWI XVirtualise_MiscOp with reason code
                  VirtualiseMiscOp_TaskStatus.
*/
int virtualise_miscop_status(wimp_t task);

/*
    Parameters  : fract     - The fraction of the logical virtual memory size
                              to claim as physical memory, or -1 to read the
                              current value. This is a fixed point value, with
                              (1 << 15) corresponding to 100%.
                  lower     - Lower limit of memory to claim, below which all
                              of the required memory is claimed, or -1 to
                              read the current value.
                  upper     - Upper limit of memory to claim, above which no
                              more memory will be claimed, or -1 to read the
                              current value.
                  keep      - Amount of memory to keep even when another task
                              is attempting to claim memory, or -1 to read the
                              current value.
                  disc      - Amount of disc space to leave free, or -1 to read
                              the current value.
                  rfract    - Optional variable to receive the fraction of the
                              logical size to claim.
                  rlower    - Optional variable to receive the minimum memory
                              to claim, in bytes.
                  rupper    - Optional variable to receive the maximum memory
                              to claim, in bytes.
                  rkeep     - Optional variable to receive the memory to keep,
                              in bytes.
                  rdisc     - Optional variable to receive the disc space to
                              leave free, in bytes.
    Returns     : os_error  - Pointer to a standard error block.
    Description : Controls how much real memory to claim.
                  This calls XVirtualise_UserConfigure.
*/
os_error *xvirtualise_user_configure(int fract, int lower = -1, int upper = -1,
                                     int keep = -1, int disc = -1,
                                     int *rfract = NULL, int *rlower = NULL,
                                     int *rupper = NULL, int *rkeep = NULL,
                                     int *rdisc = NULL);

/*
    Parameters  : fract     - The fraction of the logical virtual memory size
                              to claim as physical memory, or -1 to read the
                              current value. This is a fixed point value, with
                              (1 << 15) corresponding to 100%.
                  lower     - Lower limit of memory to claim, below which all
                              of the required memory is claimed, or -1 to
                              read the current value.
                  upper     - Upper limit of memory to claim, above which no
                              more memory will be claimed, or -1 to read the
                              current value.
                  keep      - Amount of memory to keep even when another task
                              is attempting to claim memory, or -1 to read the
                              current value.
                  disc      - Amount of disc space to leave free, or -1 to read
                              the current value.
                  rfract    - Optional variable to receive the fraction of the
                              logical size to claim.
                  rlower    - Optional variable to receive the minimum memory
                              to claim, in bytes.
                  rupper    - Optional variable to receive the maximum memory
                              to claim, in bytes.
                  rkeep     - Optional variable to receive the memory to keep,
                              in bytes.
                  rdisc     - Optional variable to receive the disc space to
                              leave free, in bytes.
    Returns     : void
    Description : Controls how much real memory to claim.
                  This calls Virtualise_UserConfigure.
*/
void virtualise_user_configure(int fract, int lower = -1, int upper = -1,
                               int keep = -1, int disc = -1,
                               int *rfract = NULL, int *rlower = NULL,
                               int *rupper = NULL, int rkeep = NULL,
                               int *rdisc = NULL);

#ifdef __cplusplus
}
#endif

#endif
