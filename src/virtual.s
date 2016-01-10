;   File        : virtual.s
;   Date        : 23-Aug-99
;   Author      : © A.Thoukydides, 1995, 1996, 1997, 1998, 1999, 2016
;   Description : Veneers for calling Virtualise SWIs from C.
;
;   License     : Virtualise is free software: you can redistribute it and/or
;                 modify it under the terms of the GNU General Public License
;                 as published by the Free Software Foundation, either
;                 version 3 of the License, or (at your option) any later
;                 version.
;
;                 Virtualise is distributed in the hope that it will be useful,
;                 but WITHOUT ANY WARRANTY; without even the implied warranty
;                 of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See
;                 the GNU General Public License for more details.
;
;                 You should have received a copy of the GNU General Public
;                 License along with Virtualise. If not, see
;                 <http://www.gnu.org/licenses/>.

; Include system header files

        GET OS:Hdr.Macros
        GET OS:Hdr.OS

; Include my header files

        GET AT:Hdr.macros

; Exported symbols

        EXPORT xvirtualise_configure, virtualise_configure
        EXPORT xvirtualise_start, virtualise_start
        EXPORT xvirtualise_end, virtualise_end
        EXPORT xvirtualise_lock, virtualise_lock
        EXPORT xvirtualise_unlock, virtualise_unlock
        EXPORT xvirtualise_miscop_physical, virtualise_miscop_physical
        EXPORT xvirtualise_miscop_policy, virtualise_miscop_policy
        EXPORT xvirtualise_miscop_faults, virtualise_miscop_faults
        EXPORT xvirtualise_miscop_pages, virtualise_miscop_pages
        EXPORT xvirtualise_miscop_freeze, virtualise_miscop_freeze
        EXPORT xvirtualise_miscop_thaw, virtualise_miscop_thaw
        EXPORT xvirtualise_miscop_status, virtualise_miscop_status
        EXPORT xvirtualise_user_configure, virtualise_user_configure

; Constants

XVirtualise_Configure           *       &6B6C0
Virtualise_Configure            *       &4B6C0
XVirtualise_Start               *       &6B6C1
Virtualise_Start                *       &4B6C1
XVirtualise_End                 *       &6B6C2
Virtualise_End                  *       &4B6C2
XVirtualise_Lock                *       &6B6C3
Virtualise_Lock                 *       &4B6C3
XVirtualise_Unlock              *       &6B6C4
Virtualise_Unlock               *       &4B6C4
XVirtualise_MiscOp              *       &6B6C5
Virtualise_MiscOp               *       &4B6C5
XVirtualise_UserConfigure       *       &6B6C6
Virtualise_UserConfigure        *       &4B6C6

VirtualiseMiscOp_AreaPhysicalSize *     &0
VirtualiseMiscOp_ReplacementPolicy *    &1
VirtualiseMiscOp_PageFaults     *       &2
VirtualiseMiscOp_PageStatus     *       &3
VirtualiseMiscOp_TaskFreeze     *       &4
VirtualiseMiscOp_TaskThaw       *       &5
VirtualiseMiscOp_TaskStatus     *       &6

; Define an area to dump everything into

        AREA    |C$$code|, CODE, READONLY

; The SWI veneers

; os_error *xvirtualise_configure(int limit, int claim, int free, int *rlimit,
;                                 int *rclaim, int *rfree)
xvirtualise_configure
        LocalLabels
        MOV     ip, sp                  ; Copy stack pointer
        STMFD   sp!, {lr}               ; Stack return address
        SWI     XVirtualise_Configure   ; Call the SWI
        BVS     err$l                   ; Skip to the exit if failed
        TEQ     r3, #0                  ; Is rlimit a valid pointer
        STRNE   r0, [r3]                ; Store return r0 if it is
        LDR     r3, [ip]                ; Get rclaim pointer
        TEQ     r3, #0                  ; Is rclaim a valid pointer
        STRNE   r1, [r3]                ; Store return r1 if it is
        LDR     r3, [ip, #4]            ; Get rfree pointer
        TEQ     r3, #0                  ; Is rfree a valid pointer
        STRNE   r2, [r3]                ; Store return r2 if it is
        MOV     r0, #0                  ; Clear error pointer if successful
err$l   LDMFD   sp!, {pc}^              ; Return from subroutine

; void virtualise_configure(int limit, int claim, int free, int *rlimit,
;                           int *rclaim, int *rfree)
virtualise_configure
        LocalLabels
        MOV     ip, sp                  ; Copy stack pointer
        STMFD   sp!, {lr}               ; Stack return address
        SWI     Virtualise_Configure    ; Call the SWI
        TEQ     r3, #0                  ; Is rlimit a valid pointer
        STRNE   r0, [r3]                ; Store return r0 if it is
        LDR     r3, [ip]                ; Get rclaim pointer
        TEQ     r3, #0                  ; Is rclaim a valid pointer
        STRNE   r1, [r3]                ; Store return r1 if it is
        LDR     r3, [ip, #4]            ; Get rfree pointer
        TEQ     r3, #0                  ; Is rfree a valid pointer
        STRNE   r2, [r3]                ; Store return r2 if it is
        LDMFD   sp!, {pc}^              ; Return from subroutine

; os_error *xvirtualise_start(os_dynamic_area_no area, int max,
;                             const char *name)
xvirtualise_start
        LocalLabels
        MOV     ip, lr                  ; Copy return address
        SWI     XVirtualise_Start       ; Call the SWI
        MOVVC   r0, #0                  ; Clear error pointer if successful
        MOVS    pc, ip                  ; Return from subroutine

; void virtualise_start(os_dynamic_area_no area, int max, const char *name)
virtualise_start
        LocalLabels
        MOV     ip, lr                  ; Copy return address
        SWI     Virtualise_Start        ; Call the SWI
        MOVS    pc, ip                  ; Return from subroutine

; os_error *xvirtualise_end(os_dynamic_area_no area)
xvirtualise_end
        LocalLabels
        MOV     ip, lr                  ; Copy return address
        SWI     XVirtualise_End         ; Call the SWI
        MOVVC   r0, #0                  ; Clear error pointer if successful
        MOVS    pc, ip                  ; Return from subroutine

; void virtualise_end(os_dynamic_area_no area)
virtualise_end
        LocalLabels
        MOV     ip, lr                  ; Copy return address
        SWI     Virtualise_End          ; Call the SWI
        MOVS    pc, ip                  ; Return from subroutine

; os_error *xvirtualise_lock(int start, int end)
xvirtualise_lock
        LocalLabels
        MOV     ip, lr                  ; Copy return address
        SWI     XVirtualise_Lock        ; Call the SWI
        MOVVC   r0, #0                  ; Clear error pointer if successful
        MOVS    pc, ip                  ; Return from subroutine

; void virtualise_lock(int start, int end)
virtualise_lock
        LocalLabels
        MOV     ip, lr                  ; Copy return address
        SWI     Virtualise_Lock         ; Call the SWI
        MOVS    pc, ip                  ; Return from subroutine

; os_error *xvirtualise_unlock(int start, int end)
xvirtualise_unlock
        LocalLabels
        MOV     ip, lr                  ; Copy return address
        SWI     XVirtualise_Unlock      ; Call the SWI
        MOVVC   r0, #0                  ; Clear error pointer if successful
        MOVS    pc, ip                  ; Return from subroutine

; void virtualise_unlock(int start, int end)
virtualise_unlock
        LocalLabels
        MOV     ip, lr                  ; Copy return address
        SWI     Virtualise_Unlock       ; Call the SWI
        MOVS    pc, ip                  ; Return from subroutine

; os_error *xvirtualise_miscop_physical(os_dynamic_area_no area, int *physical)
xvirtualise_miscop_physical
        LocalLabels
        MOV     ip, lr                  ; Copy return address
        MOV     r2, r1                  ; Copy physical pointer
        MOV     r1, r0                  ; Copy area number
        MOV     r0, #VirtualiseMiscOp_AreaPhysicalSize; Reason code
        SWI     XVirtualise_MiscOp      ; Call the SWI
        BVS     err$l                   ; Skip to the exit if failed
        TEQ     r2, #0                  ; Is physical pointer valid
        STRNE   r1, [r2]                ; Store return r1 if it is
        MOV     r0, #0                  ; Clear error pointer if successful
err$l   MOVS    pc, ip                  ; Return from subroutine

; int virtualise_miscop_physical(os_dynamic_area_no area)
virtualise_miscop_physical
        LocalLabels
        MOV     ip, lr                  ; Copy return address
        MOV     r1, r0                  ; Copy area number
        MOV     r0, #VirtualiseMiscOp_AreaPhysicalSize; Reason code
        SWI     Virtualise_MiscOp       ; Call the SWI
        MOV     r0, r1                  ; Copy physical size returned
        MOVS    pc, ip                  ; Return from subroutine

; os_error *xvirtualise_miscop_policy(int policy, int *rpolicy)
xvirtualise_miscop_policy
        LocalLabels
        MOV     ip, lr                  ; Copy return address
        MOV     r2, r1                  ; Copy rpolicy pointer
        MOV     r1, r0                  ; Copy policy index
        MOV     r0, #VirtualiseMiscOp_ReplacementPolicy; Reason code
        SWI     XVirtualise_MiscOp      ; Call the SWI
        BVS     err$l                   ; Skip to the exit if failed
        TEQ     r2, #0                  ; Is rpolicy pointer valid
        STRNE   r1, [r2]                ; Store return r1 if it is
        MOV     r0, #0                  ; Clear error pointer if successful
err$l   MOVS    pc, ip                  ; Return from subroutine

; int virtualise_miscop_policy(int policy)
virtualise_miscop_policy
        LocalLabels
        MOV     ip, lr                  ; Copy return address
        MOV     r1, r0                  ; Copy policy index
        MOV     r0, #VirtualiseMiscOp_ReplacementPolicy; Reason code
        SWI     Virtualise_MiscOp       ; Call the SWI
        MOV     r0, r1                  ; Copy replacement policy returned
        MOVS    pc, ip                  ; Return from subroutine

; os_error *xvirtualise_miscop_faults(int type, int *count)
xvirtualise_miscop_faults
        LocalLabels
        MOV     ip, lr                  ; Copy return address
        MOV     r2, r1                  ; Copy count pointer
        MOV     r1, r0                  ; Copy fault type
        MOV     r0, #VirtualiseMiscOp_PageFaults; Reason code
        SWI     XVirtualise_MiscOp      ; Call the SWI
        BVS     err$l                   ; Skip to the exit if failed
        TEQ     r2, #0                  ; Is count pointer valid
        STRNE   r1, [r2]                ; Store return r1 if it is
        MOV     r0, #0                  ; Clear error pointer if successful
err$l   MOVS    pc, ip                  ; Return from subroutine

; int virtualise_miscop_faults(int type)
virtualise_miscop_faults
        LocalLabels
        MOV     ip, lr                  ; Copy return address
        MOV     r1, r0                  ; Copy fault type
        MOV     r0, #VirtualiseMiscOp_PageFaults; Reason code
        SWI     Virtualise_MiscOp       ; Call the SWI
        MOV     r0, r1                  ; Copy number of page faults
        MOVS    pc, ip                  ; Return from subroutine

; os_error *xvirtualise_miscop_pages(os_dynamic_area_no area,
;                                    int start, int end, byte *buffer)
xvirtualise_miscop_pages
        LocalLabels
        STMFD   sp!, {v1, lr}           ; Stack return address
        MOV     r4, r3                  ; Copy buffer pointer
        MOV     r3, r2                  ; Copy end index
        MOV     r2, r1                  ; Copy start index
        MOV     r1, r0                  ; Copy area number
        MOV     r0, #VirtualiseMiscOp_PageStatus; Reason code
        SWI     XVirtualise_MiscOp      ; Call the SWI
        MOVVC   r0, #0                  ; Clear error pointer if successful
        LDMFD   sp!, {v1, pc}^          ; Return from subroutine

; void virtualise_miscop_pages(os_dynamic_area_no area,
;                              int start, int end, byte *buffer)
virtualise_miscop_pages
        LocalLabels
        STMFD   sp!, {v1, lr}           ; Stack return address
        MOV     r4, r3                  ; Copy buffer pointer
        MOV     r3, r2                  ; Copy end index
        MOV     r2, r1                  ; Copy start index
        MOV     r1, r0                  ; Copy area number
        MOV     r0, #VirtualiseMiscOp_PageStatus; Reason code
        SWI     Virtualise_MiscOp       ; Call the SWI
        LDMFD   sp!, {v1, pc}^          ; Return from subroutine

; os_error *xvirtualise_miscop_freeze(wimp_t task)
xvirtualise_miscop_freeze
        LocalLabels
        MOV     ip, lr                  ; Copy return address
        MOV     r1, r0                  ; Copy task handle
        MOV     r0, #VirtualiseMiscOp_TaskFreeze; Reason code
        SWI     XVirtualise_MiscOp      ; Call the SWI
        MOVVC   r0, #0                  ; Clear error pointer if successful
        MOVS    pc, ip                  ; Return from subroutine

; void virtualise_miscop_freeze(wimp_t task)
virtualise_miscop_freeze
        LocalLabels
        MOV     ip, lr                  ; Copy return address
        MOV     r1, r0                  ; Copy task handle
        MOV     r0, #VirtualiseMiscOp_TaskFreeze; Reason code
        SWI     Virtualise_MiscOp       ; Call the SWI
        MOVS    pc, ip                  ; Return from subroutine

; os_error *xvirtualise_miscop_thaw(wimp_t task)
xvirtualise_miscop_thaw
        LocalLabels
        MOV     ip, lr                  ; Copy return address
        MOV     r1, r0                  ; Copy task handle
        MOV     r0, #VirtualiseMiscOp_TaskThaw; Reason code
        SWI     XVirtualise_MiscOp      ; Call the SWI
        MOVVC   r0, #0                  ; Clear error pointer if successful
        MOVS    pc, ip                  ; Return from subroutine

; void virtualise_miscop_thaw(wimp_t task)
virtualise_miscop_thaw
        LocalLabels
        MOV     ip, lr                  ; Copy return address
        MOV     r1, r0                  ; Copy task handle
        MOV     r0, #VirtualiseMiscOp_TaskThaw; Reason code
        SWI     Virtualise_MiscOp       ; Call the SWI
        MOVS    pc, ip                  ; Return from subroutine

; os_error *xvirtualise_miscop_status(wimp_t task, int *status)
xvirtualise_miscop_status
        LocalLabels
        MOV     ip, lr                  ; Copy return address
        MOV     r2, r1                  ; Copy status pointer
        MOV     r1, r0                  ; Copy task handle
        MOV     r0, #VirtualiseMiscOp_TaskStatus; Reason code
        SWI     XVirtualise_MiscOp      ; Call the SWI
        BVS     err$l                   ; Skip to the exit if failed
        TEQ     r2, #0                  ; Is status pointer valid
        STRNE   r0, [r2]                ; Store task status if it is
        MOV     r0, #0                  ; Clear error pointer if successful
err$l   MOVS    pc, ip                  ; Return from subroutine

; int virtualise_miscop_status(wimp_t task)
virtualise_miscop_status
        LocalLabels
        MOV     ip, lr                  ; Copy return address
        MOV     r1, r0                  ; Copy task handle
        MOV     r0, #VirtualiseMiscOp_TaskStatus; Reason code
        SWI     Virtualise_MiscOp       ; Call the SWI
        MOVS    pc, ip                  ; Return from subroutine

; os_error *xvirtualise_user_configure(int fract, int lower, int upper,
;                                      int keep, int disc, int *rfract,
;                                      int *rlower, int *rupper, int *rkeep,
;                                      int *rdisc)
xvirtualise_user_configure
        LocalLabels
        MOV     ip, sp                  ; Copy stack pointer
        STMFD   sp!, {v1-v2, lr}        ; Stack return address
        LDR     r4, [ip]                ; Get disc value
        SWI     XVirtualise_UserConfigure; Call the SWI
        BVS     err$l                   ; Skip to the exit if failed
        LDR     r5, [ip, #4]            ; Get rfract pointer
        TEQ     r5, #0                  ; Is rfract a valid pointer
        STRNE   r0, [r5]                ; Store return r0 if it is
        LDR     r5, [ip, #8]            ; Get rlower pointer
        TEQ     r5, #0                  ; Is rlower a valid pointer
        STRNE   r1, [r5]                ; Store return r1 if it is
        LDR     r5, [ip, #12]           ; Get rupper pointer
        TEQ     r5, #0                  ; Is rupper a valid pointer
        STRNE   r2, [r5]                ; Store return r2 if it is
        LDR     r5, [ip, #16]           ; Get rkeep pointer
        TEQ     r5, #0                  ; Is rkeep a valid pointer
        STRNE   r3, [r5]                ; Store return r3 if it is
        LDR     r5, [ip, #20]           ; Get rdisc pointer
        TEQ     r5, #0                  ; Is rdisc a valid pointer
        STRNE   r4, [r5]                ; Store return r4 if it is
        MOV     r0, #0                  ; Clear error pointer if successful
err$l   LDMFD   sp!, {v1-v2, pc}^       ; Return from subroutine

; void virtualise_user_configure(int fract, int lower, int upper, int keep,
;                                int disc, int *rfract, int *rlower,
;                                int *rupper, int *rkeep, int *rdisc)
virtualise_user_configure
        LocalLabels
        MOV     ip, sp                  ; Copy stack pointer
        STMFD   sp!, {v1-v2, lr}        ; Stack return address
        LDR     r4, [ip]                ; Get disc value
        SWI     Virtualise_UserConfigure; Call the SWI
        LDR     r5, [ip, #4]            ; Get rfract pointer
        TEQ     r5, #0                  ; Is rfract a valid pointer
        STRNE   r0, [r5]                ; Store return r0 if it is
        LDR     r5, [ip, #8]            ; Get rlower pointer
        TEQ     r5, #0                  ; Is rlower a valid pointer
        STRNE   r1, [r5]                ; Store return r1 if it is
        LDR     r5, [ip, #12]           ; Get rupper pointer
        TEQ     r5, #0                  ; Is rupper a valid pointer
        STRNE   r2, [r5]                ; Store return r2 if it is
        LDR     r5, [ip, #16]           ; Get rkeep pointer
        TEQ     r5, #0                  ; Is rkeep a valid pointer
        STRNE   r3, [r5]                ; Store return r3 if it is
        LDR     r5, [ip, #20]           ; Get rdisc pointer
        TEQ     r5, #0                  ; Is rdisc a valid pointer
        STRNE   r4, [r5]                ; Store return r4 if it is
        LDMFD   sp!, {v1-v2, pc}^       ; Return from subroutine

end
