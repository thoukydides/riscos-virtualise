;   File        : virtualise.s
;   Date        : 14-Dec-99
;   Author      : © A.Thoukydides, 1995, 1996, 1997, 1998, 1999, 2016
;   Description : Dynamic area virtual memory manager module.
;                 This module will not operate in read only memory - it uses
;                 space in the module for variables.
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

; To complete RISC OS 4 support requires:
;   test_shrink$l       Return the memory that can be freed
;   handler_intercept   Replace TestShrink if virtual memory
;   start_virtualise    Change &F70 to &D70
;   pre_add_intercept   Change &F70 to &D70
;   create_dynamic      Change &103 to &303

; Include system header files

        GET OS:Hdr.FileSwitch
        GET OS:Hdr.Filter
        GET OS:Hdr.Hourglass
        GET OS:Hdr.Macros
        GET OS:Hdr.MessageTrans
        GET OS:Hdr.OS
        GET OS:Hdr.OSArgs
        GET OS:Hdr.OSByte
        GET OS:Hdr.OSFile
        GET OS:Hdr.OSFind
        GET OS:Hdr.OSFSControl
        GET OS:Hdr.OSGBPB
        GET OS:Hdr.OSHeap
        GET OS:Hdr.OSModule
        GET OS:Hdr.ResourceFS
        GET OS:Hdr.TaskManager
        GET OS:Hdr.Types
        GET OS:Hdr.Wimp
        GET OS:Hdr.WimpSpriteOp

; Include my header files

        GET AT:Hdr.macros

; Constants

Vector_SWI          * 2                 ; Number of SWI vector
Vector_DataAbort    * 4                 ; Number of Data abort vector
min_spare_pages     * 3                 ; Minimum number of spare pages to keep
auto_user_frac_shift * 15               ; Number of binary places for fraction
auto_user_frac      * 1 << (auto_user_frac_shift - 1); Default fract to claim
auto_user_min       * 1024 * 1000       ; Default minimum memory to claim
auto_user_max       * 1024 * 1024 * 256 ; Default maximum memory to claim
auto_sw_free        * 1024 * 1024 * 1   ; Default minimum memory to leave free
auto_sw_min         * 1024 * 1024 * 10  ; Default minimum memory to claim
auto_sw_keep        * 1024 * 512        ; Default minimum memory to keep
auto_sw_disc        * 1024 * 1024 * 5   ; Default minimum free disc space
resource_directory  DefS "Resources.Virtualise"; Directory to use in ResourceFS
swi_chunk           * &4B6C0            ; SWI chunk used by this module
error_base          * &80E100           ; The first error number to use
area_flag_vm        * 1 << 31           ; Area flag to inidcate virtual memory
max_area_size       * 256 * 1024 * 1024 ; Maximum size of area allowed
min_pull_count      * 2                 ; Minimum page faults between resets
shift_pull_count    * 3                 ; Shift to convert pages to delay
mult_shift_pool     * 4                 ; Shift for multiple limit from pool
mult_shift_logical  * 3                 ; Shift for multiple limit from logical
mult_shift_physical * 2                 ; Shift for multiple limit from physical
cpsr_F              * 1 << 6            ; F bit in CPSR
cpsr_I              * 1 << 7            ; I bit in CPSR
cpsr_V              * 1 << 28           ; V bit in CPSR
cpsr_C              * 1 << 29           ; C bit in CPSR
cpsr_N              * 1 << 30           ; N bit in CPSR
cpsr_Z              * 1 << 31           ; Z bit in CPSR
I                   * 1 << 27           ; I bit in PC
F                   * 1 << 26           ; F bit in PC
guard_pre           * &4D562A2F         ; Guard word before memory block "/*VM"
guard_post          * &2F2A4D56         ; Guard word after memory block "VM*/"
guard_clear_pre     * &6D767B7B         ; Guard word before memory block "{{vm"
guard_clear_post    * &7D7D6D76         ; Guard word after memory block "vm}}"
debug               DefL    {FALSE}     ; Should debugging code be included
pipe                DefL    {FALSE}     ; Should information be sent to a pipe
assert              DefL    {TRUE}      ; Should assertions be included
        [       {TRUE}
demo_area1          DefS "ProArt24DEMO" ; Demo dynamic area name
demo_area2          DefS "Compo canvasDEMO"; Demo dynamic area name
demo_area3          DefS "Compo mask undoDEMO"; Demo dynamic area name
demo_area4          DefS "Compo workspaceDEMO"; Demo dynamic area name
demo_area5          DefS "Compo canvas undoDEMO"; Demo dynamic area name
demo_area6          DefS "Compo text storeDEMO"; Demo dynamic area name
demo_area7          DefS "Compo vector storeDEMO"; Demo dynamic area name
demo_area8          DefS "Compo vignetteDEMO"; Demo dynamic area name
        |
demo_area1          DefS "ProArt24"     ; Demo dynamic area name
demo_area2          DefS "Compo canvas" ; Demo dynamic area name
demo_area3          DefS "Compo mask undo"; Demo dynamic area name
demo_area4          DefS "Compo workspace"; Demo dynamic area name
demo_area5          DefS "Compo canvas undo"; Demo dynamic area name
demo_area6          DefS "Compo text store"; Demo dynamic area name
demo_area7          DefS "Compo vector store"; Demo dynamic area name
demo_area8          DefS "Compo vignette"; Demo dynamic area name
        ]
replace_policy      * policy_random \
                      :OR: policy_flags_reads \
                      :OR: policy_flags_writes

; Define an area to dump everything into

        AREA    |main|, PIC, CODE, DATA

; Module header

base    &       0                       ; Start offset
        &       initialisation - base   ; Initialisation offset
        &       finalisation - base     ; Finalisation offset
        &       service - base          ; Service call handler offset
        &       title - base            ; Title string offset
        &       help - base             ; Help string offset
        &       commands - base         ; Help and command keyword table offset
        &       swi_chunk               ; SWI chunk base number
        &       swi_handler - base      ; SWI handler code offset
        &       swi_table - base        ; SWI decoding table offset
        &       0                       ; SWI decoding code offset

; Module title and version

title   =       "Virtualise", 0

help
        =       "Virtualise", 9, "1.17 (14 Dec 1999)"
;        =       " [TEST]"
        [       :DEF: demo
        =       " [DEMO]"
        ]
        =       " © A.Thoukydides, 1995, 1996, 1997, 1998, 1999", 0
        ALIGN

        ;   Syntax      : [<label>] debug_record <name>
        ;   Parameters  : label - An optional program label.
        ;                 name  - A string giving a name for the record
        ;   Description : Produce code to cause a debug record to be created.
        ;                 No other registers are required, not even a stack.
        ;                 In addition, this code will work in any processor
        ;                 mode.
        MACRO
$label  debug_record $name
$label
        [       debug
        MacroLabels
        B       cont$l                  ; Skip over record name
name$l  =       "$name", 0              ; The record name
        ALIGN
dump$l  %       Int                     ; Dump for r0
dump2$l %       Int                     ; Dump for r12
dump3$l %       Int                     ; Dump for CPSR
cont$l  STR     r0, dump$l              ; Store r0 somewhere first
        STR     r12, dump2$l            ; Store r12 somewhere also
        MRS     r0, CPSR                ; Get CPSR for mode and flags
        STR     r0, dump3$l             ; Store CPSR somewhere also
        ADRL    r12, workspace_ptr      ; Pointer to workspace pointer
        LDR     r12, [r12]              ; Get pointer to workspace
        LDR     r0, ws_debug_ptr        ; Get pointer to debug dump
        ADD     r0, r0, #debug_regs     ; Pointer to register dump
        STMIB   r0, {r1-pc}             ; Dump all registers except r0
        LDR     r1, dump$l              ; Get value of r0
        STR     r1, [r0]                ; Dump r0 also
        LDR     r1, dump2$l             ; Get value of r12
        STR     r1, [r0, #4 * 12]       ; Dump r12 also
        LDR     r1, dump3$l             ; Get status flags
        STR     r1, [r0, #4 * 16]       ; Dump CPSR also
        LDR     r0, ws_debug_ptr        ; Get the dump start
        LDR     r1, ws_debug_enable     ; Check if debugging enabled
        TEQ     r1, #0                  ; Is the flag set
        BEQ     none$l                  ; Skip next bit if not
        ADD     r0, r0, #debug_name     ; Pointer to name storage
        ADR     r1, name$l              ; Get pointer to dump name
loop$l  LDRB    r2, [r1], #1            ; Get character from name
        STRB    r2, [r0], #1            ; Store the character
        TEQ     r2, #0                  ; Is it the terminator
        BNE     loop$l                  ; Loop until finished
        LDR     r0, ws_debug_ptr        ; Restore pointer to debug dump
        LDR     r2, ws_debug_next       ; Get current record number
        LDR     r3, ws_debug_total      ; Get current number of pages
        ADD     r1, r0, #debug_item     ; Increment pointer to next record
        TEQ     r3, #max_debug_items    ; Are any more records allowed
        ADDNE   r3, r3, #1              ; Increment count if they are
        ADD     r2, r2, #1              ; Increment record number
        TEQ     r2, #max_debug_items    ; Is it time to wrap around
        MOVEQ   r2, #0                  ; Reset next record number
        ADREQL  r1, ws_debug_dump       ; Reset next pointer
        STR     r1, ws_debug_ptr        ; Store the updated pointer
        STR     r2, ws_debug_next       ; Store updated position number
        STR     r3, ws_debug_total      ; Store new number of dumps
none$l  ADD     r0, r0, #debug_regs     ; Pointer to register dump
        LDR     r1, dump3$l             ; Get old CPSR value
        MSR     CPSR, r1                ; Restore flags
        LDMIA   r0, {r0-r14}            ; Restore registers
        MacroLabelsEnd
        ]
        MEND

        ;   Syntax      : [<label>] pipe_value <r>
        ;   Parameters  : label - An optional program label.
        ;                 r     - The register whose value to process.
        ;   Description : This is used by the pipe_string macro to process a
        ;                 single register's value.
        MACRO
$label  pipe_value $r
$label
        [       pipe :LAND: "$r" /= ""
        MacroLabels
        STMFD   r13!, {r0-r2}           ; Stack registers
        MOV     r0, $r                  ; Copy the required value
        LDR     r1, ws_pipe_output      ; Get output position
        MOV     r2, #String             ; Size of output buffer
        SWI     XOS_ConvertHex8         ; Convert the number
        MOV     r0, #' '                ; Space character
        STRB    r0, [r1], #1            ; Append the space
        STR     r1, ws_pipe_output      ; Store new output position
        LDMFD   r13!, {r0-r2}           ; Restore registers
        MacroLabelsEnd
        ]
        MEND

        ;   Syntax      : [<label>] pipe_string <tmpl> [, <r0> [, <r1> ... ]]
        ;   Parameters  : label - An optional program label.
        ;                 templ - The template text.
        ;                 r0... - Up to 10 optional parameters which are
        ;                         substituted in place of %0 to %9 in the
        ;                         template.
        ;   Description : Write a template string to the pipe. This must only
        ;                 be used in a situation which will support normal
        ;                 file operations, i.e. the processor must be in
        ;                 USR26 or SVC26 mode and there must be a valid stack.
        ;                 It is not a good idea to use virtual memory within
        ;                 a task window when this is active.
        MACRO
$label  pipe_string $tmpl, $r0, $r1, $r2, $r3, $r4, $r5, $r6, $r7, $r8, $r9
$label
        [       pipe
        MacroLabels
        B       cont$l                  ; Skip over the template text
text$l  =       "$tmpl"                 ; The template text
        ALIGN
cont$l  STMFD   r13!, {r0}              ; Stack registers
        ADRL    r0, ws_pipe_args        ; Pointer to start of argument list
        STR     r0, ws_pipe_output      ; Store argument list pointer
        LDMFD   r13!, {r0}              ; Restore registers
        pipe_value $r0                  ; Process the 1st optional value
        pipe_value $r1                  ; Process the 2nd optional value
        pipe_value $r2                  ; Process the 3rd optional value
        pipe_value $r3                  ; Process the 4th optional value
        pipe_value $r4                  ; Process the 5th optional value
        pipe_value $r5                  ; Process the 6th optional value
        pipe_value $r6                  ; Process the 7th optional value
        pipe_value $r7                  ; Process the 8th optional value
        pipe_value $r8                  ; Process the 9th optional value
        pipe_value $r9                  ; Process the 10th optional value
        STMFD   r13!, {r0, r1, r14}     ; Stack registers
        ADR     r0, text$l              ; Get pointer to template string
        MOV     r1, #?text$l            ; Length of template string
        BL      pipe_write              ; Write the string to the pipe
        LDMFD   r13!, {r0, r1, r14}     ; Restore registers
        MacroLabelsEnd
        ]
        MEND

        ;   Syntax      : [<label>] name_check <rd> <rt> <area> <ok>
        ;   Parameters  : label - An optional program label.
        ;                 rd    - Register containing pointer to this name.
        ;                 rt    - A temporary work register
        ;                 area  - The area name to check.
        ;                 ok    - Label to jump to if matches.
        ;   Description : Check if the are name matches that specified.
        MACRO
$label  name_check $rd, $rt, $area, $ok
        MacroLabels
        ASSERT  $rd <> $rt
        ASSERT  $rd <> r4
        ASSERT  $rd <> r5
        ASSERT  $rt <> r4
        ASSERT  $rt <> r5
        LCLA    match$l                 ; Variable for match test
        LCLA    off$l                   ; Variable for branch offset
        LCLA    char$l                  ; Variable for current character
        LCLA    init$l                  ; Initial value for match
        LCLS    work$l                  ; String being manipulated
init$l  SETA    (:NOT: (. - base)) :EOR: &98765432; Generate initial value
match$l SETA    init$l                  ; Initial value for match
off$l   SETA    init$l                  ; Initial value for jump offset
work$l  SETS    $area                   ; Start with specified area name
        WHILE   0 < :LEN: work$l        ; Check if whole name processed
char$l  SETA    work$l :LEFT: 1         ; Get the next character
work$l  SETS    work$l :RIGHT: (:LEN: work$l - 1); Chop off first character
match$l SETA    match$l + char$l        ; Update the match test variable
off$l   SETA    (off$l << 1) :EOR: char$l; Update offset modifer
        WEND                            ; End of loop
$label  STMFD   r13!, {r4-r5, $rd}      ; Stack registers
        LDR     r5, = init$l            ; Initialise match test
        MOV     $rt, r5                 ; Initialise branch offset
loop$l  LDRB    r4, [$rd], #1           ; Read a character from the name
        TEQ     r4, #0                  ; Is it a terminator
        BEQ     done$l                  ; Exit loop if it is
        ADD     r5, r5, r4              ; Include this character in count
        EOR     $rt, r4, $rt, LSL#1     ; Update the branch offset
        B       loop$l                  ; Loop for next character
done$l  LDR     r4, = match$l           ; Value to compare against
        TEQ     r4, r5                  ; Does it match
        LDR     r4, = off$l :EOR: ($ok - jump$l); Jump offset correction
        EOR     $rt, $rt, r4            ; Correct jump offset
        LDMFD   r13!, {r4-r5, $rd}      ; Restore registers
jump$l  STREQ   pc, [$rt, -$rt]         ; Jump to matched code if matched
        MacroLabelsEnd
        MEND

        ;   Syntax      : [<label>] assert_registers
        ;   Parameters  : label - An optional program label.
        ;   Description : Dump the current register values.
        MACRO
$label  assert_registers
$label
        [       debug
        MacroLabels
        STR     r0, ws_debug_regs       ; Store r0 value
        ADRL    r0, ws_debug_regs       ; Pointer to register dump
        STMIB   r0, {r1-r15}            ; Store other registers
        LDR     r0, ws_debug_regs       ; Restore r0 value
        MacroLabelsEnd
        ]
        MEND

        ;   Syntax      : [<label>] assert_source <rd> <desc> <crypt>
        ;   Parameters  : label - An optional program label.
        ;                 rd    - Register containing the pointer.
        ;                 desc  - A full length description.
        ;                 crypt - A cryptic description.
        ;   Description : Check that the specified source page pointer is valid.
        MACRO
$label  assert_source $rd, $desc, $crypt
$label
        [       assert
        MacroLabels
        assert_registers                ; Save register values
        STMFD   r13!, {r0-r1}           ; Stack the registers
        MOV     r0, $rd                 ; Copy the pointer
        ADR     r1, desc$l              ; Pointer to the description
        BL      check_assert_source     ; Check the pointer
        LDMFD   r13!, {r0-r1}           ; Restore the registers
        B       skip$l                  ; Skip over the description
desc$l
        [       debug
        =       "$desc", 0              ; Full description
        |
        =       "$crypt", 0             ; Cryptic description
        ]
        ALIGN
skip$l
        MacroLabelsEnd
        ]
        MEND

        ;   Syntax      : [<label>] assert_source <rd> <desc> <crypt>
        ;   Parameters  : label - An optional program label.
        ;                 rd    - Register containing the pointer.
        ;                 desc  - A full length description.
        ;                 crypt - A cryptic description.
        ;   Description : Check that the specified area record pointer is valid.
        MACRO
$label  assert_area $rd, $desc, $crypt
$label
        [       assert
        MacroLabels
        assert_registers                ; Save register values
        STMFD   r13!, {r0-r1}           ; Stack the registers
        MOV     r0, $rd                 ; Copy the pointer
        ADR     r1, desc$l              ; Pointer to the description
        BL      check_assert_area       ; Check the pointer
        LDMFD   r13!, {r0-r1}           ; Restore the registers
        B       skip$l                  ; Skip over the description
desc$l
        [       debug
        =       "$desc", 0              ; Full description
        |
        =       "$crypt", 0             ; Cryptic description
        ]
        ALIGN
skip$l
        MacroLabelsEnd
        ]
        MEND

        ;   Syntax      : [<label>] assert_source <rd> <desc> <crypt>
        ;   Parameters  : label - An optional program label.
        ;                 rd    - Register containing the pointer.
        ;                 desc  - A full length description.
        ;                 crypt - A cryptic description.
        ;   Description : Check that the specified virtual page pointer is
        ;                 valid.
        MACRO
$label  assert_virtual $rd, $desc, $crypt
$label
        [       assert
        MacroLabels
        assert_registers                ; Save register values
        STMFD   r13!, {r0-r1}           ; Stack the registers
        MOV     r0, $rd                 ; Copy the pointer
        ADR     r1, desc$l              ; Pointer to the description
        BL      check_assert_virtual    ; Check the pointer
        LDMFD   r13!, {r0-r1}           ; Restore the registers
        B       skip$l                  ; Skip over the description
desc$l
        [       debug
        =       "$desc", 0              ; Full description
        |
        =       "$crypt", 0             ; Cryptic description
        ]
        ALIGN
skip$l
        MacroLabelsEnd
        ]
        MEND

; Handle ResourceFS files

        ;   Parameters  : None
        ;   Returns     : None
        ;   Description : Create and install the ResourceFS file, if required.
create_resource_files
        LocalLabels
        JSR     "r0-r5, r10, r11"       ; Stack registers
        ADR     r0, resources           ; Get pointer to resource files
        SWI     XResourceFS_RegisterFiles; Add the files to ResourceFS
        RTE VS                          ; Exit without wiping over r0
        RTS                             ; Return from subroutine

        ;   Parameters  : None
        ;   Returns     : None
        ;   Description : Remove any files added to ResourceFS file, and free
        ;                 the memory allocated for that purpose.
destroy_resource_files
        LocalLabels
        JSR     "r0-r2"                 ; Stack registers
        ADR     r0, resources           ; Get pointer to resource files
        SWI     XResourceFS_DeregisterFiles; Remove files from ResourceFS
        RTSS                            ; Return from subroutine

        ; Stuff to go in ResourceFS for general use
resources
        LocalLabels
        &       next$l - {PC}           ; Offset to next file
        &       &FFFFFF00               ; Load address of file (text file)
        &       0                       ; Execute address of file
        &       end$l - start$l         ; Actual file size
        &       FileSwitch_AttrOwnerRead; File attributes
        =       resource_directory      ; Directory
        =       ".Messages", 0          ; Filename
        ALIGN
        &       next$l - {PC}           ; Size of data area

start$l
        ; Error messages
        =       "EMPl:Unable to use all of the assigned memory."
        =       " There would be more physical memory than virtual memory.", 10
        =       "EMLk:Unable to release any memory."
        =       " All used pages are locked against swapping or removal.", 10
        =       "EMMv:Unable to release any memory."
        =       " Locked virtual memory pages cannot be swapped.", 10
        =       "EDHn:It is not possible to start virtual memory for a"
        =       " dynamic area with a non-intercepted handler routine", 10
        =       "EDFg:The dynamic area flags are unsuitable for use"
        =       " with virtual memory", 10
        =       "EDTb:It is not possible to start virtual memory for a"
        =       " dynamic area with a top bit set address", 10
        =       "EDSz:The current size of the dynamic area is larger"
        =       " than the specified maximum size", 10
        =       "EDMx:Unable to extend virtual memory above the maximum size"
        =       " of the dynamic area", 10
        =       "EDDf:Enlarging the dynamic area with virtual memory enabled"
        =       " would reduce the free disc space below"
        =       " the allowed minimum", 10
        =       "EDLk:Unable to disable virtual memory due to locked pages"
        =       " in dynamic area", 10
        =       "EDLs:Unable to shrink virtual memory due to locked pages"
        =       " at the end of the region to remove", 10
        =       "EDZe:Virtual memory is already minimized", 10
        =       "EDMm:Unable to claim the minimum amount of physical memory"
        =       " required to enable virtual memory", 10
        [       :DEF: demo
        =       "EDEm:That feature is not available in the demo version of"
        =       " Virtualise.", 10
        ]
        =       "EQAv:Module cannot be killed while there is"
        =       " virtual memory active", 10
        =       "EQHi:Module cannot be killed while there are"
        =       " dynamic area handlers that are being intercepted", 10
        =       "EQFa:Module cannot be killed while"
        =       " tasks are being filtered", 10
        =       "EQSm:Module cannot be killed while SWI intercepts"
        =       " are replaced by another task", 10
        =       "EQGw:A Virtualise guard word has been overwritten", 10
        =       "ELNa:The address range does not lie within a dynamic area"
        =       " for which virtual memory is active", 10
        =       "ELCp:Insufficient physical memory available to lock"
        =       " virtual memory pages", 10
        =       "EIVo:This module is only suitable for use on RISC OS 3.5"
        =       " or later", 10
        =       "EIVu:Unknown version of RISC OS", 10
        =       "EIVp:Unrecognised build of RISC OS or incompatible patch", 10
        =       "ESCa:Unable to process command"
        =       " while another is being processed", 10
        =       "ESVa:Command aborted due to virtual memory activity", 10
        =       "ERRv:It is foolish to remove the virtual memory pool", 10
        =       "EOUk:Unknown reason code for MiscOp", 10
        =       "EFMa:It is not possible to claim sufficient memory to reload"
        =       " this task. Try quitting some other applications"
        =       " to free more memory. Click Cancel to leave the task swapped"
        =       " out, or Kill Task to stop the task.", 10
        =       "EFSf:It is not possible to create a suitable swap file for"
        =       " this task; either there is insufficient disc space"
        =       " or Virtualise has been incorrectly configured", 10
        =       "EFPq:A task is currently swapped out. Click Restore to"
        =       " attempt to swap the task back in, Abort to abandon the"
        =       " shutdown, or Shutdown to kill the task and resume the"
        =       " shutdown.", 10

        ; Text for *VirtualStats
        =       "CSH0:Memory page size", 9, 9, "%0 bytes", 10
        =       "CSP0:Physical memory:", 10
        =       "CSP1:    Total memory in machine", 9, "%0 pages", 10
        =       "CSP2:    Reserved for VM", 9, 9, "%0 pages", 10
        =       "CSP3:    Currently in use", 9, 9, "%0 pages", 10
        =       "CSV0:Virtual memory:", 10
        =       "CSV1:    Total VM", 9, 9, 9, "%0 pages", 10
        =       "CSV2:    Locked VM", 9, 9, 9, "%0 pages", 10
        =       "CSA0:There are no dynamic areas"
        =       " with virtual memory active.", 10
        =       "CSA1:There is 1 dynamic area with virtual memory active.", 10
        =       "CSA2:There are %0 dynamic areas"
        =       " with virtual memory active.", 10
        =       "CSD0:Area '%3' (%0) at &%2:", 10
        =       "CSD1:    Reserved address space", 9, "%0 pages", 10
        =       "CSD2:    Logical size", 9, 9, "%0 pages", 10
        =       "CSD3:    Physical size", 9, 9, "%0 pages", 10
        =       "CSD4:    Swap file '%3'", 10
        =       "CSF0:Number of page faults generated"
        =       " by different classes of instruction:", 10
        =       "CSF1:    Single data transfer", 9
        =       "LDR %0", 9, 9, "STR %1", 10
        =       "CSF2:    Block data transfer", 9, 9
        =       "LDM %0", 9, 9, "STM %1", 10
        =       "CSF3:    Coprocessor data transfer", 9
        =       "LDC %0", 9, 9, "STC %1", 10
        =       "CSF4:    Single data swap", 9, 9
        =       "SWP %0", 10
        =       "CSC0:Processor: ", 10
        =       "CSC1:Unrecognised (ARM 610 compatible assumed)", 10
        =       "CSC2:Unrecognised (StrongARM compatible assumed)", 10
        =       "CSC3:ARM 600", 10
        =       "CSC4:ARM 610", 10
        =       "CSC5:ARM 700", 10
        =       "CSC6:ARM 710", 10
        =       "CSC7:ARM 810", 10
        =       "CSC8:StrongARM", 10

        ; Text for *VirtualPages
        =       "CPHd:Index  Page      Area Offset"
        =       " Locks   Original   Current Prot  Accesses", 10
        =       "CPL0:%0%1%2%3", 10
        =       "CPL1:%0  &%1 &%2    %3", 10
        =       "CPL2: &%0", 10
        =       "CPU0:         -", 10
        =       "CPU1:      -", 10

        ; Text for *VirtualCheck
        =       "CCOk:Internal consistency check passed.", 10
        =       "CCFl:An internal inconsistency was found."
        =       " Save any work and reset the computer as soon as possible.", 10
end$l
        ALIGN
next$l  &       0                       ; End of list of files
        ALIGN

; Error blocks for the different error tokens

error_number\
        DefA    error_base              ; The first error number

        ;   Syntax      : [<label>] ErrBlck <token>
        ;   Parameters  : label - An optional program label.
        ;                 token - The token string that references the error,
        ;                         or the error text if it will not be passed
        ;                         though MessageTrans.
        ;   Description : Construct an error block for the specified token,
        ;                 unsing the next sequential error number.
        MACRO
$label  ErrBlck $token
$label  &       error_number            ; The error number
        =       "$token", 0             ; Null terminated token
        ALIGN                           ; Back to word aligned
error_number\
        SETA    error_number + 1        ; Increment the error number
        MEND

        ; The actual error blocks
err_mpl ErrBlck "EMPl"
err_mlk ErrBlck "EMLk"
err_mmv ErrBlck "EMMv"
err_dhn ErrBlck "EDHn"
err_dfg ErrBlck "EDFg"
err_dtb ErrBlck "EDTb"
err_dsz ErrBlck "EDSz"
err_dmx ErrBlck "EDMx"
err_ddf ErrBlck "EDDf"
err_dlk ErrBlck "EDLk"
err_dls ErrBlck "EDLs"
err_dze ErrBlck "EDZe"
err_dmm ErrBlck "EDMm"
err_qav ErrBlck "EQAv"
err_qhi ErrBlck "EQHi"
err_qfa ErrBlck "EQFa"
err_qsm ErrBlck "EQSm"
err_qgw ErrBlck "EQGw"
err_lna ErrBlck "ELNa"
err_lcp ErrBlck "ELCp"
err_ivo ErrBlck "EIVo"
err_ivu ErrBlck "EIVu"
err_ivp ErrBlck "EIVp"
err_sca ErrBlck "ESCa"
err_sva ErrBlck "ESVa"
err_rrv ErrBlck "ERRv"
err_ouk ErrBlck "EOUk"
err_fma ErrBlck "EFMa"
err_fsf ErrBlck "EFSf"
err_fpq ErrBlck "EFPq"
        [       :DEF: demo
err_dem ErrBlck "EDEm"
        ]

; Help and command keyword table

commands
        ; Entry for help text
        =       "Virtualise", 0         ; Keyword
        ALIGN
        &       0                       ; No code
        &       0                       ; Flags for a help keyword
        &       0                       ; No syntax message
        &       help_virtualise - base  ; Offset to help text

        ; Entry for command to perform an internal check
        =       "VirtualCheck", 0       ; Command name
        ALIGN
        &       command_check - base    ; Offset to code
        &       0                       ; Information word
        &       syntax_check - base     ; Offset to syntax message
        &       help_check - base       ; Offset to help text

        ; Entry for command to list physical and logical pages
        =       "VirtualPages", 0       ; Command name
        ALIGN
        &       command_pages - base    ; Offset to code
        &       0                       ; Information word
        &       syntax_pages - base     ; Offset to syntax message
        &       help_pages - base       ; Offset to help text

        ; Entry for command to display interesting statistics
        =       "VirtualStats", 0       ; Command name
        ALIGN
        &       command_stats - base    ; Offset to code
        &       0                       ; Information word
        &       syntax_stats - base     ; Offset to syntax message
        &       help_stats - base       ; Offset to help text

        ;
        =       "Desktop_Virtualise", 0 ; Command name
        ALIGN
        &       command_desktop - base  ; Offset to code
        &       0                       ; Information word
        &       syntax_desktop - base   ; Offset to syntax message
        &       help_desktop - base     ; Offset to help text

        [       debug

        ; Entry for command to list debugging records
        =       "VirtualDebug", 0       ; Command name
        ALIGN
        &       command_debug - base    ; Offset to code
        &       0                       ; Information word
        &       syntax_debug - base     ; Offset to syntax message
        &       help_debug - base       ; Offset to help text

        ; Entry for command to start adding debugging records
        =       "VirtualDebugStart", 0  ; Command name
        ALIGN
        &       command_debug_start - base; Offset to code
        &       0                       ; Information word
        &       syntax_debug_start - base; Offset to syntax message
        &       help_debug_start - base ; Offset to help text

        ; Entry for command to stop adding debugging records
        =       "VirtualDebugStop", 0   ; Command name
        ALIGN
        &       command_debug_stop - base; Offset to code
        &       0                       ; Information word
        &       syntax_debug_stop - base; Offset to syntax message
        &       help_debug_stop - base  ; Offset to help text

        ]

        ; End of the help and command keyword table
        =       0
        ALIGN

; Help text on the module

        ; Help text for the module
help_virtualise
        =       "Virtualise is a virtual memory manager for Risc PC dynamic"
        =       " areas. It allows many programs to use virtual memory without"
        =       " requiring any modifications. Please see the documentation"
        =       " for more details."
        [       :DEF: demo
        =       13, 13
        =       "This is a demo version which is restricted to work with"
        =       " suitable demo copies of ProArtisan 24 and Composition only."
        ]
        =       0
        ALIGN

; Initialisation entry point

        ;   Parameters  : r10   - Pointer to environment string.
        ;                 r11   - Instantiation number.
        ;                 r12   - Pointer to module private word.
        ;                 r13   - Supervisor stack pointer.
        ;   Returns     : r7-r11    - Preserved.
        ;                 r13       - Preserved.
        ;   Description : Initialisation entry point handler.
initialisation
        LocalLabels
        JSR     "r7-r11"                ; Stack registers
        MOV     r0, #OSModule_Alloc     ; Claim entry code
        LDR     r3, = ws_end - ws_start ; Required size
        SWI     XOS_Module              ; Claim workspace
        RTE VS                          ; Exit if error
        STR     r2, [r12]               ; Store workspace pointer
        ADRL    r0, workspace_ptr       ; Pointer to workspace pointer
        STR     r2, [r0]                ; Store in module also
        MOV     r12, r2                 ; Copy workspace pointer
        [       debug
        BL      debug_init              ; Initialise debugging code
        ]
        [       pipe
        BL      pipe_open               ; Open the output pipe
        ]
        BL      initialise_intercepts   ; Initialise handler intercepts
        MOVVS   r11, r0                 ; Copy error pointer
        BVS     fail_intercepts$l       ; Fail if unable to initialise
        BL      create_resource_files   ; Register resource files
        MOVVS   r11, r0                 ; Copy error pointer
        BVS     fail_files$l            ; Fail if unable to create messages
        BL      open_messages           ; Open messages file
        MOVVS   r11, r0                 ; Copy error pointer
        BVS     fail_messages$l         ; Fail if unable to open messages
        BL      os_version$l            ; Check OS version number
        MOVVS   r11, r0                 ; Copy error pointer
        BVS     fail_version$l          ; Fail if unsuitable version
        BL      processor$l             ; Check processor type
        MOVVS   r11, r0                 ; Copy error pointer
        BVS     fail_processor$l        ; Fail if unable to recognise
        BL      create_dynamic          ; Create the physical dynamic area
        MOVVS   r11, r0                 ; Copy error pointer
        BVS     fail_dynamic$l          ; Fail if unable to create area
        BL      claim_fs_vectors        ; Claim file system vectors
        MOVVS   r11, r0                 ; Copy error pointer
        BVS     fail_vectors$l          ; Fail if unable to claim vectors
        BL      initialise_filters      ; Initialise filters
        MOVVS   r11, r0                 ; Copy error pointer
        BVS     fail_filters$l          ; Fail if unable to initialise filters
        BL      claim_swis              ; Install SWI intercepts
        MOVVS   r11, r0                 ; Copy error pointer
        BVS     fail_swis$l             ; Fail if unable to install SWIs
        BL      install_handler         ; Install Data abort handler
        MOVVS   r11, r0                 ; Copy error pointer
        BVS     fail_handler$l          ; Fail if unable to install handler
        BL      read_configuration      ; Read initial configuration
        RTSS                            ; Exit, pulling registers

        ;   Parameters  : None
        ;   Returns     : None
        ;   Description : Obtain and check OS version number.
os_version$l
        JSR     "r0-r4"                 ; Stack registers
        MOV     r0, #OSByte_InKey       ; Reason code to read RISC OS version
        MOV     r1, #0                  ; X value to read version
        MOV     r2, #&FF                ; Y value to read version
        SWI     XOS_Byte                ; Read OS version identifier
        RTE VS                          ; Exit if error
        CMP     r1, #&A5                ; Is it RISC OS 3.5
        ADRLTL  r0, err_ivo             ; Pointer to error block
        BLT     os_version_error$l      ; Return with error if too old
        BEQ     os_version_3v5          ; Jump to code for RISC OS version 3.5
        TEQ     r1, #&A6                ; Is it RISC OS 3.6
        BEQ     os_version_3v6          ; Jump to code for RISC OS version 3.6
        TEQ     r1, #&A7                ; Is it RISC OS 3.7
        BEQ     os_version_3v7          ; Jump to code for RISC OS version 3.7
        TEQ     r1, #&A8                ; Is it RISC OS 4.0
        BEQ     os_version_4v0          ; Jump to code for RISC OS version 4.0
        ADRL    r0, err_ivu             ; Pointer to error block
os_version_error$l
        ADR     r1, ws_message          ; Pointer to messages control block
        MOV     r2, #0                  ; Use internal buffer
        SWI     XMessageTrans_ErrorLookup; Lookup the error text
        RTE                             ; Return from subroutine
os_version_3v5
os_version_3v6
os_version_3v7
os_version_4v0
        LDR     r0, = &56C              ; Address of memory size in OS workspace
        STR     r0, ws_os_memory_size   ; Store pointer to memory size
        LDR     r0, [r0]                ; Get initial memory size
        STR     r0, ws_os_memory_init   ; Store initial memory size
        STR     r0, ws_os_memory_limit  ; It is also default size for -1
        LDR     r0, = &1F033FC          ; Pointer to internal SWI table
        STR     r0, ws_os_swi_table     ; Store pointer into RISC OS SWI table
        MOV     r1, #256                ; Number of SWIs to try
os_version_loop$l
        SUBS    r1, r1, #1              ; Decrement SWI number
        ADRMIL  r0, err_ivp             ; Pointer to error block if failed
        BMI     os_version_error$l      ; Return with error if unsuccessful
        LDR     r2, [r0, r1, LSL#2]     ; Get current SWI handler address
        LDR     r3, [r2], #4            ; Read first instruction of handler
        LDR     r4, = &FFFF0000         ; Instruction type mask
        AND     r3, r3, r4              ; Extract the instruction type
        LDR     r4, = &E92D0000         ; Instruction type required
        TEQ     r3, r4                  ; Check if push registers on stack
        BNE     os_version_loop$l       ; Try next SWI handler if failed
        LDR     r3, [r2], #4            ; Read second instruction of handler
        AND     r3, r3, #&FF000000      ; Extract the instruction type
        TEQ     r3, #&EB000000          ; Check if branch and link
        BNE     os_version_loop$l       ; Try next SWI handler if failed
        LDR     r3, [r2], #4            ; Read third instruction of handler
        LDR     r4, = &FFFF0000         ; Instruction type mask
        AND     r3, r3, r4              ; Extract the instruction type
        LDR     r4, = &E8BD0000         ; Instruction type required
        TEQ     r3, r4                  ; Check if pull registers from stack
        BNE     os_version_loop$l       ; Try next SWI handler if failed
        LDR     r3, [r2], #4            ; Read fourth instruction of handler
        LDR     r4, = &638EE201         ; Instruction type required
        TEQ     r3, r4                  ; Check if conditional set overflow flag
        BNE     os_version_loop$l       ; Try next SWI handler if failed
        LDR     r3, [r2], #8            ; Read fifth instruction of handler
        AND     r4, r3, #&FF000000      ; Clear the offset
        TEQ     r4, #&EA000000          ; Check if unconditional branch
        BNE     os_version_loop$l       ; Try next SWI handler if failed
        MOV     r3, r3, LSL#8           ; Mask out all except offset
        ADD     r0, r2, r3, ASR#6       ; Sign extend and shift left by 2 bits
        STR     r0, ws_os_swi_exit      ; Store pointer to return address
        RTS                             ; Return from subroutine

        ;   Parameters  : None
        ;   Returns     : None
        ;   Description : Attempt to recognise the type of processor.
processor$l
        JSR     "r0-r3"                 ; Stack registers
        MOV     r0, #0                  ; Flags and reason code zero
        SWI     XOS_PlatformFeatures    ; Read platform details
        MOV     r2, #processor_unknown  ; Start by assuming unrecognised
        MOV     r3, #processor_writeback; Default flags
        BVS     processor_done$l        ; Skip the next bit if failed
        TST     r0, #OS_PlatformFullEarlyDataAborts; Is full early timing used
        BEQ     processor_done$l        ; Skip the next bit if not
        MOV     r2, #processor_unknownsa; Start by assuming unrecognised
        MOV     r3, #0                  ; StrongARM compatible flags
processor_done$l
        MRC     p15, 0, r0, c0, c0      ; Read the processor identity
        TST     r0, #&f000              ; Is it an old processor
        MOVEQ   r0, r0, LSL#4           ; Shift ID around to be tidy
        AND     r0, r0, #&ff00          ; Get the required bits
        TEQ     r0, #&6000              ; Identity for ARM 600
        MOVEQ   r2, #processor_arm600   ; Set number if matched
        TEQ     r0, #&6100              ; Identity for ARM 610
        MOVEQ   r2, #processor_arm610   ; Set number if matched
        TEQ     r0, #&7000              ; Identity for ARM 700
        MOVEQ   r2, #processor_arm700   ; Set number if matched
        TEQ     r0, #&7100              ; Identity for ARM 710
        MOVEQ   r2, #processor_arm710   ; Set number if matched
        TEQ     r0, #&8100              ; Identity for ARM 810
        MOVEQ   r2, #processor_arm810   ; Set number if matched
        MOVEQ   r3, #0                  ; Special flags for ARM 810
        TEQ     r0, #&A100              ; Identity for StrongARM
        MOVEQ   r2, #processor_strongarm; Set number if matched
        MOVEQ   r3, #0                  ; Special flags for StrongARM
        STR     r2, ws_processor        ; Store processor type
        ADRL    r0, processor_flags     ; Pointer to flag store
        STR     r3, [r0]                ; Store processor specific flags
        RTSS                            ; Return from subroutine

; Finalisation entry point

        ;   Parameters  : r10   - Fatality indication: 0 is non-fatal,
        ;                         1 is fatal.
        ;                 r11   - Instantiation number.
        ;                 r12   - Pointer to module private word.
        ;                 r13   - Supervisor stack.
        ;   Returns     : r7-r11    - Preserved.
        ;                 r13       - Preserved.
        ;   Description : Finalisation entry point handler. This is called
        ;                 before killing the module.
finalisation
        JSR     "r7-r11"                ; Stack registers
        LDR     r12, [r12]              ; Get workspace pointer
        MOV     r11, #0                 ; Clear error pointer
        LDR     r0, ws_virtual_areas    ; Get number of active areas
        TEQ     r0, #0                  ; Test if there are any active areas
        ADRNEL  r0, err_qav             ; Pointer to error block
        BNE     error$l                 ; Produce error if active
        LDR     r0, ws_handler_ptr      ; Are any handlers intercepted
        TEQ     r0, #0                  ; Is the pointer valid
        ADRNEL  r0, err_qhi             ; Pointer to error block
        BNE     error$l                 ; Produce error if intercepts active
        LDR     r0, ws_filter_ptr       ; Are any tasks filtered
        TEQ     r0, #0                  ; Is the pointer valid
        ADRNEL  r0, err_qfa             ; Pointer to error block
        BNE     error$l                 ; Produce error if tasks filtered
        BL      release_check_swis      ; Check if SWI intercepts modified
        RTE VS                          ; Don't die if can't release
        BL      release_handler         ; Release the Data abort handler
        RTE VS                          ; Don't die if can't release
fail_handler$l
        BL      release_swis            ; Restore previous SWI handlers
fail_swis$l
        BL      finalise_filters        ; Finalise filter handling
fail_filters$l
        BL      release_fs_vectors      ; Release file system vectors
fail_vectors$l
        BL      destroy_dynamic         ; Destroy the physical dynamic area
fail_dynamic$l
fail_processor$l
fail_version$l
        BL      close_messages          ; Close messages file
fail_messages$l
        BL      destroy_resource_files  ; Delete the messages files
fail_files$l
fail_intercepts$l
        [       pipe
        BL      pipe_close              ; Close the output pipe
        ]
        MOV     r0, #OSModule_Free      ; Free entry code
        MOV     r2, r12                 ; Pointer to workspace
        SWI     XOS_Module              ; Release workspace
        TEQ     r11, #0                 ; Is there any error
        RTSS EQ                         ; Exit if not
        MOV     r0, r11                 ; Copy error pointer
        SetV                            ; Set the error flag
        RTE                             ; Return with the error

        ; Return an error
error$l ADR     r1, ws_message          ; Pointer to messages control block
        MOV     r2, #0                  ; Use internal buffer
        SWI     XMessageTrans_ErrorLookup; Lookup the error text
        MOV     r3, #0                  ; Ensure no memory moved
        RTE                             ; Return from subroutine

; A literal pool

        LTORG

; Service call handler

        ;   Parameters  : r1    - Service number.
        ;                 r12   - Pointer to module private word.
        ;                 r13   - A full, descending stack.
        ;   Returns     : r0    - May be used to pass back a result.
        ;                 r1    - Set to zero if the service is being claimed.
        ;                 r2-r8 - May be used to pass back a result.
        ;                 r13   - Preserved.
        ;   Description : Service call handler.
        LocalLabels
        &       table$l - base          ; Service table offset
service MOV     r0, r0                  ; Magic instruction for service table
        TEQ     r1, #Service_FSRedeclare
        TEQNE   r1, #Service_WimpCloseDown
        TEQNE   r1, #Service_ResourceFSStarting
        TEQNE   r1, #Service_ValidateAddress
        TEQNE   r1, #Service_PagesSafe
        TEQNE   r1, #Service_DynamicAreaRemove
        MOVNES  pc, r14                 ; Reject unrecognised calls quickly
dispatch$l
        LDR     r12, [r12]              ; Workspace pointer
        TEQ     r1, #Service_ValidateAddress
        BEQ     validate_address$l
        TEQ     r1, #Service_DynamicAreaRemove
        BEQ     dynamic_area_remove$l
        TEQ     r1, #Service_ResourceFSStarting
        BEQ     resourcefs_starting$l
        TEQ     r1, #Service_FSRedeclare
        BEQ     fs_redeclare$l
        TEQ     r1, #Service_PagesSafe
        BEQ     pages_safe$l
        TEQ     r1, #Service_WimpCloseDown
        BEQ     wimp_close_down$l
        MOVS    pc, r14                 ; Exit if this point reached

        ; Service call table (codes must be in ascending numerical order)
table$l &       0                       ; Default flags
        &       dispatch$l - base       ; Offset to handler code
        &       Service_FSRedeclare
        &       Service_WimpCloseDown
        &       Service_ResourceFSStarting
        &       Service_ValidateAddress
        &       Service_PagesSafe
        &       Service_DynamicAreaRemove
        &       0                       ; End of list of service calls

        ; OS_ValidateAddress has been called with an unrecognised area
validate_address$l
;        debug_record "validate_address"
        JSR     "r0, r2-r4"             ; Stack registers
        MOV     r4, r1                  ; Copy reason code
        MOV     r1, r2                  ; Copy start pointer
        TEQ     r2, r3                  ; Is it a single address
        SUBNE   r2, r3, #1              ; Make end inclusive if not
        BL      find_area               ; Search for address range
        TEQ     r0, #0                  ; Was the address range found
        MOVEQ   r1, r4                  ; Restore reason code if not found
        MOVNE   r1, #0                  ; Clear reason code if found
        RTSS                            ; Exit, pulling registers

        ; A dynamic area is about to be removed
dynamic_area_remove$l
        JSR     "r0"                    ; Stack registers
        MOV     r0, r2                  ; Copy dynamic area number
        BL      destroy_virtualise      ; Force the end of virtual memory
        BL      remove_intercept        ; Remove any handler intercept
        RTSS                            ; Exit, pulling registers

        ; ResourceFS module is reloaded or reinitialised
resourcefs_starting$l
        JSR     "r0-r2"                 ; Stack registers
        ADRL    r0, resources           ; Pointer to resource files
        MOV     r14, pc                 ; Copy return address
        MOV     pc, r2                  ; Call ResourceFS routine
        RTSS                            ; Exit, pulling registers

        ; Fileswitch module has been reinitialised
fs_redeclare$l
        JSR     ""                      ; Stack registers
        BL      claim_fs_vectors        ; Reclaim the filesystem vectors
        RTSS                            ; Exit, pulling registers

        ; Pages specified have been swapped for different pages
pages_safe$l
        JSR     "r0-r1, r5-r8"          ; Stack registers
        LDR     r0, ws_source_array     ; Pointer to array of pages
        LDR     r1, ws_total_pages      ; Number of pages claimed
        MOV     r5, #source_item        ; Size of each record
        MLA     r1, r5, r1, r0          ; Pointer to end of array
pages_safe_loop$l
        CMP     r0, r1                  ; Check if end of array reached
        RTSS GE                         ; Exit if all checked
        LDR     r5, [r0, #source_number]; Get the number of this page
        MOV     r6, r2                  ; Copy number of entries in page block
        MOV     r7, #0                  ; Initial page block offset
pages_safe_check$l
        SUBS    r6, r6, #1              ; Decrement number of pages to check
        BMI     pages_safe_next$l       ; Exit loop if all checked
        ADD     r8, r3, r7              ; Pointer to before page block
        LDR     r8, [r8, #OS_PageBlock_page_no]; Get original page number
        TEQ     r5, r8                  ; Check page number
        BNE     pages_safe_not$l        ; Skip next bit if not a match
        ADD     r8, r4, r7              ; Pointer to after page block
        LDR     r8, [r8, #OS_PageBlock_page_no]; Get new page number
        STR     r8, [r0, #source_number]; Correct stored page number
        B       pages_safe_next$l       ; No point checking more if matched
pages_safe_not$l
        ADD     r7, r7, #OS_PageBlock
        B       pages_safe_check$l      ; Loop to check next page
pages_safe_next$l
        ADD     r0, r0, #source_item    ; Advance to next page
        B       pages_safe_loop$l       ; Check next page

        ; The window manager is about to close down a task
wimp_close_down$l
        JSR     "r0"                    ; Stack registers
        MOV     r0, r2                  ; Copy task handle
        BL      filter_quit             ; Remove any filters attached to task
        RTSS                            ; Exit, pulling registers

; SWI handler and decoding

        ;   Parameters  : r0-r8 - Passed from SWI caller.
        ;                 r11   - SWI number modulo chunk size (i.e. 0 to 63).
        ;                 r12   - Pointer to module private word.
        ;                 r13   - A full, descending stack.
        ;                 r14   - Contains flags of SWI caller, with V cleared.
        ;   Returns     : r0-r9 - Returned to SWI caller.
        ;                 r10-r12 - Corrupted.
        ;                 r13   - Preserved.
        ;   Description : Handler for SWIs belonging to this module.
swi_handler
        LocalLabels
        LDR     r12, [r12]              ; Get pointer to workspace
        CMP     r11, #(jump_end$l - jump$l) >> 2; Is it in range
        ADDLO   pc, pc, r11, LSL#2      ; Dispatch if in range
        B       unknown$l               ; Unknown SWI
jump$l  B       configure               ; Jump to handler for 0th SWI
        B       start_virtualise        ; Jump to handler for 1st SWI
        B       end_virtualise          ; Jump to handler for 2nd SWI
        B       lock_region             ; Jump to handler for 3rd SWI
        B       unlock_region           ; Jump to handler for 4th SWI
        B       misc_op                 ; Jump to handler for 5th SWI
        B       user_configure          ; Jump to handler for 6th SWI
jump_end$l

        ; Generate an error if the SWI doesn't exist
unknown$l
        JSR     ""                      ; Stack registers
        ADR     r0, err_unknown$l       ; Pointer to error block
        MOV     r1, #0                  ; Use global messages
        MOV     r2, #0                  ; Use an internal buffer
        ADRL    r4, title               ; Name of module
        SWI     XMessageTrans_ErrorLookup; Lookup the error text
        RTE                             ; Exit without wiping over r0

        ; The error block for an unkown SWI
err_unknown$l
        &       Error_NoSuchSWI         ; Error number same as system message
        =       "BadSWI", 0             ; Token to lookup
        ALIGN

        ; SWI decoding table
swi_table
        =       "Virtualise", 0         ; SWI group prefix
        =       "Configure", 0          ; Name of 0th SWI
        =       "Start", 0              ; Name of 1st SWI
        =       "End", 0                ; Name of 2nd SWI
        =       "Lock", 0               ; Name of 3rd SWI
        =       "Unlock", 0             ; Name of 4th SWI
        =       "MiscOp", 0             ; Name of 5th SWI
        =       "UserConfigure", 0      ; Name of 6th SWI
        =       0                       ; End of decoding table
        ALIGN

; Command *VirtualStats

        ;   Parameters  : r0    - Pointer to the command tail.
        ;                 r1    - Number of parameters. This is undefined
        ;                         for a configuration keyword.
        ;                 r12   - Pointer to module private word.
        ;   Returns     : r7-r11    - Preserved.
        ;   Description : Process the command *VirtualStats.
command_stats
        LocalLabels
        JSR     "r7-r11"                ; Stack registers
        LDR     r12, [r12]              ; Get pointer to workspace
        ADR     r0, ws_command_flag     ; Get pointer to the command flag
        MOV     r1, #1                  ; Value to set flag with
        SWP     r1, r1, [r0]            ; Swap new value with old value
        TEQ     r1, #0                  ; Was old value suitable
        ADRNEL  r0, err_sca             ; Pointer to error block
        BNE     error$l                 ; Return error
        ADR     r0, tokens$l            ; Pointer to the start of the tokens
        MOV     r4, #0                  ; Value to enable justification
        LDR     r1, ws_page_size        ; Get the page size in bytes
        BL      show$l                  ; Process and display line
        BVS     tidy$l                  ; Exit if an error produced
        SWI     XOS_NewLine             ; Write a newline
        BVS     tidy$l                  ; Exit if an error produced
        BL      show$l                  ; Process and display line
        BVS     tidy$l                  ; Exit if an error produced
        LDR     r1, ws_max_pages        ; Get maximum number of pages
        BL      show$l                  ; Process and display line
        BVS     tidy$l                  ; Exit if an error produced
        LDR     r1, ws_total_pages      ; Get current number of pages
        BL      show$l                  ; Process and display line
        BVS     tidy$l                  ; Exit if an error produced
        LDR     r1, ws_used_pages       ; Get number of used pages
        BL      show$l                  ; Process and display line
        BVS     tidy$l                  ; Exit if an error produced
        SWI     XOS_NewLine             ; Write a newline
        BVS     tidy$l                  ; Exit if an error produced
        BL      show$l                  ; Process and display line
        BVS     tidy$l                  ; Exit if an error produced
        LDR     r1, ws_virtual_pages    ; Get number of virtual pages
        BL      show$l                  ; Process and display line
        BVS     tidy$l                  ; Exit if an error produced
        LDR     r1, ws_locked_pages     ; Get number of locked pages
        BL      show$l                  ; Process and display line
        BVS     tidy$l                  ; Exit if an error produced
        SWI     XOS_NewLine             ; Write a newline
        BVS     tidy$l                  ; Exit if an error produced
        MOV     r4, #1                  ; Value to disable justification
        LDR     r1, ws_virtual_areas    ; Get number of virtual dynamic areas
        CMP     r1, #1                  ; How many compare to unity
        ADRLTL  r0, token_none$l        ; Get pointer to token for 0
        ADREQL  r0, token_one$l         ; Get pointer to token for 1
        ADRGTL  r0, token_many$l        ; Get pointer to token for >1
        BL      show$l                  ; Process and display line
        BVS     tidy$l                  ; Exit if an error produced
        MOV     r4, #0                  ; Value to enable justification
        SWI     XOS_NewLine             ; Write a newline
        BVS     tidy$l                  ; Exit if an error produced
        LDR     r5, ws_area_ptr         ; Pointer to first virtual area
loop$l  TEQ     r5, #0                  ; Any areas not listed
        BEQ     done$l                  ; Exit loop if finished
        LDR     r1, [r5, #area_number]  ; Get dynamic area number
        LDR     r3, [r5, #area_base]    ; Get dynamic area base address
        BL      name$l                  ; Get dynamic area name
        BVS     tidy$l                  ; Exit if an error produced
        ADRL    r0, token_loop$l        ; Get pointer to correct token
        BL      show$l                  ; Process and display line
        BVS     tidy$l                  ; Exit if an error produced
        MOV     r4, #0                  ; Value to enable justification
        LDR     r1, [r5, #area_max_pages]; Get maximum number of pages
        BL      show$l                  ; Process and display line
        BVS     tidy$l                  ; Exit if an error produced
        LDR     r1, [r5, #area_total_pages]; Get current number of pages
        BL      show$l                  ; Process and display line
        BVS     tidy$l                  ; Exit if an error produced
        LDR     r1, [r5, #area_used_pages]; Get currently used of pages
        BL      show$l                  ; Process and display line
        BVS     tidy$l                  ; Exit if an error produced
        ADD     r4, r5, #area_file_name ; Get swap file name pointer
        BL      show$l                  ; Process and display line
        BVS     tidy$l                  ; Exit if an error produced
        MOV     r4, #0                  ; Value to enable justification
        SWI     XOS_NewLine             ; Write a newline
        BVS     tidy$l                  ; Exit if an error produced
        LDR     r5, [r5, #area_next]    ; Get pointer to next area
        B       loop$l                  ; Loop for next area
done$l  ADRL    r0, token_done$l        ; Get pointer to next token
        BL      show$l                  ; Process and display line
        BVS     tidy$l                  ; Exit if an error produced
        LDR     r1, ws_faults_ldr       ; Number of faults caused by LDRs
        LDR     r2, ws_faults_str       ; Number of faults caused by STRs
        BL      show$l                  ; Process and display line
        BVS     tidy$l                  ; Exit if an error produced
        LDR     r1, ws_faults_ldm       ; Number of faults caused by LDMs
        LDR     r2, ws_faults_stm       ; Number of faults caused by STMs
        BL      show$l                  ; Process and display line
        BVS     tidy$l                  ; Exit if an error produced
        LDR     r1, ws_faults_ldc       ; Number of faults caused by LDCs
        LDR     r2, ws_faults_stc       ; Number of faults caused by STCs
        BL      show$l                  ; Process and display line
        BVS     tidy$l                  ; Exit if an error produced
        LDR     r1, ws_faults_swp       ; Number of faults caused by SWPs
        BL      show$l                  ; Process and display line
        BVS     tidy$l                  ; Exit if an error produced
        SWI     XOS_NewLine             ; Write a newline
        BVS     tidy$l                  ; Exit if an error produced
        ADR     r0, ws_message          ; Pointer to messages control block
        ADRL    r1, token_proc$l        ; Pointer to token
        ADRL    r2, ws_string           ; Pointer to result buffer
        MOV     r3, #String             ; Size of buffer
        SWI     XMessageTrans_Lookup    ; Translate the token into a string
        BVS     tidy$l                  ; Exit if an error produced
        MOV     r0, r2                  ; Copy pointer to result string
        MOV     r1, #0                  ; Use RISC OS dictionary
        MOV     r2, #0                  ; No string to substitute
        SWI     XOS_PrettyPrint         ; Output the resulting text
        BVS     tidy$l                  ; Exit if an error produced
        LDR     r0, ws_processor        ; Get the processor type
        ADRL    r1, token_proc_unknown$l; Start with the unknown type
        TEQ     r0, #processor_unknownsa; Is it a StrongARM compatible
        ADREQL  r1, token_proc_unknownsa$l; Pointer to the relevant token
        TEQ     r0, #processor_arm600   ; Is it an ARM600
        ADREQL  r1, token_proc_arm600$l ; Pointer to the relevant token
        TEQ     r0, #processor_arm610   ; Is it an ARM610
        ADREQL  r1, token_proc_arm610$l ; Pointer to the relevant token
        TEQ     r0, #processor_arm700   ; Is it an ARM700
        ADREQL  r1, token_proc_arm700$l ; Pointer to the relevant token
        TEQ     r0, #processor_arm710   ; Is it an ARM710
        ADREQL  r1, token_proc_arm710$l ; Pointer to the relevant token
        TEQ     r0, #processor_arm810   ; Is it an ARM810
        ADREQL  r1, token_proc_arm810$l ; Pointer to the relevant token
        TEQ     r0, #processor_strongarm; Is it a StrongARM
        ADREQL  r1, token_proc_strongarm$l; Pointer to the relevant token
        ADR     r0, ws_message          ; Pointer to messages control block
        ADRL    r2, ws_string           ; Pointer to result buffer
        MOV     r3, #String             ; Size of buffer
        SWI     XMessageTrans_Lookup    ; Translate the token into a string
        BVS     tidy$l                  ; Exit if an error produced
        MOV     r0, r2                  ; Copy pointer to result string
        MOV     r1, #0                  ; Use RISC OS dictionary
        MOV     r2, #0                  ; No string to substitute
        SWI     XOS_PrettyPrint         ; Output the resulting text
        BVS     tidy$l                  ; Exit if an error produced
        SWI     XOS_NewLine             ; Write a newline
        BVS     tidy$l                  ; Exit if an error produced
        MOV     r1, #0                  ; Value to clear flag with
        STR     r1, ws_command_flag     ; Clear command flag
        RTS                             ; Return from subroutine

        ; Return if error produced
tidy$l  MOV     r1, #0                  ; Value to clear flag with
        STR     r1, ws_command_flag     ; Clear flag if set
        RTE                             ; Return with error

        ; Generate an error message and return
error$l ADR     r1, ws_message          ; Pointer to messages control block
        MOV     r2, #0                  ; Use internal buffer
        SWI     XMessageTrans_ErrorLookup; Lookup the error text
        RTE                             ; Return from subroutine

        ; Process and display a line of output
show$l  JSR     "r1-r7"                 ; Stack registers
        MOV     r7, r4                  ; Copy %3 value
        MOV     r5, r3                  ; Copy %2 value
        MOV     r4, r2                  ; Copy %1 value
        MOV     r3, r0                  ; Copy token pointer
        LDR     r0, ws_command_flag     ; Get the command flag
        TEQ     r0, #1                  ; Is it the correct value
        BNE     fail$l                  ; Generate error if not
        MOV     r0, r1                  ; Copy number to convert
        ADRL    r1, ws_num_buffer1      ; Get pointer to buffer
        MOV     r2, #OS_Cardinal4Limit + 1; Size of buffer
        SWI     XOS_ConvertCardinal4    ; Convert the first number
        RTE VS                          ; Exit if an error produced
        TEQ     r7, #0                  ; Should number be justified
        BLEQ    right_justify           ; Right justify the number
        MOV     r0, r4                  ; Copy number to convert
        ADRL    r1, ws_num_buffer2      ; Get pointer to buffer
        MOV     r2, #OS_Cardinal4Limit + 1; Size of buffer
        SWI     XOS_ConvertCardinal4    ; Convert the second number
        RTE VS                          ; Exit if an error produced
        BL      right_justify           ; Right justify the number
        MOV     r0, r5                  ; Copy number to convert
        ADRL    r1, ws_num_buffer3      ; Get pointer to buffer
        MOV     r2, #NumberBuffer       ; Size of buffer
        SWI     XOS_ConvertHex8         ; Convert the third number
        RTE VS                          ; Exit if an error produced
        ADR     r0, ws_message          ; Pointer to messages control block
        MOV     r1, r3                  ; Copy token pointer
        ADRL    r2, ws_string           ; Pointer to result buffer
        MOV     r3, #String             ; Size of buffer
        ADRL    r4, ws_num_buffer1      ; Get pointer to %0 buffer
        ADRL    r5, ws_num_buffer2      ; Get pointer to %1 buffer
        ADRL    r6, ws_num_buffer3      ; Get pointer to %2 buffer
        SWI     XMessageTrans_Lookup    ; Translate the token into a string
        RTE VS                          ; Exit if an error produced
        MOV     r4, r1                  ; Copy token terminator pointer
        MOV     r0, r2                  ; Copy pointer to result string
        MOV     r1, #0                  ; Use RISC OS dictionary
        MOV     r2, #0                  ; No string to substitute
        SWI     XOS_PrettyPrint         ; Output the resulting text
        RTE VS                          ; Exit if an error produced
        SWI     XOS_NewLine             ; Write a newline
        RTE VS                          ; Exit if an error produced
        ADD     r0, r4, #1              ; Increment token pointer
        RTS                             ; Return from subroutine
fail$l  ADRL    r0, err_sva             ; Pointer to error block
        ADR     r1, ws_message          ; Pointer to messages control block
        MOV     r2, #0                  ; Use internal buffer
        SWI     XMessageTrans_ErrorLookup; Lookup the error text
        RTE                             ; Return from subroutine

        ; Get dynamic area name
name$l  JSR     "r0-r3, r5-r8"          ; Stack registers
        MOV     r0, #OSDynamicArea_Read ; Reason code to read details
        BL      os_dynamic_area         ; Get dynamic area name
        RTE VS                          ; Return if error
        MOV     r4, r8                  ; Copy pointer to name
        RTS                             ; Return from subroutine

        ; The tokens to lookup
tokens$l
        =       "CSH0", 0               ; ws_page_size
        =       "CSP0", 0
        =       "CSP1", 0               ; ws_max_pages
        =       "CSP2", 0               ; ws_total_pages
        =       "CSP3", 0               ; ws_used_pages
        =       "CSV0", 0
        =       "CSV1", 0               ; ws_virtual_pages
        =       "CSV2", 0               ; ws_locked_pages
token_none$l
        =       "CSA0", 0
token_one$l
        =       "CSA1", 0
token_many$l
        =       "CSA2", 0               ; ws_virtual_areas
token_loop$l
        =       "CSD0", 0               ; area_number,, area_base, (name)
        =       "CSD1", 0               ; area_max_pages
        =       "CSD2", 0               ; area_total_pages
        =       "CSD3", 0               ; area_used_pages
        =       "CSD4", 0               ; area_file_name
token_done$l
        =       "CSF0", 0
        =       "CSF1", 0               ; ws_faults_ldr, ws_faults_str
        =       "CSF2", 0               ; ws_faults_ldm, ws_faults_stm
        =       "CSF3", 0               ; ws_faults_ldc, ws_faults_stc
        =       "CSF4", 0               ; ws_faults_swp
token_proc$l
        =       "CSC0", 0               ; ws_processor
token_proc_unknown$l
        =       "CSC1", 0               ; Unrecognised processor
token_proc_unknownsa$l
        =       "CSC2", 0               ; Unrecognised processor (StrongARM)
token_proc_arm600$l
        =       "CSC3", 0               ; ARM600 processor
token_proc_arm610$l
        =       "CSC4", 0               ; ARM610 processor
token_proc_arm700$l
        =       "CSC5", 0               ; ARM700 processor
token_proc_arm710$l
        =       "CSC6", 0               ; ARM710 processor
token_proc_arm810$l
        =       "CSC7", 0               ; ARM810 processor
token_proc_strongarm$l
        =       "CSC8", 0               ; StrongARM processor

        ; Help text for the command
help_stats
        =       "*VirtualStats displays some statistics"
        =       " about the current state of virtual memory.", 13

        ; Syntax message for the command
syntax_stats
        =       "Syntax: *VirtualStats", 0
        ALIGN

; Command *VirtualPages

        ;   Parameters  : r0    - Pointer to the command tail.
        ;                 r1    - Number of parameters. This is undefined
        ;                         for a configuration keyword.
        ;                 r12   - Pointer to module private word.
        ;   Returns     : r7-r11    - Preserved.
        ;   Description : Process the command *VirtualPages.
command_pages
        LocalLabels
        JSR     "r7-r11"                ; Stack registers
        LDR     r12, [r12]              ; Get pointer to workspace
        ADR     r0, ws_command_flag     ; Get pointer to the command flag
        MOV     r1, #1                  ; Value to set flag with
        SWP     r1, r1, [r0]            ; Swap new value with old value
        TEQ     r1, #0                  ; Was old value suitable
        ADRNEL  r0, err_sca             ; Pointer to error block
        BNE     error$l                 ; Return error
        ADR     r0, ws_message          ; Pointer to messages control block
        ADR     r1, head_token$l        ; Get pointer to token for header
        ADRL    r2, ws_string           ; Pointer to result buffer
        MOV     r3, #String             ; Size of buffer
        SWI     XMessageTrans_Lookup    ; Translate the token into a string
        BVS     tidy$l                  ; Exit if an error produced
        MOV     r0, r2                  ; Copy pointer to result string
        MOV     r1, #0                  ; Use RISC OS dictionary
        MOV     r2, #0                  ; No string to substitute
        SWI     XOS_PrettyPrint         ; Output the resulting text
        BVS     tidy$l                  ; Exit if an error produced
        SWI     XOS_NewLine             ; Write a newline
        BVS     tidy$l                  ; Exit if an error produced
        LDR     r11, ws_source_array    ; Get pointer to page array
        LDR     r10, ws_total_pages     ; Current number of pages
        MOV     r9, #0                  ; Number of pages listed
loop$l  CMP     r9, r10                 ; Have all pages been listed
        BGE     exit$l                  ; Exit if all done
        LDR     r0, ws_command_flag     ; Get command status flag
        TEQ     r0, #1                  ; Is it alright to continue
        ADRNEL  r0, err_sva             ; Pointer to error block
        BNE     error$l                 ; Exit with an error if not
        SWI     XOS_ReadEscapeState     ; Check for an escape condition
        BCS     escape$l                ; Branch to escape handler
        MOV     r0, r9                  ; Get page index
        ADRL    r1, ws_num_buffer1      ; Get pointer to %0 buffer
        MOV     r2, #6                  ; Size of buffer
        SWI     XOS_ConvertCardinal4    ; Convert the page index
        BVS     tidy$l                  ; Exit if an error produced
        BL      right_justify           ; Right justify the number
        LDR     r0, [r11, #source_number]; Get real page number
        ADRL    r1, ws_num_buffer2      ; Get pointer to %1 buffer
        MOV     r2, #7                  ; Size of buffer
        SWI     XOS_ConvertCardinal4    ; Convert the page number
        BVS     tidy$l                  ; Exit if an error produced
        BL      right_justify           ; Right justify the number
        LDR     r0, [r11, #source_area] ; Which area is the page in
        TEQ     r0, #0                  ; Is the page assigned
        BEQ     unused$l                ; Use dummy values if not
        LDR     r0, [r0, #area_number]  ; Get the virtual area number
        ADRL    r1, ws_num_buffer3      ; Get pointer to %2 buffer
        MOV     r2, #OS_Cardinal4Limit + 1; Size of buffer
        SWI     XOS_ConvertCardinal4    ; Convert the virtual area number
        BVS     tidy$l                  ; Exit if an error produced
        BL      right_justify           ; Right justify the number
        LDR     r0, [r11, #source_page] ; Get logical page number
        ADRL    r1, ws_num_buffer4      ; Get pointer to %3 buffer
        MOV     r2, #8                  ; Size of buffer
        SWI     XOS_ConvertCardinal4    ; Convert the logical page number
        BVS     tidy$l                  ; Exit if an error produced
        BL      right_justify           ; Right justify the number
        B       done$l                  ; Skip dummy values
unused$l
        ADR     r0, ws_message          ; Pointer to messages control block
        ADRL    r1, unused_token1$l     ; Get pointer to token for unused
        ADRL    r2, ws_num_buffer3      ; Pointer to result buffer
        MOV     r3, #NumberBuffer       ; Size of buffer
        SWI     XMessageTrans_Lookup    ; Translate the token into a string
        BVS     tidy$l                  ; Exit if an error produced
        ADR     r0, ws_message          ; Pointer to messages control block
        ADRL    r1, unused_token2$l     ; Get pointer to token for unused
        ADRL    r2, ws_num_buffer4      ; Pointer to result buffer
        MOV     r3, #NumberBuffer       ; Size of buffer
        SWI     XMessageTrans_Lookup    ; Translate the token into a string
        BVS     tidy$l                  ; Exit if an error produced
done$l  ADR     r0, ws_message          ; Pointer to messages control block
        ADRL    r1, list_token1$l       ; Get pointer to token for first half
        ADRL    r2, ws_string           ; Pointer to result buffer
        MOV     r3, #String             ; Size of buffer
        ADRL    r4, ws_num_buffer1      ; Get pointer to %0 buffer
        ADRL    r5, ws_num_buffer2      ; Get pointer to %1 buffer
        ADRL    r6, ws_num_buffer3      ; Get pointer to %2 buffer
        ADRL    r7, ws_num_buffer4      ; Get pointer to %3 buffer
        SWI     XMessageTrans_Lookup    ; Translate the token into a string
        BVS     tidy$l                  ; Exit if an error produced
        MOV     r0, r2                  ; Copy pointer to result string
        MOV     r1, #0                  ; Use RISC OS dictionary
        MOV     r2, #0                  ; No string to substitute
        SWI     XOS_PrettyPrint         ; Output the resulting text
        BVS     tidy$l                  ; Exit if an error produced
        LDR     r0, [r11, #source_locks]; Get number of times locked
        ADRL    r1, ws_num_buffer1      ; Get pointer to %0 buffer
        MOV     r2, #7                  ; Size of buffer
        SWI     XOS_ConvertCardinal4    ; Convert number of times locked
        BVS     tidy$l                  ; Exit if an error produced
        BL      right_justify           ; Right justify the number
        LDR     r0, [r11, #source_ptr]  ; Get original address
        ADRL    r1, ws_num_buffer2      ; Get pointer to %1 buffer
        MOV     r2, #NumberBuffer       ; Size of buffer
        SWI     XOS_ConvertHex8         ; Convert the original address
        BVS     tidy$l                  ; Exit if an error produced
        ADRL    r0, ws_mem_map_request  ; Pointer to request list
        LDR     r1, [r11, #source_number]; Get the physical page number
        STR     r1, [r0, #OS_MemMapRequest_page_no]; Set the physical page
        MOV     r1, #-1                 ; Request list terminator
        STR     r1, [r0, #OS_MemMapRequest]; Terminate the request list
        SWI     XOS_ReadMemMapEntries   ; Read current address and protection
        BVS     tidy$l                  ; Exit if an error produced
        ADRL    r0, ws_mem_map_request  ; Pointer to request list
        LDR     r0, [r0, #OS_MemMapRequest_map]; Get logical address
        ADRL    r1, ws_num_buffer3      ; Get pointer to %2 buffer
        MOV     r2, #NumberBuffer       ; Size of buffer
        SWI     XOS_ConvertHex8         ; Convert the logical address
        BVS     tidy$l                  ; Exit if an error produced
        ADRL    r0, ws_mem_map_request  ; Pointer to request list
        LDR     r0, [r0, #OS_MemMapRequest_access]; Get protection level
        ADRL    r1, ws_num_buffer4      ; Get pointer to %3 buffer
        MOV     r2, #2                  ; Size of buffer
        SWI     XOS_ConvertCardinal1    ; Convert the protection level
        BVS     tidy$l                  ; Exit if an error produced
        BL      right_justify           ; Right justify the number
        ADR     r0, ws_message          ; Pointer to messages control block
        ADRL    r1, list_token2$l       ; Get pointer to token for other half
        ADRL    r2, ws_string           ; Pointer to result buffer
        MOV     r3, #String             ; Size of buffer
        ADRL    r4, ws_num_buffer1      ; Get pointer to %0 buffer
        ADRL    r5, ws_num_buffer2      ; Get pointer to %1 buffer
        ADRL    r6, ws_num_buffer3      ; Get pointer to %2 buffer
        ADRL    r7, ws_num_buffer4      ; Get pointer to %3 buffer
        SWI     XMessageTrans_Lookup    ; Translate the token into a string
        BVS     tidy$l                  ; Exit if an error produced
        MOV     r0, r2                  ; Copy pointer to result string
        MOV     r1, #0                  ; Use RISC OS dictionary
        MOV     r2, #0                  ; No string to substitute
        SWI     XOS_PrettyPrint         ; Output the resulting text
        BVS     tidy$l                  ; Exit if an error produced
        LDR     r0, [r11, #source_count]; Get the counter
        ADRL    r1, ws_num_buffer1      ; Get pointer to %0 buffer
        MOV     r2, #NumberBuffer       ; Size of buffer
        SWI     XOS_ConvertHex8         ; Convert the logical address
        BVS     tidy$l                  ; Exit if an error produced
        ADR     r0, ws_message          ; Pointer to messages control block
        ADRL    r1, list_token3$l       ; Get pointer to token for third half
        ADRL    r2, ws_string           ; Pointer to result buffer
        MOV     r3, #String             ; Size of buffer
        ADRL    r4, ws_num_buffer1      ; Get pointer to %0 buffer
        SWI     XMessageTrans_Lookup    ; Translate the token into a string
        BVS     tidy$l                  ; Exit if an error produced
        MOV     r0, r2                  ; Copy pointer to result string
        MOV     r1, #0                  ; Use RISC OS dictionary
        MOV     r2, #0                  ; No string to substitute
        SWI     XOS_PrettyPrint         ; Output the resulting text
        BVS     tidy$l                  ; Exit if an error produced
        SWI     XOS_NewLine             ; Write a newline
        BVS     tidy$l                  ; Exit if an error produced
        ADD     r11, r11, #source_item  ; Increment page array pointer
        ADD     r9, r9, #1              ; Increment number of pages
        B       loop$l                  ; Loop for next page
exit$l  MOV     r1, #0                  ; Value to clear flag with
        STR     r1, ws_command_flag     ; Clear flag if set
        RTS                             ; Return from subroutine

        ; Return if error produced
tidy$l  MOV     r1, #0                  ; Value to clear flag with
        STR     r1, ws_command_flag     ; Clear flag if set
        RTE                             ; Return with error

        ; Handle an escape condition
escape$l
        MOV     r0, #OSByte_ClearEscape ; Reason code to clear escape condition
        SWI     XOS_Byte                ; Clear escape condition
        BVS     tidy$l                  ; Exit if an error produced
        SWI     XOS_NewLine             ; Write a newline
        BVS     tidy$l                  ; Exit if an error produced
        ADR     r0, err_esc$l           ; Pointer to error block
        B       error$l                 ; Generate an error

        ; The error block for escape
err_esc$l
        &       Error_Escape            ; Error number same as system message
        =       "Escape", 0             ; Token to lookup
        ALIGN

        ; Generate an error message and return
error$l MOV     r1, #0                  ; Value to clear flag with
        STR     r1, ws_command_flag     ; Clear flag if set
        ADR     r1, ws_message          ; Pointer to messages control block
        MOV     r2, #0                  ; Use internal buffer
        SWI     XMessageTrans_ErrorLookup; Lookup the error text
        RTE                             ; Return from subroutine

        ; The tokens to lookup
head_token$l
        =       "CPHd", 0
list_token1$l
        =       "CPL0", 0               ; source_number, area_number,
                                        ; source_page, source_locks
list_token2$l
        =       "CPL1", 0               ; source_ptr, (logical), (protection)

list_token3$l
        =       "CPL2", 0               ; source_count

unused_token1$l
        =       "CPU0", 0
unused_token2$l
        =       "CPU1", 0

        ; Help text for the command
help_pages
        =       "*VirtualPages lists details of all the physical pages"
        =       " currently assigned to this module. Note that this can"
        =       " be an extremely long list.", 13

        ; Syntax message for the command
syntax_pages
        =       "Syntax: *VirtualPages", 0
        ALIGN

        ;   Parameters  : r0    - Pointer to buffer containing string.
        ;                 r1    - Pointer to terminating null in buffer.
        ;                 r2    - Number of bytes free in buffer
        ;   Returns     : None
        ;   Description : Right justify the string within the buffer. This
        ;                 is suitable for use directly after an OS_Convert
        ;                 SWI call.
right_justify
        LocalLabels
        JSR     "r0-r3"                 ; Stack registers
        ADD     r2, r1, r2              ; Pointer just after end of buffer
copy$l  LDRB    r3, [r1], #-1           ; Read a character
        STRB    r3, [r2, #-1]!          ; Store the character
        CMP     r1, r0                  ; Has start of string been reached
        BGE     copy$l                  ; Copy next character if more
        MOV     r3, #' '                ; Character to pad with
pad$l   CMP     r2, r0                  ; Has start of buffer been reached
        RTS LE                          ; Return when all done
        STRB    r3, [r2, #-1]!          ; Store the padding characted
        B       pad$l                   ; Loop for next character position

; Command *Desktop_Virtualise

        ;   Parameters  : r0    - Pointer to the command tail.
        ;                 r1    - Number of parameters. This is undefined
        ;                         for a configuration keyword.
        ;                 r12   - Pointer to module private word.
        ;   Returns     : r7-r11    - Preserved.
        ;   Description : Process the command *Desktop_Virtualise.
command_desktop
        LocalLabels
        JSR     "r7-r11"                ; Stack registers
        LDR     r12, [r12]              ; Get pointer to workspace
        BL      filter_task             ; Start the task
        RTSS                            ; Return from subroutine

        ; Help text for the command
help_desktop
        =       "This is for the internal use of the Virtualise module.", 13
        =       "Do not use *Desktop_Virtualise, use *Desktop instead.", 13

        ; Syntax message for the command
syntax_desktop
        =       "Syntax: *Desktop_Virtualise", 0
        ALIGN

; Command *VirtualCheck

        ;   Parameters  : r0    - Pointer to the command tail.
        ;                 r1    - Number of parameters. This is undefined
        ;                         for a configuration keyword.
        ;                 r12   - Pointer to module private word.
        ;   Returns     : r7-r11    - Preserved.
        ;   Description : Process the command *VirtualCheck.
command_check
        LocalLabels
        JSR     "r7-r11"                ; Stack registers
        LDR     r12, [r12]              ; Get pointer to workspace
        BL      check_all               ; Perform a complete internal check
        ADRVC   r1, ok$l                ; Token for success
        ADRVS   r1, fail$l              ; Token for failure
        ADR     r0, ws_message          ; Pointer to messages control block
        ADRL    r2, ws_string           ; Pointer to result buffer
        MOV     r3, #String             ; Size of buffer
        SWI     XMessageTrans_Lookup    ; Translate the token into a string
        RTE VS                          ; Exit if an error produced
        MOV     r0, r2                  ; Copy pointer to result string
        MOV     r1, #0                  ; Use RISC OS dictionary
        MOV     r2, #0                  ; No string to substitute
        SWI     XOS_PrettyPrint         ; Output the resulting text
        SWI     XOS_NewLine             ; Write a newline
        RTSS                            ; Return from subroutine

        ; Tokens for text to output
ok$l    =       "CCOk", 0
fail$l  =       "CCFl", 0
        ALIGN

        ; Help text for the command
help_check
        =       "*VirtualCheck performs an internal consistency check of the"
        =       " Virtualise module.", 13

        ; Syntax message for the command
syntax_check
        =       "Syntax: *VirtualCheck", 0
        ALIGN

; Read configuration files

        ;   Parameters  : None
        ;   Returns     : None
        ;   Description : Read the configuration files
read_configuration
        LocalLabels
        JSR     "r0-r4"                 ; Stack registers
        ADR     r0, var$l               ; Pointer to variable name
        MOV     r2, #1:SHL:31           ; Check for existence
        MOV     r3, #0                  ; First call
        MOV     r4, #0                  ; Don't want expansion
        SWI     XOS_ReadVarVal          ; Check if variable exists
        TEQ     r2, #0                  ; Does it exist (ignore any error)
        BNE     exist$l                 ; Skip next section if it exists
        ADR     r0, var$l               ; Pointer to variable name
        ADR     r1, def$l               ; Pointer to variable value
        MOV     r2, #?def$l - 1         ; Length of string
        MOV     r3, #0                  ; First call
        MOV     r4, #OS_VartypeMacro    ; A macro variable
        SWI     XOS_SetVarVal           ; Set the variable
        RTE VS                          ; Exit without wiping over r0
exist$l ClrV                            ; Ensure that the error flag is clear
        BL      config$l                ; Read the configuration file
        BL      auto$l                  ; Set system variables
        ClrV                            ; Ensure that the error flag is clear
        RTS                             ; Return from subroutine

        ; Details of how to access configuration files
def$l   =       "<Choices$Write>.Virtualise", 0; Initial value of prefix
var$l   =       "Virtualise$ConfigDir", 0; Configuration files prefix
        ALIGN

        ;   Parameters  : None
        ;   Returns     : None
        ;   Description : Read the main configuration file to set memory usage.
config$l
        JSR     "r0-r5"                 ; Stack registers
        MOV     r0, #OSFile_LoadStampedNoPath; Reason code to read file
        ADR     r1, cfg$l               ; Configuration file name
        ADRL    r2, ws_string           ; Pointer to buffer to contain file
        MOV     r3, #0                  ; Use specified address
        SWI     XOS_File                ; Attempt to read the file
        RTE VS                          ; Exit if error produced
        ADRL    r0, ws_string           ; Restore pointer to start of buffer
loop$l  LDRB    r1, [r0], #1            ; Read the next character
        CMP     r1, #32                 ; Check if it is a terminator
        BHS     loop$l                  ; Loop if not found
        SUB     r1, r0, #1              ; Pointer to terminator
        ADRL    r0, ws_string           ; Restore pointer to start of buffer
        SUB     r1, r1, r0              ; Length of value to set
        BL      swap$l                  ; Set swap directory variable
        RTE VS                          ; Exit if error produced
        ADD     r1, r0, r1              ; Pointer to string terminator
        ADD     r1, r1, #1              ; Pointer to next value
        MOV     r0, #10 :OR: (1 << 29) :OR: (1 << 31); Set base and flags
        MOV     r2, #2560               ; Maximum acceptable value
        SWI     XOS_ReadUnsigned        ; Read the next value
        RTE VS                          ; Exit if error produced
        ADD     r1, r1, #1              ; Skip terminator
        MOV     r4, r2                  ; Copy the size limit value
        MOV     r2, #2560               ; Maximum acceptable value
        SWI     XOS_ReadUnsigned        ; Read the next value
        RTE VS                          ; Exit if error produced
        ADD     r1, r1, #1              ; Skip terminator
        MOV     r5, r2                  ; Copy the claim limit value
        MOV     r2, #2560               ; Maximum acceptable value
        SWI     XOS_ReadUnsigned        ; Read the next value
        RTE VS                          ; Exit if error produced
        ADD     r3, r1, #1              ; Skip terminator and copy pointer
        MOV     r0, r2                  ; Copy leave free value
        BL      scale$l                 ; Scale the leave free value
        MOV     r2, r0                  ; Copy the scaled leave free value
        MOV     r0, r5                  ; Copy the claim limit value
        BL      scale$l                 ; Scale the claim limit value
        MOV     r1, r0                  ; Copy the scaled claim limit value
        MOV     r0, r4                  ; Copy the size limit value
        BL      scale$l                 ; Scale the size limit value
        BL      configure               ; Configure memory usage
        MOV     r0, #10 :OR: (1 << 29) :OR: (1 << 31); Set base and flags
        MOV     r1, r3                  ; Restore buffer pointer
        MOV     r2, #2                  ; Maximum acceptable value
        SWI     XOS_ReadUnsigned        ; Read the next value
        RTE VS                          ; Exit if error produced
        ADD     r1, r1, #1              ; Skip terminator
        MOV     r4, r2                  ; Copy the page replacement policy
        MOV     r2, #1                  ; Maximum acceptable value
        SWI     XOS_ReadUnsigned        ; Read the next value
        RTE VS                          ; Exit if error produced
        ADD     r1, r1, #1              ; Skip terminator
        TEQ     r2, #0                  ; Is the flag set
        ORRNE   r4, r4, #policy_flags_reads; Include multiple page reads if so
        MOV     r2, #1                  ; Maximum acceptable value
        SWI     XOS_ReadUnsigned        ; Read the next value
        RTE VS                          ; Exit if error produced
        ADD     r5, r1, #1              ; Skip terminator and copy pointer
        TEQ     r2, #0                  ; Is the flag set
        ORRNE   r4, r4, #policy_flags_writes; Include multiple page writes if so
        MOV     r0, #1                  ; Reason code for page replacement
        MOV     r1, r4                  ; Copy replacement policy and flags
        BL      misc_op                 ; Set page replacement policy
        MOV     r0, #10 :OR: (1 << 29) :OR: (1 << 31); Set base and flags
        MOV     r1, r5                  ; Restore buffer pointer
        MOV     r2, #2560               ; Maximum acceptable value
        SWI     XOS_ReadUnsigned        ; Read the next value
        RTE VS                          ; Exit if error produced
        ADD     r1, r1, #1              ; Skip terminator
        MOV     r5, r2                  ; Copy keep value
        MOV     r2, #512                ; Maximum acceptable value
        SWI     XOS_ReadUnsigned        ; Read the next value
        RTE VS                          ; Exit if error produced
        MOV     r4, r2, LSL#20          ; Copy disc value and scale
        MOV     r0, r5                  ; Copy keep value
        ADD     r5, r1, #1              ; Skip terminator and copy pointer
        BL      scale$l                 ; Scale the value
        MOV     r3, r0                  ; Copy the scaled keep value
        MOV     r0, #-1                 ; Do not change fraction
        MOV     r1, #-1                 ; Do not change lower limit
        MOV     r2, #-1                 ; Do not change upper limit
        BL      user_configure          ; Set the amount of memory to keep
        MOV     r0, #10 :OR: (1 << 31)  ; Set base and flags
        MOV     r1, r5                  ; Restore buffer pointer
        SWI     XOS_ReadUnsigned        ; Read the next value
        RTE VS                          ; Exit if error produced
        ADD     r1, r1, #1              ; Skip terminator
        SWI     XOS_ReadUnsigned        ; Read the next value
        RTE VS                          ; Exit if error produced
        MOV     r0, #10 :OR: (1 << 29) :OR: (1 << 31); Set base and flags
        ADD     r1, r1, #1              ; Skip terminator
        MOV     r2, #1                  ; Maximum acceptable value
        SWI     XOS_ReadUnsigned        ; Read the next value
        RTE VS                          ; Exit if error produced
        TEQ     r2, #0                  ; Is the flag set
        BLNE    purge$l                 ; Purge swap directory if required
        RTSS                            ; Return from subroutine

        ; Convert a value from 0.1MB to bytes
scale$l JSR     "r1-r3"                 ; Stack registers
        MOV     r1, #1024 * 1024        ; Value to scale up by
        MUL     r1, r0, r1              ; Scale value up to 0.1 bytes
        MOV     r2, #10                 ; Value to divide by
        DivRem  r0, r1, r2, r3          ; Scale value down to bytes
        RTS                             ; Return from subroutine

        ; Set variable pointing to swap file
swap$l  JSR     "r0-r4"                 ; Stack registers
        MOV     r2, r1                  ; Copy length of value
        MOV     r1, r0                  ; Copy pointer to value
        ADR     r0, svar$l              ; Pointer to variable name
        MOV     r3, #0                  ; First call
        MOV     r4, #OS_VartypeMacro    ; Set a macro variable
        SWI     XOS_SetVarVal           ; Set the variable
        RTE VS                          ; Exit if error produced
        RTS                             ; Return from subroutine

        ; Purge the swap file directory
purge$l JSR     "r0-r7"                 ; Stack registers
        MOV     r0, #OSFSControl_Wipe   ; Reason code to wipe files
        ADR     r1, del$l               ; Path to delete
        MOV     r3, #OSFSControl_WipeForce; Force deletion
        MOV     r4, #0                  ; No start time
        MOV     r5, #0                  ; No start time
        MOV     r6, #0                  ; No end time
        MOV     r7, #0                  ; No end time
        SWI     XOS_FSControl           ; Wipe files in the swap directory
        RTSS                            ; Return from subroutine

        ; The configuration data file and swap file prefix variable
cfg$l   =       "<Virtualise$ConfigDir>.Config", 0
svar$l  =       "Virtualise$SwapDir", 0 ; Prefix variable
del$l   =       "<Virtualise$SwapDir>.*", 0; Path to purge
        ALIGN

        ;   Parameters  : None
        ;   Returns     : None
        ;   Description : Execute the automatic virtual memory file.
auto$l  JSR     "r0-r6"                 ; Stack registers
        MOV     r0, #OSFile_ReadStampedNoPath; Reason code to read information
        ADR     r1, autof$l             ; Pointer to filename
        SWI     XOS_File                ; Check if the file exists
        RTE VS                          ; Exit if error produced
        TEQ     r0, #OSFile_IsFile      ; Check object type is correct
        RTS NE                          ; Exit if not a file
        ADR     r0, cmd$l               ; Pointer to command to execute
        SWI     XOS_CLI                 ; Perform the command
        RTSS                            ; Return from subroutine

        ; Details of command and file for automatic virtual memory
cmd$l   =       "Obey "                 ; Assume it is an obey file
autof$l =       "<Virtualise$ConfigDir>.Automatic", 0; The actual filename
        ALIGN

; Handle messages file

        ;   Parameters  : None
        ;   Returns     : None
        ;   Description : Open the messages file.
open_messages
        LocalLabels
        JSR     "r0-r4"                 ; Stack registers
        MOV     r0, #0                  ; Null pointer
        STR     r0, ws_message_ptr      ; Reset message block pointer
        ADRL    r0, var$l               ; Pointer to variable name
        MOV     r2, #1:SHL:31           ; Check for existence
        MOV     r3, #0                  ; First call
        MOV     r4, #0                  ; Don't want expansion
        SWI     XOS_ReadVarVal          ; Check if variable exists
        TEQ     r2, #0                  ; Does it exist (ignore any error)
        BNE     exist$l                 ; Skip next section if it exists
        ADRL    r0, var$l               ; Pointer to variable name
        ADR     r1, def$l               ; Pointer to variable value
        MOV     r2, #?def$l - 1         ; Length of string
        MOV     r3, #0                  ; First call
        MOV     r4, #OS_VartypeLiteralString; A simple string
        SWI     XOS_SetVarVal           ; Set the variable
        RTE VS                          ; Exit without wiping over r0
exist$l ADRL    r1, file$l              ; Pointer to filename
        SWI     XMessageTrans_FileInfo  ; Get information about file
        RTE VS                          ; Exit without wiping over r0
        TST     r0, #1                  ; Is file held in memory
        BNE     mem$l                   ; No need to allocate memory if it is
        MOV     r0, #OSModule_Alloc     ; Claim entry code
        MOV     r3, r2                  ; Required size
        SWI     XOS_Module              ; Claim workspace
        RTE VS                          ; Exit without wiping over r0
        STR     r2, ws_message_ptr      ; Store memory pointer
mem$l   ADR     r0, ws_message          ; File descriptor
        ADR     r1, file$l              ; Pointer to filename
        LDR     r2, ws_message_ptr      ; Pointer to buffer
        SWI     XMessageTrans_OpenFile  ; Open the messages file
        RTE VS                          ; Exit without wiping over r0
        RTS                             ; Return from subroutine

        ; Details of how to access messages file
def$l   =       "Resources:$.":CC:resource_directory:CC:".", 0; Default path
var$l   =       "Virtualise$Path", 0    ; System variable to use
file$l  =       "Virtualise:Messages", 0; Name to access messages file as
        ALIGN

        ;   Parameters  : None
        ;   Returns     : None
        ;   Description : Close the messages file, and free any memory used.
close_messages
        LocalLabels
        JSR     "r0-r2"                 ; Stack registers
        ADR     r0, ws_message          ; File descriptor
        SWI     XMessageTrans_CloseFile ; Close the messages file
        LDR     r2, ws_message_ptr      ; Pointer to workspace
        TEQ     r2, #0                  ; Was any workspace used
        RTS EQ                          ; Exit if no workspace used
        MOV     r0, #OSModule_Free      ; Free entry code
        SWI     XOS_Module              ; Release workspace
        RTSS                            ; Return from subroutine

; Miscellaneous operations

        ;   Parameters  : r0    - Reason code.
        ;                 Other registers depend upon reason code
        ;   Returns     : r0    - Preserved.
        ;                 Other registers depend upon reason code
        ;   Description : Miscellaneous operations initially intended for use
        ;                 by !Virtualis.
misc_op LocalLabels
        CMP     r0, #(jump_end$l - jump$l) >> 2; Is it in range
        ADDLO   pc, pc, r0, LSL#2       ; Dispatch if in range
        B       unknown$l               ; Unknown SWI
jump$l  B       physical$l              ; Jump to handler for 0th reason code
        B       policy$l                ; Jump to handler for 1st reason code
        B       faults$l                ; Jump to handler for 2nd reason code
        B       pages$l                 ; Jump to handler for 3rd reason code
        B       freeze$l                ; Jump to handler for 4th reason code
        B       thaw$l                  ; Jump to handler for 5th reason code
        B       status$l                ; Jump to handler for 6th reason code
jump_end$l

        ; Generate an error if the reason code is unknown
unknown$l
        JSR     "r1-r2"                 ; Stack registers
        ADRL    r0, err_ouk             ; Pointer to error block
        ADR     r1, ws_message          ; Pointer to messages control block
        MOV     r2, #0                  ; Use an internal buffer
        SWI     XMessageTrans_ErrorLookup; Lookup the error text
        RTE                             ; Exit without wiping over r0

        ;   Parameters  : r0    - Reason code.
        ;                 r1    - Number of dynamic area to check.
        ;   Returns     : r0    - Preserved.
        ;                 r1    - Physical size of dynamic area.
        ;   Description : Return physical size of a dynamic area.
physical$l
        JSR     "r0, r2, r11"           ; Stack registers
        LDR     r11, ws_area_ptr        ; Pointer to the first area record
physical_find$l
        TEQ     r11, #0                 ; Is it a valid pointer
        BEQ     physical_simple$l       ; Pass on to OS routine if not found
        LDR     r0, [r11, #area_number] ; Get virtual area number
        TEQ     r0, r1                  ; Is it the required area
        LDRNE   r11, [r11, #area_next]  ; Pointer to next record if no match
        BNE     physical_find$l         ; Loop for next record if not found
        LDR     r1, [r11, #area_used_pages]; Get number of physical pages
        LDR     r0, ws_log_page_size    ; Log base 2 of pages size
        MOV     r1, r1, LSL r0          ; Convert physical size to bytes
        RTSS                            ; Return from subroutine

        ; Handle the case where virtual memory is not enabled for the area
physical_simple$l
        MOV     r0, r1                  ; Copy area number
        BL      os_read_dynamic_area    ; Read area size
        RTE VS                          ; Exit if error produced
        RTSS                            ; Return from subroutine

        ;   Parameters  : r0    - Reason code.
        ;                 r1    - The page replacement policy to use or -1 to
        ;                         read the current value.
        ;   Returns     : r0    - Preserved.
        ;                 r1    - The page replacement policy being used.
        ;   Description : Set or read the page replacement policy to use.
policy$l
        JSR     "r0"                    ; Stack registers
        LDR     r0, ws_replace_policy   ; Read the current setting
        CMP     r1, #-1                 ; Is it a read operation
        MOVLT   r1, #0                  ; Keep the value positive
        STRNE   r1, ws_replace_policy   ; Store the new value if required
        BIC     r1, r1, #policy_flags_mask; Only keep new policy number
        BIC     r0, r0, #policy_flags_mask; Only keep old policy number
        TEQ     r0, r1                  ; Has the policy been changed
        BLNE    policy_reset$l          ; Reset access counts if it has
        LDR     r1, ws_replace_policy   ; Get the new policy
        RTSS                            ; Return from subroutine

        ; Reset page access counts when page replacement policy has been changed
policy_reset$l
        JSR     "r0-r2"                 ; Stack registers
        LDR     r0, ws_source_array     ; Pointer to page array
        LDR     r1, ws_total_pages      ; Number of pages to process
        MOV     r2, #source_item        ; Size of the entries
        MLA     r1, r2, r1, r0          ; Pointer to end of array
policy_reset_loop$l
        CMP     r0, r1                  ; Has end of array been reached
        RTSS HS                         ; Exit if it has
        BL      count_end               ; Clear the access counter
        LDR     r2, [r0, #source_area]  ; Get pointer to area record
        TEQ     r2, #0                  ; Is the page being used
        BLNE    count_start             ; Start the counter if it is
        ADD     r0, r0, #source_item    ; Advance page pointer
        B       policy_reset_loop$l     ; Loop for next page

        ;   Parameters  : r0    - Reason code.
        ;                 r1    - The index of the instruction type to return
        ;                         details for.
        ;   Returns     : r0    - Preserved.
        ;                 r1    - The number of page faults caused by the
        ;                         specified type of instruction.
        ;   Description : Return the number of page faults caused by a
        ;                 particular type of instruction.
faults$l
        JSR     "r0"                    ; Stack registers
        ADRL    r0, ws_page_faults      ; Pointer to page fault counts
        LDR     r1, [r0, r1, LSL#2]     ; Read the page fault counter
        RTS                             ; Return from subroutine

        ;   Parameters  : r0    - Reason code.
        ;                 r1    - Number of the dynamic area.
        ;                 r2    - Index of first page to return details for.
        ;                 r3    - Index of page after last to process.
        ;                 r4    - Pointer to buffer to receive results.
        ;   Returns     : All registers preserved.
        ;   Description : Return the status of the specified pages within a
        ;                 dynamic area. On exit the buffer contains a single
        ;                 byte for each of the pages specified indicating the
        ;                 status of that page.
pages$l JSR     "r0-r8, r11"            ; Stack registers
        LDR     r11, ws_area_ptr        ; Pointer to the first area record
pages_find$l
        TEQ     r11, #0                 ; Is it a valid pointer
        BEQ     pages_not$l             ; Use simple method if not found
        LDR     r0, [r11, #area_number] ; Get virtual area number
        TEQ     r0, r1                  ; Is it the required area
        BEQ     pages_found$l           ; Exit loop if it is
        LDR     r11, [r11, #area_next]  ; Poinyer to next record if no match
        B       pages_find$l            ; Loop for the next area
pages_found$l
        MOV     r0, #virtual_item       ; Size of virtual array entries
        LDR     r5, [r11, #area_virtual_array]; Pointer to start of array
        MLA     r5, r0, r2, r5          ; Pointer to required page
pages_loop$l
        CMP     r2, r3                  ; Is it all finished
        RTS HS                          ; Return from subroutine if it is
        MOV     r0, #&10                ; Value for not a valid page
        LDR     r1, [r11, #area_total_pages]; Get logical size of area
        CMP     r2, r1                  ; Is it a valid page
        BHS     pages_done$l            ; Skip next bit if not
        LDR     r1, [r5, #virtual_ptr]  ; Get details for this page
        BIC     r0, r1, #virtual_flags_mask; Mask out flag bits
        TEQ     r0, #0                  ; Is the page in memory
        BNE     pages_in$l              ; Skip next bit if it is
        MOV     r0, #&12                ; Assume it is on disc then
        TST     r1, #virtual_flags_used ; Has the page been used
        MOVEQ   r0, #&11                ; Change value if not
        B       pages_done$l            ; Value chosen now
pages_in$l
        LDR     r1, [r0, #source_locks] ; Get number of times locked
        TEQ     r1, #0                  ; Is page locked
        MOVNE   r0, #&13                ; Value for page locked
        BNE     pages_done$l            ; Skip next bit if it is locked
        LDR     r0, [r0, #source_count] ; Get access count
        MOV     r0, r0, LSR#28          ; Shift to get correct range
pages_done$l
        STRB    r0, [r4], #1            ; Store value and advance
        ADD     r2, r2, #1              ; Increment page number
        ADD     r5, r5, #virtual_item   ; Advance to next page array entry
        B       pages_loop$l            ; Loop for next page
pages_not$l
        MOV     r0, #OSDynamicArea_Read ; Reason code to read area details
        STMFD   r13!, {r2-r4}           ; Stack useful registers
        BL      os_dynamic_area         ; Read the area details
        LDMVSFD r13!, {r2-r4}           ; Restore registers if error
        RTE VS                          ; Exit if error produced
        LDR     r0, ws_log_page_size    ; Log base 2 of page size
        MOV     r3, r2, LSR r0          ; Convert current size to pages
        MOV     r4, r5, LSR r0          ; Convert maximum size to pages
        LDMFD   r13!, {r0-r2}           ; Restore registers
pages_simple$l
        CMP     r0, r1                  ; Is it all finished
        RTS HS                          ; Return if it is
        MOV     r5, #&F                 ; Value for maximum accesses
        CMP     r0, r3                  ; Is it a valid page
        MOVHS   r5, #&10                ; Value for not a valid page
        STRB    r5, [r2], #1            ; Store value and advance
        ADD     r0, r0, #1              ; Increment page number
        B       pages_simple$l          ; Loop for next page

        ;   Parameters  : r0    - Reason code.
        ;                 r1    - Handle of task to freeze.
        ;   Returns     : None
        ;   Description : Freeze a WIMP task.
freeze$l
        JSR     "r0"                    ; Stack registers
        MOV     r0, r1                  ; Copy task handle
        BL      filter_start            ; Start filtering task
        RTE VS                          ; Return any error produced
        RTSS                            ; Return from subroutine

        ;   Parameters  : r0    - Reason code.
        ;                 r1    - Handle of task to thaw.
        ;   Returns     : None
        ;   Description : Unfreeze a previously frozen WIMP task.
thaw$l  JSR     "r0"                    ; Stack registers
        MOV     r0, r1                  ; Copy task handle
        BL      filter_end              ; Stop filtering task
        RTE VS                          ; Return any error produced
        RTSS                            ; Return from subroutine

        ;   Parameters  : r0    - Reason code.
        ;                 r1    - Handle of task to check.
        ;   Returns     : r0    - Status of the task.
        ;   Description : Check the status of a WIMP task.
status$l
        JSR     ""                      ; Stack registers
        MOV     r0, r1                  ; Copy task handle
        BL      filter_check            ; Check status of the task
        RTE VS                          ; Return any error produced
        RTSS                            ; Return from subroutine

; A literal pool

        LTORG

; Handle the virtual dynamic areas

        ;   Parameters  : r0    - The number of the dynamic area to claim.
        ;                 r1    - Maximum size of area (reserved logical
        ;                         address space), or -1 to use maximum size
        ;                         of the dynamic area.
        ;                 r2    - Name to use for swap file. This file must
        ;                         not already exist. If pointer is 0 then a
        ;                         name is chosen automatically.
        ;   Returns     : None
        ;   Description : Start virtual memory on a dynamic area. Note that
        ;                 sizes are specified in pages.
start_virtualise
        LocalLabels
;        debug_record "start_virtualise"
        JSR     "r0-r8, r11"            ; Stack registers
        pipe_string "start_virtualise: area = &%0, max size = &%1, name ptr = &%2", r0, r1, r2
        assert_registers                ; Save register values
        BLVS    check_all_debug         ; Perform a consistency check
        LDR     r11, ws_area_ptr        ; Pointer to the first area record
find$l  TEQ     r11, #0                 ; Is it a valid pointer
        BEQ     not$l                   ; Alright to continue if not found
        LDR     r3, [r11, #area_number] ; Get virtual area number
        TEQ     r0, r3                  ; Is it the required area
        RTSS EQ                         ; Exit without error if it is
        LDR     r11, [r11, #area_next]  ; Pointer to next record if no match
        B       find$l                  ; Loop for next record if not found
not$l   MOV     r3, #0                  ; Value to clear command flag with
        STR     r3, ws_command_flag     ; Clear command flag
        MOV     r4, r0                  ; Copy the dynamic area number
        MOV     r5, r1                  ; Copy maximum size
        MOV     r6, r2                  ; Copy swap file name pointer
        MOV     r0, #OSModule_Alloc     ; Claim entry code
        MOV     r3, #area_struct        ; Required size
        BL      os_module               ; Claim space for area record
        RTE VS                          ; Exit if error
        MOV     r11, r2                 ; Copy pointer to record
        LDR     r0, ws_area_ptr         ; Pointer to the first dynamic area
        STR     r0, [r11, #area_next]   ; Store as next area
        STR     r4, [r11, #area_number] ; Store dynamic area number
        STR     r5, [r11, #area_max_pages]; Store maximum number of pages
        STR     r6, [r11, #area_file_name]; Store pointer to filename
        MOV     r0, #0                  ; Null pointer for page array or handle
        STR     r0, [r11, #area_used_pages]; No physical pages
        STR     r0, [r11, #area_file_handle]; Zero file handle
        STR     r0, [r11, #area_virtual_array]; Initialise array pointer
        LDR     r0, ws_total_pages      ; Number of pages available
        RSBS    r0, r0, #min_spare_pages; Number of pages to claim
        MOVMI   r0, #0                  ; Only allow positive change
        SWI     XHourglass_On           ; Enable hourglass
        BLVS    free_memory$l           ; Free any allocated memory
        RTE VS                          ; Exit without wiping over r0
        BL      ensure_memory           ; Attempt to claim the memory
        ADRVSL  r0, err_dmm             ; Pointer to error block
        BVS     error$l                 ; Exit with error
        CMP     r0, #0                  ; Was the requested amount given
        ADRLTL  r0, err_dmm             ; Pointer to error block
        BLT     error$l                 ; Generate error if no memory
        BL      virtualise_swap_dir     ; Ensure system variable is set
        BLVS    free_memory$l           ; Free any allocated memory
        RTE VS                          ; Exit without wiping over r0
        LDR     r1, [r11, #area_number] ; Restore the area number
        MOV     r0, #OSDynamicArea_Read ; Reason code to read details of area
        BL      os_dynamic_area         ; Read the area details
        BLVS    free_memory$l           ; Free any allocated memory
        RTE VS                          ; Exit without wiping over r0
        [       :DEF: demo
        name_check r8, r0, demo_area1, demo$l; Check possible dynamic area name
        name_check r8, r0, demo_area2, demo$l; Check possible dynamic area name
        name_check r8, r0, demo_area3, demo$l; Check possible dynamic area name
        name_check r8, r0, demo_area4, demo$l; Check possible dynamic area name
        name_check r8, r0, demo_area5, demo$l; Check possible dynamic area name
        name_check r8, r0, demo_area6, demo$l; Check possible dynamic area name
        name_check r8, r0, demo_area7, demo$l; Check possible dynamic area name
        name_check r8, r0, demo_area8, demo$l; Check possible dynamic area name
        ADRL    r0, err_dem             ; Pointer to error block
        B       error$l                 ; Generate an error
demo$l
        |
        ADRL    r0, handler_intercept   ; Pointer to handler intercept
        TEQ     r6, #0                  ; Test if there is a handler routine
        TEQNE   r0, r6                  ; Test if handler is intercepted
        ADRNEL  r0, err_dhn             ; Pointer to error block
        BNE     error$l                 ; Generate error if handler routine
        ]
        AND     r0, r4, #&F70           ; Get flags of interest
        TEQ     r0, #&000               ; Are flags set suitably
        ADRNEL  r0, err_dfg             ; Pointer to error block
        BNE     error$l                 ; Generate error if flags unsuitable
        TST     r3, #1 << 31            ; Is top bit of address set
        ADRNEL  r0, err_dtb             ; Pointer to error block
        BNE     error$l                 ; Generate error if address unsuitable
        AND     r0, r4, #&F             ; Get protection level to set
        STR     r0, [r11, #area_access] ; Store protection level
        STR     r3, [r11, #area_base]   ; Store base address of area
        LDR     r1, ws_log_page_size    ; Get the log of the page size
        LDR     r0, [r11, #area_max_pages]; Get the specified maximum size
        CMP     r0, #-1                 ; Was a maximum size specified
        MOVEQ   r0, r5                  ; If not, use maximum size of area
        MOV     r0, r0, LSR r1          ; Convert maximum size to pages
        STR     r0, [r11, #area_max_pages]; If not, use maximum size of area
        MOV     r0, r2, LSR r1          ; Get current number of pages
        STR     r0, [r11, #area_total_pages]; Store initial number of pages
        LDR     r1, [r11, #area_max_pages]; Get maximum number of pages
        CMP     r0, r1                  ; Is current less that or equal maximum
        ADRGTL  r0, err_dsz             ; Pointer to error block
        BGT     error$l                 ; Generate error if too large
        LDR     r0, [r11, #area_file_name]; Get specified filename pointer
        ADD     r1, r11, #area_file_name; Pointer to file name buffer
        TEQ     r0, #0                  ; Was a name specified
        BEQ     auto_name$l             ; If not then choose a name
copy_loop$l
        LDRB    r2, [r0], #1            ; Read character from source string
        CMP     r2, #31                 ; Is it a control character
        MOVLE   r2, #0                  ; Change control character to null
        STRB    r2, [r1], #1            ; Write character to destination
        BGT     copy_loop$l             ; Loop for next character
        B       chosen_name$l           ; Exit loop when name copied
auto_name$l
        MOV     r0, #OSFile_CreateDir   ; Reason code for create directory
        ADRL    r1, swap$l              ; Pointer to directory name
        MOV     r4, #0                  ; Default directory size
        SWI     XOS_File                ; Create the directory
        BLVS    free_memory$l           ; Free any allocated memory
        RTE VS                          ; Exit if error
        ADD     r0, r11, #area_file_name; Pointer to file name buffer
auto_copy$l
        LDRB    r2, [r1], #1            ; Read a character
        STRB    r2, [r0], #1            ; Write the character
        TEQ     r2, #0                  ; Was it the terminator
        BNE     auto_copy$l             ; Loop if not finished
        SUB     r1, r0, #1              ; Back to terminator
        MOV     r2, #'.'                ; Leafname separator
        STRB    r2, [r1], #1            ; Store leafname separator
        MOV     r0, r11                 ; Copy area pointer
        MOV     r2, #OS_Hex8Limit + 1   ; Size of buffer
        SWI     XOS_ConvertHex8         ; Append the area number
        BLVS    free_memory$l           ; Free any allocated memory
        RTE VS                          ; Exit if error
chosen_name$l
;**CHECK** ws_auto_sw_disc
        MOV     r0, #OSFile_CreateStamped; Create empty file if not
        ADD     r1, r11, #area_file_name; Pointer to filename
        LDR     r2, = &FFD              ; Filetype to use
        MOV     r4, #0                  ; Start address is unused
        MOV     r5, #0                  ; Size is zero
        SWI     XOS_File                ; Write the data
        BLVS    free_memory$l           ; Free any allocated memory
        RTE VS                          ; Exit if error
        ADD     r0, r11, #area_file_name; Pointer to filename
        BL      free_space              ; Read the free space on the device
        MOV     r6, r0                  ; Copy free space
        MOV     r0, #OSFile_Delete      ; Reason code to delete file
        ADD     r1, r11, #area_file_name; Get pointer to file name
        SWI     XOS_File                ; Delete the file
        LDR     r0, ws_log_page_size    ; Get log base 2 of page size
        LDR     r1, [r11, #area_total_pages]; Get number of pages
        LDR     r2, ws_auto_sw_disc     ; Space to leave free
        ADD     r0, r2, r1, LSL r0      ; Free space required before operation
        CMP     r0, r6                  ; Is there sufficient space
        ADRL    r0, err_ddf             ; Pointer to error block
        BHI     error$l                 ; Generate error if insufficient disc
        MOV     r0, #OSFile_SaveStamped ; Reason code to save block of memory
        ADD     r1, r11, #area_file_name; Pointer to filename
        LDR     r2, = &FFD              ; Filetype to use
        LDR     r3, ws_page_size        ; The page size
        LDR     r4, [r11, #area_base]   ; Get start address of data in memory
        LDR     r5, [r11, #area_total_pages]; Get number of pages
        MLA     r5, r3, r5, r4          ; End address of data in memory
        TEQ     r4, r5                  ; Is there any data to write
        MOVEQ   r0, #OSFile_CreateStamped; Create empty file if not
        MOVEQ   r4, #0                  ; Clear register
        MOVEQ   r5, #0                  ; Size is zero
        SWI     XOS_File                ; Write the data
        BLVS    free_memory$l           ; Free any allocated memory
        RTE VS                          ; Exit if error
        MOV     r0, #OSFind_Openup:OR:OSFind_NoPath; Reason code for open file
        ADD     r1, r11, #area_file_name; Pointer to filename
        SWI     XOS_Find                ; Open the file for update
        BLVS    free_memory$l           ; Free any allocated memory
        RTE VS                          ; Exit if error
        STR     r0, [r11, #area_file_handle]; Store file handle
        MOV     r1, r0                  ; Copy file handle
        MOV     r0, #OSArgs_ReadPath    ; Reason code to canonicalise
        ADD     r2, r11, #area_file_name; Pointer to filename buffer
        MOV     r5, #String             ; Size of buffer
        SWI     XOS_Args                ; Canonicalise file name
        BLVS    free_memory$l           ; Free any allocated memory
        RTE VS                          ; Exit if error
        LDR     r0, [r11, #area_max_pages]; Get maximum number of pages
        MOV     r1, #virtual_item       ; Size of entries in page array
        MUL     r3, r0, r1              ; Memory required for page array
        MOV     r0, #OSModule_Alloc     ; Claim entry code
        BL      os_module               ; Claim space for area record
        BLVS    free_memory$l           ; Free any allocated memory
        RTE VS                          ; Exit if error
        STR     r2, [r11, #area_virtual_array]; Store page array pointer
        LDR     r0, [r11, #area_total_pages]; Get initial number of pages
        MOV     r1, #virtual_flags_used ; Indicate that page is modified
loop$l  SUBS    r0, r0, #1              ; Decrement number of pages
        BMI     done$l                  ; Exit loop when all done
        STR     r1, [r2, #virtual_ptr]  ; Store 0 pointer
        ADD     r2, r2, #virtual_item   ; Increment page pointer
        B       loop$l                  ; Loop for next page
done$l  LDR     r1, [r11, #area_total_pages]; Get number of pages
        LDR     r0, ws_log_page_size    ; Get log base 2 of page size
        MOV     r1, r1, LSL r0          ; Convert required size to bytes
        RSB     r1, r1, #0              ; Negate amount of memory to remove
        LDR     r0, [r11, #area_number] ; Get dynamic area number
        STR     r0, ws_morph_area       ; Don't pass on to handler routine
        BL      os_change_dynamic_area  ; Free any memory associated with area
        MOV     r1, #0                  ; Value to clear area number with
        STR     r1, ws_morph_area       ; Clear area number
        BLVS    free_memory$l           ; Free any allocated memory
        RTE VS                          ; Exit if error
        LDR     r0, ws_virtual_areas    ; Get previous number of areas
        ADD     r0, r0, #1              ; Increment number of areas
        STR     r0, ws_virtual_areas    ; Store new number of areas
        LDR     r0, ws_virtual_pages    ; Get number of virtual memory pages
        LDR     r1, [r11, #area_total_pages]; Get number of pages in this area
        ADD     r0, r0, r1              ; New total number of pages
        STR     r0, ws_virtual_pages    ; Store number of virtual memory pages
        STR     r11, ws_area_ptr        ; Store pointer to virtual area record
        BL      adjust_memory           ; Optimise memory usage
        SWI     XHourglass_Off          ; Turn the hourglass off
        RTSS                            ; Return from subroutine

        ; Details of where to locate the swap file
swap$l  =       "<Virtualise$SwapDir>", 0; Actual prefix string to use
        ALIGN

        ; Generate an error message and return
error$l BL      free_memory$l           ; Free any allocated memory
        ADR     r1, ws_message          ; Pointer to messages control block
        MOV     r2, #0                  ; Use internal buffer
        SWI     XMessageTrans_ErrorLookup; Lookup the error text
        RTE                             ; Return from subroutine

        ; Free any allocated memory
free_memory$l
        JSR     "r0-r3"                 ; Stack registers
        LDR     r1, [r11, #area_file_handle]; Get file handle
        TEQ     r1, #0                  ; Is the file open
        BEQ     free_memory_no_swap$l   ; Branch if no swap file
        MOV     r0, #OSFind_Close       ; Reason code to close file
        SWI     XOS_Find                ; Close the file if present
        MOV     r0, #OSFile_Delete      ; Reason code to delete file
        ADD     r1, r11, #area_file_name; Get pointer to file name
        SWI     XOS_File                ; Delete the file
free_memory_no_swap$l
        MOV     r0, #OSModule_Free      ; Free entry code
        LDR     r2, [r11, #area_virtual_array]; Get pointer to array
        TEQ     r2, #0                  ; Was any memory allocated
        BLNE    os_module               ; Release page array memory
        MOV     r2, r11                 ; Pointer to record
        BL      os_module               ; Release area record memory
        SWI     XHourglass_Off          ; Turn the hourglass off
        RTSS                            ; Return from subroutine

; Swap directory manipulation

        ;   Parameters  : None
        ;   Returns     : None
        ;   Description : Ensure swap directory variable is set.
virtualise_swap_dir
        LocalLabels
        JSR     "r0-r4"                 ; Stack registers
        ADR     r0, var$l               ; Pointer to variable name
        MOV     r1, #0                  ; No buffer for result
        MOV     r2, #1:SHL:31           ; Check for existence
        MOV     r3, #0                  ; First call
        MOV     r4, #0                  ; Don't want expansion
        SWI     XOS_ReadVarVal          ; Check if variable exists
        TEQ     r2, #0                  ; Does it exist (ignore any error)
        RTSS NE                         ; Return if it already exists
        ADR     r0, var$l               ; Pointer to variable name
        ADR     r1, init$l              ; Pointer to variable value
        MOV     r2, #?init$l - 1        ; Length of string
        MOV     r3, #0                  ; First call
        MOV     r4, #OS_VartypeMacro    ; A macro variable
        SWI     XOS_SetVarVal           ; Set the variable
        RTE VS                          ; Return if error produced
        RTS                             ; Return from subroutine

        ; Details of where to locate the swap file
var$l   =       "Virtualise$SwapDir", 0 ; Prefix variable
init$l  =       "<Wimp$ScrapDir>.Virtualise", 0; Initial value of prefix
        ALIGN

; A literal pool

        LTORG

        ;   Parameters  : r0    - The number of the dynamic area to release.
        ;   Returns     : None
        ;   Description : Stop virtual memory on a dynamic area.
end_virtualise
        LocalLabels
;        debug_record "end_virtualise"
        JSR     "r0-r4, r10-r11"        ; Stack registers
        pipe_string "end_virtualise: area = &%0", r0
        assert_registers                ; Save register values
        BLVS    check_all_debug         ; Perform a consistency check
        ADR     r10, ws_area_ptr        ; Pointer to previous pointer
        LDR     r11, [r10]              ; Pointer to the first area record
find$l  TEQ     r11, #0                 ; Is it a valid pointer
        RTSS EQ                         ; Exit without error if not found
        LDR     r1, [r11, #area_number] ; Get virtual area number
        TEQ     r0, r1                  ; Is it the required area
        ADDNE   r10, r11, #area_next    ; Next pointer to previous pointer
        LDRNE   r11, [r10]              ; Pointer to next record if no match
        BNE     find$l                  ; Loop for next record if not found
        SWI     XHourglass_On           ; Turn the hourglass on
        RTE VS                          ; Exit wth the error
        MOV     r0, #0                  ; Value to clear command flag with
        STR     r0, ws_command_flag     ; Clear command flag
        LDR     r0, [r11, #area_total_pages]; Get current number of pages
        LDR     r1, [r11, #area_virtual_array]; Pointer to page array
lock_loop$l
        SUBS    r0, r0, #1              ; Decrement number of pages
        BMI     lock_done$l             ; Exit loop when all done
        LDR     r2, [r1, #virtual_ptr]  ; Get the page pointer
        BIC     r2, r2, #virtual_flags_mask; Clear flag bits
        TEQ     r2, #0                  ; Is page present
        BEQ     lock_next$l             ; Skip next bit if not
        LDR     r3, [r2, #source_locks] ; Get number of times locked
        TEQ     r3, #0                  ; Is page locked
        ADRNEL  r0, err_dlk             ; Pointer to error block
        BNE     error$l                 ; Generate an error if locked
lock_next$l
        ADD     r1, r1, #virtual_item   ; Increment page pointer
        B       lock_loop$l             ; Loop for next page
lock_done$l
        LDR     r2, [r11, #area_total_pages]; Get current number of pages
        LDR     r1, [r11, #area_virtual_array]; Pointer to page array
free_loop$l
        SUBS    r2, r2, #1              ; Decrement number of pages
        BMI     free_done$l             ; Exit loop when all done
        LDR     r0, [r1, #virtual_ptr]  ; Get the page pointer
        BIC     r0, r0, #virtual_flags_mask; Clear flag bits
        TEQ     r0, #0                  ; Is page present
        BLNE    release_page            ; Free the page
        BLVS    tidy$l                  ; Turn the hourglass off
        RTE VS                          ; Exit with the error
        ADD     r1, r1, #virtual_item   ; Increment page pointer
        B       free_loop$l             ; Loop for next page
free_done$l
        LDR     r0, [r11, #area_number] ; Get the number of the area
        LDR     r1, [r11, #area_total_pages]; Get number of pages to claim
        LDR     r2, ws_log_page_size    ; Get the log of the page size
        MOV     r1, r1, LSL r2          ; Convert size to bytes
        STR     r0, ws_morph_area       ; Do not pass onto dynamic area handler
        BL      os_change_dynamic_area  ; Try to claim real memory for the area
        MOV     r1, #0                  ; Value to clear area number with
        STR     r1, ws_morph_area       ; Clear area number
        BVC     got_mem$l               ; Continue if managed to claim memory
        BL      minimise_memory         ; Minimise the amount of memory used
        LDR     r0, [r11, #area_number] ; Get the number of the area
        LDR     r1, [r11, #area_total_pages]; Get number of pages to claim
        LDR     r2, ws_log_page_size    ; Get the log of the page size
        MOV     r1, r1, LSL r2          ; Convert size to bytes
        STR     r0, ws_morph_area       ; Do not pass onto dynamic area handler
        BL      os_change_dynamic_area  ; Try to claim real memory for the area
        MOV     r1, #0                  ; Value to clear area number with
        STR     r1, ws_morph_area       ; Clear area number
        BLVS    tidy$l                  ; Turn the hourglass off
        RTE VS                          ; Exit wth the error
got_mem$l
        MOV     r0, #OSGBPB_ReadAt      ; Reason code to read data
        LDR     r1, [r11, #area_file_handle]; Get file handle
        LDR     r2, [r11, #area_base]   ; Start of data in memory
        LDR     r3, [r11, #area_total_pages]; Get number of pages in this area
        LDR     r4, ws_log_page_size    ; Get the log of the page size
        MOV     r3, r3, LSL r4          ; Convert size to bytes
        MOV     r4, #0                  ; Start from beginning of file
        SWI     XOS_GBPB                ; Read the data into real memory
        BVC     load$l                  ; Skip retries if loaded
        BL      reopen_swap_file        ; Ensure swap file is open
        BLVS    tidy$l                  ; Turn the hourglass off
        RTE VS                          ; Exit with the error
        MOV     r0, #OSGBPB_ReadAt      ; Reason code to read data
        LDR     r1, [r11, #area_file_handle]; Get file handle
        SWI     XOS_GBPB                ; Read the data into real memory
        BLVS    tidy$l                  ; Turn the hourglass off
        RTE VS                          ; Exit with the error
load$l  LDR     r0, [r11, #area_next]   ; Pointer to next area
        STR     r0, [r10]               ; Unlink virtual area record
        LDR     r0, ws_virtual_areas    ; Get current number of virtual areas
        SUB     r0, r0, #1              ; Decrement number of areas
        STR     r0, ws_virtual_areas    ; Store new number of virtual areas
        LDR     r0, ws_virtual_pages    ; Get current number of virtual pages
        LDR     r1, [r11, #area_total_pages]; Get number of pages in this area
        SUB     r0, r0, r1              ; Calculate new number of pages
        STR     r0, ws_virtual_pages    ; Store new number of virtual pages
        LDR     r1, [r11, #area_file_handle]; Get file handle
        TEQ     r1, #0                  ; Is the file open
        MOV     r0, #OSFind_Close       ; Reason code to close file
        SWI     XOS_Find                ; Close the file if present
        MOV     r0, #OSFile_Delete      ; Reason code to delete file
        ADD     r1, r11, #area_file_name; Get pointer to file name
        SWI     XOS_File                ; Delete the file
        MOV     r0, #OSModule_Free      ; Free entry code
        LDR     r2, [r11, #area_virtual_array]; Get pointer to array
        BL      os_module               ; Release page array memory
        MOV     r2, r11                 ; Pointer to record
        BL      os_module               ; Release area record memory
        BL      adjust_memory           ; Optimise memory usage
        SWI     XHourglass_Off          ; Turn the hourglass off
        RTSS                            ; Return from subroutine

        ; Generate an error message and return
error$l BL      tidy$l                  ; Turn the hourglass off
        ADR     r1, ws_message          ; Pointer to messages control block
        MOV     r2, #0                  ; Use internal buffer
        SWI     XMessageTrans_ErrorLookup; Lookup the error text
        RTE                             ; Return from subroutine

        ; Turn the hourglass off
tidy$l  JSR     ""                      ; Stack registers
        SWI     XHourglass_Off          ; Turn the hourglass off
        RTSS                            ; Return from subroutine

        ;   Parameters  : r0    - Dynamic area to remove virtual memory for.
        ;   Returns     : None
        ;   Description : Force disabling of virtual memory for a dynamic
        ;                 area. No error is returned if virtual memory was
        ;                 not active for the area. This call should not
        ;                 normally be used. It is intended to be used only
        ;                 when an area is removed without first disabling
        ;                 virtual memory.
destroy_virtualise
        LocalLabels
        JSR     "r0-r5, r10-r11"        ; Stack registers
        pipe_string "destroy_virtualise: area = &%0", r0
        assert_registers                ; Save register values
        BLVS    check_all_debug         ; Perform a consistency check
        ADR     r10, ws_area_ptr        ; Pointer to previous pointer
        LDR     r11, [r10]              ; Pointer to the first area record
find$l  TEQ     r11, #0                 ; Is it a valid pointer
        RTSS EQ                         ; Exit without error if not found
        LDR     r1, [r11, #area_number] ; Get virtual area number
        TEQ     r0, r1                  ; Is it the required area
        ADDNE   r10, r11, #area_next    ; Next pointer to previous pointer
        LDRNE   r11, [r10]              ; Pointer to next record if no match
        BNE     find$l                  ; Loop for next record if not found
        LDR     r1, [r11, #area_virtual_array]; Pointer to page array
        LDR     r2, [r11, #area_total_pages]; Get current number of pages
loop$l  SUBS    r2, r2, #1              ; Decrement number of pages
        BMI     unlocked$l              ; Exit loop when all done
        LDR     r0, [r1, #virtual_ptr]  ; Get the page pointer
        BIC     r0, r0, #virtual_flags_used; Mark page as unmodified
        STR     r0, [r1, #virtual_ptr]  ; Store modified flags
        BIC     r0, r0, #virtual_flags_mask; Mask out flag bits
        TEQ     r0, #0                  ; Is page present
        BEQ     next$l                  ; Skip next bit if not
        LDR     r3, [r0, #source_locks] ; Get number of times locked
        TEQ     r3, #0                  ; Is page locked
        BEQ     free$l                  ; Skip next bit if not
        MOV     r3, #0                  ; Value to clear locks with
        STR     r3, [r0, #source_locks] ; Clear locked count
        LDR     r3, ws_locked_pages     ; Get locked page count
        SUB     r3, r3, #1              ; Decrement locked pages
        STR     r3, ws_locked_pages     ; Store modified locked page count
free$l  BL      release_page            ; Free the page
next$l  ADD     r1, r1, #virtual_item   ; Increment page pointer
        B       loop$l                  ; Loop for next page
unlocked$l
        LDR     r0, [r11, #area_next]   ; Pointer to next area
        STR     r0, [r10]               ; Unlink virtual area record
        LDR     r0, ws_virtual_areas    ; Get current number of virtual areas
        SUB     r0, r0, #1              ; Decrement number of areas
        STR     r0, ws_virtual_areas    ; Store new number of virtual areas
        LDR     r0, ws_virtual_pages    ; Get current number of virtual pages
        LDR     r1, [r11, #area_total_pages]; Get number of pages in this area
        SUB     r0, r0, r1              ; Calculate new number of pages
        STR     r0, ws_virtual_pages    ; Store new number of virtual pages
        LDR     r1, [r11, #area_file_handle]; Get file handle
        TEQ     r1, #0                  ; Is the file open
        MOV     r0, #OSFind_Close       ; Reason code to close file
        SWINE   XOS_Find                ; Close the file if present
        MOV     r0, #OSFile_Delete      ; Reason code to delete file
        ADD     r1, r11, #area_file_name; Get pointer to file name
        SWI     XOS_File                ; Delete the file
        MOV     r0, #OSModule_Free      ; Free entry code
        LDR     r2, [r11, #area_virtual_array]; Get pointer to array
        BL      os_module               ; Release page array memory
        MOV     r2, r11                 ; Pointer to record
        BL      os_module               ; Release area record memory
        BL      adjust_memory           ; Optimise memory usage
        RTSS                            ; Return from subroutine

        ;   Parameters  : r0    - Number of dynamic area to alter.
        ;                 r1    - Amount to move in bytes (signed integer).
        ;   Returns     : r0    - Preserved.
        ;                 r1    - Number of bytes moved (unsigned integer).
        ;   Description : Change the size of a virtual dynamic area.
change_virtualise
        LocalLabels
        JSR     "r0, r2-r4, r10-r11"    ; Stack registers
        LDR     r11, ws_area_ptr        ; Pointer to the first area record
find$l  TEQ     r11, #0                 ; Is it a valid pointer
        BEQ     simple$l                ; Pass on to OS routine if not found
        LDR     r2, [r11, #area_number] ; Get virtual area number
        TEQ     r0, r2                  ; Is it the required area
        LDRNE   r11, [r11, #area_next]  ; Pointer to next record if no match
        BNE     find$l                  ; Loop for next record if not found
        pipe_string "change_virtualise: area = &%0, change = &%1", r0, r1
;        debug_record "change_virtualise"
        MOV     r2, #0                  ; Value to clear command flag with
        STR     r2, ws_command_flag     ; Clear command flag
        LDR     r2, ws_log_page_size    ; Log base 2 of the page size
        SUB     r1, r1, #1              ; Correct for rounding up
        MOV     r1, r1, ASR r2          ; Convert bytes to pages
        ADD     r0, r1, #1              ; Round up to next page
        MOV     r1, #0                  ; Just in case need to quit
        [       :LNOT: :DEF: demo
        BL      pre_call_handler        ; Call the pre-move handler if any
        RTE VS                          ; Exit if error produced
        ]
        SWI     XHourglass_On           ; Turn the hourglass on
        RTE VS                          ; Exit if error produced
        MOV     r10, r0                 ; Copy modified change in size
        CMP     r10, #0                 ; In which direction is the change
        BLEQ    tidy$l                  ; Tidy up if no change before exit
        RTSS EQ                         ; Exit if no change requested
        BLT     decrease$l              ; If negative then reduce area size
        LDR     r0, [r11, #area_total_pages]; Get current size of area
        LDR     r1, [r11, #area_max_pages]; Get maximum number of pages
        ADD     r2, r0, r10             ; Resulting size of area
        pipe_string "change_virtualise: from = &%0, by = &%1, increase to = &%2", r0, r10, r2
        CMP     r2, r1                  ; Compare to maximum
        ADRGTL  r0, err_dmx             ; Pointer to error block
        BGT     error$l                 ; Generate error if too large
        LDR     r1, [r11, #area_total_pages]; Get current size of area
        LDR     r0, [r11, #area_virtual_array]; Pointer to page array
        MOV     r2, #virtual_item       ; Size of virtual array entries
        MLA     r0, r1, r2, r0          ; Pointer to virtual array entry
        MOV     r2, #0                  ; Value to clear array with
        MOV     r1, #0                  ; No pages processed yet
increase_loop$l
        CMP     r1, r10                 ; Have all pages been unlocked
        BGE     done$l                  ; Exit loop when all done
;        BGT     done$l                  ; Exit loop when all done
        STR     r2, [r0, #virtual_ptr]  ; Clear the page pointer
        ADD     r0, r0, #virtual_item   ; Increment page pointer
        ADD     r1, r1, #1              ; Increment number of page
        B       increase_loop$l         ; Loop for next page
decrease$l
        LDR     r0, [r11, #area_total_pages]; Get current size of area
        TEQ     r0, #0                  ; Is area already zero size
        ADREQL  r0, err_dze             ; Pointer to error block
        BEQ     error$l                 ; Generate error if zero sized
        LDR     r2, [r11, #area_virtual_array]; Pointer to page array
        MOV     r3, #virtual_item       ; Size of virtual array entries
        MLA     r1, r0, r3, r2          ; Pointer to virtual array entry
        MOV     r3, r10                 ; Copy required change
        MOV     r10, #0                 ; No change in size yet
decrease_loop$l
        SUB     r1, r1, #virtual_item   ; Pointer to next page in array
        CMP     r1, r2                  ; Has end of array been reached
        BLT     done$l                  ; Exit loop if no more pages
        CMP     r10, r3                 ; Have sufficient pages been processed
        BLE     done$l                  ; Exit loop if enough pages
        LDR     r0, [r1, #virtual_ptr]  ; Get the page pointer
        BIC     r0, r0, #virtual_flags_used; Make page unused
        STR     r0, [r1, #virtual_ptr]  ; Store modified flags
        BIC     r0, r0, #virtual_flags_mask; Clear flag bits
        TEQ     r0, #0                  ; Is any page allocated
        BEQ     decrease_next$l         ; Skip to next page if not
        LDR     r4, [r0, #source_locks] ; Get number of times locked
        TEQ     r4, #0                  ; Is page locked
        BNE     done$l                  ; Exit loop if page locked
        BL      release_page            ; Free the page
        BVS     done$l                  ; Exit loop if unable to release page
decrease_next$l
        SUB     r10, r10, #1            ; Another page that can be removed
        B       decrease_loop$l         ; Loop for next page
done$l  TEQ     r10, #0                 ; Is there any change in size
        ADREQL  r0, err_dls             ; Pointer to error block
        BEQ     error$l                 ; Generate error if no change
        pipe_string "change_virtualise: pages change = &%0", r10
        BL      size$l                  ; Set the new file size
        BLVS    tidy$l                  ; Turn the hourglass off
        MOVVS   r1, #0                  ; No change if error
        RTE VS                          ; Exit if an error produced
        LDR     r0, [r11, #area_total_pages]; Get old number of pages in area
        ADD     r0, r0, r10             ; Adjust number of pages
        STR     r0, [r11, #area_total_pages]; Update number of pages in area
        LDR     r0, ws_virtual_pages    ; Get number of virtual memory pages
        ADD     r0, r0, r10             ; Adjust number of pages
        STR     r0, ws_virtual_pages    ; Store new number of pages
        MOV     r1, #Service_MemoryMoved; Reason code for area change
        SWI     XOS_ServiceCall         ; Let other modules know about change
        CMP     r10, #0                 ; In which direction is the change
        RSBLT   r10, r10, #0            ; Negate change if negative
        LDR     r0, ws_log_page_size    ; Get the log of the page size
        MOV     r1, r10, LSL r0         ; Convert change in pages to bytes
        BL      adjust_memory           ; Optimise memory usage
        BL      tidy$l                  ; Tidy up as required
        pipe_string "change_virtualise: changed = &%0", r1
        RTSS                            ; Return from subroutine

        ; Pass on to standard OS routine if not virtual memory
simple$l
        LDR     r2, ws_dynamic_area     ; Get the pool dynamic area number
        TEQ     r0, r2                  ; Is this the virtual memory pool
        BEQ     go$l                    ; Skip next bit if it is
        MOV     r2, r1                  ; Copy change in size of area
        TEQ     r0, #OS_DynamicAreaFreePool; Is it the free pool
        RSBEQ   r2, r2, #0              ; Reverse direction if it is
        CMP     r2, #0                  ; Is the change positive
        BLGT    ensure_free             ; Ensure sufficient memory available
go$l    BL      os_change_dynamic_area  ; Pass on to OS routine
        RTE VS                          ; Keep any error produced
        RTS                             ; Return from subroutine

        ; Generate an error message and return
error$l MOV     r10, #0                 ; No change if error to be returned
        BL      tidy$l                  ; Tidy up if required
        ADR     r1, ws_message          ; Pointer to messages control block
        MOV     r2, #0                  ; Use internal buffer
        SWI     XMessageTrans_ErrorLookup; Lookup the error text
        MOV     r1, #0                  ; No change in size
        RTE                             ; Return from subroutine

        ; Turn the hourglass off and call any dynamic area handler
tidy$l  JSR     "r0"                    ; Stack registers
        [       :LNOT: :DEF: demo
        MOVVC   r0, r10                 ; Copy actual change in size
        MOVVS   r0, #0                  ; No change if error
        BL      post_call_handler       ; Call the post-move handler
        ]
        SWI     XHourglass_Off          ; Turn the hourglass off
        RTSS                            ; Return from subroutine

        ; Update the swap file size
size$l  JSR     "r0-r2, r8-r9"          ; Stack registers
        LDR     r0, [r11, #area_total_pages]; Get old number of pages in area
        ADD     r0, r0, r10             ; Add change in size
        LDR     r1, ws_log_page_size    ; Get log base 2 of page size
        MOV     r9, r0, LSL r1          ; Required size in bytes
        MOV     r0, #OSArgs_ReadAllocation; Reason code to read allocation
        LDR     r1, [r11, #area_file_handle]; Get swap file handle
        SWI     XOS_Args                ; Read allocated size of file
        RTE VS                          ; Exit if error produced
        pipe_string "change_virtualise: allocated = &%0, required = &%1", r2, r9
        SUBS    r8, r9, r2              ; Required change in file size
        RTS MI                          ; Exit if file is large enough
        ADD     r0, r11, #area_file_name; Pointer to filename
        BL      free_space              ; Check the free space on the device
        LDR     r1, ws_auto_sw_disc     ; Disc space to leave free
        pipe_string "change_virtualise: leave = &%0, free = &%1", r1, r0
        SUBS    r0, r0, r1              ; Available disc space
        BMI     size_error$l            ; Return an error if insufficient
        CMP     r8, r0                  ; Is there sufficient space
        BHI     size_error$l            ; Return an error if insufficient
size_skip$l
        MOV     r0, #OSArgs_SetAllocation; Reason code to set allocation
        LDR     r1, [r11, #area_file_handle]; Get swap file handle
        MOV     r2, r9                  ; Copy required size
        SWI     XOS_Args                ; Set the file allocation
        RTE VS                          ; Exit if error produced
        pipe_string "change_virtualise: reserved = &%0", r2
        CMP     r2, r9                  ; Compare to required size
        RTS GE                          ; Return if sufficiently large
        MOV     r0, #OSArgs_SetExt      ; Reason code to set extent
        MOV     r2, r9                  ; Copy required size
        SWI     XOS_Args                ; Set the file extent
        RTE VS                          ; Exit if error produced
        RTS                             ; Return from subroutine

size_error$l
        ADRL    r0, err_ddf             ; Pointer to error block
        ADR     r1, ws_message          ; Pointer to messages control block
        MOV     r2, #0                  ; Use internal buffer
        SWI     XMessageTrans_ErrorLookup; Lookup the error text
        RTE                             ; Return from subroutine

        ;   Parameters  : r1    - The start of the address range to find.
        ;                 r2    - The end of the address range to find.
        ;   Returns     : r0    - Pointer to the virtual area record, or 0 if
        ;                         address range does not lie within a virtual
        ;                         dynamic area.
        ;                 r1    - The page offset for the start address.
        ;                 r2    - The page offset for the end address.
        ;   Description : Find the virtual dynamic area which contains the
        ;                 specified address range.
find_area
        LocalLabels
        JSR     "r3-r5"                 ; Stack registers
        CMP     r1, r2                  ; Is any region enclosed
        MOVHI   r0, #0                  ; Value for failure
        RTS HI                          ; Exit if no region specified
        LDR     r3, ws_log_page_size    ; Get log of the page size
        MOV     r1, r1, LSR r3          ; Convert start to page offset
        MOV     r2, r2, LSR r3          ; Convert end to page offset
        LDR     r0, ws_area_ptr         ; Pointer to the first area record
loop$l  TEQ     r0, #0                  ; Is it a valid pointer
        RTS EQ                          ; Return from subroutine if not
        LDR     r4, [r0, #area_base]    ; Get base address of virtual area
        MOV     r4, r4, LSR r3          ; Convert base to page offset
        LDR     r5, [r0, #area_total_pages];Get number of pages in area
        ADD     r5, r4, r5              ; Get end page offset of area
        CMP     r1, r4                  ; Compare lower bound
        BLT     next$l                  ; If outside then try next area
        CMP     r2, r5                  ; Compare upper bound
        BLT     found$l                 ; If inside then exit loop
next$l  LDR     r0, [r0, #area_next]    ; Pointer to next record if no match
        B       loop$l                  ; Loop for next record if not found
found$l SUB     r1, r1, r4              ; Subtract base from lower bound
        SUB     r2, r2, r4              ; Subtract base from upper bound
        RTS                             ; Return from subroutine

        ;   Parameters  : r1    - The start of the address range to find.
        ;                 r2    - The end of the address range to find.
        ;   Returns     : r0    - Pointer to the virtual area record, or 0 if
        ;                         address range does not lie even partially
        ;                         within a virtual dynamic area.
        ;                 r1    - The page offset for the start of the
        ;                         overlapping region.
        ;                 r2    - The page offset for the end if the
        ;                         overlapping region.
        ;   Description : Find the virtual dynamic area which contains at least
        ;                 part of the specified address range.
find_part
        LocalLabels
        JSR     "r3-r5"                 ; Stack registers
        CMP     r1, r2                  ; Is any region enclosed
        MOVGT   r0, #0                  ; Value for failure
        RTS GT                          ; Exit if no region specified
        LDR     r3, ws_log_page_size    ; Get log of the page size
        MOV     r1, r1, LSR r3          ; Convert start to page offset
        MOV     r2, r2, LSR r3          ; Convert end to page offset
        LDR     r0, ws_area_ptr         ; Pointer to the first area record
loop$l  TEQ     r0, #0                  ; Is it a valid pointer
        RTS EQ                          ; Return from subroutine if not
        LDR     r4, [r0, #area_base]    ; Get base address of virtual area
        MOV     r4, r4, LSR r3          ; Convert base to page offset
        LDR     r5, [r0, #area_total_pages];Get number of pages in area
        ADD     r5, r4, r5              ; Get end page offset of area
        CMP     r2, r4                  ; Compare lower bound
        BLT     next$l                  ; If outside then try next area
        CMP     r1, r5                  ; Compare upper bound
        BLT     found$l                 ; If inside then exit loop
next$l  LDR     r0, [r0, #area_next]    ; Pointer to next record if no match
        B       loop$l                  ; Loop for next record if not found
found$l CMP     r1, r4                  ; Compare lower bound
        MOVLT   r1, r4                  ; Clip against lower bound
        CMP     r2, r5                  ; Compare upper bound
        SUBGE   r2, r5, #1              ; Clip against upper bound
        SUB     r1, r1, r4              ; Subtract base from lower bound
        SUB     r2, r2, r4              ; Subtract base from upper bound
        RTS                             ; Return from subroutine

        ;   Parameters  : r0    - Pointer to error block.
        ;                 r11   - Pointer to area record.
        ;   Returns     : r0    - Pointer to error block or 0 if problem
        ;                         resolved.
        ;                 r11   - Preserved.
        ;   Description : Check if error was due to swap file being closed.
        ;                 If it was then attempt to reopen it.
reopen_swap_file
        LocalLabels
        JSR     "r1, r11"               ; Stack registers
        LDR     r1, [r0]                ; Get error number
        TEQ     r1, #Error_Channel      ; Is it file handle closed
        TEQNE   r1, #Error_NotOpenForUpdate; Is it file not open for update
        RTSS NE                         ; Return with existing error if not
        MOV     r0, #OSFind_Openup:OR:OSFind_NoPath; Reason code for old file
        ADD     r1, r11, #area_file_name; Pointer to filename
        SWI     XOS_Find                ; Open the file for update
        RTE VS                          ; Exit if error
        STR     r0, [r11, #area_file_handle]; Store file handle
        MOV     r0, #0                  ; Clear error pointer
        RTS                             ; Return from subroutine

        ;   Parameters  : None
        ;   Returns     : None
        ;   Description : Each swap file is checked in turn whether the file
        ;                 handle is valid and refers to the correct file. If it
        ;                 doesn't then it is reopened.
reopen_all_swap_files
        LocalLabels
        JSR     "r0-r1"                 ; Stack registers
        ADR     r0, call_back$l         ; Routine to call on CallBack
        MOV     r1, r12                 ; Value of r12 required
        SWI     XOS_AddCallBack         ; Add a transient CallBack.
        RTSS                            ; Return from subroutine

        ; The actual reopening is performed on a transient CallBack
call_back$l
        JSR     "r0-r3, r5, r11"        ; Stack registers
        LDR     r11, ws_area_ptr        ; Get pointer to first area record
loop$l  TEQ     r11, #0                 ; Is it a valid pointer
        RTSS EQ                         ; Return from subroutine if not
        MOV     r0, #OSArgs_ReadPath    ; Reason code to read filename
        LDR     r1, [r11, #area_file_handle]; Get current file handle
        ADRL    r2, ws_string           ; Pointer to buffer
        MOV     r5, #String             ; Size of buffer
        SWI     XOS_Args                ; Read the canonicalised file name
        BVS     reopen$l                ; If failed then reopen file
        ADRL    r0, ws_string           ; Pointer to the start of this name
        ADD     r1, r11, #area_file_name; Required filename
check$l LDRB    r2, [r0], #1            ; Read character from one string
        Lower   r2                      ; Convert it to lower case
        LDRB    r3, [r1], #1            ; Read character from other string
        Lower   r3                      ; Convert it to lower case
        TEQ     r2, r3                  ; Are the characters the same
        BNE     reopen$l                ; Reopen file if not
        TEQ     r2, #0                  ; Is it a terminator
        BNE     check$l                 ; If not loop for next character
skip$l  MOV     r0, #OSArgs_ReadInfo    ; Reason code to read file details
        LDR     r1, [r11, #area_file_handle]; Get current file handle
        SWI     XOS_Args                ; Read information about file
        TST     r0, #OSArgs_StreamUnallocated; Is stream allocated
        BNE     reopen$l                ; Must reopen if it is not
        B       done$l
reopen$l
        MOV     r0, #OSFind_Openup:OR:OSFind_NoPath; Reason code for old file
        ADD     r1, r11, #area_file_name; Pointer to filename
        SWI     XOS_Find                ; Open the file for update
        STRVC   r0, [r11, #area_file_handle]; Store file handle if no error
done$l  LDR     r11, [r11, #area_next]  ; Pointer to next area record
        B       loop$l                  ; Loop for next area

; A literal pool

        LTORG

; Handle locking and unlocking of regions and pages

        ;   Parameters  : r0    - The start address of the area to lock.
        ;                 r1    - The end address of the area to lock.
        ;   Returns     : None
        ;   Description : Lock the specified address range.
lock_region
        LocalLabels
        JSR     "r0-r2"                 ; Stack registers
        SUB     r2, r1, #1              ; Make range inclusive
        MOV     r1, r0                  ; Copy start address
        BL      find_area               ; Find which area the address is within
        TEQ     r0, #0                  ; Was an area found
        ADREQL  r0, err_lna             ; Get pointer to error block
        BEQ     error$l                 ; Generate error if not found
        BL      lock_pages              ; Lock the specified pages
        RTE VS                          ; Exit if error produced
        RTS                             ; Return from subroutine

        ;   Parameters  : r0    - The start address of the area to lock.
        ;                 r1    - The end address of the area to lock.
        ;   Returns     : None
        ;   Description : Unlock the specified address range.
unlock_region
        JSR     "r0-r2"                 ; Stack registers
        SUB     r2, r1, #1              ; Make range inclusive
        MOV     r1, r0                  ; Copy start address
        BL      find_area               ; Find which area the address is within
        TEQ     r0, #0                  ; Was an area found
        ADREQL  r0, err_lna             ; Get pointer to error block
        BEQ     error$l                 ; Generate error if not found
        BL      unlock_pages            ; Lock the specified pages
        RTE VS                          ; Exit if error produced
        RTS                             ; Return from subroutine

        ; Generate an error message and return
error$l ADR     r1, ws_message          ; Pointer to messages control block
        MOV     r2, #0                  ; Use internal buffer
        SWI     XMessageTrans_ErrorLookup; Lookup the error text
        RTE                             ; Return from subroutine

        ;   Parameters  : r0    - Pointer to the area to lock.
        ;                 r1    - The first page to lock.
        ;                 r2    - The last page to lock.
        ;   Returns     : None
        ;   Description : Lock the specified range of pages.
lock_pages
        LocalLabels
;        debug_record "lock_pages"
        JSR     "r0-r6"                 ; Stack registers
        MOV     r3, #0                  ; Value to clear command flag with
        STR     r3, ws_command_flag     ; Clear command flag
        MOV     r4, r2                  ; Copy end page number
        MOV     r3, r1                  ; Copy start page number
        MOV     r1, r0                  ; Copy area pointer
        MOV     r2, r3                  ; Copy start page number again
        MOV     r0, #virtual_item       ; Size of virtual array entries
        LDR     r5, [r1, #area_virtual_array]; Pointer to page array
        MLA     r5, r2, r0, r5          ; Get pointer into page array
        MOV     r0, #0                  ; Number of pages to newly lock
count_loop$l
        CMP     r2, r4                  ; Have all pages been counted
        BGT     count_done$l            ; Exit loop when all done
        LDR     r6, [r5, #virtual_ptr]  ; Get the page pointer
        BIC     r6, r6, #virtual_flags_mask; Clear mask bits
        TEQ     r6, #0                  ; Is page present
        ADDEQ   r0, r0, #1              ; Increment count if not
        BEQ     count_next$l            ; Skip to next one if not
        LDR     r6, [r6, #source_locks] ; Get number of times locked
        TEQ     r6, #0                  ; Is it locked
        ADDEQ   r0, r0, #1              ; Increment count if not
count_next$l
        ADD     r2, r2, #1              ; Increment page number
        ADD     r5, r5, #virtual_item   ; Advance page pointer
        B       count_loop$l            ; Loop for next page
count_done$l
        SWI     XHourglass_On           ; Turn the hourglass on
        RTE VS                          ; Exit with an error
        LDR     r2, ws_target_pages     ; Amount of memory currently available
        LDR     r5, ws_locked_pages     ; Number of immovable pages
        SUB     r2, r2, r5              ; Number of available pages
        SUB     r2, r2, #min_spare_pages; Exclude required spare pages
        SUBS    r0, r0, r2              ; Number of outstanding pages
        MOVMI   r0, #0                  ; Ensure not negative
        BL      ensure_memory           ; Claim any memory required
        MOV     r2, r3                  ; Copy start page number
        MOV     r0, #virtual_item       ; Size of virtual array entries
        LDR     r5, [r1, #area_virtual_array]; Pointer to page array
        MLA     r5, r2, r0, r5          ; Get pointer into page array
lock_loop$l
        CMP     r2, r4                  ; Have all pages been locked
        BGT     lock_done$l             ; Exit loop when all done
        LDR     r0, [r5, #virtual_ptr]  ; Get the page pointer
        BIC     r0, r0, #virtual_flags_mask; Mask out flag bits
        TEQ     r0, #0                  ; Is page present
        BEQ     lock_none$l             ; Branch if no page already there
        BL      push_page               ; Page it in
        BVS     lock_fail$l             ; Failed for some reason
        LDR     r6, [r0, #source_locks] ; Get number of times locked
        TEQ     r6, #0                  ; Is page locked
        BNE     lock_got$l              ; Branch if page is already locked
        MOV     r6, r0                  ; Copy current page pointer
        BL      choose_low_page         ; Choose a replacement page
        BVS     lock_fail$l             ; Failed for some reason
        TEQ     r0, #0                  ; Was a page found
        BEQ     lock_fail$l             ; Failed for some reason
        STMFD   r13!, {r1}              ; Stack register
        MOV     r1, r6                  ; Copy new page pointer
        BL      swap_pages              ; Swap the pages over
        LDMFD   r13!, {r1}              ; Restore register
        BVS     lock_fail$l             ; Failed for some reason
        B       lock_got$l              ; Now got a suitable page
lock_release$l
        MOV     r0, r6                  ; Restore the page pointer
        BL      release_page            ; Attempt to release the page
        BVS     lock_fail$l             ; Failed for some reason
        B       lock_unused$l           ; Now got an unused page
lock_none$l
        BL      choose_low_page         ; Get a page to assign
        BVS     lock_fail$l             ; Failed for some reason
        TEQ     r0, #0                  ; Was a page found
        BEQ     lock_fail$l             ; Failed for some reason
        LDR     r6, [r0, #source_area]  ; Where is the page
        TEQ     r6, #0                  ; Is the page in use
        BEQ     lock_unused$l           ; Skip next bit if the page is unused
        MOV     r6, r0                  ; Copy page pointer
        BL      choose_page             ; Choose another page
        BVS     lock_release$l          ; Try to actually release a page
        TEQ     r0, #0                  ; Was a page found
        BVS     lock_release$l          ; Try to actually release a page
        TEQ     r0, r6                  ; Does the page match
        BEQ     lock_unused$l           ; Skip next bit if the page was freed
        STMFD   r13!, {r1}              ; Stack register
        MOV     r1, r6                  ; Copy the old page pointer
        BL      swap_pages              ; Swap the pages over
        LDMFD   r13!, {r1}              ; Restore the register
        BVS     lock_fail$l             ; Failed for some reason
        MOV     r0, r6                  ; Copy old page pointer
lock_unused$l
        BL      assign_page             ; Assign the page
        BVS     lock_fail$l             ; Failed for some reason
        LDR     r0, [r5, #virtual_ptr]  ; Get pointer to the new page
        BIC     r0, r0, #virtual_flags_mask; Mask out flag bits
lock_got$l
        LDR     r6, [r0, #source_locks] ; Get number of times locked
        ADD     r6, r6, #1              ; Increment lock counter
        STR     r6, [r0, #source_locks] ; Store new number of times locked
        LDR     r0, ws_locked_pages     ; Get number of locked pages
        ADD     r0, r0, #1              ; Increment number of locked pages
        CMP     r6, #1                  ; Is it a newly locked page
        STREQ   r0, ws_locked_pages     ; Store new number of locked pages
        ADD     r2, r2, #1              ; Increment page number
        ADD     r5, r5, #virtual_item   ; Advance page pointer
        B       lock_loop$l             ; Loop for next page
lock_done$l
        BL      adjust_memory           ; Optimise memory usage
        SWI     XHourglass_Off          ; Turn the hourglass off
        RTS                             ; Return from subroutine

        ; Handle failure during the locking stage
lock_fail$l
        MOV     r0, r1                  ; Copy page pointer
        MOV     r1, r3                  ; Copy start page
        SUB     r2, r2, #1              ; Get end page
        BL      unlock_pages            ; Undo the locking
        BL      adjust_memory           ; Optimise memory usage
        SWI     XHourglass_Off          ; Turn the hourglass off
        ADRL    r0, err_lcp             ; Pointer to error block
        ADR     r1, ws_message          ; Pointer to messages control block
        MOV     r2, #0                  ; Use internal buffer
        SWI     XMessageTrans_ErrorLookup; Lookup the error text
        RTE                             ; Return from subroutine

        ;   Parameters  : r0    - Pointer to the area to unlock.
        ;                 r1    - The first page to unlock.
        ;                 r2    - The last page to unlock.
        ;   Returns     : None
        ;   Description : Unlock the specified range of pages.
unlock_pages
        LocalLabels
;        debug_record "unlock_pages"
        JSR     "r0-r4"                 ; Stack registers
        MOV     r3, #0                  ; Value to clear command flag with
        STR     r3, ws_command_flag     ; Clear command flag
        LDR     r3, [r0, #area_virtual_array]; Pointer to page array
        MOV     r0, #virtual_item       ; Size of virtual array entries
        MLA     r0, r1, r0, r3          ; Pointer to virtual array entry
loop$l  CMP     r1, r2                  ; Have all pages been unlocked
        BGT     done$l                  ; Exit loop when all done
        LDR     r3, [r0, #virtual_ptr]  ; Get the page pointer
        BIC     r3, r3, #virtual_flags_mask; Mask out flag bits
        TEQ     r3, #0                  ; Is page present
        BEQ     next$l                  ; Skip next bit if not
        LDR     r4, [r3, #source_locks] ; Get number of times locked
        SUBS    r4, r4, #1              ; Decrement number of times locked
        BMI     next$l                  ; Skip next bit if was not locked
        STR     r4, [r3, #source_locks] ; Store new number of times locked
        BGT     next$l                  ; Skip next bit if still locked
        LDR     r3, ws_locked_pages     ; Get locked page count
        SUB     r3, r3, #1              ; Decrement locked pages
        STR     r3, ws_locked_pages     ; Store modified locked page count
next$l  ADD     r0, r0, #virtual_item   ; Increment page pointer
        ADD     r1, r1, #1              ; Increment number of page
        B       loop$l                  ; Loop for next page
done$l  BL      adjust_memory           ; Optimise memory usage
        RTS                             ; Return from subroutine

; Handle the dynamic area used to obtain physical memory

        ;   Parameters  : None
        ;   Returns     : None
        ;   Description : Create the physical dynamic area.
create_dynamic
        LocalLabels
        JSR     "r0-r8"                 ; Stack registers
        MOV     r0, #OSMemory_ReadSize\
                :OR: (OSMemory_TypeDRAM << OSMemory_TypeShift)
        SWI     XOS_Memory              ; Get amount of DRAM and page size
        RTE VS                          ; Exit without wiping over r0
        MOV     r3, r1                  ; Copy number of pages of DRAM
        STR     r2, ws_page_size        ; Store page size
        MOV     r0, #OSMemory_ReadSize\
                :OR: (OSMemory_TypeVRAM << OSMemory_TypeShift)
        SWI     XOS_Memory              ; Get amount of VRAM and page size
        RTE VS                          ; Exit without wiping over r0
        ADD     r1, r1, r3              ; Number of pages of RAM
        STR     r1, ws_max_pages        ; Initial guess at maximum pages
        MOV     r1, #0                  ; Initial log value
        MOV     r0, #1                  ; A constant unity
log_loop$l
        ADD     r1, r1, #1              ; Try the next log value
        CMP     r2, r0, LSL r1          ; Is it large enough
        BHI     log_loop$l              ; Loop if it is not large enough
        STR     r1, ws_log_page_size    ; Store the log of the page size
        LDR     r0, ws_max_pages        ; Number of pages to allocate
        MOV     r1, #source_item        ; Memory required per page
        MUL     r3, r0, r1              ; Memory required for all pages
        MOV     r0, #OSModule_Alloc     ; Claim entry code
        BL      os_module               ; Claim memory for pages
        RTE VS                          ; Exit if error
        STR     r2, ws_source_array     ; Store pointer to the memory
        STR     r2, ws_circle           ; Store as most recently used page
        MOV     r0, #OSDynamicArea_Create; Reason code to create new area
        MOV     r1, #OSDynamicArea_AllocateArea; OS chooses area number
        MOV     r2, #0                  ; Initial size of area in bytes
        MOV     r3, #OSDynamicArea_AllocateBase; OS chooses logical address
        LDR     r4, = &103              ; Inaccessible in user mode
        MOV     r5, #-1                 ; Maximum size is RAM in machine
        ADR     r6, dynamic_handler     ; Pointer to handler routine
        MOV     r7, r12                 ; Value of r12 to pass handler
        ADR     r8, name$l              ; Name of the area
        SWI     XOS_DynamicArea         ; Create the dynamic area
        RTE VS                          ; Exit without wiping over r0
        STR     r1, ws_dynamic_area     ; Store the number of the area
        STR     r3, ws_dynamic_area_ptr ; Store the base of the area
        LDR     r0, ws_log_page_size    ; Get log of page size
        MOV     r1, r2, LSR r0          ; Calculate initial number of pages
        STR     r1, ws_total_pages      ; Store initial number of pages
        STR     r1, ws_target_pages     ; Store number of pages to use
        MOV     r1, r5, LSR r0          ; Calculate maximum number of pages
        STR     r1, ws_max_pages        ; Store maximum number of pages
        MOV     r0, #0                  ; Number of initially locked pages
        STR     r0, ws_locked_pages     ; No locked pages yet
        STR     r0, ws_virtual_pages    ; No virtual memory yet
        STR     r0, ws_virtual_areas    ; No virtual memory dynamic areas
        STR     r0, ws_area_ptr         ; No virtual areas yet
        STR     r0, ws_used_pages       ; No pages used yet
        STR     r0, ws_faults_ldr       ; No page faults yet
        STR     r0, ws_faults_str       ; No page faults yet
        STR     r0, ws_faults_ldm       ; No page faults yet
        STR     r0, ws_faults_stm       ; No page faults yet
        STR     r0, ws_faults_ldc       ; No page faults yet
        STR     r0, ws_faults_stc       ; No page faults yet
        STR     r0, ws_faults_swp       ; No page faults yet
        STR     r0, ws_pull_count       ; No pages chosen yet
        STR     r0, ws_fifo_count       ; Start from a default count value
        STR     r0, ws_command_flag     ; Clear the command flag
        STR     r0, ws_auto_flag        ; Not an internal memory movement
        LDR     r0, = replace_policy    ; Which policy to start with
        STR     r0, ws_replace_policy   ; Default page replacement policy
        LDR     r1, ws_log_page_size    ; Get log of page size
        LDR     r0, = auto_user_frac    ; Default user minimum fraction
        STR     r0, ws_auto_user_frac   ; Store user minimum fraction
        LDR     r0, = auto_user_max     ; Default user maximum
        MOV     r0, r0, LSR r1          ; Convert user maximum to pages
        STR     r0, ws_auto_user_max    ; Store user maximum size
        LDR     r0, = auto_user_min     ; Default user minimum
        MOV     r0, r0, LSR r1          ; Convert user minimum to pages
        STR     r0, ws_auto_user_min    ; Store user minimum size
        LDR     r0, = auto_sw_free      ; Default software minimum free
        MOV     r0, r0, LSR r1          ; Convert software minimum free to pages
        STR     r0, ws_auto_sw_free     ; Store software minimum free memory
        LDR     r0, = auto_sw_min       ; Default software minimum memory
        MOV     r0, r0, LSR r1          ; Convert software minimum to pages
        STR     r0, ws_auto_sw_min      ; Store software minimum memory
        LDR     r0, = auto_sw_keep      ; Default software memory to keep
        MOV     r0, r0, LSR r1          ; Convert software to keep to pages
        STR     r0, ws_auto_sw_keep     ; Store software memory to keep
        LDR     r0, = auto_sw_disc      ; Default software disc space to leave
        STR     r0, ws_auto_sw_disc     ; Store software disc space to leave
        RTS                             ; Return from subroutine

name$l  =       "Virtualise pool", 0    ; Name of the dynamic area
        ALIGN

        ;   Parameters  : None
        ;   Returns     : None
        ;   Description : Destroy the physical dynamic area.
destroy_dynamic
        LocalLabels
        JSR     "r0-r1"                 ; Stack registers
        MOV     r0, #OSDynamicArea_Delete; Reason code to remove area
        LDR     r1, ws_dynamic_area     ; The number of the area to remove
        SWI     XOS_DynamicArea         ; Delete the dynamic area
        RTE VS                          ; Exit without wiping over r0
        MOV     r0, #OSModule_Free      ; Free entry code
        LDR     r2, ws_source_array     ; Pointer to memory block
        BL      os_module               ; Release workspace
        RTSS                            ; Return from subroutine

        ;   Parameters  : r0    - Reason code
        ;                           0: PreGrow - Issued just before pages are
        ;                              moved to grow an area.
        ;                           1: PostGrow - Issued just after pages are
        ;                              moved to grow an area.
        ;                           2: PreShrink - Issued just before pages are
        ;                              moved to shrink an area.
        ;                           3: PostShrink - Issued just after pages are
        ;                              moved to shrink an area.
        ;                           4: TestShrink - Issued to check how much of
        ;                              current size may be regarded as free.
        ;                 r2    - Number of entries in page block. (grow only)
        ;                 r3    - Change in area size in bytes.
        ;                 r4    - Current/new size of area in bytes.
        ;                 r5    - Page size in bytes.
        ;                 r12   - Pointer to workspace.
        ;   Returns     : r3    - Amount area can shrink by in bytes for
        ;                         PreShrink and TestShrink only.
        ;   Description : The dynamic area handler.
dynamic_handler
        LocalLabels
        JSR     "r6"                    ; Stack registers
;        debug_record "Dynamic area pool handler entry"
        BL      decode$l                ; Decode the reason code
        MOV     r6, #0                  ; Value to clear command flag with
        STR     r6, ws_command_flag     ; Clear the command flag
;        debug_record "Dynamic area pool handler exit"
        RTS                             ; Return from subroutine

        ; Decode the dynamic area handler reason code
decode$l
        JSR     "r0-r2, r4-r7"          ; Stack registers
        TEQ     r0, #0                  ; Is it PreGrow
        BEQ     pre_grow$l              ; Branch to routine to handle PreGrow
        TEQ     r0, #1                  ; Is it PostGrow
        BEQ     post_grow$l             ; Branch to routine to handle PostGrow
        TEQ     r0, #2                  ; Is it PreShrink
        BEQ     pre_shrink$l            ; Branch to routine to handle PreShrink
        TEQ     r0, #3                  ; Is it PostShrink
        BEQ     post_shrink$l           ; Branch to routine to handle PostShrink
        TEQ     r0, #4                  ; Is it TestShrink
        BEQ     post_shrink$l           ; Branch to routine to handle TestShrink
        RTS                             ; Return from subroutine

        ; Handle PreGrow reason code
pre_grow$l
        LDR     r0, ws_virtual_areas    ; Number of virtual memory areas
        LDR     r1, ws_auto_flag        ; Is this an internal operation
        TEQ     r0, #0                  ; Is there any virtual memory
        TEQEQ   r1, #0                  ; Is this an internal operation
        BEQ     pre_grow_fail$l         ; Unable to grow if no virtual memory
        LDR     r0, ws_total_pages      ; How many pages at the moment
        LDR     r1, ws_virtual_pages    ; How much virtual memory is there
        ADD     r0, r0, r2              ; How many pages after this addition
        SUB     r0, r0, r1              ; How many pages more than required
        CMP     r0, #min_spare_pages    ; How does that compare to spare
        RTSS LE                         ; Accept the memory if not too much
pre_grow_fail$l
        ADRL    r0, err_mpl             ; Pointer to error block
        B       error$l                 ; Generate an error message

        ; Handle PostGrow reason code
post_grow$l
        LDR     r0, ws_total_pages      ; Get old number of pages
        LDR     r4, ws_log_page_size    ; Get log base 2 of page size
        LDR     r6, ws_dynamic_area_ptr ; Base of dynamic area
        ADD     r6, r6, r0, LSL r4      ; Pointer to first new page
        ADD     r4, r0, r2              ; New total number of pages
        STR     r4, ws_total_pages      ; Store new total number of pages
        LDR     r4, ws_source_array     ; Pointer to page array
        MOV     r5, #source_item        ; Size of each entry in array
        MLA     r0, r5, r0, r4          ; Pointer to correct position
        LDR     r5, ws_page_size        ; Get the page size
post_grow_loop$l
        SUBS    r2, r2, #1              ; Decrement page count
        BMI     post_grow_done$l        ; Allow pages to be used when done
        LDR     r4, [r1, #OS_PageBlock_page_no]; Get physical page number
        STR     r4, [r0, #source_number]; Store physical page number
        STR     r6, [r0, #source_ptr]   ; Store pointer to page
        MOV     r4, #0                  ; A simple constant to initialise
        STR     r4, [r0, #source_area]  ; Page is not in a virtual area
        STR     r4, [r0, #source_page]  ; Page is not currently allocated
        STR     r4, [r0, #source_locks] ; Page is not currently locked
        BL      count_end               ; Clear access count
        ADD     r1, r1, #OS_PageBlock   ; Increment page block pointer
        ADD     r0, r0, #source_item    ; Increment page array pointer
        ADD     r6, r6, r5              ; Increment page pointer
        B       post_grow_loop$l        ; Loop for next page
post_grow_done$l
        LDR     r0, ws_total_pages      ; Get new total number of pages
        STR     r0, ws_target_pages     ; Store number of pages to use
        BL      auto_update$l           ; Update how much memory to claim
        RTSS                            ; Return from subroutine

        ; Handle PreShrink reason code
pre_shrink$l
        LDR     r2, ws_total_pages      ; Current number of pages
        LDR     r0, ws_auto_sw_keep     ; Minimum amount of memory to keep
        LDR     r1, ws_auto_flag        ; Is this an internal operation
        TEQ     r1, #0                  ; Check the internal flag
        MOVEQ   r0, #0                  ; No lower limit for internal changes
        LDR     r1, ws_virtual_pages    ; Amount of virtual memory
        CMP     r0, r1                  ; Should all the memory be kept
        MOVLT   r1, r0                  ; Use the minimum to keep if smaller
        LDR     r0, ws_locked_pages     ; Number of locked pages
        ADD     r0, r0, #min_spare_pages; Include the required spare pages
        CMP     r0, r1                  ; Is it larger than the previous limit
        MOVGT   r1, r0                  ; Use this new value if it is
        LDR     r0, ws_virtual_areas    ; Number of virtual memory areas
        TEQ     r0, #0                  ; Is there any virtual memory
        MOVEQ   r1, #0                  ; All can be scrapped if none
        SUB     r0, r2, r1              ; Number of pages that can be freed
pre_shrink_all$l
        CMP     r0, #0                  ; Can any memory be released
        ADRLEL  r0, err_mlk             ; Pointer to error block
        BLE     error$l                 ; Generate error
        LDR     r1, ws_log_page_size    ; Log base 2 of page size
        CMP     r0, r3, LSR r1          ; Compare available to requested
        MOVGT   r0, r3, LSR r1          ; Truncate available if required
        SUB     r2, r2, r0              ; Target number of pages
        STR     r2, ws_target_pages     ; Store number of pages to use
        MOV     r2, r0                  ; Copy the number of pages
        MOV     r3, #0                  ; No pages released yet
        LDR     r4, ws_source_array     ; Pointer to page array
        MOV     r5, #source_item        ; Size of each entry in array
        LDR     r0, ws_total_pages      ; Current number of pages
        MLA     r4, r5, r0, r4          ; Pointer to correct position
        SWI     XHourglass_On           ; Turn the hourglass on
        RTE VS                          ; Exit if error produced
pre_shrink_loop$l
        CMP     r2, r3                  ; Check if all pages processed
        MOVLT   r3, r2                  ; Clip processed pages if required
        BLE     pre_shrink_done$l       ; Exit loop if finished
        SUB     r4, r4, #source_item    ; Position pointer for next page
        LDR     r0, [r4, #source_area]  ; Get the usage of this page
        TEQ     r0, #0                  ; Is the page in use
        BEQ     pre_shrink_next$l       ; Skip this page if it is free
        LDR     r0, [r4, #source_locks] ; Get locked status of the page
        TEQ     r0, #0                  ; Is the page locked
        BNE     pre_shrink_done$l       ; Abort release if it is
        BL      choose_page             ; Find the best page to free
        BVS     pre_shrink_this$l       ; Free this if none better
        TEQ     r0, #0                  ; Was any page found
        BEQ     pre_shrink_this$l       ; Free this if none better
        TEQ     r0, r4                  ; Was this page chosen (not possible)
        BEQ     pre_shrink_next$l       ; Next page if it was
        MOV     r1, r4                  ; Copy page pointer to free
        BL      swap_pages              ; Try to swap the pages
        BVS     pre_shrink_done$l       ; Abort release if error produced
        B       pre_shrink_next$l       ; Otherwise this page is free
pre_shrink_this$l
        MOV     r0, r4                  ; Copy page pointer
        BL      release_page            ; Release this page
        BVS     pre_shrink_done$l       ; Abort release if error produced
pre_shrink_next$l
        ADD     r3, r3, #1              ; Increment processed page count
        B       pre_shrink_loop$l       ; Loop for next page
pre_shrink_done$l
        SWI     XHourglass_Off          ; Turn the hourglass off
        LDR     r1, ws_log_page_size    ; Log base 2 of page size
        MOV     r3, r3, LSL r1          ; Calculate allowed size of shrinkage
        CMP     r3, #0                  ; Can any pages be freed
        ADRLEL  r0, err_mmv             ; Pointer to error block
        BLE     error$l                 ; Generate error
        RTSS                            ; Return from subroutine

        ; Handle PostShrink reason code
post_shrink$l
        LDR     r0, ws_total_pages      ; Get old number of pages
        LDR     r1, ws_log_page_size    ; Log base 2 of page size
        SUB     r0, r0, r2, LSR r1      ; New total number of pages
        STR     r0, ws_total_pages      ; Store new total number of pages
        STR     r0, ws_target_pages     ; Store number of pages to use
        BL      auto_update$l           ; Update how much memory to claim
        RTSS                            ; Return from subroutine

        ; Handle TestShrink reason code
test_shrink$l
        MOV     r3, #0                  ; Not shrinkable for now
        RTSS                            ; Return from subroutine
;        LDR     r2, ws_total_pages      ; Current number of pages
;        LDR     r0, ws_auto_sw_keep     ; Minimum amount of memory to keep
;        LDR     r1, ws_auto_flag        ; Is this an internal operation
;        TEQ     r1, #0                  ; Check the internal flag
;        MOVEQ   r0, #0                  ; No lower limit for internal changes
;        LDR     r1, ws_virtual_pages    ; Amount of virtual memory
;        CMP     r0, r1                  ; Should all the memory be kept
;        MOVLT   r1, r0                  ; Use the minimum to keep if smaller
;        LDR     r0, ws_locked_pages     ; Number of locked pages
;        ADD     r0, r0, #min_spare_pages; Include the required spare pages
;        CMP     r0, r1                  ; Is it larger than the previous limit
;        MOVGT   r1, r0                  ; Use this new value if it is
;        LDR     r0, ws_virtual_areas    ; Number of virtual memory areas
;        TEQ     r0, #0                  ; Is there any virtual memory
;        MOVEQ   r1, #0                  ; All can be scrapped if none
;        SUB     r0, r2, r1              ; Number of pages that can be freed
;pre_shrink_all$l
;        CMP     r0, #0                  ; Can any memory be released
;        ADRLEL  r0, err_mlk             ; Pointer to error block
;        BLE     error$l                 ; Generate error
;        LDR     r1, ws_log_page_size    ; Log base 2 of page size
;        CMP     r0, r3, LSR r1          ; Compare available to requested
;        MOVGT   r0, r3, LSR r1          ; Truncate available if required
;        SUB     r2, r2, r0              ; Target number of pages
;        STR     r2, ws_target_pages     ; Store number of pages to use
;        MOV     r2, r0                  ; Copy the number of pages
;        MOV     r3, #0                  ; No pages released yet
;        LDR     r4, ws_source_array     ; Pointer to page array
;        MOV     r5, #source_item        ; Size of each entry in array
;        LDR     r0, ws_total_pages      ; Current number of pages
;        MLA     r4, r5, r0, r4          ; Pointer to correct position
;        SWI     XHourglass_On           ; Turn the hourglass on
;        RTE VS                          ; Exit if error produced
;pre_shrink_loop$l
;        CMP     r2, r3                  ; Check if all pages processed
;        MOVLT   r3, r2                  ; Clip processed pages if required
;        BLE     pre_shrink_done$l       ; Exit loop if finished
;        SUB     r4, r4, #source_item    ; Position pointer for next page
;        LDR     r0, [r4, #source_area]  ; Get the usage of this page
;        TEQ     r0, #0                  ; Is the page in use
;        BEQ     pre_shrink_next$l       ; Skip this page if it is free
;        LDR     r0, [r4, #source_locks] ; Get locked status of the page
;        TEQ     r0, #0                  ; Is the page locked
;        BNE     pre_shrink_done$l       ; Abort release if it is
;        BL      choose_page             ; Find the best page to free
;        BVS     pre_shrink_this$l       ; Free this if none better
;        TEQ     r0, #0                  ; Was any page found
;        BEQ     pre_shrink_this$l       ; Free this if none better
;        TEQ     r0, r4                  ; Was this page chosen (not possible)
;        BEQ     pre_shrink_next$l       ; Next page if it was
;        MOV     r1, r4                  ; Copy page pointer to free
;        BL      swap_pages              ; Try to swap the pages
;        BVS     pre_shrink_done$l       ; Abort release if error produced
;        B       pre_shrink_next$l       ; Otherwise this page is free
;pre_shrink_this$l
;        MOV     r0, r4                  ; Copy page pointer
;        BL      release_page            ; Release this page
;        BVS     pre_shrink_done$l       ; Abort release if error produced
;pre_shrink_next$l
;        ADD     r3, r3, #1              ; Increment processed page count
;        B       pre_shrink_loop$l       ; Loop for next page
;pre_shrink_done$l
;        SWI     XHourglass_Off          ; Turn the hourglass off
;        LDR     r1, ws_log_page_size    ; Log base 2 of page size
;        MOV     r3, r3, LSL r1          ; Calculate allowed size of shrinkage
;        CMP     r3, #0                  ; Can any pages be freed
;        ADRLEL  r0, err_mmv             ; Pointer to error block
;        BLE     error$l                 ; Generate error
;        RTSS                            ; Return from subroutine

        ; Generate an error message and return
error$l LDR     r1, ws_total_pages      ; Get current number of pages
        STR     r1, ws_target_pages     ; Restore number of pages to use
        ADR     r1, ws_message          ; Pointer to messages control block
        MOV     r2, #0                  ; Use internal buffer
        SWI     XMessageTrans_ErrorLookup; Lookup the error text
        MOV     r3, #0                  ; Ensure no memory moved
        RTE                             ; Return from subroutine

        ; Update amount of memory automatically claimed
auto_update$l
        JSR     "r0-r3"                 ; Stack registers
        LDR     r1, ws_auto_flag        ; Check the internal flag
        TEQ     r1, #0                  ; Is it a user action
        RTSS NE                         ; Exit if not
        MOV     r2, r0                  ; Copy new number of pages
        MOV     r0, #OS_DynamicAreaFreePool; Dynamic area number for free pool
        BL      os_read_dynamic_area    ; Read amount of free memory
        LDR     r0, ws_log_page_size    ; Log base 2 of page size
        MOV     r0, r1, LSR r0          ; Convert free memory to pages
        CMP     r2, r0, LSL#1           ; Is it at least 2/3 of the memory
        STRHI   r2, ws_auto_user_max    ; Store new maximum
        CMP     r0, r2, LSL#1           ; Is it less that 1/3 of the memory
        STRHI   r2, ws_auto_user_min    ; Store new minimum
        LDR     r1, ws_auto_user_min    ; Get current minimum
        CMP     r2, r1                  ; Compare to current minimum
        STRLO   r2, ws_auto_user_min    ; Store new minimum if lower
        LDR     r1, ws_virtual_pages    ; Number of pages of virtual memory
        TEQ     r1, #0                  ; Is there any virtual memory
        RTSS EQ                         ; Exit if not
        MOV     r2, r2, LSL#auto_user_frac_shift; Scale as required
        DivRem  r0, r2, r1, r3          ; Ratio of total size to
        STR     r0, ws_auto_user_frac   ; Store fraction of total size
        RTSS                            ; Return from subroutine

        ;   Parameters  : r0    - The name of an object on the device to check.
        ;   Returns     : r0    -
        ;   Description : Read the free disc space on the specified device.
        ;                 If an error occurs or the size is too large for
        ;                 reliable manipulation then a dummy large size is
        ;                 returned.
free_space
        JSR     "r1-r3"                 ; Stack registers
        MOV     r3, r0                  ; Copy the filename pointer
        MOV     r1, r3                  ; Copy the filename pointer again
        MOV     r0, #OSFSControl_FreeSpace64; Reason code to read free space
        SWI     XOS_FSControl           ; Check the free space
        BVS     small$l                 ; Try the older call if failed
        TEQ     r1, #0                  ; Is the free space over 4GB
        BNE     fake$l                  ; Fake a suitable size if it is
        B       ok$l                    ; A reasonable size otherwise
small$l MOV     r1, r3                  ; Copy the filename pointer again
        MOV     r0, #OSFSControl_FreeSpace; Reason code to read free space
        SWI     XOS_FSControl           ; Check the free space
        BVS     fake$l                  ; If still problems then fake the size
ok$l    TST     r0, #1 << 31            ; Is the top bit set
        RTSS EQ                         ; Accept the returned size if not
fake$l  LDR     r0, = &7fffffff         ; Use a dummy free space value
        RTSS                            ; Return from subroutine

        ;   Parameters  : r0    - The requested change in size of memory.
        ;   Returns     : r0    - The difference between the amount requested
        ;                         and the actual change.
        ;   Description : Attempt to change the amount of memory used by the
        ;                 specified amount.
ensure_memory
        LocalLabels
        JSR     "r1-r4"                 ; Stack registers
        MOV     r3, r0                  ; Copy requested change
        LDR     r2, ws_log_page_size    ; Log base 2 of page size
        MOV     r1, r0, ASL r2          ; Size in bytes to claim
        LDR     r0, ws_dynamic_area     ; The area to change
        STR     r0, ws_auto_flag        ; Set internal flag
        BL      os_change_dynamic_area  ; Request the extra memory
        MOV     r4, #0                  ; Value to clear flag with
        STR     r4, ws_auto_flag        ; Clear the internal flag
        RTE VS                          ; Exit if error produced
        MOV     r1, r1, LSR r2          ; Convert change to pages
        CMP     r3, #0                  ; Was requested change negative
        RSBLT   r1, r1, #0              ; Negate change if it was
        SUB     r0, r1, r3              ; Work out difference
        RTS                             ; Return from subroutine

        ;   Parameters  : None
        ;   Returns     : None
        ;   Description : Attempt to optimise the amount of memory claimed.
adjust_memory
        LocalLabels
        JSR     "r0-r1"                 ; Stack registers
        ADR     r0, call_back$l         ; Routine to call on CallBack
        MOV     r1, r12                 ; Value of r12 required
        SWI     XOS_AddCallBack         ; Add a transient CallBack
        RTSS                            ; Return from subroutine

        ; The actual adjustment is performed on a transient CallBack
call_back$l
        JSR     "r0-r3"                 ; Stack registers
        MOV     r0, #OS_DynamicAreaFreePool; Dynamic area number for free pool
        BL      os_read_dynamic_area    ; Read amount of free memory
        LDR     r0, ws_log_page_size    ; Log base 2 of page size
        MOV     r0, r1, LSR r0          ; Convert free memory to pages
        LDR     r1, ws_total_pages      ; How much used for virtual memory
        ADD     r0, r0, r1              ; Free plus virtual memory
        LDR     r1, ws_auto_sw_free     ; Minimum pages to leave unused
        SUB     r1, r0, r1              ; Maximum memory to use
        STR     r1, ws_auto_sw_max      ; Store software maximum memory
        LDR     r0, ws_auto_user_frac   ; Get user specified fraction
        LDR     r1, ws_virtual_pages    ; Total number of virtual pages
        MUL     r0, r1, r0              ; Shifted user size
        MOV     r0, r0, LSR#auto_user_frac_shift; Shift to correct value
        LDR     r1, ws_auto_user_max    ; Get user specified maximum memory
        CMP     r0, r1                  ; Compare user minimum with maximum
        MOVLT   r1, r0                  ; Take the minimum
        LDR     r0, ws_auto_user_min    ; Get user specified minimum
        CMP     r0, r1                  ; Compare current with user minimum
        MOVGT   r1, r0                  ; Take the maximum
        LDR     r0, ws_auto_sw_min      ; Get software specified minimum memory
        CMP     r0, r1                  ; Compare current with software minimum
        MOVGT   r1, r0                  ; Take the maximum
        LDR     r0, ws_auto_sw_max      ; Get software specified maximum memory
        CMP     r0, r1                  ; Compare current with software maximum
        MOVLT   r1, r0                  ; Take the minimum
        LDR     r0, ws_auto_sw_keep     ; Get minimum number of pages to keep
        CMP     r0, r1                  ; Compare current with minimum to keep
        MOVGT   r1, r0                  ; Take the maximum
        LDR     r0, ws_locked_pages     ; Number of locked pages
        ADD     r0, r0, #min_spare_pages; Lower limit on memory usage
        CMP     r0, r1                  ; Compare current with minimum required
        MOVGT   r1, r0                  ; Take the maximum
        LDR     r0, ws_virtual_pages    ; Total number of virtual pages
        ADD     r0, r0, #min_spare_pages; Upper limit on memory useage
        CMP     r0, r1                  ; Compare current with maximum required
        MOVLT   r1, r0                  ; Take the minimum
        LDR     r0, ws_virtual_areas    ; Get number of virtual dynamic areas
        TEQ     r0, #0                  ; Are there any active areas
        MOVEQ   r1, #0                  ; No memory required if not
        LDR     r0, ws_total_pages      ; Current number of pages
        SUB     r1, r1, r0              ; Change in number of pages
        LDR     r0, ws_log_page_size    ; Log base 2 of page size
        MOV     r2, r1, ASL r0          ; Size in bytes to change
loop$l  TEQ     r2, #0                  ; Is there any change
        RTSS EQ                         ; Exit if not
        MOV     r1, r2                  ; Copy size to change
        LDR     r0, ws_dynamic_area     ; The area to change
        STR     r0, ws_auto_flag        ; Set internal flag
        BL      os_change_dynamic_area  ; Request the extra memory
        MOV     r3, #0                  ; Value to clear flag with
        STR     r3, ws_auto_flag        ; Clear the internal flag
        RTSS VC                         ; Return if succeeded
        CMP     r2, #0                  ; Was requested change positive
        RTSS LT                         ; Exit if not
        LDR     r1, ws_page_size        ; Get page size
        SUB     r2, r2, r1              ; Decrement requested size
        B       loop$l                  ; Loop for another attempt

        ;   Parameters  : None
        ;   Returns     : None
        ;   Description : Attempt to minimise the amount of memory claimed.
minimise_memory
        LocalLabels
        JSR     "r0-r2"                 ; Stack registers
        LDR     r1, ws_locked_pages     ; Number of locked pages
        ADD     r1, r1, #min_spare_pages; Lower limit on memory usage
        LDR     r0, ws_virtual_areas    ; Get number of virtual areas
        TEQ     r0, #0                  ; Are there any active areas
        MOVEQ   r1, #0                  ; Ideal memory is 0 if not
        LDR     r0, ws_total_pages      ; Current number of pages
        SUB     r1, r1, r0              ; Change in number of pages
        LDR     r0, ws_log_page_size    ; Log base 2 of page size
        MOV     r1, r1, ASL r0          ; Size in bytes to change
        TEQ     r1, #0                  ; Is there any change
        RTS EQ                          ; Exit if not
        LDR     r0, ws_dynamic_area     ; The area to change
        STR     r0, ws_auto_flag        ; Set internal flag
        BL      os_change_dynamic_area  ; Request the extra memory
        MOV     r0, #0                  ; Value to clear flag with
        STR     r0, ws_auto_flag        ; Clear the internal flag
        RTSS                            ; Return from subroutine

        ;   Parameters  : r2    - Amount of memory required to be free.
        ;   Returns     : None
        ;   Description : Ensure the specified amount of memory is available
        ;                 within the free pool.
ensure_free
        LocalLabels
        JSR     "r0-r2"                 ; Stack registers
        MOV     r0, #OS_DynamicAreaFreePool; Dynamic area number for free pool
        BL      os_read_dynamic_area    ; Find size of free pool
        SUB     r1, r1, r2              ; Amount of extra memory remaining
        LDR     r0, ws_log_page_size    ; Log base 2 of page size
        MOV     r1, r1, ASR r0          ; Convert memory remaining to pages
        LDR     r0, ws_auto_sw_free     ; Amount of free memory required
        SUB     r1, r1, r0              ; Subtract required free memory
        LDR     r0, ws_total_pages      ; Get current number of pages
        LDR     r2, ws_auto_sw_keep     ; Get minimum number of pages to keep
        SUB     r0, r2, r0              ; Maximum change in number of pages
        CMP     r0, #0                  ; Minimum is no change
        MOVGT   r0, #0                  ; Clip maximum change if required
        CMP     r1, r0                  ; Compare requested to maximum change
        MOVLT   r1, r0                  ; Clip requested change if required
        LDR     r0, ws_log_page_size    ; Log base 2 of page size
        MOV     r1, r1, ASL r0          ; Convert required change to bytes
        CMP     r1, #0                  ; Check direction of change
        RTSS GE                         ; Exit if sufficient memory
        LDR     r0, ws_dynamic_area     ; Get number of dynamic area
        STR     r0, ws_auto_flag        ; Set internal flag
        BL      os_change_dynamic_area  ; Attempt to release the memory
        MOV     r0, #0                  ; Value to clear flag with
        STR     r0, ws_auto_flag        ; Clear the internal flag
        RTSS                            ; Return from subroutine

        ;   Parameters  : r0    - Amount of address space to allocate when a
        ;                         maximum size of -1 is passed to
        ;                         OS_DynamicArea 0, or -1 to read value.
        ;                 r1    - Amount of memory to claim automatically,
        ;                         or -1 to read current value.
        ;                 r2    - Amount of memory to leave free, or -1 to
        ;                         read value.
        ;   Returns     : r0    - The address space limit.
        ;                 r1    - The amount of memory to claim.
        ;                 r2    - The amount of memory to leave free.
        ;   Description : Set or read various configuration items.
configure
        LocalLabels
        JSR     "r9-r10"                ; Stack registers
        LDR     r9, ws_page_size        ; Page size in bytes
        CMP     r0, #-1                 ; Was a new size specified
        BEQ     nr0$l                   ; Skip next bit if not
        CMP     r0, r9                  ; Compare to page size
        MOVLT   r0, r9                  ; Minimum is a single page
        CMP     r0, #max_area_size      ; Is it larger than maximum limit
        MOVGT   r0, #max_area_size      ; Clip against maximum limit
        STR     r0, ws_os_memory_limit  ; Store new logical address space limit
nr0$l   LDR     r9, ws_log_page_size    ; Log base 2 of the page size
        LDR     r10, ws_max_pages       ; Get maximum number of pages
        CMP     r1, #-1                 ; Was a new size specified
        BEQ     nr1$l                   ; Skip next bit if not
        MOVLT   r1, #0                  ; Minimum is zero
        SUB     r1, r1, #1              ; Decrement to correct rounding
        MOV     r1, r1, ASR r9          ; Convert to pages
        ADD     r1, r1, #1              ; Round up to nearest page
        CMP     r1, r10                 ; Compare to actual memory size
        MOVGT   r1, r10                 ; Limit to actual memory
        STR     r1, ws_auto_sw_min      ; Store new minimum memory to claim
nr1$l   CMP     r2, #-1                 ; Was a new size specified
        BEQ     nr2$l                   ; Skip next bit if not
        MOVLT   r2, #0                  ; Minimum is zero
        SUB     r2, r2, #1              ; Decrement to correct rounding
        MOV     r2, r2, ASR r9          ; Convert to pages
        ADD     r2, r2, #1              ; Round up to nearest page
        CMP     r2, r10                 ; Compare to actual memory size
        MOVGT   r2, r10                 ; Limit to actual memory
        STR     r2, ws_auto_sw_free     ; Store new minimum memory to leave free
nr2$l   LDR     r0, ws_os_memory_limit  ; Get logical address space limit
        LDR     r1, ws_auto_sw_min      ; Get minimum memory to claim
        MOV     r1, r1, LSL r9          ; Convert to bytes
        LDR     r2, ws_auto_sw_free     ; Get minimum memory to leave free
        MOV     r2, r2, LSL r9          ; Convert to bytes
        BL      adjust_memory           ; Optimise amount of memory used
        RTSS                            ; Return from subroutine

        ;   Parameters  : r0    - Set the fraction of the logical virtual
        ;                         memory size to claim as physical memory, or
        ;                         -1 to read the current value. This is a
        ;                         fixed point value, with (1 << 15)
        ;                         corresponding to 100%.
        ;                 r1    - Lower limit of memory to claim, below which
        ;                         all of the required memory is claimed, or -1
        ;                         to read the current value.
        ;                 r2    - Upper limit of memory to claim, above which
        ;                         no more memory will be claimed, or -1 to
        ;                         read the current value.
        ;                 r3    - Amount of memory to keep even when another
        ;                         task is attempting to claim memory, or -1
        ;                         to read the current value.
        ;                 r4    - Amount of disc space to always leave free,
        ;                         or -1 to read the current value.
        ;   Returns     : r0    - Fraction of required memory to claim.
        ;                 r1    - Minimum memory to claim, in bytes.
        ;                 r2    - Maximum memory to claim, in bytes.
        ;                 r3    - Memory to keep, in bytes.
        ;                 r4    - Disc space to leave free, in bytes.
        ;   Description : Set or read various user configuration items.
user_configure
        LocalLabels
        JSR     "r9-r10"                ; Stack registers
        LDR     r9, ws_log_page_size    ; Log base 2 of the page size
        LDR     r10, ws_max_pages       ; Get maximum number of pages
        CMP     r0, #-1                 ; Was a new fraction specified
        BEQ     nr0$l                   ; Skip next bit if not
        MOVLT   r0, #0                  ; Minimum is zero
        CMP     r0, #1 << auto_user_frac_shift; Compare to maximum allowed
        MOVGT   r0, #1 << auto_user_frac_shift; Clip against maximum limit
        STR     r0, ws_auto_user_frac   ; Store new fraction
nr0$l   CMP     r1, #-1                 ; Was a new size specified
        BEQ     nr1$l                   ; Skip next bit if not
        MOVLT   r1, #0                  ; Minimum is zero
        SUB     r1, r1, #1              ; Decrement to correct rounding
        MOV     r1, r1, ASR r9          ; Convert to pages
        ADD     r1, r1, #1              ; Round up to nearest page
        CMP     r1, r10                 ; Compare to actual memory size
        MOVGT   r1, r10                 ; Limit to actual memory
        STR     r1, ws_auto_user_min    ; Store new lower limit
nr1$l   CMP     r2, #-1                 ; Was a new size specified
        BEQ     nr2$l                   ; Skip next bit if not
        MOVLT   r2, #0                  ; Minimum is zero
        SUB     r2, r2, #1              ; Decrement to correct rounding
        MOV     r2, r2, ASR r9          ; Convert to pages
        ADD     r2, r2, #1              ; Round up to nearest page
        CMP     r2, r10                 ; Compare to actual memory size
        MOVGT   r2, r10                 ; Limit to actual memory
        STR     r2, ws_auto_user_max    ; Store new upper limit
nr2$l   CMP     r3, #-1                 ; Was a new size specified
        BEQ     nr3$l                   ; Skip next bit if not
        MOVLT   r3, #0                  ; Minimum is zero
        SUB     r3, r3, #1              ; Decrement to correct rounding
        MOV     r3, r3, ASR r9          ; Convert to pages
        ADD     r3, r3, #1              ; Round up to nearest page
        CMP     r3, r10                 ; Compare to actual memory size
        MOVGT   r3, r10                 ; Limit to actual memory
        STR     r3, ws_auto_sw_keep     ; Store new keep limit
nr3$l   CMP     r4, #-1                 ; Was a new size specified
        BEQ     nr4$l                   ; Skip next bit if not
        STR     r4, ws_auto_sw_disc     ; Store new leave disc free limit
nr4$l   LDR     r0, ws_auto_user_frac   ; Fraction of required memory to claim
        LDR     r1, ws_auto_user_min    ; Get lower limit
        MOV     r1, r1, LSL r9          ; Convert to bytes
        LDR     r2, ws_auto_user_max    ; Get upper limit
        MOV     r2, r2, LSL r9          ; Convert to bytes
        LDR     r3, ws_auto_sw_keep     ; Get limit to keep
        MOV     r3, r3, LSL r9          ; Convert to bytes
        LDR     r4, ws_auto_sw_disc     ; Get disc space to leave free limit
        BL      adjust_memory           ; Optimise amount of memory used
        RTSS                            ; Return from subroutine

; A literal pool

        LTORG

; Simple page operations upon virtual memory

        ;   Parameters  : r0    - The page to assign.
        ;                 r1    - Pointer to virtual area to assign page to.
        ;                 r2    - The page offset to assign within the area.
        ;   Returns     : None
        ;   Description : Assign a page to virtual memory. It is assumed that
        ;                 the page is not being used.
assign_page
        LocalLabels
;        debug_record "assign_page"
        JSR     "r0-r4, r10-r11"        ; Stack registers
;        pipe_string "Assign single page %0 to [%1:%2]", r0, r1, r2
        assert_source r0, "assign_page", "AP"
        assert_area r1, "assign_page", "AP"
        MOV     r10, r0                 ; Copy page pointer
        MOV     r11, r1                 ; Copy area pointer
        STR     r11, [r10, #source_area]; Store pointer to virtual area
        STR     r2, [r10, #source_page] ; Store virtual page number
        LDR     r0, [r11, #area_virtual_array]; Pointer to virtual array
        MOV     r1, #virtual_item       ; Size of virtual array entries
        MLA     r0, r2, r1, r0          ; Pointer to virtual array entry
        LDR     r0, [r0, #virtual_ptr]  ; Get the flags for the logical page
        TST     r0, #virtual_flags_used ; Has page been modified
        BEQ     clear$l                 ; No need to read from disc if not
        LDR     r1, [r11, #area_file_handle]; Get the file handle
        LDR     r2, [r10, #source_ptr]  ; Pointer to page in memory
        LDR     r3, ws_page_size        ; Number of bytes to read
        LDR     r4, [r10, #source_page] ; The virtual area page number
        LDR     r0, ws_log_page_size    ; Log base 2 of page size
        MOV     r4, r4, LSL r0          ; File byte offset
        MOV     r0, #OSGBPB_ReadAt      ; Reason code to read data
        SWI     XOS_GBPB                ; Load the page from disc
        BVC     read$l                  ; Skip retry attempt if successful
        BL      reopen_swap_file        ; Attempt to reopen swap file
        BVS     exit$l                  ; Exit if error produced
        MOV     r0, #OSGBPB_ReadAt      ; Reason code to read data
        LDR     r1, [r11, #area_file_handle]; Get the file handle
        SWI     XOS_GBPB                ; Load the page from disc
        BVS     exit$l                  ; Exit if error produced
        B       read$l                  ; File has been read
clear$l LDR     r0, [r10, #source_ptr]  ; Pointer to the page in memory
        LDR     r1, ws_page_size        ; Get the size of the pages
        MOV     r2, #0                  ; Value to clear page with
loop$l  SUBS    r1, r1, #4              ; Decrement position pointer
        BMI     read$l                  ; Exit loop when clear complete
        STR     r2, [r0, r1]            ; Write word to page
        B       loop$l                  ; Loop for next word
read$l  ADRL    r0, ws_mem_map_request  ; Pointer to request list
        LDR     r1, [r10, #source_number]; Get the physical page number
        STR     r1, [r0, #OS_MemMapRequest_page_no]; Set the physical page
        LDR     r1, [r10, #source_page] ; The virtual area page number
        LDR     r2, ws_log_page_size    ; Log base 2 of page size
        MOV     r1, r1, LSL r2          ; Byte offset from base
        LDR     r2, [r11, #area_base]   ; The virtual area base address
        ADD     r1, r1, r2              ; Get new logical address
        STR     r1, [r0, #OS_MemMapRequest_map]; Set logical address
        LDR     r1, [r11, #area_access] ; Get protection level
        STR     r1, [r0, #OS_MemMapRequest_access]; Set protection level
        MOV     r1, #-1                 ; Request list terminator
        STR     r1, [r0, #OS_MemMapRequest]; Terminate the request list
        SWI     XOS_SetMemMapEntries    ; Remap the page
        BVS     exit$l                  ; Exit if error produced
        LDR     r0, [r11, #area_used_pages]; Get number of physical pages
        ADD     r0, r0, #1              ; Increment number of physical pages
        STR     r0, [r11, #area_used_pages]; Store number of physical pages
        LDR     r2, [r10, #source_page] ; Get virtual page number
        LDR     r0, [r11, #area_virtual_array]; Pointer to virtual array
        MOV     r1, #virtual_item       ; Size of virtual array entries
        MLA     r0, r2, r1, r0          ; Pointer to virtual array entry
        LDR     r1, [r0, #virtual_ptr]  ; Get logical page details
        AND     r1, r1, #virtual_flags_mask; Only keep the flags
        ORR     r1, r1, #virtual_flags_paged :OR: virtual_flags_used
        ORR     r1, r10, r1             ; Combine flags and pointer
        STR     r1, [r0, #virtual_ptr]  ; Store pointer to physical page
        MOV     r0, #0                  ; Initial number of locks
        STR     r0, [r10, #source_locks]; Clear any locked status
        MOV     r0, r10                 ; Copy page pointer
        BL      count_start             ; Clear access counter
        BL      count_inc               ; Update access counter
        LDR     r0, ws_used_pages       ; Get current page usage
        ADD     r0, r0, #1              ; Increment usage count
        STR     r0, ws_used_pages       ; Store new usage count
        RTSS                            ; Return from subroutine

        ; Undo changes before returning an error
exit$l  MOV     r1, #0                  ; Value to clear pointers with
        STR     r1, [r10, #source_area] ; Clear pointer to virtual area
        STR     r1, [r10, #source_page] ; Clear virtual page number
        RTE                             ; Return with the error

        ;   Parameters  : r0    - The page to release.
        ;   Returns     : None
        ;   Description : Release a page. Note that the locked status of the
        ;                 page is ignored.
release_page
        LocalLabels
;        debug_record "release_page"
        JSR     "r0-r4, r10-r11"        ; Stack registers
;        pipe_string "Release single page %0", r0
        assert_source r0, "release_page", "RP"
        MOV     r10, r0                 ; Copy page pointer
        LDR     r11, [r10, #source_area]; Get pointer to virtual area
        ADRL    r0, ws_mem_map_request  ; Pointer to request list
        LDR     r1, [r10, #source_number]; Get the physical page number
        STR     r1, [r0, #OS_MemMapRequest_page_no]; Set the physical page
        LDR     r1, [r10, #source_ptr]  ; Get original logical address
        STR     r1, [r0, #OS_MemMapRequest_map]; Set logical address
        MOV     r1, #3                  ; Inaccessible in user mode
        STR     r1, [r0, #OS_MemMapRequest_access]; Set inaccessible in usr
        MOV     r1, #-1                 ; Request list terminator
        STR     r1, [r0, #OS_MemMapRequest]; Terminate the request list
        SWI     XOS_SetMemMapEntries    ; Remap the page
        RTE VS                          ; Exit if error produced
        LDR     r0, [r11, #area_virtual_array]; Pointer to array
        LDR     r1, [r10, #source_page] ; Number of page in virtual area
        MOV     r2, #virtual_item       ; Size of virtual array entries
        MLA     r0, r1, r2, r0          ; Pointer to virtual array entry
        LDR     r1, [r0, #virtual_ptr]  ; Get virtual page entry
        TST     r1, #virtual_flags_used ; Has page been modified
        BEQ     wrote$l                 ; Do not write to disc if not
        LDR     r1, [r11, #area_file_handle]; Get the file handle
        LDR     r2, [r10, #source_ptr]  ; Pointer to page in memory
        LDR     r3, ws_page_size        ; Number of bytes to read
        LDR     r4, [r10, #source_page] ; The virtual area page number
        LDR     r0, ws_log_page_size    ; Log base 2 of page size
        MOV     r4, r4, LSL r0          ; File byte offset
        MOV     r0, #OSGBPB_WriteAt     ; Reason code to write data
        SWI     XOS_GBPB                ; Write the page to disc
        BVC     wrote$l                 ; Skip retry attempt if successful
        BL      reopen_swap_file        ; Attempt to reopen swap file
        RTE VS                          ; Exit if error produced
        MOV     r0, #OSGBPB_WriteAt     ; Reason code to write data
        LDR     r1, [r11, #area_file_handle]; Get the file handle
        SWI     XOS_GBPB                ; Load the page from disc
        RTE VS                          ; Exit if error produced
wrote$l LDR     r1, [r11, #area_used_pages]; Get physical page count
        SUB     r1, r1, #1              ; Decrement physical page count
        STR     r1, [r11, #area_used_pages]; Store physical page count
        LDR     r0, [r11, #area_virtual_array]; Pointer to array
        LDR     r1, [r10, #source_page] ; Number of page in virtual area
        MOV     r2, #virtual_item       ; Size of virtual array entries
        MLA     r0, r1, r2, r0          ; Pointer to virtual array entry
        MOV     r1, #0                  ; Value to clear entry with
        STR     r1, [r10, #source_area] ; Clear virtual area pointer
        STR     r1, [r10, #source_page] ; Clear virtual page pointer
        LDR     r1, [r0, #virtual_ptr]  ; Get virtual page entry
        AND     r1, r1, #virtual_flags_mask; Only keep the flags
        BIC     r1, r1, #virtual_flags_paged; No longer paged in
        STR     r1, [r0, #virtual_ptr]  ; Clear virtual page entry
        LDR     r0, ws_used_pages       ; Get current page usage
        SUB     r0, r0, #1              ; Decrement usage count
        STR     r0, ws_used_pages       ; Store new usage count
        MOV     r0, r10                 ; Copy page pointer
        BL      count_end               ; Clear access counter
        RTSS                            ; Return from subroutine

        ;   Parameters  : r0    - The area to check.
        ;   Returns     : r0    - Maximum number of pages to read or write.
        ;   Description : Calculate the maximum number of pages to read or
        ;                 write in a single operation when not avoidable.
        ;                 This is always a power of two.
multiple_number
        LocalLabels
        JSR     "r1, r11"               ; Stack registers
        assert_area r0, "multiple_number", "MN"
        MOV     r11, r0                 ; Copy area record pointer
        LDR     r1, ws_target_pages     ; Get size of virtual memory pool
        MOV     r1, r1, LSR#mult_shift_pool; Calculate limit from pool size
        LDR     r0, [r11, #area_total_pages]; Get logical size of area
        MOV     r0, r0, LSR#mult_shift_logical; Calculate limit from logical
        CMP     r1, r0                  ; Compare number against logical limit
        MOVGT   r1, r0                  ; Clip against logical size limit
        LDR     r0, [r11, #area_used_pages]; Get physical size of area
        MOV     r0, r0, LSR#mult_shift_physical; Calculate limit from physical
        CMP     r1, r0                  ; Compare number against physical limit
        MOVGT   r1, r0                  ; Clip against physical size limit
        CMP     r1, #1                  ; Must process at least one page
        MOVLT   r1, #1                  ; Clip against lower limit
        MOV     r0, #1
loop$l  MOV     r1, r1, LSR#1           ; Shift original value
        TEQ     r1, #0                  ; Has it been shifted to zero
        RTSS EQ                         ; Exit if it has
        MOV     r0, r0, LSL#1           ; Shift new value
        B       loop$l                  ; Loop until done

        ;   Parameters  : r0    - The first page to release.
        ;                 r1    - The maximum offset of pages to release.
        ;   Returns     : None
        ;   Description : Release multiple pages starting from the one
        ;                 specified. Each page within the specified range
        ;                 within the same area is checked and individually
        ;                 released if possible.
multiple_release_pages
        LocalLabels
        JSR     "r0-r3, r11"            ; Stack registers
        assert_source r0, "multiple_release_pages", "MRP"
        LDR     r11, [r0, #source_area] ; Get the area pointer
        LDR     r2, [r11, #area_total_pages]; Get total number of pages
        LDR     r3, [r0, #source_page]  ; Get the page number
        SUB     r2, r2, r3              ; Maximum possible pages to process
        CMP     r1, r2                  ; Is end of array reached
        MOVGT   r1, r2                  ; Clip number of pages if it is
        LDR     r2, [r11, #area_virtual_array]; Get pointer to page array
        MOV     r0, #virtual_item       ; Get the size of entries in the array
        MLA     r2, r0, r3, r2          ; Get pointer to correct entry in array
loop$l  SUBS    r1, r1, #1              ; Decrement number of pages left
        RTSS MI                         ; Exit if all done
        LDR     r0, [r2, #virtual_ptr]  ; Get the page pointer
        ADD     r2, r2, #virtual_item   ; Advance to the next page
        BIC     r0, r0, #virtual_flags_mask; Clear any flags
        TEQ     r0, #0                  ; Is page used
        BEQ     loop$l                  ; Loop for next page if not
        LDR     r3, [r0, #source_locks] ; Get number of times locked
        TEQ     r3, #0                  ; Is page locked
        BNE     loop$l                  ; Loop for next page if it is
        BL      release_page            ; Release page if not locked
        RTE VS                          ; Exit with error
        B       loop$l                  ; Loop for next page

        ;   Parameters  : r0    - The page to assign.
        ;                 r1    - Pointer to virtual area to assign page to.
        ;                 r2    - The page offset to assign within the area.
        ;   Returns     : None
        ;   Description : Assign a page to virtual memory. It is assumed that
        ;                 the page is not being used. If possible read-ahead
        ;                 is used in an attempt to improve performance.
assign_more_pages
        LocalLabels
;        debug_record "assign_more_pages"
        JSR     "r0-r4"                 ; Stack registers
        assert_source r0, "assign_more_pages", "AMP"
        assert_area r1, "assign_more_pages", "AMP"
        LDR     r3, ws_replace_policy   ; Get the page replacement policy
        TST     r3, #policy_flags_reads ; Should multiple page reads be used
        BEQ     single$l                ; Only assign a single page if not
        MOV     r4, r0                  ; Copy source page pointer
        MOV     r3, r2                  ; Copy page offset to assign
        MOV     r2, r1                  ; Copy pointer to area record
        MOV     r0, r1                  ; Copy area record pointer
        BL      multiple_number         ; Choose how many pages to assign
        MOV     r1, r0, LSR#1           ; Half of the number of pages for read
        CMP     r1, #1                  ; Check that it is at least one page
        MOVLT   r1, #1                  ; Make it one page if not
        MOV     r0, r4                  ; Copy source page pointer
        BL      align_read$l            ; Assign the pages
        LDR     r4, [r2, #area_virtual_array]; Pointer to virtual array
        MOV     r1, #virtual_item       ; Size of virtual array entries
        MLA     r1, r3, r1, r4          ; Pointer to virtual array entry
        LDR     r1, [r1, #virtual_ptr]  ; Get the page pointer
        BIC     r1, r1, #virtual_flags_mask; Mask out flags
        TEQ     r1, #0                  ; Is a page present
        BEQ     undo$l                  ; Start again if page not present
        BL      swap_pages              ; Ensure page is in correct position
        RTS                             ; Return from subroutine

        ; Only read a single page
single$l
        BL      assign_page             ; Assign the single page
        RTE VS                          ; Exit if error produced
        RTS                             ; Return from subroutine

        ; Handle the case where the required page is not loaded
undo$l  LDR     r1, [r0, #source_area]  ; Find which area is using page
        TEQ     r1, #0                  ; Is the required page in use
        BEQ     free$l                  ; Skip the next bit if not used
        MOV     r1, r0                  ; Copy page pointer
        BL      choose_free_page        ; Find an unsed page
        TEQ     r0, #0                  ; Was a free page found
        BNE     got$l                   ; Skip next bit if it was
        MOV     r0, r1                  ; Restore page pointer
        BL      release_page            ; Free the page
        RTE VS                          ; Return with error
        B       free$l                  ; Got a free page now
got$l   BL      swap_pages              ; Swap the pages over
        MOV     r0, r1                  ; Restore the page pointer
free$l  MOV     r1, r2                  ; Copy area record pointer
        MOV     r2, r3                  ; Copy page offset
        BL      assign_page             ; Assign this single page
        RTE VS                          ; Return with error
        RTS                             ; Return from subroutine

        ; Assign the pages in an optimum position
align_read$l
        JSR     "r0-r3"                 ; Stack registers
        BL      align$l                 ; Align the page index
        BL      group_assign_pages      ; Assign the pages
        RTSS                            ; Return from subroutine

        ; Attempt to align the page number to improve performance
align$l JSR     "r0, r2"                ; Stack registers
        SUB     r1, r1, #1              ; Convert number of pages to mask
        LDR     r0, [r2, #area_virtual_array]; Pointer to start of array
        MOV     r2, #virtual_item       ; Size of virtual array entries
        MLA     r0, r3, r2, r0          ; Pointer into virtual array
align_loop$l
        TST     r3, r1                  ; Is page aligned
        BEQ     align_done$l            ; Exit loop if it is
        SUB     r3, r3, #1              ; Decrement page number
        SUB     r0, r0, #virtual_item   ; Decrement virtual array pointer
        LDR     r2, [r0, #virtual_ptr]  ; Get the page pointer
        TST     r2, #virtual_flags_used ; Has page been modified
        BEQ     align_exit$l            ; Exit if not
        BIC     r2, r2, #virtual_flags_mask; Clear the flags
        TEQ     r2, #0                  ; Is page present
        BEQ     align_loop$l            ; Loop for next page if not
align_exit$l
        ADD     r3, r3, #1              ; Correct page pointer
align_done$l
        AND     r0, r3, r1              ; Get non-aligned amount
        SUB     r1, r1, r0              ; Get number of pages - 1
        ADD     r1, r1, #1              ; Correct number of pages
        RTS                             ; Return from subroutine

        ;   Parameters  : r0    - The first page to assign.
        ;                 r1    - The maximum offset of pages to assign.
        ;                 r2    - Pointer to virtual area to assign page to.
        ;                 r3    - The page offset to assign within the area.
        ;   Returns     : None
        ;   Description : Assign as many pages as possible to the same area,
        ;                 within the specified range. All of the pages are read
        ;                 from disc in a single file operation. None of the
        ;                 pages are actually paged in.
group_assign_pages
        LocalLabels
;        debug_record "group_assign_pages"
        JSR     "r0-r5, r8-r11"         ; Stack registers
        assert_source r0, "group_assign_pages", "GAP"
        assert_area r2, "group_assign_pages", "GAP"
;        pipe_string "Assign up to %0 pages [%1:%2]", r1, r2, r3
        MOV     r9, r3                  ; Copy area page offset
        MOV     r10, r0                 ; Copy pointer to page pointer
        MOV     r11, r2                 ; Copy virtual area record
        LDR     r2, [r11, #area_total_pages]; Get total number of pages
        SUB     r2, r2, r3              ; Maximum possible pages to process
        CMP     r1, r2                  ; Is end of array reached
        MOVLT   r2, r1                  ; Clip number of pages if not
        LDR     r0, ws_source_array     ; Pointer to start of source array
        MOV     r1, #source_item        ; Size of array entries
        MLA     r2, r1, r2, r10         ; End of pages from number
        LDR     r4, ws_total_pages      ; Actual number of pages
        MLA     r4, r1, r4, r0          ; Actual end of array
        CMP     r2, r4                  ; Compare end positions
        MOVHI   r2, r4                  ; Choose the lower one
        LDR     r0, [r11, #area_virtual_array]; Get pointer to page array
        MOV     r1, #virtual_item       ; Get the size of entries in the array
        MLA     r4, r3, r1, r0          ; Get pointer to correct entry in array
        MOV     r3, #0                  ; No pages processed yet
        MOV     r1, r10                 ; Copy pointer to first page
group$l CMP     r1, r2                  ; Check for end of array
        BHS     ready$l                 ; Exit loop if end reached
        LDR     r5, [r4, #virtual_ptr]  ; Get page pointer
        BIC     r0, r5, #virtual_flags_mask; Clear flag bits
        TEQ     r0, #0                  ; Is page in memory
        BNE     ready$l                 ; Exit loop if page used
        TST     r5, #virtual_flags_used ; Has page been modified
        BEQ     ready$l                 ; Exit loop if page not modified
        LDR     r0, [r1, #source_locks] ; Get number of times locked
        TEQ     r0, #0                  ; Is page locked
        BNE     ready$l                 ; Exit loop if it is
        LDR     r0, [r1, #source_area]  ; Get pointer to area record
        TEQ     r0, #0                  ; Is page used
        BEQ     next$l                  ; Skip next bit if not
        BL      choose_free_page        ; Choose a page to swap it with
        BVS     ready$l                 ; Exit loop if an error produced
        TEQ     r0, #0                  ; Was a page found
        BEQ     ready$l                 ; Exit loop if not
        BL      swap_pages              ; For the used page for a free one
next$l  MOV     r0, #-1                 ; Dummy area pointer
        STR     r0, [r1, #source_area]  ; Marker to indicate page used
        ADD     r3, r3, #1              ; Increment number of pages processed
        ADD     r1, r1, #source_item    ; Advance pointer into source array
        ADD     r4, r4, #virtual_item   ; Advance pointer into virtual array
        B       group$l                 ; Loop for next page
ready$l; pipe_string "- reading %0 pages", r3
        TEQ     r3, #0                  ; Were any pages processed
        BLEQ    tidy$l                  ; Tidy up if not
        RTSS EQ                         ; Return from subroutine
        MOV     r8, r3                  ; Copy number of pages processed
        LDR     r1, [r11, #area_file_handle]; Get the file handle
        LDR     r2, [r10, #source_ptr]  ; Pointer to page in memory
        LDR     r0, ws_log_page_size    ; Log base 2 of page size
        MOV     r3, r3, LSL r0          ; Convert pages to bytes to read
        MOV     r4, r9, LSL r0          ; File byte offset
        MOV     r0, #OSGBPB_ReadAt      ; Reason code to read data
        SWI     XOS_GBPB                ; Read the pages from disc
        BVC     read$l                  ; Skip retry attempt if successful
        BL      reopen_swap_file        ; Attempt to reopen swap file
        BLVS    tidy$l                  ; If an error tidy up
        RTE VS                          ; Exit if error produced
        MOV     r0, #OSGBPB_ReadAt      ; Reason code to read data
        LDR     r1, [r11, #area_file_handle]; Get the file handle
        SWI     XOS_GBPB                ; Load the pages from disc
        BLVS    tidy$l                  ; If an error tidy up
        RTE VS                          ; Exit if error produced
read$l  LDR     r0, [r11, #area_used_pages]; Get number of physical pages
        ADD     r0, r0, r8              ; Increment number of physical pages
        STR     r0, [r11, #area_used_pages]; Store number of physical pages
        LDR     r0, ws_used_pages       ; Get current page usage
        ADD     r0, r0, r8              ; Increment usage count
        STR     r0, ws_used_pages       ; Store new usage count
        LDR     r0, [r11, #area_virtual_array]; Pointer to array
        MOV     r1, #virtual_item       ; Size of virtual array entries
        MLA     r1, r9, r1, r0          ; Pointer to virtual array entry
loop$l  SUBS    r8, r8, #1              ; Decrement pages left
        RTSS MI                         ; Exit if all done
        LDR     r0, [r1, #virtual_ptr]  ; Get the virtual page entry
        AND     r0, r0, #virtual_flags_mask; Only keep the flags
        ORR     r0, r0, r10             ; Include the page pointer
        STR     r0, [r1, #virtual_ptr]  ; Set the virtual page entry
        STR     r11, [r10, #source_area]; Set the virtual area pointer
        STR     r9, [r10, #source_page] ; Set the virtual page pointer
        MOV     r0, r10                 ; Copy source page pointer
        BL      count_start             ; Initialise access counter
        ADD     r10, r10, #source_item  ; Advance pointer into source array
        ADD     r1, r1, #virtual_item   ; Advance pointer into virtual array
        ADD     r9, r9, #1              ; Increment virtual page number
        B       loop$l                  ; Loop for next page

        ; Tidy up if something went wrong
tidy$l  JSR     "r0-r2"                 ; Stack registers
        LDR     r0, ws_source_array     ; Pointer to start of source array
        LDR     r1, ws_total_pages      ; Number of pages in source array
        MOV     r2, #source_item        ; Size of source item records
        MLA     r1, r2, r1, r0          ; End of source array
clear$l CMP     r0, r1                  ; Has end of array been reached
        RTSS HS                         ; Return when done
        LDR     r2, [r0, #source_area]  ; Get the area pointer
        TST     r2, #3                  ; Is it a valid pointer
        MOVNE   r2, #0                  ; Clear it if not
        STRNE   r2, [r0, #source_area]  ; Store the area pointer
        ADD     r0, r0, #source_item    ; Advance pointer into source array
        B       clear$l                 ; Loop for next page

        ;   Parameters  : r0    - The first page to release.
        ;                 r1    - The maximum offset of pages to release.
        ;   Returns     : None
        ;   Description : Release as many pages as possible from the same area,
        ;                 within the specified range. All of the pages are
        ;                 written to disc in a single file operation.
group_release_pages
        LocalLabels
;        debug_record "group_release_pages"
        JSR     "r0-r5, r9-r11"         ; Stack registers
        assert_source r0, "group_release_pages", "GRP"
;        pipe_string "Release up to %0 pages", r1
        MOV     r10, r0                 ; Copy first page pointer
        LDR     r11, [r10, #source_area]; Get the area pointer
        LDR     r2, [r11, #area_total_pages]; Get total number of pages
        LDR     r3, [r10, #source_page] ; Get the page number
        SUB     r2, r2, r3              ; Maximum possible pages to process
        CMP     r1, r2                  ; Is end of array reached
        MOVLT   r2, r1                  ; Clip number of pages if not
        LDR     r0, ws_source_array     ; Pointer to start of source array
        MOV     r1, #source_item        ; Size of array entries
        MLA     r2, r1, r2, r10         ; End of pages from number
        LDR     r4, ws_total_pages      ; Actual number of pages
        MLA     r4, r1, r4, r0          ; Actual end of array
        CMP     r2, r4                  ; Compare end positions
        MOVHI   r2, r4                  ; Choose the lower one
        LDR     r0, [r11, #area_virtual_array]; Get pointer to page array
        MOV     r1, #virtual_item       ; Get the size of entries in the array
        MLA     r4, r3, r1, r0          ; Get pointer to correct entry in array
        MOV     r3, #0                  ; No pages processed yet
        MOV     r1, r10                 ; Copy pointer to first page
group$l CMP     r1, r2                  ; Has end of virtual pool been reached
        BHS     ready$l                 ; Exit loop if it has
        LDR     r5, [r4, #virtual_ptr]  ; Get page pointer
        BIC     r0, r5, #virtual_flags_mask; Clear flag bits
        TEQ     r0, #0                  ; Is page in memory
        BEQ     ready$l                 ; Exit loop if page not used
        TST     r5, #virtual_flags_used ; Has page been modified
        BEQ     ready$l                 ; Exit loop if page not modified
        LDR     r5, [r1, #source_locks] ; Get number of times locked
        TEQ     r5, #0                  ; Is page locked
        BNE     ready$l                 ; Exit loop if it is
        LDR     r5, [r0, #source_locks] ; Get number of times locked
        TEQ     r5, #0                  ; Is page locked
        BNE     ready$l                 ; Exit loop if it is
        BL      swap_pages              ; Swap the pages over
        BLVS    simple$l                ; If an error just release first page
        RTE VS                          ; Exit with error
        MOV     r0, r1                  ; Copy source page pointer
        BL      pull_page               ; Page the original out
        BLVS    simple$l                ; If an error just release first page
        RTE VS                          ; Exit with error
        ADD     r3, r3, #1              ; Increment number of pages processed
        ADD     r1, r1, #source_item    ; Advance pointer into source array
        ADD     r4, r4, #virtual_item   ; Advance pointer into virtual array
        B       group$l                 ; Loop for next page
ready$l; pipe_string "- writing %0 pages", r3
        TEQ     r3, #0                  ; Are there any pages processed
        BLEQ    simple$l                ; Just do one page if not
        RTSS EQ                         ; Return from subroutine
        MOV     r9, r3                  ; Copy number of pages processed
        LDR     r1, [r11, #area_file_handle]; Get the file handle
        LDR     r2, [r10, #source_ptr]  ; Pointer to page in memory
        LDR     r0, ws_log_page_size    ; Log base 2 of page size
        MOV     r3, r3, LSL r0          ; Convert pages to bytes to write
        LDR     r4, [r10, #source_page] ; The virtual area page number
        MOV     r4, r4, LSL r0          ; File byte offset
        MOV     r0, #OSGBPB_WriteAt     ; Reason code to write data
        SWI     XOS_GBPB                ; Write the pages to disc
        BVC     wrote$l                 ; Skip retry attempt if successful
        BL      reopen_swap_file        ; Attempt to reopen swap file
        BLVS    simple$l                ; If an error just release first page
        RTE VS                          ; Exit if error produced
        MOV     r0, #OSGBPB_WriteAt     ; Reason code to write data
        LDR     r1, [r11, #area_file_handle]; Get the file handle
        SWI     XOS_GBPB                ; Write the pages to disc
        BLVS    simple$l                ; If an error just release first page
        RTE VS                          ; Exit if error produced
wrote$l LDR     r0, [r11, #area_used_pages]; Get physical page count
        SUB     r0, r0, r9              ; Decrement physical page count
        STR     r0, [r11, #area_used_pages]; Store physical page count
        LDR     r0, ws_used_pages       ; Get current page usage
        SUB     r0, r0, r9              ; Decrement usage count
        STR     r0, ws_used_pages       ; Store new usage count
        LDR     r0, [r11, #area_virtual_array]; Pointer to array
        LDR     r1, [r10, #source_page] ; Number of first page in virtual area
        MOV     r2, #virtual_item       ; Size of virtual array entries
        MLA     r1, r2, r1, r0          ; Pointer to virtual array entry
loop$l  SUBS    r9, r9, #1              ; Decrement pages left
        RTSS MI                         ; Exit if all done
        LDR     r0, [r1, #virtual_ptr]  ; Get the virtual page entry
        AND     r0, r0, #virtual_flags_mask; Only keep the flags
        STR     r0, [r1, #virtual_ptr]  ; Clear virtual page entry
        MOV     r0, #0                  ; Value to clear entries with
        STR     r0, [r10, #source_area] ; Clear virtual area pointer
        STR     r0, [r10, #source_page] ; Clear virtual page pointer
        MOV     r0, r10                 ; Copy source page pointer
        BL      count_end               ; Clear the access counter
        ADD     r10, r10, #source_item  ; Advance pointer into source array
        ADD     r1, r1, #virtual_item   ; Advance pointer into virtual array
        B       loop$l                  ; Loop for next page

        ; In an emergency just release the one page
simple$l
        JSR     "r0"                    ; Stack registers
        MOV     r0, r10                 ; Restore page pointer
        BL      release_page            ; Just release the one page
        RTE VS                          ; Return with error
        RTSS                            ; Return from subroutine

        ;   Parameters  : r0    - The page to ensure is paged in.
        ;   Returns     : None
        ;   Description : Ensure that the specified page is actually paged in,
        ;                 and update the flags and counter.
push_page
        LocalLabels
;        debug_record "push_page"
        JSR     "r0-r2, r9-r11"         ; Stack registers
        assert_source r0, "push_page", "PP"
        MOV     r10, r0                 ; Copy page pointer
        LDR     r11, [r10, #source_area]; Get pointer to area record
        LDR     r0, [r11, #area_virtual_array]; Get pointer to page array
        LDR     r1, [r10, #source_page] ; Get the page number
        MOV     r2, #virtual_item       ; Get size of entries in page array
        MLA     r9, r1, r2, r0          ; Get pointer to correct entry in array
        LDR     r0, [r9, #virtual_ptr]  ; Get pointer and flags
        TST     r0, #virtual_flags_paged; Is it paged in
        RTSS NE                         ; Return if it is
        ADRL    r0, ws_mem_map_request  ; Pointer to request list
        LDR     r1, [r10, #source_number]; Get the physical page number
        STR     r1, [r0, #OS_MemMapRequest_page_no]; Set the physical page
        LDR     r1, [r10, #source_page] ; The virtual area page number
        LDR     r2, ws_log_page_size    ; Log base 2 of page size
        MOV     r1, r1, LSL r2          ; Byte offset from base
        LDR     r2, [r11, #area_base]   ; The virtual area base address
        ADD     r1, r1, r2              ; Get new logical address
        STR     r1, [r0, #OS_MemMapRequest_map]; Set logical address
        LDR     r1, [r11, #area_access] ; Get protection level
        STR     r1, [r0, #OS_MemMapRequest_access]; Set protection level
        MOV     r1, #-1                 ; Request list terminator
        STR     r1, [r0, #OS_MemMapRequest]; Terminate the request list
        SWI     XOS_SetMemMapEntries    ; Remap the page
        RTE VS                          ; Exit if error produced
        LDR     r0, [r9, #virtual_ptr]  ; Get pointer and flags
        ORR     r0, r0, #virtual_flags_used :OR: virtual_flags_paged
        STR     r0, [r9, #virtual_ptr]  ; Store modified flags
        MOV     r0, r10                 ; Copy source page pointer
        BL      count_inc               ; Increment access counter
        RTSS                            ; Return from subroutine

        ;   Parameters  : r0    - The page to ensure is paged out.
        ;   Returns     : None
        ;   Description : Ensure that the specified page is not currently paged
        ;                 in, and update the flags.
pull_page
        LocalLabels
        JSR     "r0-r2, r9-r11"         ; Stack registers
        assert_source r0, "pull_page", "pP"
        MOV     r10, r0                 ; Copy page pointer
        LDR     r11, [r10, #source_area]; Get pointer to area record
        LDR     r0, [r11, #area_virtual_array]; Get pointer to page array
        LDR     r1, [r10, #source_page] ; Get the page number
        MOV     r2, #virtual_item       ; Get size of entries in page array
        MLA     r9, r1, r2, r0          ; Get pointer to correct entry in array
        LDR     r0, [r9, #virtual_ptr]  ; Get pointer and flags
        TST     r0, #virtual_flags_paged; Is it paged in
        RTSS EQ                         ; Return if not
        ADRL    r0, ws_mem_map_request  ; Pointer to request list
        LDR     r1, [r10, #source_number]; Get the physical page number
        STR     r1, [r0, #OS_MemMapRequest_page_no]; Set the physical page
        LDR     r1, [r10, #source_ptr]  ; Get original logical address
        STR     r1, [r0, #OS_MemMapRequest_map]; Set logical address
        MOV     r1, #3                  ; Inaccessible in user mode
        STR     r1, [r0, #OS_MemMapRequest_access]; Set inaccessible in usr
        MOV     r1, #-1                 ; Request list terminator
        STR     r1, [r0, #OS_MemMapRequest]; Terminate the request list
        SWI     XOS_SetMemMapEntries    ; Remap the page
        RTE VS                          ; Exit if error produced
        LDR     r0, [r9, #virtual_ptr]  ; Get pointer and flags
        ORR     r0, r0, #virtual_flags_used; Page has been modified
        BIC     r0, r0, #virtual_flags_paged; No longer paged in
        STR     r0, [r9, #virtual_ptr]  ; Store modified flags
        RTSS                            ; Return from subroutine

        ;   Parameters  : None
        ;   Returns     : None
        ;   Description : Page out, but keep in memory, all pages that are not
        ;                 locked. The flags and counters are updated.
pull_all_pages
        LocalLabels
        JSR     "r0-r2, r10-r11"        ; Stack registers
        BL      adjust_memory           ; Optimise memory usage
        LDR     r0, ws_source_array     ; Pointer to page array
        LDR     r1, ws_total_pages      ; Number of pages to process
loop$l  SUBS    r1, r1, #1              ; Decrement number of pages remaining
        RTSS MI                         ; Return if all pages processed
        BL      count_dec               ; Decrement the access counter
        LDR     r2, [r0, #source_area]  ; Get pointer to area record
        TEQ     r2, #0                  ; Is page being used
        BEQ     next$l                  ; Skip page if not
        LDR     r2, [r0, #source_locks] ; Get number of times locked
        TEQ     r2, #0                  ; Is page being locked
        BNE     next$l                  ; Skip page if it is
        BL      pull_page               ; Page page out
        RTE VS                          ; Exit if error produced
next$l  ADD     r0, r0, #source_item    ; Increment array pointer
        B       loop$l                  ; Loop round for next page

        ;   Parameters  : r0    - The page to release.
        ;                 r1    - The page to replace it with.
        ;   Returns     : None
        ;   Description : Swap over the specified pages. This is useful when
        ;                 a page is about to be locked to change it to an
        ;                 earlier page without any disc access. One or both
        ;                 pages could be in use.
swap_pages
        LocalLabels
;        debug_record "swap_pages"
        JSR     "r0-r4, r10-r11"        ; Stack registers
        assert_source r0, "swap_pages 1", "SP1"
        assert_source r1, "swap_pages 2", "SP2"
        TEQ     r0, r1                  ; Are the pages the same
        RTSS EQ                         ; Exit immediately if they are
        MOV     r10, r0                 ; Copy source page pointer
        MOV     r11, r1                 ; Copy destination page pointer
        ADRL    r0, ws_mem_map_request  ; Pointer to request list
        LDR     r1, [r10, #source_number]; Get the physical page number
        STR     r1, [r0, #OS_MemMapRequest_page_no]; Set the physical page
        LDR     r1, [r10, #source_ptr]  ; Get original logical address
        STR     r1, [r0, #OS_MemMapRequest_map]; Set logical address
        MOV     r1, #3                  ; Inaccessible in user mode
        STR     r1, [r0, #OS_MemMapRequest_access]; Set inaccessible in USR
        ADD     r0, r0, #OS_MemMapRequest; Advance request list pointer
        LDR     r1, [r11, #source_number]; Get the physical page number
        STR     r1, [r0, #OS_MemMapRequest_page_no]; Set the physical page
        LDR     r1, [r11, #source_ptr]  ; Get original logical address
        STR     r1, [r0, #OS_MemMapRequest_map]; Set logical address
        MOV     r1, #3                  ; Inaccessible in user mode
        STR     r1, [r0, #OS_MemMapRequest_access]; Set inaccessible in USR
        MOV     r1, #-1                 ; Request list terminator
        STR     r1, [r0, #OS_MemMapRequest]; Terminate the request list
        ADRL    r0, ws_mem_map_request  ; Reset pointer to request list
        SWI     XOS_SetMemMapEntries    ; Remap the pages
        RTE VS                          ; Exit if error produced
        LDR     r0, [r10, #source_ptr]  ; Pointer to the source page
        LDR     r1, [r11, #source_ptr]  ; Pointer to the destination page
        LDR     r2, ws_page_size        ; Get the size of the pages
copy$l  SUBS    r2, r2, #4              ; Decrement position pointer
        BMI     copy_done$l             ; Exit loop when copy complete
        LDR     r3, [r0, r2]            ; Read word from source page
        LDR     r4, [r1, r2]            ; Read word from destination page
        STR     r4, [r0, r2]            ; Write word to source page
        STR     r3, [r1, r2]            ; Write word to destination page
        B       copy$l                  ; Loop for next word
copy_done$l
        LDR     r3, [r10, #source_area] ; Pointer to first area record
        LDR     r4, [r11, #source_area] ; Pointer to second area record
        ADRL    r0, ws_mem_map_request  ; Pointer to request list
        TEQ     r4, #0                  ; Is first page used
        BEQ     no1st$l                 ; Skip next bit if not
        LDR     r1, [r10, #source_number]; Get the physical page number
        STR     r1, [r0, #OS_MemMapRequest_page_no]; Set the physical page
        LDR     r1, [r11, #source_page] ; The virtual area page number
        LDR     r2, ws_log_page_size    ; Log base 2 of page size
        MOV     r1, r1, LSL r2          ; Byte offset from base
        LDR     r2, [r4, #area_base]    ; The virtual area base address
        ADD     r1, r1, r2              ; Get new logical address
        STR     r1, [r0, #OS_MemMapRequest_map]; Set logical address
        LDR     r1, [r4, #area_access]  ; Get protection level
        STR     r1, [r0, #OS_MemMapRequest_access]; Set protection level
        ADD     r0, r0, #OS_MemMapRequest; Advance request list pointer
no1st$l TEQ     r3, #0                  ; Is second page used
        BEQ     no2nd$l                 ; Skip next bit if not
        LDR     r1, [r11, #source_number]; Get the physical page number
        STR     r1, [r0, #OS_MemMapRequest_page_no]; Set the physical page
        LDR     r1, [r10, #source_page] ; The virtual area page number
        LDR     r2, ws_log_page_size    ; Log base 2 of page size
        MOV     r1, r1, LSL r2          ; Byte offset from base
        LDR     r2, [r3, #area_base]    ; The virtual area base address
        ADD     r1, r1, r2              ; Get new logical address
        STR     r1, [r0, #OS_MemMapRequest_map]; Set logical address
        LDR     r1, [r3, #area_access]  ; Get protection level
        STR     r1, [r0, #OS_MemMapRequest_access]; Set protection level
        ADD     r0, r0, #OS_MemMapRequest; Advance request list pointer
no2nd$l MOV     r1, #-1                 ; Request list terminator
        STR     r1, [r0]                ; Terminate the request list
        ADRL    r0, ws_mem_map_request  ; Reset pointer to request list
        SWI     XOS_SetMemMapEntries    ; Remap the page
        RTE VS                          ; Exit if error produced
        LDR     r0, [r10, #source_area] ; Get first area record pointer
        LDR     r1, [r11, #source_area] ; Get second area record pointer
        STR     r1, [r10, #source_area] ; Store first area record pointer
        STR     r0, [r11, #source_area] ; Store second area record pointer
        LDR     r0, [r10, #source_page] ; Get first page number
        LDR     r1, [r11, #source_page] ; Get second page number
        STR     r1, [r10, #source_page] ; Store first page number
        STR     r0, [r11, #source_page] ; Store second page number
        LDR     r0, [r10, #source_locks]; Get first number of times locked
        LDR     r1, [r11, #source_locks]; Get second number of times locked
        STR     r1, [r10, #source_locks]; Store first number of times locked
        STR     r0, [r11, #source_locks]; Store second number of times locked
        LDR     r0, [r10, #source_count]; Get first access count
        LDR     r1, [r11, #source_count]; Get second access count
        STR     r1, [r10, #source_count]; Store first access count
        STR     r0, [r11, #source_count]; Store second access count
        LDR     r0, [r10, #source_area] ; Pointer to the first area record
        TEQ     r0, #0                  ; Is the first page used
        BEQ     skip1$l                 ; Skip update if first page not used
        LDR     r0, [r0, #area_virtual_array]; Pointer to page array
        LDR     r1, [r10, #source_page] ; Get page number offset
        MOV     r2, #virtual_item       ; Size of virtual array entries
        MLA     r0, r1, r2, r0          ; Pointer to virtual array entry
        LDR     r1, [r0, #virtual_ptr]  ; Get the page flags
        AND     r1, r1, #virtual_flags_mask; Only keep the flags
        ORR     r1, r1, #virtual_flags_paged; It is now paged in
        ORR     r1, r1, r10             ; Include the new address
        STR     r1, [r0, #virtual_ptr]  ; Store the new address and flags
skip1$l LDR     r0, [r11, #source_area] ; Pointer to the second area record
        TEQ     r0, #0                  ; Is the second page used
        RTSS EQ                         ; Return if second page not used
        LDR     r0, [r0, #area_virtual_array]; Pointer to page array
        LDR     r1, [r11, #source_page] ; Get page number offset
        MOV     r2, #virtual_item       ; Size of virtual array entries
        MLA     r0, r1, r2, r0          ; Pointer to virtual array entry
        LDR     r1, [r0, #virtual_ptr]  ; Get the page flags
        AND     r1, r1, #virtual_flags_mask; Only keep the flags
        ORR     r1, r1, #virtual_flags_paged; It is now paged in
        ORR     r1, r1, r11             ; Include the new address
        STR     r1, [r0, #virtual_ptr]  ; Store the new address and flags
        RTSS                            ; Return from subroutine

        ;   Parameters  : None
        ;   Returns     : r0    - The selected page.
        ;   Description : Choose a page to be (re)allocated. If required it
        ;                 is released first.
choose_page
        LocalLabels
;        debug_record "choose_page"
        JSR     "r1-r2"                 ; Stack registers
        LDR     r0, ws_replace_policy   ; Get page replacement policy
        BIC     r0, r0, #policy_flags_mask; Clear the flag bits
        TEQ     r0, #policy_nfu         ; Is it not frequently used
        BNE     no$l                    ; Do not pull pages otherwise
        LDR     r0, ws_pull_count       ; How many choices since last page out
        ADD     r0, r0, #1              ; Increment count
        LDR     r1, ws_target_pages     ; Number of pages to use
        MOV     r1, r1, LSR#shift_pull_count; Calculate delay between resets
        CMP     r1, #min_pull_count     ; Is it larger than the minimum
        MOVLT   r1, #min_pull_count     ; Minimum number of faults between
        CMP     r0, r1                  ; Is it time to perform a page out
        MOVHI   r0, #0                  ; Reset counter if it is
        STR     r0, ws_pull_count       ; Store modified count
        BLHI    pull_all_pages          ; Page all unlocked pages out
no$l    BL      choose_single_page      ; Choose the starting page
        RTE VS                          ; Return if an error produced
        TEQ     r0, #0                  ; Was a page found
        RTS EQ                          ; Return failed if not
        LDR     r1, ws_replace_policy   ; Get page replacement policy
        TST     r1, #policy_flags_writes; Should multiple page writes be used
        BEQ     single$l                ; Only perform single write if not
        LDR     r1, [r0, #source_area]  ; Get pointer to area record
        TEQ     r1, #0                  ; Is page used
        BEQ     found$l                 ; Skip next bit if not
        BL      release$l               ; Release page
        RTE VS                          ; Return if an error produced
found$l LDR     r1, ws_source_array     ; Pointer to the start of the array
search$l
        SUB     r0, r0, #source_item    ; Move pointer to previous page
        CMP     r0, r1                  ; Is it within the array
        BLO     done$l                  ; Exit loop if not
        LDR     r2, [r0, #source_area]  ; Get pointer to area record
        TEQ     r2, #0                  ; Is page used
        BEQ     search$l                ; Loop to try the next page if not
done$l  ADD     r0, r0, #source_item    ; Back to the previous position
        LDR     r1, [r0, #source_area]  ; Get pointer to area record
        TEQ     r1, #0                  ; Is page used
        MOVNE   r0, #0                  ; Clear page pointer if it is
        RTS NE                          ; Exit if page is in use
        STR     r0, ws_circle           ; Store selected page pointer
        RTSS                            ; Return from subroutine

        ; Only perform a single page write operation
single$l
        LDR     r1, [r0, #source_area]  ; Get pointer to area record
        TEQ     r1, #0                  ; Is page used
        BEQ     single_found$l          ; Skip next bit if not
        BL      release_page            ; Release the one page
        RTE VS                          ; Exit if error produced
single_found$l
        STR     r0, ws_circle           ; Store page pointer
        RTS                             ; Return from subroutine

        ; Release some extra pages to speed up disc access
release$l
        JSR     "r1-r2"                 ; Stack registers
        MOV     r2, r0                  ; Copy page pointer
        LDR     r0, [r2, #source_area]  ; Get pointer to area record
        BL      multiple_number         ; Choose number of pages to free
        MOV     r1, r0                  ; Copy number of pages to free
        BL      align$l                 ; Align the page number
        MOV     r0, r2                  ; Copy first page pointer
        BL      group_release_pages     ; Release the pages in a block
        MOV     r0, r2                  ; Restore first page pointer if error
        BL      multiple_release_pages  ; Release extra pages if any left
        RTS                             ; Return from subroutine

        ; Attempt to align the page number to improve performance
align$l JSR     "r0, r3"                ; Stack registers
        SUB     r1, r1, #1              ; Convert number of pages to mask
        LDR     r0, [r2, #source_page]  ; Get the current page number
        LDR     r2, [r2, #source_area]  ; Get the area record pointer
        LDR     r2, [r2, #area_virtual_array]; Get the virtual array pointer
        MOV     r3, #virtual_item       ; Size of virtual array entries
        MLA     r2, r0, r3, r2          ; Pointer to virtual array entry
align_loop$l
        TST     r0, r1                  ; Is page aligned
        BEQ     align_done$l            ; Exit loop if it is
        SUB     r0, r0, #1              ; Decrement page number
        SUB     r2, r2, #virtual_item   ; Decrement virtual array pointer
        LDR     r3, [r2, #virtual_ptr]  ; Get the page pointer
        BIC     r3, r3, #virtual_flags_mask; Clear the flag bits
        TEQ     r3, #0                  ; Is page used
        BNE     align_loop$l            ; Loop for next page if it is
        ADD     r2, r2, #virtual_item   ; Otherwise restore virtual pointer
        ADD     r1, r1, #1              ; Restore page number
align_done$l
        AND     r0, r0, r1              ; Get non-aligned amount
        SUB     r1, r1, r0              ; Get number of pages - 1
        ADD     r1, r1, #1              ; Correct number of pages
        LDR     r2, [r2, #virtual_ptr]  ; Get the page pointer
        BIC     r2, r2, #virtual_flags_mask; Clear the flag bits
        RTS                             ; Return from subroutine

        ;   Parameters  : None
        ;   Returns     : r0    - Pointer to selected page, or 0 if none found.
        ;   Description : Choose a single page to re(use) based on the current
        ;                 page replacement policy. The page is not released.
choose_single_page
        LocalLabels
        JSR     "r1-r6"                 ; Stack registers
        LDR     r0, ws_source_array     ; Pointer to array of pages
        LDR     r1, ws_target_pages     ; Number of pages to use
        MOV     r2, #source_item        ; Size of each record
        MLA     r1, r2, r1, r0          ; Pointer to end of array
        LDR     r0, ws_circle           ; Start from last selected page
        CMP     r0, r1                  ; Is it past the end of the array
        LDRHS   r0, ws_source_array     ; Start from beginning if it is
        MOV     r2, r0                  ; Copy start position
        LDR     r3, ws_replace_policy   ; The replacement policy to use
        BIC     r3, r3, #policy_flags_mask; Clear the flag bits
        CMP     r3, #(jump_end$l - jump$l) >> 2; Is it in range
        ADDLO   pc, pc, r3, LSL#2       ; Dispatch if in range
        B       nfu$l                   ; Use NFU if not in range
jump$l  B       nfu$l                   ; Jump to handler for 0th policy
        B       fifo$l                  ; Jump to handler for 1st policy
        B       random$l                ; Jump to handler for 2nd policy
jump_end$l

        ; Not frequently used (NFU) page replacement policy
nfu$l   MOV     r3, #0                  ; No best page yet
        MOV     r4, #-1                 ; Value of best page is maximum possible
nfu_loop$l
        ADD     r0, r0, #source_item    ; Size of each record
        CMP     r0, r1                  ; Compare to end of array
        LDRHS   r0, ws_source_array     ; Wrap around to the start if required
        LDR     r5, [r0, #source_area]  ; Get pointer to area record
        TEQ     r5, #0                  ; Is page in use
        RTSS EQ                         ; Return page if not
        LDR     r5, [r0, #source_locks] ; Get number of times locked
        TEQ     r5, #0                  ; Is page locked
        BNE     nfu_next$l              ; Skip the next bit if it is
        TEQ     r3, #0                  ; Has any page been found yet
        BEQ     nfu_better$l            ; This page is better than nothing
        LDR     r5, [r0, #source_count] ; Get access count
        CMP     r5, r4                  ; Compare to best access count
        BLO     nfu_better$l            ; If accessed less then remember it
nfu_next$l
        TEQ     r0, r2                  ; Was this the first page
        BNE     nfu_loop$l              ; Check next page if not
        MOV     r0, r3                  ; Copy pointer to best page
        RTSS                            ; Return from subroutine
nfu_better$l
        MOV     r3, r0                  ; Copy page pointer
        MOV     r4, r5                  ; Copy access count for this page
        B       nfu_next$l              ; Loop for the next page

        ; First-in, first-out (FIFO) page replacement policy
fifo$l  MOV     r3, #0                  ; No best page yet
        MOV     r4, #0                  ; Worst case is age of zero
        LDR     r5, ws_fifo_count       ; Get the current page counter
fifo_loop$l
        ADD     r0, r0, #source_item    ; Size of each record
        CMP     r0, r1                  ; Compare to end of array
        LDRHS   r0, ws_source_array     ; Wrap around to the start if required
        LDR     r6, [r0, #source_area]  ; Get pointer to area record
        TEQ     r6, #0                  ; Is page in use
        RTSS EQ                         ; Return page if not
        LDR     r6, [r0, #source_locks] ; Get number of times locked
        TEQ     r6, #0                  ; Is page locked
        BNE     nfu_next$l              ; Skip the next bit if it is
        TEQ     r3, #0                  ; Has any page been found yet
        BEQ     fifo_better$l           ; This page is better than nothing
        LDR     r6, [r0, #source_count] ; Get page count number
        SUB     r6, r5, r6              ; Calculate age of page
        CMP     r6, r4                  ; Compare to best age
        BHI     fifo_better$l           ; If older then remember it
fifo_next$l
        TEQ     r0, r2                  ; Was this the first page
        BNE     fifo_loop$l             ; Check the next page if not
        MOV     r0, r3                  ; Copy pointer to best page
        RTSS                            ; Return from subroutine
fifo_better$l
        MOV     r3, r0                  ; Copy page pointer
        MOV     r4, r6                  ; Copy age of this page
        B       fifo_next$l             ; Loop for the next page

        ; Random page replacement policy
random$l
        LDR     r0, ws_target_pages     ; Number of pages to choose from
        BL      random_range            ; Choose a random start page
        LDR     r3, ws_source_array     ; Pointer to start of array
        MOV     r2, #source_item        ; Size of each record
        MLA     r0, r2, r0, r3          ; Calculate pointer to page record
        MOV     r2, r0                  ; Copy start position
        MOV     r3, #0                  ; No best page so far
random_loop$l
        ADD     r0, r0, #source_item    ; Advance to the next item
        CMP     r0, r1                  ; Compare to end of array
        LDRHS   r0, ws_source_array     ; Wrap around to the start if required
        LDR     r4, [r0, #source_area]  ; Get pointer to area record
        TEQ     r4, #0                  ; Is page in use
        RTSS EQ                         ; Return page if not
        LDR     r4, [r0, #source_locks] ; Get number of times locked
        TEQ     r4, #0                  ; Is page locked
        BNE     random_next$l           ; Skip next bit it if is
        TEQ     r3, #0                  ; Has a page been found yet
        MOVEQ   r3, r0                  ; Copy page pointer
random_next$l
        TEQ     r0, r2                  ; Was this the first page
        BNE     random_loop$l           ; Check next page if not
        MOV     r0, r3                  ; Copy pointer to best page
        RTSS                            ; Return from subroutine

        ;   Parameters  : r0    - Pointer to page in source array.
        ;   Returns     : None
        ;   Description : Initialises the access counter for the specified page
        ;                 in a manner appropriate to the current page
        ;                 replacement policy. If the page is also paged in then
        ;                 count_inc should also be called.
count_start
        LocalLabels
        JSR     "r1"                    ; Stack registers
        assert_source r0, "count_start", "CS"
        LDR     r1, ws_replace_policy   ; The replacement policy to use
        BIC     r1, r1, #policy_flags_mask; Clear the flag bits
        CMP     r1, #(jump_end$l - jump$l) >> 2; Is it in range
        ADDLO   pc, pc, r1, LSL#2       ; Dispatch if in range
        B       nfu$l                   ; Use NFU if not in range
jump$l  B       nfu$l                   ; Jump to handler for 0th policy
        B       fifo$l                  ; Jump to handler for 1st policy
        B       random$l                ; Jump to handler for 2nd policy
jump_end$l

        ; Initialise access counter for not frequently used (NFU) policy
nfu$l   MOV     r1, #0                  ; Page has not been used yet
        STR     r1, [r0, #source_count] ; Store initial access counter
        RTS                             ; Return from subroutine

        ; Initialise access counter for first-in, first-out (FIFO) policy
fifo$l  LDR     r1, ws_fifo_count       ; Get last value of counter
        ADD     r1, r1, #1              ; Increment counter (may wrap)
        STR     r1, ws_fifo_count       ; Store the new counter value
        STR     r1, [r0, #source_count] ; Store initial access counter
        RTS                             ; Return from subroutine

        ; Initialise access counter for random policy
random$l
        MOV     r1, #-1                 ; Maximum possible useage
        STR     r1, [r0, #source_count] ; Store initial access counter
        RTS                             ; Return from subroutine

        ;   Parameters  : r0    - Pointer to page in source array.
        ;   Returns     : None
        ;   Description : Clears the the access counter for the specified page
        ;                 in a manner appropriate to the current page
        ;                 replacement policy.
count_end
        LocalLabels
        JSR     "r1"                    ; Stack registers
        assert_source r0, "count_end", "CE"
        MOV     r1, #0                  ; Value to clear counter with
        STR     r1, [r0, #source_count] ; Clear the access counter
        RTS                             ; Return from subroutine

        ;   Parameters  : r0    - Pointer to page in source array.
        ;   Returns     : None
        ;   Description : Increments the access counter for the specified page
        ;                 in a manner appropriate to the current page
        ;                 replacement policy.
count_inc
        LocalLabels
        JSR     "r1"                    ; Stack registers
        assert_source r0, "count_inc", "CI"
        LDR     r1, ws_replace_policy   ; The replacement policy to use
        BIC     r1, r1, #policy_flags_mask; Clear the flag bits
        CMP     r1, #(jump_end$l - jump$l) >> 2; Is it in range
        ADDLO   pc, pc, r1, LSL#2       ; Dispatch if in range
        B       nfu$l                   ; Use NFU if not in range
jump$l  B       nfu$l                   ; Jump to handler for 0th policy
        B       fifo$l                  ; Jump to handler for 1st policy
        B       random$l                ; Jump to handler for 2nd policy
jump_end$l

        ; Increment access counter for not frequently used (NFU) policy
nfu$l   LDR     r1, [r0, #source_count] ; Get current access counter
        ORR     r1, r1, #1 << 31        ; Set the top bit
        STR     r1, [r0, #source_count] ; Store updated access counter
        RTS                             ; Return from subroutine

        ; Increment access counter for first-in, first-out (FIFO) policy
fifo$l  RTS                             ; Return from subroutine

        ; Increment access counter for random policy
random$l
        RTS                             ; Return from subroutine

        ;   Parameters  : r0    - Pointer to page in source array.
        ;   Returns     : None
        ;   Description : Decrements the access counter for the specified page
        ;                 in a manner appropriate to the current page
        ;                 replacement policy.
count_dec
        LocalLabels
        JSR     "r1"                    ; Stack registers
        assert_source r0, "count_dec", "CD"
        LDR     r1, ws_replace_policy   ; The replacement policy to use
        BIC     r1, r1, #policy_flags_mask; Clear the flag bits
        CMP     r1, #(jump_end$l - jump$l) >> 2; Is it in range
        ADDLO   pc, pc, r1, LSL#2       ; Dispatch if in range
        B       nfu$l                   ; Use NFU if not in range
jump$l  B       nfu$l                   ; Jump to handler for 0th policy
        B       fifo$l                  ; Jump to handler for 1st policy
        B       random$l                ; Jump to handler for 2nd policy
jump_end$l

        ; Decrement access counter for not frequently used (NFU) policy
nfu$l   LDR     r1, [r0, #source_count] ; Get current access counter
        MOV     r1, r1, LSR#1           ; Shift right one bit
        STR     r1, [r0, #source_count] ; Store updated access counter
        RTS                             ; Return from subroutine

        ; Decrement access counter for first-in, first-out (FIFO) policy
fifo$l  RTS                             ; Return from subroutine

        ; Decrement access counter for random policy
random$l
        RTS                             ; Return from subroutine

        ;   Parameters  : None
        ;   Returns     : r0    - The selected page.
        ;   Description : Choose a page to be locked. The page is not freed.
choose_low_page
        LocalLabels
        JSR     "r1-r2"                 ; Stack registers
        LDR     r0, ws_source_array     ; Pointer to array of pages
        LDR     r1, ws_target_pages     ; Number of pages to use
        MOV     r2, #source_item        ; Size of each record
        MLA     r1, r2, r1, r0          ; Pointer to end of array
loop$l  CMP     r0, r1                  ; Check if end of array reached
        MOVGE   r0, #0                  ; Indicator that page was not found
        RTSS GE                         ; Exit if no suitable page
        LDR     r2, [r0, #source_locks] ; Get locked status
        TEQ     r2, #0                  ; Is page locked
        ADDNE   r0, r0, #source_item    ; Advance to next page
        BNE     loop$l                  ; Loop if it is locked
        STR     r0, ws_circle           ; Store selected page pointer
        RTSS                            ; Return from subroutine

        ;   Parameters  : None
        ;   Returns     : r0    - The selected page.
        ;   Description : Find an unused page to be allocated if any exist.
choose_free_page
        LocalLabels
        JSR     "r1-r3"                 ; Stack registers
        LDR     r0, ws_source_array     ; Pointer to array of pages
        LDR     r1, ws_target_pages     ; Number of pages to use
        MOV     r2, #source_item        ; Size of each record
        MLA     r1, r2, r1, r0          ; Pointer to end of array
        LDR     r0, ws_circle           ; Start from last selected page
        CMP     r0, r1                  ; Is it past the end of the array
        LDRHS   r0, ws_source_array     ; Start from beginning if it is
        MOV     r2, r0                  ; Copy start position
loop$l  ADD     r0, r0, #source_item    ; Advance to the next item
        CMP     r0, r1                  ; Compare to end of array
        LDRHS   r0, ws_source_array     ; Wrap around to the start if required
        LDR     r3, [r0, #source_area]  ; Get pointer to area record
        TEQ     r3, #0                  ; Is page in use
        STREQ   r0, ws_circle           ; Remember position if not
        RTSS EQ                         ; Return page if not
        TEQ     r0, r2                  ; Was this the first page
        BNE     loop$l                  ; Check next page if not
        MOV     r0, #0                  ; No page found
        RTSS                            ; Return from subroutine

; Installing and releasing the Data abort handler

        ;   Parameters  : None
        ;   Returns     : None
        ;   Description : Install the Data abort handler.
install_handler
        LocalLabels
        JSR     "r0-r2"                 ; Stack registers
        MOV     r0, #OSClaimProcessorVector_Alloc :OR: Vector_DataAbort
        ADR     r1, data_abort_handler  ; Pointer to replacement handler
        SWI     XOS_ClaimProcessorVector; Claim the Data abort vector
        RTE VS                          ; Exit without wiping over r0
        STR     r1, previous_handler    ; Store pointer to previous owner
        RTS                             ; Return from subroutine

        ;   Parameters  : None
        ;   Returns     : None
        ;   Description : Release the Data abort handler.
release_handler
        LocalLabels
        JSR     "r0-r2"                 ; Stack registers
        MOV     r0, #OSClaimProcessorVector_Free :OR: Vector_DataAbort
        LDR     r1, previous_handler    ; Pointer to previous owner
        ADR     r2, data_abort_handler  ; Handler to remove
        SWI     XOS_ClaimProcessorVector; Release the Data abort vector
        RTE VS                          ; Exit without wiping over r0
        RTS                             ; Return from subroutine

; A literal pool

        LTORG

; The Data abort handler

        ;   Parameters  : r14_abt   - r15 when abort occurred.
        ;                 spsr_abt  - CPSR when abort occurred.
        ;   Returns     : All non-ABT registers preserved.
        ;   Description : The Data abort handler. This is entered in ABT mode,
        ;                 with interrupts disabled.
data_abort_handler
        LocalLabels
        debug_record "data_abort_handler"
        STR     r13, dump_r13abt_temp   ; Store r13_abt (the stack pointer)
        MRS     r13, SPSR               ; Get stored status register
;        debug_record "data_abort_handler: with r13 = SPSR"
        ANDS    r13, r13, #&1F          ; Check if stored mode is User26
        TEQNE   r13, #&03               ; Check if stored mode is SVC26
        LDR     r13, handler_semaphore  ; Get value of semaphore
        TEQEQ   r13, #0                 ; Check if semaphore is set
        LDR     r13, dump_r13abt_temp   ; Restore r13_abt
        LDRNE   pc, previous_handler    ; Pass on if not User26 or SVC26
        STR     r13, dump_r13abt        ; Store r13_abt properly
        MOV     r13, #1                 ; Value used to set semaphore
        STR     r13, handler_semaphore  ; Set the semaphore
        STR     r14, dump_r14abt        ; Store r14_abt (return PC)
        ADR     r13, dump_r0_r7         ; Pointer to first half of dump
        STMIA   r13, {r0-r7}            ; Store non-banked registers
        ADR     r2, dump_r8_r14         ; Pointer to next part of dump
        MRS     r0, SPSR                ; Get stored status register
        TST     r0, #&1F                ; Check if stored mode is User26
        STMEQIA r2!, {r8-r12}           ; Store non-banked registers
        STMEQIA r2, {r13, r14}^         ; Store the banked registers
        SUBEQ   r0, r0, #5 * 4          ; Adjust the base register back
        BEQ     regs_stored$l           ; Skip to next bit if User/User26
        ORR     r0, r0, #&C0            ; Disable interrupts for stored mode
        MRS     r1, CPSR                ; Get current status register
        MSR     CPSR, r0                ; Change to stored mode
        STMIA   r2, {r8-r14}            ; Store banked registers
        MSR     CPSR, r1                ; Return to ABT mode
regs_stored$l
        LDR     r12, workspace_ptr      ; Pointer to module workspace
        LDR     r1, dump_r14abt         ; Get the PC at time of abort
        BIC     r1, r1, #&FC000003      ; Clear flags to get address
        LDR     r0, [r1, #-8]           ; Read the offending instruction
        AND     r1, r0, #&0C000000      ; Mask out bits specific to LDR/STR
        TEQ     r1, #&04000000          ; Test if it is LDR/STR
        BEQ     single_data_transfer$l  ; Branch if it is LDR/STR
        AND     r1, r0, #&0E000000      ; Mask out bits specific to LDM/STM
        TEQ     r1, #&08000000          ; Test if it is LDM/STM
        BEQ     block_data_transfer$l   ; Branch if it is LDM/STM
        LDR     r2, = &0FB00FF0         ; Get mask for bits specific to SWP
        AND     r1, r0, r2              ; Mask out bits specific to SWP
        LDR     r2, = &01000090         ; Get bits which should be set for SWP
        TEQ     r1, r2                  ; Test if it is SWP
        BEQ     single_data_swap$l      ; Branch if it is SWP
        AND     r1, r0, #&0E000000      ; Mask out bits specific to LDC/STC
        TEQ     r1, #&0C000000          ; Test if it is LDC/STC
        BEQ     coprocessor_data_transfer$l; Branch if it is LDC/STC
;        debug_record "data_abort_handler: not a suitable instruction"
        B       pass_on$l               ; Don't bother with other instructions

; A literal pool

        LTORG

; Modifiable data in code space

        ; Register dump area
dump_r0_r7          % 8 * Bits          ; Registers r0-r7 when abort occurred
dump_r8_r14         % 7 * Bits          ; Registers r8-r14 when abort occurred
dump_r14abt         % Bits              ; The r14_abt register (old r15)
dump_r13abt         % Bits              ; The r13_abt register
dump_r13svc         % Bits              ; The r13_svc register
dump_r14svc         % Bits              ; The r14_svc register
dump_r13abt_temp    % Bits              ; The r13_abt register (temporary)

        ; Data to restore writeback value of base register
writeback_ptr       % Ptr               ; Pointer to the base register
writeback_old       % Bits              ; The original value of the base

        ; Other general data
workspace_ptr       % Ptr               ; Value of module private word
previous_handler    % Ptr               ; Previous Data abort handler
handler_semaphore   % Bool              ; Is a Data abort being processed
processor_flags     % Bits              ; Processor specific behaviour

        ; Process LDR/STR instructions
single_data_transfer$l
        ADRL    r1, dump_r0_r7          ; Get pointer to register dump
        AND     r3, r0, #&000F0000      ; Get number of base register
        ADD     r2, r1, r3, LSR#(16-2)  ; Pointer to base register in dump
        STR     r2, writeback_ptr       ; Store pointer to base register
        LDR     r2, [r2]                ; Get value of base register
        STR     r2, writeback_old       ; Store value of base register
        TEQ     r3, #&000F0000          ; Is base r15
        BICEQ   r2, r2, #&FC000003      ; Clear PSR flags if it is
        LDR     r3, = &FFF              ; Mask for offset
        AND     r3, r0, r3              ; Get offset from instruction
        TST     r0, #(1<<25)            ; Is it an immediate offset
        BEQ     single_data_transfer_offset$l; Got offset if it is
        MOV     r4, r3, LSR#5           ; Get shift type and amount
        AND     r3, r0, #&F             ; Get number of offset register
        LDR     r3, [r1, r3, LSL#2]     ; Get value of offset register
        AND     r5, r4, #&3             ; Get the shift type
        TEQ     r5, #0                  ; Is it a logical shift left (LSL)
        BEQ     single_data_transfer_lsl$l; Jump to handler if it is
        TEQ     r5, #1                  ; Is it a logical shift right (LSR)
        BEQ     single_data_transfer_lsr$l; Jump to handler if it is
        TEQ     r5, #2                  ; Is it an arithmetic shift right (ASR)
        BEQ     single_data_transfer_asr$l; Jump to handler if it is
        MOVS    r4, r4, LSR#2           ; Get shift amount
        BEQ     single_data_transfer_rrx$l; Jump to handler for RRX
        MOV     r3, r3, ROR r4          ; Perform the ROR
        B       single_data_transfer_offset$l; Finished so skip the next bit
single_data_transfer_lsl$l
        MOV     r4, r4, LSR#2           ; Get shift amount
        MOV     r3, r3, LSL r4          ; Perform the LSL
        B       single_data_transfer_offset$l; Finished so skip the next bit
single_data_transfer_lsr$l
        MOVS    r4, r4, LSR#2           ; Get shift amount
        MOVEQ   r4, #32                 ; Handle encoding of 32 bit shift
        MOV     r3, r3, LSR r4          ; Peform the LSR
        B       single_data_transfer_offset$l; Finished so skip the next bit
single_data_transfer_asr$l
        MOVS    r4, r4, LSR#2           ; Get shift amount
        MOVEQ   r4, #32                 ; Handle encoding of 32 bit shift
        MOV     r3, r3, ASR r4          ; Peform the ASR
        B       single_data_transfer_offset$l; Finished so skip the next bit
single_data_transfer_rrx$l
        LDR     r4, dump_r14abt         ; Get aborted program counter
        TEQ     pc, r4, LSL#3           ; Get aborted carry flag
        MOV     r3, r3, RRX             ; Perform the RRX
single_data_transfer_offset$l
        TST     r0, #(1<<23)            ; Is it up or down
        RSBEQ   r3, r3, #0              ; Negate offset if down
        AND     r4, r0, #(1<<21) + (1<<24); Get writeback and pre/post bits
        TEQ     r4, #1<<24              ; Is it pre-indexed without writeback
        LDR     r4, processor_flags     ; Get processor specific flags
        TSTNE   r4, #processor_writeback; Only undo writeback if required
        LDR     r4, writeback_ptr       ; Get pointer to base register
        SUBNE   r2, r2, r3              ; Undo writeback if required
        STRNE   r2, [r4]                ; Store modified base register
        TST     r0, #(1<<24)            ; Is it post-indexed
        ADDNE   r2, r2, r3              ; Add offset to base if pre-indexed
        MOV     r1, r2                  ; Copy address
        TST     r0, #(1<<20)            ; Is it a load or a store
        MOVEQ   r0, #1                  ; Store to memory (STR)
        MOVNE   r0, #0                  ; Load from memory (LDR)
        B       check_address$l         ; Check if it is in range of interest

        ; Process LDM/STM instructions
block_data_transfer$l
        ADRL    r1, dump_r0_r7          ; Get pointer to register dump
        AND     r2, r0, #&000F0000      ; Get number of base register
        ADD     r2, r1, r2, LSR#(16-2)  ; Pointer to base register in dump
        STR     r2, writeback_ptr       ; Store pointer to base register
        LDR     r2, [r2]                ; Get value of base register
        STR     r2, writeback_old       ; Store value of base register
        LDR     r2, = &FFFF             ; Register list mask
        AND     r2, r0, r2              ; Get register list
        MOV     r3, #0                  ; Initially no registers counted
block_data_transfer_loop$l
        MOVS    r2, r2, LSR#1           ; Get the lowest bit
        ADDCS   r3, r3, #4              ; Increment the register count if set
        BNE     block_data_transfer_loop$l; Loop if some registers remaining
        TST     r0, #(1<<23)            ; Is it up or down
        RSBEQ   r3, r3, #0              ; Negate count if down
        LDR     r1, writeback_ptr       ; Get pointer to base register
        LDR     r2, [r1]                ; Get final value of base register
        TST     r0, #(1<<21)            ; Did writeback occur
        LDR     r4, processor_flags     ; Get processor specific flags
        TSTNE   r4, #processor_writeback; Only undo writeback if required
        SUBNE   r2, r2, r3              ; Undo writeback if required
        STRNE   r2, [r1]                ; Store modified base register
        TST     r0, #(1<<23)            ; Is it up or down
        MOVEQ   r1, #-4                 ; Single register offset for down
        MOVNE   r1, #4                  ; Single register offset for up
        SUB     r3, r3, r1              ; Correct offset
        TST     r0, #(1<<24)            ; Is it pre or post indexed
        ADDNE   r2, r2, r1              ; Correct base for pre-indexed
        ADD     r3, r2, r3              ; Calculate last address
        CMP     r2, r3                  ; Are the addresses ordered
        MOVLS   r1, r2                  ; Copy start address if ordered
        MOVLS   r2, r3                  ; Copy end address if ordered
        MOVHI   r1, r3                  ; Copy start address if unordered
        TST     r0, #(1<<20)            ; Is it a load or a store
        MOVEQ   r0, #3                  ; Store to memory (STM)
        MOVNE   r0, #2                  ; Load from memory (LDM)
        B       check_address$l         ; Check if it is in range of interest

        ; Process SWP instructions
single_data_swap$l
        ADRL    r1, dump_r0_r7          ; Get pointer to register dump
        AND     r2, r0, #&000F0000      ; Get number of base register
        ADD     r2, r1, r2, LSR#(16-2)  ; Pointer to base register in dump
        STR     r2, writeback_ptr       ; Store pointer to base register
        LDR     r2, [r2]                ; Get value of register
        STR     r2, writeback_old       ; Store value of base register
        MOV     r1, r2                  ; Copy value of base register
        MOV     r0, #6                  ; A store is always performed
        B       check_address$l         ; Check if it is in range of interest

        ; Process LDC/STC instructions (with special case for LDF/STF)
coprocessor_data_transfer$l
        ADRL    r1, dump_r0_r7          ; Get pointer to register dump
        AND     r3, r0, #&000F0000      ; Get number of base register
        ADD     r2, r1, r3, LSR#(16-2)  ; Pointer to base register in dump
        STR     r2, writeback_ptr       ; Store pointer to base register
        LDR     r2, [r2]                ; Get value of base register
        STR     r2, writeback_old       ; Store value of base register
        TEQ     r3, #&000F0000          ; Is base r15
        BICEQ   r2, r2, #&FC000003      ; Clear PSR flags if it is
        AND     r2, r0, #&FF            ; Get the immediate offset
        MOV     r2, r2, LSL#2           ; Convert word offset to byte offset
        TST     r0, #(1<<23)            ; Is it up or down
        RSBEQ   r2, r2, #0              ; Give the offset the correct sign
        TST     r0, #(1<<21)            ; Is writeback used
        TEQNE   r3, #&000F0000          ; Is base r15
        LDR     r3, writeback_ptr       ; Get pointer to base register
        LDR     r1, [r3]                ; Get value of base register
        SUBNE   r1, r1, r2              ; Subtract offset if writeback
        STRNE   r1, [r3]                ; Store original base if writeback
        TST     r0, #(1<<24)            ; Is it pre-indexed
        ADDNE   r1, r1, r2              ; Add offset if pre-indexed
        MOV     r2, r1                  ; Copy the address
        AND     r3, r0, #&F00           ; Get the coprocessor number
        TEQ     r3, #&100               ; Is it the floating point processor
        BNE     coprocessor_data_transfer_done$l; Don't know about any others
        LDR     r3, = &408000           ; Mask for precision
        AND     r3, r0, r3              ; Get the precision used
        TEQ     r3, #&008000            ; Is it 64 bit (2 words)
        ADDEQ   r2, r2, #4              ; It is double precision
        TST     r3, #&400000            ; Is it 96 bit (3 words)
        ADDNE   r2, r2, #8              ; Extended precision or packed decimal
coprocessor_data_transfer_done$l
        TST     r0, #(1<<20)            ; Is it a load or a store
        MOVEQ   r0, #5                  ; Store to memory (STC)
        MOVNE   r0, #4                  ; Load from memory (LDC)
        B       check_address$l         ; Check if it is in range of interest

; A literal pool

        LTORG

        ; Check whether the abort address is in the virtual memory area
check_address$l
;        debug_record "data_abort_handler: check_address"
        [       :DEF: demo
        LDR     r4, dump_r14abt         ; Get the PC at time of abort
        BIC     r4, r4, #&FC000003      ; Clear flags to get address
        SUB     r4, r4, #8              ; Address of aborted instruction
        ADRL    r5, base                ; Address of the start of the module
        CMP     r4, r5                  ; Compare address to module start
        BLO     check_address_norm$l    ; Skip next bit if below the module
        ADRL    r5, end                 ; Address of the end of the module
        CMP     r4, r5                  ; Compare address to module end
        BHS     check_address_norm$l    ; Skip next bit if above the module
        MRS     r1, CPSR                ; Get status register
        BIC     r1, r1, #&1F            ; Clear current mode bits
        ORR     r1, r1, #&03            ; Set SVC26 mode
        MSR     CPSR, r1                ; Change to SVC26 mode
        STR     r13, dump_r13svc        ; Store r13_svc
        STR     r14, dump_r14svc        ; Store r14_svc
        LDR     r12, workspace_ptr      ; Pointer to module workspace
        ADRL    r0, dump_r0_r7          ; Pointer to register dump
        LDR     r0, [r0, #4 * 8]        ; Pointer to dynamic area name
check_address_loop$l
        LDRB    r2, [r0], #1            ; Read a character
        TEQ     r2, #0                  ; Is it the terminator
        BEQ     check_address_out$l     ; Exit loop if it is the terminator
        ADD     r1, r2, r1, LSL#8       ; Include this character
        B       check_address_loop$l    ; Loop for next character
check_address_out$l
        LDR     r0, = ('D' << 24) + ('E' << 16) + ('M' << 8) + 'O'; DEMO
        TEQ     r0, r1                  ; Does it match
        BNE     check_address_not$l     ; Skip next bit if not
        LDR     r0, dump_r14abt         ; Get return address
        LDR     r1, writeback_old       ; Value of base register
        ADD     r0, r0, r1              ; Correct return address
        STR     r0, dump_r14abt         ; Store the modified address
        B       continue$l              ; Resume execution
check_address_not$l
        LDR     r0, dump_r14abt         ; Get return address
        ADD     r0, r0, #4              ; Skip aborted instruction
        STR     r0, dump_r14abt         ; Store the modified return address
        B       continue$l              ; Resume execution
check_address_norm$l
        ]
        CMP     r2, #&1F                ; Is it one of the processor vectors
        BLS     pass_on_restore$l       ; Pass it on immediately if it is
        LDR     r4, ws_log_page_size    ; Get log of the page size
        MOV     r1, r1, LSR r4          ; Convert start to page offset
        MOV     r2, r2, LSR r4          ; Convert end to page offset
        LDR     r3, ws_area_ptr         ; Pointer to the first area record
check_address_find$l
        TEQ     r3, #0                  ; Is it a valid pointer
        BEQ     pass_on_restore$l       ; Pass on if not found
        LDR     r5, [r3, #area_base]    ; Get base address of virtual area
        MOV     r5, r5, LSR r4          ; Convert base to page offset
        LDR     r6, [r3, #area_total_pages];Get number of pages in area
        ADD     r6, r5, r6              ; Get end page offset of area
        CMP     r1, r5                  ; Compare lower bound
        BLT     check_address_next$l    ; If outside then try next area
        CMP     r2, r6                  ; Compare upper bound
        BLT     check_address_found$l   ; If inside then exit loop
check_address_next$l
        LDR     r3, [r3, #area_next]    ; Pointer to next record if no match
        B       check_address_find$l    ; Loop for next record if not found
check_address_found$l
        SUB     r4, r1, r5              ; Subtract base from lower bound
        SUB     r5, r2, r5              ; Subtract base from upper bound
        LDR     r1, [r3, #area_virtual_array]; Get pointer to page array
        MOV     r2, #virtual_item       ; Size of entries in array
        MLA     r6, r2, r4, r1          ; Pointer to start page entry
        MLA     r7, r2, r5, r1          ; Pointer to end page entry
        LDR     r6, [r6, #virtual_ptr]  ; Get page array entry
        TST     r6, #virtual_flags_paged; Is it paged in
        LDR     r7, [r7, #virtual_ptr]  ; Get page array entry
        TSTNE   r7, #virtual_flags_paged; Is it paged in
        ADRNEL  r0, page_in_err$l       ; Pointer to error block
        BNE     pass_on_restore$l       ; Pass on if memory already present
        BIC     r6, r6, #virtual_flags_mask; Mask out flag bits
        BIC     r7, r7, #virtual_flags_mask; Mask out flag bits
        MOV     r1, #0                  ; Value to clear command flag with
        STR     r1, ws_command_flag     ; Clear command flag
        ADRL    r2, ws_page_faults      ; Pointer to page fault table
        LDR     r1, [r2, r0, LSL#2]     ; Get old number of faults
        ADD     r1, r1, #1              ; Increment number of faults
        STR     r1, [r2, r0, LSL#2]     ; Store new number of faults
        MRS     r1, CPSR                ; Get status register
        BIC     r1, r1, #&1F            ; Clear current mode bits
        ORR     r1, r1, #&03            ; Set SVC26 mode
        MSR     CPSR, r1                ; Change to SVC26 mode
        STR     r13, dump_r13svc        ; Store r13_svc
        STR     r14, dump_r14svc        ; Store r14_svc
        LDR     r12, workspace_ptr      ; Pointer to module workspace
        TEQ     r6, #0                  ; Is a page already assigned
        BNE     page_in_got_first$l     ; Skip assignment if present
        BL      choose_page             ; Choose a suitable page
        BVS     return_error_svc$l      ; Return an error if generated
        TEQ     r0, #0                  ; Was a suitable page found
        ADREQ   r0, page_in_err$l       ; Pointer to error block
        BEQ     return_error_svc$l      ; Return an error if generated
        MOV     r6, r0                  ; Copy page pointer
        MOV     r1, r3                  ; Copy pointer to virtual area
        MOV     r2, r4                  ; Copy start page offset
        BL      assign_more_pages       ; Page in the first page
        BVS     return_error_svc$l      ; Return an error if generated
page_in_got_first$l
        MOV     r0, r6                  ; Copy page pointer
        BL      push_page               ; Ensure that it is paged in
        BVS     page_in_fail$l          ; Pass on if error produced
        LDR     r0, [r6, #source_locks] ; Get number of times locked
        ADD     r0, r0, #1              ; Increment lock count
        STR     r0, [r6, #source_locks] ; Store new number of locks
        TEQ     r4, r5                  ; Is it a single page
        BEQ     page_in_done$l          ; Don't handle second page if it is
        LDR     r1, [r3, #area_virtual_array]; Get pointer to page array
        MOV     r2, #virtual_item       ; Size of entries in array
        MLA     r7, r2, r5, r1          ; Pointer to end page entry
        LDR     r7, [r7, #virtual_ptr]  ; Get page array entry
        BIC     r7, r7, #virtual_flags_mask; Mask out flag bits
        TEQ     r7, #0                  ; Is a page already assigned
        BNE     page_in_push$l          ; Skip assignment if present
        BL      choose_page             ; Choose a suitable page
        BVS     return_error_svc$l      ; Return an error if generated
        TEQ     r0, #0                  ; Was a suitable page found
        BEQ     page_in_fail$l          ; Pass on if none found
        MOV     r7, r0                  ; Copy page pointer
        MOV     r1, r3                  ; Copy pointer to virtual area
        MOV     r2, r5                  ; Copy end page offset
        BL      assign_more_pages       ; Page in the second page
        BVS     page_in_fail$l          ; Pass on if error produced
page_in_push$l
        MOV     r0, r7                  ; Copy page pointer
        BL      push_page               ; Ensure that it is paged in
        BVS     page_in_fail$l          ; Pass on if errr produced
page_in_done$l
        LDR     r0, [r6, #source_locks] ; Get number of times locked
        SUB     r0, r0, #1              ; Decrement lock count
        STR     r0, [r6, #source_locks] ; Store new number of locks
        B       continue$l              ; Resume execution
page_in_fail$l
        LDR     r1, [r6, #source_locks] ; Get number of times locked
        SUB     r1, r1, #1              ; Decrement lock count
        STR     r1, [r6, #source_locks] ; Store new number of locks
        ADRVC   r0, page_in_err$l       ; Pointer to error block
        B       return_error_svc$l      ; Return an error if generated

page_in_err$l
        ErrBlck "Virtualise has suffered a serious internal error"

        ; Pass on to the previous handler from SVC26 mode
pass_on_restore_svc$l
        assert_registers                ; Save register values
        BL      check_all_debug         ; Perform a consistency check
        LDR     r13, dump_r13svc        ; Retore r13_svc
        LDR     r14, dump_r14svc        ; Retore r14_svc
        MRS     r0, CPSR                ; Get the status register
        BIC     r0, r0, #&1F            ; Clear the mode bits
        ORR     r0, r0, #&17            ; Set ABT mode
        ORR     r0, r0, #&C0            ; Ensure interrupts are disabled
        MSR     CPSR, r0                ; Change to ABT mode

        ; Pass on to the previous handler after restoring base register
pass_on_restore$l
        LDR     r13, writeback_ptr      ; Pointer to base register in dump
        LDR     r14, writeback_old      ; Get the original value of base
        STR     r14, [r13]              ; Restore original value of base

        ; Pass on to the previous handler
pass_on$l
        MOV     r13, #0                 ; Value to clear semaphore with
        STR     r13, handler_semaphore  ; Clear the semaphore
        ADRL    r13, dump_r0_r7         ; Pointer to register dump
        LDMIA   r13, {r0-r12}           ; Restore main registers
        LDR     r13, dump_r13abt        ; Restore stack pointer
        LDR     r14, dump_r14abt        ; Get return address
;        debug_record "data_abort_handler: pass_on"
        LDR     pc, previous_handler    ; Jump to previous handler

        ; Exit with an error from SVC26 mode
return_error_svc$l
        SetV                            ; Ensure that error flag is set
        assert_registers                ; Save register values
        BL      check_all_debug         ; Perform a consistency check
        LDR     r13, dump_r13svc        ; Retore r13_svc
        LDR     r14, dump_r14svc        ; Retore r14_svc
        MRS     r1, CPSR                ; Get the status register
        BIC     r1, r1, #&1F            ; Clear the mode bits
        ORR     r1, r1, #&17            ; Set ABT mode
        ORR     r1, r1, #&C0            ; Ensure interrupts are disabled
        MSR     CPSR, r1                ; Change to ABT mode

        ; Exit with an error
return_error$l
        MOV     r13, #0                 ; Value to clear semaphore with
        STR     r13, handler_semaphore  ; Clear the semaphore
        LDR     r13, dump_r13abt        ; Restore stack pointer
        MRS     r1, SPSR                ; Get the stored status register
        MSR     CPSR, r1                ; Change to original mode
;        debug_record "data_abort_handler: return_error"
        SWI     OS_GenerateError        ; Generate the error

        ; Continue execution from the aborted instruction
continue$l
        LDR     r13, dump_r13svc        ; Retore r13_svc
        LDR     r14, dump_r14svc        ; Retore r14_svc
        MRS     r0, CPSR                ; Get the status register
        BIC     r0, r0, #&1F            ; Clear the mode bits
        ORR     r0, r0, #&17            ; Set ABT mode
        ORR     r0, r0, #&C0            ; Ensure interrupts are disabled
        MSR     CPSR, r0                ; Change to ABT mode
        ADRL    r2, dump_r8_r14         ; Pointer to second half of dump
        MRS     r0, SPSR                ; Get stored status register
        TST     r0, #&1F                ; Check if stored mode is User26
        LDMEQIA r2, {r8-r14}^           ; If so, restore user bank registers
        BEQ     continue_restored$l     ; Skip to next bit if User26
        ORR     r0, r0, #&C0            ; Disable interrupts for stored mode
        MRS     r1, CPSR                ; Get current status register
        MSR     CPSR, r0                ; Change to stored mode
        LDMIA   r2, {r8-r14}            ; Restore banked registers
        MSR     CPSR, r1                ; Return to ABT mode
continue_restored$l
        MOV     r13, #0                 ; Value to clear semaphore with
        STR     r13, handler_semaphore  ; Clear the semaphore
        ADRL    r13, dump_r0_r7         ; Pointer to register dump
        LDMIA   r13, {r0-r7}            ; Restore first half of registers
        LDR     r13, dump_r13abt        ; Restore stack pointer
        LDR     r14, dump_r14abt        ; Get return address
;        debug_record "data_abort_handler: continue_restored"
        SUBS    pc, r14, #8             ; Return to aborted instruction

; A literal pool

        LTORG

; Handle file system vectors

        ;   Parameters  : None
        ;   Returns     : None
        ;   Description : Claim the file system vectors.
claim_fs_vectors
        LocalLabels
        JSR     "r0-r2, r11"            ; Stack registers
        MOV     r2, r12                 ; Value to be passed in r12
        MOV     r0, #FileV              ; Number of FileV vector
        ADRL    r1, filev_handler       ; Pointer to handler for FileV
        SWI     XOS_Claim               ; Claim the FileV vector
        RTE VS                          ; Return with error
        MOV     r0, #ArgsV              ; Number of ArgsV vector
        ADRL    r1, argsv_handler       ; Pointer to handler for ArgsV
        SWI     XOS_Claim               ; Claim the ArgsV vector
        MOVVS   r11, r0                 ; Copy error pointer
        BVS     abort_filev$l           ; Release any vectors already claimed
        MOV     r0, #GBPBV              ; Number of GBPBV vector
        ADRL    r1, gbpbv_handler       ; Pointer to handler for GBPBV
        SWI     XOS_Claim               ; Claim the GBPBV vector
        MOVVS   r11, r0                 ; Copy error pointer
        BVS     abort_argsv$l           ; Release any vectors already claimed
        MOV     r0, #FindV              ; Number of FindV vector
        ADRL    r1, findv_handler       ; Pointer to handler for FindV
        SWI     XOS_Claim               ; Claim the FindV vector
        MOVVS   r11, r0                 ; Copy error pointer
        BVS     abort_gbpbv$l           ; Release any vectors already claimed
        MOV     r0, #FSCV               ; Number of FSCV vector
        ADRL    r1, fscv_handler        ; Pointer to handler for FSCV
        SWI     XOS_Claim               ; Claim the FSCV vector
        MOVVS   r11, r0                 ; Copy error pointer
        BVS     abort_findv$l           ; Release any vectors already claimed
        RTS                             ; Return from subroutine

        ;   Parameters  : None
        ;   Returns     : None
        ;   Description : Release the file system vectors.
release_fs_vectors
        JSR     "r0-r2, r11"            ; Stack registers
        MOV     r11, #0                 ; Clear error pointer
        MOV     r2, r12                 ; Value to be passed in r12
        MOV     r0, #FSCV               ; Number of FSCV vector
        ADRL    r1, fscv_handler        ; Pointer to handler for FSCV
        SWI     XOS_Release             ; Release FSCV vector
abort_findv$l
        MOV     r0, #FindV              ; Number of FindV vector
        ADRL    r1, findv_handler       ; Pointer to handler for FindV
        SWI     XOS_Release             ; Release FindV vector
abort_gbpbv$l
        MOV     r0, #GBPBV              ; Number of GBPBV vector
        ADRL    r1, gbpbv_handler       ; Pointer to handler for GBPBV
        SWI     XOS_Release             ; Release GBPBV vector
abort_argsv$l
        MOV     r0, #ArgsV              ; Number of ArgsV vector
        ADRL    r1, argsv_handler       ; Pointer to handler for ArgsV
        SWI     XOS_Release             ; Release ArgsV vector
abort_filev$l
        MOV     r0, #FileV              ; Number of FileV vector
        ADRL    r1, filev_handler       ; Pointer to handler for FileV
        SWI     XOS_Release             ; Release FileV vector
        TEQ     r11, #0                 ; Is there any error to return
        RTSS EQ                         ; Return from subroutine if not
        MOV     r0, r11                 ; Copy the error pointer
        SetV                            ; Set the error flag
        RTE                             ; Return with the error

        ;   Parameters  : r0    - Reason code.
        ;                 Other registers depend on reason code.
        ;   Returns     : r0    - Corrupted.
        ;                 Other registers depend on reason code.
        ;   Description : Handle the FileV vector, through which OS_File is
        ;                 indirected.
filev_handler
        LocalLabels
        debug_record "filev_handler"
        STMFD   r13!, {r0-r12, r14}     ; Stack registers
        MOV     r11, r13                ; Copy stack pointer after dump
        CMP     r0, #(jump_end$l - jump$l) >> 2; Is it in range
        ADDLO   pc, pc, r0, LSL#2       ; Dispatch if in range
        B       jump_end$l              ; Reason code not in jump table
jump$l  B       save$l                  ; OSFile_Save (0)
        B       misc_op$l               ; OSFile_Write (1)
        B       misc_op$l               ; OSFile_WriteLoad (2)
        B       misc_op$l               ; OSFile_WriteExec (3)
        B       misc_op$l               ; OSFile_WriteAttr (4)
        B       misc_op$l               ; OSFile_Read (5)
        B       misc_op$l               ; OSFile_Delete (6)
        B       misc_op$l               ; OSFile_Create (7)
        B       misc_op$l               ; OSFile_CreateDir (8)
        B       misc_op$l               ; OSFile_Stamp (9)
        B       save$l                  ; OSFile_SaveStamped (10)
        B       misc_op$l               ; OSFile_CreateStamped (11)
        B       load$l                  ; OSFile_LoadPath/StampedPath (12)
        B       misc_op_path$l          ; OSFile_ReadPath (13)
        B       load$l                  ; OSFile_LoadPathVar/StampedPathVar (14)
        B       misc_op_path$l          ; OSFile_ReadPathVar (15)
        B       load$l                  ; OSFile_LoadNoPath/StampedNoPath (16)
        B       misc_op$l               ; OSFile_ReadNoPath (17)
        B       misc_op$l               ; OSFile_SetType (18)
        B       misc_op$l               ; OSFile_MakeError (19)
        B       misc_op$l               ; OSFile_ReadStamped (20)
        B       misc_op_path$l          ; OSFile_ReadStampedPath (21)
        B       misc_op_path$l          ; OSFile_ReadStampedPathVar (22)
        B       misc_op$l               ; OSFile_ReadStampedNoPath (23)
        B       misc_op$l               ; OSFile_ReadBlockSize (24)
jump_end$l
        TEQ     r0, #OSFile_Load        ; OSFile_Load (255)
        BEQ     load$l                  ; Branch to OSFile_Load handler
        B       vector_pass_on          ; Unknown reason code

        ; Load a named file
load$l  BL      stack_lock_prepare      ; Prepare stack for locked list
        MOV     r0, r1                  ; Copy pointer to object name
        BL      stack_lock_string       ; Lock memory used by object name
        BVS     exit$l                  ; Return with error
        LDMIA   r11, {r0}               ; Restore parameters
        TEQ     r0, #OSFile_LoadPath    ; Does it use a path string
        TEQNE   r0, #OSFile_LoadPathVar ; Does it use a path variable
        MOVEQ   r0, r4                  ; Copy pointer to path string
        BLEQ    stack_lock_string       ; Lock memory used by path
        BVS     exit$l                  ; Return with error
        LDMIA   r11, {r0}               ; Restore original reason code
        TEQ     r0, #OSFile_LoadPath    ; Is a path string used
        MOVEQ   r0, #OSFile_ReadPath    ; Reason code using path string
        TEQ     r0, #OSFile_LoadPathVar ; Is a path variable used
        MOVEQ   r0, #OSFile_ReadPathVar ; Reason code using path variable
        TEQ     r0, #OSFile_LoadNoPath  ; Is any path being used
        MOVEQ   r0, #OSFile_ReadNoPath  ; Reason code using no path
        TEQ     r0, #OSFile_Load        ; Is File$Path used
        MOVEQ   r0, #OSFile_Read        ; Reason code using File$Path
        BL      vector_call             ; Call the previous vector claimant
        BVS     exit$l                  ; Return with error
        MOV     r5, r2                  ; Copy load address
        MOV     r6, r4                  ; Copy object length
        LDMIA   r11, {r0-r4}            ; Restore parameters
        TST     r3, #&FF                ; Is load address specified
        MOVEQ   r5, r2                  ; Use specified load address if given
        MOV     r1, r5                  ; Copy load (start) address
        ADD     r2, r1, r6              ; Calculate end address
        SUB     r2, r2, #1              ; Make range inclusive
        BL      find_part               ; Test for overlap with virtual memory
        TEQ     r0, #0                  ; Is virtual memory used
        BEQ     simple$l                ; Use the simple approach if not
        LDMIA   r11, {r0-r4}            ; Restore parameters
        MOV     r2, r4                  ; Copy optional pointer to path
        MOV     r3, r0                  ; Copy original reason code
        MOV     r0, #OSFind_Openin\
                :OR: OSFind_ErrorIfAbsent\
                :OR: OSFind_ErrorIfDir  ; Reason code to open file with errors
        TEQ     r3, #OSFile_LoadPath    ; Is a path string used
        ORREQ   r0, r0, #OSFind_Path    ; Reason code using path string
        TEQ     r3, #OSFile_LoadPathVar ; Is a path variable used
        ORREQ   r0, r0, #OSFind_PathVar ; Reason code using path variable
        TEQ     r3, #OSFile_LoadNoPath  ; Is any path being used
        ORREQ   r0, r0, #OSFind_NoPath  ; Reason code using no path
        SWI     XOS_Find                ; Open the file for read access only
        BVS     exit$l                  ; Return with error
        MOV     r10, r0                 ; Copy open file handle
        MOV     r0, #OSGBPB_ReadAt      ; Reason code to read from my pointer
        MOV     r1, r10                 ; Copy file handle
        MOV     r2, r5                  ; Copy start address in memory
        MOV     r3, r6                  ; Copy number of bytes to read
        MOV     r4, #0                  ; Start from beginning of file
        SWI     XOS_GBPB                ; Read the data in one lump
        BL      close$l                 ; Close the file, keeping error
        BVS     exit$l                  ; Return with error
        LDMIA   r11, {r0-r4}            ; Restore parameters
        TEQ     r0, #OSFile_LoadPath    ; Is a path string used
        MOVEQ   r0, #OSFile_ReadPath    ; Reason code using path string
        TEQ     r0, #OSFile_LoadPathVar ; Is a path variable used
        MOVEQ   r0, #OSFile_ReadPathVar ; Reason code using path variable
        TEQ     r0, #OSFile_LoadNoPath  ; Is any path being used
        MOVEQ   r0, #OSFile_ReadNoPath  ; Reason code using no path
        TEQ     r0, #OSFile_Load        ; Is File$Path used
        MOVEQ   r0, #OSFile_Read        ; Reason code using File$Path
        BL      vector_call             ; Call the previous vector claimant
        ADD     r10, r11, #4 * 6        ; Pointer to parameter r6 on stack
        LDMIA   r10, {r6-r9}            ; Restore parameters
        B       exit$l                  ; Return to caller

        ; Save a block of memory as a file
save$l  BL      stack_lock_prepare      ; Prepare stack for locked list
        MOV     r0, r1                  ; Copy pointer to filename
        BL      stack_lock_string       ; Lock memory used by object name
        BVS     exit$l                  ; Return with error
        MOV     r1, r4                  ; Copy start address
        SUB     r2, r5, #1              ; Calculate inclusive end address
        BL      find_part               ; Test for overlap with virtual memory
        TEQ     r0, #0                  ; Is virtual memory used
        BEQ     simple$l                ; Use simple approach if not
        LDMIA   r11, {r0-r2}            ; Restore parameters
        TEQ     r0, #OSFile_Save        ; Is it load and execute addresses
        MOVEQ   r0, #OSFile_Create      ; Create with load and execute
        TEQ     r0, #OSFile_SaveStamped ; Is it file type
        MOVEQ   r0, #OSFile_CreateStamped; Create with file type
        BL      vector_call             ; Create empty file with attributes
        BVS     exit$l                  ; Return with error
        LDMIA   r11, {r0-r1}            ; Restore parameters
        MOV     r0, #OSFind_Openup\
                :OR: OSFind_ErrorIfAbsent\
                :OR: OSFind_ErrorIfDir\
                :OR: OSFind_NoPath      ; Reason code to open file with errors
        SWI     XOS_Find                ; Open the file for read access only
        BVS     exit$l                  ; Return with error
        MOV     r10, r0                 ; Copy open file handle
        LDMIA   r11, {r0-r1}            ; Restore parameters
        MOV     r0, #OSGBPB_WriteAt     ; Reason code to write to my pointer
        MOV     r1, r10                 ; Copy file handle
        MOV     r2, r4                  ; Copy start address in memory
        SUB     r3, r5, r4              ; Calculate number of bytes to write
        MOV     r4, #0                  ; Start at beginning of file
        SWI     XOS_GBPB                ; Write the data in one lump
        BLVS    tidy$l                  ; Close and delete file if error
        BVS     exit$l                  ; Return with the error
        BL      close$l                 ; Close the file
        LDMIA   r11, {r0-r3}            ; Restore parameters
        TEQ     r0, #OSFile_Save        ; Is it load and execute addresses
        MOVEQ   r0, #OSFile_Write       ; Write load and execute
        TEQ     r0, #OSFile_SaveStamped ; Is it file type
        MOVEQ   r0, #OSFile_SetType     ; Write file type
        MOV     r5, #FileSwitch_AttrOwnerRead\
                :OR: FileSwitch_AttrOwnerWrite; File attributes
        BL      vector_call             ; Set file attributes
        BVS     exit$l                  ; Return with error
        LDMIA   r11, {r0-r4}            ; Restore parameters
        B       exit$l                  ; Return normally

        ; Tidy up if a save operation goes wrong
tidy$l  JSR     "r0"                    ; Stack registers
        BL      close$l                 ; Close the file
        LDMIA   r11, {r0-r1}            ; Restore parameters
        MOV     r0, #OSFile_Delete      ; Reason code to delete file
        BL      vector_call             ; Call the previous vector claimant
        LDMIA   r11, {r0-r4}            ; Restore parameters
        RTSS                            ; Return from the subroutine

        ; Close file keeping any error pointer and condition
close$l JSR     "r0, r1"                ; Stack registers
        MOV     r0, #OSFind_Close       ; Reason code to close file
        MOV     r1, r10                 ; Copy open file handle
        SWI     XOS_Find                ; Close the file
        RTSS                            ; Return from subroutine

        ; Handle loads and saves when virtual memory is not used
simple$l
        LDMIA   r11, {r0-r9}            ; Restore all parameters
        BL      vector_call             ; Call the previous vector claimant
        B       exit$l                  ; Return to caller

        ; Perform a miscellaneous operation without a path
misc_op$l
        BL      stack_lock_prepare      ; Prepare stack for locked list
        MOV     r0, r1                  ; Copy pointer to object name
        BL      stack_lock_string       ; Lock memory used by object name
        BVS     exit$l                  ; Return with error
        LDMIA   r11, {r0}               ; Restore parameters
        BL      vector_call             ; Call the previous vector claimant
        B       exit$l                  ; Return to caller

        ; Perform a miscellaneous operation using a specified path
misc_op_path$l
        BL      stack_lock_prepare      ; Prepare stack for locked list
        MOV     r0, r1                  ; Copy pointer to object name
        BL      stack_lock_string       ; Lock memory used by object name
        BVS     exit$l                  ; Return with error
        MOV     r0, r4                  ; Copy pointer to path
        BL      stack_lock_string       ; Lock memory used by path
        BVS     exit$l                  ; Return with error
        LDMIA   r11, {r0}               ; Restore parameters
        BL      vector_call             ; Call the previous vector claimant
        B       exit$l                  ; Return to caller

        ; Return from any of the intercepts, unlocking pages
exit$l  BL      stack_unlock            ; Unlock pages in list on stack
        B       vector_return           ; Return to caller

        ;   Parameters  : r0    - Reason code.
        ;                 r1    - File handle, or 0.
        ;                 r2    - Attribute to write, or not used.
        ;   Returns     : r0    - Filing system number, or preserved.
        ;                 r1    - Preserved.
        ;                 r2    - Attribute that was read, or preserved.
        ;   Description : Handle the ArgsV vector, through which OS_Args is
        ;                 indirected.
argsv_handler
        LocalLabels
        debug_record "argsv_handler"
        STMFD   r13!, {r0-r12, r14}     ; Stack registers
        MOV     r11, r13                ; Copy stack pointer after dump
        TEQ     r0, #OSArgs_ReadPath    ; Is reason code for canonicalise name
        BNE     vector_pass_on          ; Pass on to original handler if not
        BL      stack_lock_prepare      ; Prepare stack for locked list
        MOV     r0, r2                  ; Copy pointer to buffer
        ADD     r1, r0, r5              ; Calculate end address of buffer
        BL      stack_lock_region       ; Lock memory used by buffer
        BVS     exit$l                  ; Return with error
        LDMIA   r11, {r0-r1}            ; Restore parameters
        BL      vector_call             ; Call the previous vector claimant
exit$l  BL      stack_unlock            ; Unlock pages in list on stack
        B       vector_return           ; Return to caller

        ;   Parameters  : r0    - Reason code.
        ;                 Other registers depend on reason code.
        ;   Returns     : r0    - Preserved.
        ;                 Other registers depends on reason code.
        ;   Description : Handle the GBPBV vector, through which OS_GBPB is
        ;                 indirected.
gbpbv_handler
        LocalLabels
        debug_record "gbpbv_handler"
        STMFD   r13!, {r0-r12, r14}     ; Stack registers
        MOV     r11, r13                ; Copy stack pointer after dump
        CMP     r0, #(jump_end$l - jump$l) >> 2; Is it in range
        ADDLO   pc, pc, r0, LSL#2       ; Dispatch if in range
        B       vector_pass_on          ; Unknown reason code
jump$l  B       vector_pass_on          ; Reason code is unused (0)
        B       read_write$l            ; OSGBPB_WriteAt (1)
        B       read_write$l            ; OSGBPB_Write (2)
        B       read_write$l            ; OSGBPB_ReadAt (3)
        B       read_write$l            ; OSGBPB_Read (4)
        B       read_name$l             ; OSGBPB_ReadDiscName (5)
        B       read_name$l             ; OSGBPB_ReadCSDName (6)
        B       read_name$l             ; OSGBPB_ReadLibName (7)
        B       csd_entries$l           ; OSGBPB_CSDEntries (8)
        B       dir_entries$l           ; OSGBPB_DirEntries (9)
        B       dir_entries$l           ; OSGBPB_DirEntriesInfo (10)
        B       dir_entries$l           ; OSGBPB_DirEntriesSystemInfo (11)
        B       dir_entries$l           ; OSGBPB_DirEntriesInfoStamped (12)
jump_end$l

        ; Read bytes from or write bytes to an open file
read_write$l
        LDR     r0, ws_area_ptr         ; Get pointer to first area record
read_write_loop$l
        TEQ     r0, #0                  ; Are there any more areas
        BEQ     read_write_ext$l        ; Branch to external access handler
        LDR     r2, [r0, #area_file_handle]; Get the file handle
        TEQ     r1, r2                  ; Do the file handles match
        BEQ     read_write_int$l        ; Branch to internal access handler
        LDR     r0, [r0, #area_next]    ; Get pointer to next area record
        B       read_write_loop$l       ; Loop for next area
read_write_int$l
        LDMIA   r11, {r0-r2}            ; Restore parameters
        B       vector_pass_on          ; Pass on unmolested
read_write_ext$l
        LDMIA   r11, {r0-r2}            ; Restore parameters
        BL      stack_lock_prepare      ; Prepare stack for locked list
        MOV     r0, r2                  ; Copy pointer to buffer
        ADD     r1, r0, r3              ; Calculate end address of buffer
        BL      stack_lock_region       ; Attempt to lock memory used by buffer
        BVS     read_write_split$l      ; Split operation if too large
        LDMIA   r11, {r0-r1}            ; Restore parameters
        BL      vector_call             ; Call the previous vector claimant
        B       exit$l                  ; Return to caller
read_write_split$l
        LDMIA   r11, {r0-r1}            ; Restore parameters
        TEQ     r0, #OSGBPB_WriteAt     ; Is it write at specified
        TEQNE   r0, #OSGBPB_ReadAt      ; Is it read at specified
        BNE     read_write_pos$l        ; Skip next bit if use current pointer
        MOV     r0, #OSArgs_SetPtr      ; Reason code to set sequential pointer
        MOV     r2, r4                  ; Copy required pointer position
        SWI     XOS_Args                ; Set the file's sequential pointer
        BVS     exit$l                  ; Return with error
        LDMIA   r11, {r0-r2}            ; Restore parameters
read_write_pos$l
        MOV     r5, r2                  ; Copy start address in memory
        MOV     r6, r3                  ; Copy number of bytes to transfer
read_write_next$l
        BL      stack_unlock            ; Unlock any previously locked pages
        BL      stack_lock_prepare      ; Prepare stack for locked list
        MOV     r0, r5                  ; Copy next start address
        ADD     r1, r0, r6              ; Calculate end address
        BL      stack_lock_initial      ; Lock the next block of memory
        BVS     exit$l                  ; Return with error
        MOV     r2, r5                  ; Copy pointer to next byte to transfer
        SUBS    r3, r0, r2              ; Number of bytes in this transfer
        BLE     read_write_done$l       ; Exit if nothing to transfer
        MOV     r5, r3                  ; Copy number of bytes in transfer
        LDMIA   r11, {r0-r1}            ; Restore parameters
        TEQ     r0, #OSGBPB_WriteAt     ; Is it write at specified
        MOVEQ   r0, #OSGBPB_Write       ; Change to write at current
        TEQ     r0, #OSGBPB_ReadAt      ; Is it read at specified
        MOVEQ   r0, #OSGBPB_Read        ; Change to read at current
        BL      vector_call             ; Call the previous vector claimant
        BVS     exit$l                  ; Return with error
        BCS     read_write_done$l       ; Exit if transfer not completed
        SUB     r3, r5, r3              ; Calculate bytes transferred
        MOV     r5, r2                  ; Copy pointer to end of transfer
        SUB     r6, r6, r3              ; Calculate number of bytes remaining
        CMP     r6, #0                  ; Is the read/write complete
        BGT     read_write_next$l       ; Loop if not
read_write_done$l
        LDMIA   r11, {r0-r1}            ; Restore parameters
        MOV     r0, #OSArgs_ReadPtr     ; Reason code to read sequential pointer
        SWI     XOS_Args                ; Read file pointer
        BVS     exit$l                  ; Return with error
        MOV     r4, r2                  ; Copy sequential file pointer
        MOV     r2, r5                  ; Copy pointer to after last byte
        MOV     r3, r6                  ; Copy number of bytes remaining
        ADD     r0, r11, #4 * 4         ; Pointer to r4 on stack
        LDMIA   r0, {r4-r6}             ; Restore parameters
        LDMIA   r11, {r0-r1}            ; Restore more parameters
        ClearFlags                      ; Clear carry flag
        CMP     r3, #0                  ; Any bytes not transferred
        BLE     exit$l                  ; Return to caller if finished
        SetC                            ; Set the carry flag
        B       exit$l                  ; Return to caller

        ; Read information on a filing system
read_name$l
        BL      stack_lock_prepare      ; Prepare stack for locked list
        MOV     r0, r2                  ; Copy pointer to buffer
        LDR     r1, = 259               ; Maximum length of buffer
        ADD     r1, r0, r1              ; Calculate end address of buffer
        BL      stack_lock_region       ; Lock memory used by buffer
        BVS     exit$l                  ; Return with error
        LDMIA   r11, {r0-r1}            ; Restore parameters
        BL      vector_call             ; Call the previous vector claimant
        B       exit$l                  ; Return to caller

        ; Reads entries from the current directory
csd_entries$l
        BL      stack_lock_prepare      ; Prepare stack for locked list
        MOV     r0, r2                  ; Copy pointer to buffer
        LDR     r1, = 257               ; Maximum size of each entry
        MLA     r1, r3, r1, r0          ; Calculate end address of buffer
        BL      stack_lock_region       ; Lock memory used by buffer
        BVS     exit$l                  ; Return with error
        LDMIA   r11, {r0-r1}            ; Restore parameters
        BL      vector_call             ; Call the previous vector claimant
        B       exit$l                  ; Return to caller

        ; Read entries and file information from a specified directory
dir_entries$l
        BL      stack_lock_prepare      ; Prepare stack for locked list
        MOV     r0, r1                  ; Copy pointer to directory name
        BL      stack_lock_string       ; Lock memory used by directory name
        BVS     exit$l                  ; Return with error
        MOV     r0, r2                  ; Copy pointer to buffer
        ADD     r1, r0, r5              ; Calculate end address of buffer
        BL      stack_lock_region       ; Lock memory used by buffer
        BVS     exit$l                  ; Return with error
        MOV     r0, r6                  ; Copy pointer to name to match
        BL      stack_lock_string       ; Lock memory used by name to match
        BVS     exit$l                  ; Return with error
        LDMIA   r11, {r0-r1}            ; Restore parameters
        BL      vector_call             ; Call the previous vector claimant

        ; Return to caller after unlocking pages
exit$l  BL      stack_unlock            ; Unlock pages in list on stack
        B       vector_return_c         ; Return to caller preserving carry

        ;   Parameters  : r0    - Reason code.
        ;                 Other registers depend on reason code.
        ;   Returns     : Depends on reason code.
        ;   Description : Handle the FindV vector, through which OS_Find is
        ;                 indirected.
findv_handler
        LocalLabels
        debug_record "findv_handler"
        STMFD   r13!, {r0-r12, r14}     ; Stack registers
        MOV     r11, r13                ; Copy stack pointer after dump
        TST     r0, #&FF                ; Test low byte of reason code
        BEQ     close$l                 ; Branch if reason code is close file
        BL      stack_lock_prepare      ; Prepare stack for locked list
        MOV     r0, r1                  ; Copy pointer to object name
        BL      stack_lock_string       ; Lock memory used by object name
        BVS     exit$l                  ; Return with error
        LDMIA   r11, {r0}               ; Restore reason code
        AND     r0, r0, #3              ; Get type of path
        TEQ     r0, #OSFind_Path        ; Is it a path string
        TEQNE   r0, #OSFind_PathVar     ; Is it a path variable
        BNE     skip$l                  ; Skip locking of path variable
        MOV     r0, r2                  ; Copy pointer to path string
        BL      stack_lock_string       ; Lock memory used by path string
        BVS     exit$l                  ; Return with error
skip$l  LDMIA   r11, {r0}               ; Restore reason code
        BL      vector_call             ; Call the previous vector claimant
exit$l  BL      stack_unlock            ; Unlock pages in list on stack
        B       vector_return           ; Return to caller
close$l TEQ     r1, #0                  ; Is it an attempt to close all files
        BEQ     all$l                   ; Reopen swap files after closing
        BL      swap$l                  ; Check if it is a swap file
        B       vector_pass_on          ; Pass on unmodified if not
all$l   BL      vector_call             ; Call the previous vector claimant
        BL      reopen_all_swap_files   ; Reopen the swap files
        B       vector_return           ; Return to caller

        ; Attempt to prevent closure of swap file
swap$l  JSR     "r0-r2"                 ; Stack registers
        LDR     r0, ws_virtual_areas    ; Get number of virtual dynamic areas
        TEQ     r0, #0                  ; Are there any virtual dynamic areas
        RTSS EQ                         ; Return if there are not any
        LDR     r0, ws_area_ptr         ; Get pointer to first area record
loop$l  TEQ     r0, #0                  ; Are there any more areas
        RTSS EQ                         ; Exit if not
        LDR     r2, [r0, #area_file_handle]; Get the file handle
        TEQ     r1, r2                  ; Do the file handles match
        LDRNE   r0, [r0, #area_next]    ; Get pointer to next area record
        BNE     loop$l                  ; Loop for next area
        BL      reopen_all_swap_files   ; Ensure that file is reopened
        RTSS EQ                         ; Return from subroutine

        ;   Parameters  : r0    - Reason code.
        ;                 Other registers depend on reason code.
        ;   Returns     : r0    - Preserved.
        ;                 Other registers depend on reason code.
        ;   Description : Handle the FSCV vector, through which OS_FSControl
        ;                 is indirected.
fscv_handler
        LocalLabels
        debug_record "fscv_handler"
        STMFD   r13!, {r0-r12, r14}     ; Stack registers
        MOV     r11, r13                ; Copy stack pointer after dump
        CMP     r0, #(jump_end$l - jump$l) >> 2; Is it in range
        ADDLO   pc, pc, r0, LSL#2       ; Dispatch if in range
        B       vector_pass_on          ; Unknown reason code
jump$l  B       misc_op$l               ; OSFSControl_Dir (0)
        B       misc_op$l               ; OSFSControl_Lib (1)
        B       vector_pass_on          ; OSFSControl_StartApplication (2)
        B       vector_pass_on          ; Reserved for internal use (3)
        B       vector_pass_on          ; OSFSControl_Run (4)
        B       misc_op$l               ; OSFSControl_Cat (5)
        B       misc_op$l               ; OSFSControl_Ex (6)
        B       misc_op$l               ; OSFSControl_LCat (7)
        B       misc_op$l               ; OSFSControl_LEx (8)
        B       misc_op$l               ; OSFSControl_Info (9)
        B       vector_pass_on          ; OSFSControl_Opt (10)
        B       misc_op$l               ; OSFSControl_SetTemporaryFS (11)
        B       vector_pass_on          ; OSFSControl_AddFS (12)
        B       fs_op$l                 ; OSFSControl_LookupFS (13)
        B       fs_op$l                 ; OSFSControl_SelectFS (14)
        B       vector_pass_on          ; OSFSControl_BootFromFS (15)
        B       misc_op$l               ; OSFSControl_RemoveFS (16)
        B       misc_op2$l              ; OSFSControl_AddSecondaryFS (17)
        B       vector_pass_on          ; OSFSControl_ReadFileType (18)
        B       vector_pass_on          ; OSFSControl_RestoreCurrent (19)
        B       vector_pass_on          ; OSFSControl_ReadModuleBase (20)
        B       vector_pass_on          ; OSFSControl_ReadFSHandle (21)
        B       close$l                 ; OSFSControl_Shut (22)
        B       close$l                 ; OSFSControl_Shutdown (23)
        B       misc_op2$l              ; OSFSControl_Access (24)
        B       misc_op2$l              ; OSFSControl_Rename (25)
        B       copy$l                  ; OSFSControl_Copy (26)
        B       misc_op$l               ; OSFSControl_Wipe (27)
        B       misc_op$l               ; OSFSControl_Count (28)
        B       vector_pass_on          ; Reserved for internal use (29)
        B       vector_pass_on          ; OSFSControl_ReadSecondaryFS (30)
        B       misc_op$l               ; OSFSControl_FileTypeFromString (31)
        B       misc_op$l               ; OSFSControl_FileInfo (32)
        B       misc_buffer$l           ; OSFSControl_ReadFSName (33)
        B       vector_pass_on          ; Reserved for future expansion (34)
        B       vector_pass_on          ; OSFSControl_RegisterImageFS (35)
        B       vector_pass_on          ; OSFSControl_DeregisterImageFS (36)
        B       canonicalise$l          ; OSFSControl_CanonicalisePath (37)
        B       misc_op$l               ; OSFSControl_InfoToFileType (38)
        B       misc_op$l               ; OSFSControl_URD (39)
        B       vector_pass_on          ; OSFSControl_Back (40)
        B       misc_buffer$l           ; OSFSControl_DefectList (41)
        B       misc_op$l               ; OSFSControl_AddDefect (42)
        B       vector_pass_on          ; OSFSControl_NoDir (43)
        B       vector_pass_on          ; OSFSControl_NoURD (44)
        B       vector_pass_on          ; OSFSControl_NoLib (45)
        B       misc_buffer$l           ; OSFSControl_UsedSpaceMap (46)
        B       misc_op$l               ; OSFSControl_ReadBootOption (47)
        B       misc_op$l               ; OSFSControl_WriteBootOption (48)
        B       misc_op$l               ; OSFSControl_FreeSpace (49)
        B       misc_op2$l              ; OSFSControl_NameDisc (50)
        B       misc_op$l               ; OSFSControl_StampImage (51)
        B       find$l                  ; OSFSControl_ObjectAtOffset (52)
        B       set_dir$l               ; OSFSControl_SetDir (53)
        B       read_dir$l              ; OSFSControl_ReadDir (54)
        B       misc_op$l               ; OSFSControl_FreeSpace64 (55)
        B       misc_buffer$l           ; OSFSControl_DefectList64 (56)
        B       misc_op$l               ; OSFSControl_AddDefect64 (57)
jump_end$l

        ; Perform a miscellaneous operation with a single string
misc_op$l
        BL      stack_lock_prepare      ; Prepare stack for locked list
        MOV     r0, r1                  ; Copy pointer to string
        BL      stack_lock_string       ; Lock memory used by string
        BVS     exit$l                  ; Return with error
        LDMIA   r11, {r0}               ; Restore parameters
        BL      vector_call             ; Call the previous vector claimant
        B       exit$l                  ; Return to caller

        ; Perform a miscellaneous operation with two strings
misc_op2$l
        BL      stack_lock_prepare      ; Prepare stack for locked list
        MOV     r0, r1                  ; Copy pointer to first string
        BL      stack_lock_string       ; Lock memory used by first string
        BVS     exit$l                  ; Return with error
        MOV     r0, r2                  ; Copy pointer to second string
        BL      stack_lock_string       ; Lock memory used by second string
        BVS     exit$l                  ; Return with error
        LDMIA   r11, {r0}               ; Restore parameters
        BL      vector_call             ; Call the previous vector claimant
        B       exit$l                  ; Return to caller

        ; Perform a miscellaneous operation with a string and a buffer
misc_buffer$l
        BL      stack_lock_prepare      ; Prepare stack for locked list
        MOV     r0, r1                  ; Copy pointer to first string
        BL      stack_lock_string       ; Lock memory used by first string
        BVS     exit$l                  ; Return with error
        MOV     r0, r2                  ; Copy pointer to buffer
        ADD     r1, r0, r3              ; Calculate end address of buffer
        BL      stack_lock_region       ; Lock memory used by buffer
        BVS     exit$l                  ; Return with error
        LDMIA   r11, {r0, r1}           ; Restore parameters
        BL      vector_call             ; Call the previous vector claimant
        B       exit$l                  ; Return to caller

        ; Perform an operation with either a file system number of name
fs_op$l BL      stack_lock_prepare      ; Prepare stack for locked list
        MOV     r0, r1                  ; Copy pointer to file system name
        CMP     r0, #&100               ; Is it a number or pointer
        BLGE    stack_lock_string       ; Lock memory used by name
        BVS     exit$l                  ; Return with error
        LDMIA   r11, {r0}               ; Restore parameters
        BL      vector_call             ; Call the previous vector claimant
        B       exit$l                  ; Return to caller

        ; Handle OSFSControl_Copy
copy$l  BL      stack_lock_prepare      ; Prepare stack for locked list
        MOV     r0, r1                  ; Copy pointer to source
        BL      stack_lock_string       ; Lock memory used by source
        BVS     exit$l                  ; Return with error
        MOV     r0, r2                  ; Copy pointer to destination
        BL      stack_lock_string       ; Lock memory used by destination
        BVS     exit$l                  ; Return with error
        TST     r3, #OSFSControl_CopyGivenDescriptor; Is buffer provided
        BEQ     copy_no_buffer$l        ; Skip next bit if no buffer
        LDR     r0, [r8, #OSFSControl_Descriptor_addr]; Get buffert start
        LDR     r1, [r8, #OSFSControl_Descriptor_size]; Get buffer start
        ADD     r1, r0, r1              ; Calculate end address of buffer
        BL      stack_lock_region       ; Lock memory used by buffer
        BVS     exit$l                  ; Return with error
copy_no_buffer$l
        LDMIA   r11, {r0, r1}           ; Restore parameters
        BL      vector_call             ; Call the previous vector claimant
        B       exit$l                  ; Return to caller

        ; Handle OSFSControl_CanonicalisePath
canonicalise$l
        BL      stack_lock_prepare      ; Prepare stack for locked list
        MOV     r0, r1                  ; Copy pointer to pathname
        BL      stack_lock_string       ; Lock memory used by pathname
        BVS     exit$l                  ; Return with error
        MOV     r0, r3                  ; Copy pointer to path variable
        BL      stack_lock_string       ; Lock memory used by path variable
        BVS     exit$l                  ; Return with error
        MOV     r0, r4                  ; Copy pointer to path string
        BL      stack_lock_string       ; Lock memory used by path string
        BVS     exit$l                  ; Return with error
        MOV     r0, r2                  ; Copy pointer to buffer
        ADD     r1, r0, r5              ; Calculate end address of buffer
        BL      stack_lock_region       ; Lock memory used by buffer
        BVS     exit$l                  ; Return with error
        LDMIA   r11, {r0, r1}           ; Restore parameters
        BL      vector_call             ; Call the previous vector claimant
        B       exit$l                  ; Return to caller

        ; Handle OSFSControl_ObjectAtOffset
find$l  BL      stack_lock_prepare      ; Prepare stack for locked list
        MOV     r0, r1                  ; Copy pointer to object name
        BL      stack_lock_string       ; Lock memory used by object name
        BVS     exit$l                  ; Return with error
        MOV     r0, r3                  ; Copy pointer to buffer
        ADD     r1, r0, r4              ; Calculate end address of buffer
        BL      stack_lock_region       ; Lock memory used by buffer
        BVS     exit$l                  ; Return with error
        LDMIA   r11, {r0, r1}           ; Restore parameters
        BL      vector_call             ; Call the previous vector claimant
        B       exit$l                  ; Return to caller

        ; Handle OSFSControl_SetDir
set_dir$l
        BL      stack_lock_prepare      ; Prepare stack for locked list
        MOV     r0, r1                  ; Copy pointer to rest of path
        BL      stack_lock_string       ; Lock memory used by rest of path
        BVS     exit$l                  ; Return with error
        MOV     r0, r2                  ; Copy pointer to directory
        BL      stack_lock_string       ; Lock memory used by directory
        BVS     exit$l                  ; Return with error
        MOV     r0, r3                  ; Copy pointer to file system
        BL      stack_lock_string       ; Lock memory used by file system
        BVS     exit$l                  ; Return with error
        MOV     r0, r6                  ; Copy pointer to special field
        BL      stack_lock_string       ; Lock memory used by special field
        BVS     exit$l                  ; Return with error
        LDMIA   r11, {r0}               ; Restore parameters
        BL      vector_call             ; Call the previous vector claimant
        B       exit$l                  ; Return to caller

        ; Handle OSFSControl_ReadDir
read_dir$l
        BL      stack_lock_prepare      ; Prepare stack for locked list
        MOV     r0, r2                  ; Copy pointer to directory
        BL      stack_lock_string       ; Lock memory used by directory
        BVS     exit$l                  ; Return with error
        MOV     r0, r3                  ; Copy pointer to file system
        BL      stack_lock_string       ; Lock memory used by file system
        BVS     exit$l                  ; Return with error
        MOV     r0, r1                  ; Copy pointer to buffer
        ADD     r1, r0, r5              ; Calculate end address of buffer
        BL      stack_lock_region       ; Lock memory used by buffer
        BVS     exit$l                  ; Return with error
        LDMIA   r11, {r0, r1}           ; Restore parameters
        BL      vector_call             ; Call the previous vector claimant
        B       exit$l                  ; Return to caller

        ; Handle OSFSControl_Shut and OSFSControl_Shutdown
close$l BL      vector_call             ; Call the previous vector claimant
        BL      reopen_all_swap_files   ; Reopen all of the swap files
        B       vector_return           ; Return to caller

        ; Return from any of the intercepts, unlocking pages
exit$l  BL      stack_unlock            ; Unlock pages in list on stack
        B       vector_return           ; Return to caller

        ;   Parameters  : r0-r9     - Parameters to pass on.
        ;                 r11       - Stack pointer after register dump.
        ;   Returns     : r0-r9     - Return values.
        ;                 r10-r12   - Preserved.
        ;   Description : Call the previous claimant of the vector.
vector_call
        LocalLabels
        JSR     "r10-r12"               ; Stack local registers
        debug_record "vector_call"
        ADD     r11, r11, #4 * 10       ; Skip original parameters on stack
        STMFD   r13!, {pc}              ; Stack return address
        LDMIA   r11, {r10-r12, pc}^     ; Pass on, restoring vector registers
        NOP
        RTS                             ; Restore local registers

        ;   Parameters  : r0-r9 - Parameters to pass on.
        ;                 r11   - Stack pointer after register dump.
        ;   Returns     : Does not return.
        ;   Description : Call the previous claimant of the vector, and return
        ;                 directly to calling routine.
vector_pass_on
        LocalLabels
        debug_record "vector_pass_on"
        ADD     r13, r11, #4 * 10       ; Skip original parameters on stack
        LDMFD   r13!, {r10-r12, pc}     ; Pass on, restoring vector registers

        ;   Parameters  : r0-r9 - Return values.
        ;                 r11   - Stack pointer after register dump.
        ;   Returns     : Does not return.
        ;   Description : Return to calling routine, restoring original flags.
vector_return
        LocalLabels
        debug_record "vector_return"
        ADD     r13, r11, #4 * 10       ; Skip original parameters on stack
        LDR     r11, [r13, #4 * 4]      ; Get return address and flags
        BICVC   r11, r11, #V            ; Clear error state in return address
        ORRVS   r11, r11, #V            ; Set error state in return address
        STR     r11, [r13, #4 * 4]      ; Store return address and flags
        LDMFD   r13!, {r10-r12, r14, pc}^; Return to caller, keeping results

        ;   Parameters  : r0-r9 - Return values.
        ;                 r11   - Stack pointer after register dump.
        ;   Returns     : Does not return.
        ;   Description : Return to calling routine, restoring original flags
        ;                 except for C.
vector_return_c
        LocalLabels
        debug_record "vector_return_c"
        ADD     r13, r11, #4 * 10       ; Skip original parameters on stack
        LDR     r11, [r13, #4 * 4]      ; Get return address and flags
        BIC     r11, r11, #V + C        ; Clear return V and C flags
        ORRVS   r11, r11, #V            ; Set error state in return address
        ORRCS   r11, r11, #C            ; Set carry state in return address
        STR     r11, [r13, #4 * 4]      ; Store return address and flags
        LDMFD   r13!, {r10-r12, r14, pc}^; Return to caller, keeping results

        ;   Parameters  : None
        ;   Returns     : None
        ;   Description : Prepare the stack for a list of locked pages. Data
        ;                 already on the stack should not be accessed through
        ;                 r13 until stack_unlock has been called.
stack_lock_prepare
        LocalLabels
        SUB     r13, r13, #4            ; Reserve space on stack
        JSR     "r0"                    ; Stack registers
        MOV     r0, #0                  ; End of list marker
        STR     r0, [r13, #4 * 2]       ; Store end of list marker
        RTSS                            ; Return from subroutine

        ;   Parameters  : None
        ;   Returns     : None
        ;   Description : Unlock any pages that are listed on the stack, and
        ;                 restore r13 to its position before stack_lock_prepare
        ;                 was called.
stack_unlock
        LocalLabels
        JSR     "r0-r2, r10, r13"       ; Stack registers
        ADD     r10, r13, #4 * 6        ; Pointer to locked list on stack
loop$l  LDMFD   r10!, {r0}              ; Get the area pointer
        TEQ     r0, #0                  ; Is it the end of the list
        BEQ     done$l                  ; Exit the loop if it is
        LDMFD   r10!, {r1-r2}           ; Get the page range
        BL      unlock_pages            ; Unlock the pages
        B       loop$l                  ; Loop for the next page
done$l  STR     r10, [r13, #4 * 4]      ; Store new stack pointer
        RTSS                            ; Return from subroutine

        ;   Parameters  : r0    - Pointer to string to attempt to lock.
        ;   Returns     : r0    - Preserved.
        ;   Description : Lock the memory used by the specified string, and
        ;                 store the information on the stack.
stack_lock_string
        LocalLabels
        TEQ     r0, #0                  ; Is it a valid pointer
        MOVEQS  pc, r14                 ; Return if not
        SUB     r13, r13, #4 * 3        ; Reserve space on the stack
        STMFD   r13!, {r1, r10, r14}    ; Stack registers
        ADD     r10, r13, #4 * 6        ; Pointer to lock storage
        ADD     r1, r0, #256            ; End address is after maximum string
        BL      add$l                   ; Lock as much as possible of string
        LDMFD   r13!, {r1, r10, r14}    ; Restore registers
        B       exit$l                  ; Exit from subroutine

        ;   Parameters  : r0    - The start address of the area to lock.
        ;                 r1    - The end address of the area to lock.
        ;   Returns     : None
        ;   Description : Lock all of the specified address range that lies
        ;                 within virtual memory, and store the information on
        ;                 the stack.
stack_lock_region
        SUB     r13, r13, #4 * 3        ; Reserve space on the stack
        STMFD   r13!, {r10, r14}        ; Stack registers
        ADD     r10, r13, #4 * 5        ; Pointer to lock storage
        BL      add$l                   ; Lock as much as possible of region
        LDMFD   r13!, {r10, r14}        ; Restore registers
        B       exit$l                  ; Exit from subroutine

        ;   Parameters  : r0    - The start address of the area to lock.
        ;                 r1    - The end address of the area to lock.
        ;   Returns     : r0    - The first address after the region locked.
        ;   Description : Lock an intial segment of the specified address
        ;                 range if it lies within virtual memory, and store
        ;                 the information on the stack.
stack_lock_initial
        SUB     r13, r13, #4 * 3        ; Reserve space on the stack
        STMFD   r13!, {r1-r3, r10, r14} ; Stack registers
        ADD     r10, r13, #4 * 8        ; Pointer to lock storage
        SUB     r2, r1, #1              ; Make end address inclusive
        MOV     r1, r0                  ; Copy start address
        BL      find_part               ; Find which virtual area is used
        STMDB   r10, {r0-r2}            ; Store details of region to lock
        TEQ     r0, #0                  ; Is there any overlapping region
        LDMEQFD r13, {r0}               ; Use original end address if not
        BEQ     stack_lock_initial_exit$l; Exit if no overlapping region
        SUB     r2, r2, r1              ; Calculate maximum number of pages
        ADD     r2, r2, #1              ; Correct count (pages are inclusive)
        LDR     r0, ws_locked_pages     ; Get number of locked pages
        LDR     r1, ws_target_pages     ; The number of physical pages to use
        SUB     r0, r1, r0              ; Number of available pages
        SUB     r0, r0, #min_spare_pages; Remember to leave some spares
        CMP     r0, #1                  ; Check that there are some pages
        MOVLT   r0, #1                  ; Minimum of 1 page
        CMP     r0, r2                  ; Compare to required number
        MOVLT   r2, r0                  ; Take smallest of the two values
        SUB     r3, r2, #1              ; Reverse count correction
        LDMDB   r10, {r0-r2}            ; Restore page numbers
        ADD     r2, r1, r3              ; Calculate end page number
        STMDB   r10, {r0-r2}            ; Store details of region to lock
        BL      lock_pages              ; Attempt to lock the pages
        BVC     stack_lock_initial_done$l; Skip retry if successful
        LDMDB   r10, {r0-r2}            ; Restore page numbers
        MOV     r2, r1                  ; Only lock one page to be safe
        STMDB   r10, {r0-r2}            ; Store details of region to lock
        BL      lock_pages              ; Attempt to lock the pages
        BVS     stack_lock_initial_exit$l; Abandon attempt if error
stack_lock_initial_done$l
        ADD     r2, r2, #1              ; Make end page exclusive
        LDR     r0, [r0, #area_base]    ; Base address of dynamic area
        LDR     r1, ws_log_page_size    ; Log base 2 of page size
        ADD     r0, r0, r2, LSL r1      ; Calculate end address
stack_lock_initial_exit$l
        LDMFD   r13!, {r1-r3, r10, r14} ; Restore registers
        BVS     exit$l                  ; Exit if error produced
        CMP     r0, r1                  ; Compare new end against parameter
        MOVHI   r0, r1                  ; Clip address against original
        B       exit$l                  ; Exit from subroutine

        ; Find a region, add it to the stack, and lock it
add$l   JSR     "r0-r2"                 ; Stack registers
        SUB     r2, r1, #1              ; Make end address inclusive
        MOV     r1, r0                  ; Copy start address
        BL      find_part               ; Find which virtual area is used
        STMDB   r10, {r0-r2}            ; Store details of region to lock
        TEQ     r0, #0                  ; Is there any overlapping region
        RTS EQ                          ; Exit if no overlapping region
        BL      lock_pages              ; Lock required pages
        RTE VS                          ; Exit if error produced
        RTS                             ; Return from subroutine

        ; Exit from a locking routine, checking if stacked values are sensible
exit$l  BVS     fail$l                  ; If error then branch to error exit
        STMFD   r13!, {r0}              ; Stack register
        LDR     r0, [r13, #4]           ; Get stacked area pointer
        TEQ     r0, #0                  ; Is pointer valid
        LDMFD   r13!, {r0}              ; Restore register
        MOVNE   pc, r14                 ; Return if valid
fail$l  ADD     r13, r13, #4 * 3        ; Skip reserved space on stack
        MOV     pc, r14                 ; Return with error

; A literal pool

        LTORG

; RISC OS SWI intercepts

        ;   Parameters  : None
        ;   Returns     : None
        ;   Description : Intercept OS_DynamicArea and OS_ChangeDynamicArea
        ;                 SWIs.
claim_swis
        LocalLabels
        JSR     "r0-r2"                 ; Stack registers
        LDR     r0, ws_os_swi_table     ; Get pointer to RISC OS SWI table
        ADD     r1, r0, #OS_DynamicArea << 2; Pointer to OS_DynamicArea
        LDR     r2, [r1]                ; Get previous OS_DynamicArea
        STR     r2, ws_prv_dynamic_area ; Store pointer to previous handler
        ADR     r2, os_dynamic_area_handler; Pointer to replacement handler
        STR     r2, [r1]                ; Perform the intercept
        ADD     r1, r0, #OS_ChangeDynamicArea << 2; Pointer OS_ChangeDynamicArea
        LDR     r2, [r1]                ; Get previous OS_ChangeDynamicArea
        STR     r2, ws_prv_change_area  ; Store pointer to previous handler
        ADRL    r2, os_change_dynamic_area_handler; Pointer to replacement
        STR     r2, [r1]                ; Perform the intercept
        ADD     r1, r0, #OS_ReadDynamicArea << 2; Pointer OS_ReadDynamicArea
        LDR     r2, [r1]                ; Get previous OS_ChangeDynamicArea
        STR     r2, ws_prv_read_area    ; Store pointer to previous handler
        ADRL    r2, os_read_dynamic_area_handler; Pointer to replacement
        STR     r2, [r1]                ; Perform the intercept
        ADD     r1, r0, #OS_Heap << 2   ; Pointer OS_Heap handler
        LDR     r2, [r1]                ; Get previous OS_Heap handler
        STR     r2, ws_prv_heap         ; Store pointer to previous handler
        ADRL    r2, os_heap_handler     ; Pointer to replacement
        STR     r2, [r1]                ; Perform the intercept
        MOV     r0, #0                  ; Initial value for semaphores
        STR     r0, ws_sma_dynamic_area ; Clear OS_DynamicArea semaphore
        STR     r0, ws_sma_change_area  ; Clear OS_ChangeDynamicArea semaphore
        STR     r0, ws_sma_read_area    ; Clear OS_ReadDynamicArea semaphore
        RTSS                            ; Return from subroutine

        ;   Parameters  : None
        ;   Returns     : None
        ;   Description : Restore OS_DynamicArea and OS_ChangeDynamicArea SWIs.
release_check_swis
        LocalLabels
        JSR     "r0-r3"                 ; Stack registers
        LDR     r0, ws_os_swi_table     ; Get pointer to RISC OS SWI table
        ADD     r1, r0, #OS_DynamicArea << 2; Pointer to OS_DynamicArea
        LDR     r2, [r1]                ; Get current OS_DynamicArea
        ADR     r3, os_dynamic_area_handler; Pointer to replacement handler
        TEQ     r2, r3                  ; Check whether the handler replaced
        BNE     fail$l                  ; Unable to die if it has
        ADD     r1, r0, #OS_ChangeDynamicArea << 2; Pointer OS_ChangeDynamicArea
        LDR     r2, [r1]                ; Get current OS_ChangeDynamicArea
        ADRL    r3, os_change_dynamic_area_handler; Pointer to replacement
        TEQ     r2, r3                  ; Check whether the handler replaced
        BNE     fail$l                  ; Unable to die if it has
        ADD     r1, r0, #OS_ReadDynamicArea << 2; Pointer OS_ReadDynamicArea
        LDR     r2, [r1]                ; Get current OS_ChangeDynamicArea
        ADRL    r3, os_read_dynamic_area_handler; Pointer to replacement
        TEQ     r2, r3                  ; Check whether the handler replaced
        BNE     fail$l                  ; Unable to die if it has
        ADD     r1, r0, #OS_Heap << 2   ; Pointer OS_Heap
        LDR     r2, [r1]                ; Get current OS_Heap handler
        ADRL    r3, os_heap_handler     ; Pointer to replacement handler
        TEQ     r2, r3                  ; Check whether the handler replaced
        BNE     fail$l                  ; Unable to die if it has
        RTSS                            ; Return from subroutine
fail$l  ADRL    r0, err_qsm             ; Pointer to error block
        ADR     r1, ws_message          ; Pointer to messages control block
        MOV     r2, #0                  ; Use internal buffer
        SWI     XMessageTrans_ErrorLookup; Lookup the error text
        RTE                             ; Return from subroutine

        ;   Parameters  : None
        ;   Returns     : None
        ;   Description : Restore OS_DynamicArea and OS_ChangeDynamicArea SWIs.
release_swis
        LocalLabels
        JSR     "r0-r2"                 ; Stack registers
        LDR     r0, ws_os_swi_table     ; Get pointer to RISC OS SWI table
        ADD     r1, r0, #OS_DynamicArea << 2; Pointer to OS_DynamicArea
        LDR     r2, ws_prv_dynamic_area ; Previous OS_DynamicArea handler
        STR     r2, [r1]                ; Restore previous handler
        ADD     r1, r0, #OS_ChangeDynamicArea << 2; Pointer OS_ChangeDynamicArea
        LDR     r2, ws_prv_change_area  ; Previous OS_ChangeDynamicArea handler
        STR     r2, [r1]                ; Restore previous handler
        ADD     r1, r0, #OS_ReadDynamicArea << 2; Pointer OS_ReadDynamicArea
        LDR     r2, ws_prv_read_area    ; Previous OS_ReadDynamicArea handler
        STR     r2, [r1]                ; Restore previous handler
        ADD     r1, r0, #OS_Heap << 2   ; Pointer OS_Heap
        LDR     r2, ws_prv_heap         ; Previous OS_Heap handler
        STR     r2, [r1]                ; Restore previous handler
        RTSS                            ; Return from subroutine

        ;   Parameters  : r0    - Reason code.
        ;                 r1    - Pointer to heap.
        ;                 r2    - Pointer to block if relevant.
        ;                 r3    - Depends upon reason code.
        ;   Returns     : Depends upon reason code.
        ;   Description : Replacement OS_Heap handler.
os_heap_handler
        LocalLabels
;        debug_record "os_heap_handler"
        STMFD   r13!, {r11-r12, pc}     ; Stack registers
        ADRL    r12, workspace_ptr      ; Pointer to workspace pointer
        LDR     r12, [r12]              ; Get pointer to module workspace
        LDR     r11, ws_virtual_areas   ; Get number of active areas
        TEQ     r11, #0                 ; Test if there are any active areas
        TEQNEP  pc, #I + 3              ; Disable interrupts if any active
        LDR     r11, ws_prv_heap        ; Pointer to the original routine
        STR     r11, [r13, #8]          ; Store pointer to original routine
        LDMFD   r13!, {r11-r12, pc}     ; Jump to the original routine

        ;   Parameters  : r0    - Reason code.
        ;                 Other registers depend upon the reason code.
        ;   Returns     : r0    - Preserved.
        ;                 Other registers depend upon the reason code.
        ;   Description : Call the previous OS_DynamicArea handler.
os_dynamic_area
        LocalLabels
        JSR     "r10"                   ; Stack registers
        LDR     r10, ws_sma_dynamic_area; Get OS_DynamicArea semaphore
        ADD     r10, r10, #1            ; Increment semaphore
        STR     r10, ws_sma_dynamic_area; Store OS_DynamicArea semaphore
        SWI     XOS_DynamicArea         ; Call the SWI
        LDR     r10, ws_sma_dynamic_area; Get OS_DynamicArea semaphore
        SUB     r10, r10, #1            ; Decrement semaphore
        STR     r10, ws_sma_dynamic_area; Store OS_DynamicArea semaphore
        RTS                             ; Return from subroutine

        ;   Parameters  : r0    - Number of dynamic area to alter.
        ;                 r1    - Amount to move in bytes (signed integer).
        ;   Returns     : r0    - Preserved.
        ;                 r1    - Number of bytes moved (unsigned integer).
        ;   Description : Call the previous OS_ChangeDynamicArea handler.
os_change_dynamic_area
        LocalLabels
        JSR     "r10"                   ; Stack registers
        LDR     r10, ws_sma_change_area ; Get OS_ChangeDynamicArea semaphore
        ADD     r10, r10, #1            ; Increment semaphore
        STR     r10, ws_sma_change_area ; Store OS_ChangeDynamicArea semaphore
        SWI     XOS_ChangeDynamicArea   ; Call the SWI
        LDR     r10, ws_sma_change_area ; Get OS_ChangeDynamicArea semaphore
        SUB     r10, r10, #1            ; Decrement semaphore
        STR     r10, ws_sma_change_area ; Store OS_ChangeDynamicArea semaphore
        RTS                             ; Return from subroutine

        ;   Parameters  : r0    - Number of dynamic area to alter.
        ;   Returns     : r0    - Pointer to start of area.
        ;                 r1    - Current number of bytes in area.
        ;                 r2    - Maximum size of area, if bit 7 of r0 was
        ;                         set on entry; preserved otherwise.
        ;   Description : Call the previous OS_ChangeDynamicArea handler.
os_read_dynamic_area
        LocalLabels
        JSR     "r10"                   ; Stack registers
        LDR     r10, ws_sma_read_area   ; Get OS_ReadDynamicArea semaphore
        ADD     r10, r10, #1            ; Increment semaphore
        STR     r10, ws_sma_read_area   ; Store OS_ReadDynamicArea semaphore
        SWI     XOS_ReadDynamicArea     ; Call the SWI
        LDR     r10, ws_sma_read_area   ; Get OS_ReadDynamicArea semaphore
        SUB     r10, r10, #1            ; Decrement semaphore
        STR     r10, ws_sma_read_area   ; Store OS_ReadDynamicArea semaphore
        RTS                             ; Return from subroutine

        ;   Parameters  : r0    - Reason code.
        ;                 Other registers depend upon the reason code.
        ;   Returns     : r0    - Preserved.
        ;                 Other registers depend upon the reason code.
        ;   Description : Replacement OS_DynamicArea handler.
os_dynamic_area_handler
        STMFD   r13!, {r0}              ; Stack registers
;        debug_record "os_dynamic_area_handler"
        ADRL    r12, workspace_ptr      ; Pointer to workspace pointer
        LDR     r12, [r12]              ; Get pointer to module workspace
        LDR     r0, ws_sma_dynamic_area ; Get OS_DynamicArea semaphore
        TEQ     r0, #0                  ; Is semaphore set
        LDMFD   r13!, {r0}              ; Restore registers
        LDRNE   pc, ws_prv_dynamic_area ; Jump to the original routine
        CMP     r0, #(jump_end$l - jump$l) >> 2; Is it in range
        ADDLO   pc, pc, r0, LSL#2       ; Dispatch if in range
        LDR     pc, ws_prv_dynamic_area ; Unknown reason code
jump$l  B       create$l                ; OSDynamicArea_Create (0)
        B       remove$l                ; OSDynamicArea_Delete (1)
        B       read$l                  ; OSDynamicArea_Read (2)
        LDR     pc, ws_prv_dynamic_area ; OSDynamicArea_Enumerate (3)
        LDR     pc, ws_prv_dynamic_area ; OSDynamicArea_Renumber (4)
        LDR     pc, ws_prv_dynamic_area ; OSDynamicArea_TotalFree (5)
        LDR     pc, ws_prv_dynamic_area ; OSDynamicArea_Reserved1 (6)
        LDR     pc, ws_prv_dynamic_area ; OSDynamicArea_Reserved2 (7)
        B       clamp$l                 ; OSDynamicArea_ClampSize (8)
        LDR     pc, ws_prv_dynamic_area ; OSDynamicArea_SparseAlloc (9)
        LDR     pc, ws_prv_dynamic_area ; OSDynamicArea_SparseFree (10)
jump_end$l

        ; Clamp the size of future dynamic areas
clamp$l STMFD   r13!, {r0-r2, r14}      ; Stack registers
        MOV     r0, r1                  ; Copy size limit
        MOV     r1, #-1                 ; Do not change memory to auto claim
        MOV     r2, #-1                 ; Do not change memory to leave free
        BL      configure               ; Configure the internal limit
        LDMFD   r13!, {r0-r2, r14}      ; Restore registers
        LDRNE   pc, ws_prv_dynamic_area ; Pass on to original handler

        ; Remove a dynamic area
remove$l
        STMFD   r13!, {r2}              ; Stack parameters
;        debug_record "os_dynamic_area_handler: remove"
        LDR     r2, ws_dynamic_area     ; Get the physical dynamic area number
        TEQ     r1, r2                  ; Does the area number match
        LDMFD   r13!, {r2}              ; Restore register
        LDRNE   pc, ws_prv_dynamic_area ; Pass on to original handler
        STMFD   r13!, {r1-r3, r14}      ; Stack parameters and return address
        ADRL    r0, err_rrv             ; Pointer to error block
        ADR     r1, ws_message          ; Pointer to messages control block
        MOV     r2, #0                  ; Use internal buffer
        SWI     XMessageTrans_ErrorLookup; Lookup the error text
        MOV     r3, #0                  ; Ensure no memory moved
        LDMFD   r13!, {r1-r3, r14}      ; Restore parameters
        B       exit$l                  ; Return to caller with the error

        ; Create a new dynamic area
create$l
        STMFD   r13!, {r14}             ; Stack return address
;        debug_record "os_dynamic_area_handler: create"
        BL      auto$l                  ; Check if auto VM should be used
        [       :LNOT: :DEF: demo
        BL      pre_add_intercept       ; Prepare to intercept area handler
        ]
        BL      form$l                  ; Use OS to actually create the area
        [       :LNOT: :DEF: demo
        BLVC    post_add_intercept      ; Construct the intercept details
        ]
        LDMFD   r13!, {r14}             ; Restore return address
        BVS     exit$l                  ; Exit if error produced
        TST     r4, #area_flag_vm       ; Is the virtual memory flag set
        BEQ     exit$l                  ; Exit normally if not
        STMFD   r13!, {r0-r2, r14}      ; Stack registers
        MOV     r0, r1                  ; Copy dynamic area number
        MOV     r1, #-1                 ; Use allocated maximum size
        MOV     r2, #0                  ; Use automatic swap file name
        BL      start_virtualise        ; Start virtual memory
        ClrV                            ; Ignore any error produced
        LDMFD   r13!, {r0-r2, r14}      ; Restore rgisters
        STMFD   r13!, {r14}             ; Stack return address
        BL      grow$l                  ; Grow area to required size
        LDMFD   r13!, {r14}             ; Restore return address
        BVC     exit$l                  ; Return to caller if successful
        STMFD   r13!, {r0, r14}         ; Stack registers
        MOV     r0, r1                  ; Copy dynamic area number
        BL      end_virtualise          ; End virtual memory
        BL      grow$l                  ; Grow area to required size
        LDMFD   r13!, {r0, r14}         ; Restore registers
        BVC     exit$l                  ; Return to caller if successful
        STMFD   r13!, {r0, r14}         ; Stack registers
        MOV     r0, #OSDynamicArea_Delete; Reason code to delete dynamic area
        BL      os_dynamic_area         ; Create the dynamic area
        LDMFD   r13!, {r0, r14}         ; Restore registers
        SetV                            ; Ensure error flag set
        B       exit$l                  ; Return to caller with error

; A literal pool

        LTORG

        [       :DEF: demo
        ; Check if this is one of the demo dynamic areas
auto$l  JSR     "r1"                    ; Stack registers
        TST     r4, #area_flag_vm       ; Is virtual memory explicit
        RTSS EQ                         ; Return if not virtual memory
        name_check r8, r1, demo_area1, demo$l; Check possible dynamic area name
        name_check r8, r1, demo_area2, demo$l; Check possible dynamic area name
        name_check r8, r1, demo_area3, demo$l; Check possible dynamic area name
        name_check r8, r1, demo_area4, demo$l; Check possible dynamic area name
        name_check r8, r1, demo_area5, demo$l; Check possible dynamic area name
        name_check r8, r1, demo_area6, demo$l; Check possible dynamic area name
        name_check r8, r1, demo_area7, demo$l; Check possible dynamic area name
        name_check r8, r1, demo_area8, demo$l; Check possible dynamic area name
        BIC     r4, r4, #area_flag_vm   ; Clear the virtual memory flag
demo$l  RTSS                            ; Return from subroutine
        |
        ; Check if can automatically use virtual memory
auto$l  JSR     "r0-r4"                 ; Stack registers
        TST     r4, #area_flag_vm       ; Is virtual memory explicit
        RTS NE                          ; Return if already virtual memory
        ADRL    r0, ws_string           ; Pointer to variable name buffer
        ADR     r1, var$l               ; Pointer to variable prefix
copy1$l LDRB    r2, [r1], #1            ; Read a character of variable name
        STRB    r2, [r0], #1            ; Write character of variable name
        TEQ     r2, #0                  ; Was it the terminator
        BNE     copy1$l                 ; Loop if not the end
        SUB     r0, r0, #1              ; Place pointer at terminator
        MOV     r1, r8                  ; Copy pointer to area name
copy2$l LDRB    r2, [r1], #1            ; Read character from area name
        TEQ     r2, #'#'                ; Is it a '#' character
        TEQNE   r2, #'*'                ; Is it a '*' character
        TEQNE   r2, #' '                ; Is it a space character
        MOVEQ   r2, #'_'                ; Replace awkward characters with '_'
        CMP     r2, #31                 ; Is it a control character
        MOVLE   r2, #0                  ; Use 0 as terminator
        STRB    r2, [r0], #1            ; Store character
        TEQ     r2, #0                  ; Was it the terminator
        BNE     copy2$l                 ; Loop if not finished
        ADRL    r0, ws_string           ; Pointer to variable name buffer
        ADRL    r1, ws_num_buffer1      ; Pointer to result buffer
        MOV     r2, #Int                ; Length of buffer for integer
        MOV     r3, #0                  ; Initial context pointer
        MOV     r4, #0                  ; Do not want expanded string
        SWI     XOS_ReadVarVal          ; Try to read the variable
        RTSS VS                         ; Return if variable not found
        pipe_string "after OS_ReadVarVal type = %0", r4
        TEQ     r4, #OS_VartypeNumber   ; Was a number returned
        RTSS NE                         ; Return if not numeric
        PULL                            ; Restore registers
        CMP     r5, #-1                 ; Only replace limit if maximum size
        LDREQ   r5, ws_num_buffer1      ; Get specified maximum size
        MOVEQ   r5, r5, LSL#20          ; Convert to megabytes
        ORR     r4, r4, #area_flag_vm   ; Set the virtual memory flag
        MOVS    pc, r14                 ; Return from subroutine

        ; System variable prefix for automatic virtual memory
var$l   =       "Virtualise$Area_", 0
        ALIGN
        ]

; A literal pool

        LTORG

        ; Create a new dynamic area
form$l  JSR     "r2, r4, r9-r10"        ; Stack registers
        LDR     r9, ws_os_memory_size   ; Get pointer to OS workspace
        LDR     r10, = max_area_size    ; Upper limit to logical address space
        CMP     r5, #-1                 ; Was a maximum size specified
        LDREQ   r10, ws_os_memory_limit ; Use configured memory size if not
        STR     r10, [r9]               ; Overwrite memory size in OS workspace
        TST     r4, #area_flag_vm       ; Will virtual memory be used now
        MOVNE   r2, #0                  ; Do not allocate memory if it will
        BIC     r4, r4, #area_flag_vm   ; Do not let OS have extra flag bit
        BL      os_dynamic_area         ; Create the dynamic area
        LDR     r9, ws_os_memory_size   ; Get pointer to OS workspace
        LDR     r10, ws_os_memory_init  ; Get initial memory size
        STR     r10, [r9]               ; Restore memory size
        RTS                             ; Return from subroutine

        ; Grow a possibly virtual memory dynamic area to required size
grow$l  JSR     "r0-r2"                 ; Stack registers
        MOV     r0, r1                  ; Copy dynamic area number
        MOV     r1, r2                  ; Copy required size
        BL      change_virtualise       ; Grow the area (real or virtual)
        RTE VS                          ; Return any error produced
        RTS                             ; Return from subroutine

        ; Returns information on a dynamic area
read$l  STMFD   r13!, {r14}             ; Stack return address
        BL      os_dynamic_area         ; Obtain standard information from OS
        LDMFD   r13!, {r14}             ; Restore return address
        BVS     exit$l                  ; Return if error produced
        BIC     r4, r4, #area_flag_vm   ; Clear special flag bit
        STMFD   r13!, {r10-r11}         ; Stack registers
        [       :LNOT: :DEF: demo
        ADR     r11, handler_intercept  ; Pointer to intercept handler
        TEQ     r6, r11                 ; Is handler indirected
        BNE     nindr$l                 ; Skip next bit if not indirected
        LDR     r6, [r7, #handler_ptr]  ; Get correct handler pointer
        LDR     r7, [r7, #handler_r12]  ; Get correct workspace pointer
nindr$l
        ]
        LDR     r11, ws_area_ptr        ; Pointer to the first area record
find$l  TEQ     r11, #0                 ; Is it a valid pointer
        BEQ     not$l                   ; Exit loop if no more records
        LDR     r10, [r11, #area_number]; Get virtual area number
        TEQ     r10, r1                 ; Is it the required area
        LDRNE   r11, [r11, #area_next]  ; Pointer to next record if no match
        BNE     find$l                  ; Loop for next record if not found
        LDR     r10, ws_log_page_size   ; Get log of page size
        LDR     r2, [r11, #area_total_pages]; Get current size in pages
        MOV     r2, r2, LSL r10         ; Convert size to bytes
        LDR     r5, [r11, #area_max_pages]; Get maximum size in pages
        MOV     r5, r5, LSL r10         ; Convert maximum size to bytes
        ORR     r4, r4, #area_flag_vm   ; Indicate virtual memory active
not$l   LDMFD   r13!, {r10-r11}         ; Restore registers
        B       exit$l                  ; Return to caller

        ; Return to caller
exit$l  ORRVS   r14, r14, #V            ; Set error state in return address
        LDR     pc, ws_os_swi_exit      ; Return to OS SWI exit handler

        ;   Parameters  : r0    - Number of dynamic area to alter.
        ;                 r1    - Amount to move in bytes (signed integer).
        ;   Returns     : r0    - Preserved.
        ;                 r1    - Number of bytes moved (unsigned integer).
        ;   Description : Replacement OS_ChangeDynamicArea handler.
os_change_dynamic_area_handler
        LocalLabels
        STMFD   r13!, {r0}              ; Stack registers
;        debug_record "os_change_dynamic_area_handler"
        ADRL    r12, workspace_ptr      ; Pointer to workspace pointer
        LDR     r12, [r12]              ; Get pointer to module workspace
        LDR     r0, ws_sma_change_area  ; Get OS_ChangeDynamicArea semaphore
        TEQ     r0, #0                  ; Is semaphore set
        LDMFD   r13!, {r0}              ; Restore registers
        LDRNE   pc, ws_prv_change_area  ; Jump to the original routine
        STMFD   r13!, {r14}             ; Stack return address
        BL      change_virtualise       ; Change amount of memory used
        LDMFD   r13!, {r14}             ; Restore return address
        ORRVS   r14, r14, #V            ; Set error state in return address
        LDR     pc, ws_os_swi_exit      ; Return to OS SWI exit handler

        ;   Parameters  : r0    - Number of dynamic area to alter.
        ;   Returns     : r0    - Pointer to start of area.
        ;                 r1    - Current number of bytes in area.
        ;                 r2    - Maximum size of area, if bit 7 of r0 was
        ;                         set on entry; preserved otherwise.
        ;   Description : Replacement OS_ReadDynamicArea handler.
os_read_dynamic_area_handler
        LocalLabels
        STMFD   r13!, {r0}              ; Stack registers
;        debug_record "os_read_dynamic_area_handler"
        ADRL    r12, workspace_ptr      ; Pointer to workspace pointer
        LDR     r12, [r12]              ; Get pointer to module workspace
        LDR     r0, ws_sma_read_area    ; Get OS_ReadDynamicArea semaphore
        TEQ     r0, #0                  ; Is semaphore set
        LDMFD   r13!, {r0}              ; Restore registers
        LDRNE   pc, ws_prv_read_area    ; Jump to the original routine
        STMFD   r13!, {r10-r11}         ; Stack registers
        LDR     r11, ws_area_ptr        ; Pointer to the first area record
find$l  TEQ     r11, #0                 ; Is it a valid pointer
        BEQ     not$l                   ; Exit loop if no more records
        LDR     r10, [r11, #area_number]; Get virtual area number
        TEQ     r10, r0                 ; Is it the required area
        LDRNE   r11, [r11, #area_next]  ; Pointer to next record if no match
        BNE     find$l                  ; Loop for next record if not found
        LDR     r0, [r11, #area_base]   ; Get the base address of the area
        LDR     r10, ws_log_page_size   ; Get log of page size
        LDR     r1, [r11, #area_total_pages]; Get current size in pages
        MOV     r1, r1, LSL r10         ; Convert size to bytes
        LDR     r2, [r11, #area_max_pages]; Get maximum size in pages
        MOV     r2, r2, LSL r10         ; Convert maximum size to bytes
        LDMFD   r13!, {r10-r11}         ; Restore registers
        LDR     pc, ws_os_swi_exit      ; Return to OS SWI exit handler

        ; Not a virtual memory dynamic area
not$l   LDMFD   r13!, {r10-r11}         ; Restore registers
        LDR     pc, ws_prv_read_area    ; Jump to the original routine


; Handle indirected dynamic area handlers

        ;   Parameters  : None
        ;   Returns     : None
        ;   Description : Initialise the dynamic area handler intercepts.
initialise_intercepts
        LocalLabels
        JSR     "r0"                    ; Stack registers
        MOV     r0, #0                  ; Null pointer
        STR     r0, ws_handler_ptr      ; Start the list without entries
        STR     r0, ws_morph_area       ; Pass on to all areas
        STR     r0, ws_handler_new_ptr  ; Pointer to new record
        RTSS                            ; Return from subroutine

        [       :LNOT: :DEF: demo
        ;   Parameters  : r4    - The dynamic area flags.
        ;                 r6    - Pointer to dynamic area handler routine,
        ;                         or 0 if no routine.
        ;                 r7    - Pointer to value to pass in r12 to handler
        ;                         routine; or -1 for OS to instead pass base
        ;                         address of area; or 0 if r6 = 0.
        ;   Returns     : r4    - Preserved.
        ;                 r6    - Pointer to (possibly different) dynamic area
        ;                         handler routine, or 0 if no routine.
        ;                 r7    - Pointer to (possibly different) value to pass
        ;                         in r12 to handler routine.
        ;   Description : Add a dynamic area handler intercept. This should
        ;                 be called immediately before the OS_DynamicArea 0
        ;                 SWI that creates the area. After the area has been
        ;                 created successfully post_add_intercept should be
        ;                 called to finish the intercept.
pre_add_intercept
        LocalLabels
        JSR     "r0-r3"                 ; Stack registers
        TEQ     r6, #0                  ; Is there an area handler
        RTSS EQ                         ; Exit if not
        AND     r0, r4, #&F70           ; Get flags of interest
        TEQ     r0, #&000               ; Are flags set suitably
        RTSS NE                         ; Exit if it may
        LDR     r2, ws_handler_new_ptr  ; Is there a free record
        TEQ     r2, #0                  ; Is the pointer valid
        BNE     got$l                   ; Skip claim if already got memory
        MOV     r0, #OSModule_Alloc     ; Claim entry code
        MOV     r3, #handler_struct     ; Required size
        BL      os_module               ; Claim space for intercept area record
        RTSS VS                         ; Exit if error
got$l   STR     r2, ws_handler_new_ptr  ; Store pointer to record
        MOV     r0, #0                  ; Value to clear pointers with
        STR     r0, [r2, #handler_next] ; Initially no pointer to next record
        STR     r0, [r2, #handler_number]; Clear the dynamic area number
        STR     r6, [r2, #handler_ptr]  ; Store pointer to handler
        STR     r7, [r2, #handler_r12]  ; Store specified handler workspace
        CMP     r7, #-1                 ; Should area base be passed
        STRNE   r7, [r2, #handler_ws]   ; Store specified handler workspace
        STREQ   r0, [r2, #handler_ws]   ; Store zero pointer until base known
        ADR     r6, handler_intercept   ; Pointer to replacement handler
        MOV     r7, r2                  ; Copy intercept record pointer
        RTSS                            ; Return from subroutine

        ;   Parameters  : r1    - The allocated dynamic area number.
        ;                 r6    - Pointer to dynamic area handler routine,
        ;                         or 0 if no routine.
        ;                 r7    - Pointer to value to pass in r12 to handler
        ;                         routine; or -1 for OS to instead pass base
        ;                         address of area; or 0 if r6 = 0.
        ;   Returns     : None
        ;   Description : Finish adding a dynamic area handler intercept.
post_add_intercept
        LocalLabels
        JSR     "r0-r1"                 ; Stack registers
        ADR     r0, handler_intercept   ; Pointer to replacement handler
        TEQ     r0, r6                  ; Was this area intercepted
        RTSS NE                         ; Not interested if it was not
        STR     r1, [r7, #handler_number]; Store area number
        LDR     r0, [r7, #handler_r12]  ; Get the specified handler workspace
        CMP     r0, #-1                 ; Should area base be passed
        STRNE   r0, [r7, #handler_ws]   ; Store pointer to handler workspace
        STREQ   r3, [r7, #handler_ws]   ; Store pointer to handler workspace
        LDR     r0, ws_handler_ptr      ; Get old head of list
        STR     r0, [r7, #handler_next] ; Put it after this one
        STR     r7, ws_handler_ptr      ; Place this one at the start
        MOV     r0, #0                  ; Value to clear pointer with
        STR     r0, ws_handler_new_ptr  ; Clear free record pointer
        LDR     r6, [r7, #handler_ptr]  ; Restore original handler pointer
        LDR     r7, [r7, #handler_r12]  ; Restore original handler workspace
        RTSS                            ; Return from subroutine
        ]

        ;   Parameters  : r0    - The dynamic area to remove the intercept for.
        ;   Returns     : None
        ;   Description : Remove a dynamic area handler intercept (if any).
        ;                 This must be called as the last action of an area
        ;                 about to removed.
remove_intercept
        LocalLabels
        JSR     "r0-r3, r10-r11"        ; Stack registers
        ADR     r10, ws_handler_ptr     ; Pointer to previous pointer
        LDR     r11, [r10]              ; Pointer to the first intercept record
find$l  TEQ     r11, #0                 ; Is it a valid pointer
        RTSS EQ                         ; Exit without error if not found
        LDR     r1, [r11, #handler_number]; Get dynamic area number
        TEQ     r0, r1                  ; Is it the required area
        ADDNE   r10, r11, #handler_next ; Next pointer to previous pointer
        LDRNE   r11, [r10]              ; Pointer to next record if no match
        BNE     find$l                  ; Loop for next record if not found
        LDR     r0, [r11, #handler_next]; Pointer to next intercept record
        STR     r0, [r10]               ; Unlink intercept area record
        MOV     r0, #OSModule_Free      ; Free entry code
        MOV     r2, r11                 ; Pointer to record
        BL      os_module               ; Release area record memory
        RTSS                            ; Return from subroutine

        [       :LNOT: :DEF: demo
        ;   Parameters  : r0    - Reason code
        ;                           0: PreGrow - Issued just before pages are
        ;                              moved to grow an area.
        ;                           1: PostGrow - Issued just after pages are
        ;                              moved to grow an area.
        ;                           2: PreShrink - Issued just before pages are
        ;                              moved to shrink an area.
        ;                           3: PostShrink - Issued just after pages are
        ;                              moved to shrink an area.
        ;                           4: TestShrink - Issued to check how much of
        ;                              current size may be regarded as free.
        ;                 r2    - Number of entries in page block. (grow only)
        ;                 r3    - Change in area size in bytes.
        ;                 r4    - Current/new size of area in bytes.
        ;                 r5    - Page size in bytes.
        ;                 r12   - Pointer to dynamic area intercept record.
        ;   Returns     : r3    - Amount area can shrink by in bytes for
        ;                         PreShrink and TestShrink only.
        ;   Description : The dynamic area handler.
handler_intercept
        LocalLabels
        JSR     "r6-r8, r12"            ; Stack registers
;        debug_record "handler_intercept"
        MOV     r6, r12                 ; Copy pointer to intercept record
        ADRL    r12, workspace_ptr      ; Pointer to workspace pointer
        LDR     r12, [r12]              ; Get pointer to workspace
        LDR     r7, ws_morph_area       ; Number of area being morphed
        LDR     r8, [r6, #handler_number]; Get number of this dynamic area
        TEQ     r8, #0                  ; Has the handler been initialised
        BLEQ    init$l                  ; Perform any initialisation
        TEQ     r7, r8                  ; Is this area being morphed
        RTSS EQ                         ; Don't do anything if it is
        LDR     r12, [r6, #handler_ws]  ; Get required workspace pointer
        MOV     r14, pc                 ; Construct return address
        LDR     pc, [r6, #handler_ptr]  ; Jump to original handler
        RTS                             ; Return from subroutine
init$l  JSR     "r0-r7, r9"             ; Stack registers
        MOV     r9, r6                  ; Copy pointer to area record
        MOV     r1, #-1                 ; Start enumeration
initl$l MOV     r0, #OSDynamicArea_Enumerate; Reason code to enumerate areas
        BL      os_dynamic_area         ; Read the first dynamic area number
        CMP     r1, #-1                 ; Are there any more areas
        LDREQ   r8, ws_morph_area       ; Number of area being morphed
        RTSS EQ                         ; Return from subroutine if not found
        MOV     r0, #OSDynamicArea_Read ; Reason code to read details
        BL      os_dynamic_area         ; Read the dynamic area details
        TEQ     r7, r9                  ; Does the workspace pointer match
        BNE     initl$l                 ; Loop for the next area if not
        STR     r1, [r9, #handler_number]; Store the dynamic area number
        LDR     r0, [r9, #handler_r12]  ; Get specified handler workspace
        CMP     r0, #-1                 ; Should area base be passed
        STREQ   r3, [r9, #handler_ws]   ; Store zero pointer until base known
        MOV     r8, r1                  ; Copy the dynamic area number
        RTSS                            ; Return from subroutine

        ;   Parameters  : r0    - Proposed change in size of area in pages
        ;                         (signed integer).
        ;                 r11   - Pointer to dynamic area record.
        ;   Returns     : r0    - Allowed change in size of area in pages
        ;                         (signed integer).
        ;                 r11   - Preserved.
        ;   Description : Check that a change in virtual memory dynamic area
        ;                 size is acceptable to the handler routine. This may
        ;                 return with an error if the handler does. Note that
        ;                 post_call_handler must also be called if this does
        ;                 not return an error, even if the actual resize
        ;                 failed.
pre_call_handler
        LocalLabels
        JSR     "r1-r5, r10"            ; Stack registers
        STR     r0, ws_shrink_grow      ; Store change in size
        LDR     r1, [r11, #area_number] ; Get dynamic area number
        LDR     r10, ws_handler_ptr     ; Start of list of intercepts
loop$l  TEQ     r10, #0                 ; Is it a valid pointer
        RTSS EQ                         ; Exit if not found
        LDR     r2, [r10, #handler_number]; Get dynamic area number
        TEQ     r1, r2                  ; Is it the required area
        LDRNE   r10, [r10, #handler_next]; Get pointer to next record if not
        BNE     loop$l                  ; Loop if not the required area
        CMP     r0, #0                  ; In which direction is the change
        RTSS EQ                         ; Exit if no change
        BLT     shrink$l                ; Shrink if less than zero
        MOV     r2, r0                  ; Copy requested change in size
        MOV     r0, #0                  ; Reason code for PreGrow
        MOV     r1, #0                  ; No page block
        MOV     r3, r2                  ; Copy change in size in pages
        B       call$l                  ; Call the handler
shrink$l
        RSB     r3, r0, #0              ; Number of pages to shrink by
        MOV     r0, #2                  ; Reason code for PreShrink
call$l  LDR     r4, [r11, #area_total_pages]; New size of area in pages
        LDR     r5, ws_log_page_size    ; Log base 2 of page size
        MOV     r3, r3, LSL r5          ; Convert change in size to bytes
        MOV     r4, r4, LSL r5          ; Convert new size to bytes
        LDR     r5, ws_page_size        ; The page size in bytes
        STMFD   r13!, {r12}             ; Stack workspace pointer
        LDR     r12, [r10, #handler_ws] ; Get required workspace pointer
;        debug_record "pre_call_handler"
        MOV     r14, pc                 ; Construct return address
        LDR     pc, [r10, #handler_ptr] ; Jump to original handler
        LDMFD   r13!, {r12}             ; Restore workspace pointer
        RTE VS                          ; Return with error if any produced
        LDR     r0, ws_shrink_grow      ; Get original change in size
        CMP     r0, #0                  ; Check sign of change
        RTSS GE                         ; Exit if it is grow
        LDR     r1, ws_log_page_size    ; Log base 2 of page size
        MOV     r0, r3, LSR r1          ; Amount to shrink in pages
        RSB     r0, r0, #0              ; Negate change in size
        RTSS                            ; Return from subroutine

        ;   Parameters  : r0    - Actual change in size of area in pages
        ;                         (signed or unsigned integer).
        ;                 r11   - Pointer to dynamic area record.
        ;   Returns     : r0    - Preserved.
        ;                 r11   - Preserved.
        ;   Description : Inform a dynamic area handler routine of the size of
        ;                 a change. This must be called after pre_call_handler,
        ;                 even if no change in size was possible.
post_call_handler
        LocalLabels
        JSR     "r0-r5, r10, r12"       ; Stack registers
        LDR     r1, [r11, #area_number] ; Get dynamic area number
        LDR     r10, ws_handler_ptr     ; Start of list of intercepts
loop$l  TEQ     r10, #0                 ; Is it a valid pointer
        RTSS EQ                         ; Exit if not found
        LDR     r2, [r10, #handler_number]; Get dynamic area number
        TEQ     r1, r2                  ; Is it the required area
        LDRNE   r10, [r10, #handler_next]; Get pointer to next record if not
        BNE     loop$l                  ; Loop if not the required area
        CMP     r0, #0                  ; Is actual change negative
        RSBLT   r0, r0, #0              ; Make it positive
        LDR     r1, ws_shrink_grow      ; Get original change in size
        CMP     r1, #0                  ; Was it grow or shrink
        BLT     shrink$l                ; Shrink if less than zero
        TEQ     r0, #0                  ; Was there any change
        RTSS EQ                         ; Exit if not
        MOV     r2, r0                  ; Copy number of pages grown by
        MOV     r0, #1                  ; Reason code for PostGrow
        MOV     r1, #0                  ; No page block
        MOV     r3, r2                  ; Copy change in size in pages
        B       call$l                  ; Call the handler
shrink$l
        MOV     r3, r0                  ; Number of pages shrunk by
        MOV     r0, #3                  ; Reason code for PostShrink
call$l  LDR     r4, [r11, #area_total_pages]; New size of area in pages
        LDR     r5, ws_log_page_size    ; Log base 2 of page size
        MOV     r3, r3, LSL r5          ; Convert change in size to bytes
        MOV     r4, r4, LSL r5          ; Convert new size to bytes
        LDR     r5, ws_page_size        ; The page size in bytes
        STMFD   r13!, {r12}             ; Stack workspace pointer
        LDR     r12, [r10, #handler_ws] ; Get required workspace pointer
;        debug_record "post_call_handler"
        MOV     r14, pc                 ; Construct return address
        LDR     pc, [r10, #handler_ptr] ; Jump to original handler
        LDMFD   r13!, {r12}             ; Restore workspace pointer
        RTSS                            ; Return from subroutine
        ]

; More modifiable data in code space

        ; SWI intercept data
previous_swi_handler % Ptr              ; Previous SWI handler

        ; Current task details
filter_task_record  % Ptr               ; Pointer to record for this task

; Filter handling for suspending tasks

        ;   Parameters  : None
        ;   Returns     : None
        ;   Description : Initialise filter handling.
initialise_filters
        LocalLabels
        JSR     "r0-r4"                 ; Stack registers
        MOV     r0, #0                  ; Value to clear pointer with
        STR     r0, ws_filter_pre       ; Clear pre filter pointer
        STR     r0, ws_filter_post      ; Clear post filter pointer
        STR     r0, ws_filter_r12       ; Clear filter workspace pointer
        STR     r0, ws_filter_ptr       ; Clear pointer to first record
        STR     r0, previous_swi_handler; Clear claimed flag
        STR     r0, filter_task_record  ; Clear task record pointer
        MOV     r0, #OS_HandlerUndefinedInstruction; First handler to read
        ADRL    r4, ws_filter_handlers  ; Pointer to default handlers dump
loop$l  TEQ     r0, #OS_HandlerApplicationSpace; Is it a special handler
        TEQNE   r0, #OS_HandlerCAO      ; Is it a special handler
        ADDEQ   r0, r0, #1              ; Skip handler if it is
        BEQ     loop$l                  ; Loop for next handler
        SWI     XOS_ReadDefaultHandler  ; Read the default handler details
        STMIA   r4!, {r0-r3}            ; Store default handler details
        TEQ     r0, #OS_HandlerUpCall   ; Is it the last handler
        ADDNE   r0, r0, #1              ; Increment handler number if not
        BNE     loop$l                  ; Loop for next handler if not
        MOV     r0, #-1                 ; Marker for end of list
        STR     r0, [r4]                ; Store end of list marker
        RTSS                            ; Return from subroutine

        ;   Parameters  : None
        ;   Returns     : None
        ;   Description : Finalise filter handling.
finalise_filters
        LocalLabels
        JSR     ""                      ; Stack registers
        BL      filter_lose_filter      ; Ensure that no filters are claimed
        RTSS                            ; Return from subroutine

        ;   Parameters  : None
        ;   Returns     : None
        ;   Description : Claim the filters directly from the WIMP. The
        ;                 original handler details are stored.
filter_claim_filter
        LocalLabels
        JSR     "r0-r5"                 ; Stack registers
        MOV     r0, #OSModule_Lookup    ; Reason code to read module details
        ADR     r1, name$l              ; Pointer to module name
        BL      os_module               ; Read details of module
        BVS     fail$l                  ; Skip next bit if not found
        LDR     r0, [r3, #&14]          ; Get offset to help text
        ADD     r0, r3, r0              ; Pointer to help text
loop$l  LDRB    r1, [r0], #1            ; Read next character of module name
        TEQ     r1, #9                  ; Is it a tab character
        BNE     loop$l                  ; Loop if still part of module name
        LDRB    r1, [r0]                ; Read next character
        TEQ     r1, #9                  ; Is it a tab character
        LDREQB  r1, [r0, #1]!           ; Read next character if it is
        SUB     r2, r1, #'0'            ; Convert digit to a number
        LDRB    r1, [r0, #2]!           ; Read next character
        SUB     r1, r1, #'0'            ; Convert digit to a number
        ORR     r2, r1, r2, LSL#4       ; Combine with existing number
        LDRB    r1, [r0, #1]            ; Read next character
        SUB     r1, r1, #'0'            ; Convert digit to a number
        ORR     r2, r1, r2, LSL#4       ; Combine with existing number
        TEQ     r2, #&009               ; Is version number 0v09
        BEQ     v009$l                  ; Branch to relevant code if it is
        TEQ     r2, #&011               ; Is version number 0v11
        BEQ     v011$l                  ; Branch to relevant code if it is
        MOV     r0, r3                  ; Copy module start address
        LDR     r1, swi$l               ; Get SWI instruction to find
fswi$l  LDR     r2, [r0], #4            ; Read next instruction
        TEQ     r1, r2                  ; Does it match
        BNE     fswi$l                  ; Loop if not
        SUB     r0, r0, #16             ; Back to important instruction
        LDR     r2, [r0, #44]           ; Read next SWI instruction
        TEQ     r1, r2                  ; Check that it is also correct
        RTSS NE                         ; Exit if it is not
        LDR     r1, = &128f1            ; Shifted ADR instruction
        LDR     r2, [r0]                ; Get first ADR instruction
        MOV     r2, r2, LSR#12          ; Shift to remove constant
        TEQ     r1, r2                  ; Is it the correct instruction
        RTSS NE                         ; Exit if it is not
        LDR     r2, [r0, #32]           ; Get second ADR instruction
        MOV     r2, r2, LSR#12          ; Shift to remove constant
        TEQ     r1, r2                  ; Is it the correct instruction
        RTSS NE                         ; Exit if it is not
        LDR     r1, = &12811            ; Shifted ADD instruction
        LDR     r2, [r0, #4]            ; Get first ADD instruction
        MOV     r2, r2, LSR#12          ; Shift to remove constant
        TEQ     r1, r2                  ; Is it the correct instruction
        RTSS NE                         ; Exit if it is not
        LDR     r2, [r0, #36]           ; Get second ADD instruction
        MOV     r2, r2, LSR#12          ; Shift to remove constant
        TEQ     r1, r2                  ; Is it the correct instruction
        RTSS NE                         ; Exit if it is not
        ADD     r1, r0, #8              ; Base address for first pointer
        LDR     r2, [r0]                ; Get first ADR instruction again
        BL      dec$l                   ; Decode the offset
        LDR     r2, [r0, #4]            ; Get first ADD instruction again
        BL      dec$l                   ; Decode the offset
        STR     r1, ws_filter_pre       ; Store pointer to pre-filter
        ADD     r1, r0, #40             ; Base address for second pointer
        LDR     r2, [r0, #32]           ; Get second ADR instruction again
        BL      dec$l                   ; Decode the offset
        LDR     r2, [r0, #36]           ; Get second ADD instruction again
        BL      dec$l                   ; Decode the offset
        STR     r1, ws_filter_post      ; Store pointer to post-filter
        B       full$l                  ; Skip to next bit
v009$l  LDR     r1, = &64C              ; Offset to pre-filter handler
        LDR     r2, = &698              ; Offset to post-filter handler
        B       done$l                  ; Skip to next bit
v011$l  LDR     r1, = &7AC              ; Offset to pre-filter handler
        LDR     r2, = &7F8              ; Offset to post-filter handler
done$l  ADD     r0, r3, r1              ; Pointer to pre-filter handler
        STR     r0, ws_filter_pre       ; Store pointer to pre-filter
        ADD     r0, r3, r2              ; Pointer to post-filter handler
        STR     r0, ws_filter_post      ; Store pointer to post-filter
full$l  STR     r4, ws_filter_r12       ; Store workspace pointer
fail$l  MOV     r0, #0                  ; Reason code for pre-filter
        ADR     r1, pre$l               ; Pointer to pre-filter reason code
        MOV     r2, r12                 ; Use module workspace for filter
        SWI     XWimp_RegisterFilter    ; Register WIMP pre-filter
        MOV     r0, #1                  ; Reason code for post-filter
        ADR     r1, post$l              ; Pointer to post-filter reason code
        SWI     XWimp_RegisterFilter    ; Register WIMP post-filter
        RTSS                            ; Return from subroutine
swi$l   SWINE   XWimp_RegisterFilter    ; Instruction to find
dec$l   JSR     "r0, r2"                ; Stack registers
        AND     r0, r2, #&f00           ; Copy shift amount
        MOV     r0, r0, LSR#7           ; Shift rotate amount correctly
        AND     r2, r2, #&ff            ; Mask immediate value
        ADD     r1, r1, r2, ROR r0      ; Add immediate offset
        RTS                             ; Return from subroutine
name$l  =       "FilterManager", 0      ; Module name to find
        ALIGN

        ; Replacement pre-filter
pre$l   JSR     "r12"                   ; Stack registers
        STMFD   r13!, {r0-r1}           ; Stack more registers
        MOV     r0, r2                  ; Copy task handle
        BL      filter_find             ; Check if this task is filtered
        TEQ     r0, #0                  ; Was the task found
        MOVNE   r12, r0                 ; Copy workspace pointer if found
        LDMFD   r13!, {r0-r1}           ; Restore registers
        BEQ     pre_not$l               ; Skip next bit if not matched
        BL      filter_pre              ; Call filter handler
        RTSS                            ; Return from filter handler
pre_not$l
        LDR     r14, ws_filter_pre      ; Get original filter pointer
        STMFD   r13!, {r14}             ; Stack original filter pointer
        LDR     r12, ws_filter_r12      ; Get original workspace pointer
        MOV     r14, pc                 ; Store return address
        LDMFD   r13!, {pc}              ; Jump to original filter handler
        RTSS                            ; Return from filter handler

        ; Replacement post-filter
post$l  JSR     "r12"                   ; Stack registers
        STMFD   r13!, {r0-r1}           ; Stack more registers
        MOV     r0, r2                  ; Copy task handle
        BL      filter_find             ; Check if this task is filtered
        TEQ     r0, #0                  ; Was the task found
        MOVNE   r12, r0                 ; Copy workspace pointer if found
        LDMFD   r13!, {r0-r1}           ; Restore registers
        BEQ     post_not$l              ; Skip next bit if not matched
        BL      filter_post             ; Call filter handler
        RTSS                            ; Return from filter handler
post_not$l
        LDR     r14, ws_filter_post     ; Get original filter pointer
        STMFD   r13!, {r14}             ; Stack original filter pointer
        LDR     r12, ws_filter_r12      ; Get original workspace pointer
        MOV     r14, pc                 ; Store return address
        LDMFD   r13!, {pc}              ; Jump to original filter handler
        RTSS                            ; Return from filter handler

        ;   Parameters  : None
        ;   Returns     : None
        ;   Description : Restore the original filter handler.
filter_lose_filter
        LocalLabels
        JSR     "r0-r1"                 ; Stack registers
        LDR     r0, ws_filter_ptr       ; Get pointer to first filter
        TEQ     r0, #0                  ; Are any filters left
        RTSS NE                         ; Exit if there are any
        MOV     r0, #0                  ; Reason code for pre-filter
        MOV     r1, #0                  ; Deregister filter
        SWI     XWimp_RegisterFilter    ; Deregister WIMP pre-filter
        MOV     r0, #1                  ; Reason code for post-filter
        SWI     XWimp_RegisterFilter    ; Deregister WIMP post-filter
        MOV     r1, #Service_WimpRegisterFilters; Service call for filters
        SWI     XOS_ServiceCall         ; Generate service call
        MOV     r0, #0                  ; Value to clear pointers with
        STR     r0, ws_filter_pre       ; Clear pre filter pointer
        STR     r0, ws_filter_post      ; Clear post filter pointer
        STR     r0, ws_filter_r12       ; Clear filter workspace pointer
        RTSS                            ; Return from subroutine

        ;   Parameters  : r0    - Task handle to check.
        ;   Returns     : r0    - Status of the task.
        ;   Description : Check the status of a filtered task.
filter_check
        LocalLabels
        JSR     "r1"                    ; Stack registers
        BL      filter_find             ; Find details for task
        TEQ     r0, #0                  ; Was a record found
        MOVEQ   r0, #-1                 ; Set an illegal value if not found
        RTSS EQ                         ; Exit if not found
        LDR     r1, [r0, #filter_state] ; Get the status of the task
        LDR     r0, [r0, #filter_size]  ; Get the size of the task
        TEQ     r1, #filter_state_frozen; Is task frozen
        MOVNE   r0, #-1                 ; Otherwise it is not suspended
        RTSS                            ; Return from subroutine

        ;   Parameters  : r0    - Task handle to filter.
        ;   Returns     : None
        ;   Description : Start filtering a task.
filter_start
        LocalLabels
        JSR     "r0-r5, r10-r11"        ; Stack registers
        MOV     r11, r0                 ; Copy task handle
        SWI     XTaskManager_TaskNameFromHandle; Check task is valid
        RTE VS                          ; Exit if error produced.
        MOV     r10, r0                 ; Copy task name pointer
        ADRL    r0, cmd$l               ; Command to execute
        SWI     XWimp_StartTask         ; Start task to read wimp handler
        BL      virtualise_swap_dir     ; Ensure swap directory is known
        RTE VS                          ; Exit if error produced
        MOV     r0, r11                 ; Restore task handle
        BL      filter_find             ; Check if already filtered
        TEQ     r0, #0                  ; Was a match found
        RTSS NE                         ; Exit if already filtered
        MOV     r0, #OSModule_Alloc     ; Claim entry code
        MOV     r3, #filter_struct      ; Size of record
        BL      os_module               ; Claim memory for new record
        RTE VS                          ; Exit if error produced
        LDR     r0, ws_filter_ptr       ; Get previous head of list
        STR     r0, [r2, #filter_next]  ; Store pointer to previous head
        STR     r2, ws_filter_ptr       ; Place record at head of list
        STR     r11, [r2, #filter_task_handle]; Store task handle in record
        ADRL    r0, title               ; Use module title for filters
        ADR     r1, filter_pre          ; Pointer to pre-filter
        MOV     r3, r11                 ; Copy task handle
        SWI     XFilter_RegisterPreFilter; Register pre-filter
        ADRL    r1, filter_post         ; Pointer to post-filter
        MOV     r4, #0                  ; Filter requires all events
        SWI     XFilter_RegisterPostFilter; Register post-filter
        MOV     r0, #filter_state_move_block; Initial state of filter
        STR     r0, [r2, #filter_state] ; Store initial state
        MOV     r11, r2                 ; Copy record pointer
        MOV     r0, #OSFile_CreateDir   ; Reason code for create directory
        ADRL    r1, swap$l              ; Pointer to directory name
        MOV     r4, #0                  ; Default directory size
        SWI     XOS_File                ; Create the directory
        BVS     exit$l                  ; Exit if error produced
        ADD     r0, r11, #filter_file_name; Pointer to file name buffer
copy$l  LDRB    r2, [r1], #1            ; Read a character
        STRB    r2, [r0], #1            ; Write the character
        TEQ     r2, #0                  ; Was it the terminator
        BNE     copy$l                  ; Loop if not finished
        SUB     r1, r0, #1              ; Back to terminator
        MOV     r2, #'.'                ; Leafname separator
        STRB    r2, [r1], #1            ; Store leafname separator
        MOV     r0, r11                 ; Copy area pointer
        MOV     r2, #OS_Hex8Limit + 1   ; Size of buffer
        SWI     XOS_ConvertHex8         ; Append the area number
        BVS     exit$l                  ; Exit if error produced
        MOV     r0, #OSFile_CreateStamped; Reason code to create file
        ADD     r1, r11, #filter_file_name; Pointer to file name buffer
        LDR     r2, = OSFile_TypeData   ; Create a data file
        MOV     r4, #0                  ; Default load address
        MOV     r5, #0                  ; Create zero length file initially
        SWI     XOS_File                ; Create the swap file
        BVS     exit$l                  ; Exit if error produced
        MOV     r0, #0                  ; Value to clear file handle with
        STR     r0, [r11, #filter_file_handle]; Swap file is not open initially
        MOV     r0, #0                  ; Minimum x coordinate
        STR     r0, [r11, #filter_icon + OS_Box_x0]; Set icon minimum x
        MOV     r0, #-16                ; Minimum y coordinate
        STR     r0, [r11, #filter_icon + OS_Box_y0]; Set icon minimum y
        MOV     r0, #68                 ; Maximum x coordinate
        STR     r0, [r11, #filter_icon + OS_Box_x1]; Set icon maximum x
        MOV     r0, #20 + 68            ; Maximum y coordinate
        STR     r0, [r11, #filter_icon + OS_Box_y1]; Set icon maximum y
        LDR     r0, = Wimp_IconText :OR: Wimp_IconSprite \
                      :OR: Wimp_IconHCentred  :OR: Wimp_IconIndirected \
                      :OR: (Wimp_ColourBlack << Wimp_IconFGColourShift)
        ORR     r0, r0, #Wimp_ColourVeryLightGrey << Wimp_IconBGColourShift
        ORR     r0, r0, #Wimp_ButtonClick << Wimp_IconButtonTypeShift
        STR     r0, [r11, #filter_icon + Wimp_Icon_flags]; Set icon flags
        ADD     r0, r11, #filter_icon_text; Pointer to icon text string
        STR     r0, [r11, #filter_icon + Wimp_Icon_data \
                     + Wimp_IconData_indirected_text_and_sprite_text]
        ADD     r0, r11, #filter_icon_validation; Pointer to validation string
        STR     r0, [r11, #filter_icon + Wimp_Icon_data \
                     + Wimp_IconData_indirected_text_and_sprite_validation]
        MOV     r0, #?filter_icon_validation; Validation string size
        STR     r0, [r11, #filter_icon + Wimp_Icon_data \
                     + Wimp_IconData_indirected_text_and_sprite_size]
        ADD     r1, r11, #filter_icon_text; Pointer to icon text buffer
        MOV     r2, #0                  ; Start from first character
name_loop$l
        LDRB    r0, [r10, r2]           ; Read next byte of task name
        CMP     r0, #31                 ; Is it a terminator
        BLS     name_done$l             ; Exit loop if it is
        STRB    r0, [r1, r2]            ; Copy character
        ADD     r2, r2, #1              ; Increment position pointer
        CMP     r2, #?filter_icon_text - 1; Has maximum length been reached
        BLO     name_loop$l             ; Loop for next character of name
name_done$l
        MOV     r0, #0                  ; Null terminator for string
        STRB    r0, [r1, r2]            ; Terminate icon text string
        ADD     r1, r11, #filter_icon_validation; Pointer to validation text
        LDR     r0, = 's' + ('i' << 8) + ('c' << 16) + ('_') << 24
        STR     r0, [r1], #4            ; Store first word of string
        MOV     r2, #0                  ; Start from first character
sprite_loop$l
        LDRB    r0, [r10, r2]           ; Read next byte of task name
        CMP     r0, #31                 ; Is it a terminator
        BLS     sprite_done$l           ; Exit loop if it is
        STRB    r0, [r1, r2]            ; Copy character
        ADD     r2, r2, #1              ; Increment position pointer
        CMP     r2, #7                  ; Has maximum length been reached
        BLO     sprite_loop$l           ; Loop for next character of name
sprite_done$l
        MOV     r0, #0                  ; Null terminator for string
        STRB    r0, [r1, r2]            ; Terminate icon text string
        MOV     r0, #WimpSpriteOp_ReadSpriteSize; Reason code to read details
        ADD     r2, r11, #filter_icon_validation + 1; Sprite name pointer
        SWI     XWimp_SpriteOp          ; Read sprite details
        MOVVS   r0, #'?'                ; Last resort task name for sprite
        STRVS   r0, [r11, #filter_icon_validation + 4]; Store new name
        LDR     r0, [r11, #filter_task_handle]; Restore task handle
        BL      filter_message          ; Send a message to wake up the task
        BL      filter_claim_filter     ; Replace the filter manager
        RTSS                            ; Return from subroutine

        ; Handle failed initialisation
exit$l  STMFD   r13!, {r0}              ; Store error pointer
        LDR     r0, [r11, #filter_task_handle]; Restore task handle
        BL      filter_stop             ; Remove filters and free the memory
        LDMFD   r13!, {r0}              ; Restore error pointer
        SetV                            ; Ensure error flag is set
        RTE                             ; Return with the error

        ; Details of where to locate the swap file
swap$l  =       "<Virtualise$SwapDir>", 0; Actual prefix string to use

        ; Command to execute to read handler details
cmd$l   =       "Desktop_Virtualise", 0 ; The command for internal use only
        ALIGN

        ;   Parameters  : r0    - Task handle to send message to.
        ;   Returns     : None
        ;   Description : Send a Message_Quit to the specified task.
filter_message
        LocalLabels
        JSR     "r0-r2"                 ; Stack registers
        MOV     r2, r0                  ; Copy destination task handle
        MOV     r0, #Wimp_UserMessage   ; Event code for a user message
        ADR     r1, filter_event        ; Pointer to message block
        SWI     XWimp_SendMessage       ; Send a message to take action
        RTE VS                          ; Return with any error produced
        RTS                             ; Return from subroutine

        ; The message block to send
filter_event
        &       24                      ; Size of message block
        &       0                       ; This will be the task handle of sender
        &       0                       ; This will be my_ref
        &       0                       ; An original message, so no reference
        &       Message_Quit            ; Message action
        &       swi_chunk               ; A special code

        ;   Parameters  : r0    - Handle of a task to thaw.
        ;   Returns     : None
        ;   Description : Thaw a frozen task.
filter_end
        LocalLabels
        JSR     "r0-r1"                 ; Stack registers
        BL      filter_find             ; Find the record for this task
        TEQ     r0, #0                  ; Was a match found
        RTSS EQ                         ; Exit if not
        LDR     r1, [r0, #filter_state] ; Read the current task state
        TEQ     r1, #filter_state_frozen; Is task currently frozen
        RTSS NE                         ; Exit if not
        MOV     r1, #filter_state_thaw  ; State to thaw task
        STR     r1, [r0, #filter_state] ; Set the state of this task
        LDR     r0, [r0, #filter_task_handle]; Read task handle
        BL      filter_message          ; Send a message to the task
        RTSS                            ; Return from subroutine

        ;   Parameters  : r0    - Task handle to stop filtering.
        ;   Returns     : None
        ;   Description : End filtering a task.
filter_stop
        LocalLabels
        JSR     "r0-r5"                 ; Stack registers
        MOV     r3, r0                  ; Copy task handle
        BL      filter_find             ; Find the record for this task
        TEQ     r0, #0                  ; Was a match found
        RTSS EQ                         ; Exit if not
        BL      close$l                 ; Remove any swap file
        MOV     r5, r1                  ; Copy previous record pointer
        MOV     r2, r0                  ; Copy this record pointer
        ADRL    r0, title               ; Use module title for filters
        ADR     r1, filter_pre          ; Pointer to pre-filter
        SWI     XFilter_DeRegisterPreFilter; Deregister pre-filter
        ADR     r1, filter_post         ; Pointer to post-filter
        MOV     r4, #0                  ; Filter used all events
        SWI     XFilter_DeRegisterPostFilter; Deregister post-filter
        LDR     r0, [r2, #filter_next]  ; Get the next record pointer
        STR     r0, [r5]                ; Unlink this record
        MOV     r0, #OSModule_Free      ; Free entry code
        BL      os_module               ; Release the memory
        LDR     r0, filter_task_record  ; Get current task record pointer
        TEQ     r0, r2                  ; Is it this one
        MOV     r0, #0                  ; Value to clear pointer with
        STREQ   r0, filter_task_record  ; Clear record pointer if it matches
        BL      filter_lose_filter      ; Lose the filters if claimed
        RTSS                            ; Return from subroutine

        ; Close and delete any swap file for this task
close$l JSR     "r0-r5, r11"            ; Stack registers
        MOV     r11, r0                 ; Copy task record pointer
        BL      filter_close            ; Close swap file if it is open
        MOV     r0, #OSFile_Delete      ; Reason code to delete file
        ADD     r1, r11, #filter_file_name; Get pointer to file name
        SWI     XOS_File                ; Delete the file
        RTSS                            ; Return from subroutine

        ;   Parameters  : r0    - Task handle to stop filtering.
        ;   Returns     : None
        ;   Description : Remove any filters from a task. This is called
        ;                 if the task is quit unexpectedly.
filter_quit
        LocalLabels
        JSR     ""                      ; Stack registers
        BL      filter_stop             ; Remove any filters
        BL      filter_release_swi      ; Release SWI vector
        RTSS                            ; Return from subroutine

        ;   Parameters  : r0    - Task handle to find.
        ;   Returns     : r0    - Pointer to record, or 0 if not found.
        ;                 r1    - Pointer to the next pointer of the previous
        ;                         record.
        ;   Description : Find the record for a particular task handle.
filter_find
        LocalLabels
        JSR     "r2-r3"                 ; Stack registers
        MOV     r2, r0, LSL#16          ; Copy task handle
        MOV     r2, r2, LSR#16          ; Move back to bottom bits
        ADR     r1, ws_filter_ptr       ; Pointer to head of list
loop$l  LDR     r0, [r1]                ; Get pointer to this record
        TEQ     r0, #0                  ; Are there any more record
        RTSS EQ                         ; Exit if not found
        LDR     r3, [r0, #filter_task_handle]; Read the task handle
        MOV     r3, r3, LSL#16          ; Clear top 16 bits
        MOV     r3, r3, LSR#16          ; Move back to bottom bits
        TEQ     r2, r3                  ; Does the handle match
        RTSS EQ                         ; Exit if a match found
        ADD     r1, r0, #filter_next    ; Pointer to next pointer
        B       loop$l                  ; Loop for next record

; A literal pool

        LTORG

        ;   Parameters  : r0    - Event mask, as passed to Wimp_Poll.
        ;                 r1    - Pointer to event block.
        ;                 r2    - Task handle.
        ;                 r12   - Value of r2 when filter registered.
        ;   Returns     : r0    - Modified event mask.
        ;                 r1-r2 - Preserved.
        ;   Description : Filter called when the task calls Wimp_Poll.
filter_pre
        LocalLabels
        JSR     "r0-r5, r11-r12"        ; Stack registers
        MOV     r11, r12                ; Copy pointer to task record
        ADRL    r12, workspace_ptr      ; Pointer to workspace pointer
        LDR     r12, [r12]              ; Obtain pointer to workspace
        LDR     r0, [r11, #filter_state]; Get the current state of this task
        TEQ     r0, #filter_state_close_task; Should task be closed
        BEQ     close$l                 ; Close it if it should
exit$l  BL      filter_release_swi      ; Release SWI vector
        MOV     r0, #0                  ; Value to clear pointer with
        STR     r0, filter_task_record  ; Clear task record pointer
        LDR     r0, [r11, #filter_state]; Get the current state of this task
        TEQ     r0, #filter_state_frozen; Is the task frozen
        TEQNE   r0, #filter_state_thaw  ; or waiting to be thawed
        BNE     out$l                   ; Skip next bit if task not frozen
        LDR     r0, [r13]               ; Get event mask
        ORR     r0, r0, #Wimp_MaskNull  ; Do not return null polls
        ORR     r0, r0, #Wimp_MaskPollword; Do not check poll word
        BIC     r0, r0, #Wimp_GivenPollword :OR: Wimp_PollHighPriority
        STR     r0, [r13]               ; Store modified mask
out$l   RTSS                            ; Return from subroutine
close$l MOV     r0, #-1                 ; Read current slot size
        MOV     r1, #-1                 ; Read next slot size
        SWI     XWimp_SlotSize          ; Read the current size of the wimpslot
        STR     r0, [r11, #filter_size] ; Store original workspace size
        MOV     r0, r11                 ; Copy task record pointer
        BL      filter_open             ; Open swap file
        TEQ     r0, #0                  ; Was file opened
        BEQ     fail$l                  ; Fail if not
        MOV     r1, r0                  ; Copy file handle
        MOV     r0, #OSGBPB_WriteAt     ; Reason code to write to file
        MOV     r2, #&8000              ; Start from application base address
        LDR     r3, [r11, #filter_size] ; Size of workspace to write
        MOV     r4, #0                  ; Write at start of the file
        SWI     XOS_GBPB                ; Write the workspace to swap file
        BVS     fail$l                  ; Generate an error if failed
        ADD     r4, r11, #filter_handlers; Pointer to dump for handlers
        ADRL    r5, ws_filter_handlers  ; Pointer to default handlers dump
env_loop$l
        LDMIA   r5!, {r0-r3}            ; Read default handler details
        CMP     r0, #-1                 ; Is it the end of the list
        BEQ     env_done$l              ; Exit loop when finished
        SWI     XOS_ChangeEnvironment   ; Restore previous handler
        STMIA   r4!, {r0-r3}            ; Store previous handler details
        B       env_loop$l              ; Loop for next handler
env_done$l
        MOV     r0, #-1                 ; Value to terminate list with
        STR     r0, [r4]                ; Terminate the list
        MOV     r1, #Wimp_CloseMenu     ; Code to close open menus
        SWI     XWimp_CreateMenu        ; Close any open menus
        MOV     r0, #0                  ; No windows closed initially
        STR     r0, [r11, #filter_windows]; Store count of windows closed
        ADR     r1, win$l               ; Pointer to window block
        SWI     XWimp_CreateWindow      ; Create a dummy window
        BVS     win_bad$l               ; Skip next bit if error produced
        MOV     r2, r0                  ; Copy dummy window handle
        ADRL    r1, ws_string           ; Pointer to general purpose buffer
        STR     r0, [r1, #Wimp_Open_w]  ; Store window handle
        SWI     XWimp_GetWindowState    ; Read the window details
        BVS     win_done$l              ; Skip next bit if error produced
        MOV     r0, #Wimp_Hidden        ; Window handle to open at bottom
        STR     r0, [r1, #Wimp_Open_next]; Store window stack position
        SWI     XWimp_OpenWindow        ; Open the window
        BVS     win_done$l              ; Skip next bit if error produced
        SWI     XWimp_GetWindowState    ; Read the window details again
        BVS     win_done$l              ; Skip next bit if error produced
win_loop$l
        LDR     r0, [r1, #Wimp_Open_next]; Get next window handle
        CMP     r0, #Wimp_Top           ; Are there any more windows
        BEQ     win_done$l              ; Exit loop if not
        STR     r0, [r1, #Wimp_Open_w]  ; Store window handle
        SWI     XWimp_GetWindowState    ; Find out about the next window
        BVS     win_done$l              ; Exit loop if error produced
        LDR     r0, [r1, #Wimp_Open_w]  ; Restore window handle
        CMP     r0, #&FFFFFFFE          ; Is it the iconbar
        BEQ     win_loop$l              ; Skip this window if it is
        BL      filter_handle           ; Find owner of window
        LDR     r3, [r11, #filter_task_handle]; Get current task handle
        TEQ     r0, r3                  ; Does handle match
        BNE     win_loop$l              ; Loop for next window if not
        MOV     r0, #OSGBPB_Write       ; Reason code to write to file
        LDR     r1, [r11, #filter_file_handle]; Get swap file handle
        STMFD   r13!, {r2}              ; Stack registers
        ADRL    r2, ws_string           ; Pointer to buffer
        MOV     r3, #Wimp_WindowState   ; Number of bytes to write
        SWI     XOS_GBPB                ; Write details of window to swap file
        LDMFD   r13!, {r2}              ; Restore registers
        BVS     win_done$l              ; Exit loop if error produced
        LDR     r0, [r11, #filter_windows]; Get count of windows closed
        ADD     r0, r0, #1              ; Increment count
        STR     r0, [r11, #filter_windows]; Store the updated count
        ADRL    r1, ws_string           ; Restore buffer pointer
        SWI     XWimp_CloseWindow       ; Close the window
        B       win_loop$l              ; Loop for the next window
win_done$l
        STR     r2, [r1, #Wimp_Open_w]  ; Store window handle
        SWI     XWimp_DeleteWindow      ; Delete the window
win_bad$l
        MOV     r0, #0                  ; Value to clear pointer with
        STR     r0, [r11, #filter_icons]; Clear icon list pointer
        ADRL    r1, ws_string           ; Pointer to a buffer
        MOV     r0, #Wimp_IconBar       ; Handle of icon bar
        STR     r0, [r1, #Wimp_WindowInfo_w]; Store handle of icon bar
        ORR     r1, r1, #1              ; Only return window header
        SWI     XWimp_GetWindowInfo     ; Read iconbar details
        BVS     icons_done$l            ; Skip next bit if unable to read icons
        BIC     r1, r1, #1              ; Clear header only flag
        LDR     r0, [r1, #Wimp_WindowInfo_icon_count]; Get number of icons
        MOV     r1, #Wimp_Icon          ; Size of each icon block
        MOV     r2, #Wimp_WindowInfo    ; Size of header block
        MLA     r3, r0, r1, r2          ; Calculate required block size
        MOV     r0, #OSModule_Alloc     ; Claim entry code
        BL      os_module               ; Claim memory for icon data
        BVS     icons_done$l            ; Skip next bit if unable to get memory
        MOV     r1, r2                  ; Copy block pointer
        MOV     r0, #Wimp_IconBar       ; Handle of icon bar
        STR     r0, [r1, #Wimp_WindowInfo_w]; Store handle of icon bar
        SWI     XWimp_GetWindowInfo     ; Read iconbar details with icons
        BVS     icons_done$l            ; Skip next bit if unable to read icons
        STR     r1, [r11, #filter_icons]; Store pointer to details
        ADD     r0, r1, #Wimp_WindowInfo_icons; Pointer to first icon
        LDR     r2, [r1, #Wimp_WindowInfo_icon_count]; Get number of icons
        MOV     r1, #0                  ; First icon is number 0
icons_loop$l
        SUBS    r2, r2, #1              ; Decrement icon count
        BMI     icons_done$l            ; Exit loop when finished
        STMFD   r13!, {r0-r2}           ; Stack registers
        LDR     r0, [r0, #Wimp_Icon_flags]; Get icon flags
        TST     r0, #Wimp_IconDeleted   ; Has icon been deleted
        BNE     icons_next$l            ; Skip next bit if it has
        MOV     r0, #Wimp_IconBar       ; Handle of icon bar
        BL      filter_handle           ; Find task handle for icon
        LDR     r2, [r11, #filter_task_handle]; Get current task handle
        TEQ     r0, r2                  ; Does handle match
        BNE     icons_next$l            ; Skip next bit if not
        MOV     r0, r1                  ; Copy icon handle
        ADD     r1, r11, #filter_icon   ; Pointer to icon block
        BL      filter_icon_replace     ; Replace the icon
icons_next$l
        LDMFD   r13!, {r0-r2}           ; Restore registers
        ADD     r0, r0, #Wimp_Icon      ; Pointer to next icon details
        ADD     r1, r1, #1              ; Increment icon number
        B       icons_loop$l            ; Loop for next icon
icons_done$l
        MOV     r0, r11                 ; Copy task record pointer
        BL      filter_close            ; Close swap file
        MOV     r0, #0                  ; New size of slot is zero
        MOV     r1, #-1                 ; Do not change next slot
        SWI     XWimp_SlotSize          ; Remove the wimpslot
        MOV     r0, #filter_state_frozen; Change to next state for task
        STR     r0, [r11, #filter_state]; Set the current state of this task
        B       exit$l                  ; Exit from the filter
fail$l  ADRL    r0, err_fsf             ; Pointer to error token
        BL      filter_error            ; Display the error message
        BL      filter_release_swi      ; Release SWI vector
        MOV     r0, #0                  ; Value to clear pointer with
        STR     r0, filter_task_record  ; Clear task record pointer
        LDR     r0, [r11, #filter_task_handle]; Get task handle
        BL      filter_stop             ; Remove the filters for this task
        RTSS                            ; Return from filter

        ; Dummy window definition to enumerate windows
win$l   &       0                       ; Minimum visible area x coordinate
        &       0                       ; Minimum visible area y coordinate
        &       0                       ; Maximum visible area x coordinate
        &       0                       ; Maximum visible area y coordinate
        &       0                       ; Scroll x offset
        &       0                       ; Scroll y offset
        &       -2                      ; Open window at bottom
        &       Wimp_WindowAutoRedraw :OR: Wimp_WindowNoBounds; Window flags
        =       Wimp_ColourTransparent :AND: &FF; Title foreground
        =       Wimp_ColourTransparent :AND: &FF; Title background
        =       Wimp_ColourTransparent :AND: &FF; Work area foreground
        =       Wimp_ColourTransparent :AND: &FF; Work area background
        =       Wimp_ColourTransparent :AND: &FF; Scroll bar outer colour
        =       Wimp_ColourTransparent :AND: &FF; Scroll bar inner colour
        =       Wimp_ColourTransparent :AND: &FF; Title highlight colour
        =       0
        &       0                       ; Work area minimum x coordinate
        &       0                       ; Work area minimum y coordinate
        &       0                       ; Work area maximum x coordinate
        &       0                       ; Work area maximum y coordinate
        &       0                       ; Title bar icon flags
        &       0                       ; Work area flags
        &       1                       ; Use wimp sprite area
        &       0                       ; Minimum window size
        %       12                      ; Title data
        &       0                       ; Number of icons

        ;   Parameters  : r0    - Event reason code.
        ;                 r1    - Pointer to event block.
        ;                 r2    - Task handle.
        ;                 r12   - Value of r2 when filter registered.
        ;   Returns     : r0    - Modified event code, or -1 to claim.
        ;                 r1-r2 - Preserved.
        ;   Description : Filter called when WIMP is about to return from
        ;                 Wimp_Poll to the task.
filter_post
        LocalLabels
        JSR     "r1-r3, r11-r12"        ; Stack registers
        MOV     r11, r12                ; Copy pointer to task record
        ADRL    r12, workspace_ptr      ; Pointer to workspace pointer
        LDR     r12, [r12]              ; Obtain pointer to workspace
        STR     r11, filter_task_record ; Set the task record pointer
        TEQ     r0, #Wimp_UserMessage   ; Is it a user message
        BNE     not$l                   ; Skip next bit if not
        LDR     r2, [r1, #Wimp_MessageHeader_size]; Read size of message
        LDR     r3, filter_event + Wimp_MessageHeader_size; Get special msg size
        TEQ     r2, r3                  ; Is size correct for special message
        BNE     not$l                   ; Skip next bit if not
        LDR     r2, [r1, #Wimp_MessageHeader_action]; Get message number
        LDR     r3, filter_event + Wimp_MessageHeader_action; Get special action
        TEQ     r2, r3                  ; Is action correct for special message
        BNE     not$l                   ; Skip next bit if not
        LDR     r2, [r1, #Wimp_Message_data]; Get message data
        LDR     r3, filter_event + Wimp_Message_data; Get special data
        TEQ     r2, r3                  ; Is data correct for special message
        BNE     not$l                   ; Skip next bit if not
        LDR     r2, [r11, #filter_state]; Get the state of this task
        TEQ     r2, #filter_state_move_block; Should SWI be intercepted
        BLEQ    filter_install_swi      ; Claim SWI vector
        LDR     r2, = swi_chunk         ; Convert to a safe message
        STR     r2, [r1, #Wimp_MessageHeader_action]; Set new message number
        RTSS                            ; Return from filter

        ; Handle other normal events and messages
not$l   LDR     r2, [r11, #filter_state]; Get the state of this task
        TEQ     r2, #filter_state_frozen; Is the task completely frozen
        BEQ     froze$l                 ; Handle some other events when frozen
        TEQ     r2, #filter_state_thaw  ; or waiting to be thawed
        RTSS NE                         ; Return from filter if not
exit$l  MOV     r0, #-1                 ; Stop event being passed to task
        RTSS                            ; Return from filter
froze$l TEQ     r0, #Wimp_MouseClick    ; Is it a mouse click
        BEQ     click$l                 ; Branch if it is
        TEQ     r0, #Wimp_UserMessage   ; Is it a user message
        TEQNE   r0, #Wimp_UserMessageRecorded; or a recorded message
        BEQ     msg$l                   ; Branch if it is
        B       exit$l                  ; Do not bother with other events
click$l LDR     r0, [r11, #filter_task_handle]; Restore task handle
        BL      filter_end              ; Attempt to thaw task
        B       exit$l                  ; Exit filter
msg$l   LDR     r2, [r1, #Wimp_MessageHeader_action]; Get message number
        TEQ     r2, #Message_Prequit    ; Is it a pre quit message
        BEQ     pre$l                   ; Branch if it is
        B       exit$l                  ; Exit filter
pre$l   BL      query$l                 ; Check if user wishes to continue
        BVS     exit$l                  ; Exit if error produced
        TEQ     r2, #5                  ; The only button that continues
        SWIEQ   XOS_Exit                ; Exit the task if shutdown selected
        BEQ     exit$l                  ; Continue shutdown
        STMFD   r13!, {r0}              ; Stack registers
        TEQ     r2, #3                  ; Should the task be restored
        LDR     r0, [r11, #filter_task_handle]; Restore task handle
        BLEQ    filter_end              ; Restore task if required
        LDMFD   r13!, {r0}              ; Restore registers
        MOV     r0, #Wimp_UserMessageAcknowledge; Event to acknowledge message
        LDR     r2, [r1, #Wimp_MessageHeader_my_ref]; Get senders reference
        STR     r2, [r1, #Wimp_MessageHeader_your_ref]; Copy reference value
        LDR     r2, [r1, #Wimp_MessageHeader_sender]; Task handle of sender
        SWI     XWimp_SendMessage       ; Object to the potential closedown
        B       exit$l                  ; Exit filter
query$l JSR     "r0-r1, r3-r5"          ; Stack registers
        ADRL    r0, err_fpq             ; Pointer to error block
        ADR     r1, ws_message          ; Pointer to messages control block
        MOV     r2, #0                  ; Use internal buffer
        SWI     XMessageTrans_ErrorLookup; Lookup the error text
        LDR     r1, = Wimp_ErrorBoxGivenCategory \
                :OR: Wimp_ErrorBoxHighlightCancel \
                :OR: (Wimp_ErrorBoxCategoryInfo << Wimp_ErrorBoxCategoryShift)
        ADRL    r2, title               ; Module title
        ADR     r3, sprite$l            ; Sprite to use
        MOV     r4, #1                  ; Use WIMP sprite pool
        ADR     r5, buttons$l           ; Pointer to text for buttons
        SWI     XWimp_ReportErrorByCategory; Report the error
        MOV     r2, r1                  ; Copy button selected
        RTS                             ; Return from subroutine

        ; Special details for error window
sprite$l
        =       "!virtualis", 0         ; Sprite to use in error window
buttons$l
        =       "Restore,Abort,Shutdown", 0; Buttons to display
        ALIGN

        ;   Parameters  : r0    - Handle of icon to replace.
        ;                 r1    - Pointer to icon block.
        ;   Returns     : None
        ;   Description : Delete the specified icon, and attempt to create a
        ;                 new icon with the same handle in at the same position
        ;                 on the icon-bar.
filter_icon_replace
        LocalLabels
        JSR     "r0-r2, r9-r11"         ; Stack registers
        MOV     r11, r0                 ; Copy original icon handle
        MOV     r10, r1                 ; Copy pointer to icon data
        ADR     r1, dummy$l             ; Pointer to icon data
        SWI     XWimp_CreateIcon        ; Create dummy icon to left of original
        RTE VS                          ; Exit if error produced
        MOV     r9, r0                  ; Copy dummy icon handle
        MOV     r0, r11                 ; Copy handle of icon to delete
        BL      del$l                   ; Delete original icon
        RTE VS                          ; Exit if error produced
        SUB     r1, r10, #4             ; Pointer to icon data
        LDR     r10, [r1]               ; Read current value of first word
        MOV     r0, #Wimp_IconBarRightRelative; Create to right of dummy icon
        STR     r0, [r1]                ; Store position for icon
        MOV     r0, r9                  ; Copy dummy icon handle
        MOV     r2, r11                 ; Copy original icon handle
        BL      new$l                   ; Create the new icon
        STR     r10, [r1]               ; Restore first word
        RTE VS                          ; Exit if error produced
        MOV     r0, r9                  ; Copy dummy icon handle
        BL      del$l                   ; Delete the dummy icon
        RTE VS                          ; Exit if error produced
        RTS                             ; Return from subroutine

        ; Create the new icon
new$l   JSR     "r0-r3"                 ; Stack registers
        MOV     r3, r13                 ; Copy curent stack pointer
loop$l  SWI     XWimp_CreateIcon        ; Create replacement icon
        MOVVS   r13, r3                 ; Restore stack pointer if error
        RTE VS                          ; Exit if error produced
        TEQ     r0, r2                  ; Check if correct icon handle
        BEQ     done$l                  ; Skip next bit if correct
        STMFD   r13!, {r0}              ; Stack bad icon handle
        B       loop$l                  ; Loop for next icon to create
done$l  TEQ     r3, r13                 ; Check if any extra icons created
        RTS EQ                          ; Exit if finished
        LDMFD   r13!, {r0}              ; Restore next bad icon handle
        BL      del$l                   ; Delete this icon
        B       done$l                  ; Loop for next icon to delete

        ; Delete a specified icon
del$l   JSR     "r0-r1"                 ; Stack registers
        SUB     r13, r13, #8            ; Reserve two words of stack space
        MOV     r1, r13                 ; Copy pointer to block
        STR     r0, [r1, #4]            ; Store dummy icon handle
        MOV     r0, #Wimp_IconBar       ; Handle of icon bar
        STR     r0, [r1]                ; Store icon bar window handle
        SWI     XWimp_DeleteIcon        ; Delete the dummy icon
        ADD     r13, r13, #8            ; Restore stack pointer
        RTE VS                          ; Exit if error produced
        RTS                             ; Return from subroutine

        ; Dummy icon data
dummy$l &       Wimp_IconBarLeftRelative; Create to the left of icon
        &       0                       ; Minimum x coordinate
        &       0                       ; Minimum y coordinate
        &       0                       ; Maximum x coordinate
        &       0                       ; Maximum y coordinate
        &       0                       ; No text or sprite
        %       12                      ; 12 bytes of dummy data

        ;   Parameters  : r0    - Pointer to task record.
        ;   Returns     : r0    - File handle, or 0 if unable to open file.
        ;   Description : Open the swap file for update.
filter_open
        LocalLabels
        JSR     "r1-r2, r11"            ; Stack registers
        MOV     r11, r0                 ; Copy task record pointer
        LDR     r0, [r11, #filter_file_handle]; Read file handle
        TEQ     r0, #0                  ; Check if it is valid
        RTSS NE                         ; Return if file is already open
        MOV     r0, #OSFind_Openup :OR: OSFind_ErrorIfDir :OR: OSFind_NoPath
        ADD     r1, r11, #filter_file_name; Pointer to file name
        MOV     r2, #0                  ; Do not use a path string
        SWI     XOS_Find                ; Attempt to open the existing file
        MOVVS   r0, #0                  ; Clear handle if error produced
        STR     r0, [r11, #filter_file_handle]; Store the new file handle
        RTSS                            ; Return from subroutine

        ;   Parameters  : r0    - Pointer to task record.
        ;   Returns     : None
        ;   Description : Close the swap file if it is open.
filter_close
        LocalLabels
        JSR     "r0-r1, r11"            ; Stack registers
        MOV     r11, r0                 ; Copy task record pointer
        LDR     r1, [r11, #filter_file_handle]; Read file handle
        TEQ     r1, #0                  ; Check if it is valid
        RTSS EQ                         ; Exit if not
        MOV     r0, #OSFind_Close       ; Reason code to close file
        SWI     XOS_Find                ; Close the file
        MOV     r0, #0                  ; Value to clear handle with
        STR     r0, [r11, #filter_file_handle]; Store cleared file handle
        RTSS                            ; Return from subroutine

        ;   Parameters  : None
        ;   Returns     : None
        ;   Description : Install the SWI handler.
filter_install_swi
        LocalLabels
        JSR     "r0-r2"                 ; Stack registers
        LDR     r0, previous_swi_handler; Check if already claimed
        TEQ     r0, #0                  ; Is it claimed
        RTSS NE                         ; Exit if it is
        LDR     r0, = OSClaimProcessorVector_Alloc :OR: Vector_SWI
        ADR     r1, filter_swi_handler  ; Pointer to replacement handler
        SWI     XOS_ClaimProcessorVector; Claim the Data abort vector
        RTE VS                          ; Exit without wiping over r0
        STR     r1, previous_swi_handler; Store pointer to previous owner
        RTS                             ; Return from subroutine

        ;   Parameters  : None
        ;   Returns     : None
        ;   Description : Release the SWI handler.
filter_release_swi
        LocalLabels
        JSR     "r0-r2"                 ; Stack registers
        LDR     r0, = OSClaimProcessorVector_Free :OR: Vector_SWI
        LDR     r1, previous_swi_handler; Pointer to previous owner
        TEQ     r1, #0                  ; Is vector claimed
        RTSS EQ                         ; Exit if not
        ADR     r2, filter_swi_handler  ; Handler to remove
        SWI     XOS_ClaimProcessorVector; Release the Data abort vector
        RTE VS                          ; Exit without wiping over r0
        MOV     r0, #0                  ; Value to clear flag with
        STR     r0, previous_swi_handler; Clear claimed flag
        RTS                             ; Return from subroutine

        ;   Parameters  : r14_svc   - r15 when SWI occurred.
        ;                 spsr_svc  - CPSR when abort occurred.
        ;   Returns     : Registers updated with SWI return values.
        ;   Description : The SWI handler. This is entered in SVC mode,
        ;                 with interrupts disabled.
filter_swi_handler
        LocalLabels
        STMFD   r13!, {r4-r5, r12}      ; Stack registers
        BIC     r4, r14, #&FC000003     ; Clear flags to get address
        LDR     r4, [r4, #-4]           ; Read the SWI instruction
        BIC     r4, r4, #&FF000000      ; Clear the instruction and condition
        BIC     r4, r4, #&00020000      ; Clear X bit of SWI number
        LDR     r5, = Wimp_Poll         ; First SWI to check against
        TEQ     r4, r5                  ; Is it Wimp_Poll
        LDR     r5, = Wimp_PollIdle     ; Second SWI to check against
        TEQNE   r4, r5                  ; Is it Wimp_PollIdle
        BNE     exit$l                  ; Pass on unmodified if no match
        LDR     r4, filter_task_record  ; Get pointer to record for this task
        TEQ     r4, #0                  ; Is it a valid pointer
        BEQ     exit$l                  ; Pass on unmodified if not
        ADRL    r12, workspace_ptr      ; Pointer to workspace pointer
        LDR     r12, [r12]              ; Pointer to workspace
        LDR     r5, [r4, #filter_state] ; Check state of task
        TEQ     r5, #filter_state_move_block; Is it the initial state
        BNE     exit$l                  ; Pass on unmodified if not
        MOV     r5, #filter_state_close_task; Select the next task state
        STR     r5, [r4, #filter_state] ; Store the new state
        STR     r0, [r4, #filter_mask]  ; Store event mask
        STR     r1, [r4, #filter_ptr]   ; Store poll block pointer
        STR     r2, [r4, #filter_time]  ; Etore earliest to return
        ADD     r1, r4, #filter_block   ; Pointer to new poll block
        MRS     r5, SPSR                ; Get stored status register
        TST     r5, #&1F                ; Check if stored mode if User26
        ADD     r5, r4, #filter_sp      ; Pointer to store for stack pointer
        STRNE   r13, [r5]               ; Store SVC26 mode stack pointer
        STMEQIA r5, {r13}^              ; Store user mode stack pointer
        ADRL    r5, ws_filter_stack_end ; Pointer to new stack
        STR     r5, [r4, #filter_pc]    ; Store new stack pointer
        ADD     r5, r4, #filter_pc      ; Pointer to new stack pointer
        LDMEQIA r5, {r13}^              ; Read new stack pointer for User26
        STR     r14, [r4, #filter_pc]   ; Store instruction pointer
        ADR     r4, fake$l + 4          ; Pointer to fake poll loop
        AND     r14, r14, #&FC000003    ; Only keep flags
        ORR     r14, r14, r4            ; Modified instruction pointer
exit$l  LDMFD   r13!, {r4-r5, r12}      ; Restore registers
        LDR     pc, previous_swi_handler; Jump to original owner of vector

        ; Entry point for fake Wimp_Poll handler
fake$l  SWI     XWimp_Poll              ; Perform poll instruction
        STMFD   r13!, {r0-r2, r11-r14, pc}; Stack registers
        SUB     r11, r1, #filter_block  ; Pointer to task record
        ADRL    r12, workspace_ptr      ; Pointer to workspace pointer
        LDR     r12, [r12]              ; Obtain pointer to workspace
        LDR     r1, [r11, #filter_mask] ; Get event mask
        STR     r1, [r13]               ; Store mask on stack
        LDR     r1, [r11, #filter_ptr]  ; Get poll block pointer
        STR     r1, [r13, #4]           ; Store poll block pointer on stack
        LDR     r1, [r11, #filter_time] ; Get earliest time to return
        STR     r1, [r13, #4 * 2]       ; Store return time on stack
        LDR     r1, [r11, #filter_sp]   ; Get original stack pointer
        STR     r1, [r13, #4 * 5]       ; Store stack pointer on stack
        LDR     r1, [r11, #filter_pc]   ; Get return address and flags
        SUB     r1, r1, #4              ; Correct return address
        STR     r1, [r13, #4 * 7]       ; Store return address on stack
        LDR     r1, [r11, #filter_state]; Get the task state
        TEQ     r1, #filter_state_thaw  ; Is it time to do something
        BNE     loop$l                  ; Skip next bit if not
        LDR     r0, [r11, #filter_size] ; Read original workspace size
        MOV     r1, #-1                 ; Do not change next slot
        SWI     XWimp_SlotSize          ; Restore the wimpslot
        BVS     error$l                 ; Display the error and continue
        LDR     r1, [r11, #filter_size] ; Read required workspace size
        CMP     r0, r1                  ; Was sufficient memory allocated
        BLT     error$l                 ; Generate an error if not
        MOV     r0, r11                 ; Copy task record pointer
        BL      filter_open             ; Open swap file
        TEQ     r0, #0                  ; Was file opened successfully
        BEQ     error$l                 ; Generate an error if not
        BL      load$l                  ; Reload the task
        BVS     error$l                 ; Display an error if unsuccessful
        LDR     r1, [r11, #filter_icons]; Pointer to icon information
        ADD     r2, r1, #Wimp_WindowInfo_icons; Pointer to first icon
        LDR     r0, [r1, #Wimp_WindowInfo_icon_count]; Get number of icons
        MOV     r1, #0                  ; First icon is number 0
icons_loop$l
        SUBS    r0, r0, #1              ; Decrement icon count
        BMI     icons_done$l            ; Exit loop when finished
        STMFD   r13!, {r0-r3}           ; Stack registers
        LDR     r0, [r2, #Wimp_Icon_flags]; Get icon flags
        TST     r0, #Wimp_IconDeleted   ; Has icon been deleted
        BNE     icons_next$l            ; Skip next bit if it has
        MOV     r0, #Wimp_IconBar       ; Handle of icon bar
        BL      filter_handle           ; Find task handle for icon
        LDR     r3, [r11, #filter_task_handle]; Get current task handle
        TEQ     r0, r3                  ; Does handle match
        BNE     icons_next$l            ; Skip next bit if not
        MOV     r0, r1                  ; Copy icon handle
        MOV     r1, r2                  ; Copy icon data pointer
        BL      filter_icon_replace     ; Replace the icon
icons_next$l
        LDMFD   r13!, {r0-r3}           ; Restore registers
        ADD     r2, r2, #Wimp_Icon      ; Pointer to next icon details
        ADD     r1, r1, #1              ; Increment icon number
        B       icons_loop$l            ; Loop for next icon
icons_done$l
        MOV     r0, #OSModule_Free      ; Free entry code
        LDR     r2, [r11, #filter_icons]; Pointer to icon information
        BL      os_module               ; Release workspace
        LDR     r0, [r11, #filter_windows]; Number of windows to restore
        MOV     r1, #Wimp_Top           ; Window to open first behind
window_loop$l
        SUBS    r0, r0, #1              ; Decrement number of windows
        BMI     window_done$l           ; Exit loop if finished
        STMFD   r13!, {r0-r4}           ; Stack registers
        LDR     r1, [r11, #filter_size] ; Offset to first window details
        MOV     r3, #Wimp_WindowState   ; Size of data to read
        MLA     r4, r0, r3, r1          ; Calculate file offset for window
        MOV     r0, #OSGBPB_ReadAt      ; Reason code to read from file
        LDR     r1, [r11, #filter_file_handle]; Get swap file handle
        ADRL    r2, ws_string           ; Pointer to buffer
        SWI     XOS_GBPB                ; Read the workspace contents
        LDMFD   r13!, {r0-r4}           ; Restore registers
        BVS     window_done$l           ; Exit loop if error produced
        STMFD   r13!, {r0}              ; Stack registers
        MOV     r0, r1                  ; Copy window handle to open behind
        ADRL    r1, ws_string           ; Pointer to buffer
        STR     r0, [r1, #Wimp_Open_next]; Store window handle to open behind
        LDR     r0, [r1, #Wimp_WindowState_flags]; Get window flags
        TST     r0, #Wimp_WindowBack    ; Should window be at the back
        MOVNE   r0, #Wimp_Bottom        ; Open window at bottom
        STRNE   r0, [r1, #Wimp_Open_next]; Store window handle to open behind
        SWI     XWimp_OpenWindow        ; Open the window
        LDMFD   r13!, {r0}              ; Restore registers
        BVS     window_done$l           ; Exit loop if error produced
        LDR     r1, [r1, #Wimp_Open_w]  ; Get new window handle to open behind
        B       window_loop$l           ; Loop for next window
window_done$l
        STMFD   r13!, {r3-r4}           ; Stack registers
        ADD     r4, r11, #filter_handlers; Pointer to dump for handlers
link$l  LDMIA   r4!, {r0-r3}            ; Read handler details
        CMP     r0, #-1                 ; Is it the end of the list
        BEQ     done$l                  ; Exit loop when finished
        SWI     XOS_ChangeEnvironment   ; Restore previous handler
        B       link$l                  ; Loop for next handler
done$l  LDMFD   r13!, {r3-r4}           ; Restore registers
        MOV     r0, #Wimp_UserMessage   ; Event code for a user message
        ADR     r1, msg$l               ; Pointer to message block
        LDR     r2, [r11, #filter_task_handle]; Read the task handle
        SWI     XWimp_SendMessage       ; Send a mode changed message
        LDR     r0, [r11, #filter_task_handle]; Read the task handle
        BL      filter_stop             ; Remove the filters
        LDMFD   r13, {r0-r2, r11-r14, pc}^; Return to original code
loop$l  ADD     r1, r11, #filter_block  ; Event block pointer
        STR     r1, [r13, #4]           ; Store poll block pointer on stack
        LDMFD   r13!, {r0-r2, r11-r12}  ; Restore registers
        ADD     r13, r13, #4            ; Skip stack pointer on stack
        LDMFD   r13!, {r14}             ; Restore link register
        ADD     r13, r13, #4            ; Skip return address on stack
        B       fake$l                  ; Loop for next poll
error$l ADRL    r0, err_fma             ; Pointer to error block
        BL      filter_error            ; Display the error message
        MOV     r0, #0                  ; Remove the wimpslot again
        MOV     r1, #-1                 ; Do not change next slot
        SWI     XWimp_SlotSize          ; Restore the wimpslot
        MOV     r0, #filter_state_frozen; Previous state of task
        STR     r0, [r11, #filter_state]; Store new task state
        B       loop$l                  ; Loop for another poll

        ; Attempt to reload the task
load$l  JSR     "r0-r4"                 ; Stack registers
        MOV     r1, r0                  ; Copy file handle
        MOV     r0, #OSGBPB_ReadAt      ; Reason code to read from file
        MOV     r2, #&8000              ; Read at start of application space
        LDR     r3, [r11, #filter_size] ; Size of workspace to read
        MOV     r4, #0                  ; Read from start of file
        SWI     XOS_GBPB                ; Read the workspace contents
        RTE VS                          ; Exit if error produced
        RTS                             ; Return from subroutine

        ; The message block to send for a faked mode change
msg$l   &       20                      ; Size of message block
        &       0                       ; This will be the task handle of sender
        &       0                       ; This will be my_ref
        &       0                       ; An original message, so no reference
        &       Message_ModeChange      ; Message action

        ;   Parameters  : r0    - Pointer to error token.
        ;   Returns     : None
        ;   Description : Display a specified error message.
filter_error
        LocalLabels
        JSR     "r0-r5"                 ; Stack registers
        ADR     r1, ws_message          ; Pointer to messages control block
        MOV     r2, #0                  ; Use internal buffer
        SWI     XMessageTrans_ErrorLookup; Lookup the error text
        LDR     r1, = Wimp_ErrorBoxGivenCategory \
                :OR: Wimp_ErrorBoxCancelIcon \
                :OR: Wimp_ErrorBoxHighlightCancel \
                :OR: (Wimp_ErrorBoxCategoryInfo << Wimp_ErrorBoxCategoryShift)
        ADRL    r2, title               ; Module title
        ADR     r3, sprite$l            ; Sprite to use
        MOV     r4, #1                  ; Use WIMP sprite pool
        ADR     r5, buttons$l           ; Pointer to text for extra buttons
        SWI     XWimp_ReportErrorByCategory; Report the error
        TEQ     r1, #3                  ; Check if new button was clicked
        SWIEQ   XOS_Exit                ; Exit task if it was
        RTS                             ; Return from subroutine

        ; Special details for error window
sprite$l
        =       "!virtualis", 0         ; Sprite to use in error window
buttons$l
        =       "Kill Task", 0          ; Buttons to display
        ALIGN

        ;   Parameters  : r0    - Window handle.
        ;                 r1    - Icon handle.
        ;   Returns     : r0    - Task handle.
        ;   Description : Find the task handle of a specified window or icon.
filter_handle
        LocalLabels
        JSR     "r1-r3"                 ; Stack registers
        MOV     r2, r0                  ; Copy window handle
        MOV     r3, r1                  ; Copy icon handle
        ADR     r1, event$l             ; Pointer to event block
        MOV     r0, #0                  ; Clear your_ref for first call
        STR     r0, [r1, #Wimp_MessageHeader_your_ref]; Clear your_ref
        MOV     r0, #Wimp_UserMessageAcknowledge; Event type to acknowledge
        SWI     XWimp_SendMessage       ; Find task handle
        RTE VS                          ; Return if error
        MOV     r0, r2                  ; Copy task handle
        RTS                             ; Return from subroutine
event$l &       20                      ; Size of message block
        &       0                       ; This will be the task handle of sender
        &       0                       ; This will be my_ref
        &       0                       ; An original message, so no reference
        &       0                       ; Message action

        ;   Parameters  : None
        ;   Returns     : None
        ;   Description : This should be called in the WIMP environment as a
        ;                 new task.
filter_task
        LocalLabels
        JSR     "r0-r4"                 ; Stack registers
        ADRL    r4, ws_filter_handlers  ; Pointer to default handlers dump
loop$l  LDR     r0, [r4]                ; Read handler number
        CMP     r0, #-1                 ; Is it the end of the list
        RTSS EQ                         ; Exit if it is
        MOV     r1, #0                  ; Read details of current handler
        MOV     r2, #0                  ; Read details of workspace
        MOV     r3, #0                  ; Clear register although not used
        SWI     XOS_ChangeEnvironment   ; Read handler details
        STMIA   r4!, {r0-r3}            ; Store previous handler details
        B       loop$l                  ; Loop for next handler

; A literal pool

        LTORG

; Memory allocation with guard words

        ;   Parameters  : As for OS_Module.
        ;   Returns     : As for OS_Module.
        ;   Description : This is a replacement for SWI XOS_Module that places
        ;                 guard words before and after all memory allocations.
        ;                 These are checked before each memory block is
        ;                 deallocated, and on demand.
os_module
        LocalLabels
        JSR     ""                      ; Stack registers
        TEQ     r0, #OSModule_Alloc     ; Is it a memory allocation request
        BEQ     alloc$l                 ; Jump to allocation handler if it is
        TEQ     r0, #OSModule_Free      ; Is it a memory deallocation request
        BEQ     free$l                  ; Jump to deallocation handler if it is
        SWI     XOS_Module              ; Otherwise pass on to OS_Module
        RTS                             ; Return from subroutine
alloc$l ADD     r3, r3, #Int * 3        ; Increase the size of the block
        SWI     XOS_Module              ; Attempt to allocate the memory
        SUB     r3, r3, #Int * 3        ; Restore the original size requested
        RTE VS                          ; Return if an error produced
        STMFD   r13!, {r0-r1, r3}       ; Stack registers
        ADD     r3, r3, #3              ; Prepare to round the block size up
        BIC     r3, r3, #3              ; Round to a word boundary
        STR     r3, [r2], #4            ; Store the size of the block
        LDR     r0, = guard_pre         ; Value of the first guard word
        STR     r0, [r2], #4            ; Store the first guard word
        LDR     r0, = guard_post        ; Value of the first guard word
        ADD     r1, r2, r3              ; Pointer to second guard word
        STR     r0, [r1]                ; Store the second guard word
        LDMFD   r13!, {r0-r1, r3}       ; Restore saved registers
        RTS                             ; Return from subroutine
free$l  BL      os_module_free          ; Check and release the block
        RTS                             ; Return from subroutine

        ;   Parameters  : r3    - Size of the required block.
        ;   Returns     : r2    - Pointer to the block of memory.
        ;   Description : Allocate a block of memory of the requested size.
        ;                 This also clears the block and writes guard words at
        ;                 both ends.
os_module_claim
        LocalLabels
        JSR     "r3"                    ; Stack registers
        ADD     r3, r3, #3 * 4 + 3      ; Increase the size requested
        BIC     r3, r3, #3              ; Round to a word boundary
        SWI     XOS_Module              ; Attempt to allocate the memory
        RTE VS                          ; Exit if an error was produced
        SUB     r3, r3, #3 * 4          ; Restore the previous block size
        ADD     r2, r2, #2 * 4          ; Skip over the guard words
        BL      guard$l                 ; Write the guard words
        BL      clb$l                   ; Zero the rest of the block
        RTS                             ; Return from subroutine
guard$l JSR     "r0"                    ; Stack registers
        STR     r3, [r2, #-8]           ; Store the size of the block
        LDR     r0, = guard_pre         ; Value of the first guard word
        STR     r0, [r2, #-4]           ; Store the first guard word
        LDR     r0, = guard_post        ; Value of the first guard word
        STR     r0, [r2, r3]            ; Store the second guard word
        RTSS                            ; Return from subroutine
clb$l   JSR     "r0, r3"                ; Stack registers
        MOV     r0, #0                  ; Value to clear with
clbl$l  SUBS    r3, r3, #4              ; Decrement number of words left
        RTSS MI                         ; Return from subroutine when finished
        STR     r0, [r2, r3]            ; Clear this word
        B       clbl$l                  ; Loop for the next word

        ;   Parameters  : r2    - Pointer to the block of memory to free.
        ;   Returns     : None
        ;   Description : Check and release the specified block of memory.
        ;                 This overwrites the guard words to aid debugging.
os_module_free
        LocalLabels
        JSR     "r2"                    ; Stack registers
        MOV     r0, r2                  ; Copy the pointer to the block
        BL      os_module_check_err     ; Verify the block
        RTE VS                          ; Exit if an error produced
        BL      clear$l                 ; Overwrite the guard words
        MOV     r0, #OSModule_Free      ; Restore the reason code
        SUB     r2, r2, #2 * 4          ; Restore the original pointer
        SWI     XOS_Module              ; Release the memory as usual
        RTS                             ; Return from subroutine
clear$l JSR     "r0-r1"                 ; Stack registers
        LDR     r0, = guard_clear_pre   ; Value to clear first guard word with
        STR     r0, [r2, #-4]           ; Overwrite the first guard word
        LDR     r1, [r2, #-8]           ; Read size of heap block
        LDR     r0, = guard_clear_post  ; Value to clear second guard word with
        STR     r0, [r2, r1]            ; Overwrite the second guard word
        RTSS                            ; Return from subroutine

        ;   Parameters  : r0    - Pointer to the memory block to check.
        ;   Returns     : r0    - Preserved if valid, or cleared if not.
        ;   Description : Check whether the specified pointer is valid.
os_module_check
        LocalLabels
        JSR     "r1-r2"                 ; Stack registers
        LDR     r1, = guard_pre         ; Required first guard word
        LDR     r2, [r0, #-4]           ; Actual value of first guard word
        TEQ     r1, r2                  ; Veify guard word
        BNE     fail$l                  ; Clear the pointer if not valid
        LDR     r1, = guard_post        ; Required second guard word
        LDR     r2, [r0, #-8]           ; Read size of heap block
        LDR     r2, [r0, r2]            ; Actual value of second guard word
        TEQ     r1, r2                  ; Veify guard word
        BNE     fail$l                  ; Clear the pointer if not valid
        RTSS                            ; Return from subroutine
fail$l  MOV     r0, #0                  ; Clear the pointer
        RTSS                            ; Return from subroutine

        ;   Parameters  : r0    - Pointer to the memory block to check.
        ;   Returns     : None
        ;   Description : Check whether the specified pointer is valid. An
        ;                 error is generated if not.
os_module_check_err
        LocalLabels
        JSR     "r1-r2"                 ; Stack registers
        BL      os_module_check         ; Check the pointer
        TEQ     r0, #0                  ; Is the pointer valid
        RTSS NE                         ; Return from subroutine if it is
        [       debug
        ADR     r0, desc$l              ; Pointer to description
        |
        MOV     r0, #0                  ; No description
        ]
        MOV     r1, #0                  ; No location details
        BL      check_fail              ; Display an error message
        ADRL    r0, err_qsm             ; Pointer to error block
        ADR     r1, ws_message          ; Pointer to messages control block
        MOV     r2, #0                  ; Use internal buffer
        SWI     XMessageTrans_ErrorLookup; Lookup the error text
        RTE                             ; Return from subroutine

        [       debug
desc$l  =       "Guard word overwritten", 0
        ALIGN
        ]

; A pseudo-random number generator

        ;   Parameters  : r0    - The maximum number to return + 1.
        ;   Returns     : r0    - The random number.
        ;   Description : Generate a random number in the range 0 to (r0 - 1).
random_range
        LocalLabels
        JSR     "r1-r3"                 ; Stack registers
        MOV     r2, r0                  ; Copy the required range
        LDR     r0, ws_seed_lsb         ; Get the bottom 32 bits of seed
        LDR     r1, ws_seed_msb         ; Get the remaining bit of the seed
        BL      random_generator        ; Generate the new seed
        STR     r0, ws_seed_lsb         ; Store the new bottom 32 bits of seed
        STR     r1, ws_seed_msb         ; Store the new top bit of the seed
        DivRem  r1, r0, r2, r3          ; Get value into range
        RTSS                            ; Return from subroutine

        ;   Parameters  : r0    - The bottom 32 bits of the seed.
        ;                 r1    - The top bit of the seed in bit 0.
        ;   Returns     : r0    - The new bottom 32 bits of the seed.
        ;                 r1    - The new top bit of the seed in bit 0.
        ;   Description : Perform a single interation of the pseudo-random
        ;                 number generator. This is a 33 bit shift generator
        ;                 with taps at bits 20 and 33 from the ObjAsm manual.
random_generator
        LocalLabels
        JSR     "r2"                    ; Stack registers
        TST     r1, r1, LSR#1           ; Copy the top bit into carry flag
        MOVS    r2, r0, RRX             ; 33 bit rotate right
        ADC     r1, r1, r1              ; Carry into bit 0 of r1
        EOR     r2, r2, r0, LSL#12      ; Involved
        EOR     r0, r2, r2, LSR#20      ; Similarly involved
        RTSS                            ; Return from subroutine

; Internal consistency checks

        [       assert

        ;   Parameters  : r0    - A source page pointer.
        ;                 r1    - An optional description of this check.
        ;   Returns     : None
        ;   Description : Check whether the page pointer is valid.
check_assert_source
        LocalLabels
        JSR     "r0"                    ; Stack registers
        BL      check_range_source      ; Check that the pointer is valid
        TEQ     r0, #0                  ; Is the pointer valid
        BEQ     fail$l                  ; Fail if not

        [       debug

        BL      check_detail_source     ; Check the details
        TEQ     r0, #0                  ; Is the pointer valid
        BEQ     fail$l                  ; Fail if not

        ]

        RTSS                            ; Return from subroutine
fail$l
        [       debug

        ADR     r0, desc$l              ; Pointer to description

        |

        MOV     r0, #0                  ; No description

        ]

        BL      check_fail              ; Handle the failure
        RTSS                            ; Return from subroutine

        [       debug

desc$l  =       "Invalid source page pointer", 0
        ALIGN

        ]

        ;   Parameters  : r0    - A virtual area record pointer.
        ;                 r1    - An optional description of this check.
        ;   Returns     : None
        ;   Description : Check whether the area pointer is valid.
check_assert_area
        LocalLabels
        JSR     "r0"                    ; Stack registers
        BL      check_range_area        ; Check that the pointer is valid
        TEQ     r0, #0                  ; Is the pointer valid
        BEQ     fail$l                  ; Fail if not

        [       debug

        BL      check_detail_area       ; Check the details
        TEQ     r0, #0                  ; Is the pointer valid
        BEQ     fail$l                  ; Fail if not

        ]

        RTSS                            ; Return from subroutine
fail$l
        [       debug

        ADR     r0, desc$l              ; Pointer to description

        |

        MOV     r0, #0                  ; No description

        ]

        BL      check_fail              ; Handle the failure
        RTSS                            ; Return from subroutine

        [       debug

desc$l  =       "Invalid virtual dynamic area record pointer", 0
        ALIGN

        ]

        ;   Parameters  : r0    - A virtual array page pointer.
        ;                 r1    - An optional description of this check.
        ;   Returns     : None
        ;   Description : Check whether the page pointer is valid.
check_assert_virtual
        LocalLabels
        JSR     "r0"                    ; Stack registers
        BL      check_range_virtual     ; Check that the pointer is valid
        TEQ     r0, #0                  ; Is the pointer valid
        BEQ     fail$l                  ; Fail if not

        [       debug

        BL      check_detail_virtual    ; Check the details
        TEQ     r0, #0                  ; Is the pointer valid
        BEQ     fail$l                  ; Fail if not

        ]

        RTSS                            ; Return from subroutine
fail$l
        [       debug

        ADR     r0, desc$l              ; Pointer to description

        |

        MOV     r0, #0                  ; No description

        ]

        BL      check_fail              ; Handle the failure
        RTSS                            ; Return from subroutine

        [       debug

desc$l  =       "Invalid virtual page pointer", 0
        ALIGN

        ]

        ]

        ;   Parameters  : None
        ;   Returns     : None
        ;   Description : Perform a complete internal consistency check.
check_all
        LocalLabels
        JSR     "r0-r1"                 ; Stack registers
        LDR     r0, ws_source_array     ; Pointer to source array
        LDR     r1, ws_total_pages      ; Number of pages to check
src$l   SUBS    r1, r1, #1              ; Decrement number of pages to check
        BMI     cont$l                  ; Start next bit if finished
        BL      check_detail_source     ; Check the details of this page
        TEQ     r0, #0                  ; Is the pointer valid
        BEQ     fail$l                  ; Fail if not
        ADD     r0, r0, #source_item    ; Pointer to next
        B       src$l                   ; Loop for the next page
cont$l  LDR     r0, ws_area_ptr         ; Pointer to the first area record
loop$l  TEQ     r0, #0                  ; Is the pointer valid
        RTSS EQ                         ; Exit if finished
        BL      check_detail_area       ; Check this area
        TEQ     r0, #0                  ; Is the area valid
        BEQ     fail$l                  ; Fail if not
        LDR     r0, [r0, #area_next]    ; Pointer to the next area record
        B       loop$l                  ; Loop for the next area
fail$l  SetV                            ; Set error flag
        RTS                             ; Return from subroutine

        ;   Parameters  : None
        ;   Returns     : None
        ;   Description : Perform a complete internal consistency check.
        ;                 A suitable message is displayed if a problem is
        ;                 found.
check_all_debug
        LocalLabels
        JSR     "r0-r1"                 ; Stack registers

        [       debug

        ADDVS   r1, r0, #4              ; Copy error message pointer
        MOVVC   r1, #0                  ; Clear pointer if no error
        ADR     r0, desc$l              ; Pointer to problem description

        |

        MOV     r0, #0                  ; No test description
        MOV     r1, #0                  ; Clear any error pointer

        ]

        ClearFlags                      ; Clear any error flag
        BL      check_all               ; Check for consistency
        BVS     fail$l                  ; Fail if inconsistent
        MOV     r0, #0                  ; Clear description if consistent
        TEQ     r1, #0                  ; Was there any error
        RTSS EQ                         ; Exit if not
fail$l  BL      check_fail              ; Handle the problem
        RTSS                            ; Return from subroutine

        [       debug

        ; Description of the test that failed
desc$l  =       "Internal consistency check failed", 0
        ALIGN

        ]

        ;   Parameters  : r0    - A pointer to a description of the check
        ;                         that failed, or 0 if none.
        ;                 r1    - A pointer to a description of the source
        ;                         of the check, or 0 if none. This can be a
        ;                         pointer to an error message.
        ;   Returns     : None
        ;   Description : Display an error message, and either enter the
        ;                 debugger or wait for a key press.
check_fail
        LocalLabels
        JSR     "r0-r3"                 ; Stack registers
        SWI     XOS_WriteS              ; Display message header
        =       6, 4, 26, 20
        =       "** Message from Virtualise **", 0
        ALIGN
        SWI     XOS_NewLine             ; Start a new line
        SWI     XOS_NewLine             ; Start another new line
        SWI     XOS_WriteS              ; Continue the header
        =       "A serious error has been detected.", 0
        ALIGN
        SWI     XOS_NewLine             ; Start a new line
        TEQ     r0, #0                  ; Is there a problem description
        BEQ     ref$l                   ; Skip the next bit if not
        SWI     XOS_WriteS              ; Write description prefix
        =       "Problem:   ", 0
        ALIGN
        SWI     XOS_Write0              ; Display the description
        SWI     XOS_NewLine             ; Start a new line
ref$l   TEQ     r1, #0                  ; Is there any reference
        BEQ     act$l                   ; Skip the next bit if not
        SWI     XOS_WriteS              ; Write reference prefix
        =       "Reference: ", 0
        ALIGN
        MOV     r0, r1                  ; Copy the reference pointer
        SWI     XOS_Write0              ; Display the reference
        SWI     XOS_NewLine             ; Start a new line
act$l   SWI     XOS_NewLine             ; Start another new line
        SWI     XOS_WriteS              ; Display message footer

        [       debug

        =       "Press Escape to exit from the debugger.", 0
        ALIGN
        SWI     XOS_NewLine             ; Start a new line
        MOV     r0, #0                  ; Value to disable debug trace
        STR     r0, ws_debug_enable     ; Disable debug record storage
        MOV     r0, #OS_HandlerExceptionRegisters; Code for exception registers
        MOV     r1, #0                  ; Read address of registers
        MOV     r2, #0                  ; Ignore workspace pointer
        MOV     r3, #0                  ; Ignore buffer pointer
        SWI     XOS_ChangeEnvironment   ; Read register dump address
        ADRL    r0, ws_debug_regs       ; Pointer to source buffer
        LDMIA   r0!, {r2-r5}            ; Read r0 to r3
        STMIA   r1!, {r2-r5}            ; Write r0 to r3
        LDMIA   r0!, {r2-r5}            ; Read r4 to r7
        STMIA   r1!, {r2-r5}            ; Write r4 to r7
        LDMIA   r0!, {r2-r5}            ; Read r8 to r11
        STMIA   r1!, {r2-r5}            ; Write r8 to r11
        LDMIA   r0!, {r2-r5}            ; Read r12 to r15
        STMIA   r1!, {r2-r5}            ; Write r12 to r15
        ADR     r0, debug$l             ; Command to start the debugger
        SWI     XOS_CLI                 ; Enter the debugger
        RTSS

debug$l =       "Debug", 0
        ALIGN

        |

        =       "To prevent data loss: Save any work,"
        =       " and reset the computer as soon as possible.", 0
        ALIGN
        SWI     XOS_NewLine             ; Start a new line
        SWI     XOS_WriteS              ; Continue the message
        =       "Press Q to quit the current application,"
        =       " or any other key to continue.", 0
        ALIGN
        SWI     XOS_ReadC               ; Wait for a key press
        TEQ     r0, #'Q'                ; Is it a capital Q
        TEQNE   r0, #'q'                ; Is it a lower case Q
        SWIEQ   XOS_Exit                ; Terminate program if it is
        RTSS                            ; Return from subroutine

        ]

        ;   Parameters  : r0    - A source page pointer.
        ;   Returns     : r0    - Preserved if valid, or cleared if not.
        ;   Description : Check if the page pointer is correctly aligned and
        ;                 lies within the source page array.
check_range_source
        LocalLabels
        JSR     "r1-r4"                 ; Stack registers
        LDR     r1, ws_source_array     ; Start of source record array
        SUBS    r1, r0, r1              ; Offset into array
        BMI     fail$l                  ; Fail if before the start
        MOV     r2, #source_item        ; Size of source page records
        DivRem  r3, r1, r2, r4          ; Calculate record number
        TEQ     r1, #0                  ; Is alignment correct
        BNE     fail$l                  ; Fail if not
        LDR     r1, ws_total_pages      ; Current number of pages
        CMP     r3, r1                  ; Is the page number sensible
        BHS     fail$l                  ; Fail if not
        RTSS                            ; Return from subroutine
fail$l  MOV     r0, #0                  ; Clear the pointer
        RTSS                            ; Return from subroutine

        ;   Parameters  : r0    - A virtual area record pointer.
        ;   Returns     : r0    - Preserved if valid, or cleared if not.
        ;   Description : Check if the virtual area record pointer is valid.
check_range_area
        LocalLabels
        JSR     "r1"                    ; Stack registers
        BL      os_module_check         ; Check whether the pointer is valid
        TEQ     r0, #0                  ; Is the pointer valid
        RTSS EQ                         ; Exit if the pointer is bad
        MOV     r1, r0                  ; Copy virtual area record pointer
        LDR     r0, ws_area_ptr         ; Pointer to first area record pointer
loop$l  TEQ     r0, #0                  ; Have all record been checked
        RTSS EQ                         ; Exit if they have
        TEQ     r0, r1                  ; Do the pointers match
        RTSS EQ                         ; Exit if they do
        LDR     r0, [r0, #area_next]    ; Pointer to the next record
        B       loop$l                  ; Loop for the next record

        ;   Parameters  : r0    - A virtual array page pointer.
        ;   Returns     : r0    - Preserved if valid, or cleared if not.
        ;   Description : Check if the virtual array entry pointer is correctly
        ;                 aligned and lies within a virtual array.
check_range_virtual
        LocalLabels
        JSR     "r1-r5"                 ; Stack registers
        LDR     r1, ws_area_ptr         ; Pointer to first area record pointer
loop$l  TEQ     r1, #0                  ; Have all records been checked
        BEQ     fail$l                  ; Fail if they have
        LDR     r2, [r1, #area_virtual_array]; Virtual array pointer
        SUBS    r2, r0, r2              ; Offset from start of array
        BMI     next$l                  ; Check the next record if before start
        MOV     r3, #virtual_item       ; Size of the page records
        DivRem  r4, r2, r3, r5          ; Calculate record number
        TEQ     r2, #0                  ; Is alignment correct
        BNE     next$l                  ; Check the next record if not
        LDR     r2, [r1, #area_total_pages]; Current number of pages
        CMP     r4, r2                  ; Is the page number sensible
        BHS     next$l                  ; Check the next record if not
        MOV     r2, r0                  ; Store the page pointer
        LDR     r0, [r1, #area_virtual_array]; Virtual array pointer
        BL      os_module_check         ; Check whether the pointer is valid
        TEQ     r0, #0                  ; Is the pointer valid
        MOVNE   r0, r2                  ; Restore the original pointer
        RTSS                            ; Return from subroutine
next$l  LDR     r1, [r1, #area_next]    ; Pointer to the next record
        B       loop$l                  ; Loop for the next record
fail$l  MOV     r0, #0                  ; Clear pointer
        RTSS                            ; Return from subroutine

        ;   Parameters  : r0    - A source page pointer.
        ;   Returns     : r0    - Preserved if valid, or cleared if not.
        ;   Description : Check if the details of the source page record are
        ;                 valid. No check is made whether the page pointer
        ;                 itself is valid.
check_detail_source
        LocalLabels
        JSR     "r0-r4"                 ; Stack registers
        MOV     r1, r0                  ; Copy source page pointer.
        LDR     r0, [r1, #source_area]  ; Source area pointer
        TEQ     r0, #0                  ; Is the page used
        BEQ     not$l                   ; Skip the next bit if not
        BL      check_range_area        ; Check that the pointer is valid
        TEQ     r0, #0                  ; Is the pointer valid
        BEQ     fail$l                  ; Fail if not
        LDR     r2, [r1, #source_page]  ; Get page number offset
        CMP     r2, #0                  ; Is the page number positive
        BLT     fail$l                  ; Fail if not
        LDR     r3, [r0, #area_total_pages]; Get number of pages
        CMP     r2, r3                  ; Is page number in range
        BHS     fail$l                  ; Fail if not
        LDR     r3, [r0, #area_virtual_array]; Virtual array pointer
        MOV     r4, #virtual_item       ; Size of each record
        MLA     r3, r2, r4, r3          ; Calculate array entry pointer
        LDR     r3, [r3, #virtual_ptr]  ; Get virtual array entry
        BIC     r4, r3, #virtual_flags_mask; Clear flags
        TEQ     r4, r1                  ; Check that pointer is correct
        BNE     fail$l                  ; Fail if not
        TST     r3, #virtual_flags_paged; Is page swapped in
        BEQ     not$l                   ; Use original address if not
        LDR     r0, [r0, #area_base]    ; Base address of dynamic area
        LDR     r3, ws_log_page_size    ; Log of page size
        ADD     r0, r0, r2, LSL r3      ; Calculate expected address
        B       done$l                  ; Skip the next bit
not$l   LDR     r0, [r1, #source_ptr]   ; Original address of the page
done$l  MOV     r2, r0                  ; Copy expected address pointer
        ADRL    r0, ws_mem_map_request  ; Pointer to request list
        LDR     r1, [r1, #source_number]; Get the physical page number
        STR     r1, [r0, #OS_MemMapRequest_page_no]; Set the physical page
        MOV     r1, #-1                 ; Request list terminator
        STR     r1, [r0, #OS_MemMapRequest]; Terminate the request list
        SWI     XOS_ReadMemMapEntries   ; Read current address and protection
        BVS     fail$l                  ; Fail if unable to read address
        LDR     r0, [r0, #OS_MemMapRequest_map]; Get logical address
        TEQ     r0, r2                  ; Check that address is correct
        BNE     fail$l                  ; Fail if incorrect
        RTSS                            ; Return from subroutine
fail$l  MOV     r0, #0                  ; Clear the pointer
        RTE                             ; Return from subroutine

        ;   Parameters  : r0    - A virtual area record pointer.
        ;   Returns     : r0    - Preserved if valid, or cleared if not.
        ;   Description : Check a complete virtual area. No check is made
        ;                 whether the area pointer itself is valid.
check_detail_area
        LocalLabels
        JSR     "r0-r1"                 ; Stack registers
        BL      os_module_check         ; Check whether the pointer is valid
        TEQ     r0, #0                  ; Is the pointer valid
        RTE EQ                          ; Exit if the pointer is bad
        LDR     r1, [r0, #area_total_pages]; Number of pages
        LDR     r0, [r0, #area_virtual_array]; Pointer to virtual array
        BL      os_module_check         ; Check whether the pointer is valid
        TEQ     r0, #0                  ; Is the pointer valid
        RTE EQ                          ; Exit if the pointer is bad
loop$l  SUBS    r1, r1, #1              ; Decrement number of pages
        RTSS MI                         ; Return from subroutine if done
        BL      check_detail_virtual    ; Check the page entry
        TEQ     r0, #0                  ; Was the entry valid
        RTE EQ                          ; Exit if not
        ADD     r0, r0, #virtual_item   ; Advance to the next page
        B       loop$l                  ; Loop for the next virtual page

        ;   Parameters  : r0    - A virtual array page pointer.
        ;   Returns     : r0    - Preserved if valid, or cleared if not.
        ;   Description : Check if the details of the virtual array entry are
        ;                 valid. No check is made whether the pointer itself
        ;                 is valid.
check_detail_virtual
        LocalLabels
        JSR     "r0-r3"                 ; Stack registers
        MOV     r1, r0                  ; Copy virtual array entry
        LDR     r0, [r1, #virtual_ptr]  ; Get page pointer
        BIC     r0, r0, #virtual_flags_mask; Clear flags
        TEQ     r0, #0                  ; Is the page used
        BEQ     not$l                   ; Skip the next bit if not
        BL      check_range_source      ; Validate the pointer
        TEQ     r0, #0                  ; Was the pointer valid
        BEQ     fail$l                  ; Fail if not
        LDR     r3, [r0, #source_page]  ; Get page number within area
        LDR     r0, [r0, #source_area]  ; Get area record pointer
        BL      check_range_area        ; Validate the pointer
        TEQ     r0, #0                  ; Was the pointer valid
        BEQ     fail$l                  ; Fail if not
        LDR     r2, [r0, #area_total_pages]; Get number of pages
        CMP     r3, r2                  ; Is the page number valid
        BHS     fail$l                  ; Fail if not
        LDR     r0, [r0, #area_virtual_array]; Get virtual array pointer
        MOV     r2, #virtual_item       ; Size of virtual array entries
        MLA     r0, r2, r3, r0          ; Pointer to required page
        TEQ     r0, r1                  ; Do the pointers match
        BNE     fail$l                  ; Fail if not
        RTSS                            ; Return from subroutine
not$l   LDR     r0, [r1, #virtual_ptr]  ; Get the flags again
        TST     r0, #virtual_flags_paged; Check paged in flag
        BNE     fail$l                  ; Fail if it is
        RTSS                            ; Return from subroutine
fail$l  MOV     r0, #0                  ; Clear pointer
        RTE                             ; Return from subroutine

        [       debug

; Debugging code

        ;   Parameters  : None
        ;   Returns     : None
        ;   Description : Initialise debugging information.
debug_init
        LocalLabels
        JSR     "r0"                    ; Stack registers
        MOV     r0, #0                  ; Value to clear variables with
        STR     r0, ws_debug_next       ; Number of the next dump
        STR     r0, ws_debug_total      ; No debugging dumps initially
        ADRL    r0, ws_debug_dump       ; Get pointer to start of dump area
        STR     r0, ws_debug_ptr        ; Store initial dump position
        MOV     r0, #1                  ; Value to enable debug trace
        STR     r0, ws_debug_enable     ; Enable debug record storage
        RTS                             ; Return from subroutine

        ;   Parameters  : r0    - Pointer to the command tail.
        ;                 r1    - Number of parameters. This is undefined
        ;                         for a configuration keyword.
        ;                 r12   - Pointer to module private word.
        ;   Returns     : r7-r11    - Preserved.
        ;   Description : Process the command *VirtualDebug.
command_debug
        LocalLabels
        JSR     "r0-r1"                 ; Stack registers
        LDR     r12, [r12]              ; Get pointer to workspace
        SWI     XOS_WriteS              ; Display header text
        =       "Debugging records, starting with most recent:", 0
        ALIGN
        SWI     XOS_NewLine             ; A blank line
        LDR     r0, ws_debug_next       ; Number of record to use next
        LDR     r1, ws_debug_total      ; Number of used records
next$l  SUBS    r1, r1, #1              ; Decrement number remaining
        RTS MI                          ; Return if all done
        SWI     XOS_NewLine             ; Extra blank line
        SUBS    r0, r0, #1              ; Number of next record
        MOVMI   r0, #max_debug_items - 1; Start again at other end
        BL      show$l                  ; Show this record
        B       next$l                  ; Loop for next record

        ; Display a single debug record
show$l  JSR     "r0-r4, r11"            ; Stack registers
        MOV     r1, #debug_item         ; Get size of debug records
        ADRL    r2, ws_debug_dump       ; Get address of the debug dump
        MLA     r11, r0, r1, r2         ; Get pointer to the debug record
        SWI     XOS_WriteS              ; Display start of header
        =       "Debug record '", 0
        ALIGN
        ADD     r0, r11, #debug_name    ; Get pointer to title of record
        SWI     XOS_Write0              ; Display the debug record name
        SWI     XOS_WriteS              ; Display end of header
        =       "' register dump:", 0
        ALIGN
        SWI     XOS_NewLine             ; Start a newline
        MOV     r3, #0                  ; No registers processed yet
        ADD     r4, r11, #debug_regs    ; Pointer to register dump
loop$l  SWI     XOS_WriteI + 'R'        ; Start of register number
        MOV     r0, r3                  ; Copy register number
        ADRL    r1, ws_num_buffer1      ; Pointer to the number buffer
        MOV     r2, #3                  ; Size of buffer
        SWI     XOS_ConvertCardinal1    ; Convert register number
        SWI     XOS_Write0              ; Display register number
        TEQ     r2, #1                  ; Was it one or two characters
        SWINE   XOS_WriteI + ' '        ; Pad with a space if required
        SWI     XOS_WriteS              ; Add the bit between name and value
        =       " = ", 0
        ALIGN
        LDR     r0, [r4], #4            ; Load register value
        ADRL    r1, ws_num_buffer1      ; Pointer to the number buffer
        MOV     r2, #NumberBuffer       ; Size of buffer
        SWI     XOS_ConvertHex8         ; Convert the register value
        SWI     XOS_Write0              ; Display register value
        ADD     r3, r3, #1              ; Increment number of registers
        TST     r3, #3                  ; Is register an end of line
        SWIEQ   XOS_NewLine             ; Start a newline if it is
        SWINE   XOS_WriteI + ' '        ; Otherwise separate with space
        TEQ     r3, #16                 ; Have all registers been processed
        BNE     loop$l                  ; Loop for next register
done$l  SWI     XOS_WriteS              ; Write start of line
        =       "Mode ", 0
        ALIGN
        LDR     r1, [r4], #4            ; Get the CPSR word
        AND     r2, r1, #&1f            ; Get mode bits
        TEQ     r2, #2_10111            ; Is it ABT mode
        MOVEQ   r2, #2_1000             ; Fake ABT mode value
        TEQ     r2, #2_11011            ; Is it UND mode
        MOVEQ   r2, #2_1001             ; Fake UND mode value
        TST     r2, #2_10000            ; Does the value require correction
        ORRNE   r2, r2, #2_100          ; Include correction if required
        AND     r2, r2, #&f             ; Ensure that the value is in range
        ADR     r0, modes$l             ; Pointer to start of descriptions
        ADD     r0, r0, r2, LSL#3       ; Pointer to correct entry
        SWI     XOS_Write0              ; Display mode
        SWI     XOS_WriteS              ; The bit in the middle
        =       " flags set: ", 0
        ALIGN
        TST     r1, #cpsr_N             ; Is negative flag set
        SWINE   XOS_WriteI + 'N'        ; N flag is set
        SWIEQ   XOS_WriteI + 'n'        ; N flag is not set
        TST     r1, #cpsr_Z             ; Is zero flag set
        SWINE   XOS_WriteI + 'Z'        ; Z flag is set
        SWIEQ   XOS_WriteI + 'z'        ; Z flag is not set
        TST     r1, #cpsr_C             ; Is carry flag set
        SWINE   XOS_WriteI + 'C'        ; C flag is set
        SWIEQ   XOS_WriteI + 'c'        ; C flag is not set
        TST     r1, #cpsr_V             ; Is overflow flag set
        SWINE   XOS_WriteI + 'V'        ; V flag is set
        SWIEQ   XOS_WriteI + 'v'        ; V flag is not set
        TST     r1, #cpsr_I             ; Is IRQ disable flag set
        SWINE   XOS_WriteI + 'I'        ; I flag is set
        SWIEQ   XOS_WriteI + 'i'        ; I flag is not set
        TST     r1, #cpsr_F             ; Is FIQ disable flag set
        SWINE   XOS_WriteI + 'F'        ; F flag is set
        SWIEQ   XOS_WriteI + 'f'        ; F flag is not set
        SWI     XOS_NewLine             ; Start a newline
        RTS                             ; Return from subroutine

        ; Mode descriptions
        ALIGN   8
modes$l =       "User26", 0
        ALIGN   8
        =       "FIQ26", 0
        ALIGN   8
        =       "IRQ26", 0
        ALIGN   8
        =       "SVC26", 0
        ALIGN   8
        =       "User", 0
        ALIGN   8
        =       "FIQ", 0
        ALIGN   8
        =       "IRQ", 0
        ALIGN   8
        =       "SVC", 0
        ALIGN   8
        =       "ABT", 0
        ALIGN   8
        =       "UND", 0
        ALIGN

        ; Help text for the command
help_debug
        =       "*VirtualDebug lists all stored debug records."
        =       " It is for internal software devlopment purposes only."
        =       " This command will not be in the final release version.", 13

        ; Syntax message for the command
syntax_debug
        =       "Syntax: *VirtualDebug", 0
        ALIGN

        ;   Parameters  : r0    - Pointer to the command tail.
        ;                 r1    - Number of parameters. This is undefined
        ;                         for a configuration keyword.
        ;                 r12   - Pointer to module private word.
        ;   Returns     : r7-r11    - Preserved.
        ;   Description : Process the command *VirtualDebugStart.
command_debug_start
        LocalLabels
        JSR     ""                      ; Stack registers
        LDR     r12, [r12]              ; Get pointer to workspace
        BL      debug_init              ; Reinitialise record storage
        RTS                             ; Return from subroutine

        ; Help text for the command
help_debug_start
        =       "*VirtualDebugStart clears all stored debug records and"
        =       " enables the addition of new records."
        =       " It is for internal software devlopment purposes only."
        =       " This command will not be in the final release version.", 13

        ; Syntax message for the command
syntax_debug_start
        =       "Syntax: *VirtualDebugStart", 0
        ALIGN

        ;   Parameters  : r0    - Pointer to the command tail.
        ;                 r1    - Number of parameters. This is undefined
        ;                         for a configuration keyword.
        ;                 r12   - Pointer to module private word.
        ;   Returns     : r7-r11    - Preserved.
        ;   Description : Process the command *VirtualDebugStop.
command_debug_stop
        LocalLabels
        JSR     "r0"                    ; Stack registers
        LDR     r12, [r12]              ; Get pointer to workspace
        MOV     r0, #0                  ; Value to disable debug trace
        STR     r0, ws_debug_enable     ; Disable debug record storage
        RTS                             ; Return from subroutine

        ; Help text for the command
help_debug_stop
        =       "*VirtualDebugStop prevents new debug records being stored."
        =       " It is for internal software devlopment purposes only."
        =       " This command will not be in the final release version.", 13

        ; Syntax message for the command
syntax_debug_stop
        =       "Syntax: *VirtualDebugStop", 0
        ALIGN

        ]

        [       pipe

; Tracing code

        ;   Parameters  : None
        ;   Returns     : None
        ;   Description : Open the pipe output file.
pipe_open
        LocalLabels
        JSR     "r0-r2"                 ; Stack registers
        MOV     r0, #OSFind_Openout     ; Reason code to create a new pipe
        ADR     r1, filename$l          ; Name of pipe
        SWI     XOS_Find                ; Open the pipe file
        STR     r0, ws_pipe_handle      ; Store pipe handle
        RTS                             ; Return from subroutine

        ; The filename of the pipe
filename$l
        =       "pipe:$.Virtualise.Trace", 0
        ALIGN

        ;   Parameters  : None
        ;   Returns     : None
        ;   Description : Close the pipe output file.
pipe_close
        LocalLabels
        JSR     "r0, r1"                ; Stack registers
        MOV     r0, #OSFind_Close       ; Reason code to close the pipe
        LDR     r1, ws_pipe_handle      ; Get the pipe handle
        SWI     XOS_Find                ; Close the pipe file
        RTS                             ; Return from subroutine

        ;   Parameters  : r0    - Pointer to the template string to write.
        ;                 r1    - Length of template string.
        ;   Returns     : None
        ;   Description : Write the specified template string to the pipe.
        ;                 Any occurrencies of %0 to %9 are replaced by the
        ;                 appropriate entry from the argument list.
pipe_write
        LocalLabels
        JSR     "r0-r4"                 ; Stack registers
        MOV     r3, r0                  ; Copy template string pointer
        MOV     r4, r1                  ; Copy template string length
        LDR     r0, ws_pipe_output      ; Pointer to end of argument list
        MOV     r1, #0                  ; Null terminator
        STRB    r1, [r0]                ; Terminate argument list
        ADRL    r0, ws_pipe_args        ; Pointer to argument list
        ORR     r0, r0, #1 << 31        ; Set the top bit to prevent appending
        ADRL    r1, ws_pipe_result      ; Pointer to output buffer
        MOV     r2, #String             ; Length of output buffer
        SWI     XOS_SubstituteArgs      ; Perform any required substitution
        RTE VS                          ; Return if error produced
        MOV     r2, r1                  ; Copy pointer to string
        LDR     r1, ws_pipe_handle      ; Get the pipe handle
write$l LDRB    r0, [r2], #1            ; Read a character from the string
        TEQ     r0, #0                  ; Is it the end of the string
        BEQ     done$l                  ; Exit loop if it is
        SWI     XOS_BPut                ; Write the character
        RTE VS                          ; Return if error produced
        B       write$l                 ; Loop for the next character
done$l  MOV     r0, #13                 ; Carriage return character
        SWI     XOS_BPut                ; Write the carriage return character
        RTE VS                          ; Return if error produced
        MOV     r0, #10                 ; Line feed character
        SWI     XOS_BPut                ; Write line feed character
        RTE VS                          ; Return if error produced
        MOV     r0, #OSArgs_Ensure      ; Reason code to flush buffer
        SWI     XOS_Args                ; Flush the buffer
        RTE VS                          ; Return if error produced
        RTS                             ; Return from subroutine

        ]

end

; Various useful types

String              * 256               ; A generic string
NumberBuffer        * 16                ; A number conversion buffer

; Various dynamically allocated structures

        ; Data for individual pages of the physical dynamic area
                    ^ 0
source_number       # Int               ; The physical page number
source_ptr          # Ptr               ; The original address of the page
source_area         # Ptr               ; The current virtual area
source_page         # Int               ; Page number within area
source_locks        # Int               ; How many times has page been locked
source_count        # Int               ; The aged useage count for the page
source_item         * @                 ; End of the structure

        ; Data for dynamic areas with indirected handlers
                    ^ 0
handler_next        # Ptr               ; Pointer to the next handler record
handler_number      # Int               ; The dynamic area number
handler_ptr         # Ptr               ; Pointer to handler routine
handler_r12         # Int               ; Value of r12 used with OS_DynamicArea
handler_ws          # Int               ; Value of r12 to pass to handler
handler_struct      * @                 ; End of the structure

        ; Data for each virtual dynamic area being managed
                    ^ 0
area_next           # Ptr               ; Pointer to the next virtual area
area_number         # Int               ; The number of the area
area_base           # Ptr               ; The base address of the area
area_total_pages    # Int               ; The actual number of logical pages
area_used_pages     # Int               ; The actual number of physical pages
area_max_pages      # Int               ; The maximum number of logical pages
area_access         # Bits              ; The access privileges to be used
area_file_name      # String            ; The swap file name
area_file_handle    # Int               ; The swap file handle
area_virtual_array  # Ptr               ; Information about individual pages
area_struct         * @                 ; End of the structure

        ; Data for task filters
                    ^ 0
filter_next         # Ptr               ; Pointer to next filter record
filter_task_handle  # Wimp_T            ; Task handle
filter_state        # Int               ; Current state of filter
filter_mask         # Int               ; Original poll mask
filter_ptr          # Ptr               ; Original poll block pointer
filter_time         # Int               ; Original time to return
filter_sp           # Ptr               ; Original stack pointer
filter_pc           # Ptr               ; Pointer to original SWI
filter_size         # Int               ; Original workspace size
filter_icon         # Wimp_Icon         ; Icon to use for task
filter_icon_text    # 32                ; Buffer for icon text
filter_icon_validation # 12             ; Buffer for icon validation string
filter_file_name    # String            ; The swap file name
filter_file_handle  # Int               ; The swap file handle
filter_handlers     # Int * 4 * 18      ; Dump for previous handler details
filter_icons        # Ptr               ; Pointer to icon bar details
filter_windows      # Int               ; Number of windows closed
filter_block        # Wimp_Block        ; Poll block for this task
filter_struct       * @                 ; End of the structure

        ; Possible filter states
filter_state_move_block * 0             ; The poll event block needs to move
filter_state_close_task * 1             ; The task is waiting to be shutdown
filter_state_frozen * 2                 ; The task is fully frozen
filter_state_thaw   * 3                 ; Attempt to restore the task

        ; Data for individual pages of each virtual dynamic area being managed
                    ^ 0
virtual_ptr         # Ptr               ; Pointer to associated physical page
virtual_item        * @                 ; End of the structure

        ; Flags for virtual pages, contained in top bits of physical pointer
virtual_flags_mask  * &FC000003         ; Address is in RMA, so only 24 bits
virtual_flags_paged * 1 << 0            ; Is page paged in fully
virtual_flags_used  * 1 << 1            ; Has page been modified

        ; Page replacement policy configuration
policy_flags_mask   * &FF000000         ; Top byte contains flags
policy_flags_reads  * 1<< 24            ; Should multiple page reads be used
policy_flags_writes * 1<< 25            ; Should multiple page writes be used
policy_nfu          * 0                 ; Number of NFU page replacement policy
policy_fifo         * 1                 ; Number of FIFO page replacement policy
policy_random       * 2                 ; Number of random replacement policy

        ; Task status
task_memory         * 0                 ; Task is in memory
task_disc           * 1                 ; Task is on disc
task_change         * 2                 ; Status of task is being changed

        ; Processor types
processor_unknown   * 0                 ; Unrecognised processor
processor_unknownsa * 1                 ; Unrecognised processor (StrongARM)
processor_arm600    * 2                 ; ARM600 processor
processor_arm610    * 3                 ; ARM610 processor
processor_arm700    * 4                 ; ARM700 processor
processor_arm710    * 5                 ; ARM710 processor
processor_arm810    * 6                 ; ARM810 processor
processor_strongarm * 7                 ; StrongARM processor

        ; Processor flags
processor_writeback * 1 << 0            ; Does writeback occur

        [       debug

        ; Constants relating to debugging data
debug_name_size     * 80                ; Maximum size of the entry names
max_debug_items     * 100               ; Number of items to maintain

        ; Debugging information records
                    ^ 0
debug_name          # debug_name_size   ; Name of register dump
debug_regs          # 17 * Int          ; Place to dump all the registers
debug_item          * @

        ]

; Usage of workspace

                    ^ 0, r12
ws_start            * @                 ; Start of workspace

        ; Bits relating to messages files
ws_message_ptr      # Ptr               ; Pointer to memory for messages
ws_message          # MessageTrans_ControlBlock; Message file control block

        ; RISC OS version specific and related details
ws_os_memory_size   # Ptr               ; Pointer to OS store of memory size
ws_os_memory_init   # Int               ; Initial memory size
ws_os_memory_limit  # Int               ; Size limit for maximum size -1
ws_os_swi_table     # Ptr               ; RISC OS SWI table pointer
ws_os_swi_exit      # Ptr               ; Return address from RISC OS SWIs
ws_prv_dynamic_area # Ptr               ; Previous OS_DynamicArea handler
ws_prv_change_area  # Ptr               ; Previous OS_ChangeDynamicArea handler
ws_prv_read_area    # Ptr               ; Previous OS_ReadDynamicArea handler
ws_prv_heap         # Ptr               ; Previous OS_Heap handler
ws_sma_dynamic_area # Int               ; Semaphore for OS_DynamicArea
ws_sma_change_area  # Int               ; Semaphore for OS_ChangeDynamicArea
ws_sma_read_area    # Int               ; Semaphore for OS_ReadDynamicArea
ws_command_flag     # Int               ; Is a *command active
ws_processor        # Int               ; The main processor type

        ; Dynamic areas with indirected handlers
ws_handler_ptr      # Ptr               ; Pointer to first handler record
ws_handler_new_ptr  # Ptr               ; An unlinked intercept record
ws_morph_area       # Int               ; Number of area not to pass on
ws_shrink_grow      # Int               ; Requested size change

        ; The physical dynamic area
ws_dynamic_area     # Int               ; The physical dynamic area number
ws_dynamic_area_ptr # Ptr               ; The physical dynamic area base
ws_max_pages        # Int               ; Maximum number of pages possible
ws_total_pages      # Int               ; The actual number of pages assigned
ws_target_pages     # Int               ; The ideal number of pages assigned
ws_used_pages       # Int               ; Number of pages currently assigned
ws_source_array     # Ptr               ; Pointer to array of page details
ws_circle           # Ptr               ; Pointer to last used page
ws_fifo_count       # Int               ; An incrementing counter for FIFO
ws_page_size        # Int               ; Size of memory pages in bytes
ws_log_page_size    # Int               ; Log base 2 of the page size
ws_pull_count       # Int               ; Counter to time pageing out
ws_replace_policy   # Int               ; The page replacement policy to use

        ; Details of amount of memory to automatically claim
ws_auto_user_frac   # Int               ; Lower limit as a fraction set by user
ws_auto_user_max    # Int               ; Upper limit set by user
ws_auto_user_min    # Int               ; Lower limit derived from fraction
ws_auto_sw_free     # Int               ; Software set minimum free memory
ws_auto_sw_min      # Int               ; Lower limit set by software
ws_auto_sw_keep     # Int               ; Limit to keep set by software
ws_auto_sw_disc     # Int               ; Software set minimum free disc space
ws_auto_sw_max      # Int               ; Upper limit derived from free memory
ws_auto_flag        # Int               ; Is a change in size internal

        ; Virtual memory dynamic areas
ws_area_ptr         # Ptr               ; Pointer to first virtual area
ws_virtual_pages    # Int               ; Maximum number of useful pages
ws_locked_pages     # Int               ; The number of locked pages
ws_virtual_areas    # Int               ; Number of virtual dynamic areas

        ; Task filters
ws_filter_ptr       # Ptr               ; Pointer to first filter record
ws_filter_handlers  # Int * 4 * 18      ; Dump for WIMP handler details
ws_filter_pre       # Ptr               ; Pointer to original pre filter
ws_filter_post      # Ptr               ; Pointer to original post filter
ws_filter_r12       # Ptr               ; R12 value for original filter
ws_filter_stack     # 1024              ; A stack for filtered tasks
ws_filter_stack_end * @                 ; Initial value for stack pointer

        ; General purpose buffers
ws_string           # String            ; A general purpose string
ws_num_buffer1      # NumberBuffer      ; A buffer for number conversions
ws_num_buffer2      # NumberBuffer      ; A buffer for number conversions
ws_num_buffer3      # NumberBuffer      ; A buffer for number conversions
ws_num_buffer4      # NumberBuffer      ; A buffer for number conversions
ws_mem_map_request  # OS_MemMapRequest*2; List for FindMemMapEntries
                    # Bits

        ; Random number seed
ws_seed_lsb         # Int               ; Least significant 32 bits of seed
ws_seed_msb         # Int               ; Most significant bit of the seed

        ; Number of page faults generated by different instructions
ws_page_faults      * @                 ; Start of page faults table
ws_faults_ldr       # Int               ; Number of faults caused by LDRs
ws_faults_str       # Int               ; Number of faults caused by STRs
ws_faults_ldm       # Int               ; Number of faults caused by LDMs
ws_faults_stm       # Int               ; Number of faults caused by STMs
ws_faults_ldc       # Int               ; Number of faults caused by LDCs
ws_faults_stc       # Int               ; Number of faults caused by STCs
ws_faults_swp       # Int               ; Number of faults caused by SWPs

        ; Output pipe
        [       pipe
ws_pipe_handle      # Int               ; Pipe file handle
ws_pipe_args        # String            ; A string to contain the arguments
ws_pipe_result      # String            ; A string to contain the result
ws_pipe_output      # Ptr               ; Current position in argument string
        ]

        ; Debugging information
        [       debug
ws_debug_regs       # Int * 16          ; Registers to copy to debugger
ws_debug_enable     # Int               ; Is debug record storage enabled
ws_debug_total      # Int               ; Number of entries in the dump
ws_debug_next       # Int               ; Number of the last addition to dump
ws_debug_ptr        # Ptr               ; Pointer to next dump position
ws_debug_dump       # debug_item * max_debug_items; Dump of debugging info
        ]

        ; End of workspace
ws_end              * @                 ; End of workspace

        END
