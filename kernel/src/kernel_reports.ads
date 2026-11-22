-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- Reports the kernel owes a process about another: a child's exit (to its
-- parent and to the process manager) and a fault (to the supervisor)
-- (docs/ipc-delivery.md, "Events are state on kernel objects").
--
-- @description
-- A report is kept on its subject (the process it is about) until its
-- recipient takes it, so it is never lost: there is one slot per subject
-- and kind, and the subject's PID is not reused while a report about it is
-- unread (Request_Free defers the release; Take and Close say when to do
-- it). A recipient that has ended can take nothing: Close drops what it
-- was owed. A fault while an earlier one is unread is counted in it.
--
-- The kernel holds one table under a leaf lock (Process.IPC.reportLock).
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package Kernel_Reports with
    SPARK_Mode => On
is
    Process_Count : constant := 256;
    subtype Process is Natural range 0 .. Process_Count - 1;
    No_Process : constant Process := 0;

    type Report_Kind is (Exit_To_Parent, Exit_To_Manager, Fault_To_Supervisor);
    subtype Exit_Kind is Report_Kind range Exit_To_Parent .. Exit_To_Manager;

    -- A report as delivered: an event message's label and words, and (in
    -- the tag's reserved field) how many more faults it stands for.
    Report_Words : constant := 4;
    subtype Word_Count is Natural range 0 .. Report_Words;
    type Word_Array is array (0 .. Report_Words - 1) of Unsigned_64;
    type Report is record
        Label   : Unsigned_32 := 0;
        Length  : Word_Count := 0;
        Words   : Word_Array := (others => 0);
        Further : Unsigned_16 := 0;
    end record;

    type Recipient is record
        Id         : Process := No_Process;
        Generation : Unsigned_64 := 0;
    end record;

    type Slot is record
        Unread : Boolean := False;
        To     : Recipient;
        Value  : Report;
    end record;
    type Slots is array (Report_Kind) of Slot;

    type Subject_State is record
        Reports       : Slots;
        -- The subject's PID is waiting for its last report to be taken.
        Free_Deferred : Boolean := False;
    end record;

    type Openness is record
        Open       : Boolean := False;
        Generation : Unsigned_64 := 0;
    end record;

    type Subject_Table is array (Process) of Subject_State;
    type Open_Table is array (Process) of Openness;

    type Table is record
        Subjects   : Subject_Table;
        Recipients : Open_Table;
    end record;

    function Any_Unread (S : Subject_State) return Boolean is
      (for some K in Report_Kind => S.Reports (K).Unread);

    -- A deferred release always waits for something.
    function Valid (T : Table) return Boolean is
      (for all P in Process =>
         (if T.Subjects (P).Free_Deferred then Any_Unread (T.Subjects (P))));

    function Unread_For (T : Table; R : Process) return Boolean is
      (for some P in Process =>
         (for some K in Report_Kind =>
            T.Subjects (P).Reports (K).Unread and then
            T.Subjects (P).Reports (K).To.Id = R));

    function Accepts (T : Table; To : Recipient) return Boolean is
      (To.Id /= No_Process and then T.Recipients (To.Id).Open and then
       T.Recipients (To.Id).Generation = To.Generation);

    -- Process R (this incarnation) may be sent reports.
    procedure Open (T : in out Table; R : Process; Generation : Unsigned_64)
      with Pre  => Valid (T) and then R /= No_Process,
           Post => Valid (T) and then
                   T.Recipients (R) = (Open => True, Generation => Generation) and then
                   T.Subjects = T.Subjects'Old;

    -- Process R ended: nothing more is sent to it, and what it was owed is
    -- dropped (no one can take it). Released: a subject whose PID may now
    -- be freed, when some is; call again until Released = No_Process.
    procedure Close (T : in out Table; R : Process; Released : out Process)
      with Pre  => Valid (T),
           Post => Valid (T) and then not T.Recipients (R).Open and then
                   (if Released /= No_Process then
                      not Any_Unread (T.Subjects (Released)) and then
                      not T.Subjects (Released).Free_Deferred);

    -- Process R ended, and Close returned No_Process: nothing remains for it.
    function Closed (T : Table; R : Process) return Boolean is
      (not T.Recipients (R).Open and then not Unread_For (T, R));

    -- Keep a report on Subject for To. An exit is kept once per life (its
    -- slot is free: the PID cannot be reused while it is unread). A fault
    -- while one is unread adds to its count. Kept: whether To takes reports.
    procedure Put
      (T : in out Table; Subject : Process; Kind : Report_Kind;
       To : Recipient; Value : Report; Kept : out Boolean)
      with Pre  => Valid (T) and then Subject /= No_Process and then
                   (if Kind in Exit_Kind then not T.Subjects (Subject).Reports (Kind).Unread),
           Post => Valid (T) and then
                   Kept = Accepts (T'Old, To) and then
                   (if Kept then T.Subjects (Subject).Reports (Kind).Unread
                                 and then T.Subjects (Subject).Reports (Kind).To = To);

    -- R takes one of its reports, if any. Released: as for Close.
    procedure Take
      (T : in out Table; R : Process; Value : out Report; Found : out Boolean;
       Released : out Process)
      with Pre  => Valid (T),
           Post => Valid (T) and then
                   (if not Unread_For (T'Old, R) then not Found and then Released = No_Process) and then
                   (if Released /= No_Process then
                      not Any_Unread (T.Subjects (Released)) and then
                      not T.Subjects (Released).Free_Deferred);

    -- Subject retired: may its PID be freed now? If not, it is deferred
    -- until its last report is taken (Take or Close say Released).
    procedure Request_Free
      (T : in out Table; Subject : Process; Free_Now : out Boolean)
      with Pre  => Valid (T) and then Subject /= No_Process,
           Post => Valid (T) and then
                   Free_Now = not Any_Unread (T.Subjects'Old (Subject)) and then
                   (if Free_Now then not T.Subjects (Subject).Free_Deferred
                    else T.Subjects (Subject).Free_Deferred);

end Kernel_Reports;
