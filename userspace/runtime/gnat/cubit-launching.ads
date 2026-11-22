------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Starting a program from an Ada program (docs/process-arguments.md,
--  docs/self-hosting.md item 4): procmgr's OP_LAUNCH with a launch block
--  (CuBit.Launch_Arguments) and the places the launcher delegates to the
--  child (CuBit.Launch_Grants). The libc's posix_spawn is the C path.
--
--  The program must be in the launcher's launch table (may-launch). The
--  child holds a subset of the launcher's authority: procmgr checks its
--  manifest requests and every delegated place against what the launcher
--  holds, and refuses the launch otherwise.
--
--  The request goes through one buffer lent to procmgr on first use; calls
--  must be serialized by the caller.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

with CuBit.Child_Exits;
with CuBit.Process_IDs;
with CuBit.Control_Events;
with CuBit.Launch_Arguments;
with CuBit.Launch_Authority;
with CuBit.Launch_Grants;
with CuBit.Program_Descriptions;
with CuBit.Service_Replacement;
with CuBit.Outlet_Rings;
with CuBit.Streams;
with CuBit.Memory_Grants;
with CuBit.Messages;

with CuBit.Handles;
with CuBit.Handles.Debug;
with CuBit.Handles.Services;
with CuBit.Launch_Handles;
package CuBit.Launching is

   --  What a program starts programs through: made only from its
   --  process-manager startup handle (the generated Startup function), so
   --  a launch through nothing cannot be written (CAP-003).
   type Launcher (<>) is private;
   function Using (Process_Manager : CuBit.Handles.Services.Process_Manager.Endpoint) return Launcher;
   --  The manifest declares process-manager optional: when the launch had
   --  none, every call answers No_Process_Manager.
   function Using (Process_Manager : CuBit.Handles.Services.Process_Manager.Optional_Endpoint) return Launcher;

   type Launch_Result is
     (Launched,
      No_Process_Manager,   --  the request buffer could not be lent
      Refused);             --  procmgr refused: see Failure

   --  The child's identity (KERN-003): one life, matched exactly by its
   --  exit event; and its launch handle (CAP-012, CuBit.Launch_Handles):
   --  procmgr's endpoint badged for this child, with the rights the launch
   --  gave (Observe, Stop, Attach, Introduce, Debug). The child's kernel
   --  parent is procmgr, its session supervisor's process: the handle is
   --  this program's way to it, never its authority.
   type Child is record
      Process : CuBit.Process_IDs.Process_ID := CuBit.Process_IDs.No_Process;
      Handle  : CuBit.Handles.Endpoint := CuBit.Handles.No_Endpoint;
      Rights  : CuBit.Launch_Handles.Rights := CuBit.Launch_Handles.No_Rights;
   end record;

   --  How long a call through a launch handle waits for procmgr's answer
   --  (it answers at once; this bounds a procmgr that is gone).
   Handle_Answer_Ms : constant := 2_000;

   --  Start Program with the given launch block (Arguments: a finished
   --  Launch_Arguments builder's block, or empty) and delegated places
   --  (Grants: a finished Launch_Grants region, or empty).
   --  Rings: rings this launcher lends the child for its outlets
   --  (Lend_Ring), which it then reads in place
   --  (CuBit.Stream_Regions.Read_Owned).
   procedure Launch
     (Through   : Launcher;
      Program   : String;
      Arguments : CuBit.Launch_Arguments.Block;
      Grants    : CuBit.Launch_Grants.Bytes;
      Started   : out Child;
      Result    : out Launch_Result;
      Failure   : out CuBit.Launch_Arguments.Launch_Failure;
      Rings     : CuBit.Outlet_Rings.Table := (others => <>))
   with Pre => Program'Length in 1 .. CuBit.Launch_Arguments.Maximum_Name_Bytes
               and then Arguments'Length <= CuBit.Launch_Arguments.Maximum_Block_Bytes
               and then Grants'Length <= CuBit.Launch_Grants.Maximum_Bytes;

   --  Launch goes through the program's Launch badge (startup binding
   --  "launch", CAP-012, docs/authority-model.md 7.5) when it has one: the
   --  child is started into the badge's session, with its install approval
   --  within the session's envelope, never this program's authority.
   --  Without a badge procmgr refuses (Not_Granted).
   --  After a Request_Withheld refusal: the startup label of the request
   --  the session withheld.
   function Withheld_Request return String;

   --  A ring for a child's outlet (docs/ccl-launch-parameters.md,
   --  "Launcher-owned outlet rings"): a Stream_Rings region for Pages
   --  declared pages (Stream_Rings.Region_Pages of them) of this process's
   --  memory at Base (a fresh allocation when Base is 0 on entry, else that
   --  region again, page aligned and at least as long), made empty and lent
   --  to procmgr (with grant events: CuBit.Control_Events), which derives the
   --  child's grant at launch. Add the entry (its outlet, Grant) to the
   --  Rings passed to Launch; revoke Reference (CuBit.Memory_Grants.Revoke) once
   --  the run is over, before reusing the pages.
   procedure Lend_Ring
     (Through   : Launcher;
      Pages : Positive;
      Entry_Type : CuBit.Streams.TypeTag;
      Base : in out Unsigned_64; Grant : out Unsigned_64;
      Reference : out CuBit.Memory_Grants.Grant_Reference; Success : out Boolean);

   --  Block until Started ends; Ended is its exit report (procmgr's exit
   --  notice: CuBit.Child_Exits.Is_Exit). Other events that arrive
   --  meanwhile are kept (CuBit.Process_Events). Once it is taken, the
   --  launch handle is let go (Release), here and in Poll_Exit.
   procedure Wait (Started : Child; Ended : out CuBit.Child_Exits.Report);

   --  Whether Started has ended, without blocking (Ended its report). A
   --  program polling several children calls this for each; other
   --  children's exits are kept until asked for (CuBit.Process_Events).
   procedure Poll_Exit
     (Started : Child; Has_Ended : out Boolean; Ended : out CuBit.Child_Exits.Report);

   --  Send Started a control message (docs/ipc-delivery.md, "Control is
   --  per capability") through its launch handle's Stop right: procmgr
   --  sends it with the Control it holds, and the kernel keeps it until
   --  Started reads it. Busy: Started has not read other senders' messages
   --  yet; try again later. Refused: Started has ended, or its handle does
   --  not carry Stop.
   type Control_Result is (Sent, Busy, Refused);
   procedure Send_Control
     (Started : Child; Kind : CuBit.Control_Events.Control_Kind;
      Result : out Control_Result);

   --  End Started after a Stop it did not obey within the launcher's own
   --  deadline (the handle's Stop right; procmgr kills it with the process
   --  authority it holds). Its exit then arrives as usual (Wait, Poll_Exit).
   procedure End_Child (Started : Child; Result : out Launch_Result);

   --  Started's state through its handle (Observe): Running, or how it
   --  ended and its code. Outcome says why not when it is not Done.
   procedure Observe
     (Started : Child; State : out CuBit.Launch_Handles.Child_State;
      Code : out Unsigned_64; Outcome : out CuBit.Launch_Handles.Outcome);

   --  A Debug_Observe session on Started (DBG-001; the handle's Debug
   --  right): procmgr mints it from Started's Control, within the session's
   --  debug envelope, and hands it over. Until_Time: an absolute monotonic
   --  millisecond (CuBit.Messages.Deadline_After).
   procedure Debug
     (Started : Child; Until_Time : Unsigned_64; Session : out CuBit.Handles.Debug.Session;
      Outcome : out CuBit.Launch_Handles.Outcome);

   --  A narrower handle on Started (at most its rights, only those asked),
   --  to give someone else; revoking Started's handle revokes it too.
   procedure Narrow
     (Started : Child; Rights : CuBit.Launch_Handles.Rights; Narrowed : out Child;
      Outcome : out CuBit.Launch_Handles.Outcome);

   --  Let Started's handle go: procmgr revokes it (and every handle
   --  narrowed from it), and this process's copy is closed.
   procedure Release (Started : Child);

   --  The Desktop's "Allow app replacement" switch, told to procmgr
   --  (CuBit.Launch_Authority.Staged_Switch_Operation): the request, which
   --  the Desktop submits to procmgr without waiting (capSubmit; its event
   --  loop never blocks on a peer), and whether procmgr's reply (the
   --  completion's message) confirms it. procmgr refuses every caller but
   --  the registered Desktop.
   function Set_Staged_Launches (Allowed : Boolean) return CuBit.Messages.Message;
   function Staged_Launches_Set (Reply : CuBit.Messages.Message) return Boolean;

   --  This process's own launch table (CuBit.Launch_Authority, procmgr's
   --  OP_LAUNCH_TABLE): Table (1 .. Length), Length 0 when it may start
   --  nothing, when Result = Launched (here: answered).
   procedure Launch_Table
     (Through   : Launcher;
      Table  : out CuBit.Launch_Authority.Table_Bytes;
      Length : out CuBit.Launch_Authority.Table_Length;
      Result : out Launch_Result)
   with Pre => Table'First = 1
               and then Table'Length = CuBit.Launch_Authority.Maximum_Table_Bytes;

   --  The places this process's launcher delegated to it
   --  (OP_DELEGATED_PLACES), as a Launch_Grants region to pass on to a child
   --  it starts: Region (1 .. Length), Length 0 when it was given none,
   --  when Result = Launched (here: answered).
   procedure Delegated_Places
     (Through   : Launcher;
      Region : out CuBit.Launch_Grants.Bytes;
      Length : out CuBit.Launch_Grants.Byte_Count;
      Result : out Launch_Result)
   with Pre => Region'First = 1
               and then Region'Length = CuBit.Launch_Grants.Maximum_Bytes;

   --  A program's .cubit.description descriptor, through procmgr
   --  (OP_PROGRAM_DESCRIPTION): Descriptor (1 .. Length), Length 0 when it
   --  declares none, when Result = Launched (here: answered). Decode it
   --  before use (CuBit.Program_Descriptions).
   procedure Describe
     (Through   : Launcher;
      Program    : String;
      Descriptor : out CuBit.Program_Descriptions.Bytes;
      Length     : out CuBit.Program_Descriptions.Descriptor_Length;
      Result     : out Launch_Result;
      Failure    : out CuBit.Launch_Arguments.Launch_Failure)
   with Pre => Program'Length in 1 .. CuBit.Launch_Arguments.Maximum_Name_Bytes
               and then Descriptor'First = 1
               and then Descriptor'Length = CuBit.Program_Descriptions.Maximum_Descriptor_Bytes;

   --  Switch the startup-plan service Service ("acpi.svc") to Program, a
   --  staged program (a system generation's pin:
   --  "@system/storehouse/executables/<sha256>"), or back to its image's
   --  program when Program is empty (CuBit.Service_Replacement). procmgr
   --  answers when the switch has ended; this waits at most
   --  CuBit.Service_Replacement.Answer_Bound_Ms (Grace, Health) plus
   --  Answer_Margin_Ms, never forever (Timed_Out past that: the switch's
   --  outcome is then unknown here; procmgr's log says it).
   Answer_Margin_Ms : constant := 2_000;
   type Switch_Answer is (Answered, Refused_Switch, Timed_Out, No_Process_Manager_Switch);
   type Switch_Result is record
      Answer   : Switch_Answer := No_Process_Manager_Switch;
      Why      : CuBit.Service_Replacement.Refusal := CuBit.Service_Replacement.Malformed;
      Outcome  : CuBit.Service_Replacement.Outcome := CuBit.Service_Replacement.Rollback_Failed;
      Failure  : CuBit.Service_Replacement.Health_Failure := CuBit.Service_Replacement.Healthy;
      Running  : CuBit.Process_IDs.Process_ID := CuBit.Process_IDs.No_Process;
   end record;
   procedure Switch_Service
     (Through : Launcher; Service : String; Program : String;
      Grace : CuBit.Service_Replacement.Stop_Grace_Ms;
      Health : CuBit.Service_Replacement.Health_Window_Ms;
      Result : out Switch_Result)
   with Pre => Service'Length in 1 .. CuBit.Service_Replacement.Maximum_Service_Name_Bytes
               and then Program'Length <= CuBit.Service_Replacement.Maximum_Program_Bytes;

   --  Ask procmgr to admit Program for Service at the next boot
   --  (CuBit.Service_Replacement.Boot_Stage_Operation: a (replace reboot)
   --  entry's pin). Answered: admitted (nothing was stopped or started);
   --  Refused_Switch: Why says why; Timed_Out: no answer within
   --  Stage_Deadline_Ms.
   Stage_Deadline_Ms : constant := 10_000;
   procedure Stage_Service_For_Boot
     (Through : Launcher; Service : String; Program : String; Result : out Switch_Result)
   with Pre => Service'Length in 1 .. CuBit.Service_Replacement.Maximum_Service_Name_Bytes
               and then Program'Length in 1 .. CuBit.Service_Replacement.Maximum_Program_Bytes;

private
   type Launcher is record
      Service : CuBit.Handles.Endpoint;
   end record;
   function Using (Process_Manager : CuBit.Handles.Services.Process_Manager.Endpoint) return Launcher is
     ((Service => CuBit.Handles.Services.Process_Manager.Handle (Process_Manager)));
   function Using (Process_Manager : CuBit.Handles.Services.Process_Manager.Optional_Endpoint) return Launcher is
     ((Service => (if Process_Manager.Present
                   then CuBit.Handles.Services.Process_Manager.Handle (Process_Manager.Value)
                   else CuBit.Handles.No_Endpoint)));
end CuBit.Launching;
