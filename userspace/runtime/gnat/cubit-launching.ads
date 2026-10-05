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
with CuBit.Launch_Arguments;
with CuBit.Launch_Authority;
with CuBit.Launch_Grants;
with CuBit.Program_Descriptions;
with CuBit.Outlet_Rings;
with CuBit.Streams;
with CuBit.Memory_Grants;

package CuBit.Launching is

   type Launch_Result is
     (Launched,
      No_Process_Manager,   --  the request buffer could not be lent
      Refused);             --  procmgr refused: see Failure

   type Child is record
      Process    : Unsigned_64 := 0;
      Generation : Unsigned_64 := 0;   --  matches its exit event exactly
   end record;

   --  Start Program with the given launch block (Arguments: a finished
   --  Launch_Arguments builder's block, or empty) and delegated places
   --  (Grants: a finished Launch_Grants region, or empty).
   --  Rings: rings this launcher lends the child for its outlets
   --  (Lend_Ring), which it then reads in place (CuBit.Streams.Read_Owned).
   procedure Launch
     (Program   : String;
      Arguments : CuBit.Launch_Arguments.Block;
      Grants    : CuBit.Launch_Grants.Bytes;
      Started   : out Child;
      Result    : out Launch_Result;
      Failure   : out CuBit.Launch_Arguments.Launch_Failure;
      Rings     : CuBit.Outlet_Rings.Table := (others => <>))
   with Pre => Program'Length in 1 .. CuBit.Launch_Arguments.Maximum_Name_Bytes
               and then Arguments'Length <= CuBit.Launch_Arguments.Maximum_Block_Bytes
               and then Grants'Length <= CuBit.Launch_Grants.Maximum_Bytes;

   --  A ring for a child's outlet (docs/ccl-launch-parameters.md,
   --  "Launcher-owned outlet rings"): Pages pages of this process's memory
   --  at Base (a fresh allocation when Base is 0 on entry, else those pages
   --  again, page aligned and at least Pages long), initialized with this
   --  process as its one subscriber and lent to procmgr, which derives the
   --  child's grant at launch. Add the entry (Outlet, Grant) to the Rings
   --  passed to Launch; revoke Reference (CuBit.Memory_Grants.Revoke) once
   --  the run is over, before reusing the pages.
   procedure Lend_Ring
     (Outlet : CuBit.Program_Descriptions.Connector_Index; Pages : Positive;
      Entry_Type : CuBit.Streams.TypeTag;
      Base : in out Unsigned_64; Grant : out Unsigned_64;
      Reference : out CuBit.Memory_Grants.Grant_Reference; Success : out Boolean);

   --  Block until Started ends; Ended is its exit report. Exit events of
   --  other children that arrive meanwhile are kept for Poll_Exit; any
   --  other event is dropped: a program that waits here owns its event
   --  queue.
   procedure Wait (Started : Child; Ended : out CuBit.Child_Exits.Report);

   --  Whether Started has ended, without blocking (Ended its report). A
   --  program polling several children calls this for each; other
   --  children's exits are kept until asked for (at most 32 at a time).
   procedure Poll_Exit
     (Started : Child; Has_Ended : out Boolean; Ended : out CuBit.Child_Exits.Report);

   --  This process's own launch table (CuBit.Launch_Authority, procmgr's
   --  OP_LAUNCH_TABLE): Table (1 .. Length), Length 0 when it may start
   --  nothing, when Result = Launched (here: answered).
   procedure Launch_Table
     (Table  : out CuBit.Launch_Authority.Table_Bytes;
      Length : out CuBit.Launch_Authority.Table_Length;
      Result : out Launch_Result)
   with Pre => Table'First = 1
               and then Table'Length = CuBit.Launch_Authority.Maximum_Table_Bytes;

   --  A program's .cubit.description descriptor, through procmgr
   --  (OP_PROGRAM_DESCRIPTION): Descriptor (1 .. Length), Length 0 when it
   --  declares none, when Result = Launched (here: answered). Decode it
   --  before use (CuBit.Program_Descriptions).
   procedure Describe
     (Program    : String;
      Descriptor : out CuBit.Program_Descriptions.Bytes;
      Length     : out CuBit.Program_Descriptions.Descriptor_Length;
      Result     : out Launch_Result;
      Failure    : out CuBit.Launch_Arguments.Launch_Failure)
   with Pre => Program'Length in 1 .. CuBit.Launch_Arguments.Maximum_Name_Bytes
               and then Descriptor'First = 1
               and then Descriptor'Length = CuBit.Program_Descriptions.Maximum_Descriptor_Bytes;

end CuBit.Launching;
