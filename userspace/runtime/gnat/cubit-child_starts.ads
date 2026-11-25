------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Start_Child for any program (SUP-001): the one library a supervisor
--  uses to have procmgr start a child with typed launch parameters.
--
--  @description
--  Build a spec (Child, then Give_Value or Give_Class with Add_Fact, ...),
--  then:
--    * Start: a call to procmgr that waits for the receipt until an
--      explicit deadline (never forever). For programs whose calls to
--      procmgr go down the IPC graph.
--    * Submit: the request without waiting; procmgr's receipt arrives as
--      an asynchronous completion carrying Token (Receipt_Of reads it).
--      For the device manager, which procmgr itself calls: it must never
--      block on procmgr (the IPC graph stays a DAG).
--  procmgr answers at once (it never waits for the child's readiness):
--  Started with the child's identity, or Refused with a typed reason.
--  The child's EVENT_CHILD_EXIT reaches the supervisor from the kernel
--  (CuBit.Process_Events, CuBit.Launching.Poll_Exit's way). When the
--  supervisor ends, procmgr stops its children.
--
--  The spec travels in one buffer lent read-only to procmgr on first
--  use: calls are serialized by the caller, and a submitted request must
--  have its receipt before the next Start or Submit.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

with CuBit.Child_Specs;
with CuBit.Launch_Arguments;
with CuBit.Messages;
with CuBit.Process_IDs;
with CuBit.Handles;
with CuBit.Handles.Services;

package CuBit.Child_Starts is
   package CS renames CuBit.Child_Specs;

   type Starter (<>) is private;
   --  From a program's process-manager startup handle.
   function Using (Process_Manager : CuBit.Handles.Services.Process_Manager.Endpoint) return Starter;
   --  From an endpoint to procmgr the caller holds by other means (the
   --  device manager started procmgr and holds its mailbox).
   function Using_Endpoint (Process_Manager : CuBit.Handles.Endpoint) return Starter;

   ---------------------------------------------------------------------------
   --  Building a spec.
   ---------------------------------------------------------------------------
   --  A child running Program, with no parameters yet. Restart Temporary
   --  takes no intensity (Intensity and Window 0); Transient and Permanent
   --  need both (CS.Valid).
   function Child
     (Program    : String;
      Restart    : CS.Restart_Kind;
      Stop_Grace : CS.Grace_Ms;
      Kind       : CS.Child_Kind := CS.Worker;
      Intensity  : CS.Intensity_Count := 0;
      Window     : CS.Window_Ms := 0;
      --  The root stop's stage (PWR-003): a fact the supervisor states.
      Stage      : CS.Stop_Stage := CS.Services) return CS.Spec
   with Pre => Program'Length in 1 .. CS.Maximum_Name_Bytes;

   --  Its parameters: Value, CCL source of the child's parameter record.
   procedure Give_Value (S : in out CS.Spec; Value : String)
   with Pre => Value'Length in 1 .. CS.Maximum_Value_Bytes;

   --  Its parameters from the startup plan's class launch block for Class
   --  (an ACPI ID), plus the facts added with Add_Fact.
   procedure Give_Class (S : in out CS.Spec; Class : String)
   with Pre => Class'Length in 1 .. CS.Maximum_Class_Bytes;

   procedure Add_Fact (S : in out CS.Spec; Field : String; Value : Integer_64)
   with Pre => S.Facts < CS.Maximum_Facts and then Field'Length in 1 .. CS.Maximum_Field_Bytes;

   --  Provider: the process whose endpoint the child's Label is (by its
   --  identity word); No_Provider, the role's registered provider.
   procedure Add_Binding
     (S : in out CS.Spec; Label : String; Badge : Unsigned_64;
      Provider : Unsigned_64 := CS.No_Provider)
   with Pre => S.Bindings < CS.Maximum_Bindings and then Label'Length in 1 .. CS.Maximum_Field_Bytes;

   --  Start the child held: created, not run, until Release (below).
   procedure Hold (S : in out CS.Spec);

   --  Ask for a sink in the receipt: an endpoint to the child, Write and
   --  Grant, badged Badge (device manager only).
   procedure Want_Sink (S : in out CS.Spec; Badge : Unsigned_64)
   with Pre => Badge /= 0;

   procedure Add_Dependency (S : in out CS.Spec; Sibling : CuBit.Process_IDs.Process_ID)
   with Pre => S.Dependencies < CS.Maximum_Dependencies;

   ---------------------------------------------------------------------------
   --  The receipt.
   ---------------------------------------------------------------------------
   type Receipt_Kind is
     (Started,      --  Child is running, supervised by the caller
      Refused,      --  Why (and Failure, for Launch_Refused)
      Not_Sent,     --  the spec is not valid, or the buffer could not be lent
      No_Answer);   --  no receipt by the deadline, or the request ended unanswered

   type Receipt is record
      Kind    : Receipt_Kind := Not_Sent;
      Child   : CuBit.Process_IDs.Process_ID := CuBit.Process_IDs.No_Process;
      Why     : CS.Refusal := CS.Malformed_Request;
      Failure : CuBit.Launch_Arguments.Launch_Failure := CuBit.Launch_Arguments.Spawn_Failed;
      --  The sink a spec asked for, in this process's table (Started
      --  only; the null handle when none came). The caller closes it.
      Sink    : CuBit.Handles.Endpoint := CuBit.Handles.No_Endpoint;
   end record;

   --  Ask and wait for the receipt until Deadline (an absolute monotonic
   --  millisecond).
   procedure Start (Through : Starter; Spec : CS.Spec; Deadline : Unsigned_64; Result : out Receipt);

   --  Ask without waiting; Sent False: nothing was sent (Not_Sent).
   procedure Submit (Through : Starter; Spec : CS.Spec; Token : Unsigned_64; Sent : out Boolean);

   --  The receipt in a completion of a Submit (match its token first).
   function Receipt_Of (Completion : CuBit.Messages.CompletionEntry) return Receipt;

   --  "started pid N" / "refused NOT_GRANTED" ..., for logs.
   function Image (Item : Receipt) return String;

   ---------------------------------------------------------------------------
   --  Releasing a held child (its supervisor only, once).
   ---------------------------------------------------------------------------
   type Release_Kind is (Released, Release_Refused, Release_No_Answer, Release_Not_Sent);
   type Release_Receipt is record
      Kind : Release_Kind := Release_Not_Sent;
      Why  : CS.Release_Refusal := CS.Malformed_Release;
   end record;
   procedure Submit_Release
     (Through : Starter; Child : CuBit.Process_IDs.Process_ID; Decision : CS.Release_Decision;
      Token : Unsigned_64; Sent : out Boolean);
   --  The same, waiting for the answer until Deadline (never forever).
   procedure Release
     (Through : Starter; Child : CuBit.Process_IDs.Process_ID; Decision : CS.Release_Decision;
      Deadline : Unsigned_64; Result : out Release_Receipt);
   function Release_Receipt_Of (Completion : CuBit.Messages.CompletionEntry) return Release_Receipt;
   function Image (Item : Release_Receipt) return String;

private
   type Starter is record
      Service : CuBit.Handles.Endpoint;
   end record;
   function Using (Process_Manager : CuBit.Handles.Services.Process_Manager.Endpoint) return Starter is
     ((Service => CuBit.Handles.Services.Process_Manager.Handle (Process_Manager)));
   function Using_Endpoint (Process_Manager : CuBit.Handles.Endpoint) return Starter is
     ((Service => Process_Manager));
end CuBit.Child_Starts;
