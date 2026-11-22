------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Process control by handle (CAP-002): spawning, installing a child's
--  startup capabilities, starting, stopping and placing processes, and
--  the device manager's per-child resources.
--
--  @description
--  Every call names the authority it relies on. Process authority is a
--  Process handle naming the target (SPAWN gives the spawner one per child)
--  or the device manager's wildcard root (and procmgr's copy of it).
--  Installing into another process's table needs its Cspace, which only
--  SPAWN creates (one per child) and which works only until the child first
--  runs.
------------------------------------------------------------------------------
pragma Ada_2022;
with System;
with CuBit.Process_IDs;
with CuBit.Startup_Tables;

package CuBit.Handles.Control is

   subtype Process_ID is CuBit.Process_IDs.Process_ID;

   ---------------------------------------------------------------------------
   --  Spawning
   ---------------------------------------------------------------------------
   type Spawned_Child is record
      Child              : Process_ID := CuBit.Process_IDs.No_Process;
      Control            : Process := No_Process;     --  all rights
      Mailbox            : Endpoint := No_Endpoint;   --  untagged, may badge
      Space              : Cspace := No_Cspace;       --  until it first runs
      --  The child's own handles, for its startup table.
      Child_Self         : Foreign_Handle;
      Child_Self_Process : Foreign_Handle;
   end record;

   type Spawn_Result (Spawned : Boolean := False) is record
      case Spawned is
         when True  => Value : Spawned_Child;
         when False => Why   : Refusal := ABI.Invalid_Argument;
      end case;
   end record;

   --  Create a suspended process from the ELF image at Image (Bytes long),
   --  named Name (at most a process name's length), whose parent is Parent
   --  (No_Process: the caller), with the handle limit Limit.
   function Spawn
     (Authority : Process;
      Image     : System.Address;
      Bytes     : Unsigned_64;
      Priority  : Unsigned_64;
      Name      : String;
      Parent    : Process_ID;
      Limit     : CuBit.Startup_Tables.Handle_Limit) return Spawn_Result;

   --  Place a copy of one of this process's capabilities in a child's
   --  table, before the child first runs.
   function Install (Space : Cspace; Copy : Derivation) return Install_Result;

   --  A copy of one of this process's own capabilities in its own table
   --  (DERIVE_CAPABILITY): a badged endpoint to keep, a narrower range to
   --  pass on. The null handle when the kernel refuses, or when Copy is not
   --  of the result's kind.
   function Derive (Copy : Derivation) return Endpoint;
   function Derive (Copy : Derivation) return Notification;
   function Derive (Copy : Derivation) return Device_Memory;
   function Derive (Copy : Derivation) return IO_Ports;
   function Derive (Copy : Derivation) return Interrupt;
   function Derive (Copy : Derivation) return Scheduling;
   function Derive (Copy : Derivation) return Process;

   --  The word of a handle placed in another process: what that process's
   --  startup table records. It names nothing in this process.
   function Table_Word (Item : Foreign_Handle) return Unsigned_64;

   ---------------------------------------------------------------------------
   --  Lifecycle and placement. True on success.
   ---------------------------------------------------------------------------
   --  Start a suspended child. Max_Frames 0 is unlimited; CPU_Quota is the
   --  quota in µs (low 32 bits) and its period (high 32 bits), 0 unlimited.
   function Resume
     (Target : Process_ID; Authority : Process;
      Max_Frames, CPU_Quota : Unsigned_64) return Boolean;
   function Kill (Target : Process_ID; Authority : Process) return Boolean;
   function Install_Launch_Block
     (Target : Process_ID; Authority : Process;
      Block : System.Address; Bytes : Unsigned_64) return Boolean;
   function Set_CPU (Target : Process_ID; Authority : Process; CPU : Unsigned_64)
     return Boolean;
   --  SEND_CONTROL: 0 accepted, Busy_Word busy, else refused. Authority
   --  may be No_Process for the target's own launcher.
   Busy_Word : constant Unsigned_64 := Unsigned_64'Last - 1;
   function Send_Control
     (Target : Process_ID; Kind : Unsigned_64; Authority : Process) return Unsigned_64;

   --  Contiguous DMA pages mapped into Target at Virtual (see the kernel's
   --  ALLOC_DMA): the physical address, or Unsigned_64'Last.
   function Allocate_DMA
     (Target : Process_ID; Authority : Process;
      Order, Virtual, Mode, Ceiling : Unsigned_64) return Unsigned_64;

   --  Map pages into Target: boot-image pages (Kind_Boot_Image, Source a
   --  byte offset; Region unused) or device pages Region covers
   --  (Kind_Device, Source a physical address).
   Kind_Boot_Image : constant Unsigned_64 := 1;
   Kind_Device     : constant Unsigned_64 := 2;
   function Map_Into
     (Target : Process_ID; Authority : Process; Region : Device_Memory;
      Source, Virtual, Pages, Kind : Unsigned_64) return Boolean;

   --  Read the process list into Buffer (Bytes long): the entry count, or
   --  Unsigned_64'Last. Authority: any process handle with Read.
   function Process_List
     (Authority : Process; Buffer : System.Address; Bytes : Unsigned_64)
      return Unsigned_64;

   ---------------------------------------------------------------------------
   --  Device interrupts (the device manager).
   ---------------------------------------------------------------------------
   type Vector_Block (Allocated : Boolean := False) is record
      case Allocated is
         when True =>
            Allocation : Unsigned_64;      --  Device_Vector_ABI word
            Owner      : Foreign_Handle;   --  the owner's CAP_IRQ handle
         when False =>
            Why        : Refusal := ABI.Invalid_Argument;
      end case;
   end record;

   function Allocate_Vectors
     (Owner : Process_ID; Space : Cspace; Count, CPU : Unsigned_64;
      Authority : Interrupt) return Vector_Block;
   function Release_Vectors (First : Unsigned_64; Authority : Interrupt) return Boolean;
   --  A Device_Vector_ABI field, or Unsigned_64'Last.
   function Read_Vector (Vector, Field : Unsigned_64; Authority : Interrupt)
     return Unsigned_64;

   --  Route a legacy interrupt line to Owner: Lines holds the line (Grant),
   --  Authority is process authority (Grant) over Owner. Route is the
   --  kernel's route word (CPU, trigger, polarity).
   function Enable_IRQ
     (Vector : Unsigned_64; Owner : Process_ID; Route : Unsigned_64;
      Authority : Process; Lines : Interrupt) return Boolean;

end CuBit.Handles.Control;
