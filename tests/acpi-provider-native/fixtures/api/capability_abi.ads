------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The capability-handle system-call ABI (CAP-002,
--  docs/capability-handles.md), shared by the kernel and the runtime.
--
--  @description
--  A handle names one entry of the calling process's capability table. On
--  the wire it is one word: the slot's generation in the high 32 bits, the
--  slot in the low 32 bits. The kernel chooses every slot; generation 0 is
--  never issued, so a word whose high half is zero never names an entry.
--  That is what lets one result word carry either a handle or a refusal:
--
--     Handle_Result word = a handle word        (generation field /= 0)
--                        | a Refusal code 1..N  (generation field  = 0)
--
--  Only the runtime's syscall glue (CuBit.Handles and its children) reads
--  or writes these words; programs hold typed, private handles.
--
--  Records passed by address (Derivation, Spawn_Request, Spawn_Result,
--  Vector_Allocation, Inspection) are arrays of little-endian words in the
--  order given; the kernel copies them in or out once.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Capability_ABI with Pure, SPARK_Mode is

   ---------------------------------------------------------------------------
   --  Handle words
   ---------------------------------------------------------------------------
   Slot_Field_Bits : constant := 32;
   No_Handle       : constant Unsigned_64 := 0;

   function Generation_Field (Word : Unsigned_64) return Unsigned_32 is
     (Unsigned_32 (Shift_Right (Word, Slot_Field_Bits)));

   --  The word could name a table entry (it is not a refusal or zero).
   function Is_Handle_Word (Word : Unsigned_64) return Boolean is
     (Generation_Field (Word) /= 0);

   ---------------------------------------------------------------------------
   --  Refusals: why the kernel did not do what a handle call asked. Codes
   --  are fixed; a new reason takes a new code.
   ---------------------------------------------------------------------------
   type Refusal is
     (Invalid_Handle,       --  never issued by this table
      Stale_Handle,         --  closed, or its slot reused since
      Revoked_Handle,       --  revoked from outside
      Wrong_Kind,           --  names a capability of another kind
      Insufficient_Rights,  --  lacks a right the call needs
      Would_Amplify,        --  a derivation asked for more than its source
      Not_Transferable,     --  this kind is never copied to another table
      Limit_Reached,        --  the target's handle limit is reached
      Table_Full,           --  the target's table is at its capacity
      Out_Of_Memory,        --  no page for the target's table
      Sealed,               --  the CSPACE's process has started running
      Target_Gone,          --  the named process has exited or was replaced
      Not_Accepted,         --  the request did not accept a capability
      Invalid_Argument,     --  a malformed argument or record
      Not_Permitted);       --  the capability does not cover the request
   for Refusal use
     (Invalid_Handle      => 1,
      Stale_Handle        => 2,
      Revoked_Handle      => 3,
      Wrong_Kind          => 4,
      Insufficient_Rights => 5,
      Would_Amplify       => 6,
      Not_Transferable    => 7,
      Limit_Reached       => 8,
      Table_Full          => 9,
      Out_Of_Memory       => 10,
      Sealed              => 11,
      Target_Gone         => 12,
      Not_Accepted        => 13,
      Invalid_Argument    => 14,
      Not_Permitted       => 15);

   function Refusal_Word (Why : Refusal) return Unsigned_64 is
     (Unsigned_64 (Refusal'Enum_Rep (Why)));

   function Is_Refusal_Word (Word : Unsigned_64) return Boolean is
     (Word in Refusal_Word (Refusal'First) .. Refusal_Word (Refusal'Last));

   function To_Refusal (Word : Unsigned_64) return Refusal
   with Pre  => Is_Refusal_Word (Word),
        Post => Refusal_Word (To_Refusal'Result) = Word;

   ---------------------------------------------------------------------------
   --  Capability kinds as INSPECT_CAPABILITY reports them (the kernel's
   --  Capabilities.CapabilityType positions).
   ---------------------------------------------------------------------------
   type Kind is
     (No_Capability, Endpoint, Notification, Memory, IO_Ports, Interrupt,
      Process, Device_Memory, Reply, Resource, Cspace, Scheduling,
      Hardware_Group, Hardware_Register, Firmware_Snapshot);
   for Kind use
     (No_Capability     => 0,
      Endpoint          => 1,
      Notification      => 2,
      Memory            => 3,
      IO_Ports          => 4,
      Interrupt         => 5,
      Process           => 6,
      Device_Memory     => 7,
      Reply             => 8,
      Resource          => 9,
      Cspace            => 10,
      Scheduling        => 11,
      Hardware_Group    => 12,
      Hardware_Register => 13,
      Firmware_Snapshot => 14);

   ---------------------------------------------------------------------------
   --  Rights, as a mask of these bits.
   ---------------------------------------------------------------------------
   Right_Read    : constant Unsigned_64 := 2 ** 0;
   Right_Write   : constant Unsigned_64 := 2 ** 1;
   Right_Execute : constant Unsigned_64 := 2 ** 2;
   Right_Grant   : constant Unsigned_64 := 2 ** 3;
   Right_Revoke  : constant Unsigned_64 := 2 ** 4;
   All_Rights    : constant Unsigned_64 :=
     Right_Read or Right_Write or Right_Execute or Right_Grant or Right_Revoke;

   ---------------------------------------------------------------------------
   --  Message tag flags: one byte of the tag, two typed components.
   --  Protocol_Flags (the low four bits) belong to the protocol between the
   --  two processes; the kernel delivers them unchanged. A protocol that
   --  needs more than four bits carries them in the payload words.
   --  IPC_Flags (the high four bits) belong to the IPC layer, and a program
   --  sets one only by naming it:
   --  * Accepts_Capability: set by a requester; the reply may carry one
   --    capability (REPLY_WITH_CAPABILITY).
   --  * Capability_Transferred / Capability_Refused: set by the kernel on a
   --    reply; its authorityTag is the new handle word in the requester's
   --    table, or the Refusal code.
   --  * Capability_Offered: set by the kernel on a delivered request whose
   --    caller offered a capability (CALL_OFFERING_CAPABILITY); the server
   --    may take it with ACCEPT_OFFERED_CAPABILITY while it answers.
   --  The kernel refuses anything a process sends that claims one of its
   --  own three (Claims_Kernel_Flags); it never corrects them silently.
   ---------------------------------------------------------------------------
   type Protocol_Flags is mod 2 ** 4 with Size => 4;

   type IPC_Flags is record
      Accepts_Capability     : Boolean := False;
      Capability_Transferred : Boolean := False;
      Capability_Refused     : Boolean := False;
      Capability_Offered     : Boolean := False;
   end record with Size => 4;
   for IPC_Flags use record
      Accepts_Capability     at 0 range 0 .. 0;
      Capability_Transferred at 0 range 1 .. 1;
      Capability_Refused     at 0 range 2 .. 2;
      Capability_Offered     at 0 range 3 .. 3;
   end record;

   No_IPC_Flags : constant IPC_Flags := (others => False);
   Accepting    : constant IPC_Flags := (Accepts_Capability => True, others => False);

   --  The flag byte as it is on the wire, for decoders that check the whole
   --  byte (an exact-match validator rejects unexpected IPC bits too).
   IPC_Shift : constant := 4;
   function Flag_Byte (Protocol : Protocol_Flags; IPC : IPC_Flags) return Unsigned_8 is
     (Unsigned_8 (Protocol)
      + (if IPC.Accepts_Capability     then 2 ** (IPC_Shift + 0) else 0)
      + (if IPC.Capability_Transferred then 2 ** (IPC_Shift + 1) else 0)
      + (if IPC.Capability_Refused     then 2 ** (IPC_Shift + 2) else 0)
      + (if IPC.Capability_Offered     then 2 ** (IPC_Shift + 3) else 0));

   --  Whether a message a process sends claims one of the kernel's flags.
   function Claims_Kernel_Flags (Flags : IPC_Flags) return Boolean is
     (Flags.Capability_Transferred or else Flags.Capability_Refused
      or else Flags.Capability_Offered);

   ---------------------------------------------------------------------------
   --  Derivation: what INSTALL_CAPABILITY and REPLY_WITH_CAPABILITY copy.
   --  The copy has Rights (a subset of the source's, which must include
   --  Grant). Set_Tag stamps Tag (nonzero) on a copy of an untagged
   --  endpoint or notification: a badge, never a process identity. Narrow selects [Base, Base + Length) inside a range
   --  capability's range, or (Base = budget, Length = period) covered by a
   --  scheduling capability; without it the copy names the source's whole
   --  object and Base and Length must be zero.
   ---------------------------------------------------------------------------
   Set_Tag : constant Unsigned_64 := 2 ** 0;
   Narrow  : constant Unsigned_64 := 2 ** 1;
   Derivation_Flags : constant Unsigned_64 := Set_Tag or Narrow;

   type Derivation_Field is (Source, Rights, Flags, Tag, Base, Length);
   type Derivation is array (Derivation_Field) of Unsigned_64;
   Derivation_Bytes : constant := 6 * 8;

   ---------------------------------------------------------------------------
   --  SPAWN (60): arg0 = spawn authority (CAP_PROCESS with Execute),
   --  arg1 = address of a Spawn_Request, arg2 = address of a Spawn_Result.
   --  Returns 0, or a Refusal code. A handle limit is Bounded (Count in
   --  1 .. Maximum_Handle_Count) or Kernel_Limit (Count zero: only the kernel's
   --  table maximum and the memory quota bound it).
   ---------------------------------------------------------------------------
   Maximum_Handle_Count : constant := 32_768;
   type Limit_Kind is (Bounded, Kernel_Limit);
   for Limit_Kind use (Bounded => 1, Kernel_Limit => 2);

   type Spawn_Request_Field is
     (Image, Image_Bytes, Priority, Name, Parent, Limit, Limit_Count);
   type Spawn_Request is array (Spawn_Request_Field) of Unsigned_64;
   Spawn_Request_Bytes : constant := 7 * 8;

   --  Child: the new process's identity. Process, Endpoint, Cspace: the
   --  spawner's new handles for it (control, an untagged endpoint, and
   --  installation until it first runs). Child_Self, Child_Self_Process:
   --  the child's own handles, for its startup table.
   type Spawn_Result_Field is
     (Child, Process, Endpoint, Cspace, Child_Self, Child_Self_Process);
   type Spawn_Result is array (Spawn_Result_Field) of Unsigned_64;
   Spawn_Result_Bytes : constant := 6 * 8;

   ---------------------------------------------------------------------------
   --  ALLOCATE_DEVICE_VECTORS (130) result: the Device_Vector_ABI
   --  allocation word, and the owner's interrupt handle in its own table.
   ---------------------------------------------------------------------------
   type Vector_Allocation_Field is (Allocation, Owner_Handle);
   type Vector_Allocation is array (Vector_Allocation_Field) of Unsigned_64;
   Vector_Allocation_Bytes : constant := 2 * 8;

   ---------------------------------------------------------------------------
   --  INSPECT_CAPABILITY (84): arg0 = handle, arg1 = address of an
   --  Inspection. Object is a process identity for process-naming kinds.
   ---------------------------------------------------------------------------
   type Inspection_Field is
     (Capability_Kind, Rights_Mask, Authority_Tag, Object, Parameter,
      Generation);
   type Inspection is array (Inspection_Field) of Unsigned_64;
   Inspection_Bytes : constant := 6 * 8;

end Capability_ABI;
