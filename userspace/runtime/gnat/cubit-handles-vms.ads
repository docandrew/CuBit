------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Virtual machines (VMM-001, VM_ABI): the creation authority, a VM and a
--  vCPU, through Call on their kernel-object handles (no new system calls).
--
--  @description
--  A monitor holds a VM_Create handle (procmgr installs it only when its
--  manifest declares vm-create and its startup entry approves it). Create_VM
--  makes it the VM's one monitor, for life: only it may change, run and
--  inspect the VM; the handles are useless to any other process.
------------------------------------------------------------------------------
pragma Ada_2022;
with System;
with VM_ABI;

package CuBit.Handles.VMs is

   package ABI renames VM_ABI;

   type VM is private;
   type VCPU is private;
   No_VM   : constant VM;
   No_VCPU : constant VCPU;
   function Valid (Item : VM) return Boolean;
   function Valid (Item : VCPU) return Boolean;

   --  A Call's answer: the outcome and words 1 .. 3 (Outcome Refused when
   --  the handle names no such object or the reply was unreadable).
   type Reply is record
      Outcome    : ABI.Outcome := ABI.Refused;
      W1, W2, W3 : Unsigned_64 := 0;
   end record;

   function Create_VM (Authority : VM_Create; Machine : out VM) return Reply;
   --  Map Bytes of this process's own committed memory at Address into the
   --  guest at First (guest-physical), with ABI.Right_* Rights.
   function Add_RAM (Machine : VM; First, Address, Bytes, Rights : Unsigned_64) return Reply;
   function Create_VCPU (Machine : VM; CPU : out VCPU) return Reply;
   function Destroy (Machine : VM) return Reply;
   function Describe (Machine : VM) return Reply;

   --  Run's answer: why it returned, and the exit when it is Exited.
   type Exit_Report is record
      Status : ABI.Run_Status := ABI.Run_Refused;
      Kind   : Unsigned_64 := 0;     --  ABI.Exit_Kind'Enum_Rep
      Code   : Unsigned_64 := 0;     --  the back end's raw exit code
      Info_1 : Unsigned_64 := 0;
      Info_2 : Unsigned_64 := 0;
   end record;

   function Is_Kind (R : Exit_Report; K : ABI.Exit_Kind) return Boolean is
     (R.Kind = Unsigned_64 (ABI.Exit_Kind'Enum_Rep (K)));

   function Run
     (CPU : VCPU; Deadline : Unsigned_64; Flags : Unsigned_64 := 0;
      RAX, RDX : Unsigned_64 := 0) return Exit_Report;

   function Get_State (CPU : VCPU; Set : ABI.State_Set; Buffer : System.Address;
                       Bytes : Unsigned_64) return Reply;
   function Set_State (CPU : VCPU; Set : ABI.State_Set; Buffer : System.Address;
                       Bytes : Unsigned_64) return Reply;
   function Inject (CPU : VCPU; Kind : ABI.Event_Kind; Vector : Unsigned_64;
                    Error : Unsigned_64 := 0; Has_Error : Boolean := False) return Reply;
   function Kick (CPU : VCPU) return Reply;
   --  Entries, tag flushes, exits returned (words 1 .. 3).
   function Statistics (CPU : VCPU) return Reply;
   --  The host's SPEC_CTRL now, the bits a guest may set, Speculation_*
   --  flags (words 1 .. 3).
   function Speculation (CPU : VCPU) return Reply;

private
   type VM is record
      Word : Unsigned_64 := 0;
   end record;
   type VCPU is record
      Word : Unsigned_64 := 0;
   end record;
   No_VM   : constant VM := (Word => 0);
   No_VCPU : constant VCPU := (Word => 0);
end CuBit.Handles.VMs;
