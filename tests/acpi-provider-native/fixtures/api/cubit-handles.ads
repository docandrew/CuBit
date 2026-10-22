------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Capability handles, one private type per kind (CAP-002,
--  docs/capability-handles.md, "No numbers in source").
--
--  @description
--  A handle names one entry of this process's capability table. The kernel
--  chose where the entry went; the handle carries its slot and generation,
--  so a handle kept after Close or revocation never reaches whatever is
--  placed there later.
--
--  The types are private and have no constructor from an integer. A program
--  gets handles only:
--  * by name from its startup table (CuBit.Handles.Startup, or the
--    generated Startup package of its manifest);
--  * as results of the calls that create them (SPAWN gives a launcher its
--    child's process, endpoint and CSPACE; MOVE_REPLY_CAPABILITY gives a
--    saved reply);
--  * in a reply to a request that accepted one (CuBit.Handles.Transfers).
--  Only the children of this package see the kernel's words; programs never
--  do (tools/check_capability_numbers.py rejects a program unit in this
--  hierarchy, and an integer passed where a handle belongs).
--
--  Every result that may be a refusal is a discriminated record the caller
--  must examine: the 2026-10-12 NUC freeze was a refusal turned into
--  success.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Capability_ABI;

package CuBit.Handles with Pure, SPARK_Mode is

   package ABI renames Capability_ABI;
   subtype Refusal is ABI.Refusal;

   --  Each kind of authority is its own type: a device-memory handle cannot
   --  be passed where an endpoint is expected.
   type Endpoint is private;       --  call, send, submit; badge and pass on
   type Reply is private;          --  a saved reply (answer it later)
   type Process is private;        --  spawn, resume, kill, map into, ...
   type Cspace is private;         --  install into one child before it runs
   type Device_Memory is private;  --  map device pages (a framebuffer too)
   type IO_Ports is private;       --  port I/O over a range
   type Interrupt is private;      --  interrupt lines or a vector block
   type Notification is private;   --  service roles: register, publish
   type Scheduling is private;     --  reserve real-time CPU

   --  The null handle of each kind: the kernel never resolves it
   --  (generation 0 is never issued).
   No_Endpoint      : constant Endpoint;
   No_Reply         : constant Reply;
   No_Process       : constant Process;
   No_Cspace        : constant Cspace;
   No_Device_Memory : constant Device_Memory;
   No_IO_Ports      : constant IO_Ports;
   No_Interrupt     : constant Interrupt;
   No_Notification  : constant Notification;
   No_Scheduling    : constant Scheduling;

   --  Whether a handle could name an entry (it is not a null handle).
   --  A valid handle may still be stale; the kernel decides.
   function Valid (Item : Endpoint) return Boolean;
   function Valid (Item : Reply) return Boolean;
   function Valid (Item : Process) return Boolean;
   function Valid (Item : Cspace) return Boolean;
   function Valid (Item : Device_Memory) return Boolean;
   function Valid (Item : IO_Ports) return Boolean;
   function Valid (Item : Interrupt) return Boolean;
   function Valid (Item : Notification) return Boolean;
   function Valid (Item : Scheduling) return Boolean;

   ---------------------------------------------------------------------------
   --  Rights a derived copy keeps (a subset of its source's).
   ---------------------------------------------------------------------------
   type Rights is record
      Read    : Boolean := False;
      Write   : Boolean := False;
      Execute : Boolean := False;
      Grant   : Boolean := False;
      Revoke  : Boolean := False;
   end record;

   Read_Only   : constant Rights := (Read => True, others => False);
   Write_Only  : constant Rights := (Write => True, others => False);
   Read_Write  : constant Rights := (Read | Write => True, others => False);
   Grant_Only  : constant Rights := (Grant => True, others => False);

   function Mask (Item : Rights) return Unsigned_64 is
     ((if Item.Read then ABI.Right_Read else 0)
      or (if Item.Write then ABI.Right_Write else 0)
      or (if Item.Execute then ABI.Right_Execute else 0)
      or (if Item.Grant then ABI.Right_Grant else 0)
      or (if Item.Revoke then ABI.Right_Revoke else 0));

   ---------------------------------------------------------------------------
   --  Derivations: what INSTALL_CAPABILITY and REPLY_WITH_CAPABILITY copy.
   --  Built only from a handle this process holds.
   ---------------------------------------------------------------------------
   type Derivation is private;
   --  The kind of the handle the derivation copies.
   function Kind_Of (Copy : Derivation) return ABI.Kind;

   --  The whole object with Kept rights.
   function Copy (Item : Endpoint; Kept : Rights) return Derivation;
   function Copy (Item : Process; Kept : Rights) return Derivation;
   function Copy (Item : Device_Memory; Kept : Rights) return Derivation;
   function Copy (Item : IO_Ports; Kept : Rights) return Derivation;
   function Copy (Item : Interrupt; Kept : Rights) return Derivation;
   function Copy (Item : Notification; Kept : Rights) return Derivation;
   function Copy (Item : Scheduling; Kept : Rights) return Derivation;

   --  A copy of an untagged endpoint carrying Tag (nonzero): the server
   --  sees Tag as the authority of every message sent through it.
   function Badged (Item : Endpoint; Kept : Rights; Tag : Unsigned_64)
     return Derivation
   with Pre => Tag /= 0;

   --  A copy of an untagged notification over [First, First + Count) of its
   --  roles carrying Tag (nonzero): a receiver sees Tag as the authority of
   --  every event published through it (a badge, never an identity).
   function Badged (Item : Notification; Kept : Rights; First, Count, Tag : Unsigned_64)
     return Derivation
   with Pre => Tag /= 0;

   --  [First, First + Count) of a range: bytes of device memory, ports,
   --  interrupt lines, service roles.
   function Narrowed (Item : Device_Memory; Kept : Rights; First, Count : Unsigned_64)
     return Derivation;
   function Narrowed (Item : IO_Ports; Kept : Rights; First, Count : Unsigned_64)
     return Derivation;
   function Narrowed (Item : Interrupt; Kept : Rights; First, Count : Unsigned_64)
     return Derivation;
   function Narrowed (Item : Notification; Kept : Rights; First, Count : Unsigned_64)
     return Derivation;
   --  A share no larger than the source's: Budget every Period µs.
   function Narrowed (Item : Scheduling; Budget, Period : Unsigned_64)
     return Derivation;

   ---------------------------------------------------------------------------
   --  A handle word placed in another process's table (INSTALL_CAPABILITY's
   --  result): it names nothing here, and is only recorded in that child's
   --  startup table.
   ---------------------------------------------------------------------------
   type Foreign_Handle is private;

   type Install_Result (Installed : Boolean := False) is record
      case Installed is
         when True  => Placed : Foreign_Handle;
         when False => Why    : Refusal := ABI.Invalid_Argument;
      end case;
   end record;

private
   type Word_Holder is record
      Word : Unsigned_64 := ABI.No_Handle;
   end record;

   type Endpoint is new Word_Holder;
   type Reply is new Word_Holder;
   type Process is new Word_Holder;
   type Cspace is new Word_Holder;
   type Device_Memory is new Word_Holder;
   type IO_Ports is new Word_Holder;
   type Interrupt is new Word_Holder;
   type Notification is new Word_Holder;
   type Scheduling is new Word_Holder;
   type Foreign_Handle is new Word_Holder;

   No_Endpoint      : constant Endpoint := (Word => ABI.No_Handle);
   No_Reply         : constant Reply := (Word => ABI.No_Handle);
   No_Process       : constant Process := (Word => ABI.No_Handle);
   No_Cspace        : constant Cspace := (Word => ABI.No_Handle);
   No_Device_Memory : constant Device_Memory := (Word => ABI.No_Handle);
   No_IO_Ports      : constant IO_Ports := (Word => ABI.No_Handle);
   No_Interrupt     : constant Interrupt := (Word => ABI.No_Handle);
   No_Notification  : constant Notification := (Word => ABI.No_Handle);
   No_Scheduling    : constant Scheduling := (Word => ABI.No_Handle);

   function Valid (Item : Endpoint) return Boolean is (ABI.Is_Handle_Word (Item.Word));
   function Valid (Item : Reply) return Boolean is (ABI.Is_Handle_Word (Item.Word));
   function Valid (Item : Process) return Boolean is (ABI.Is_Handle_Word (Item.Word));
   function Valid (Item : Cspace) return Boolean is (ABI.Is_Handle_Word (Item.Word));
   function Valid (Item : Device_Memory) return Boolean is (ABI.Is_Handle_Word (Item.Word));
   function Valid (Item : IO_Ports) return Boolean is (ABI.Is_Handle_Word (Item.Word));
   function Valid (Item : Interrupt) return Boolean is (ABI.Is_Handle_Word (Item.Word));
   function Valid (Item : Notification) return Boolean is (ABI.Is_Handle_Word (Item.Word));
   function Valid (Item : Scheduling) return Boolean is (ABI.Is_Handle_Word (Item.Word));

   type Derivation is record
      Words  : ABI.Derivation := [others => 0];
      --  The source's kind: what a local derivation's handle is.
      Source : ABI.Kind := ABI.No_Capability;
   end record;

   function Make
     (Of_Kind : ABI.Kind; Source : Unsigned_64; Kept : Rights;
      Flags, Tag, First, Count : Unsigned_64) return Derivation is
     ((Words  => [ABI.Source => Source, ABI.Rights => Mask (Kept), ABI.Flags => Flags,
                  ABI.Tag => Tag, ABI.Base => First, ABI.Length => Count],
       Source => Of_Kind));

   function Kind_Of (Copy : Derivation) return ABI.Kind is (Copy.Source);

   function Copy (Item : Endpoint; Kept : Rights) return Derivation is
     (Make (ABI.Endpoint, Item.Word, Kept, 0, 0, 0, 0));
   function Copy (Item : Process; Kept : Rights) return Derivation is
     (Make (ABI.Process, Item.Word, Kept, 0, 0, 0, 0));
   function Copy (Item : Device_Memory; Kept : Rights) return Derivation is
     (Make (ABI.Device_Memory, Item.Word, Kept, 0, 0, 0, 0));
   function Copy (Item : IO_Ports; Kept : Rights) return Derivation is
     (Make (ABI.IO_Ports, Item.Word, Kept, 0, 0, 0, 0));
   function Copy (Item : Interrupt; Kept : Rights) return Derivation is
     (Make (ABI.Interrupt, Item.Word, Kept, 0, 0, 0, 0));
   function Copy (Item : Notification; Kept : Rights) return Derivation is
     (Make (ABI.Notification, Item.Word, Kept, 0, 0, 0, 0));
   function Copy (Item : Scheduling; Kept : Rights) return Derivation is
     (Make (ABI.Scheduling, Item.Word, Kept, 0, 0, 0, 0));

   function Badged (Item : Endpoint; Kept : Rights; Tag : Unsigned_64)
     return Derivation is
     (Make (ABI.Endpoint, Item.Word, Kept, ABI.Set_Tag, Tag, 0, 0));

   function Badged (Item : Notification; Kept : Rights; First, Count, Tag : Unsigned_64)
     return Derivation is
     (Make (ABI.Notification, Item.Word, Kept, ABI.Narrow + ABI.Set_Tag, Tag, First, Count));

   function Narrowed (Item : Device_Memory; Kept : Rights; First, Count : Unsigned_64)
     return Derivation is (Make (ABI.Device_Memory, Item.Word, Kept, ABI.Narrow, 0, First, Count));
   function Narrowed (Item : IO_Ports; Kept : Rights; First, Count : Unsigned_64)
     return Derivation is (Make (ABI.IO_Ports, Item.Word, Kept, ABI.Narrow, 0, First, Count));
   function Narrowed (Item : Interrupt; Kept : Rights; First, Count : Unsigned_64)
     return Derivation is (Make (ABI.Interrupt, Item.Word, Kept, ABI.Narrow, 0, First, Count));
   function Narrowed (Item : Notification; Kept : Rights; First, Count : Unsigned_64)
     return Derivation is (Make (ABI.Notification, Item.Word, Kept, ABI.Narrow, 0, First, Count));
   function Narrowed (Item : Scheduling; Budget, Period : Unsigned_64)
     return Derivation is (Make (ABI.Scheduling, Item.Word, (others => False), ABI.Narrow, 0, Budget, Period));
end CuBit.Handles;
