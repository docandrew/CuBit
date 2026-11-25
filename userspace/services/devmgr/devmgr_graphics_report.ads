------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  devmgr's copy of the renderer report (GPU-014, CuBit.Graphics_Report):
--  one entry per subject, posted by whoever knows its path, read by
--  observers against a generation.
--
--  @description
--  A post replaces its subject's entry or adds one. A poster that exits
--  (procmgr tells devmgr) leaves its entries in place, marked Stale, so the
--  report still says what Desktop last rendered after Desktop died. The
--  report holds Capacity entries; a post that finds no room is counted in
--  Dropped (and visible to readers), never silently lost. Every change
--  moves the generation, so a reader paging through entries notices.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Graphics_Report;

package Devmgr_Graphics_Report with SPARK_Mode is
   package GR renames CuBit.Graphics_Report;

   --  Outputs, GPUs and the programs on a GPU or Vulkan path: one machine's
   --  worth with room to spare.
   Capacity : constant := 128;
   subtype Entry_Count is Natural range 0 .. Capacity;
   subtype Entry_Index is Positive range 1 .. Capacity;

   --  A poster's pid word (CuBit.Process_IDs.To_Word).
   type Poster_Id is new Unsigned_64;
   No_Poster : constant Poster_Id := 0;

   type Report is private;

   function Empty return Report;
   function Count (R : Report) return Entry_Count;
   function Generation (R : Report) return GR.Generation;
   function Dropped (R : Report) return Natural;
   function Item (R : Report; Index : Entry_Index) return GR.Report_Entry
     with Pre => Index <= Count (R);

   --  Poster's Item replaces its subject's entry, or is added.
   procedure Post (R : in out Report; Item : GR.Report_Entry; Poster : Poster_Id)
     with Post => Count (R) >= Count (R'Old);

   --  Poster (a program's pid) has gone: what it posted, and its own entry
   --  (procmgr's post about it), stay, marked Stale.
   procedure Poster_Gone (R : in out Report; Poster : Poster_Id)
     with Post => Count (R) = Count (R'Old);

   ---------------------------------------------------------------------------
   --  devmgr's own entry: the GPU and its driver (the chain's root).
   ---------------------------------------------------------------------------
   --  The GPU drivers devmgr binds by presence (HW-002).
   type GPU_Driver is (No_Driver, Intel_GPU, Virtio_GPU);
   type GPU_Facts is record
      Display_Present : Boolean := False;      --  any display controller
      Vendor, Device  : Unsigned_16 := 0;      --  the one a driver binds, or the first
      Location        : GR.Subject_Id := 0;    --  its PCI bus, device and function
      Driver          : GPU_Driver := No_Driver;
      Running         : Boolean := False;      --  the driver started and said ready
      Since           : GR.Milliseconds := 0;
   end record;

   --  Software No_GPU_Device without a display controller; No_Driver_Bound
   --  when none of devmgr's GPU drivers binds it; Driver_Failed (Init) when
   --  the bound driver did not come up; virtio-gpu running is Software,
   --  Venus_Not_Offered No_3D (it drives scanout only); intel-gpu running is
   --  Hardware.
   function GPU_Entry (Facts : GPU_Facts) return GR.Report_Entry;

   ---------------------------------------------------------------------------
   --  devmgr's side of the wire (CuBit.Graphics_Report): one message in,
   --  at most one reply out.
   ---------------------------------------------------------------------------
   type Words is array (0 .. 3) of Unsigned_64;
   subtype Word_Count is Natural range 0 .. 4;

   --  First halves of posts waiting for their end, one per poster.
   Pending_Capacity : constant := 16;
   type Pending is private;
   function No_Pending return Pending;

   type Answer is record
      Reply  : Boolean := False;     --  a call to answer
      Label  : Unsigned_32 := 0;
      Length : Word_Count := 0;
      Values : Words := [others => 0];
   end record;

   --  Take one message labelled Label (Length words) from Sender, sent on
   --  a capability badged Badge; From_Parent when Sender is procmgr (posts
   --  without the poster's badge, and the only one whose Gone is taken).
   --  Handled is False for a label not the report's.
   procedure Handle
     (R : in out Report; Halves : in out Pending; Label : Unsigned_32; Length : Natural;
      Values : Words; Badge : Unsigned_64; Sender : Poster_Id; From_Parent : Boolean;
      Result : out Answer; Handled : out Boolean);

private
   type Half is record
      Poster : Poster_Id := No_Poster;
      Values : Words := [others => 0];
   end record;
   type Pending is array (1 .. Pending_Capacity) of Half;
   function No_Pending return Pending is [others => <>];

   type Slot is record
      Value  : GR.Report_Entry;
      Poster : Poster_Id := No_Poster;
   end record;
   type Slot_Array is array (Entry_Index) of Slot;

   type Report is record
      Slots     : Slot_Array;
      Used      : Entry_Count := 0;
      Current   : GR.Generation := 0;
      Lost      : Natural := 0;
   end record;

   function Empty return Report is ((others => <>));
   function Count (R : Report) return Entry_Count is (R.Used);
   function Generation (R : Report) return GR.Generation is (R.Current);
   function Dropped (R : Report) return Natural is (R.Lost);
   function Item (R : Report; Index : Entry_Index) return GR.Report_Entry is (R.Slots (Index).Value);
end Devmgr_Graphics_Report;
