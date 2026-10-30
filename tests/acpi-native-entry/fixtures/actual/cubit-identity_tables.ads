------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Per-process state keyed by process identity (KERN-003,
--  docs/process-objects.md), for services that keep something about each
--  process. Identities are opaque 64-bit words, so a service never indexes
--  an array by one.
--
--  @description
--  Open addressing with linear probing and backward-shift deletion: no
--  tombstones, so lookups stay short however many processes come and go.
--  Process_IDs.Hash spreads identities. Values live in the table and move only when Remove closes a gap,
--  so a reference must not be held across Remove.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Process_IDs; use CuBit.Process_IDs;

generic
   type Element is private;
   Empty : Element;
   --  Capacity is 2 ** Capacity_Bits; keep it at least twice the processes
   --  expected, so probes stay short.
   Capacity_Bits : Positive;
package CuBit.Identity_Tables is
   Capacity : constant Positive := 2 ** Capacity_Bits;
   subtype Index is Natural range 0 .. Capacity;          --  0: none
   subtype Valid_Index is Index range 1 .. Capacity;

   type Value_Array is array (Valid_Index) of Element;
   Values : Value_Array := [others => Empty];

   --  Where Key's entry is, or 0.
   function Find (Key : Process_ID) return Index;

   --  Key's entry, added (as Empty) if absent; 0 if Key is No_Process or
   --  the table is full.
   procedure Ensure (Key : Process_ID; At_Index : out Index);
   function Ensure (Key : Process_ID) return Index;

   --  Forget Key's entry, if any.
   procedure Remove (Key : Process_ID);

   function Count return Natural;
end CuBit.Identity_Tables;
