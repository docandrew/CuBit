------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  devmgr's claim ledgers on the heap (LIM-001b): the proved port and
--  memory cores (Devmgr_Port_Claims, Devmgr_Memory_Claims) over arrays that
--  grow with what they record, so no number of drivers, restarts or ACPI
--  windows fills them.
--
--  @description
--  Each operation hands the core the store's array; a core that answers
--  Needs_Room gets a larger array and the same question again. When the
--  heap refuses, the claim is not granted and Report names why
--  (Devmgr_Growable.Capacity_Report: demand, the capacity kept, the bytes
--  asked for); Capacity_Text spells it for devmgr's CAPACITY warning.
--  Generic over the allocator so the hosted tests can run the same code
--  over the C heap, and refuse on purpose.
------------------------------------------------------------------------------
pragma Ada_2022;
with System;
with Interfaces; use Interfaces;
with Devmgr_Port_Claims;
with Devmgr_Memory_Claims;
with Devmgr_Growable;

generic
   with function Allocate (Bytes, Alignment : Unsigned_64) return System.Address;
   with procedure Release (Block : System.Address);
package Devmgr_Claim_Ledgers is

   package PC renames Devmgr_Port_Claims;
   package MC renames Devmgr_Memory_Claims;
   use type PC.Holder;

   package Port_Store is new Devmgr_Growable (PC.Claim, Allocate, Release);
   package Memory_Store is new Devmgr_Growable (MC.Claim, Allocate, Release);
   subtype Capacity_Report is Port_Store.Capacity_Report;

   ---------------------------------------------------------------------------
   --  Ports
   ---------------------------------------------------------------------------
   type Port_Ledger is limited private;

   --  The kernel's and devmgr's own ranges (PC.Initialize).
   procedure Initialize (L : in out Port_Ledger; Report : out Capacity_Report);

   procedure Take
     (L : in out Port_Ledger; First, Ports : Unsigned_64; By : PC.Holder;
      Owner : PC.Owner_Word; Granted : out Boolean; Report : out Capacity_Report)
   with Pre => By /= PC.Kernel and then (Owner = PC.No_Owner) = (By /= PC.Driver);

   --  The driver Owner exited: its claims are given back (Released of them).
   procedure Release_Owner (L : in out Port_Ledger; Owner : PC.Owner_Word; Released : out Natural)
   with Pre => Owner /= PC.No_Owner;

   function Count (L : Port_Ledger) return Natural;
   function Claim_At (L : Port_Ledger; Index : Positive) return PC.Claim
   with Pre => Index <= Count (L);

   ---------------------------------------------------------------------------
   --  Physical pages
   ---------------------------------------------------------------------------
   type Memory_Ledger is limited private;

   procedure Claim
     (L : in out Memory_Ledger; Physical, Bytes : Unsigned_64; By : MC.Holder;
      Result : out MC.Claim_Result; Report : out Capacity_Report);

   function Clear_Of_ACPI (L : Memory_Ledger; Physical, Bytes : Unsigned_64) return Boolean;
   function Clear_Of_Devices (L : Memory_Ledger; Physical, Bytes : Unsigned_64) return Boolean;
   --  The first page of the lowest claim starting above Page (MC.Next_Start).
   function Next_Start (L : Memory_Ledger; Page : Unsigned_64) return Unsigned_64;

   function Count (L : Memory_Ledger) return Natural;
   function Claim_At (L : Memory_Ledger; Index : Positive) return MC.Claim
   with Pre => Index <= Count (L);

   --  "CAPACITY What: demand N, limit M, heap refused B bytes": a growth
   --  that failed, for devmgr's warning.
   function Capacity_Text (What : String; Report : Capacity_Report) return String;

private
   type Port_Ledger is limited record
      Claims : Port_Store.Store;
   end record;
   type Memory_Ledger is limited record
      Claims : Memory_Store.Store;
   end record;
end Devmgr_Claim_Ledgers;
