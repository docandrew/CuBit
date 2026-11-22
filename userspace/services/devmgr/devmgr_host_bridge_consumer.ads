pragma Ada_2022;
with CuBit.Messages;
------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  devmgr's side of acpi.svc's host-bridge snapshot channel (PCI-BAR-001):
--  it receives each PCI root's live _CRS and records the root's memory
--  windows (Devmgr_Host_Windows), so an unassigned BAR can be placed.
--
--  @description
--  acpi.svc opens a CuBit.Host_Bridge_Windows duplex through its
--  pci-windows endpoint. Only that endpoint's badge
--  (ACPI_PCI_Window_Exchange.Backend_Tag), the producing side and the
--  host-bridge contract (any measured page count in its range) are
--  admitted. One channel at a time: a new open from acpi.svc (a live
--  replace, a restart) replaces the old one, and the BARs already placed
--  stay where they are.
--
--  Per request: Root_ID, Sequence and Packet_Bytes. The packet is copied
--  once into private memory before anything reads it (copy, then
--  validate). It is decoded with acpi.svc's codec
--  (ACPI_Host_Bridge_Snapshot) and answered:
--  - Accepted;
--  - Malformed (the codec refused it);
--  - Identity_Mismatch (the request's root or sequence is not the header's);
--  - Stale_Sequence (not above the root's last);
--  - Too_Large (past the opener's region).
--  A Complete snapshot replaces its root's windows (Host_Bridge_Rows). An
--  Incomplete or Failed one is acknowledged and logged with its reason,
--  and changes nothing. Acceptance acknowledges the copy only, never
--  placement or authority.
------------------------------------------------------------------------------
generic
   --  Where its log lines go (devmgr's Warnings.Inform).
   with procedure Report (Line : String);
package Devmgr_Host_Bridge_Consumer is

   --  Whether Request from From is this channel's open; Answer is the
   --  reply to send when it is.
   procedure Handle_Open
     (From : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message;
      Is_Open : out Boolean; Answer : out CuBit.Messages.Message);

   --  Whether Request (OP_CLOSE) ends this channel; it is closed if so.
   procedure Handle_Close
     (From : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message; Closed : out Boolean);

   --  Take and answer every waiting request. Published is True when a
   --  root's windows were replaced.
   procedure Pump (Published : out Boolean);

   --  Before devmgr sleeps: True when it may (no request waits; the server
   --  is armed so acpi.svc's next submission wakes it).
   function Arm return Boolean;
end Devmgr_Host_Bridge_Consumer;
