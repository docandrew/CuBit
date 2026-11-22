-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- Intel VT-d DMA remapping (IOMMU-001): the kernel's mechanism half.
--
-- @description
-- Setup brings up every remapping unit the admitted DMAR (acpi.dmarFacts)
-- names, in the VT-d specification's order, with every wait on a status
-- bit bounded by a named deadline:
--  1. map the unit's registers, read CAP/ECAP, choose the address width
--     (IO_Page_Tables.Choose_Width) and allocate its root table;
--  2. give every device a reserved memory region (RMRR) names its domain,
--     with the region identity-mapped;
--  3. per unit: turn off translation and queued invalidation if firmware
--     left them on, set the root-table pointer (legacy mode), invalidate
--     the context cache and the IOTLB globally, arm the fault event, enable
--     translation, then drop the firmware's protected memory regions.
-- A device with no domain has no context entry, so every DMA it issues
-- faults: bus mastering is useless to a device until devmgr binds it.
--
-- Domains (IOMMU_Domains holds the proved ledger): one per (unit,
-- requester ID), created by the first device-bound ALLOC_DMA for the
-- requester (Map_Grant) and bound to the target process. Its second-level
-- tables map exactly the ledger's Mapped runs: identity, so the device uses
-- the physical address ALLOC_DMA returns, and reaches nothing else.
-- Unmap_Grant clears the entries, invalidates the domain's IOTLB and waits
-- for it, and only then releases the run: the frames may be reused only
-- when it says Released (otherwise they stay quarantined).
--
-- The integrated GPU is not translated yet (IOMMU-001 slice 5): a unit
-- whose scope is only display devices is left untranslated, and on a unit
-- that includes all devices the GPU at 00:02.0 gets a pass-through
-- context entry. Interrupt remapping is not enabled (a later slice).
--
-- No IOMMU (no DMAR, a rejected one, or no unit could translate): boot
-- continues with DMA unconfined, and the state is a typed record
-- (Device_Memory_Diagnostics.Record_IOMMU_State) in the boot log, never
-- silent. Every DMA fault is a typed record too (Record_DMA_Fault).
--
-- Locking: Lock serializes the ledger, the tables and the units' command
-- and invalidation registers (bounded waits run under it). Fault_Lock
-- serializes fault-record draining (interrupt path). Lock order: process
-- mailbox locks, then Process.DMA's registry lock, then Lock.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package DMA_Remapping with
    SPARK_Mode => Off   -- MMIO registers, page overlays, cache flushes
is
    -- After the physical classification is published and before the first
    -- process. One-shot.
    procedure Setup;

    -- Some unit translates (DMA is confined for the devices behind it).
    function Translating return Boolean;

    type Grant_Result is
      (Mapped_In_Domain,   -- Domain names the requester's domain
       Unconfined,         -- the requester's unit does not translate
       Refused);           -- the domain is another owner's, overlap, capacity

    -- Map Pages frames from Physical (identity) into Requester's domain,
    -- binding it to Owner (a process identity word). Refused leaves nothing
    -- mapped.
    procedure Map_Grant
      (Requester : Unsigned_16;
       Owner     : Unsigned_64;
       Physical  : Unsigned_64;
       Pages     : Unsigned_64;
       Writable  : Boolean;
       Domain    : out Unsigned_64;
       Result    : out Grant_Result);

    -- Unmap a run Map_Grant mapped into Domain, then invalidate and wait.
    -- Released: the frames may be reused. False: the invalidation did not
    -- complete in time; the frames must never be reused.
    procedure Unmap_Grant
      (Domain   : Unsigned_64;
       Physical : Unsigned_64;
       Pages    : Unsigned_64;
       Released : out Boolean);

    -- Owner (a process identity word) has ended: its domains keep their
    -- requesters and mappings but may be bound again.
    procedure Release_Owner (Owner : Unsigned_64);

    -- The fault-event vector (InterruptNumbers.IOMMU_FAULT): drain every
    -- unit's fault records into typed records.
    procedure Service_Faults;
end DMA_Remapping;
