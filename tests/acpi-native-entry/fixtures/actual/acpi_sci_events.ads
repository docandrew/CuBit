pragma Ada_2022;
with Interfaces;
with CuBit.ACPI_Windows;
with CuBit.Firmware_Snapshot;
with CuBit.Messages;
with CuBit.Process_IDs;
with ACPI_SCI_Registers;
with ACPI_SCI_Quiesce;
-- SCI-DELIVERY: the thin native glue between the kernel's SCI line and the
-- ACPI event consumers, in acpi.svc's one serialized loop.
--
-- The line (CuBit.ACPI_Windows.Grant.SCI) is level-triggered. The kernel
-- masks it when it delivers it (the IRQ doorbell event) and unmasks it only
-- when this service acknowledges (CuBit.Handles.Lines.Acknowledge, which is
-- ENABLE_IRQ's owner form: no system call exists only for it). One doorbell:
--   1. read PM1a/PM1b status and enable, and every GPE status/enable byte,
--      through the launch grant's I/O windows;
--   2. each enabled fixed event that is set: clear its status (write 1),
--      then hand it to Fixed_Event; Unbound disables it;
--   3. each enabled GPE that is set: hand it to GPE_Event, which owns that
--      GPE's status and enable (ACPI 6.5, 5.6.4: mask, clear edge status,
--      run, clear level status, enable); Unbound masks it and clears it;
--   4. acknowledge the line.
-- Whatever asserted after step 1 keeps the (level) line asserted, so the
-- kernel delivers again at once after step 4: no event is lost, and none is
-- delivered while this service still has to look.
--
-- A GPE_Event consumer must leave its GPE either cleared or masked when it
-- returns Handled (it may defer work with the GPE masked and later Clear_GPE
-- and Enable_GPE): an enabled GPE whose status stays set re-delivers the SCI
-- after every acknowledgement.
package ACPI_SCI_Events is
   package R renames ACPI_SCI_Registers;
   subtype Word is Interfaces.Unsigned_64;

   type Arm_Outcome is
     (Armed,
      No_SCI_Line,             -- the launch grant has no SCI (procmgr logged why)
      FADT_Unreadable,         -- the snapshot's FADT is absent or unreadable
      FADT_Malformed,          -- ACPI_FADT.Decode refused it
      Layout_Refused,          -- ACPI_SCI_Registers refused it (Refusal)
      Register_Outside_Grant,  -- a PM1/GPE register no I/O window covers
      ACPI_Mode_Port_Outside_Grant, -- SCI_EN=0 and no window covers SMI_CMD
      ACPI_Mode_Not_Entered,   -- ACPI_ENABLE written, SCI_EN not set by the deadline
      Enable_Refused);         -- a port access through a window was refused

   -- Once, before serving. When PM1a control's SCI_EN is 0 and the FADT
   -- defines a transition, it enters ACPI mode first (ACPI_ENABLE to SMI_CMD,
   -- then SCI_EN within a named deadline; never ACPI_DISABLE). Enables the
   -- fixed-hardware power and sleep
   -- buttons the FADT declares (after clearing a stale press); every other
   -- source is enabled by its consumer. Not Armed: SCI events are a typed
   -- unavailable state; nothing polls instead.
   procedure Arm
     (Grant : CuBit.ACPI_Windows.Grant;
      Tables : CuBit.Firmware_Snapshot.Authority;
      Outcome : out Arm_Outcome;
      Layout_Why : out R.Refusal);
   function Is_Armed return Boolean;
   -- Successful SCI admission plus FADT fixed-hardware declaration; no probe.
   function Fixed_Power_Button_Available return Boolean;

   ---------------------------------------------------------------------------
   -- Disarm: Arm's counterpart, for live replacement and clean shutdown.
   -- Stops dispatching; disables every PM1 fixed event and masks every GPE,
   -- clearing no status (an event in the handover gap stays latched for the
   -- next owner, which delivers it when it enables the source); then
   -- acknowledges the line so this owner never holds it masked. After it,
   -- a doorbell is only acknowledged, never dispatched, until this process
   -- exits (exit releases the line in the kernel). Pending is retriable:
   -- call Disarm again; Quiesced is final. No SCI line: Quiesced at once.
   ---------------------------------------------------------------------------
   procedure Disarm (Result : out ACPI_SCI_Quiesce.Result);

   -- Acknowledge-only, for loops that drain events without dispatching
   -- them (retirement, Park: One.Refuse_Pending): when Item is this
   -- service's SCI doorbell, acknowledge the line so a retained or
   -- replaced generation never holds a shared level line masked. No
   -- consumer, no logging, no AML, no hardware register access. A foreign
   -- item is left alone (Not_SCI_Doorbell): the caller handles it.
   procedure Acknowledge_Only
     (Publisher : CuBit.Process_IDs.Process_ID; Item : CuBit.Messages.Message;
      Result : out ACPI_SCI_Quiesce.Acknowledgement);

   ---------------------------------------------------------------------------
   -- Register primitives for consumers (the shapes of ACPI_GPE_Lifecycle's
   -- Mask/Clear/Enable formals, with this outcome type). Only after Arm.
   -- Clear changes status only; Mask/Enable change enable only.
   ---------------------------------------------------------------------------
   type IO_Outcome is (Completed, Refused);
   procedure Mask_GPE (GPE : R.GPE_Number; Result : out IO_Outcome);
   procedure Clear_GPE (GPE : R.GPE_Number; Result : out IO_Outcome);
   procedure Enable_GPE (GPE : R.GPE_Number; Result : out IO_Outcome);
   procedure Enable_Fixed (Event : R.Fixed_Event; Result : out IO_Outcome);
   procedure Disable_Fixed (Event : R.Fixed_Event; Result : out IO_Outcome);

   type Disposition is (Handled, Unbound);

   ---------------------------------------------------------------------------
   -- BINDING POINT for the ACPI agent's consumers (AML _Lxx/_Exx, EC _Qxx,
   -- Notify, lid/battery/AC): instantiate with them and pass Handle_Event to
   -- ACPI_Native_Server.With_Dispatch as its Handle_Event. Both run in the
   -- serialized loop, never in interrupt context; no synchronous upward IPC.
   ---------------------------------------------------------------------------
   generic
      -- Status already cleared. Handled keeps it enabled.
      with procedure Fixed_Event (Event : R.Fixed_Event; Result : out Disposition);
      -- Status and enable untouched: the consumer owns them (see above).
      with procedure GPE_Event (GPE : R.GPE_Number; Result : out Disposition);
   package With_Consumer is
      -- The IRQ doorbell is the SCI; every other event goes on to
      -- ACPI_Native_Events.Handle.
      procedure Handle_Event
        (Publisher : CuBit.Process_IDs.Process_ID; Item : CuBit.Messages.Message;
         Stop : out Boolean; Handled : out Boolean);
   end With_Consumer;

   -- The minimal consumers until the ACPI agent's are bound: log the power
   -- and sleep buttons (Handled); every other fixed event and every GPE is
   -- logged once and Unbound (so disabled or masked, and cleared).
   procedure Log_Fixed_Event (Event : R.Fixed_Event; Result : out Disposition);
   procedure Unbound_GPE (GPE : R.GPE_Number; Result : out Disposition);

   type Statistics is record
      Interrupts, Acknowledged, Acknowledge_Refusals : Word := 0;
      Fixed_Events, GPE_Events, Unbound_Events, Register_Refusals : Word := 0;
      -- The line's kernel delivery count at the last acknowledgement.
      Kernel_Deliveries : Word := 0;
   end record;
   function Observe return Statistics;
end ACPI_SCI_Events;
