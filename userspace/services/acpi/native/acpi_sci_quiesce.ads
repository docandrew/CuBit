pragma Ada_2022;
with Interfaces; use Interfaces;
with ACPI_SCI_Registers;
-- SCI-DELIVERY: what Disarm writes, and what it reports, as pure functions
-- (the native glue, ACPI_SCI_Events.Disarm, applies them through its
-- windows). Handover rule: every source is disabled, no status bit is
-- cleared. An event that happens between this owner's Disarm and the next
-- owner's Arm stays latched in its status bit and is delivered when the next
-- owner enables that source: nothing is lost, and nothing asserts the SCI
-- while no owner dispatches it.
package ACPI_SCI_Quiesce with SPARK_Mode, Pure is
   package R renames ACPI_SCI_Registers;

   -- PM1 enable after Disarm: every fixed event off (those Arm enabled and
   -- any a consumer enabled); bits that are not fixed events, such as
   -- PCIEXP_WAKE_DIS, are kept as found.
   function Fixed_Mask_Of (Events : R.Fixed_Set) return Unsigned_16
     with Post => (for all E in R.Fixed_Event =>
                     (if Events (E) then (Fixed_Mask_Of'Result and R.Fixed_Mask (E)) /= 0));

   All_Fixed : constant R.Fixed_Set := [others => True];

   function Disarmed_PM1_Enable (Current : Unsigned_16) return Unsigned_16 is
     (Current and not Fixed_Mask_Of (All_Fixed))
     with Post => (for all E in R.Fixed_Event =>
                     (Disarmed_PM1_Enable'Result and R.Fixed_Mask (E)) = 0)
                  and then (Disarmed_PM1_Enable'Result and not Fixed_Mask_Of (All_Fixed))
                           = (Current and not Fixed_Mask_Of (All_Fixed));

   -- Every GPE enable byte after Disarm: all masked (the next owner's
   -- consumers enable what they bind).
   Disarmed_GPE_Enable : constant Unsigned_8 := 0;
   pragma Compile_Time_Error (Disarmed_GPE_Enable /= 0, "Disarm masks every GPE");

   -- The typed result the shutdown controller's barrier consumes.
   type Outcome is (Quiesced, Pending);
   type Pending_Reason is
     (No_Reason,
      Fixed_Disable_Refused,   -- a PM1 enable write was refused
      GPE_Mask_Refused,        -- a GPE enable write was refused
      Acknowledge_Refused);    -- the line could not be acknowledged
   type Result (State : Outcome := Quiesced) is record
      case State is
         when Quiesced => null;
         when Pending  => Why : Pending_Reason := Fixed_Disable_Refused;
      end case;
   end record;

   -- First failing step wins; all steps done is Quiesced. Not armed (no
   -- SCI line, or already disarmed) needs only the acknowledgement.
   function Report
     (Fixed_Done, GPE_Done, Acknowledged : Boolean) return Result
   is (if not Fixed_Done then (State => Pending, Why => Fixed_Disable_Refused)
       elsif not GPE_Done then (State => Pending, Why => GPE_Mask_Refused)
       elsif not Acknowledged then (State => Pending, Why => Acknowledge_Refused)
       else (State => Quiesced))
     with Post => (Report'Result.State = Quiesced) =
                    (Fixed_Done and then GPE_Done and then Acknowledged);

   -- Which event items are this service's SCI doorbell: the kernel's IRQ
   -- doorbell label, from the kernel (no publisher), with the line routed
   -- here. Anything else is Foreign and left alone.
   type Doorbell_Class is (Foreign, No_Line, SCI_Doorbell);
   function Classify
     (Doorbell_Label, From_Kernel, Line_Held : Boolean) return Doorbell_Class
   is (if not (Doorbell_Label and then From_Kernel) then Foreign
       elsif not Line_Held then No_Line
       else SCI_Doorbell)
     with Post => (Classify'Result = SCI_Doorbell) =
                    (Doorbell_Label and then From_Kernel and then Line_Held);

   -- Acknowledge_Only's result (ACPI_SCI_Events).
   type Acknowledgement is
     (Not_SCI_Doorbell,      -- a foreign item: untouched, the caller handles it
      No_SCI_Line,           -- a kernel doorbell, but no SCI line is held here
      Acknowledged,          -- the line was acknowledged (nothing dispatched)
      Acknowledge_Refused);  -- the kernel refused (typed reason not kept)
end ACPI_SCI_Quiesce;
