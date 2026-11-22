------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  acpi.svc tells devmgr how its startup AML ended (every device
--  initializer, then HID and power discovery): Completed or
--  Completed_With_Errors (the walk ran to its end; some nodes failed and
--  were logged, as ACPICA continues; user decision 2026-11-13) lets devmgr
--  start the driver of a GPIO controller it verified for use.
--
--  @description
--  Why: a GPIO driver's startup parks pins, and device _INI methods touch
--  GPIO pad registers too (the IdeaPad's TPD0._INI). The two must not
--  overlap. Windows' order: every _INI first, then the GPIO driver.
--  devmgr records the controller when the GPIO bootstrap admission
--  verifies it (CuBit.GPIO_Bootstrap_Exchange) and defers its launch; this
--  message, with Completed, releases it. acpi.svc sends it after ALL its
--  startup AML, so devmgr assumes no further ordering. devmgr queues the
--  launch on its deferred launch path and replies at once; it never
--  launches inside the request.
--
--  Transport: the firmware-data binding, as the GPIO bootstrap admission:
--  served only on devmgr's endpoint badged
--  CuBit.Firmware_Data_Exchange.Authority_Tag. A bounded synchronous call
--  DOWN the IPC graph (the requester's own finite deadline, Now + at most
--  Completion_Budget_Milliseconds) that the reply settles as a receipt; it
--  is not a wait for any child's health. devmgr only replies.
--
--  Request (no capability carried), Request_Words words:
--    Request_Word  the requester's request id (never zero)
--    Epoch_Word    the ACPI namespace epoch at the end of the phase
--    Status_Word   Phase_Status'Enum_Rep
--  Reply, label Consumer_Initialization_Complete, Reply_Words words:
--    Reply_Request_Word  the request id
--    Receipt_Word        Receipt'Enum_Rep
--    Released_Word       how many driver launches this declaration released
--  The reply says nothing of which drivers start or whether they are
--  healthy; Released_Word only tells acpi.svc whether any consumer exists
--  for this epoch (zero: it may retire normally on Stop).
--  devmgr records the first declaration per (sender, epoch):
--    * the same (sender, request id) asked again gets the recorded receipt;
--    * the same status for a recorded (sender, epoch) is Received and
--      changes nothing;
--    * a different status for a recorded (sender, epoch) (Incomplete,
--      then Completed) is Conflicting: refused and logged, never a
--      promotion;
--    * only a first status that ran to the end (Ran_To_End: Completed or
--      Completed_With_Errors) releases that sender's deferred launches
--      (each controller launches once); Incomplete and Failed are recorded,
--      logged and launch nothing.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Consumer_Initialization_Exchange with Pure, SPARK_Mode is

   --  After the GPIO bootstrap admission's Admission_Request (16#0251#).
   Consumer_Initialization_Complete : constant Unsigned_32 := 16#0253#;

   Request_Words : constant := 3;
   Request_Word  : constant := 0;
   Epoch_Word    : constant := 1;
   Status_Word   : constant := 2;

   Reply_Words        : constant := 3;
   Reply_Request_Word : constant := 0;
   Receipt_Word       : constant := 1;
   --  How many driver launches this declaration released (Received and
   --  Completed only; 0 otherwise). Zero means devmgr started no consumer
   --  for this epoch: acpi.svc may retire normally on Stop. Nonzero keeps it
   --  RETAINED until consumer quiescence exists (ACPI-QUIESCE-001). A
   --  repeat of the same request answers the recorded count.
   Released_Word      : constant := 2;

   --  The most a requester waits for the receipt (its own deadline).
   Completion_Budget_Milliseconds : constant := 1_000;

   --  How acpi.svc's startup AML ended. Ran_To_End (Completed or
   --  Completed_With_Errors) opens the deferred GPIO launch.
   type Phase_Status is
     (Completed,              --  every initializer and discovery step returned
      Completed_With_Errors,  --  it ran to the end, some steps failed
      Incomplete,             --  it stopped before the end
      Failed);                --  it could not run
   for Phase_Status use
     (Completed             => 1,
      Completed_With_Errors => 2,
      Incomplete            => 3,
      Failed                => 4);

   type Receipt is
     (Received,      --  recorded (Completed: deferred launches are queued)
      Malformed,     --  request id zero, or an unknown status
      Not_Approved,  --  not through the firmware-data badge
      Conflicting,   --  another status was declared for this epoch already
      Capacity);     --  devmgr's declaration records could not grow: nothing recorded
   for Receipt use
     (Received     => 1,
      Malformed    => 2,
      Not_Approved => 3,
      Conflicting  => 4,
      Capacity     => 5);

   --  The walk reached its end: per-node errors (Completed_With_Errors) were
   --  logged and passed over, as ACPICA's initialization walk does.
   function Ran_To_End (S : Phase_Status) return Boolean is
     (S in Completed | Completed_With_Errors);

   function Valid_Status (Word : Unsigned_64) return Boolean is
     (for some S in Phase_Status => Phase_Status'Enum_Rep (S) = Word);

   function Name (S : Phase_Status) return String is
     (case S is
        when Completed             => "COMPLETED",
        when Completed_With_Errors => "COMPLETED_WITH_ERRORS",
        when Incomplete            => "INCOMPLETE",
        when Failed                => "FAILED");

   function Name (R : Receipt) return String is
     (case R is
        when Received     => "RECEIVED",
        when Malformed    => "MALFORMED",
        when Not_Approved => "NOT_APPROVED",
        when Conflicting  => "CONFLICTING",
        when Capacity     => "CAPACITY");

   function Valid_Receipt (Word : Unsigned_64) return Boolean is
     (for some R in Receipt => Receipt'Enum_Rep (R) = Word);

end CuBit.Consumer_Initialization_Exchange;
