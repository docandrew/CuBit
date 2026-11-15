------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Asking the power manager (power.svc) to turn the machine off (PWR-003
--  slice 1, docs/power-states.md).
--
--  @description
--  Requesters are separate, each by its own capability: procmgr mints
--  power.svc's endpoint badged Desktop_Tag for catalog role Desktop_Role
--  ("power-desktop") and Console_Tag for Console_Role ("power-console"),
--  only for programs the trusted startup plan started. power.svc classifies
--  a request by the kernel-stamped badge alone; an unbadged or unknown one
--  is Not_Approved. The power button is not a requester: acpi.svc reports
--  presses to power.svc (CuBit.Power_Event_Exchange), whose own policy
--  turns one into a Power_Off.
--
--  Request_Label (Request_Words words, no capability carried):
--    Request_Word  the requester's request id (never zero)
--    Action_Word   Action'Enum_Rep
--  power.svc answers at once (it never makes the requester wait for the
--  stop, which ends with the machine off): Reply, label Request_Label,
--  Reply_Words words: Reply_Request_Word, Answer_Word (Answer'Enum_Rep).
--  Accepted means the stop has begun; the requester is stopped with
--  everything else.
--
--  What follows an accepted request (power.svc's sequence):
--    1. CuBit.System_Stops.Stop_Apps_Operation to procmgr;
--    2. Power_Off: CuBit.Power_Event_Exchange.Prepare_To_Sleep (S5), while
--       ACPI and its AML providers still run;
--    3. CuBit.System_Stops.Stop_Operation: the rest, keeping the protected
--       tail (acpi.svc and what it depends on) for Power_Off;
--    4. Power_Off: Enter_Sleep_State (S5);
--    5. if that is unavailable or fails: Halted, the fallback: nothing
--       runs but procmgr, devmgr, acpi.svc and power.svc, the storage is
--       committed and detached, and the serial console and the log say the
--       machine may be switched off (it idles; there is no kernel power-off
--       or reset path to take instead).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Power_Requests with Pure, SPARK_Mode is

   --  Catalog roles (native-runtime-services.ccl): power.svc registers
   --  Service_Role; Desktop_Role and Console_Role are its requesters'.
   Service_Role : constant := 42;
   Desktop_Role : constant := 43;
   Console_Role : constant := 44;

   --  "PWRDSK" / "PWRCON" and a version: kernel-stamped badges.
   Desktop_Tag : constant Unsigned_64 := 16#5057_5244_534B_0001#;
   Console_Tag : constant Unsigned_64 := 16#5057_5243_4F4E_0001#;

   type Requester is (Unknown, Desktop, Console, Power_Button);

   function Requester_Of (Tag : Unsigned_64) return Requester is
     (if Tag = Desktop_Tag then Desktop
      elsif Tag = Console_Tag then Console
      else Unknown);

   --  The power block of labels (CuBit.Power_Event_Exchange has 16#0270#
   --  .. 16#0272#).
   Request_Label : constant Unsigned_32 := 16#0273#;

   Request_Words : constant := 2;
   Request_Word  : constant := 0;
   Action_Word   : constant := 1;

   Reply_Words        : constant := 2;
   Reply_Request_Word : constant := 0;
   Answer_Word        : constant := 1;

   --  The most a requester waits for the answer (its own deadline).
   Answer_Budget_Milliseconds : constant := 2_000;

   type Action is
     (Power_Off,   --  stop everything, then firmware power-off (S5)
      Halt);       --  stop everything, then idle: safe to switch off
   for Action use (Power_Off => 1, Halt => 2);

   type Answer is
     (Accepted,       --  the stop has begun
      Busy,           --  a stop is already running
      Not_Approved,   --  not a requester's badge
      Malformed,      --  id zero, an unknown action
      Not_Ready);     --  power.svc cannot ask procmgr (not registered)
   for Answer use
     (Accepted     => 1,
      Busy         => 2,
      Not_Approved => 3,
      Malformed    => 4,
      Not_Ready    => 5);

   function Valid_Action (Word : Unsigned_64) return Boolean is
     (for some A in Action => Action'Enum_Rep (A) = Word);
   function To_Action (Word : Unsigned_64) return Action is
     (if Word = Action'Enum_Rep (Halt) then Halt else Power_Off)
   with Pre => Valid_Action (Word);

   function Valid_Answer (Word : Unsigned_64) return Boolean is
     (for some A in Answer => Answer'Enum_Rep (A) = Word);
   function To_Answer (Word : Unsigned_64) return Answer is
     (if Word = Answer'Enum_Rep (Busy) then Busy
      elsif Word = Answer'Enum_Rep (Not_Approved) then Not_Approved
      elsif Word = Answer'Enum_Rep (Malformed) then Malformed
      elsif Word = Answer'Enum_Rep (Not_Ready) then Not_Ready
      else Accepted)
   with Pre => Valid_Answer (Word);

   --  How a stop ended for power.svc (its log line and transition record).
   type Final_Outcome is
     (Firmware_Off_Requested,  --  Enter_Sleep_State was sent (on success it never answers)
      Halted_No_Firmware,      --  no acpi-power endpoint: halted
      Halted_Firmware_Refused, --  acpi.svc answered Enter_Sleep_State: halted
      Halted_Firmware_Silent,  --  no answer by the deadline and still running
      Halted_By_Request);      --  Halt was asked for

   function Name (A : Action) return String is
     (case A is when Power_Off => "POWER_OFF", when Halt => "HALT");

   function Name (A : Answer) return String is
     (case A is
        when Accepted     => "ACCEPTED",
        when Busy         => "BUSY",
        when Not_Approved => "NOT_APPROVED",
        when Malformed    => "MALFORMED",
        when Not_Ready    => "NOT_READY");

   function Name (R : Requester) return String is
     (case R is
        when Unknown      => "UNKNOWN",
        when Desktop      => "DESKTOP",
        when Console      => "CONSOLE",
        when Power_Button => "POWER_BUTTON");
end CuBit.Power_Requests;
