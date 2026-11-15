------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  What the power manager (power.svc) asks of acpi.svc (PWR-003 slice 1,
--  docs/power-states.md): a watch for power-button presses; Prepare_To_Sleep
--  while ACPI and its AML providers are still up; and, at the very end,
--  Enter_Sleep_State. acpi.svc reports; it does not decide.
--
--  @description
--  Order (ACPI 6.6, 16.1.7), for S5 in this slice:
--    1. procmgr stops the apps (CuBit.System_Stops.Stop_Apps_Operation);
--    2. Prepare_To_Sleep (S5): \_TTS (5) if present, then \_PTS (5) if
--       present, while the EC (inside acpi.svc), the GPIO drivers and every
--       region handler AML may touch still run;
--    3. procmgr stops the rest (services, storage, drivers) keeping the
--       protected tail: acpi.svc, the plan entries it depends on, and the
--       devmgr drivers it declared as its AML providers;
--    4. Enter_Sleep_State (S5): the sleep registers. Never answered on success.
--
--  Call direction (the IPC graph stays a DAG): power.svc calls acpi.svc,
--  never the other way. acpi.svc answers a watch when a press is counted;
--  until then it keeps the reply (CuBit.Handles.IPC.Save_Reply) and blocks
--  on nothing. power.svc submits the watch asynchronously (capSubmit).
--
--  Transport and authority: acpi.svc's registration endpoint (its
--  DRIVER_ACPI role), through a copy badged with Authority_Tag. procmgr
--  mints it for catalog service role Service_Role ("acpi-power") only for
--  com.cubit.power started by the trusted startup plan; every other ACPI
--  client holds ACPI_Protocol.Observer_Tag. acpi.svc serves these three
--  operations only when the kernel-stamped authorityTag of the request is
--  Authority_Tag; any other tag is answered Not_Approved and changes
--  nothing. acpi.svc's manifest needs no new request.
--
--  Button_Watch (Watch_Request_Words words, no capability carried):
--    Request_Word  the watcher's request id (never zero)
--    Seen_Word     the press count the watcher last saw (0 at first)
--  acpi.svc counts presses from its own start, from 0, adding one per
--  press it handles: the fixed event (PM1 PWRBTN_STS, cleared by writing
--  one), a control-method button's Notify (PNP0C0C, 16#80#), or the Intel
--  HID event device's power-button event (INTC1070, 16#CE#). Then:
--    * Count > Seen: answered at once, Pressed (presses are coalesced into
--      the count, never lost);
--    * Count = Seen: the reply is kept and answered Pressed at the next
--      press. One watch is kept at a time: a newer one answers the kept
--      one Superseded first;
--    * Seen > Count (the watcher saw an earlier acpi.svc instance's count):
--      answered at once, Counter_Reset, with the current count. No press is
--      invented;
--    * no power button at all (the FADT's PWR_BUTTON flag says there is no
--      fixed one and the namespace has no PNP0C0C or INTC1070): at once,
--      No_Power_Button;
--    * acpi.svc stopping or retiring: a kept watch is answered Stopping;
--    * the reply cannot be kept (the kernel refused Save_Reply for its
--      transport resources): answered at once, Watch_Unavailable. Transient:
--      the watcher submits the watch again after Watch_Retry_Milliseconds
--      (and logs each one); distinct from Stopping (the owner retires) and
--      No_Power_Button (there is none). A watch that keeps failing leaves
--      the button unwatched, visibly; it never blocks a stop asked by other
--      means.
--  Reply, label Button_Watch, Watch_Reply_Words words:
--    Reply_Request_Word  the request id
--    Watch_Status_Word   Watch_Status'Enum_Rep
--    Count_Word          acpi.svc's press count now
--    Source_Word         Press_Source'Enum_Rep of the latest press
--                        (No_Press while the count is 0)
--
--  Prepare_To_Sleep (Sleep_Request_Words words, no capability carried):
--    Request_Word  the request id (never zero)
--    State_Word    Sleep_State'Enum_Rep (this slice asks only S5)
--    Confirm_Word  Sleep_Confirmation, exactly
--  acpi.svc, while every AML provider still runs: evaluates \_TTS (State)
--  if present, then \_PTS (State) if present, each once, bounded; then
--  evaluates the state's package (\_S5) and copies and validates
--  SLP_TYPa / SLP_TYPb (each 0 .. 7) for Enter_Sleep_State, which then
--  needs no AML. An absent \_TTS or \_PTS is not a failure. The copy is
--  acpi.svc's own; the reply's values are for the log only.
--  Reply, label Prepare_To_Sleep, Prepare_Reply_Words words:
--    Reply_Request_Word  the request id
--    Prepare_Status_Word Prepare_Status'Enum_Rep
--    Detail_Word         Method_Failed and No_Sleep_Package: the failing
--                        object's ACPI status code; else 0
--    Sleep_Types_Word    the copied SLP_TYPa (bits 0 .. 7) and SLP_TYPb
--                        (bits 8 .. 15) when Prepared or Method_Failed;
--                        else 0 (Sleep_Types_Of)
--  power.svc waits until its own deadline, at most
--  Prepare_Budget_Milliseconds; a failure or no answer is logged and the
--  stop goes on (Linux's acpi_power_off_prepare does the same).
--
--  Enter_Sleep_State (Sleep_Request_Words words, the same layout), called
--  only after procmgr reported the system stopped but the protected tail:
--  acpi.svc, in this order (ACPICA's acpi_hw_legacy_sleep):
--    1. Refuse (Not_Prepared) a state with no valid copied sleep types (no
--       Prepare_To_Sleep for it, or one answered No_Sleep_Package). No AML
--       runs from here on.
--    2. Take SLP_TYPa (and SLP_TYPb) from the copy Prepare_To_Sleep made.
--    3. Disable and clear every GPE (no wake source is armed for S5);
--       clear WAK_STS.
--    4. PM1 control (the classified sleep registers; the IdeaPad is
--       PM1A_ONLY): write SLP_TYPa into SLP_TYP (bits 10 .. 12) keeping the
--       register's other bits but SLP_EN, then the same value with SLP_EN
--       (bit 13) set: two writes (PM1_Sleep_Value). PM1A_And_B writes
--       SLP_TYPb to PM1b the same way between them. A hardware-reduced
--       platform writes SLP_TYPx (bits 2 .. 4) with SLP_EN (bit 5) to the
--       FADT's sleep control register instead.
--  On success the machine is off and no reply is ever sent. acpi.svc
--  replies only when it could not: at once for a refusal, or, after the
--  write, when the machine still runs Settle_Milliseconds later
--  (Still_Running).
--  Reply, label Enter_Sleep_State, Enter_Reply_Words words:
--    Reply_Request_Word  the request id
--    Enter_Status_Word   Enter_Status'Enum_Rep
--    Detail_Word         0 (reserved)
--  power.svc waits until its own deadline, at most Enter_Budget_Milliseconds;
--  any reply, a timeout or the call ending without a reply means firmware
--  power-off failed, and power.svc halts (CuBit.Power_Requests).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Power_Event_Exchange with Pure, SPARK_Mode is

   --  The catalog role (native-runtime-services.ccl, "acpi-power"): a
   --  client of it gets acpi.svc's endpoint badged Authority_Tag.
   Service_Role : constant := 41;
   --  "PWRMGR" and a version: kernel-stamped on the copy procmgr mints, so
   --  knowing the number grants nothing.
   Authority_Tag : constant Unsigned_64 := 16#5057_524D_4752_0001#;

   --  The power block of labels (16#0270# .. 16#027F#): acpi.svc's three
   --  first, then CuBit.Power_Requests and CuBit.System_Stops.
   Button_Watch      : constant Unsigned_32 := 16#0270#;
   Prepare_To_Sleep  : constant Unsigned_32 := 16#0271#;
   Enter_Sleep_State : constant Unsigned_32 := 16#0272#;

   Request_Word       : constant := 0;
   Reply_Request_Word : constant := 0;

   ---------------------------------------------------------------------------
   --  Button_Watch
   ---------------------------------------------------------------------------
   Watch_Request_Words : constant := 2;
   Seen_Word    : constant := 1;

   Watch_Reply_Words  : constant := 4;
   Watch_Status_Word  : constant := 1;
   Count_Word         : constant := 2;
   Source_Word        : constant := 3;

   type Watch_Status is
     (Pressed,          --  Count > Seen: at least one press since
      Superseded,       --  a newer watch replaced this kept one
      Counter_Reset,    --  Seen > Count: a new acpi.svc instance; rebaseline
      Not_Approved,     --  not through the Authority_Tag badge
      Malformed,        --  request id zero, or the wrong length
      No_Power_Button,  --  the platform has no power button acpi.svc can see
      Stopping,         --  acpi.svc is stopping or retiring
      Watch_Unavailable);  --  the reply could not be kept: watch again later
   for Watch_Status use
     (Pressed           => 1,
      Superseded        => 2,
      Counter_Reset     => 3,
      Not_Approved      => 4,
      Malformed         => 5,
      No_Power_Button   => 6,
      Stopping          => 7,
      Watch_Unavailable => 8);

   --  How long the watcher waits before submitting a watch again after
   --  Watch_Unavailable.
   Watch_Retry_Milliseconds : constant := 1_000;

   type Press_Source is
     (No_Press,         --  nothing counted yet
      Fixed_Event,      --  PM1 PWRBTN_STS
      Control_Method,   --  PNP0C0C, Notify 16#80#
      Intel_HID);       --  INTC1070, event 16#CE#
   for Press_Source use
     (No_Press       => 0,
      Fixed_Event    => 1,
      Control_Method => 2,
      Intel_HID      => 3);

   ---------------------------------------------------------------------------
   --  Prepare_To_Sleep and Enter_Sleep_State
   ---------------------------------------------------------------------------
   --  The ACPI sleeping states (\_S1 .. \_S5); this slice asks only S5.
   type Sleep_State is (S1, S2, S3, S4, S5);
   for Sleep_State use (S1 => 1, S2 => 2, S3 => 3, S4 => 4, S5 => 5);

   Sleep_Request_Words : constant := 3;
   State_Word          : constant := 1;
   Confirm_Word        : constant := 2;
   --  "SLEEP" and a version: a stray or truncated message never sleeps.
   Sleep_Confirmation : constant Unsigned_64 := 16#534C_4545_5000_0001#;

   Prepare_Reply_Words : constant := 4;
   Enter_Reply_Words   : constant := 3;
   Prepare_Status_Word : constant := 1;
   Enter_Status_Word   : constant := 1;
   Detail_Word         : constant := 2;
   Sleep_Types_Word    : constant := 3;

   type Prepare_Status is
     (Prepared,           --  \_TTS / \_PTS returned or are absent; types copied
      Not_Approved,       --  not through the Authority_Tag badge
      Malformed,          --  id zero, wrong length, state or confirmation
      Not_Ready,          --  the namespace is not loaded
      Unsupported_State,  --  acpi.svc does not prepare this state (yet)
      Method_Failed,      --  \_TTS or \_PTS failed (Detail_Word); types copied
      No_Sleep_Package);  --  no valid \_Sx package: Enter_Sleep_State will refuse
   for Prepare_Status use
     (Prepared          => 1,
      Not_Approved      => 2,
      Malformed         => 3,
      Not_Ready         => 4,
      Unsupported_State => 5,
      Method_Failed     => 6,
      No_Sleep_Package  => 7);

   --  Whether Enter_Sleep_State can follow (the types were copied).
   function Types_Copied (S : Prepare_Status) return Boolean is (S in Prepared | Method_Failed);

   type Enter_Status is
     (Enter_Not_Approved,      --  not through the Authority_Tag badge
      Enter_Malformed,         --  id zero, wrong length, state or confirmation
      Not_Prepared,            --  no valid copied sleep types for this state
      No_Sleep_Registers,      --  no PM1 control (nor sleep control) register
      Write_Refused,           --  the kernel refused the register write
      Still_Running);          --  written, and the machine did not go off
   for Enter_Status use
     (Enter_Not_Approved => 1,
      Enter_Malformed    => 2,
      Not_Prepared       => 3,
      No_Sleep_Registers => 4,
      Write_Refused      => 5,
      Still_Running      => 6);

   --  The PM1 control bits acpi.svc writes (ACPI 6.6, 4.8.3.2.1; 4.8.3.7 for
   --  the hardware-reduced sleep control register).
   SLP_TYP_Shift        : constant := 10;
   SLP_TYP_Mask         : constant Unsigned_16 := 16#1C00#;
   SLP_EN               : constant Unsigned_16 := 16#2000#;
   Reduced_SLP_TYP_Shift : constant := 2;
   Reduced_SLP_TYP_Mask  : constant Unsigned_8 := 16#1C#;
   Reduced_SLP_EN        : constant Unsigned_8 := 16#20#;

   --  How long \_TTS and \_PTS may take, at most (power.svc's deadline).
   Prepare_Budget_Milliseconds : constant := 5_000;
   --  How long acpi.svc lets the machine take to go off after the write
   --  before it answers Still_Running.
   Settle_Milliseconds : constant := 2_000;
   --  The most power.svc waits for Enter_Sleep_State (its own deadline).
   Enter_Budget_Milliseconds : constant := 5_000;

   function Valid_Watch_Status (Word : Unsigned_64) return Boolean is
     (for some S in Watch_Status => Watch_Status'Enum_Rep (S) = Word);
   function Valid_Source (Word : Unsigned_64) return Boolean is
     (for some S in Press_Source => Press_Source'Enum_Rep (S) = Word);
   function Valid_State (Word : Unsigned_64) return Boolean is
     (for some S in Sleep_State => Sleep_State'Enum_Rep (S) = Word);
   function Valid_Prepare_Status (Word : Unsigned_64) return Boolean is
     (for some S in Prepare_Status => Prepare_Status'Enum_Rep (S) = Word);
   function Valid_Enter_Status (Word : Unsigned_64) return Boolean is
     (for some S in Enter_Status => Enter_Status'Enum_Rep (S) = Word);

   function To_Watch_Status (Word : Unsigned_64) return Watch_Status is
     (if Word = Watch_Status'Enum_Rep (Pressed) then Pressed
      elsif Word = Watch_Status'Enum_Rep (Superseded) then Superseded
      elsif Word = Watch_Status'Enum_Rep (Counter_Reset) then Counter_Reset
      elsif Word = Watch_Status'Enum_Rep (Not_Approved) then Not_Approved
      elsif Word = Watch_Status'Enum_Rep (Malformed) then Malformed
      elsif Word = Watch_Status'Enum_Rep (No_Power_Button) then No_Power_Button
      elsif Word = Watch_Status'Enum_Rep (Stopping) then Stopping
      else Watch_Unavailable)
   with Pre => Valid_Watch_Status (Word);

   function To_Source (Word : Unsigned_64) return Press_Source is
     (if Word = Press_Source'Enum_Rep (Fixed_Event) then Fixed_Event
      elsif Word = Press_Source'Enum_Rep (Control_Method) then Control_Method
      elsif Word = Press_Source'Enum_Rep (Intel_HID) then Intel_HID
      else No_Press)
   with Pre => Valid_Source (Word);

   --  The copied sleep types, as Sleep_Types_Word carries them.
   Sleep_Type_Bits : constant := 8;
   function Sleep_Types_Of (A, B : Unsigned_16) return Unsigned_64 is
     (Unsigned_64 (A and 7) or Shift_Left (Unsigned_64 (B and 7), Sleep_Type_Bits));
   function Type_A_Of (Word : Unsigned_64) return Unsigned_64 is (Word and 7);
   function Type_B_Of (Word : Unsigned_64) return Unsigned_64 is (Shift_Right (Word, Sleep_Type_Bits) and 7);

   --  A sleep request acpi.svc can act on (who asks is checked separately).
   function Sleep_Request_Valid (Length : Unsigned_64; Id, State, Confirm : Unsigned_64) return Boolean is
     (Length = Sleep_Request_Words and then Id /= 0 and then Valid_State (State)
      and then Confirm = Sleep_Confirmation);

   --  The PM1 control value of a sleep write: Current with SLP_TYP set to
   --  Kind, and SLP_EN set for the second write.
   subtype Sleep_Type is Unsigned_16 range 0 .. 7;
   function PM1_Sleep_Value (Current : Unsigned_16; Kind : Sleep_Type; Enable : Boolean)
     return Unsigned_16 is
     ((Current and not (SLP_TYP_Mask or SLP_EN))
      or Shift_Left (Kind, SLP_TYP_Shift)
      or (if Enable then SLP_EN else 0));

   function Name (S : Watch_Status) return String is
     (case S is
        when Pressed         => "PRESSED",
        when Superseded      => "SUPERSEDED",
        when Counter_Reset   => "COUNTER_RESET",
        when Not_Approved    => "NOT_APPROVED",
        when Malformed       => "MALFORMED",
        when No_Power_Button => "NO_POWER_BUTTON",
        when Stopping        => "STOPPING",
        when Watch_Unavailable => "WATCH_UNAVAILABLE");

   function Name (S : Press_Source) return String is
     (case S is
        when No_Press       => "NO_PRESS",
        when Fixed_Event    => "FIXED_EVENT",
        when Control_Method => "CONTROL_METHOD",
        when Intel_HID      => "INTEL_HID");

   function Name (S : Prepare_Status) return String is
     (case S is
        when Prepared          => "PREPARED",
        when Not_Approved      => "NOT_APPROVED",
        when Malformed         => "MALFORMED",
        when Not_Ready         => "NOT_READY",
        when Unsupported_State => "UNSUPPORTED_STATE",
        when Method_Failed     => "METHOD_FAILED",
        when No_Sleep_Package  => "NO_SLEEP_PACKAGE");

   function Name (S : Enter_Status) return String is
     (case S is
        when Enter_Not_Approved => "NOT_APPROVED",
        when Enter_Malformed    => "MALFORMED",
        when Not_Prepared       => "NOT_PREPARED",
        when No_Sleep_Registers => "NO_SLEEP_REGISTERS",
        when Write_Refused      => "WRITE_REFUSED",
        when Still_Running      => "STILL_RUNNING");

end CuBit.Power_Event_Exchange;
