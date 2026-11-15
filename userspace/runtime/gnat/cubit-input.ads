------------------------------------------------------------------------------
--  CuBit typed input-source publication protocol.
--
--  A report is a state-bearing message, not an unauthenticated device event.
--  It is accepted only through a publication capability carrying the input
--  badge (Publisher_Tag, minted by devmgr or procmgr for an input role's
--  publish notification), and its source is the publisher's identity as the
--  kernel reports it; the sequence, generation, and recovery flag make
--  bounded transport loss observable. This is the driver-facing half of the future input.svc stream.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

with CuBit.Messages;

package CuBit.Input with SPARK_Mode => On is

   OP_SOURCE_REPORT : constant Unsigned_32 := 16#0A00#;

   --  The badge on every input publication capability ("INPUT", version 1).
   Publisher_Tag : constant Unsigned_64 := 16#494E_5055_5400_0001#;

   --  The roles whose publish notifications carry Publisher_Tag.
   function Is_Input_Role (Role : Unsigned_64) return Boolean is
     (Role in CuBit.Messages.DRIVER_KEYBOARD | CuBit.Messages.DRIVER_MOUSE);

   --  RELATIVE_POINTER reports carry displacement (Pointer_Pending wire
   --  layout, ACCUMULABLE_DISPLACEMENT); ABSOLUTE_POINTER reports carry a
   --  normalized position (Absolute_Pointer wire layout, REPLACEABLE_STATE).
   type Device_Class is (KEYBOARD, RELATIVE_POINTER, ABSOLUTE_POINTER);
   for Device_Class use
     (KEYBOARD => 1, RELATIVE_POINTER => 2, ABSOLUTE_POINTER => 3);

   type Delivery_Class is
     (ORDERED_TRANSITION, ACCUMULABLE_DISPLACEMENT, REPLACEABLE_STATE);
   for Delivery_Class use
     (ORDERED_TRANSITION       => 1,
      ACCUMULABLE_DISPLACEMENT => 2,
      REPLACEABLE_STATE        => 3);

   type Report_Flag is (RESYNCHRONIZE);
   type Report_Flags is array (Report_Flag) of Boolean with Pack;
   NO_REPORT_FLAGS : constant Report_Flags := (others => False);

   subtype Source_Generation is Unsigned_32;
   subtype Source_Sequence is Unsigned_64;

   type Source_Report is record
      --  The publisher's identity (Decode sets it from the kernel's report;
      --  a publisher leaves it zero).
      sourceIdentity : Unsigned_64 := 0;
      sequence    : Source_Sequence := 0;
      generation  : Source_Generation := 0;
      device      : Device_Class := KEYBOARD;
      delivery    : Delivery_Class := ORDERED_TRANSITION;
      flags       : Report_Flags := NO_REPORT_FLAGS;
      payload     : Unsigned_64 := 0;
      snapshot    : Unsigned_64 := 0;
   end record;

   --  Pointer snapshot (relative and absolute): buttons in low eight bits, optional driver
   --  acquisition time in the high 56 bits. Stored time is milliseconds + 1;
   --  zero means unavailable. No wrap: unrepresentable clocks are unstamped.
   --  Keyboard snapshots and the four-word wire envelope are unchanged.
   Maximum_Pointer_Time : constant Unsigned_64 := 16#00FF_FFFF_FFFF_FFFE#;
   function Pointer_Time (Snapshot : Unsigned_64) return Unsigned_64 is
     (if Shift_Right (Snapshot, 8) = 0 then Unsigned_64'Last
      else Shift_Right (Snapshot, 8) - 1);
   function Pointer_Snapshot
     (Buttons, Observed_Ms : Unsigned_64) return Unsigned_64 is
     ((Buttons and 16#FF#) or
      (if Observed_Ms <= Maximum_Pointer_Time then
         Shift_Left (Observed_Ms + 1, 8) else 0))
     with Post =>
       (Pointer_Snapshot'Result and 16#FF#) = (Buttons and 16#FF#) and
       Pointer_Time (Pointer_Snapshot'Result) =
         (if Observed_Ms <= Maximum_Pointer_Time then Observed_Ms
          else Unsigned_64'Last);

   --  Sub-millisecond acquisition time (LAT-002): a driver stamping in
   --  microseconds puts the milliseconds in the snapshot (Pointer_Snapshot
   --  layout, keyboards with no buttons) and the remaining microseconds in
   --  header bits 17 .. 26 (Encode_Stamped). A publisher stamping whole
   --  milliseconds (or not at all) leaves them zero. The microsecond clock
   --  is CuBit.Monotonic.Read, which shares Milliseconds' epoch.
   Microseconds_Per_Millisecond : constant := 1_000;
   subtype Sub_Millisecond is Unsigned_64 range 0 .. Microseconds_Per_Millisecond - 1;
   --  The largest millisecond stamp whose microsecond time fits a word.
   Maximum_Stamped_Ms : constant Unsigned_64 :=
     (Unsigned_64'Last - (Microseconds_Per_Millisecond - 1)) / Microseconds_Per_Millisecond;
   --  The snapshot of a report acquired at Observed_Us (Unsigned_64'Last:
   --  unstamped), Buttons in the low eight bits (zero for keyboards).
   function Source_Snapshot (Buttons, Observed_Us : Unsigned_64) return Unsigned_64 is
     (Pointer_Snapshot
        (Buttons, (if Observed_Us = Unsigned_64'Last then Unsigned_64'Last
                   else Observed_Us / Microseconds_Per_Millisecond)));
   function Sub_Millisecond_Of (Observed_Us : Unsigned_64) return Sub_Millisecond is
     (if Observed_Us = Unsigned_64'Last then 0
      else Observed_Us mod Microseconds_Per_Millisecond);

   NULL_SOURCE_REPORT : constant Source_Report := (others => <>);

   function Encode (report : Source_Report) return CuBit.Messages.Message;
   --  Encode, carrying Observed_Us's sub-millisecond part; report.snapshot
   --  should be Source_Snapshot (buttons, Observed_Us).
   function Encode_Stamped
     (report : Source_Report; Observed_Us : Unsigned_64) return CuBit.Messages.Message;

   --  The acquisition time of a valid source report message in microseconds,
   --  Unsigned_64'Last when unstamped. A whole-millisecond publisher's
   --  stamp reads as its millisecond times 1000.
   function Source_Time_Us (msg : CuBit.Messages.Message) return Unsigned_64;

   --  Publisher: the kernel-reported publisher of the event (Poll_Event).
   function Is_Source_Report
     (msg : CuBit.Messages.Message; publisher : CuBit.Messages.Process_ID) return Boolean;

   procedure Decode
     (msg       : CuBit.Messages.Message;
      publisher : CuBit.Messages.Process_ID;
      report    : out Source_Report;
      valid     : out Boolean);

   --  Sequence zero is reserved for uninitialized state. Wrap to one.
   function Next_Sequence (sequence : Source_Sequence)
     return Source_Sequence is
     (if sequence = Source_Sequence'Last then 1 else sequence + 1);

   function Is_Immediate_Successor
     (previous, current : Source_Sequence) return Boolean is
     (previous /= 0 and then current = Next_Sequence (previous));

end CuBit.Input;
