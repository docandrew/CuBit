------------------------------------------------------------------------------
--  CuBit typed input-source protocol.
--
--  A report is state-bearing, not an unauthenticated device event; the
--  sequence, generation, and recovery flag make bounded loss observable.
--  Reports travel two ways:
--  * a source ring (IPC-009, CuBit.Input_Rings, CuBit.Input_Source): one
--    ring per source device, from its driver straight to Desktop, which
--    owns focus and routing and consumes directly. There is no input.svc
--    hop: other producers (ccl-remote's remote input, synthetic or
--    accessibility input) are more sources, each with its own granted
--    ring. Only an endpoint carrying Source_Ring_Tag (devmgr mints it for
--    input drivers) opens one, and the source's identity is the ring
--    itself, bound when Desktop admits it; nothing in a report names it.
--    Elements are Report_Words, encoded once (Encode_Words).
--  * the kernel event lane (until every source has its ring: hid-i2c):
--    a report message published through a capability carrying the input
--    badge (Publisher_Tag, minted by devmgr or procmgr for an input role's
--    publish notification); its source is the publisher's identity as the
--    kernel reports it.
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

   --  The badge on the endpoint to Desktop an input driver opens its source
   --  rings with ("INRING", version 1): Desktop admits no other.
   Source_Ring_Tag : constant Unsigned_64 := 16#494E_5249_4E47_0001#;

   --  A report as four words: the element of a source ring and the words
   --  of a report message, the same encoding.
   type Report_Words is record
      Sequence, Header, Payload, Snapshot : Unsigned_64 := 0;
   end record;
   --  Encode_Stamped's words.
   function Encode_Words
     (report : Source_Report; Observed_Us : Unsigned_64) return Report_Words;
   --  The words as a report message (Desktop's dispatch takes messages).
   function To_Message (Words : Report_Words) return CuBit.Messages.Message;
   --  A ring's element: valid as a report message is, its source the ring's
   --  identity Source (bound when the ring was admitted).
   procedure Decode_Words
     (Words  : Report_Words;
      Source : Unsigned_64;
      report : out Source_Report;
      valid  : out Boolean);

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
