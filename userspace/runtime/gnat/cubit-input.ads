------------------------------------------------------------------------------
--  CuBit typed input-source publication protocol.
--
--  A report is a state-bearing message, not an unauthenticated device event.
--  The kernel stamps Message.authorityTag from the publication capability; the
--  sequence, generation, and recovery flag make bounded transport loss
--  observable. This is the driver-facing half of the future input.svc stream.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

with CuBit.Messages;

package CuBit.Input with SPARK_Mode => On is

   OP_SOURCE_REPORT : constant Unsigned_32 := 16#0A00#;

   type Device_Class is (KEYBOARD, RELATIVE_POINTER);
   for Device_Class use (KEYBOARD => 1, RELATIVE_POINTER => 2);

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
      sourceAuthorityTag : Unsigned_64 := 0;
      sequence    : Source_Sequence := 0;
      generation  : Source_Generation := 0;
      device      : Device_Class := KEYBOARD;
      delivery    : Delivery_Class := ORDERED_TRANSITION;
      flags       : Report_Flags := NO_REPORT_FLAGS;
      payload     : Unsigned_64 := 0;
      snapshot    : Unsigned_64 := 0;
   end record;

   NULL_SOURCE_REPORT : constant Source_Report := (others => <>);

   function Encode (report : Source_Report) return CuBit.Messages.Message;

   function Is_Source_Report
     (msg : CuBit.Messages.Message) return Boolean;

   procedure Decode
     (msg    : CuBit.Messages.Message;
      report : out Source_Report;
      valid  : out Boolean);

   --  Sequence zero is reserved for uninitialized state. Wrap to one.
   function Next_Sequence (sequence : Source_Sequence)
     return Source_Sequence is
     (if sequence = Source_Sequence'Last then 1 else sequence + 1);

   function Is_Immediate_Successor
     (previous, current : Source_Sequence) return Boolean is
     (previous /= 0 and then current = Next_Sequence (previous));

end CuBit.Input;
