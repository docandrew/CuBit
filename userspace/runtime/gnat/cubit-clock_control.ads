pragma Ada_2022;
with Interfaces; use Interfaces;

--  Wall-clock adjustment authority, separate from reading time. procmgr mints
--  the clock endpoint with Authority_Tag only for a declared request approved
--  by the trusted startup plan. The clock service checks the kernel-stamped
--  tag; possessing ordinary clock read access never implies adjustment.
--  This package is the pure wire encoding; see CuBit.Clock_Control.Client.
package CuBit.Clock_Control with SPARK_Mode is
   Service_Role : constant Unsigned_64 := 22;
   Authority_Tag : constant Unsigned_64 := 16#8000_0000_0000_0002#;
   Endpoint_Slot : constant Unsigned_64 := 28;
   Submit_Sample : constant Unsigned_32 := 16#0B02#;

   Maximum_Sources : constant := 255;

   --  One time estimate: UTC (Unix epoch milliseconds) as it was at a local
   --  monotonic instant, with an error bound. Authenticated means every
   --  contributing source was cryptographically authenticated (NTS).
   type Sample is record
      UTC_MS : Unsigned_64 := 0;
      Observed_Monotonic_MS : Unsigned_64 := 0;
      Uncertainty_MS : Unsigned_32 := 0;
      Sources : Natural range 0 .. Maximum_Sources := 0;
      Authenticated : Boolean := False;
   end record;

   type Words is array (0 .. 3) of Unsigned_64;

   --  Word 2: uncertainty in bits 0..31, sources in bits 32..39,
   --  authenticated in bit 40. Bits 41..63 and word 3 are reserved zero.
   function Encode (Item : Sample) return Words;
   procedure Decode (Value : Words; Item : out Sample; Success : out Boolean)
   with Post => (if Success then Encode (Item) = Value);

   --  Replies are REPLY_OK with word 0 = Outcome, word 1 = resulting
   --  CuBit.Clocks.Time_Quality. Unauthorized or malformed requests get
   --  REPLY_ERR instead.
   type Outcome is
     (Stepped, Rejected_Stale, Rejected_Future_Observation,
      Rejected_Uncertainty, Rejected_Sources, Rejected_Below_Floor,
      Rejected_Conflict, Rejected_Out_Of_Range);
   for Outcome use
     (Stepped => 0, Rejected_Stale => 1, Rejected_Future_Observation => 2,
      Rejected_Uncertainty => 3, Rejected_Sources => 4,
      Rejected_Below_Floor => 5, Rejected_Conflict => 6,
      Rejected_Out_Of_Range => 7);

   --  The runtime discards enumeration names, so 'Image gives positions.
   function Name (Value : Outcome) return String is
     (case Value is
         when Stepped => "stepped",
         when Rejected_Stale => "stale",
         when Rejected_Future_Observation => "future observation",
         when Rejected_Uncertainty => "too uncertain",
         when Rejected_Sources => "too few sources",
         when Rejected_Below_Floor => "below time floor",
         when Rejected_Conflict => "conflict",
         when Rejected_Out_Of_Range => "out of range");
end CuBit.Clock_Control;
