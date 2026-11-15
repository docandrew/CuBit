pragma Ada_2022;
with Interfaces; use Interfaces;

--  When a presented frame reached the screen (LAT-002), as the display
--  backend saw it, carried back with the Present answer to display.svc and
--  from there with the frame result to the client. Microseconds on the
--  published monotonic clock (CuBit.Monotonic), which every process shares.
--
--  Facts, not analysis: the backend reports when it armed the frame, when
--  the frame latched (a hardware estimate, or a host's fence for a virtual
--  GPU), when it observed that, and its refresh period if it has one. The
--  client derives refreshes late, slack and stage latencies.
package CuBit.Present_Timing with Pure, SPARK_Mode is
   --  How the backend knows the frame is on screen.
   --  Hardware_Latch: a vblank latched PLANE_SURF (intel-gpu); Latched_Us
   --  is the latch time estimated from the pipe's frame timestamp and scan
   --  line, Observed_Us when the driver's poll saw it.
   --  Host_Fence: a virtual GPU's flush fence completed (virtio-gpu); no
   --  refresh exists, Latched_Us = Observed_Us and Period_Us = 0.
   type Latch_Source is (None, Hardware_Latch, Host_Fence);
   for Latch_Source use (None => 0, Hardware_Latch => 1, Host_Fence => 2);

   --  Refresh periods up to about four seconds; zero means none known.
   Period_Bits : constant := 22;
   Maximum_Period_Us : constant := 2 ** Period_Bits - 1;
   subtype Period_Us is Unsigned_64 range 0 .. Maximum_Period_Us;
   --  Observation lag (Observed - Latched) up to about 16 s, saturating.
   Lag_Bits : constant := 24;
   Maximum_Lag_Us : constant := 2 ** Lag_Bits - 1;
   subtype Lag_Us is Unsigned_64 range 0 .. Maximum_Lag_Us;

   type Timing is record
      Source : Latch_Source := None;
      Armed_Us : Unsigned_64 := 0;
      Latched_Us : Unsigned_64 := 0;
      Observed_Us : Unsigned_64 := 0;
      Period : Period_Us := 0;
   end record;
   No_Timing : constant Timing := (others => <>);
   --  None carries nothing; otherwise arm <= latch <= observation, the
   --  observation lag fits its field and only a hardware latch has a period.
   function Valid (Item : Timing) return Boolean is
     (if Item.Source = None then Item = No_Timing
      else Item.Armed_Us <= Item.Latched_Us and then
           Item.Latched_Us <= Item.Observed_Us and then
           Item.Observed_Us - Item.Latched_Us <= Maximum_Lag_Us and then
           (Item.Source = Hardware_Latch or else Item.Period = 0));

   ---------------------------------------------------------------------------
   --  Backend -> display.svc: OP_GPU_PRESENT_BUFFER answer words 1 .. 3
   --  (word 0 stays the status). Word 1 Armed_Us, word 2 Latched_Us, word 3
   --  source (bits 0 .. 7), period (bits 8 .. 29), observation lag (bits
   --  32 .. 55). All zero: no timing (an older or unpaced backend).
   ---------------------------------------------------------------------------
   type Reply_Index is range 1 .. 3;
   type Reply_Words is array (Reply_Index) of Unsigned_64;
   function Encode_Reply (Item : Timing) return Reply_Words
     with Pre => Valid (Item);
   function Decode_Reply (Words : Reply_Words) return Timing
     with Post => Valid (Decode_Reply'Result);

   ---------------------------------------------------------------------------
   --  display.svc -> client: the frame result's words 2 and 3 keep their
   --  enumeration in bits 0 .. 7; bits 8 .. 63 carry Compact. Times are the
   --  low 32 bits of the microsecond clock, expanded by the receiver
   --  against its own clock (Expand): exact while the frame is younger than
   --  2**32 us (71 minutes).
   ---------------------------------------------------------------------------
   Low_Bits : constant := 32;
   Low_Modulus : constant := 2 ** Low_Bits;
   subtype Low_Time is Unsigned_64 range 0 .. Low_Modulus - 1;
   type Compact is record
      Source : Latch_Source := None;
      Period : Period_Us := 0;
      Lag : Lag_Us := 0;
      Armed_Low, Latched_Low : Low_Time := 0;
   end record;
   No_Compact : constant Compact := (others => <>);
   function Valid (Item : Compact) return Boolean is
     (if Item.Source = None then Item = No_Compact
      else Item.Source = Hardware_Latch or else Item.Period = 0);
   function Low (Value : Unsigned_64) return Low_Time is (Value mod Low_Modulus);
   function To_Compact (Item : Timing) return Compact
     with Pre => Valid (Item), Post => Valid (To_Compact'Result);
   --  The latest time at or before Now whose low bits are Value (zero if
   --  that would precede the clock's start).
   function Back (Value : Low_Time; Now : Unsigned_64) return Low_Time is
     ((Low (Now) - Value) mod Low_Modulus);
   function Full (Value : Low_Time; Now : Unsigned_64) return Unsigned_64 is
     (if Back (Value, Now) > Now then 0 else Now - Back (Value, Now))
     with Post => Full'Result <= Now and then
                  (if Back (Value, Now) <= Now then Now - Full'Result < Low_Modulus);
   --  Item as seen at Now (which must not precede the observation).
   function Expand (Item : Compact; Now : Unsigned_64) return Timing;

   --  Words 2 and 3 high parts (bits 8 .. 63) for Item, and back. Packed
   --  words with a nonzero high part that do not decode give No_Compact.
   Enumeration_Bits : constant := 8;
   Enumeration_Mask : constant Unsigned_64 := 2 ** Enumeration_Bits - 1;
   type Packed_Pair is record
      Outcome_High, Buffer_High : Unsigned_64 := 0;
   end record;
   function Pack (Item : Compact) return Packed_Pair
     with Pre => Valid (Item),
          Post => (Pack'Result.Outcome_High and Enumeration_Mask) = 0 and then
                  (Pack'Result.Buffer_High and Enumeration_Mask) = 0;
   function Unpack (Item : Packed_Pair) return Compact
     with Post => Valid (Unpack'Result);
end CuBit.Present_Timing;
