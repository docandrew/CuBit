with Interfaces; use Interfaces;
with CuBit.Log_Protocol;
package Log_Budgets with SPARK_Mode is
   --  One record is one credit (the wire record is already bounded to 544 B).
   --  Development defaults, not a throughput/latency guarantee.
   Burst : constant := 64;
   Refill_Ms : constant Unsigned_64 := 100;
   subtype Credits is Unsigned_64 range 0 .. Burst;
   type Limiter is limited private;
   function Remaining
     (Item : Limiter; Budget : CuBit.Log_Protocol.Budget_Id) return Credits;
   function Rejected
     (Item : Limiter; Budget : CuBit.Log_Protocol.Budget_Id) return Unsigned_64;
   procedure Advance_Time (Item : in out Limiter; Now_Ms : Unsigned_64);
   --  Called only after authenticated publication authority and wire-header
   --  validation, before acquiring/decoding a producer grant. Bad payloads
   --  spend their credit too: there is no refund/retry work amplification.
   procedure Admit
     (Item : in out Limiter; Budget : CuBit.Log_Protocol.Budget_Id;
      Accepted : out Boolean)
     with Post =>
       Accepted = (Remaining (Item, Budget)'Old > 0) and
       Remaining (Item, Budget) + Unsigned_64 (Boolean'Pos (Accepted)) =
         Remaining (Item, Budget)'Old;
private
   type Bucket is record
      Available : Credits := Burst;
      Last_Refill : Unsigned_64 := 0;
      Dropped : Unsigned_64 := 0;
   end record;
   type Buckets is array (CuBit.Log_Protocol.Budget_Id) of Bucket;
   type Limiter is limited record
      Pools : Buckets;
   end record;
end Log_Budgets;
