pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Log_Records;
package CuBit.Log_Protocol with Pure, SPARK_Mode is
   Observer_Service_Role : constant Unsigned_64 := 21;
   Publisher_Slot : constant Unsigned_64 := 23;
   Observer_Slot : constant Unsigned_64 := 27;
   Publisher_Tag_Base : constant Unsigned_64 := 16#4C4F_5000_0000_0000#;
   Observer_Tag_Base : constant Unsigned_64 := 16#4C4F_4700_0000_0000#;
   --  Issuer-selected pool, never a producer-controlled message field.
   subtype Budget_Id is Unsigned_64 range 1 .. 15;
   Bootstrap_Budget : constant Budget_Id := 1;
   subtype Publication_Issuance is Unsigned_64 range 1 .. 16#0FFF_FFFF#;
   function Publisher_Tag
     (Budget : Budget_Id; Issuance : Publication_Issuance)
      return Unsigned_64 is
     (Publisher_Tag_Base + Budget * 16#1000_0000# + Issuance);
   Publisher_Authority_Tag : constant Unsigned_64 :=
     Publisher_Tag_Base + Bootstrap_Budget * 16#1000_0000# + 1;
   Observer_Authority_Tag : constant Unsigned_64 := Observer_Tag_Base + 1;
   function Is_Observer (Tag : Unsigned_64) return Boolean is
     ((Tag and 16#FFFF_FFFF_0000_0000#) = Observer_Tag_Base and then
      (Tag and 16#FFFF_FFFF#) /= 0);
   function Is_Publisher (Tag : Unsigned_64) return Boolean is
     ((Tag and 16#FFFF_FFFF_0000_0000#) = Publisher_Tag_Base and then
      (Tag and 16#F000_0000#) /= 0 and then
      (Tag and 16#0FFF_FFFF#) /= 0);
   function Publication_Budget (Tag : Unsigned_64) return Budget_Id is
     ((Tag / 16#1000_0000#) mod 16)
     with Pre => Is_Publisher (Tag);
   type Operation is (Publish, Subscribe, Read_Next, Close);
   for Operation use
     (Publish => 16#0C00#, Subscribe => 16#0C01#,
      Read_Next => 16#0C02#, Close => 16#0C03#);
   type Event is record
      Source : Unsigned_64 := 0;
      Publication_Tag : Unsigned_64 := 0;
      Monotonic_Ms : Unsigned_64 := 0;
      Data : CuBit.Log_Records.Log_Record := CuBit.Log_Records.Empty_Record;
   end record;
   type Status is (OK, Denied, Invalid_Request, Exhausted, Empty, Gap,
                   Unavailable, Rate_Limited);
   for Status use
     (OK => 16#F000#, Denied => 16#F002#, Invalid_Request => 16#F003#,
      Exhausted => 16#F004#, Empty => 16#F005#, Gap => 16#F006#,
      Unavailable => 16#F007#, Rate_Limited => 16#F008#);
   --  Rate_Limited: all-zero words, no acquisition and no record accepted.
   --  All requests/replies: length four, zero flags/reserved.
   --  Publish: read-only grant slot, generation, encoded length, zero.
   --  Subscribe: all zero. OK reply: subscription handle, zero, zero, zero.
   --  Read: subscription, writable grant slot, generation, capacity (544).
   --  OK: source PID, observed ms, encoded bytes, publisher authority tag.
   --  Close: subscription, zero, zero, zero. Gap reply: lost count, zeroes.
   --  Buffers belong to clients. Service returns acquisitions BEFORE replying.
   --  Tags are kernel-stamped; knowing these numeric values grants nothing.
   function May_Invoke
     (Authority_Tag : Unsigned_64; Op : Operation) return Boolean is
     (if Op = Publish then Is_Publisher (Authority_Tag)
      else Is_Observer (Authority_Tag));
end CuBit.Log_Protocol;
