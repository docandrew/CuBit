pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Log_Records;
package CuBit.Log_Protocol with Pure, SPARK_Mode is
   Observer_Service_Role : constant Unsigned_64 := 21;
   Publisher_Slot : constant Unsigned_64 := 23;
   --  Records an observer's queue in logstore holds (including boot replay),
   --  beyond what its stream ring holds. A reader that falls further behind
   --  loses the oldest and is told how many in an explicit Gap entry; part
   --  of the observer contract.
   Observer_Queue_Records : constant := 512;
   --  Subscribe's words: (0) the minimum severity, (1) the publishing process
   --  to keep (Every_Source: all of them). Its reply: (0) the handle, (1) the
   --  source filter applied.
   Every_Source : constant Unsigned_64 := 0;
   Observer_Slot : constant Unsigned_64 := 27;
   --  The log-control role: changes what logstore keeps (Set_Minimum).
   Control_Service_Role : constant Unsigned_64 := 27;
   Control_Slot : constant Unsigned_64 := 32;
   Control_Tag_Base : constant Unsigned_64 := 16#4C4F_4300_0000_0000#;
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
   function Control_Tag (Issuance : Publication_Issuance) return Unsigned_64 is
     (Control_Tag_Base + Issuance);
   function Is_Control (Tag : Unsigned_64) return Boolean is
     ((Tag and 16#FFFF_FFFF_0000_0000#) = Control_Tag_Base and then
      (Tag and 16#FFFF_FFFF#) /= 0);
   function Is_Publisher (Tag : Unsigned_64) return Boolean is
     ((Tag and 16#FFFF_FFFF_0000_0000#) = Publisher_Tag_Base and then
      (Tag and 16#F000_0000#) /= 0 and then
      (Tag and 16#0FFF_FFFF#) /= 0);
   function Publication_Budget (Tag : Unsigned_64) return Budget_Id is
     ((Tag / 16#1000_0000#) mod 16)
     with Pre => Is_Publisher (Tag);
   type Operation is (Publish, Subscribe, Close, Set_Minimum, Get_Minimum);
   for Operation use
     (Publish => 16#0C00#, Subscribe => 16#0C01#, Close => 16#0C03#,
      Set_Minimum => 16#0C04#, Get_Minimum => 16#0C05#);
   --  The node an event was published on: a 16-byte identity, stamped by the
   --  logstore that ingested it, never by the publisher. This_Node (zero)
   --  until nodes have identities; ingestion from other nodes keeps theirs.
   type Node_Id is record
      High, Low : Unsigned_64 := 0;
   end record;
   This_Node : constant Node_Id := (High => 0, Low => 0);
   type Event is record
      Source : Unsigned_64 := 0;
      Node : Node_Id := This_Node;
      Publication_Tag : Unsigned_64 := 0;
      Monotonic_Ms : Unsigned_64 := 0;
      Data : CuBit.Log_Records.Log_Record := CuBit.Log_Records.Empty_Record;
   end record;
   type Status is (OK, Denied, Invalid_Request, Exhausted, Empty, Gap,
                   Unavailable, Rate_Limited, Below_Minimum);
   for Status use
     (OK => 16#F000#, Denied => 16#F002#, Invalid_Request => 16#F003#,
      Exhausted => 16#F004#, Empty => 16#F005#, Gap => 16#F006#,
      Unavailable => 16#F007#, Rate_Limited => 16#F008#,
      Below_Minimum => 16#F009#);
   --  Rate_Limited: all-zero words, no acquisition and no record accepted.
   --  All requests/replies: length four, zero flags/reserved.
   --  Publish: read-only grant slot, generation, encoded length, zero.
   --    OK or Below_Minimum (valid, not kept: under logstore's minimum)
   --    reply: the minimum kept (Log_Records.Severity'Pos), zeroes.
   --  Set_Minimum (log-control only): the new minimum's 'Pos, zeroes. OK
   --    reply: the previous minimum, zeroes.
   --  Get_Minimum (any logstore role): zeroes. OK reply: the minimum, zeroes.
   --  Subscribe: minimum severity (Log_Records.Severity'Pos, zero = all),
   --    source (Every_Source: all), and the reader's stream region: a
   --    writable grant's slot and generation (CuBit.Log_Streams layout).
   --    logstore keeps the region mapped and writes the reader's events into
   --    its ring; reading takes no IPC. OK reply: subscription handle, the
   --    source filter applied, zero, zero. Repeating it (same owner and
   --    authority) renews the lease and updates the filter; it keeps the
   --    stream.
   --  Close: subscription, zero, zero, zero. logstore stops writing and
   --    returns the stream region before replying.
   --  Buffers belong to clients. Service returns acquisitions BEFORE replying.
   --  Tags are kernel-stamped; knowing these numeric values grants nothing.
   function May_Invoke
     (Authority_Tag : Unsigned_64; Op : Operation) return Boolean is
     (case Op is
         when Publish => Is_Publisher (Authority_Tag),
         when Set_Minimum => Is_Control (Authority_Tag),
         when Get_Minimum =>
           Is_Publisher (Authority_Tag) or else Is_Observer (Authority_Tag) or else Is_Control (Authority_Tag),
         when Subscribe | Close => Is_Observer (Authority_Tag));
end CuBit.Log_Protocol;
