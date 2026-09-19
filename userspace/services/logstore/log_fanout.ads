with Interfaces; use Interfaces;
with CuBit.Log_Protocol;
package Log_Fanout with SPARK_Mode is
   --  Single-owner broker; logstore serializes access in its service loop.
   --  Caller and Authority_Tag must be authenticated by a trusted adapter.
   --  Publish is an internal trusted operation, not an IPC authorization gate.
   Capacity : constant := 16;
   Maximum_Subscribers : constant := 8;
   type Broker is limited private;
   Subscription_Lease_Ms : constant Unsigned_64 := 30_000;
   --  Service supplies trusted monotonic time before handling each message.
   --  Expiration frees dead/idle subscribers without polling the process table.
   procedure Advance_Time (Item : in out Broker; Now_Ms : Unsigned_64);
   procedure Publish
     (Item : in out Broker; Value : CuBit.Log_Protocol.Event);
   procedure Subscribe
     (Item : in out Broker; Caller, Authority_Tag : Unsigned_64;
      Handle : out Unsigned_64; Result : out CuBit.Log_Protocol.Status);
   procedure Read_Next
     (Item : in out Broker; Caller, Authority_Tag, Handle : Unsigned_64;
      Value : out CuBit.Log_Protocol.Event; Lost : out Unsigned_64;
      Result : out CuBit.Log_Protocol.Status);
   procedure Close
     (Item : in out Broker; Caller, Authority_Tag, Handle : Unsigned_64;
      Result : out CuBit.Log_Protocol.Status);
private
   subtype Index is Natural range 0 .. Capacity - 1;
   subtype Count is Natural range 0 .. Capacity;
   type Events is array (Index) of CuBit.Log_Protocol.Event;
   type Queue is record
      Data : Events;
      Head : Index := 0;
      Used : Count := 0;
      Lost : Unsigned_64 := 0;
   end record;
   type Subscriber is record
      Owner : Unsigned_64 := 0;
      Authority_Tag : Unsigned_64 := 0;
      Last_Use : Unsigned_64 := 0;
      Handle : Unsigned_64 := 0;
      Pending : Queue;
   end record;
   type Subscribers is array (Positive range 1 .. Maximum_Subscribers)
     of Subscriber;
   type Broker is limited record
      Recent : Queue;
      Clients : Subscribers;
      Next_Handle : Unsigned_64 := 1;
      Now_Ms : Unsigned_64 := 0;
   end record;
end Log_Fanout;
