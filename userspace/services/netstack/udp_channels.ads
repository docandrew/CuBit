pragma Ada_2022;
with Interfaces; use Interfaces;
with Network_Channel_Handles;

--  Bounded state for connected UDP channels. A channel has one remote IPv4
--  address and port, and a service-chosen ephemeral local port. Only
--  datagrams from exactly that remote endpoint to that local port are
--  queued for it. Policy admission, grants and packet IO stay in the caller.
package UDP_Channels with SPARK_Mode is
   subtype Channel_Index is Network_Channel_Handles.Channel_Index;

   --  1500-byte Ethernet MTU minus IPv4 (20) and UDP (8) headers. The stack
   --  does not reassemble IPv4 fragments, so larger datagrams never arrive.
   Maximum_Payload : constant := 1472;
   Queue_Depth : constant := 4;
   First_Ephemeral : constant Unsigned_16 := 49_152;
   Last_Ephemeral : constant Unsigned_16 := 65_535;

   subtype Payload_Length is Natural range 0 .. Maximum_Payload;
   type Byte_Array is array (Positive range <>) of Unsigned_8;

   type Delivery is (Queued, Queue_Full, No_Channel, Oversized);

   type Table is private;

   function Active (Item : Table; Index : Channel_Index) return Boolean;
   function Local_Port (Item : Table; Index : Channel_Index) return Unsigned_16;
   function Remote_Address (Item : Table; Index : Channel_Index) return Unsigned_32;
   function Remote_Port (Item : Table; Index : Channel_Index) return Unsigned_16;
   function Queued_Count (Item : Table; Index : Channel_Index) return Natural;
   function Port_In_Use (Item : Table; Port : Unsigned_16) return Boolean;

   --  Activate an inactive slot with a fresh ephemeral port that no other
   --  active channel uses. Fails without change if the slot is active, the
   --  remote endpoint is unusable, or no port is free.
   procedure Open
     (Item : in out Table; Index : Channel_Index;
      Address : Unsigned_32; Port : Unsigned_16; Success : out Boolean)
   with
     Post =>
       (if Success then
          Active (Item, Index) and then
          Remote_Address (Item, Index) = Address and then
          Remote_Port (Item, Index) = Port and then
          Local_Port (Item, Index) in First_Ephemeral .. Last_Ephemeral and then
          Queued_Count (Item, Index) = 0 and then
          (for all J in Channel_Index =>
             (if J /= Index and then Active (Item, J) then
                Local_Port (Item, J) /= Local_Port (Item, Index))));

   --  Queue a received datagram for the unique channel whose local port and
   --  remote endpoint all match. Datagrams are copied; Payload is not kept.
   procedure Deliver
     (Item : in out Table; Destination_Port : Unsigned_16;
      Source_Address : Unsigned_32; Source_Port : Unsigned_16;
      Payload : Byte_Array; Index : out Channel_Index; Result : out Delivery)
   with
     Post =>
       (if Result in Queued | Queue_Full then
          Active (Item, Index) and then
          Local_Port (Item, Index) = Destination_Port and then
          Remote_Address (Item, Index) = Source_Address and then
          Remote_Port (Item, Index) = Source_Port);

   --  Remove the oldest queued datagram. At most Output'Length bytes are
   --  copied; Truncated reports that the datagram was longer.
   procedure Take
     (Item : in out Table; Index : Channel_Index; Output : out Byte_Array;
      Length : out Natural; Truncated : out Boolean; Found : out Boolean)
   with
     Post => Length <= Output'Length and then (if not Found then Length = 0);

   procedure Close (Item : in out Table; Index : Channel_Index)
   with Post => not Active (Item, Index) and then Queued_Count (Item, Index) = 0;
private
   type Datagram is record
      Length : Payload_Length := 0;
      Data : Byte_Array (1 .. Maximum_Payload) := [others => 0];
   end record;
   subtype Queue_Index is Natural range 0 .. Queue_Depth - 1;
   subtype Queue_Count is Natural range 0 .. Queue_Depth;
   type Queue is array (Queue_Index) of Datagram;
   type Channel is record
      Active : Boolean := False;
      Local_Port : Unsigned_16 := 0;
      Remote_Address : Unsigned_32 := 0;
      Remote_Port : Unsigned_16 := 0;
      Head : Queue_Index := 0;
      Count : Queue_Count := 0;
      Pending : Queue;
   end record;
   type Channel_Array is array (Channel_Index) of Channel;
   type Table is record
      Channels : Channel_Array;
      Next_Port : Unsigned_16 := First_Ephemeral;
   end record;

   function Active (Item : Table; Index : Channel_Index) return Boolean is
     (Item.Channels (Index).Active);
   function Local_Port (Item : Table; Index : Channel_Index) return Unsigned_16 is
     (Item.Channels (Index).Local_Port);
   function Remote_Address (Item : Table; Index : Channel_Index) return Unsigned_32 is
     (Item.Channels (Index).Remote_Address);
   function Remote_Port (Item : Table; Index : Channel_Index) return Unsigned_16 is
     (Item.Channels (Index).Remote_Port);
   function Queued_Count (Item : Table; Index : Channel_Index) return Natural is
     (Item.Channels (Index).Count);
   function Port_In_Use (Item : Table; Port : Unsigned_16) return Boolean is
     (for some C of Item.Channels => C.Active and then C.Local_Port = Port);
end UDP_Channels;
