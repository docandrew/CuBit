pragma Ada_2022;
with Interfaces; use Interfaces;
with Network_Channel_Handles;

--  Bounded state for connected UDP channels. A channel has one remote IPv4
--  address and port, and a service-chosen ephemeral local port. Only
--  datagrams from exactly that remote endpoint to that local port match
--  it; they go to its receive ring (CuBit.Datagram_Rings), not a queue
--  here. Policy admission, grants and packet IO stay in the caller.
package UDP_Channels with SPARK_Mode is
   subtype Channel_Index is Network_Channel_Handles.Channel_Index;

   --  1500-byte Ethernet MTU minus IPv4 (20) and UDP (8) headers. The stack
   --  does not reassemble IPv4 fragments, so larger datagrams never arrive.
   Maximum_Payload : constant := 1472;
   First_Ephemeral : constant Unsigned_16 := 49_152;
   Last_Ephemeral : constant Unsigned_16 := 65_535;

   subtype Payload_Length is Natural range 0 .. Maximum_Payload;

   type Delivery is (Matched, No_Channel, Oversized);

   type Table is private;

   function Active (Item : Table; Index : Channel_Index) return Boolean;
   function Local_Port (Item : Table; Index : Channel_Index) return Unsigned_16;
   function Remote_Address (Item : Table; Index : Channel_Index) return Unsigned_32;
   function Remote_Port (Item : Table; Index : Channel_Index) return Unsigned_16;
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
          (for all J in Channel_Index =>
             (if J /= Index and then Active (Item, J) then
                Local_Port (Item, J) /= Local_Port (Item, Index))));

   --  The unique channel whose local port and remote endpoint all match an
   --  arriving datagram, if any (Oversized: longer than an unfragmented
   --  datagram can be).
   procedure Deliver
     (Item : Table; Destination_Port : Unsigned_16;
      Source_Address : Unsigned_32; Source_Port : Unsigned_16;
      Payload_Bytes : Natural; Index : out Channel_Index; Result : out Delivery)
   with
     Post =>
       (if Result = Matched then
          Active (Item, Index) and then
          Local_Port (Item, Index) = Destination_Port and then
          Remote_Address (Item, Index) = Source_Address and then
          Remote_Port (Item, Index) = Source_Port and then
          Payload_Bytes <= Maximum_Payload);

   procedure Close (Item : in out Table; Index : Channel_Index)
   with Post => not Active (Item, Index);
private
   type Channel is record
      Active : Boolean := False;
      Local_Port : Unsigned_16 := 0;
      Remote_Address : Unsigned_32 := 0;
      Remote_Port : Unsigned_16 := 0;
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
   function Port_In_Use (Item : Table; Port : Unsigned_16) return Boolean is
     (for some C of Item.Channels => C.Active and then C.Local_Port = Port);
end UDP_Channels;
