with Interfaces; use Interfaces;

--  Bounded standard-descriptor decoding shared by HID and optical discovery.
--  A configuration advertises interfaces; it does not grant clients authority.
package USB_Configurations with SPARK_Mode => On is
   Maximum_Descriptor_Bytes : constant := 4096;
   type Bytes is array (Positive range <>) of Unsigned_8;
   type Endpoint is record
      Address : Unsigned_8 := 0;
      Packet_Bytes : Natural range 0 .. 1024 := 0;
      Interval : Unsigned_8 := 0;
   end record;
   type Mouse_Interface is record
      Present : Boolean := False;
      Number : Unsigned_8 := 0;
      Input : Endpoint;
   end record;
   type Storage_Interface is record
      Present : Boolean := False;
      Number : Unsigned_8 := 0;
      Input, Output : Endpoint;
   end record;
   type Configuration is record
      Value : Unsigned_8 := 0;
      Mouse : Mouse_Interface;
      Storage : Storage_Interface;
   end record;
   type Decode_Result is (Decoded, Malformed);
   procedure Decode
     (Data : Bytes; Value : out Configuration; Result : out Decode_Result);
end USB_Configurations;
