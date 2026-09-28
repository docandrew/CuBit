with Interfaces; use Interfaces;
with XHCI_Topology;
package USB_Hubs with SPARK_Mode is
   type Bytes is array (Positive range <>) of Unsigned_8;
   type Power_Mode is (Ganged, Individual, Always_On);
   type Descriptor is record
      Ports : Natural range 0 .. 255 := 0;
      Power : Power_Mode := Always_On;
      Power_Delay_MS : Natural range 0 .. 510 := 0;
      TT_Think_Time : Natural range 0 .. 3 := 0;
   end record;
   type Decode_Result is (Decoded, Malformed, Unsupported);
   procedure Decode (Data : Bytes; Value : out Descriptor; Result : out Decode_Result)
     with Post => (if Result = Decoded then Value.Ports > 0);

   type Port_Action is
     (Disconnected, Power_Required, Overcurrent, Reset_Required,
      Resetting, Ready, Invalid_Status);
   function Action (Status : Unsigned_16) return Port_Action;
   function Rate (Status : Unsigned_16) return XHCI_Topology.Speed
     with Pre => Action (Status) = Ready;
end USB_Hubs;
