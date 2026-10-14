with Interfaces; use Interfaces;
with CuBit.Handles;
with CuBit.Handles.Fixture;
with CuBit.Startup_State;
--  Hosted fixture of ps2.drv's generated Startup package: distinct handle
--  words per binding, so the fixture can check which one the driver used.
package Startup is
   Data_Word            : constant Unsigned_64 := 16#1_0000_0004#;
   Command_Word         : constant Unsigned_64 := 16#1_0000_0005#;
   Keyboard_Events_Word : constant Unsigned_64 := 16#1_0000_0008#;
   Mouse_Events_Word    : constant Unsigned_64 := 16#1_0000_0009#;
   function Data return CuBit.Handles.IO_Ports is (CuBit.Handles.Fixture.IO_Ports_Of (Data_Word));
   function Command return CuBit.Handles.IO_Ports is (CuBit.Handles.Fixture.IO_Ports_Of (Command_Word));
   function Keyboard_Line return CuBit.Handles.Interrupt is (CuBit.Handles.Fixture.Interrupt_Of (16#1_0000_0006#));
   function Mouse_Line return CuBit.Handles.Interrupt is (CuBit.Handles.Fixture.Interrupt_Of (16#1_0000_0007#));
   function Keyboard_Events return CuBit.Handles.Notification is
     (CuBit.Handles.Fixture.Notification_Of (Keyboard_Events_Word));
   function Mouse_Events return CuBit.Handles.Notification is
     (CuBit.Handles.Fixture.Notification_Of (Mouse_Events_Word));
   function Ready return CuBit.Handles.Endpoint is (CuBit.Handles.Fixture.Endpoint_Of (16#1_0000_000F#));
   function Verify return CuBit.Startup_State.Verification is ((Loaded => True));
end Startup;
