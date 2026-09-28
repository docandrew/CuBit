with Ada.Text_IO;
with Interfaces; use Interfaces;
with XHCI_Topology; use XHCI_Topology;
procedure Topology_Tests is
   Hub : constant Path := Root (1, High_Speed);
   Mouse, Keyboard, Nested, Leaf, Rejected : Path;
   Result : Attach_Result;
begin
   pragma Assert (Route_String (Hub) = 0 and TT_Context (Hub) = 0);
   Child (Hub, 7, 1, Full_Speed, Mouse, Result);
   pragma Assert (Result = Attached and Route_String (Mouse) = 1);
   pragma Assert (TT_Context (Mouse) = 16#0107#);
   Child (Hub, 7, 4, Full_Speed, Keyboard, Result);
   pragma Assert (Result = Attached and Route_String (Keyboard) = 4);
   pragma Assert (TT_Context (Keyboard) = 16#0407#);
   -- A full-speed hub retains the ancestor high-speed translator identity.
   Child (Hub, 7, 2, Full_Speed, Nested, Result);
   Child (Nested, 8, 3, Low_Speed, Leaf, Result);
   pragma Assert (Result = Attached and Route_String (Leaf) = 16#32#);
   pragma Assert (TT_Context (Leaf) = 16#0207#);
   for Port in Port_Number loop
      Child (Hub, 255, Port, Full_Speed, Leaf, Result);
      pragma Assert (Result = Attached);
      pragma Assert (Route_String (Leaf) = Unsigned_32 (Natural'Min (Port, 15)));
      pragma Assert (TT_Context (Leaf) = Unsigned_32 (Port) * 256 + 255);
   end loop;
   Nested := Hub;
   for Depth in 1 .. 5 loop
      Child (Nested, 7, 15, High_Speed, Leaf, Result);
      pragma Assert (Result = Attached and TT_Context (Leaf) = 0);
      Nested := Leaf;
   end loop;
   pragma Assert (Route_String (Nested) = 16#F_FFFF#);
   Child (Nested, 7, 1, Full_Speed, Rejected, Result);
   pragma Assert (Result = Depth_Exceeded);
   Child (Mouse, 7, 1, High_Speed, Rejected, Result);
   pragma Assert (Result = Unsupported_Speed);
   Child (Root (2, Super_Speed), 7, 1, Super_Speed, Rejected, Result);
   pragma Assert (Result = Unsupported_Speed);
   Ada.Text_IO.Put_Line ("PASS: USB2 routes, TT inheritance and depth limits");
end Topology_Tests;
