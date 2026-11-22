with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;
with Intel_GPU_ADLN_Vertex_Fetch; use Intel_GPU_ADLN_Vertex_Fetch;
with Intel_GPU_ADLN_Triangle;
procedure Vertex_Fetch_Tests is
   package Triangle renames Intel_GPU_ADLN_Triangle;
   use type Triangle.Packet;
   function Top is new Ada.Unchecked_Conversion (Unsigned_32, Triangle.Topology_Control);
   function DH is new Ada.Unchecked_Conversion (Unsigned_32, Triangle.Draw_Header);
   function DC is new Ada.Unchecked_Conversion (Unsigned_32, Triangle.Draw_Control);
   function H is new Ada.Unchecked_Conversion (Unsigned_32, Header);
   function B is new Ada.Unchecked_Conversion (Unsigned_32, Buffer_Control);
   function E is new Ada.Unchecked_Conversion (Unsigned_32, Element_Control);
   function C is new Ada.Unchecked_Conversion (Unsigned_32, Component_Control);
   function F is new Ada.Unchecked_Conversion (Unsigned_32, VF_Control);
   function Ins is new Ada.Unchecked_Conversion (Unsigned_32, Instancing_Control);
   function SGV is new Ada.Unchecked_Conversion (Unsigned_32, SGV_Control);
   function Ext is new Ada.Unchecked_Conversion (Unsigned_32, Extended_SGV_Control);
   function XP2 is new Ada.Unchecked_Conversion (Unsigned_32, XP2_Control);
   Expected : Words := [16#78080003#,16#02064010#,16#206100#,0,48,
                        16#78090001#,16#02000000#,16#11110000#,
                        16#780C0000#,0,16#78490001#,0,0,
                        16#784A0000#,0,16#78560001#,0,0];
   Result : Image;
   Value : Unsigned_32;
begin
   for Bit in 0 .. 31 loop
      Value := Shift_Left (1, Bit);
      pragma Assert (Encode (H (Value)) = Value);
      pragma Assert (Encode (B (Value)) = Value);
      pragma Assert (Encode (E (Value)) = Value);
      pragma Assert (Encode (C (Value)) = Value);
      pragma Assert (Encode (F (Value)) = Value);
      pragma Assert (Encode (Ins (Value)) = Value);
      pragma Assert (Encode (SGV (Value)) = Value);
      pragma Assert (Encode (Ext (Value)) = Value);
      pragma Assert (Encode (XP2 (Value)) = Value);
      pragma Assert (Triangle.Encode (Top (Value)) = Value);
      pragma Assert (Triangle.Encode (DH (Value)) = Value);
      pragma Assert (Triangle.Encode (DC (Value)) = Value);
   end loop;
   pragma Assert (Triangle.Topology = Triangle.Packet'[16#784B0000#,4]);
   pragma Assert (Triangle.Draw = Triangle.Packet'[16#7B000005#,0,3,0,1,0,0]);
   pragma Assert (Build (6).Data = Expected);
   for MOCS in Unsigned_32 range 0 .. 255 loop
      Result := Build (MOCS);
      pragma Assert (Result.Valid = (MOCS > 0 and MOCS <= 126 and MOCS mod 2 = 0));
      if Result.Valid then
         Expected (1) := 16#02004010# or Shift_Left (MOCS, 16);
         pragma Assert (Result.Data = Expected);
      else
         pragma Assert (for all W of Result.Data => W = 0);
      end if;
   end loop;
   pragma Assert (not Build (Unsigned_32'Last).Valid);
   Ada.Text_IO.Put_Line ("Vertex fetch/triangle PASS: 384 record bits, Mesa packet fixture, MOCS rejection; NOT submitted");
end Vertex_Fetch_Tests;
