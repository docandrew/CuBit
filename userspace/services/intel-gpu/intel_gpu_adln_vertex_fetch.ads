with Interfaces; use Interfaces;
with System;
package Intel_GPU_ADLN_Vertex_Fetch with SPARK_Mode is
   -- TGL Vol2a pp143-146; Vol2d pp1141-1147. Numeric packet state, not MMIO.
   -- Fixed private triangle input only; not an application buffer validator.
   type B1 is mod 2 ** 1 with Size => 1;
   type B2 is mod 2 ** 2 with Size => 2;
   type B3 is mod 2 ** 3 with Size => 3;
   type B6 is mod 2 ** 6 with Size => 6;
   type B7 is mod 2 ** 7 with Size => 7;
   type B8 is mod 2 ** 8 with Size => 8;
   type B9 is mod 2 ** 9 with Size => 9;
   type B12 is mod 2 ** 12 with Size => 12;
   type B16 is mod 2 ** 16 with Size => 16;
   type Header is record
      Length : B8 := 0;
      Reserved : B8 := 0;
      Subopcode : B8 := 0;
      Opcode : B3 := 0;
      Subtype_Code : B2 := 3;
      Command_Type : B3 := 3;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Header use record
      Length at 0 range 0 .. 7; Reserved at 0 range 8 .. 15;
      Subopcode at 0 range 16 .. 23; Opcode at 0 range 24 .. 26;
      Subtype_Code at 0 range 27 .. 28; Command_Type at 0 range 29 .. 31;
   end record;
   type Buffer_Control is record
      Pitch : B12 := 16;
      Reserved_12 : B1 := 0;
      Null_Buffer : B1 := 0;
      Address_Modify : B1 := 1;
      Reserved_15 : B1 := 0;
      MOCS : B7 := 0;
      Reserved_23 : B2 := 0;
      L3_Bypass_Disable : B1 := 1;
      Buffer_Index : B6 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Buffer_Control use record
      Pitch at 0 range 0 .. 11; Reserved_12 at 0 range 12 .. 12;
      Null_Buffer at 0 range 13 .. 13; Address_Modify at 0 range 14 .. 14;
      Reserved_15 at 0 range 15 .. 15; MOCS at 0 range 16 .. 22;
      Reserved_23 at 0 range 23 .. 24; L3_Bypass_Disable at 0 range 25 .. 25;
      Buffer_Index at 0 range 26 .. 31;
   end record;
   type Element_Control is record
      Source_Offset : B12 := 0;
      Reserved : B3 := 0;
      Edge_Flag : B1 := 0;
      Format : B9 := 0; -- R32G32B32A32_FLOAT.
      Valid : B1 := 1;
      Buffer_Index : B6 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Element_Control use record
      Source_Offset at 0 range 0 .. 11; Reserved at 0 range 12 .. 14;
      Edge_Flag at 0 range 15 .. 15; Format at 0 range 16 .. 24;
      Valid at 0 range 25 .. 25; Buffer_Index at 0 range 26 .. 31;
   end record;
   type Component_Control is record
      Reserved_Low : B16 := 0;
      Component_3 : B3 := 1;
      Reserved_19 : B1 := 0;
      Component_2 : B3 := 1;
      Reserved_23 : B1 := 0;
      Component_1 : B3 := 1;
      Reserved_27 : B1 := 0;
      Component_0 : B3 := 1;
      Reserved_31 : B1 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Component_Control use record
      Reserved_Low at 0 range 0 .. 15; Component_3 at 0 range 16 .. 18;
      Reserved_19 at 0 range 19 .. 19; Component_2 at 0 range 20 .. 22;
      Reserved_23 at 0 range 23 .. 23; Component_1 at 0 range 24 .. 26;
      Reserved_27 at 0 range 27 .. 27; Component_0 at 0 range 28 .. 30;
      Reserved_31 at 0 range 31 .. 31;
   end record;
   function Encode (V : Header) return Unsigned_32 is
     (Unsigned_32 (V.Length) or Shift_Left (Unsigned_32 (V.Reserved), 8) or
      Shift_Left (Unsigned_32 (V.Subopcode), 16) or Shift_Left (Unsigned_32 (V.Opcode), 24) or
      Shift_Left (Unsigned_32 (V.Subtype_Code), 27) or Shift_Left (Unsigned_32 (V.Command_Type), 29));
   function Encode (V : Buffer_Control) return Unsigned_32 is
     (Unsigned_32 (V.Pitch) or Shift_Left (Unsigned_32 (V.Reserved_12), 12) or
      Shift_Left (Unsigned_32 (V.Null_Buffer), 13) or Shift_Left (Unsigned_32 (V.Address_Modify), 14) or
      Shift_Left (Unsigned_32 (V.Reserved_15), 15) or Shift_Left (Unsigned_32 (V.MOCS), 16) or
      Shift_Left (Unsigned_32 (V.Reserved_23), 23) or Shift_Left (Unsigned_32 (V.L3_Bypass_Disable), 25) or
      Shift_Left (Unsigned_32 (V.Buffer_Index), 26));
   function Encode (V : Element_Control) return Unsigned_32 is
     (Unsigned_32 (V.Source_Offset) or Shift_Left (Unsigned_32 (V.Reserved), 12) or
      Shift_Left (Unsigned_32 (V.Edge_Flag), 15) or Shift_Left (Unsigned_32 (V.Format), 16) or
      Shift_Left (Unsigned_32 (V.Valid), 25) or Shift_Left (Unsigned_32 (V.Buffer_Index), 26));
   function Encode (V : Component_Control) return Unsigned_32 is
     (Unsigned_32 (V.Reserved_Low) or Shift_Left (Unsigned_32 (V.Component_3), 16) or
      Shift_Left (Unsigned_32 (V.Reserved_19), 19) or Shift_Left (Unsigned_32 (V.Component_2), 20) or
      Shift_Left (Unsigned_32 (V.Reserved_23), 23) or Shift_Left (Unsigned_32 (V.Component_1), 24) or
      Shift_Left (Unsigned_32 (V.Reserved_27), 27) or Shift_Left (Unsigned_32 (V.Component_0), 28) or
      Shift_Left (Unsigned_32 (V.Reserved_31), 31));
   -- Vol2a pp150-152. Bits13/14 are documented even though Mesa's GFX12
   -- packer does not expose them; the fixed path keeps both disabled.
   type VF_Control is record
      Length : B8 := 0;
      Indexed_Cut : B1 := 0;
      Component_Packing : B1 := 0;
      Sequential_Cut : B1 := 0;
      Vertex_ID_Offset : B1 := 0;
      Reserved_12 : B1 := 0;
      Instance_ID_Offset : B1 := 0;
      Force_Sequential : B1 := 0;
      Reserved_15 : B1 := 0;
      Subopcode : B8 := 12;
      Opcode : B3 := 0;
      Subtype_Code : B2 := 3;
      Command_Type : B3 := 3;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for VF_Control use record
      Length at 0 range 0 .. 7; Indexed_Cut at 0 range 8 .. 8;
      Component_Packing at 0 range 9 .. 9; Sequential_Cut at 0 range 10 .. 10;
      Vertex_ID_Offset at 0 range 11 .. 11; Reserved_12 at 0 range 12 .. 12;
      Instance_ID_Offset at 0 range 13 .. 13; Force_Sequential at 0 range 14 .. 14;
      Reserved_15 at 0 range 15 .. 15; Subopcode at 0 range 16 .. 23;
      Opcode at 0 range 24 .. 26; Subtype_Code at 0 range 27 .. 28;
      Command_Type at 0 range 29 .. 31;
   end record;
   function Encode (V : VF_Control) return Unsigned_32 is
     (Unsigned_32 (V.Length) or Shift_Left (Unsigned_32 (V.Indexed_Cut), 8) or
      Shift_Left (Unsigned_32 (V.Component_Packing), 9) or Shift_Left (Unsigned_32 (V.Sequential_Cut), 10) or
      Shift_Left (Unsigned_32 (V.Vertex_ID_Offset), 11) or Shift_Left (Unsigned_32 (V.Reserved_12), 12) or
      Shift_Left (Unsigned_32 (V.Instance_ID_Offset), 13) or Shift_Left (Unsigned_32 (V.Force_Sequential), 14) or
      Shift_Left (Unsigned_32 (V.Reserved_15), 15) or Shift_Left (Unsigned_32 (V.Subopcode), 16) or
      Shift_Left (Unsigned_32 (V.Opcode), 24) or Shift_Left (Unsigned_32 (V.Subtype_Code), 27) or
      Shift_Left (Unsigned_32 (V.Command_Type), 29));
   -- Vol2d pp132-138: optional instancing and system-generated inputs.
   type B22 is mod 2 ** 22 with Size => 22;
   type Instancing_Control is record
      Element : B6 := 0;
      Reserved_Low : B2 := 0;
      Enable : B1 := 0;
      Stride_Enable : B1 := 0;
      Reserved_High : B22 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Instancing_Control use record
      Element at 0 range 0 .. 5; Reserved_Low at 0 range 6 .. 7;
      Enable at 0 range 8 .. 8; Stride_Enable at 0 range 9 .. 9;
      Reserved_High at 0 range 10 .. 31;
   end record;
   type SGV_Control is record
      Vertex_Element : B6 := 0;
      Reserved_Low : B7 := 0;
      Vertex_Component : B2 := 0;
      Vertex_Enable : B1 := 0;
      Instance_Element : B6 := 0;
      Reserved_High : B7 := 0;
      Instance_Component : B2 := 0;
      Instance_Enable : B1 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for SGV_Control use record
      Vertex_Element at 0 range 0 .. 5; Reserved_Low at 0 range 6 .. 12;
      Vertex_Component at 0 range 13 .. 14; Vertex_Enable at 0 range 15 .. 15;
      Instance_Element at 0 range 16 .. 21; Reserved_High at 0 range 22 .. 28;
      Instance_Component at 0 range 29 .. 30; Instance_Enable at 0 range 31 .. 31;
   end record;
   type Extended_SGV_Control is record
      XP0_Element : B6 := 0;
      Reserved_Low : B6 := 0;
      XP0_Source : B1 := 0;
      XP0_Component : B2 := 0;
      XP0_Enable : B1 := 0;
      XP1_Element : B6 := 0;
      Reserved_High : B6 := 0;
      XP1_Source : B1 := 0;
      XP1_Component : B2 := 0;
      XP1_Enable : B1 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Extended_SGV_Control use record
      XP0_Element at 0 range 0 .. 5; Reserved_Low at 0 range 6 .. 11;
      XP0_Source at 0 range 12 .. 12; XP0_Component at 0 range 13 .. 14;
      XP0_Enable at 0 range 15 .. 15; XP1_Element at 0 range 16 .. 21;
      Reserved_High at 0 range 22 .. 27; XP1_Source at 0 range 28 .. 28;
      XP1_Component at 0 range 29 .. 30; XP1_Enable at 0 range 31 .. 31;
   end record;
   type XP2_Control is record
      Element : B6 := 0;
      Reserved_Low : B7 := 0;
      Component : B2 := 0;
      Enable : B1 := 0;
      Reserved_High : B16 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for XP2_Control use record
      Element at 0 range 0 .. 5; Reserved_Low at 0 range 6 .. 12;
      Component at 0 range 13 .. 14; Enable at 0 range 15 .. 15;
      Reserved_High at 0 range 16 .. 31;
   end record;
   function Encode (V : Instancing_Control) return Unsigned_32 is
     (Unsigned_32 (V.Element) or Shift_Left (Unsigned_32 (V.Reserved_Low), 6) or
      Shift_Left (Unsigned_32 (V.Enable), 8) or Shift_Left (Unsigned_32 (V.Stride_Enable), 9) or
      Shift_Left (Unsigned_32 (V.Reserved_High), 10));
   function Encode (V : SGV_Control) return Unsigned_32 is
     (Unsigned_32 (V.Vertex_Element) or Shift_Left (Unsigned_32 (V.Reserved_Low), 6) or
      Shift_Left (Unsigned_32 (V.Vertex_Component), 13) or Shift_Left (Unsigned_32 (V.Vertex_Enable), 15) or
      Shift_Left (Unsigned_32 (V.Instance_Element), 16) or Shift_Left (Unsigned_32 (V.Reserved_High), 22) or
      Shift_Left (Unsigned_32 (V.Instance_Component), 29) or Shift_Left (Unsigned_32 (V.Instance_Enable), 31));
   function Encode (V : Extended_SGV_Control) return Unsigned_32 is
     (Unsigned_32 (V.XP0_Element) or Shift_Left (Unsigned_32 (V.Reserved_Low), 6) or
      Shift_Left (Unsigned_32 (V.XP0_Source), 12) or Shift_Left (Unsigned_32 (V.XP0_Component), 13) or
      Shift_Left (Unsigned_32 (V.XP0_Enable), 15) or Shift_Left (Unsigned_32 (V.XP1_Element), 16) or
      Shift_Left (Unsigned_32 (V.Reserved_High), 22) or Shift_Left (Unsigned_32 (V.XP1_Source), 28) or
      Shift_Left (Unsigned_32 (V.XP1_Component), 29) or Shift_Left (Unsigned_32 (V.XP1_Enable), 31));
   function Encode (V : XP2_Control) return Unsigned_32 is
     (Unsigned_32 (V.Element) or Shift_Left (Unsigned_32 (V.Reserved_Low), 6) or
      Shift_Left (Unsigned_32 (V.Component), 13) or Shift_Left (Unsigned_32 (V.Enable), 15) or
      Shift_Left (Unsigned_32 (V.Reserved_High), 16));
   type Words is array (Natural range 0 .. 17) of Unsigned_32;
   type Image is record
      Valid : Boolean := False;
      Data : Words := [others => 0];
   end record;
   -- Encoded MOCS must additionally be admitted against installed policy.
   function Build (MOCS : Unsigned_32) return Image;
end Intel_GPU_ADLN_Vertex_Fetch;
