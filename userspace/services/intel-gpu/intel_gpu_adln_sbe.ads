with Interfaces; use Interfaces;
with System;
with Intel_GPU_ADLN_Probe_Shaders;
with Intel_GPU_ADLN_Vertex_Fetch;
package Intel_GPU_ADLN_SBE with SPARK_Mode is
   -- Intel TGL Vol2d84-90. Fixed shader has zero varying attributes.
   type B1 is mod 2 ** 1 with Size => 1;
   type B2 is mod 2 ** 2 with Size => 2;
   type B5 is mod 2 ** 5 with Size => 5;
   type B6 is mod 2 ** 6 with Size => 6;
   type Control is record
      Primitive_ID_Attribute : B5 := 0;
      Read_Offset_32B : B6 := 0;
      Read_Length_32B : B5 := 0;
      Primitive_ID_X : B1 := 0;
      Primitive_ID_Y : B1 := 0;
      Primitive_ID_Z : B1 := 0;
      Primitive_ID_W : B1 := 0;
      Sprite_Lower_Left : B1 := 0;
      Swizzle : B1 := 0;
      Output_Attributes : B6 := 0;
      Force_Read_Offset : B1 := 0;
      Force_Read_Length : B1 := 0;
      Reserved : B2 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Control use record
      Primitive_ID_Attribute at 0 range 0 .. 4;
      Read_Offset_32B at 0 range 5 .. 10;
      Read_Length_32B at 0 range 11 .. 15;
      Primitive_ID_X at 0 range 16 .. 16;
      Primitive_ID_Y at 0 range 17 .. 17;
      Primitive_ID_Z at 0 range 18 .. 18;
      Primitive_ID_W at 0 range 19 .. 19;
      Sprite_Lower_Left at 0 range 20 .. 20;
      Swizzle at 0 range 21 .. 21;
      Output_Attributes at 0 range 22 .. 27;
      Force_Read_Offset at 0 range 28 .. 28;
      Force_Read_Length at 0 range 29 .. 29;
      Reserved at 0 range 30 .. 31;
   end record;
   function Encode (V : Control) return Unsigned_32 is
     (Unsigned_32 (V.Primitive_ID_Attribute) or
      Shift_Left (Unsigned_32 (V.Read_Offset_32B), 5) or
      Shift_Left (Unsigned_32 (V.Read_Length_32B), 11) or
      Shift_Left (Unsigned_32 (V.Primitive_ID_X), 16) or
      Shift_Left (Unsigned_32 (V.Primitive_ID_Y), 17) or
      Shift_Left (Unsigned_32 (V.Primitive_ID_Z), 18) or
      Shift_Left (Unsigned_32 (V.Primitive_ID_W), 19) or
      Shift_Left (Unsigned_32 (V.Sprite_Lower_Left), 20) or
      Shift_Left (Unsigned_32 (V.Swizzle), 21) or
      Shift_Left (Unsigned_32 (V.Output_Attributes), 22) or
      Shift_Left (Unsigned_32 (V.Force_Read_Offset), 28) or
      Shift_Left (Unsigned_32 (V.Force_Read_Length), 29) or
      Shift_Left (Unsigned_32 (V.Reserved), 30));
   type Attribute_Mask is record
      Attribute_0 : B1 := 0;
      Attribute_1 : B1 := 0;
      Attribute_2 : B1 := 0;
      Attribute_3 : B1 := 0;
      Attribute_4 : B1 := 0;
      Attribute_5 : B1 := 0;
      Attribute_6 : B1 := 0;
      Attribute_7 : B1 := 0;
      Attribute_8 : B1 := 0;
      Attribute_9 : B1 := 0;
      Attribute_10 : B1 := 0;
      Attribute_11 : B1 := 0;
      Attribute_12 : B1 := 0;
      Attribute_13 : B1 := 0;
      Attribute_14 : B1 := 0;
      Attribute_15 : B1 := 0;
      Attribute_16 : B1 := 0;
      Attribute_17 : B1 := 0;
      Attribute_18 : B1 := 0;
      Attribute_19 : B1 := 0;
      Attribute_20 : B1 := 0;
      Attribute_21 : B1 := 0;
      Attribute_22 : B1 := 0;
      Attribute_23 : B1 := 0;
      Attribute_24 : B1 := 0;
      Attribute_25 : B1 := 0;
      Attribute_26 : B1 := 0;
      Attribute_27 : B1 := 0;
      Attribute_28 : B1 := 0;
      Attribute_29 : B1 := 0;
      Attribute_30 : B1 := 0;
      Attribute_31 : B1 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Attribute_Mask use record
      Attribute_0 at 0 range 0 .. 0;
      Attribute_1 at 0 range 1 .. 1;
      Attribute_2 at 0 range 2 .. 2;
      Attribute_3 at 0 range 3 .. 3;
      Attribute_4 at 0 range 4 .. 4;
      Attribute_5 at 0 range 5 .. 5;
      Attribute_6 at 0 range 6 .. 6;
      Attribute_7 at 0 range 7 .. 7;
      Attribute_8 at 0 range 8 .. 8;
      Attribute_9 at 0 range 9 .. 9;
      Attribute_10 at 0 range 10 .. 10;
      Attribute_11 at 0 range 11 .. 11;
      Attribute_12 at 0 range 12 .. 12;
      Attribute_13 at 0 range 13 .. 13;
      Attribute_14 at 0 range 14 .. 14;
      Attribute_15 at 0 range 15 .. 15;
      Attribute_16 at 0 range 16 .. 16;
      Attribute_17 at 0 range 17 .. 17;
      Attribute_18 at 0 range 18 .. 18;
      Attribute_19 at 0 range 19 .. 19;
      Attribute_20 at 0 range 20 .. 20;
      Attribute_21 at 0 range 21 .. 21;
      Attribute_22 at 0 range 22 .. 22;
      Attribute_23 at 0 range 23 .. 23;
      Attribute_24 at 0 range 24 .. 24;
      Attribute_25 at 0 range 25 .. 25;
      Attribute_26 at 0 range 26 .. 26;
      Attribute_27 at 0 range 27 .. 27;
      Attribute_28 at 0 range 28 .. 28;
      Attribute_29 at 0 range 29 .. 29;
      Attribute_30 at 0 range 30 .. 30;
      Attribute_31 at 0 range 31 .. 31;
   end record;
   function Encode (V : Attribute_Mask) return Unsigned_32 is
     (Unsigned_32 (V.Attribute_0) or
      Shift_Left (Unsigned_32 (V.Attribute_1), 1) or
      Shift_Left (Unsigned_32 (V.Attribute_2), 2) or
      Shift_Left (Unsigned_32 (V.Attribute_3), 3) or
      Shift_Left (Unsigned_32 (V.Attribute_4), 4) or
      Shift_Left (Unsigned_32 (V.Attribute_5), 5) or
      Shift_Left (Unsigned_32 (V.Attribute_6), 6) or
      Shift_Left (Unsigned_32 (V.Attribute_7), 7) or
      Shift_Left (Unsigned_32 (V.Attribute_8), 8) or
      Shift_Left (Unsigned_32 (V.Attribute_9), 9) or
      Shift_Left (Unsigned_32 (V.Attribute_10), 10) or
      Shift_Left (Unsigned_32 (V.Attribute_11), 11) or
      Shift_Left (Unsigned_32 (V.Attribute_12), 12) or
      Shift_Left (Unsigned_32 (V.Attribute_13), 13) or
      Shift_Left (Unsigned_32 (V.Attribute_14), 14) or
      Shift_Left (Unsigned_32 (V.Attribute_15), 15) or
      Shift_Left (Unsigned_32 (V.Attribute_16), 16) or
      Shift_Left (Unsigned_32 (V.Attribute_17), 17) or
      Shift_Left (Unsigned_32 (V.Attribute_18), 18) or
      Shift_Left (Unsigned_32 (V.Attribute_19), 19) or
      Shift_Left (Unsigned_32 (V.Attribute_20), 20) or
      Shift_Left (Unsigned_32 (V.Attribute_21), 21) or
      Shift_Left (Unsigned_32 (V.Attribute_22), 22) or
      Shift_Left (Unsigned_32 (V.Attribute_23), 23) or
      Shift_Left (Unsigned_32 (V.Attribute_24), 24) or
      Shift_Left (Unsigned_32 (V.Attribute_25), 25) or
      Shift_Left (Unsigned_32 (V.Attribute_26), 26) or
      Shift_Left (Unsigned_32 (V.Attribute_27), 27) or
      Shift_Left (Unsigned_32 (V.Attribute_28), 28) or
      Shift_Left (Unsigned_32 (V.Attribute_29), 29) or
      Shift_Left (Unsigned_32 (V.Attribute_30), 30) or
      Shift_Left (Unsigned_32 (V.Attribute_31), 31));
   type Component_Group is record
      Attribute_0 : B2 := 0;
      Attribute_1 : B2 := 0;
      Attribute_2 : B2 := 0;
      Attribute_3 : B2 := 0;
      Attribute_4 : B2 := 0;
      Attribute_5 : B2 := 0;
      Attribute_6 : B2 := 0;
      Attribute_7 : B2 := 0;
      Attribute_8 : B2 := 0;
      Attribute_9 : B2 := 0;
      Attribute_10 : B2 := 0;
      Attribute_11 : B2 := 0;
      Attribute_12 : B2 := 0;
      Attribute_13 : B2 := 0;
      Attribute_14 : B2 := 0;
      Attribute_15 : B2 := 0;
   end record with Size => 32, Bit_Order => System.Low_Order_First;
   for Component_Group use record
      Attribute_0 at 0 range 0 .. 1;
      Attribute_1 at 0 range 2 .. 3;
      Attribute_2 at 0 range 4 .. 5;
      Attribute_3 at 0 range 6 .. 7;
      Attribute_4 at 0 range 8 .. 9;
      Attribute_5 at 0 range 10 .. 11;
      Attribute_6 at 0 range 12 .. 13;
      Attribute_7 at 0 range 14 .. 15;
      Attribute_8 at 0 range 16 .. 17;
      Attribute_9 at 0 range 18 .. 19;
      Attribute_10 at 0 range 20 .. 21;
      Attribute_11 at 0 range 22 .. 23;
      Attribute_12 at 0 range 24 .. 25;
      Attribute_13 at 0 range 26 .. 27;
      Attribute_14 at 0 range 28 .. 29;
      Attribute_15 at 0 range 30 .. 31;
   end record;
   function Encode (V : Component_Group) return Unsigned_32 is
     (Unsigned_32 (V.Attribute_0) or
      Shift_Left (Unsigned_32 (V.Attribute_1), 2) or
      Shift_Left (Unsigned_32 (V.Attribute_2), 4) or
      Shift_Left (Unsigned_32 (V.Attribute_3), 6) or
      Shift_Left (Unsigned_32 (V.Attribute_4), 8) or
      Shift_Left (Unsigned_32 (V.Attribute_5), 10) or
      Shift_Left (Unsigned_32 (V.Attribute_6), 12) or
      Shift_Left (Unsigned_32 (V.Attribute_7), 14) or
      Shift_Left (Unsigned_32 (V.Attribute_8), 16) or
      Shift_Left (Unsigned_32 (V.Attribute_9), 18) or
      Shift_Left (Unsigned_32 (V.Attribute_10), 20) or
      Shift_Left (Unsigned_32 (V.Attribute_11), 22) or
      Shift_Left (Unsigned_32 (V.Attribute_12), 24) or
      Shift_Left (Unsigned_32 (V.Attribute_13), 26) or
      Shift_Left (Unsigned_32 (V.Attribute_14), 28) or
      Shift_Left (Unsigned_32 (V.Attribute_15), 30));
   type State is record
      Configuration : Control;
      Point_Sprites : Attribute_Mask;
      Constant_Interpolation : Attribute_Mask;
      Components_0_15 : Component_Group;
      Components_16_31 : Component_Group;
   end record with Size => 160, Bit_Order => System.Low_Order_First;
   for State use record
      Configuration at 0 range 0 .. 31;
      Point_Sprites at 4 range 0 .. 31;
      Constant_Interpolation at 8 range 0 .. 31;
      Components_0_15 at 12 range 0 .. 31;
      Components_16_31 at 16 range 0 .. 31;
   end record;
   type Body_Words is array (Natural range 0 .. 4) of Unsigned_32;
   function Encode (V : State) return Body_Words is
     [Encode (V.Configuration), Encode (V.Point_Sprites),
      Encode (V.Constant_Interpolation), Encode (V.Components_0_15),
      Encode (V.Components_16_31)];
   Fixed_State : constant State :=
     (Configuration => (Read_Offset_32B => 1, Read_Length_32B => 1,
         Force_Read_Offset => 1, Force_Read_Length => 1, others => <>),
      Point_Sprites => (others => <>), Constant_Interpolation => (others => <>),
      Components_0_15 => (others => 3), Components_16_31 => (others => 3));
   -- Read length cannot be zero even with zero output attributes. The
   -- [32,64) read remains within the one-row64B VS allocation. No varying
   -- output is delivered, consistent with PS_EXTRA.Attributes=0.
   pragma Compile_Time_Error
     (Intel_GPU_ADLN_Probe_Shaders.Vertex_URB_Entry_Size * 64 < 64,
      "SBE read exceeds fixed VS allocation");
   Encoded_State : constant Body_Words := Encode (Fixed_State);
   type Words is array (Natural range 0 .. 5) of Unsigned_32;
   Initial : constant Words :=
     [Intel_GPU_ADLN_Vertex_Fetch.Encode
        (Intel_GPU_ADLN_Vertex_Fetch.Header'
           (Length => 4, Subopcode => 16#1F#, others => <>)),
      Encoded_State (0), Encoded_State (1), Encoded_State (2),
      Encoded_State (3), Encoded_State (4)];
end Intel_GPU_ADLN_SBE;
