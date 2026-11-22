with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;
with Intel_GPU_ADLN_Pixel_Shader; use Intel_GPU_ADLN_Pixel_Shader;
with Intel_GPU_ADLN_Pixel_Extra;
procedure Pixel_Shader_Tests is
   package Extra renames Intel_GPU_ADLN_Pixel_Extra;
   use type Extra.Words;
   function To_Extra is new Ada.Unchecked_Conversion (Unsigned_32, Extra.Control);
   function To_Shader is new Ada.Unchecked_Conversion (Unsigned_32, Shader_Control);
   function To_Dispatch is new Ada.Unchecked_Conversion (Unsigned_32, Dispatch_Control);
   function To_Payload is new Ada.Unchecked_Conversion (Unsigned_32, Payload_Control);
   R : Image;
begin
   for Bit in 0 .. 31 loop
      declare V : constant Unsigned_32 := Shift_Left (1, Bit); begin
         pragma Assert (Encode (To_Shader (V)) = V);
         pragma Assert (Encode (To_Dispatch (V)) = V);
         pragma Assert (Encode (To_Payload (V)) = V);
         pragma Assert (Extra.Encode (To_Extra (V)) = V);
      end;
   end loop;
   R := Build (64);
   pragma Assert (Extra.Enabled = Extra.Words'[16#784F0000#, 16#80000000#]);
   pragma Assert (R.Valid and then R.Data = Words'
     [16#7820000A#, 16#100#, 0, 16#40000000#, 0, 0,
      16#1F800003#, 16#00020002#, 16#100#, 0, 16#140#, 0]);
   for Limit in 0 .. 1024 loop
      R := Build (Limit);
      pragma Assert (R.Valid = (Limit in 1 .. 64));
      if R.Valid then
         pragma Assert (Shift_Right (R.Data (6), 23) + 1 = Unsigned_32 (Limit));
         pragma Assert ((R.Data (6) and 16#7FFFFF#) = 3);
      else
         pragma Assert (R.Data = Words'(others => 0));
      end if;
   end loop;
   R := Build (Natural'Last);
   pragma Assert (not R.Valid);
   Put_Line ("pixel shader PASS: 128 bit checks, thread bounds, Mesa PS/PS_EXTRA fixtures");
end Pixel_Shader_Tests;
