with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;
with Intel_GPU_ADLN_Vertex_Shader; use Intel_GPU_ADLN_Vertex_Shader;
procedure Vertex_Shader_Tests is
   function To_Kernel_Control is new Ada.Unchecked_Conversion (Unsigned_64, Kernel_Control);
   function To_Shader_Control is new Ada.Unchecked_Conversion (Unsigned_32, Shader_Control);
   function To_Scratch_Control is new Ada.Unchecked_Conversion (Unsigned_64, Scratch_Control);
   function To_Payload_Control is new Ada.Unchecked_Conversion (Unsigned_32, Payload_Control);
   function To_Dispatch_Control is new Ada.Unchecked_Conversion (Unsigned_32, Dispatch_Control);
   function To_Output_Control is new Ada.Unchecked_Conversion (Unsigned_32, Output_Control);
   R : Image;
begin
   for Bit in 0 .. 31 loop
      declare V : constant Unsigned_32 := Shift_Left (1, Bit); begin
         pragma Assert (Encode (To_Shader_Control (V)) = V);
         pragma Assert (Encode (To_Payload_Control (V)) = V);
         pragma Assert (Encode (To_Dispatch_Control (V)) = V);
         pragma Assert (Encode (To_Output_Control (V)) = V);
      end;
   end loop;
   for Bit in 0 .. 63 loop
      declare V : constant Unsigned_64 := Shift_Left (1, Bit); begin
         pragma Assert (Encode (To_Kernel_Control (V)) = V);
         pragma Assert (Encode (To_Scratch_Control (V)) = V);
      end;
   end loop;
   R := Build (546);
   pragma Assert (R.Valid and then R.Data = Words'
     [16#78100007#, 0, 0, 0, 0, 0, 16#00200800#, 16#88400405#, 0]);
   for Limit in 0 .. 1024 loop
      R := Build (Limit);
      pragma Assert (R.Valid = (Limit in 1 .. 546));
      if R.Valid then
         pragma Assert (Shift_Right (R.Data (7), 22) + 1 = Unsigned_32 (Limit));
         pragma Assert ((R.Data (7) and 16#3FFFFF#) = 16#405#);
      else
         pragma Assert (R.Data = Words'(others => 0));
      end if;
   end loop;
   R := Build (Natural'Last);
   pragma Assert (not R.Valid);
   Put_Line ("vertex shader PASS: 256 bit checks, thread limits, Mesa nine-word fixture");
end Vertex_Shader_Tests;
