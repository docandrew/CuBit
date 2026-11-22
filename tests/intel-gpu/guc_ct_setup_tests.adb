with Interfaces; use Interfaces;
with Intel_GPU_GuC_CT_Setup; use Intel_GPU_GuC_CT_Setup;
with Ada.Text_IO;
procedure GuC_CT_Setup_Tests is
   P : Plan;
begin
   P := Prepare (16#200000#, 32768, 16#200000#);
   pragma Assert (P.Valid);
   pragma Assert (P.Register_Buffers = Requests'
     [(4, [16#508#,16#9060002#,16#201000#,0]),
      (4, [16#508#,16#9050002#,16#203000#,0]),
      (4, [16#508#,16#9070001#,16384,0]),
      (4, [16#508#,16#9030002#,16#200000#,0]),
      (4, [16#508#,16#9020002#,16#202000#,0]),
      (4, [16#508#,16#9040001#,4096,0])]);
   pragma Assert (P.Enable = Request'(2,[16#4509#,1,0,0]));
   for Bytes in Unsigned_64 range 0 .. 65536 loop
      pragma Assert (Prepare (4096, Bytes, 4096).Valid =
        (Bytes >= 28672 and Bytes mod 4096 = 0));
   end loop;
   for Offset in Unsigned_64 range 0 .. 8191 loop
      pragma Assert (Prepare (16#FEE00000# - 28672 + Offset,28672,4096).Valid = (Offset = 0));
   end loop;
   pragma Assert (not Prepare (0,32768,4096).Valid);
   pragma Assert (not Prepare (4096,32768,8192).Valid);
   pragma Assert (not Prepare (4096,32768,0).Valid);
   pragma Assert (not Prepare (4096,32768,1).Valid);
   pragma Assert (not Prepare (Unsigned_64'Last,32768,4096).Valid);
   pragma Assert (not Prepare (4096,Unsigned_64'Last,4096).Valid);
   pragma Assert (Registered_Response (16#F0000001#));
   pragma Assert (Enabled_Response (16#F0000000#));
   for Bit in 0 .. 31 loop
      pragma Assert (not Registered_Response (16#F0000001# xor Shift_Left (Unsigned_32'(1),Bit)));
      pragma Assert (not Enabled_Response (16#F0000000# xor Shift_Left (Unsigned_32'(1),Bit)));
   end loop;
   Ada.Text_IO.Put_Line ("CT setup PASS: literal KLV order/words, bounds, exact success responses (no transport)");
end GuC_CT_Setup_Tests;
