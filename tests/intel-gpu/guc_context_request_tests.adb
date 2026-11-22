with Interfaces; use Interfaces;
with Intel_GPU_GuC_Context_Request;
procedure GuC_Context_Request_Tests is
   package Encoder renames Intel_GPU_GuC_Context_Request;
   use type Encoder.Request_Words;
   procedure Reject (ID : Unsigned_32; GPU, Bias : Unsigned_64) is
      Value : constant Encoder.Request := Encoder.Build (ID, GPU, Bias);
   begin
      pragma Assert (not Value.Valid and Value.Words = [0 .. 11 => 0]);
   end Reject;
begin
   for ID in Unsigned_32 range 0 .. 65534 loop
      declare
         Value : constant Encoder.Request := Encoder.Build (ID, 16#200000#, 4096);
      begin
         pragma Assert (Value.Valid and Value.Words =
           [16#20004502#, 1, ID, 0, 1, 0, 0, 0, 0, 0, 16#20031D#, 0]);
      end;
   end loop;
   Reject (65535, 16#200000#, 4096);
   Reject (Unsigned_32'Last, 16#200000#, 4096);
   Reject (0, 0, 4096);
   Reject (0, 16#200000#, 0);
   Reject (0, 16#200000#, 1);
   Reject (0, 16#200000#, 16#201000#);
   Reject (0, 16#FEDF1000#, 4096);
   Reject (0, Unsigned_64'Last, 4096);
   for Offset in Unsigned_64 range 1 .. 4095 loop
      Reject (0, 16#200000# + Offset, 4096);
   end loop;
   declare
      Value : constant Encoder.Request := Encoder.Build (65534, 16#FEDF0000#, 16#FEDF0000#);
   begin
      pragma Assert (Value.Valid and Value.Words (10) = 16#FEDF031D#);
   end;
end GuC_Context_Request_Tests;
