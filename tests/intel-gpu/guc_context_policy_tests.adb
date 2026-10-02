with Interfaces; use Interfaces;
with Intel_GPU_GuC_Context_Request;
procedure GuC_Context_Policy_Tests is
   package Encoder renames Intel_GPU_GuC_Context_Request;
   use type Encoder.Request_Words;
   use type Encoder.Mode_Words;
begin
   for ID in Unsigned_32 range 0 .. 65534 loop
      for Forced in Boolean loop
         declare
            Value : constant Encoder.Policy_Request :=
              Encoder.Policy (ID, 1000, 500000, Forced);
         begin
            pragma Assert (Value.Length = (if Forced then 12 else 10));
            pragma Assert (Value.Words =
              [16#2000100B#, ID, 16#20030001#, 2,
               16#20010001#, 1000, 16#20020001#, 500000,
               16#20050001#, 0, (if Forced then 16#20040001# else 0),
               (if Forced then 1 else 0)]);
            pragma Assert (Encoder.Scheduling_Mode (ID, Forced) =
              [16#20001001#, ID, (if Forced then 1 else 0)]);
         end;
      end loop;
   end loop;
   for Invalid in 0 .. 3 loop
      declare
         Value : constant Encoder.Policy_Request := Encoder.Policy
           ((if Invalid = 0 then 65535 elsif Invalid = 1 then Unsigned_32'Last else 0),
            (if Invalid = 2 then 0 else 1), (if Invalid = 3 then 0 else 1), True);
      begin
         pragma Assert (Value.Length = 0 and Value.Words = [0 .. 11 => 0]);
      end;
   end loop;
   pragma Assert (Encoder.Scheduling_Mode (65535, True) = [0, 0, 0]);
   pragma Assert (Encoder.Scheduling_Mode (Unsigned_32'Last, False) = [0, 0, 0]);
   declare
      Value : constant Encoder.Policy_Request :=
        Encoder.Policy (65534, Unsigned_32'Last, Unsigned_32'Last, False);
   begin
      pragma Assert (Value.Length = 10 and Value.Words (5) = Unsigned_32'Last
                     and Value.Words (7) = Unsigned_32'Last);
   end;
end GuC_Context_Policy_Tests;
