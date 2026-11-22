package body Intel_GPU_GuC_TLB_Protocol with SPARK_Mode is
   function Build (Sequence : Unsigned_32; Domain : Target) return Request_Words is
      Fields : constant Options :=
        (Kind => (if Domain = Engines then 0 else 3),
         Mode => 0, Reserved => 0, Flush_Cache => 1);
   begin
      return [16#20007000#, Sequence, Encode (Fields)];
   end Build;
   function Decode_Completion (Payload : Words) return Completion is
   begin
      if Payload'Length /= 2 or else Payload (Payload'First) /= 16#90007001# then
         return (False, 0);
      end if;
      return (True, Payload (Payload'First + 1));
   end Decode_Completion;
end Intel_GPU_GuC_TLB_Protocol;
