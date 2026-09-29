package body Intel_GPU_GuC_CTB with SPARK_Mode is
   function Header (Fence : Unsigned_16; Payload_Words : Unsigned_32)
     return Unsigned_32 is (Shift_Left (Unsigned_32 (Fence), 16) or Payload_Words);
   function Send (Size : Ring_Size; Head, Tail, Local_Tail, Status,
                  Payload_Words : Unsigned_32; Fence : Unsigned_16) return Plan
   is
      Free, Words : Unsigned_32;
   begin
      if Status /= 0 or Head >= Size or Tail >= Size or Tail /= Local_Tail then
         return (others => <>);
      end if;
      if Payload_Words not in 1 .. 255 then
         return (State => Invalid_Message, others => <>);
      end if;
      Free := (Head + Size - Tail - 1) mod Size;
      Words := Payload_Words + 1;
      if Words > Free then return (State => Full, others => <>); end if;
      return (Ready, Tail, (Tail + Words) mod Size, Words, Fence);
   end Send;
   function Receive (Size : Ring_Size; Head, Tail, Local_Head, Status,
                     Frame_Header : Unsigned_32) return Plan
   is
      Available, Words : Unsigned_32;
   begin
      if Status /= 0 or Head >= Size or Tail >= Size or Head /= Local_Head then
         return (others => <>);
      end if;
      Available := (Tail + Size - Head) mod Size;
      if Available = 0 then return (State => Empty, others => <>); end if;
      if (Frame_Header and 16#FF00#) /= 0 or (Frame_Header and 255) = 0 then
         return (State => Invalid_Message, others => <>);
      end if;
      Words := (Frame_Header and 255) + 1;
      if Words > Available then return (State => Truncated, others => <>); end if;
      return (Ready, Head, (Head + Words) mod Size, Words,
              Unsigned_16 (Shift_Right (Frame_Header, 16)));
   end Receive;
end Intel_GPU_GuC_CTB;
