package body Intel_GPU_Display_Presence with SPARK_Mode is
   function Decode (Vendor, Device : Unsigned_16; Class : Unsigned_8;
     First, Second : Unsigned_32) return Snapshot is
      F : constant Pipe_Fuses := From_Word (First);
   begin
      if Vendor /= 16#8086# or else Class /= 3 or else
        Device not in 16#46D0# .. 16#46D4# or else
        First /= Second or else First = Unsigned_32'Last
      then return (others => <>); end if;
      return (True,
        [A => (if F.Disable_A = 0 then Present else Absent),
         B => (if F.Disable_B = 0 then Present else Absent),
         C => (if F.Disable_C = 0 then Present else Absent),
         D => (if F.Disable_D = 0 then Present else Absent)]);
   end Decode;
end Intel_GPU_Display_Presence;
