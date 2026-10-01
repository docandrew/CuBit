package body Intel_GPU_ADLN_L3 with SPARK_Mode is
   function Enabled_Banks (V : Fuse_Control) return Natural is
      Count : Natural := 0;
   begin
      for Bit in 0 .. 7 loop
         pragma Loop_Invariant (Count <= Bit);
         if (Unsigned_32 (V.Disabled_Banks) and Shift_Left (1, Bit)) = 0 then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Enabled_Banks;

   function Probe_URB_KiB
     (Owned : Boolean; Fuse : Fuse_Control; Info : Parameters;
      Observed : Allocation) return Natural is
   begin
      if not Owned or Fuse.Disabled_Banks /= 16#F0# or Fuse.Hash_Mode /= 0 or
        Fuse.WGBox_Configuration /= 0 or
        Natural (Info.Tagged_Ways) + Natural (Info.Untagged_Ways) /= 120 or
        Info.Untagged_Ways not in 16 .. 32 or Info.Tagged_Ways not in 88 .. 104 or
        Observed.Error_Status /= 0 or Observed.URB_Ways /= 32 or
        Observed.All_Client_Ways /= 88 or Observed.Read_Only_Ways /= 0 or
        Observed.Data_Ways /= 0
      then
         return 0;
      end if;
      return 512; -- four banks * 32 URB ways * 4 KiB/way
   end Probe_URB_KiB;
end Intel_GPU_ADLN_L3;
