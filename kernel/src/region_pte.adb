package body Region_PTE with SPARK_Mode => On is
   function Plan (Old, Expected_Address : Unsigned_64; Mode : Access_Mode)
     return Decision is
      NX : constant Unsigned_64 := 16#8000_0000_0000_0000#;
      -- Reject unknown/software metadata too; the dedicated allocator must
      -- construct plain entries. Hardware accessed/dirty bits are retained.
      Known : constant Unsigned_64 := Frame_Mask or NX or 16#67#;
      Flags : Unsigned_64;
   begin
      if Expected_Address = 0 or (Expected_Address and not Frame_Mask) /= 0 or
        (Old and Frame_Mask) /= Expected_Address or (Old and 4) = 0 or
        (Old and not Known) /= 0 or
        ((Old and 2) /= 0 and (Old and NX) = 0) or
        (Mode /= Inaccessible and (Old and 1) /= 0)
      then return (False, Old); end if;
      Flags := (case Mode is
         when Inaccessible => NX,
         when Read_Only => NX or 1,
         when Read_Write => NX or 3,
         when Read_Execute => 1);
      return (True, (Old and not Change_Mask) or Flags);
   end Plan;
end Region_PTE;
