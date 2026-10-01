package body Intel_GPU_Nonpriv_Registers with SPARK_Mode is
   function Evaluate
     (Entries : Register_List; Offset : Unsigned_32; Access_Kind : Operation)
      return Decision
   is
      Result : Decision := Unspecified;
      Granularity : Unsigned_32;
   begin
      if Offset mod 4 /= 0 or Offset >= 2 ** 26 then return Invalid; end if;
      -- Validate all entries first: a matching allow/deny must not conceal an
      -- uninterpretable entry elsewhere in the snapshot.
      for Value of Entries loop
         if Value.Reserved /= 0 or Value.Access_Selection = 3 or
           Value.Virtual_Function /= 0
         then return Invalid; end if;
      end loop;
      for Value of Entries loop
         Granularity := (case Value.Offset_Range is
                           when 0 => 4, when 1 => 16,
                           when 2 => 64, when 3 => 256);
         if Offset / Granularity =
           (Unsigned_32 (Value.Address_DWords) * 4) / Granularity and then
           (Value.Access_Selection = 0 or else
            (Value.Access_Selection = 1 and Access_Kind = Read_Register) or else
            (Value.Access_Selection = 2 and Access_Kind = Write_Register))
         then
            if Value.Denylist = 1 then return Deny; end if;
            Result := Allow;
         end if;
      end loop;
      return Result;
   end Evaluate;
end Intel_GPU_Nonpriv_Registers;
