package body Intel_GPU_ADLN_Golden with SPARK_Mode is
   use Intel_GPU_ADLN_Inventory;
   function Required_Bytes (Description : Inventory) return Unsigned_64 is
      Result : Unsigned_64 := 16 * 4096; -- render14 + copy2 pages
   begin
      if not Description.Valid or else not Description.Engines (Render) or else
        not Description.Engines (Copy)
      then return 0; end if;
      if Description.Engines (Video_0) or Description.Engines (Video_2) then
         Result := Result + 2 * 4096;
      end if;
      if Description.Engines (Enhance_0) then Result := Result + 2 * 4096; end if;
      return Result;
   end Required_Bytes;
   function Plan (Description : Inventory; GPU_Base, Capacity : Unsigned_64)
     return Reservation is
      Result : Reservation;
      Required : constant Unsigned_64 := Required_Bytes (Description);
      Limit : constant Unsigned_64 := 16#FEE0_0000#;
      Cursor : Unsigned_64 := GPU_Base;
      type Class_Order is array (Positive range 1 .. 4) of Natural;
      Order : constant Class_Order := [0, 3, 1, 2]; -- render,copy,video,enhance
      Enabled : constant array (Positive range 1 .. 4) of Boolean :=
        [True, True, Description.Engines (Video_0) or Description.Engines (Video_2),
         Description.Engines (Enhance_0)];
      Full_Size : Unsigned_64;
   begin
      if Required = 0 or else Required > Capacity or else GPU_Base = 0 or else
        GPU_Base mod 4096 /= 0 or else GPU_Base >= Limit or else Required > Limit - GPU_Base
      then return Result; end if;
      for I in Order'Range loop
         if Enabled (I) then
            Full_Size := (if I = 1 then 14 * 4096 else 2 * 4096);
            if Cursor >= Limit or else Full_Size > Limit - Cursor then
               return (others => <>);
            end if;
            Result.Addresses (Order (I)) := Unsigned_32 (Cursor);
            Result.State_Bytes (Order (I)) := Unsigned_32 (Full_Size - 4416);
            Cursor := Cursor + Full_Size;
         end if;
      end loop;
      Result.Bytes := Required;
      Result.Valid := True;
      return Result;
   end Plan;
end Intel_GPU_ADLN_Golden;
