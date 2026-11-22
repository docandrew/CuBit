with Intel_GPU_ADLN_Capture;
with Intel_GPU_Capture_List;
package body Intel_GPU_ADS_Capture_Image with SPARK_Mode is
   function Build
     (Description : Intel_GPU_ADLN_Inventory.Inventory;
      Steering : Intel_GPU_ADLN_Steering.Topology;
      GPU_Base, Backing_Bytes : Unsigned_64) return Capture_Image
   is
      Source : constant Intel_GPU_ADLN_Capture.Lists :=
        Intel_GPU_ADLN_Capture.Build (Description, Steering);
      Result : Capture_Image;
      Required : Natural := 2 * 4096; -- null page and PF global page
      Cursor : Natural := 4096;
      Limit : constant Unsigned_64 := 16#FEE0_0000#;
      procedure Put (Offset : Natural; Address : Unsigned_32)
        with Pre => Offset <= 260
      is
      begin
         for J in Natural range 0 .. 3 loop
            Result.Pointers (Offset + J) :=
              Unsigned_8 (Shift_Right (Address, J * 8) and 255);
         end loop;
      end Put;
      procedure Append (Data : Intel_GPU_Capture_List.Page; Pointer : Natural)
        with Pre => Cursor <= Capacity - 4096 and then Pointer <= 260
          and then GPU_Base <= Limit - Unsigned_64 (Capacity),
             Post => Cursor = Cursor'Old + 4096
      is
      begin
         Put (Pointer, Unsigned_32 (GPU_Base + Unsigned_64 (Cursor)));
         for J in Data'Range loop
            Result.Data (Cursor + J) := Data (J);
         end loop;
         Cursor := Cursor + 4096;
      end Append;
   begin
      if not Source.Valid then return Result; end if;
      for C in Intel_GPU_ADLN_Capture.Class_ID loop
         if Source.Classes (C)(0) /= 0 then Required := Required + 4096; end if;
         if Source.Instances (C)(0) /= 0 then Required := Required + 4096; end if;
      end loop;
      -- Reserve the complete bounded image range so append arithmetic has one
      -- uniform contract. Used still reports only the populated page prefix.
      if GPU_Base = 0 or else GPU_Base mod 4096 /= 0 or else
        GPU_Base > Limit - Unsigned_64 (Capacity) or else
        Backing_Bytes < Unsigned_64 (Capacity) or else Required > Capacity
      then return Result; end if;
      for I in Natural range 0 .. 65 loop
         Put (I * 4, Unsigned_32 (GPU_Base));
      end loop;
      for C in Intel_GPU_ADLN_Capture.Class_ID loop
         -- Defensive capacity checks also keep this safe if tables expand.
         if Source.Classes (C)(0) /= 0 then
            if Cursor > Capacity - 4096 then return (others => <>); end if;
            Append (Source.Classes (C), 128 + C * 4);
         end if;
         if Source.Instances (C)(0) /= 0 then
            if Cursor > Capacity - 4096 then return (others => <>); end if;
            Append (Source.Instances (C), C * 4);
         end if;
      end loop;
      if Cursor > Capacity - 4096 then return (others => <>); end if;
      Append (Source.Global, 256);
      if Cursor /= Required then return (others => <>); end if;
      Result.Used := Cursor;
      Result.Valid := True;
      return Result;
   end Build;
end Intel_GPU_ADS_Capture_Image;
