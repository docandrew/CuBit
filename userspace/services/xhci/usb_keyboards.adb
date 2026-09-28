package body USB_Keyboards with SPARK_Mode is
   procedure Decode (Data : Report; Keys : out Key_Set; Result : out Decode_Result) is
   begin
      Keys := [others => False];
      Result := Malformed;
      if Data (2) /= 0 then return; end if;
      -- ErrorRollOver, POSTFail and ErrorUndefined are not key usages.
      for I in 3 .. 8 loop
         if Data (I) in 1 .. 3 then
            Result := Rollover;
            return;
         end if;
         -- Modifier usages belong only in byte one of a boot report.
         if Data (I) >= 16#E0# then return; end if;
      end loop;
      for I in 3 .. 8 loop
         if Data (I) /= 0 then Keys (Data (I)) := True; end if;
      end loop;
      for Bit in 0 .. 7 loop
         Keys (16#E0# + Unsigned_8 (Bit)) :=
           (Data (1) and Shift_Left (Unsigned_8'(1), Bit)) /= 0;
      end loop;
      Result := Decoded;
   end Decode;

   procedure Update
     (Previous : in out State; Data : Report; Events : out Changes;
      Result : out Decode_Result)
   is
      Current : Key_Set;
   begin
      Decode (Data, Current, Result);
      Events := (others => [others => False]);
      if Result /= Decoded then return; end if;
      for K in Key_Set'Range loop
         Events.Released (K) := Previous.Held (K) and not Current (K);
         Events.Pressed (K) := Current (K) and not Previous.Held (K);
      end loop;
      Previous.Held := Current;
   end Update;

   procedure Release_All (Previous : in out State; Events : out Changes) is
   begin
      Events := (Released => Previous.Held, Pressed => [others => False]);
      Previous.Held := [others => False];
   end Release_All;
end USB_Keyboards;
