------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.Wifi_Report with SPARK_Mode is

   function Bit (I : WS.Entry_Index) return Unsigned_32 is (Shift_Left (1, I - 1));

   function Encode (R : Report) return Words is
      W : Words := [others => 0];
      Remembered : Unsigned_32 := 0;
   begin
      W (Generation_Word) := R.Generation;
      W (State_Word) := Unsigned_32 (C.State'Enum_Rep (R.State))
        or Shift_Left (Unsigned_32 (C.Failure'Pos (R.Failure)), Failure_Shift)
        or (if R.Target_Valid then Target_Flag else 0);
      for I in WS.Entry_Index loop
         pragma Loop_Invariant (W (Generation_Word) = R.Generation);
         if R.Remembered (I) then
            Remembered := Remembered or Bit (I);
         end if;
      end loop;
      W (Remembered_Word) := Remembered;
      if R.Target_Valid then
         declare
            T : constant WS.Entry_Words_Array := WS.Encode (R.Target);
         begin
            for K in WS.Entry_Word_Index loop
               pragma Loop_Invariant (W (Generation_Word) = R.Generation);
               W (Target_First + K) := T (K);
            end loop;
         end;
      end if;
      declare
         B : constant WS.Block := WS.Encode (R.Scan, R.Scan_Generation);
      begin
         for K in WS.Block_Word_Index loop
            pragma Loop_Invariant (W (Generation_Word) = R.Generation);
            W (Scan_First + K) := B (K);
         end loop;
      end;
      return W;
   end Encode;

   function Decode (W : Words) return Decoded_Report is
      R : Report;
      State_Value   : constant Unsigned_32 := W (State_Word) and Byte_Mask;
      Failure_Value : constant Unsigned_32 := Shift_Right (W (State_Word), Failure_Shift) and Byte_Mask;
      Block : WS.Block;
      Target_Words : WS.Entry_Words_Array;
   begin
      if (W (State_Word) and not (Byte_Mask or Shift_Left (Byte_Mask, Failure_Shift) or Target_Flag)) /= 0
        or else State_Value > Unsigned_32 (C.State'Enum_Rep (C.State'Last))
        or else Failure_Value > Unsigned_32 (C.Failure'Pos (C.Failure'Last))
      then
         return (Valid => False);
      end if;
      R.Generation := W (Generation_Word);
      R.State := C.State'Enum_Val (State_Value);
      R.Failure := C.Failure'Val (Failure_Value);
      R.Target_Valid := (W (State_Word) and Target_Flag) /= 0;
      for K in WS.Entry_Word_Index loop
         Target_Words (K) := W (Target_First + K);
      end loop;
      if R.Target_Valid then
         declare
            T : constant WS.Decoded_Entry := WS.Decode (Target_Words);
         begin
            if not T.Valid then
               return (Valid => False);
            end if;
            R.Target := T.Item;
         end;
      end if;
      for K in WS.Block_Word_Index loop
         Block (K) := W (Scan_First + K);
      end loop;
      declare
         S : constant WS.Decoded_Result := WS.Decode (Block);
      begin
         if not S.Valid then
            return (Valid => False);
         end if;
         R.Scan := S.Item;
         R.Scan_Generation := Block (WS.Generation_Word);
      end;
      for I in WS.Entry_Index loop
         R.Remembered (I) := (W (Remembered_Word) and Bit (I)) /= 0;
      end loop;
      if not Normal (R) or else Encode (R) /= W then
         return (Valid => False);
      end if;
      return (Valid => True, Item => R);
   end Decode;

end CuBit.Wifi_Report;
