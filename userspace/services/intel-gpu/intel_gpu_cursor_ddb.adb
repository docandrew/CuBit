package body Intel_GPU_Cursor_DDB with SPARK_Mode is
   function Decode (Word : Unsigned_32) return Range_Decoding is
      First : constant Unsigned_32 := Word and Start_Mask;
      Last  : constant Unsigned_32 := Shift_Right (Word, End_Shift) and End_Mask;
   begin
      if (Word and Buffer_Spare_Bits) /= 0 then
         return (Valid => False);
      elsif Word = 0 then
         return (True, (0, 0));
      elsif Last < First then
         return (Valid => False);
      end if;
      return (True, (Block (First), Block (Last) + 1));
   end Decode;

   function Plan
     (Plane_Buffer : Unsigned_32; Other_Buffers : Other_Planes; Plane_WM : Watermarks;
      Cursor_Buffer : Unsigned_32; Single_Pipe : Boolean) return Plan_Result
   is
      Result : Plan_Result;
      Old : Range_Decoding;
      Want : Natural := (if Single_Pipe then Single_Pipe_Minimum else Several_Pipe_Minimum);
      Cursor_Size : Block;
      Plane_Size : Block;
   begin
      if Cursor_Buffer /= 0 then
         Result.State := Cursor_Already_Allocated;
         return Result;
      end if;
      Old := Decode (Plane_Buffer);
      if not Old.Valid or else Empty (Old.Value) then
         return Result;
      end if;
      for P in Other_Planes'Range loop
         if not Decode (Other_Buffers (P)).Valid then
            return Result;
         end if;
      end loop;
      --  The cursor's allocation: the base minimum, or more for its derived
      --  watermark levels, within Cursor_Limit.
      for S in Watermark_Slot loop
         if Enabled (Plane_WM (S)) then
            declare
               Need : constant Positive :=
                 Minimum_Allocation (Cursor_Watermark_Blocks (Plane_WM (S)));
            begin
               if Need <= Cursor_Limit then
                  Want := Natural'Max (Want, Need);
               end if;
            end;
         end if;
         pragma Loop_Invariant (Want <= Cursor_Limit);
         pragma Loop_Invariant
           (Want >= (if Single_Pipe then Single_Pipe_Minimum else Several_Pipe_Minimum));
      end loop;
      Cursor_Size := Want;
      if Size (Old.Value) <= Cursor_Size then
         Result.State := Plane_Too_Small;
         return Result;
      end if;
      Plane_Size := Size (Old.Value) - Cursor_Size;
      Result.Plane_Before := Old.Value;
      Result.Plane_After := (Old.Value.First, Old.Value.Stop - Cursor_Size);
      Result.Cursor := (Old.Value.Stop - Cursor_Size, Old.Value.Stop);
      for P in Other_Planes'Range loop
         if not Disjoint (Decode (Other_Buffers (P)).Value, Result.Cursor) then
            Result.State := Overlaps_Other_Plane;
            return Result;
         end if;
         pragma Loop_Invariant
           (for all Q in Other_Planes'First .. P =>
              Decode (Other_Buffers (Q)).Valid and then
              Disjoint (Decode (Other_Buffers (Q)).Value, Result.Cursor));
      end loop;
      --  Plane 1 keeps every watermark that still fits its smaller range.
      if not Enabled (Plane_WM (Level_0)) or else Blocks (Plane_WM (Level_0)) >= Plane_Size then
         Result.State := Plane_Too_Small;
         return Result;
      end if;
      for S in Watermark_Slot loop
         if Fits (Plane_WM (S), Plane_Size) then
            Result.Plane_WM (S) := Plane_WM (S);
         else
            Result.Plane_WM (S) := Plane_WM (S) - Enable_Bit;
            Result.Plane_Levels_Disabled := Result.Plane_Levels_Disabled + 1;
         end if;
         pragma Loop_Invariant (Result.Plane_Levels_Disabled <= Watermark_Slot'Pos (S) + 1);
         pragma Loop_Invariant
           (for all T in Watermark_Slot'First .. S => Fits (Result.Plane_WM (T), Plane_Size));
         pragma Loop_Invariant (Result.Plane_WM (Level_0) = Plane_WM (Level_0));
      end loop;
      --  The cursor's derived watermarks, enabled where they fit.
      for S in Watermark_Slot loop
         if Enabled (Plane_WM (S)) then
            declare
               Value : constant Positive := Cursor_Watermark_Blocks (Plane_WM (S));
               Line_Count : constant Positive :=
                 Natural'Max (1, Natural'Min (Max_Lines, Lines (Plane_WM (S))));
               Block_Unit : constant := 4_096;   --  2 ** 12: above the blocks field
               Line_Units : constant Unsigned_32 := Unsigned_32 (Line_Count) * 4;
               Flag_Units : constant Unsigned_32 :=
                 (if Minimum_Allocation (Value) <= Cursor_Size then Enable_Bit / Block_Unit else 0);
               Units : constant Unsigned_32 := Flag_Units + Line_Units;
               Word : Unsigned_32;
               Low : constant Unsigned_32 := Unsigned_32 (Value);
            begin
               pragma Assert (Value < Block_Unit);
               pragma Assert (Units <= Enable_Bit / Block_Unit + 1020);
               Word := Units * Block_Unit + Low;
               pragma Assert (Natural (Low) = Value);
               pragma Assert (Word mod Block_Unit = Low);
               pragma Assert (if Flag_Units = 0 then Word < Enable_Bit);
               pragma Assert (Blocks_Mask + 1 = Block_Unit);
               pragma Assert (Blocks (Word) = Value);
               pragma Assert (if Enabled (Word) then Flag_Units /= 0);
               pragma Assert (Fits (Word, Cursor_Size));
               Result.Cursor_WM (S) := Word;
            end;
         else
            Result.Cursor_WM (S) := 0;
         end if;
         pragma Loop_Invariant
           (for all T in Watermark_Slot'First .. S =>
              Fits (Result.Cursor_WM (T), Cursor_Size));
      end loop;
      Result.Plane_Buffer := Encode (Result.Plane_After);
      Result.Cursor_Buffer := Encode (Result.Cursor);
      Result.State := Ready;
      return Result;
   end Plan;
end Intel_GPU_Cursor_DDB;
