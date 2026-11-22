with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
package body Intel_GPU_Native_Combo_State is
   use Intel_GPU_Combo_PHY;
   Active : Boolean := False;
   function Hex (Value : Unsigned_32) return String is
      Characters : constant String := "0123456789ABCDEF";
      Result : String (1 .. 8);
      Rest : Unsigned_32 := Value;
   begin
      for I in reverse Result'Range loop
         Result (I) := Characters (Natural (Rest and 15) + 1);
         Rest := Shift_Right (Rest, 4);
      end loop;
      return Result;
   end Hex;
   function Diagnostic (Value : Observation) return String is
     ("sample=" & (case Value.Reason is
        when None => "none", when Power_Lost => "power-lost",
        when Invalid_MMIO => "invalid-MMIO", when Unstable => "changing") &
      " reads=" & Natural'Image (Value.Reads) &
      " pass=" & Natural'Image (Value.Failed_Pass) &
      (if Value.Field_Known then " reg=" & Hex (Value.Offset) &
         " first=" & Hex (Value.First_Value) & " second=" & Hex (Value.Second_Value)
       else " reg=none"));
   function Capture (Owner : Boolean; Port : PHY) return Observation is
      Samples : array (1 .. 2) of Snapshot := (others => (others => 0));
      Result : Observation;
   begin
      if Active or else not Owner then return Result; end if;
      if not Power_Held then Result.Reason := Power_Lost; return Result; end if;
      Active := True; Result.Status := Read_Failed;
      for Pass in Samples'Range loop
         for F in Field loop
            if not Power_Held then
               Result.Reason := Power_Lost; Result.Failed_Pass := Pass;
               Active := False; return Result;
            end if;
            declare
               Value : Unsigned_32 with Import, Volatile_Full_Access,
                 Address => To_Address (16#60000000# + Integer_Address (Read_Offset (Port, F)));
               Copied : constant Unsigned_32 := Value;
            begin
               if Copied = Unsigned_32'Last then
                  Result.Reason := Invalid_MMIO; Result.Field_Known := True;
                  Result.Offset := Read_Offset (Port, F); Result.Failed_Pass := Pass;
                  Result.First_Value := (if Pass = 1 then Copied else Samples (1) (F));
                  Result.Second_Value := Copied;
                  Active := False; return Result;
               end if;
               Samples (Pass) (F) := Copied; Result.Reads := Result.Reads + 1;
            end;
         end loop;
      end loop;
      -- Keep the reentry guard through the last callback as well.
      if not Power_Held then
         Result.Reason := Power_Lost; Active := False; return Result;
      end if;
      Active := False;
      for F in Field loop
         if not Same_Configuration (F, Samples (1) (F), Samples (2) (F)) then
            Result.Status := Changing; Result.Reason := Unstable;
            Result.Field_Known := True; Result.Offset := Read_Offset (Port, F);
            Result.First_Value := Samples (1) (F); Result.Second_Value := Samples (2) (F);
            Result.Failed_Pass := 2;
            return Result;
         end if;
      end loop;
      Result.Status := Collected; Result.Values := Samples (2);
      return Result;
   end Capture;
end Intel_GPU_Native_Combo_State;
