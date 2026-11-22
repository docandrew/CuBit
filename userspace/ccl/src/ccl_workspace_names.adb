package body CCL_Workspace_Names with SPARK_Mode => On is
   function Filename (Number : Saved_Revision; Pending : Boolean) return String is
      Digits_Text : String (1 .. 4);
      Remaining : Revision := Number;
   begin
      for Index in reverse Digits_Text'Range loop
         Digits_Text (Index) := Character'Val (Character'Pos ('0') + Remaining mod 10);
         Remaining := Remaining / 10;
      end loop;
      return "ccl-" & Digits_Text & (if Pending then ".pending" else ".ccl");
   end Filename;

   procedure Decode
     (Name : String; Number : out Revision; Pending : out Boolean)
   is
      Value : Revision := 0;
      Decimal_Limits : constant array (Natural range 0 .. 3) of Positive :=
        [1, 10, 100, 1_000];
   begin
      Number := 0;
      Pending := False;
      if Name'Length not in 12 | 16 then
         return;
      end if;
      if Name (Name'First .. Name'First + 3) /= "ccl-" then
         return;
      end if;
      for Index in 0 .. 3 loop
         pragma Loop_Invariant (Value < Decimal_Limits (Index));
         declare
            C : constant Character := Name (Name'First + 4 + Index);
         begin
            if C not in '0' .. '9' then
               return;
            end if;
            Value := Value * 10 + Character'Pos (C) - Character'Pos ('0');
         end;
      end loop;
      if Name (Name'First + 8 .. Name'Last) = ".pending" then
         Pending := True;
      elsif Name (Name'First + 8 .. Name'Last) /= ".ccl" then
         return;
      end if;
      Number := Value;
   end Decode;
end CCL_Workspace_Names;
