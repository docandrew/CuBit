pragma Ada_2022;
package body ACPI_Initializer_History with SPARK_Mode is
   Bits_Per_Byte : constant := 8;
   Octet_Base : constant Digest_Value := 2 ** Bits_Per_Byte;
   FNV_Prime : constant Digest_Value := 16#0000_0100_0000_01B3#;
   Position_Bytes : constant Positive := Node_Position'Size / Bits_Per_Byte;
   Outcome_Bytes : constant Positive := Outcome_Code'Size / Bits_Per_Byte;
   Charge_Bytes : constant Positive := Fuel_Charge'Size / Bits_Per_Byte;
   function Next_Digest (Previous : Digest_Value; Item : Invocation) return Digest_Value is
      Result : Digest_Value := Previous;
      procedure Mix (Value : Digest_Value; Bytes : Positive) is
         Remaining : Digest_Value := Value;
      begin
         for I in 1 .. Bytes loop
            Result := (Result xor (Remaining mod Octet_Base)) * FNV_Prime;
            Remaining := Remaining / Octet_Base;
         end loop;
      end Mix;
   begin
      Mix (Digest_Value (Item.Scope), Position_Bytes);
      Mix (Digest_Value (Item.Method), Position_Bytes);
      Mix (Digest_Value (Item.Access_Result), Outcome_Bytes);
      Mix (Digest_Value (Item.Execution), Outcome_Bytes);
      Mix (Digest_Value (Item.Reported_Charge), Charge_Bytes);
      return Result;
   end Next_Digest;
   procedure Invalidate (Run : in out State) is
   begin Run.All_Observed := False; end Invalidate;
   procedure Append (Run : in out State; Item : Invocation; Accepted : out Boolean) is
   begin
      Accepted := False;
      if not Run.All_Observed then return; end if;
      if Run.Used = Invocation_Count'Last then
         Run.All_Observed := False; return;
      end if;
      Run.Ordered := Next_Digest (Run.Ordered, Item);
      Run.Used := Run.Used + 1;
      Accepted := True;
   end Append;
end ACPI_Initializer_History;
