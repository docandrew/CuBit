package body Region_Mock with SPARK_Mode is
   procedure Transact
     (Hardware : in out State;
      Space : ACPI_Region_Policy.Address_Space; Address : Unsigned_64;
      Width : ACPI_Region_Policy.Access_Width; For_Write : Boolean;
      Input : Unsigned_64; Output : out Unsigned_64; Completed : out Boolean) is
      pragma Unreferenced (Space, Width);
   begin
      if Hardware.Calls < Natural'Last then Hardware.Calls := Hardware.Calls + 1; end if;
      Hardware.Last_Address := Address; Hardware.Last_Input := Input;
      Hardware.Last_Write := For_Write;
      Output := Hardware.Next_Value; Completed := Hardware.Complete;
   end Transact;
end Region_Mock;
