with Interfaces; use Interfaces;
with ACPI_Region_Policy;
package Region_Mock with SPARK_Mode is
   type State is record
      Calls : Natural := 0;
      Complete : Boolean := True;
      Next_Value : Unsigned_64 := 0;
      Last_Address, Last_Input : Unsigned_64 := 0;
      Last_Write : Boolean := False;
   end record;
   procedure Transact
     (Hardware : in out State;
      Space : ACPI_Region_Policy.Address_Space; Address : Unsigned_64;
      Width : ACPI_Region_Policy.Access_Width; For_Write : Boolean;
      Input : Unsigned_64; Output : out Unsigned_64; Completed : out Boolean)
     with Global => null;
end Region_Mock;
