pragma Ada_2022;
package body Reader_Proof with SPARK_Mode => Off is
   procedure Ask
     (Request : Power_ACPI_Facts.Words; Label : out Unsigned_32; Reply : out Power_ACPI_Facts.Words;
      Delivered : out Boolean) is
   begin
      Label := 0;
      Reply := [others => Request (0)];
      Delivered := False;
   end Ask;
end Reader_Proof;
