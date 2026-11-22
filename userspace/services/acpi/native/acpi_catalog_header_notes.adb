pragma Ada_2022;
package body ACPI_Catalog_Header_Notes with SPARK_Mode is
   procedure Format (Item : Header; Text : out Line_Text;
      Last : out Line_Length; Status : out Format_Status) is
      Position : Line_Length := 0;
      Overflowed : Boolean := False;
      Escape : constant Character := Character'Val (16#5C#);
      Hex : constant String := "0123456789ABCDEF";
      Hex_Radix : constant Positive := 16;
      First_Printable : constant Natural := 16#20#;
      Last_Printable : constant Natural := 16#7E#;
      procedure Put (C : Character) is
      begin
         if Position < Line_Capacity then
            Position := Position + 1; Text (Position) := C;
         else Overflowed := True; end if;
      end Put;
      procedure Append (S : String) is
      begin
         for C of S loop Put (C); end loop;
      end Append;
      procedure Quoted (S : String) is
      begin
         Put ('"');
         for C of S loop
            if Character'Pos (C) not in First_Printable .. Last_Printable then
               Put (Escape); Put ('x');
               Put (Hex (Character'Pos (C) / Hex_Radix + Hex'First));
               Put (Hex (Character'Pos (C) mod Hex_Radix + Hex'First));
            else
               if C = '"' or else C = Escape then Put (Escape); end if;
               Put (C);
            end if;
         end loop;
         Put ('"');
      end Quoted;
   begin
      Text := [others => ' '];
      Append ("acpi: stage table-header ok index="); Append (Item.Index'Image);
      Append (" id="); Append (Item.ID'Image);
      Append (" sig="); Quoted (Item.Identity.Name);
      Append (" bytes="); Append (Item.Length'Image);
      Append (" rev="); Append (Item.Revision'Image);
      Append (" oem="); Quoted (Item.Identity.OEM);
      Append (" oem-table="); Quoted (Item.Identity.OEM_Table);
      Append (if Item.Checksum_Admitted then " checksum=OK" else " checksum=UNVERIFIED");
      Last := Position;
      Status := (if Overflowed then Format_Limit else Formatted);
   end Format;
end ACPI_Catalog_Header_Notes;
