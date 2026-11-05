pragma Ada_2022;
package body Firmware_Tables.Copies with SPARK_Mode is
   function Matches (Expected : Catalog.Descriptor; Data : Bytes) return Boolean is
      Header : Table_Result;
   begin
      if not Catalog.Fits (Expected) or else Expected.Name = "FACS"
        or else Data'Length /= Expected.Extent
      then return False; end if;
      Header := Read_Table (Data, Expected.Name);
      return Header.Status = Accepted and then Header.Extent = Expected.Extent
        and then Header.Revision = Expected.Revision;
   end Matches;
   procedure Copy_Validated
     (Expected : Catalog.Descriptor; Source : Bytes;
      Destination : out Bytes; Success : out Boolean) is
   begin
      Destination := [others => 0];
      Success := False;
      if Destination'Length < Expected.Extent or else
        Source'Length /= Expected.Extent
      then return; end if;
      Destination (Destination'First .. Destination'First + (Expected.Extent - 1)) := Source;
      Success := Matches (Expected,
        Destination (Destination'First .. Destination'First + (Expected.Extent - 1)));
      if not Success then Destination := [others => 0]; end if;
   end Copy_Validated;
end Firmware_Tables.Copies;
