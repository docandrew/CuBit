------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.Disk_Install with SPARK_Mode is

   procedure Decode
     (Words : Request_Words; Item : out Open_Request; Valid : out Boolean) is
   begin
      Item := (others => <>);
      Valid := False;
      if Words (Ordinal_Word) > Unsigned_64 (Disk_Ordinal'Last) then
         return;
      end if;
      Item.Disk := Disk_Ordinal (Words (Ordinal_Word));
      if Words (Access_Word) = Unsigned_64 (Disk_Access'Enum_Rep (Read_Only)) then
         Item.Mode := Read_Only;
      elsif Words (Access_Word) = Unsigned_64 (Disk_Access'Enum_Rep (Read_Write)) then
         Item.Mode := Read_Write;
      else
         return;
      end if;
      Valid := True;
   end Decode;

end CuBit.Disk_Install;
