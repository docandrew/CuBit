pragma Ada_2022;
package body ACPI_FADT.Registers with SPARK_Mode is
   function Describe (Item : Description; Kind : Block_Kind;
                      Last_Memory_Address : Unsigned_64;
                      Last_IO_Port : Unsigned_16 := Unsigned_16'Last) return Block_Result is
      Extended : constant Optional_Address := Item.Extended (Kind);
      Has_Extended : constant Boolean := Extended.Present and then Extended.Value.Address /= 0;
      Length : Block_Length;
      Limit : Unsigned_64;
      Space : Space_Kind;
   begin
      if ACPI_FADT.Hardware_Reduced (Item) then return (Code => Hardware_Reduced); end if;
      if not Has_Extended and then Item.Legacy (Kind) = 0 then return (Code => Absent); end if;
      if not Valid_Length (Kind, Item.Lengths (Kind)) then return (Code => Bad_Length); end if;
      Length := Natural (Item.Lengths (Kind));
      if Has_Extended and then Extended.Value.Space in 0 | 1 then
         Space := (if Extended.Value.Space = 0 then System_Memory else System_IO);
         Limit := (if Space = System_Memory then Last_Memory_Address else Unsigned_64 (Last_IO_Port));
         if Fits (Extended.Value.Address, Length, Limit) then
            return (Described, Kind, Space, Extended.Value.Address, Length, Extended_Address, False);
         end if;
      end if;
      if Fits (Unsigned_64 (Item.Legacy (Kind)), Length, Unsigned_64 (Last_IO_Port)) then
         return (Described, Kind, System_IO, Unsigned_64 (Item.Legacy (Kind)), Length, Legacy_Address, Has_Extended);
      end if;
      return (Code => No_Usable_Address);
   end Describe;
   function Enable_Base (Block : Block_Result) return Unsigned_64 is
     (Block.Base + Unsigned_64 (Block.Length / 2));
end ACPI_FADT.Registers;
