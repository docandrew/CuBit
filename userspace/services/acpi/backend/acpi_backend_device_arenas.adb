pragma Ada_2022;
package body ACPI_Backend_Device_Arenas with SPARK_Mode is
   function Plan (Available : Window; Want_PCI, Want_Data : Boolean) return Selection is
      Result : Selection;
      Pages, PCI_Pages, Split : Word;
      Consumer_Partitions : constant Word := 2;
   begin
      if not Valid (Available) then return Result; end if;
      Result.Status := Ready;
      Pages := (Available.Limit - Available.First) / Page_Bytes;
      if Want_Data then Result.Has_Data := True; Result.Data := Available; end if;
      if Want_PCI then
         PCI_Pages := (if Want_Data then Pages / Consumer_Partitions else Pages);
         if PCI_Pages >= Minimum_PCI_Pages then
            Split := Available.First + PCI_Pages * Page_Bytes;
            Result.Has_PCI := True;
            Result.PCI := (Available.First, Split);
            if Want_Data then Result.Data.First := Split; end if;
         end if;
      end if;
      return Result;
   end Plan;
end ACPI_Backend_Device_Arenas;
