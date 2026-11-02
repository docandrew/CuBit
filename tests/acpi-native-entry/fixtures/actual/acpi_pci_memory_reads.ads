pragma Ada_2022;
with ACPI_PCI_Config_Reads;
-- Geometry conversion only. Window bounds must come from the current named
-- backend PCI record. Neither an AML address nor this result grants authority.
-- Requests still name that record and carry only PCI function-relative data.
package ACPI_PCI_Memory_Reads with Pure, SPARK_Mode is
   package P renames ACPI_PCI_Config_Reads;
   use type P.Outcome, P.Word;
   type Translation (Status : P.Outcome := P.Invalid_Window) is record
      case Status is
         when P.Read_Ready =>
            Region : P.Region;
            Offset : P.Word;
            First : P.ECAM_Offset;
         when others => null;
      end case;
   end record;
   function Plan
     (Window_First, Window_Bytes, Region_First, Region_Bytes, Offset : P.Word;
      Width : P.T.Access_Width; For_Write : Boolean := False) return Translation
     with Global => null,
       Post => (if Plan'Result.Status = P.Read_Ready then
         not For_Write and then Window_Bytes = P.Bus_Bytes
         and then Region_Bytes /= 0 and then Region_First >= Window_First
         and then Region_First - Window_First < Window_Bytes
         and then Region_Bytes <= Window_Bytes - (Region_First - Window_First)
         and then Offset < Region_Bytes
         and then P.Word (Plan'Result.First) = Region_First - Window_First + Offset
         and then P.Plan (Plan'Result.Region, Plan'Result.Offset, Width, Window_Bytes).Status = P.Read_Ready);
end ACPI_PCI_Memory_Reads;
