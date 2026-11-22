pragma Ada_2022;
with Interfaces;
with CuBit.ACPI_Memory_Classes;
-- Pure classification of kernel inspection facts. This does not create a
-- capability, attest a firmware map or authorize an IPC-supplied address.
package ACPI_Backend_Firmware_Memory with Pure, SPARK_Mode is
   package M renames CuBit.ACPI_Memory_Classes;
   use type Interfaces.Unsigned_64, M.Memory_Kind, M.Cache_Mode;
   type Kernel_Rights is record
      Readable, Writable, Executable, Delegable, Revocable : Boolean := False;
   end record;
   type Kernel_Facts is record
      Current, Memory_Handle : Boolean := False;
      Rights : Kernel_Rights;
      Tag, First, Bytes : Interfaces.Unsigned_64 := 0;
   end record;
   subtype Page_Number is Interfaces.Unsigned_64 range 0 .. M.Page_Limit - 1;
   subtype Page_Count is Interfaces.Unsigned_64 range 1 .. M.Page_Limit;
   type Accepted_Memory is record
      First_Page : Page_Number := 0;
      Pages : Page_Count := 1;
      Kind : M.Memory_Kind := M.ACPI_NVS;
      Cache : M.Cache_Mode := M.Uncached;
   end record;
   type Outcome is (Admitted, Not_Current, Wrong_Handle, Wrong_Rights,
     Not_Firmware, Bad_Geometry, Claim_Mismatch);
   type Report (Status : Outcome := Not_Current) is record
      case Status is
         when Admitted => Memory : Accepted_Memory;
         when others => null;
      end case;
   end record;
   function Decode (Item : Kernel_Facts) return Report
     with Post => (if Decode'Result.Status = Admitted then
       Item.Current and then Item.Memory_Handle and then Item.Rights.Readable
       and then not Item.Rights.Executable and then not Item.Rights.Delegable
       and then not Item.Rights.Revocable and then M.Firmware_Tag (Item.Tag)
       and then Decode'Result.Memory.Kind = M.Kind_Of (Item.Tag)
       and then Decode'Result.Memory.Cache = M.Cache_Of (M.Kind_Of (Item.Tag))
       and then Item.Rights.Writable = M.Writable_Of (M.Kind_Of (Item.Tag))
       and then Decode'Result.Memory.First_Page * M.Page_Bytes = Item.First
       and then Decode'Result.Memory.Pages * M.Page_Bytes = Item.Bytes
       and then Decode'Result.Memory.Pages <= M.Page_Limit - Decode'Result.Memory.First_Page
       and then (if M.Kind_Of (Item.Tag) = M.PCI_Config then Item.Bytes = M.PCI_Bus_Bytes));
   function Check_Claim
     (Actual : Report; First_Page, Pages : Interfaces.Unsigned_64;
      Kind : M.Memory_Kind; Cache : M.Cache_Mode) return Report
     with Post => (if Check_Claim'Result.Status = Admitted then
       Actual.Status = Admitted and then Check_Claim'Result.Memory = Actual.Memory
       and then First_Page = Actual.Memory.First_Page and then Pages = Actual.Memory.Pages
       and then Kind = Actual.Memory.Kind and then Cache = Actual.Memory.Cache);
   -- Classification only. A live retained handle/view and named record still
   -- authorize each effect; control/PCI windows never supply generic fields.
   function Allows_Named_Reads (Item : Accepted_Memory) return Boolean
     with Post => (if Allows_Named_Reads'Result then Item.Kind = M.ACPI_NVS and then Item.Cache = M.Write_Back);
   -- PCI reads require their separate function-relative planner/provider.
   function Allows_PCI_Reads (Item : Accepted_Memory) return Boolean
     with Post => (if Allows_PCI_Reads'Result then Item.Kind = M.PCI_Config
       and then Item.Cache = M.Uncached and then Item.Pages = M.PCI_Bus_Bytes / M.Page_Bytes
       and then Item.Pages <= M.Page_Limit - Item.First_Page);
   -- Reject an ordinary named record intersecting control or PCI pages; no
   -- address+length addition is used, and wrapped spans conflict too.
   function Generic_Field_Conflict
     (Item : Accepted_Memory; Address, Length : Interfaces.Unsigned_64) return Boolean
     with Post =>
       (if Item.Kind = M.ACPI_NVS or else Length = 0 then not Generic_Field_Conflict'Result
        elsif Length - 1 > Interfaces.Unsigned_64'Last - Address then Generic_Field_Conflict'Result
        else Generic_Field_Conflict'Result =
          ((Address <= Item.First_Page * M.Page_Bytes
            and then Item.First_Page * M.Page_Bytes - Address < Length)
           or else (Item.First_Page * M.Page_Bytes <= Address
            and then Address - Item.First_Page * M.Page_Bytes < Item.Pages * M.Page_Bytes)));
   function Coherent_For_FACS (Item : Accepted_Memory) return Boolean
     with Post => (if Coherent_For_FACS'Result then Item.Kind /= M.PCI_Config and then Item.Cache = M.Write_Back);
end ACPI_Backend_Firmware_Memory;
