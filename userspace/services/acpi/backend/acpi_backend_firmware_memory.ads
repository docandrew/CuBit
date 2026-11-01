pragma Ada_2022;
with Interfaces;
with CuBit.ACPI_Memory_Classes;
-- Pure classification of kernel inspection facts. This does not create a
-- capability, attest a firmware map or authorize an IPC-supplied address.
package ACPI_Backend_Firmware_Memory with Pure, SPARK_Mode is
   package M renames CuBit.ACPI_Memory_Classes;
   use type Interfaces.Unsigned_64;
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
   function Decode (Item : Kernel_Facts) return Report;
   function Check_Claim
     (Actual : Report; First_Page, Pages : Interfaces.Unsigned_64;
      Kind : M.Memory_Kind; Cache : M.Cache_Mode) return Report;
   -- Classification only. A live retained handle/view and named record still
   -- authorize each effect; Firmware_Control never supplies a generic field.
   function Allows_Named_Reads (Item : Accepted_Memory) return Boolean;
   -- Reject an ordinary named record intersecting a FACS-only page; no
   -- address+length addition is used, and wrapped spans conflict too.
   function Control_Conflict
     (Item : Accepted_Memory; Address, Length : Interfaces.Unsigned_64) return Boolean;
   function Coherent_For_FACS (Item : Accepted_Memory) return Boolean;
end ACPI_Backend_Firmware_Memory;
