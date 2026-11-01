pragma Ada_2022;
package body ACPI_Backend_Firmware_Memory with SPARK_Mode is
   use type M.Memory_Kind, M.Cache_Mode;
   function Decode (Item : Kernel_Facts) return Report is
      Kind : M.Memory_Kind;
   begin
      if not Item.Current then return (Status => Not_Current); end if;
      if not Item.Memory_Handle then return (Status => Wrong_Handle); end if;
      if not Item.Rights.Readable or else not Item.Rights.Writable
        or else Item.Rights.Executable or else Item.Rights.Delegable or else Item.Rights.Revocable
      then return (Status => Wrong_Rights); end if;
      if not M.Firmware_Tag (Item.Tag) then return (Status => Not_Firmware); end if;
      if not M.Admits (Item.Tag, Item.First, Item.Bytes) then return (Status => Bad_Geometry); end if;
      Kind := M.Kind_Of (Item.Tag);
      return (Status => Admitted, Memory =>
        (Page_Number (Item.First / M.Page_Bytes), Page_Count (Item.Bytes / M.Page_Bytes),
         Kind, M.Cache_Of (Kind)));
   end Decode;
   function Check_Claim
     (Actual : Report; First_Page, Pages : Interfaces.Unsigned_64;
      Kind : M.Memory_Kind; Cache : M.Cache_Mode) return Report is
   begin
      if Actual.Status /= Admitted then return Actual; end if;
      if First_Page /= Actual.Memory.First_Page or else Pages /= Actual.Memory.Pages
        or else Kind /= Actual.Memory.Kind or else Cache /= Actual.Memory.Cache
      then return (Status => Claim_Mismatch); end if;
      return Actual;
   end Check_Claim;
   function Allows_Named_Reads (Item : Accepted_Memory) return Boolean is
     (Item.Kind = M.ACPI_NVS and then Item.Cache = M.Write_Back);
   function Control_Conflict
     (Item : Accepted_Memory; Address, Length : Interfaces.Unsigned_64) return Boolean
   is
      First : constant Interfaces.Unsigned_64 := Item.First_Page * M.Page_Bytes;
      Bytes : constant Interfaces.Unsigned_64 := Item.Pages * M.Page_Bytes;
   begin
      if Item.Kind /= M.Firmware_Control or else Length = 0 then return False; end if;
      if Length - 1 > Interfaces.Unsigned_64'Last - Address then return True; end if;
      return (if Address >= First then Address - First < Bytes else First - Address < Length);
   end Control_Conflict;
   function Coherent_For_FACS (Item : Accepted_Memory) return Boolean is
     (Item.Cache = M.Write_Back);
end ACPI_Backend_Firmware_Memory;
