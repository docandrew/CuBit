------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The manifest request for a kernel publication page (KERN-002):
--  (request-publication NAME). A request, never a grant: procmgr grants a
--  declared one by passing its bit to SYSCALL_INSTALL_LAUNCH_ARGUMENTS, and
--  the child's startup table then holds the page's address.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System_Publications;
with CPU_Times_Publication;

package CuBit.Publication_Grants with Pure, SPARK_Mode is
   package SP renames System_Publications;

   --  The .cubit.caps request kind (beside Network_Authority's 10 ..
   --  ACPI_Hardware_Authority's 13).
   Manifest_Request : constant Unsigned_8 := 14;

   --  A request's Param0: the publication (Granted_Publication'Pos + 1;
   --  0 is no publication).
   function Code (P : SP.Granted_Publication) return Unsigned_32 is
     (Unsigned_32 (SP.Granted_Publication'Pos (P)) + 1);
   function Known (Code : Unsigned_32) return Boolean is
     (Code in 1 .. Unsigned_32 (SP.Granted_Publication'Pos (SP.Granted_Publication'Last)) + 1);
   function Publication_Of (Code : Unsigned_32) return SP.Granted_Publication is
     (SP.Granted_Publication'Val (Code - 1))
   with Pre => Known (Code);

   --  The manifest's name for each, and where the kernel maps it.
   CPU_Times_Name : constant String := "cpu-times";
   function Name (P : SP.Granted_Publication) return String is
     (case P is when SP.CPU_Times => CPU_Times_Name);
   function Page_Address (P : SP.Granted_Publication) return Unsigned_64 is
     (case P is when SP.CPU_Times => CPU_Times_Publication.Page_Address);
end CuBit.Publication_Grants;
