------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The ACPI hardware grant from this process's startup table
--  (CuBit.ACPI_Hardware_Authority says how the launcher records it).
--
--  @description
--  Named reads the declared entry and then the window entries
--  IO_Label (Label, 1), IO_Label (Label, 2), ... in order, and asks the
--  kernel what each handle is (INSPECT_CAPABILITY): a window's First and
--  Count are the range its own IO_Ports capability covers, never a number
--  the launcher wrote beside it. The grant is all or nothing: a missing
--  declared entry, a window handle that is not a current I/O-port
--  capability with read and write, or windows out of order or
--  overlapping give the empty Grant (nothing admitted). Then the memory
--  windows Memory_Label (Label, 1), ... likewise: each must be a current
--  firmware-memory capability (a kernel firmware-memory tag,
--  CuBit.ACPI_Memory_Classes) with read and write, covering whole pages;
--  First_Page and Pages are what the kernel says it covers, and Kind and
--  Cache what its kernel tag says (an unknown tag empties the grant). A
--  grant with no window of either kind is empty too; a memory-only grant
--  (a hardware-reduced platform) is admitted
--  (CuBit.ACPI_Hardware_Authority.Declared_Holds).
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Startup_State;
with CuBit.ACPI_Hardware_Authority;
with CuBit.ACPI_Memory_Classes;

package CuBit.ACPI_Windows.Launch with SPARK_Mode is
   use type Interfaces.Unsigned_32;

   pragma Compile_Time_Error
     (CuBit.ACPI_Hardware_Authority.Maximum_IO_Windows /= Maximum_IO_Windows,
      "the launcher's window labels must cover the grant's I/O windows");
   pragma Compile_Time_Error
     (CuBit.ACPI_Hardware_Authority.Maximum_Memory_Windows /= Maximum_Memory_Windows,
      "the launcher's window labels must cover the grant's memory windows");

   --  A physical page frame below the kernel's physical limit (2 ** 47).
   Page_Bytes : constant := CuBit.ACPI_Memory_Classes.Page_Bytes;
   Page_Limit : constant := CuBit.ACPI_Memory_Classes.Page_Limit;

   --  The grant the startup table holds under Label (the generated Startup
   --  package calls this; programs use Startup.<Binding>).
   function Named (Label : String) return Grant
   with Global => CuBit.Startup_State.Loaded_Table,
        Pre    => Label'Length <= CuBit.ACPI_Hardware_Authority.Maximum_Binding_Length;

   --  Sorted by first port and pairwise disjoint (each window ends before
   --  the next begins); the slots past IO_Used hold no handle.
   function Well_Formed (Item : Grant) return Boolean is
     ((for all K in 2 .. Item.IO_Used =>
         Interfaces.Unsigned_32 (Item.IO (K - 1).First) + Item.IO (K - 1).Count
           <= Interfaces.Unsigned_32 (Item.IO (K).First))
      and then (for all K in 1 .. Item.IO_Used =>
                  Interfaces.Unsigned_32 (Item.IO (K).First) + Item.IO (K).Count <= Port_Count'Last)
      --  Memory windows likewise, by page, below the physical limit.
      and then (for all K in 1 .. Item.Memory_Used =>
                  Item.Memory (K).First_Page < Page_Limit
                  and then Item.Memory (K).Pages <= Page_Limit - Item.Memory (K).First_Page)
      and then (for all K in 2 .. Item.Memory_Used =>
                  Item.Memory (K - 1).First_Page < Page_Limit
                  and then Item.Memory (K - 1).Pages <= Page_Limit - Item.Memory (K - 1).First_Page
                  and then Item.Memory (K - 1).First_Page + Item.Memory (K - 1).Pages
                             <= Item.Memory (K).First_Page));

end CuBit.ACPI_Windows.Launch;
