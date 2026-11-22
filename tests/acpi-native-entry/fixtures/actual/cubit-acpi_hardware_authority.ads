pragma Ada_2022;
with Interfaces;
with CuBit.Startup_Tables;

--  The manifest request for the ACPI hardware windows (Request.ACPI_Hardware,
--  coordination/acpi-region-boundary.md): a request, never a grant. procmgr
--  grants it only when the program's startup entry approves it,
--  (acpi-hardware approve-declared), and only to a program the trusted
--  startup plan started.
--
--  How the grant reaches the program: the declared entry (kind
--  ACPI_Hardware) marks it; each I/O window is its own IO_Ports startup
--  entry, which procmgr appends after the declared ones and names
--  IO_Label (Binding, K), K = 1 .. the window count, in ascending port
--  order. A grant is never empty (Declared_Holds): with neither an I/O nor
--  a memory window procmgr refuses the launch. The declared entry holds the
--  first I/O window's handle, or, with none (a hardware-reduced platform has
--  no FADT fixed I/O blocks), the first memory window's.
--  Each firmware-memory window (slice 2: an ACPI NVS run or the FACS page;
--  ACPI-PCI-001: PCI segment 0 bus 0's ECAM slice, read-only, only with
--  (acpi-pci-config approve-declared) too) is its own Device_Memory entry
--  after the I/O windows, named Memory_Label (Binding, K), K counting only
--  the windows installed, in ascending address order.
--  CuBit.ACPI_Windows.Launch reads them back as a CuBit.ACPI_Windows.Grant.
package CuBit.ACPI_Hardware_Authority with Pure, SPARK_Mode is

   Manifest_Request : constant Interfaces.Unsigned_8 := 13;

   --  CuBit.ACPI_Windows.Maximum_IO_Windows (checked where both are seen).
   Maximum_IO_Windows : constant := 16;
   subtype IO_Window_Number is Positive range 1 .. Maximum_IO_Windows;

   --  CuBit.ACPI_Windows.Maximum_Memory_Windows (checked likewise).
   Maximum_Memory_Windows : constant := 16;
   subtype Memory_Window_Number is Positive range 1 .. Maximum_Memory_Windows;

   IO_Infix     : constant String := "-io-";
   Memory_Infix : constant String := "-mem-";
   --  The longest suffix a label adds: the longer infix and two digits.
   Maximum_Suffix : constant := Memory_Infix'Length + 2;
   pragma Compile_Time_Error (IO_Infix'Length > Memory_Infix'Length,
                              "Maximum_Suffix must cover both infixes");
   --  The longest binding whose window labels are still startup names.
   Maximum_Binding_Length : constant :=
     CuBit.Startup_Tables.Maximum_Name_Bytes - Maximum_Suffix;

   subtype IO_Window_Count     is Natural range 0 .. Maximum_IO_Windows;
   subtype Memory_Window_Count is Natural range 0 .. Maximum_Memory_Windows;

   --  Which window's handle the declared entry holds: a grant is admitted
   --  exactly when it has at least one window of either kind.
   type Declared_Source is (No_Window, First_IO_Window, First_Memory_Window);
   function Declared_Holds
     (IO_Windows : IO_Window_Count; Memory_Windows : Memory_Window_Count)
      return Declared_Source
   is (if IO_Windows > 0 then First_IO_Window
       elsif Memory_Windows > 0 then First_Memory_Window
       else No_Window)
   with Post => (Declared_Holds'Result = No_Window) = (IO_Windows + Memory_Windows = 0);

   --  Two decimal digits at most.
   subtype Label_Number is Positive range 1 .. 99;
   pragma Compile_Time_Error
     (Maximum_IO_Windows > Label_Number'Last or else Maximum_Memory_Windows > Label_Number'Last,
      "window numbers must fit two digits");

   function Digits_Of (K : Label_Number) return String is
     (if K < 10 then [Character'Val (Character'Pos ('0') + K)]
      else [Character'Val (Character'Pos ('0') + K / 10),
            Character'Val (Character'Pos ('0') + K mod 10)]);

   --  "hardware-io-3": window K's startup name.
   function IO_Label (Binding : String; K : IO_Window_Number) return String is
     (Binding & IO_Infix & Digits_Of (K))
   with Pre => Binding'Length <= Maximum_Binding_Length
               and then Binding'Last <= Natural'Last - Maximum_Suffix;

   --  "hardware-mem-2": memory window K's startup name.
   function Memory_Label (Binding : String; K : Memory_Window_Number) return String is
     (Binding & Memory_Infix & Digits_Of (K))
   with Pre => Binding'Length <= Maximum_Binding_Length
               and then Binding'Last <= Natural'Last - Maximum_Suffix;

end CuBit.ACPI_Hardware_Authority;
