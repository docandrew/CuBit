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
--  order. The declared entry holds the first window's handle (a grant is
--  never empty: with no admissible window procmgr refuses the launch).
--  CuBit.ACPI_Windows.Launch reads them back as a CuBit.ACPI_Windows.Grant.
package CuBit.ACPI_Hardware_Authority with Pure, SPARK_Mode is

   Manifest_Request : constant Interfaces.Unsigned_8 := 13;

   --  CuBit.ACPI_Windows.Maximum_IO_Windows (checked where both are seen).
   Maximum_IO_Windows : constant := 16;
   subtype IO_Window_Number is Positive range 1 .. Maximum_IO_Windows;

   IO_Infix : constant String := "-io-";
   --  The longest suffix IO_Label adds: the infix and two digits.
   Maximum_Suffix : constant := IO_Infix'Length + 2;
   --  The longest binding whose window labels are still startup names.
   Maximum_Binding_Length : constant :=
     CuBit.Startup_Tables.Maximum_Name_Bytes - Maximum_Suffix;

   function Digits_Of (K : IO_Window_Number) return String is
     (if K < 10 then [Character'Val (Character'Pos ('0') + K)]
      else [Character'Val (Character'Pos ('0') + K / 10),
            Character'Val (Character'Pos ('0') + K mod 10)]);

   --  "hardware-io-3": window K's startup name.
   function IO_Label (Binding : String; K : IO_Window_Number) return String is
     (Binding & IO_Infix & Digits_Of (K))
   with Pre => Binding'Length <= Maximum_Binding_Length;

end CuBit.ACPI_Hardware_Authority;
