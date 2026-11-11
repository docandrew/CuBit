pragma Ada_2022;
with ACPI_PCI_Config_Reads;
-- Pure arithmetic for one described ECAM bus. This descriptor is metadata,
-- not authority: the backend must independently authenticate its named record,
-- retained handle, PCI classification, cache/rights and lifetime before I/O.
-- No physical mapping, register operation or capability is performed here.
package ACPI_PCI_Bus_Geometry with Pure, SPARK_Mode is
   package P renames ACPI_PCI_Config_Reads;
   use type P.Word, P.T.Access_Width;
   use type P.Segment_Number, P.Bus_Number;
   type Bus_Window is record
      Segment : P.Segment_Number := 0;
      Bus : P.Bus_Number := 0;
      First : P.Word := 0;
      Bytes : P.Word := 0;
   end record;
   function Valid (Window : Bus_Window) return Boolean is
     (Window.Bytes = P.Bus_Bytes
      and then Window.First mod P.Function_Bytes = 0
      and then Window.Bytes - 1 <= P.Word'Last - Window.First);
   type Outcome is (Read_Ready, Invalid_Window, Write_Refused,
      Wrong_Segment, Wrong_Bus, Unsupported_Width, Invalid_Region,
      Outside_Window, Outside_Region, Misaligned);
   type View (Status : Outcome := Invalid_Window) is record
      case Status is
         when Read_Ready =>
            First : P.ECAM_Offset;
            Bytes : P.Read_Bytes;
            Width : P.T.Access_Width;
         when others => null;
      end case;
   end record;
   function Plan_Config
     (Window : Bus_Window; Item : P.Region; Offset : P.Word;
      Width : P.T.Access_Width; For_Write : Boolean := False) return View
     with Global => null,
       Post => (if Plan_Config'Result.Status = Read_Ready then
         Valid (Window) and then not For_Write
         and then Item.Device.Segment = Window.Segment
         and then Item.Device.Bus = Window.Bus
         and then P.Valid_Region (Item)
         and then Width /= P.T.Qword_Access
         and then Plan_Config'Result.Width = Width
         and then Plan_Config'Result.Bytes = P.T.Octets (Width)
         and then Offset < Item.Length
         and then P.Word (P.T.Octets (Width)) <= Item.Length - Offset
         and then P.Word (Plan_Config'Result.First) =
           (P.Word (Item.Device.Device) * P.Functions_Per_Device + P.Word (Item.Device.Func))
             * P.Function_Bytes + Item.Base + Offset
         and then P.Word (Plan_Config'Result.First) <= Window.Bytes - P.Word (Plan_Config'Result.Bytes));
   type Translation (Status : Outcome := Invalid_Window) is record
      case Status is
         when Read_Ready =>
            Region : P.Region;
            Offset : P.Word;
            First : P.ECAM_Offset;
         when others => null;
      end case;
   end record;
   function Plan_Alias
     (Window : Bus_Window; Region_First, Region_Bytes, Offset : P.Word;
      Width : P.T.Access_Width; For_Write : Boolean := False) return Translation
     with Global => null,
       Post => (if Plan_Alias'Result.Status = Read_Ready then
         Valid (Window) and then not For_Write
         and then Region_First >= Window.First
         and then Region_First - Window.First < Window.Bytes
         and then Region_Bytes <= Window.Bytes - (Region_First - Window.First)
         and then Offset < Region_Bytes
         and then P.Word (Plan_Alias'Result.First) = Region_First - Window.First + Offset
         and then Plan_Config (Window, Plan_Alias'Result.Region,
           Plan_Alias'Result.Offset, Width).Status = Read_Ready);
end ACPI_PCI_Bus_Geometry;
