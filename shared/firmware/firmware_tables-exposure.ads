pragma Ada_2022;
with Firmware_Tables.Catalog; use Firmware_Tables.Catalog;

-- Content check only. A retained candidate still needs backing/cache/lifetime
-- approval by the mapping adapter. Gaps are never scrubbed in firmware memory.
package Firmware_Tables.Exposure with SPARK_Mode, Pure is
   use type Address_Value;
   Page_Size : constant := 4096;
   subtype Page_Offset is Address_Value range 0 .. Page_Size - 1;
   -- Largest representable table, including a worst-case unaligned start.
   subtype Page_Count is Address_Value range 1 ..
     (Address_Value (Table_Length'Last) + 2 * (Page_Size - 1)) / Page_Size;
   type Window is record
      First, Last : Address_Value;
      Offset : Page_Offset;
      Pages : Page_Count;
   end record;
   function Page_Window (D : Descriptor) return Window
     with Pre => Fits (D),
       Post => Page_Window'Result.First mod Page_Size = 0
         and then Page_Window'Result.Last mod Page_Size = Page_Size - 1
         and then Page_Window'Result.First <= D.Physical
         and then D.Physical - Page_Window'Result.First =
           Page_Window'Result.Offset
         and then Page_Window'Result.Last >= D.Physical
         and then Page_Window'Result.Last - D.Physical >=
           Address_Value (D.Extent - 1)
         and then Page_Window'Result.Last - Page_Window'Result.First =
           Page_Window'Result.Pages * Page_Size - 1;

   function Known_Byte (S : State; At_Address : Address_Value) return Boolean
     with Ghost;
   function Known_Range (S : State; First, Last : Address_Value) return Boolean
     with Ghost, Pre => First <= Last;
   type Disposition is (Copy_Required, Retained_Candidate);
   type Plan is record
      Kind : Disposition;
      Span : Window;
   end record;
   function Describe (S : State; Index : Positive) return Plan
     with Pre => Index <= Count (S),
       Post => Describe'Result.Span = Page_Window (Item (S, Index))
         and then (if Describe'Result.Kind = Retained_Candidate then
           Known_Range (S, Describe'Result.Span.First,
                        Describe'Result.Span.Last));
end Firmware_Tables.Exposure;
