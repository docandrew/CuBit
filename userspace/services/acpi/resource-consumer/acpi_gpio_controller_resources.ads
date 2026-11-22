pragma Ada_2022;
with Firmware_Tables;
with ACPI_Resource_Data;
with ACPI_Resource_Data.Extended_Interrupts;
-- Copied _CRS descriptions for the existing four-resource GPIO driver shape.
-- Never a mapping, pool admission, pin capability, resolved IRQ or device ID.
package ACPI_GPIO_Controller_Resources with SPARK_Mode, Pure is
   package R renames ACPI_Resource_Data;
   package IRQ renames R.Extended_Interrupts;
   Community_Resources:constant Positive:=4;
   subtype Resource_Slot is Positive range 1..Community_Resources;
   type Offset is range 0..2**32;
   Address_Space_Limit:constant Offset:=2**32;
   subtype Address is Offset range 0..Address_Space_Limit-1;
   type Memory_Window is record
      Base,Bytes:Address:=0;
      Declared_Writeable:Boolean:=False;
      Raw_Information:Firmware_Tables.Byte:=0;
   end record;
   type Windows is array(Resource_Slot) of Memory_Window;
   type Interrupt_Description is record
      Present:Boolean:=False;
      Number:IRQ.Interrupt_Number:=0;
      Mode:IRQ.Interrupt_Mode:=IRQ.Level_Triggered;
      Polarity:IRQ.Interrupt_Polarity:=IRQ.Active_High;
      Role:IRQ.Resource_Role:=IRQ.Consumer;
      Shared,Wake_Capable:Boolean:=False;
      Source:IRQ.Source_Field:=(Present=>False);
   end record;
   type Description is record
      Memory:Windows:=[others=>(others=><>)];
      Interrupt:Interrupt_Description:=(others=><>);
   end record;
   Empty:constant Description:=(others=><>);
   function Valid_Span(Item:Memory_Window) return Boolean is
     (Item.Bytes>0 and then Item.Bytes<=Address_Space_Limit-Item.Base);
   function Overlap(Left,Right:Memory_Window) return Boolean
     with Pre=>Valid_Span(Left) and then Valid_Span(Right);
   function Valid(Item:Description) return Boolean;
   type Outcome is (Described,Invalid_Template,Unsupported_Descriptor,
      Invalid_Descriptor,Wrong_Memory_Count,Wrong_Interrupt_Count,
      Invalid_Memory,Overlapping_Memory);
   type Report is record
      Status:Outcome:=Invalid_Template;
      At_Offset:R.Byte_Offset:=0;
      Value:Description:=Empty;
   end record;
   -- Full template framing/checksum validation before decoding. Preserve the
   -- four memory descriptors in ORIGINAL order: do not sort or infer physical
   -- bases/community identities. Reject partial/overlapping/wrapping spans.
   -- Unaligned bases stay exact, with no page rounding. Memory writeability is
   -- a firmware claim only. IRQ source spans refer to this immutable Data copy;
   -- no namespace resolution/interrupt authority is asserted. Caller retains
   -- exact _HID/_STA, owner/catalog provenance and raw bytes independently.
   procedure Describe(Data:Firmware_Tables.Bytes;Result:out Report)
     with Global=>null,Post=>(if Result.Status=Described then Valid(Result.Value)
                             else Result.Value=Empty);
end ACPI_GPIO_Controller_Resources;
