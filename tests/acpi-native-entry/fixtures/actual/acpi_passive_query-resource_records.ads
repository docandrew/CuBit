pragma Ada_2022;
-- Typed passive facts, not hardware rights. One record per GPIO pin/GSI keeps
-- ordering, duplicates and subentry continuation without fixed pin/IRQ arrays.
package ACPI_Passive_Query.Resource_Records with SPARK_Mode, Pure is
   subtype U8 is Interfaces.Unsigned_8;
   subtype U16 is Interfaces.Unsigned_16;
   subtype U32 is Interfaces.Unsigned_32;
   type Resource_Kind is
     (Fixed_Memory, Memory_Range, Address_Window, IO_Range,
      Interrupt_Entry, GPIO_Pin, I2C_Connection, Unsupported_Descriptor);
   type Descriptor_Encoding is (Small_Item, Large_Item);
   type Role is (Producer, Consumer);
   type Trigger is (Level, Edge);
   type Polarity is (Active_High, Active_Low, Active_Both);
   type Controller_Kind is (No_Controller, Resolved_Node, Unresolved_Path);
   type Byte_Span is record
      Offset, Length : U32 := 0;
   end record;
   type Controller_Target is record
      Kind : Controller_Kind := No_Controller;
      Node : Node_Ref;
      -- Full unresolved path in the reply payload, never a truncated name.
      -- If it cannot fit, the page builder must report Insufficient_Space.
      Path : Byte_Span;
   end record;
   type Memory_Encoding is (Memory_24, Memory_32);
   type Memory_Fact is record
      Encoding : Memory_Encoding := Memory_32;
      Information : U8 := 0;
      Writeable : Boolean := False;
      Minimum_Base, Maximum_Base, Encoded_Alignment, Alignment, Length : U32 := 0;
   end record;
   type Fixed_Memory_Fact is record
      Information : U8 := 0;
      Writeable : Boolean := False;
      Base, Length : U32 := 0;
   end record;
   type Address_Encoding is (Word_Address, DWord_Address, QWord_Address, Extended_Address);
   type Decode_Mode is (Positive_Decode, Subtractive_Decode);
   type Address_Fact is record
      Encoding : Address_Encoding := QWord_Address;
      Resource_Type : U8 := 0;
      Decode : Decode_Mode := Positive_Decode;
      Minimum_Fixed, Maximum_Fixed : Boolean := False;
      Type_Flags : U8 := 0;
      Granularity, Minimum, Maximum, Translation_Offset, Length : Word := 0;
      Revision : U8 := 0;
      Attributes : Word := 0;
   end record;
   type IO_Decode_Width is (Decode_10, Decode_16);
   type IO_Fact is record
      Fixed : Boolean := False;
      Width : IO_Decode_Width := Decode_10;
      Minimum, Maximum : U16 := 0;
      Alignment, Length : U8 := 0;
   end record;
   type Interrupt_Fact is record
      Mode : Trigger := Level;
      Active : Polarity := Active_High;
      Wake : Boolean := False;
      GSI : U32 := 0;
   end record;
   type GPIO_Kind is (Interrupt_Connection, IO_Connection);
   type IO_Restriction is (Unrestricted, Input_Only, Output_Only, Preserve);
   type GPIO_Fact is record
      Kind : GPIO_Kind := IO_Connection;
      Pin : U16 := 0; -- Includes 0xFFFF; consumers must ignore that sentinel.
      Configuration : U8 := 0;
      Drive_Hundredths_Milliampere : U16 := 0;
      Debounce_Ten_Microseconds : U16 := 0;
      Mode : Trigger := Level;
      Active : Polarity := Active_High;
      Wake : Boolean := False;
      Restriction : IO_Restriction := Unrestricted;
   end record;
   type I2C_Address_Mode is (Seven_Bit, Ten_Bit);
   type Bus_Initiator is (Controller_Initiated, Device_Initiated);
   type I2C_Fact is record
      Revision, Type_Revision : U8 := 0;
      Initiator : Bus_Initiator := Controller_Initiated;
      Addressing : I2C_Address_Mode := Seven_Bit;
      Slave_Address : U16 := 0;
      Speed_Hertz : U32 := 0;
      Legacy_Virtual_Register : U8 := 0;
   end record;
   type Resource_Record (Kind : Resource_Kind := Unsupported_Descriptor) is record
      Encoding : Descriptor_Encoding := Large_Item;
      Name : U8 := 0;
      Source_Offset : U32 := 0;
      Subentry : U32 := 0;
      Total_Subentries : U32 := 1;
      Controller : Controller_Target;
      ResourceSource_Index : U8 := 0;
      ResourceSource_Offset : U32 := 0;
      Raw_General_Flags, Raw_Specific_Flags : U16 := 0;
      Resource_Role : Role := Consumer;
      Shared : Boolean := False;
      case Kind is
         when Fixed_Memory => Fixed : Fixed_Memory_Fact;
         when Memory_Range => Memory : Memory_Fact;
         when Address_Window => Window : Address_Fact;
         when IO_Range => IO : IO_Fact;
         when Interrupt_Entry => IRQ : Interrupt_Fact;
         when GPIO_Pin => GPIO : GPIO_Fact;
         when I2C_Connection => I2C : I2C_Fact;
         when Unsupported_Descriptor => null;
      end case;
   end record;
   -- Wire validation is not firmware parsing, geometry/ownership admission, or
   -- controller liveness. Unresolved spans must fit the complete reply payload;
   -- resolved refs must additionally match the reply's instance/revision there.
   function Valid (Item : Resource_Record; Limit : Node_Limit; Payload_Bytes : U32)
     return Boolean with Global => null;
   Record_Word_Count : constant := 36;
   subtype Record_Words is Word_Array (0 .. Record_Word_Count - 1);
   Record_Magic : constant Word := 16#3153_4552_4950_4341#; -- ACPIRES1
   Record_Version : constant Word := 1;
   function Encode (Item : Resource_Record; Limit : Node_Limit; Payload_Bytes : U32)
     return Record_Words with Global => null, Pre => Valid (Item, Limit, Payload_Bytes);
   type Resource_Result (Accepted : Boolean := False) is record
      case Accepted is
         when True => Value : Resource_Record;
         when False => null;
      end case;
   end record;
   function Decode (Data : Record_Words; Limit : Node_Limit; Payload_Bytes : U32)
     return Resource_Result with Global => null,
       Post => (if Decode'Result.Accepted then
         Valid (Decode'Result.Value, Limit, Payload_Bytes) and then
         Encode (Decode'Result.Value, Limit, Payload_Bytes) = Data);
end ACPI_Passive_Query.Resource_Records;
