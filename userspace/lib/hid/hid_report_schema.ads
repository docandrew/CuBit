pragma Ada_2022;
with Interfaces;
generic
   Maximum_Global_Depth, Maximum_Collection_Depth : Positive;
package HID_Report_Schema with SPARK_Mode is
   subtype Byte is Interfaces.Unsigned_8;
   subtype Word is Interfaces.Unsigned_32;
   subtype Wide is Interfaces.Unsigned_64;
   type Bytes is array (Natural range <>) of Byte;
   Maximum_Descriptor_Bytes : constant := 65_535;
   Maximum_Report_Bits : constant := 8 * 65_535;
   type Count is range 0 .. Maximum_Descriptor_Bytes;
   type Bit_Count is range 0 .. Maximum_Report_Bits;
   type Numeric_Value is range -(2 ** 31) .. 2 ** 32 - 1;
   type Report_ID is range 0 .. 255;
   type Report_Kind is (Input, Output, Feature);
   type Length_Table is array (Report_Kind, Report_ID) of Bit_Count;
   type Presence_Table is array (Report_Kind, Report_ID) of Boolean;
   type Usage_Term is record
      First, Last : Word := 0;
      -- Resolved extended page:usage values on successful Parse.
      Uses_Current_Page : Boolean := False;
   end record;
   type Usage_Table is array (Positive range <>) of Usage_Term;
   type Collection is record
      Parent : Count := 0;
      Kind : Byte := 0;
      Usage : Word := 0;
   end record;
   type Collection_Table is array (Positive range <>) of Collection;
   type Field_Span is record
      Kind : Report_Kind := Input;
      ID : Report_ID := 0;
      Offset, Size, Elements : Bit_Count := 0;
      Flags : Word := 0;
      Collection_ID : Count := 0;
      Usage_First, Usage_Terms : Count := 0;
      Logical_Minimum, Logical_Maximum : Numeric_Value := 0;
      Physical_Minimum, Physical_Maximum : Numeric_Value := 0;
      Unit : Word := 0;
      Unit_Exponent : Integer range -8 .. 7 := 0;
   end record;
   type Field_Table is array (Positive range <>) of Field_Span;
   type Status is (Decoded, Need_Capacity, Empty_Descriptor, Descriptor_Too_Long,
      Truncated_Item, Unsupported_Item, Unsupported_Long_Item, Invalid_Global,
      Invalid_Usage_Range, Invalid_Delimiter, Unsupported_Delimiter_Range,
      Invalid_Collection, Unbalanced_Collection, Global_Stack_Underflow,
      Unbalanced_Global_Stack, Depth_Refused, Invalid_Report_ID,
      Mixed_Report_IDs, Invalid_Geometry, Report_Too_Large, Invalid_Value_Range,
      Dangling_Local_Items);
   type Result is record
      Outcome : Status := Empty_Descriptor;
      At_Byte : Count := 0;
      Fields_Required, Usages_Required, Collections_Required : Count := 0;
      Has_Report_IDs : Boolean := False;
      Bits : Length_Table := [others => [others => 0]];
      Present : Presence_Table := [others => [others => False]];
   end record;
   procedure Parse
     (Data : Bytes; Fields : out Field_Table; Usages : out Usage_Table;
      Collections : out Collection_Table; Info : out Result);
   -- Caller-owned storage; no allocation/IO. Need_Capacity provides exact
   -- required counts after validating the whole descriptor. On other failures
   -- required counts are zero; tables may contain a partial prefix and must
   -- not be used. Decoded is the only usable schema. Offsets exclude report-ID
   -- and HID-over-I2C length-prefix bytes. Arrays may have shifted bounds.
   -- One span represents a Main item, so a 256-byte vendor feature does not
   -- consume 256 schema entries. Generic depth bounds are explicit policy.
end HID_Report_Schema;
