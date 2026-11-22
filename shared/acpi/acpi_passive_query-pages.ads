pragma Ada_2022;
package ACPI_Passive_Query.Pages with SPARK_Mode, Pure is
   subtype Byte is Interfaces.Unsigned_8;
   use type Byte;
   type Byte_Array is array (Natural range <>) of Byte;
   Bytes_Per_Word : constant := Word'Size / Byte'Size;
   Frame_Byte_Count : constant := Frame_Word_Count * Bytes_Per_Word;
   subtype Frame_Bytes is Byte_Array (0 .. Frame_Byte_Count - 1);

   -- Operate on a stable private copy, never a caller-writable loan mapping.
   -- The native handler must bound the declared extent before copying it.
   -- Header only. Use Encode_Request to include complete external identifier
   -- bytes and a distinct full copy of a continued cursor's filter string.
   function Encode (Item : Request; Limit : Node_Limit) return Frame_Bytes
     with Pre => Valid (Item, Limit), Global => null;
   function Canonical (Data : Byte_Array; Expected : Frame_Bytes) return Boolean
     with Global => null;
   function Required_Bytes (Item : Request) return Word;
   function Valid_Request_Layout (Item : Request) return Boolean;
   function ID_Text_Valid (Item : Request; Data : Byte_Array) return Boolean;
   function Same_ID_Value
     (Left : Device_ID; Left_Data : Byte_Array; Right : Device_ID; Right_Data : Byte_Array)
      return Boolean;
   -- Rebase metadata when carrying a returned next cursor into a new request,
   -- or placing the filter pool after a newly-built reply payload. Does not
   -- copy text or assert that the destination buffer contains the bytes.
   procedure Rebase_Text
     (Item : in out Request; Origin : Interfaces.Unsigned_32; Limit : Node_Limit; Accepted : out Boolean)
     with Global => null,
       Post => (if Accepted then Valid (Item, Limit) else Item = Item'Old);
   procedure Encode_Request
     (Item : Request; Text : Byte_Array; Data : out Byte_Array; Limit : Node_Limit;
      Written : out Word; Accepted : out Boolean)
     with Global => null,
       Post => (if Accepted then Valid (Item, Limit) and then Written = Required_Bytes (Item)
         else Written = 0 and then (for all B of Data => B = 0));
   -- Short buffers and nonzero bytes beyond header+text are malformed. Full
   -- external ID and cursor-filter copies must be equal and contain no NUL.
   function Decode (Data : Byte_Array; Limit : Node_Limit) return Decode_Result
     with Global => null,
       Post => (if Decode'Result.Status = Decoded then
         Valid (Decode'Result.Value, Limit) and then
         ID_Text_Valid (Decode'Result.Value, Data) and then
         (if not Is_External (Decode'Result.Value.Key.ID) then
           Canonical (Data, Encode (Decode'Result.Value, Limit))));
end ACPI_Passive_Query.Pages;
