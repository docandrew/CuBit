pragma Ada_2022;
with Interfaces;
with ACPI_Passive_Query.Replies;
-- Allocated native query framing. A reference/decoded packet is not authority.
package ACPI_Query_Transport with SPARK_Mode, Pure is
   subtype Word is Interfaces.Unsigned_64;
   use type Word;
   type Message_Label is new Interfaces.Unsigned_32;
   type Header_Byte is new Interfaces.Unsigned_8;
   type Reserved_Field is new Interfaces.Unsigned_16;
   Request_Label : constant Message_Label := 29;
   Reply_Label : constant Message_Label := Request_Label;
   Word_Count : constant Positive := 4;
   Canonical_Length : constant Header_Byte := Header_Byte (Word_Count);
   subtype Word_Index is Natural range 0 .. Word_Count - 1;
   type Words is array (Word_Index) of Word;
   Reference_Index : constant Word_Index := 0;
   Length_Index : constant Word_Index := 1;
   Request_Reserved_First : constant Word_Index := 2;
   Last_Index : constant Word_Index := Word_Index'Last;
   Status_Index : constant Word_Index := 0;
   Written_Index : constant Word_Index := 1;
   Required_Index : constant Word_Index := 2;
   type Packet is record
      Label : Message_Label := 0;
      Length : Header_Byte := 0;
      Flags : Header_Byte := 0;
      Reserved : Reserved_Field := 0;
      Data : Words := [others => 0];
   end record;
   function Canonical_Header (Data : Packet) return Boolean is
     (Data.Label = Request_Label and then Data.Length = Canonical_Length
      and then Data.Flags = 0 and then Data.Reserved = 0) with Global => null;
   type Request_Decode is record
      Selected, Decoded : Boolean := False;
      Reference_Word, Declared_Length : Word := 0;
   end record;
   -- Shape only. The admission layer validates identity/extent and kernel
   -- stamp/sender; acquisition still authenticates expected owner and rights.
   function Decode_Request (Data : Packet) return Request_Decode
     with Global => null,
       Post => Decode_Request'Result.Selected = (Data.Label = Request_Label)
         and then Decode_Request'Result.Decoded =
           (Canonical_Header (Data) and then Data.Data (Request_Reserved_First .. Last_Index) = [0, 0])
         and then (if Decode_Request'Result.Decoded then
           Decode_Request'Result.Reference_Word = Data.Data (Reference_Index)
           and then Decode_Request'Result.Declared_Length = Data.Data (Length_Index)
         else Decode_Request'Result.Reference_Word = 0
           and then Decode_Request'Result.Declared_Length = 0);
   function Encode_Request (Reference_Word, Declared_Length : Word) return Packet
     with Global => null,
       Post => Decode_Request (Encode_Request'Result).Decoded
         and then Decode_Request (Encode_Request'Result).Reference_Word = Reference_Word
         and then Decode_Request (Encode_Request'Result).Declared_Length = Declared_Length;
   type Call_Status is
     (Completed, Denied, Malformed, Invalid_Sender, Invalid_Reference,
      Invalid_Extent, Busy, Acquire_Refused, Copy_In_Failed,
      Evaluation_Refused, Invalid_Response, Copy_Out_Failed, Save_Refused);
   for Call_Status use
     (Completed => 0, Denied => 1, Malformed => 2, Invalid_Sender => 3,
      Invalid_Reference => 4, Invalid_Extent => 5, Busy => 6,
      Acquire_Refused => 7, Copy_In_Failed => 8, Evaluation_Refused => 9,
      Invalid_Response => 10, Copy_Out_Failed => 11, Save_Refused => 12);
   Last_Status_Code : constant Word := 12;
   Minimum_Reply_Bytes : constant Word := ACPI_Passive_Query.Replies.Header_Byte_Count;
   Maximum_Reply_Number : constant Word := Word (Interfaces.Integer_64'Last);
   function Status_Code (Value : Call_Status) return Word is
     (Word (Call_Status'Enum_Rep (Value)))
     with Global => null, Post => Status_Code'Result <= Last_Status_Code;
   function Status_From_Code (Value : Word) return Call_Status
     with Global => null, Pre => Value <= Last_Status_Code,
       Post => Status_Code (Status_From_Code'Result) = Value;
   type Reply_Details is record
      Status : Call_Status := Malformed;
      Written, Required : Word := 0;
   end record;
   function Valid (Value : Reply_Details) return Boolean is
     (Value.Written <= Maximum_Reply_Number and then Value.Required <= Maximum_Reply_Number
      and then (if Value.Status = Completed then Value.Written >= Minimum_Reply_Bytes
                else Value.Written = 0)) with Global => null;
   function Valid_Reply_Words (Data : Words; Declared_Length : Word) return Boolean is
     (Data (Last_Index) = 0 and then Data (Status_Index) <= Last_Status_Code
      and then Data (Written_Index) <= Maximum_Reply_Number
      and then Data (Required_Index) <= Maximum_Reply_Number
      and then (if Data (Status_Index) = Status_Code (Completed) then
         Data (Written_Index) >= Minimum_Reply_Bytes and then Data (Written_Index) <= Declared_Length
       else Data (Written_Index) = 0)) with Global => null;
   type Reply_Decode is record
      Selected, Decoded : Boolean := False;
      Value : Reply_Details;
   end record;
   -- Checks framing/counts only, not peer authentication, grant retirement,
   -- payload validity, inner Query_Status or correlation with the request.
   function Decode_Reply (Data : Packet; Declared_Length : Word) return Reply_Decode
     with Global => null,
       Post => Decode_Reply'Result.Selected = (Data.Label = Reply_Label)
         and then Decode_Reply'Result.Decoded =
           (Canonical_Header (Data) and then Valid_Reply_Words (Data.Data, Declared_Length))
         and then (if Decode_Reply'Result.Decoded then
           Valid (Decode_Reply'Result.Value)
           and then Status_Code (Decode_Reply'Result.Value.Status) = Data.Data (Status_Index)
           and then Decode_Reply'Result.Value.Written = Data.Data (Written_Index)
           and then Decode_Reply'Result.Value.Required = Data.Data (Required_Index)
         else Decode_Reply'Result.Value = (Status => Malformed, Written => 0, Required => 0));
   -- Invalid backend counts become an explicit Invalid_Response with no
   -- payload/requirement, never truncation or an invented successful reply.
   -- The session still must establish Written<=the admitted extent.
   function Encode_Reply (Value : Reply_Details) return Packet
     with Global => null,
       Post => Decode_Reply (Encode_Reply'Result, Word'Last).Decoded
         and then (if Valid (Value) then
           Decode_Reply (Encode_Reply'Result, Word'Last).Value = Value
         else Decode_Reply (Encode_Reply'Result, Word'Last).Value =
           (Status => Invalid_Response, Written => 0, Required => 0));
end ACPI_Query_Transport;
