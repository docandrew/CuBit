pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Desktop_Protocol;
with CuBit.Grant_References;

--  Compositor -> scanout service. Reuse the desktop's bounded BGRA layout
--  and wire envelope, but not surface names or application session semantics.
package CuBit.Display_Protocol with SPARK_Mode, Pure is
   package DP renames CuBit.Desktop_Protocol;
   use type DP.Wire_Message;
   type Operation is
     (Get_Information, Attach_Buffer, Present_Rectangle, Clear,
      Get_Status, Acquire_Display, Release_Display, Map_Backbuffer,
      Present_Immediate_Rectangle, Present_Region, Present_Immediate_Region,
      Open_Presentation_Session, Submit_Frame);
   for Operation use
     (Get_Information => 16#0900#, Attach_Buffer => 16#0901#,
      Present_Rectangle => 16#0902#, Clear => 16#0903#,
      Get_Status => 16#0904#, Acquire_Display => 16#0905#,
      Release_Display => 16#0906#, Map_Backbuffer => 16#0907#,
      Present_Immediate_Rectangle => 16#0908#, Present_Region => 16#0909#,
      Present_Immediate_Region => 16#090A#,
      Open_Presentation_Session => 16#090B#, Submit_Frame => 16#090C#);
   function Code (Item : Operation) return Unsigned_32 is
     (Operation'Enum_Rep (Item));
   subtype Wire_Message is DP.Wire_Message;
   subtype Buffer_Layout is DP.Buffer_Layout;
   subtype Lease_Operation is Operation
     range Acquire_Display .. Release_Display;
   function Valid_Lease_Request
     (Wire : Wire_Message; Kind : Lease_Operation) return Boolean is
     (Wire.Label = Code (Kind) and then Wire.Length = 4 and then
      Wire.Flags = 0 and then Wire.Reserved = 0 and then
      (for all Word of Wire.Words => Word = 0));
   function Encode_Lease_Request (Kind : Lease_Operation)
      return Wire_Message is
     (Label => Code (Kind), Length => 4, others => <>);
   function Encode_Open_Session return Wire_Message is
     (Label => Code (Open_Presentation_Session), Length => 4, others => <>);
   function Valid_Open_Session (Wire : Wire_Message) return Boolean is
     (Wire = Encode_Open_Session);
   type Attachment_Request is record
      Grant : CuBit.Grant_References.Reference;
      Layout : Buffer_Layout;
   end record;
   type Attachment_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Attachment_Request;
         when False => null;
      end case;
   end record;

   --  [global slot, generation, width | height << 32, pitch]. A decoded
   --  reference is NOT authority: acquire it against the authenticated sender
   --  and the complete byte range before replacing the existing attachment.
   function Decode_Attachment (Wire : Wire_Message)
      return Attachment_Decoding
     with Post => (if Decode_Attachment'Result.Valid then
       DP.Valid_Layout (Decode_Attachment'Result.Value.Layout));
   function Encode_Attachment (Item : Attachment_Request) return Wire_Message
     with Pre => DP.Valid_Layout (Item.Layout);

   subtype Live_ID is Unsigned_64 range 1 .. Unsigned_64'Last;
   type Frame_Request is record
      Session : Live_ID;
      Frame : Live_ID;
      Area : DP.Rectangle;
   end record;
   type Frame_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Frame_Request;
         when False => null;
      end case;
   end record;
   --  The session binds the current acquired attachment and its generation.
   --  No mutable shared command list: all damage is an inline wire snapshot.
   --  Codec only: the service must reject empty/out-of-layout areas, stale
   --  sessions and replayed frames against its authenticated owner state.
   function Decode_Frame (Wire : Wire_Message) return Frame_Decoding;
   function Encode_Frame (Item : Frame_Request) return Wire_Message;
   type Frame_Outcome is (Published, Rejected, Failed);
   for Frame_Outcome use (Published => 0, Rejected => 1, Failed => 2);
   type Buffer_Disposition is (Not_Acquired, Released, Still_Held);
   for Buffer_Disposition use
     (Not_Acquired => 0, Released => 1, Still_Held => 2);
   type Frame_Result is record
      Session : Live_ID;
      Frame : Live_ID;
      Outcome : Frame_Outcome;
      Buffer_State : Buffer_Disposition;
   end record;
   --  A decoded result alone is not a release fence: match a successful
   --  kernel completion to the outstanding token/session/frame first.
   type Frame_Result_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Frame_Result;
         when False => null;
      end case;
   end record;
   function Encode_Frame_Result (Item : Frame_Result) return Wire_Message;
   function Decode_Frame_Result (Wire : Wire_Message)
      return Frame_Result_Decoding;
end CuBit.Display_Protocol;
