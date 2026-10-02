pragma Ada_2022;
with CuBit.Display_Protocol;
package CuBit.Display_Pool_Protocol with Pure, SPARK_Mode is
   package D renames CuBit.Display_Protocol;
   use type D.Wire_Message;
   subtype Buffer_Slot is Positive range 1 .. 3;
   -- 090D/090E belong to output discovery; keep its namespace disjoint.
   Attach_Buffer : constant := 16#0910#;
   Open_Session : constant := 16#0911#;
   Submit_Frame : constant := 16#0912#;
   -- Fixed three-slot registration, immutable throughout a presentation session.
   -- Flags carry the pool-local slot; Reserved retains Display output routing.
   -- Decode only normalized output-zero messages, as the existing protocol does.
   type Attachment is record
      Buffer : Buffer_Slot;
      Source : D.Attachment_Request;
   end record;
   type Attachment_Result (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Attachment;
         when False => null;
      end case;
   end record;
   function Encode (Item : Attachment) return D.Wire_Message
     with Pre => D.DP.Valid_Layout (Item.Source.Layout);
   function Decode_Attachment (Wire : D.Wire_Message) return Attachment_Result;
   function Encode_Open return D.Wire_Message is
     (Label => Open_Session, Length => 4, others => <>);
   function Valid_Open (Wire : D.Wire_Message) return Boolean is (Wire = Encode_Open);
   type Frame is record
      Buffer : Buffer_Slot;
      Request : D.Frame_Request;
   end record;
   type Frame_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Frame;
         when False => null;
      end case;
   end record;
   function Encode (Item : Frame) return D.Wire_Message;
   function Decode_Frame (Wire : D.Wire_Message) return Frame_Decoding;
   type Completion is record
      Buffer : Buffer_Slot;
      Result : D.Frame_Result;
   end record;
   type Completion_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Completion;
         when False => null;
      end case;
   end record;
   function Encode (Item : Completion) return D.Wire_Message;
   function Decode_Completion (Wire : D.Wire_Message) return Completion_Decoding;
end CuBit.Display_Pool_Protocol;
