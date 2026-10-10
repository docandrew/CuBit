pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Display_Protocol;
with CuBit.Grant_References;
with CuBit.Display_Planes;
with CuBit.Display_Pool_Protocol;

--  Client (Desktop) -> display.svc plane request protocol. Codecs only:
--  display checks the display lease and its own request table before acting.
--
--  A plane request is content the client would like scanned out by a
--  hardware plane instead of composited by itself. Requests are addressed by
--  Request_Id and carry a Plane_Kind. Cursor requests are implemented now;
--  Overlay (video) and Primary (fullscreen bypass) requests reuse the same
--  identities, planner and reports later (docs/display-planes.md).
--
--  Positions are anchor positions in desktop space; Place_Output declares
--  where an output sits in that space. Query_Output and Place_Output are
--  routed by the output number in the envelope; every other request uses
--  output zero (Reserved = 0).
--
--  Every request except Query_Output is answered by a Report: the status,
--  the newest proposed plan epoch, and which requests that proposal and the
--  committed (displayed) plan put on hardware or leave composited. A client
--  composites the proposal's Composited set into its next frame and tags the
--  frame with the epoch (Submit_Plan_Frame below).
package CuBit.Display_Plane_Protocol with Pure, SPARK_Mode is
   package D renames CuBit.Display_Protocol;
   package DPL renames CuBit.Display_Planes;
   use type D.Wire_Message;
   subtype Status_Code is D.DP.Status_Code;

   --  0x090D..0x0916 belong to discovery, pools and IPC fixtures; 0x092A is
   --  the epoch-tagged pool frame (Submit_Plan_Frame).
   type Operation is
     (Query_Output, Create_Request, Destroy_Request, Set_Cursor_Image,
      Set_Anchor, Move_Request, Set_Visibility, Set_Priority, Place_Output,
      Get_Plan);
   for Operation use
     (Query_Output => 16#0920#, Create_Request => 16#0921#,
      Destroy_Request => 16#0922#, Set_Cursor_Image => 16#0923#,
      Set_Anchor => 16#0924#, Move_Request => 16#0925#,
      Set_Visibility => 16#0926#, Set_Priority => 16#0927#,
      Place_Output => 16#0928#, Get_Plan => 16#0929#);
   function Code (Item : Operation) return Unsigned_32 is
     (Operation'Enum_Rep (Item));
   type Operation_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Operation;
         when False => null;
      end case;
   end record;
   function Decode_Operation (Label : Unsigned_32) return Operation_Decoding
     with Post => (if Decode_Operation'Result.Valid then
                     Code (Decode_Operation'Result.Value) = Label);

   Payload_Words : constant := 4;

   --  Requests without a payload: Query_Output, Get_Plan.
   function Encode_Empty (Kind : Operation) return D.Wire_Message is
     (Label => Code (Kind), Length => Payload_Words, others => <>);
   function Valid_Empty (Wire : D.Wire_Message; Kind : Operation)
      return Boolean is
     (Wire = Encode_Empty (Kind));

   --  Create_Request (kind, priority, input) and Set_Priority (the other
   --  fields zero). Absolute: the pointer driving this cursor reports
   --  absolute positions (a tablet or touch screen, a remote session);
   --  only such cursors may use host-pointer planes (DPL.Plane_Descriptor).
   type Identity is record
      Request  : DPL.Request_Id := DPL.Request_Id'First;
      Kind     : DPL.Plane_Kind := DPL.Cursor;
      Priority : DPL.Request_Priority := DPL.Request_Priority'First;
      Absolute : Boolean := False;
   end record;
   subtype Identity_Operation is Operation range Create_Request .. Set_Priority
     with Static_Predicate =>
       Identity_Operation in Create_Request | Set_Priority;
   type Identity_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Identity;
         when False => null;
      end case;
   end record;
   function Encode (Kind : Identity_Operation; Item : Identity)
      return D.Wire_Message;
   function Decode_Identity (Kind : Identity_Operation; Wire : D.Wire_Message)
      return Identity_Decoding
     with Post => (if Decode_Identity'Result.Valid then
                     Encode (Kind, Decode_Identity'Result.Value) = Wire);

   type Request_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : DPL.Request_Id;
         when False => null;
      end case;
   end record;
   function Encode_Destroy (Request : DPL.Request_Id) return D.Wire_Message;
   function Decode_Destroy (Wire : D.Wire_Message) return Request_Decoding
     with Post => (if Decode_Destroy'Result.Valid then
                     Encode_Destroy (Decode_Destroy'Result.Value) = Wire);

   --  Set_Cursor_Image: premultiplied ARGB8888, rows tightly packed (pitch =
   --  4 * Width) from byte zero of the granted range. Image and hotspot
   --  change together, so a shape change never shows a stale hotspot. The
   --  grant is NOT authority: display acquires it against the authenticated
   --  sender and copies the pixels before replying.
   Bytes_Per_Pixel : constant := 4;
   type Cursor_Image is record
      Request : DPL.Request_Id := DPL.Request_Id'First;
      Grant   : CuBit.Grant_References.Reference;
      Width, Height : DPL.Cursor_Extent := DPL.Cursor_Extent'First;
      Hot_X, Hot_Y  : DPL.Hotspot_Coordinate := 0;
   end record;
   function Valid_Image (Item : Cursor_Image) return Boolean is
     (DPL.Valid_Hotspot (Item.Width, Item.Height, Item.Hot_X, Item.Hot_Y));
   function Image_Bytes (Item : Cursor_Image) return Unsigned_64 is
     (Unsigned_64 (Item.Width) * Unsigned_64 (Item.Height) * Bytes_Per_Pixel);
   type Image_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Cursor_Image;
         when False => null;
      end case;
   end record;
   function Encode (Item : Cursor_Image) return D.Wire_Message
     with Pre => Valid_Image (Item);
   function Decode_Image (Wire : D.Wire_Message) return Image_Decoding
     with Post => (if Decode_Image'Result.Valid then
                     Valid_Image (Decode_Image'Result.Value) and then
                     Encode (Decode_Image'Result.Value) = Wire);

   --  Set_Anchor (a cursor's hotspot): checked against the current image.
   type Anchor is record
      Request : DPL.Request_Id := DPL.Request_Id'First;
      X, Y    : DPL.Hotspot_Coordinate := 0;
   end record;
   type Anchor_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Anchor;
         when False => null;
      end case;
   end record;
   function Encode (Item : Anchor) return D.Wire_Message;
   function Decode_Anchor (Wire : D.Wire_Message) return Anchor_Decoding
     with Post => (if Decode_Anchor'Result.Valid then
                     Encode (Decode_Anchor'Result.Value) = Wire);

   --  Move_Request: signed positions, two's complement in each word.
   type Move is record
      Request : DPL.Request_Id := DPL.Request_Id'First;
      X, Y    : DPL.Space_Coordinate := 0;
   end record;
   type Move_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Move;
         when False => null;
      end case;
   end record;
   function Encode (Item : Move) return D.Wire_Message;
   function Decode_Move (Wire : D.Wire_Message) return Move_Decoding
     with Post => (if Decode_Move'Result.Valid then
                     Encode (Decode_Move'Result.Value) = Wire);

   type Visibility is record
      Request : DPL.Request_Id := DPL.Request_Id'First;
      Visible : Boolean := False;
   end record;
   type Visibility_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Visibility;
         when False => null;
      end case;
   end record;
   function Encode (Item : Visibility) return D.Wire_Message;
   function Decode_Visibility (Wire : D.Wire_Message)
      return Visibility_Decoding
     with Post => (if Decode_Visibility'Result.Valid then
                     Encode (Decode_Visibility'Result.Value) = Wire);

   --  Place_Output: the routed output's top-left in desktop space.
   type Origin is record
      X, Y : DPL.Space_Coordinate := 0;
   end record;
   type Origin_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Origin;
         when False => null;
      end case;
   end record;
   function Encode (Item : Origin) return D.Wire_Message;
   function Decode_Origin (Wire : D.Wire_Message) return Origin_Decoding
     with Post => (if Decode_Origin'Result.Valid then
                     Encode (Decode_Origin'Result.Value) = Wire);

   --  Query_Output reply: how many of this output's scanout planes can take
   --  each request kind, how many host-pointer cursor planes it has (usable
   --  by absolute cursors only), the cursor size limit and the request
   --  capacity. Zero cursor planes of either sort: every cursor here is
   --  composited.
   type Kind_Counts is array (DPL.Plane_Kind) of DPL.Plane_Count;
   type Capability is record
      Planes     : Kind_Counts := [others => 0];
      Host_Pointer_Cursors : DPL.Plane_Count := 0;
      Max_Width  : DPL.Cursor_Extent := DPL.Cursor_Extent'First;
      Max_Height : DPL.Cursor_Extent := DPL.Cursor_Extent'First;
      Capacity   : DPL.Request_Count := DPL.Request_Capacity;
   end record;
   type Capability_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Capability;
         when False => null;
      end case;
   end record;
   function Encode (Item : Capability) return D.Wire_Message;
   function Decode_Capability (Wire : D.Wire_Message)
      return Capability_Decoding
     with Post => (if Decode_Capability'Result.Valid then
                     Encode (Decode_Capability'Result.Value) = Wire);

   type Request_Set is array (DPL.Request_Id) of Boolean;
   No_Requests : constant Request_Set := [others => False];
   Set_Bits : constant := DPL.Request_Capacity;
   --  Request R is bit R - 1.
   function Bit (R : DPL.Request_Id) return Unsigned_64 is
     (Shift_Left (1, Natural (R) - 1));
   function To_Bits (Set : Request_Set) return Unsigned_64
     with Post => To_Bits'Result < 2 ** Set_Bits and then
       (for all R in DPL.Request_Id =>
          ((To_Bits'Result and Bit (R)) /= 0) = Set (R));
   function From_Bits (Bits : Unsigned_64) return Request_Set is
     ([for R in DPL.Request_Id => (Bits and Bit (R)) /= 0]);

   type Report is record
      Status : Status_Code := D.DP.Bad_State;
      Epoch  : DPL.Plan_Epoch := DPL.No_Epoch;
      Proposed_Hardware, Proposed_Composited : Request_Set := No_Requests;
      Committed_Hardware, Committed_Composited : Request_Set := No_Requests;
   end record;
   type Report_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Report;
         when False => null;
      end case;
   end record;
   function Encode (Kind : Operation; Item : Report) return D.Wire_Message;
   Set_Field_Bits : constant := 8;
   pragma Compile_Time_Error
     (Set_Bits > Set_Field_Bits, "request sets exceed their report fields");
   function Decode_Report (Kind : Operation; Wire : D.Wire_Message)
      return Report_Decoding
     with Post => (if Decode_Report'Result.Valid then
                     Wire.Label = Code (Kind) and then
                     Wire.Length = Payload_Words and then
                     Status_Code'Enum_Rep (Decode_Report'Result.Value.Status)
                       = Wire.Words (0) and then
                     Unsigned_64 (Decode_Report'Result.Value.Epoch)
                       = Wire.Words (1));

   --  A pool frame rendered for plan Epoch: it composites exactly that
   --  proposal's Composited set. The pool frame's own words 0 .. 2 follow,
   --  word 3 carries the epoch. Display commits the epoch's plane changes
   --  when it publishes the frame; the completion is the ordinary pool
   --  Submit_Frame completion.
   Submit_Plan_Frame : constant Unsigned_32 := 16#092A#;
   subtype Live_Epoch is DPL.Plan_Epoch range 1 .. DPL.Plan_Epoch'Last;
   type Plan_Frame is record
      Item  : CuBit.Display_Pool_Protocol.Frame;
      Epoch : Live_Epoch;
   end record;
   type Plan_Frame_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Plan_Frame;
         when False => null;
      end case;
   end record;
   function Encode (Item : Plan_Frame) return D.Wire_Message;
   function Decode_Plan_Frame (Wire : D.Wire_Message)
      return Plan_Frame_Decoding
     with Post => (if Decode_Plan_Frame'Result.Valid then
                     Wire.Label = Submit_Plan_Frame and then
                     Unsigned_64 (Decode_Plan_Frame'Result.Value.Epoch) =
                       Wire.Words (3));
end CuBit.Display_Plane_Protocol;
