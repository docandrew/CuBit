pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Display_Protocol;
with CuBit.Grant_References;
with CuBit.Display_Planes;

--  display.svc -> GPU driver hardware planes. One request names one plane
--  of the output routed in the envelope (Reserved); decoders accept only
--  normalized messages (Reserved = 0), as the driver strips routing.
--
--  Query describes the driver's planes for the output (kinds, formats, size
--  limits, scaler, stacking); display feeds them to the pure planner
--  (CuBit.Display_Planes). A driver without such planes answers Query with
--  Unsupported and display composites every request.
--
--  Cursor planes take premultiplied ARGB8888 images, Max_Width x Max_Height,
--  pitch 4 * Max_Width, in a driver-owned buffer granted once by Map_Plane.
--  The image sits at the top-left and the rest is transparent. Display
--  writes the buffer only while no Show for that plane is outstanding.
--  Positions are the image top-left relative to the output, possibly
--  negative. Overlay/primary scanout of client surfaces is not part of this
--  protocol yet (docs/display-planes.md, "Video planes").
package CuBit.GPU_Plane_Protocol with Pure, SPARK_Mode is
   package D renames CuBit.Display_Protocol;
   package DPL renames CuBit.Display_Planes;
   use type D.Wire_Message;

   --  0x0A20.. belong to the Intel render/buffer protocol.
   type Operation is
     (Query, Map_Plane, Show_Plane, Move_Plane, Hide_Plane, Describe_Scanout);
   for Operation use
     (Query => 16#0A10#, Map_Plane => 16#0A11#, Show_Plane => 16#0A12#,
      Move_Plane => 16#0A13#, Hide_Plane => 16#0A14#,
      Describe_Scanout => 16#0A15#);
   function Code (Item : Operation) return Unsigned_32 is
     (Operation'Enum_Rep (Item));

   Payload_Words : constant := 4;
   Status_Words  : constant := 1;
   type Status is (Accepted, Bad_State, Unsupported);
   for Status use (Accepted => 0, Bad_State => 3, Unsupported => 5);

   function Encode_Status (Kind : Operation; Item : Status)
      return D.Wire_Message is
     (Label => Code (Kind), Length => Status_Words,
      Words => [Status'Enum_Rep (Item), 0, 0, 0], others => <>);
   function Accepted_Reply (Kind : Operation; Wire : D.Wire_Message)
      return Boolean is
     (Wire = Encode_Status (Kind, Accepted));

   --  Query (Encode (Query, Plane)): describe plane number Plane of the routed output.
   --  Planes are numbered 1 .. Count; Plane > Count answers Unsupported.
   type Description is record
      Count      : DPL.Plane_Number := DPL.Plane_Number'First;
      Descriptor : DPL.Plane_Descriptor;
   end record;
   type Description_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Description;
         when False => null;
      end case;
   end record;
   function Encode (Item : Description) return D.Wire_Message;
   function Decode_Description (Wire : D.Wire_Message)
      return Description_Decoding
     with Post => (if Decode_Description'Result.Valid then
                     Encode (Decode_Description'Result.Value) = Wire);

   --  Plane-only requests: Query, Map_Plane and Hide_Plane.
   subtype Plane_Operation is Operation range Query .. Hide_Plane
     with Static_Predicate =>
       Plane_Operation in Query | Map_Plane | Hide_Plane;
   type Plane_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : DPL.Plane_Number;
         when False => null;
      end case;
   end record;
   function Encode (Kind : Plane_Operation; Plane : DPL.Plane_Number)
      return D.Wire_Message is
     (Label => Code (Kind), Length => Payload_Words,
      Words => [Unsigned_64 (Plane), 0, 0, 0], others => <>);
   function Decode_Plane (Kind : Plane_Operation; Wire : D.Wire_Message)
      return Plane_Decoding
     with Post => (if Decode_Plane'Result.Valid then
                     Encode (Kind, Decode_Plane'Result.Value) = Wire);

   --  Map_Plane reply: the plane buffer grant and its fixed layout.
   Bytes_Per_Pixel : constant := 4;
   type Buffer is record
      Grant  : CuBit.Grant_References.Reference;
      Width, Height : DPL.Cursor_Extent := DPL.Cursor_Extent'First;
   end record;
   function Pitch (Item : Buffer) return Unsigned_64 is
     (Unsigned_64 (Item.Width) * Bytes_Per_Pixel);
   function Bytes (Item : Buffer) return Unsigned_64 is
     (Pitch (Item) * Unsigned_64 (Item.Height));
   type Buffer_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Buffer;
         when False => null;
      end case;
   end record;
   function Encode (Item : Buffer) return D.Wire_Message;
   function Decode_Buffer (Wire : D.Wire_Message) return Buffer_Decoding
     with Post => (if Decode_Buffer'Result.Valid then
                     Encode (Decode_Buffer'Result.Value) = Wire);

   type Position is record
      X, Y : DPL.Local_Coordinate := 0;
   end record;

   --  Show_Plane: upload the plane buffer, set the hotspot and show it.
   --  The driver answers after the image is in place, so display may then
   --  reuse the buffer. Hotspot is metadata for hosts that draw the pointer.
   type Show is record
      Plane : DPL.Plane_Number := DPL.Plane_Number'First;
      Hot_X, Hot_Y : DPL.Hotspot_Coordinate := 0;
      At_Position : Position;
   end record;
   type Show_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Show;
         when False => null;
      end case;
   end record;
   function Encode (Item : Show) return D.Wire_Message;
   function Decode_Show (Wire : D.Wire_Message) return Show_Decoding
     with Post => (if Decode_Show'Result.Valid then
                     Encode (Decode_Show'Result.Value) = Wire);

   --  Move_Plane: answered at once; drivers apply the newest position.
   type Move is record
      Plane : DPL.Plane_Number := DPL.Plane_Number'First;
      At_Position : Position;
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

   --  Describe_Scanout: which scanout the driver's planes overlay on the
   --  routed output. A driver that owns its output's scanout answers
   --  Driver_Output. A driver that overlays a firmware framebuffer it found
   --  being scanned out (Intel on the firmware mode) answers
   --  Firmware_Framebuffer with that framebuffer's geometry; display uses
   --  the planes on its firmware output only when the geometry equals the
   --  framebuffer the kernel handed it. A driver without planes answers
   --  Unsupported (a one-word status), which decodes as invalid here.
   type Scanout_Source is (Driver_Output, Firmware_Framebuffer);
   for Scanout_Source use (Driver_Output => 1, Firmware_Framebuffer => 2);
   type Scanout is record
      Source : Scanout_Source := Driver_Output;
      Width, Height : D.DP.Positive_Extent := 1;
      Pitch : D.DP.Buffer_Pitch := 0;
   end record;
   type Scanout_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Scanout;
         when False => null;
      end case;
   end record;
   Describe_Request : constant D.Wire_Message :=
     (Label => Operation'Enum_Rep (Describe_Scanout), Length => Payload_Words,
      Words => [0, 0, 0, 0], others => <>);
   function Encode (Item : Scanout) return D.Wire_Message is
     (Label => Code (Describe_Scanout), Length => Payload_Words,
      Words => [Scanout_Source'Enum_Rep (Item.Source),
                Unsigned_64 (Item.Width), Unsigned_64 (Item.Height),
                Unsigned_64 (Item.Pitch)],
      others => <>);
   function Decode_Scanout (Wire : D.Wire_Message) return Scanout_Decoding
     with Post => (if Decode_Scanout'Result.Valid then
                     Encode (Decode_Scanout'Result.Value) = Wire);

   --  Positions as two 32-bit two's complement halves (x low, y high).
   function Pack (Item : Position) return Unsigned_64;
   function Packed_Valid (Word : Unsigned_64) return Boolean;
   function Unpack (Word : Unsigned_64) return Position
     with Pre => Packed_Valid (Word),
          Post => Pack (Unpack'Result) = Word;
end CuBit.GPU_Plane_Protocol;
