pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Grant_References;

--  Application -> compositor protocol. No IPC, addresses, allocation or
--  toolkit dependencies: native clients, the service and tests share a codec.
package CuBit.Desktop_Protocol with SPARK_Mode, Pure is
   type Operation is
     (Hello, Goodbye, Get_Information, Create_Surface, Destroy_Surface,
      Present_Surface, Resize_Surface, Attach_Buffer, Set_Pointer_Cursor,
      Poll_Input, Wait_Input, Set_Window_Limits, Set_Window_Title);
   for Operation use
     (Hello => 16#0800#, Goodbye => 16#0801#, Get_Information => 16#0802#,
      Create_Surface => 16#0810#, Destroy_Surface => 16#0811#,
      Present_Surface => 16#0812#, Resize_Surface => 16#0813#,
      Attach_Buffer => 16#0814#, Set_Pointer_Cursor => 16#0815#,
      Poll_Input => 16#0821#, Wait_Input => 16#0822#,
      Set_Window_Limits => 16#0841#, Set_Window_Title => 16#0842#);
   function Code (Item : Operation) return Unsigned_32 is
     (Operation'Enum_Rep (Item));
   type Operation_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Operation;
         when False => null;
      end case;
   end record;
   function Decode_Operation (Label : Unsigned_32) return Operation_Decoding;

   type Status_Code is
     (Success, Denied, Bad_Object, Bad_State, Invalid_Request, Unsupported,
      Resources_Exhausted);
   for Status_Code use
     (Success => 0, Denied => 1, Bad_Object => 2, Bad_State => 3,
      Invalid_Request => 4, Unsupported => 5, Resources_Exhausted => 6);
   type Payload is array (Natural range 0 .. 3) of Unsigned_64;
   type Wire_Message is record
      Label : Unsigned_32 := 0;
      Length : Unsigned_8 := 0;
      Flags : Unsigned_8 := 0;
      Reserved : Unsigned_16 := 0;
      Words : Payload := [others => 0];
   end record;

   --  A name alone confers no authority. Resolve against kernel-authenticated
   --  caller identity and service-owned object state before every operation.
   type Surface_Name is new Unsigned_64;
   subtype Live_Surface_Name is Surface_Name range 1 .. Surface_Name'Last;
   type Pixel_Coordinate is range 0 .. 65_535;
   type Pixel_Extent is range 0 .. 65_535;
   subtype Positive_Extent is Pixel_Extent range 1 .. Pixel_Extent'Last;
   type Surface_Kind is (Plain_Surface, Shell_Surface, Window_Surface);
   for Surface_Kind use (Plain_Surface => 0, Shell_Surface => 1,
     Window_Surface => 2);

   type Protocol_Revision is record
      Major, Minor : Unsigned_32;
   end record;
   Current_Revision : constant Protocol_Revision := (0, 1);
   type Hello_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Revision : Protocol_Revision;
         when False => null;
      end case;
   end record;
   function Decode_Hello (Wire : Wire_Message) return Hello_Decoding;
   function Encode_Hello (Revision : Protocol_Revision) return Wire_Message;
   subtype Empty_Session_Operation is Operation
     with Static_Predicate => Empty_Session_Operation in
       Goodbye | Get_Information;
   function Valid_Empty_Request
     (Wire : Wire_Message; Kind : Empty_Session_Operation) return Boolean;
   function Encode_Empty_Request (Kind : Empty_Session_Operation)
      return Wire_Message;

   type Session_Identifier is new Unsigned_64 range 1 .. Unsigned_64'Last;
   type Surface_Capacity is range 1 .. 65_535;
   --  Informational compositor/session identifier and shared table capacity.
   --  Neither identifies authority, reserves objects, or installs a session.
   type Hello_Result (Status : Status_Code := Invalid_Request) is record
      case Status is
         when Success =>
            Session : Session_Identifier;
            Capacity : Surface_Capacity;
         when others => null;
      end case;
   end record;
   function Decode_Hello_Result (Wire : Wire_Message) return Hello_Result;
   function Encode_Hello_Result (Item : Hello_Result) return Wire_Message;
   type Pixel_Format is (BGRA_8888);
   for Pixel_Format use (BGRA_8888 => 1);
   subtype Display_Scale is Unsigned_32 range 1 .. Unsigned_32'Last;
   Unit_Scale : constant Display_Scale := 2 ** 16;
   type Information_Result (Status : Status_Code := Invalid_Request) is record
      case Status is
         when Success =>
            Width, Height : Positive_Extent;
            Format : Pixel_Format;
            Scale : Display_Scale;
         when others => null;
      end case;
   end record;
   function Decode_Information_Result (Wire : Wire_Message)
      return Information_Result;
   function Encode_Information_Result (Item : Information_Result)
      return Wire_Message;

   subtype Input_Operation is Operation range Poll_Input .. Wait_Input;
   type Input_Request (Kind : Input_Operation := Poll_Input) is record
      Surface : Live_Surface_Name := 1;
      After_Serial : Unsigned_64 := 0;
      case Kind is
         when Wait_Input => Deadline : Unsigned_64 := 0;
         when Poll_Input => null;
      end case;
   end record;
   type Input_Request_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Input_Request;
         when False => null;
      end case;
   end record;
   function Decode_Input_Request (Wire : Wire_Message)
      return Input_Request_Decoding;
   function Encode_Input_Request (Item : Input_Request) return Wire_Message;

   type Input_Event_Kind is
     (No_Input, Key_Pressed, Key_Released, Pointer_Moved, Pointer_Pressed,
      Pointer_Released, Text_Entered, Wheel_Turned, Surface_Configured,
      Input_Resynchronized);
   for Input_Event_Kind use
     (No_Input => 0, Key_Pressed => 1, Key_Released => 2, Pointer_Moved => 3,
      Pointer_Pressed => 4, Pointer_Released => 5, Text_Entered => 6,
      Wheel_Turned => 7, Surface_Configured => 8, Input_Resynchronized => 9);
   --  Checked event envelope. Packed words remain for the existing toolkit
   --  ABI; validity establishes their per-kind bounds before narrowing.
   type Input_Envelope is record
      Kind : Input_Event_Kind := No_Input;
      Serial, Payload0, Payload1 : Unsigned_64 := 0;
      More_Pending : Boolean := False;
   end record;
   More_Pending_Flag : constant Unsigned_8 := 1;
   function Valid_Input_Envelope (Item : Input_Envelope) return Boolean
     with Annotate => (GNATprove, Inline_For_Proof);
   type Input_Result (Status : Status_Code := Invalid_Request) is record
      case Status is
         when Success => Value : Input_Envelope;
         when others => null;
      end case;
   end record;
   function Decode_Input_Result
     (Wire : Wire_Message; Expected : Input_Operation)
      return Input_Result
     with Post => (if Decode_Input_Result'Result.Status = Success then
       Valid_Input_Envelope (Decode_Input_Result'Result.Value));
   function Encode_Input_Reply (Kind : Input_Operation; Item : Input_Envelope)
      return Wire_Message with Pre => Valid_Input_Envelope (Item);

   --  Status-only acknowledgements: exact operation label, one status word,
   --  zero flags/reserved fields and zero unused words. Not a buffer fence.
   function Decode_Status (Wire : Wire_Message; Expected : Operation)
      return Status_Code;
   function Encode_Status (Kind : Operation; Status : Status_Code)
      return Wire_Message;

   type Destroy_Request is record
      Surface : Live_Surface_Name := 1;
   end record;
   type Destroy_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Destroy_Request;
         when False => null;
      end case;
   end record;
   function Decode_Destroy (Wire : Wire_Message) return Destroy_Decoding;
   function Encode_Destroy (Item : Destroy_Request) return Wire_Message;

   type Cursor_Style is
     (Default_Cursor, Text_Cursor, Horizontal_Resize_Cursor,
      Vertical_Resize_Cursor, Diagonal_Resize_Cursor);
   for Cursor_Style use
     (Default_Cursor => 0, Text_Cursor => 1, Horizontal_Resize_Cursor => 2,
      Vertical_Resize_Cursor => 3, Diagonal_Resize_Cursor => 4);
   type Cursor_Request is record
      Surface : Live_Surface_Name := 1;
      Style : Cursor_Style := Default_Cursor;
   end record;
   type Cursor_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Cursor_Request;
         when False => null;
      end case;
   end record;
   function Decode_Cursor (Wire : Wire_Message) return Cursor_Decoding;
   function Encode_Cursor (Item : Cursor_Request) return Wire_Message;

   --  Existing inline wire format: up to 23 bytes, not Unicode characters.
   --  The discriminant owns the length; there is no separate buffer/count
   --  invariant for callers or the compositor to maintain.
   subtype Title_Length is Natural range 0 .. 23;
   type Inline_Title (Length : Title_Length := 0) is record
      Text : String (1 .. Length);
   end record;
   function Make_Title (Text : String) return Inline_Title;
   type Title_Request is record
      Surface : Live_Surface_Name := 1;
      Title : Inline_Title;
   end record;
   type Title_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Title_Request;
         when False => null;
      end case;
   end record;
   function Decode_Title (Wire : Wire_Message) return Title_Decoding;
   function Encode_Title (Item : Title_Request) return Wire_Message;

   type Create_Request is record
      --  Zero requests compositor-selected size.
      Width, Height : Pixel_Extent := 0;
      Kind : Surface_Kind := Window_Surface;
   end record;
   type Create_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Create_Request;
         when False => null;
      end case;
   end record;
   function Decode_Create (Wire : Wire_Message) return Create_Decoding
     with Annotate => (GNATprove, Inline_For_Proof);
   function Encode_Create (Item : Create_Request) return Wire_Message
     with Post => Decode_Create (Encode_Create'Result).Valid and then
       Decode_Create (Encode_Create'Result).Value = Item;

   type Creation_Result (Status : Status_Code := Bad_State) is record
      case Status is
         when Success =>
            Surface : Live_Surface_Name;
            Width, Height : Positive_Extent;
            Serial : Unsigned_64 range 1 .. Unsigned_64'Last;
         when others => null;
      end case;
   end record;
   function Decode_Creation_Result (Wire : Wire_Message) return Creation_Result
     with Annotate => (GNATprove, Inline_For_Proof);
   --  Success: four words [name, width, height, serial]. Failure: two words
   --  [0, status]. A failure can NEVER be mistaken for a nonzero surface name.
   function Encode_Creation_Result (Item : Creation_Result) return Wire_Message
     with Post => (if Item.Status /= Success then
                     Encode_Creation_Result'Result.Words (0) = 0);

   type Rectangle is record
      X, Y : Pixel_Coordinate := 0;
      Width, Height : Pixel_Extent := 0;
   end record;

   type Resize_Request is record
      Surface : Live_Surface_Name := 1;
      Width, Height : Pixel_Extent := 0;
   end record;
   type Resize_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Resize_Request;
         when False => null;
      end case;
   end record;
   function Decode_Resize (Wire : Wire_Message) return Resize_Decoding;
   function Encode_Resize (Item : Resize_Request) return Wire_Message;
   type Resize_Result (Status : Status_Code := Invalid_Request) is record
      case Status is
         when Success =>
            Width, Height : Pixel_Extent;
            Serial : Unsigned_64;
         when others => null;
      end case;
   end record;
   function Decode_Resize_Result (Wire : Wire_Message) return Resize_Result;
   function Encode_Resize_Result (Item : Resize_Result) return Wire_Message;

   type Window_Feature is
     (Decorated, Resizable, Minimizable, Maximizable, Closeable,
      Fullscreenable, Pointer_Capture, Fixed_Size);
   for Window_Feature use
     (Decorated => 1, Resizable => 2, Minimizable => 4, Maximizable => 8,
      Closeable => 16, Fullscreenable => 32, Pointer_Capture => 64,
      Fixed_Size => 128);
   type Window_Features is array (Window_Feature) of Boolean;
   function Feature_Bits (Features : Window_Features) return Unsigned_64;
   type Window_Bounds is record
      Minimum_Width, Minimum_Height : Pixel_Extent := 0;
      --  Zero maximum means no application-supplied upper limit.
      Maximum_Width, Maximum_Height : Pixel_Extent := 0;
   end record;
   function Valid_Bounds (Bounds : Window_Bounds) return Boolean is
     ((Bounds.Maximum_Width = 0 or else
       Bounds.Maximum_Width >= Bounds.Minimum_Width) and then
      (Bounds.Maximum_Height = 0 or else
       Bounds.Maximum_Height >= Bounds.Minimum_Height));
   type Limits_Request is record
      Surface : Live_Surface_Name := 1;
      Bounds : Window_Bounds;
      Features : Window_Features := [others => False];
   end record;
   type Limits_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Limits_Request;
         when False => null;
      end case;
   end record;
   function Decode_Limits (Wire : Wire_Message) return Limits_Decoding
     with Post => (if Decode_Limits'Result.Valid then
       Valid_Bounds (Decode_Limits'Result.Value.Bounds));
   function Encode_Limits (Item : Limits_Request) return Wire_Message
     with Pre => Valid_Bounds (Item.Bounds);
   type Limits_Result (Status : Status_Code := Invalid_Request) is record
      case Status is
         when Success => Bounds : Window_Bounds; Serial : Unsigned_64;
         when others => null;
      end case;
   end record;
   function Decode_Limits_Result (Wire : Wire_Message) return Limits_Result;
   function Encode_Limits_Result (Item : Limits_Result) return Wire_Message
     with Pre => (if Item.Status = Success then Valid_Bounds (Item.Bounds));
   type Damage_Mode is (Whole_Surface, Damaged_Rectangle);
   type Present_Request is record
      Surface : Live_Surface_Name := 1;
      Area : Rectangle;
   end record;
   --  Both zero extents at origin mean Whole_Surface. Mixed zero extents and
   --  empty rectangles at another origin are invalid, not full damage.
   function Mode (Item : Present_Request) return Damage_Mode is
     (if Item.Area.Width = 0 and Item.Area.Height = 0 then Whole_Surface
       else Damaged_Rectangle);
   function Valid_Damage (Area : Rectangle) return Boolean is
     ((Area.Width > 0 and Area.Height > 0) or else
      (Area.Width = 0 and Area.Height = 0 and Area.X = 0 and Area.Y = 0));
   type Present_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Present_Request;
         when False => null;
      end case;
   end record;
   function Decode_Present (Wire : Wire_Message) return Present_Decoding
     with Annotate => (GNATprove, Inline_For_Proof);
   function Encode_Present (Item : Present_Request) return Wire_Message
     with Pre => Valid_Damage (Item.Area);

   --  Fixed BGRA8888 attachment. Other formats need explicit schema support.
   Maximum_Buffer_Bytes : constant := 16 * 1024 * 1024;
   subtype Buffer_Pitch is Natural range 0 .. Maximum_Buffer_Bytes;
   type Buffer_Layout is record
      Width, Height : Positive_Extent := 1;
      Pitch : Buffer_Pitch := 4;
   end record;
   function Byte_Length (Layout : Buffer_Layout) return Unsigned_64 is
     (Unsigned_64 (Layout.Pitch) * Unsigned_64 (Layout.Height));
   function Valid_Layout (Layout : Buffer_Layout) return Boolean is
     (Layout.Pitch >= Natural (Layout.Width) * 4 and then
      Layout.Pitch <= Maximum_Buffer_Bytes / Natural (Layout.Height));
   type Attachment_Request is record
      Surface : Live_Surface_Name := 1;
      Grant : CuBit.Grant_References.Reference;
      Layout : Buffer_Layout;
   end record;
   type Attachment_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Attachment_Request;
         when False => null;
      end case;
   end record;
   function Decode_Attachment (Wire : Wire_Message)
      return Attachment_Decoding
     with Post => (if Decode_Attachment'Result.Valid then
       Valid_Layout (Decode_Attachment'Result.Value.Layout));
   function Encode_Attachment (Item : Attachment_Request) return Wire_Message
     with Pre => Valid_Layout (Item.Layout);

   function Clip (Area : Rectangle; Width, Height : Natural) return Rectangle
     with Post => Natural (Clip'Result.X) <= Width and then
       Natural (Clip'Result.Y) <= Height and then
       Natural (Clip'Result.Width) <= Width - Natural (Clip'Result.X) and then
       Natural (Clip'Result.Height) <=
         Height - Natural (Clip'Result.Y) and then
       Clip'Result.Width <= Area.Width and then
       Clip'Result.Height <= Area.Height;

   function Surface_Access
     (Exists : Boolean; Owner, Caller : Unsigned_64) return Status_Code
     with Post =>
       (Surface_Access'Result = Success) =
         (Exists and Owner = Caller and Caller /= 0);
end CuBit.Desktop_Protocol;
