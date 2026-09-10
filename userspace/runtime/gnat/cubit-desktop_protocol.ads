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
     with Post => Decode_Create (Encode_Create'Result) =
       (Valid => True, Value => Item);

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
