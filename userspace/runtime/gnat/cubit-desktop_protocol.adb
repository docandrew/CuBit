pragma Ada_2022;
package body CuBit.Desktop_Protocol with SPARK_Mode is
   Word_Radix : constant Unsigned_64 := 2 ** 32;
   function Decode_Operation (Label : Unsigned_32) return Operation_Decoding is
   begin
      for Item in Operation loop
         if Label = Code (Item) then
            return (True, Item);
         end if;
      end loop;
      return (Valid => False);
   end Decode_Operation;

   function Header
     (Wire : Wire_Message; Kind : Operation; Length : Unsigned_8 := 4)
      return Boolean is
     (Wire.Label = Code (Kind) and Wire.Length = Length and Wire.Flags = 0
       and Wire.Reserved = 0)
     with Annotate => (GNATprove, Inline_For_Proof);

   function Decode_Create (Wire : Wire_Message) return Create_Decoding is
     (if not Header (Wire, Create_Surface) or else Wire.Words (3) /= 0 or else
        Wire.Words (0) > Unsigned_64 (Pixel_Extent'Last) or else
        Wire.Words (1) > Unsigned_64 (Pixel_Extent'Last) or else
        Wire.Words (2) > 2
      then (Valid => False)
      else
        (True, (Pixel_Extent (Wire.Words (0)), Pixel_Extent (Wire.Words (1)),
                (case Wire.Words (2) is
                   when 0 => Plain_Surface,
                   when 1 => Shell_Surface,
                   when others => Window_Surface))));

   function Encode_Create (Item : Create_Request) return Wire_Message is
     (Label => Code (Create_Surface), Length => 4,
      Words => [Unsigned_64 (Item.Width), Unsigned_64 (Item.Height),
                Surface_Kind'Enum_Rep (Item.Kind), 0], others => <>);

   function Decode_Creation_Result
     (Wire : Wire_Message) return Creation_Result is
     (if Header (Wire, Create_Surface, 2) and then Wire.Words (0) = 0 and then
        Wire.Words (2) = 0 and then Wire.Words (3) = 0
      then
         (case Wire.Words (1) is
            when 1 => (Status => Denied),
            when 2 => (Status => Bad_Object),
            when 3 => (Status => Bad_State),
            when 5 => (Status => Unsupported),
            when 6 => (Status => Resources_Exhausted),
            when others => (Status => Invalid_Request))
      elsif Header (Wire, Create_Surface) and then Wire.Words (0) /= 0 and then
        Wire.Words (1) in 1 .. Unsigned_64 (Pixel_Extent'Last) and then
        Wire.Words (2) in 1 .. Unsigned_64 (Pixel_Extent'Last) and then
          Wire.Words (3) /= 0
      then
         (Success, Live_Surface_Name (Wire.Words (0)),
          Positive_Extent (Wire.Words (1)),
          Positive_Extent (Wire.Words (2)), Wire.Words (3))
      else (Status => Invalid_Request));

   function Encode_Creation_Result
     (Item : Creation_Result) return Wire_Message is
   begin
      if Item.Status = Success then
         return (Label => Code (Create_Surface), Length => 4,
                 Words => [Unsigned_64 (Item.Surface),
                           Unsigned_64 (Item.Width),
                           Unsigned_64 (Item.Height), Item.Serial],
                 others => <>);
      else
         return (Label => Code (Create_Surface), Length => 2,
                 Words => [0, Status_Code'Enum_Rep (Item.Status), 0, 0],
                 others => <>);
      end if;
   end Encode_Creation_Result;

   function Decode_Present (Wire : Wire_Message) return Present_Decoding is
     (declare
      X : constant Unsigned_64 := Wire.Words (1) mod Word_Radix;
      Y : constant Unsigned_64 := Wire.Words (1) / Word_Radix;
      W : constant Unsigned_64 := Wire.Words (2) mod Word_Radix;
      H : constant Unsigned_64 := Wire.Words (2) / Word_Radix;
   begin
     (if not Header (Wire, Present_Surface) or else Wire.Words (0) = 0 or else
        Wire.Words (3) /= 0 or else
        X > Unsigned_64 (Pixel_Coordinate'Last) or else
        Y > Unsigned_64 (Pixel_Coordinate'Last) or else
        W > Unsigned_64 (Pixel_Extent'Last) or else
        H > Unsigned_64 (Pixel_Extent'Last) or else
        not ((W > 0 and H > 0) or else (W = 0 and H = 0 and X = 0 and Y = 0))
      then (Valid => False)
      else (True, (Live_Surface_Name (Wire.Words (0)),
                   (Pixel_Coordinate (X), Pixel_Coordinate (Y),
                    Pixel_Extent (W), Pixel_Extent (H))))));

   function Encode_Present (Item : Present_Request) return Wire_Message is
     (Label => Code (Present_Surface), Length => 4,
      Words => [Unsigned_64 (Item.Surface),
                Unsigned_64 (Item.Area.X) + Unsigned_64 (Item.Area.Y) *
                  Word_Radix,
                Unsigned_64 (Item.Area.Width) +
                  Unsigned_64 (Item.Area.Height) * Word_Radix, 0],
      others => <>);

   function Decode_Attachment (Wire : Wire_Message)
      return Attachment_Decoding
   is
      Width : constant Unsigned_64 := Wire.Words (3) mod 2 ** 16;
      Height : constant Unsigned_64 :=
        (Wire.Words (3) / 2 ** 16) mod 2 ** 16;
      Pitch : constant Unsigned_64 := Wire.Words (3) / Word_Radix;
      Layout : Buffer_Layout;
   begin
      if not Header (Wire, Attach_Buffer) or else Wire.Words (0) = 0 or else
        Wire.Words (1) > Grant_References.Maximum_Slot or else
        Wire.Words (2) not in 1 .. Grant_References.Maximum_Generation or else
        Width = 0 or else Height = 0 or else Pitch > Maximum_Buffer_Bytes
      then
         return (Valid => False);
      end if;
      Layout := (Positive_Extent (Width), Positive_Extent (Height),
                 Buffer_Pitch (Pitch));
      if not Valid_Layout (Layout) then
         return (Valid => False);
      end if;
      return (True, (Live_Surface_Name (Wire.Words (0)),
                     (Wire.Words (1), Wire.Words (2)), Layout));
   end Decode_Attachment;

   function Encode_Attachment (Item : Attachment_Request) return Wire_Message
   is
     (Label => Code (Attach_Buffer), Length => 4,
      Words => [Unsigned_64 (Item.Surface), Item.Grant.slot,
                Item.Grant.generation,
                Unsigned_64 (Item.Layout.Width) +
                  Unsigned_64 (Item.Layout.Height) * 2 ** 16 +
                  Unsigned_64 (Item.Layout.Pitch) * Word_Radix], others => <>);

   function Clip
     (Area : Rectangle; Width, Height : Natural) return Rectangle is
      X : constant Natural := Natural'Min (Natural (Area.X), Width);
      Y : constant Natural := Natural'Min (Natural (Area.Y), Height);
   begin
      return (Pixel_Coordinate (X), Pixel_Coordinate (Y),
              Pixel_Extent (Natural'Min (Natural (Area.Width), Width - X)),
              Pixel_Extent (Natural'Min (Natural (Area.Height), Height - Y)));
   end Clip;

   function Surface_Access (Exists : Boolean; Owner, Caller : Unsigned_64)
     return Status_Code is
   begin
      if not Exists then
         return Bad_Object;
      elsif Caller = 0 or else Owner /= Caller then
         return Denied;
      else
         return Success;
      end if;
   end Surface_Access;
end CuBit.Desktop_Protocol;
