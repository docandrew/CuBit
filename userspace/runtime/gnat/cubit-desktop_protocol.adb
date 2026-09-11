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

   function Decode_Status (Wire : Wire_Message; Expected : Operation)
      return Status_Code is
   begin
      if not Header (Wire, Expected, 1) or else Wire.Words (1) /= 0 or else
        Wire.Words (2) /= 0 or else Wire.Words (3) /= 0
      then
         return Invalid_Request;
      end if;
      for Status in Status_Code loop
         if Wire.Words (0) = Status_Code'Enum_Rep (Status) then
            return Status;
         end if;
      end loop;
      return Invalid_Request;
   end Decode_Status;

   function Encode_Status (Kind : Operation; Status : Status_Code)
      return Wire_Message is
     (Code (Kind), 1, 0, 0, [Status_Code'Enum_Rep (Status), 0, 0, 0]);

   function Decode_Destroy (Wire : Wire_Message) return Destroy_Decoding is
     (if not Header (Wire, Destroy_Surface) or else Wire.Words (0) = 0 or else
       Wire.Words (1) /= 0 or else Wire.Words (2) /= 0 or else
       Wire.Words (3) /= 0
      then (Valid => False)
      else (True, (Surface => Live_Surface_Name (Wire.Words (0)))));

   function Encode_Destroy (Item : Destroy_Request) return Wire_Message is
     (Code (Destroy_Surface), 4, 0, 0, [Unsigned_64 (Item.Surface), 0, 0, 0]);

   function Decode_Cursor (Wire : Wire_Message) return Cursor_Decoding is
   begin
      if not Header (Wire, Set_Pointer_Cursor) or else
        Wire.Words (0) = 0 or else Wire.Words (2) /= 0 or else
        Wire.Words (3) /= 0
      then
         return (Valid => False);
      end if;
      for Style in Cursor_Style loop
         if Wire.Words (1) = Cursor_Style'Enum_Rep (Style) then
            return (True, (Live_Surface_Name (Wire.Words (0)), Style));
         end if;
      end loop;
      return (Valid => False);
   end Decode_Cursor;

   function Encode_Cursor (Item : Cursor_Request) return Wire_Message is
     (Code (Set_Pointer_Cursor), 4, 0, 0,
      [Unsigned_64 (Item.Surface), Cursor_Style'Enum_Rep (Item.Style), 0, 0]);

   function Make_Title (Text : String) return Inline_Title is
      Size : constant Title_Length :=
        Natural'Min (Text'Length, Title_Length'Last);
      Result : Inline_Title (Size) := (Size, [others => Character'Val (0)]);
   begin
      for Index in Result.Text'Range loop
         Result.Text (Index) := Text (Text'First + (Index - 1));
      end loop;
      return Result;
   end Make_Title;

   function Decode_Title (Wire : Wire_Message) return Title_Decoding is
      Size : constant Unsigned_64 := Shift_Right (Wire.Words (3), 56);
   begin
      if not Header (Wire, Set_Window_Title) or else
        Wire.Words (0) = 0 or else Size > Unsigned_64 (Title_Length'Last)
      then
         return (Valid => False);
      end if;
      declare
         Title : Inline_Title (Title_Length (Size)) :=
           (Title_Length (Size), [others => Character'Val (0)]);
         Byte : Unsigned_64;
      begin
         for Index in 1 .. Title_Length'Last loop
            Byte := Shift_Right (Wire.Words (1 + (Index - 1) / 8),
                                 ((Index - 1) mod 8) * 8) and 255;
            if Index <= Title.Length then
               Title.Text (Index) := Character'Val (Byte);
            elsif Byte /= 0 then
               --  No hidden payload after the declared title.
               return (Valid => False);
            end if;
         end loop;
         return (True, (Live_Surface_Name (Wire.Words (0)), Title));
      end;
   end Decode_Title;

   function Encode_Title (Item : Title_Request) return Wire_Message is
      Wire : Wire_Message :=
        (Code (Set_Window_Title), 4, 0, 0,
         [Unsigned_64 (Item.Surface), 0, 0,
          Shift_Left (Unsigned_64 (Item.Title.Length), 56)]);
   begin
      for Index in Item.Title.Text'Range loop
         Wire.Words (1 + (Index - 1) / 8) :=
           Wire.Words (1 + (Index - 1) / 8) or
             Shift_Left (Unsigned_64 (Character'Pos (Item.Title.Text (Index))),
                         ((Index - 1) mod 8) * 8);
      end loop;
      return Wire;
   end Encode_Title;

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

   subtype Failure_Status is Status_Code range Denied .. Resources_Exhausted;
   function Failure (Wire : Wire_Message; Kind : Operation)
      return Failure_Status;
   function Failure (Wire : Wire_Message; Kind : Operation)
      return Failure_Status is
   begin
      if not Header (Wire, Kind, 1) or else Wire.Words (1) /= 0 or else
        Wire.Words (2) /= 0 or else Wire.Words (3) /= 0
      then
         return Invalid_Request;
      end if;
      case Wire.Words (0) is
         when 1 => return Denied;
         when 2 => return Bad_Object;
         when 3 => return Bad_State;
         when 5 => return Unsupported;
         when 6 => return Resources_Exhausted;
         when others => return Invalid_Request;
      end case;
   end Failure;

   function Revision_Word (Revision : Protocol_Revision) return Unsigned_64 is
     (Unsigned_64 (Revision.Major) +
      Unsigned_64 (Revision.Minor) * Word_Radix);

   function Decode_Hello (Wire : Wire_Message) return Hello_Decoding is
     (if not Header (Wire, Hello) or else Wire.Words (1) /= 0 or else
       Wire.Words (2) /= 0 or else Wire.Words (3) /= 0
      then (Valid => False)
      else (True, (Unsigned_32 (Wire.Words (0) mod Word_Radix),
                   Unsigned_32 (Wire.Words (0) / Word_Radix))));

   function Encode_Hello (Revision : Protocol_Revision) return Wire_Message is
     (Code (Hello), 4, 0, 0, [Revision_Word (Revision), 0, 0, 0]);

   function Valid_Empty_Request
     (Wire : Wire_Message; Kind : Empty_Session_Operation) return Boolean is
     (Header (Wire, Kind) and then Wire.Words = [0, 0, 0, 0]);

   function Encode_Empty_Request (Kind : Empty_Session_Operation)
      return Wire_Message is
     (Code (Kind), 4, 0, 0, [others => 0]);

   function Session_Failure (Wire : Wire_Message; Kind : Operation)
      return Failure_Status is
     (if Header (Wire, Kind, 2) and then Wire.Words (0) = 0 and then
       Wire.Words (2) = 0 and then Wire.Words (3) = 0
      then Failure ((Code (Kind), 1, 0, 0, [Wire.Words (1), 0, 0, 0]), Kind)
      else Invalid_Request);

   function Decode_Hello_Result (Wire : Wire_Message) return Hello_Result is
     (if Header (Wire, Hello) and then Wire.Words (0) /= 0 and then
       Wire.Words (1) = 0 and then
       Wire.Words (2) in 1 .. Unsigned_64 (Surface_Capacity'Last) and then
       Wire.Words (3) = Revision_Word (Current_Revision)
      then (Success, Session_Identifier (Wire.Words (0)),
            Surface_Capacity (Wire.Words (2)))
      else (Status => Session_Failure (Wire, Hello)));

   function Encode_Hello_Result (Item : Hello_Result) return Wire_Message is
     (if Item.Status = Success then
        (Code (Hello), 4, 0, 0,
         [Unsigned_64 (Item.Session), 0, Unsigned_64 (Item.Capacity),
          Revision_Word (Current_Revision)])
      else (Code (Hello), 2, 0, 0,
            [0, Status_Code'Enum_Rep (Item.Status), 0, 0]));

   function Decode_Information_Result (Wire : Wire_Message)
      return Information_Result is
     (if Header (Wire, Get_Information) and then
       Wire.Words (0) in 1 .. Unsigned_64 (Positive_Extent'Last) and then
       Wire.Words (1) in 1 .. Unsigned_64 (Positive_Extent'Last) and then
       Wire.Words (2) = Pixel_Format'Enum_Rep (BGRA_8888) and then
       Wire.Words (3) in 1 .. Unsigned_64 (Display_Scale'Last)
      then (Success, Positive_Extent (Wire.Words (0)),
            Positive_Extent (Wire.Words (1)), BGRA_8888,
            Display_Scale (Wire.Words (3)))
      else (Status => Session_Failure (Wire, Get_Information)));

   function Encode_Information_Result (Item : Information_Result)
      return Wire_Message is
     (if Item.Status = Success then
        (Code (Get_Information), 4, 0, 0,
         [Unsigned_64 (Item.Width), Unsigned_64 (Item.Height),
          Pixel_Format'Enum_Rep (Item.Format), Unsigned_64 (Item.Scale)])
      else (Code (Get_Information), 2, 0, 0,
            [0, Status_Code'Enum_Rep (Item.Status), 0, 0]));

   function Decode_Input_Request (Wire : Wire_Message)
      return Input_Request_Decoding is
   begin
      if Wire.Words (0) = 0 or else Wire.Words (3) /= 0 then
         return (Valid => False);
      elsif Header (Wire, Poll_Input) and then Wire.Words (2) = 0 then
         return (True, (Poll_Input, Live_Surface_Name (Wire.Words (0)),
                        Wire.Words (1)));
      elsif Header (Wire, Wait_Input) then
         return (True, (Wait_Input, Live_Surface_Name (Wire.Words (0)),
                        Wire.Words (1), Wire.Words (2)));
      else
         return (Valid => False);
      end if;
   end Decode_Input_Request;

   function Encode_Input_Request (Item : Input_Request) return Wire_Message is
     (Code (Item.Kind), 4, 0, 0,
      [Unsigned_64 (Item.Surface), Item.After_Serial,
       (if Item.Kind = Wait_Input then Item.Deadline else 0), 0]);

   function Valid_Input_Envelope (Item : Input_Envelope) return Boolean is
     (case Item.Kind is
         when No_Input =>
           Item.Payload0 = 0 and Item.Payload1 = 0 and not Item.More_Pending,
         when Key_Pressed | Key_Released =>
           Item.Payload0 <= 127 and Item.Payload1 <= 15,
         when Text_Entered => Item.Payload0 <= 255 and Item.Payload1 = 0,
         when Surface_Configured =>
           Item.Payload0 <= Unsigned_64 (Pixel_Extent'Last) and
           Item.Payload1 <= Unsigned_64 (Pixel_Extent'Last),
         when Pointer_Moved | Pointer_Pressed | Pointer_Released |
              Wheel_Turned | Input_Resynchronized =>
           Item.Payload0 mod Word_Radix <= Unsigned_64 (Pixel_Coordinate'Last)
           and Item.Payload0 / Word_Radix <=
             Unsigned_64 (Pixel_Coordinate'Last) and
           (case Item.Kind is
               when Pointer_Moved | Pointer_Pressed | Pointer_Released =>
                 Item.Payload1 < Word_Radix,
               when Input_Resynchronized => Item.Payload1 / Word_Radix <= 15,
               when others => True));

   function Decode_Input_Result
     (Wire : Wire_Message; Expected : Input_Operation) return Input_Result
   is
      Item : Input_Envelope;
   begin
      if Wire.Label /= Code (Expected) or else Wire.Length /= 4 or else
        Wire.Flags > More_Pending_Flag or else Wire.Reserved /= 0
      then
         return (Status => Failure (Wire, Expected));
      end if;
      for Kind in Input_Event_Kind loop
         if Wire.Words (0) = Input_Event_Kind'Enum_Rep (Kind) then
            Item := (Kind, Wire.Words (1), Wire.Words (2), Wire.Words (3),
                     Wire.Flags = More_Pending_Flag);
            if Valid_Input_Envelope (Item) then
               return (Success, Item);
            end if;
            return (Status => Invalid_Request);
         end if;
      end loop;
      return (Status => Invalid_Request);
   end Decode_Input_Result;

   function Encode_Input_Reply (Kind : Input_Operation; Item : Input_Envelope)
      return Wire_Message is
     (Code (Kind), 4, (if Item.More_Pending then More_Pending_Flag else 0), 0,
      [Input_Event_Kind'Enum_Rep (Item.Kind), Item.Serial,
       Item.Payload0, Item.Payload1]);

   function Decode_Resize (Wire : Wire_Message) return Resize_Decoding is
     (if not Header (Wire, Resize_Surface) or else Wire.Words (0) = 0 or else
       Wire.Words (1) > Unsigned_64 (Pixel_Extent'Last) or else
       Wire.Words (2) > Unsigned_64 (Pixel_Extent'Last) or else
       Wire.Words (3) /= 0
      then (Valid => False)
      else (True, (Live_Surface_Name (Wire.Words (0)),
                    Pixel_Extent (Wire.Words (1)),
                    Pixel_Extent (Wire.Words (2)))));

   function Encode_Resize (Item : Resize_Request) return Wire_Message is
     (Code (Resize_Surface), 4, 0, 0,
      [Unsigned_64 (Item.Surface), Unsigned_64 (Item.Width),
       Unsigned_64 (Item.Height), 0]);

   function Decode_Resize_Result (Wire : Wire_Message) return Resize_Result is
     (if Header (Wire, Resize_Surface) and then Wire.Words (0) = 0 and then
       Wire.Words (1) <= Unsigned_64 (Pixel_Extent'Last) and then
       Wire.Words (2) <= Unsigned_64 (Pixel_Extent'Last)
      then (Success, Pixel_Extent (Wire.Words (1)),
            Pixel_Extent (Wire.Words (2)), Wire.Words (3))
      else (Status => Failure (Wire, Resize_Surface)));

   function Encode_Resize_Result (Item : Resize_Result) return Wire_Message is
     (if Item.Status = Success then
        (Code (Resize_Surface), 4, 0, 0,
         [0, Unsigned_64 (Item.Width), Unsigned_64 (Item.Height), Item.Serial])
      else (Code (Resize_Surface), 1, 0, 0,
            [Status_Code'Enum_Rep (Item.Status), 0, 0, 0]));

   function Feature_Bits (Features : Window_Features) return Unsigned_64 is
      Result : Unsigned_64 := 0;
   begin
      for Feature in Window_Feature loop
         if Features (Feature) then
            Result := Result or Window_Feature'Enum_Rep (Feature);
         end if;
      end loop;
      return Result;
   end Feature_Bits;

   function Bounds_Fit (Minimum, Maximum : Unsigned_64) return Boolean is
     (Minimum mod Word_Radix <= Unsigned_64 (Pixel_Extent'Last) and
      Minimum / Word_Radix <= Unsigned_64 (Pixel_Extent'Last) and
      Maximum mod Word_Radix <= Unsigned_64 (Pixel_Extent'Last) and
      Maximum / Word_Radix <= Unsigned_64 (Pixel_Extent'Last));

   function Unpack_Bounds (Minimum, Maximum : Unsigned_64)
      return Window_Bounds
   is
     (Pixel_Extent (Minimum mod Word_Radix),
      Pixel_Extent (Minimum / Word_Radix),
      Pixel_Extent (Maximum mod Word_Radix),
      Pixel_Extent (Maximum / Word_Radix))
     with Pre => Bounds_Fit (Minimum, Maximum);

   function Decode_Limits (Wire : Wire_Message) return Limits_Decoding is
      Item : Limits_Request;
   begin
      if not Header (Wire, Set_Window_Limits) or else
        Wire.Words (0) = 0 or else
        not Bounds_Fit (Wire.Words (1), Wire.Words (2)) or else
        Wire.Words (3) > Feature_Bits ([others => True])
      then
         return (Valid => False);
      end if;
      Item.Surface := Live_Surface_Name (Wire.Words (0));
      Item.Bounds := Unpack_Bounds (Wire.Words (1), Wire.Words (2));
      if not Valid_Bounds (Item.Bounds) then
         return (Valid => False);
      end if;
      for Feature in Window_Feature loop
         Item.Features (Feature) :=
           (Wire.Words (3) and Window_Feature'Enum_Rep (Feature)) /= 0;
      end loop;
      return (True, Item);
   end Decode_Limits;

   function Encode_Limits (Item : Limits_Request) return Wire_Message is
     (Code (Set_Window_Limits), 4, 0, 0,
      [Unsigned_64 (Item.Surface),
       Unsigned_64 (Item.Bounds.Minimum_Width) +
         Unsigned_64 (Item.Bounds.Minimum_Height) * Word_Radix,
       Unsigned_64 (Item.Bounds.Maximum_Width) +
         Unsigned_64 (Item.Bounds.Maximum_Height) * Word_Radix,
       Feature_Bits (Item.Features)]);

   function Decode_Limits_Result (Wire : Wire_Message) return Limits_Result is
      Bounds : Window_Bounds;
   begin
      if not Header (Wire, Set_Window_Limits) or else
        Wire.Words (0) /= 0 or else
        not Bounds_Fit (Wire.Words (1), Wire.Words (2))
      then
         return (Status => Failure (Wire, Set_Window_Limits));
      end if;
      Bounds := Unpack_Bounds (Wire.Words (1), Wire.Words (2));
      if not Valid_Bounds (Bounds) then
         return (Status => Invalid_Request);
      end if;
      return (Success, Bounds, Wire.Words (3));
   end Decode_Limits_Result;

   function Encode_Limits_Result (Item : Limits_Result) return Wire_Message is
   begin
      if Item.Status = Success then
         return (Code (Set_Window_Limits), 4, 0, 0,
           [0, Unsigned_64 (Item.Bounds.Minimum_Width) +
                Unsigned_64 (Item.Bounds.Minimum_Height) * Word_Radix,
            Unsigned_64 (Item.Bounds.Maximum_Width) +
                Unsigned_64 (Item.Bounds.Maximum_Height) * Word_Radix,
            Item.Serial]);
      else
         return (Code (Set_Window_Limits), 1, 0, 0,
           [Status_Code'Enum_Rep (Item.Status), 0, 0, 0]);
      end if;
   end Encode_Limits_Result;

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
