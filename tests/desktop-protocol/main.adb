with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Interfaces.C;
with CuBit.Desktop_Protocol; use CuBit.Desktop_Protocol;
with CuBit.Display_Protocol;
procedure Main is
   use type Interfaces.C.int;
   function C_Input_Valid (Expected, Label : Unsigned_32;
                          Length, Flags : Unsigned_8; Reserved : Unsigned_16;
                          W0, W1, W2, W3 : Unsigned_64) return Interfaces.C.int
     with Import, Convention => C, External_Name => "cubit_test_input_reply_valid";
   procedure Compare_Input_Decoders (Wire : Wire_Message; Expected : Input_Operation) is
   begin
      pragma Assert ((Decode_Input_Result (Wire, Expected).Status = Success) =
        (C_Input_Valid (Code (Expected), Wire.Label, Wire.Length, Wire.Flags,
                       Wire.Reserved, Wire.Words (0), Wire.Words (1),
                       Wire.Words (2), Wire.Words (3)) = 1));
   end Compare_Input_Decoders;
   Wire : Wire_Message;
   Created : Creation_Result;
   Area : Rectangle;
   Present : Present_Request;
   Attachment : Attachment_Request;
   Coordinates : constant array (Positive range 1 .. 4) of Pixel_Coordinate :=
     [0, 1, 32_767, Pixel_Coordinate'Last];
   Extents : constant array (Positive range 1 .. 4) of Positive_Extent :=
     [1, 2, 32_767, Positive_Extent'Last];
begin
   declare
      package DSP renames CuBit.Display_Protocol;
      use type DSP.Attachment_Decoding;
      Item : DSP.Attachment_Request := ((0, 1), (800, 600, 3200));
      Canonical : Wire_Message := DSP.Encode_Attachment (Item);
   begin
      pragma Assert (DSP.Decode_Attachment (Canonical) = (True, Item));
      pragma Assert (DSP.Valid_Open_Session (DSP.Encode_Open_Session));
      for Length in Unsigned_8 loop
         Wire := DSP.Encode_Open_Session; Wire.Length := Length;
         pragma Assert (DSP.Valid_Open_Session (Wire) = (Length = 4));
         Wire := DSP.Encode_Open_Session; Wire.Flags := Length;
         pragma Assert (DSP.Valid_Open_Session (Wire) = (Length = 0));
      end loop;
      Wire := DSP.Encode_Open_Session; Wire.Reserved := 1;
      pragma Assert (not DSP.Valid_Open_Session (Wire));
      for Field in Payload'Range loop
         Wire := DSP.Encode_Open_Session; Wire.Words (Field) := 1;
         pragma Assert (not DSP.Valid_Open_Session (Wire));
      end loop;
      for Op in DSP.Operation loop
         Wire := DSP.Encode_Open_Session; Wire.Label := DSP.Code (Op);
         pragma Assert (DSP.Valid_Open_Session (Wire) =
           (Wire.Label = DSP.Code (DSP.Open_Presentation_Session)));
      end loop;
      for Op in DSP.Lease_Operation loop
         Wire := DSP.Encode_Lease_Request (Op);
         pragma Assert (DSP.Valid_Lease_Request (Wire, Op));
         for Length in Unsigned_8 loop
            Wire.Length := Length;
            pragma Assert (DSP.Valid_Lease_Request (Wire, Op) = (Length = 4));
         end loop;
         for Field in Payload'Range loop
            Wire := DSP.Encode_Lease_Request (Op); Wire.Words (Field) := 1;
            pragma Assert (not DSP.Valid_Lease_Request (Wire, Op));
         end loop;
      end loop;
      for Length in Unsigned_8 loop
         Wire := Canonical; Wire.Length := Length;
         pragma Assert (DSP.Decode_Attachment (Wire).Valid = (Length = 4));
         Wire := Canonical; Wire.Flags := Length;
         pragma Assert (DSP.Decode_Attachment (Wire).Valid = (Length = 0));
      end loop;
      Wire := Canonical; Wire.Reserved := 1;
      pragma Assert (not DSP.Decode_Attachment (Wire).Valid);
      for Op in DSP.Operation loop
         Wire := Canonical; Wire.Label := DSP.Code (Op);
         pragma Assert (DSP.Decode_Attachment (Wire).Valid =
           (Wire.Label = DSP.Code (DSP.Attach_Buffer)));
      end loop;
      Wire := Canonical; Wire.Words (0) := 4096;
      pragma Assert (not DSP.Decode_Attachment (Wire).Valid);
      Wire := Canonical; Wire.Words (1) := 0;
      pragma Assert (not DSP.Decode_Attachment (Wire).Valid);
      Wire.Words (1) := 2 ** 32;
      pragma Assert (not DSP.Decode_Attachment (Wire).Valid);
      Wire := Canonical; Wire.Words (2) := 0;
      pragma Assert (not DSP.Decode_Attachment (Wire).Valid);
      Wire.Words (2) := 65_536 + 600 * 2 ** 32;
      pragma Assert (not DSP.Decode_Attachment (Wire).Valid);
      Wire.Words (2) := 800 + 65_536 * 2 ** 32;
      pragma Assert (not DSP.Decode_Attachment (Wire).Valid);
      Wire := Canonical; Wire.Words (3) := 3199;
      pragma Assert (not DSP.Decode_Attachment (Wire).Valid);
      Wire.Words (3) := Unsigned_64'Last;
      pragma Assert (not DSP.Decode_Attachment (Wire).Valid);
      Item := ((4095, 2 ** 32 - 1), (4096, 1024, 16_384));
      Canonical := DSP.Encode_Attachment (Item);
      pragma Assert (DSP.Decode_Attachment (Canonical) = (True, Item));
      Wire := Canonical; Wire.Words (3) := 16_385;
      pragma Assert (not DSP.Decode_Attachment (Wire).Valid);
      -- Every bit of every field is hostile input; accepted forms must be
      -- canonical and retain the complete bounded layout on round trip.
      for Field in Payload'Range loop
         for Bit in 0 .. 63 loop
            Wire := Canonical;
            Wire.Words (Field) := Wire.Words (Field) xor Shift_Left (1, Bit);
            declare
               Decoded : constant DSP.Attachment_Decoding :=
                 DSP.Decode_Attachment (Wire);
            begin
               if Decoded.Valid then
                  pragma Assert (Valid_Layout (Decoded.Value.Layout));
                  pragma Assert (DSP.Encode_Attachment (Decoded.Value) = Wire);
               end if;
            end;
         end loop;
      end loop;
      Put_Line ("PASS: display attachment codec, hostile words and bounded layouts");
   end;
   declare
      package DSP renames CuBit.Display_Protocol;
      use type DSP.Frame_Decoding;
      use type DSP.Frame_Result_Decoding;
      Item : DSP.Frame_Request := (1, 1, (0, 0, 800, 600));
      Canonical : Wire_Message := DSP.Encode_Frame (Item);
   begin
      pragma Assert (DSP.Decode_Frame (Canonical) = (True, Item));
      for X of Coordinates loop
         for Y of Coordinates loop
            for Width of Extents loop
               for Height of Extents loop
                  Item := (DSP.Live_ID'Last, DSP.Live_ID'Last,
                           (X, Y, Width, Height));
                  pragma Assert
                    (DSP.Decode_Frame (DSP.Encode_Frame (Item)) = (True, Item));
               end loop;
            end loop;
         end loop;
      end loop;
      for Length in Unsigned_8 loop
         Wire := Canonical; Wire.Length := Length;
         pragma Assert (DSP.Decode_Frame (Wire).Valid = (Length = 4));
         Wire := Canonical; Wire.Flags := Length;
         pragma Assert (DSP.Decode_Frame (Wire).Valid = (Length = 0));
      end loop;
      Wire := Canonical; Wire.Reserved := 1;
      pragma Assert (not DSP.Decode_Frame (Wire).Valid);
      for Op in DSP.Operation loop
         Wire := Canonical; Wire.Label := DSP.Code (Op);
         pragma Assert (DSP.Decode_Frame (Wire).Valid =
           (Wire.Label = DSP.Code (DSP.Submit_Frame)));
      end loop;
      for Field in 0 .. 1 loop
         Wire := Canonical; Wire.Words (Field) := 0;
         pragma Assert (not DSP.Decode_Frame (Wire).Valid);
      end loop;
      for Field in Payload'Range loop
         for Bit in 0 .. 63 loop
            Wire := Canonical;
            Wire.Words (Field) := Wire.Words (Field) xor Shift_Left (1, Bit);
            declare
               Decoded : constant DSP.Frame_Decoding := DSP.Decode_Frame (Wire);
            begin
               if Decoded.Valid then
                  pragma Assert (DSP.Encode_Frame (Decoded.Value) = Wire);
               end if;
            end;
         end loop;
      end loop;
      -- Outcome and source lifetime are distinct: presentation alone never
      -- authorizes reuse. Even a failed/dropped frame can still hold a loan.
      for Outcome in DSP.Frame_Outcome loop
         for Disposition in DSP.Buffer_Disposition loop
            declare
               Result : constant DSP.Frame_Result :=
                 (DSP.Live_ID'Last, DSP.Live_ID'Last, Outcome, Disposition);
            begin
               Canonical := DSP.Encode_Frame_Result (Result);
               pragma Assert
                 (DSP.Decode_Frame_Result (Canonical) = (True, Result));
               for Field in Payload'Range loop
                  for Bit in 0 .. 63 loop
                     Wire := Canonical;
                     Wire.Words (Field) :=
                       Wire.Words (Field) xor Shift_Left (1, Bit);
                     declare
                        Decoded : constant DSP.Frame_Result_Decoding :=
                          DSP.Decode_Frame_Result (Wire);
                     begin
                        if Decoded.Valid then
                           pragma Assert
                             (DSP.Encode_Frame_Result (Decoded.Value) = Wire);
                        end if;
                     end;
                  end loop;
               end loop;
            end;
         end loop;
      end loop;
      for Length in Unsigned_8 loop
         Wire := Canonical; Wire.Length := Length;
         pragma Assert (DSP.Decode_Frame_Result (Wire).Valid = (Length = 4));
         Wire := Canonical; Wire.Flags := Length;
         pragma Assert (DSP.Decode_Frame_Result (Wire).Valid = (Length = 0));
      end loop;
      Wire := Canonical; Wire.Reserved := 1;
      pragma Assert (not DSP.Decode_Frame_Result (Wire).Valid);
      for Op in DSP.Operation loop
         Wire := Canonical; Wire.Label := DSP.Code (Op);
         pragma Assert (DSP.Decode_Frame_Result (Wire).Valid =
           (Wire.Label = DSP.Code (DSP.Submit_Frame)));
      end loop;
      for Field in 0 .. 1 loop
         Wire := Canonical; Wire.Words (Field) := 0;
         pragma Assert (not DSP.Decode_Frame_Result (Wire).Valid);
      end loop;
      Put_Line ("PASS: asynchronous frame and completion codecs, hostile words");
   end;
   for Op in Operation loop
      pragma Assert (Decode_Operation (Code (Op)) = (True, Op));
   end loop;
   pragma Assert (not Decode_Operation (0).Valid);
   pragma Assert (not Decode_Operation (Unsigned_32'Last).Valid);
   for Kind in Surface_Kind loop
      for Size in Pixel_Extent range 0 .. 32 loop
         Wire := Encode_Create ((Size, Size, Kind));
         pragma Assert (Decode_Create (Wire) = (True, (Size, Size, Kind)));
      end loop;
   end loop;
   Wire := Encode_Create ((Pixel_Extent'Last, Pixel_Extent'Last, Window_Surface));
   pragma Assert (Decode_Create (Wire).Valid);
   Wire.Words (0) := Unsigned_64'Last;
   pragma Assert (not Decode_Create (Wire).Valid);
   Wire := Encode_Create ((800, 600, Window_Surface));
   for Length in Unsigned_8 loop
      Wire.Length := Length;
      pragma Assert (Decode_Create (Wire).Valid = (Length = 4));
   end loop;
   Wire.Length := 4; Wire.Words (2) := 3;
   pragma Assert (not Decode_Create (Wire).Valid);
   Wire.Words (2) := 2; Wire.Words (3) := 1;
   pragma Assert (not Decode_Create (Wire).Valid);
   for Status in Status_Code range Denied .. Resources_Exhausted loop
      Wire := Encode_Creation_Result ((Status => Status));
      pragma Assert (Wire.Words (0) = 0);
      pragma Assert (Decode_Creation_Result (Wire).Status = Status);
   end loop;
   Created := (Success, Live_Surface_Name'Last, Positive_Extent'Last, 1, Unsigned_64'Last);
   pragma Assert (Decode_Creation_Result (Encode_Creation_Result (Created)) = Created);
   for Width of Extents loop
      for Height of Extents loop
         Created := (Success, 1, Width, Height, 1);
         pragma Assert
           (Decode_Creation_Result (Encode_Creation_Result (Created)) = Created);
      end loop;
   end loop;
   Wire := (Code (Create_Surface), 1, 0, 0, [3, 0, 0, 0]); -- old ambiguous failure
   pragma Assert (Decode_Creation_Result (Wire).Status = Invalid_Request);
   Present := (7, (0, 0, 0, 0));
   pragma Assert (Decode_Present (Encode_Present (Present)) = (True, Present));
   for X of Coordinates loop
      for Y of Coordinates loop
         for Width of Extents loop
            for Height of Extents loop
               Present := (Live_Surface_Name'Last, (X, Y, Width, Height));
               pragma Assert
                 (Decode_Present (Encode_Present (Present)) = (True, Present));
            end loop;
         end loop;
      end loop;
   end loop;
   Wire := Encode_Present ((7, (Pixel_Coordinate'Last, 5, Pixel_Extent'Last, 8)));
   pragma Assert (Decode_Present (Wire).Valid);
   Wire.Words (1) := Unsigned_64'Last;
   pragma Assert (not Decode_Present (Wire).Valid);
   Wire := Encode_Present ((7, (0, 0, 5, 8))); Wire.Words (2) := 5;
   pragma Assert (not Decode_Present (Wire).Valid);
   Wire := Encode_Present ((7, (0, 0, 5, 8))); Wire.Flags := 1;
   pragma Assert (not Decode_Present (Wire).Valid);
   for X in Pixel_Coordinate range 0 .. 100 loop
      Area := Clip ((X, 90, 30, 40), 80, 100);
      pragma Assert (Natural (Area.X) + Natural (Area.Width) <= 80);
      pragma Assert (Natural (Area.Y) + Natural (Area.Height) <= 100);
   end loop;
   Area := Clip ((Pixel_Coordinate'Last, Pixel_Coordinate'Last, Pixel_Extent'Last, Pixel_Extent'Last), 0, 0);
   pragma Assert (Area = (0, 0, 0, 0));
   pragma Assert (Surface_Access (True, 7, 7) = Success);
   pragma Assert (Surface_Access (True, 7, 8) = Denied);
   pragma Assert (Surface_Access (False, 7, 7) = Bad_Object);
   pragma Assert (Surface_Access (True, 0, 0) = Denied);
   Attachment := (1, (4095, Unsigned_64 (Unsigned_32'Last)), (1024, 4096, 4096));
   pragma Assert (Valid_Layout (Attachment.Layout));
   pragma Assert (Byte_Length (Attachment.Layout) = Maximum_Buffer_Bytes);
   pragma Assert
     (Decode_Attachment (Encode_Attachment (Attachment)) = (True, Attachment));
   for Width of Extents loop
      Attachment := (7, (0, 1), (Width, 1, Natural (Width) * 4));
      pragma Assert
        (Decode_Attachment (Encode_Attachment (Attachment)) = (True, Attachment));
   end loop;
   Wire := Encode_Attachment (Attachment);
   for Length in Unsigned_8 loop
      Wire.Length := Length;
      pragma Assert (Decode_Attachment (Wire).Valid = (Length = 4));
   end loop;
   Wire := Encode_Attachment (Attachment); Wire.Words (1) := 4096;
   pragma Assert (not Decode_Attachment (Wire).Valid);
   Wire := Encode_Attachment (Attachment); Wire.Words (2) := 0;
   pragma Assert (not Decode_Attachment (Wire).Valid);
   Wire := Encode_Attachment (Attachment); Wire.Words (2) := 2 ** 32;
   pragma Assert (not Decode_Attachment (Wire).Valid);
   Wire := Encode_Attachment (Attachment); Wire.Words (3) := Unsigned_64'Last;
   pragma Assert (not Decode_Attachment (Wire).Valid);
   Attachment := (1, (0, 1), (4, 2, 16));
   Wire := Encode_Attachment (Attachment);
   Wire.Words (3) := 4 + 2 * 2 ** 16 + 15 * 2 ** 32;
   pragma Assert (not Decode_Attachment (Wire).Valid);
   Wire.Words (3) := 1024 + 4097 * 2 ** 16 + 4096 * 2 ** 32;
   pragma Assert (not Decode_Attachment (Wire).Valid);
   Wire := Encode_Attachment (Attachment); Wire.Flags := 1;
   pragma Assert (not Decode_Attachment (Wire).Valid);
   declare
      Resize : Resize_Request := (7, 320, 200);
      Limits : Limits_Request := (7, (120, 80, 640, 480), [others => False]);
      Resized : Resize_Result;
      Applied : Limits_Result;
      Invalid_Statuses : constant array (Positive range 1 .. 3) of Unsigned_64 :=
        [0, 7, Unsigned_64'Last];
      Hostile : constant array (Positive range 1 .. 4) of Unsigned_64 :=
        [65_536, 2 ** 31, 2 ** 32 - 1, Unsigned_64'Last];
   begin
      for Width of Coordinates loop
         for Height of Coordinates loop
            Resize := (7, Pixel_Extent (Width), Pixel_Extent (Height));
            pragma Assert (Decode_Resize (Encode_Resize (Resize)) = (True, Resize));
            Resized := (Success, Resize.Width, Resize.Height, Unsigned_64'Last);
            pragma Assert (Decode_Resize_Result (Encode_Resize_Result (Resized)) = Resized);
            Limits.Bounds := (Resize.Width, Resize.Height, Resize.Width, Resize.Height);
            pragma Assert (Decode_Limits (Encode_Limits (Limits)) = (True, Limits));
            Applied := (Success, Limits.Bounds, Unsigned_64'Last);
            pragma Assert (Decode_Limits_Result (Encode_Limits_Result (Applied)) = Applied);
         end loop;
      end loop;
      Limits.Bounds := (120, 80, 0, 0);
      for Bits in Unsigned_64 range 0 .. 255 loop
         for Feature in Window_Feature loop
            Limits.Features (Feature) := (Bits and Window_Feature'Enum_Rep (Feature)) /= 0;
         end loop;
         pragma Assert (Feature_Bits (Limits.Features) = Bits);
         pragma Assert (Decode_Limits (Encode_Limits (Limits)) = (True, Limits));
      end loop;
      for Length in Unsigned_8 loop
         Wire := Encode_Resize (Resize); Wire.Length := Length;
         pragma Assert (Decode_Resize (Wire).Valid = (Length = 4));
         Wire := Encode_Limits (Limits); Wire.Length := Length;
         pragma Assert (Decode_Limits (Wire).Valid = (Length = 4));
         Wire := Encode_Resize_Result (Resized); Wire.Length := Length;
         pragma Assert ((Decode_Resize_Result (Wire).Status = Success) = (Length = 4));
         Wire := Encode_Limits_Result (Applied); Wire.Length := Length;
         pragma Assert ((Decode_Limits_Result (Wire).Status = Success) = (Length = 4));
      end loop;
      for Bad of Hostile loop
         for Field in 1 .. 2 loop
            Wire := Encode_Resize (Resize); Wire.Words (Field) := Bad;
            pragma Assert (not Decode_Resize (Wire).Valid);
            Wire := Encode_Resize_Result (Resized); Wire.Words (Field) := Bad;
            pragma Assert (Decode_Resize_Result (Wire).Status = Invalid_Request);
            for Half in 0 .. 1 loop
               Wire := Encode_Limits (Limits);
               Wire.Words (Field) := Shift_Left (Bad, Half * 32);
               pragma Assert (not Decode_Limits (Wire).Valid);
               Wire := Encode_Limits_Result (Applied);
               Wire.Words (Field) := Shift_Left (Bad, Half * 32);
               pragma Assert (Decode_Limits_Result (Wire).Status = Invalid_Request);
            end loop;
         end loop;
      end loop;
      Wire := Encode_Resize (Resize); Wire.Words (0) := 0;
      pragma Assert (not Decode_Resize (Wire).Valid);
      Wire := Encode_Limits (Limits); Wire.Words (0) := 0;
      pragma Assert (not Decode_Limits (Wire).Valid);
      Wire := Encode_Resize (Resize); Wire.Words (3) := 1;
      pragma Assert (not Decode_Resize (Wire).Valid);
      Wire := Encode_Limits (Limits); Wire.Words (3) := 256;
      pragma Assert (not Decode_Limits (Wire).Valid);
      Wire := Encode_Limits (Limits); Wire.Words (2) := 119;
      pragma Assert (not Decode_Limits (Wire).Valid);
      Wire.Words (2) := 79 * 2 ** 32;
      pragma Assert (not Decode_Limits (Wire).Valid);
      Wire := Encode_Limits_Result ((Success, Limits.Bounds, 1));
      Wire.Words (2) := 119;
      pragma Assert (Decode_Limits_Result (Wire).Status = Invalid_Request);
      for Op in Operation loop
         Wire := Encode_Resize (Resize); Wire.Label := Code (Op);
         pragma Assert (Decode_Resize (Wire).Valid = (Op = Resize_Surface));
         Wire := Encode_Limits (Limits); Wire.Label := Code (Op);
         pragma Assert (Decode_Limits (Wire).Valid = (Op = Set_Window_Limits));
      end loop;
      for Reserved_Field in 1 .. 2 loop
         Wire := Encode_Resize (Resize);
         if Reserved_Field = 1 then Wire.Flags := 1; else Wire.Reserved := 1; end if;
         pragma Assert (not Decode_Resize (Wire).Valid);
         Wire.Label := Code (Set_Window_Limits);
         pragma Assert (not Decode_Limits (Wire).Valid);
         Wire.Words := [0, 0, 0, 0];
         pragma Assert (Decode_Limits_Result (Wire).Status = Invalid_Request);
         Wire.Label := Code (Resize_Surface);
         pragma Assert (Decode_Resize_Result (Wire).Status = Invalid_Request);
      end loop;
      for Status in Status_Code range Denied .. Resources_Exhausted loop
         pragma Assert (Decode_Resize_Result (Encode_Resize_Result ((Status => Status))).Status = Status);
         pragma Assert (Decode_Limits_Result (Encode_Limits_Result ((Status => Status))).Status = Status);
      end loop;
      for Invalid_Status of Invalid_Statuses loop
         Wire := Encode_Resize_Result ((Status => Denied));
         Wire.Words (0) := Invalid_Status;
         pragma Assert (Decode_Resize_Result (Wire).Status = Invalid_Request);
         Wire.Label := Code (Set_Window_Limits);
         pragma Assert (Decode_Limits_Result (Wire).Status = Invalid_Request);
      end loop;
      Wire := Encode_Resize_Result ((Status => Denied)); Wire.Words (1) := 1;
      pragma Assert (Decode_Resize_Result (Wire).Status = Invalid_Request);
      Wire.Label := Code (Set_Window_Limits);
      pragma Assert (Decode_Limits_Result (Wire).Status = Invalid_Request);
   end;
   declare
      Cursor : Cursor_Request;
      Destroy : constant Destroy_Request := (Surface => Live_Surface_Name'Last);
   begin
      pragma Assert (Decode_Destroy (Encode_Destroy (Destroy)) = (True, Destroy));
      for Style in Cursor_Style loop
         Cursor := (Live_Surface_Name'Last, Style);
         pragma Assert (Decode_Cursor (Encode_Cursor (Cursor)) = (True, Cursor));
      end loop;
      for Value in Unsigned_64 range 0 .. 255 loop
         Wire := Encode_Cursor (Cursor); Wire.Words (1) := Value;
         pragma Assert (Decode_Cursor (Wire).Valid = (Value <= 4));
      end loop;
      Wire.Words (1) := Unsigned_64'Last;
      pragma Assert (not Decode_Cursor (Wire).Valid);
      for Length in Unsigned_8 loop
         Wire := Encode_Cursor (Cursor); Wire.Length := Length;
         pragma Assert (Decode_Cursor (Wire).Valid = (Length = 4));
         Wire := Encode_Destroy (Destroy); Wire.Length := Length;
         pragma Assert (Decode_Destroy (Wire).Valid = (Length = 4));
         Wire := Encode_Status (Destroy_Surface, Success); Wire.Length := Length;
         pragma Assert ((Decode_Status (Wire, Destroy_Surface) = Success) = (Length = 1));
      end loop;
      for Field in 1 .. 3 loop
         Wire := Encode_Destroy (Destroy); Wire.Words (Field) := 1;
         pragma Assert (not Decode_Destroy (Wire).Valid);
         Wire := Encode_Status (Destroy_Surface, Success); Wire.Words (Field) := 1;
         pragma Assert (Decode_Status (Wire, Destroy_Surface) = Invalid_Request);
      end loop;
      for Field in 2 .. 3 loop
         Wire := Encode_Cursor (Cursor); Wire.Words (Field) := 1;
         pragma Assert (not Decode_Cursor (Wire).Valid);
      end loop;
      for Malformation in 1 .. 3 loop
         Wire := Encode_Destroy (Destroy);
         case Malformation is
            when 1 => Wire.Words (0) := 0;
            when 2 => Wire.Flags := 1;
            when 3 => Wire.Reserved := 1;
         end case;
         pragma Assert (not Decode_Destroy (Wire).Valid);
         Wire.Label := Code (Set_Pointer_Cursor);
         pragma Assert (not Decode_Cursor (Wire).Valid);
      end loop;
      for Op in Operation loop
         Wire := Encode_Destroy (Destroy); Wire.Label := Code (Op);
         pragma Assert (Decode_Destroy (Wire).Valid = (Op = Destroy_Surface));
         Wire := Encode_Cursor (Cursor); Wire.Label := Code (Op);
         pragma Assert (Decode_Cursor (Wire).Valid = (Op = Set_Pointer_Cursor));
         for Status in Status_Code loop
            Wire := Encode_Status (Op, Status);
            pragma Assert (Decode_Status (Wire, Op) = Status);
            for Other in Operation loop
               if Other /= Op then
                  pragma Assert (Decode_Status (Wire, Other) = Invalid_Request);
               end if;
            end loop;
         end loop;
      end loop;
      for Value in Unsigned_64 range 7 .. 255 loop
         Wire := Encode_Status (Set_Pointer_Cursor, Success); Wire.Words (0) := Value;
         pragma Assert (Decode_Status (Wire, Set_Pointer_Cursor) = Invalid_Request);
      end loop;
      Wire.Words (0) := Unsigned_64'Last;
      pragma Assert (Decode_Status (Wire, Set_Pointer_Cursor) = Invalid_Request);
      Wire := Encode_Status (Set_Pointer_Cursor, Success); Wire.Flags := 1;
      pragma Assert (Decode_Status (Wire, Set_Pointer_Cursor) = Invalid_Request);
      Wire := Encode_Status (Set_Pointer_Cursor, Success); Wire.Reserved := 1;
      pragma Assert (Decode_Status (Wire, Set_Pointer_Cursor) = Invalid_Request);
   end;
   declare
      Item : Title_Request;
      Decoded : Title_Decoding;
      Sample : String (7 .. 46);
      Empty : String (Integer'Last .. Integer'Last - 1);
   begin
      for Index in Sample'Range loop
         Sample (Index) := Character'Val (Index);
      end loop;
      pragma Assert (Make_Title (Empty) = (0, ""));
      pragma Assert (Make_Title (Sample).Text = Sample (7 .. 29));
      for Size in Title_Length loop
         Item := (Live_Surface_Name'Last, Make_Title (Sample (7 .. 6 + Size)));
         pragma Assert (Item.Title.Length = Size);
         pragma Assert (Decode_Title (Encode_Title (Item)) = (True, Item));
         for Padding in Size + 1 .. Title_Length'Last loop
            Wire := Encode_Title (Item);
            Wire.Words (1 + (Padding - 1) / 8) :=
              Wire.Words (1 + (Padding - 1) / 8) or
                Shift_Left (1, ((Padding - 1) mod 8) * 8);
            pragma Assert (not Decode_Title (Wire).Valid);
         end loop;
      end loop;
      -- Exercise every byte value, including NUL and high-bit bytes, across
      -- both word boundaries and the byte adjoining the length field.
      for Byte in Character loop
         Item := (1, (23, [others => Byte]));
         Decoded := Decode_Title (Encode_Title (Item));
         pragma Assert (Decoded = (True, Item));
      end loop;
      Item := (1, Make_Title ("abc"));
      pragma Assert (Encode_Title (Item).Words = [1, 16#63_62_61#, 0, 3 * 2 ** 56]);
      for Length in Unsigned_8 loop
         Wire := Encode_Title (Item); Wire.Length := Length;
         pragma Assert (Decode_Title (Wire).Valid = (Length = 4));
      end loop;
      for Length in Unsigned_64 range 24 .. 255 loop
         Wire := Encode_Title (Item); Wire.Words (3) := Shift_Left (Length, 56);
         pragma Assert (not Decode_Title (Wire).Valid);
      end loop;
      for Op in Operation loop
         Wire := Encode_Title (Item); Wire.Label := Code (Op);
         pragma Assert (Decode_Title (Wire).Valid = (Op = Set_Window_Title));
      end loop;
      Wire := Encode_Title (Item); Wire.Words (0) := 0;
      pragma Assert (not Decode_Title (Wire).Valid);
      Wire := Encode_Title (Item); Wire.Flags := 1;
      pragma Assert (not Decode_Title (Wire).Valid);
      Wire := Encode_Title (Item); Wire.Reserved := 1;
      pragma Assert (not Decode_Title (Wire).Valid);
      Wire := Encode_Title ((1, Make_Title (""))); Wire.Words (1) := 1;
      pragma Assert (not Decode_Title (Wire).Valid);
   end;
   declare
      Greeting : constant Hello_Result := (Success, Session_Identifier'Last, Surface_Capacity'Last);
      Info : constant Information_Result := (Success, Positive_Extent'Last, 1, BGRA_8888, Display_Scale'Last);
      Empty_Operations : constant array (Positive range 1 .. 2) of Empty_Session_Operation :=
        [Goodbye, Get_Information];
   begin
      pragma Assert (Encode_Hello (Current_Revision).Words = [2 ** 32, 0, 0, 0]);
      pragma Assert (Decode_Hello (Encode_Hello (Current_Revision)) = (True, Current_Revision));
      pragma Assert (Decode_Hello (Encode_Hello ((Unsigned_32'Last, Unsigned_32'Last))) =
                     (True, (Unsigned_32'Last, Unsigned_32'Last)));
      pragma Assert (Decode_Hello_Result (Encode_Hello_Result (Greeting)) = Greeting);
      pragma Assert (Decode_Information_Result (Encode_Information_Result (Info)) = Info);
      for Length in Unsigned_8 loop
         Wire := Encode_Hello (Current_Revision); Wire.Length := Length;
         pragma Assert (Decode_Hello (Wire).Valid = (Length = 4));
         Wire := Encode_Hello_Result (Greeting); Wire.Length := Length;
         pragma Assert ((Decode_Hello_Result (Wire).Status = Success) = (Length = 4));
         Wire := Encode_Information_Result (Info); Wire.Length := Length;
         pragma Assert ((Decode_Information_Result (Wire).Status = Success) = (Length = 4));
         for Kind of Empty_Operations loop
            Wire := Encode_Empty_Request (Kind); Wire.Length := Length;
            pragma Assert (Valid_Empty_Request (Wire, Kind) = (Length = 4));
         end loop;
      end loop;
      for Op in Operation loop
         Wire := Encode_Hello (Current_Revision); Wire.Label := Code (Op);
         pragma Assert (Decode_Hello (Wire).Valid = (Op = Hello));
         Wire := Encode_Hello_Result (Greeting); Wire.Label := Code (Op);
         pragma Assert ((Decode_Hello_Result (Wire).Status = Success) = (Op = Hello));
         Wire := Encode_Information_Result (Info); Wire.Label := Code (Op);
         pragma Assert ((Decode_Information_Result (Wire).Status = Success) = (Op = Get_Information));
      end loop;
      for Field in 1 .. 3 loop
         Wire := Encode_Hello (Current_Revision); Wire.Words (Field) := 1;
         pragma Assert (not Decode_Hello (Wire).Valid);
      end loop;
      for Kind of Empty_Operations loop
         for Field in 0 .. 3 loop
            Wire := Encode_Empty_Request (Kind); Wire.Words (Field) := 1;
            pragma Assert (not Valid_Empty_Request (Wire, Kind));
         end loop;
         Wire := Encode_Empty_Request (Kind); Wire.Flags := 1;
         pragma Assert (not Valid_Empty_Request (Wire, Kind));
         Wire := Encode_Empty_Request (Kind); Wire.Reserved := 1;
         pragma Assert (not Valid_Empty_Request (Wire, Kind));
      end loop;
      Wire := Encode_Hello_Result (Greeting); Wire.Words (0) := 0;
      pragma Assert (Decode_Hello_Result (Wire).Status = Invalid_Request);
      Wire := Encode_Hello_Result (Greeting); Wire.Words (1) := 1;
      pragma Assert (Decode_Hello_Result (Wire).Status = Invalid_Request);
      Wire := Encode_Hello_Result (Greeting); Wire.Words (2) := 0;
      pragma Assert (Decode_Hello_Result (Wire).Status = Invalid_Request);
      Wire.Words (2) := 65_536;
      pragma Assert (Decode_Hello_Result (Wire).Status = Invalid_Request);
      Wire := Encode_Hello_Result (Greeting); Wire.Words (3) := 0;
      pragma Assert (Decode_Hello_Result (Wire).Status = Invalid_Request);
      for Field in 0 .. 3 loop
         Wire := Encode_Information_Result (Info); Wire.Words (Field) := 0;
         pragma Assert (Decode_Information_Result (Wire).Status = Invalid_Request);
         Wire := Encode_Information_Result (Info); Wire.Words (Field) := Unsigned_64'Last;
         pragma Assert (Decode_Information_Result (Wire).Status = Invalid_Request);
      end loop;
      for Status in Status_Code range Denied .. Resources_Exhausted loop
         pragma Assert (Decode_Hello_Result (Encode_Hello_Result ((Status => Status))).Status = Status);
         pragma Assert (Decode_Information_Result (Encode_Information_Result ((Status => Status))).Status = Status);
      end loop;
      Wire := Encode_Hello_Result ((Status => Denied)); Wire.Words (1) := 0;
      pragma Assert (Decode_Hello_Result (Wire).Status = Invalid_Request);
      Wire.Label := Code (Get_Information);
      pragma Assert (Decode_Information_Result (Wire).Status = Invalid_Request);
      Wire := Encode_Hello (Current_Revision); Wire.Flags := 1;
      pragma Assert (not Decode_Hello (Wire).Valid);
      Wire := Encode_Hello (Current_Revision); Wire.Reserved := 1;
      pragma Assert (not Decode_Hello (Wire).Valid);
      Wire := Encode_Hello_Result (Greeting); Wire.Flags := 1;
      pragma Assert (Decode_Hello_Result (Wire).Status = Invalid_Request);
      Wire := Encode_Information_Result (Info); Wire.Reserved := 1;
      pragma Assert (Decode_Information_Result (Wire).Status = Invalid_Request);
   end;
   declare
      Request : Input_Request;
      Item : Input_Envelope;
      Canonical : Wire_Message;
   begin
      for Op in Input_Operation loop
         if Op = Poll_Input then
            Request := (Poll_Input, Live_Surface_Name'Last, Unsigned_64'Last);
         else
            Request := (Wait_Input, Live_Surface_Name'Last, Unsigned_64'Last, Unsigned_64'Last);
         end if;
         pragma Assert (Decode_Input_Request (Encode_Input_Request (Request)) = (True, Request));
         for Length in Unsigned_8 loop
            Wire := Encode_Input_Request (Request); Wire.Length := Length;
            pragma Assert (Decode_Input_Request (Wire).Valid = (Length = 4));
         end loop;
         Wire := Encode_Input_Request (Request); Wire.Words (0) := 0;
         pragma Assert (not Decode_Input_Request (Wire).Valid);
         Wire := Encode_Input_Request (Request); Wire.Words (3) := 1;
         pragma Assert (not Decode_Input_Request (Wire).Valid);
         Wire := Encode_Input_Request (Request); Wire.Flags := 1;
         pragma Assert (not Decode_Input_Request (Wire).Valid);
         Wire := Encode_Input_Request (Request); Wire.Reserved := 1;
         pragma Assert (not Decode_Input_Request (Wire).Valid);
         for Kind in Input_Event_Kind loop
            Item := (Kind, Unsigned_64'Last, 0, 0, Kind /= No_Input);
            case Kind is
               when Key_Pressed | Key_Released => Item.Payload0 := 127; Item.Payload1 := 15;
               when Text_Entered => Item.Payload0 := 255;
               when Surface_Configured => Item.Payload0 := 65_535; Item.Payload1 := 65_535;
               when Pointer_Moved | Pointer_Pressed | Pointer_Released | Wheel_Turned | Input_Resynchronized =>
                  Item.Payload0 := 65_535 + 65_535 * 2 ** 32;
                  Item.Payload1 := (if Kind = Wheel_Turned then Unsigned_64'Last
                                    elsif Kind = Input_Resynchronized then 16 * 2 ** 32 - 1
                                    else 2 ** 32 - 1);
               when No_Input => null;
            end case;
            pragma Assert (Valid_Input_Envelope (Item));
            Canonical := Encode_Input_Reply (Op, Item);
            pragma Assert (Decode_Input_Result (Canonical, Op) = (Success, Item));
            Compare_Input_Decoders (Canonical, Op);
            --  Independent rejection oracles, not just agreement between
            --  implementations that could otherwise share a bad bound.
            Wire := Canonical;
            case Kind is
               when No_Input => Wire.Words (2) := 1;
               when Key_Pressed | Key_Released => Wire.Words (2) := 128;
               when Text_Entered => Wire.Words (2) := 256;
               when others => Wire.Words (2) := 65_536;
            end case;
            pragma Assert (Decode_Input_Result (Wire, Op).Status = Invalid_Request);
            Compare_Input_Decoders (Wire, Op);
            Wire := Canonical;
            case Kind is
               when No_Input | Text_Entered => Wire.Words (3) := 1;
               when Key_Pressed | Key_Released => Wire.Words (3) := 16;
               when Surface_Configured => Wire.Words (3) := 65_536;
               when Pointer_Moved | Pointer_Pressed | Pointer_Released =>
                  Wire.Words (3) := 2 ** 32;
               when Input_Resynchronized => Wire.Words (3) := 16 * 2 ** 32;
               when Wheel_Turned => Wire.Words (2) := 65_536 * 2 ** 32;
            end case;
            pragma Assert (Decode_Input_Result (Wire, Op).Status = Invalid_Request);
            Compare_Input_Decoders (Wire, Op);
            Wire := Canonical; Wire.Words (0) := 10;
            pragma Assert (Decode_Input_Result (Wire, Op).Status = Invalid_Request);
            Compare_Input_Decoders (Wire, Op);
            -- Mutate every bit of every payload word, including kind/serial.
            for Field in Payload'Range loop
               for Bit in 0 .. 63 loop
                  Wire := Canonical; Wire.Words (Field) := Wire.Words (Field) xor Shift_Left (1, Bit);
                  Compare_Input_Decoders (Wire, Op);
               end loop;
            end loop;
            for Length in Unsigned_8 loop
               Wire := Canonical; Wire.Length := Length;
               pragma Assert ((Decode_Input_Result (Wire, Op).Status = Success) = (Length = 4));
               Compare_Input_Decoders (Wire, Op);
               Wire := Canonical; Wire.Flags := Length;
               Compare_Input_Decoders (Wire, Op);
            end loop;
            Wire := Canonical; Wire.Reserved := 1;
            pragma Assert (Decode_Input_Result (Wire, Op).Status = Invalid_Request);
            Compare_Input_Decoders (Wire, Op);
            for Other in Operation loop
               Wire := Canonical; Wire.Label := Code (Other);
               pragma Assert ((Decode_Input_Result (Wire, Op).Status = Success) = (Other = Op));
               Compare_Input_Decoders (Wire, Op);
            end loop;
         end loop;
         for Status in Status_Code range Denied .. Resources_Exhausted loop
            Wire := Encode_Status (Op, Status);
            pragma Assert (Decode_Input_Result (Wire, Op).Status = Status);
            Compare_Input_Decoders (Wire, Op);
         end loop;
         Wire := Encode_Status (Op, Success);
         pragma Assert (Decode_Input_Result (Wire, Op).Status = Invalid_Request);
         Compare_Input_Decoders (Wire, Op);
      end loop;
      Wire := Encode_Input_Request ((Poll_Input, 1, 0)); Wire.Words (2) := 1;
      pragma Assert (not Decode_Input_Request (Wire).Valid);
      pragma Assert (Decode_Input_Request (Encode_Input_Request ((Wait_Input, 1, 0, 0))).Valid);
   end;
   Put_Line ("PASS: desktop codecs, hostile words, input C/Ada parity, geometry and surface ownership");
end Main;
