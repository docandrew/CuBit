with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Desktop_Protocol; use CuBit.Desktop_Protocol;
procedure Main is
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
   Put_Line ("PASS: desktop codecs, hostile words, creation results, clipping and surface ownership");
end Main;
