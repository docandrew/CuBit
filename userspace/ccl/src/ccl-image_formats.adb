package body CCL.Image_Formats with SPARK_Mode is
   QOI_OP_RGB  : constant Unsigned_8 := 16#FE#;
   QOI_OP_RGBA : constant Unsigned_8 := 16#FF#;
   QOI_TAG_INDEX : constant Unsigned_8 := 0;
   QOI_TAG_DIFF  : constant Unsigned_8 := 1;
   QOI_TAG_LUMA  : constant Unsigned_8 := 2;
   QOI_TAG_MASK  : constant Unsigned_8 := 16#3F#;
   QOI_MAGIC : constant String := "qoif";
   --  Width, height (four bytes each, big-endian), channels, colour space.
   QOI_HEADER_BYTES : constant := 10;
   QOI_END_MARK : constant Unsigned_8 := 1;   --  after seven zero bytes
   PPM_MAXIMUM_LIMIT : constant := 255;
   PPM_VALUE_LIMIT : constant := 65_535;
   OPAQUE : constant Unsigned_8 := 255;

   procedure Reset (Item : out Decoder) is
   begin
      Item := (others => <>);
   end Reset;

   function Complete (Item : Decoder) return Boolean is (Item.Stage = Done);
   function Failed (Item : Decoder) return Boolean is (Item.Stage = Broken);
   function Kind (Item : Decoder) return Format is (Item.Image_Format);

   function Hash (Pixel : RGBA) return Natural is
     ((Natural (Pixel.R) * 3 + Natural (Pixel.G) * 5 + Natural (Pixel.B) * 7 +
       Natural (Pixel.A) * 11) mod 64);

   --  Over black: each channel scaled by alpha.
   function Composite (Pixel : RGBA) return Unsigned_32 is
      function Channel (C : Unsigned_8) return Unsigned_32 is
        (Unsigned_32 (Natural (C) * Natural (Pixel.A) / Natural (OPAQUE)));
   begin
      return Shift_Left (Channel (Pixel.R), 16) or Shift_Left (Channel (Pixel.G), 8) or Channel (Pixel.B);
   end Composite;

   function Total (Item : Decoder) return Natural is (Item.Width * Item.Height);

   --  Start the image once its size is known and acceptable.
   procedure Begin_Image (Item : in out Decoder; Next : Phase) is
   begin
      if Item.Width in 1 .. Maximum_Side and then Item.Height in 1 .. Maximum_Side and then
        Item.Width * Item.Height <= Maximum_Pixels
      then
         Start (Item.Width, Item.Height);
         Item.Stage := Next;
      else
         Item.Stage := Broken;
      end if;
   end Begin_Image;

   procedure Put (Item : in out Decoder; Pixel : RGBA) is
   begin
      if Item.Position >= Total (Item) then
         Item.Stage := Broken;
         return;
      end if;
      Emit (Item.Position mod Item.Width, Item.Position / Item.Width, Composite (Pixel));
      Item.Position := Item.Position + 1;
   end Put;

   procedure After_Pixels (Item : in out Decoder) is
   begin
      if Item.Stage /= Broken and then Item.Position = Total (Item) then
         Item.Stage := (if Item.Image_Format = QOI_Format then QOI_End else Done);
         Item.Seen := 0;
      end if;
   end After_Pixels;

   --  One QOI chunk whose operands (if any) have all arrived.
   procedure QOI_Chunk (Item : in out Decoder) is
      B : constant Unsigned_8 := Item.Operation;
      P : RGBA := Item.Previous;
      Run : Natural := 1;
   begin
      if B = QOI_OP_RGB then
         P := (Item.Operands (1), Item.Operands (2), Item.Operands (3), P.A);
      elsif B = QOI_OP_RGBA then
         P := (Item.Operands (1), Item.Operands (2), Item.Operands (3), Item.Operands (4));
      else
         case Shift_Right (B, 6) is
            when QOI_TAG_INDEX => P := Item.Index (Natural (B and QOI_TAG_MASK));
            when QOI_TAG_DIFF =>
               P.R := P.R + (Shift_Right (B, 4) and 3) - 2;
               P.G := P.G + (Shift_Right (B, 2) and 3) - 2;
               P.B := P.B + (B and 3) - 2;
            when QOI_TAG_LUMA =>
               declare
                  Green : constant Unsigned_8 := (B and QOI_TAG_MASK) - 32;
               begin
                  P.R := P.R + Green + Shift_Right (Item.Operands (1), 4) - 8;
                  P.G := P.G + Green;
                  P.B := P.B + Green + (Item.Operands (1) and 16#0F#) - 8;
               end;
            when others => Run := Natural (B and QOI_TAG_MASK) + 1;
         end case;
      end if;
      Item.Index (Hash (P)) := P;
      Item.Previous := P;
      for I in 1 .. Run loop
         exit when Item.Stage = Broken;
         Put (Item, P);
      end loop;
      Item.Stage := (if Item.Stage = Broken then Broken else QOI_Chunks);
      After_Pixels (Item);
   end QOI_Chunk;

   procedure Feed (Item : in out Decoder; Byte : Unsigned_8) is
      C : constant Character := Character'Val (Byte);
   begin
      case Item.Stage is
         when Done | Broken =>
            Item.Stage := Broken;   --  bytes after the image: not one image
         when Magic =>
            Item.Seen := Item.Seen + 1;
            if Item.Seen = 1 then
               Item.Image_Format :=
                 (if C = QOI_MAGIC (1) then QOI_Format elsif C = 'P' then PPM_Format else Unknown_Format);
               if Item.Image_Format = Unknown_Format then Item.Stage := Broken; end if;
            elsif Item.Image_Format = QOI_Format then
               if C /= QOI_MAGIC (Item.Seen) then
                  Item.Stage := Broken;
               elsif Item.Seen = QOI_MAGIC'Length then
                  Item.Stage := QOI_Header;
                  Item.Seen := 0;
               end if;
            elsif Item.Seen = 2 then
               if C = '6' then Item.Stage := PPM_Header; else Item.Stage := Broken; end if;
            end if;
         when QOI_Header =>
            Item.Seen := Item.Seen + 1;
            if Item.Seen <= 8 then
               declare
                  Part : Natural := (if Item.Seen <= 4 then Item.Width else Item.Height);
               begin
                  if Part > Maximum_Side then
                     Item.Stage := Broken;
                     return;
                  end if;
                  Part := Part * 256 + Natural (Byte);
                  if Item.Seen <= 4 then Item.Width := Part; else Item.Height := Part; end if;
               end;
            elsif Item.Seen = 9 then
               if Byte not in 3 | 4 then Item.Stage := Broken; end if;
            elsif Item.Seen = QOI_HEADER_BYTES then
               if Byte > 1 then
                  Item.Stage := Broken;
               else
                  Begin_Image (Item, QOI_Chunks);
               end if;
            end if;
         when QOI_Chunks =>
            Item.Operation := Byte;
            Item.Have := 0;
            Item.Needed :=
              (if Byte = QOI_OP_RGB then 3 elsif Byte = QOI_OP_RGBA then 4
               elsif Shift_Right (Byte, 6) = QOI_TAG_LUMA then 1 else 0);
            if Item.Needed = 0 then
               QOI_Chunk (Item);
            else
               Item.Stage := QOI_Operand;
            end if;
         when QOI_Operand =>
            Item.Have := Item.Have + 1;
            Item.Operands (Item.Have) := Byte;
            if Item.Have = Item.Needed then
               QOI_Chunk (Item);
            end if;
         when QOI_End =>
            Item.Seen := Item.Seen + 1;
            if Item.Seen < QOI_END_BYTES then
               if Byte /= 0 then Item.Stage := Broken; end if;
            elsif Byte = QOI_END_MARK then
               Item.Stage := Done;
            else
               Item.Stage := Broken;
            end if;
         when PPM_Header =>
            if Item.In_Comment then
               if C in ASCII.LF | ASCII.CR then Item.In_Comment := False; end if;
            elsif C = '#' and then not Item.In_Number then
               Item.In_Comment := True;
            elsif C in '0' .. '9' then
               Item.In_Number := True;
               if Item.Value > PPM_VALUE_LIMIT then
                  Item.Stage := Broken;
               else
                  Item.Value := Item.Value * 10 + (Character'Pos (C) - Character'Pos ('0'));
               end if;
            elsif C in ' ' | ASCII.HT | ASCII.LF | ASCII.CR then
               if Item.In_Number then
                  Item.In_Number := False;
                  case Item.Field is
                     when 1 => Item.Width := Item.Value;
                     when 2 => Item.Height := Item.Value;
                     when 3 =>
                        --  One whitespace byte ends the header; pixels follow.
                        if Item.Value in 1 .. PPM_MAXIMUM_LIMIT then
                           Item.Header (1) := Unsigned_8 (Item.Value);
                           Begin_Image (Item, PPM_Pixels);
                        else
                           Item.Stage := Broken;
                        end if;
                  end case;
                  if Item.Field < 3 then Item.Field := Item.Field + 1; end if;
                  Item.Value := 0;
               end if;
            else
               Item.Stage := Broken;
            end if;
         when PPM_Pixels =>
            declare
               Maximum : constant Natural := Natural (Item.Header (1));
               Scaled : constant Unsigned_8 :=
                 Unsigned_8 (Natural'Min (Natural (Byte), Maximum) * Natural (OPAQUE) / Maximum);
            begin
               case Item.Channel is
                  when 0 => Item.Pending.R := Scaled;
                  when 1 => Item.Pending.G := Scaled;
                  when 2 => Item.Pending.B := Scaled;
               end case;
               if Item.Channel = 2 then
                  Item.Channel := 0;
                  Item.Pending.A := OPAQUE;
                  Put (Item, Item.Pending);
                  After_Pixels (Item);
               else
                  Item.Channel := Item.Channel + 1;
               end if;
            end;
      end case;
   end Feed;
end CCL.Image_Formats;
