package body CuBit.QOI with SPARK_Mode is

   --  Header field positions (1-based within Header_Data).
   Magic_At      : constant := 1;
   Width_At      : constant := 5;
   Height_At     : constant := 9;
   Channels_At   : constant := 13;
   Colorspace_At : constant := 14;
   Diff_Mask     : constant := 16#03#;
   Red_Diff_Shift   : constant := 4;
   Green_Diff_Shift : constant := 2;
   Nibble_Bits   : constant := 4;
   Nibble_Mask   : constant := 16#0F#;
   RGB_Bytes     : constant := 3;
   RGBA_Bytes    : constant := 4;
   Luma_Bytes    : constant := 1;
   Red_Weight    : constant := 3;
   Green_Weight  : constant := 5;
   Blue_Weight   : constant := 7;
   Alpha_Weight  : constant := 11;
   Byte_Bits     : constant := 8;

   function Hash (C : Color) return Slot is
     ((Natural (C.R) * Red_Weight + Natural (C.G) * Green_Weight +
       Natural (C.B) * Blue_Weight + Natural (C.A) * Alpha_Weight) mod Index_Slots);

   function To_Pixel (C : Color) return Pixel is
     (Shift_Left (Unsigned_32 (C.A), 3 * Byte_Bits) or
      Shift_Left (Unsigned_32 (C.R), 2 * Byte_Bits) or
      Shift_Left (Unsigned_32 (C.G), Byte_Bits) or Unsigned_32 (C.B));

   function Big_Endian (H : Header_Data; At_Byte : Positive) return Unsigned_32 is
     (Shift_Left (Unsigned_32 (H (At_Byte)), 3 * Byte_Bits) or
      Shift_Left (Unsigned_32 (H (At_Byte + 1)), 2 * Byte_Bits) or
      Shift_Left (Unsigned_32 (H (At_Byte + 2)), Byte_Bits) or
      Unsigned_32 (H (At_Byte + 3)))
     with Pre => At_Byte in Width_At | Height_At;

   procedure Fail (D : in out Decoder; Why : Failure)
     with Pre  => Why /= No_Failure and then D.Done <= D.Pixels and then
                  D.Pixels <= D.Capacity and then Chunk_Valid (D),
          Post => Valid (D) and then D.State = Failed and then
                  D.Capacity = D'Old.Capacity and then D.Done = D'Old.Done
   is
   begin
      D.State := Failed;
      D.Problem := Why;
   end Fail;

   procedure Start (D : out Decoder; Capacity : Pixel_Limit) is
   begin
      D := (Capacity => Capacity, others => <>);
   end Start;

   --  The fourteenth header byte has arrived.
   procedure Read_Header (D : in out Decoder)
     with Pre  => D.State = Reading_Header and then D.Problem = No_Failure and then
                  D.Have = Header_Bytes and then D.Pixels = 0 and then D.Done = 0 and then
                  D.Owed = 0 and then D.Got = 0 and then D.Ending = 0,
          Post => Valid (D) and then D.Capacity = D'Old.Capacity and then
                  D.Done = 0 and then D.State in Reading_Pixels | Failed
   is
      H : Header_Data renames D.Header;
      Columns : constant Unsigned_32 := Big_Endian (H, Width_At);
      Rows    : constant Unsigned_32 := Big_Endian (H, Height_At);
   begin
      if H (Magic_At) /= Magic_Q or else H (Magic_At + 1) /= Magic_O or else
        H (Magic_At + 2) /= Magic_I or else H (Magic_At + 3) /= Magic_F
      then
         Fail (D, Bad_Magic);
      elsif Columns not in 1 .. Maximum_Side or else Rows not in 1 .. Maximum_Side then
         Fail (D, Bad_Size);
      elsif H (Channels_At) not in Channels_RGB | Channels_RGBA then
         Fail (D, Bad_Channels);
      elsif H (Colorspace_At) not in Colorspace_SRGB | Colorspace_Linear then
         Fail (D, Bad_Colorspace);
      else
         declare
            W : constant Side := Side (Columns);
            R : constant Side := Side (Rows);
         begin
            --  Both sides are at most 2**14: the product fits a Natural.
            if W > D.Capacity / R then
               Fail (D, Over_Limit);
            else
               pragma Assert (W * R <= D.Capacity);
               D.Columns := W;
               D.Rows := R;
               D.Pixels := W * R;
               D.State := Reading_Pixels;
            end if;
         end;
      end if;
   end Read_Header;

   --  Write one decoded pixel and remember it.
   procedure Emit (D : in out Decoder; C : Color; Output : in out Pixel_Buffer)
     with Pre  => Valid (D) and then D.State = Reading_Pixels and then D.Owed = 0 and then
                  Output'First = 0 and then Output'Length >= D.Capacity,
          Post => Valid (D) and then D.Capacity = D'Old.Capacity and then
                  D.Done = D'Old.Done + 1 and then
                  D.State in Reading_Pixels | Reading_Marker
   is
   begin
      Output (D.Done) := To_Pixel (C);
      D.Seen (Hash (C)) := C;
      D.Previous := C;
      D.Done := D.Done + 1;
      if D.Done = D.Pixels then
         D.State := Reading_Marker;
      end if;
   end Emit;

   --  A run of Length copies of the previous pixel.
   procedure Run (D : in out Decoder; Length : Positive; Output : in out Pixel_Buffer)
     with Pre  => Valid (D) and then D.State = Reading_Pixels and then D.Owed = 0 and then
                  Output'First = 0 and then Output'Length >= D.Capacity,
          Post => Valid (D) and then D.Capacity = D'Old.Capacity and then
                  D.Done >= D'Old.Done and then
                  D.State in Reading_Pixels | Reading_Marker | Failed
   is
      Value : constant Pixel := To_Pixel (D.Previous);
      First : constant Pixel_Count := D.Done;
   begin
      if Length > D.Pixels - D.Done then
         Fail (D, Run_Past_End);
         return;
      end if;
      for I in First .. First + Length - 1 loop
         Output (I) := Value;
      end loop;
      --  The specification indexes every pixel, runs included.
      D.Seen (Hash (D.Previous)) := D.Previous;
      D.Done := First + Length;
      if D.Done = D.Pixels then
         D.State := Reading_Marker;
      end if;
   end Run;

   --  The last byte of an Op_RGB, Op_RGBA or Op_Luma chunk has arrived.
   procedure Finish_Chunk (D : in out Decoder; Output : in out Pixel_Buffer)
     with Pre  => Valid (D) and then D.State = Reading_Pixels and then D.Owed > 0 and then
                  D.Got = D.Owed - 1 and then
                  Output'First = 0 and then Output'Length >= D.Capacity,
          Post => Valid (D) and then D.Capacity = D'Old.Capacity and then
                  D.Done = D'Old.Done + 1 and then
                  D.State in Reading_Pixels | Reading_Marker
   is
      C : Color := D.Previous;
      K : Chunk_Data renames D.Chunk;
   begin
      if D.Tag = Op_RGB then
         C.R := K (1); C.G := K (2); C.B := K (3);
      elsif D.Tag = Op_RGBA then
         C := (R => K (1), G => K (2), B => K (3), A => K (4));
      else
         --  Op_Luma: a green delta, then red and blue relative to it.
         declare
            Green : constant Unsigned_8 := (D.Tag and Payload_Mask) - Luma_Green_Bias;
         begin
            C.R := C.R + Green + Shift_Right (K (1), Nibble_Bits) - Luma_Delta_Bias;
            C.G := C.G + Green;
            C.B := C.B + Green + (K (1) and Nibble_Mask) - Luma_Delta_Bias;
         end;
      end if;
      D.Owed := 0;
      D.Got := 0;
      Emit (D, C, Output);
   end Finish_Chunk;

   --  One byte of chunk data while reading pixels.
   procedure Pixel_Byte (D : in out Decoder; Value : Unsigned_8; Output : in out Pixel_Buffer)
     with Pre  => Valid (D) and then D.State = Reading_Pixels and then
                  Output'First = 0 and then Output'Length >= D.Capacity,
          Post => Valid (D) and then D.Capacity = D'Old.Capacity and then
                  D.Done >= D'Old.Done and then
                  D.State in Reading_Pixels | Reading_Marker | Failed
   is
      C : Color;
   begin
      if D.Owed > 0 then
         D.Chunk (D.Got + 1) := Value;
         if D.Got + 1 = D.Owed then
            Finish_Chunk (D, Output);
         else
            D.Got := D.Got + 1;
         end if;
      elsif Value = Op_RGB then
         D.Tag := Value; D.Owed := RGB_Bytes; D.Got := 0;
      elsif Value = Op_RGBA then
         D.Tag := Value; D.Owed := RGBA_Bytes; D.Got := 0;
      else
         case Value and Tag_Mask is
            when Op_Index =>
               C := D.Seen (Natural (Value and Payload_Mask));
               Emit (D, C, Output);
            when Op_Diff =>
               C := D.Previous;
               C.R := C.R + (Shift_Right (Value, Red_Diff_Shift) and Diff_Mask) - Diff_Bias;
               C.G := C.G + (Shift_Right (Value, Green_Diff_Shift) and Diff_Mask) - Diff_Bias;
               C.B := C.B + (Value and Diff_Mask) - Diff_Bias;
               Emit (D, C, Output);
            when Op_Luma =>
               D.Tag := Value; D.Owed := Luma_Bytes; D.Got := 0;
            when others =>
               Run (D, Natural (Value and Payload_Mask) + Run_Bias, Output);
         end case;
      end if;
   end Pixel_Byte;

   procedure Step (D : in out Decoder; Value : Unsigned_8; Output : in out Pixel_Buffer)
     with Pre  => Valid (D) and then Output'First = 0 and then Output'Length >= D.Capacity,
          Post => Valid (D) and then D.Capacity = D'Old.Capacity and then
                  D.Done >= D'Old.Done and then
                  (if D'Old.State = Failed then D.State = Failed)
   is
   begin
      case D.State is
         when Reading_Header =>
            D.Header (D.Have + 1) := Value;
            D.Have := D.Have + 1;
            if D.Have = Header_Bytes then
               Read_Header (D);
            end if;
         when Reading_Pixels =>
            Pixel_Byte (D, Value, Output);
         when Reading_Marker =>
            if Value /= (if D.Ending = Marker_Bytes - 1 then Marker_Last else 0) then
               Fail (D, Bad_Marker);
            else
               D.Ending := D.Ending + 1;
               if D.Ending = Marker_Bytes then
                  D.State := Complete;
               end if;
            end if;
         when Complete =>
            Fail (D, Trailing_Data);
         when Failed =>
            null;
      end case;
   end Step;

   procedure Feed (D : in out Decoder; Input : Byte_Array; Output : in out Pixel_Buffer) is
   begin
      for I in Input'Range loop
         pragma Loop_Invariant (Valid (D));
         pragma Loop_Invariant (D.Capacity = D'Loop_Entry.Capacity);
         pragma Loop_Invariant (D.Done >= D'Loop_Entry.Done);
         pragma Loop_Invariant (if D'Loop_Entry.State = Failed then D.State = Failed);
         exit when D.State = Failed;
         Step (D, Input (I), Output);
      end loop;
   end Feed;

   procedure Finish (D : in out Decoder) is
   begin
      if D.State /= Complete and then D.State /= Failed then
         Fail (D, Truncated);
      end if;
   end Finish;
end CuBit.QOI;
