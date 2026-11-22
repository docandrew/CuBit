pragma Ada_2022;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;

package body Dash_Labels is
   use type System.Address;
   Bytes_Per_Pixel : constant := 4;

   type Slot_Pixels is array (0 .. Slot_Width * Slot_Height - 1) of Unsigned_32;
   type Slot is record
      Text : String (1 .. Maximum_Text) := [others => ' '];
      Length : Natural range 0 .. Maximum_Text := 0;
      Ink, Under : CuBit.UI.Color := 0;
      Width, Height : Natural := 0;
      Used : Boolean := False;
      Pixels : Slot_Pixels := [others => 0];
   end record;
   type Slot_Array is array (1 .. Slots) of Slot;
   type Slot_Array_Access is access Slot_Array;
   Cache : Slot_Array_Access;
   --  The next slot to reuse (round robin: the oldest made).
   Next_Slot : Positive := 1;
   Hit_Count, Miss_Count : Natural := 0;

   function Hits return Natural is (Hit_Count);
   function Misses return Natural is (Miss_Count);

   function Find (Text : String; Ink, Under : CuBit.UI.Color) return Natural is
   begin
      for I in Cache'Range loop
         declare
            S : Slot renames Cache (I);
         begin
            if S.Used and then S.Length = Text'Length and then S.Ink = Ink and then S.Under = Under
              and then S.Text (1 .. S.Length) = Text
            then
               return I;
            end if;
         end;
      end loop;
      return 0;
   end Find;

   procedure Draw (C : CuBit.UI.Canvas; X, Y : Natural; Text : String; Ink, Under : CuBit.UI.Color) is
      W : constant Natural := CuBit.UI.UI_Text_Width (Text);
      H : constant Natural := CuBit.UI.UI_Text_Height;
      Index : Natural;
   begin
      if C.densityNumerator /= C.densityDenominator or else Text'Length > Maximum_Text or else W > Slot_Width
        or else H > Slot_Height or else W = 0 or else C.addr = System.Null_Address
      then
         CuBit.UI.Draw_UI_Text_Transparent (C, X, Y, Text, Ink);
         return;
      end if;
      if Cache = null then
         Cache := new Slot_Array;
      end if;
      Index := Find (Text, Ink, Under);
      if Index = 0 then
         --  Rasterize once, over the card's colour.
         Miss_Count := Miss_Count + 1;
         Index := Next_Slot;
         Next_Slot := (if Next_Slot = Slots then 1 else Next_Slot + 1);
         declare
            S : Slot renames Cache (Index);
            Scratch : constant CuBit.UI.Canvas :=
              (addr => S.Pixels'Address, width => W, height => H, pitch => Slot_Width * Bytes_Per_Pixel,
               others => <>);
         begin
            S.Text (1 .. Text'Length) := Text;
            S.Length := Text'Length;
            S.Ink := Ink;
            S.Under := Under;
            S.Width := W;
            S.Height := H;
            S.Used := True;
            CuBit.UI.Fill_Rect (Scratch, (0, 0, W, H), Under);
            CuBit.UI.Draw_UI_Text_Transparent (Scratch, 0, 0, Text, Ink);
         end;
      else
         Hit_Count := Hit_Count + 1;
      end if;
      declare
         S : Slot renames Cache (Index);
         --  The part of the label the canvas (and its clip) shows.
         Shown : constant CuBit.UI.Rect := CuBit.UI.Clamp_Rect (C, (X, Y, S.Width, S.Height));
         Pitch : constant Natural := C.pitch / Bytes_Per_Pixel;
      begin
         if CuBit.UI.Is_Empty (Shown) or else C.pitch mod Bytes_Per_Pixel /= 0 then
            return;
         end if;
         for Row in 0 .. Shown.h - 1 loop
            declare
               Source_Row : constant Natural := Shown.y - Y + Row;
               Source_Column : constant Natural := Shown.x - X;
               type Row_Pixels is array (0 .. Shown.w - 1) of Unsigned_32;
               Target : Row_Pixels
                 with Import, Address => C.addr + Storage_Offset (((Shown.y + Row) * Pitch + Shown.x) * Bytes_Per_Pixel);
               Source : Row_Pixels
                 with Import, Address => S.Pixels (Source_Row * Slot_Width + Source_Column)'Address;
            begin
               Target := Source;
            end;
         end loop;
      end;
   end Draw;
end Dash_Labels;
