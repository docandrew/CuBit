pragma Ada_2022;

package body CuBit.Directory_Pages with SPARK_Mode is

   Byte_Bits : constant := 8;

   procedure Put_8 (P : in out Page; At_Byte : Page_Index; Value : Unsigned_8)
   with Inline;
   procedure Put_8 (P : in out Page; At_Byte : Page_Index; Value : Unsigned_8) is
   begin
      P (At_Byte) := Value;
   end Put_8;

   procedure Put_16 (P : in out Page; At_Byte : Natural; Value : Unsigned_16)
   with Inline, Pre => At_Byte <= Page_Bytes - 2;
   procedure Put_16 (P : in out Page; At_Byte : Natural; Value : Unsigned_16) is
   begin
      P (At_Byte) := Unsigned_8 (Value and 16#FF#);
      P (At_Byte + 1) := Unsigned_8 (Shift_Right (Value, Byte_Bits));
   end Put_16;

   procedure Put_32 (P : in out Page; At_Byte : Natural; Value : Unsigned_32)
   with Inline, Pre => At_Byte <= Page_Bytes - 4;
   procedure Put_32 (P : in out Page; At_Byte : Natural; Value : Unsigned_32) is
   begin
      for I in 0 .. 3 loop
         P (At_Byte + I) := Unsigned_8 (Shift_Right (Value, I * Byte_Bits) and 16#FF#);
      end loop;
   end Put_32;

   procedure Put_64 (P : in out Page; At_Byte : Natural; Value : Unsigned_64)
   with Inline, Pre => At_Byte <= Page_Bytes - 8;
   procedure Put_64 (P : in out Page; At_Byte : Natural; Value : Unsigned_64) is
   begin
      for I in 0 .. 7 loop
         P (At_Byte + I) := Unsigned_8 (Shift_Right (Value, I * Byte_Bits) and 16#FF#);
      end loop;
   end Put_64;

   function Get_16 (P : Page; At_Byte : Natural) return Unsigned_16 is
     (Unsigned_16 (P (At_Byte)) or Shift_Left (Unsigned_16 (P (At_Byte + 1)), Byte_Bits))
   with Pre => At_Byte <= Page_Bytes - 2;

   function Get_32 (P : Page; At_Byte : Natural) return Unsigned_32 is
     (Unsigned_32 (P (At_Byte))
      or Shift_Left (Unsigned_32 (P (At_Byte + 1)), Byte_Bits)
      or Shift_Left (Unsigned_32 (P (At_Byte + 2)), 2 * Byte_Bits)
      or Shift_Left (Unsigned_32 (P (At_Byte + 3)), 3 * Byte_Bits))
   with Pre => At_Byte <= Page_Bytes - 4;

   function Get_64 (P : Page; At_Byte : Natural) return Unsigned_64 is
     (Unsigned_64 (Get_32 (P, At_Byte))
      or Shift_Left (Unsigned_64 (Get_32 (P, At_Byte + 4)), 4 * Byte_Bits))
   with Pre => At_Byte <= Page_Bytes - 8;

   procedure Start (P : out Page; W : out Writer) is
   begin
      P := [others => 0];
      W := (Count => 0, Used => Header_Bytes);
   end Start;

   procedure Append
     (P : in out Page; W : in out Writer; Item : Facts; Name : Name_Bytes; Length : Name_Length)
   is
      Base : constant Natural := W.Used;
      Size : constant Record_Size := Record_Bytes (Length);
   begin
      for I in Base .. Base + Size - 1 loop
         P (I) := 0;
      end loop;
      Put_16 (P, Base + Record_Bytes_At, Unsigned_16 (Size));
      Put_8 (P, Base + Name_Length_At, Unsigned_8 (Length));
      Put_8 (P, Base + Kind_At, Item.Kind);
      Put_32 (P, Base + Valid_At, Item.Valid);
      Put_64 (P, Base + Object_At, Item.Object);
      Put_64 (P, Base + Size_At, Item.Size);
      Put_64 (P, Base + Modified_At, Item.Modified);
      Put_64 (P, Base + Changed_At, Item.Changed);
      Put_64 (P, Base + Accessed_At, Item.Accessed);
      Put_32 (P, Base + Mode_At, Item.Mode);
      Put_32 (P, Base + Links_At, Item.Links);
      Put_32 (P, Base + Owner_At, Item.Owner);
      Put_32 (P, Base + Group_At, Item.Group);
      for I in 1 .. Length loop
         pragma Loop_Invariant (Base + Name_At + I - 1 < Page_Bytes);
         P (Base + Name_At + I - 1) := Name (I);
      end loop;
      W := (Count => W.Count + 1, Used => Base + Size);
   end Append;

   procedure Finish
     (P : in out Page; W : Writer; Ended : Boolean; Resume, Stamp : Unsigned_64) is
   begin
      Put_16 (P, Version_At, Version);
      Put_16 (P, Header_Bytes_At, Header_Bytes);
      Put_16 (P, Count_At, Unsigned_16 (W.Count));
      Put_16 (P, Used_At, Unsigned_16 (W.Used));
      Put_32 (P, Flags_At, (if Ended then Page_End else 0));
      Put_32 (P, Reserved_At, 0);
      Put_64 (P, Resume_At, Resume);
      Put_64 (P, Stamp_At, Stamp);
   end Finish;

   procedure Get
     (P : Page; Offset : Natural; Limit : Natural; Item : out Facts;
      Name : out Name_Bytes; Length : out Name_Length; Next : out Natural;
      OK : out Boolean)
   is
      Stored : Unsigned_16;
      Raw_Length : Unsigned_8;
   begin
      Item := (others => <>);
      Name := [others => 0];
      Length := 0;
      Next := Offset;
      OK := False;
      if Limit > Page_Bytes or else Offset < Header_Bytes or else Offset > Limit
        or else Limit - Offset < Smallest_Record
      then
         return;
      end if;
      Stored := Get_16 (P, Offset + Record_Bytes_At);
      Raw_Length := P (Offset + Name_Length_At);
      if Raw_Length = 0
        or else Natural (Stored) /= Record_Bytes (Natural (Raw_Length))
        or else Natural (Stored) > Limit - Offset
      then
         return;
      end if;
      Length := Natural (Raw_Length);
      Item :=
        (Kind     => P (Offset + Kind_At),
         Valid    => Get_32 (P, Offset + Valid_At),
         Object   => Get_64 (P, Offset + Object_At),
         Size     => Get_64 (P, Offset + Size_At),
         Modified => Get_64 (P, Offset + Modified_At),
         Changed  => Get_64 (P, Offset + Changed_At),
         Accessed => Get_64 (P, Offset + Accessed_At),
         Mode     => Get_32 (P, Offset + Mode_At),
         Links    => Get_32 (P, Offset + Links_At),
         Owner    => Get_32 (P, Offset + Owner_At),
         Group    => Get_32 (P, Offset + Group_At));
      for I in 1 .. Length loop
         pragma Loop_Invariant (Offset + Name_At + I - 1 < Page_Bytes);
         Name (I) := P (Offset + Name_At + I - 1);
      end loop;
      Next := Offset + Natural (Stored);
      OK := True;
   end Get;

   procedure Check
     (P : Page; Valid : out Boolean; Count : out Entry_Count; Used : out Used_Bytes;
      Ended : out Boolean; Resume, Stamp : out Unsigned_64)
   is
      Stored_Count : constant Unsigned_16 := Get_16 (P, Count_At);
      Stored_Used  : constant Unsigned_16 := Get_16 (P, Used_At);
      Flags        : constant Unsigned_32 := Get_32 (P, Flags_At);
      At_Entry     : Natural := Header_Bytes;
      Item : Facts;
      Name : Name_Bytes;
      Length : Name_Length;
      Next : Natural;
      OK : Boolean;
   begin
      Valid := False;
      Count := 0;
      Used := Header_Bytes;
      Ended := False;
      Resume := 0;
      Stamp := 0;
      if Get_16 (P, Version_At) /= Version
        or else Get_16 (P, Header_Bytes_At) /= Header_Bytes
        or else Natural (Stored_Count) > Maximum_Entries
        or else Natural (Stored_Used) not in Used_Bytes
        or else (Flags and not Page_End) /= 0
        or else Get_32 (P, Reserved_At) /= 0
      then
         return;
      end if;
      for I in 1 .. Natural (Stored_Count) loop
         pragma Loop_Invariant (At_Entry in Header_Bytes .. Natural (Stored_Used));
         Get (P, At_Entry, Natural (Stored_Used), Item, Name, Length, Next, OK);
         if not OK or else not Valid_Name (Name, Length) or else Item.Kind > Last_Kind then
            return;
         end if;
         At_Entry := Next;
      end loop;
      if At_Entry /= Natural (Stored_Used) then
         return;
      end if;
      Valid := True;
      Count := Natural (Stored_Count);
      Used := Natural (Stored_Used);
      Ended := Flags = Page_End;
      Resume := Get_64 (P, Resume_At);
      Stamp := Get_64 (P, Stamp_At);
   end Check;

end CuBit.Directory_Pages;
