------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.Launch_Arguments with SPARK_Mode is

   function Validate (Item : Block) return Validation is
      Count : Natural := 0;
   begin
      if Item'Length not in Present_Length then
         return Wrong_Length;
      elsif Field_16 (Item, Version_Offset) /= Format_Version
        or else Field_16 (Item, Reserved_Offset) /= 0
      then
         return Unknown_Format;
      elsif Field_32 (Item, Argument_Count_Offset) > Maximum_Strings
        or else Field_32 (Item, Environment_Count_Offset) >
                Maximum_Strings - Field_32 (Item, Argument_Count_Offset)
      then
         return Too_Many_Strings;
      elsif Field_32 (Item, String_Bytes_Offset) /=
            Unsigned_32 (Item'Length - Header_Bytes)
      then
         return Length_Mismatch;
      elsif Item'Length > Header_Bytes
        and then Item (Item'Last) /= Terminator
      then
         return Unterminated;
      end if;
      pragma Assert (Header_Valid (Item));

      for I in Header_Bytes + 1 .. Item'Last loop
         if Item (I) = Terminator then
            Count := Count + 1;
         end if;
         pragma Loop_Invariant (Count = Terminators (Item, I));
      end loop;
      pragma Assert (Count = Terminators (Item, Item'Last));

      if Count /= Strings_Declared (Item) then
         return Count_Mismatch;
      end if;
      return Valid;
   end Validate;

   procedure Next_String
     (Item : Block; Position : Positive; Last : out Natural;
      Next : out Cursor)
   is
      I : Positive := Position;
   begin
      pragma Assert (Item (Item'Last) = Terminator);
      loop
         pragma Loop_Invariant
           (I in Position .. Item'Last
            and then (for all K in Position .. I - 1 =>
                        Item (K) /= Terminator));
         pragma Loop_Variant (Increases => I);
         exit when Item (I) = Terminator;
         I := I + 1;
      end loop;
      Last := I - 1;
      Next := I + 1;
   end Next_String;

   procedure Locate
     (Item : Block; Index : Positive; First : out Positive;
      Last : out Natural; Found : out Boolean)
   is
      Position : Cursor := Header_Bytes + 1;
      Next : Cursor;
   begin
      First := Header_Bytes + 1;
      Last := Header_Bytes;
      Found := False;
      for Number in 1 .. Index loop
         exit when Position > Item'Last;
         Next_String (Item, Position, Last, Next);
         if Number = Index then
            First := Position;
            Found := True;
            return;
         end if;
         Position := Next;
         pragma Loop_Invariant (Position in Header_Bytes + 1 .. Item'Last + 1);
      end loop;
   end Locate;

   procedure Start (B : out Builder) is
   begin
      B := (Data => [others => 0], Used => Header_Bytes, Arguments => 0,
            Environment => 0, In_Environment => False);
   end Start;

   --  Appends Value and its terminator if it fits and holds no NUL.
   procedure Append
     (B : in out Builder; Value : String; Accepted : out Boolean)
   with Pre => Builder_Valid (B),
        Post => Builder_Valid (B)
                and then B.Arguments = B.Arguments'Old
                and then B.Environment = B.Environment'Old
                and then B.In_Environment = B.In_Environment'Old;

   procedure Append
     (B : in out Builder; Value : String; Accepted : out Boolean)
   is
   begin
      Accepted := False;
      if B.Arguments + B.Environment >= Maximum_Strings
        or else Value'Length >= Maximum_Block_Bytes - B.Used
      then
         return;
      end if;
      for C of Value loop
         if Character'Pos (C) = Natural (Terminator) then
            return;
         end if;
      end loop;
      for K in 0 .. Value'Length - 1 loop
         B.Data (B.Used + 1 + K) :=
           Unsigned_8 (Character'Pos (Value (Value'First + K)));
         pragma Loop_Invariant (Builder_Valid (B));
      end loop;
      B.Data (B.Used + Value'Length + 1) := Terminator;
      B.Used := B.Used + Value'Length + 1;
      Accepted := True;
   end Append;

   procedure Add_Argument
     (B : in out Builder; Value : String; Accepted : out Boolean)
   is
   begin
      Accepted := False;
      if B.In_Environment
        or else B.Arguments + B.Environment >= Maximum_Strings
      then
         return;
      end if;
      Append (B, Value, Accepted);
      if Accepted then
         B.Arguments := B.Arguments + 1;
      end if;
   end Add_Argument;

   procedure Add_Environment
     (B : in out Builder; Value : String; Accepted : out Boolean)
   is
   begin
      Accepted := False;
      if B.Arguments + B.Environment >= Maximum_Strings then
         return;
      end if;
      Append (B, Value, Accepted);
      if Accepted then
         B.Environment := B.Environment + 1;
         B.In_Environment := True;
      end if;
   end Add_Environment;

   procedure Put_16 (B : in out Builder; Offset : Natural; Value : Unsigned_16)
   with Pre => Offset <= Header_Bytes - Field_16_Bytes,
        Post => B.Used = B.Used'Old and then B.Arguments = B.Arguments'Old
                and then B.Environment = B.Environment'Old;

   procedure Put_16 (B : in out Builder; Offset : Natural; Value : Unsigned_16)
   is
   begin
      B.Data (Offset + 1) := Unsigned_8 (Value and 16#FF#);
      B.Data (Offset + 2) := Unsigned_8 (Shift_Right (Value, 8));
   end Put_16;

   procedure Put_32 (B : in out Builder; Offset : Natural; Value : Unsigned_32)
   with Pre => Offset <= Header_Bytes - Field_32_Bytes,
        Post => B.Used = B.Used'Old and then B.Arguments = B.Arguments'Old
                and then B.Environment = B.Environment'Old;

   procedure Put_32 (B : in out Builder; Offset : Natural; Value : Unsigned_32)
   is
   begin
      for K in 0 .. Field_32_Bytes - 1 loop
         B.Data (Offset + 1 + K) :=
           Unsigned_8 (Shift_Right (Value, 8 * K) and 16#FF#);
      end loop;
   end Put_32;

   procedure Finish
     (B : in out Builder; Length : out Present_Length; Accepted : out Boolean)
   is
   begin
      Put_16 (B, Version_Offset, Format_Version);
      Put_16 (B, Reserved_Offset, 0);
      Put_32 (B, Argument_Count_Offset, Unsigned_32 (B.Arguments));
      Put_32 (B, Environment_Count_Offset, Unsigned_32 (B.Environment));
      Put_32 (B, String_Bytes_Offset, Unsigned_32 (B.Used - Header_Bytes));
      Length := B.Used;
      Accepted := Validate (B.Data (1 .. Length)) = Valid;
   end Finish;

end CuBit.Launch_Arguments;
