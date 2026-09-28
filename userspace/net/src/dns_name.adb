------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body DNS_Name with SPARK_Mode is

   procedure Encode (Host : String; Name : out Wire; Length : out Wire_Length; OK : out Boolean)
   is
      Label_Start : Natural := 0;   --  where the current label's length byte goes
      Label_Len   : Natural := 0;
      Pos         : Natural := 1;   --  next byte to write
   begin
      Name := [others => 0];
      Length := 0;
      OK := False;
      if Host'Length = 0 or else Host'Length > Maximum_Wire - 2 then
         return;
      end if;
      for I in Host'Range loop
         pragma Loop_Invariant (Label_Start < Maximum_Wire and then Label_Len <= Maximum_Label);
         pragma Loop_Invariant (Pos = Label_Start + 1 + Label_Len);
         pragma Loop_Invariant (Pos <= I - Host'First + 1);
         if Host (I) = '.' then
            if Label_Len = 0 then
               return;   --  an empty label
            end if;
            Name (Label_Start) := Unsigned_8 (Label_Len);
            Label_Start := Pos;
            Label_Len := 0;
            Pos := Pos + 1;
         elsif Host_Character (Host (I)) and then Label_Len < Maximum_Label then
            Name (Pos) := Lower (Unsigned_8 (Character'Pos (Host (I))));
            Pos := Pos + 1;
            Label_Len := Label_Len + 1;
         else
            return;   --  not a host name character, or a label over 63
         end if;
      end loop;
      if Label_Len = 0 then
         return;   --  a trailing dot or an empty name
      end if;
      Name (Label_Start) := Unsigned_8 (Label_Len);
      Name (Pos) := 0;   --  the root label
      Length := Pos + 1;
      OK := True;
   end Encode;

   procedure Read_Name (Message : Bytes; Offset : in out Natural; Name : out Wire;
                        Length : out Wire_Length; OK : out Boolean)
   is
      Label : Natural;
   begin
      Name := [others => 0];
      Length := 0;
      OK := False;
      loop
         pragma Loop_Invariant (Length < Maximum_Wire and then Offset >= Offset'Loop_Entry);
         pragma Loop_Variant (Increases => Offset);
         exit when Offset >= Message'Length;
         Label := Natural (Message (Offset));
         if Label = 0 then
            Name (Length) := 0;
            Length := Length + 1;
            Offset := Offset + 1;
            OK := True;
            return;
         elsif Label > Maximum_Label then
            return;   --  a compression pointer or a reserved form
         elsif Label >= Message'Length - Offset or else Length + Label + 1 >= Maximum_Wire then
            return;   --  past the message, or too long a name
         end if;
         Name (Length) := Unsigned_8 (Label);
         for K in 1 .. Label loop
            pragma Loop_Invariant (Length + Label + 1 < Maximum_Wire);
            Name (Length + K) := Lower (Message (Offset + K));
         end loop;
         Length := Length + Label + 1;
         Offset := Offset + Label + 1;
      end loop;
   end Read_Name;

end DNS_Name;
