pragma Ada_2022;
with CuBit.Directory_Paths;

package body Directory_Blocks with SPARK_Mode => On is
   Header_Bytes : constant := 8;
   type Record_Info is record
      Inode : Unsigned_32 := 0;
      Kind : Unsigned_8 := 0;
      Name : String (1 .. 255) := [others => ' '];
      Length : Natural range 0 .. 255 := 0;
   end record;
   type Read_Result is (Available, End_Of_Block, Malformed);

   procedure Next
     (Data : Block_Data; Size : Block_Length; Maximum_Inode : Unsigned_32;
      Position : in out Byte_Count; Item : out Record_Info;
      Result : out Read_Result)
     with Post =>
       (if Result = Available then
          Position >= Position'Old + Header_Bytes and Position <= Size and
          Item.Inode <= Maximum_Inode)
   is
      Span : Natural;
   begin
      Item := (others => <>);
      Result := Malformed;
      if Position = Size then
         Result := End_Of_Block;
         return;
      elsif Position > Size or else Size - Position < Header_Bytes then
         return;
      end if;
      Span := Natural (Data (Position + 5)) +
        256 * Natural (Data (Position + 6));
      if Span < Header_Bytes or else Span mod 4 /= 0 or else
        Span > Size - Position
      then
         return;
      end if;
      Item.Length := Natural (Data (Position + 7));
      if Item.Length > Span - Header_Bytes then
         return;
      end if;
      Item.Inode := Unsigned_32 (Data (Position + 1)) or
        Shift_Left (Unsigned_32 (Data (Position + 2)), 8) or
        Shift_Left (Unsigned_32 (Data (Position + 3)), 16) or
        Shift_Left (Unsigned_32 (Data (Position + 4)), 24);
      Item.Kind := Data (Position + 8);
      if Item.Inode > Maximum_Inode or else
        (Item.Inode /= 0 and then Item.Length = 0)
      then
         return;
      end if;
      for Index in 1 .. Item.Length loop
         Item.Name (Index) := Character'Val (Data (Position + Header_Bytes + Index));
      end loop;
      Position := Position + Span;
      Result := Available;
   end Next;

   function Matches (Item : Record_Info; Name : String) return Boolean is
     (Item.Inode /= 0 and then Item.Name (1 .. Item.Length) = Name);

   procedure Set_Span
     (Data : in out Block_Data; Position : Byte_Count; Span : Block_Length)
     with Pre => Position <= Maximum_Bytes - Header_Bytes
   is
   begin
      Data (Position + 5) := Unsigned_8 (Span mod 256);
      Data (Position + 6) := Unsigned_8 (Span / 256);
   end Set_Span;

   procedure Prepare_Rename
     (Data : in out Block_Data; Size : Block_Length;
      Maximum_Inode : Unsigned_32; Old_Name, New_Name : String;
      Result : out Prepare_Result)
   is
      Position, Written, Last_Record : Byte_Count := 0;
      Item : Record_Info;
      Read_Status : Read_Result;
      Found : Boolean := False;
      Destination_Found : Boolean := False;
      Candidate : Block_Data := Data;
   begin
      Result := Invalid_Name;
      if not CuBit.Directory_Paths.Valid_Child_Name (Old_Name) or else
        not CuBit.Directory_Paths.Valid_Child_Name (New_Name)
      then
         return;
      end if;
      Result := Malformed_Block;
      if Size mod 4 /= 0 then
         return;
      end if;
      while Position < Size loop
         pragma Loop_Invariant (Position <= Size);
         pragma Loop_Variant (Decreases => Size - Position);
         Next (Data, Size, Maximum_Inode, Position, Item, Read_Status);
         exit when Read_Status = End_Of_Block;
         if Read_Status = Malformed then
            return;
         end if;
         if Matches (Item, Old_Name) then
            if Found then
               return; -- Duplicate source names are malformed metadata.
            end if;
            Found := True;
         end if;
         Destination_Found := Destination_Found or else Matches (Item, New_Name);
      end loop;
      if not Found then
         Result := Source_Not_Found;
         return;
      elsif Old_Name = New_Name then
         Result := Unchanged;
         return;
      elsif Destination_Found then
         Result := Destination_Exists;
         return;
      end if;

      --  Pack live records into a separate block, retaining inode/type and
      --  changing only the selected name. Free records and slack are reclaimed
      --  inside this block; no extra allocation or directory-size change.
      Candidate (1 .. Size) := [others => 0];
      Position := 0;
      while Position < Size loop
         pragma Loop_Invariant (Position <= Size);
         pragma Loop_Variant (Decreases => Size - Position);
         pragma Loop_Invariant (Written <= Size);
         pragma Loop_Invariant (Last_Record <= Size - Header_Bytes);
         Next (Data, Size, Maximum_Inode, Position, Item, Read_Status);
         exit when Read_Status = End_Of_Block;
         if Read_Status = Malformed then
            return;
         end if;
         if Item.Inode /= 0 then
            declare
               Name : constant String :=
                 (if Matches (Item, Old_Name) then New_Name else
                    Item.Name (1 .. Item.Length));
               Needed : constant Positive :=
                 ((Header_Bytes + Name'Length + 3) / 4) * 4;
            begin
               if Needed > Size - Written then
                  Result := Insufficient_Space;
                  return;
               end if;
               Last_Record := Written;
               Candidate (Written + 1) := Unsigned_8 (Item.Inode and 255);
               Candidate (Written + 2) :=
                 Unsigned_8 (Shift_Right (Item.Inode, 8) and 255);
               Candidate (Written + 3) :=
                 Unsigned_8 (Shift_Right (Item.Inode, 16) and 255);
               Candidate (Written + 4) :=
                 Unsigned_8 (Shift_Right (Item.Inode, 24));
               Set_Span (Candidate, Written, Needed);
               Candidate (Written + 7) := Unsigned_8 (Name'Length);
               Candidate (Written + 8) := Item.Kind;
               for Index in 1 .. Name'Length loop
                  Candidate (Written + Header_Bytes + Index) :=
                    Character'Pos (Name (Name'First + (Index - 1)));
               end loop;
               Written := Written + Needed;
            end;
         end if;
      end loop;
      --  A source was found above, hence at least one live record was emitted.
      --  Extend its last record to absorb all remaining block padding.
      Set_Span (Candidate, Last_Record, Size - Last_Record);
      Data := Candidate;
      Result := Prepared;
   end Prepare_Rename;

   procedure Put_Inode
     (Data : in out Block_Data; Position : Byte_Count; Number : Unsigned_32)
     with Pre => Position <= Maximum_Bytes - Header_Bytes
   is
   begin
      Data (Position + 1) := Unsigned_8 (Number and 255);
      Data (Position + 2) := Unsigned_8 (Shift_Right (Number, 8) and 255);
      Data (Position + 3) := Unsigned_8 (Shift_Right (Number, 16) and 255);
      Data (Position + 4) := Unsigned_8 (Shift_Right (Number, 24));
   end Put_Inode;

   procedure Prepare_Remove
     (Data : in out Block_Data; Size : Block_Length;
      Maximum_Inode : Unsigned_32; Name : String;
      Removed : out Unsigned_32; Kind : out Unsigned_8;
      Result : out Prepare_Result)
   is
      Position, Last_Start : Byte_Count := 0;
      Start : Byte_Count;
      Match_Start, Match_End, Match_Previous : Byte_Count := 0;
      Has_Previous, Match_Has_Previous, Found : Boolean := False;
      Item : Record_Info;
      Read_Status : Read_Result;
      Number : Unsigned_32 := 0;
   begin
      Removed := 0;
      Kind := 0;
      Result := Invalid_Name;
      if not CuBit.Directory_Paths.Valid_Child_Name (Name) then
         return;
      end if;
      Result := Malformed_Block;
      if Size mod 4 /= 0 then
         return;
      end if;
      while Position < Size loop
         pragma Loop_Invariant (Position <= Size);
         pragma Loop_Invariant
           (if Has_Previous then Last_Start + Header_Bytes <= Position);
         pragma Loop_Invariant
           (if Found then
              Match_Start + Header_Bytes <= Match_End and
              Match_End <= Position and
              Number in 1 .. Maximum_Inode and
              (if Match_Has_Previous then
                 Match_Previous + Header_Bytes <= Match_Start));
         pragma Loop_Variant (Decreases => Size - Position);
         Start := Position;
         Next (Data, Size, Maximum_Inode, Position, Item, Read_Status);
         exit when Read_Status = End_Of_Block;
         if Read_Status = Malformed then
            return;
         end if;
         if Matches (Item, Name) then
            if Found then
               return; -- Duplicate names are malformed metadata.
            end if;
            Found := True;
            Match_Start := Start;
            Match_End := Position;
            Match_Previous := Last_Start;
            Match_Has_Previous := Has_Previous;
            Number := Item.Inode;
            Kind := Item.Kind;
         end if;
         Last_Start := Start;
         Has_Previous := True;
      end loop;
      if not Found then
         Kind := 0;
         Result := Source_Not_Found;
         return;
      end if;
      if Match_Has_Previous then
         Set_Span (Data, Match_Previous, Match_End - Match_Previous);
      else
         Put_Inode (Data, Match_Start, 0);
      end if;
      Removed := Number;
      Result := Prepared;
   end Prepare_Remove;

   procedure Count_Children
     (Data : Block_Data; Size : Block_Length; Maximum_Inode : Unsigned_32;
      Children : out Byte_Count; Result : out Prepare_Result)
   is
      Position : Byte_Count := 0;
      Item : Record_Info;
      Read_Status : Read_Result;
   begin
      Children := 0;
      Result := Malformed_Block;
      if Size mod 4 /= 0 then
         return;
      end if;
      while Position < Size loop
         pragma Loop_Invariant (Position <= Size);
         pragma Loop_Invariant (Children * Header_Bytes <= Position);
         pragma Loop_Variant (Decreases => Size - Position);
         Next (Data, Size, Maximum_Inode, Position, Item, Read_Status);
         exit when Read_Status = End_Of_Block;
         if Read_Status = Malformed then
            return;
         end if;
         if Item.Inode /= 0 and then
           Item.Name (1 .. Item.Length) /= "." and then
           Item.Name (1 .. Item.Length) /= ".."
         then
            Children := Children + 1;
         end if;
      end loop;
      Result := Prepared;
   end Count_Children;

   procedure Initial_Block
     (Data : out Block_Data; Size : Block_Length;
      Self, Parent : Unsigned_32; Directory_Kind : Unsigned_8)
   is
      Dot_Span : constant := 12;
   begin
      Data := [others => 0];
      Put_Inode (Data, 0, Self);
      Set_Span (Data, 0, Dot_Span);
      Data (7) := 1;
      Data (8) := Directory_Kind;
      Data (Header_Bytes + 1) := Character'Pos ('.');
      Put_Inode (Data, Dot_Span, Parent);
      Set_Span (Data, Dot_Span, Size - Dot_Span);
      Data (Dot_Span + 7) := 2;
      Data (Dot_Span + 8) := Directory_Kind;
      Data (Dot_Span + Header_Bytes + 1) := Character'Pos ('.');
      Data (Dot_Span + Header_Bytes + 2) := Character'Pos ('.');
   end Initial_Block;
end Directory_Blocks;
