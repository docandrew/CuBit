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
       (if Result = Available then Position > Position'Old and Position <= Size)
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
end Directory_Blocks;
