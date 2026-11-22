package body Files_Listing with SPARK_Mode is

   procedure Clear (L : in out Listing) is
   begin
      L.Count := 0;
      L.Used := 0;
      L.Complete := False;
   end Clear;

   function Prefix_Of (Name : Name_Bytes) return Sort_Prefix is
      Result : Sort_Prefix := 0;
      Taken : Natural range 0 .. PREFIX_BYTES := 0;
   begin
      for B of Name loop
         exit when Taken = PREFIX_BYTES;
         if Is_Digit (B) then
            Result := Result * 256 + Sort_Prefix (DIGIT_MARK);
            Taken := Taken + 1;
            exit;
         end if;
         Result := Result * 256 + Sort_Prefix (Fold (B));
         Taken := Taken + 1;
      end loop;
      --  Zero bytes after the name: a shorter name sorts first.
      for Pad in Taken + 1 .. PREFIX_BYTES loop
         Result := Result * 256;
      end loop;
      return Result;
   end Prefix_Of;

   procedure Append (L : in out Listing; Name : Name_Bytes; Facts : Entry_Facts) is
      First : constant Arena_Index := L.Used + 1;
      Extension : Name_Length := 0;
   begin
      for Offset in 0 .. Name'Length - 1 loop
         pragma Loop_Invariant (L.Used = L.Used'Loop_Entry and then L.Count = L.Count'Loop_Entry);
         L.Names (First + Offset) := Name (Name'First + Offset);
         --  The last dot that is neither the first nor the last byte.
         if Name (Name'First + Offset) = Character'Pos ('.') and then Offset > 0
           and then Offset < Name'Length - 1
         then
            Extension := Offset + 2;
         end if;
      end loop;
      L.Count := L.Count + 1;
      L.Entries (L.Count) :=
        (Facts => Facts, Name_First => First, Length => Name'Length, Extension_At => Extension,
         Prefix => Prefix_Of (Name));
      L.Used := L.Used + Name'Length;
   end Append;

   function Byte_At (L : Listing; Id : Entry_Id; Position : Name_Position) return Unsigned_8 is
   begin
      if not Is_Entry (L, Id) or else Position > L.Entries (Id).Length then
         return 0;
      end if;
      declare
         First : constant Arena_Index := L.Entries (Id).Name_First;
      begin
         if First > L.Arena_Bytes - (Position - 1) then
            return 0;
         end if;
         return L.Names (First + Position - 1);
      end;
   end Byte_At;

   function Name (L : Listing; Id : Entry_Id) return String is
      Result : String (1 .. Length (L, Id));
   begin
      for Position in Result'Range loop
         Result (Position) := Character'Val (Byte_At (L, Id, Position));
      end loop;
      return Result;
   end Name;
end Files_Listing;
