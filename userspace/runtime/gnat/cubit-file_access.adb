pragma Ada_2022;
package body CuBit.File_Access with SPARK_Mode => On is
   function Rights_From_Wire (Raw : Unsigned_8) return Rights_Set is
   begin
      if not Valid_Rights (Raw) then
         return No_Rights;
      end if;
      return [Read_Objects => (Raw and 1) /= 0,
              Write_Objects => (Raw and 2) /= 0,
              Execute_Objects => (Raw and 4) /= 0,
              Create_Objects => (Raw and 8) /= 0];
   end Rights_From_Wire;

   function Valid_Path (Name : String) return Boolean is
   begin
      for Index in Name'Range loop
         if Name (Index) = Character'Val (0) then
            return False;
         elsif Name (Index) = '.' and then Index < Name'Last and then
           Name (Index + 1) = '.' and then
           (Index = Name'First or else Name (Index - 1) = '/') and then
           (Index + 1 = Name'Last or else Name (Index + 2) = '/')
         then
            return False;
         end if;
      end loop;
      return True;
   end Valid_Path;

   function Scope_Matches (Scope, Name : String) return Boolean is
   begin
      if Scope'Length = 0 then
         return True;
      elsif Name'Length < Scope'Length then
         return False;
      elsif Name (Name'First .. Name'First + (Scope'Length - 1)) /= Scope then
         return False;
      end if;
      return Name'Length = Scope'Length or else
        Scope (Scope'Last) = '/' or else
        Name (Name'First + Scope'Length) = '/';
   end Scope_Matches;

   function Allows
     (Item : Policy; Name : String; Requested : Rights_Set) return Boolean
   is
     (Requested /= No_Rights and then Valid_Path (Name) and then
      (for some Index in 1 .. Item.Count =>
         Includes (Item.Entries (Index).Rights, Requested) and then
         Scope_Matches
           (Item.Entries (Index).Prefix (1 .. Item.Entries (Index).Length),
            Name)));

   procedure Clear (Item : out Policy) is
   begin
      Item := (others => <>);
   end Clear;

   procedure Allow_All_For_Bootstrap (Item : out Policy) is
   begin
      Item := (others => <>);
      Item.Count := 1;
      Item.Entries (1).Rights := All_Rights;
   end Allow_All_For_Bootstrap;

   procedure Decode
     (Data : Wire_Bytes; Item : out Policy; Success : out Boolean)
   is
      Candidate : Policy;
   begin
      Clear (Item);
      Success := False;
      if Data'Length = 0 or else Data'Length mod Wire_Entry_Bytes /= 0 then
         return;
      end if;
      Candidate.Count := Data'Length / Wire_Entry_Bytes;
      for Index in 1 .. Candidate.Count loop
         declare
            Base : constant Wire_Index :=
              Data'First + (Index - 1) * Wire_Entry_Bytes;
            Length : constant Natural :=
              Natural (Data (Base + 1)) + 256 * Natural (Data (Base + 2));
         begin
            if not Valid_Rights (Data (Base)) or else
              Length > Maximum_Prefix_Bytes
            then
               return;
            end if;
            for Reserved in 3 .. 7 loop
               if Data (Base + Reserved) /= 0 then
                  return;
               end if;
            end loop;
            Candidate.Entries (Index).Length := Length;
            Candidate.Entries (Index).Rights := Rights_From_Wire (Data (Base));
            for Offset in 1 .. Length loop
               Candidate.Entries (Index).Prefix (Offset) :=
                 Character'Val (Data (Base + 7 + Offset));
            end loop;
            if not Valid_Path
              (Candidate.Entries (Index).Prefix (1 .. Length))
            then
               return;
            end if;
         end;
      end loop;
      Item := Candidate;
      Success := True;
   end Decode;

   function Rights_To_Wire (Rights : Rights_Set) return Unsigned_8 is
     ((if Rights (Read_Objects) then 1 else 0) +
      (if Rights (Write_Objects) then 2 else 0) +
      (if Rights (Execute_Objects) then 4 else 0) +
      (if Rights (Create_Objects) then 8 else 0));

   procedure Encode (Item : Policy; Data : out Wire_Bytes; Count : out Natural) is
   begin
      Data := [others => 0];
      Count := Item.Count;
      for Index in 1 .. Item.Count loop
         pragma Loop_Invariant (Count = Item.Count);
         declare
            Base : constant Wire_Index := Data'First + (Index - 1) * Wire_Entry_Bytes;
            Length : constant Natural := Item.Entries (Index).Length;
         begin
            Data (Base) := Rights_To_Wire (Item.Entries (Index).Rights);
            Data (Base + 1) := Unsigned_8 (Length mod 256);
            Data (Base + 2) := Unsigned_8 (Length / 256);
            for Offset in 1 .. Length loop
               Data (Base + 7 + Offset) :=
                 Character'Pos (Item.Entries (Index).Prefix (Offset));
            end loop;
         end;
      end loop;
   end Encode;
end CuBit.File_Access;
