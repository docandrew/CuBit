------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.File_Access;

package body CuBit.Launch_Authority with SPARK_Mode is

   function Valid (Item : Table_Bytes) return Boolean is
      Count : Name_Count;
      Position : Positive := Header_Bytes + 1;
      Length : Natural;
   begin
      if not Header_Valid (Item) then
         return False;
      end if;
      Count := Natural (U16_At (Item, Count_Offset));
      for Done in 0 .. Count - 1 loop
         pragma Loop_Invariant
           (Position <= Item'Last + 1
            and then Names_Valid (Item, Header_Bytes + 1, Count) =
                     Names_Valid (Item, Position, Count - Done));
         if Position > Item'Last then
            return False;
         end if;
         Length := Natural (Item (Position));
         if Length = 0 or else Length > Item'Last - Position then
            return False;
         end if;
         for K in Position + 1 .. Position + Length loop
            if Item (K) = 0 then
               return False;
            end if;
            pragma Loop_Invariant
              (for all J in Position + 1 .. K => Item (J) /= 0);
         end loop;
         Position := Position + 1 + Length;
      end loop;
      return Position = Item'Last + 1;
   end Valid;

   function Contains (Item : Table_Bytes; Name : String) return Boolean is
      Count : constant Name_Count := Natural (U16_At (Item, Count_Offset));
      Position : Positive := Header_Bytes + 1;
      Length : Natural;
      Same : Boolean;
   begin
      for Done in 1 .. Count loop
         pragma Loop_Invariant (Position <= Item'Last + 1);
         exit when Position > Item'Last;
         Length := Natural (Item (Position));
         exit when Length > Item'Last - Position;
         if Length = Name'Length then
            Same := True;
            for C in Name'Range loop
               if Item (Position + 1 + (C - Name'First)) /=
                  Character'Pos (Name (C))
               then
                  Same := False;
                  exit;
               end if;
            end loop;
            if Same then
               return True;
            end if;
         end if;
         Position := Position + 1 + Length;
      end loop;
      return False;
   end Contains;

   function Scope_Covered
     (Held_Service, Held_Rights : Unsigned_8; Held_Prefix : String;
      Service, Rights : Unsigned_8; Prefix : String) return Boolean is
   begin
      if Held_Service /= Service or else not Rights_Within (Held_Rights, Rights)
      then
         return False;
      elsif Service = Filesystem_Service then
         return CuBit.File_Access.Scope_Matches (Held_Prefix, Prefix);
      else
         return Held_Prefix = Prefix;
      end if;
   end Scope_Covered;

end CuBit.Launch_Authority;
