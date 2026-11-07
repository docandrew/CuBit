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

   function Admits_Staged (Item : Table_Bytes; Name : String) return Boolean is
      Count : constant Name_Count := Natural (U16_At (Item, Count_Offset));
      Position : Positive := Header_Bytes + 1;
      Length : Natural;
      Same : Boolean;

      --  No "", "." or ".." component in Name (From .. Name'Last).
      function Plain_Tail (From : Positive) return Boolean
        with Pre => From >= Name'First and then Name'Last < Positive'Last;
      function Plain_Tail (From : Positive) return Boolean is
         Start : Positive := From;
      begin
         if From > Name'Last then
            return False;
         end if;
         for K in From .. Name'Last loop
            pragma Loop_Invariant (Start in From .. K);
            if Name (K) = '/' then
               --  The component Name (Start .. K - 1) ends here.
               if K = Start
                 or else Name (Start .. K - 1) = "."
                 or else Name (Start .. K - 1) = ".."
                 or else K = Name'Last
               then
                  return False;
               end if;
               Start := K + 1;
            end if;
         end loop;
         return Name (Start .. Name'Last) /= "." and then Name (Start .. Name'Last) /= "..";
      end Plain_Tail;
   begin
      if Name'Length < 3 or else Name (Name'First) /= '@' or else Name'Last = Positive'Last then
         return False;
      end if;
      for Done in 1 .. Count loop
         pragma Loop_Invariant (Position <= Item'Last + 1);
         exit when Position > Item'Last;
         Length := Natural (Item (Position));
         exit when Length > Item'Last - Position;
         if Length >= 3 and then Length < Name'Length
           and then Item (Position + 1) = Character'Pos ('@')
           and then Item (Position + Length) = Character'Pos (Staging_Mark)
         then
            Same := True;
            for C in 0 .. Length - 1 loop
               if Item (Position + 1 + C) /= Character'Pos (Name (Name'First + C)) then
                  Same := False;
                  exit;
               end if;
            end loop;
            if Same and then Plain_Tail (Name'First + Length) then
               return True;
            end if;
         end if;
         Position := Position + 1 + Length;
      end loop;
      return False;
   end Admits_Staged;

   procedure Name_At
     (Item : Table_Bytes; Index : Positive; First : out Positive; Last : out Natural;
      Found : out Boolean)
   is
      Count : constant Name_Count := Natural (U16_At (Item, Count_Offset));
      Position : Positive := Header_Bytes + 1;
      Length : Natural;
   begin
      First := 1;
      Last := 0;
      Found := False;
      for Number in 1 .. Count loop
         pragma Loop_Invariant (Position <= Item'Last + 1);
         exit when Position > Item'Last;
         Length := Natural (Item (Position));
         exit when Length > Item'Last - Position;
         if Number = Index then
            First := Position + 1;
            Last := Position + Length;
            Found := True;
            return;
         end if;
         Position := Position + 1 + Length;
      end loop;
   end Name_At;

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
