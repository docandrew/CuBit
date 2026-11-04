with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Exceptions;
with Ada.Strings.Hash;
with Build_Encoding;
with Build_Generations;
with Storehouse.Hex;

package body Build_Collection is
   use type Build_Store.Byte_Count;
   use Storehouse.Profiles;

   package Name_Sets is new Ada.Containers.Indefinite_Hashed_Sets (String, Ada.Strings.Hash, "=");

   function Key (Kind : Entry_Kind; Identity : Digest) return String is
     (Entry_Kind'Image (Kind) & "/" & Storehouse.Hex.Image (Identity));

   procedure Collect
     (Store : Build_Store.Store; Dry_Run : Boolean; Keep_Generations : Storehouse.Profiles.Generation_Count;
      Result : out Summary)
   is
      use Build_Generations;
      Marked : Name_Sets.Set;

      procedure Mark (Items : Item_Vectors.Vector) is
      begin
         for I of Items loop
            Marked.Include (Key (I.Kind, I.Identity));
         end loop;
      end Mark;

      procedure Mark_Root (Name : String; Data : Byte_Array; Live : Boolean) is
         Products, Pins : Item_Vectors.Vector;
      begin
         if not Live then
            Result.Dead_Roots := Result.Dead_Roots + 1;
            return;
         end if;
         Decode_Root (Data, Products, Pins);
         Mark (Products);
         Mark (Pins);
      exception
         when E : Build_Encoding.Decode_Error =>
            raise Collection_Error with "root " & Name & " is damaged (" & Ada.Exceptions.Exception_Message (E) &
              "); nothing was deleted";
      end Mark_Root;
      procedure Mark_Roots is new Build_Store.For_Each_Root (Mark_Root);

      procedure Sweep (Kind : Entry_Kind; Identity : Digest) is
      begin
         if Marked.Contains (Key (Kind, Identity)) then
            Result.Kept := Result.Kept + 1;
            return;
         end if;
         declare
            Bytes : constant Build_Store.Byte_Count := Store.Size_Of (Kind, Identity);
         begin
            Report (Kind, Identity, Bytes);
            Result.Deleted := Result.Deleted + 1;
            Result.Freed := Result.Freed + Bytes;
            if not Dry_Run then
               Store.Discard (Kind, Identity);
            end if;
         end;
      end Sweep;
      procedure Sweep_All is new Build_Store.For_Each_Entry (Sweep);
      Unlocked_Users : constant String := Store.Other_Users;
   begin
      Result := (others => <>);
      if Unlocked_Users /= "" then
         raise Collection_Error with "processes without the collection lock (older ccl-builds) are using the store:" &
           Unlocked_Users & "; nothing was deleted";
      end if;
      --  Mark: the generations kept, then the roots. Everything is read
      --  before anything is deleted.
      for Profile_Name of Build_Generations.Profiles (Store) loop
         declare
            All_Numbers : constant Number_Vectors.Vector := Numbers (Store, Profile_Name);
            Now_Current : constant Generation_Count := Current (Store, Profile_Name);
            Retained : Natural := 0;
         begin
            for Position in reverse All_Numbers.First_Index .. All_Numbers.Last_Index loop
               declare
                  N : constant Generation_Number := All_Numbers (Position);
                  Keep : constant Boolean := Keep_Generations = 0 or else Retained < Keep_Generations
                    or else N = Now_Current;
               begin
                  if Keep then
                     Retained := Retained + 1;
                     declare
                        G : constant Generation := Read (Store, Profile_Name, N);
                     begin
                        Mark (G.Value.Parameters);
                        for Part of G.Value.Components loop
                           Mark (Part.Outputs);
                        end loop;
                     end;
                  else
                     Result.Generations_Deleted := Result.Generations_Deleted + 1;
                  end if;
               end;
            end loop;
         end;
      end loop;
      Mark_Roots (Store);
      if Dry_Run then
         Sweep_All (Store);
         return;
      end if;
      --  Sweep: generations and dead roots first, so a crash part way
      --  leaves no root naming a deleted entry.
      if Keep_Generations /= 0 then
         for Profile_Name of Build_Generations.Profiles (Store) loop
            declare
               Deleted : Natural;
            begin
               Delete_Old (Store, Profile_Name, Keep_Generations, Deleted);
            end;
         end loop;
      end if;
      Store.Remove_Dead_Roots;
      Sweep_All (Store);
      Store.Empty_Damaged (Result.Damaged_Removed);
   end Collect;

end Build_Collection;
