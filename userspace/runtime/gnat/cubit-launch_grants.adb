pragma Ada_2022;

package body CuBit.Launch_Grants with SPARK_Mode is

   function Valid (Item : Bytes) return Boolean is
      Position : Positive := Header_Bytes + 1;
      Remaining : Grant_Count;
      Length : Natural;
   begin
      if not Header_Valid (Item) then
         return False;
      end if;
      Remaining := Count_Of (Item);
      while Remaining > 0 loop
         pragma Loop_Invariant (Position <= Item'Last + 1);
         pragma Loop_Invariant
           (Entries_Valid (Item, Header_Bytes + 1, Count_Of (Item)) =
            Entries_Valid (Item, Position, Remaining));
         pragma Loop_Variant (Decreases => Remaining);
         if Position + 2 > Item'Last or else not Valid_Rights (Item (Position)) then
            return False;
         end if;
         Length := Length_At (Item, Position);
         if Length not in Name_Length or else Length > Item'Last - Position - 2
           or else Item (Position + 3) /= Character'Pos ('@')
         then
            return False;
         end if;
         for K in Position + 3 .. Position + 2 + Length loop
            if Item (K) not in 32 .. 126 then
               return False;
            end if;
            pragma Loop_Invariant
              (for all J in Position + 3 .. K => Item (J) in 32 .. 126);
         end loop;
         pragma Assert
           (for all J in Position + 3 .. Position + 2 + Length_At (Item, Position) =>
              Item (J) in 32 .. 126);
         --  This entry checks out: validity from here is validity of the rest.
         pragma Assert
           (Entries_Valid (Item, Position, Remaining) =
            Entries_Valid (Item, Position + 3 + Length, Remaining - 1));
         Position := Position + 3 + Length;
         Remaining := Remaining - 1;
      end loop;
      return Position = Item'Last + 1;
   end Valid;

   procedure Next
     (Item : Bytes; Position : in out Positive; Rights : out Unsigned_8;
      Name_First, Name_Last : out Positive) is
   begin
      Rights := Item (Position);
      Name_First := Position + 3;
      Name_Last := Position + 2 + Length_At (Item, Position);
      Position := Name_Last + 1;
   end Next;

   procedure Start (B : out Builder) is
   begin
      B := (Data => [others => 0], Used => Header_Bytes, Grants => 0);
   end Start;

   procedure Add (B : in out Builder; Rights : Unsigned_8; Name : String;
                  Added : out Boolean) is
   begin
      Added := B.Grants < Maximum_Grants and then Valid_Rights (Rights)
        and then Valid_Name (Name);
      if not Added then
         return;
      end if;
      B.Data (B.Used + 1) := Rights;
      B.Data (B.Used + 2) := Unsigned_8 (Name'Length mod 256);
      B.Data (B.Used + 3) := Unsigned_8 (Name'Length / 256);
      for K in Name'Range loop
         B.Data (B.Used + 3 + K) := Character'Pos (Name (K));
         pragma Loop_Invariant (B.Used + 3 + K <= Maximum_Bytes);
      end loop;
      B := (B with delta Used => B.Used + Entry_Header_Bytes + Name'Length,
                         Grants => B.Grants + 1);
   end Add;

   procedure Finish (B : Builder; Region : out Bytes; Length : out Byte_Count) is
   begin
      Region := B.Data;
      Region (1) := Version;
      Region (2) := 0;
      Region (3) := Unsigned_8 (B.Grants mod 256);
      Region (4) := Unsigned_8 (B.Grants / 256);
      Length := (if B.Grants = 0 then 0 else B.Used);
   end Finish;
end CuBit.Launch_Grants;
