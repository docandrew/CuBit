package body Jbd2_Revokes with SPARK_Mode is
   procedure Clear (Revokes : out Table) is
   begin
      Revokes := (Entries => [others => (Home => 0, Sequence => 0)], Count => 0);
   end Clear;

   procedure Record_Revoke
     (Revokes : in out Table; Home : Unsigned_64; Sequence : Unsigned_32;
      Stored : out Boolean)
   is
   begin
      Stored := Revokes.Count < Capacity;
      if Stored then
         Revokes.Count := Revokes.Count + 1;
         Revokes.Entries (Revokes.Count) := (Home => Home, Sequence => Sequence);
      end if;
   end Record_Revoke;

   function Action
     (Revokes : Table; Home : Unsigned_64; Transaction : Unsigned_32;
      Filesystem_Blocks : Unsigned_32) return Replay_Action
   is
   begin
      if Home >= Unsigned_64 (Filesystem_Blocks) then
         return Out_Of_Range;
      end if;
      for I in 1 .. Revokes.Count loop
         if Cancels (Revokes.Entries (I), Home, Transaction) then
            return Skip_Revoked;
         end if;
         pragma Loop_Invariant
           (for all J in 1 .. I => not Cancels (Revokes.Entries (J), Home, Transaction));
      end loop;
      return Write_Home;
   end Action;
end Jbd2_Revokes;
