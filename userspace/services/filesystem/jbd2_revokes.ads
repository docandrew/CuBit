with Interfaces; use Interfaces;
with Jbd2_Format;

--  The revoke table built from a journal's committed revoke blocks, and the
--  replay decision for one journaled block. A bounded table: a journal with
--  more revoke records is refused (not partially honoured).
package Jbd2_Revokes with Pure, SPARK_Mode is
   Capacity : constant := 16_384;
   subtype Entry_Count is Natural range 0 .. Capacity;
   subtype Entry_Index is Positive range 1 .. Capacity;

   type Revocation is record
      Home : Unsigned_64;
      Sequence : Unsigned_32; -- transaction that revoked Home
   end record;
   type Revocation_Array is array (Entry_Index) of Revocation;
   type Table is record
      Entries : Revocation_Array;
      Count : Entry_Count;
   end record;

   Empty : constant Table :=
     (Entries => [others => (Home => 0, Sequence => 0)], Count => 0);

   --  Linux jbd2_journal_test_revoke: a revoke recorded in transaction R
   --  cancels replay of that block from transaction T unless T is later.
   function Cancels (Item : Revocation; Home : Unsigned_64; Transaction : Unsigned_32)
      return Boolean is
     (Item.Home = Home and then not Jbd2_Format.Later (Transaction, Item.Sequence));

   function Revoked (Revokes : Table; Home : Unsigned_64; Transaction : Unsigned_32)
      return Boolean is
     (for some I in 1 .. Revokes.Count => Cancels (Revokes.Entries (I), Home, Transaction));

   --  Append one record; Stored is False (and nothing changes) only when the
   --  table is full.
   procedure Record_Revoke
     (Revokes : in out Table; Home : Unsigned_64; Sequence : Unsigned_32;
      Stored : out Boolean)
     with Post =>
       Stored = (Revokes'Old.Count < Capacity) and then
       (if Stored then
          Revokes.Count = Revokes'Old.Count + 1 and then
          Revokes.Entries (Revokes.Count) = (Home => Home, Sequence => Sequence) and then
          (for all I in 1 .. Revokes'Old.Count =>
             Revokes.Entries (I) = Revokes'Old.Entries (I))
        else Revokes = Revokes'Old);

   type Replay_Action is (Write_Home, Skip_Revoked, Out_Of_Range);

   --  What replay does with the log copy of Home from Transaction: never a
   --  write outside the filesystem, never a write a later revoke cancelled.
   function Action
     (Revokes : Table; Home : Unsigned_64; Transaction : Unsigned_32;
      Filesystem_Blocks : Unsigned_32) return Replay_Action
     with Post =>
       (Action'Result = Write_Home) =
         (Home < Unsigned_64 (Filesystem_Blocks) and then
          not Revoked (Revokes, Home, Transaction)) and then
       (Action'Result = Out_Of_Range) = (Home >= Unsigned_64 (Filesystem_Blocks));
end Jbd2_Revokes;
