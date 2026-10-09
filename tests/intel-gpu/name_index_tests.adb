with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Name_Index;
procedure Name_Index_Tests is
   package N renames Intel_GPU_Name_Index;
   use type N.Node;
   Nodes : array (1 .. 2048) of N.Node := [others => N.Empty];
   Available : Natural := 16;
   Reads, Writes : Natural := 0;
   function Capacity return Natural is (Available);
   function Read (Slot : Positive) return N.Node is
   begin Reads := Reads + 1; return Nodes (Slot); end Read;
   procedure Write (Slot : Positive; Item : N.Node) is
   begin Writes := Writes + 1; Nodes (Slot) := Item; end Write;
   package T is new N.Table (Capacity, Read, Write);
   Object : N.Index;
   Keys : array (Nodes'Range) of Unsigned_32 := [others => 0];
   OK : Boolean;
   Freed : Natural;
   Seed : Unsigned_32 := 16#C0FFEE01#;
   function Next_Key return Unsigned_32 is
   begin
      Seed := Seed xor Shift_Left (Seed, 13);
      Seed := Seed xor Shift_Right (Seed, 17);
      Seed := Seed xor Shift_Left (Seed, 5);
      return Seed;
   end Next_Key;
   procedure Check_All is
   begin
      for I in Keys'Range loop
         if Keys (I) /= 0 then
            Reads := 0;
            pragma Assert (T.Lookup (Object, Keys (I)) = I);
            pragma Assert (Reads <= 33);
         end if;
      end loop;
   end Check_All;
begin
   for I in Nodes'Range loop
      if I > Available then Available := Natural'Min (Nodes'Last, Available * 2); end if;
      Keys (I) := Next_Key;
      Reads := 0; Writes := 0;
      T.Insert (Object, Keys (I), I, I, OK);
      pragma Assert (OK and Reads <= 35 and Writes <= 2);
   end loop;
   Check_All;
   pragma Assert (T.Count (Object) = Nodes'Length);
   -- Repeatedly remove/reinsert at arbitrary positions, including internal
   -- nodes. Named object slots remain unchanged while index nodes move.
   for Turn in 1 .. 8192 loop
      declare
         Slot : constant Positive := Natural (Next_Key mod Unsigned_32 (Nodes'Length)) + 1;
         Old : constant Unsigned_32 := Keys (Slot);
      begin
         Reads := 0; Writes := 0;
         T.Remove (Object, Old, Freed, OK);
         pragma Assert (OK and Freed /= 0 and Nodes (Freed) = N.Empty);
         pragma Assert (Reads <= 68 and Writes <= 3);
         pragma Assert (T.Lookup (Object, Old) = 0);
         T.Insert (Object, Keys (1 + Slot mod Nodes'Length), Slot, Freed, OK);
         pragma Assert (not OK and Nodes (Freed) = N.Empty);
         Keys (Slot) := Next_Key;
         T.Insert (Object, Keys (Slot), Slot, Freed, OK); pragma Assert (OK);
         if Turn mod 128 = 0 then Check_All; end if;
      end;
   end loop;
   Check_All;
   for Key of Keys loop
      T.Remove (Object, Key, Freed, OK); pragma Assert (OK);
      pragma Assert (T.Lookup (Object, Key) = 0);
   end loop;
   pragma Assert (T.Count (Object) = 0 and not T.Quarantined (Object));
   -- Sequential names are adversarial for an ordinary unbalanced BST.
   for I in 1 .. 1024 loop
      Reads := 0;
      T.Insert (Object, Unsigned_32 (I), I, I, OK);
      pragma Assert (OK and Reads <= 35);
   end loop;
   for I in 1 .. 1024 loop
      Reads := 0;
      pragma Assert (T.Lookup (Object, Unsigned_32 (I)) = I and Reads <= 33);
   end loop;
   Nodes (1).Left := Nodes'Last + 1;
   T.Insert (Object, 1025, 1025, 1025, OK);
   pragma Assert (not OK and T.Quarantined (Object));
   pragma Assert (T.Lookup (Object, 1) = 0);
   Ada.Text_IO.Put_Line ("Name index PASS:2048 grown nodes,8192 replacements, bounded lookup/removal, stale names, sequential keys, quarantine");
end Name_Index_Tests;
