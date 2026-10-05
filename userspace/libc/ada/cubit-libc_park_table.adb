------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.Libc_Park_Table with SPARK_Mode is

   FNV_Offset : constant Unsigned_64 := 16#CBF2_9CE4_8422_2325#;
   FNV_Prime  : constant Unsigned_64 := 16#0000_0100_0000_01B3#;
   Hash_Shift : constant := 40;

   function Name_Hash (Name : String) return Unsigned_64 is
      H : Unsigned_64 := FNV_Offset;
   begin
      for C of Name loop
         H := (H xor Unsigned_64 (Character'Pos (C))) * FNV_Prime;
      end loop;
      return H;
   end Name_Hash;

   function Bucket_Of (Hash : Unsigned_64) return Bucket is
     (Bucket (Shift_Right (Hash, Hash_Shift) and (Buckets - 1)));

   procedure Set_Name (T : in out Table; S : Slot; Name : String) is
   begin
      T.Entries (S).Name := [others => ' '];
      T.Entries (S).Name (1 .. Name'Length) := Name;
      T.Entries (S).Length := Name'Length;
      T.Entries (S).Hash := Name_Hash (Name);
   end Set_Name;

   procedure Forget_Name (T : in out Table; S : Slot) is
   begin
      T.Entries (S).Length := 0;
   end Forget_Name;

   procedure Find (T : Table; Name : String; Found : out Link) is
      Hash : constant Unsigned_64 := Name_Hash (Name);
      L : Link := T.Heads (Bucket_Of (Hash));
   begin
      Found := No_Link;
      for Steps in 1 .. Slots loop
         exit when L = No_Link;
         if T.Entries (L - 1).Parked and then T.Entries (L - 1).Hash = Hash
           and then T.Entries (L - 1).Length = Name'Length
           and then T.Entries (L - 1).Name (1 .. Name'Length) = Name
         then
            Found := L;
            return;
         end if;
         L := T.Entries (L - 1).Chain;
      end loop;
   end Find;

   procedure Insert (T : in out Table; S : Slot) is
      B : constant Bucket := Bucket_Of (T.Entries (S).Hash);
   begin
      T.Entries (S).Parked := True;
      T.Entries (S).Chain := T.Heads (B);
      T.Heads (B) := S + 1;
      T.Entries (S).Newer := No_Link;
      T.Entries (S).Older := T.Newest;
      if T.Newest /= No_Link then
         T.Entries (T.Newest - 1).Newer := S + 1;
      end if;
      T.Newest := S + 1;
      if T.Oldest = No_Link then
         T.Oldest := S + 1;
      end if;
      if T.Count < Slots then
         T.Count := T.Count + 1;
      end if;
   end Insert;

   procedure Remove (T : in out Table; S : Slot) is
      B : constant Bucket := Bucket_Of (T.Entries (S).Hash);
      L : Link := T.Heads (B);
      Newer : constant Link := T.Entries (S).Newer;
      Older : constant Link := T.Entries (S).Older;
   begin
      --  Out of its hash chain.
      if L = S + 1 then
         T.Heads (B) := T.Entries (S).Chain;
      else
         for Steps in 1 .. Slots loop
            exit when L = No_Link;
            if T.Entries (L - 1).Chain = S + 1 then
               T.Entries (L - 1).Chain := T.Entries (S).Chain;
               exit;
            end if;
            L := T.Entries (L - 1).Chain;
            pragma Loop_Invariant (T.Entries (S).Parked
                                   and then T.Entries (S).Length = T.Entries'Loop_Entry (S).Length);
         end loop;
      end if;
      --  Out of the recency list.
      if Newer /= No_Link then
         T.Entries (Newer - 1).Older := Older;
      else
         T.Newest := Older;
      end if;
      if Older /= No_Link then
         T.Entries (Older - 1).Newer := Newer;
      else
         T.Oldest := Newer;
      end if;
      T.Entries (S).Parked := False;
      T.Entries (S).Chain := No_Link;
      if T.Count > 0 then
         T.Count := T.Count - 1;
      end if;
   end Remove;

end CuBit.Libc_Park_Table;
