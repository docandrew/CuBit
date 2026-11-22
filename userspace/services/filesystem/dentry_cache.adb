package body Dentry_Cache with SPARK_Mode is
   pragma Assertion_Policy (Pre => Ignore, Post => Ignore, Ghost => Ignore,
                            Loop_Invariant => Ignore, Assert => Ignore);

   FNV_Offset : constant Unsigned_32 := 16#811C_9DC5#;
   FNV_Prime  : constant Unsigned_32 := 16#0100_0193#;

   --  FNV-1a over the name, mixed with the directory and volume.
   function Make_Key (Volume : Unsigned_64; Parent : Unsigned_32; Name : String)
      return Name_Key
   is
      Result : Name_Key :=
        (Volume => Volume, Parent => Parent, Length => Name'Length,
         Name => [others => ASCII.NUL], Set => 0);
      Hash : Unsigned_32 := FNV_Offset;
   begin
      for Index in 1 .. Name'Length loop
         Result.Name (Index) := Name (Name'First + (Index - 1));
         Hash := (Hash xor Character'Pos (Result.Name (Index))) * FNV_Prime;
      end loop;
      Hash := (Hash xor Parent) * FNV_Prime;
      Hash := (Hash xor Unsigned_32 (Volume and 16#FFFF_FFFF#)) * FNV_Prime;
      Result.Set := Set_Index (Hash mod Sets);
      return Result;
   end Make_Key;

   --  Under Valid, a key can only be held within its own set.
   procedure Lemma_Outside_Set (Cache : Table; Key : Name_Key)
     with Ghost,
          Pre => Valid (Cache) and then
                 (for all Way in Way_Index =>
                    not Holds (Cache, Slot_Of (Set_Of (Key), Way), Key)),
          Post => not Contains (Cache, Key)
   is
   begin
      for Slot in Slot_Index loop
         pragma Assert
           (if Holds (Cache, Slot, Key) then
              Slot / Ways = Set_Of (Key) and then
              Slot = Slot_Of (Set_Of (Key), Slot mod Ways));
         pragma Loop_Invariant
           (for all Earlier in Slot_Index'First .. Slot =>
              not Holds (Cache, Earlier, Key));
      end loop;
   end Lemma_Outside_Set;

   procedure Find
     (Cache : Table; Key : Name_Key; Found : out Boolean;
      Inode : out Unsigned_32)
   is
      Set : constant Set_Index := Set_Of (Key);
   begin
      Found := False;
      Inode := No_Inode;
      for Way in Way_Index loop
         if Holds (Cache, Slot_Of (Set, Way), Key) then
            Found := True;
            Inode := Cache.Inodes (Slot_Of (Set, Way));
            return;
         end if;
         pragma Loop_Invariant
           (for all Earlier in Way_Index'First .. Way =>
              not Holds (Cache, Slot_Of (Set, Earlier), Key));
      end loop;
      Lemma_Outside_Set (Cache, Key);
   end Find;

   procedure Clear (Cache : out Table) is
   begin
      Cache :=
        (Keys => [others => (Volume => 0, Parent => 0, Length => 0,
                             Name => [others => ASCII.NUL], Set => 0)],
         Inodes => [others => No_Inode], Used => [others => False],
         Hands => [others => 0],
         Directories => [others => (Volume => 0, Parent => 0)],
         Complete => [others => False], Complete_Hand => 0);
   end Clear;

   procedure Mark_Incomplete
     (Cache : in out Table; Volume : Unsigned_64; Parent : Unsigned_32) is
   begin
      for Index in Complete_Index loop
         if Cache.Directories (Index) = (Volume => Volume, Parent => Parent) then
            Cache.Complete (Index) := False;
         end if;
         pragma Loop_Invariant
           (for all Earlier in Complete_Index'First .. Index =>
              (if Cache.Complete (Earlier) then
                 Cache.Directories (Earlier) /= (Volume => Volume, Parent => Parent)));
         pragma Loop_Invariant (Cache.Directories = Cache'Loop_Entry.Directories);
         pragma Loop_Invariant
           (for all Other in Complete_Index =>
              (if Cache.Complete (Other) then Cache'Loop_Entry.Complete (Other)));
         pragma Loop_Invariant
           (Cache.Keys = Cache'Loop_Entry.Keys and then
            Cache.Used = Cache'Loop_Entry.Used and then
            Cache.Inodes = Cache'Loop_Entry.Inodes);
      end loop;
   end Mark_Incomplete;

   procedure Mark_Complete
     (Cache : in out Table; Volume : Unsigned_64; Parent : Unsigned_32)
   is
      Hand : constant Complete_Index := Cache.Complete_Hand;
   begin
      if Is_Complete (Cache, Volume, Parent) then
         return;
      end if;
      Cache.Directories (Hand) := (Volume => Volume, Parent => Parent);
      Cache.Complete (Hand) := True;
      Cache.Complete_Hand := (if Hand = Complete_Index'Last then 0 else Hand + 1);
   end Mark_Complete;

   procedure Insert
     (Cache : in out Table; Key : Name_Key; Inode : Unsigned_32;
      Displaced : out Boolean; Displaced_From : out Directory_Id)
   is
      Set : constant Set_Index := Set_Of (Key);
      Target : Way_Index := Cache.Hands (Set);
      Existing : Boolean := False;
   begin
      Displaced := False;
      Displaced_From := (Volume => 0, Parent => 0);
      for Way in Way_Index loop
         if Holds (Cache, Slot_Of (Set, Way), Key) then
            Target := Way;
            Existing := True;
            exit;
         end if;
         pragma Loop_Invariant
           (for all Earlier in Way_Index'First .. Way =>
              not Holds (Cache, Slot_Of (Set, Earlier), Key));
      end loop;
      if not Existing then
         Lemma_Outside_Set (Cache, Key);
         for Way in Way_Index loop
            if not Cache.Used (Slot_Of (Set, Way)) then
               Target := Way;
               exit;
            end if;
         end loop;
         --  Round-robin among full ways.
         Cache.Hands (Set) := (if Target = Way_Index'Last then 0 else Target + 1);
         if Cache.Used (Slot_Of (Set, Target)) then
            --  Another name leaves the cache: its directory is incomplete.
            Displaced := True;
            Displaced_From :=
              (Volume => Cache.Keys (Slot_Of (Set, Target)).Volume,
               Parent => Cache.Keys (Slot_Of (Set, Target)).Parent);
            Mark_Incomplete (Cache, Displaced_From.Volume, Displaced_From.Parent);
         end if;
      end if;
      Cache.Keys (Slot_Of (Set, Target)) := Key;
      Cache.Inodes (Slot_Of (Set, Target)) := Inode;
      Cache.Used (Slot_Of (Set, Target)) := True;
   end Insert;

   procedure Forget (Cache : in out Table; Key : Name_Key) is
      Set : constant Set_Index := Set_Of (Key);
   begin
      for Way in Way_Index loop
         if Holds (Cache, Slot_Of (Set, Way), Key) then
            Cache.Used (Slot_Of (Set, Way)) := False;
         end if;
         pragma Loop_Invariant
           (for all Earlier in Way_Index'First .. Way =>
              not Holds (Cache, Slot_Of (Set, Earlier), Key));
         pragma Loop_Invariant (Cache.Keys = Cache'Loop_Entry.Keys);
         pragma Loop_Invariant
           (Cache.Complete = Cache'Loop_Entry.Complete and then
            Cache.Directories = Cache'Loop_Entry.Directories);
         pragma Loop_Invariant
           (for all Slot in Slot_Index =>
              (if Cache.Used (Slot) then Cache'Loop_Entry.Used (Slot)));
      end loop;
      Lemma_Outside_Set (Cache, Key);
   end Forget;

   procedure Forget_Directory
     (Cache : in out Table; Volume : Unsigned_64; Parent : Unsigned_32) is
   begin
      Mark_Incomplete (Cache, Volume, Parent);
      for Slot in Slot_Index loop
         if Cache.Used (Slot) and then Cache.Keys (Slot).Volume = Volume and then
           Cache.Keys (Slot).Parent = Parent
         then
            Cache.Used (Slot) := False;
         end if;
         pragma Loop_Invariant
           (for all Earlier in Slot_Index'First .. Slot =>
              (if Cache.Used (Earlier) then
                 Cache.Keys (Earlier).Volume /= Volume or else
                 Cache.Keys (Earlier).Parent /= Parent));
         pragma Loop_Invariant (Cache.Keys = Cache'Loop_Entry.Keys);
         pragma Loop_Invariant
           (Cache.Complete = Cache'Loop_Entry.Complete and then
            Cache.Directories = Cache'Loop_Entry.Directories);
         pragma Loop_Invariant
           (for all Other in Slot_Index =>
              (if Cache.Used (Other) then Cache'Loop_Entry.Used (Other)));
      end loop;
   end Forget_Directory;

   procedure Discard_Volume (Cache : in out Table; Volume : Unsigned_64) is
   begin
      for Index in Complete_Index loop
         if Cache.Directories (Index).Volume = Volume then
            Cache.Complete (Index) := False;
         end if;
         pragma Loop_Invariant
           (for all Earlier in Complete_Index'First .. Index =>
              (if Cache.Complete (Earlier) then
                 Cache.Directories (Earlier).Volume /= Volume));
      end loop;
      for Slot in Slot_Index loop
         if Cache.Used (Slot) and then Cache.Keys (Slot).Volume = Volume then
            Cache.Used (Slot) := False;
         end if;
         pragma Loop_Invariant
           (for all Earlier in Slot_Index'First .. Slot =>
              (if Cache.Used (Earlier) then Cache.Keys (Earlier).Volume /= Volume));
         pragma Loop_Invariant (Cache.Keys = Cache'Loop_Entry.Keys);
         pragma Loop_Invariant
           (Cache.Complete = Cache'Loop_Entry.Complete and then
            Cache.Directories = Cache'Loop_Entry.Directories);
         pragma Loop_Invariant
           (for all Other in Slot_Index =>
              (if Cache.Used (Other) then Cache'Loop_Entry.Used (Other)));
      end loop;
   end Discard_Volume;
end Dentry_Cache;
