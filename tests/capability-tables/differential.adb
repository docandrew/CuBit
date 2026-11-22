with Ada.Text_IO; use Ada.Text_IO;
with Checks; use Checks;
with Fake_Pages;

procedure Differential
  (Seed       : Unsigned_64;
   Operations : Natural;
   Owner      : Unsigned_64;
   Name       : String)
is
   use Tab;
   use type Fake_Pages.Page_Total;

   Max : constant Natural := Natural (Max_Slots);
   Per_Leaf : constant Natural := Natural (Entries_Per_Leaf);
   subtype Ref_Index is Natural range 0 .. Max - 1;

   -- Reference model: deliberately plain, written from the specification
   -- in docs/capability-handles.md, not from the implementation.
   type Ref_State is (Unused, Free, Live, Retired);
   type Ref_Slot is record
      State : Ref_State := Unused;
      Gen   : Unsigned_32 := 0;
      Cause : Vacancy_Cause := Closed;
      Id    : Unsigned_64 := 0;
   end record;
   type Ref_Array is array (Ref_Index) of Ref_Slot;
   type Ref_Array_Access is access Ref_Array;
   type Fifo_Array is array (Ref_Index) of Ref_Index;
   type Fifo_Access is access Fifo_Array;

   Ref       : constant Ref_Array_Access := new Ref_Array;
   Fifo      : constant Fifo_Access := new Fifo_Array;
   Fifo_Head : Natural := 0;   -- index of the oldest freed slot
   Fifo_Len  : Natural := 0;
   High      : Natural := 0;
   Live_N    : Natural := 0;
   Ref_Limit : Handle_Limit := Default_Limit;

   T : Table;

   -- xorshift64*: fixed seed, reproducible on every host.
   State_Word : Unsigned_64 := (if Seed = 0 then 1 else Seed);
   function Next_Random return Unsigned_64 is
   begin
      State_Word := State_Word xor Shift_Right (State_Word, 12);
      State_Word := State_Word xor Shift_Left (State_Word, 25);
      State_Word := State_Word xor Shift_Right (State_Word, 27);
      return State_Word * 16#2545_F491_4F6C_DD1D#;
   end Next_Random;
   function Below (N : Positive) return Natural is
     (Natural (Next_Random mod Unsigned_64 (N)));

   -- Handles to try: recently issued ones (some now stale) and forgeries.
   Pool_Size : constant := 512;
   type Pool_Array is array (0 .. Pool_Size - 1) of Unsigned_64;
   Pool   : Pool_Array := [others => 0];
   Pooled : Natural := 0;

   Next_Id : Unsigned_64 := 1;

   function Word (Slot : Natural; Gen : Unsigned_32) return Unsigned_64 is
     (Shift_Left (Unsigned_64 (Gen), 32) or Unsigned_64 (Slot));

   function Expected_Pages return Natural is
     (if High = 0 then 0 else 1 + (High + Per_Leaf - 1) / Per_Leaf);

   function Classify (W : Unsigned_64) return Handle_Check is
      Slot : constant Unsigned_64 := W and 16#FFFF_FFFF#;
      Gen  : constant Unsigned_32 := Unsigned_32 (Shift_Right (W, 32));
   begin
      if Slot >= Unsigned_64 (High) or else Gen = 0 then
         return Invalid;
      end if;
      declare
         R : Ref_Slot renames Ref (Natural (Slot));
      begin
         if Gen > R.Gen then
            return Invalid;
         elsif Gen < R.Gen then
            return Stale;
         elsif R.State = Live then
            return Current;
         elsif R.Cause = Revoked then
            return Revoked;
         else
            return Stale;
         end if;
      end;
   end Classify;

   procedure Remember (W : Unsigned_64) is
   begin
      Pool (Pooled mod Pool_Size) := W;
      Pooled := Pooled + 1;
   end Remember;

   function Pick_Word return Unsigned_64 is
   begin
      case Below (10) is
         when 0 =>
            -- Forged: random slot near the high-water mark, random generation.
            return Word (Below (Max + 2) mod (Max + 2),
                         Unsigned_32 (Below (Natural (Unsigned_32'Min
                           (Last_Generation, 1000)) + 2)));
         when 1 =>
            return Next_Random;  -- any word at all
         when others =>
            if Pooled = 0 then
               return 0;
            end if;
            return Pool (Below (Natural'Min (Pooled, Pool_Size)));
      end case;
   end Pick_Word;

   procedure Compare_All (Op : Natural) is
      Bad : Boolean := False;
   begin
      Check (High_Water (T) = Slot_Count (High), "high water at op" & Op'Image);
      Check (Live_Count (T) = Slot_Count (Live_N), "live count at op" & Op'Image);
      Check (Natural (Pages_Held (T)) = Expected_Pages, "pages held at op" & Op'Image);
      Check (Natural (Fake_Pages.Held (Owner)) = Expected_Pages,
             "allocator charge = pages held at op" & Op'Image);
      Check (Live_Count (T) <= Bound (Limit (T)), "live <= limit at op" & Op'Image);
      for S in 0 .. Natural'Min (High, Max - 1) loop
         declare
            E : constant Slot_Entry := Entry_Of (T, Slot_Index (S));
            R : Ref_Slot renames Ref (S);
         begin
            if S = High then
               Bad := Bad or else E /= Unused_Entry;
            else
               Bad := Bad
                 or else E.Gen /= R.Gen
                 or else (case R.State is
                            when Unused  => True,
                            when Free    => E.State /= Free,
                            when Live    => E.State /= Live
                                            or else Id_Of (E.Item) /= R.Id,
                            when Retired => E.State /= Retired)
                 or else (R.State /= Live and then E.Cause /= R.Cause)
                 or else (R.State = Live) /= Is_Live_At (T, Slot_Index (S));
            end if;
         end;
      end loop;
      Check (not Bad, "every slot matches the model at op" & Op'Image);
   end Compare_All;

   procedure Do_Insert (Op : Natural; Budget : Integer) is
      Result : Insert_Result;
      Id     : constant Unsigned_64 := Next_Id;
      Want   : Insert_Status;
      Slot   : Natural := 0;
      Need   : Natural := 0;  -- pages a fresh slot needs
   begin
      Next_Id := Next_Id + 1;
      if Ref_Limit.Kind = Bounded and then Live_N >= Natural (Ref_Limit.Most) then
         Want := Limit_Reached;
      elsif Live_N >= Max then
         Want := Table_Full;
      elsif Fifo_Len > 0 then
         Want := Opened;
         Slot := Fifo (Fifo_Head);
      elsif High < Max then
         Slot := High;
         Need := (if High = 0 then 2 elsif High mod Per_Leaf = 0 then 1 else 0);
         Want := (if Budget >= 0 and then Need > Budget then Out_Of_Memory
                  else Opened);
      else
         Want := Table_Full;
      end if;

      if Budget >= 0 then
         Fake_Pages.Set_Budget (Fake_Pages.Page_Total (Budget));
      end if;
      Insert (T, Make (Id), Result);
      Fake_Pages.Clear_Budget;

      Check (Result.Status = Want, "insert status at op" & Op'Image & ": got "
             & Result.Status'Image & " want " & Want'Image);
      if Result.Status /= Want or else Want /= Opened then
         return;
      end if;

      if Fifo_Len > 0 then
         Fifo_Head := (Fifo_Head + 1) mod Max;
         Fifo_Len := Fifo_Len - 1;
         Ref (Slot).Gen := Ref (Slot).Gen + 1;
      else
         High := High + 1;
         Ref (Slot).Gen := 1;
      end if;
      Ref (Slot).State := Live;
      Ref (Slot).Id := Id;
      Live_N := Live_N + 1;
      Check (To_Word (Result.Opened_Handle) = Word (Slot, Ref (Slot).Gen),
             "insert chose the least recently freed slot at op" & Op'Image);
      Remember (To_Word (Result.Opened_Handle));
   end Do_Insert;

   procedure Do_Remove (Op : Natural; Cause : Vacancy_Cause) is
      W      : constant Unsigned_64 := Pick_Word;
      Want   : constant Handle_Check := Classify (W);
      Result : Remove_Result;
   begin
      if Cause = Closed then
         Close (T, From_Word (W), Result);
      else
         Revoke (T, From_Word (W), Result);
      end if;
      Check (Result.Check = Want, "remove check at op" & Op'Image);
      if Result.Check /= Want or else Want /= Current then
         return;
      end if;
      declare
         S : constant Natural := Natural (W and 16#FFFF_FFFF#);
      begin
         Check (Id_Of (Result.Removed) = Ref (S).Id, "removed item at op" & Op'Image);
         Ref (S).Cause := Cause;
         Ref (S).Id := 0;
         if Ref (S).Gen < Last_Generation then
            Ref (S).State := Free;
            Fifo ((Fifo_Head + Fifo_Len) mod Max) := S;
            Fifo_Len := Fifo_Len + 1;
         else
            Ref (S).State := Retired;
         end if;
         Live_N := Live_N - 1;
         Check (Lookup (T, From_Word (W)).Check =
                  (if Cause = Revoked then Revoked else Stale),
                "removed handle no longer resolves at op" & Op'Image);
      end;
   end Do_Remove;

   procedure Do_Lookup (Op : Natural) is
      W      : constant Unsigned_64 := Pick_Word;
      Want   : constant Handle_Check := Classify (W);
      Result : constant Lookup_Result := Lookup (T, From_Word (W));
   begin
      Check (Result.Check = Want, "lookup check at op" & Op'Image);
      if Result.Check = Current and then Want = Current then
         Check (Id_Of (Result.Item) = Ref (Natural (W and 16#FFFF_FFFF#)).Id,
                "lookup item at op" & Op'Image);
      end if;
      Check (To_Word (From_Word (W)) = W, "word round trip at op" & Op'Image);
   end Do_Lookup;

   procedure Do_Restart (Op : Natural) is
   begin
      Release_All (T);
      Check (Is_Empty (T), "empty after release at op" & Op'Image);
      Check (Fake_Pages.Held (Owner) = 0, "every page returned at op" & Op'Image);
      case Below (4) is
         when 0 => Ref_Limit := Kernel_Limit_Handles;
         when 1 => Ref_Limit := (Kind => Bounded, Most => Limit_Count (1 + Below (8)));
         when 2 => Ref_Limit := (Kind => Bounded, Most => Limit_Count (1 + Below (Max)));
         when others => Ref_Limit := Default_Limit;
      end case;
      Configure (T, Owner, Ref_Limit);
      Ref.all := [others => (State => Unused, Gen => 0, Cause => Closed, Id => 0)];
      Fifo_Head := 0;
      Fifo_Len := 0;
      High := 0;
      Live_N := 0;
   end Do_Restart;

   Inserts, Removes, Lookups, Restarts, Failures_Injected : Natural := 0;
begin
   Section ("differential " & Name);
   Configure (T, Owner, Ref_Limit);
   for Op in 1 .. Operations loop
      declare
         Roll : constant Natural := Below (1000);
      begin
         if Roll < 430 then
            Do_Insert (Op, -1);
            Inserts := Inserts + 1;
         elsif Roll < 460 then
            -- Insert with the allocator refusing after 0 or 1 pages.
            Do_Insert (Op, Below (2));
            Failures_Injected := Failures_Injected + 1;
         elsif Roll < 680 then
            Do_Remove (Op, Closed);
            Removes := Removes + 1;
         elsif Roll < 760 then
            Do_Remove (Op, Revoked);
            Removes := Removes + 1;
         elsif Roll < 998 then
            Do_Lookup (Op);
            Lookups := Lookups + 1;
         else
            Do_Restart (Op);
            Restarts := Restarts + 1;
         end if;
      end;
      Compare_All (Op);
   end loop;
   Release_All (T);
   Check (Fake_Pages.Held (Owner) = 0, "every page returned at the end");
   Check (Fake_Pages.Bad_Releases = 0, "no foreign or double release");
   Put_Line ("   ops" & Operations'Image & ": inserts" & Inserts'Image
             & ", failing inserts" & Failures_Injected'Image
             & ", removes" & Removes'Image & ", lookups" & Lookups'Image
             & ", restarts" & Restarts'Image);
end Differential;
