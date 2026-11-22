pragma Ada_2022;

package body CuBit.Introduction_Ledgers with SPARK_Mode is

   procedure Keep (L : in out Ledger; D : Decision; Into : out Slot_Count) is
      Free : Slot_Count := 0;
   begin
      Into := 0;
      for S in Slot loop
         if Used (L (S)) and then L (S).Holder = D.Holder and then L (S).Index = D.Index then
            return;
         end if;
         if Free = 0 and then not Used (L (S)) then
            Free := S;
         end if;
         pragma Loop_Invariant (Free = 0 or else (Free <= S and then not Used (L (Free))));
      end loop;
      if Free /= 0 then
         L (Free) := D;
         Into := Free;
      end if;
   end Keep;

   function Find (L : Ledger; Holder : Process_Word; Index : Unsigned_64) return Slot_Count is
   begin
      for S in Slot loop
         if Used (L (S)) and then L (S).Holder = Holder and then L (S).Index = Index then
            return S;
         end if;
         pragma Loop_Invariant
           (for all P in Slot'First .. S =>
              not (Used (L (P)) and then L (P).Holder = Holder and then L (P).Index = Index));
      end loop;
      return 0;
   end Find;

   function Next_For (L : Ledger; Role : Unsigned_64; From : Slot) return Slot_Count is
   begin
      for S in From .. Slot'Last loop
         if Used (L (S)) and then L (S).Role = Role
           and then (L (S).Way = Offer or else L (S).Waiting)
         then
            return S;
         end if;
      end loop;
      return 0;
   end Next_For;

   procedure Set_Waiting (L : in out Ledger; At_Slot : Slot) is
   begin
      L (At_Slot).Waiting := True;
   end Set_Waiting;

   procedure Remove (L : in out Ledger; At_Slot : Slot) is
   begin
      L (At_Slot) := (others => <>);
   end Remove;

   procedure Forget (L : in out Ledger; Holder : Process_Word) is
   begin
      for S in Slot loop
         if Used (L (S)) and then L (S).Holder = Holder then
            L (S) := (others => <>);
         end if;
         pragma Loop_Invariant
           (for all P in Slot =>
              (if P <= S then
                 (if Used (L'Loop_Entry (P)) and then L'Loop_Entry (P).Holder = Holder
                  then not Used (L (P)) else L (P) = L'Loop_Entry (P))
               else L (P) = L'Loop_Entry (P)));
      end loop;
   end Forget;

   function Count (L : Ledger) return Slot_Count is
      N : Slot_Count := 0;
   begin
      for S in Slot loop
         if Used (L (S)) then
            N := N + 1;
         end if;
         pragma Loop_Invariant (N <= S);
      end loop;
      return N;
   end Count;

end CuBit.Introduction_Ledgers;
