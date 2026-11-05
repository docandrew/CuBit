------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.Channel_Admission with SPARK_Mode is

   procedure Start (S : out State; Declared : Limit_Table) is
   begin
      S.Declared := Declared;
      S.Totals := No_Counts;
      S.Clients := [others => No_Record];
      for O in Offer_Index loop
         for N in 1 .. S.Size loop
            pragma Loop_Invariant (Sum (S.Clients, O, N - 1) = 0);
         end loop;
         pragma Loop_Invariant
           (for all P in Offer_Index'First .. O => Sum (S.Clients, P, S.Size) = 0);
      end loop;
   end Start;

   function Index_Of (S : State; Client : Client_Id) return Table_Size is
   begin
      for C in 1 .. S.Size loop
         if S.Clients (C).Id = Client then
            return C;
         end if;
         pragma Loop_Invariant (for all D in 1 .. C => S.Clients (D).Id /= Client);
      end loop;
      raise Program_Error;  --  unreachable: the precondition names one
   end Index_Of;

   procedure Lemma_Sum_Step
     (Before, After : Client_Table; Offer : Offer_Index; K : Table_Size; Up : Boolean) is
   begin
      for N in 1 .. Before'Last loop
         pragma Loop_Invariant
           (if N <= K then Sum (After, Offer, N - 1) = Sum (Before, Offer, N - 1)
            elsif Up then Sum (After, Offer, N - 1) = Sum (Before, Offer, N - 1) + 1
            else Sum (After, Offer, N - 1) + 1 = Sum (Before, Offer, N - 1));
      end loop;
   end Lemma_Sum_Step;

   procedure Lemma_Sum_Same (Before, After : Client_Table; Offer : Offer_Index) is
   begin
      for N in 1 .. Before'Last loop
         pragma Loop_Invariant (Sum (After, Offer, N - 1) = Sum (Before, Offer, N - 1));
      end loop;
   end Lemma_Sum_Same;

   procedure Lemma_Sum_Extended (Before, After : Client_Table; Offer : Offer_Index) is
   begin
      for N in 1 .. Before'Last loop
         pragma Loop_Invariant (Sum (After, Offer, N - 1) = Sum (Before, Offer, N - 1));
      end loop;
      for N in Before'Last + 1 .. After'Last loop
         pragma Loop_Invariant (Sum (After, Offer, N - 1) = Sum (Before, Offer, Before'Last));
      end loop;
   end Lemma_Sum_Extended;

   procedure Admit
     (S : in out State; Offer : Offer_Index; Client : Client_Id; Result : out Admission)
   is
      Found : Natural := 0;
      Free  : Natural := 0;
      Old   : constant Client_Table := S.Clients with Ghost;
   begin
      for C in 1 .. S.Size loop
         if S.Clients (C).Id = Client then
            Found := C;
         elsif Free = 0 and then S.Clients (C).Id = No_Client then
            Free := C;
         end if;
         pragma Loop_Invariant (Found <= C and then Free <= C);
         pragma Loop_Invariant
           (if Found /= 0 then S.Clients (Found).Id = Client
            else (for all D in 1 .. C => S.Clients (D).Id /= Client));
         pragma Loop_Invariant (if Free /= 0 then S.Clients (Free).Id = No_Client);
      end loop;
      if S.Totals (Offer) = Maximum_Total
        or else not Within (S.Declared (Offer).Total, S.Totals (Offer) + 1)
      then
         Result := Total_Limit;
         return;
      elsif Found /= 0 and then
        (S.Clients (Found).Counts (Offer) = Maximum_Total
         or else not Within (S.Declared (Offer).Per_Client, S.Clients (Found).Counts (Offer) + 1))
      then
         Result := Per_Client_Limit;
         return;
      elsif Found = 0 and then Free = 0 then
         Result := No_Client_Room;
         return;
      elsif Found = 0 and then not Within (S.Declared (Offer).Per_Client, 1) then
         Result := Per_Client_Limit;
         return;
      end if;
      if Found = 0 then
         Found := Free;
         S.Clients (Found).Id := Client;
         for O in Offer_Index loop
            Lemma_Sum_Same (Old, S.Clients, O);
            pragma Loop_Invariant
              (for all P in Offer_Index'First .. O =>
                 Sum (S.Clients, P, S.Size) = Sum (Old, P, S.Size));
         end loop;
      end if;
      declare
         Mid : constant Client_Table := S.Clients with Ghost;
      begin
         S.Clients (Found).Counts (Offer) := S.Clients (Found).Counts (Offer) + 1;
         S.Totals (Offer) := S.Totals (Offer) + 1;
         Lemma_Sum_Step (Mid, S.Clients, Offer, Found, Up => True);
         for O in Offer_Index loop
            if O /= Offer then
               Lemma_Sum_Same (Mid, S.Clients, O);
            end if;
            pragma Loop_Invariant
              (for all P in Offer_Index'First .. O =>
                 (if P /= Offer then
                    Sum (S.Clients, P, S.Size) = Sum (Mid, P, S.Size)));
         end loop;
      end;
      Result := Admitted;
   end Admit;

   procedure Release (S : in out State; Offer : Offer_Index; Client : Client_Id) is
      Found : Natural := 0;
   begin
      for C in 1 .. S.Size loop
         if S.Clients (C).Id = Client then
            Found := C;
         end if;
         pragma Loop_Invariant (Found <= C);
         pragma Loop_Invariant
           (if Found /= 0 then S.Clients (Found).Id = Client
            else (for all D in 1 .. C => S.Clients (D).Id /= Client));
      end loop;
      if Found = 0 or else S.Clients (Found).Counts (Offer) = 0 then
         return;
      end if;
      declare
         Mid : constant Client_Table := S.Clients with Ghost;
      begin
         S.Clients (Found).Counts (Offer) := S.Clients (Found).Counts (Offer) - 1;
         Lemma_Sum_Step (Mid, S.Clients, Offer, Found, Up => False);
         S.Totals (Offer) := S.Totals (Offer) - 1;
         for O in Offer_Index loop
            if O /= Offer then
               Lemma_Sum_Same (Mid, S.Clients, O);
            end if;
            pragma Loop_Invariant
              (for all P in Offer_Index'First .. O =>
                 (if P /= Offer then
                    Sum (S.Clients, P, S.Size) = Sum (Mid, P, S.Size)));
         end loop;
      end;
      --  A client with no channel left frees its record.
      if (for all O in Offer_Index => S.Clients (Found).Counts (O) = 0) then
         declare
            Mid : constant Client_Table := S.Clients with Ghost;
         begin
            S.Clients (Found).Id := No_Client;
            for O in Offer_Index loop
               Lemma_Sum_Same (Mid, S.Clients, O);
               pragma Loop_Invariant
                 (for all P in Offer_Index'First .. O =>
                    Sum (S.Clients, P, S.Size) = Sum (Mid, P, S.Size));
            end loop;
         end;
      end if;
   end Release;

   procedure Grow (From : State; Into : out State) is
   begin
      Into.Declared := From.Declared;
      Into.Totals := From.Totals;
      Into.Clients := [others => No_Record];
      Into.Clients (1 .. From.Size) := From.Clients;
      for O in Offer_Index loop
         Lemma_Sum_Extended (From.Clients, Into.Clients, O);
         pragma Loop_Invariant
           (for all P in Offer_Index'First .. O =>
              Sum (Into.Clients, P, Into.Size) = Sum (From.Clients, P, From.Size));
      end loop;
   end Grow;

end CuBit.Channel_Admission;
