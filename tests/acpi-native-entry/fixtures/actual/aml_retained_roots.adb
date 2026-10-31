package body AML_Retained_Roots with SPARK_Mode is
   function Valid (Store : State) return Boolean is
   begin
      if Store.Data.Last > Max_Incarnation then return False; end if;
      for I in Root_Index loop
         declare E : Entry_Data renames Store.Data.Entries (I); begin
            if E.Phase /= Published and then E.Value /= Empty_Value then return False; end if;
            if E.Phase = Vacant then
               if E.Stamp /= 0 then return False; end if;
            else
               if Store.Data.Arena = AML_Identity.No_Identity or else
                 E.Stamp = 0 or else E.Stamp > Store.Data.Last then return False; end if;
               for J in Root_Index loop
                  if J /= I and then Store.Data.Entries (J).Phase /= Vacant
                    and then Store.Data.Entries (J).Stamp = E.Stamp then return False; end if;
               end loop;
            end if;
         end;
      end loop;
      return True;
   end Valid;
   function Pending_Count (Store : State) return Root_Count is
      N : Root_Count := 0;
   begin
      for E of Store.Data.Entries loop if E.Phase = Reserved then N := N+1; end if; end loop;
      return N;
   end Pending_Count;
   function Published_Count (Store : State) return Root_Count is
      N : Root_Count := 0;
   begin
      for E of Store.Data.Entries loop if E.Phase = Published then N := N+1; end if; end loop;
      return N;
   end Published_Count;
   function Bound (After, Before : Model; New_Owner : AML_Identity.Identity;
                   Clear : Boolean) return Boolean is
     (After.Arena = New_Owner and then After.Last = Before.Last and then
      (if Clear then (for all E of After.Entries => E = Entry_Data'(others => <>))
       else After.Entries = Before.Entries));
   function Changed (After, Before : Model; Root : Token;
                     Previous, Phase : Entry_Phase; Value : Value_Type;
                     Issued : Boolean) return Boolean is
     (Root /= No_Token and then Root.Slot /= 0 and then Root.Arena = Before.Arena
      and then Before.Entries (Root.Slot).Phase = Previous
      and then After.Arena = Before.Arena
      and then (if Issued then Before.Last < Max_Incarnation
        and then After.Last = Before.Last+1 and then Root.Stamp = After.Last
        and then Before.Entries (Root.Slot).Phase = Vacant
        else After.Last = Before.Last and then Before.Entries (Root.Slot).Stamp = Root.Stamp)
      and then (if Phase = Reserved then Issued
        elsif Phase = Published then Before.Entries (Root.Slot).Phase = Reserved
        else Before.Entries (Root.Slot).Phase /= Vacant)
      and then (for all I in Root_Index =>
        (if I = Root.Slot then After.Entries (I) =
          Entry_Data'(Phase, (if Phase = Vacant then 0 else Root.Stamp), Value)
         else After.Entries (I) = Before.Entries (I))));
   function Published_From_Reservation
     (After, Before : Model; Root : Token; Value : Value_Type) return Boolean is
      Expected : Model := Before;
   begin
      if Root.Slot = 0 or else Root.Arena /= Before.Arena
        or else Before.Last = Max_Incarnation
        or else Before.Entries (Root.Slot).Phase /= Vacant
        or else Root.Stamp /= Before.Last+1 then return False; end if;
      Expected.Last := Before.Last+1;
      Expected.Entries (Root.Slot) := (Published, Root.Stamp, Value);
      return After = Expected;
   end Published_From_Reservation;
   function Replaced (After, Before : Model; Root : Token; Value : Value_Type)
     return Boolean is
     (Root.Arena /= AML_Identity.No_Identity and then Root.Arena = Before.Arena
      and then Root.Slot /= 0 and then Root.Stamp /= 0
      and then Before.Entries (Root.Slot).Phase = Published
      and then Before.Entries (Root.Slot).Stamp = Root.Stamp
      and then After = (Before with delta Entries =>
        (Before.Entries with delta Root.Slot => Entry_Data'(Published, Root.Stamp, Value))));
   function Discarded_Reservation (After, Before : Model) return Boolean is
     (Before.Last < Max_Incarnation and then After = (Before with delta Last => Before.Last + 1));
   function Matches (Store : State; Root : Token) return Boolean is
     (Root.Arena /= AML_Identity.No_Identity and then Root.Arena = Store.Data.Arena
      and then Root.Slot /= 0 and then Root.Stamp /= 0
      and then Store.Data.Entries (Root.Slot).Phase /= Vacant
      and then Store.Data.Entries (Root.Slot).Stamp = Root.Stamp);
   function Census_Matches (Store : State; Index : Root_Index; Result : Read_Result)
                            return Boolean is
     (Result = (if Store.Data.Entries (Index).Phase = Published
       then Read_Result'(Ready, Store.Data.Entries (Index).Value)
       else Read_Result'(Wrong_Phase, Empty_Value)));
   function Read_Matches (Store : State; Root : Token; Result : Read_Result)
                          return Boolean is
     (if Matches (Store, Root) then Census_Matches (Store, Root.Slot, Result)
      else Result = Read_Result'(Invalid_Root, Empty_Value));
   procedure Bind (Store : in out State; Arena : AML_Identity.Identity;
                   Status : out Result_Status) is
   begin
      Status := Invalid_Owner;
      if Store.Data.Arena /= AML_Identity.No_Identity or else Arena = AML_Identity.No_Identity then return; end if;
      Store.Data.Arena := Arena; Status := Ready;
   end Bind;
   procedure Reset (Store : in out State; Arena : AML_Identity.Identity;
                    Status : out Result_Status) is
   begin
      Status := Invalid_Owner;
      if Store.Data.Arena = AML_Identity.No_Identity or else Arena = AML_Identity.No_Identity
        or else Arena = Store.Data.Arena then return; end if;
      Store.Data.Arena := Arena; Store.Data.Entries := [others => <>]; Status := Ready;
   end Reset;
   procedure Reserve (Store : in out State; Root : out Token;
                      Status : out Result_Status) is
   begin
      Root := No_Token; Status := Invalid_Owner;
      if Store.Data.Arena = AML_Identity.No_Identity then return; end if;
      for I in Root_Index loop
         if Store.Data.Entries (I).Phase = Vacant then
            Status := Identity_Exhausted;
            if Store.Data.Last = Max_Incarnation then return; end if;
            Store.Data.Last := Store.Data.Last+1;
            Store.Data.Entries (I) := (Reserved, Store.Data.Last, Empty_Value);
            Root := (Store.Data.Arena, I, Store.Data.Last); Status := Ready; return;
         end if;
      end loop;
      Status := Root_Limit;
   end Reserve;
   procedure Publish (Store : in out State; Root : Token; Value : Value_Type;
                      Status : out Result_Status) is
   begin
      Status := Invalid_Root;
      if not Matches (Store, Root) then return; end if;
      Status := Wrong_Phase;
      if Store.Data.Entries (Root.Slot).Phase /= Reserved then return; end if;
      Store.Data.Entries (Root.Slot).Value := Value;
      Store.Data.Entries (Root.Slot).Phase := Published; Status := Ready;
   end Publish;
   procedure Replace (Store : in out State; Root : Token; Value : Value_Type;
                      Status : out Result_Status) is
   begin
      Status := Invalid_Root;
      if not Matches (Store, Root) then return; end if;
      Status := Wrong_Phase;
      if Store.Data.Entries (Root.Slot).Phase /= Published then return; end if;
      Store.Data.Entries (Root.Slot).Value := Value; Status := Ready;
   end Replace;
   function Read (Store : State; Root : Token) return Read_Result is
   begin
      if not Matches (Store, Root) then return (Invalid_Root, Empty_Value); end if;
      return Published_At (Store, Root.Slot);
   end Read;
   procedure Remove (Store : in out State; Root : in out Token;
                     Expected : Entry_Phase; Status : out Result_Status) is
   begin
      Status := Invalid_Root;
      if not Matches (Store, Root) then return; end if;
      Status := Wrong_Phase;
      if Store.Data.Entries (Root.Slot).Phase /= Expected then return; end if;
      Store.Data.Entries (Root.Slot) := (others => <>); Root := No_Token; Status := Ready;
   end Remove;
   procedure Cancel (Store : in out State; Root : in out Token;
                     Status : out Result_Status) is
   begin Remove (Store, Root, Reserved, Status); end Cancel;
   procedure Release (Store : in out State; Root : in out Token;
                      Status : out Result_Status) is
   begin Remove (Store, Root, Published, Status); end Release;
   function Published_At (Store : State; Index : Root_Index) return Read_Result is
   begin
      if Store.Data.Entries (Index).Phase /= Published then return (Wrong_Phase, Empty_Value); end if;
      return (Ready, Store.Data.Entries (Index).Value);
   end Published_At;
end AML_Retained_Roots;
