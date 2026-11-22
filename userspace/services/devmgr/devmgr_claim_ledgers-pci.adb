pragma Ada_2022;
package body Devmgr_Claim_Ledgers.PCI is
   use type P.Process_ID, Q.Request_ID, Q.Request, Q.Bus_Key, Q.Access_Mode;
   use type C.Window, C.Reader_State, C.Function_Key;
   use type Reader_Store.Growth_Status, Owner_Store.Growth_Status;
   function Count (L : Ledger) return Natural is (Reader_Store.Length (L.Rows));
   function Claim_At (L : Ledger; Index : Positive) return C.Claim is
     (Reader_Store.Element_At (L.Rows,Index));
   function Reader_Claims (L : Ledger; Key : Q.Bus_Key) return Natural is
      N : Natural := 0;
   begin
      for I in 1 .. Count (L) loop
         declare R : constant C.Claim := Claim_At (L,I); begin
            if R.Request.Key = Key and then C.May_Hold (R) then N:=N+1; end if;
         end;
      end loop;
      return N;
   end Reader_Claims;
   procedure Prepare
     (L : in out Ledger; Reader : P.Process_ID; Stamp : Q.Word;
      Request : Q.Request; Backing : C.Window; Result : out Preparation;
      Report : out Capacity_Report)
   is
      Growth : Reader_Store.Capacity_Report;
   begin
      Result := (others => <>);
      Report := (Status => Port_Store.Grown, others => 0);
      if L.Closed then return;
      elsif not P.Is_Process (Reader) or else Stamp /= Q.Backend_Tag then
         Result.Why:=Q.Not_Approved;return;
      elsif not Q.Valid (Request) then Result.Why:=Q.Malformed;return;
      elsif Request.Mode /= Q.Read_Only then Result.Why:=Q.Write_Refused;return;
      end if;
      for I in 1 .. Count (L) loop
         declare Row : constant C.Claim := Claim_At (L,I); begin
            if Row.Reader = Reader and then Row.Request.ID = Request.ID then
               if Row.Request /= Request then Result.Why:=Q.Request_Changed;
               elsif Row.Backing /= Backing or else not C.Valid (Backing) then Result.Why:=Q.No_Root;
               elsif Row.State = C.Granted_Lifetime_Unconfirmed then
                  Result := (Replay_Ready,Q.Granted,I,Row.ID);
               elsif Row.State in C.Preparing | C.Reply_Uncertain then Result.Why:=Q.Delivery_Uncertain;
               else Result.Why:=Q.Unavailable;
               end if;
               return;
            end if;
         end;
      end loop;
      if not C.Valid (Backing) or else Backing.Key /= Request.Key then
         Result.Why:=Q.No_Root;return;
      elsif L.Next_ID=Q.No_Record then Result.Why:=Q.Capacity_Refused;return;
      end if;
      Reader_Store.Append (L.Rows,(Reader,Request,L.Next_ID,Backing,C.Preparing),Growth);
      Report := (Status => Port_Store.Growth_Status'Val
        (Reader_Store.Growth_Status'Pos (Growth.Status)),
        Demand => Growth.Demand, Limit => Growth.Limit, Requested => Growth.Requested);
      if Growth.Status /= Reader_Store.Grown then Result.Why:=Q.Capacity_Refused;return;end if;
      Result := (Prepared,Q.Granted,Count(L),L.Next_ID);
      L.Next_ID:=C.Successor(L.Next_ID);
   end Prepare;
   procedure Complete
     (L : in out Ledger; Index : Positive; Expected : Q.Record_ID;
      Fact : C.Delivery; Accepted : out Boolean)
   is
      Row : C.Claim;
   begin
      Accepted:=False;
      if L.Closed or else Index>Count(L) then return;end if;
      Row:=Claim_At(L,Index);
      if Expected=Q.No_Record or else Row.ID/=Expected or else Row.State/=C.Preparing then return;end if;
      Reader_Store.Set_At(L.Rows,Index,C.Complete(Row,Fact));Accepted:=True;
   end Complete;
   function Owner_Of (L : Ledger; Key : C.Function_Key) return P.Process_ID is
   begin
      for I in 1 .. Owner_Store.Length(L.Owners) loop
         declare Item : constant C.Function_Owner := Owner_Store.Element_At(L.Owners,I);begin
            if Item.Key=Key then return Item.Owner;end if;
         end;
      end loop;
      return P.No_Process;
   end Owner_Of;
   procedure Bind_Owner
     (L : in out Ledger; Key : C.Function_Key;
      Expected, Owner : P.Process_ID; Result : out Owner_Outcome;
      Report : out Capacity_Report)
   is
      Growth : Owner_Store.Capacity_Report;
   begin
      Result:=Closed;Report:=(Status=>Port_Store.Grown,others=>0);
      if L.Closed then return;end if;
      Result:=Invalid;if not P.Is_Process(Owner) then return;end if;
      for I in 1 .. Owner_Store.Length(L.Owners) loop
         declare Item : constant C.Function_Owner := Owner_Store.Element_At(L.Owners,I);begin
            if Item.Key=Key then
               Result:=Conflict;if Item.Owner/=Expected then return;end if;
               Owner_Store.Set_At(L.Owners,I,(Key,Owner));Result:=Bound;return;
            end if;
         end;
      end loop;
      Result:=Conflict;if Expected/=P.No_Process then return;end if;
      Owner_Store.Append(L.Owners,(Key,Owner),Growth);
      Report:=(Status=>Port_Store.Growth_Status'Val(Owner_Store.Growth_Status'Pos(Growth.Status)),
        Demand=>Growth.Demand,Limit=>Growth.Limit,Requested=>Growth.Requested);
      Result:=(if Growth.Status=Owner_Store.Grown then Bound else Capacity_Refused);
   end Bind_Owner;
   procedure Retire
     (L : in out Ledger; Life : P.Process_ID; Readers_Removed, Owners_Cleared : out Natural)
   is
      Kept : Natural := 0;
   begin
      Readers_Removed:=0;Owners_Cleared:=0;
      if L.Closed or else not P.Is_Process(Life) then return;end if;
      for I in 1 .. Count(L) loop
         declare Row : constant C.Claim := Claim_At(L,I);begin
            if Row.Reader=Life then Readers_Removed:=Readers_Removed+1;
            else Kept:=Kept+1;if Kept/=I then Reader_Store.Set_At(L.Rows,Kept,Row);end if;end if;
         end;
      end loop;
      Reader_Store.Set_Length(L.Rows,Kept);
      for I in 1 .. Owner_Store.Length(L.Owners) loop
         declare Item : constant C.Function_Owner := Owner_Store.Element_At(L.Owners,I);begin
            if Item.Owner=Life then
               Owner_Store.Set_At(L.Owners,I,(Item.Key,P.No_Process));Owners_Cleared:=Owners_Cleared+1;
            end if;
         end;
      end loop;
   end Retire;
   procedure Close (L : in out Ledger) is
   begin
      Reader_Store.Clear(L.Rows);Owner_Store.Clear(L.Owners);L.Closed:=True;
   end Close;
end Devmgr_Claim_Ledgers.PCI;
