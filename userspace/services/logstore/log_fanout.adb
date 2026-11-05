with CuBit.Log_Protocol; use CuBit.Log_Protocol;
package body Log_Fanout with SPARK_Mode is
   use type CuBit.Log_Records.Severity;

   function Passes
     (Value : Event; Minimum : CuBit.Log_Records.Severity; Source : Unsigned_64)
      return Boolean is
     (CuBit.Log_Records.Level (Value.Data) >= Minimum and then
      (Source = Every_Source or else Value.Source = Source));

   function Wanted (Client : Subscriber; Value : Event) return Boolean is
     (Passes (Value, Client.Minimum, Client.Source));

   procedure Count_Loss (Lost : in out Unsigned_64) is
   begin
      if Lost < Unsigned_64'Last then
         Lost := Lost + 1;
      end if;
   end Count_Loss;

   ---------------------------------------------------------------------------
   --  Per-source histories
   ---------------------------------------------------------------------------

   function Total (H : History) return History_Count is (H.Boot_Used + H.Newest_Used);

   --  Where position P (at or after the boot window) is in the newest ring.
   function Ring_Slot (H : History; P : History_Position) return Newest_Index is
     ((H.Newest_Head + (P - H.Boot_Used)) mod Newest_Records)
     with Pre => P >= H.Boot_Used and then P < Total (H);

   function Order_At (H : History; P : History_Position) return Order_Number is
     (if P < H.Boot_Used then H.Boot (P).Order else H.Newest (Ring_Slot (H, P)).Order)
     with Pre => P < Total (H);

   function Value_At (H : History; P : History_Position) return Event is
     (if P < H.Boot_Used then H.Boot (P).Value else H.Newest (Ring_Slot (H, P)).Value)
     with Pre => P < Total (H);

   function Passes_At
     (H : History; P : History_Position; Minimum : CuBit.Log_Records.Severity)
      return Boolean is
     (CuBit.Log_Records.Level
        ((if P < H.Boot_Used then H.Boot (P).Value.Data
          else H.Newest (Ring_Slot (H, P)).Value.Data)) >= Minimum)
     with Pre => P < Total (H);

   --  The first position in H whose order is From or later (Total (H) if
   --  none): positions are in publish order.
   function First_From (H : History; From : Order_Number) return History_Count is
      Low : History_Count := 0;
      High : History_Count := Total (H);
      Middle : History_Position;
   begin
      while Low < High loop
         pragma Loop_Invariant (Low < High and then High <= Total (H));
         pragma Loop_Variant (Decreases => High - Low);
         Middle := Low + (High - Low) / 2;
         if Order_At (H, Middle) < From then
            Low := Middle + 1;
         else
            High := Middle;
         end if;
      end loop;
      return Low;
   end First_From;

   --  The earliest retained record, over every source, whose order is in
   --  From .. Before - 1 and that passes Minimum and Source.
   procedure Earliest
     (Item : Broker; From, Before : Order_Number;
      Minimum : CuBit.Log_Records.Severity; Source : Unsigned_64;
      Found : out Boolean; Slot : out Source_Slot; Position : out History_Position)
     with Post => (if Found then Position < Total (Item.Kept (Slot)))
   is
      Best : Order_Number := Order_Number'Last;
      P : History_Count;
   begin
      Found := False;
      Slot := Source_Slot'First;
      Position := 0;
      for S in Source_Slot loop
         pragma Loop_Invariant (if Found then Position < Total (Item.Kept (Slot)));
         if Item.Kept (S).In_Use and then
           (Source = Every_Source or else Item.Kept (S).Source = Source)
         then
            P := First_From (Item.Kept (S), From);
            while P < Total (Item.Kept (S)) loop
               pragma Loop_Invariant (if Found then Position < Total (Item.Kept (Slot)));
               exit when Order_At (Item.Kept (S), P) >= Before
                 or else (Found and then Order_At (Item.Kept (S), P) >= Best);
               if Passes_At (Item.Kept (S), P, Minimum) then
                  Found := True;
                  Slot := S;
                  Position := P;
                  Best := Order_At (Item.Kept (S), P);
                  exit;
               end if;
               P := P + 1;
            end loop;
         end if;
      end loop;
   end Earliest;

   --  Gone is no longer retained: whoever was still due it lost it.
   procedure Forget (Item : in out Broker; Gone : Retained) is
   begin
      for Client of Item.Clients loop
         if Client.Owner /= No_Process and then
           Gone.Order >= Client.Replay_Next and then Gone.Order < Client.Replay_End and then
           Wanted (Client, Gone.Value)
         then
            Count_Loss (Client.Pending.Lost);
         end if;
      end loop;
   end Forget;

   --  Give up every record of the history in Slot and free it.
   procedure Clear (Item : in out Broker; Slot : Source_Slot) is
      Gone : Retained;
   begin
      for P in History_Position loop
         exit when P >= Total (Item.Kept (Slot));
         Gone := (Order => Order_At (Item.Kept (Slot), P),
                  Value => Value_At (Item.Kept (Slot), P));
         Forget (Item, Gone);
      end loop;
      Item.Kept (Slot).In_Use := False;
      Item.Kept (Slot).Ended := False;
      Item.Kept (Slot).Latest := No_Order;
      Item.Kept (Slot).Boot_Used := 0;
      Item.Kept (Slot).Newest_Head := 0;
      Item.Kept (Slot).Newest_Used := 0;
   end Clear;

   --  Which slots give way first to a new source: ended, then already
   --  persisted, then the least recently published.
   subtype Reclaim_Rank is Natural range 0 .. 3;
   Ended_Rank_Weight : constant := 0;
   Live_Rank_Weight : constant := 2;
   Persisted_Rank_Weight : constant := 0;
   Unpersisted_Rank_Weight : constant := 1;
   function Rank (Item : Broker; Slot : Source_Slot) return Reclaim_Rank is
     ((if Item.Kept (Slot).Ended then Ended_Rank_Weight else Live_Rank_Weight) +
      (if Item.Kept (Slot).Latest < Item.Persist_Next then Persisted_Rank_Weight
       else Unpersisted_Rank_Weight));

   --  The slot holding Source's history, claimed (and reclaimed) if needed.
   procedure Claim (Item : in out Broker; Source : Unsigned_64; Slot : out Source_Slot) is
      Free : Source_Slot := Source_Slot'First;
      Have_Free : Boolean := False;
      Victim : Source_Slot := Source_Slot'First;
   begin
      for S in Source_Slot loop
         if Item.Kept (S).In_Use and then Item.Kept (S).Source = Source then
            Slot := S;
            return;
         elsif not Have_Free and then not Item.Kept (S).In_Use then
            Free := S;
            Have_Free := True;
         end if;
      end loop;
      if not Have_Free then
         for S in Source_Slot loop
            if Rank (Item, S) < Rank (Item, Victim) or else
              (Rank (Item, S) = Rank (Item, Victim) and then
               Item.Kept (S).Latest < Item.Kept (Victim).Latest)
            then
               Victim := S;
            end if;
         end loop;
         Clear (Item, Victim);
         Free := Victim;
      end if;
      Slot := Free;
      Item.Kept (Slot).In_Use := True;
      Item.Kept (Slot).Source := Source;
   end Claim;

   --  Append Value to the history in Slot: into the boot window while it has
   --  room, else into the newest ring, giving up its oldest when full.
   procedure Keep (Item : in out Broker; Slot : Source_Slot; Value : Retained) is
      Gone : Retained;
   begin
      Item.Kept (Slot).Latest := Value.Order;
      Item.Kept (Slot).Ended := False;
      if Item.Kept (Slot).Boot_Used < Boot_Records then
         Item.Kept (Slot).Boot (Item.Kept (Slot).Boot_Used) := Value;
         Item.Kept (Slot).Boot_Used := Item.Kept (Slot).Boot_Used + 1;
      elsif Item.Kept (Slot).Newest_Used < Newest_Records then
         Item.Kept (Slot).Newest
           ((Item.Kept (Slot).Newest_Head + Item.Kept (Slot).Newest_Used) mod Newest_Records) :=
           Value;
         Item.Kept (Slot).Newest_Used := Item.Kept (Slot).Newest_Used + 1;
      else
         Gone := Item.Kept (Slot).Newest (Item.Kept (Slot).Newest_Head);
         Forget (Item, Gone);
         Item.Kept (Slot).Newest (Item.Kept (Slot).Newest_Head) := Value;
         Item.Kept (Slot).Newest_Head := (Item.Kept (Slot).Newest_Head + 1) mod Newest_Records;
      end if;
   end Keep;

   ---------------------------------------------------------------------------

   procedure Advance_Time (Item : in out Broker; Now_Ms : Unsigned_64) is
   begin
      Item.Now_Ms := Unsigned_64'Max (Item.Now_Ms, Now_Ms);
      for Client of Item.Clients loop
         if Client.Owner /= No_Process and then
           Item.Now_Ms >= Client.Last_Use and then
           Item.Now_Ms - Client.Last_Use >= Subscription_Lease_Ms
         then
            Client := (others => <>);
         end if;
      end loop;
   end Advance_Time;

   procedure Append (Item : in out Queue; Value : Event) is
   begin
      if Item.Used = Capacity then
         Item.Head := (Item.Head + 1) mod Capacity;
         Count_Loss (Item.Lost);
      else
         Item.Used := Item.Used + 1;
      end if;
      Item.Data ((Item.Head + Item.Used - 1) mod Capacity) := Value;
   end Append;

   procedure Publish (Item : in out Broker; Value : Event) is
      Order : constant Order_Number := Item.Next_Order;
      Slot : Source_Slot;
   begin
      if Item.Next_Order < Order_Number'Last then
         Item.Next_Order := Item.Next_Order + 1;
      end if;
      Claim (Item, Value.Source, Slot);
      Keep (Item, Slot, (Order => Order, Value => Value));
      for Client of Item.Clients loop
         if Client.Owner /= No_Process and then Wanted (Client, Value) then
            Append (Client.Pending, Value);
         end if;
      end loop;
   end Publish;

   procedure Retire (Item : in out Broker; Source : Unsigned_64) is
   begin
      for H of Item.Kept loop
         if H.In_Use and then H.Source = Source then
            H.Ended := True;
         end if;
      end loop;
   end Retire;

   function Holds_History (Item : Broker; Source : Unsigned_64) return Boolean is
     (for some H of Item.Kept =>
        H.In_Use and then H.Source = Source and then (H.Boot_Used > 0 or else H.Newest_Used > 0));

   procedure Subscribe
     (Item : in out Broker; Caller : Process_ID; Authority_Tag : Unsigned_64;
      Handle : out Unsigned_64; Result : out Status;
      Minimum : CuBit.Log_Records.Severity := CuBit.Log_Records.Trace;
      Source : Unsigned_64 := Every_Source) is
   begin
      Handle := 0;
      Result := Denied;
      if Caller = No_Process or else not May_Invoke (Authority_Tag, Subscribe) then return; end if;
      Result := Exhausted;
      --  One subscription per process/issued authority; retries do not reset
      --  its cursor or occupy more slots.
      for Client of Item.Clients loop
         if Client.Owner = Caller and then Client.Authority_Tag = Authority_Tag then
            Handle := Client.Handle;
            Client.Last_Use := Item.Now_Ms;
            Client.Minimum := Minimum;
            Client.Source := Source;
            Result := OK;
            return;
         end if;
      end loop;
      if Item.Next_Handle = Unsigned_64'Last then return; end if;
      for Client of Item.Clients loop
         if Client.Owner = No_Process then
            Handle := Item.Next_Handle;
            Item.Next_Handle := Item.Next_Handle + 1;
            Client.Owner := Caller;
            Client.Authority_Tag := Authority_Tag;
            Client.Last_Use := Item.Now_Ms;
            Client.Handle := Handle;
            Client.Minimum := Minimum;
            Client.Source := Source;
            Client.Pending.Head := 0;
            Client.Pending.Used := 0;
            --  Retained history is complete as retained: what the per-source
            --  windows gave up before this subscription is not this reader's
            --  loss. Read_Next replays it before anything published later.
            Client.Pending.Lost := 0;
            Client.Replay_Next := First_Order;
            Client.Replay_End := Item.Next_Order;
            Result := OK;
            return;
         end if;
      end loop;
   end Subscribe;

   --  The live subscription Handle of Caller under Authority_Tag, or 0.
   function Find
     (Item : Broker; Caller : Process_ID; Authority_Tag, Handle : Unsigned_64) return Natural
     with Post => Find'Result in 0 | Subscriber_Index
   is
   begin
      for C in Subscriber_Index loop
         if Item.Clients (C).Owner = Caller and then Item.Clients (C).Authority_Tag = Authority_Tag
           and then Item.Clients (C).Handle = Handle
         then
            return C;
         end if;
      end loop;
      return 0;
   end Find;

   procedure Read_Next
     (Item : in out Broker; Caller : Process_ID; Authority_Tag, Handle : Unsigned_64;
      Value : out Event; Lost : out Unsigned_64; Result : out Status; Renew : Boolean := True) is
      C : Natural;
      Found : Boolean;
      Slot : Source_Slot;
      Position : History_Position;
      Taken : Order_Number;
   begin
      Value := (others => <>); Lost := 0; Result := Denied;
      if Caller = No_Process or else not Is_Observer (Authority_Tag) then return; end if;
      C := Find (Item, Caller, Authority_Tag, Handle);
      if C = 0 then return; end if;
      if Renew then
         Item.Clients (C).Last_Use := Item.Now_Ms;
      end if;
      if Item.Clients (C).Pending.Lost /= 0 then
         Lost := Item.Clients (C).Pending.Lost;
         Item.Clients (C).Pending.Lost := 0;
         Result := Gap;
         return;
      end if;
      if Item.Clients (C).Replay_Next < Item.Clients (C).Replay_End then
         Earliest (Item, Item.Clients (C).Replay_Next, Item.Clients (C).Replay_End,
                   Item.Clients (C).Minimum, Item.Clients (C).Source, Found, Slot, Position);
         if Found then
            Value := Value_At (Item.Kept (Slot), Position);
            Taken := Order_At (Item.Kept (Slot), Position);
            Item.Clients (C).Replay_Next :=
              (if Taken < Order_Number'Last then Taken + 1 else Taken);
            Result := OK;
            return;
         end if;
         Item.Clients (C).Replay_Next := Item.Clients (C).Replay_End;
      end if;
      if Item.Clients (C).Pending.Used = 0 then
         Result := Empty;
      else
         Value := Item.Clients (C).Pending.Data (Item.Clients (C).Pending.Head);
         Item.Clients (C).Pending.Head := (Item.Clients (C).Pending.Head + 1) mod Capacity;
         Item.Clients (C).Pending.Used := Item.Clients (C).Pending.Used - 1;
         Result := OK;
      end if;
   end Read_Next;

   function Active (Item : Broker; Handle : Unsigned_64) return Boolean is
     (Handle /= 0 and then (for some Client of Item.Clients => Client.Owner /= No_Process and then Client.Handle = Handle));

   procedure Close
     (Item : in out Broker; Caller : Process_ID; Authority_Tag, Handle : Unsigned_64;
      Result : out Status) is
   begin
      Result := Denied;
      if Caller = No_Process or else not May_Invoke (Authority_Tag, Close) then return; end if;
      for Client of Item.Clients loop
         if Client.Owner = Caller and then Client.Authority_Tag = Authority_Tag
           and then Client.Handle = Handle then
            Client := (others => <>); Result := OK; return;
         end if;
      end loop;
   end Close;

   procedure Take_Persistent
     (Item : in out Broker; Value : out Event; Found : out Boolean; Lost : out Unsigned_64) is
      Slot : Source_Slot;
      Position : History_Position;
      Taken : Order_Number;
   begin
      --  The earliest retained record from Persist_Next on: every order
      --  number before it was given up, so their count is the loss, in place.
      Earliest (Item, Item.Persist_Next, Item.Next_Order, CuBit.Log_Records.Trace, Every_Source,
                Found, Slot, Position);
      if Found then
         Value := Value_At (Item.Kept (Slot), Position);
         Taken := Order_At (Item.Kept (Slot), Position);
      else
         Value := (others => <>);
         Taken := Item.Next_Order;
      end if;
      Lost := (if Taken > Item.Persist_Next then Taken - Item.Persist_Next else 0);
      Item.Persist_Next :=
        (if Found and then Taken < Order_Number'Last then Taken + 1 else Taken);
   end Take_Persistent;

end Log_Fanout;
