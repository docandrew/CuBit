pragma Ada_2022;
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

   --  A + B, saturating: counts never wrap.
   function Plus (A, B : Unsigned_64) return Unsigned_64 is
     (if A > Unsigned_64'Last - B then Unsigned_64'Last else A + B);

   procedure Count_Loss (Lost : in out Unsigned_64; By : Unsigned_64 := 1) is
   begin
      Lost := Plus (Lost, By);
   end Count_Loss;

   ---------------------------------------------------------------------------
   --  Per-source histories
   ---------------------------------------------------------------------------

   --  Where position P (in publish order) is in H's block.
   function Block_Index (H : History; P : Natural) return Natural is
     (if P < H.Boot_Used then P
      else H.Boot_Used + (H.Ring_Head + (P - H.Boot_Used)) mod Ring_Size (H))
   with Pre  => Well_Formed (H) and then P < Held (H),
        Post => Block_Index'Result < Block_Size (H);

   function Entry_At (H : History; P : Natural) return Retained is
     (Blocks.Get (H.Store, Block_Index (H, P)))
   with Pre => Well_Formed (H) and then P < Held (H);

   function Order_At (H : History; P : Natural) return Order_Number is (Entry_At (H, P).Order)
   with Pre => Well_Formed (H) and then P < Held (H);

   --  Whether a reader filtering by Minimum is due position P: it passes,
   --  or records before it were not retained (a gap is shown whatever the
   --  severity).
   function Due_At
     (H : History; P : Natural; Minimum : CuBit.Log_Records.Severity) return Boolean is
     (CuBit.Log_Records.Level (Entry_At (H, P).Value.Data) >= Minimum
      or else Entry_At (H, P).Gap_Before /= 0)
   with Pre => Well_Formed (H) and then P < Held (H);

   function Tally_Of (H : History) return Source_Tally is
     ((Source => H.Source, Published => Unsigned_64 (H.Published), Retained => Unsigned_64 (Held (H)),
       Not_Retained => Unsigned_64 (H.Not_Retained), Over_Budget => H.Over_Budget,
       Publisher_Full => H.Publisher_Full))
   with Pre => Well_Formed (H);

   --  Records every source holds together.
   function Held_Total (Item : Broker) return Natural
   with Pre => Accounted (Item)
   is
      Sum : Natural := 0;
   begin
      for S in Source_Slot loop
         pragma Loop_Invariant (Sum <= (S - 1) * Greatest_Per_Source);
         Sum := Sum + Held (Item.Kept (S));
      end loop;
      return Sum;
   end Held_Total;

   --  The first position in H whose order is From or later (Held (H) if
   --  none): positions are in publish order.
   function First_From (H : History; From : Order_Number) return Natural
   with Pre => Well_Formed (H), Post => First_From'Result <= Held (H)
   is
      Low : Natural := 0;
      High : Natural := Held (H);
      Middle : Natural;
   begin
      while Low < High loop
         pragma Loop_Invariant (Low < High and then High <= Held (H));
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
   --  From .. Before - 1, of Source (Every_Source: any), that a reader
   --  filtering by Minimum is due (Due_At).
   procedure Earliest
     (Item : Broker; From, Before : Order_Number;
      Minimum : CuBit.Log_Records.Severity; Source : Unsigned_64;
      Found : out Boolean; Slot : out Source_Slot; Position : out Natural)
   with Pre  => Accounted (Item),
        Post => (if Found then Position < Held (Item.Kept (Slot)))
   is
      Best : Order_Number := Order_Number'Last;
      P : Natural;
   begin
      Found := False;
      Slot := Source_Slot'First;
      Position := 0;
      for S in Source_Slot loop
         pragma Loop_Invariant (if Found then Position < Held (Item.Kept (Slot)));
         if Item.Kept (S).In_Use and then
           (Source = Every_Source or else Item.Kept (S).Source = Source)
         then
            P := First_From (Item.Kept (S), From);
            while P < Held (Item.Kept (S)) loop
               pragma Loop_Invariant (if Found then Position < Held (Item.Kept (Slot)));
               pragma Loop_Variant (Increases => P);
               exit when Order_At (Item.Kept (S), P) >= Before
                 or else (Found and then Order_At (Item.Kept (S), P) >= Best);
               if Due_At (Item.Kept (S), P, Minimum) then
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

   --  Gone (of its source, with Gap_Before records already given up before
   --  it) is no longer retained, and Successor is the order of the next
   --  retained record of its source (Order_Number'Last: none yet), which
   --  now carries the gap. A reader still due Gone in its replay whose
   --  replay ends before Successor cannot be shown that gap: it is that
   --  reader's loss.
   procedure Forget (Clients : in out Subscribers; Gone : Retained; Successor : Order_Number) is
      Owed : Unsigned_64;
   begin
      for Client of Clients loop
         if Client.Owner /= No_Process and then
           Gone.Order >= Client.Replay_Next and then Gone.Order < Client.Replay_End and then
           Successor >= Client.Replay_End and then
           (Client.Source = Every_Source or else Client.Source = Gone.Value.Source)
         then
            Owed := Plus ((if Wanted (Client, Gone.Value) then 1 else 0), Gone.Gap_Before);
            if Owed /= 0 then
               Count_Loss (Client.Pending.Lost, Owed);
            end if;
         end if;
      end loop;
   end Forget;

   --  A history that gave way to a new source, for Take_Reclaimed.
   procedure Note_Reclaimed (Item : in out Broker; Gone : Source_Tally)
   with Pre => Accounted (Item), Post => Accounted (Item)
   is
   begin
      if Item.Reclaim_Waiting then
         Count_Loss (Item.Reclaims_Missed);
      end if;
      Item.Reclaimed := Gone;
      Item.Reclaim_Waiting := True;
   end Note_Reclaimed;

   --  Give up every record of the history in Slot and free it.
   procedure Clear (Item : in out Broker; Slot : Source_Slot)
   with Pre  => Accounted (Item),
        Post => Accounted (Item) and then not Item.Kept (Slot).In_Use
   is
   begin
      for P in 0 .. Held (Item.Kept (Slot)) - 1 loop
         Forget (Item.Clients, Entry_At (Item.Kept (Slot), P), Order_Number'Last);
      end loop;
      if Item.Kept (Slot).Published /= 0 then
         Note_Reclaimed (Item, Tally_Of (Item.Kept (Slot)));
      end if;
      Blocks.Release (Item.Kept (Slot).Store);
      Item.Kept (Slot) := (others => <>);
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
   procedure Claim (Item : in out Broker; Source : Unsigned_64; Slot : out Source_Slot)
   with Pre  => Accounted (Item),
        Post => Accounted (Item) and then Item.Kept (Slot).In_Use
                and then Item.Kept (Slot).Source = Source
   is
      Free : Source_Slot := Source_Slot'First;
      Have_Free : Boolean := False;
      Victim : Source_Slot := Source_Slot'First;
   begin
      for S in Source_Slot loop
         pragma Loop_Invariant (if Have_Free then not Item.Kept (Free).In_Use);
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

   --  Give up the oldest record after Slot's boot window (its ring's
   --  oldest): counted as not retained, and its gap and itself carried to
   --  the next record of the source (Pending_Gap when there is none yet).
   procedure Evict_Oldest (Item : in out Broker; Slot : Source_Slot)
   with Pre  => Accounted (Item) and then Item.Kept (Slot).Ring_Used > 0,
        Post => Accounted (Item)
                and then Held (Item.Kept (Slot)) = Held (Item.Kept'Old (Slot)) - 1
                and then Item.Kept (Slot).Published = Item.Kept'Old (Slot).Published
                and then Item.Kept (Slot).In_Use = Item.Kept'Old (Slot).In_Use
                and then Item.Kept (Slot).Source = Item.Kept'Old (Slot).Source
                and then Block_Size (Item.Kept (Slot)) = Block_Size (Item.Kept'Old (Slot))
                and then (for all S in Source_Slot =>
                            (if S /= Slot then Item.Kept (S).Published = Item.Kept'Old (S).Published))
   is
      H : History := Item.Kept (Slot);
      Gone : constant Retained := Entry_At (H, H.Boot_Used);
      Carry : constant Unsigned_64 := Plus (Gone.Gap_Before, 1);
      Next : Retained;
      Successor : Order_Number := Order_Number'Last;
      Kept_Before : constant Record_Total := Record_Total (Held (H));
   begin
      pragma Assert (H.Published - H.Not_Retained = Kept_Before and then Kept_Before >= 1);
      pragma Assert (H.Not_Retained < H.Published);
      H.Ring_Used := H.Ring_Used - 1;
      H.Ring_Head := (if H.Ring_Used = 0 then 0 else (H.Ring_Head + 1) mod Ring_Size (H));
      pragma Assert (Record_Total (Held (H)) = Kept_Before - 1);
      H.Not_Retained := H.Not_Retained + 1;
      pragma Assert (H.Published - H.Not_Retained = Kept_Before - 1);
      if H.Ring_Used = 0 then
         H.Pending_Gap := Plus (H.Pending_Gap, Carry);
      else
         Next := Entry_At (H, H.Boot_Used);
         Next.Gap_Before := Plus (Next.Gap_Before, Carry);
         Successor := Next.Order;
         Blocks.Set (H.Store, Block_Index (H, H.Boot_Used), Next);
      end if;
      pragma Assert (Valid (H));
      Item.Kept (Slot) := H;
      Forget (Item.Clients, Gone, Successor);
   end Evict_Oldest;

   --  A record of Slot's source that cannot be retained at all: counted,
   --  and its gap is the next retained record's.
   procedure Give_Up (Item : in out Broker; Slot : Source_Slot)
   with Pre  => Accounted (Item) and then Item.Kept (Slot).Published < Record_Total'Last,
        Post => Accounted (Item)
   is
      H : History := Item.Kept (Slot);
   begin
      H.Published := H.Published + 1;
      H.Not_Retained := H.Not_Retained + 1;
      H.Pending_Gap := Plus (H.Pending_Gap, 1);
      pragma Assert (Valid (H));
      Item.Kept (Slot) := H;
   end Give_Up;

   --  A larger block for Slot's history (doubling, at least Initial_History,
   --  at most the source's ceiling), its records copied over in order.
   --  Grown False when it is at its ceiling or the heap refuses.
   procedure Grow (Item : in out Broker; Slot : Source_Slot; Grown : out Boolean)
   with Pre  => Accounted (Item),
        Post => Accounted (Item)
                and then Held (Item.Kept (Slot)) = Held (Item.Kept'Old (Slot))
                and then Item.Kept (Slot).Published = Item.Kept'Old (Slot).Published
                and then Item.Kept (Slot).Ring_Used = Item.Kept'Old (Slot).Ring_Used
                and then Item.Kept (Slot).Boot_Used = Item.Kept'Old (Slot).Boot_Used
                and then (if Grown then Held (Item.Kept (Slot)) < Block_Size (Item.Kept (Slot)))
   is
      H : History := Item.Kept (Slot);
      Current : constant Natural := Block_Size (H);
      Ceiling : constant Per_Source_Ceiling := Item.Limits.Per_Source;
      Wanted_Size : Positive;
      Larger : Blocks.Block;
   begin
      Grown := False;
      if Current >= Ceiling then
         return;
      end if;
      Wanted_Size :=
        (if Current < Initial_History then Initial_History
         elsif Current > Ceiling / 2 then Ceiling
         else 2 * Current);
      Blocks.Allocate (Larger, Wanted_Size);
      if Blocks.Capacity (Larger) = 0 then
         return;
      end if;
      pragma Assert (Held (H) <= Current and then Current < Wanted_Size and then Wanted_Size <= Greatest_Per_Source);
      for P in 0 .. Held (H) - 1 loop
         pragma Loop_Invariant (Blocks.Capacity (Larger) = Wanted_Size);
         Blocks.Set (Larger, P, Entry_At (H, P));
      end loop;
      pragma Warnings
        (GNATprove, Off, "statement has no effect",
         Reason => "Release gives the old block back to the heap, outside SPARK's model");
      pragma Warnings
        (GNATprove, Off, "* is set by * but not used after the call",
         Reason => "Release gives the old block back to the heap, outside SPARK's model");
      Blocks.Release (H.Store);
      pragma Warnings (GNATprove, On, "statement has no effect");
      pragma Warnings (GNATprove, On, "* is set by * but not used after the call");
      H.Store := Larger;
      H.Ring_Head := 0;
      pragma Assert (Block_Size (H) = Wanted_Size);
      pragma Assert (Valid (H));
      Item.Kept (Slot) := H;
      Grown := True;
   end Grow;

   --  The slot whose oldest record after its boot window gives way for the
   --  total ceiling: the largest history that has one. Found False: every
   --  history is its boot window.
   procedure Total_Victim (Item : Broker; Victim : out Source_Slot; Found : out Boolean)
   with Pre  => Accounted (Item),
        Post => (if Found then Item.Kept (Victim).Ring_Used > 0)
   is
   begin
      Victim := Source_Slot'First;
      Found := False;
      for S in Source_Slot loop
         pragma Loop_Invariant (if Found then Item.Kept (Victim).Ring_Used > 0);
         if Item.Kept (S).In_Use and then Item.Kept (S).Ring_Used > 0
           and then (not Found or else Held (Item.Kept (S)) > Held (Item.Kept (Victim)))
         then
            Victim := S;
            Found := True;
         end if;
      end loop;
   end Total_Victim;

   --  Append Value (published as Order) to the history in Slot, within the
   --  source's ceiling and the total: into the boot window while it has
   --  room and the ring is empty, else into the ring.
   procedure Keep (Item : in out Broker; Slot : Source_Slot; Value : Event; Order : Order_Number)
   with Pre  => Accounted (Item) and then Item.Kept (Slot).In_Use,
        Post => Accounted (Item)
   is
      Victim : Source_Slot;
      Found, Grown : Boolean;
      Item_Entry : Retained;
      At_Index : Natural;
   begin
      if Item.Kept (Slot).Published = Record_Total'Last then
         --  2**63 records from one source: past what the counts can say.
         return;
      end if;
      declare
         H : History := Item.Kept (Slot);
      begin
         H.Latest := Order;
         H.Ended := False;
         pragma Assert (Valid (H));
         Item.Kept (Slot) := H;
      end;
      --  The source's own ceiling: its oldest after the boot window goes.
      if Held (Item.Kept (Slot)) >= Item.Limits.Per_Source then
         if Item.Kept (Slot).Ring_Used = 0 then
            Give_Up (Item, Slot);
            return;
         end if;
         Evict_Oldest (Item, Slot);
      end if;
      --  The total: the largest history's oldest after its boot window.
      if Held_Total (Item) >= Item.Limits.Total then
         Total_Victim (Item, Victim, Found);
         if not Found then
            Give_Up (Item, Slot);
            return;
         end if;
         Evict_Oldest (Item, Victim);
      end if;
      --  Room in the block, growing it, else the source's oldest goes.
      if Held (Item.Kept (Slot)) >= Block_Size (Item.Kept (Slot)) then
         Grow (Item, Slot, Grown);
         if not Grown then
            if Item.Kept (Slot).Ring_Used = 0 then
               Give_Up (Item, Slot);
               return;
            end if;
            Evict_Oldest (Item, Slot);
         end if;
      end if;
      pragma Assert (Held (Item.Kept (Slot)) < Block_Size (Item.Kept (Slot)));
      declare
         H : History := Item.Kept (Slot);
         Kept_Before : constant Record_Total := Record_Total (Held (H));
      begin
         pragma Assert (H.Published - H.Not_Retained = Kept_Before);
         Item_Entry := (Order => Order, Value => Value, Gap_Before => H.Pending_Gap);
         H.Pending_Gap := 0;
         if H.Ring_Used = 0 and then H.Boot_Used < Item.Limits.Boot then
            Blocks.Set (H.Store, H.Boot_Used, Item_Entry);
            H.Boot_Used := H.Boot_Used + 1;
         else
            At_Index := H.Boot_Used + (H.Ring_Head + H.Ring_Used) mod Ring_Size (H);
            Blocks.Set (H.Store, At_Index, Item_Entry);
            H.Ring_Used := H.Ring_Used + 1;
         end if;
         pragma Assert (Record_Total (Held (H)) = Kept_Before + 1);
         H.Published := H.Published + 1;
         pragma Assert (H.Published - H.Not_Retained = Kept_Before + 1);
         pragma Assert (Valid (H));
         Item.Kept (Slot) := H;
      end;
   end Keep;

   ---------------------------------------------------------------------------

   procedure Configure (Item : in out Broker; Limits : Ceilings) is
   begin
      Item.Limits := Limits;
   end Configure;

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
      Keep (Item, Slot, Value, Order);
      for Client of Item.Clients loop
         if Client.Owner /= No_Process and then Wanted (Client, Value) then
            Append (Client.Pending, Value);
         end if;
      end loop;
   end Publish;

   procedure Count_Shed
     (Item : in out Broker; Source : Unsigned_64; Over_Budget, Publisher_Full : Unsigned_64)
   is
      Slot : Source_Slot;
   begin
      if Over_Budget = 0 and then Publisher_Full = 0 then
         return;
      end if;
      Claim (Item, Source, Slot);
      Item.Kept (Slot).Over_Budget := Plus (Item.Kept (Slot).Over_Budget, Over_Budget);
      Item.Kept (Slot).Publisher_Full := Plus (Item.Kept (Slot).Publisher_Full, Publisher_Full);
   end Count_Shed;

   procedure Retire (Item : in out Broker; Source : Unsigned_64) is
   begin
      for S in Source_Slot loop
         pragma Loop_Invariant (Accounted (Item));
         if Item.Kept (S).In_Use and then Item.Kept (S).Source = Source then
            declare
               H : History := Item.Kept (S);
            begin
               H.Ended := True;
               pragma Assert (Valid (H));
               Item.Kept (S) := H;
            end;
         end if;
      end loop;
   end Retire;

   function Holds_History (Item : Broker; Source : Unsigned_64) return Boolean is
     (for some S in Source_Slot =>
        Item.Kept (S).In_Use and then Item.Kept (S).Source = Source
        and then Held (Item.Kept (S)) > 0);

   function Tally (Item : Broker; Source : Unsigned_64) return Source_Tally is
   begin
      for S in Source_Slot loop
         if Item.Kept (S).In_Use and then Item.Kept (S).Source = Source then
            return Tally_Of (Item.Kept (S));
         end if;
      end loop;
      return (Source => Source, others => 0);
   end Tally;

   procedure Take_Reclaimed
     (Item : in out Broker; Gone : out Source_Tally; Found : out Boolean; Missed : out Unsigned_64) is
   begin
      Found := Item.Reclaim_Waiting;
      Gone := (if Found then Item.Reclaimed else (others => <>));
      Missed := Item.Reclaims_Missed;
      Item.Reclaim_Waiting := False;
      Item.Reclaims_Missed := 0;
   end Take_Reclaimed;

   procedure Subscribe
     (Item : in out Broker; Caller : Process_ID; Authority_Tag : Unsigned_64;
      Handle : out Unsigned_64; Result : out Status;
      Minimum : CuBit.Log_Records.Severity := CuBit.Log_Records.Trace;
      Source : Unsigned_64 := Every_Source;
      Report : Replay_Report := Records_Only) is
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
            --  What the histories gave up before this subscription is shown
            --  where it was (each record's gap), not counted as this
            --  reader's loss. Read_Next replays it before anything
            --  published later.
            Client.Pending.Lost := 0;
            Client.Replay_Next := First_Order;
            Client.Replay_End := Item.Next_Order;
            Client.Tally_Next := (if Report = With_Tallies then Source_Slot'First else No_Tally);
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
      Value : out Event; Missing : out Omission; Tally : out Source_Tally;
      Result : out Status; Renew : Boolean := True) is
      C : Natural;
      Found : Boolean;
      Slot : Source_Slot;
      Position : Natural;
      Taken : Retained;
   begin
      Value := (others => <>); Missing := No_Omission; Tally := (others => <>); Result := Denied;
      if Caller = No_Process or else not Is_Observer (Authority_Tag) then return; end if;
      C := Find (Item, Caller, Authority_Tag, Handle);
      if C = 0 then return; end if;
      if Renew then
         Item.Clients (C).Last_Use := Item.Now_Ms;
      end if;
      if Item.Clients (C).Pending.Lost /= 0 then
         Missing.Lost := Item.Clients (C).Pending.Lost;
         Item.Clients (C).Pending.Lost := 0;
         Result := Gap;
         return;
      end if;
      if Item.Clients (C).Replay_Next < Item.Clients (C).Replay_End then
         Earliest (Item, Item.Clients (C).Replay_Next, Item.Clients (C).Replay_End,
                   Item.Clients (C).Minimum, Item.Clients (C).Source, Found, Slot, Position);
         if Found then
            Taken := Entry_At (Item.Kept (Slot), Position);
            Item.Clients (C).Replay_Next :=
              (if Taken.Order < Order_Number'Last then Taken.Order + 1 else Taken.Order);
            Missing := (Source => Item.Kept (Slot).Source, Not_Retained => Taken.Gap_Before, Lost => 0);
            if CuBit.Log_Records.Level (Taken.Value.Data) >= Item.Clients (C).Minimum then
               Value := Taken.Value;
               Result := OK;
            else
               --  Only the gap before a record this reader leaves out.
               Result := Gap;
            end if;
            return;
         end if;
         Item.Clients (C).Replay_Next := Item.Clients (C).Replay_End;
      end if;
      --  The replay's tallies: each source it covered, once.
      if Item.Clients (C).Tally_Next /= No_Tally then
         for S in Item.Clients (C).Tally_Next .. Source_Slot'Last loop
            if Item.Kept (S).In_Use and then
              (Item.Clients (C).Source = Every_Source or else Item.Kept (S).Source = Item.Clients (C).Source)
            then
               Tally := Tally_Of (Item.Kept (S));
               --  A source has one history: a replay of one source is done.
               Item.Clients (C).Tally_Next :=
                 (if S = Source_Slot'Last or else Item.Clients (C).Source /= Every_Source then No_Tally
                  else S + 1);
               Result := Summary;
               return;
            end if;
         end loop;
         Item.Clients (C).Tally_Next := No_Tally;
         if Item.Clients (C).Source /= Every_Source then
            --  A source with no history still ends its replay with a tally.
            Tally := (Source => Item.Clients (C).Source, others => 0);
            Result := Summary;
            return;
         end if;
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
      Position : Natural;
      Taken : Order_Number;
   begin
      --  The earliest retained record from Persist_Next on: every order
      --  number before it was given up, so their count is the loss, in place.
      Earliest (Item, Item.Persist_Next, Item.Next_Order, CuBit.Log_Records.Trace, Every_Source,
                Found, Slot, Position);
      if Found then
         Value := Entry_At (Item.Kept (Slot), Position).Value;
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
