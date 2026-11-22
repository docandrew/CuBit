pragma Ada_2022;
with CuBit.Channel_Contracts;
with CuBit.Channel_Protocol;
with CuBit.Metric_Updates;
with CuBit.Process_IDs;
with Heap_Growth;

package body Metric_Subscriptions is
   package Channels renames CuBit.Channels;
   package Updates renames CuBit.Metric_Updates;
   use type CuBit.Messages.Process_ID;
   use type CuBit.Channel_Contracts.Contract;
   use type Channels.Side;
   use type Channels.Put_Result;
   use type Metric_Series.Name_Match;

   Initial_Subscriptions : constant := 4;
   Initial_Decisions : constant := 64;

   procedure Grow_Items is new Heap_Growth (Subscription_Blocks);
   procedure Grow_Decisions is new Heap_Growth (Decision_Blocks);

   function Add (A, B : Unsigned_64) return Unsigned_64 is
     (if A > Unsigned_64'Last - B then Unsigned_64'Last else A + B);

   procedure Finish (T : in out Table; S : in out Subscription);
   procedure Finish (T : in out Table; S : in out Subscription) is
   begin
      if S.Link.Active then
         Channels.Close (S.Link);
      end if;
      if S.Subscribed and then T.Live > 0 then
         T.Live := T.Live - 1;
      end if;
      Decision_Blocks.Release (S.Decisions);
      S := (others => <>);
   end Finish;

   procedure Open
     (T : in out Table; From : CuBit.Messages.Process_ID; Authority : Unsigned_64;
      Request : CuBit.Messages.Message; Reply : out CuBit.Messages.Message)
   is
      Chosen : Natural := 0;
      Is_Open, Valid, Grown : Boolean;
      Offered : CuBit.Channel_Contracts.Contract;
      Opener_Side : Channels.Side;
      Ignore_Connector : Unsigned_16;
      S : Subscription;
      pragma Unreferenced (Authority);
   begin
      Channels.Decode_Open (Request, Is_Open, Valid, Offered, Opener_Side, Ignore_Connector);
      if not Valid or else Offered /= Updates.CONTRACT or else Opener_Side /= Channels.Consuming then
         Reply := Channels.Refusal_Reply (CuBit.Channel_Protocol.Unknown_Type);
         return;
      end if;
      for I in 0 .. T.Used - 1 loop
         if Chosen = 0 and then not Subscription_Blocks.Get (T.Items, I).Link.Active then
            Chosen := I + 1;
         end if;
      end loop;
      if Chosen = 0 then
         Grow_Items (T.Items, T.Used, Initial_Subscriptions, Maximum_Subscriptions, T.Used + 1, Grown);
         if not Grown then
            Reply := Channels.Refusal_Reply (CuBit.Channel_Protocol.No_Room);
            return;
         end if;
         T.Used := T.Used + 1;
         Chosen := T.Used;
         Subscription_Blocks.Set (T.Items, Chosen - 1, (others => <>));
      end if;
      Channels.Accept_Open (From, Request, Number_Base + Unsigned_64 (Chosen), S.Link, Reply);
      if S.Link.Active then
         S.Owner := From;
         S.Reader := CuBit.Process_IDs.To_Word (From);
         T.Sums.Subscriptions := Add (T.Sums.Subscriptions, 1);
      end if;
      Subscription_Blocks.Set (T.Items, Chosen - 1, S);
   end Open;

   function Index_Of (T : Table; Number : Unsigned_64) return Natural is
     (if Number > Number_Base and then Number - Number_Base <= Unsigned_64 (T.Used)
      then Natural (Number - Number_Base) else 0);

   procedure Close (T : in out Table; From : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message) is
      I : constant Natural := Index_Of (T, Channels.Number_Of (Request));
      S : Subscription;
   begin
      if I = 0 then
         return;
      end if;
      S := Subscription_Blocks.Get (T.Items, I - 1);
      if S.Owner = From and then Channels.Closed_By (S.Link, From, Request) then
         Finish (T, S);
         Subscription_Blocks.Set (T.Items, I - 1, S);
      end if;
   end Close;

   procedure Ended (T : in out Table; Event : CuBit.Control_Events.Event) is
      S : Subscription;
   begin
      for I in 0 .. T.Used - 1 loop
         S := Subscription_Blocks.Get (T.Items, I);
         if S.Link.Active and then Channels.Ended (S.Link, Event) then
            Finish (T, S);
            Subscription_Blocks.Set (T.Items, I, S);
         end if;
      end loop;
   end Ended;

   procedure Subscribe
     (T : in out Table; Scopes : Metric_Scopes.Table; From : CuBit.Messages.Process_ID; Number : Unsigned_64;
      Sel : Metric_Series.Selector; Result : out CuBit.Metric_Protocol.Status)
   is
      I : constant Natural := Index_Of (T, Number);
      S : Subscription;
   begin
      Result := CuBit.Metric_Protocol.Not_Found;
      if I = 0 then
         return;
      end if;
      S := Subscription_Blocks.Get (T.Items, I - 1);
      if S.Owner /= From or else not S.Link.Active then
         return;
      end if;
      --  The read scopes, as for every read: none at all, or an exact name
      --  outside them, is refused here; a broader selector leaves out, per
      --  series, what the reader may not see (Decide).
      if not Metric_Scopes.Has_Any (Scopes, S.Reader)
        or else (Sel.Match = Metric_Series.Exact_Name
                 and then not Metric_Scopes.May_Read (Scopes, S.Reader, Sel.Name (1 .. Sel.Name_Length)))
      then
         Result := CuBit.Metric_Protocol.Out_Of_Scope;
         return;
      end if;
      if not S.Subscribed then
         T.Live := T.Live + 1;
      end if;
      S.Subscribed := True;
      S.Sel := Sel;
      --  A new selector: every series is decided again.
      Decision_Blocks.Release (S.Decisions);
      Subscription_Blocks.Set (T.Items, I - 1, S);
      Result := CuBit.Metric_Protocol.OK;
   end Subscribe;

   function Active (T : Table) return Boolean is (T.Live > 0);

   --  Whether S takes series Index (decided once, then remembered).
   procedure Decide
     (S : in out Subscription; Store : Metric_Series.Store; Scopes : Metric_Scopes.Table;
      Index : Metric_Series.Series_Index; Takes : out Boolean);
   procedure Decide
     (S : in out Subscription; Store : Metric_Series.Store; Scopes : Metric_Scopes.Table;
      Index : Metric_Series.Series_Index; Takes : out Boolean)
   is
      Grown : Boolean;
      Known : Decision := Undecided;
   begin
      if Index <= Decision_Blocks.Capacity (S.Decisions) then
         Known := Decision_Blocks.Get (S.Decisions, Index - 1);
      end if;
      if Known /= Undecided then
         Takes := Known = Taken;
         return;
      end if;
      Takes := Metric_Series.Selects (Store, S.Sel, Index)
        and then Metric_Scopes.May_Read
          (Scopes, S.Reader, Metric_Series.Text_Of (Store, Metric_Series.Info (Store, Index).With_Labels.Name));
      if Index > Decision_Blocks.Capacity (S.Decisions) then
         declare
            Old_Capacity : constant Natural := Decision_Blocks.Capacity (S.Decisions);
         begin
            Grow_Decisions (S.Decisions, Old_Capacity, Initial_Decisions, Metric_Series.Maximum_Series, Index,
                            Grown);
            if Grown then
               for K in Old_Capacity .. Decision_Blocks.Capacity (S.Decisions) - 1 loop
                  Decision_Blocks.Set (S.Decisions, K, Undecided);
               end loop;
            end if;
         end;
      end if;
      if Index <= Decision_Blocks.Capacity (S.Decisions) then
         Decision_Blocks.Set (S.Decisions, Index - 1, (if Takes then Taken else Passed));
      end if;
   end Decide;

   --  One element into S's ring: first any shed still unreported (one gap),
   --  then Item; what does not fit is counted for the next gap.
   procedure Put (T : in out Table; S : in out Subscription; Item : Updates.Element; Time_Us : Unsigned_64);
   procedure Put (T : in out Table; S : in out Subscription; Item : Updates.Element; Time_Us : Unsigned_64) is
      Wire : Updates.Element_Buffer;
      Outcome : Channels.Put_Result;
   begin
      if S.Shed > 0 then
         Updates.Encode ((Kind => Updates.Gap, Series => 0, Cause => Updates.Subscriber_Shed, Missing => S.Shed,
                          First_Us => S.Shed_First_Us, Last_Us => S.Shed_Last_Us), Wire);
         Channels.Put (S.Link, Wire'Address, Wire'Length, Outcome);
         if Outcome = Channels.Put then
            S.Shed := 0;
            S.Fresh := True;
         end if;
      end if;
      if S.Shed = 0 then
         Updates.Encode (Item, Wire);
         Channels.Put (S.Link, Wire'Address, Wire'Length, Outcome);
         if Outcome = Channels.Put then
            S.Fresh := True;
            T.Sums.Put := Add (T.Sums.Put, 1);
            return;
         end if;
      end if;
      --  No room (or an earlier shed still waits): counted, reported later.
      if S.Shed = 0 then
         S.Shed_First_Us := Time_Us;
      end if;
      S.Shed := Add (S.Shed, 1);
      S.Shed_First_Us := Unsigned_64'Min (S.Shed_First_Us, Time_Us);
      S.Shed_Last_Us := Unsigned_64'Max (S.Shed_Last_Us, Time_Us);
      T.Sums.Shed := Add (T.Sums.Shed, 1);
   end Put;

   procedure Offer
     (T : in out Table; Store : Metric_Series.Store; Scopes : Metric_Scopes.Table;
      Index : Metric_Series.Series_Index; Item : Updates.Element; Time_Us : Unsigned_64);
   procedure Offer
     (T : in out Table; Store : Metric_Series.Store; Scopes : Metric_Scopes.Table;
      Index : Metric_Series.Series_Index; Item : Updates.Element; Time_Us : Unsigned_64)
   is
      S : Subscription;
      Takes : Boolean;
   begin
      if T.Live = 0 then
         return;
      end if;
      for I in 0 .. T.Used - 1 loop
         S := Subscription_Blocks.Get (T.Items, I);
         if S.Subscribed and then S.Link.Active then
            Decide (S, Store, Scopes, Index, Takes);
            if Takes then
               Put (T, S, Item, Time_Us);
            end if;
            Subscription_Blocks.Set (T.Items, I, S);
         end if;
      end loop;
   end Offer;

   procedure Offer_Sample
     (T : in out Table; Store : Metric_Series.Store; Scopes : Metric_Scopes.Table;
      Index : Metric_Series.Series_Index; Time_Us, Value : Unsigned_64) is
   begin
      Offer (T, Store, Scopes, Index,
             (Kind => Updates.Sample, Series => Metric_Series.Info (Store, Index).Id, Time_Us => Time_Us,
              Value => Value), Time_Us);
   end Offer_Sample;

   procedure Offer_Gap
     (T : in out Table; Store : Metric_Series.Store; Scopes : Metric_Scopes.Table;
      Index : Metric_Series.Series_Index; Count, First_Us, Last_Us : Unsigned_64) is
   begin
      Offer (T, Store, Scopes, Index,
             (Kind => Updates.Gap, Series => Metric_Series.Info (Store, Index).Id, Cause => Updates.Producer_Shed,
              Missing => Count, First_Us => Unsigned_64'Min (First_Us, Last_Us),
              Last_Us => Unsigned_64'Max (First_Us, Last_Us)), Last_Us);
   end Offer_Gap;

   procedure Flush (T : in out Table) is
      S : Subscription;
      Ignore : Channels.Signal_Result;
      Wire : Updates.Element_Buffer;
      Outcome : Channels.Put_Result;
   begin
      if T.Live = 0 then
         return;
      end if;
      for I in 0 .. T.Used - 1 loop
         S := Subscription_Blocks.Get (T.Items, I);
         if S.Subscribed and then S.Link.Active then
            if S.Shed > 0 then
               Updates.Encode ((Kind => Updates.Gap, Series => 0, Cause => Updates.Subscriber_Shed,
                                Missing => S.Shed, First_Us => S.Shed_First_Us, Last_Us => S.Shed_Last_Us), Wire);
               Channels.Put (S.Link, Wire'Address, Wire'Length, Outcome);
               if Outcome = Channels.Put then
                  S.Shed := 0;
                  S.Fresh := True;
               end if;
            end if;
            if S.Fresh and then Channels.Consumer_Waiting (S.Link) then
               --  One wake per pass, for a reader asleep on its ring (one
               --  that reads on its own schedule is never interrupted).
               Channels.Signal (S.Link, Sync => False, Result => Ignore);
            end if;
            S.Fresh := False;
            Subscription_Blocks.Set (T.Items, I, S);
         end if;
      end loop;
   end Flush;

   function Accounting (T : Table) return Totals is (T.Sums);
end Metric_Subscriptions;
