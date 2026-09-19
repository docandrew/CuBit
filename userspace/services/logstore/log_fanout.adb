with CuBit.Log_Protocol; use CuBit.Log_Protocol;
package body Log_Fanout with SPARK_Mode is
   procedure Advance_Time (Item : in out Broker; Now_Ms : Unsigned_64) is
   begin
      Item.Now_Ms := Unsigned_64'Max (Item.Now_Ms, Now_Ms);
      for Client of Item.Clients loop
         if Client.Owner /= 0 and then
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
         if Item.Lost < Unsigned_64'Last then Item.Lost := Item.Lost + 1; end if;
      else
         Item.Used := Item.Used + 1;
      end if;
      Item.Data ((Item.Head + Item.Used - 1) mod Capacity) := Value;
   end Append;

   procedure Publish (Item : in out Broker; Value : Event) is
   begin
      Append (Item.Recent, Value);
      for Client of Item.Clients loop
         if Client.Owner /= 0 then Append (Client.Pending, Value); end if;
      end loop;
   end Publish;

   procedure Subscribe
     (Item : in out Broker; Caller, Authority_Tag : Unsigned_64;
      Handle : out Unsigned_64; Result : out Status) is
   begin
      Handle := 0;
      Result := Denied;
      if Caller = 0 or else not May_Invoke (Authority_Tag, Subscribe) then return; end if;
      Result := Exhausted;
      --  One subscription per process/issued authority; retries do not reset
      --  its cursor or occupy more slots.
      for Client of Item.Clients loop
         if Client.Owner = Caller and then Client.Authority_Tag = Authority_Tag then
            Handle := Client.Handle;
            Client.Last_Use := Item.Now_Ms;
            Result := OK;
            return;
         end if;
      end loop;
      if Item.Next_Handle = Unsigned_64'Last then return; end if;
      for Client of Item.Clients loop
         if Client.Owner = 0 then
            Handle := Item.Next_Handle;
            Item.Next_Handle := Item.Next_Handle + 1;
            Client := (Owner => Caller, Authority_Tag => Authority_Tag,
                       Last_Use => Item.Now_Ms, Handle => Handle, Pending => Item.Recent);
            --  Subscription starts with the retained snapshot. Earlier events
            --  were never accepted for this subscriber: no hidden total count.
            Client.Pending.Lost := 0;
            Result := OK;
            return;
         end if;
      end loop;
   end Subscribe;

   procedure Read_Next
     (Item : in out Broker; Caller, Authority_Tag, Handle : Unsigned_64;
      Value : out Event; Lost : out Unsigned_64; Result : out Status) is
   begin
      Value := (others => <>); Lost := 0; Result := Denied;
      if Caller = 0 or else not May_Invoke (Authority_Tag, Read_Next) then return; end if;
      for Client of Item.Clients loop
         if Client.Owner = Caller and then Client.Authority_Tag = Authority_Tag
           and then Client.Handle = Handle then
            Client.Last_Use := Item.Now_Ms;
            if Client.Pending.Lost /= 0 then
               Lost := Client.Pending.Lost; Client.Pending.Lost := 0; Result := Gap;
            elsif Client.Pending.Used = 0 then
               Result := Empty;
            else
               Value := Client.Pending.Data (Client.Pending.Head);
               Client.Pending.Head := (Client.Pending.Head + 1) mod Capacity;
               Client.Pending.Used := Client.Pending.Used - 1;
               Result := OK;
            end if;
            return;
         end if;
      end loop;
   end Read_Next;

   procedure Close
     (Item : in out Broker; Caller, Authority_Tag, Handle : Unsigned_64;
      Result : out Status) is
   begin
      Result := Denied;
      if Caller = 0 or else not May_Invoke (Authority_Tag, Close) then return; end if;
      for Client of Item.Clients loop
         if Client.Owner = Caller and then Client.Authority_Tag = Authority_Tag
           and then Client.Handle = Handle then
            Client := (others => <>); Result := OK; return;
         end if;
      end loop;
   end Close;
end Log_Fanout;
