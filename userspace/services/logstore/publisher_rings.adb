pragma Ada_2022;
with CuBit.Channel_Contracts;
with CuBit.Channel_Protocol;
with CuBit.Log_Protocol;
with CuBit.Log_Publish_Rings;
with CuBit.Console_Lines;

with CuBit.Process_IDs.Text;
with Publisher_Slots;
package body Publisher_Rings is
   use CuBit.Messages;
   package Channels renames CuBit.Channels;
   package Publish renames CuBit.Log_Publish_Rings;
   package Logs renames CuBit.Log_Records;
   use type Channels.Take_Result;
   use type Channels.Side;
   use type CuBit.Channel_Contracts.Contract;
   use type Logs.Severity;

   Mirror : Console_Mirror := null;

   --  The minimum publishers are told: everything while mirroring.
   function Advertised (Minimum : Logs.Severity) return Logs.Severity is
     (if Mirror /= null then Logs.Trace else Minimum);

   procedure Set_Mirror (Mirror : not null Console_Mirror) is
   begin
      Publisher_Rings.Mirror := Mirror;
   end Set_Mirror;

   --  logstore's own record about a publisher (what it shed).
   procedure Note_Shed
     (Store : in out Log_Fanout.Broker; Owner : Process_ID;
      Over_Budget, Publisher_Full, Now_Ms : Unsigned_64);
   procedure Note_Shed
     (Store : in out Log_Fanout.Broker; Owner : Process_ID;
      Over_Budget, Publisher_Full, Now_Ms : Unsigned_64)
   is
      Made : constant Logs.Decoded :=
        Logs.Make ("logstore: shed records from pid " & CuBit.Process_IDs.Text.Image (Owner) & ":" &
                   Unsigned_64'Image (Over_Budget) & " over budget," &
                   Unsigned_64'Image (Publisher_Full) & " by the publisher (ring full)", Logs.Warning);
   begin
      if Made.Success then
         Log_Fanout.Publish
           (Store, (Source => CuBit.Messages.Own_Process_Word,
                    Node => CuBit.Log_Protocol.This_Node, Publication_Tag => 0,
                    Monotonic_Ms => Now_Ms, Data => Made.Value));
      end if;
   end Note_Shed;

   --  Take up to a batch of R's records into Store.
   procedure Drain_One
     (R : in out Ring; Store : in out Log_Fanout.Broker; Budgets : in out Log_Budgets.Limiter;
      Minimum : Logs.Severity; Now_Ms : Unsigned_64; Pending : in out Boolean);
   procedure Drain_One
     (R : in out Ring; Store : in out Log_Fanout.Broker; Budgets : in out Log_Budgets.Limiter;
      Minimum : Logs.Severity; Now_Ms : Unsigned_64; Pending : in out Boolean)
   is
      Wire : Logs.Wire_Buffer;
      Length : Natural;
      Taken : Channels.Take_Result;
      Admitted : Boolean;
      Decoded : Logs.Decoded;
      --  Shed by logstore's budget, and by the publisher (its ring full).
      Over_Budget, Publisher_Full : Unsigned_64 := 0;
      Took : Boolean := False;
      Reported_Shed : constant Unsigned_64 := Channels.Shed_Count (R.Link);
   begin
      for Count in 1 .. Batch_Records loop
         --  Copied out of the publisher's ring, then decoded (untrusted).
         Channels.Take (R.Link, Wire'Address, Wire'Length, Length, Taken, Hold => True);
         exit when Taken /= Channels.Taken;
         Took := True;
         if Length in Logs.Header_Bytes .. Natural (Logs.Wire_Count'Last) then
            Decoded := Logs.Decode (Wire, Logs.Wire_Count (Length));
            if Decoded.Success and then Mirror /= null
              and then CuBit.Console_Lines.Is_Console (Decoded.Value)
            then
               Mirror (R.Owner, Decoded.Value);
            end if;
            if Decoded.Success and then Logs.Level (Decoded.Value) >= Minimum then
               Log_Budgets.Admit (Budgets, CuBit.Log_Protocol.Publication_Budget (R.Authority), Admitted);
               if Admitted then
                  Log_Fanout.Publish
                    (Store, (Source => To_Word (R.Owner), Node => CuBit.Log_Protocol.This_Node,
                             Publication_Tag => R.Authority, Monotonic_Ms => Now_Ms,
                             Data => Decoded.Value));
               elsif Over_Budget < Unsigned_64'Last then
                  Over_Budget := Over_Budget + 1;
               end if;
            end if;
         end if;
         if Count = Batch_Records then
            Pending := True;
         end if;
      end loop;
      if Took then
         --  Released after this pass has also written them to the readers'
         --  streams (Release): only then may the publisher count them done.
         R.Unreleased := True;
         R.Active_Ms := Now_Ms;
      end if;
      Channels.Set_Consumer_Word (R.Link, Publish.Minimum_Word, Logs.Severity'Pos (Advertised (Minimum)));
      --  What the publisher could not fit, as it counted, since last time.
      if Reported_Shed > R.Shed_Seen then
         Publisher_Full := Reported_Shed - R.Shed_Seen;
      end if;
      R.Shed_Seen := Reported_Shed;
      if Over_Budget > 0 or else Publisher_Full > 0 then
         --  In the source's tally (logs.recent shows it), and as logstore's
         --  own record.
         Log_Fanout.Count_Shed (Store, To_Word (R.Owner), Over_Budget, Publisher_Full);
         Note_Shed (Store, R.Owner, Over_Budget, Publisher_Full, Now_Ms);
      end if;
   end Drain_One;

   --  Drain R one last time and let it go.
   procedure Finish
     (R : in out Ring; Store : in out Log_Fanout.Broker; Budgets : in out Log_Budgets.Limiter;
      Minimum : Logs.Severity; Now_Ms : Unsigned_64);
   procedure Finish
     (R : in out Ring; Store : in out Log_Fanout.Broker; Budgets : in out Log_Budgets.Limiter;
      Minimum : Logs.Severity; Now_Ms : Unsigned_64)
   is
      Pending : Boolean := False;
   begin
      Drain_One (R, Store, Budgets, Minimum, Now_Ms, Pending);
      --  Its history stays, but gives way first to a new source (LOG-002).
      Log_Fanout.Retire (Store, To_Word (R.Owner));
      Channels.Release (R.Link);
      Channels.Close (R.Link);
      R := (others => <>);
   end Finish;

   --  The ring with number Number (1 .. Used * Chunk_Rings).
   function Ring_At (Item : Table; Number : Positive) return access Ring is
     (Item.Chunks ((Number - 1) / Chunk_Rings + 1) ((Number - 1) mod Chunk_Rings + 1)'Access);

   --  logstore's own record: an open it refused.
   procedure Note_Refused (Store : in out Log_Fanout.Broker; From : Process_ID; Now_Ms : Unsigned_64);
   procedure Note_Refused (Store : in out Log_Fanout.Broker; From : Process_ID; Now_Ms : Unsigned_64) is
      Made : constant Logs.Decoded :=
        Logs.Make ("logstore: refused a log channel for pid " & CuBit.Process_IDs.Text.Image (From) &
                   ": no room (" & Natural'Image (Maximum_Publishers) & " channels at most, or no memory)",
                   Logs.Warning);
   begin
      if Made.Success then
         Log_Fanout.Publish
           (Store, (Source => CuBit.Messages.Own_Process_Word,
                    Node => CuBit.Log_Protocol.This_Node, Publication_Tag => 0,
                    Monotonic_Ms => Now_Ms, Data => Made.Value));
      end if;
   end Note_Refused;

   procedure Open
     (Item : in out Table; Store : in out Log_Fanout.Broker; Budgets : in out Log_Budgets.Limiter;
      From : CuBit.Messages.Process_ID; Authority : Unsigned_64;
      Request : CuBit.Messages.Message; Minimum : CuBit.Log_Records.Severity;
      Now_Ms : Unsigned_64; Reply : out CuBit.Messages.Message)
   is
      Chosen : Natural := 0;
      Is_Open, Valid : Boolean;
      Offered : CuBit.Channel_Contracts.Contract;
      Opener_Side : Channels.Side;
      Ignore_Connector : Unsigned_16;
   begin
      Channels.Decode_Open (Request, Is_Open, Valid, Offered, Opener_Side, Ignore_Connector);
      if not Valid or else Offered /= Publish.CONTRACT or else Opener_Side /= Channels.Producing then
         Reply := Channels.Refusal_Reply (CuBit.Channel_Protocol.Unknown_Type);
         return;
      end if;
      --  The same publisher again: its earlier channel (drained before it
      --  is let go), else a free entry, else a new chunk's first; never
      --  another publisher's live channel (Publisher_Slots).
      declare
         function Slot_Count return Natural is (Item.Used * Chunk_Rings);
         function Slot_Live (I : Positive) return Boolean is (Ring_At (Item, I).Link.Active);
         function Slot_Owner (I : Positive) return Unsigned_64 is (To_Word (Ring_At (Item, I).Owner));
         function Slot_Authority (I : Positive) return Unsigned_64 is (Ring_At (Item, I).Authority);
         package Slots is new Publisher_Slots (Slot_Count, Slot_Live, Slot_Owner, Slot_Authority);
         Pick : constant Slots.Choice :=
           Slots.Choose (To_Word (From), Authority, May_Grow => Item.Used < Maximum_Chunks);
      begin
         case Pick.Kind is
            when Slots.Same_Publisher | Slots.Free_Entry =>
               Chosen := Pick.Index;
            when Slots.Grow =>
               begin
                  Item.Chunks (Item.Used + 1) := new Chunk;
                  Item.Used := Item.Used + 1;
                  Chosen := (Item.Used - 1) * Chunk_Rings + 1;
               exception
                  when Storage_Error =>
                     null;
               end;
            when Slots.No_Room =>
               null;
         end case;
      end;
      if Chosen = 0 then
         Note_Refused (Store, From, Now_Ms);
         Reply := Channels.Refusal_Reply (CuBit.Channel_Protocol.No_Room);
         return;
      end if;
      declare
         R : Ring renames Ring_At (Item, Chosen).all;
      begin
         if R.Link.Active then
            Finish (R, Store, Budgets, Minimum, Now_Ms);
         end if;
         Channels.Accept_Open (From, Request, Unsigned_64 (Chosen), R.Link, Reply);
         if R.Link.Active then
            R.Owner := From;
            R.Authority := Authority;
            R.Shed_Seen := Channels.Shed_Count (R.Link);
            R.Active_Ms := Now_Ms;
            Channels.Set_Consumer_Word (R.Link, Publish.Minimum_Word, Logs.Severity'Pos (Advertised (Minimum)));
         end if;
      end;
   end Open;

   procedure Close
     (Item : in out Table; Store : in out Log_Fanout.Broker; Budgets : in out Log_Budgets.Limiter;
      From : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message;
      Minimum : CuBit.Log_Records.Severity; Now_Ms : Unsigned_64) is
      Number : constant Unsigned_64 := Channels.Number_Of (Request);
   begin
      if Number in 1 .. Unsigned_64 (Item.Used * Chunk_Rings)
        and then Ring_At (Item, Positive (Number)).Owner = From
        and then Channels.Closed_By (Ring_At (Item, Positive (Number)).Link, From, Request)
      then
         Finish (Ring_At (Item, Positive (Number)).all, Store, Budgets, Minimum, Now_Ms);
      end if;
   end Close;

   procedure Ended
     (Item : in out Table; Store : in out Log_Fanout.Broker; Budgets : in out Log_Budgets.Limiter;
      Event : CuBit.Control_Events.Event; Minimum : CuBit.Log_Records.Severity;
      Now_Ms : Unsigned_64) is
   begin
      for C in 1 .. Item.Used loop
         for R of Item.Chunks (C).all loop
            if Channels.Ended (R.Link, Event) then
               Finish (R, Store, Budgets, Minimum, Now_Ms);
            end if;
         end loop;
      end loop;
   end Ended;

   procedure Drain
     (Item : in out Table; Store : in out Log_Fanout.Broker; Budgets : in out Log_Budgets.Limiter;
      Minimum : CuBit.Log_Records.Severity; Now_Ms : Unsigned_64; Pending : out Boolean) is
   begin
      Pending := False;
      for C in 1 .. Item.Used loop
         for R of Item.Chunks (C).all loop
            if R.Link.Active then
               Drain_One (R, Store, Budgets, Minimum, Now_Ms, Pending);
            end if;
         end loop;
      end loop;
   end Drain;

   procedure Release (Item : in out Table) is
   begin
      for C in 1 .. Item.Used loop
         for R of Item.Chunks (C).all loop
            if R.Unreleased then
               Channels.Release (R.Link);
               R.Unreleased := False;
            end if;
         end loop;
      end loop;
   end Release;

   procedure Arm (Item : in out Table; Pending : out Boolean) is
   begin
      Pending := False;
      for C in 1 .. Item.Used loop
         for R of Item.Chunks (C).all loop
            --  A record put before the publisher saw us armed sent no kick.
            if R.Link.Active and then not Channels.Arm (R.Link) then
               Pending := True;
            end if;
         end loop;
      end loop;
   end Arm;

   procedure Disarm (Item : in out Table) is
   begin
      for C in 1 .. Item.Used loop
         for R of Item.Chunks (C).all loop
            if R.Link.Active then
               Channels.Disarm (R.Link);
            end if;
         end loop;
      end loop;
   end Disarm;
end Publisher_Rings;
