pragma Ada_2022;
with CuBit.Channel_Contracts;
with CuBit.Channel_Protocol;
with CuBit.Log_Protocol;
with CuBit.Log_Publish_Rings;

package body Publisher_Rings is
   package Channels renames CuBit.Channels;
   package Publish renames CuBit.Log_Publish_Rings;
   package Logs renames CuBit.Log_Records;
   use type Channels.Take_Result;
   use type Channels.Side;
   use type CuBit.Channel_Contracts.Contract;
   use type Logs.Severity;

   --  logstore's own record about a publisher (what it shed).
   procedure Note_Shed
     (Store : in out Log_Fanout.Broker; Owner, Count, Now_Ms : Unsigned_64);
   procedure Note_Shed
     (Store : in out Log_Fanout.Broker; Owner, Count, Now_Ms : Unsigned_64)
   is
      Made : constant Logs.Decoded :=
        Logs.Make ("logstore: shed" & Unsigned_64'Image (Count) & " records from pid" &
                   Unsigned_64'Image (Owner) & " (ring full or over budget)", Logs.Warning);
   begin
      if Made.Success then
         Log_Fanout.Publish
           (Store, (Source => CuBit.Messages.syscall (CuBit.Messages.SYSCALL_GETPID),
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
      Shed : Unsigned_64 := 0;
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
            if Decoded.Success and then Logs.Level (Decoded.Value) >= Minimum then
               Log_Budgets.Admit (Budgets, CuBit.Log_Protocol.Publication_Budget (R.Authority), Admitted);
               if Admitted then
                  Log_Fanout.Publish
                    (Store, (Source => R.Owner, Node => CuBit.Log_Protocol.This_Node,
                             Publication_Tag => R.Authority, Monotonic_Ms => Now_Ms,
                             Data => Decoded.Value));
               else
                  Shed := Shed + 1;
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
      Channels.Set_Consumer_Word (R.Link, Publish.Minimum_Word, Logs.Severity'Pos (Minimum));
      --  What the publisher could not fit, as it counted, since last time.
      if Reported_Shed > R.Shed_Seen then
         Shed := Shed + (Reported_Shed - R.Shed_Seen);
      end if;
      R.Shed_Seen := Reported_Shed;
      if Shed > 0 then
         Note_Shed (Store, R.Owner, Shed, Now_Ms);
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
      Channels.Release (R.Link);
      Channels.Close (R.Link);
      R := (others => <>);
   end Finish;

   procedure Open
     (Item : in out Table; Store : in out Log_Fanout.Broker; Budgets : in out Log_Budgets.Limiter;
      From : CuBit.Messages.ProcessID; Authority : Unsigned_64;
      Request : CuBit.Messages.Message; Minimum : CuBit.Log_Records.Severity;
      Now_Ms : Unsigned_64; Reply : out CuBit.Messages.Message)
   is
      Chosen : Natural := 0;
      Oldest : Unsigned_64 := Unsigned_64'Last;
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
      --  The same publisher again: its earlier channel first, then a free
      --  entry, then the longest idle (drained before it is let go).
      for I in Item'Range loop
         if Item (I).Link.Active
           and then Item (I).Owner = Unsigned_64 (From) and then Item (I).Authority = Authority
         then
            Chosen := I;
         end if;
      end loop;
      if Chosen = 0 then
         for I in Item'Range loop
            if not Item (I).Link.Active then
               Chosen := I;
               exit;
            end if;
         end loop;
      end if;
      if Chosen = 0 then
         for I in Item'Range loop
            if Item (I).Active_Ms < Oldest then
               Oldest := Item (I).Active_Ms;
               Chosen := I;
            end if;
         end loop;
      end if;
      if Item (Chosen).Link.Active then
         Finish (Item (Chosen), Store, Budgets, Minimum, Now_Ms);
      end if;
      Channels.Accept_Open (From, Request, Unsigned_64 (Chosen), Item (Chosen).Link, Reply);
      if Item (Chosen).Link.Active then
         Item (Chosen).Owner := Unsigned_64 (From);
         Item (Chosen).Authority := Authority;
         Item (Chosen).Shed_Seen := Channels.Shed_Count (Item (Chosen).Link);
         Item (Chosen).Active_Ms := Now_Ms;
         Channels.Set_Consumer_Word (Item (Chosen).Link, Publish.Minimum_Word, Logs.Severity'Pos (Minimum));
      end if;
   end Open;

   procedure Close
     (Item : in out Table; Store : in out Log_Fanout.Broker; Budgets : in out Log_Budgets.Limiter;
      From : CuBit.Messages.ProcessID; Number : Unsigned_64;
      Minimum : CuBit.Log_Records.Severity; Now_Ms : Unsigned_64) is
   begin
      if Number in 1 .. Maximum_Publishers
        and then Item (Natural (Number)).Link.Active
        and then Item (Natural (Number)).Owner = Unsigned_64 (From)
      then
         Finish (Item (Natural (Number)), Store, Budgets, Minimum, Now_Ms);
      end if;
   end Close;

   procedure Ended
     (Item : in out Table; Store : in out Log_Fanout.Broker; Budgets : in out Log_Budgets.Limiter;
      Event : CuBit.Control_Events.Event; Minimum : CuBit.Log_Records.Severity;
      Now_Ms : Unsigned_64) is
   begin
      for R of Item loop
         if Channels.Ended (R.Link, Event) then
            Finish (R, Store, Budgets, Minimum, Now_Ms);
         end if;
      end loop;
   end Ended;

   procedure Drain
     (Item : in out Table; Store : in out Log_Fanout.Broker; Budgets : in out Log_Budgets.Limiter;
      Minimum : CuBit.Log_Records.Severity; Now_Ms : Unsigned_64; Pending : out Boolean) is
   begin
      Pending := False;
      for R of Item loop
         if R.Link.Active then
            Drain_One (R, Store, Budgets, Minimum, Now_Ms, Pending);
         end if;
      end loop;
   end Drain;

   procedure Release (Item : in out Table) is
   begin
      for R of Item loop
         if R.Unreleased then
            Channels.Release (R.Link);
            R.Unreleased := False;
         end if;
      end loop;
   end Release;

   procedure Arm (Item : in out Table; Pending : out Boolean) is
   begin
      Pending := False;
      for R of Item loop
         --  A record put before the publisher saw us armed sent no kick.
         if R.Link.Active and then not Channels.Arm (R.Link) then
            Pending := True;
         end if;
      end loop;
   end Arm;

   procedure Disarm (Item : in out Table) is
   begin
      for R of Item loop
         if R.Link.Active then
            Channels.Disarm (R.Link);
         end if;
      end loop;
   end Disarm;
end Publisher_Rings;
