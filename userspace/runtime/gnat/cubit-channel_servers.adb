------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with System.Address_To_Access_Conversions;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Channel_Bells;
with CuBit.Grant_References;
with CuBit.Channel_Protocol;
with CuBit.Process_IDs;
with CuBit.Protocols;

package body CuBit.Channel_Servers is

   package CC renames CuBit.Channel_Contracts;
   package CP renames CuBit.Channel_Protocol;
   use type CC.Channel_Kind;
   use type CuBit.Channel_Bells.Watch_Result;
   use type Admission.Admission;
   use type System.Address;

   ---------------------------------------------------------------------------
   --  Memory: the process heap (CuAlloc), which answers 0 when it is out,
   --  so a table that cannot grow refuses a channel instead of failing.
   ---------------------------------------------------------------------------
   function CuAlloc_Allocate (Bytes, Alignment : Unsigned_64) return Unsigned_64
     with Import, Convention => C, External_Name => "cualloc_allocate";
   procedure CuAlloc_Free (Item : Unsigned_64)
     with Import, Convention => C, External_Name => "cualloc_free";
   Alignment : constant := 64;

   function Allocate (Bytes : Storage_Count) return System.Address;
   function Allocate (Bytes : Storage_Count) return System.Address is
     (To_Address (Integer_Address (CuAlloc_Allocate (Unsigned_64 (Bytes), Alignment))));
   procedure Free (Item : System.Address);
   procedure Free (Item : System.Address) is
   begin
      if Item /= System.Null_Address then
         CuAlloc_Free (Unsigned_64 (To_Integer (Item)));
      end if;
   end Free;

   type Block is array (1 .. Block_Size) of Connection;
   package Connection_Pointers is new System.Address_To_Access_Conversions (Connection);
   subtype Link is Connection_Pointers.Object_Pointer;

   function Link_Of (S : Server; I : Index) return Link
   with Pre => I <= S.Size;
   function Link_Of (S : Server; I : Index) return Link is
     (Connection_Pointers.To_Pointer
        (S.Blocks ((I - 1) / Block_Size + 1)
         + Storage_Offset (((I - 1) mod Block_Size) * (Block'Component_Size / System.Storage_Unit))));

   function Address_Of (Base : Unsigned_64) return System.Address;
   function Address_Of (Base : Unsigned_64) return System.Address is
     (To_Address (Integer_Address (Base)));

   function Client_Word (P : CuBit.Messages.Process_ID) return Admission.Client_Id;
   function Client_Word (P : CuBit.Messages.Process_ID) return Admission.Client_Id is
     (Admission.Client_Id (CuBit.Process_IDs.To_Word (P)));

   function Size (S : Server) return Natural is (S.Size);

   --  Replace the table with one of New_Size channels (a multiple of
   --  Block_Size): new blocks, and the admission and schedule states copied
   --  into larger ones. False: no memory (nothing changed).
   procedure Grow_To (S : in out Server; New_Size : Index; Grown : out Boolean);
   procedure Grow_To (S : in out Server; New_Size : Index; Grown : out Boolean) is
      subtype New_Limits is Admission.State (New_Size);
      subtype New_Order is Schedule.State (New_Size);
      First_New_Block : constant Positive := S.Size / Block_Size + 1;
      Last_New_Block  : constant Positive := New_Size / Block_Size;
      Limits_At : constant System.Address := Allocate (New_Limits'Max_Size_In_Storage_Elements);
      Order_At  : constant System.Address :=
        (if Limits_At = System.Null_Address then System.Null_Address
         else Allocate (New_Order'Max_Size_In_Storage_Elements));
      Made : Natural := 0;
   begin
      Grown := False;
      if Order_At = System.Null_Address then
         Free (Limits_At);
         return;
      end if;
      for B in First_New_Block .. Last_New_Block loop
         S.Blocks (B) := Allocate (Block'Max_Size_In_Storage_Elements);
         exit when S.Blocks (B) = System.Null_Address;
         Made := Made + 1;
         declare
            Where : constant System.Address := S.Blocks (B);
            --  Declared without Import: every connection starts inactive.
            Fresh : Block with Address => Where;
            pragma Unreferenced (Fresh);
         begin
            null;
         end;
      end loop;
      if Made /= Last_New_Block - First_New_Block + 1 then
         for B in First_New_Block .. First_New_Block + Made - 1 loop
            Free (S.Blocks (B));
            S.Blocks (B) := System.Null_Address;
         end loop;
         Free (Order_At);
         Free (Limits_At);
         return;
      end if;
      declare
         Into_Limits : New_Limits with Address => Limits_At;
         Into_Order  : New_Order with Address => Order_At;
      begin
         if S.Size = 0 then
            declare
               Declared : Admission.Limit_Table;
            begin
               for O in Offer_Index loop
                  Declared (O) := (Per_Client => S.Offers (O).Per_Client, Total => S.Offers (O).Total);
               end loop;
               Admission.Start (Into_Limits, Declared);
            end;
         else
            declare
               Old_Limits : Admission.State (S.Size) with Import, Address => S.Limits_At;
               Old_Order  : Schedule.State (S.Size) with Import, Address => S.Order_At;
            begin
               Admission.Grow (Old_Limits, Into_Limits);
               Schedule.Grow (Old_Order, Into_Order);
            end;
         end if;
      end;
      Free (S.Limits_At);
      Free (S.Order_At);
      S.Limits_At := Limits_At;
      S.Order_At := Order_At;
      S.Size := New_Size;
      Grown := True;
   end Grow_To;

   procedure Configure (S : in out Server; Offers : Offer_Table; Configured : out Boolean) is
   begin
      S.Offers := Offers;
      S.Size := 0;
      Grow_To (S, Block_Size, Configured);
      S.Started := Configured;
   end Configure;

   --  The offer whose contract the open names (same schema identity and
   --  version, and the duplex layout this server serves).
   procedure Find_Offer (S : Server; Item : CC.Contract; Found : out Boolean; O : out Offer_Index);
   procedure Find_Offer (S : Server; Item : CC.Contract; Found : out Boolean; O : out Offer_Index) is
      use type CuBit.Protocols.Schema_Id, CuBit.Protocols.Protocol_Version;
   begin
      Found := False;
      O := Offer_Index'First;
      if Item.Kind /= CC.Duplex or else Item.Pages /= RC.Client_Pages
        or else Item.Buffers /= RC.Server_Pages
      then
         return;
      end if;
      for P in Offer_Index loop
         if S.Offers (P).Element.Element.Identity = Item.Element.Identity
           and then S.Offers (P).Element.Element.Version = Item.Element.Version
         then
            Found := True;
            O := P;
            return;
         end if;
      end loop;
   end Find_Offer;

   procedure Count (C : in out Unsigned_64);
   procedure Count (C : in out Unsigned_64) is
   begin
      if C < Unsigned_64'Last then
         C := C + 1;
      end if;
   end Count;

   --  A free connection, growing the table when it is full (False: the
   --  table is at its structural bound or out of memory).
   procedure Find_Free (S : in out Server; Free : out Natural);
   procedure Find_Free (S : in out Server; Free : out Natural) is
      Grown : Boolean;
      Old_Size : constant Natural := S.Size;
   begin
      for I in 1 .. S.Size loop
         if not Link_Of (S, I).Active then
            Free := I;
            return;
         end if;
      end loop;
      Free := 0;
      if S.Size >= Schedule.Maximum_Size then
         return;
      end if;
      Grow_To (S, Natural'Min (2 * S.Size, Schedule.Maximum_Size), Grown);
      if Grown then
         Free := Old_Size + 1;
      end if;
   end Find_Free;

   procedure Handle_Open
     (S : in out Server; From : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message;
      Is_Open : out Boolean; Reply : out CuBit.Messages.Message;
      Opened : out Boolean; At_Index : out Index)
   is
      Valid, Found : Boolean;
      Item : CC.Contract;
      Opener_Side : CuBit.Channels.Side;
      Connector : Unsigned_16;
      O : Offer_Index;
      Free : Natural;
      Admitted : Admission.Admission;
      Watched : CuBit.Channel_Bells.Watch_Result;
      use type CuBit.Channels.Side;
   begin
      Opened := False;
      At_Index := Index'First;
      Reply := CuBit.Messages.NULL_MESSAGE;
      CuBit.Channels.Decode_Open (Request, Is_Open, Valid, Item, Opener_Side, Connector);
      if not Is_Open then
         return;
      end if;
      Find_Offer (S, Item, Found, O);
      if not S.Started or else not Valid or else not Found
        or else Opener_Side /= CuBit.Channels.Producing
      then
         Count (S.Counts.Unknown);
         Reply := CuBit.Channels.Refusal_Reply (CP.Unknown_Type);
         return;
      end if;
      if not Admit (O, From, Request.authorityTag) then
         Count (S.Counts.Not_Admitted);
         Reply := CuBit.Channels.Refusal_Reply (CP.Not_Admitted);
         return;
      end if;
      if not Can_Serve_Another (O) then
         Count (S.Counts.Exhausted);
         Reply := CuBit.Channels.Refusal_Reply (CP.Limit_Reached);
         return;
      end if;
      Find_Free (S, Free);
      if Free = 0 then
         Count (S.Counts.Exhausted);
         Reply := CuBit.Channels.Refusal_Reply (CP.Limit_Reached);
         return;
      end if;
      declare
         Limits : Admission.State (S.Size) with Import, Address => S.Limits_At;
      begin
         Admission.Admit (Limits, O, Client_Word (From), Admitted);
         if Admitted /= Admission.Admitted then
            Count (S.Counts.Limit);
            Reply := CuBit.Channels.Refusal_Reply (CP.Limit_Reached);
            return;
         end if;
         declare
            L : constant Link := Link_Of (S, Free);
            Order : Schedule.State (S.Size) with Import, Address => S.Order_At;
         begin
            CuBit.Channels.Accept_Open (From, Request, Unsigned_64 (Free), L.Channel, Reply);
            if not L.Channel.Active then
               Admission.Release (Limits, O, Client_Word (From));
               Count (S.Counts.Bad_Grant);
               return;
            end if;
            RC.Attach_Server (L.Ring, Address_Of (L.Channel.Own_Base), Address_Of (L.Channel.Peer_Base));
            CuBit.Channel_Bells.Watch (L.Channel.Peer_Grant, Watched);
            L.Routed := Watched = CuBit.Channel_Bells.Routed;
            L.Client := From;
            L.Offer := O;
            L.Ring_Owed := False;
            L.Needs_Arm := True;
            L.Active := True;
            Schedule.Open (Order, Free, S.Offers (O).Service);
            --  The client may have queued requests before the reply arrives.
            Schedule.Mark_Ready (Order, Free);
            Opened := True;
            At_Index := Free;
         end;
      end;
   end Handle_Open;

   procedure Refresh (S : in out Server) is
      Order : Schedule.State (S.Size) with Import, Address => S.Order_At;
      use type CuBit.Grant_References.Reference;

      procedure Look (I : Index);
      procedure Look (I : Index) is
         L : constant Link := Link_Of (S, I);
      begin
         L.Needs_Arm := True;
         if RC.Can_Take (L.Ring) then
            Schedule.Mark_Ready (Order, I);
         end if;
      end Look;
   begin
      if S.Wake_Bells_Valid and then not CuBit.Channel_Bells.Noted then
         --  Only the channels whose grants the wake named.
         for B in 1 .. S.Wake_Bell_Count loop
            for I in 1 .. S.Size loop
               declare
                  L : constant Link := Link_Of (S, I);
               begin
                  if L.Active and then L.Channel.Peer_Grant = S.Wake_Bells (B) then
                     Look (I);
                     exit;
                  end if;
               end;
            end loop;
         end loop;
      else
         CuBit.Channel_Bells.Clear_Noted;
         for I in 1 .. S.Size loop
            declare
               L : constant Link := Link_Of (S, I);
            begin
               if L.Active and then CuBit.Channel_Bells.Take (L.Channel.Peer_Grant, L.Routed) then
                  Look (I);
               end if;
            end;
         end loop;
      end if;
      S.Wake_Bells_Valid := False;
      S.Wake_Bell_Count := 0;
   end Refresh;

   procedure Next (S : in out Server; Found : out Boolean; I : out Index) is
      Order : Schedule.State (S.Size) with Import, Address => S.Order_At;
   begin
      Schedule.Next (Order, Found, I);
      if not Found then
         Schedule.Begin_Turn (Order);
         Schedule.Next (Order, Found, I);
      end if;
   end Next;

   procedure Ring_Client (L : Connection; Sync : Boolean);
   procedure Ring_Client (L : Connection; Sync : Boolean) is
      Ignore : CuBit.Channels.Signal_Result;
   begin
      CuBit.Channels.Signal (L.Channel, Sync, Ignore);
   end Ring_Client;

   procedure Take (S : in out Server; I : Index; Item : out RC.Queues.Submission; Got : out Boolean) is
      L : constant Link := Link_Of (S, I);
      Ring : Boolean;
   begin
      Got := False;
      if not L.Active then
         return;
      end if;
      RC.Take (L.Ring, Item, Got, Ring);
      if Ring then
         L.Ring_Owed := True;
      end if;
   end Take;

   procedure Complete (S : in out Server; I : Index; Tag : RC.Token; Answer : RC.Result) is
      L : constant Link := Link_Of (S, I);
      Ring : Boolean;
   begin
      if not L.Active or else RC.Owed (L.Ring) = 0 then
         return;
      end if;
      RC.Complete (L.Ring, Tag, Answer, Ring);
      if Ring then
         L.Ring_Owed := True;
      end if;
   end Complete;

   procedure Served (S : in out Server; I : Index) is
      Order : Schedule.State (S.Size) with Import, Address => S.Order_At;
      L : constant Link := Link_Of (S, I);
   begin
      Schedule.Served (Order, I, Still_Ready => L.Active and then RC.Can_Take (L.Ring));
      L.Needs_Arm := True;
   end Served;

   procedure Ring_Now (S : in out Server; I : Index) is
      L : constant Link := Link_Of (S, I);
   begin
      if L.Active and then L.Ring_Owed then
         L.Ring_Owed := False;
         Ring_Client (L.all, Sync => False);
      end if;
   end Ring_Now;

   function Arm (S : in out Server) return Boolean is
      Order : Schedule.State (S.Size) with Import, Address => S.Order_At;
      Quiet : Boolean := True;
   begin
      for I in 1 .. S.Size loop
         declare
            L : constant Link := Link_Of (S, I);
         begin
            if L.Active and then L.Needs_Arm then
               if RC.Arm_For_Requests (L.Ring) then
                  L.Needs_Arm := False;
               else
                  Schedule.Mark_Ready (Order, I);
                  Quiet := False;
               end if;
            end if;
         end;
      end loop;
      return Quiet;
   end Arm;

   procedure Sleep (S : in out Server; Deadline : CuBit.Deadlines.Deadline; Woke : out Wake) is
      Quiet : constant Boolean := Arm (S);
      Last : Natural := 0;
      Received : Boolean;
      Is_Wake : Boolean;
      Count : CuBit.Doorbells.Wake_Count;
   begin
      Woke := (others => <>);
      --  Every owed wake goes now (each to a client that declared itself
      --  asleep; a busy client costs nothing); the last is marked Sync when
      --  this thread is about to block, so that client runs here next.
      for I in 1 .. S.Size loop
         declare
            L : constant Link := Link_Of (S, I);
         begin
            if L.Active and then L.Ring_Owed then
               L.Ring_Owed := False;
               if Last /= 0 then
                  Ring_Client (Link_Of (S, Last).all, Sync => False);
               end if;
               Last := I;
            end if;
         end;
      end loop;
      if Last /= 0 then
         Ring_Client (Link_Of (S, Last).all, Sync => Quiet);
      end if;
      if not Quiet then
         Woke.Kind := Work_Waiting;
         return;
      end if;
      CuBit.Messages.receiveUntil (CuBit.Deadlines.Kernel_Word (Deadline), Woke.From, Woke.Msg, Received);
      if not Received then
         Woke.Kind := Deadline_Passed;
         return;
      end if;
      CuBit.Doorbells.Decode
        (Woke.Msg.tag.label, Woke.Msg.tag.length,
         [Woke.Msg.words (0), Woke.Msg.words (1), Woke.Msg.words (2), Woke.Msg.words (3)],
         Is_Wake, S.Wake_Bells, Count);
      if Is_Wake and then CuBit.Messages."=" (Woke.From, CuBit.Messages.No_Process) then
         S.Wake_Bell_Count := Count;
         S.Wake_Bells_Valid := True;
         Woke.Kind := Channels_Woken;
      else
         Woke.Kind := Message_Received;
      end if;
   end Sleep;

   procedure Handle_Event (S : in out Server; Event : CuBit.Control_Events.Event;
                           Ended : out Boolean; At_Index : out Index) is
   begin
      Ended := False;
      At_Index := Index'First;
      for I in 1 .. S.Size loop
         declare
            L : constant Link := Link_Of (S, I);
         begin
            if L.Active and then CuBit.Channels.Ended (L.Channel, Event) then
               declare
                  Limits : Admission.State (S.Size) with Import, Address => S.Limits_At;
                  Order : Schedule.State (S.Size) with Import, Address => S.Order_At;
               begin
                  CuBit.Channel_Bells.Unwatch (L.Channel.Peer_Grant);
                  RC.Detach_Server (L.Ring);
                  CuBit.Channels.Close (L.Channel);
                  Admission.Release (Limits, L.Offer, Client_Word (L.Client));
                  Schedule.Close (Order, I);
                  L.Active := False;
                  L.Client := CuBit.Messages.No_Process;
               end;
               Ended := True;
               At_Index := I;
               return;
            end if;
         end;
      end loop;
   end Handle_Event;

   function Is_Active (S : Server; I : Index) return Boolean is
     (I <= S.Size and then Link_Of (S, I).Active);
   function Client_Of (S : Server; I : Index) return CuBit.Messages.Process_ID is
     (Link_Of (S, I).Client);
   function Offer_Of (S : Server; I : Index) return Offer_Index is (Link_Of (S, I).Offer);
   function Owed (S : Server; I : Index) return Natural is (RC.Owed (Link_Of (S, I).Ring));
   function Refusals (S : Server) return Refusal_Counts is (S.Counts);

end CuBit.Channel_Servers;
