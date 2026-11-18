------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The windows' surface rings at Desktop (see the spec).
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Channel_Bells;
with CuBit.Channel_Limits;
with CuBit.Channel_Servers;
with CuBit.Process_IDs;
with Channel_Wake_ABI;
with Compositor_Surface_Ring;
with Compositor_Surface_Wire;
with Desktop_Surface_Ring_Service;

package body Desktop_Surface_Rings is

   use type CuBit.Messages.Process_ID;
   use type CuBit.Process_IDs.Process_ID;
   package SR renames Compositor_Surface_Ring;

   --  Every process holding Desktop's endpoint may open its windows'
   --  rings; the offer's limits bound them (Channels_Per_Client each).
   function Admit (Offer : Positive; From : CuBit.Messages.Process_ID; Badge : Unsigned_64) return Boolean is
     (Offer = 1 and then From /= CuBit.Messages.No_Process);
   function Can_Serve_Another (Offer : Positive) return Boolean is (Offer = 1);

   package Servers is new CuBit.Channel_Servers
     (RC => SR.RC, Offer_Count => 1, Admit => Admit, Can_Serve_Another => Can_Serve_Another);
   Rings : Servers.Server;
   Configured : Boolean := False;

   --  Requests taken from one ring per turn before the next ring's.
   Ring_Budget : constant := 16;

   function Is_Active (I : Servers.Index) return Boolean is (Servers.Is_Active (Rings, I));
   function Client_Of (I : Servers.Index) return CuBit.Process_IDs.Process_ID is
     (Servers.Client_Of (Rings, I));
   procedure Complete (I : Servers.Index; Tag : SR.RC.Token; Answer : Compositor_Surface_Wire.Result_Words) is
   begin
      Servers.Complete (Rings, I, Tag, Answer);
   end Complete;
   procedure End_Ring (I : Servers.Index) is
   begin
      Servers.End_Channel (Rings, I);
   end End_Ring;

   package Service is new Desktop_Surface_Ring_Service
     (Ring_Index => Servers.Index, Is_Active => Is_Active, Client_Of => Client_Of,
      Complete => Complete, End_Ring => End_Ring,
      Surface_Access => Surface_Access, Bounds => Bounds,
      Present_Attached => Present_Attached, Stage => Stage, Publish => Publish,
      Retirement => Retirement, Next_Input => Next_Input, Input_After => Input_After,
      Report => Report);

   procedure Configure (Ready : out Boolean) is
   begin
      Servers.Configure
        (Rings,
         [1 => (Element    => SR.Element,
                Per_Client => (Kind => CuBit.Channel_Limits.At_Most, Count => SR.Channels_Per_Client),
                Total      => (Kind => CuBit.Channel_Limits.Server_Limit),
                Service    => CuBit.Channel_Limits.Interactive)],
         Configured);
      if Configured then
         Service.Grow (Servers.Index (Servers.Size (Rings)), Configured);
      end if;
      Ready := Configured;
   end Configure;

   function Ready return Boolean is (Configured);

   procedure Handle_Open
     (From : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message;
      Is_Open : out Boolean; Answer : out CuBit.Messages.Message)
   is
      Opened, Tracked : Boolean;
      At_Index : Servers.Index;
   begin
      Is_Open := False;
      Answer := CuBit.Messages.NULL_MESSAGE;
      if not Configured then
         return;
      end if;
      Servers.Handle_Open (Rings, From, Request, Is_Open, Answer, Opened, At_Index);
      if Opened then
         --  The debts table follows the channel table's growth.
         Service.Grow (Servers.Index (Servers.Size (Rings)), Tracked);
         if not Service.Tracked (At_Index) then
            Report ("desktop: surface ring" & Servers.Index'Image (At_Index) &
                    " ended: no memory to track what it is owed");
            Servers.End_Channel (Rings, At_Index);
         else
            Service.Ring_Ended (At_Index);
         end if;
      end if;
   end Handle_Open;

   procedure Serve (Served : out Boolean) is
      Found, Got, Ended : Boolean;
      I : Servers.Index;
      Item : SR.RC.Queues.Submission;
   begin
      Served := False;
      if not Configured then
         return;
      end if;
      Servers.Look_All (Rings);
      for Turn in 1 .. Servers.Size (Rings) loop
         Servers.Next (Rings, Found, I);
         exit when not Found;
         Ended := False;
         for Taken in 1 .. Ring_Budget loop
            exit when not Service.Tracked (I);
            --  Desktop's own copy of the request (CuBit.Request_Channels
            --  reads the client's slot once).
            Servers.Take (Rings, I, Item, Got);
            exit when not Got;
            Served := True;
            Service.Serve (I, Item, Ended);
            exit when Ended;
         end loop;
         if not Ended then
            Servers.Served (Rings, I);
         end if;
      end loop;
   end Serve;

   procedure Input_Queued (Target : Unsigned_64) is
   begin
      if Configured then
         Service.Input_Queued (Target);
      end if;
   end Input_Queued;

   procedure Frame_Drawn is
   begin
      if Configured then
         Service.Frame_Drawn;
      end if;
   end Frame_Drawn;

   procedure Pump_Retirements is
   begin
      if Configured then
         Service.Pump_Retirements;
      end if;
   end Pump_Retirements;

   procedure Surface_Ended (Target : Unsigned_64) is
   begin
      if Configured then
         Service.Surface_Ended (Target);
      end if;
   end Surface_Ended;

   procedure Wake is
   begin
      if Configured then
         Servers.Wake_Owed (Rings);
      end if;
   end Wake;

   function Quiet return Boolean is
     (not Configured or else Servers.Arm (Rings));

   procedure Take_Notice
     (Notice : CuBit.Messages.Message; Publisher : CuBit.Messages.Process_ID;
      Kind : out Notice_Kind; Event : out CuBit.Control_Events.Event)
   is
      use type CuBit.Control_Events.Event_Kind;
      Ended, Is_Notice : Boolean;
      At_Index : Servers.Index;
   begin
      Kind := Not_Ours;
      Event := (Kind => CuBit.Control_Events.Not_Ours);
      if Publisher /= CuBit.Messages.No_Process or else not Configured then
         return;
      end if;
      if Notice.tag.label = Channel_Wake_ABI.Wake_Label then
         --  The rings are looked at every turn anyway (Serve).
         CuBit.Channel_Bells.Note_Notice
           (Notice.tag.label, Notice.tag.length,
            [Notice.words (0), Notice.words (1), Notice.words (2), Notice.words (3)], Is_Notice);
         Kind := Handled;
         return;
      end if;
      Event := CuBit.Control_Events.Decode
        (Notice.tag.label, Notice.tag.length, Notice.words (0), Notice.words (1), Notice.words (2));
      if Event.Kind not in CuBit.Control_Events.Grant_Revoked | CuBit.Control_Events.Grant_Returned then
         return;
      end if;
      Servers.Handle_Event (Rings, Event, Ended, At_Index);
      if Ended then
         Service.Ring_Ended (At_Index);
         Kind := Handled;
      else
         Kind := Grant_Event;
      end if;
   end Take_Notice;

   function Holds_Ring (Client : CuBit.Messages.Process_ID) return Boolean is
   begin
      if not Configured then
         return False;
      end if;
      for I in 1 .. Servers.Size (Rings) loop
         if Servers.Is_Active (Rings, Servers.Index (I))
           and then Servers.Client_Of (Rings, Servers.Index (I)) = Client
         then
            return True;
         end if;
      end loop;
      return False;
   end Holds_Ring;

   function Ended_For_Bad_Requests return Unsigned_64 is
      Total : Unsigned_64 := 0;
   begin
      for N of Service.Ended_Rings loop
         Total := (if N > Unsigned_64'Last - Total then Unsigned_64'Last else Total + N);
      end loop;
      return Total;
   end Ended_For_Bad_Requests;

   function Skipped_Input return Unsigned_64 is (Service.Skipped_Input);

end Desktop_Surface_Rings;
