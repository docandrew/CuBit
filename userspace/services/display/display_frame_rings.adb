------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Display's frame rings (see the spec).
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Channel_Bells;
with CuBit.Control_Events;
with CuBit.Channel_Limits;
with CuBit.Channel_Servers;
with Display_Logs;
with CuBit.Monotonic;
with Channel_Wake_ABI;

package body Display_Frame_Rings is

   use type CuBit.Messages.Process_ID;

   --  Every holder of Display's endpoint may open a ring; its frames are
   --  checked against what the sender holds, frame by frame.
   function Admit (Offer : Positive; From : CuBit.Messages.Process_ID; Badge : Unsigned_64) return Boolean is
      pragma Unreferenced (Badge);
   begin
      return Offer = 1 and then From /= CuBit.Messages.No_Process;
   end Admit;
   function Can_Serve_Another (Offer : Positive) return Boolean is (Offer = 1);

   package Servers is new CuBit.Channel_Servers
     (RC => FR.RC, Offer_Count => 1, Admit => Admit, Can_Serve_Another => Can_Serve_Another);
   Rings : Servers.Server;
   Configured : Boolean := False;

   --  Presents taken from one ring before the next ring's turn.
   Ring_Budget : constant := FR.Frames_In_Flight;

   --  The serving pass Next_Present walks: Look_All, then each ready ring.
   Pass_Started : Boolean := False;
   Turns        : Natural := 0;
   In_Ring      : Boolean := False;
   Current      : Servers.Index := Servers.Index'First;
   Taken        : Natural := 0;

   procedure Say (Text : String) is
   begin
      Display_Logs.Write (Text & ASCII.LF);
   end Say;

   function Image (Value : Natural) return String is
      Text : constant String := Natural'Image (Value);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Image;

   procedure Configure (Ready : out Boolean) is
   begin
      Servers.Configure
        (Rings,
         [1 => (Element    => FR.Element,
                Per_Client => (Kind => CuBit.Channel_Limits.At_Most, Count => FR.Channels_Per_Client),
                Total      => (Kind => CuBit.Channel_Limits.Server_Limit),
                Service    => CuBit.Channel_Limits.Interactive)],
         Configured);
      Ready := Configured;
   end Configure;

   procedure Handle_Open
     (From : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message;
      Is_Open : out Boolean; Answer : out CuBit.Messages.Message)
   is
      Opened : Boolean;
      At_Index : Servers.Index;
   begin
      Is_Open := False;
      Answer := CuBit.Messages.NULL_MESSAGE;
      if not Configured then
         return;
      end if;
      Servers.Handle_Open (Rings, From, Request, Is_Open, Answer, Opened, At_Index);
      if Is_Open then
         if Opened then
            Say ("display: frame ring " & Image (At_Index) & " opened (process" &
                            Unsigned_64'Image (CuBit.Messages.To_Word (From)) & ")");
         else
            Say ("display: frame ring refused (process" &
                               Unsigned_64'Image (CuBit.Messages.To_Word (From)) & ")");
         end if;
      end if;
   end Handle_Open;

   procedure Next_Present
     (Got : out Boolean; Ring : out Ring_Number; From : out CuBit.Messages.Process_ID;
      Tag : out FR.RC.Token; Item : out FR.Present; Valid : out Boolean)
   is
      Found, Got_One : Boolean;
      I : Servers.Index;
      Taken_Item : FR.RC.Queues.Submission;
   begin
      Got := False;
      Ring := No_Ring;
      From := CuBit.Messages.No_Process;
      Tag := 0;
      Item := (others => <>);
      Valid := False;
      if not Configured then
         return;
      end if;
      if not Pass_Started then
         Servers.Look_All (Rings);
         Pass_Started := True;
         Turns := 0;
         In_Ring := False;
      end if;
      loop
         if not In_Ring then
            exit when Turns >= Servers.Size (Rings);
            Servers.Next (Rings, Found, I);
            exit when not Found;
            Turns := Turns + 1;
            Current := I;
            In_Ring := True;
            Taken := 0;
         end if;
         if Taken < Ring_Budget then
            --  Display's own copy of the present.
            Servers.Take (Rings, Current, Taken_Item, Got_One);
            if Got_One then
               Taken := Taken + 1;
               declare
                  Decoded : constant FR.Present_Decoding := FR.Decode (Taken_Item.Item);
               begin
                  if Decoded.Valid then
                     Item := Decoded.Value;
                     Valid := True;
                  end if;
               end;
               Got := True;
               Ring := Ring_Number (Current);
               From := Servers.Client_Of (Rings, Current);
               Tag := Taken_Item.Tag;
               return;
            end if;
         end if;
         Servers.Served (Rings, Current);
         In_Ring := False;
      end loop;
      Pass_Started := False;
   end Next_Present;

   procedure Answer
     (Ring : Ring_Number; Tag : FR.RC.Token; Result : CuBit.Desktop_Protocol.Wire_Message;
      Taken_Us : Unsigned_64) is
   begin
      if not Configured or else Ring = No_Ring or else Natural (Ring) > Servers.Size (Rings) then
         return;
      end if;
      declare
         I : constant Servers.Index := Servers.Index (Ring);
      begin
         if not Servers.Is_Active (Rings, I) or else Servers.Owed (Rings, I) = 0 then
            return;
         end if;
         Servers.Complete
           (Rings, I, Tag,
            FR.Encode ((Result => Result, Taken_Us => Taken_Us,
                        Answered_Us => CuBit.Monotonic.Microseconds)));
         --  Desktop waits on this frame for its next one: ring now, not at
         --  the end of Display's turn (the one wake point for IPC-010).
         Servers.Ring_Now (Rings, I);
      end;
   end Answer;

   function Quiet return Boolean is
   begin
      if not Configured then
         return True;
      end if;
      Servers.Wake_Owed (Rings);
      return Servers.Arm (Rings);
   end Quiet;

   procedure Take_Notice
     (Notice : CuBit.Messages.Message; Handled : out Boolean; Ended : out Ring_Number)
   is
      use type CuBit.Control_Events.Event_Kind;
      Event : CuBit.Control_Events.Event;
      Is_Notice, Ended_One : Boolean;
      At_Index : Servers.Index;
   begin
      Handled := False;
      Ended := No_Ring;
      if not Configured then
         return;
      end if;
      if Notice.tag.label = Channel_Wake_ABI.Wake_Label then
         --  The rings are looked at every turn anyway (Next_Present).
         CuBit.Channel_Bells.Note_Notice
           (Notice.tag.label, Notice.tag.length,
            [Notice.words (0), Notice.words (1), Notice.words (2), Notice.words (3)], Is_Notice);
         Handled := True;
         return;
      end if;
      Event := CuBit.Control_Events.Decode
        (Notice.tag.label, Notice.tag.length, Notice.words (0), Notice.words (1), Notice.words (2));
      if Event.Kind not in CuBit.Control_Events.Grant_Revoked | CuBit.Control_Events.Grant_Returned then
         return;
      end if;
      Servers.Handle_Event (Rings, Event, Ended_One, At_Index);
      Handled := Ended_One;
      if Ended_One then
         Ended := Ring_Number (At_Index);
         --  A serving pass must not go on in a ring that is gone.
         if In_Ring and then Current = At_Index then
            In_Ring := False;
         end if;
         Say ("display: frame ring " & Image (At_Index) & " ended");
      end if;
   end Take_Notice;

   function Open_Rings return Natural is
      Count : Natural := 0;
   begin
      if Configured then
         for I in 1 .. Servers.Size (Rings) loop
            if Servers.Is_Active (Rings, Servers.Index (I)) then
               Count := Count + 1;
            end if;
         end loop;
      end if;
      return Count;
   end Open_Rings;

end Display_Frame_Rings;
