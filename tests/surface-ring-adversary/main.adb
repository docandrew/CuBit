------------------------------------------------------------------------------
--  Hosted adversary test for Desktop's side of the surface rings (IPC-004,
--  Desktop_Surface_Ring_Service): the same code Desktop runs, against a
--  model Desktop, with hostile clients beside an honest one.
--
--  What it shows:
--  - every request is served from Desktop's own copy: a client rewriting
--    its ring (its whole page) after the request was taken changes nothing;
--  - damage is clamped to the bounds Desktop fixed for the surface, never
--    to anything a client wrote; a present wholly outside is answered and
--    draws nothing;
--  - composing reads pixels only inside Desktop's own geometry while the
--    client rewrites them (and plants a bigger "width" in its pixels);
--  - a malformed request, or a ring that breaks its declared limits, ends
--    that ring with a typed reason in Desktop's log, never Desktop's loop;
--  - a ring whose whole page is random bytes is taken from until it ends,
--    without one check failing;
--  - the honest window is served after every hostile act.
--  Built with -gnata -gnato: any index or range failure is a failure here.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Text_IO;
with Ada.Strings.Fixed;
with Interfaces; use Interfaces;
with System.Storage_Elements;
with CuBit.Desktop_Protocol;
with CuBit.Desktop_Protocol.Publication;
with CuBit.Grant_References;
with CuBit.Process_IDs;
with Compositor_Surface_Ring;
with Compositor_Surface_Wire;
with Desktop_Surface_Ring_Service;
with CuAlloc_Host;
pragma Unreferenced (CuAlloc_Host);

procedure Main is
   package DP renames CuBit.Desktop_Protocol;
   package Pub renames CuBit.Desktop_Protocol.Publication;
   package Wire renames Compositor_Surface_Wire;
   package SR renames Compositor_Surface_Ring;
   package RC renames SR.RC;
   use type DP.Status_Code, DP.Pixel_Coordinate, DP.Pixel_Extent, DP.Rectangle;
   use type Wire.Request_Kind;
   use type RC.Token;
   use type CuBit.Process_IDs.Process_ID;
   use type System.Storage_Elements.Storage_Element;

   Failures : Natural := 0;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Ada.Text_IO.Put_Line ("FAIL " & What);
         Failures := Failures + 1;
      end if;
   end Check;

   ---------------------------------------------------------------------------
   --  Rings: 1 honest; 2 .. 5 hostile, one hostile act each.
   ---------------------------------------------------------------------------
   type Ring_Index is range 1 .. 5;
   Honest : constant Ring_Index := 1;
   Rewriter : constant Ring_Index := 2;   --  rewrites its page after a take; bad damage
   Over_Limit : constant Ring_Index := 3; --  more frames in flight than allowed
   Twice : constant Ring_Index := 4;      --  a second input request
   Noise : constant Ring_Index := 5;      --  its whole page is random bytes

   type Region is array (1 .. 8 * RC.Page_Bytes) of System.Storage_Elements.Storage_Element
     with Alignment => 4096;
   type Region_Access is access Region;
   Client_Pages, Server_Pages : array (Ring_Index) of Region_Access :=
     [others => null];
   Clients : array (Ring_Index) of RC.Client;
   Servers : array (Ring_Index) of RC.Server;
   Active : array (Ring_Index) of Boolean := [others => True];
   Ended_Count : array (Ring_Index) of Natural := [others => 0];

   function Pid (I : Ring_Index) return CuBit.Process_IDs.Process_ID is
     (CuBit.Process_IDs.To_Process_ID (Unsigned_64 (I) + 10, 1));

   --  Surfaces: each ring's client owns the one named like its ring.
   --  Geometry Desktop fixed at attach: Width x Height, 4 bytes a pixel.
   Width : constant := 64;
   Height : constant := 48;
   type Pixel_Array is array (0 .. Width * Height - 1) of Unsigned_32;
   Pixels : array (Ring_Index) of Pixel_Array := [others => [others => 0]];
   Composed_Pixels : Natural := 0;
   Presents_Seen : Natural := 0;
   Publishes_Seen : Natural := 0;
   Last_Report : String (1 .. 200) := [others => ' '];
   Last_Report_Length : Natural := 0;

   --  A deterministic noise source (xorshift).
   Seed : Unsigned_64 := 16#9E37_79B9_7F4A_7C15#;
   function Next_Noise return Unsigned_64 is
   begin
      Seed := Seed xor Shift_Left (Seed, 13);
      Seed := Seed xor Shift_Right (Seed, 7);
      Seed := Seed xor Shift_Left (Seed, 17);
      return Seed;
   end Next_Noise;

   procedure Scribble (Pages : Region_Access; From : Positive := 1) is
   begin
      for K in From .. Pages'Last loop
         Pages (K) := System.Storage_Elements.Storage_Element (Next_Noise and 16#FF#);
      end loop;
   end Scribble;

   ---------------------------------------------------------------------------
   --  The model Desktop (the service's formals).
   ---------------------------------------------------------------------------
   function Is_Active (I : Ring_Index) return Boolean is (Active (I));
   function Client_Of (I : Ring_Index) return CuBit.Process_IDs.Process_ID is (Pid (I));
   procedure Complete (I : Ring_Index; Tag : RC.Token; Answer : Wire.Result_Words) is
      Ring_Peer : Boolean;
   begin
      Check (Active (I), "answers go only to open rings");
      RC.Complete (Servers (I), Tag, Answer, Ring_Peer);
   end Complete;
   --  Unrequested input noted per ring (Compositor_Surface_Ring's word).
   Unrequested : array (Ring_Index) of Natural := [others => 0];
   procedure Note_Unrequested_Input (I : Ring_Index) is
   begin
      Unrequested (I) := Unrequested (I) + 1;
   end Note_Unrequested_Input;

   procedure End_Ring (I : Ring_Index) is
   begin
      Active (I) := False;
      Ended_Count (I) := Ended_Count (I) + 1;
   end End_Ring;

   function Owner (Target : Unsigned_64) return Ring_Index'Base is
     (if Target in 1 .. Unsigned_64 (Ring_Index'Last) then Ring_Index'Base (Target) else 0);

   function Surface_Access (From : CuBit.Process_IDs.Process_ID; Target : Unsigned_64)
     return DP.Status_Code is
     (if Owner (Target) = 0 then DP.Bad_Object
      elsif Pid (Owner (Target)) = From then DP.Success
      else DP.Denied);

   procedure Bounds
     (Target : Unsigned_64; Attached_Width, Attached_Height : out Natural;
      Source_Width, Source_Height : out Natural) is
   begin
      Attached_Width := Width; Attached_Height := Height;
      Source_Width := Width; Source_Height := Height;
      if Owner (Target) = 0 then
         Attached_Width := 0; Attached_Height := 0; Source_Width := 0; Source_Height := 0;
      end if;
   end Bounds;

   function Inside (Area : DP.Rectangle) return Boolean is
     ((Area = (0, 0, 0, 0)) or else
      (Area.Width > 0 and then Area.Height > 0 and then
       Natural (Area.X) + Natural (Area.Width) <= Width and then
       Natural (Area.Y) + Natural (Area.Height) <= Height));

   --  Compose the damaged rows: read each pixel while the client rewrites
   --  its buffer, and plants a "width" ten times larger in the first word.
   procedure Compose (Target : Ring_Index; Area : DP.Rectangle) is
      X0 : constant Natural := Natural (Area.X);
      Y0 : constant Natural := Natural (Area.Y);
      W : constant Natural := (if Area.Width = 0 then Width else Natural (Area.Width));
      H : constant Natural := (if Area.Width = 0 then Height else Natural (Area.Height));
      Sum : Unsigned_32 := 0;
   begin
      for Y in Y0 .. Y0 + H - 1 loop
         Pixels (Target) (0) := Unsigned_32 (Width * 10);
         for X in X0 .. X0 + W - 1 loop
            Pixels (Target) ((Y * Width + X + 7) mod Pixels (Target)'Length) :=
              Unsigned_32 (Next_Noise and 16#FFFF_FFFF#);
            --  Desktop's geometry indexes; the client's words never do.
            Sum := Sum xor Pixels (Target) (Y * Width + X);
            Composed_Pixels := Composed_Pixels + 1;
         end loop;
      end loop;
      pragma Unreferenced (Sum);
   end Compose;

   procedure Present_Attached
     (From : CuBit.Process_IDs.Process_ID; Value : DP.Present_Request; Status : out DP.Status_Code) is
   begin
      Presents_Seen := Presents_Seen + 1;
      Check (Surface_Access (From, Unsigned_64 (Value.Surface)) = DP.Success, "present only for its owner");
      Check (Inside (Value.Area), "present damage inside the surface");
      if Inside (Value.Area) and then Owner (Unsigned_64 (Value.Surface)) /= 0 then
         Compose (Owner (Unsigned_64 (Value.Surface)), Value.Area);
      end if;
      Status := DP.Success;
   end Present_Attached;

   Next_Ticket : Pub.Identity := 1;
   procedure Stage (From : CuBit.Process_IDs.Process_ID; Value : Pub.Stage_Request; Result : out Pub.Receipt) is
   begin
      Check (Surface_Access (From, Unsigned_64 (Value.Surface)) = DP.Success, "stage only for its owner");
      Next_Ticket := (if Next_Ticket = Pub.Identity'Last then 1 else Next_Ticket + 1);
      Result := (DP.Success, Value.Epoch, Next_Ticket);
   end Stage;

   procedure Publish (From : CuBit.Process_IDs.Process_ID; Value : Pub.Publish_Request; Result : out Pub.Receipt) is
   begin
      Publishes_Seen := Publishes_Seen + 1;
      Check (Surface_Access (From, Unsigned_64 (Value.Surface)) = DP.Success, "publish only for its owner");
      Check (Inside (Value.Area), "publish damage inside the buffer");
      Result := (DP.Success, Value.Epoch, Value.Ticket);
   end Publish;

   --  Each ticket is read once more before it retires.
   Retire_Asked : array (Ring_Index) of Natural := [others => 0];
   procedure Retirement (From : CuBit.Process_IDs.Process_ID; Value : Pub.Query; Result : out Pub.Receipt) is
      O : constant Ring_Index'Base := Owner (Unsigned_64 (Value.Surface));
   begin
      if O = 0 or else Pid (O) /= From or else Value.Ticket = 0 or else Value.Ticket > Pub.Identity'Last then
         Result := (Status => DP.Bad_Object);
         return;
      end if;
      Retire_Asked (O) := Retire_Asked (O) + 1;
      Result := (if Retire_Asked (O) mod 2 = 1 then (Status => DP.Bad_State)
                 else (DP.Success, 1, Value.Ticket));
   end Retirement;

   --  Input queued for each surface, oldest first (serials increase).
   type Queue is array (1 .. 16) of DP.Input_Envelope;
   Queues : array (Ring_Index) of Queue := [others => [others => (others => <>)]];
   Queued : array (Ring_Index) of Natural := [others => 0];
   Serial : Unsigned_64 := 0;
   procedure Push_Key (Target : Ring_Index) is
   begin
      Serial := Serial + 1;
      Queued (Target) := Queued (Target) + 1;
      Queues (Target) (Queued (Target)) := (DP.Key_Pressed, Serial, 16#1E#, 0, False);
   end Push_Key;

   procedure Next_Input
     (Target, After : Unsigned_64; Item : out DP.Input_Envelope; Got : out Boolean)
   is
      O : constant Ring_Index'Base := Owner (Target);
   begin
      Item := (others => <>);
      Got := False;
      if O = 0 then
         return;
      end if;
      for K in 1 .. Queued (O) loop
         if Queues (O) (K).Serial > After then
            Item := Queues (O) (K);
            Got := True;
            Queues (O) (K .. Queued (O) - 1) := Queues (O) (K + 1 .. Queued (O));
            Queued (O) := Queued (O) - 1;
            return;
         end if;
      end loop;
   end Next_Input;

   function Input_After (Target, After : Unsigned_64) return Boolean is
     (Owner (Target) /= 0 and then
      (for some K in 1 .. Queued (Owner (Target)) => Queues (Owner (Target)) (K).Serial > After));

   procedure Report (Text : String) is
      N : constant Natural := Natural'Min (Text'Length, Last_Report'Length);
   begin
      Ada.Text_IO.Put_Line ("  log: " & Text);
      Last_Report (1 .. N) := Text (Text'First .. Text'First + N - 1);
      Last_Report_Length := N;
   end Report;

   function Reported (Part : String) return Boolean is
     (Ada.Strings.Fixed.Index (Last_Report (1 .. Last_Report_Length), Part) > 0);

   package Service is new Desktop_Surface_Ring_Service
     (Ring_Index => Ring_Index, Is_Active => Is_Active, Client_Of => Client_Of,
      Complete => Complete, End_Ring => End_Ring, Note_Unrequested_Input => Note_Unrequested_Input,
      Surface_Access => Surface_Access,
      Bounds => Bounds, Present_Attached => Present_Attached, Stage => Stage,
      Publish => Publish, Retirement => Retirement, Next_Input => Next_Input,
      Input_After => Input_After, Report => Report);

   ---------------------------------------------------------------------------
   --  Client and Desktop steps.
   ---------------------------------------------------------------------------
   Grant : constant CuBit.Grant_References.Reference := (slot => 7, generation => 3);
   Tags : array (Ring_Index) of RC.Token := [others => 0];

   procedure Submit_Words (I : Ring_Index; Words : Wire.Request_Words) is
      Submitted, Ring : Boolean;
   begin
      Tags (I) := Tags (I) + 1;
      RC.Submit (Clients (I), Tags (I), Words, Submitted, Ring);
      Check (Submitted, "ring" & I'Image & " request queued");
   end Submit_Words;

   procedure Submit (I : Ring_Index; Item : Wire.Request) is
   begin
      Submit_Words (I, Wire.Encode (Item));
   end Submit;

   --  Desktop's turn for ring I: take and serve up to Budget requests.
   procedure Serve (I : Ring_Index; Budget : Positive := 16) is
      Item : RC.Queues.Submission;
      Got, Ring, Ended : Boolean;
   begin
      for N in 1 .. Budget loop
         exit when not Active (I);
         RC.Take (Servers (I), Item, Got, Ring);
         exit when not Got;
         Service.Serve (I, Item, Ended);
         exit when Ended;
      end loop;
   end Serve;

   --  The answer the honest window reaps next, decoded as Kind.
   function Reap (I : Ring_Index; Kind : Wire.Request_Kind; Status : DP.Status_Code := DP.Success)
     return Boolean
   is
      Answer : RC.Queues.Completion;
      Got : Boolean;
   begin
      RC.Reap (Clients (I), Answer, Got);
      if not Got then
         return False;
      end if;
      declare
         D : constant Wire.Result_Decoding := Wire.Decode (Answer.Answer, Kind);
      begin
         return D.Valid and then D.Value.Status = Status;
      end;
   end Reap;

   function Nothing_To_Reap (I : Ring_Index) return Boolean is
      Answer : RC.Queues.Completion;
      Got : Boolean;
   begin
      RC.Reap (Clients (I), Answer, Got);
      return not Got;
   end Nothing_To_Reap;

   --  The honest window does a whole frame and input round, served.
   Rounds : Natural := 0;
   procedure Honest_Round is
      Before : constant Natural := Composed_Pixels;
   begin
      Rounds := Rounds + 1;
      Check (Active (Honest), "the honest ring stays open");
      Submit (Honest, (Kind => Wire.Present, Surface => 1, Area => (2, 3, 10, 5), Input_After => 0));
      Submit (Honest, (Kind => Wire.Receive_Input, Surface => 1, After => Serial));
      Serve (Honest);
      Check (Composed_Pixels = Before + 50, "honest present composed");
      Service.Frame_Drawn;
      Check (Reap (Honest, Wire.Present), "honest present answered");
      Push_Key (Honest);
      Service.Input_Queued (1);
      Check (Reap (Honest, Wire.Receive_Input), "honest input answered");
      Submit (Honest, (Kind => Wire.Publish, Surface => 1, Area => (0, 0, 4, 4), Input_After => Serial,
                       Epoch => 1, Grant => Grant));
      Submit (Honest, (Kind => Wire.Await_Retirement, Surface => 1, Retiring_Epoch => 1, Ticket => 5));
      Serve (Honest);
      Check (Reap (Honest, Wire.Publish), "honest publish answered");
      Service.Pump_Retirements;
      Check (Reap (Honest, Wire.Await_Retirement), "honest retirement answered");
      Check (Service.Owed (Honest) = 0, "nothing left owed the honest ring");
   end Honest_Round;

   Grown : Boolean;
begin
   for I in Ring_Index loop
      Client_Pages (I) := new Region'(others => 0);
      Server_Pages (I) := new Region'(others => 0);
      RC.Attach_Client (Clients (I), Client_Pages (I).all'Address, Server_Pages (I).all'Address);
      RC.Attach_Server (Servers (I), Server_Pages (I).all'Address, Client_Pages (I).all'Address);
   end loop;
   Service.Grow (Ring_Index'Last, Grown);
   Check (Grown and then Service.Tracked (Ring_Index'Last), "debts tracked for every ring");

   Honest_Round;

   --  Rewriter: takes are copies. A valid present is taken, then the client
   --  overwrites its whole page; Desktop serves the copy as it was.
   Submit (Rewriter, (Kind => Wire.Present, Surface => 2, Area => (1, 1, 4, 4), Input_After => 0));
   declare
      Item : RC.Queues.Submission;
      Got, Ring, Ended : Boolean;
      Before : constant Natural := Composed_Pixels;
   begin
      RC.Take (Servers (Rewriter), Item, Got, Ring);
      Check (Got, "rewriter's present taken");
      Scribble (Client_Pages (Rewriter));
      Service.Serve (Rewriter, Item, Ended);
      Check (not Ended and then Composed_Pixels = Before + 16, "the copy is served as taken");
   end;
   --  Its page is noise now: reattach it as a fresh ring for the next acts.
   Client_Pages (Rewriter).all := [others => 0];
   Server_Pages (Rewriter).all := [others => 0];
   RC.Attach_Client (Clients (Rewriter), Client_Pages (Rewriter).all'Address, Server_Pages (Rewriter).all'Address);
   RC.Attach_Server (Servers (Rewriter), Server_Pages (Rewriter).all'Address, Client_Pages (Rewriter).all'Address);
   Service.Ring_Ended (Rewriter);
   Honest_Round;

   --  Damage past the surface is clamped to Desktop's bounds; damage wholly
   --  outside draws nothing and is answered at once.
   declare
      Before_Presents : constant Natural := Presents_Seen;
   begin
      Submit (Rewriter, (Kind => Wire.Present, Surface => 2, Area => (60, 40, 65_535, 65_535), Input_After => 0));
      Serve (Rewriter);
      Check (Presents_Seen = Before_Presents + 1, "overhanging damage presented, clamped");
      Service.Frame_Drawn;
      Check (Reap (Rewriter, Wire.Present), "clamped present answered");
      Submit (Rewriter, (Kind => Wire.Present, Surface => 2, Area => (1_000, 1_000, 5, 5), Input_After => 0));
      Serve (Rewriter);
      Check (Presents_Seen = Before_Presents + 1, "damage wholly outside draws nothing");
      Check (Reap (Rewriter, Wire.Present), "outside present answered at once");
      Submit (Rewriter, (Kind => Wire.Publish, Surface => 2, Area => (63, 47, 40_000, 40_000), Input_After => 0,
                         Epoch => 1, Grant => Grant));
      Serve (Rewriter);
      Check (Reap (Rewriter, Wire.Publish), "overhanging publish clamped and answered");
   end;
   --  Another window's surface: refused, the ring stays.
   Submit (Rewriter, (Kind => Wire.Present, Surface => 1, Area => (0, 0, 0, 0), Input_After => 0));
   Submit (Rewriter, (Kind => Wire.Receive_Input, Surface => 1, After => 0));
   Serve (Rewriter);
   Check (Reap (Rewriter, Wire.Present, DP.Denied), "another window's present denied");
   Check (Reap (Rewriter, Wire.Receive_Input, DP.Denied), "another window's input denied");
   Check (Active (Rewriter), "a denied request does not end the ring");
   --  A surface that does not exist.
   Submit (Rewriter, (Kind => Wire.Present, Surface => 99, Area => (0, 0, 0, 0), Input_After => 0));
   Serve (Rewriter);
   Check (Reap (Rewriter, Wire.Present, DP.Bad_Object), "unknown surface answered Bad_Object");
   --  A malformed request ends the ring, with its reason.
   Submit_Words (Rewriter, (Kind => 99, Surface => 2, A => 1, B => 2, C => 3, D => 4));
   Submit (Rewriter, (Kind => Wire.Present, Surface => 2, Area => (0, 0, 0, 0), Input_After => 0));
   Serve (Rewriter);
   Check (not Active (Rewriter) and then Ended_Count (Rewriter) = 1, "malformed request ends the ring once");
   Check (Reported ("malformed request"), "the log says the request was malformed");
   Check (Nothing_To_Reap (Rewriter), "an ended ring is answered nothing more");
   Check (Service.Owed (Rewriter) = 0, "an ended ring is owed nothing");
   Honest_Round;

   --  Over_Limit: three presents in flight where two are allowed.
   for N in 1 .. 3 loop
      Submit (Over_Limit, (Kind => Wire.Present, Surface => 3, Area => (0, 0, 2, 2), Input_After => 0));
   end loop;
   Serve (Over_Limit);
   Check (not Active (Over_Limit), "a third frame in flight ends the ring");
   Check (Reported ("more frames in flight"), "the log names the frame limit");
   Service.Frame_Drawn;
   Check (Nothing_To_Reap (Over_Limit), "its owed presents are dropped");
   Honest_Round;

   --  Input queued while the window has no input request out: the owner's
   --  ring is told (its next request wakes Desktop), no other ring is, and
   --  the next request is answered at once. With a request out, input
   --  answers it and nothing is told.
   declare
      Told : constant Natural := Unrequested (Honest);
   begin
      Push_Key (Honest);
      Service.Input_Queued (1);
      Check (Unrequested (Honest) = Told + 1, "unrequested input is told to the owner's ring");
      Check ((for all R in Ring_Index => R = Honest or else Unrequested (R) = 0),
             "no other client's ring is told");
      Submit (Honest, (Kind => Wire.Receive_Input, Surface => 1, After => Serial - 1));
      Serve (Honest);
      Check (Reap (Honest, Wire.Receive_Input), "the next request takes the queued input at once");
      Submit (Honest, (Kind => Wire.Receive_Input, Surface => 1, After => Serial));
      Serve (Honest);
      Push_Key (Honest);
      Service.Input_Queued (1);
      Check (Unrequested (Honest) = Told + 1, "input a request takes is not told");
      Check (Reap (Honest, Wire.Receive_Input), "and answers that request");
   end;

   --  Twice: a second input request while one is owed.
   Submit (Twice, (Kind => Wire.Receive_Input, Surface => 4, After => Serial));
   Submit (Twice, (Kind => Wire.Receive_Input, Surface => 4, After => Serial));
   Serve (Twice);
   Check (not Active (Twice), "a second input request ends the ring");
   Check (Reported ("second input request"), "the log names the input limit");
   Push_Key (Twice);
   Service.Input_Queued (4);
   Check (Nothing_To_Reap (Twice), "no input answer to an ended ring");
   Honest_Round;

   --  Noise: the client's page is random bytes before every take. Desktop
   --  takes what the ring offers and serves or ends; no check fails, and
   --  the honest ring is served between rounds.
   for Trial in 1 .. 200 loop
      if not Active (Noise) then
         Client_Pages (Noise).all := [others => 0];
         Server_Pages (Noise).all := [others => 0];
         RC.Attach_Client (Clients (Noise), Client_Pages (Noise).all'Address, Server_Pages (Noise).all'Address);
         RC.Attach_Server (Servers (Noise), Server_Pages (Noise).all'Address, Client_Pages (Noise).all'Address);
         Service.Ring_Ended (Noise);
         Active (Noise) := True;
      end if;
      if Trial mod 2 = 1 then
         --  The whole page, counters included.
         Scribble (Client_Pages (Noise));
      else
         --  Eight requests queued properly, then their slots rewritten.
         for N in 1 .. 8 loop
            Submit (Noise, (Kind => Wire.Present, Surface => 5, Area => (0, 0, 1, 1), Input_After => 0));
         end loop;
         Scribble (Client_Pages (Noise), From => RC.Ring_At + 1);
      end if;
      Serve (Noise, Budget => 64);
      Service.Frame_Drawn;
      Service.Pump_Retirements;
      if Trial mod 20 = 0 then
         Honest_Round;
      end if;
   end loop;
   Check (Ended_Count (Noise) > 0, "random pages end the ring");
   Ada.Text_IO.Put_Line ("  noise ring ended" & Ended_Count (Noise)'Image & " times in 200 rounds");
   Honest_Round;

   Check (Rounds >= 15 and then Service.Owed (Honest) = 0, "the honest window was served throughout");
   if Failures = 0 then
      Ada.Text_IO.Put_Line ("SURFACE-RING-ADVERSARY: PASS checks=" & Checks'Image);
   else
      Ada.Text_IO.Put_Line ("SURFACE-RING-ADVERSARY: FAIL" & Failures'Image & " of" & Checks'Image);
   end if;
end Main;
