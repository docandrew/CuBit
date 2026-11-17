------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  Hosted tests for Desktop's frame ring to Display (IPC-009, second
--  half): Display_Frame_Client over a real CuBit.Request_Channels channel
--  in two memory regions, with this test standing in for Display's side.
--    1. Every present is answered, or fails typed when the channel ends.
--    2. A superseded frame is answered (and Desktop's presentation state
--       takes it as a drop, not a fault).
--    3. A forged answer (unknown tag, or another frame's) is refused and
--       counted; the present stays outstanding for its real answer.
--    4. A full ring is back pressure: the next present is refused, never
--       dropped, and fits once an answer was taken.
--    5. The entry's words: round trips, a header with its unused bits set
--       is refused, the damage boxes travel.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;

with CuBit.Display_Frame_Damage;
with CuBit.Display_Pool_Protocol;
with CuBit.Display_Protocol;
with Compositor_Presentation;
with Display_Frame_Client;
with Display_Frame_Ring;

procedure Main is
   package FR renames Display_Frame_Ring;
   package FC renames Display_Frame_Client;
   package PW renames CuBit.Display_Pool_Protocol;
   package DSP renames CuBit.Display_Protocol;
   package FD renames CuBit.Display_Frame_Damage;
   package CP renames Compositor_Presentation;
   use type FR.RC.Token;
   use type FR.Present;
   use type FC.Submit_Result;
   use type DSP.Frame_Outcome;
   use type DSP.Buffer_Disposition;

   Checks : Natural := 0;
   Failures : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Ada.Text_IO.Put_Line ("FRAME-RINGS: FAIL " & What);
      end if;
   end Check;

   Page : constant := 4_096;
   type Region is array (1 .. 8 * Page) of Unsigned_8 with Alignment => Page;
   Client_Memory, Server_Memory : Region;

   Session : constant Unsigned_64 := 5;

   function Frame_Present (Frame : Unsigned_64) return FR.Present is
      Damage : FD.Damage;
   begin
      Damage.Count := 1;
      Damage.Boxes (1) := (10, 20, 30, 40);
      return (Frame => PW.Encode (PW.Frame'(Buffer => 1,
                                  Request => (Session, Frame, (0, 0, 100, 100)))),
              Damage => FD.Encode (Frame, Damage), Submitted_Us => 1_000 + Frame);
   end Frame_Present;

   function Result_For
     (Frame : Unsigned_64; Outcome : DSP.Frame_Outcome; Released : Boolean := True) return FR.Answer_Words is
     (FR.Encode
        ((Result => PW.Encode (PW.Completion'(Buffer => 1, Result =>
            (Session => Session, Frame => Frame, Outcome => Outcome,
             Buffer_State => (if Released then DSP.Released else DSP.Still_Held), others => <>))),
          Taken_Us => 7, Answered_Us => 9)));

   Client : FC.Client;
   Server : FR.RC.Server;

   procedure Fresh is
   begin
      Client_Memory := [others => 0];
      Server_Memory := [others => 0];
      FC.Detach (Client);
      declare
         Token : Unsigned_64;
         Got : Boolean;
      begin
         loop
            FC.Fail_Next (Client, Token, Got);
            exit when not Got;
         end loop;
      end;
      FC.Attach (Client, Client_Memory'Address, Server_Memory'Address);
      FR.RC.Attach_Server (Server, Server_Memory'Address, Client_Memory'Address);
   end Fresh;

   procedure Submit (Frame : Unsigned_64; Result : out FC.Submit_Result) is
      Ring : Boolean;
   begin
      FC.Submit (Client, Frame_Present (Frame), Token => 100 + Frame, Frame => Frame,
                 Result => Result, Wake_Display => Ring);
   end Submit;

   --  Display's side: take the next present (its copy), returning its tag
   --  and the frame its words carry.
   procedure Take (Tag : out FR.RC.Token; Frame : out Unsigned_64; Got : out Boolean) is
      Item : FR.RC.Queues.Submission;
      Ring : Boolean;
   begin
      Frame := 0;
      Tag := 0;
      FR.RC.Take (Server, Item, Got, Ring);
      if Got then
         Tag := Item.Tag;
         declare
            P : constant FR.Present_Decoding := FR.Decode (Item.Item);
            F : constant PW.Frame_Decoding :=
              (if P.Valid then PW.Decode_Frame (P.Value.Frame) else (Valid => False));
         begin
            if F.Valid then
               Frame := F.Value.Request.Frame;
            end if;
         end;
      end if;
   end Take;

   procedure Answer (Tag : FR.RC.Token; Words : FR.Answer_Words) is
      Ring : Boolean;
   begin
      FR.RC.Complete (Server, Tag, Words, Ring);
   end Answer;

   R : FC.Submit_Result;
   Item : FC.Reaped;
   Got : Boolean;
   Tag : FR.RC.Token;
   Frame : Unsigned_64;
begin
   ---------------------------------------------------------------------------
   --  5. The words.
   ---------------------------------------------------------------------------
   declare
      P : constant FR.Present := Frame_Present (3);
      W : FR.Present_Words := FR.Encode (P);
      D : constant FR.Present_Decoding := FR.Decode (W);
   begin
      Check (D.Valid and then D.Value = P, "a present round-trips");
      Check (FD.Decode (D.Value.Damage, 3, (0, 0, 100, 100)).Valid, "its damage boxes decode");
      Check (not FD.Decode (D.Value.Damage, 4, (0, 0, 100, 100)).Valid,
             "boxes recorded for another frame are refused");
      W (FR.Frame_Header_Word) := W (FR.Frame_Header_Word) or 16#0000_1000_0000_0000#;
      Check (not FR.Decode (W).Valid, "a header with unused bits set is refused");
      declare
         A : FR.Answer_Words := Result_For (3, DSP.Superseded);
      begin
         Check (FR.Decode (A).Valid, "an answer decodes");
         A (FR.Answer_Header_Word) := A (FR.Answer_Header_Word) or 16#0000_8000_0000_0000#;
         Check (not FR.Decode (A).Valid, "an answer header with unused bits set is refused");
      end;
      for O in DSP.Frame_Outcome loop
         declare
            Wire : constant DSP.Wire_Message := DSP.Encode_Frame_Result
              ((Session, 9, O, DSP.Released, others => <>));
            Back : constant DSP.Frame_Result_Decoding := DSP.Decode_Frame_Result (Wire);
         begin
            Check (Back.Valid and then Back.Value.Outcome = O, "outcome " & O'Image & " round-trips");
         end;
      end loop;
   end;

   ---------------------------------------------------------------------------
   --  1. A completion for every present, or a typed failure.
   ---------------------------------------------------------------------------
   Fresh;
   for F in 1 .. Unsigned_64 (FR.Frames_In_Flight) loop
      Submit (F, R);
      Check (R = FC.Queued, "present" & F'Image & " queued");
   end loop;
   Check (FC.Outstanding (Client) = FR.Frames_In_Flight, "every present outstanding");
   --  Display answers them newest first: each still finds its present.
   declare
      Tags : array (1 .. FR.Frames_In_Flight) of FR.RC.Token;
      Frames : array (1 .. FR.Frames_In_Flight) of Unsigned_64;
   begin
      for I in Tags'Range loop
         Take (Tags (I), Frames (I), Got);
         Check (Got and then Frames (I) = Unsigned_64 (I), "Display takes present" & I'Image & " in order");
      end loop;
      for I in reverse Tags'Range loop
         Answer (Tags (I), Result_For (Frames (I), DSP.Published));
      end loop;
   end;
   declare
      Seen : array (1 .. FR.Frames_In_Flight) of Natural := [others => 0];
   begin
      loop
         FC.Reap (Client, Item, Got);
         exit when not Got;
         Check (Item.Token in 101 .. 100 + Unsigned_64 (FR.Frames_In_Flight), "an answer names a present");
         if Item.Token in 101 .. 100 + Unsigned_64 (FR.Frames_In_Flight) then
            Seen (Natural (Item.Token - 100)) := Seen (Natural (Item.Token - 100)) + 1;
         end if;
         Check (Item.Taken_Us = 7 and then Item.Answered_Us = 9, "Display's stamps arrive");
      end loop;
      Check ((for all N of Seen => N = 1), "every present answered exactly once");
      Check (FC.Outstanding (Client) = 0 and then FC.Strays (Client) = 0, "nothing outstanding, nothing stray");
   end;
   --  Display goes away with three presents taken or queued: each fails once.
   for F in 20 .. 22 loop
      Submit (Unsigned_64 (F), R);
   end loop;
   Take (Tag, Frame, Got);
   Answer (Tag, Result_For (Frame, DSP.Published));
   FC.Reap (Client, Item, Got);
   Check (Got and then Item.Token = 120, "the answered one is reaped");
   FC.Detach (Client);
   declare
      Failed : array (Unsigned_64 range 121 .. 122) of Natural := [others => 0];
      Token : Unsigned_64;
   begin
      loop
         FC.Fail_Next (Client, Token, Got);
         exit when not Got;
         Check (Token in 121 .. 122, "only unanswered presents fail");
         if Token in 121 .. 122 then
            Failed (Token) := Failed (Token) + 1;
         end if;
      end loop;
      Check ((for all N of Failed => N = 1), "each unanswered present fails exactly once");
      FC.Reap (Client, Item, Got);
      Check (not Got, "a detached ring is not read");
      Submit (30, R);
      Check (R = FC.Not_Attached, "nothing is queued to a ring that ended");
   end;

   ---------------------------------------------------------------------------
   --  2. A superseded frame is answered; Desktop takes it as a drop.
   ---------------------------------------------------------------------------
   Fresh;
   Submit (41, R);
   Take (Tag, Frame, Got);
   Answer (Tag, Result_For (Frame, DSP.Superseded));
   FC.Reap (Client, Item, Got);
   Check (Got and then Item.Token = 141, "the superseded frame's answer arrives");
   declare
      Result : constant PW.Completion_Decoding := PW.Decode_Completion (Item.Result);
      S : CP.State := CP.Open (Session);
      Started : Boolean;
      Reply : CP.Completion;
   begin
      Check (Result.Valid and then Result.Value.Result.Outcome = DSP.Superseded and then
             Result.Value.Result.Buffer_State = DSP.Released, "it says superseded, buffer back");
      CP.Prepare (S, 41, Started);
      CP.Submitted (S, True, 1);
      Reply := (Kernel_Valid => True, Kernel_OK => True, Payload_Valid => Result.Valid,
                Kernel_Token => 41, Payload_Session => Session, Payload_Frame => 41,
                Published => Result.Value.Result.Outcome = DSP.Published,
                Released => Result.Value.Result.Buffer_State = DSP.Released);
      Check (Started and then CP.Drops (S, Reply) and then not CP.Releases (S, Reply),
             "the presentation takes it as a drop (redraw), not a fault");
   end;

   ---------------------------------------------------------------------------
   --  3. A forged answer is refused.
   ---------------------------------------------------------------------------
   Fresh;
   declare
      Tags : array (51 .. 54) of FR.RC.Token;
      Frames : array (51 .. 54) of Unsigned_64;
   begin
      for F in Tags'Range loop
         Submit (Unsigned_64 (F), R);
      end loop;
      for F in Tags'Range loop
         Take (Tags (F), Frames (F), Got);
      end loop;
      --  An answer under a tag nobody submitted.
      Answer (Tags (54) + 1000, Result_For (54, DSP.Published));
      FC.Reap (Client, Item, Got);
      Check (not Got and then FC.Strays (Client) = 1, "an unknown tag is refused and counted");
      --  The right tag, another frame's result (a wrong sequence).
      Answer (Tags (52), Result_For (Frames (52) + 7, DSP.Published));
      FC.Reap (Client, Item, Got);
      Check (not Got and then FC.Strays (Client) = 2, "another frame's result is refused and counted");
      --  A malformed entry (header bits set).
      declare
         Bad : FR.Answer_Words := Result_For (53, DSP.Published);
      begin
         Bad (FR.Answer_Header_Word) := Bad (FR.Answer_Header_Word) or 16#0000_1000_0000_0000#;
         Answer (Tags (53), Bad);
      end;
      FC.Reap (Client, Item, Got);
      Check (not Got and then FC.Strays (Client) = 3, "a malformed answer is refused and counted");
      Check (FC.Outstanding (Client) = 4, "every present is still outstanding");
      --  The real answer still lands.
      Answer (Tags (51), Result_For (51, DSP.Published));
      FC.Reap (Client, Item, Got);
      Check (Got and then Item.Token = 151, "the real answer is taken after the forged ones");
      Check (FC.Outstanding (Client) = 3, "the refused ones stay outstanding");
   end;

   ---------------------------------------------------------------------------
   --  4. Back pressure.
   ---------------------------------------------------------------------------
   Fresh;
   for F in 61 .. 60 + Unsigned_64 (FR.Frames_In_Flight) loop
      Submit (F, R);
   end loop;
   Submit (99, R);
   Check (R = FC.Full, "a full ring refuses the next present (the caller tries again)");
   Check (FC.Outstanding (Client) = FR.Frames_In_Flight, "nothing was dropped");
   Take (Tag, Frame, Got);
   Answer (Tag, Result_For (Frame, DSP.Failed));
   FC.Reap (Client, Item, Got);
   Check (Got and then Item.Token = 161, "a failed frame is answered too");
   Submit (99, R);
   Check (R = FC.Queued, "once an answer is taken, the next present fits");

   if Failures = 0 then
      Ada.Text_IO.Put_Line ("FRAME-RINGS: PASS checks=" & Checks'Image);
   else
      Ada.Text_IO.Put_Line ("FRAME-RINGS: FAIL failures=" & Failures'Image & " of" & Checks'Image);
   end if;
end Main;
