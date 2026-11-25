------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  GPU-014's desktop-down check: the renderer report answers when Desktop
--  is gone. Once a second it reads devmgr's report (CuBit.Graphics_Report:
--  the summary, then every page of every entry against one generation)
--  and logs its entries when they change. It passes once the GPU's entry
--  is there and Desktop's output entry is there and stale (Desktop exited:
--  procmgr's Gone), and fails after Give_Up_Ms.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Console_Route;
with CuBit.Graphics_Report;
with CuBit.Handles;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Monotonic;
with CuBit.Waits;
with Startup;

procedure Main is
   package GR renames CuBit.Graphics_Report;
   use type GR.Subject_Kind;
   Give_Up_Ms : constant := 60_000;
   Period_Ms : constant := 1_000;
   Answer_Ms : constant := 2_000;
   Target : constant CuBit.Handles.Endpoint := Startup.Graphics_Observer;
   Next_Id : Unsigned_64 := 1;
   Last_Generation : Unsigned_64 := 0;
   Began : constant Unsigned_64 := CuBit.Monotonic.Milliseconds;
   Ignored : Unsigned_64;

   function Call (Label : Unsigned_32; Length : Unsigned_8; W1, W2, W3 : Unsigned_64; Msg : out Message)
     return Boolean
   is
      Tag : MessageTag;
      Id : constant Unsigned_64 := Next_Id;
   begin
      Next_Id := Next_Id + 1;
      Msg := (tag => (label => Label, length => Length, protocol => 0, ipc => <>, reserved => 0),
              authorityTag => 0, words => [Id, W1, W2, W3]);
      Tag := capCall (Target, Msg, Deadline_After (Answer_Ms));
      return Ending_Of (Tag) = Call_Answered and then Tag.label = Label and then Msg.words (0) = Id;
   end Call;

   function Image (Value : Unsigned_64) return String is
      S : constant String := Unsigned_64'Image (Value);
   begin
      return S (S'First + 1 .. S'Last);
   end Image;

   function Kind_Name (K : GR.Subject_Kind) return String is
     (case K is when GR.Output => "output", when GR.Program => "program", when GR.GPU => "gpu");

   --  One read: True when it passes.
   function Read return Boolean is
      Msg : Message;
      Generation, Count : Unsigned_64;
      Wire : GR.Entry_Wire;
      Item : GR.Report_Entry;
      Have_GPU, Desktop_Stale : Boolean := False;
   begin
      if not Call (GR.Summary_Label, GR.Request_Words, 0, 0, 0, Msg) then
         debugPrint ("GRAPHICS-CHECK: devmgr did not answer the summary" & ASCII.LF);
         return False;
      end if;
      Generation := Msg.words (GR.Generation_Word);
      Count := Msg.words (GR.Count_Word);
      for I in 1 .. Count loop
         for Page in GR.Page_Index loop
            if not Call (GR.Entry_Label, GR.Entry_Request_Words, Generation, I, Unsigned_64 (Page), Msg) then
               return False;   --  the report moved on: next time
            end if;
            for Offset in 0 .. GR.Page_Words - 1 loop
               Wire (GR.Page_Words * Page + Offset) := Msg.words (GR.Payload_Word + Offset) and GR.Maximum_Word;
            end loop;
         end loop;
         if not GR.Valid (Wire) then
            debugPrint ("GRAPHICS-CHECK: FAIL entry" & Unsigned_64'Image (I) & " does not decode" & ASCII.LF);
            return False;
         end if;
         Item := GR.Decode (Wire);
         if Generation /= Last_Generation then
            debugPrint ("GRAPHICS-CHECK: entry " & Kind_Name (Item.Kind) & " " & GR.Image (Item.Subject) & " "
                        & GR.Path_Name (Item.Path) & " " & GR.Reason_Name (Item.Why) & " device "
                        & GR.Image (Item.Device) & (if Item.Stale then " stale" else " live")
                        & " generation " & Image (Generation) & ASCII.LF);
         end if;
         Have_GPU := Have_GPU or else Item.Kind = GR.GPU;
         Desktop_Stale := Desktop_Stale or else (Item.Kind = GR.Output and then Item.Stale);
      end loop;
      Last_Generation := Generation;
      return Have_GPU and then Desktop_Stale;
   end Read;
begin
   CuBit.Console_Route.Route_Named ("logstore");
   if not CuBit.Handles.Valid (Target) then
      debugPrint ("GRAPHICS-CHECK: FAIL no graphics-observer endpoint" & ASCII.LF);
      return;
   end if;
   loop
      if Read then
         debugPrint ("GRAPHICS-CHECK: PASS the report answers with Desktop gone: the GPU's entry and "
                     & "Desktop's last, stale" & ASCII.LF);
         return;
      end if;
      exit when CuBit.Monotonic.Milliseconds - Began > Give_Up_Ms;
      Ignored := CuBit.Waits.Sleep_For (Period_Ms);
   end loop;
   debugPrint ("GRAPHICS-CHECK: FAIL no stale Desktop entry within" & Image (Give_Up_Ms / 1000) & " s"
               & ASCII.LF);
end Main;
