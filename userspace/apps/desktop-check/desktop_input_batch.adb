with Interfaces; use Interfaces;
with UI_Desktop_Service;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Desktop_Protocol;
with CuBit.Desktop_Messages;
with CuBit.UI.App;
with Client_Input_Channel;
with Client_Input_Batch_Cache;
with CuBit.Monotonic;
procedure Desktop_Input_Batch (Passed : out Boolean) is
   --  How long a window waits for its ring's first answer in this check.
   RING_WAIT_MS : constant := 1_000;
   package DP renames CuBit.Desktop_Protocol;
   package App renames CuBit.UI.App;
   package C renames Client_Input_Batch_Cache;
   use type DP.Status_Code;
   use type DP.Input_Event_Kind;
   Created : DP.Creation_Result;
   Cache : C.State;
   Loaded, OK, Found : Boolean;
   After : Unsigned_64 := 0;
   Item : DP.Input_Result;
   Win : App.Window;
   Event : App.Input_Event;
   Previous : Unsigned_64;
   Stats : App.Input_Diagnostics;
   function Send (Wire : DP.Wire_Message) return DP.Wire_Message is
      M : Message := CuBit.Desktop_Messages.From_Wire (Wire);
   begin
      M.tag := capCall (UI_Desktop_Service.Endpoint, M, CuBit.Messages.Wait_Forever);
      return CuBit.Desktop_Messages.To_Wire (M);
   end Send;
   procedure Check (Condition : Boolean; Name : String) is
   begin
      if not Condition then Passed := False;
         debugPrint ("TEST: FAIL input batch " & Name & ASCII.LF);
      end if;
   end Check;
   procedure Destroy (Surface : DP.Live_Surface_Name) is
   begin
      Check (DP.Decode_Status (Send (DP.Encode_Destroy ((Surface => Surface))), DP.Destroy_Surface) = DP.Success,
             "destroy surface");
   end Destroy;
begin
   Passed := True;
   Created := DP.Decode_Creation_Result (Send (DP.Encode_Create ((64, 64, DP.Plain_Surface, DP.Desktop_Sized))));
   Check (Created.Status = DP.Success, "create raw surface");
   if Created.Status /= DP.Success then return; end if;
   for I in 1 .. 11 loop
      Check (DP.Decode_Resize_Result (Send (DP.Encode_Resize
        ((Created.Surface, DP.Pixel_Extent (100 + I), 64)))).Status = DP.Success, "queue configure");
   end loop;
   for Batch in 1 .. 2 loop
      Client_Input_Channel.Fetch (Cache, Unsigned_64 (Created.Surface), After, Loaded);
      Check (Loaded, "native grant snapshot, no single-poll fallback");
      if not Loaded then Destroy (Created.Surface); return; end if;
      Check (C.Remaining (Cache) = (if Batch = 1 then 8 else 4), "bounded batch length");
      for I in 1 .. (if Batch = 1 then 8 else 4) loop
         C.Take (Cache, Unsigned_64 (Created.Surface), After, Item);
         Check (Item.Status = DP.Success, "take cached event");
         if Item.Status /= DP.Success then Destroy (Created.Surface); return; end if;
         Check (Item.Value.Kind = DP.Surface_Configured and Item.Value.Serial > After, "configure order");
         After := Item.Value.Serial;
      end loop;
   end loop;
   Client_Input_Channel.Fetch (Cache, Unsigned_64 (Created.Surface), After, Loaded);
   Check (Loaded and C.Remaining (Cache) = 0, "acknowledge final batch");
   Destroy (Created.Surface);
   --  A toolkit window receives input through its surface ring (IPC-004):
   --  one answer carries the queued batch.
   declare
      Open_Outcome : App.Open_Result;
   begin
      App.Open (Win, 160, 120, 0, Open_Outcome, title => "Batch input probe");
      OK := Open_Outcome in App.Opened;
   end;
   Check (OK, "ring UI window");
   if not OK then return; end if;
   for I in 1 .. 3 loop
      Check (DP.Decode_Resize_Result (Send (DP.Encode_Resize
        ((DP.Live_Surface_Name (App.Surface_ID (Win)), DP.Pixel_Extent (160 + I), 120)))).Status = DP.Success,
        "queue UI configure");
   end loop;
   App.Wait_Input_Until (Win, CuBit.Monotonic.Milliseconds + RING_WAIT_MS, Event, Found);
   Check (Found and App.Input_May_Remain (Win), "UI wait receives the ring's batch");
   Stats := App.Input_Statistics (Win);
   Check (Stats.Batch_Enabled and not Stats.Channel_Disabled and
          Stats.Successful_Fetches = 1 and Stats.Fetched_Events >= 2 and
          Stats.Delivered_Events = 1 and Stats.Fallback_Polls = 0, "ring batch diagnostics");
   Previous := Event.serial;
   App.Poll_Input (Win, Event, Found);
   Check (Found and Event.serial > Previous, "UI poll takes the next received event, no call");
   Check (App.Input_Statistics (Win).Delivered_Events = 2, "received poll diagnostics");
   App.Close (Win);
   Check (not App.Is_Open (Win), "close with received events");
   Check (App.Input_Statistics (Win).Delivered_Events = 2, "diagnostics survive close");
   C.Clear (Cache);
   --  A second window has a ring of its own and fresh diagnostics.
   declare
      Open_Outcome : App.Open_Result;
   begin
      App.Open (Win, 160, 120, 0, Open_Outcome, title => "Second ring window");
      OK := Open_Outcome in App.Opened;
   end;
   Check (OK, "second window");
   if not OK then return; end if;
   App.Wait_Input_Until (Win, CuBit.Monotonic.Milliseconds + RING_WAIT_MS, Event, Found);
   Check (Found, "second window's ring delivers");
   Stats := App.Input_Statistics (Win);
   Check (not Stats.Channel_Disabled and Stats.Successful_Fetches = 1 and
          Stats.Delivered_Events = 1 and Stats.Fallback_Polls = 0, "reopen resets diagnostics");
   App.Close (Win);
   Check (not App.Is_Open (Win), "second close");
   if Passed then debugPrint ("DESKTOP-INPUT-BATCH-CHECK: PASS batches=2 events=12 ring=2" & ASCII.LF); end if;
end Desktop_Input_Batch;
