with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Desktop_Protocol;
with CuBit.Desktop_Messages;
with CuBit.UI.App;
with Client_Input_Channel;
with Client_Input_Batch_Cache;
procedure Desktop_Input_Batch (Passed : out Boolean) is
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
      M.tag := capCall (CAP_SLOT_DESKTOP, M, CuBit.Messages.Wait_Forever);
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
   Created := DP.Decode_Creation_Result (Send (DP.Encode_Create ((64, 64, DP.Plain_Surface))));
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
   App.Open (Win, 160, 120, 0, OK, title => "Batch input probe", batched_input => True);
   Check (OK, "opt-in UI window");
   if not OK then return; end if;
   for I in 1 .. 3 loop
      Check (DP.Decode_Resize_Result (Send (DP.Encode_Resize
        ((DP.Live_Surface_Name (App.Surface_ID (Win)), DP.Pixel_Extent (160 + I), 120)))).Status = DP.Success,
        "queue UI configure");
   end loop;
   App.Poll_Input (Win, Event, Found);
   Check (Found and App.Input_May_Remain (Win), "UI poll receives cached batch");
   Stats := App.Input_Statistics (Win);
   Check (Stats.Batch_Enabled and not Stats.Channel_Disabled and
          Stats.Successful_Fetches = 1 and Stats.Fetched_Events >= 2 and
          Stats.Delivered_Events = 1 and Stats.Fallback_Polls = 0, "positive batch diagnostics");
   Previous := Event.serial;
   App.Wait_Input_Until (Win, syscall (SYSCALL_GETTIME) + 100, Event, Found);
   Check (Found and Event.serial > Previous, "UI wait consumes next cached event");
   Check (App.Input_Statistics (Win).Delivered_Events = 2, "cached wait diagnostics");
   App.Close (Win);
   Check (not App.Is_Open (Win), "close with cached events");
   Check (App.Input_Statistics (Win).Delivered_Events = 2, "diagnostics survive close");
   C.Clear (Cache);
   Created := DP.Decode_Creation_Result (Send (DP.Encode_Create ((64, 64, DP.Plain_Surface))));
   Check (Created.Status = DP.Success, "recreate after cached close");
   if Created.Status /= DP.Success then return; end if;
   Client_Input_Channel.Fetch (Cache, Unsigned_64 (Created.Surface), 0, Loaded);
   Check (Loaded, "process page survives window close");
   Destroy (Created.Surface);
   C.Clear (Cache);
   -- Deliberate rejected request quarantines this process's one page. The
   -- following opted-in window must remain usable through ordinary polling.
   Client_Input_Channel.Fetch (Cache, Unsigned_64'Last, 0, Loaded);
   Check (not Loaded, "rejected surface disables page reuse");
   App.Open (Win, 160, 120, 0, OK, title => "Batch fallback probe", batched_input => True);
   Check (OK, "fallback window");
   if not OK then return; end if;
   App.Poll_Input (Win, Event, Found);
   Check (Found, "single-event fallback after quarantine");
   Stats := App.Input_Statistics (Win);
   Check (Stats.Channel_Disabled and Stats.Successful_Fetches = 0 and
          Stats.Fetched_Events = 0 and Stats.Delivered_Events = 0 and
          Stats.Fallback_Polls = 1, "fallback diagnostics and reopen reset");
   App.Close (Win);
   Check (not App.Is_Open (Win), "fallback close");
   if Passed then debugPrint ("DESKTOP-INPUT-BATCH-CHECK: PASS batches=2 events=12 fallback=1" & ASCII.LF); end if;
end Desktop_Input_Batch;
