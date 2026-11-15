pragma Ada_2022;
package body ACPI_Power_Button_Watches with SPARK_Mode is
   use type Word, E.Press_Source;
   function Observe (Item : State) return Snapshot is (Item.View);
   function Valid (Item : State) return Boolean is
     (Item.View.Count <= Maximum_Count
      and then (Item.View.Kept = (Item.Token /= No_Reply))
      and then (Item.View.Kept = (Item.View.Request_ID /= 0))
      and then (if not Item.View.Kept then Item.View.Seen = 0)
      and then ((Item.View.Count = 0) = (Item.View.Source = E.No_Press)));
   function Well_Formed (Input : Request) return Boolean is
     (Input.Length = E.Watch_Request_Words and then Input.ID /= 0
      and then Input.Flags = 0 and then Input.Reserved = 0);
   procedure Set_Availability (Item : in out State; Value : Availability) is
   begin Item.View.Hardware := Value; end Set_Availability;
   procedure Record_Press (Item : in out State; Source : Actual_Source) is
   begin
      if Item.View.Stop /= Not_Stopping then return; end if;
      if Item.View.Count = Maximum_Count then
         -- Never wrap or silently alias a previous press count. The native
         -- owner must actually retire when this bounded counter is exhausted.
         Item.View.Stop := Counter_Exhausted;
         return;
      end if;
      Item.View.Count := Item.View.Count + 1;
      Item.View.Source := Source;
      Item.View.Hardware := Present;
   end Record_Press;
   procedure Begin_Stop (Item : in out State) is
   begin
      if Item.View.Stop = Not_Stopping then Item.View.Stop := Owner_Stopping; end if;
   end Begin_Stop;
   function Retirement_Requested (Item : State) return Boolean is
     (Item.View.Stop /= Not_Stopping);
   function Runnable (Item : State) return Boolean is
     (Item.View.Kept and then (Retirement_Requested (Item)
       or else Item.View.Hardware /= Present or else Item.View.Count > Item.View.Seen));
   function Quiet (Item : State) return Boolean is (not Item.View.Kept);
   procedure Answer (Item : in out State; Status : E.Watch_Status; Result : out Delivery) is
      Delivered : Boolean;
   begin
      Answer_Saved (Item.Token, Item.View.Request_ID, Status,
        Item.View.Count, Item.View.Source, Delivered);
      Item.Token := No_Reply;
      Item.View.Kept := False;
      Item.View.Request_ID := 0;
      Item.View.Seen := 0;
      Result := (if Delivered then Answered else Caller_Gone);
   end Answer;
   procedure Advance (Item : in out State; Owner_Quiet : Boolean; Result : out Delivery) is
   begin
      Result := No_Attempt;
      if not Owner_Quiet or else not Runnable (Item) then return; end if;
      if Retirement_Requested (Item) then Answer (Item, E.Stopping, Result);
      elsif Item.View.Hardware = Absent then Answer (Item, E.No_Power_Button, Result);
      elsif Item.View.Hardware = Unobserved then Answer (Item, E.Watch_Unavailable, Result);
      else Answer (Item, E.Pressed, Result);
      end if;
   end Advance;
   procedure Watch (Item : in out State; Input : Request; Owner_Quiet : Boolean;
      Reply : out Response; Deferred : out Boolean; Previous : out Delivery) is
      New_Token : Reply_Token := No_Reply;
      Saved : Boolean;
   begin
      Reply := (ID => Input.ID, Count => Item.View.Count, Source => Item.View.Source, others => <>);
      Deferred := False; Previous := No_Attempt;
      if Input.Authority /= E.Authority_Tag then Reply.Status := E.Not_Approved; return; end if;
      if not Well_Formed (Input) then Reply.Status := E.Malformed; return; end if;
      if not Owner_Quiet then Reply.Status := E.Watch_Unavailable; return; end if;
      -- Pending facts precede new watches, matching the native owner-before-IPC turn.
      Advance (Item, Owner_Quiet, Previous);
      if Retirement_Requested (Item) then Reply.Status := E.Stopping; return; end if;
      if Item.View.Hardware = Unobserved then Reply.Status := E.Watch_Unavailable; return; end if;
      if Item.View.Hardware = Absent then Reply.Status := E.No_Power_Button; return; end if;
      if Input.Seen /= Item.View.Count then
         if Item.View.Kept then Answer (Item, E.Superseded, Previous); end if;
         Reply.Status := (if Input.Seen < Item.View.Count then E.Pressed else E.Counter_Reset);
         return;
      end if;
      Save_Current (New_Token, Saved);
      if not Saved or else New_Token = No_Reply then Reply.Status := E.Watch_Unavailable; return; end if;
      if Item.View.Kept then Answer (Item, E.Superseded, Previous); end if;
      Item.Token := New_Token;
      Item.View.Kept := True;
      Item.View.Request_ID := Input.ID;
      Item.View.Seen := Input.Seen;
      Deferred := True;
   end Watch;
end ACPI_Power_Button_Watches;
