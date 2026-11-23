pragma Ada_2022;
with ACPI_Protocol;
package body ACPI_Power_Inventory_Watches with SPARK_Mode is
   use type Word;
   function Generation (Item : State) return Word is (Item.Current);
   function Quiet (Item : State) return Boolean is (for all E of Item.Holders=>E.Token=No_Reply);
   function Ready (Item : State; E : Holder) return Boolean is
     (E.Token/=No_Reply and then (Item.Stopped or Item.Overflow or E.Seen/=Item.Current));
   function Runnable (Item : State) return Boolean is (for some E of Item.Holders=>Ready(Item,E));
   function Valid (Item : State) return Boolean is
     (Maximum_Generation>=1 and then Item.Current in 1..Maximum_Generation
      and then (for all I in Slot =>
        (if Item.Holders(I).Token=No_Reply then Item.Holders(I).Sender=0 and Item.Holders(I).ID=0 and Item.Holders(I).Seen=0
         else Item.Holders(I).ID/=0 and Item.Holders(I).Seen<=Item.Current))
      and then (for all I in Slot => (for all J in Slot =>
        (if I/=J and then Item.Holders(I).Token/=No_Reply and then Item.Holders(J).Token/=No_Reply
         then Item.Holders(I).Sender/=Item.Holders(J).Sender and Item.Holders(I).Token/=Item.Holders(J).Token))));
   procedure Changed (Item : in out State) is
   begin
      if Item.Stopped or Item.Overflow then return;end if;
      if Item.Current=Maximum_Generation then Item.Overflow:=True;
      else Item.Current:=Item.Current+1;end if;
   end Changed;
   procedure Begin_Stop (Item : in out State) is begin Item.Stopped:=True;end Begin_Stop;
   procedure Answer (Item : in out State; At_Slot : Slot; Status : P.Status; Result : out Delivery) is
      Delivered : Boolean;
   begin
      Answer_Saved(Item.Holders(At_Slot).Token,Item.Holders(At_Slot).ID,Status,Item.Current,Delivered);
      Item.Holders(At_Slot):=(others=><>);
      Result:=(if Delivered then Answered else Caller_Gone);
   end Answer;
   procedure Advance (Item : in out State; Owner_Quiet : Boolean; Result : out Delivery) is
      I : Slot:=Item.Next_Slot;
   begin
      Result:=No_Attempt;if not Owner_Quiet then return;end if;
      for Step in Slot loop
         if Ready(Item,Item.Holders(I)) then
            Answer(Item,I,(if Item.Stopped then P.Stopping elsif Item.Overflow then P.Exhausted else P.Changed),Result);
            Item.Next_Slot:=(if I=Slot'Last then Slot'First else I+1);return;
         end if;
         I:=(if I=Slot'Last then Slot'First else I+1);
      end loop;
   end Advance;
   procedure Watch (Item : in out State; Input : Request; Owner_Quiet : Boolean;
      Reply : out Response; Deferred : out Boolean; Previous : out Delivery) is
      Free,Same : Natural range 0..P.Maximum_Watches:=0;
      Token : Reply_Token:=No_Reply;Saved:Boolean;
   begin
      Reply:=(Input.ID,P.Unavailable,Item.Current);Deferred:=False;Previous:=No_Attempt;
      if not ACPI_Protocol.Is_Observer(ACPI_Protocol.Role_Tag(Input.Authority)) then Reply.Status:=P.Denied;Reply.Generation:=0;return;end if;
      if Input.Length/=4 or Input.ID=0 or Input.Zero_1/=0 or Input.Zero_2/=0 or Input.Flags/=0 or Input.Reserved/=0 then Reply.Status:=P.Malformed;return;end if;
      if Item.Stopped then Reply.Status:=P.Stopping;return;end if;
      if not Owner_Quiet then return;end if;
      if Item.Overflow then Reply.Status:=P.Exhausted;return;end if;
      if Input.Seen>Item.Current then Reply.Status:=P.Future_Generation;return;end if;
      for I in Slot loop
         if Item.Holders(I).Token=No_Reply then
            if Free=0 then Free:=I;end if;
         elsif Item.Holders(I).Sender=Input.Sender then Same:=I;end if;
      end loop;
      if Input.Seen<Item.Current then
         if Same/=0 then Answer(Item,Slot(Same),P.Superseded,Previous);end if;
         Reply.Status:=P.Changed;return;
      end if;
      if Same=0 and Free=0 then Reply.Status:=P.Full;return;end if;
      Save_Current(Token,Saved);
      if not Saved or else Token=No_Reply then return;end if;
      -- Obtain new authority before consuming the predecessor; a save refusal
      -- cannot cancel this sender's old watch or another holder's watch.
      if Same/=0 then Answer(Item,Slot(Same),P.Superseded,Previous);Free:=Same;end if;
      Item.Holders(Slot(Free)):=(Token,Input.Sender,Input.ID,Input.Seen);Deferred:=True;
   end Watch;
end ACPI_Power_Inventory_Watches;
