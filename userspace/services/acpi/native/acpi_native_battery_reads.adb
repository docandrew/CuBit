pragma Ada_2022;
with Interfaces;
with CuBit.Handles.IPC;
with ACPI_Native_Cached_Battery;
with ACPI_Native_Event_Domain;
with ACPI_Cached_Battery_Codec;
with ACPI_Battery_Observation_Protocol;
with ACPI_Cached_Power_Values;
with ACPI_Protocol;
with ACPI_Endpoint;
package body ACPI_Native_Battery_Reads is
   package M renames CuBit.Messages;package H renames CuBit.Handles;package IPC renames H.IPC;
   package D renames ACPI_Native_Event_Domain;
   package P renames ACPI_Battery_Observation_Protocol;
   package V renames ACPI_Cached_Power_Values;
   package Codec is new ACPI_Cached_Battery_Codec(P.Read_Metrics_Label,P.Battery_Page);
   use type V.Generation,H.Reply,Interfaces.Unsigned_64,Interfaces.Unsigned_32,Codec.Decision,P.Subpage;
   subtype Slot is Positive range 1..Maximum_Pending;
   type Holder is record
      Token:H.Reply:=H.No_Reply;
      Request:M.Message:=M.NULL_MESSAGE;
      Ticket:Interfaces.Unsigned_64:=0;
      Refused:Boolean:=False;
   end record;
   type Holders is array(Slot) of Holder;
   Items:Holders;
   Next_Slot:Slot:=Slot'First;
   Stopped:Boolean:=False;
   function Quiet return Boolean is(for all Item of Items=>Item.Token=H.No_Reply);
   function Ready(Item:Holder) return Boolean is
     (Item.Token/=H.No_Reply and then (Stopped or else Item.Refused or else D.Battery_Refresh_Ready(Item.Ticket)));
   function Pending return Boolean is(for some Item of Items=>Ready(Item));
   procedure Begin_Stop is begin Stopped:=True;end Begin_Stop;
   procedure Unavailable(Server:ACPI_Requests.State;Request:M.Message;Reply:out M.Message) is
      Handled:Boolean;
   begin
      ACPI_Native_Cached_Battery.Dispatch(Server,Request,False,Reply,Handled);
      if not Handled then
         declare Packet:constant ACPI_Requests.Packet:=ACPI_Endpoint.Encode((Status=>ACPI_Requests.Wrong_Order,others=><>));begin
            Reply:=M.NULL_MESSAGE;Reply.tag.label:=Packet.Label;Reply.tag.length:=Packet.Length;Reply.words:=M.MessageWords(Packet.Data);
         end;
      end if;
   end Unavailable;
   procedure Advance(Server:ACPI_Requests.State;Owner_Quiet,Read_Allowed:Boolean;Did_Work:out Boolean) is
      I:Slot:=Next_Slot;Reply:M.Message;Handled:Boolean;
   begin
      Did_Work:=False;if not Owner_Quiet then return;end if;
      for Step in Slot loop
         if Ready(Items(I)) then
            if Stopped or Items(I).Refused or not Read_Allowed then Unavailable(Server,Items(I).Request,Reply);
            else
               -- One passive final read only. Age0 or failed publication may
               -- still be unavailable; never reschedule this saved request.
               ACPI_Native_Cached_Battery.Dispatch(Server,Items(I).Request,True,Reply,Handled);
               if not Handled then Unavailable(Server,Items(I).Request,Reply);end if;
            end if;
            declare Delivered:constant Boolean:=IPC.Answer(Items(I).Token,Reply);
               pragma Unreferenced(Delivered);
            begin null;end;
            -- The kernel consumes the authority even when delivery fails.
            Items(I):=(others=><>);Did_Work:=True;
            Next_Slot:=(if I=Slot'Last then Slot'First else I+1);return;
         end if;
         I:=(if I=Slot'Last then Slot'First else I+1);
      end loop;
   end Advance;
   procedure Dispatch(Server:ACPI_Requests.State;Request:M.Message;Read_Allowed:Boolean;
      Reply:out M.Message;Handled,Reply_Now:out Boolean) is
      Selected:Codec.Selection;
      Free:Natural range 0..Maximum_Pending:=0;
      Now,Ticket:Interfaces.Unsigned_64;
      Allowed,Accepted:Boolean;
   begin
      Reply_Now:=True;
      ACPI_Native_Cached_Battery.Dispatch(Server,Request,Read_Allowed,Reply,Handled);
      if not Handled or not Read_Allowed or Stopped then return;end if;
      Selected:=Codec.Inspect((Request.tag.label,Request.tag.length,M.Flag_Byte(Request.tag),
        Request.tag.reserved,Codec.Words(Request.words)),ACPI_Protocol.Role_Tag(Request.authorityTag));
      if Selected.Kind/=Codec.Read_Inventory or else Selected.Page=P.Sample_Time
        or else Reply.tag.label/=ACPI_Endpoint.Reply_OK then return;end if;
      -- The trusted copied reply has the shared codec's availability layout.
      -- Pending header reads may join an already admitted attempt; auxiliary
      -- generation errors remain immediate errors and never trigger AML.
      if Reply.words(1)/=V.Availability'Pos(V.Too_Old) and then
        not (Reply.words(1)=V.Availability'Pos(V.Not_Ready) and then Selected.Page=P.Inventory
          and then Selected.Expected=0 and then not Quiet) then return;end if;
      for I in Slot loop if Items(I).Token=H.No_Reply then Free:=I;exit;end if;end loop;
      if Free=0 then return;end if;
      D.Preview_Battery_Refresh(Server,Now,Allowed);if not Allowed then return;end if;
      declare Saved:constant IPC.Saved_Reply:=IPC.Save_Reply;begin
         if not Saved.Saved then return;end if;
         -- No reentry at Save_Reply: install the owned authority before asking
         -- the fixed native producer to commit the metadata-only demand.
         Items(Slot(Free)):=(Saved.Handle,Request,0,True);Reply_Now:=False;
      end;
      D.Commit_Battery_Refresh(Server,Now,Ticket,Accepted);
      Items(Slot(Free)).Ticket:=Ticket;Items(Slot(Free)).Refused:=not Accepted;
   end Dispatch;
end ACPI_Native_Battery_Reads;
