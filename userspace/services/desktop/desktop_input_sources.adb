------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with System.Storage_Elements;
with CuBit.Channel_Contracts;
with CuBit.Channel_Protocol;
with CuBit.Channels;
with CuBit.Input_Rings;
with Input_Source_Set;
with Input_Ring_Admission;

package body Desktop_Input_Sources is

   package IR renames CuBit.Input_Rings;
   use type CuBit.Messages.Process_ID;
   use type CuBit.Channel_Contracts.Contract;
   use type CuBit.Channels.Side;

   Links : array (1 .. Maximum_Sources) of CuBit.Channels.Channel;

   --  The one place Desktop wakes a source's driver (room in its ring):
   --  IPC-010 swaps the doorbell for a shared futex here.
   procedure Wake_Producer (Slot : Positive) is
      Ignore : CuBit.Channels.Signal_Result;
   begin
      CuBit.Channels.Signal (Links (Slot), Sync => False, Result => Ignore);
   end Wake_Producer;

   package Set is new Input_Source_Set
     (Capacity => Maximum_Sources, Wake_Producer => Wake_Producer, Deliver => Deliver);

   function Address_Of (Base : Unsigned_64) return System.Address is
     (System.Storage_Elements.To_Address (System.Storage_Elements.Integer_Address (Base)));

   function Image (Value : Unsigned_64) return String is
      Text : constant String := Unsigned_64'Image (Value);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Image;

   function Rings_Of (From : CuBit.Messages.Process_ID) return Natural is
      Total : Natural := 0;
   begin
      for S in Links'Range loop
         if Set.Active (S) and then Links (S).Peer = From then
            Total := Total + 1;
         end if;
      end loop;
      return Total;
   end Rings_Of;

   procedure Handle_Open
     (From : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message;
      Is_Open : out Boolean; Answer : out CuBit.Messages.Message)
   is
      Valid : Boolean;
      Item : CuBit.Channel_Contracts.Contract;
      Opener_Side : CuBit.Channels.Side;
      Connector : Unsigned_16;
      Slot : Natural;
      Source : Unsigned_64;
      Driver : constant String := Image (CuBit.Messages.To_Word (From));
   begin
      Answer := CuBit.Messages.NULL_MESSAGE;
      CuBit.Channels.Decode_Open (Request, Is_Open, Valid, Item, Opener_Side, Connector);
      --  Only source-ring opens are ours: surface rings and the rest go on.
      if not Is_Open or else not Valid or else Item /= IR.Contract then
         Is_Open := False;
         return;
      end if;
      Slot := Set.Free_Slot;
      declare
         use type Input_Ring_Admission.Verdict;
         Verdict : constant Input_Ring_Admission.Verdict := Input_Ring_Admission.Decide
           (Request.authorityTag, Opener_Side = CuBit.Channels.Producing, Rings_Of (From), Slot);
      begin
         if Verdict /= Input_Ring_Admission.Admitted then
            Report ("desktop: input source ring refused (process " & Driver & "): " &
                    Input_Ring_Admission.Reason (Verdict));
            Answer := CuBit.Channels.Refusal_Reply
              (if Verdict = Input_Ring_Admission.Source_Limit then CuBit.Channel_Protocol.Limit_Reached
               else CuBit.Channel_Protocol.Not_Admitted);
            return;
         end if;
      end;
      CuBit.Channels.Accept_Open (From, Request, Unsigned_64 (Slot), Links (Slot), Answer);
      if not Links (Slot).Active then
         Report ("desktop: input source ring refused (process " & Driver & "): its grant was unusable");
         return;
      end if;
      Set.Add (Slot, Address_Of (Links (Slot).Own_Base), Address_Of (Links (Slot).Peer_Base), Source);
      if Source = 0 then
         CuBit.Channels.Close (Links (Slot));
         Answer := CuBit.Channels.Refusal_Reply (CuBit.Channel_Protocol.Limit_Reached);
         return;
      end if;
      Report ("desktop: input source " & Image (Source) & " opened (process " & Driver & ")");
   end Handle_Open;

   procedure End_Slot (Slot : Positive; Why : String) is
      Source : constant Unsigned_64 := Set.Source_Of (Slot);
      Lost : constant Unsigned_32 := Set.Producer_Overflows (Slot);
   begin
      Set.Drain_Slot (Slot);
      Set.Remove (Slot);
      CuBit.Channels.Close (Links (Slot));
      Report ("desktop: input source " & Image (Source) & " ended: " & Why &
              (if Lost /= 0 then "; its driver overflowed" & Unsigned_32'Image (Lost) & " times" else ""));
      Source_Ended (Source);
   end End_Slot;

   procedure Handle_Close
     (From : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message; Closed : out Boolean) is
   begin
      Closed := False;
      for S in Links'Range loop
         if Set.Active (S) and then CuBit.Channels.Closed_By (Links (S), From, Request) then
            End_Slot (S, "its driver closed it");
            Closed := True;
            return;
         end if;
      end loop;
   end Handle_Close;

   procedure Drain (Delivered : out Natural) is
   begin
      Set.Drain (Drain_Budget, Delivered);
   end Drain;

   function Quiet return Boolean is (Set.Quiet);
   function Waiting return Boolean is (Set.Waiting);

   procedure Disarm is
   begin
      Set.Disarm_All;
   end Disarm;

   procedure Take_Event (Event : CuBit.Control_Events.Event; Ended : out Boolean) is
   begin
      Ended := False;
      for S in Links'Range loop
         if Set.Active (S) and then CuBit.Channels.Ended (Links (S), Event) then
            End_Slot (S, "its driver or device went away");
            Ended := True;
            return;
         end if;
      end loop;
   end Take_Event;

   function Count return Natural is (Set.Count);

end Desktop_Input_Sources;
