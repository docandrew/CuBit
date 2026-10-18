------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System.Storage_Elements;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Streams;
with CuBit.Channel_Bells;

package body CuBit.Process_Events is

   package CE renames CuBit.Child_Exits;
   package CV renames CuBit.Control_Events;
   package KA renames CuBit.Kernel_ABI;
   use type CV.Event_Kind;

   subtype Kept_Index is Positive range 1 .. Kept_Events;
   subtype Kept_Count is Natural range 0 .. Kept_Events;

   Exits : array (Kept_Index) of CE.Report;
   Exit_Count : Kept_Count := 0;
   Events : array (Kept_Index) of CV.Event;
   Event_Count : Kept_Count := 0;

   --  Poll reads only while both stores have room, so these never drop:
   --  what does not fit stays with the kernel until it does.
   procedure Keep_Exit (Report : CE.Report)
   with Pre => Exit_Count < Kept_Events;
   procedure Keep_Exit (Report : CE.Report) is
   begin
      Exit_Count := Exit_Count + 1;
      Exits (Exit_Count) := Report;
   end Keep_Exit;

   procedure Keep_Event (Item : CV.Event)
   with Pre => Event_Count < Kept_Events;
   procedure Keep_Event (Item : CV.Event) is
   begin
      Event_Count := Event_Count + 1;
      Events (Event_Count) := Item;
   end Keep_Event;

   function Has_Room return Boolean is
     (Exit_Count < Kept_Events and then Event_Count < Kept_Events);

   procedure File (Event : Message) is
      Bell_Notice : Boolean;
   begin
      --  Rung channel grants (CAP-002 step 3) go to CuBit.Channel_Bells.
      CuBit.Channel_Bells.Note_Notice
        (Event.tag.label, Event.tag.length,
         [Event.words (0), Event.words (1), Event.words (2), Event.words (3)], Bell_Notice);
      if Bell_Notice then
         return;
      end if;
      if Event.tag.label = CE.Event_Label then
         if CE.Valid (Event.tag.length, Event.words (0), Event.words (1), Event.words (2)) then
            Keep_Exit (CE.Decode (Event.words (0), Event.words (1), Event.words (2)));
         end if;
      else
         declare
            Item : constant CV.Event :=
              CV.Decode (Event.tag.label, Event.tag.length, Event.words (0), Event.words (1),
                         Event.words (2));
         begin
            case Item.Kind is
               when CV.Grant_Revoked =>
                  --  The default: a ring this process adopted goes back.
                  if not CuBit.Streams.Return_Revoked (Item.Slot, Item.Generation) then
                     Keep_Event (Item);
                  end if;
               when CV.Grant_Returned =>
                  --  The default: a reader of an outlet let go.
                  if not CuBit.Streams.Forget_Returned (Item) then
                     Keep_Event (Item);
                  end if;
               when CV.Control =>
                  Keep_Event (Item);
               when CV.Not_Ours =>
                  null;
            end case;
         end;
      end if;
   end File;

   procedure Poll is
      M : aliased Message;
   begin
      while Has_Room
        and then CuBit.Kernel_Calls.Call
              (KA.Receive_Event_Nonblocking,
               Unsigned_64 (System.Storage_Elements.To_Integer (M'Address)))
            = KA.Event_Received
      loop
         File (M);
      end loop;
   end Poll;

   procedure Take_Exit
     (Process : CuBit.Process_IDs.Process_ID; Found : out Boolean; Report : out CE.Report) is
   begin
      Poll;
      Found := False;
      Report := (others => <>);
      for K in 1 .. Exit_Count loop
         if Exits (K).Process = Process then
            Report := Exits (K);
            Exits (K .. Exit_Count - 1) := Exits (K + 1 .. Exit_Count);
            Exit_Count := Exit_Count - 1;
            Found := True;
            return;
         end if;
      end loop;
   end Take_Exit;

   procedure Next (Item : out CV.Event; Found : out Boolean) is
   begin
      Poll;
      Found := Event_Count > 0;
      if Found then
         Item := Events (1);
         Events (1 .. Event_Count - 1) := Events (2 .. Event_Count);
         Event_Count := Event_Count - 1;
      else
         Item := (Kind => CV.Not_Ours);
      end if;
   end Next;

end CuBit.Process_Events;
