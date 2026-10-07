------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with System.Storage_Elements;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Streams;

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

   procedure Keep_Exit (Report : CE.Report);
   procedure Keep_Exit (Report : CE.Report) is
   begin
      if Exit_Count = Kept_Events then
         Exits (1 .. Kept_Events - 1) := Exits (2 .. Kept_Events);
         Exit_Count := Exit_Count - 1;
      end if;
      Exit_Count := Exit_Count + 1;
      Exits (Exit_Count) := Report;
   end Keep_Exit;

   procedure Keep_Event (Item : CV.Event);
   procedure Keep_Event (Item : CV.Event) is
   begin
      if Event_Count = Kept_Events then
         Events (1 .. Kept_Events - 1) := Events (2 .. Kept_Events);
         Event_Count := Event_Count - 1;
      end if;
      Event_Count := Event_Count + 1;
      Events (Event_Count) := Item;
   end Keep_Event;

   procedure Poll is
      M : aliased Message;
   begin
      while CuBit.Kernel_Calls.Call
              (KA.Receive_Event_Nonblocking,
               Unsigned_64 (System.Storage_Elements.To_Integer (M'Address)))
            = KA.Event_Received
      loop
         if M.tag.label = CE.Event_Label then
            if CE.Valid (M.tag.length, M.words (0), M.words (1), M.words (2)) then
               Keep_Exit (CE.Decode (M.words (0), M.words (1), M.words (2), M.words (3)));
            end if;
         else
            declare
               Item : constant CV.Event :=
                 CV.Decode (M.tag.label, M.tag.length, M.words (0), M.words (1), M.words (2));
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
      end loop;
   end Poll;

   procedure Take_Exit
     (Process, Generation : Unsigned_64; Found : out Boolean; Report : out CE.Report) is
   begin
      Poll;
      Found := False;
      Report := (others => <>);
      for K in 1 .. Exit_Count loop
         if Exits (K).Process = Process and then Exits (K).Generation = Generation then
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
