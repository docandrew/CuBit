with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with Intel_GPU_Context_Table;
with Intel_GPU_Context_Table.Draining;
with Intel_GPU_GuC_Actions;
with Intel_GPU_GuC_Context_Event;
with Intel_GPU_GuC_Context_Lifecycle;
with Intel_GPU_GuC_Context_Session;
with Intel_GPU_GuC_Submission_Policy;
with Test_Storage;
-- GuC context IDs and G2H reply credits under recycling (Linux-hosted, the
-- production context table, GuC session and lifecycle against a modelled
-- GuC that answers MODE_SET and DEREGISTER late and in any order).
-- Checked: an ID is released (reusable) only after the GuC acknowledged
-- its deregistration or never saw it; released IDs are reused; answers in
-- flight never exceed the pool (requests wait with back pressure instead);
-- the pool's accounting equals the answers the model GuC still owes.
procedure Context_Recycling_Tests is
   package Events renames Intel_GPU_GuC_Context_Event;
   package Life renames Intel_GPU_GuC_Context_Lifecycle;
   package Actions renames Intel_GPU_GuC_Actions;
   use type Life.Phase;
   First : constant := 7;
   Capacity : constant := 48;
   Answer_Words : constant := Intel_GPU_GuC_Submission_Policy.Reply_Credit_Words;
   -- Room for three answers: back pressure is routine here.
   Pool_Words : constant := 3 * Answer_Words;
   subtype Context_ID is Unsigned_32 range First .. First + Capacity - 1;
   -- The model GuC: what it knows and the answers it owes.
   type Owed_Answer is (None, Mode_Done_On, Mode_Done_Off, Deregister_Done);
   Known_To_GuC : array (Context_ID) of Boolean := [others => False];
   Owed : array (Context_ID) of Owed_Answer := [others => None];
   Due : array (Context_ID) of Natural := [others => 0];
   Seed : Unsigned_64 := 16#D1B5_4A32_D192_ED03#;
   function Random (Below : Positive) return Natural is
   begin
      Seed := Seed xor Shift_Left (Seed, 13);
      Seed := Seed xor Shift_Right (Seed, 7);
      Seed := Seed xor Shift_Left (Seed, 17);
      return Natural (Seed mod Unsigned_64 (Below));
   end Random;
   function Ready return Boolean is (True);
   procedure Queue (Payload : Events.Words; Result : out Life.Send_Result) is
      Action : constant Actions.Action_Code := Actions.Action_Of (Payload (Payload'First));
      -- REGISTER_CONTEXT names the context in its third word, the others
      -- in their second.
      ID : constant Unsigned_32 :=
        (if Action = Actions.Register_Context then Payload (Payload'First + 2)
         else Payload (Payload'First + 1));
   begin
      Result := Life.Queued;
      if ID not in Context_ID then return; end if;
      if Action = Actions.Register_Context then
         Known_To_GuC (ID) := True;
      elsif Action = Actions.Sched_Context_Mode_Set then
         Owed (ID) := (if Payload (Payload'First + 2) = Actions.Context_Enable
                       then Mode_Done_On else Mode_Done_Off);
         Due (ID) := Random (4);
      elsif Action = Actions.Deregister_Context then
         Owed (ID) := Deregister_Done; Due (ID) := Random (4);
      end if;
   end Queue;
   Retained : Natural := 0;
   procedure Retain (Payload : Events.Words; Fence : Unsigned_16; Success : out Boolean) is
      pragma Unreferenced (Payload, Fence);
   begin
      Retained := Retained + 1; Success := True;
   end Retain;
   package Driver is new Intel_GPU_GuC_Context_Session (Ready, Queue, Retain);
   package Pool is new Intel_GPU_Context_Table
     (Capacity, Driver, Ready, Retain, First_ID => First, Reply_Words => Pool_Words);
   function Now_Us return Unsigned_64 is (1);
   function Work_Drained (Session : Unsigned_64) return Boolean is (Session /= 0);
   package Draining is new Pool.Draining (Now_Us, Work_Drained);
   T : Pool.Table;
   Checks, Failures : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         if Failures <= 20 then Put_Line ("FAIL: " & What); end if;
      end if;
   end Check;
   Opened_Count, Released_Count, Reused, Backpressures : Natural := 0;
   Ever_Used : array (Context_ID) of Boolean := [others => False];
   Next_Session : Unsigned_64 := 100;
   Status : Driver.Result;
   use type Driver.Result;
   function Owed_Words return Natural is
      N : Natural := 0;
   begin
      for A of Owed loop
         if A in Mode_Done_On | Mode_Done_Off then N := N + Answer_Words;
         elsif A = Deregister_Done then N := N + 3; end if;
      end loop;
      return N;
   end Owed_Words;
   procedure Answer_Due is
      ID_Out : Unsigned_32;
      Delivery : Pool.Dispatch_Result;
   begin
      for ID in Context_ID loop
         if Owed (ID) /= None then
            if Due (ID) > 0 then Due (ID) := Due (ID) - 1;
            else
               case Owed (ID) is
                  when Mode_Done_On | Mode_Done_Off =>
                     Pool.Dispatch (T, [Actions.Event_Header (Actions.Sched_Context_Mode_Done), ID,
                       (if Owed (ID) = Mode_Done_On then 1 else 0)], 1, ID_Out, Delivery);
                  when Deregister_Done =>
                     Known_To_GuC (ID) := False;
                     Pool.Dispatch (T, [Actions.Event_Header (Actions.Deregister_Context_Done), ID],
                       1, ID_Out, Delivery);
                  when None => null;
               end case;
               Owed (ID) := None;
               Check (ID_Out = ID, "answer reaches its context");
            end if;
         end if;
      end loop;
   end Answer_Due;
   procedure Open_One is
      ID : constant Context_ID := Context_ID (First + Random (Capacity));
      Accepted : Boolean;
   begin
      if Pool.In_Use (T, ID) then return; end if;
      Check (not Known_To_GuC (ID), "a free ID is unknown to the GuC");
      Pool.Open (T, ID, 16#200000# + Unsigned_64 (ID) * 16#10000#, 4096, 1000, 500_000, True,
                 Accepted, Session => Next_Session);
      Next_Session := Next_Session + 1;
      Check (Accepted, "a free ID opens");
      Check (not Pool.Releasable (T, ID), "an admitted context is never released");
      if Ever_Used (ID) then Reused := Reused + 1; end if;
      Ever_Used (ID) := True;
      Opened_Count := Opened_Count + 1;
      if Random (8) /= 0 then
         Pool.Submit (T, ID, Life.Register_Context, Status);
         Pool.Submit (T, ID, Life.Set_Policy, Status);
      end if;
   end Open_One;
   procedure Work_On (ID : Context_ID) is
      Accepted : Boolean;
      Session : constant Unsigned_64 := Pool.Session_Of (T, ID);
   begin
      if not Pool.In_Use (T, ID) then return; end if;
      case Pool.State (T, ID) is
         when Life.Policy_Queued | Life.Disabled =>
            -- A registered context is enabled before it may retire (the
            -- driver's registration path does so); a parked one may retire.
            if Pool.Session_Context (T, ID, Session) = ID and then
              (Pool.State (T, ID) = Life.Policy_Queued or else Random (3) /= 0)
            then
               Pool.Submit (T, ID, Life.Enable, Status);
               if Status = Driver.Backpressure then Backpressures := Backpressures + 1; end if;
               Check (Status in Driver.Queued | Driver.Backpressure, "enable queued or waits");
            else
               Pool.Retire_Session (T, ID, Session);
            end if;
         when Life.Enabled =>
            if Random (4) = 0 then Pool.Retire_Session (T, ID, Session); end if;
         when Life.Ready =>
            -- Opened, never registered: retire; nothing reached the GuC.
            Pool.Retire_Session (T, ID, Session);
         when others => null;
      end case;
      if Pool.Releasable (T, ID) then
         Check (not Known_To_GuC (ID), "released only after the GuC let go of the ID");
         Check (Owed (ID) = None, "released with no answer owed");
         Pool.Release (T, ID, Accepted);
         Check (Accepted and not Pool.In_Use (T, ID), "release frees the ID");
         Released_Count := Released_Count + 1;
      else
         Pool.Release (T, ID, Accepted);
         Check (not Accepted, "release refused while the GuC may still know the context");
      end if;
   end Work_On;
   Fault : Boolean;
   Grown : Boolean;
begin
   if Ada.Command_Line.Argument_Count = 1 then
      Seed := Unsigned_64'Value (Ada.Command_Line.Argument (1));
   end if;
   Pool.Attach (T, Test_Storage.Allocate (Pool.Storage_Bytes (Capacity)));
   Pool.Grow (T, Capacity, Grown);
   Check (Grown, "grow");
   for Round in 1 .. 200_000 loop
      if Random (3) = 0 then Open_One; end if;
      Work_On (Context_ID (First + Random (Capacity)));
      Draining.Tick (T, Fault);
      Check (not Fault, "the drain never quarantines a context the GuC answers");
      Answer_Due;
      Check (Pool.Reply_Words_In_Use (T) <= Pool_Words, "answers in flight fit the pool");
      Check (Pool.Reply_Words_In_Use (T) = Owed_Words, "the pool accounts exactly the answers owed");
      Check (not Pool.Failed (T), "table healthy");
   end loop;
   Put_Line ("opened" & Natural'Image (Opened_Count) & ", released" & Natural'Image (Released_Count) &
             ", IDs reused" & Natural'Image (Reused) & ", back pressure" & Natural'Image (Backpressures) &
             ", checks" & Natural'Image (Checks));
   Check (Reused > 0 and Backpressures > 0 and Released_Count > 1_000, "the run exercised reuse and back pressure");
   if Failures = 0 then
      Put_Line ("Context recycling PASS: IDs reused only after DEREGISTER_DONE, G2H credits per request");
   else
      Put_Line ("Context recycling FAIL:" & Natural'Image (Failures));
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Context_Recycling_Tests;
