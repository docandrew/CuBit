with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_Render_Admission; use Intel_Render_Admission;
with Intel_GPU_Render_Control;
procedure Render_Admission_Tests is
   package GPU renames Intel_GPU_Render_Control;
   Target : constant Unsigned_64 := 7 * 2 ** 32 + 42;
begin
   for Cancel_At in 0 .. 3 loop
      declare
         Item : Transaction;
         Driver : GPU.Controller;
         Accepted, Consumed : Boolean;
         Next_ID : Unsigned_64 := 0;
         procedure Exchange (Cancel_Pending : Boolean := False) is
            Payload : constant Words := Request (Item);
            Reply : GPU.Words;
         begin
            Next_ID := Next_ID + 1;
            Prepare (Item, Next_ID, Accepted); pragma Assert (Accepted);
            Submitted (Item, True);
            GPU.Handle (Driver, 10, 99, True, GPU.Label, 4, 0, 0,
              GPU.Words (Payload), Reply, Recipient_Ready => True);
            if Cancel_Pending then Cancel (Item); end if;
            Complete (Item, Next_ID + 1, True, Words (Reply), Consumed);
            pragma Assert (not Consumed);
            Complete (Item, Next_ID, True, Words (Reply), Consumed);
            pragma Assert (Consumed);
            Complete (Item, Next_ID, True, Words (Reply), Consumed);
            pragma Assert (not Consumed);
         end Exchange;
      begin
         GPU.Bind (Driver, 10, 99);
         Start (Item, Target);
         Exchange (Cancel_At = 1);
         pragma Assert (Identity (Item) = Target and Session (Item) /= 0);
         if Cancel_At = 1 then
            pragma Assert (State (Item) = Abort_Ready);
         else
            pragma Assert (State (Item) = Delegate_Ready);
            Delegated (Item, Cancel_At /= 2);
            if Cancel_At /= 2 then Exchange (Cancel_At = 3); end if;
         end if;
         if Cancel_At = 0 then
            pragma Assert (State (Item) = Active);
            pragma Assert (GPU.Resolve (Driver, 42, Session (Item)) /= 0);
            Cancel (Item);
         end if;
         pragma Assert (State (Item) = Abort_Ready);
         Exchange;
         pragma Assert (State (Item) = Failed);
         pragma Assert (GPU.Resolve (Driver, 42, Session (Item)) = 0);
         Start (Item, Target + 2 ** 32);
         pragma Assert (Identity (Item) = Target and State (Item) = Failed);
      end;
   end loop;
   for Malformed in Boolean loop
      declare
         Item : Transaction;
         OK : Boolean;
      begin
         Start (Item, Target);
         Prepare (Item, 1, OK); pragma Assert (OK);
         Submitted (Item, True);
         Complete (Item, 1, not Malformed, [0, 1, 0, 0], OK);
         pragma Assert (OK and State (Item) = Quarantined);
      end;
   end loop;
   Put_Line ("RENDER-ADMISSION: PASS actual controller, mock receipt/delegation");
end Render_Admission_Tests;
