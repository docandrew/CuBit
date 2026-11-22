with Ada.Text_IO;
with Interfaces;
with Input_Pending; use Input_Pending;
procedure Pending_Tests is
   use type Interfaces.Unsigned_64;
   Q : Queue;
   Lost : Boolean;
   Delivered : Natural := 0;
   Position : Integer := 360;
   Saved : Item;
   Expected : Word := 1;
begin
   pragma Assert (Count (Q) = 0);
   pragma Assert (Next_Sequence (Word'Last) = 1);
   pragma Assert (Wake_Deadline (Q, 100, Word'Last) = Word'Last);
   pragma Assert (Wake_Deadline (Q, 100, 90) = 90);
   -- Four refused -70 motion reports reproduce the native 280px drift.
   -- Retrying after input stops must deliver all four, without marking a
   -- recovered transport refusal as an actual lost source report.
   for I in 1 .. 4 loop
      Append (Q, Word ((-70) mod 4096) * 256, Lost);
      pragma Assert (not Lost);
   end loop;
   pragma Assert (Wake_Deadline (Q, 100, Word'Last) = 101);
   pragma Assert (Wake_Deadline (Q, 100, 99) = 99);
   pragma Assert (Wake_Deadline (Q, 100, 100) = 100);
   pragma Assert (Wake_Deadline (Q, Word'Last, Word'Last) = Word'Last);
   pragma Assert (Wake_Deadline (Q, Word'Last - 1, Word'Last) = Word'Last);
   Saved := Element (Q, 0);
   for Refusal in 1 .. 100 loop
      pragma Assert (Element (Q, 0) = Saved and Count (Q) = 4);
   end loop;
   while Count (Q) > 0 loop
      pragma Assert (Element (Q, 0).Sequence = Word (Delivered + 1));
      pragma Assert (not Element (Q, 0).Recover);
      Position := Position + Integer (Element (Q, 0).Payload / 256) - 4096;
      Acknowledge (Q);
      Delivered := Delivered + 1;
   end loop;
   pragma Assert (Position = 80 and Delivered = 4);
   -- Repeated circular wrap, varied backlog, and opaque complete payloads:
   -- button edges, opposite motion and wheel packets cannot be merged away.
   Expected := 5;
   for Batch in 1 .. 200 loop
      for I in 1 .. Capacity loop
         Append (Q, Expected + Word (I - 1), Lost);
         pragma Assert (not Lost);
      end loop;
      for I in 1 .. Capacity loop
         pragma Assert (Element (Q, 0).Payload = Expected);
         pragma Assert (Element (Q, 0).Sequence = Expected);
         Acknowledge (Q);
         Expected := Expected + 1;
         if I mod 3 = 0 then
            -- Exercise nonzero heads without modifying the remaining order.
            Saved := Element (Q, 0);
            pragma Assert (Saved.Payload = Expected);
         end if;
      end loop;
   end loop;
   -- Full retention is finite: latest button-up state survives overload and
   -- explicitly reports the discarded history, even with no subsequent IRQ.
   for I in 1 .. Capacity loop
      Append (Q, 1, Lost);
      pragma Assert (not Lost);
   end loop;
   Append (Q, 0, Lost);
   pragma Assert (Lost and Count (Q) = 1);
   pragma Assert (Element (Q, 0).Payload = 0 and Element (Q, 0).Recover);
   Saved := Element (Q, 0);
   Acknowledge (Q);
   Append (Q, 16#FF00_0000_0000_00FF#, Lost);
   pragma Assert (not Lost and not Element (Q, 0).Recover);
   pragma Assert (Element (Q, 0).Sequence = Next_Sequence (Saved.Sequence));
   -- Consumer replacement cannot inherit stale pending deltas or buttons.
   Saved := Element (Q, 0);
   Reset (Q);
   pragma Assert (Count (Q) = 0);
   Append (Q, 0, Lost);
   pragma Assert (not Lost and Element (Q, 0).Recover);
   pragma Assert (Element (Q, 0).Sequence = Next_Sequence (Saved.Sequence));
   Ada.Text_IO.Put_Line ("Queue bytes:" & Natural'Image (Q'Size / 8));
   Ada.Text_IO.Put_Line ("INPUT-PENDING: PASS retention/order/overflow/reset/wrap");
end Pending_Tests;
