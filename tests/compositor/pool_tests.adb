with Ada.Text_IO;
with Interfaces; use Interfaces;
with Compositor_Pool; use Compositor_Pool;
procedure Pool_Tests is
   S : State := Open (9);
   A, B, C, D, Got : Ticket;
   Last : ID := 0;
   GPU_Held, Display_Held : Slot := 0;
   procedure Acquire_Checked (T : out Ticket) is
   begin
      Acquire (S, T);
      pragma Assert (T /= None and Writable (S, T));
      pragma Assert (T.Buffer /= GPU_Held and T.Buffer /= Display_Held);
      pragma Assert (T.Serial > Last and T.Epoch = 9);
      Last := T.Serial;
   end Acquire_Checked;
   procedure Render (T : Ticket; Result : Render_Outcome := Completed) is
   begin
      pragma Assert (GPU_Held = 0);
      Start_Render (S, T);
      GPU_Held := T.Buffer;
      pragma Assert (not Writable (S, T));
      Acquire (S, Got);
      pragma Assert (Got = None);
      Finish_Render (S, T, Result);
      GPU_Held := 0;
   end Render;
begin
   -- Hold scanout while two newer frames complete. Only newest may be sent.
   for Cycle in 1 .. 3_000 loop
      Acquire_Checked (A); Render (A); Present (S, Got);
      pragma Assert (Got = A); Display_Held := A.Buffer;
      Acquire_Checked (B); Render (B);
      Acquire_Checked (C);
      pragma Assert (C.Buffer /= A.Buffer and C.Buffer /= B.Buffer);
      Render (C);
      pragma Assert (Ready (S) = C);
      Acquire_Checked (D);
      pragma Assert (D.Buffer = B.Buffer);
      Render (D, Failed_Quiescent);
      pragma Assert (Ready (S) = C);
      Present (S, Got);
      pragma Assert (Got = None and Displayed (S) = A);
      Retire_Display (S, A, True); Display_Held := 0;
      Present (S, Got);
      pragma Assert (Got = C); Display_Held := C.Buffer;
      Retire_Display (S, C, True); Display_Held := 0;
      pragma Assert (Valid (S) and not Faulted (S));
   end loop;
   -- A ready frame can present while a newer frame is still rendering.
   S := Open (9); Acquire (S, A); Start_Render (S, A);
   Finish_Render (S, A, Completed);
   Acquire (S, B); Start_Render (S, B);
   Present (S, Got);
   pragma Assert (Got = A and Rendering (S) and Writer (S) = B);
   Finish_Render (S, B, Completed);
   Retire_Display (S, A, True); Present (S, Got);
   pragma Assert (Got = B);
   -- A matching but non-releasing acknowledgement retains Display ownership.
   Retire_Display (S, B, False);
   pragma Assert (Faulted (S) and Displayed (S) = B);
   Retire_Display (S, B, True);
   pragma Assert (Faulted (S) and Displayed (S) = B);
   -- An old display fence cannot retire a later frame in the same allocation.
   S := Open (9); Acquire (S, A); Start_Render (S, A);
   Finish_Render (S, A, Completed); Present (S, Got);
   Retire_Display (S, A, True);
   Acquire (S, B); Start_Render (S, B);
   Finish_Render (S, B, Completed); Present (S, Got);
   Retire_Display (S, A, True);
   pragma Assert (Faulted (S) and Displayed (S) = B);
   for Fault in 1 .. 5 loop
      S := Open (9); Acquire (S, A); Start_Render (S, A);
      case Fault is
         when 1 => Finish_Render (S, A, Unknown);
         when 2 => Finish_Render (S, (A.Buffer, 8, A.Serial), Completed);
         when 3 => Finish_Render (S, (A.Buffer, 9, A.Serial + 1), Completed);
         when 4 => Start_Render (S, A);
         when 5 => Retire_Display (S, A, True);
      end case;
      pragma Assert (Faulted (S) and Writer (S) = A);
      Finish_Render (S, A, Completed);
      Acquire (S, Got);
      pragma Assert (Got = None and Faulted (S) and not Writable (S, A));
   end loop;
   for Released in Boolean loop
      S := Open (9); Acquire (S, A); Start_Render (S, A);
      Finish_Render (S, A, Completed); Present (S, Got);
      Retire_Display (S, (A.Buffer, 8, A.Serial), Released);
      pragma Assert (Faulted (S) and Displayed (S) = A);
      Retire_Display (S, A, True);
      pragma Assert (Faulted (S) and Displayed (S) = A);
   end loop;
   -- Old valid ticket cannot release a reused slot's newer rendering.
   S := Open (9); Acquire (S, A); Start_Render (S, A);
   Finish_Render (S, A, Completed); Present (S, Got);
   Retire_Display (S, A, True);
   Acquire (S, B); Start_Render (S, B);
   pragma Assert (B.Buffer = A.Buffer and B.Serial /= A.Serial);
   Finish_Render (S, A, Completed);
   pragma Assert (Faulted (S) and Writer (S) = B);
   declare
      Pixels : array (Live_Slot, 1 .. 256) of ID := (others => (others => 0));
      Visible : Ticket := None;
      Old_State : State;
      Previous : Ticket;
      procedure Paint (T : Ticket) is
      begin
         -- Independent model of the allocation currently scanned out.
         pragma Assert (T /= None and T.Buffer /= Visible.Buffer);
         pragma Assert (Writable (S, T));
         for I in 1 .. 256 loop Pixels (T.Buffer, I) := T.Serial * 256 + ID (I); end loop;
         if Visible /= None then
            for I in 1 .. 256 loop
               pragma Assert (Pixels (Visible.Buffer, I) = Visible.Serial * 256 + ID (I));
            end loop;
         end if;
      end Paint;
      procedure Complete_CPU (T : Ticket) is
      begin Start_Render (S, T); Finish_Render (S, T, Completed); end;
      procedure Latch (T : Ticket) is
      begin
         Latch_Display (S, T, Visible, True);
         pragma Assert (not Faulted (S) and Front (S) = T and Displayed (S) = None);
         Visible := T;
         for I in 1 .. 256 loop
            pragma Assert (Pixels (Visible.Buffer, I) = Visible.Serial * 256 + ID (I));
         end loop;
      end;
   begin
      S := Open (17);
      Acquire (S, A); Paint (A); Complete_CPU (A); Present (S, Got); Latch (Got);
      for Cycle in 1 .. 5_000 loop
         Previous := Visible;
         Acquire (S, A); Paint (A); Complete_CPU (A); Present (S, Got);
         pragma Assert (Got = A and Front (S) = Previous);
         Acquire (S, B); Paint (B); Complete_CPU (B);
         -- Front + pending + ready exhaust exactly three allocations.
         Old_State := S; Acquire (S, Got);
         pragma Assert (Got = None and S = Old_State and not Has_Free (S));
         pragma Assert (not Writable (S, Previous) and not Writable (S, A));
         Latch (A);
         Acquire (S, C);
         pragma Assert (C.Buffer = Previous.Buffer and C.Serial /= Previous.Serial);
         Paint (C); Start_Render (S, C);
         Present (S, Got); pragma Assert (Got = B and Rendering (S));
         -- Rendering and pending presentation coexist beside the held front.
         Finish_Render (S, C, Completed);
         Old_State := S; Acquire (S, Got); pragma Assert (Got = None and S = Old_State);
         Latch (B);
         Present (S, Got); pragma Assert (Got = C); Latch (C);
         pragma Assert (Valid (S));
      end loop;
      -- New input may replace ready work while front and pending stay held.
      for Cycle in 1 .. 1_000 loop
         Acquire (S, A); Paint (A); Complete_CPU (A); Present (S, Got);
         Acquire (S, B); Paint (B); Complete_CPU (B);
         for Update in 1 .. 8 loop
            Previous := B;
            Acquire (S, B, Replace_Ready => True);
            pragma Assert (B.Buffer = Previous.Buffer and B.Serial > Previous.Serial);
            pragma Assert (Front (S) = Visible and Displayed (S) = A and Ready (S) = None);
            Paint (B); Complete_CPU (B);
            pragma Assert (Ready (S) = B);
         end loop;
         Latch (A); Present (S, Got); pragma Assert (Got = B); Latch (Got);
      end loop;
      -- Failed quiescent rendering after reclamation has no old ready pixels
      -- to resurrect; the front and already pending frame remain held.
      Acquire (S, A); Paint (A); Complete_CPU (A); Present (S, Got);
      Acquire (S, B); Paint (B); Complete_CPU (B);
      Acquire (S, C, Replace_Ready => True); Paint (C);
      Start_Render (S, C); Finish_Render (S, C, Failed_Quiescent);
      pragma Assert (Ready (S) = None and Front (S) = Visible and Displayed (S) = A);
      Latch (A);
      Ada.Text_IO.Put_Line ("pool-latest: PASS 8000 ready replacements, front/pending exclusion and failed-render retention");
      Retire_Front (S, Visible, True);
      pragma Assert (Front (S) = None and not Faulted (S));
      for Fault in 1 .. 9 loop
         S := Open (17); Visible := None;
         Acquire (S, A); Paint (A); Complete_CPU (A); Present (S, Got); Latch (Got);
         Acquire (S, B); Paint (B); Complete_CPU (B); Present (S, Got);
         case Fault is
            when 1 => Latch_Display (S, (B.Buffer, B.Epoch, B.Serial + 1), A, True);
            when 2 => Latch_Display (S, (B.Buffer, B.Epoch + 1, B.Serial), A, True);
            when 3 => Latch_Display (S, A, A, True);
            when 4 => Latch_Display (S, B, (A.Buffer, A.Epoch, A.Serial + 1), True);
            when 5 => Latch_Display (S, B, None, True);
            when 6 => Latch_Display (S, B, A, False);
            when 7 => Retire_Front (S, B, True);
            when 8 => Retire_Front (S, A, False);
            when 9 => Retire_Front (S, (A.Buffer, A.Epoch + 1, A.Serial), True);
         end case;
         pragma Assert (Faulted (S) and Front (S) = A and Displayed (S) = B);
         Old_State := S;
         Latch_Display (S, B, A, True); Retire_Front (S, A, True);
         Acquire (S, Got); pragma Assert (Got = None and S = Old_State);
      end loop;
      -- A duplicate successful latch cannot retire the new visible target.
      S := Open (17); Visible := None;
      Acquire (S, A); Paint (A); Complete_CPU (A); Present (S, Got); Latch (Got);
      Latch_Display (S, A, None, True);
      pragma Assert (Faulted (S) and Front (S) = A);
      Ada.Text_IO.Put_Line ("pool-front: PASS 15001 latches, visible pixels, three-target exhaustion and sticky fault retention");
   end;
   Ada.Text_IO.Put_Line ("pool: PASS 3000 held-display/mailbox cycles and fault traces");
end Pool_Tests;
