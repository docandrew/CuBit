with Vulkan_Submission;
package body Desktop_GPU_Scene with SPARK_Mode is
   use type Compositor_Pool.Ticket;
   use type Vulkan_Glyph_Sources.Key, R.Outcome, D.Frame_Result, D.Poll_Result, D.Capture_Admission;
   procedure Retire (S : in out State; Safe : out Boolean)
     with Global => (In_Out => D.Engine), Pre => Valid (S) and D.Valid,
       Post => Valid (S) and D.Valid and (if Safe then Current (S) = Idle)
   is
      Cancelled : Boolean;
   begin
      Safe := False;
      if not D.Can_Retire_Readers then return; end if;
      -- Drop the CPU snapshot BEFORE attesting that its references retired.
      S.Scene := V.Open (V.Output (S.Scene));
      if S.Reservation /= Compositor_Pool.None then
         D.Cancel_Capture (S.Reservation, Cancelled);
         if not Cancelled then S.Status := Quarantined; return; end if;
         S.Reservation := Compositor_Pool.None;
      end if;
      for I in 1 .. S.Used loop
         R.Release (S.Cache, S.Readers (I).Reader, True);
         if R.Held (S.Cache, S.Readers (I).Reader) then
            S.Status := Quarantined; return;
         end if;
         pragma Loop_Invariant (Valid (S));
      end loop;
      for I in 1 .. S.Images_Used loop
         D.Unpin_Source (S.Images (I).Reader, True, Safe);
         if not Safe then S.Status := Quarantined; return; end if;
         pragma Loop_Invariant (Valid (S));
         pragma Loop_Invariant (D.Valid);
      end loop;
      S.Images_Used := 0;
      S.Used := 0; S.Status := Idle; S.Cold := False; S.Invalid := False; Safe := True;
   end Retire;
   procedure Capture_Repaint (S : State; Plan : out Compositor_Damage.State;
      Accepted : out Boolean) is
   begin
      Accepted := False;
      Compositor_Damage.Clear (Plan);
      if S.Status /= Capturing then return; end if;
      D.Capture_Repaint (S.Reservation, Plan, Accepted);
   end Capture_Repaint;
   procedure Note_Placeholder (S : in out State) is
   begin
      if S.Placeholders < Natural'Last then S.Placeholders := S.Placeholders + 1; end if;
   end Note_Placeholder;
   procedure Prepare_Glyph_Cells (S : in out State; Scale : V.A.G.UI_Scale; Prepared : out Natural) is
   begin
      Prepared := 0;
      if S.Status = Idle then R.Prepare_Cells (S.Cache, Scale, Prepared); end if;
   end Prepare_Glyph_Cells;
   procedure Begin_Frame (S : in out State; Screen : V.A.G.Output;
      Background : V.A.Word; Accepted : out Boolean) is
   begin
      Accepted := S.Status = Idle and then D.Admit_Capture (Screen) = D.Capture_Allowed;
      if not Accepted then return; end if;
      D.Reserve_Capture (Screen, S.Reservation);
      Accepted := S.Reservation /= Compositor_Pool.None;
      if not Accepted then return; end if;
      S.Scene := V.Open (Screen, Background); S.Status := Capturing;
      S.Cold := False; S.Invalid := False;
   end Begin_Frame;
   procedure Pin_Image (S : in out State; Source : Vulkan_Submission.Source_Ticket; Accepted : out Boolean) is
      use type Vulkan_Submission.Source_Ticket, D.Source_Reader;
      Reader : D.Source_Reader;
   begin
      Accepted := False;
      if S.Status /= Capturing or S.Cold or S.Invalid then return; end if;
      for I in 1 .. S.Images_Used loop
         if S.Images (I).Source = Source then Accepted := True; return; end if;
      end loop;
      if S.Images_Used = Image_Count'Last then S.Invalid := True; return; end if;
      D.Pin_Source (Source, Reader);
      if Reader = D.No_Source_Reader then S.Invalid := True; return; end if;
      S.Images_Used := S.Images_Used + 1;
      S.Images (S.Images_Used) := (Source, Reader); Accepted := True;
   end Pin_Image;
   procedure Append (S : in out State; Item : V.Layer; Accepted : out Boolean) is
      use type V.Layer_Kind;
   begin
      Accepted := False;
      if S.Status /= Capturing or S.Cold or S.Invalid then return; end if;
      if Item.Kind = V.Glyph_Mask then S.Invalid := True; return; end if;
      if Item.Kind in V.Textured | V.Straight_Textured | V.Backdrop_Fill | V.Backdrop_Fit | V.Backdrop_Center then
         Pin_Image (S, Item.Source, Accepted);
         if not Accepted then return; end if;
      end if;
      V.Append (S.Scene, Item, Accepted);
      if not Accepted then S.Invalid := True; end if;
   end Append;
   procedure Set_Clip (S : in out State; Area : V.A.G.Physical_Rectangle; Accepted : out Boolean) is
   begin
      Accepted := False;
      if S.Status /= Capturing or S.Cold or S.Invalid then return; end if;
      V.Append_Physical_Clip (S.Scene, Area, Accepted);
      if not Accepted then S.Invalid := True; end if;
   end Set_Clip;
   procedure Add_Glyph (S : in out State; Key : Vulkan_Glyph_Sources.Key;
      Cell : V.A.G.Logical_Rectangle; Tint : V.A.Word; Accepted : out Boolean) is
      Found : Boolean := False;
      Source : Vulkan_Submission.Source_Ticket;
      Reader : R.C.Lease;
      Result : R.Outcome;
   begin
      Accepted := False;
      if S.Status /= Capturing or S.Cold or S.Invalid then return; end if;
      for I in 1 .. S.Used loop
         if S.Readers (I).Key = Key then Found := True; exit; end if;
      end loop;
      if not Found then
         if S.Used = Count'Last then S.Invalid := True; return; end if;
         R.Acquire (S.Cache, Key, Source, Reader, Result);
         case Result is
            when R.Available =>
               S.Used := S.Used + 1; S.Readers (S.Used) := (Key, Reader);
            when R.Uploading | R.Deferred => S.Cold := True; return;
            when R.Rejected => S.Invalid := True; return;
            when R.Unsafe => S.Status := Quarantined; return;
         end case;
      end if;
      D.Capture_Glyph (S.Scene, Key, Cell, Tint, Accepted);
      if not Accepted then S.Invalid := True; end if;
   end Add_Glyph;
   -- Record why a capture ends without a frame, before its snapshot drops.
   procedure Classify (S : in out State)
     with Global => null, Pre => Valid (S), Post => Valid (S) and S.Status = S.Status'Old
   is
   begin
      S.Peak := V.Length'Max (S.Peak, V.Count (S.Scene));
      S.Failure :=
        (if not S.Invalid then (if S.Cold then Cold_Source else No_Failure)
         elsif V.Count (S.Scene) = V.Maximum_Layers or else
           V.Current (S.Scene) = V.Rejected then Layer_Limit
         elsif S.Used = Count'Last then Glyph_Limit
         elsif S.Images_Used = Image_Count'Last then Image_Limit
         else Rejected_Draw);
   end Classify;
   procedure Discard (S : in out State; Result : out Outcome) is
      Safe : Boolean;
   begin
      Result := Rejected;
      if S.Status = Quarantined then Result := Unsafe; return; end if;
      if S.Status in Submitted | Uploading then Result := Pending; return; end if;
      if S.Status = Closed then return; end if;
      if S.Status = Capturing then Classify (S); end if;
      S.Scene := V.Open (V.Output (S.Scene));
      if R.Pending (S.Cache) then S.Status := Uploading; Result := Pending; return; end if;
      Retire (S, Safe);
      if Safe then Result := Retry; else S.Status := Quarantined; Result := Unsafe; end if;
   end Discard;
   procedure Finish (S : in out State; Result : out Outcome) is
      OK : Boolean;
      Frame : D.Frame_Result;
      Was_Invalid : constant Boolean := S.Invalid;
   begin
      Result := Rejected;
      if S.Status = Quarantined then Result := Unsafe; return; end if;
      if S.Status /= Capturing then return; end if;
      if S.Cold or S.Invalid then
         Discard (S, Result);
         if Was_Invalid and Result = Retry then Result := Rejected; end if;
         return;
      end if;
      S.Peak := V.Length'Max (S.Peak, V.Count (S.Scene));
      S.Failure := No_Failure;
      V.Seal (S.Scene, OK);
      if not OK then S.Invalid := True; Discard (S, Result); if Result = Retry then Result := Rejected; end if; return; end if;
      D.Render (S.Scene, Frame, S.Reservation);
      case Frame is
         when D.Submitted => S.Reservation := Compositor_Pool.None; S.Status := Submitted; Result := Pending;
         when D.Failed => S.Status := Quarantined; Result := Unsafe;
         when D.Deferred | D.Rejected =>
            Discard (S, Result);
            if Frame = D.Rejected and Result = Retry then Result := Rejected; end if;
      end case;
   end Finish;
   procedure Poll (S : in out State; Result : out Outcome) is
      Upload : R.Outcome;
      Frame : D.Poll_Result;
      Safe : Boolean;
      Was_Upload : constant Boolean := S.Status = Uploading;
   begin
      Result := Rejected;
      if S.Status = Quarantined then Result := Unsafe; return; end if;
      if S.Status = Uploading then
         R.Poll (S.Cache, Upload);
         if Upload = R.Uploading then Result := Pending; return; end if;
         if Upload /= R.Available then S.Status := Quarantined; Result := Unsafe; return; end if;
      elsif S.Status = Submitted then
         D.Poll_Frame (Frame);
         if Frame = D.Pending then Result := Pending; return; end if;
         if Frame /= D.Completed then S.Status := Quarantined; Result := Unsafe; return; end if;
      else return;
      end if;
      Retire (S, Safe);
      if Safe then Result := (if Was_Upload then Retry else Complete);
      else S.Status := Quarantined; Result := Unsafe; end if;
   end Poll;
   function Software_Ready (S : State) return Boolean is
     (S.Status = Idle and then D.Can_Retire_Readers);
   procedure Complete_Output
     (S : in out State; Poll_Only, Capture_Accepted : Boolean;
      Result : out Output_Completion)
   is
      Finished : Outcome := Rejected;
   begin
      Result := Output_Unsafe;
      if S.Status in Quarantined | Closed then return; end if;
      if S.Status in Submitted | Uploading then
         if not Poll_Only then Result := Output_Pending; return; end if;
         Poll (S, Finished);
      elsif S.Status = Capturing then
         -- An observation is never permission to finish or abandon capture.
         if Poll_Only then return; end if;
         if Capture_Accepted then Finish (S, Finished);
         else Discard (S, Finished); end if;
      end if;
      case Finished is
         when Complete =>
            if Software_Ready (S) then Result := Output_Complete; end if;
         when Rejected | Retry =>
            if Software_Ready (S) then Result := Output_Repaint; end if;
         when Pending => Result := Output_Pending;
         when Unsafe => null;
      end case;
   end Complete_Output;
   procedure Close (S : in out State; Safe : out Boolean) is
      Result : Outcome;
   begin
      Safe := False;
      if S.Status = Closed then Safe := True; return; end if;
      if S.Status = Capturing then Discard (S, Result); end if;
      if S.Status /= Idle then return; end if;
      R.Close (S.Cache, Safe);
      if Safe then S.Status := Closed; else S.Status := Quarantined; end if;
   end Close;
end Desktop_GPU_Scene;
