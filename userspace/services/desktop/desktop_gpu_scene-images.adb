with Desktop_GPU_Scene.Drawing;
package body Desktop_GPU_Scene.Images with SPARK_Mode => Off is
   use type Registry.I.Outcome;
   procedure Capture (Scene : in out State; Sources : in out Registry.State;
      Image : Compositor_Formats.Image; Bytes : Natural;
      Surface : V.A.G.Logical_Rectangle; Damage : V.A.G.Physical_Rectangle;
      Accepted : out Boolean; Over : Boolean := False; Straight_Alpha : Boolean := False)
   is
      Ticket : V.V.Source_Ticket;
      Result : Registry.I.Outcome;
   begin
      Accepted := False;
      if Scene.Status /= Capturing or else Scene.Cold or else Scene.Invalid then return; end if;
      Registry.Ensure (Sources, Image, Bytes, Ticket, Result);
      case Result is
         when Registry.I.Available =>
            Drawing.Image (Scene, Ticket, Surface, Damage, Accepted, Over, Straight_Alpha);
         when Registry.I.Pending | Registry.I.Deferred => Scene.Cold := True;
         when Registry.I.Rejected => Scene.Invalid := True;
         when Registry.I.Unsafe => Scene.Status := Quarantined;
      end case;
   end Capture;
   procedure Complete
     (Scene : in out State; Sources : in out Registry.State;
      Copy : in out Output.R.State; Target : Compositor_Formats.Image;
      Bytes : Compositor_Formats.Byte_Count; Writer : Output.R.P.Ticket;
      Poll_Only, Capture_Accepted : Boolean; Byte_Budget : Natural;
      Result : out Output_Completion)
   is
      Progress : Registry.I.Outcome;
   begin
      Result := Output_Unsafe;
      if Scene.Status = Quarantined then return; end if;
      if Registry.Upload_Work (Sources) then
         if Poll_Only then
            Registry.Poll (Sources, Progress);
            if Progress = Registry.I.Unsafe then Scene.Status := Quarantined; return; end if;
         end if;
         Result := Output_Pending; return;
      end if;
      -- A failed cold capture has never submitted a frame. Once its external
      -- uploads drain it must be discarded even on an observation-only turn.
      if Scene.Status = Capturing and then (Scene.Cold or Scene.Invalid) then
         Output.Pump (Scene, Copy, Target, Bytes, Writer, False, False, Byte_Budget, Result);
      else
         Output.Pump (Scene, Copy, Target, Bytes, Writer, Poll_Only,
           Capture_Accepted, Byte_Budget, Result);
      end if;
   end Complete;
end Desktop_GPU_Scene.Images;
