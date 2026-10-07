package body Desktop_GPU_Scene.Output with SPARK_Mode => Off is
   use type R.Phase, Compositor_Formats.Word, Compositor_Formats.Byte_Count;
   procedure Pump
     (Scene : in out State; Copy : in out R.State;
      Target : Compositor_Formats.Image; Bytes : Compositor_Formats.Byte_Count;
      Writer : R.P.Ticket; Poll_Only, Capture_Accepted : Boolean;
      Byte_Budget : Natural; Result : out Output_Completion)
   is
      Finished : Output_Completion;
      OK : Boolean;
   begin
      Result := Output_Unsafe;
      if not Compositor_Formats.Valid (Target, Bytes) or else Target.Writable /= 1 or else
         Bytes > Compositor_Formats.Byte_Count (Natural'Last) then return; end if;
      if R.Current (Copy) /= R.Idle and then not R.Matches
        (Copy, Writer, Target.Pixels, R.G.Pixel_Edge (Target.Width),
         R.G.Pixel_Edge (Target.Height), Natural (Bytes), Natural (Target.Pitch))
      then return; end if;
      case R.Current (Copy) is
         when R.Idle =>
            Complete_Output (Scene, Poll_Only, Capture_Accepted, Finished);
            if Finished /= Output_Complete then Result := Finished; return; end if;
            R.Begin_Transfer (Copy, Writer, Target.Pixels,
              R.G.Pixel_Edge (Target.Width), R.G.Pixel_Edge (Target.Height),
              Natural (Bytes), Natural (Target.Pitch), OK);
            if not OK then
               if R.Current (Copy) = R.Idle and then Software_Ready (Scene) then
                  Result := Output_Repaint;
               end if;
               return;
            end if;
         when R.Transferring =>
            if Poll_Only then R.Poll_Transfer (Copy, Writer); end if;
         when R.Copying =>
            if Poll_Only then R.Advance (Copy, Writer, Byte_Budget, OK); end if;
         when R.Complete | R.Repaint => null;
         when R.Quarantined => return;
      end case;
      case R.Current (Copy) is
         when R.Complete | R.Repaint =>
            Finished := (if R.Current (Copy) = R.Complete then Output_Complete else Output_Repaint);
            R.Acknowledge (Copy, Writer, OK);
            if OK then Result := Finished; end if;
         when R.Transferring | R.Copying => Result := Output_Pending;
         when R.Idle | R.Quarantined => null;
      end case;
   end Pump;
end Desktop_GPU_Scene.Output;
