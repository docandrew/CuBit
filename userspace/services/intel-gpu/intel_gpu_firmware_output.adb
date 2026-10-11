package body Intel_GPU_Firmware_Output with SPARK_Mode is
   function Select_Output
     (Planes : Primaries; Table_Bytes, FB_Width, FB_Height, FB_Pitch : Unsigned_64)
      return Selection
   is
      Wanted : Geometry;
      Found : Boolean := False;
      Chosen : Topology.Pipe := Topology.Pipe'First;
   begin
      if not Framebuffer_Valid (FB_Width, FB_Height, FB_Pitch) then
         return (State => Invalid_Framebuffer);
      end if;
      Wanted := (Extent (FB_Width), Extent (FB_Height), Pitch_Bytes (FB_Pitch));
      for P in Topology.Pipe loop
         if Matches (Planes (P), Table_Bytes, Wanted) then
            if Found then
               pragma Assert (Chosen /= P);
               return (State => Several_Pipes);
            end if;
            Found := True;
            Chosen := P;
         end if;
         pragma Loop_Invariant
           (if Found then
              Chosen <= P and then Matches (Planes (Chosen), Table_Bytes, Wanted) and then
              (for all Q in Topology.Pipe'First .. P =>
                 (if Q /= Chosen then not Matches (Planes (Q), Table_Bytes, Wanted)))
            else
              (for all Q in Topology.Pipe'First .. P =>
                 not Matches (Planes (Q), Table_Bytes, Wanted)));
      end loop;
      if not Found then
         return (State => No_Pipe);
      end if;
      return (State => Selected, Pipe => Chosen, Shape => Wanted,
              Surface => Planes (Chosen).Before.Live_Surface,
              Control => Planes (Chosen).Before.Control);
   end Select_Output;
end Intel_GPU_Firmware_Output;
