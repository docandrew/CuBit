package body Compositor_Text with SPARK_Mode is
   use type A.Word, A.Signed;
   function Edge (Value : A.Signed; Limit : G.Physical_Extent) return G.Pixel_Edge is (G.Pixel_Edge (Value))
     with Pre => Value in 0 .. A.Signed (Limit),
       Post => Edge'Result <= Limit and A.Signed (Edge'Result) = Value;
   function Clip (Screen : G.Output; Cell : G.Logical_Rectangle;
                  Damage : G.Physical_Rectangle) return G.Physical_Rectangle is
      Full : constant A.Result := A.Plan (Screen, Cell, True);
   begin
      if not Full.Visible then return G.Empty; end if;
      declare Part : constant A.Result := A.Clip (Full.Value, Screen.Width, Screen.Height, Damage); begin
         if not Part.Visible then return G.Empty; end if;
         return (G.Pixel_Edge (Part.Value.Clip_X), G.Pixel_Edge (Part.Value.Clip_Y),
           Edge (A.Signed (Part.Value.Clip_X) + A.Signed (Part.Value.Clip_W), Screen.Width),
           Edge (A.Signed (Part.Value.Clip_Y) + A.Signed (Part.Value.Clip_H), Screen.Height));
      end;
   end Clip;
end Compositor_Text;
