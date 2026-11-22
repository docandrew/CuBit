pragma Ada_2022;
with CuBit.Output_Discovery;

--  DISP-009/010: the monitors a GPU reports attached but not lit
--  (CuBit.Output_Discovery.Connected_Not_Lit) in display.svc's catalog.
--  They follow the broker's own outputs, in the GPU's order, so Settings
--  can name them; nothing presents to them. A GPU catalog read at startup
--  or after a GPU catalog change (a hot-plug event) replaces them all, and
--  Changed says whether the broker's catalog differs, so a connect or a
--  disconnect bumps the catalog revision exactly once.
package CuBit.Detected_Outputs with SPARK_Mode, Pure is
   package OD renames CuBit.Output_Discovery;
   use type OD.Output_Role, OD.Connection_State, OD.Output_Source, OD.Description;

   function Not_Lit (Item : OD.Description) return Boolean is
     (Item.Role = OD.Detected_Only and then Item.Connection = OD.Connected_Not_Lit);

   function Well_Formed (Data : OD.Catalog) return Boolean is
     (for all I in 1 .. Data.Count => OD.Consistent (Data.Items (I)));

   --  The listed entries are the same (slots past Count do not count).
   function Same (Left, Right : OD.Catalog) return Boolean is
     (Left.Count = Right.Count and then
      (for all I in 1 .. Left.Count => Left.Items (I) = Right.Items (I)));

   --  Current without its not-lit entries from Source, then Fresh's not-lit
   --  entries from Source in order (as many as fit). Other entries keep
   --  their place.
   procedure Replace
     (Current : in out OD.Catalog; Source : OD.Output_Source; Fresh : OD.Catalog;
      Changed : out Boolean)
     with Pre => Well_Formed (Current) and then Well_Formed (Fresh),
          Post => Well_Formed (Current) and then
            Changed = not Same (Current, Current'Old) and then
            (for all I in 1 .. Current.Count =>
               (if Current.Items (I).Source = Source and then Not_Lit (Current.Items (I))
                then (for some J in 1 .. Fresh.Count => Fresh.Items (J) = Current.Items (I))));

   --  The not-lit entry of Source with native number Native: its index, or
   --  zero.
   function Find
     (Data : OD.Catalog; Source : OD.Output_Source; Native : OD.Native_Output_Number)
      return OD.Output_Count
     with Post => (if Find'Result /= 0 then Find'Result <= Data.Count and then
                     Not_Lit (Data.Items (Find'Result)) and then
                     Data.Items (Find'Result).Source = Source and then
                     Data.Items (Find'Result).Native_Number = Native);
end CuBit.Detected_Outputs;
