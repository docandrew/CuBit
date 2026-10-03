with Compositor_Glyph_Cache;
with Compositor_Glyph_Storage;
package Compositor_Software_Text with SPARK_Mode is
   package C is new Compositor_Glyph_Cache (Maximum_Readers => 1);
   package Storage renames Compositor_Glyph_Storage;
   package Software renames Storage.Software;
   package G renames Storage.L.G;
   use type Software.Word;
   -- Synchronous CPU owner: no foreign readers or queued rendering work.
   -- Raster storage and accounting share the existing 512 KiB upper bound.
   type State is limited private with Default_Initial_Condition => Valid (State);
   function Valid (S : State) return Boolean;
   function Charged (S : State) return C.Byte_Count;
   function Quiescent (S : State) return Boolean;
   function Enabled (S : State) return Boolean;
   procedure Disable (S : in out State)
     with Pre => Valid (S), Post => Valid (S) and not Enabled (S);
   procedure Paint
     (S : in out State; Key : C.Key; Screen : G.Output;
      Origin : G.Logical_Point; Damage : G.Physical_Rectangle;
      Target : in out Software.Pixels; Pitch : Positive;
      Tint : Software.Word; Success : out Boolean)
     with Pre => Valid (S) and Software.Fits_Target (Screen, Target, Pitch),
       Post => Valid (S) and
         (for all I in Target'Range =>
           (if not Success or else not Software.Inside
             (I, Pitch, Software.Bounds (Screen, Origin, Damage)) then
                Target (I) = Target'Old (I)));
   procedure Shutdown (S : in out State; Success : out Boolean)
     with Pre => Valid (S), Post => Valid (S) and
       (if Success then Charged (S) = 0);
private
   function Consistent (S : State) return Boolean;
   type State is limited record
      Registry : C.State := C.Open (524_288);
      Backing : Storage.State;
      Running : Boolean := True;
   end record with Type_Invariant => Consistent (State);
   function Consistent (S : State) return Boolean is
     (C.Valid (S.Registry) and then C.Reader_Count (S.Registry) = 0);
   function Valid (S : State) return Boolean is (Consistent (S));
   function Enabled (S : State) return Boolean is (S.Running);
   function Quiescent (S : State) return Boolean is (C.Reader_Count (S.Registry) = 0);
   function Charged (S : State) return C.Byte_Count is (C.Charged (S.Registry));
end Compositor_Software_Text;
