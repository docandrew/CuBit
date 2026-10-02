with System;
with Compositor_Glyph_Cache;
with Compositor_Glyph_Storage;
-- Serialized CPU glyph-cache owner. Reuses the compositor's proved cache and
-- 512 KiB arena. A view belongs to this SAME owner until Finish; consumers must
-- stop reading its address before Finish. No GPU/foreign reader is permitted.
package Client_Glyphs with SPARK_Mode is
   package C is new Compositor_Glyph_Cache;
   package L renames C.L;
   package Storage renames Compositor_Glyph_Storage;
   Budget : constant := 524_288;
   type State is limited private with Default_Initial_Condition => Valid (State);
   type View is limited private;
   function Valid (S : State) return Boolean with Ghost;
   function Charged (S : State) return C.Byte_Count;
   function Readers (S : State) return Natural;
   function Ready (V : View) return Boolean;
   function Belongs (S : State; V : View) return Boolean;
   function Pixels (V : View) return System.Address;
   function Raster (V : View) return L.Layout;
   procedure Read (S : in out State; Key : C.Key; V : in out View)
     with Pre => Valid (S) and not Ready (V),
       Post => Valid (S) and Charged (S) <= Budget and
         (if Ready (V) then Belongs (S, V));
   procedure Finish (S : in out State; V : in out View)
     with Pre => Valid (S), Post => Valid (S) and
       (if Belongs (S, V)'Old or not Ready (V)'Old then not Ready (V) else Ready (V)) and
       Charged (S) = Charged (S)'Old;
   -- Terminal close; pinned masks stay charged until their views are finished.
   procedure Close (S : in out State; Retired : out Boolean)
     with Pre => Valid (S), Post => Valid (S) and
       (Retired = (Charged (S) = 0));
private
   use type C.Lease, System.Address;
   type State is limited record
      Registry : C.State := C.Open (Budget);
      Backing : Storage.State;
      Running : Boolean := True;
   end record;
   type View is limited record
      Mask : C.Token := C.No_Token;
      Reading : C.Lease := C.No_Lease;
      Address : System.Address := System.Null_Address;
      Layout : L.Layout := L.Plan ((1, 1));
   end record;
   function Valid (S : State) return Boolean is
     (C.Valid (S.Registry) and then C.Limit (S.Registry) = Budget);
   function Charged (S : State) return C.Byte_Count is (C.Charged (S.Registry));
   function Readers (S : State) return Natural is (C.Reader_Count (S.Registry));
   function Ready (V : View) return Boolean is (V.Reading /= C.No_Lease);
   function Belongs (S : State; V : View) return Boolean is
     (Ready (V) and then V.Address /= System.Null_Address and then
      C.Current (S.Registry, V.Mask) and then
      C.Reads_Slot (S.Registry, V.Reading, V.Mask.Position) and then
      Storage.Has (S.Backing, V.Mask.Position) and then
      Storage.Pixels (S.Backing, V.Mask.Position) = V.Address);
   function Pixels (V : View) return System.Address is (V.Address);
   function Raster (V : View) return L.Layout is (V.Layout);
end Client_Glyphs;
