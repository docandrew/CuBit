with Compositor_Glyph_Cache;
with Compositor_Glyph_Storage;
with Compositor_Glyph_Placement;
with Compositor_Mask_Batch;
with Mesa_Cache;
package Compositor_Glyph_Renderer with SPARK_Mode is
   package C is new Compositor_Glyph_Cache;
   package P renames Compositor_Glyph_Placement;
   package B renames Compositor_Mask_Batch;
   package Storage renames Compositor_Glyph_Storage;
   package Software renames Storage.Software;
   use type Software.Word;
   type State is limited private with Default_Initial_Condition => Valid (State);
   function Valid (S : State) return Boolean;
   function Charged (S : State) return C.Byte_Count;
   function Queued (S : State) return B.Count;
   function Enabled (S : State) return Boolean;
   function Software_Active (S : State) return Boolean;
   -- Cancel unsent commands, retire every mask import, keep raster backing.
   -- The outer owner may then shut down the shared Mesa context. Unknown
   -- completion refuses the transition and retains all backing for recovery.
   procedure Use_Software (S : in out State; Views : in out Mesa_Cache.State; Safe : out Boolean)
     with Pre => Valid (S), Post => Valid (S) and Charged (S) = Charged (S)'Old and
       (if Safe then Software_Active (S) and Queued (S) = 0 and Mesa_Cache.Can_Retire (Views) and
          (for all I in Mesa_Cache.Mask_Slot => Mesa_Cache.Empty (Views, I)));
   procedure Paint (S : in out State; Views : in out Mesa_Cache.State;
                    Key : C.Key; Screen : P.G.Output; Origin : P.G.Logical_Point;
                    Damage : P.G.Physical_Rectangle; Target : in out Software.Pixels;
                    Pitch : Positive; Tint : Software.Word; Success : out Boolean)
     with Pre => Valid (S) and Software.Fits_Target (Screen, Target, Pitch),
       Post => Valid (S) and
         (for all I in Target'Range =>
           (if not Success or else not Software.Inside (I, Pitch, Software.Bounds (Screen, Origin, Damage)) then
              Target (I) = Target'Old (I)));
   -- Views must be the SAME context throughout this owner's lifetime. Its mask
   -- slots are exclusively owned here. Flush before any intervening non-text
   -- draw. On quiescent draw failure, repaint the affected target in fallback.
   procedure Queue
     (S : in out State; Views : in out Mesa_Cache.State; Target : Mesa_Cache.Target_Slot;
      Key : C.Key; Screen : P.G.Output; Origin : P.G.Logical_Point;
      Damage : P.G.Physical_Rectangle; Tint : B.A.Word; Success : out Boolean)
     with Pre => Valid (S), Post => Valid (S);
   procedure Flush (S : in out State; Views : in out Mesa_Cache.State; Success : out Boolean)
     with Pre => Valid (S), Post => Valid (S) and Software_Active (S) = Software_Active (S)'Old;
   procedure Shutdown (S : in out State; Views : in out Mesa_Cache.State; Safe : out Boolean)
     with Pre => Valid (S), Post => Valid (S) and then
       (if Safe then Charged (S) = 0 and Queued (S) = 0 and Mesa_Cache.Can_Retire (Views));
private
   type Leases is array (B.Index) of C.Lease;
   type State is limited record
      Registry : C.State := C.Open (524_288);
      Backing : Storage.State;
      Packet : B.Packet;
      Reading : Leases := (others => C.No_Lease);
      Target : Mesa_Cache.Target_Slot := 0;
      Running : Boolean := True;
      CPU : Boolean := False;
   end record with Type_Invariant => Consistent (State);
   function Consistent (S : State) return Boolean is
     (C.Valid (S.Registry) and then B.Valid (S.Packet) and then
       (if S.CPU then S.Packet.Length = 0) and then
       (for all I in 1 .. S.Packet.Length =>
          C.Reads_Slot (S.Registry, S.Reading (I), S.Packet.Items (I).Mask + 1)));
   function Valid (S : State) return Boolean is (Consistent (S));
   function Charged (S : State) return C.Byte_Count is (C.Charged (S.Registry));
   function Queued (S : State) return B.Count is (S.Packet.Length);
   function Enabled (S : State) return Boolean is (S.Running);
   function Software_Active (S : State) return Boolean is (S.CPU);
end Compositor_Glyph_Renderer;
