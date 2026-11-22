with Desktop_Image_Source;
with Compositor_Formats;
with Compositor_Source_Content;
with Compositor_Source_Residency;
with System;
-- Persistent client-surface sources over the configured client slots, keyed
-- by surface (Source_Key), NOT by CPU mapping. Each key owns at most one
-- slot whose GPU image is updated in place for every new content version;
-- steady-state publication allocates nothing. A slot is reused only by
-- evicting the least-recently-used key that no scene reads and no pass
-- writes. CPU mappings are only read synchronously into staging, so Forget
-- returns them without GPU work unless a pass still copies from them.
-- This is NOT a device memory quota or global BO limit.
package Desktop_Image_Registry with SPARK_Mode is
   package I renames Desktop_Image_Source;
   package D renames I.D;
   package C renames Compositor_Source_Content;
   package Book is new Compositor_Source_Residency (I.V.Client_Slot);
   type State is limited private
     with Default_Initial_Condition => Valid (State);
   function Valid (S : State) return Boolean;
   type Capacity_Pressure is (None, Slots_Full, Memory_Exhausted);
   -- Describes the last Ensure only. This is NOT evidence of GPU quiescence
   -- and never authorizes source reuse or an in-place backend switch.
   function Last_Pressure (S : State) return Capacity_Pressure;
   function Upload_Work (S : State) return Boolean;
   function Faulted (S : State) return Boolean;
   -- Live slots (for evidence and tests).
   function Resident_Keys (S : State) return Natural;
   -- Rows of Key changed by its newest version (source pixels). Keys
   -- without a slot need nothing: their first pass copies every row.
   procedure Note_Change (S : in out State; Key : C.Source_Key; Rows : C.Row_Band)
     with Global => null, Pre => Valid (S), Post => Valid (S);
   -- Available returns a source holding exactly Version. Pending/Deferred
   -- leave the scene cold; Unaffordable means no allocation could be made
   -- even after evicting every idle key (the caller degrades that draw).
   procedure Ensure (S : in out State; Key : C.Source_Key; Version : C.Content_Version;
      Image : Compositor_Formats.Image; Bytes : Natural;
      Source : out I.V.Source_Ticket; Result : out I.Outcome)
     with Global => (In_Out => D.Engine),
       Pre => Valid (S) and D.Valid, Post => Valid (S) and D.Valid;
   -- At most one upload owner's bounded progress per event-loop call.
   procedure Poll (S : in out State; Result : out I.Outcome)
     with Global => (In_Out => D.Engine),
       Pre => Valid (S) and D.Valid, Post => Valid (S) and D.Valid;
   -- The caller is about to return this CPU mapping. Safe unless a pass
   -- still copies from it. Never touches GPU state.
   procedure Forget (S : in out State; Pixels : System.Address; Safe : out Boolean)
     with Global => null, Pre => Valid (S), Post => Valid (S);
   -- The surface is gone. Its slot is freed by Collect once idle.
   procedure Retire (S : in out State; Key : C.Source_Key)
     with Global => null, Pre => Valid (S), Post => Valid (S);
   -- Free idle slots of retired keys. Call with no scene capture open.
   procedure Collect (S : in out State)
     with Global => (In_Out => D.Engine),
       Pre => Valid (S) and D.Valid, Post => Valid (S) and D.Valid;
   procedure Close (S : in out State; Capture_Retired : Boolean; Safe : out Boolean)
     with Global => (In_Out => D.Engine),
       Pre => Valid (S) and D.Valid, Post => Valid (S) and D.Valid;
private
   type Owner_Table is array (I.V.Client_Slot) of I.State;
   type Flag_Table is array (I.V.Client_Slot) of Boolean;
   type State is limited record
      Owners : Owner_Table;
      Keys : Book.State;
      Retired : Flag_Table := (others => False);
      Cursor : I.V.Client_Slot := I.V.Client_Slot'First;
      Pressure : Capacity_Pressure := None;
   end record;
   function Valid (S : State) return Boolean is
     (Book.Valid (S.Keys) and (for all N in I.V.Client_Slot => I.Valid (S.Owners (N))));
end Desktop_Image_Registry;
