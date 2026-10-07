with Desktop_Image_Source;
with Compositor_Formats;
with Interfaces;
with System;
-- Private cache over the configured client descriptor slots, NOT a device
-- memory quota or global BO limit. CPU acquisitions remain immutable until
-- Forget succeeds. The caller must call Forget before returning/reusing a
-- source mapping; an address is not authority or a content version.
package Desktop_Image_Registry is
   package I renames Desktop_Image_Source;
   type State is limited private;
   type Capacity_Pressure is (None, Slots_Full, Generation_Exhausted);
   -- Describes the last Ensure only. This is NOT evidence of GPU quiescence
   -- and never authorizes source reuse or an in-place backend switch.
   function Last_Pressure (S : State) return Capacity_Pressure;
   function Upload_Work (S : State) return Boolean;
   function Faulted (S : State) return Boolean;
   procedure Close (S : in out State; Capture_Retired : Boolean; Safe : out Boolean);
   procedure Ensure (S : in out State; Image : Compositor_Formats.Image;
      Bytes : Natural; Source : out I.V.Source_Ticket; Result : out I.Outcome);
   -- At most one upload owner's bounded progress per event-loop call.
   procedure Poll (S : in out State; Result : out I.Outcome);
   procedure Forget (S : in out State; Pixels : System.Address;
      Capture_Retired : Boolean; Safe : out Boolean);
private
   type Cache_Item is limited record
      Owner : I.State;
      Image : Compositor_Formats.Image;
      Bytes : Natural := 0;
      Generation : Interfaces.Unsigned_64 := 0;
      Closing : Boolean := False;
   end record;
   type Entries is array (I.V.Client_Slot) of Cache_Item;
   type State is limited record
      Items : Entries;
      Serial : Interfaces.Unsigned_64 := 0;
      Cursor : I.V.Client_Slot := I.V.Client_Slot'First;
      Pressure : Capacity_Pressure := None;
   end record;
end Desktop_Image_Registry;
