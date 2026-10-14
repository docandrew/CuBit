--  Hosted fixture: a startup table that verified.
package CuBit.Startup_State is
   type Verification is record
      Loaded : Boolean := True;
   end record;
   function Ready (Item : Verification) return Boolean is (Item.Loaded);
end CuBit.Startup_State;
