with Interfaces;
generic
   -- Serialized ADL-N owner with required forcewake held for the entire
   -- transaction. No other MMIO user may change steering in this interval.
   with function Owner_Ready return Boolean;
   with function Read32 (Offset : Interfaces.Unsigned_32) return Interfaces.Unsigned_32;
   with procedure Write32
     (Offset, Value : Interfaces.Unsigned_32; Success : out Boolean);
package Intel_GPU_MCR_Access is
   type State is limited private;
   procedure Access_Register
     (Object : in out State; Offset : Interfaces.Unsigned_32;
      Instance : Natural; Write : Boolean;
      Value : in out Interfaces.Unsigned_32; Success : out Boolean);
   function Failed (Object : State) return Boolean;
private
   type State is limited record
      Faulted : Boolean := False;
   end record;
end Intel_GPU_MCR_Access;
