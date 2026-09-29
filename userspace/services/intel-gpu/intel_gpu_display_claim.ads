with Interfaces;
package Intel_GPU_Display_Claim with SPARK_Mode is
   use Interfaces;
   -- One object per static device claim. No reset/rebind/release operation:
   -- owner death or lost reply must not permit a second power controller.
   -- The broker supplies authenticated caller/badge and freshly validated
   -- D0/PCI identity/BAR evidence. This package does not authenticate IPC.
   type Claim is limited private;
   function Owner (Object : Claim) return Unsigned_64;
   procedure Take
     (Object : in out Claim; Designated, Caller : Unsigned_64;
      Badge_Valid, Device_Valid : Boolean; Allowed : out Boolean)
   with Post =>
     (Allowed = (Owner (Object)'Old = 0 and Designated /= 0 and
        Caller = Designated and Badge_Valid and Device_Valid)) and
     (not Allowed or Owner (Object) = Caller) and
     (Allowed or Owner (Object) = Owner (Object)'Old);
private
   type Claim is limited record
      Holder : Unsigned_64 := 0;
   end record;
end Intel_GPU_Display_Claim;
