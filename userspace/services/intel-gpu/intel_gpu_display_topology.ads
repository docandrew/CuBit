with Interfaces;
package Intel_GPU_Display_Topology with SPARK_Mode is
   use Interfaces;
   -- ADL-N/Xe-LPD pipe-access subset only; not DDI/AUX/audio ownership.
   -- PW1 precedes DC-off; parent wells precede every dependent pipe well.
   type Well is (PW1, DC_Off, PW2, PWA, PWB, PWC, PWD);
   type Pipe is (A, B, C, D);
   function Bit (Item : Well) return Unsigned_64 is
     (Shift_Left (Unsigned_64'(1), Well'Pos (Item)));
   function Ancestors (Item : Well) return Unsigned_64 is
     (case Item is
        when PW1 => 0,
        when DC_Off | PW2 | PWA => Bit (PW1),
        when PWB => Bit (PW1) or Bit (PW2),
        when PWC | PWD => Bit (PW1) or Bit (DC_Off) or Bit (PW2));
   function Valid (Bits : Unsigned_64) return Boolean is
     (Bits /= 0 and then (Bits and not 127) = 0 and then
      (for all Item in Well =>
         (Bits and Bit (Item)) = 0 or else
         (Bits and Ancestors (Item)) = Ancestors (Item)));
   function Pipe_Well (Item : Pipe) return Well is
     (case Item is when A => PWA, when B => PWB, when C => PWC, when D => PWD);
   function Required (Item : Pipe) return Unsigned_64 is
     (Bit (Pipe_Well (Item)) or Ancestors (Pipe_Well (Item)))
   with Post => Valid (Required'Result);
   -- DC-off is a separate transition, NOT a power-well request bit.
   subtype Request_Well is Well with Static_Predicate => Request_Well /= DC_Off;
   function Request_Mask (Item : Request_Well) return Unsigned_32 is
     (case Item is
        when PW1 => 16#2#, when PW2 => 16#8#,
        when PWA => 16#800#, when PWB => 16#2000#,
        when PWC => 16#8000#, when PWD => 16#20000#);
   function State_Mask (Item : Request_Well) return Unsigned_32 is
     (Shift_Right (Request_Mask (Item), 1));
   -- Register snapshot mask only. DC-off is deliberately not encoded here;
   -- a matching snapshot is not a reference or evidence DC states are off.
   function Pipe_Request_State_Mask (Item : Pipe) return Unsigned_32;
end Intel_GPU_Display_Topology;
