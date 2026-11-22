with Interfaces;
with CuBit.Input;
with CuBit.Messages;
with Input_Pending;
--  A retained report's acquisition stamp on the wire (LAT-002): the
--  microsecond time when the driver recorded one (sub-millisecond bits in
--  the header, CuBit.Input.Encode_Stamped), else its millisecond time.
package Input_Stamps with SPARK_Mode is
   use type Interfaces.Unsigned_64;
   function Snapshot (Buttons : Interfaces.Unsigned_64; Item : Input_Pending.Item)
     return Interfaces.Unsigned_64 is
     (if Item.Observed_Us /= Input_Pending.Word'Last
      then CuBit.Input.Source_Snapshot (Buttons, Item.Observed_Us)
      else CuBit.Input.Pointer_Snapshot (Buttons, Item.Observed_Ms));
   --  Report.snapshot must be Snapshot (buttons, Item).
   function Encode (Report : CuBit.Input.Source_Report; Item : Input_Pending.Item)
     return CuBit.Messages.Message is
     (CuBit.Input.Encode_Stamped (Report, Item.Observed_Us));
end Input_Stamps;
