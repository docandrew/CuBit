pragma Ada_2022;
with Parameters;
with HID_I2C_Touchpad;
generic
   with package App is new HID_I2C_Touchpad (<>);
package HID_Launch_Adapter with SPARK_Mode is
   type Status is (Accepted, Contact_Capacity_Exceeded, Unconfigured);
   type Result (Outcome : Status := Unconfigured) is record
      case Outcome is
         when Accepted =>
            Settings : App.Configuration;
            Descriptor_Register : App.D.Register_Number;
            Timing : Parameters.HID_Timing_Value;
         when others => null;
      end case;
   end record;
   function Translate (Raw : Parameters.HID_Parameters_Value) return Result;
   type Deadline_Result (Available : Boolean := False) is record
      case Available is
         when True => Limit : App.W.Millisecond;
         when False => null;
      end case;
   end record;
   function Deadline (Now : App.W.Word; Span : Parameters.HID_Milliseconds_Value)
      return Deadline_Result;
   -- No hardware defaults, clock reads, authority resolution or state changes.
   -- Pointer bounds come from the parsed report; only scaling/orientation and
   -- instance-capacity policy are translated here. Absolute deadlines reject
   -- exhaustion without wrap. Parser readiness remains a separate admission.
end HID_Launch_Adapter;
