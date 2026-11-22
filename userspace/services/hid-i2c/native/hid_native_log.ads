pragma Ada_2022;
with Interfaces;
with CuBit.Logging;
with CuBit.Control_Events;
generic
   Target : CuBit.Logging.Logstore_Handles.Optional_Endpoint;
package HID_Native_Log is
   type Binding_Result is (Bound, Missing, Refused, Already_Initialized);
   procedure Initialize (Result : out Binding_Result);
   type Bringup_Stage is (Identify, Bindings, Buffers, Reset, Descriptor,
      Report_Parser, Serving_Pin, Publication, Readiness);
   type Reason is (Confirmed, Not_Available, Invalid_Authority, Invalid_Configuration,
      Invalid_Descriptor, Unsupported_Report, Clock_Failure, Timed_Out,
      Operation_Refused, Effects_Uncertain, Cleanup_Pending);
   procedure Stage (Item : Bringup_Stage; OK : Boolean; Why : Reason;
      Details : String := "");
   procedure Summary;
   procedure Grant_Notice (Item : CuBit.Control_Events.Event);
   procedure Disable;
   function Detached return Boolean;
   function Hex (Key : String; Value : Interfaces.Unsigned_64) return String;
   -- Main binds generated Startup.Logstore and calls Initialize before stages.
   -- Details contain compiled enum names/numeric values, never firmware text.
   -- Stages use the existing logger unchanged (Info/Warning), mirrored to serial.
   -- Repeated outcomes are counted; Summary emits once. Publisher is process-
   -- lived even if disconnect leaves channel pages pending authoritative return.
end HID_Native_Log;
