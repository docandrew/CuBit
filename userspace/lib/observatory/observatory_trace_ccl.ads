with Interfaces; use Interfaces;
with CCL.Catalog;
with CCL.Host_Values;
with Observatory_Trace_View;
package Observatory_Trace_CCL with SPARK_Mode is
   package V renames Observatory_Trace_View;
   subtype Context is V.State;
   type Operation is (Ready, Row_Count, Total_Count, Page_Number, Lossy, Stop_Reason, Kind, Time_Us, Duration_Us, Has_Duration, Event_ID, Pid, Publisher, Incarnation, Output_ID, Surface, Source_Epoch, Source_Ticket, Writer_Buffer, Writer_Epoch, Writer_Serial, Session_ID, Frame_ID, Input_Serial, Input_After, Producer_Dropped, Batch_Gaps, History_Sequence, Batch);
   Binding_Base : constant Unsigned_32 := 16#4F54_0100#;
   function Binding (Op : Operation) return Unsigned_32 is (Binding_Base + Operation'Pos (Op));
   function Namespace (Op : Operation) return String is
     (if Op <= Surface then "trace" else "trace-detail");
   function Name (Op : Operation) return String;
   procedure Publish (Catalog : in out CCL.Catalog.Interface_Catalog;
      Error : out CCL.Catalog.Catalog_Error);
   procedure Invoke (Item : in out Context; Host_Binding : Unsigned_32;
      Argument : CCL.Host_Values.Value; Reply : out CCL.Host_Values.Call_Result);
end Observatory_Trace_CCL;
