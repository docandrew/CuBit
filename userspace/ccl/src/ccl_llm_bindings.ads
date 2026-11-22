with Interfaces;
with CCL.Catalog;
with CCL.Host_Values;
with CCL_Stream_Table;

--  llm.* (CCL.Interfaces.Llm) over the platform's CCL_Llm. llm.ask starts
--  an exchange and opens its text stream and reply task in the session's
--  table at once; Pump moves arrived text into the stream as it comes and
--  completes the task with the typed reply when the exchange ends.
package CCL_Llm_Bindings is
   procedure Install
     (Catalog : in out CCL.Catalog.Interface_Catalog;
      Grants : in out CCL.Catalog.Granted_Bindings; Success : out Boolean);
   function Handles (Binding : Interfaces.Unsigned_32) return Boolean;
   procedure Invoke
     (Binding : Interfaces.Unsigned_32; Argument : CCL.Host_Values.Value;
      Streams : in out CCL_Stream_Table.Table;
      Reply : out CCL.Host_Values.Call_Result);
   procedure Pump (Streams : in out CCL_Stream_Table.Table; Arrived : out Boolean);
   --  An exchange is still running: a front end that sleeps until its next
   --  deadline wakes within Poll_Ms to pump it, even if no completion
   --  woke it.
   Poll_Ms : constant := 50;
   function Running return Boolean;

   --  The exchanges llm.ask started since the last call, for a front end
   --  that gives each a live card (the console).
   MAXIMUM_STARTED : constant := 4;
   type Started_Array is array (1 .. MAXIMUM_STARTED) of Interfaces.Unsigned_64;
   procedure Take_Started (Items : out Started_Array; Count : out Natural);
end CCL_Llm_Bindings;
