--  Retained, checked entry points, not source strings or native addresses.
package CCL.Language.Handlers with SPARK_Mode is
   type Profile is (Boolean_Action);
   --  Boolean_Action is () -> Boolean. The Boolean is the application's
   --  result, not a request to retry or evidence of event delivery.
   type Preparation_Status is
     (Prepared, Invalid_Source, Unknown_Entry, Wrong_Profile, Admission_Denied);
   type Handler is private;
   procedure Prepare
     (Source, Entry_Name : String; Expected : Profile;
      Catalog : CCL.Catalog.Interface_Catalog;
      Grants : CCL.Catalog.Granted_Bindings;
      Item : out Handler; Status : out Preparation_Status;
      Diagnostic : out Interpretation_Result);
   function Ready (Item : Handler) return Boolean;

   --  No reparse, editor access, or lifetime borrowed from a REPL history slot.
   --  Bindings must still exist and have the SAME runtime identity at dispatch.
   generic
      type Host_Context is limited private;
      with procedure Invoke
        (Context : in out Host_Context; Binding : Interfaces.Unsigned_32;
         Argument : CCL.Host_Values.Value; Reply : out CCL.Host_Values.Call_Result);
   procedure Execute
     (Item : Handler; Fuel : Natural;
      Current_Grants : CCL.Catalog.Granted_Bindings;
      Context : in out Host_Context; Outcome : out Interpretation_Result)
     with Post => Outcome.Fuel_Remaining <= Fuel;
private
   type Handler is record
      Valid : Boolean := False;
      Program : Analysis_Result;
      Bindings : CCL.Catalog.Granted_Bindings;
   end record;
   function Ready (Item : Handler) return Boolean is (Item.Valid);
end CCL.Language.Handlers;
