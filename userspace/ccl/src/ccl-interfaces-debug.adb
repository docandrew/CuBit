with CCL.Host_Values;
with CCL.Language;
with CCL.Objects.Catalog;
with CCL.Types;
with CCL.VM;

package body CCL.Interfaces.Debug with SPARK_Mode is
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Language.Analysis_Status;
   use type CCL.Types.List_Result;
   use type CCL.Objects.Catalog.Publication_Result;

   procedure Define_Types (Bound : out Contracts; Accepted : out Boolean) is
      Checked : CCL.Language.Analysis_Result;
      Types : CCL.Types.Registry;

      function Named (Name : String) return CCL.Types.Type_Reference is
        (CCL.Types.Find (Types, CCL.Types.Named (Name)));

      procedure Bind (Of_Type : CCL.Types.Type_Reference; Key : CCL.Objects.Schema_Key;
                      Contract : in out CCL.Objects.Binding) is
      begin
         if Accepted then
            CCL.Objects.Bind (Types, Of_Type, Key, Contract, Accepted);
         end if;
      end Bind;

      procedure Bind_List (Element : String; Key : CCL.Objects.Schema_Key;
                           Contract : in out CCL.Objects.Binding) is
         List_Type : CCL.Types.Type_Reference;
         Listed : CCL.Types.List_Result;
      begin
         if Accepted then
            CCL.Types.Specialize_List (Types, Named (Element), List_Type, Listed);
            Accepted := Listed in CCL.Types.List_Specialized | CCL.Types.List_Already_Specialized;
            Bind (List_Type, Key, Contract);
         end if;
      end Bind_List;
   begin
      Bound := (others => <>);
      --  The declarations, then a trivial expression: one checked program.
      CCL.Language.Analyze (TYPE_SOURCE & "0", Checked);
      Accepted := CCL.Language.Analysis_Status_Of (Checked) = CCL.Language.Analysis_Succeeded;
      if not Accepted then return; end if;
      Types := CCL.Language.Analysis_Types (Checked);
      Bind (Named ("Debug_Session"), SESSION_KEY, Bound.Session);
      Bind (Named ("Attach_Query"), ATTACH_QUERY_KEY, Bound.Attach_Query);
      Bind_List ("Debug_Thread", THREADS_KEY, Bound.Threads);
      Bind (Named ("Frame_Query"), FRAME_QUERY_KEY, Bound.Frame_Query);
      Bind_List ("Debug_Frame", FRAMES_KEY, Bound.Frames);
      Bind (Named ("Locals_Query"), LOCALS_QUERY_KEY, Bound.Locals_Query);
      Bind (Named ("Debug_Binding"), BINDING_KEY, Bound.Binding);
      Bind_List ("Debug_Binding", BINDINGS_KEY, Bound.Bindings);
      Bind (Named ("Value_Query"), VALUE_QUERY_KEY, Bound.Value_Query);
      Bind (Named ("Children_Query"), CHILDREN_QUERY_KEY, Bound.Children_Query);
      Bind (Named ("Graph_Query"), GRAPH_QUERY_KEY, Bound.Graph_Query);
      Bind (Named ("Debug_Graph"), GRAPH_KEY, Bound.Graph);
      Bind (Named ("Memory_Query"), MEMORY_QUERY_KEY, Bound.Memory_Query);
      Bind (Named ("Debug_Memory"), MEMORY_KEY, Bound.Memory);
   end Define_Types;

   procedure Publish
     (Item : in out CCL.Catalog.Interface_Catalog; Error : out CCL.Catalog.Catalog_Error)
   is
      Bound : Contracts;
      Accepted : Boolean;
      Result : CCL.Objects.Catalog.Publication_Result;
      Descriptor : CCL.Catalog.Interface_Descriptor;
      Entry_Operation : CCL.Catalog.Operation_Descriptor;
      procedure Publish_Schema (Contract : CCL.Objects.Binding) is
      begin
         if Accepted then
            CCL.Catalog.Publish_Schema (Item, Contract, Result);
            Accepted := Result in CCL.Objects.Catalog.Published | CCL.Objects.Catalog.Already_Published;
         end if;
      end Publish_Schema;
      --  Reading a process's threads, stack and memory observes it: the
      --  Observe class, whatever the debug kind (only Debug_Observe exists
      --  for debug.*).
      function Object_To (Argument, Result : CCL.Objects.Schema_Key) return CCL.Host_Values.Import_Declaration is
        ((Argument => CCL.Host_Values.Object_Value, Argument_Schema => Argument,
          Result => CCL.Host_Values.Object_Value, Result_Schema => Result,
          Authority => CCL.VM.Observe_Authority, others => <>));
      function Session_To (Result : CCL.Objects.Schema_Key) return CCL.Host_Values.Import_Declaration is
        ((Argument => CCL.Host_Values.Integer_Value,
          Result => CCL.Host_Values.Object_Value, Result_Schema => Result,
          Authority => CCL.VM.Observe_Authority, others => <>));
      DETACH_CONTRACT : constant CCL.Host_Values.Import_Declaration :=
        (Argument => CCL.Host_Values.Integer_Value, Result => CCL.Host_Values.Boolean_Value,
         Authority => CCL.VM.Observe_Authority, others => <>);
      function Contract_Of (Op : Operation) return CCL.Host_Values.Import_Declaration is
        (case Op is
            when Attach => Object_To (ATTACH_QUERY_KEY, SESSION_KEY),
            when Threads => Session_To (THREADS_KEY),
            when Frames => Object_To (FRAME_QUERY_KEY, FRAMES_KEY),
            when Locals => Object_To (LOCALS_QUERY_KEY, BINDINGS_KEY),
            when Value => Object_To (VALUE_QUERY_KEY, BINDING_KEY),
            when Children => Object_To (CHILDREN_QUERY_KEY, BINDINGS_KEY),
            when Graph => Object_To (GRAPH_QUERY_KEY, GRAPH_KEY),
            when Memory => Object_To (MEMORY_QUERY_KEY, MEMORY_KEY),
            when Detach => DETACH_CONTRACT);
   begin
      Error := CCL.Catalog.Invalid_Host_Contract;
      Define_Types (Bound, Accepted);
      Publish_Schema (Bound.Session);
      Publish_Schema (Bound.Attach_Query);
      Publish_Schema (Bound.Threads);
      Publish_Schema (Bound.Frame_Query);
      Publish_Schema (Bound.Frames);
      Publish_Schema (Bound.Locals_Query);
      Publish_Schema (Bound.Binding);
      Publish_Schema (Bound.Bindings);
      Publish_Schema (Bound.Value_Query);
      Publish_Schema (Bound.Children_Query);
      Publish_Schema (Bound.Graph_Query);
      Publish_Schema (Bound.Graph);
      Publish_Schema (Bound.Memory_Query);
      Publish_Schema (Bound.Memory);
      if not Accepted then
         return;
      end if;
      CCL.Catalog.Define_Interface ("debug", 1, 0, DIGEST, Descriptor, Error);
      for Op in Operation loop
         exit when Error /= CCL.Catalog.Catalog_Valid;
         CCL.Catalog.Define_Host_Operation (Name (Op), 1, Contract_Of (Op), Entry_Operation, Error);
         if Error = CCL.Catalog.Catalog_Valid then
            CCL.Catalog.Add_Operation (Descriptor, Entry_Operation, Error);
         end if;
      end loop;
      if Error = CCL.Catalog.Catalog_Valid then
         CCL.Catalog.Publish (Item, Descriptor, Error);
      end if;
   end Publish;
end CCL.Interfaces.Debug;
