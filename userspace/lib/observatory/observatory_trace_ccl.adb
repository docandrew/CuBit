with CCL.VM;
package body Observatory_Trace_CCL with SPARK_Mode is
   package H renames CCL.Host_Values;
   package W renames V.A.W;
   use type H.Value_Kind, CCL.Catalog.Catalog_Error, W.Event_Kind, W.RT.Phase;
   function Name (Op : Operation) return String is
     (case Op is
      when Ready => "ready",
      when Row_Count => "count",
      when Total_Count => "total",
      when Page_Number => "page",
      when Lossy => "lossy",
      when Stop_Reason => "stop-reason",
      when Kind => "kind",
      when Time_Us => "time-us",
      when Duration_Us => "duration-us",
      when Has_Duration => "has-duration",
      when Event_ID => "event-id",
      when Pid => "pid",
      when Publisher => "publisher",
      when Incarnation => "observer",
      when Output_ID => "output",
      when Surface => "surface",
      when Source_Epoch => "source-epoch",
      when Source_Ticket => "source-ticket",
      when Writer_Buffer => "writer-buffer",
      when Writer_Epoch => "writer-epoch",
      when Writer_Serial => "writer-serial",
      when Session_ID => "session",
      when Frame_ID => "frame",
      when Input_Serial => "input-serial",
      when Input_After => "input-watermark",
      when Producer_Dropped => "producer-dropped",
      when Batch_Gaps => "batch-gaps",
      when History_Sequence => "history-sequence",
      when Batch => "batch");
   procedure Publish (Catalog : in out CCL.Catalog.Interface_Catalog;
      Error : out CCL.Catalog.Catalog_Error) is
      D : CCL.Catalog.Interface_Descriptor;
      O : CCL.Catalog.Operation_Descriptor;
   begin
      CCL.Catalog.Define_Interface ("trace", 1, 0,
         [16#CF39B1D37D582014#, 16#24E9783BDB30592A#, 16#60A5EF3DDA1B9980#, 16#B7419FD5A3CF1001#], D, Error);
      if Error /= CCL.Catalog.Catalog_Valid then return; end if;
      for Op in Operation loop
         if Op = Source_Epoch then
            CCL.Catalog.Publish (Catalog, D, Error);
            if Error /= CCL.Catalog.Catalog_Valid then return; end if;
            CCL.Catalog.Define_Interface ("trace-detail", 1, 0,
              [16#CF39B1D37D582014#, 16#24E9783BDB30592A#, 16#60A5EF3DDA1B9980#, 16#B7419FD5A3CF1002#], D, Error);
            if Error /= CCL.Catalog.Catalog_Valid then return; end if;
         end if;
         CCL.Catalog.Define_Host_Operation
           (Name (Op), (if Op in Ready .. Stop_Reason then 0 else 1),
            (Argument => H.Integer_Value,
             Result => (if Op in Ready | Lossy | Has_Duration then H.Boolean_Value
                        elsif Op in Row_Count | Total_Count | Page_Number then H.Integer_Value else H.Text_Value),
             Result_Text_Limit => (if Op in Ready .. Lossy or Op = Has_Duration then 0 else 20),
             Authority => CCL.VM.Observe_Authority, others => <>), O, Error);
         if Error /= CCL.Catalog.Catalog_Valid then return; end if;
         CCL.Catalog.Add_Operation (D, O, Error);
         if Error /= CCL.Catalog.Catalog_Valid then return; end if;
      end loop;
      CCL.Catalog.Publish (Catalog, D, Error);
   end Publish;
   procedure Invoke (Item : in out Context; Host_Binding : Unsigned_32;
      Argument : H.Value; Reply : out H.Call_Result) is
      Op : Operation;
      Row : V.A.S.Capture;
      procedure Text (Value : String) is
         Buffer : H.Text;
      begin
         H.Copy_Text (Value, Buffer, Reply.Success);
         Reply.Value := H.Text_Constant (Buffer);
      end Text;
      procedure Number (Value : Unsigned_64) is
         Image : constant String := Value'Image;
      begin Text (Image (Image'First + 1 .. Image'Last)); end Number;
   begin
      Reply := (Value => H.Integer_Constant (0), Success => False, Why => <>);
      if Host_Binding < Binding (Operation'First) or else Host_Binding > Binding (Operation'Last)
        or else Argument.Kind /= H.Integer_Value then return; end if;
      Op := Operation'Val (Host_Binding - Binding_Base);
      if Op in Ready .. Stop_Reason then
         if Argument.Integer /= 0 then return; end if;
         if Op = Stop_Reason then
            Text ((if not V.Ready (Item) then "unavailable"
                   elsif V.Observer_Failed (Item) then "observer-failed"
                   elsif V.Budget_Limited (Item) then "budget-reached" else "requested-stop"));
            return;
         end if;
         Reply.Success := True;
         Reply.Value := (case Op is
            when Ready => H.Boolean_Constant (V.Ready (Item)),
            when Row_Count => H.Integer_Constant (Integer_64 (V.Length (Item))),
            when Total_Count => H.Integer_Constant (Integer_64 (V.Total (Item))),
            when Page_Number => H.Integer_Constant (Integer_64 (V.Page (Item))),
            when Lossy => H.Boolean_Constant (V.Lossy (Item)),
            when others => H.Boolean_Constant (False));
         return;
      end if;
      if Argument.Integer < 0 or else Argument.Integer >= Integer_64 (V.Length (Item)) then return; end if;
      Row := V.Value_At (Item, V.Index (Argument.Integer + 1));
      if Op = Has_Duration then
         Reply.Success := True; Reply.Value := H.Boolean_Constant (Row.Value.Kind = W.Frame_Event); return;
      elsif Op = Kind then
         Text ((case Row.Value.Kind is
            when W.Input_Event => "input-dequeued", when W.Source_Event => "publication-accepted",
            when W.Render_Event => (if Row.Value.Render.Kind = W.RT.Draw then "draw-checkpoint" else "submitted"),
            when W.Frame_Event => "completion-collected")); return;
      elsif Op = Time_Us then
         Number ((case Row.Value.Kind is
            when W.Input_Event => Row.Value.Input.Dequeued,
            when W.Source_Event => Row.Value.Source.Accepted,
            when W.Render_Event => Row.Value.Render.Observed,
            when W.Frame_Event => Row.Value.Frame.Completed)); return;
      end if;
      case Op is
         when Event_ID => Number (Row.Value.Event_ID);
         when Pid => Number (Row.Pid);
         when Publisher => Number (Row.Publisher);
         when Incarnation => Number (Row.Incarnation);
         when Producer_Dropped => Number (Row.Producer_Dropped);
         when Batch_Gaps => Number (Row.Batch_Gaps);
         when History_Sequence => Number (Row.First_Sequence);
         when Batch => Number (Row.Batch);
         when Duration_Us =>
            if Row.Value.Kind = W.Frame_Event then Number (Row.Value.Frame.Completed - Row.Value.Frame.Submitted);
            else Text ("unavailable"); end if;
         when Output_ID =>
            if Row.Value.Kind = W.Frame_Event then Number (Unsigned_64 (Row.Value.Frame.Output_ID));
            elsif Row.Value.Kind = W.Render_Event then Number (Unsigned_64 (Row.Value.Render.Output_ID));
            else Text ("unavailable"); end if;
         when Surface =>
            if Row.Value.Kind = W.Input_Event then Number (Row.Value.Input.Surface);
            elsif Row.Value.Kind = W.Source_Event then Number (Row.Value.Source.Surface);
            elsif Row.Value.Kind = W.Render_Event and then Row.Value.Render.Kind = W.RT.Draw then Number (Row.Value.Render.Surface);
            else Text ("unavailable"); end if;
         when Source_Epoch =>
            if Row.Value.Kind = W.Source_Event then Number (Row.Value.Source.Epoch);
            elsif Row.Value.Kind = W.Render_Event and then Row.Value.Render.Kind = W.RT.Draw then Number (Row.Value.Render.Source_Epoch);
            else Text ("unavailable"); end if;
         when Source_Ticket =>
            if Row.Value.Kind = W.Source_Event then Number (Row.Value.Source.Ticket);
            elsif Row.Value.Kind = W.Render_Event and then Row.Value.Render.Kind = W.RT.Draw then Number (Row.Value.Render.Source_Ticket);
            else Text ("unavailable"); end if;
         when Input_Serial =>
            if Row.Value.Kind = W.Input_Event then Number (Row.Value.Input.Serial);
            else Text ("unavailable"); end if;
         when Input_After =>
            if Row.Value.Kind = W.Source_Event then Number (Row.Value.Source.Input_After);
            else Text ("unavailable"); end if;
         when Writer_Buffer =>
            if Row.Value.Kind = W.Render_Event then Number (Row.Value.Render.Buffer);
            else Text ("unavailable"); end if;
         when Writer_Epoch =>
            if Row.Value.Kind = W.Render_Event then Number (Row.Value.Render.Writer_Epoch);
            else Text ("unavailable"); end if;
         when Writer_Serial =>
            if Row.Value.Kind = W.Render_Event then Number (Row.Value.Render.Writer_Serial);
            else Text ("unavailable"); end if;
         when Session_ID =>
            if Row.Value.Kind = W.Frame_Event then Number (Row.Value.Frame.Session);
            elsif Row.Value.Kind = W.Render_Event and then Row.Value.Render.Kind = W.RT.Submit then Number (Row.Value.Render.Session);
            else Text ("unavailable"); end if;
         when Frame_ID =>
            if Row.Value.Kind = W.Frame_Event then Number (Row.Value.Frame.Frame);
            elsif Row.Value.Kind = W.Render_Event and then Row.Value.Render.Kind = W.RT.Submit then Number (Row.Value.Render.Frame);
            else Text ("unavailable"); end if;
         when others => null;
      end case;
   end Invoke;
end Observatory_Trace_CCL;
