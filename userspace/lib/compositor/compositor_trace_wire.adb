package body Compositor_Trace_Wire with SPARK_Mode is
   procedure Lemma_Decoded_Valid (Data : Packet) is
   begin
      null;
   end Lemma_Decoded_Valid;

   function Encode (Value : Event) return Packet is
   begin
      case Value.Kind is
         when Input_Event =>
            return (0 => Magic, 1 => 1, 2 => Value.Event_ID,
                    3 => Value.Input.Surface, 4 => Value.Input.Serial,
                    5 => Value.Input.Kind, 6 => Value.Input.Dequeued,
                    others => 0);
         when Source_Event =>
            return (0 => Magic, 1 => 2, 2 => Value.Event_ID,
                    3 => Value.Source.Surface, 4 => Value.Source.Epoch,
                    5 => Value.Source.Ticket, 6 => Value.Source.Input_After,
                    7 => Value.Source.Accepted, others => 0);
         when Render_Event =>
            return (0 => Magic, 1 => 3, 2 => Value.Event_ID,
                    3 => RT.Phase'Pos (Value.Render.Kind),
                    4 => Word (Value.Render.Output_ID),
                    5 => Value.Render.Buffer, 6 => Value.Render.Writer_Epoch,
                    7 => Value.Render.Writer_Serial, 8 => Value.Render.Surface,
                    9 => Value.Render.Source_Epoch,
                    10 => Value.Render.Source_Ticket, 11 => Value.Render.Session,
                    12 => Value.Render.Frame, 13 => Value.Render.Observed,
                    others => 0);
         when Frame_Event =>
            return (0 => Magic, 1 => 4, 2 => Value.Event_ID,
                    3 => Word (Value.Frame.Output_ID),
                    4 => Value.Frame.Session, 5 => Value.Frame.Frame,
                    6 => Value.Frame.Submitted, 7 => Value.Frame.Completed,
                    others => 0);
      end case;
   end Encode;
end Compositor_Trace_Wire;
