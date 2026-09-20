with CCL.Host_Values;

--  Host-owned text output, not a system log or a process-wide standard stream.
--  Append is atomic: exhaustion returns false without discarding earlier text.
package CCL.UI_Outputs with SPARK_Mode is
   Maximum_Length : constant := 16_384;
   subtype Text_Length is Natural range 0 .. Maximum_Length;
   type Operation is (Append_Line, Clear_Output);
   function Name (Op : Operation) return String is
     (case Op is when Append_Line => "output-append",
                 when Clear_Output => "output-clear");
   type Model is private;
   function Length (Item : Model) return Text_Length;
   function Content (Item : Model) return String;
   function Changed (Item : Model) return Boolean;
   procedure Append
     (Item : in out Model; Text : String; Accepted : out Boolean);
   procedure Clear (Item : in out Model);
   procedure Painted (Item : in out Model);
   procedure Apply
     (Item : in out Model; Op : Operation; Argument : CCL.Host_Values.Value;
      Accepted : out Boolean);
private
   type Model is record
      Data : String (1 .. Maximum_Length) := [others => ' '];
      Last : Text_Length := 0;
      Dirty : Boolean := False;
   end record;
   function Length (Item : Model) return Text_Length is (Item.Last);
   function Content (Item : Model) return String is
     (Item.Data (1 .. Item.Last));
   function Changed (Item : Model) return Boolean is (Item.Dirty);
end CCL.UI_Outputs;
