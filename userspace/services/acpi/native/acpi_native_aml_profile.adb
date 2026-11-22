pragma Ada_2022;
with Interfaces;
with System.Machine_Code;
with AML_Dispatch_Profile;
with AML_Dispatch_Tails;
with AML_Decode;
with AML_Profile_Classifier;
with ACPI_Native_Log;
package body ACPI_Native_AML_Profile is
   package P renames AML_Dispatch_Profile;
   package E renames AML_Profile_Events;
   use type P.Counter, E.Logical_Table_ID;
   State : P.Context;
   Tail : AML_Dispatch_Tails.Context;
   Tail_Reported : Boolean := False;
   Began, Ended : P.Counter := 0;
   Reported, Table_Open, Bad_Table_Order : Boolean := False;
   Current_Table, Last_Table : E.Logical_Table_ID := E.Unknown_Table;
   Maximum_Reported_Rows : constant Positive := 8;
   -- Keep the generic flag static while avoiding false unreachable-code
   -- diagnostics in the default-disabled observer/report implementation.
   function Requested return Boolean is (Enabled);
   function Counter return P.Counter is
      Low, High : Interfaces.Unsigned_32;
      use System.Machine_Code;
   begin
      Asm ("lfence; rdtsc; lfence",
        Outputs => [Interfaces.Unsigned_32'Asm_Output ("=a", Low),
                    Interfaces.Unsigned_32'Asm_Output ("=d", High)],
        Clobber => "memory", Volatile => True);
      return Interfaces.Shift_Left (P.Counter (High), 32) or P.Counter (Low);
   end Counter;
   procedure Start is
   begin
      if not Requested then return; end if;
      Began := Counter; P.Start (State, Began);
   end Start;
   procedure Observe (Item : E.Event) is
      Where : P.Site := P.Load_Phase;
      Now : P.Counter;
   begin
      if not Requested or else not P.Active (State) then return; end if;
      AML_Dispatch_Tails.Observe (Tail, Item);
      Now := Counter;
      case Item.Kind is
         when E.Table_Enter =>
            if Table_Open or else Item.Logical_Table = E.Unknown_Table then Bad_Table_Order := True; end if;
            Table_Open := True; Current_Table := Item.Logical_Table; Last_Table := Item.Logical_Table;
         when E.Table_Leave =>
            if not Table_Open or else Current_Table /= Item.Logical_Table then Bad_Table_Order := True; end if;
            Table_Open := False; Current_Table := E.Unknown_Table;
         when E.Statement_Visit => Where := P.Statement_Dispatch;
         when E.Operand_Visit => Where := P.Operand_Dispatch;
      end case;
      P.Observe (State, Where, AML_Profile_Classifier.Classify (Item), Now);
   end Observe;
   procedure Stop is
   begin
      if not Requested then return; end if;
      Ended := Counter; P.Stop (State, Ended);
   end Stop;
   function Site_Name (Where : P.Site) return String is
     (case Where is when P.Load_Phase => "LOAD", when P.Statement_Dispatch => "STATEMENT", when P.Operand_Dispatch => "OPERAND");
   function Class_Name (Item : P.Instruction_Class) return String is
     (case Item is
        when P.Table_Load => "TABLE_LOAD", when P.Name_Reference_Or_Call => "NAME_OR_CALL",
        when P.Integer_Literal => "INTEGER", when P.String_Literal => "STRING",
        when P.Buffer_Or_Package => "BUFFER_PACKAGE", when P.Arithmetic => "ARITHMETIC",
        when P.Logical => "LOGIC", when P.Store_Or_Reference => "STORE_REFERENCE",
        when P.Namespace_Declaration => "DECLARATION", when P.Control_Flow => "CONTROL",
        when P.Synchronization => "SYNC", when P.Table_Operation => "TABLE_OPERATION",
        when P.Other => "OTHER");
   procedure Report is
      type Marks is array (P.Site, P.Instruction_Class) of Boolean;
      Used : Marks := [others => [others => False]];
   begin
      if not Requested or else Reported then return; end if;
      Reported := True;
      ACPI_Native_Log.Info ("acpi: stage aml-profile ok quality=" &
        (if P.Complete (State) and then not Table_Open and then not Bad_Table_Order then "COMPLETE" else "INCOMPLETE") &
        " basis=TSC_WALL scope=MODULE span_ticks=" &
        P.Counter'Image (if Ended >= Began then Ended - Began else 0) &
        " regressions=" & P.Clock_Regressions (State)'Image & " saturated=" & P.Saturated (State)'Image &
        " last_table=" & Last_Table'Image & " n=DISPATCH_VISITS includes=EXTERNAL_PREEMPTION_PROBE");
      for Row in 1 .. Maximum_Reported_Rows loop
         declare
            Found : Boolean := False;
            Best_Site : P.Site := P.Site'First;
            Best_Class : P.Instruction_Class := P.Instruction_Class'First;
            Best : P.Counters;
         begin
            for Where in P.Site loop
               for Item in P.Instruction_Class loop
                  declare Candidate : constant P.Counters := P.Read (State, Where, Item); begin
                     if not Used (Where, Item) and then (Candidate.Visits /= 0 or else Candidate.Interval_Ticks /= 0)
                       and then (not Found or else Candidate.Interval_Ticks > Best.Interval_Ticks)
                     then Found := True; Best_Site := Where; Best_Class := Item; Best := Candidate; end if;
                  end;
               end loop;
            end loop;
            exit when not Found;
            Used (Best_Site, Best_Class) := True;
            ACPI_Native_Log.Info ("acpi: stage aml-profile-class ok site=" & Site_Name (Best_Site) &
              " class=" & Class_Name (Best_Class) & " n=" & Best.Visits'Image &
              " interval_ticks=" & Best.Interval_Ticks'Image);
         end;
      end loop;
   end Report;
   function Hex_Byte (Value : AML_Decode.Byte) return String is
      Alphabet : constant String := "0123456789ABCDEF";
      Number : constant Natural := Natural (Value);
      Radix : constant Positive := 16;
   begin
      return String'[Alphabet (Number / Radix + Alphabet'First),
        Alphabet (Number mod Radix + Alphabet'First)];
   end Hex_Byte;
   procedure Report_Tail (IO_Pending, IO_Uncertain : Boolean) is
   begin
      if not Requested or else P.Active (State) or else Tail_Reported
        or else IO_Pending or else IO_Uncertain then return; end if;
      Tail_Reported := True;
      for Row in 1 .. AML_Dispatch_Tails.Length (Tail) loop
         declare Item : constant AML_Dispatch_Tails.Observation := AML_Dispatch_Tails.Read (Tail, Row); begin
            ACPI_Native_Log.Warning ("acpi: stage aml-tail trace row=" & Row'Image &
              " batch-table=" & Item.Batch_Table'Image & " scope=FRAME_RELATIVE_DISPATCH kind=" &
              (case Item.Syntax.Kind is when E.Statement_Visit => "STATEMENT",
                when E.Operand_Visit => "OPERAND", when others => "TABLE") &
              " pos=" & Item.Syntax.Position'Image & " opcode=0x" & Hex_Byte (Item.Syntax.Opcode) &
              (if Item.Syntax.Has_Extension then " ext=0x" & Hex_Byte (Item.Syntax.Extension) else ""));
         end;
      end loop;
   end Report_Tail;
end ACPI_Native_AML_Profile;
