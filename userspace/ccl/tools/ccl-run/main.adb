with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;

with CCL.Language;
with CCL.VM;

procedure Main is
   use type CCL.Language.Interpretation_Status;
   use type CCL.VM.Value_Kind;

   DEFAULT_FUEL : constant Natural := 1_024;
   PROMPT       : constant String := "ccl> ";

   procedure Print_Result (Result : CCL.Language.Interpretation_Result) is
   begin
      case Result.Status is
         when CCL.Language.Succeeded =>
            if not Result.Has_Value then
               Put_Line ("ok");
            elsif Result.Has_Text then
               if Result.Result_Text.Length = 0 then
                  New_Line;
               else
                  Put_Line
                    (Result.Result_Text.Data (1 .. Result.Result_Text.Length));
               end if;
            elsif Result.Has_Character then
               Put_Line (String'(1 => Result.Result_Character));
            elsif Result.Result_Value.Kind = CCL.VM.Integer_Value then
               Put_Line (Integer_64'Image (Result.Result_Value.Integer));
            else
               Put_Line
                 ((if Result.Result_Value.Boolean then "true" else "false"));
            end if;
         when others =>
            Put_Line
              ("error: " & CCL.Language.Interpretation_Status'Image
               (Result.Status) & " / " &
               CCL.Language.Diagnostic_Code'Image (Result.Diagnostic) &
               (if Result.Diagnostic_Position = 0 then ""
                else " at" & Natural'Image (Result.Diagnostic_Position)));
      end case;
   end Print_Result;

   function Run_Source (Source : String) return Boolean is
      Result : CCL.Language.Interpretation_Result;
   begin
      CCL.Language.Interpret (Source, DEFAULT_FUEL, Result);
      Print_Result (Result);
      return Result.Status = CCL.Language.Succeeded;
   end Run_Source;

   procedure Run_Arguments is
      Buffer : String (1 .. CCL.Language.MAX_SOURCE_LENGTH);
      Length : Natural := 0;
      Ok     : Boolean;
   begin
      for Index in 1 .. Ada.Command_Line.Argument_Count loop
         declare
            Part : constant String := Ada.Command_Line.Argument (Index);
            Extra : constant Natural :=
              Part'Length + (if Length = 0 then 0 else 1);
         begin
            if Extra > Buffer'Length - Length then
               Put_Line ("error: source exceeds Workbench input bound");
               Ada.Command_Line.Set_Exit_Status
                 (Ada.Command_Line.Failure);
               return;
            end if;
            if Length > 0 then
               Length := Length + 1;
               Buffer (Length) := ' ';
            end if;
            if Part'Length > 0 then
               Buffer (Length + 1 .. Length + Part'Length) := Part;
               Length := Length + Part'Length;
            end if;
         end;
      end loop;
      Ok := Run_Source (Buffer (1 .. Length));
      if not Ok then
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      end if;
   end Run_Arguments;

   procedure Run_REPL is
      Buffer : String (1 .. CCL.Language.MAX_SOURCE_LENGTH);
      Last   : Natural;
      Ignore : Boolean;
   begin
      Put_Line ("CuBit Control Language Workbench");
      Put_Line ("Type :help for help or :quit to leave.");
      loop
         Put (PROMPT);
         Get_Line (Buffer, Last);
         if Last = 0 then
            null;
         elsif Buffer (1 .. Last) = ":quit" or else
           Buffer (1 .. Last) = ":q"
         then
            exit;
         elsif Buffer (1 .. Last) = ":help" then
            Put_Line ("Enter one CCL expression, for example (+ 20 22).");
            Put_Line ("Evaluation is type-checked and fuel-bounded.");
         else
            Ignore := Run_Source (Buffer (1 .. Last));
         end if;
      end loop;
   exception
      when End_Error =>
         New_Line;
   end Run_REPL;

begin
   if Ada.Command_Line.Argument_Count = 0 then
      Run_REPL;
   else
      Run_Arguments;
   end if;
end Main;
