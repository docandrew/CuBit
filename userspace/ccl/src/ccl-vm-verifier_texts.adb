with CCL.Text_Operations;
with CCL.VM.Operations;

package body CCL.VM.Verifier_Texts with SPARK_Mode => On is
   package T renames CCL.Text_Operations;
   package L renames CCL.List_Operations;
   use type L.Operation;
   use type CCL.Streams.View_Kind;

   procedure Step_Text
     (Candidate : Program; Ins : Instruction; State : in out Abstract_State; Error : in out Validation_Error)
   is
   begin
      if Error /= Valid then
         return;
      end if;
      case Ins.Op is
         when Push_Text =>
            if Ins.Immediate not in 0 .. Integer_64 (Candidate.Constants_Length) - 1 then
               Error := Invalid_Constant;
            else
               Push_Kind (State, Text_Value, Error);
            end if;
         when Concat_Text | Equal_Text =>
            Pop_Kind (State, Text_Value, Error);
            Pop_Kind (State, Text_Value, Error);
            Push_Kind (State, (if Ins.Op = Concat_Text then Text_Value else Boolean_Value), Error);
         when Length_Text =>
            Pop_Kind (State, Text_Value, Error);
            Push_Kind (State, Integer_Value, Error);
         when Text_At =>
            Pop_Kind (State, Integer_Value, Error);
            Pop_Kind (State, Text_Value, Error);
            Push_Kind (State, Character_Value, Error);
         when Integer_To_Text =>
            Pop_Kind (State, Integer_Value, Error);
            Push_Kind (State, Text_Value, Error);
         when Variant_To_Text =>
            if not CCL.Types.Is_Enumeration (Candidate.Data_Types, Ins.Data_Type) or else
              Ins.Alternative /= 0
            then Error := Invalid_Data_Type;
            else
               Pop_Kind (State, Variant_Value, Error, Ins.Data_Type);
               Push_Kind (State, Text_Value, Error);
            end if;
         when Text_Builtin =>
            declare
               Operation : T.Operation;
               Known : Boolean;
            begin
               Operations.Find_Operation (Ins.Immediate, Operation, Known);
               if not Known then
                  Error := Invalid_Builtin;
               else
                  declare
                     Sig : constant T.Signature := T.Signature_Of (Operation);
                  begin
                     Pop_Kind (State, Text_Value, Error);
                     for I in reverse 1 .. Sig.Count loop
                        Pop_Kind (State, Operations.Kind_Of (Sig.Operands (I)), Error);
                     end loop;
                     Push_Kind (State, Operations.Kind_Of (Sig.Result), Error);
                  end;
               end if;
            end;
         when others => null;
      end case;
   end Step_Text;

   procedure Step_List
     (Candidate : Program; Ins : Instruction; State : in out Abstract_State; Error : in out Validation_Error)
   is
      Types : CCL.Types.Registry renames Candidate.Data_Types;
   begin
      if Error /= Valid then
         return;
      end if;
      if Ins.Op = List_Builtin then
         declare
            Operation : L.Operation;
            Known : Boolean;
            List_Type : constant CCL.Types.Type_Reference := Ins.Data_Type;
         begin
            Operations.Find_List_Operation (Ins.Immediate, Operation, Known);
            if not Known or else Ins.Alternative /= 0 or else
              not Operations.List_Builtin_Applies (Types, Operation, List_Type)
            then
               Error := Invalid_Builtin;
            else
               case Operation is
                  when L.Range_Items =>
                     Pop_Kind (State, Integer_Value, Error);
                     Pop_Kind (State, Integer_Value, Error);
                  when L.Split_Text =>
                     Pop_Kind (State, Text_Value, Error);
                     Pop_Kind (State, Text_Value, Error);
                  when others =>
                     Pop_Kind (State, List_Value, Error, List_Type);
                     case Operation is
                        when L.First_Items | L.Last_Items | L.Skip_Items =>
                           Pop_Kind (State, Integer_Value, Error);
                        when L.Contains_Item =>
                           Pop_Kind (State, Element_Kind (Types, List_Type), Error,
                                     Element_Data_Type (Types, List_Type));
                        when L.Join_Items =>
                           Pop_Kind (State, Text_Value, Error);
                        when others => null;
                     end case;
               end case;
               case Operation is
                  when L.Sum_Items | L.Min_Items | L.Max_Items =>
                     Push_Kind (State, Integer_Value, Error);
                  when L.Contains_Item =>
                     Push_Kind (State, Boolean_Value, Error);
                  when L.Join_Items =>
                     Push_Kind (State, Text_Value, Error);
                  when others =>
                     Push_Kind (State, List_Value, Error, List_Type);
               end case;
            end if;
         end;
      elsif not Supported_List (Types, Ins.Data_Type) or else
        Ins.Alternative /= 0 or else
        (case Ins.Op is
            when New_List => Ins.Immediate not in 0 .. MAX_LIST_ELEMENTS,
            when Fill_List => Ins.Immediate not in 1 .. MAX_LIST_ELEMENTS,
            when others => Ins.Immediate /= 0)
      then Error := Invalid_Data_Type;
      else
         case Ins.Op is
            when New_List => null;
            when Fill_List =>
               Pop_Kind (State, Element_Kind (Types, Ins.Data_Type), Error,
                         Element_Data_Type (Types, Ins.Data_Type));
               Pop_Kind (State, List_Value, Error, Ins.Data_Type);
            when Length_List =>
               Pop_Kind (State, List_Value, Error, Ins.Data_Type);
            when others =>
               Pop_Kind (State, Integer_Value, Error);
               Pop_Kind (State, List_Value, Error, Ins.Data_Type);
         end case;
         case Ins.Op is
            when New_List | Fill_List =>
               Push_Kind (State, List_Value, Error, Ins.Data_Type);
            when Length_List =>
               Push_Kind (State, Integer_Value, Error);
            when others =>
               Push_Kind (State, Element_Kind (Types, Ins.Data_Type), Error,
                          Element_Data_Type (Types, Ins.Data_Type));
         end case;
      end if;
   end Step_List;

   procedure Step_Stream
     (Candidate : Program; Ins : Instruction; State : in out Abstract_State; Error : in out Validation_Error)
   is
      Types : CCL.Types.Registry renames Candidate.Data_Types;
   begin
      if Error /= Valid then
         return;
      end if;
      if Ins.Op = Push_Stream then
         if not CCL.Types.Is_Handle (Types, Ins.Data_Type) or else
           Ins.Immediate not in 1 .. CCL.Streams.Maximum_Handle or else
           Ins.Alternative /= 0
         then Error := Invalid_Data_Type;
         else
            Push_Kind (State, Integer_Value, Error, Ins.Data_Type);
         end if;
      else
         declare
            View : CCL.Streams.View_Kind;
            Known : Boolean;
            Result : CCL.Types.Type_Reference;
         begin
            Operations.Find_View (Ins.Immediate, View, Known);
            if not Known then
               Error := Invalid_Builtin;
            else
               Result := Stream_View_Type (Types, Ins.Data_Type, View);
               if Ins.Alternative /= 0 or else Result = CCL.Types.Invalid_Type or else
                 not Function_Data (Types, Kind_For_Type (Types, Result), Reference_For_Type (Types, Result))
               then Error := Invalid_Data_Type;
               else
                  Pop_Kind (State, Integer_Value, Error, Ins.Data_Type);
                  if View = CCL.Streams.Window_View then
                     Pop_Kind (State, Integer_Value, Error);
                  end if;
                  Push_Kind (State, Kind_For_Type (Types, Result), Error, Reference_For_Type (Types, Result));
               end if;
            end if;
         end;
      end if;
   end Step_Stream;
end CCL.VM.Verifier_Texts;
