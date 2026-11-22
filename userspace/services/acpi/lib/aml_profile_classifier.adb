pragma Ada_2022;
with AML_Decode;
with AML_Integers;
with AML_Logic;
with AML_Names;
package body AML_Profile_Classifier with SPARK_Mode is
   package P renames AML_Dispatch_Profile;
   package E renames AML_Profile_Events;
   use type E.Event_Kind, AML_Decode.Byte;
   Zero_Op : constant AML_Decode.Byte := 16#00#;
   One_Op : constant AML_Decode.Byte := 16#01#;
   Ones_Op : constant AML_Decode.Byte := 16#FF#;
   Byte_Op : constant AML_Decode.Byte := 16#0A#;
   Word_Op : constant AML_Decode.Byte := 16#0B#;
   Dword_Op : constant AML_Decode.Byte := 16#0C#;
   Qword_Op : constant AML_Decode.Byte := 16#0E#;
   String_Op : constant AML_Decode.Byte := 16#0D#;
   Alias_Op : constant AML_Decode.Byte := 16#06#;
   Name_Op : constant AML_Decode.Byte := 16#08#;
   Scope_Op : constant AML_Decode.Byte := 16#10#;
   Buffer_Op : constant AML_Decode.Byte := 16#11#;
   Package_Op : constant AML_Decode.Byte := 16#12#;
   Variable_Package_Op : constant AML_Decode.Byte := 16#13#;
   Method_Op : constant AML_Decode.Byte := 16#14#;
   Local_First : constant AML_Decode.Byte := 16#60#;
   Argument_Last : constant AML_Decode.Byte := 16#6E#;
   Store_Op : constant AML_Decode.Byte := 16#70#;
   Ref_Of_Op : constant AML_Decode.Byte := 16#71#;
   Deref_Of_Op : constant AML_Decode.Byte := 16#83#;
   Index_Op : constant AML_Decode.Byte := 16#88#;
   Copy_Object_Op : constant AML_Decode.Byte := 16#9D#;
   Create_Dword_Op : constant AML_Decode.Byte := 16#8A#;
   Create_Bit_Op : constant AML_Decode.Byte := 16#8D#;
   Create_Qword_Op : constant AML_Decode.Byte := 16#8F#;
   Continue_Op : constant AML_Decode.Byte := 16#9F#;
   If_Op : constant AML_Decode.Byte := 16#A0#;
   Break_Op : constant AML_Decode.Byte := 16#A5#;
   Breakpoint_Op : constant AML_Decode.Byte := 16#CC#;
   Mutex_Extension : constant AML_Decode.Byte := 16#01#;
   Event_Extension : constant AML_Decode.Byte := 16#02#;
   Conditional_Ref_Extension : constant AML_Decode.Byte := 16#12#;
   Create_Field_Extension : constant AML_Decode.Byte := 16#13#;
   Load_Table_Extension : constant AML_Decode.Byte := 16#1F#;
   Load_Extension : constant AML_Decode.Byte := 16#20#;
   Stall_Extension : constant AML_Decode.Byte := 16#21#;
   Release_Extension : constant AML_Decode.Byte := 16#27#;
   Fatal_Extension : constant AML_Decode.Byte := 16#32#;
   Operation_Region_Extension : constant AML_Decode.Byte := 16#80#;
   Bank_Field_Extension : constant AML_Decode.Byte := 16#87#;
   Root_Name_Prefix : constant AML_Decode.Byte := 16#5C#;
   Parent_Name_Prefix : constant AML_Decode.Byte := 16#5E#;
   Dual_Name_Prefix : constant AML_Decode.Byte := 16#2E#;
   Multi_Name_Prefix : constant AML_Decode.Byte := 16#2F#;
   function Classify (Item : E.Event) return P.Instruction_Class is
      B : constant AML_Decode.Byte := Item.Opcode;
   begin
      if Item.Kind = E.Table_Enter then return P.Table_Load;
      elsif Item.Kind = E.Table_Leave then return P.Other;
      elsif B = E.Extended_Prefix then
         if not Item.Has_Extension then return P.Other; end if;
         case Item.Extension is
            when Mutex_Extension | Event_Extension | Create_Field_Extension |
                 Operation_Region_Extension .. Bank_Field_Extension => return P.Namespace_Declaration;
            when Load_Table_Extension | Load_Extension => return P.Table_Operation;
            when Stall_Extension .. Release_Extension => return P.Synchronization;
            when Conditional_Ref_Extension => return P.Store_Or_Reference;
            when Fatal_Extension => return P.Control_Flow;
            when others => return P.Other;
         end case;
      elsif B in Zero_Op | One_Op | Ones_Op | Byte_Op | Word_Op | Dword_Op | Qword_Op then return P.Integer_Literal;
      elsif B = String_Op then return P.String_Literal;
      elsif B in Buffer_Op | Package_Op | Variable_Package_Op then return P.Buffer_Or_Package;
      elsif AML_Integers.Supported (B) then return P.Arithmetic;
      elsif AML_Logic.Supported (B) then return P.Logical;
      elsif B in Alias_Op | Name_Op | Scope_Op | Method_Op | Create_Dword_Op .. Create_Bit_Op | Create_Qword_Op then
         return P.Namespace_Declaration;
      elsif B in If_Op .. Break_Op | Continue_Op | Breakpoint_Op then return P.Control_Flow;
      elsif B in Local_First .. Argument_Last | Store_Op | Ref_Of_Op | Deref_Of_Op | Index_Op | Copy_Object_Op then
         return P.Store_Or_Reference;
      elsif AML_Names.Lead (B) or else B in Root_Name_Prefix | Parent_Name_Prefix |
        Dual_Name_Prefix | Multi_Name_Prefix then return P.Name_Reference_Or_Call;
      else return P.Other;
      end if;
   end Classify;
end AML_Profile_Classifier;
