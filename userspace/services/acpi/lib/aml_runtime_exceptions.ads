with AML_Decode;
package AML_Runtime_Exceptions with SPARK_Mode, Pure is
   use type AML_Decode.Integer_Value;
   use type AML_Decode.Byte;
   subtype Status_Code is AML_Decode.Integer_Value range 0 .. 16#FFFF_FFFF#;
   Max_Name_Length : constant Positive := 28;
   type Fault is (Unload_Bad_Handle, Mutex_Not_Acquired, Division_By_Zero);
   function Code_For (Item : Fault) return Status_Code is
     (case Item is
        when Unload_Bad_Handle => 16#3003#,
        when Mutex_Not_Acquired => 16#3016#,
        when Division_By_Zero => 16#300C#);
   function Normalize (Value : AML_Decode.Integer_Value) return Status_Code
     with Global => null,
       Post => Normalize'Result = (Value and 16#FFFF_FFFF#);
   function Name (Code : Status_Code) return String
     with Global => null,
       Post => Name'Result'First = 1 and then Name'Result'Length in 1 .. Max_Name_Length;
   function Message (Code : Status_Code) return AML_Decode.Bytes
     with Global => null,
       Post => Message'Result'First = 1
         and then Message'Result'Length = Name (Code)'Length
         and then (for all Index in Message'Result'Range =>
           Message'Result (Index) = AML_Decode.Byte (Character'Pos (Name (Code) (Index))));
end AML_Runtime_Exceptions;
