pragma Ada_2022;
with AML_Decode;
with AML_Objects;
package AML_Data with SPARK_Mode, Pure is
   use type AML_Decode.Status;
   use type AML_Objects.State;
   use type AML_Objects.Object_Kind;
   type Count_Result is record
      Kind : AML_Decode.Status := AML_Decode.Unsupported;
      Value : AML_Decode.Integer_Value := 0;
      Consumed : Natural := 0;
   end record;
   generic
      type Context is private;
      with function Read_Count
        (Environment : Context; Data : AML_Decode.Bytes;
         Width : AML_Decode.Integer_Width) return Count_Result;
   procedure Load_Bound
     (Store : in out AML_Objects.State; Data : AML_Decode.Bytes;
      Width : AML_Decode.Integer_Width; Environment : Context;
      ID : out AML_Objects.Object_ID;
      Consumed : out Natural; Status : out AML_Decode.Status)
     with Pre => AML_Objects.Valid (Store),
          Post => AML_Objects.Valid (Store) and then
            (if Status = AML_Decode.Accepted then
               ID > 0 and then ID <= AML_Objects.Count (Store)
               and then Consumed > 0 and then Consumed <= Data'Length
               and then AML_Objects.Count (Store) >= AML_Objects.Count (Store'Old)
               and then (for all J in 1 .. AML_Objects.Count (Store'Old) =>
                 AML_Objects.Kind (Store, J) = AML_Objects.Kind (Store'Old, J)
                 and then AML_Objects.Length (Store, J) = AML_Objects.Length (Store'Old, J))
             else Store = Store'Old and ID = 0 and Consumed = 0);
   --  Constant DataRefObjects, including nested Package/VarPackage objects.
   --  Name references and computed sizes require namespace execution later.
   --  Transactional: malformed/unsupported input consumes no arena storage.
   procedure Load
     (Store : in out AML_Objects.State; Data : AML_Decode.Bytes;
      Width : AML_Decode.Integer_Width; ID : out AML_Objects.Object_ID;
      Consumed : out Natural; Status : out AML_Decode.Status)
     with Pre => AML_Objects.Valid (Store),
          Post => AML_Objects.Valid (Store) and then
            (if Status = AML_Decode.Accepted then
               ID > 0 and then ID <= AML_Objects.Count (Store)
               and then Consumed > 0 and then Consumed <= Data'Length
               and then AML_Objects.Count (Store) >= AML_Objects.Count (Store'Old)
               and then (for all J in 1 .. AML_Objects.Count (Store'Old) =>
                 AML_Objects.Kind (Store, J) = AML_Objects.Kind (Store'Old, J)
                 and then AML_Objects.Length (Store, J) = AML_Objects.Length (Store'Old, J))
             else Store = Store'Old and ID = 0 and Consumed = 0);
end AML_Data;
