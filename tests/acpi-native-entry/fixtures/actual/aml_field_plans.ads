pragma Ada_2022;
with AML_Decode;
with AML_Field_Datums;
with AML_Field_Layout;
package AML_Field_Plans with SPARK_Mode, Pure is
   use type AML_Decode.Integer_Value;
   use type AML_Field_Datums.Datum_Index;
   Maximum_Datums : constant Positive := 9;
   Maximum_Bits : constant AML_Decode.Integer_Value := 64;
   type Datum_Count is new Natural range 0 .. Maximum_Datums;
   subtype Datum_Position is Datum_Count range 1 .. Datum_Count'Last;
   type Read_Form is (Integer_Result, Buffer_Result);
   type Plan is private;
   -- Geometry only. Every datum still requires a separate trusted named target.
   -- NoLock ordinary Memory/IO fields through 64 bits; AnyAcc uses byte datums.
   function Valid (Item : Plan) return Boolean;
   function Count (Item : Plan) return Datum_Count;
   function Prepare (Field : AML_Field_Layout.Field_Geometry) return Plan
      with Post => (if Count (Prepare'Result) > 0 then Valid (Prepare'Result));
   function Rule (Item : Plan) return AML_Field_Datums.Update_Rule;
   function Datum_At (Item : Plan; Position : Datum_Position)
      return AML_Field_Datums.Datum
      with Pre => Valid (Item) and then Position <= Count (Item);
   function Matches (Item : Plan; Position : Datum_Position;
      Register : AML_Field_Layout.Register_Geometry) return Boolean;
   -- The table's integer width controls the result type, not datum admission.
   function Result_Form (Item : Plan; Width : AML_Decode.Integer_Width)
      return Read_Form with Pre => Valid (Item);
   function Buffer_Length (Item : Plan) return Positive
      with Pre => Valid (Item), Post => Buffer_Length'Result <= 8;
private
   type Plan is record
      Ready : Boolean := False;
      Geometry : AML_Field_Layout.Field_Geometry;
      Datums : AML_Field_Datums.Plan;
      Number : Datum_Count := 0;
      Update : AML_Field_Datums.Update_Rule := AML_Field_Datums.Preserve;
   end record;
   function Valid (Item : Plan) return Boolean is
     (Item.Ready
      and then Item.Geometry.Bit_Length in 1 .. Maximum_Bits
      and then Item.Number in Datum_Position
      and then AML_Field_Datums.Valid (Item.Datums)
      and then AML_Field_Datums.Count (Item.Datums) = AML_Field_Datums.Datum_Index (Item.Number));
   function Count (Item : Plan) return Datum_Count is (Item.Number);
   function Rule (Item : Plan) return AML_Field_Datums.Update_Rule is (Item.Update);
end AML_Field_Plans;
