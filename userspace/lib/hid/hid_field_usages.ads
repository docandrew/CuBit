pragma Ada_2022;
with HID_Report_Schema;
generic
   with package Schema is new HID_Report_Schema (<>);
package HID_Field_Usages with SPARK_Mode is
   use type Schema.Word;
   type Status is (Resolved, No_Usage, Invalid_Schema, Invalid_Field,
                   Array_Field, Element_Out_Of_Range, Invalid_Usage_Table);
   procedure Resolve
     (Layout : Schema.Result; Field : Schema.Field_Span;
      Usages : Schema.Usage_Table; Element : Schema.Bit_Count;
      Usage : out Schema.Word; Outcome : out Status)
     with Global => null,
       Post => (if Outcome /= Resolved then Usage = 0);
   -- Variable fields map usages in descriptor order, expanding ranges without
   -- allocating. Excess elements repeat the final usage (HID 1.11 6.2.2.8).
   -- Array controls require value-to-usage mapping and are explicitly distinct.
   -- Field and Usages must come from one successful Parse. The whole selected
   -- span is validated before returning, including unused trailing terms.
end HID_Field_Usages;
