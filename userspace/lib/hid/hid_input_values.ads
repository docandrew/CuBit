pragma Ada_2022;
with HID_Report_Schema;
generic
   with package Schema is new HID_Report_Schema (<>);
package HID_Input_Values with SPARK_Mode is
   use type Schema.Numeric_Value;
   type Status is (Decoded, Invalid_Schema, Not_Input, Constant_Field,
      Unsupported_Width, Element_Out_Of_Range, Report_ID_Mismatch,
      Invalid_Length, Invalid_Field_Range, Value_Outside_Logical_Range,
      Null_Value);
   procedure Read_Element
     (Data : Schema.Bytes; Layout : Schema.Result; Field : Schema.Field_Span;
      Element : Schema.Bit_Count; Value : out Schema.Numeric_Value;
      Outcome : out Status)
     with Global => null,
       Post => (if Outcome /= Decoded then Value = 0 else
          Value >= Field.Logical_Minimum and then Value <= Field.Logical_Maximum);
   -- Data begins with the report ID when Layout.Has_Report_IDs; it excludes
   -- the HID-over-I2C two-byte length prefix. Element is zero-based. Exact
   -- report length is required; unused high bits in its last byte are ignored.
   -- Caller selects Field from the successfully decoded descriptor. Geometry,
   -- ID, payload bounds and logical range are checked again here. Value is
   -- zero unless Outcome is Decoded. Null_Value is a reported HID null state,
   -- never a coordinate. Constant padding is not interpreted as input data.
end HID_Input_Values;
