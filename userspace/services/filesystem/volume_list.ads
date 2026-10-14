pragma SPARK_Mode;
with CuBit.Handles;
with Volume_Lists;

--  The filesystem's volume list: block drivers by endpoint handle.
package Volume_List is new Volume_Lists
  (Endpoint_Type => CuBit.Handles.Endpoint,
   No_Endpoint   => CuBit.Handles.No_Endpoint,
   Valid         => CuBit.Handles.Valid);
