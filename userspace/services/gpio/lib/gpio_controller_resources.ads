pragma Ada_2022;
with Intel_ADLN_GPIO;
package GPIO_Controller_Resources with SPARK_Mode, Pure is
   Maximum_Managed_Pins : constant := Integer (Intel_ADLN_GPIO.Pad_Number'Last) + 1;
   type Pin_Count is range 0 .. Maximum_Managed_Pins;
   subtype Pin_Limit is Pin_Count range 1 .. Pin_Count'Last;
   -- Capacity is explicit and bounded by physical pad cardinality. Unmapped
   -- pads still refuse geometry, so at most251 distinct GPIO pins can register.
end GPIO_Controller_Resources;
