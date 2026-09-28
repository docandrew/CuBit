pragma Ada_2022;
with Boot_Panel;
package Boot_QR_Capsule with SPARK_Mode, Pure is
   Maximum_Length : constant := 78;
   function Build (Current, Completed, Detail, Failure : Boot_Panel.Line)
     return String;
end Boot_QR_Capsule;
