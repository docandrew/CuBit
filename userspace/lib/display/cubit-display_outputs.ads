with CuBit.Output_Registry;

--  Production counter budget; tests instantiate smaller budgets as well.
package CuBit.Display_Outputs with SPARK_Mode, Pure is
   package Registry is new CuBit.Output_Registry;
end CuBit.Display_Outputs;
