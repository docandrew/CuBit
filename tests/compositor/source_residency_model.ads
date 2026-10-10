pragma SPARK_Mode (On);
with Compositor_Source_Residency;
package Source_Residency_Model is
   type Model_Slot is range 132 .. 147;
   package Book is new Compositor_Source_Residency (Model_Slot);
end Source_Residency_Model;
