pragma Ada_2022;
with Intel_GPIO_Controller;
with GPIO_Native_Clock;
with GPIO_Native_Communities;
package GPIO_Dynamic_Controller_Instance is
   -- Semantic-only fixture capacity, not a production/hardware limit.
   package Controller is new Intel_GPIO_Controller
     (2, GPIO_Native_Clock.Read, GPIO_Native_Communities.Read, GPIO_Native_Communities.Write);
end GPIO_Dynamic_Controller_Instance;
