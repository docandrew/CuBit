pragma SPARK_Mode;
with Interfaces; use Interfaces;
with Volume_Lists;
with Volume_List_Endpoints;

--  The hosted instance: endpoints are numbers (0 is none).
package Volume_List is new Volume_Lists
  (Endpoint_Type => Unsigned_64, No_Endpoint => 0, Valid => Volume_List_Endpoints.Valid);
