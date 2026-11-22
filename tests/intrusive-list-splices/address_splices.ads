pragma SPARK_Mode (On);
with System;
with Intrusive_List_Splices;
package Address_Splices is new Intrusive_List_Splices (System.Address);
