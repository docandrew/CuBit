pragma SPARK_Mode (On);
with AML_Delays;
with AML_Namespace;
package Namespace_Instance is new AML_Namespace (Perform_Delay => AML_Delays.Unavailable_Provider, Capacity => 128);
