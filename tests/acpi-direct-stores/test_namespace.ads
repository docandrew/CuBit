pragma SPARK_Mode (On);
with AML_Delays;
with AML_Namespace;
package Test_Namespace is new AML_Namespace (16, AML_Delays.Unavailable_Provider);
