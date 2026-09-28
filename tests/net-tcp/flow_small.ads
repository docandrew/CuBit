pragma SPARK_Mode (On);
with Endpoint_Small;
with TCP_Flow;
package Flow_Small is new TCP_Flow (Endpoints => Endpoint_Small);
