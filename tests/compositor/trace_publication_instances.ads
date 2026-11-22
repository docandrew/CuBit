with Compositor_Trace_Publication;
package Trace_Publication_Instances with SPARK_Mode is
   package Production is new Compositor_Trace_Publication;
   package Short_Run is new Compositor_Trace_Publication (8);
   package Empty_Run is new Compositor_Trace_Publication (0);
end Trace_Publication_Instances;
