package body Parameters is
   function Value return GPIO_Parameters_Value is
     (capacity => 2, operation_budget => 32, startup_budget => 4096,
      pages_per_resource => 2, work => (events => 2, requests => 2, steps => 4),
      timing => (startup => 5000, admission => 1000, cleanup => 1000,
         readiness => 100, poll_interval => 10, poll_window => 100, progress => 1));
end Parameters;
