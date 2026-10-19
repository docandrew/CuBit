with Intel_GPU_Queue_Service;
-- Test storage for every session of a hosted queue service table.
generic
   with package Service is new Intel_GPU_Queue_Service (<>);
procedure Queue_Service_Prepare (T : in out Service.Table);
