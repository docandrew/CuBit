with Heap_Growth;
package body Heap_Growth_Proof with SPARK_Mode is
   procedure Exercise (Count, Fail_At : Natural; Success : out Boolean) is
      procedure Add (Index : Natural; OK : out Boolean) is
      begin
         OK := Index /= Fail_At;
      end Add;
      procedure Unmap (Index : Natural) is null;
      procedure Synchronize is null;
      procedure Release_Latest is null;
      procedure Grow is new Heap_Growth.Apply (Add, Unmap, Synchronize, Release_Latest);
   begin
      Grow (Count, Success);
   end Exercise;
end Heap_Growth_Proof;
