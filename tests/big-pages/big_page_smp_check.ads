package Big_Page_SMP_Check is
   procedure Prepare;
   procedure Secondary (CPU : Positive);
   procedure Finish (Count : Natural);
end Big_Page_SMP_Check;
