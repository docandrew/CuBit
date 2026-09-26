--  Concrete instance for proof and hosted tests.
pragma SPARK_Mode (On);
with Id_Ledger;
package Test_Ledger is new Id_Ledger
  (Max_Id => 64, Entries_Per_Page => 8, Reserved_Last => 4,
   Generation_Limit => 40);
