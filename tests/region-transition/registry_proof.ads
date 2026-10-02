with Region_Registry;
-- Representative bounded instance; not a claim for all generic parameters.
package Registry_Proof with SPARK_Mode => On is
   package Model is new Region_Registry (64, 16#5000_0000_0000#, 16#5001_0000_0000#);
end Registry_Proof;
