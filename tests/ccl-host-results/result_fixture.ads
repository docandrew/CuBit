with CCL.Host_Values;
package Result_Fixture with SPARK_Mode is
   use type CCL.Host_Values.Value_Kind;
   procedure Reply
     (Kind : CCL.Host_Values.Value_Kind; Item : out CCL.Host_Values.Call_Result)
     with Global => null, Post => Item.Success and Item.Value.Kind = Kind;
end Result_Fixture;
