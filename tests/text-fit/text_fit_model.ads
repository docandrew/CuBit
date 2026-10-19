pragma SPARK_Mode;
with Model_Width;
with Text_Fit;
--  The instance gnatprove proves Text_Fit's body through.
package Text_Fit_Model is new Text_Fit (Model_Width.Width, Model_Width.Hint);
