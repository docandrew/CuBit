with CCL.Control;
with Control_Wire;

--  Responses to operations 7 (Present_Expression) and 8 (Read_Image_Rows):
--  a result as CCL.Presentations describes it, and bands of a stored
--  image's pixels (README, "Presentations"). Kept outside the proved
--  Control_Wire codec: the presentation model and the image store are not
--  SPARK units, so these encoders are regression-tested, not proved.
--  Control_Wire still decodes and validates both requests.
package Control_Presentation is
   procedure Encode
     (Query : Control_Wire.Request; Value : CCL.Control.Response;
      Data : out Control_Wire.Response)
   with Pre => Query.Op in CCL.Control.Present_Expression | CCL.Control.Read_Image_Rows |
                           CCL.Control.Present_Monitor | CCL.Control.Complete_Expression;
end Control_Presentation;
