with ACPI_Serving_State;
package ACPI_Serving_Properties with SPARK_Mode, Ghost is
   package P renames ACPI_Serving_State;
   procedure Observer_Requires_All_Facts(Facts:P.Eligibility;Retired:Boolean);
   procedure Consumer_Duty_Blocks_Observer(Facts:P.Eligibility;Retired:Boolean)
     with Pre=>not Facts.Consumer_Owner_Quiet;
   procedure Retirement_Is_Required(Facts:P.Eligibility);
   procedure Runtime_Requires_Admission(Admitted:Boolean);
   procedure Observer_Choice_Is_Final(Facts:P.Eligibility)
     with Pre=>P.Eligible(Facts);
end ACPI_Serving_Properties;
