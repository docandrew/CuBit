with Compositor_Source_Loans;
package Source_Loans_Check with SPARK_Mode is
   -- Production bound: eight surfaces, two publication slots plus one
   -- replacement-acquisition allowance per surface. Metadata only.
   package L is new Compositor_Source_Loans (24);
end Source_Loans_Check;
