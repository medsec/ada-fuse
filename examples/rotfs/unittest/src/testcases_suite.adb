--
--
--


-- used packages


with Test_Setup;


-- package specification


package body Testcases_Suite is


   package AU_TS renames AUnit.Test_Suites;


   function Suite return AU_TS.Access_Test_Suite is
      TS_Ptr : constant AU_TS.Access_Test_Suite := new AU_TS.Test_Suite;
   begin
      TS_Ptr.Add_Test(new Test_Setup.TC);
      return TS_Ptr;
   end Suite;


end Testcases_Suite;
