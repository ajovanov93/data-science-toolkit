-- | Provide instances of FromNamedRecord for named tuples up to 24 fields.
--
-- Also provides helpful functions for reading them.
--
-- Import like: @import Labels.Cassava@
--
-- Taken from https://github.com/chrisdone/labels/tree/master/labels-csv
module Labels.Csv
  (DowncaseColumns(..))
  where

import Labels.Cassava.Instances (DowncaseColumns(..))
