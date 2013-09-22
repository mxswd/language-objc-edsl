
-- newArrayProp :: OM (Bind Global TTrue (NSArray t))
-- newArrayProp = do
--   let n = "wow"
--   addOM (NoBind (FFunction [cexp|$id:n = [[NSArray alloc] init]|])) -- global
--   mkOM $ Bind n FArray -- local

newTextField :: OM (Bind Global TTrue (NSTextField))
newTextField = undefined

  -- printString h -- This is a type error!
